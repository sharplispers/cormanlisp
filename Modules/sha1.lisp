;;;; This is an implementation of the US Secure Hash Algorithm 1 (SHA1),
;;;; defined in RFC 3174, written by D. Eastlake and P. Jones, September
;;;; 2001.  The RFC was based on the document "Secure Hash Standard",
;;;; United States of America, National Institute of Science and Technology,
;;;; Federal Information Processing Standard (FIPS) 180-1, April 1993.
;;;;
;;;; It was written by Nathan J. Froyd, with many of the main ideas and
;;;; functions grabbed from Pierre R. Mai's CL implementation of MD5,
;;;; available at http://www.pmsf.de/pmai/MD5.html.
;;;;
;;;; This implementation should work on any conforming Common Lisp
;;;; implementation, but it has been optimized for CMU CL.
;;;;
;;;; The implementation makes heavy use of (UNSIGNED-BYTE 32) arithmetic;
;;;; if your CL implementation does not implement unboxed arithmetic on
;;;; such numbers, performance will likely be greater in a 16-bit
;;;; implementation. 
;;;;
;;;; A test framework has be included; enable it before compilation by
;;;; adding :sha-testing to *FEATURES*.  After doing so, compiling, and
;;;; loading, you may run the testsuite by (SHA::TEST-RFC3174).  This will
;;;; run the test cases in section 7.3 of the RFC and report on the results.
;;;;
;;;; This software is "as is", and has no warranty of any kind.  The
;;;; authors assume no responsibility for the consequences of any use
;;;; of this software.
;;;;
;;;; RGC 07/02/08--added support for buffer-index=55 case
;;;;

(defpackage :SHA (:use :CL)
  (:export
   ;; low-level types and functions
   #:sha1-regs #:initial-sha1-regs #:sha1regs-digest #:expand-block
   #:update-sha1-block #:fill-block #:fill-block-ub8 #:fill-block-char
   ;; mid-level types and functions
   #:sha1-state #:sha1-state-p #:make-sha1-state
   #:update-sha1-state #:finalize-sha1-state
   ;; high-level functions
   #:sha1sum-sequence #:sha1sum-stream #:sha1sum-file))

(in-package :sha)
(provide :sha)

#+cmu
(eval-when (:compile-toplevel)
  (defparameter *old-expansion-limit* ext:*inline-expansion-limit*)
  (setq ext:*inline-expansion-limit* (max ext:*inline-expansion-limit* 1000)))

#+cmu
(eval-when (:compile-toplevel :execute)
  (defparameter *old-features* *features*)
  (pushnew (c:backend-byte-order c:*target-backend*) *features*))

#+sbcl
(eval-when (:compile-toplevel)
  (defparameter *old-features* *features*)
  (pushnew sb-c:*backend-byte-order* *features*))

(deftype ub32 ()
  "Corresponds to the 32bit quantity word of the MD5 Spec"
  `(unsigned-byte 32))

(defmacro assemble-ub32 (a b c d)
  "Assemble an ub32 value from the given (unsigned-byte 8) values,
where a is the intended low-order byte and d the high-order byte."
  `(the ub32 (logior (ash ,d 24) (ash ,c 16) (ash ,b 8) ,a)))

;;; nonlinear functions

(declaim (inline f1 f2 f3 f4)
         (ftype (function (ub32 ub32 ub32) ub32) f1 f2 f3 f4))

;;; t=0..19
(defun f1 (x y z)
  (declare (type ub32 x y z)
	   (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  #+cmu
  (kernel:32bit-logical-or (kernel:32bit-logical-and x y)
                           (kernel:32bit-logical-andc1 x z))
  #+sbcl
  (sb-kernel:32bit-logical-or (sb-kernel:32bit-logical-and x y)
                              (sb-kernel:32bit-logical-andc1 x z))
  #-(or sbcl cmu)
  (logior (logand x y) (logandc1 x z)))

(defconstant +k1+ (assemble-ub32 #x99 #x79 #x82 #x5a))

;;; t=20..39
(defun f2 (x y z)
  (declare (type ub32 x y z)
	   (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  #+cmu
  (kernel:32bit-logical-xor x (kernel:32bit-logical-xor y z))
  #+sbcl
  (sb-kernel:32bit-logical-xor x (sb-kernel:32bit-logical-xor y z))
  #-(or sbcl cmu)
  (logxor x y z))

(defconstant +k2+ (assemble-ub32 #xa1 #xeb #xd9 #x6e))

;;; t=40..59
(defun f3 (x y z)
  (declare (type ub32 x y z)
	   (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  #+cmu
  (kernel:32bit-logical-or (kernel:32bit-logical-or
                            (kernel:32bit-logical-and x y)
                            (kernel:32bit-logical-and x z))
                           (kernel:32bit-logical-and y z))
  #+sbcl
  (sb-kernel:32bit-logical-or (sb-kernel:32bit-logical-or
                               (sb-kernel:32bit-logical-and x y)
                               (sb-kernel:32bit-logical-and x z))
                              (sb-kernel:32bit-logical-and y z))
  #-(or sbcl cmu)
  (logior (logior (logand x y) (logand x z))
          (logand y z)))

(defconstant +k3+ (assemble-ub32 #xdc #xbc #x1b #x8f))

;;; t=60..79 reuses f2

(defconstant +k4+ (assemble-ub32 #xd6 #xc1 #x62 #xca))

(declaim (inline mod32+)
	 (ftype (function (ub32 ub32) ub32) mod32+))
(defun mod32+ (a b)
  (declare (type ub32 a b) (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (ldb (byte 32 0) (+ a b)))

#+cmu
(define-compiler-macro mod32+ (a b)
  `(ext:truly-the ub32 (+ ,a ,b)))

#+sbcl
;;; FIXME: Check whether this actually does the right thing on the
;;; alpha.
(define-compiler-macro mod32+ (a b)
  `(sb-ext:truly-the ub32 (+ ,a ,b)))

(declaim (inline rol32)
	 (ftype (function (ub32 (unsigned-byte 5)) ub32) rol32))
(defun rol32 (a s)
  (declare (type ub32 a) (type (unsigned-byte 5) s)
	   (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  #+cmu
  (kernel:32bit-logical-or #+little-endian (kernel:shift-towards-end a s)
			   #+big-endian (kernel:shift-towards-start a s)
			   (ash a (- s 32)))
  #+sbcl
  (sb-rotate-byte:rotate-byte s (byte 32 0) a)
  #-(or sbcl cmu)
  (logior (ldb (byte 32 0) (ash a s)) (ash a (- s 32))))

;;; helper macro for rounds, with variable capture
(defmacro with-sha1-round ((block func constant) &rest clauses)
  (loop for (a b c d e i) in clauses
        collect
        `(psetq ,e ,d
                ,d ,c
                ,c (rol32 ,b 30)
                ,b ,a
                ,a (mod32+ (rol32 ,a 5)
                           (mod32+ (mod32+ (,func ,b ,c ,d) ,e)
                                   (mod32+ (aref ,block ,i) ,constant))))
        into result
        finally (return `(progn ,@result))))

;;; working set

(deftype sha1-regs ()
  `(simple-array (unsigned-byte 32) (5)))

(defmacro sha1-regs-a (regs)
  `(aref ,regs 0))

(defmacro sha1-regs-b (regs)
  `(aref ,regs 1))

(defmacro sha1-regs-c (regs)
  `(aref ,regs 2))

(defmacro sha1-regs-d (regs)
  `(aref ,regs 3))

(defmacro sha1-regs-e (regs)
  `(aref ,regs 4))

(defconstant +sha1-magic-a+ (assemble-ub32 #x01 #x23 #x45 #x67)
  "Initial value of Register A of the SHA1 working state.")
(defconstant +sha1-magic-b+ (assemble-ub32 #x89 #xab #xcd #xef)
  "Initial value of Register B of the SHA1 working state.")
(defconstant +sha1-magic-c+ (assemble-ub32 #xfe #xdc #xba #x98)
  "Initial value of Register C of the SHA1 working state.")
(defconstant +sha1-magic-d+ (assemble-ub32 #x76 #x54 #x32 #x10)
  "Initial value of Register D of the SHA1 working state.")
(defconstant +sha1-magic-e+ (assemble-ub32 #xf0 #xe1 #xd2 #xc3)
  "Initial value of Register E of the SHA1 working state.")

(declaim (inline initial-sha1-regs))
(defun initial-sha1-regs ()
  "Create the initial working state of an SHA1 run."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (let ((regs (make-array 5 :element-type '(unsigned-byte 32))))
    (declare (type sha1-regs regs))
    (setf (sha1-regs-a regs) +sha1-magic-a+
	  (sha1-regs-b regs) +sha1-magic-b+
	  (sha1-regs-c regs) +sha1-magic-c+
	  (sha1-regs-d regs) +sha1-magic-d+
          (sha1-regs-e regs) +sha1-magic-e+)
    regs))

(defun update-sha1-block (regs block)
  (declare (type sha1-regs regs)
           (type (simple-array ub32 (80)) block)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (let ((a (sha1-regs-a regs)) (b (sha1-regs-b regs))
	(c (sha1-regs-c regs)) (d (sha1-regs-d regs))
        (e (sha1-regs-e regs)))
    (with-sha1-round (block f1 +k1+)
      (a b c d e  0)(a b c d e  1)(a b c d e  2)(a b c d e  3)(a b c d e  4)
      (a b c d e  5)(a b c d e  6)(a b c d e  7)(a b c d e  8)(a b c d e  9)
      (a b c d e 10)(a b c d e 11)(a b c d e 12)(a b c d e 13)(a b c d e 14)
      (a b c d e 15)(a b c d e 16)(a b c d e 17)(a b c d e 18)(a b c d e 19))
    (with-sha1-round (block f2 +k2+)
      (a b c d e 20)(a b c d e 21)(a b c d e 22)(a b c d e 23)(a b c d e 24)
      (a b c d e 25)(a b c d e 26)(a b c d e 27)(a b c d e 28)(a b c d e 29)
      (a b c d e 30)(a b c d e 31)(a b c d e 32)(a b c d e 33)(a b c d e 34)
      (a b c d e 35)(a b c d e 36)(a b c d e 37)(a b c d e 38)(a b c d e 39))
    (with-sha1-round (block f3 +k3+)
      (a b c d e 40)(a b c d e 41)(a b c d e 42)(a b c d e 43)(a b c d e 44)
      (a b c d e 45)(a b c d e 46)(a b c d e 47)(a b c d e 48)(a b c d e 49)
      (a b c d e 50)(a b c d e 51)(a b c d e 52)(a b c d e 53)(a b c d e 54)
      (a b c d e 55)(a b c d e 56)(a b c d e 57)(a b c d e 58)(a b c d e 59))
    (with-sha1-round (block f2 +k4+)
      (a b c d e 60)(a b c d e 61)(a b c d e 62)(a b c d e 63)(a b c d e 64)
      (a b c d e 65)(a b c d e 66)(a b c d e 67)(a b c d e 68)(a b c d e 69)
      (a b c d e 70)(a b c d e 71)(a b c d e 72)(a b c d e 73)(a b c d e 74)
      (a b c d e 75)(a b c d e 76)(a b c d e 77)(a b c d e 78)(a b c d e 79))
    ;; update and return
    (setf (sha1-regs-a regs) (mod32+ (sha1-regs-a regs) a)
	  (sha1-regs-b regs) (mod32+ (sha1-regs-b regs) b)
	  (sha1-regs-c regs) (mod32+ (sha1-regs-c regs) c)
	  (sha1-regs-d regs) (mod32+ (sha1-regs-d regs) d)
          (sha1-regs-e regs) (mod32+ (sha1-regs-e regs) e))
    regs))

(declaim (inline expand-block fill-block fill-block-ub8 fill-block-char))
(defun expand-block (block)
  "Expand the first 16 words in BLOCK to fill the entire 80 word space
available."
  (declare (type (simple-array ub32 (80)) block)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (loop for i of-type (integer 16 80) from 16 below 80
        do (setf (aref block i)
                 (rol32 #+cmu
                        (kernel:32bit-logical-xor
                         (kernel:32bit-logical-xor (aref block (- i 3))
                                                   (aref block (- i 8)))
                         (kernel:32bit-logical-xor (aref block (- i 14))
                                                   (aref block (- i 16))))
                        #+sbcl
                        (sb-kernel:32bit-logical-xor
                         (sb-kernel:32bit-logical-xor (aref block (- i 3))
                                                      (aref block (- i 8)))
                         (sb-kernel:32bit-logical-xor (aref block (- i 14))
                                                      (aref block (- i 16))))
                        #-(or sbcl cmu)
                        (logxor
                         (logxor (aref block (- i 3))
                                 (aref block (- i 8)))
                         (logxor (aref block (- i 14))
                                 (aref block (- i 16))))
                        1))))
    
(defun fill-block (block buffer offset)
  "Convert a complete 64 byte input vector segment into the given 80
word SHA1 block.  This currently works on (unsigned-byte 8) and
character simple-arrays, via the functions `fill-block-ub8' and
`fill-block-char' respectively."
  (declare (type (integer 0 #.(- most-positive-fixnum 64)) offset)
	   (type (simple-array ub32 (80)) block)
	   (type (simple-array * (*)) buffer)
	   (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (etypecase buffer
    ((simple-array (unsigned-byte 8) (*))
     (fill-block-ub8 block buffer offset))
    (simple-string
     (fill-block-char block buffer offset)))
  (expand-block block))

(defun fill-block-ub8 (block buffer offset)
  "Convert a complete 64 (unsigned-byte 8) input vector segment
starting from offset into the given 16 word SHA1 block.  Calling this function
without subsequently calling EXPAND-BLOCK results in undefined behavior."
  (declare (type (integer 0 #.(- most-positive-fixnum 64)) offset)
	   (type (simple-array ub32 (80)) block)
	   (type (simple-array (unsigned-byte 8) (*)) buffer)
	   (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  ;; convert to 32-bit words
  #+(and :cmu :big-endian)
  (kernel:bit-bash-copy
   buffer (+ (* vm:vector-data-offset vm:word-bits)
             (* offset vm:byte-bits))
   block (* vm:vector-data-offset vm:word-bits)
   (* 64 vm:byte-bits))
  #+(and :sbcl :big-endian)
  (sb-kernel:bit-bash-copy
   buffer (+ (* sb-vm:vector-data-offset sb-vm:n-word-bits)
             (* offset sb-vm:n-byte-bits))
   block (* sb-vm:vector-data-offset sb-vm:n-word-bits)
   (* 64 sb-vm:n-byte-bits))
  #-(or (and :sbcl :big-endian) (and :cmu :big-endian))
  (loop for i of-type (integer 0 16) from 0
        for j of-type (integer 0 #.most-positive-fixnum)
        from offset to (+ offset 63) by 4
        do (setf (aref block i)
                 (assemble-ub32 (aref buffer (+ j 3))
                                (aref buffer (+ j 2))
                                (aref buffer (+ j 1))
                                (aref buffer j)))))

(defun fill-block-char (block buffer offset)
  "Convert a complete 64 character input string segment starting from
offset into the given 16 word SHA1 block.  Calling this function without
subsequently calling EXPAND-BLOCK results in undefined behavior."
  (declare (type (integer 0 #.(- most-positive-fixnum 64)) offset)
	   (type (simple-array ub32 (80)) block)
	   (type simple-string buffer)
	   (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  #+(and :cmu :big-endian)
  (kernel:bit-bash-copy
   buffer (+ (* vm:vector-data-offset vm:word-bits)
             (* offset vm:byte-bits))
   block (* vm:vector-data-offset vm:word-bits)
   (* 64 vm:byte-bits))
  #+(and :sbcl :big-endian)
  (sb-kernel:bit-bash-copy
   buffer (+ (* sb-vm:vector-data-offset sb-vm:n-word-bits)
             (* offset sb-vm:n-byte-bits))
   block (* sb-vm:vector-data-offset sb-vm:n-word-bits)
   (* 64 sb-vm:n-byte-bits))
  #-(or (and :sbcl :big-endian) (and :cmu :big-endian))
  (loop for i of-type (integer 0 16) from 0
        for j of-type (integer 0 #.most-positive-fixnum)
        from offset to (+ offset 63) by 4
        do (setf (aref block i)
                 (assemble-ub32 (char-code (schar buffer (+ j 3)))
                                (char-code (schar buffer (+ j 2)))
                                (char-code (schar buffer (+ j 1)))
                                (char-code (schar buffer j))))))

(declaim (inline sha1regs-digest))
(defun sha1regs-digest (regs)
  "Create the final 20 byte message-digest from the SHA1 working state in
REGS.  Returns a (simple-array (unsigned-byte 8) (20))."
  (declare (type sha1-regs regs)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (let ((result (make-array 20 :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (20)) result))
    (macrolet ((frob (reg offset)
		 (let ((var (gensym)))
		   `(let ((,var ,reg))
		      (declare (type ub32 ,var))
		      (setf
		       (aref result ,offset) (ldb (byte 8 24) ,var)
		       (aref result ,(+ offset 1)) (ldb (byte 8 16) ,var)
		       (aref result ,(+ offset 2)) (ldb (byte 8 8) ,var)
		       (aref result ,(+ offset 3)) (ldb (byte 8 0) ,var))))))
      (frob (sha1-regs-a regs) 0)
      (frob (sha1-regs-b regs) 4)
      (frob (sha1-regs-c regs) 8)
      (frob (sha1-regs-d regs) 12)
      (frob (sha1-regs-e regs) 16))
    result))

;;; mid-level

(defstruct (sha1-state
             (:constructor make-sha1-state ())
             (:copier))
  (regs (initial-sha1-regs) :type sha1-regs :read-only t)
  (amount 0 :type (unsigned-byte 64))   ; ugly bignums
  (block (make-array 80 :element-type '(unsigned-byte 32)) :read-only t
         :type (simple-array (unsigned-byte 32) (80)))
  (buffer (make-array 64 :element-type '(unsigned-byte 8)) :read-only t
          :type (simple-array (unsigned-byte 8) (64)))
  (buffer-index 0 :type (integer 0 63))
  (finalized-p nil))

(declaim (inline copy-to-buffer))
(defun copy-to-buffer (from from-offset count buffer buffer-offset)
  "Copy a partial segment from input vector from starting at
from-offset and copying count elements into the 64 byte buffer
starting at buffer-offset."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
	   (type (unsigned-byte 29) from-offset)
	   (type (integer 0 63) count buffer-offset)
	   (type (simple-array * (*)) from)
	   (type (simple-array (unsigned-byte 8) (64)) buffer))
  #+cmu
  (kernel:bit-bash-copy
   from (+ (* vm:vector-data-offset vm:word-bits)
           (* from-offset vm:byte-bits))
   buffer (+ (* vm:vector-data-offset vm:word-bits)
	     (* buffer-offset vm:byte-bits))
   (* count vm:byte-bits))
  #+sbcl
  (sb-kernel:bit-bash-copy
   from (+ (* sb-vm:vector-data-offset sb-vm:n-word-bits)
           (* from-offset sb-vm:n-byte-bits))
   buffer (+ (* sb-vm:vector-data-offset sb-vm:n-word-bits)
	     (* buffer-offset sb-vm:n-byte-bits))
   (* count sb-vm:n-byte-bits))
  #-(or sbcl cmu)
  (etypecase from
    (simple-string
     (loop for buffer-index of-type (integer 0 64) from buffer-offset
	   for from-index of-type fixnum from from-offset
	   below (+ from-offset count)
	   do
	   (setf (aref buffer buffer-index)
		 (char-code (schar (the simple-string from) from-index)))))
    ((simple-array (unsigned-byte 8) (*))
     (loop for buffer-index of-type (integer 0 64) from buffer-offset
	   for from-index of-type fixnum from from-offset
	   below (+ from-offset count)
	   do
	   (setf (aref buffer buffer-index)
		 (aref (the (simple-array (unsigned-byte 8) (*)) from)
		       from-index))))))

(defun update-sha1-state (state sequence &key (start 0) (end (length sequence)))
  (declare (type sha1-state state)
           (type (simple-array * (*)) sequence)
           (type fixnum start end)
           (optimize (speed 3) #+(or sbcl cmu) (safety 0) (space 0) (debug 0))
           #+cmu
           (ext:optimize-interface (safety 1) (debug 1)))
  (let ((regs (sha1-state-regs state))
        (block (sha1-state-block state))
        (buffer (sha1-state-buffer state))
        (buffer-index (sha1-state-buffer-index state))
        (length (- end start)))
    (declare (type sha1-regs regs) (type fixnum length)
	     (type (integer 0 63) buffer-index)
	     (type (simple-array (unsigned-byte 32) (80)) block)
	     (type (simple-array (unsigned-byte 8) (64)) buffer))
    ;; Handle old rest
    (unless (zerop buffer-index)
      (let ((amount (min (- 64 buffer-index) length)))
	(declare (type (integer 0 63) amount))
	(copy-to-buffer sequence start amount buffer buffer-index)
	(setq start (the fixnum (+ start amount)))
	(when (>= start end)
	  (setf (sha1-state-buffer-index state) (+ buffer-index amount))
	  (return-from update-sha1-state state)))
      (fill-block-ub8 block buffer 0)
      (expand-block block)
      (update-sha1-block regs block))
    ;; Handle main-part and new-rest
    (etypecase sequence
      ((simple-array (unsigned-byte 8) (*))
       (locally
	   (declare (type (simple-array (unsigned-byte 8) (*)) sequence))
	 (loop for offset of-type (unsigned-byte 29) from start below end by 64
	       until (< (- end offset) 64)
	       do
	       (fill-block-ub8 block sequence offset)
               (expand-block block)
	       (update-sha1-block regs block)
	       finally
	       (let ((amount (- end offset)))
		 (unless (zerop amount)
		   (copy-to-buffer sequence offset amount buffer 0))
		 (setf (sha1-state-buffer-index state) amount)))))
      (simple-string
       (locally
	   (declare (type simple-string sequence))
	 (loop for offset of-type (unsigned-byte 29) from start below end by 64
	       until (< (- end offset) 64)
	       do
	       (fill-block-char block sequence offset)
               (expand-block block)
	       (update-sha1-block regs block)
	       finally
	       (let ((amount (- end offset)))
		 (unless (zerop amount)
		   (copy-to-buffer sequence offset amount buffer 0))
		 (setf (sha1-state-buffer-index state) amount))))))
    (setf (sha1-state-amount state)
          (+ (sha1-state-amount state) length))
    state))

(defun finalize-sha1-state (state)
  (declare (type sha1-state state)
	   (optimize (speed 3) #+(or sbcl cmu) (safety 0) (space 0) (debug 0))
           #+cmu
           (ext:optimize-interface (safety 1) (debug 1)))
  (or (sha1-state-finalized-p state)
      (let ((regs (sha1-state-regs state))
	    (block (sha1-state-block state))
	    (buffer (sha1-state-buffer state))
	    (buffer-index (sha1-state-buffer-index state))
	    (total-length (* 8 (sha1-state-amount state))))
	(declare (type sha1-regs regs)
		 (type (integer 0 63) buffer-index)
		 (type (simple-array ub32 (80)) block)
		 (type (simple-array (unsigned-byte 8) (*)) buffer))
        (setf (aref buffer buffer-index) #x80)
        (when (> buffer-index 55)
          (loop for index of-type (integer 0 64)
                from (1+ buffer-index) below 64
                do (setf (aref buffer index) #x00))
          (fill-block-ub8 block buffer 0)
          (expand-block block)
          (update-sha1-block regs block)
          (loop for index of-type (integer 0 14)
                from 0 below 14
                do (setf (aref block index) #x00000000)))
        (when (<= buffer-index 55)          ;; RGC 07/02/08--added support for buffer-index=55 case
          (loop for index of-type (integer 0 56)
                from (1+ buffer-index) below 56
                do (setf (aref buffer index) #x00))
          ;; copy the data to BLOCK prematurely
          (fill-block-ub8 block buffer 0))
        ;; fill in the remaining block data
        (setf (aref block 14) (ldb (byte 32 32) total-length)
              (aref block 15) (ldb (byte 32 0) total-length))
        (expand-block block)
	(update-sha1-block regs block)
	(setf (sha1-state-finalized-p state)
	      (sha1regs-digest regs)))))

(defun sha1sum-sequence (sequence &key (start 0) end)
  "Calculate the SHA1 message-digest of data in sequence.  On CMU CL
this works for all sequences whose element-type is supported by the
underlying SHA1 routines, on other implementations it only works for 1d
simple-arrays with such element types."
  (declare (optimize (speed 3) (space 0) (debug 0))
	   (type vector sequence) (type fixnum start))
  (let ((state (make-sha1-state)))
    (declare (type sha1-state state))
    #+cmu
    (lisp::with-array-data ((data sequence) (real-start start) (real-end end))
      (update-sha1-state state data :start real-start :end real-end))
    #+sbcl
    (sb-kernel:with-array-data ((data sequence) (real-start start) (real-end end))
      (update-sha1-state state data :start real-start :end real-end))
    #-(or sbcl cmu)
    (let ((real-end (or end (length sequence))))
      (declare (type fixnum real-end))
      (update-sha1-state state sequence :start start :end real-end))
    (finalize-sha1-state state)))

(defconstant +buffer-size+ (* 128 1024)
  "Size of internal buffer to use for sha1sum-stream and sha1sum-file
operations.  This should be a multiple of 64, the SHA1 block size.")

(deftype buffer-index () `(integer 0 ,+buffer-size+))

(defun sha1sum-stream (stream)
  "Calculate an SHA1 message-digest of the contents of stream.  Its
element-type has to be either (unsigned-byte 8) or character."
  (declare (optimize (speed 3) (space 0) (debug 0)))
  (let ((state (make-sha1-state)))
    (declare (type sha1-state state))
    (cond
      ((equal (stream-element-type stream) '(unsigned-byte 8))
       (let ((buffer (make-array +buffer-size+
				 :element-type '(unsigned-byte 8))))
	 (declare (type (simple-array (unsigned-byte 8) (#.+buffer-size+))
			buffer))
	 (loop for bytes of-type buffer-index = (read-sequence buffer stream)
	       do (update-sha1-state state buffer :end bytes)
	       until (< bytes +buffer-size+)
	       finally
	       (return (finalize-sha1-state state)))))
      ((equal (stream-element-type stream) 'character)
       (let ((buffer (make-string +buffer-size+)))
	 (declare (type (simple-string #.+buffer-size+) buffer))
	 (loop for bytes of-type buffer-index = (read-sequence buffer stream)
	       do (update-sha1-state state buffer :end bytes)
	       until (< bytes +buffer-size+)
	       finally
	       (return (finalize-sha1-state state)))))
      (t
       (error "Unsupported stream element-type ~S for stream ~S."
	      (stream-element-type stream) stream)))))

(defun sha1sum-file (pathname)
  "Calculate the SHA1 message-digest of the file specified by pathname."
  (declare (optimize (speed 3) (space 0) (debug 0)))
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (sha1sum-stream stream)))

#+sha1-testing
(defconstant +rfc3174-testsuite+
  '(("abc" 1 "a9993e364706816aba3e25717850c26c9cd0d89d")
    ("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" 1
     "84983e441c3bd26ebaae4aa1f95129e5e54670f1")
    ("a" 1000000 "34aa973cd4c4daa4f61eeb2bdbad27316534016f")
    ("0123456701234567012345670123456701234567012345670123456701234567" 10
     "dea356a2cddd90c7a7ecedc5ebb563934f460452"))
  "AList of test input strings, repeat amounts, and stringified
message-digests according to the test suite in section 7.3 of RFC 3174.")

#+sha1-testing
(defun test-rfc3174 ()
  (let ((failed 0))
    (loop for count from 1
          for (source repeat sha1-string) in +rfc3174-testsuite+
          do (let* ((source-length (length source))
                    (real-sequence (make-sequence 'string (* repeat source-length))))
               (dotimes (i repeat)
                 (dotimes (j source-length)
                   (setf (aref real-sequence (+ (* i source-length) j))
                         (aref source j))))
               (let* ((sha1-digest (sha1sum-sequence real-sequence))
                      (sha1-result-string (format nil "~(~{~2,'0X~}~)"
                                                  (map 'list #'identity sha1-digest))))
                 (format *trace-output*
                         "~2&Test-Case ~D:~%  Input: ~S~%  Required: ~A~%  Returned: ~A~%"
                         count source sha1-string sha1-result-string)
                 (if (string= sha1-string sha1-result-string)
                     (format *trace-output* "  OK~%")
                     (progn
                       (incf failed)
                       (format *trace-output* "  FAILED~%")))))
          finally
          (format *trace-output*
                  "~2&~[All ~D test cases succeeded~:;~:*~D of ~D test cases failed~].~%"
                  failed (1- count))
          (return (zerop failed)))))

#+cmu
(eval-when (:compile-toplevel :execute)
  (setq *features* *old-features*))

#+cmu
(eval-when (:compile-toplevel)
  (setq ext:*inline-expansion-limit* *old-expansion-limit*))

#+sbcl
(eval-when (:compile-toplevel)
  (setq *features* *old-features*))