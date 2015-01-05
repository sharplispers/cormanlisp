;;;-*- Mode: Lisp; Package: (FFC) -*-

;; Foreign function compatibility module for MCL, LWW and ACL (ACL version)
;; Version 0.85
;; Copyright (C) Paul Meurer 1999. All rights reserved.
;; paul.meurer@hit.uib.no
;;
;; Documentation and the license agreement can be found in file 
;; "sql-odbc-documentation.lisp".
;; Bug reports and suggestions are highly welcome.

;; In this file the platform specific code is isolated.
;; The code in this file consists mostly of wrapper functions and macros 
;; around the platform-dependent foreign function interface.

;; This file contains Corman Lisp (Version 1.4) specific code

(defpackage "FFC"
  (:use "COMMON-LISP" "WIN32")
  (:export "*FOREIGN-MODULE*" "DEFINE-FOREIGN-FUNCTION" 
    "MAKE-RECORD"
    "%WITH-TEMPORARY-ALLOCATION" "%WITH-SQL-POINTER" "%GET-CSTRING"
    "%CSTRING-INTO-STRING"
    "%CSTRING-INTO-VECTOR"
    "%GET-CSTRING-LENGTH" "WITH-CSTR" "%GET-PTR" "%NEW-PTR" "%DISPOSE-PTR"
    "%GET-SIGNED-WORD"
    "%GET-UNSIGNED-LONG"
    "%GET-SIGNED-LONG"
    "%GET-SINGLE-FLOAT"
    "%GET-DOUBLE-FLOAT"
    "%GET-WORD"
    "%GET-LONG"
    "%GET-SHORT"
    "%GET-SIGNED-LONG"
    "%PUT-STR"
    "%PUT-WORD"
    "%PUT-SHORT"
    "%PUT-LONG"
    "%NEW-CSTRING" 
    "%NULL-PTR"
    "%PTR-EQL"
    "SHORT-TO-SIGNED-SHORT" ; #+allegro
    "STRING-PTR" "SQL-HANDLE" "SQL-HANDLE-PTR"
    "%GET-BINARY" "%PUT-BINARY" "%NEW-BINARY"))

(in-package :ffc)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *foreign-module* nil))

(eval-when (:load-toplevel :compile-toplevel :execute)
 (use-package :c-types))

(defun %get-cstring-length (ptr)
   (c-string-length ptr))


(defun %cstring-into-vector (ptr vector offset size-in-bytes)
  "Copy C string into Lisp vector."
   (dotimes (i size-in-bytes)
      (setf (aref vector offset)
            (code-char (cref (:unsigned-char *) ptr i)))
      (incf offset))
   offset)

(defun %cstring-into-string (ptr string offset size-in-bytes)
   (%cstring-into-vector ptr string offset size-in-bytes))

(defun %new-ptr (type &optional (bytecount 1))
	(let ((ptr (malloc (* (sizeof (canonical-to-corman-type type)) bytecount))))
		(loop for n from 0 below bytecount do
			(setf (cref (:unsigned-char *) ptr n) 0))
		ptr))

(defmacro %dispose-ptr (p)
   `(free ,p))

(defmacro %with-sql-pointer ((pointer-var) &body body)
  `(let ((,pointer-var (malloc 4)))
		(setf (cref (:unsigned-long *) ,pointer-var 0) 0)
     ,@body))

(defun allocate-dynamic-string (size)
   (let ((str (coerce (make-array size :initial-element #\Space) 'string)))
      (lisp-string-to-c-string str)))

(defmacro %new-cstring (size)
   `(allocate-dynamic-string ,size))

(defmacro %with-temporary-allocation (bindings &body body)
  (let ((simple-types ())
        (strings ())
        (free-strings ())) 
    (dolist (binding bindings)
      (case (cadr binding)
        (:string 
          (push (list (car binding)
                  (list 'allocate-dynamic-string (caddr binding))) strings)
          (push (list 'free (car binding)) free-strings))
        (:ptr (push (list (car binding) 
							(list '%new-ptr ':ptr)) simple-types))
        (otherwise (push (list (car binding)
							(list '%new-ptr (cadr binding))) simple-types))))
    `(let ,simple-types
       (let ,strings
          (unwind-protect
           (progn ,@body)
           ,@free-strings)))))

(defmacro with-cstr ((ptr str) &body body)
   `(let ((,ptr (lisp-string-to-c-string ,str)))
       (unwind-protect
        (progn ,@body)
        (free ,ptr))))

(defun %null-ptr ()
  null)

(defmacro %ptr-eql (ptr1 ptr2)
  `(cpointer= ,ptr1 ,ptr2)) 

(defun %get-ptr (ptr)
	(int-to-foreign-ptr (ct:cref (:unsigned-long *) ptr 0)))

(defun %get-short (ptr)
  (cref (:short *) ptr 0))

(defun %get-long (ptr)
  (cref (:long *) ptr 0))

(defmacro %put-long (ptr long) 
  `(setf (cref (:long *) ,ptr 0) ,long))

(defun %get-signed-word (ptr)
  (cref (:short *) ptr 0))

(defun %get-word (ptr)
  (cref (:unsigned-short *) ptr 0))

(defmacro %put-word (ptr word) 
  `(setf (cref (:short *) ,ptr 0) ,word))

(defun %get-unsigned-long (ptr)
  (cref (:unsigned-long *) ptr 0))

(defmacro %get-signed-long (ptr)
  `(cref (:long *) ,ptr 0))

(defmacro %get-single-float (ptr)
  `(cref (:single-float *) ,ptr 0))

(defmacro %get-double-float (ptr)
  `(cref (:double-float *) ,ptr 0))

(defmacro %get-cstring (ptr &optional (start 0))
  `(c-string-to-lisp-string (int-to-foreign-ptr 
			(+ (foreign-ptr-to-int ,ptr) ,start))))

(defmacro %put-str (ptr string &optional max-length)
	(declare (ignore max-length))
  `(lisp-string-to-c-string ,string ,ptr))

(defun %new-binary (bytecount)
   (malloc bytecount))

(defun %get-binary (ptr len format)
  "FORMAT is one of :unsigned-byte-vector, :bit-vector (:string, :hex-string)"
  (ecase format
    (:unsigned-byte-vector
     (let ((vector (make-array len :element-type 'unsigned-byte)))
       (dotimes (i len)
         (setf (svref vector i)
           (cref (:unsigned-char *) ptr i)))
       vector))
    #-cormanlisp (:bit-vector
     (let ((vector (make-array (ash len 3) :element-type 'bit)))
       (dotimes (i len)
         (let ((byte (cref (:unsigned-char *) ptr i :unsigned-byte)))
           (dotimes (j 8)
             (setf (bit vector (+ (ash i 3) j)) (logand (ash byte (- j 7)) 1)))))
       vector))))

;; returns size in bytes
(defun %put-binary (ptr vector &optional max-length)
  (cond ((bit-vector-p vector)
         (let* ((bit-count (length vector))
                (byte-count (print (ceiling bit-count 8))))
           (when (and max-length (> byte-count max-length))
             (error "bit vector of length ~d is longer than max-length: ~d"
                    bit-count (* max-length 4)))
           (dotimes (i byte-count)
             (let ((byte 0))
               (dotimes (j 8)
                 (let ((index (+ (ash i 3) j)))
                   (if (< index bit-count)
                       (setf byte (logior byte (ash (bit vector (+ (ash i 3) j)) (- 7 j))))
                     (return))))
               (setf (cref (:unsigned-char *) ptr i) byte)))
           byte-count))
        (t (error "not yet implemented"))))
 
(defmacro make-record (type)
  `(malloc (sizeof (canonical-to-corman-type ',type))))

#+ignore
(defmacro defcstruct-make (name &rest other)
  `(progn
     (ct:defcstruct ,name ,@other)
     (defmacro ,(intern (format nil "~a-~s" :make name))
	 ()
       `(ct:callocate ,',name))))

(defctype sql-handle (:VOID *))
(defctype sql-handle-ptr (sql-handle *))
(defctype string-ptr (:char *))

(defun c-to-lisp-type (c-type)
   (ecase c-type
     ((:ptr sql-handle sql-handle-ptr) t)
     (string-ptr 'string)
     ((:word :short :signed-short :long) 'fixnum)))

(defun canonical-to-corman-type (type)
   (case type
     (:signed-short :short)
	 (:double :double-float)
     ;;(string-ptr :long) ; ***
     (:ptr '(:void *))
     (otherwise type)))

(defmacro define-foreign-function (c-name args result-type &key documentation module)
  (declare (ignore documentation))
  (let ((name (intern (string-upcase c-name))))
	`(defwinapi 
    	,name
       	,args
		:entry-name ,c-name
    	:return-type ,(if (eq result-type :signed-short) :short result-type)
		:library-name (or ,module "odbc32.dll")
		:linkage-type :pascal)))

