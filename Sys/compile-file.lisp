;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		compile-file.lisp
;;;;	Contents:	Common Lisp COMPILE-FILE implementation.
;;;;	History:	6/4/98   RGC  Created.
;;;;				10/12/99 RGC  Added compression, finished implementation.
;;;;							  Note the file format is now changed from
;;;;							  previous versions.
;;;;				02/18/01 RGC  Added *LOAD-PATHNAME*, *LOAD-TRUENAME* variables.
;;;;							  These are bound during LOAD.
;;;;				04/27/01 RGC  Modified COMPILE-FILE to allow input file to be a pathname.
;;;;				05/13/01 RGC  Added *COMPILE-FILE-PATHNAME*, *COMPILE-FILE-TRUENAME* variables.
;;;;							  These are bound during COMPILE-FILE.
;;;;				05/31/02 RGC  Added COMPILE-DLL functionality.
;;;;                12/19/02 RGC  Incorporated JP Massar's fixes to LOAD of 9/28/01.
;;;;                              Added (partial) implementation of WITH-COMPILATION-UNIT.
;;;;                              Incorporated JP Massar's fix for COMPILE-FILE and PROGN
;;;;                              top-level forms.
;;;;                9/19/03  RGC  Added COMPILE-FILE-PATHNAME implementation.
;;;;                8/21/06  RGC  Integrated Espen Wiborg's slime patches: literal packages
;;;;                              can be handled in FASL code.
;;;;                6/27/08  RGC  Enhanced LOAD to allow the extension to be unspecified. 
;;;;                              In this case it assume ".lisp" unless there is a newer 
;;;;                              file with the extension ".fasl".
;;;;                11/11/16  Artem Boldarev
;;;;                              Both COMPILE-FILE and LOAD support ".lisp", ".lsp" and ".cl" extensions.
;;;;                              LOAD looks for a file in the installation directory as the last resort (for comatibility with older versions).
;;;;
;;;;
;;;; JPM 9/28/01
;;;; Modifications (LOAD):
;;;
;;; 1.  Added *load-print* variable.
;;; 2.  Bound *readtable* as per hyperspec.
;;; 3.  Made default for VERBOSE, PRINT, IF-DOES-NOT-EXIST and EXTERNAL-FORMAT
;;;     conform to hyperspec.
;;; 4.  Made code signal return NIL if file does not exist and 
;;;     IF-DOES-NOT-EXIST is NIL, as per hyperspec.
;;; 5.  Make code print out a ';; Loading ...' message if VERBOSE is true, 
;;;     as per hyperspec.
;;; 6.  Made code print out evaluated forms using PRINT argument, not 
;;;     *LOAD-VERBOSE*, as per hyperspec recommendation.
;;; 7.  Created the auxiliary function process-top-level-form-for-load,
;;;     to actually deal with each form LOAD reads.
;;; 8.  Made process-top-level-form-for-load apply EVAL to each form of 
;;;     a PROGN, instead of just calling EVAL on the PROGN form.   
;;;    (See compile-file-patch.lisp for example of problem)
;;; 9.  Made process-top-level-form-for-load macroexpand macros, instead of
;;;     just calling EVAL on the macro.

;;; Problems:  
;;; -- Doesn't deal with LOCALLY, MACROLET, or SYMBOL-MACROLET
;;; as per Hyperspec Section 3.2.3.1 Processing of Top Level Forms.
;;; -- OPEN doesn't work correctly; it doesn't handle :if-does-not-exist nil,
;;; so LOAD is still always going to fail if the file isn't found


(in-package :common-lisp)

;;; Common Lisp *LOAD-PATHNAME* variable
(defvar *load-pathname* nil)

;;; Common Lisp *LOAD-TRUENAME* variable
(defvar *load-truename* nil)

;;; Common Lisp *COMPILE-FILE-PATHNAME* variable
(defvar *compile-file-pathname* nil)

;;; Common Lisp *COMPILE-FILE-TRUENAME* variable
(defvar *compile-file-truename* nil)

;; need to override warning here
(setq cl::*COMPILER-WARN-ON-UNDEFINED-FUNCTION* nil)

(defconstant magic-header-id (map 'list #'char-int "COCL"))
(defconstant fasl-header-size 128)
(defconstant forward-tag #x09)
(defconstant hashed-tag  #x11)

(defvar *compiled-objects-hash-table* nil)
(defvar *hashed-objects-index* nil)
(defvar *preloaded-objects-table* nil)
(defvar *compress-fasl-files* t)

(defvar *fasl-file-read-address* nil "Address of mapped file we are loading")
(defvar *fasl-file-read-index* 0 "Index into mapped file")
(defvar *fasl-file-read-length* -1 "Length mapped file")
(defvar *fasl-eof-value* (cons 'eof nil))
(defvar *fasl-output-buffer* nil)
(defvar *fasl-compression* nil)

(defconstant uvector-header-tag 6)
(defconstant uvector-class-tag  31)
(defconstant symbol-header (+ (ash uvector-symbol-tag 3) uvector-header-tag))
(defconstant class-header  (+ (ash uvector-class-tag 3)  uvector-header-tag))
(defconstant package-header (+ (ash uvector-package-tag 3) uvector-header-tag))

(defconstant lisp-file-extension "lisp")
(defconstant fasl-file-extension "fasl")
(defconstant dll-file-extension  "dll" )

(defun get-next-byte () 
	(let ((i *fasl-file-read-index*))
		(if (>= i *fasl-file-read-length*)
			(return-from get-next-byte *fasl-eof-value*))
		(let ((result (ct:cref (:unsigned-char *) *fasl-file-read-address* i)))
			(setf *fasl-file-read-index* (+ i 1))
			result)))

(defun peek-next-byte () 
	(let ((i *fasl-file-read-index*))
		(if (>= i *fasl-file-read-length*)
			(return-from peek-next-byte *fasl-eof-value*))
		(ct:cref (:unsigned-char *) *fasl-file-read-address* i)))

(defun get-next-dword () 
	(let ((i *fasl-file-read-index*))
		(if (>= i *fasl-file-read-length*)
			(return-from get-next-dword *fasl-eof-value*))
		(let ((result (ct:cref (:unsigned-long *) *fasl-file-read-address* (ash i -2))))
			(setf *fasl-file-read-index* (+ i 4))
			result)))

(defun skip-read-bytes (n) (incf *fasl-file-read-index* n))

(defun put-byte (b) (vector-push-extend b *fasl-output-buffer*))

(ccl:defasm create-tagged-cell-from-fixnum (fixnum)
	{
		push	ebp
		mov		ebp, esp
		mov		eax, [ebp + ARGS_OFFSET]
		shr		eax, 3
		pop		ebp
		ret
	})

(ccl:defasm create-tagged-cell-from-bignum (bignum)
	{
		push	ebp
		mov		ebp, esp
		mov		eax, [ebp + ARGS_OFFSET]
		mov		eax, [eax + (- 8 uvector-tag)]
		pop		ebp
		ret
	})

(ccl:defasm get-embedded-unsigned-word (uvector byte-offset)
	{
		push	ebp
		mov		ebp, esp
		begin-atomic
		mov		edx, [ebp + ARGS_OFFSET]	;; edx = byte-offset
		shr		edx, 3
		mov		ecx, [ebp + (+ ARGS_OFFSET 4)]	;; eax = uvec
		xor		eax, eax
		mov		ax, [ecx + edx - 5]
		shl		eax, 3
		mov		ecx, 1
		end-atomic
		pop		ebp
		ret
	})

(ccl:defasm set-embedded-unsigned-word (uvector byte-offset value)
	{
		push	ebp
		mov		ebp, esp
		begin-atomic
		mov		ecx, [ebp + ARGS_OFFSET]		;; ecx = value
		mov		edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = byte-offset
		shr		edx, 3
		mov		eax, [ebp + (+ ARGS_OFFSET 8)]	;; eax = uvec
		shl		ecx, 13
		shr		ecx, 16
		mov		[eax + edx - 5], cx
		mov		ecx, 1
		end-atomic
		pop		ebp
		ret
	})
		
(defun heap-object-p (obj) (or (uvectorp obj)(consp obj)))

(defconstant max-tagged-cells -1)

;; a table of the first non-tagged cell of each uvector type
(defconstant num-tagged-cells-table
	(vector
		3					;; 0. uvector-function-tag
		2					;; 1. uvector-kfunction-tag
		max-tagged-cells	;; 2. uvector-structure-tag
		2					;; 3. uvector-array-tag
		8					;; 4. uvector-symbol-tag
		max-tagged-cells	;; 5. uvector-stream-tag
		0					;; 6. uvector-double-float-tag
		max-tagged-cells	;; 7. uvector-package-tag
		max-tagged-cells	;; 8. uvector-hashtable-tag
		0					;; 9. uvector-foreign-tag
		4					;; 10.uvector-compiled-code-tag
		max-tagged-cells	;; 11.uvector-readtable-tag
		3					;; 12.uvector-complex-tag
		3					;; 13.uvector-ratio-tag
		0					;; 14.uvector-bignum-tag
		0					;; 15.uvector-foreign-heap-tag
		2					;; 16.uvector-weak-ptr-tag
		max-tagged-cells	;; 17.uvector-simple-vector-tag
		0					;; 18.uvector-simple-char-vector-tag
		0					;; 19.uvector-simple-byte-vector-tag
		0					;; 20.uvector-simple-short-vector-tag
		0					;; 21.uvector-simple-double-float-vector-tag
		0					;; 22.uvector-simple-bit-vector-tag
		0					;; 23.uvector-simple-single-float-vector-tag
		0					;; 24.uvector-single-float-tag
		max-tagged-cells	;; 25.CLOS instance
		0))					;; 26.foreign stack pointer

;; tagged cells are always in the range 0 .. num-tags
(defun num-tagged-cells (uvec)
	(let ((length (svref num-tagged-cells-table (ccl:uvector-type-tag uvec))))
		(if (= length max-tagged-cells) (ccl:uvector-length uvec) length)))
			
(defun write-binary-cell (cell)
	(let ((n (lisp-object-id cell)))
		(put-byte (logand n #xff))
		(put-byte (logand (ash n -8) #xff))
		(put-byte (logand (ash n -16) #xff))
		(put-byte (logand (ash n -24) #xff))))

(defun output-binary-cell (cell os)
	(let ((n (lisp-object-id cell)))
		(write-byte (logand n #xff) os)
		(write-byte (logand (ash n -8) #xff) os)
		(write-byte (logand (ash n -16) #xff) os)
		(write-byte (logand (ash n -24) #xff)  os)))

(defun create-tagged-cell-from-integer (n)
	(if (fixnump n)
		(create-tagged-cell-from-fixnum n)
		(create-tagged-cell-from-bignum n)))

(defun read-binary-cell ()
	(let ((value (get-next-dword)))
		(if (fixnump value)
			(create-tagged-cell-from-fixnum value)
			(create-tagged-cell-from-bignum value))))
		

(defun write-forward-cell ()
	(put-byte forward-tag)
	(put-byte 0)
	(put-byte 0)
	(put-byte 0 ))

(defun write-header (os)
	(let ((message (cl::copyright-notice-short)))

		;; output the magic header id
		(dolist (i magic-header-id)
			(write-byte i os))
	
		;; output the header length
		(output-binary-cell fasl-header-size os)
		
		;; output compression flag- 0 = none, 1 = compressed
		(output-binary-cell (if *compress-fasl-files* 1 0) os)
		
		;; output copyright message
		(dotimes (i (length message))
			(write-byte (char-int (char message i)) os))
		
		;; output the remainder of the header
		(dotimes (i (- fasl-header-size (length magic-header-id) (length message) 8))
			(write-byte 0 os))))	 

#|
(defun create-new-output-file (path)
	(let ((os (open path :direction :output :element-type 'integer)))
		(write-header os)
		os))
|#

(defun output-fasl-symbol (sym)
	(let* ((package (symbol-package sym))
		   (package-name (if package (package-name package)))
		   (symbol-name (symbol-name sym)))
		(write-binary-cell (uref sym 0))
		(unless package-name (setq package-name ""))
		(output-fasl-heap-object package-name)
		(output-fasl-heap-object symbol-name)))

(defun output-fasl-package (package)
  (let* ((package-name (package-name package)))
    (write-binary-cell (uref package 0))
    (output-fasl-heap-object package-name)))

;;;
;;; If we are writing a CLOS instance, handle class instances separately.
;;; We need to avoid trying to write and read class instances. Rather we just
;;; save the name of the class, and at load time call (find-class class-name)
;;; to resolve the class instance. If the CLOS instance is not a class, then just
;;; write the instance using the normal heap writing function.
;;;
(defun output-fasl-clos-instance (obj)
    (if (cl::standard-class-p obj)
        (progn
            (write-binary-cell class-header)
            (output-fasl-heap-object (class-name obj)))     ;; writes the symbol
        (output-fasl-uvector obj)))

(defun write-prehashed-cell (num)
	(put-byte hashed-tag)
	(put-byte (logand num #xff))
	(put-byte (logand (ash num -8) #xff))
	(put-byte (logand (ash num -16) #xff)))	
	
(defun output-fasl-heap-object (obj)
	;; see if we have already written this object
	(let ((v (gethash obj *compiled-objects-hash-table*)))
		(when (integerp v)
			(write-prehashed-cell v)
			(return-from output-fasl-heap-object)))
	(if (hash-table-p obj)
		(setf (hash-table-rehash-needed obj) 2))	;; make sure a loaded hash table gets rehashed
	(setf (gethash obj *compiled-objects-hash-table*) *hashed-objects-index*)
	(incf *hashed-objects-index*)
	(cond
		((consp obj)(output-fasl-cons obj))
		((symbolp obj)(output-fasl-symbol obj))
		((cl::CLOS-INSTANCE-P obj)(output-fasl-clos-instance obj))
        ((packagep obj)
         (output-fasl-package obj))
        (t 
      #|    (if *compile-verbose*
                (unless (or (stringp obj)(cl::compiled-code-p obj)(vectorp obj)(functionp obj))
                    (format t "Writing literal object ~A~%" obj))) |#
            (output-fasl-uvector obj))))

(defun output-fasl-uvector (uvec)
    (if (cl::foreign-heap-p uvec)
        (error "Cannot COMPILE-FILE on function that has a literal foreign heap block"))
 ;   (if (and (cl::foreignp uvec) (not (ct:cpointer-null uvec)))
 ;       (error "Cannot COMPILE-FILE on function that has a literal (non-null) foreign pointer"))
    (let ((embedded-objects nil)
          (tagged-cells (num-tagged-cells uvec)))
		(dotimes (i (ccl:uvector-length uvec))
			(let ((cell (uref uvec i)))
				(if (and (< i tagged-cells) (heap-object-p cell))
					(progn
						(push cell embedded-objects)
						(write-forward-cell))
					(write-binary-cell cell))))
		(setq embedded-objects (nreverse embedded-objects))

		;; now write out all the embedded objects
		(dolist (x embedded-objects)
			(output-fasl-heap-object x))))

(defun output-fasl-cons (cons)
	(if (heap-object-p (car cons))
		(write-forward-cell)
		(write-binary-cell (car cons)))
	(if (heap-object-p (cdr cons))
		(write-forward-cell)
		(write-binary-cell (cdr cons)))
	(if (heap-object-p (car cons))
		(output-fasl-heap-object (car cons)))
	(if (heap-object-p (cdr cons))
		(output-fasl-heap-object (cdr cons))))
	
(defun output-fasl-function (func)
	(output-fasl-uvector func))	

;; Returns the address of the mapped file (for calling unmap later).
(defun open-fasl-file (path)
	(let ((value nil)
		  compression
		  (exe-size nil)
	      (exe-position nil)
		  (ret ct:null))
		(if (file-is-executable path)
			(with-open-file (is path :direction :input :element-type 'unsigned-byte)
				(setf exe-size (win::open-read-exe is ".fasl"))
				(setf exe-position (file-position is))))
		(multiple-value-setq
			(*fasl-file-read-address*
			 *fasl-file-read-length*)
			(ccl:map-file (namestring path)))
		(setf ret *fasl-file-read-address*)
		(when exe-size 
			(setf *fasl-file-read-length* exe-size)
			(setf *fasl-file-read-address* 
				(ct:int-to-foreign-ptr 
					(+ (ct:foreign-ptr-to-int *fasl-file-read-address*) exe-position))))
		(unless *fasl-file-read-address*
			(error "Could not open requested file: ~A" path))
		(setf *fasl-file-read-index* 0)
		(dotimes (i 4) (push (get-next-byte) value))
		(unless (equal (nreverse value) magic-header-id)
			(error "Not a valid FASL file: ~A" path))
		(setq value (read-binary-cell))
		(unless (fixnump value)
			(error "Not a valid FASL file: ~A" path))
		(setq compression (read-binary-cell))
		(unless (fixnump compression)
			(error "Not a valid FASL file: ~A" path))
		(setf *fasl-compression* (if (= compression 0) nil t))
		(setf *fasl-file-read-index* value)		;; skip header
		ret))	  

(defun input-fasl-uvector ()
	(let* ((b0 (peek-next-byte))
		   (d0 (get-next-dword))
		   (len (ash d0 -8))
		   (embedded-objects nil)
		   (uvec (alloc-uvector 
					(- (* len 2) 1)
					(logand (ash b0 -3) #x1f)))
		   (tagged-cells (num-tagged-cells uvec))
		   value)
		(do* ((i 1 (+ i 1)))
			((= i (ccl:uvector-length uvec)))
			(let* ((b0 (peek-next-byte)))
				(if (and (= b0 forward-tag)(< i tagged-cells))
					(progn
						(skip-read-bytes 4)
						(push i embedded-objects))
					(progn
						(setf value (get-next-dword))
						(setf (uref uvec i)
							(create-tagged-cell-from-integer value))))))
		(vector-push-extend uvec *preloaded-objects-table*)
		(setq embedded-objects (nreverse embedded-objects))
		(dolist (x embedded-objects)
			(setf (uref uvec x) (input-fasl-heap-object t)))
		uvec))

(defun input-fasl-preloaded-object ()
	(let* ((d0 (get-next-dword)))
		(elt *preloaded-objects-table* (ash d0 -8))))

(pl:defasm store-code-reference (cb pos ref)
	{
		push	ebp
		mov		ebp, esp
		mov		eax, [ebp + (+ ARGS_OFFSET 8)]	;; eax = cb
		mov		ecx, [ebp + (+ ARGS_OFFSET 4)]	;; ecx = pos
		shr		ecx, 3
		mov		edx, [ebp + ARGS_OFFSET]		;; edx = ref		
		add		ecx, (- (* 4 cl::compiled-code-code-offset) uvector-tag)
		mov		[eax + ecx], edx
		mov		ecx, 1
		pop		ebp
		ret
	})

(pl:defasm store-fixnum (cb pos num)
	{
		push	ebp
		mov		ebp, esp
		mov		eax, [ebp + (+ ARGS_OFFSET 8)]	;; eax = cb
		mov		ecx, [ebp + (+ ARGS_OFFSET 4)]	;; ecx = pos
		shr		ecx, 3
		mov		edx, [ebp + ARGS_OFFSET]		;; edx = num		
		add		ecx, (- (* 4 cl::compiled-code-code-offset) uvector-tag)
		shr		edx, 3
		mov		[eax + ecx], edx
		mov		ecx, 1
		pop		ebp
		ret
	})

(defun ensure-jump-table-entry (symbol)
	(if (zerop (uref symbol cl::symbol-jump-table-offset))
		(cl::%create-func-table-entry symbol))
	(uref symbol cl::symbol-jump-table-offset))

(defun ensure-var-table-entry (symbol)
	(if (zerop (uref symbol cl::symbol-var-table-offset))
		(cl::%create-var-table-entry symbol))
	(uref symbol cl::symbol-var-table-offset))

(defun update-code-references (obj)
	(let* ((refs (uref obj cl::compiled-code-references-offset)))
		(if (and (integerp refs)(not (zerop refs)))
			(error "A function was written to a file which was not compiled properly: ~A" obj))
		(let ((numrefs (if (and (integerp refs)(zerop refs)) 0 (/ (length refs) 2))))
			(dotimes (i numrefs)
				(let ((ref (elt refs (* i 2)))
					  (pos (elt refs (+ 1 (* i 2)))))
					(store-code-reference obj pos ref)))))
	(let* ((info (uref obj cl::compiled-code-info-offset))
		   (env-refs (getf info 'cl::*code-env-table-refs*))
		   (jump-refs (getf info 'cl::*code-jump-table-refs*))
		   (var-refs (getf info 'cl::*code-var-table-refs*))
           (load-time-values (getf info 'cl::*load-time-values*))
		   (num-env-refs (/ (length env-refs) 2))
		   (num-jump-refs (/ (length jump-refs) 2))
		   (num-var-refs (/ (length var-refs) 2)))
                          
		(dotimes (i num-env-refs)
			(let ((ref (* 4 (ensure-jump-table-entry (elt env-refs (* i 2)))))
				  (pos (elt env-refs (+ 1 (* i 2)))))
				(store-fixnum obj pos ref)))
		(dotimes (i num-jump-refs)
			(let ((ref (+ 4 (* 4 (ensure-jump-table-entry (elt jump-refs (* i 2))))))
				  (pos (elt jump-refs (+ 1 (* i 2)))))
				(store-fixnum obj pos ref)))
		(dotimes (i num-var-refs)
			(let ((ref (* 4 (ensure-var-table-entry (elt var-refs (* i 2)))))
				  (pos (elt var-refs (+ 1 (* i 2)))))
				(store-fixnum obj pos ref)))
                ;; evaluate load-time-values
        (if load-time-values
            (do* ((p load-time-values (cddr p))
                  (expr (car p)(car p))
                  (pos (cadr p)(cadr p)))
                ((null p))
                (store-code-reference obj pos (eval expr))))))
	
(defun input-fasl-heap-object (eof-error-p)
	(let* ((byte (peek-next-byte))
		   (tag)
		   (object nil))
		(if (eq byte *fasl-eof-value*)
			(if eof-error-p 
				(error "EOF encountered where a form was expected")
				(return-from input-fasl-heap-object *fasl-eof-value*)))
		(setq tag (logand byte 7))
		(setq object
			(cond
				((= byte hashed-tag)(return-from input-fasl-heap-object (input-fasl-preloaded-object)))
				((/= tag uvector-header-tag)(input-fasl-cons))
				((= byte symbol-header)(input-fasl-symbol))
                ((= byte class-header) (input-fasl-class))
                ((= byte package-header) (input-fasl-package))
   				(t (input-fasl-uvector))))
		(if (compiled-code-p object)
			(update-code-references object))
		object))	

(defun input-fasl-cons ()
	(let* ((d0 (get-next-dword))
		   (d1 (get-next-dword))
		   (c (cons 0 0)))
		(vector-push-extend c *preloaded-objects-table*)
		(if (eq d0 forward-tag)
			(setf (car c)(input-fasl-heap-object t))
			(setf (car c)(create-tagged-cell-from-integer d0)))
		(if (eq d1 forward-tag)
			(setf (cdr c)(input-fasl-heap-object t))
			(setf (cdr c)(create-tagged-cell-from-integer d1)))		
		c))

(defun input-fasl-symbol ()
	;; ignore the first four bytes
	(skip-read-bytes 4)
	(let ((save-pos (fill-pointer *preloaded-objects-table*)))
		(vector-push-extend nil *preloaded-objects-table*)
		(let* ((package-name (input-fasl-heap-object t))
		   	(sym-name (input-fasl-heap-object t)))
			(setf (elt *preloaded-objects-table* save-pos)
				(if (string= package-name "")
					(make-symbol sym-name)
					(intern sym-name package-name))))))

(defun input-fasl-class ()
    (skip-read-bytes 4)     ;; ignore the first four bytes
    (let ((save-pos (fill-pointer *preloaded-objects-table*)))
		(vector-push-extend nil *preloaded-objects-table*)
		(let* ((class-name (input-fasl-heap-object t)))
			(setf (elt *preloaded-objects-table* save-pos) (find-class class-name)))))

(defun input-fasl-package ()
  (skip-read-bytes 4)
  (let ((save-pos (fill-pointer *preloaded-objects-table*)))
    (vector-push-extend nil *preloaded-objects-table*)
    (let* ((package-name (input-fasl-heap-object t)))
      (setf (elt *preloaded-objects-table* save-pos) (find-package package-name)))))
    
(defun file-is-executable (path)
	(let ((ext (pathname-type path)))
		(or (string-equal ext "EXE") (string-equal ext "DLL"))))

(defparameter *form-verbose* nil)

(defun compile-file-toplevel-form (form)
    (flet ((do-it (form)
                (when *form-verbose* (format t "~%Form being compiled: ~A~%" form))
                (let ((compiled-form (compile-form form)))
                    (ct::unlink-all-dll-functions)  ;; rgc
                    (output-fasl-heap-object compiled-form)
                    (funcall compiled-form))))
        (if (and (consp form) (eq (car form) 'progn))
            (mapc (function do-it) (cdr form))
            (do-it form))))

(defun resolve-lisp-file-path (file)
  "Checks if supplied file exists with well-known Lisp files extensions. If the check fails this functions returns NIL."
  (let ((res))
	(if (and (pathname-type file)
			 (setq res (probe-file file)))
		res
		(or (probe-file (make-pathname :name file
									   :type "lisp"
									   :version nil))
			(probe-file (make-pathname :name file
									   :type "lsp"
									   :version nil))
			(probe-file (make-pathname :name file
									   :type "cl"
									   :version nil))))))

;;;
;;; Common Lisp COMPILE-FILE-PATHNAME function.
;;;
(defun compile-file-pathname (input-file &key output-file &allow-other-keys)
    (if output-file 
        (pathname output-file)
        (merge-pathnames (make-pathname :type cl::fasl-file-extension) (truename input-file))))
    			
;;;
;;;	Common Lisp COMPILE-FILE function.
;;;
(defun compile-file (input-file 
		&key (output-file nil)
			 (verbose *compile-verbose*)
			 (print *compile-print*)
			 (external-format :default))
    (declare (ignore external-format print))
    (let ((old-input-file input-file))
	  (setq input-file (or (resolve-load-path input-file)
					       ;; try to find file in the Corman Lisp installation directory.
					       (resolve-load-path (concatenate 'string
													       (cl::cormanlisp-directory)
													(if (stringp input-file)
														input-file
														(namestring input-file))))))
	  (unless input-file
	    (error "Can't resolve lisp file path ~S" old-input-file)))
	(setq output-file (compile-file-pathname input-file :output-file output-file))
				
	(let* ((*fasl-output-buffer* (make-array #x2000 :element-type 'byte :fill-pointer 0 :adjustable t))
		   (*compiled-objects-hash-table* (make-hash-table :test 'eq))
		   (*hashed-objects-index* 0)
		   (*package* *package*)
		   (*print-level* *print-level*)
		   (*read-level* *read-level*)
           (*compile-verbose* verbose)
           (*compile-print* print)
		   (pl::*source-file* (namestring input-file))
		   (pl::*source-line* nil)
		   (pl::*compiler-collect-jump-table-refs* t)
		   (pl::*compiler-collect-var-table-refs* t)
		   (cl::*compiler-save-lambdas* nil)
		   (cl::*append-refs-to-code* nil)
		   (cl::*COMPILER-WARN-ON-UNDEFINED-FUNCTION* nil)
		   (cl::*UNDEFINED-FUNCTIONS* t)
		   (*compile-file-pathname* (pathname (merge-pathnames (truename input-file))))
		   (*compile-file-truename* *compile-file-pathname*)
		   (count 0)
		   (eof-value (cons 'eof nil))
		   (warnings-p nil)
		   (failure-p nil))
		(declare (special pl::*compiler-collect-jump-table-refs*
						  pl::*compiler-collect-var-table-refs*
						  cl::*append-refs-to-code*
						  cl::*compiler-save-lambdas*
						  cl::*COMPILER-WARN-ON-UNDEFINED-FUNCTION*
						  cl::*UNDEFINED-FUNCTIONS*))
		(when verbose
			(format t "Compiling file ~A~%" input-file)
			(format t "Producing  output file ~A~%" output-file))

		(with-open-file (is input-file :direction :input)
			(do ((x 
					(progn 
						(setq ccl::*source-line* nil)
						(read is nil eof-value nil)) 
					(progn 
						(setq ccl::*source-line* nil)
						(read is nil eof-value nil)))) 
				((eq x eof-value))
				(incf count)
                (compile-file-toplevel-form  x))
			(if *compress-fasl-files*
				(setf *fasl-output-buffer* (cl::compress-bytes *fasl-output-buffer*)))
			(if (file-is-executable output-file)
				(let* ((len (length *fasl-output-buffer*))
					   (buf (make-array (+ len fasl-header-size) :element-type 'byte)))
					;; copy the output byte to a new buffer, with room for the header			
					(dotimes (i len)
						(setf (aref buf (+ i fasl-header-size)) (aref *fasl-output-buffer* i)))
					(win:write-exe-section output-file ".fasl" buf)
					(with-open-file (os output-file :direction :io :element-type 'unsigned-byte)
						(win::open-read-exe os ".fasl")
						(write-header os)))
				(with-open-file (os output-file :direction :output :element-type 'unsigned-byte)
					(write-header os)					
					(dotimes (i (length *fasl-output-buffer*))
						(write-byte (elt *fasl-output-buffer* i) os)))))
	
		(unless (eq cl::*UNDEFINED-FUNCTIONS* t)
			(let ((undefined '()))
				(do* ((x cl::*UNDEFINED-FUNCTIONS* (cddr x)))
					((not (consp x)))
					(if (or (not (fboundp (car x)))(eq (symbol-function (car x)) (cadr x)))
						(push (car x) undefined)))
				(when undefined
					(format *error-output* ";;; Warning: the following function(s) are called from forms~%~
											;;; in file \"~A\" but were not defined:~%" input-file)
					(dolist (s undefined)
						(format *error-output* ";;;     ~S~%" s)))))		
		(when verbose
			(format t "~A forms were compiled~%" count))
		(values output-file warnings-p failure-p)))

(defun offset-foreign-ptr (p offset) 
	(ct:int-to-foreign-ptr (+ (ct:cpointer-value p) offset)))

(defun load-fasl-file (input-file &key 
			(verbose nil) 
			(print nil))
	(let* ((*preloaded-objects-table* (make-array 1000 :fill-pointer 0 :adjustable t))
		   (*hashed-objects-index* 0)
		   (*package* *package*)
		   (*print-level* *print-level*)
		   (*read-level* *read-level*)
		   (*fasl-compression* nil)
		   (*fasl-file-read-address* nil)
		   (*fasl-file-read-index* nil)
		   (*fasl-file-read-length* nil)
		   (cl::*COMPILER-WARN-ON-UNDEFINED-FUNCTION* nil)
		   (cl::*UNDEFINED-FUNCTIONS* t)
		   (address (open-fasl-file input-file))
		   (count 0))
		(unwind-protect
			(progn
				(when *fasl-compression*
					(setf *fasl-file-read-address*
						(cl::uncompress-foreign-bytes 
							(offset-foreign-ptr *fasl-file-read-address* *fasl-file-read-index*)))
					(setf *fasl-file-read-index* 0)
					(setf *fasl-file-read-length* (uref *fasl-file-read-address* 2)))
				(do ((ret)
					 (x 
						(input-fasl-heap-object nil) 
						(input-fasl-heap-object nil)))
					((eq x *fasl-eof-value*))
					(incf count)
					(setq ret (funcall x))
					(when print
						(format t "~A~%" ret)))
				(when *fasl-compression* (ct:free *fasl-file-read-address*)))
			(ccl:unmap-file address))
		(unless (eq cl::*UNDEFINED-FUNCTIONS* t)
			(let ((undefined '()))
				(do* ((x cl::*UNDEFINED-FUNCTIONS* (cddr x)))
					((not (consp x)))
					(if (or (not (fboundp (car x)))(eq (symbol-function (car x)) (cadr x)))
						(push (car x) undefined)))
				(when undefined
					(format *error-output* ";;; Warning: the following function(s) are called from forms~%~
											;;; in file \"~A\" but have not yet been defined:~%" input-file)
					(dolist (s undefined)
						(format *error-output* ";;;     ~S~%" s))))) 
		(when verbose
			(format t "~A forms were loaded~%" count))
		count))

;;; utility functions
(defun function-compiled-code (func)
	(unless (pl::kernel-function-p func) 
		(uref func function-code-buffer-offset)))

(defun function-compiled-code-references (func)
	(let ((compiled-code (function-compiled-code func)))
		(if compiled-code 
			(uref compiled-code cl::compiled-code-references-offset))))

(defun path-is-fasl-file-name (path)
	(or (file-is-executable path)
		(string-equal (pathname-type path) fasl-file-extension)))

;;;;
;;;; Common Lisp *LOAD-PRINT* variable.
;;;;
(defparameter *load-print* nil)

(defun process-top-level-form-for-load (x load-print)
    (flet ((write-evaluated-form (x) (write x :stream *standard-output*) (terpri)))
        (cond
            ((and (listp x) (eq 'progn (first x)))
                (dolist (form (rest x)) (process-top-level-form-for-load form load-print)))
            ((and (listp x) (symbolp (first x)) (macro-function (first x)))
                (process-top-level-form-for-load (macroexpand-1 x) load-print))
            (t (if *load-without-eval*
                    (write-evaluated-form x)
                    (progn
                        (setq x (eval x))
                        (if load-print (write-evaluated-form x))))))))

;;;
;;; Common Lisp WITH-COMPILATION-UNIT macro.
;;; This is a trivial definition which probably needs some work.
;;;
(defmacro with-compilation-unit (args &body body)
    (unless (listp args)
        (error "First form of WITH-COMPILATION-UNIT must be a list"))
    `(progn ,@body))

;;;
;;;	Common Lisp LOAD function.
;;;
(setf ccl::*save-relative-source-file-names* t)   ;; after the image loads, this is turned off

(defun resolve-load-path (file)
  (let ((resolved-lisp-file-path (resolve-lisp-file-path file))
		(compiled-file-path (compile-file-pathname file)))
	(cond
	  ((and resolved-lisp-file-path
			(probe-file compiled-file-path)
			(< (file-write-date resolved-lisp-file-path)
			   (file-write-date compiled-file-path)))
	   compiled-file-path)
	  ((and (not resolved-lisp-file-path)
			(probe-file compiled-file-path))
	   compiled-file-path)
	  (t resolved-lisp-file-path))))

;;;;
;;;; Common Lisp LOAD function
;;;; Enhanced LOAD to allow the extension to be unspecified. In this case it 
;;;; assume ".lisp" unless there is a newer file with the extension ".fasl".
;;;;
(defun load (path &key 
                (verbose *load-verbose*) 
		        (print *load-print*) 
                (if-does-not-exist t) 
                (external-format :default))
   (declare (ignore external-format))
   (let ((old-path path))
	 (setf path (or (resolve-load-path path)
					;; try to find file in the Corman Lisp installation directory.
					(resolve-load-path (concatenate 'string
													(cl::cormanlisp-directory)
													(if (stringp path)
														path
														(namestring path))))))
	 (unless path
	   (error "Can't resolve load path for file ~S" old-path)))
	 (if (path-is-fasl-file-name path)
	   (return-from load (load-fasl-file path :verbose verbose :print print)))
	(let* ((*package* *package*)
		   (*print-level* *print-level*)
		   (*read-level* *read-level*)
		   (*load-pathname* (pathname (merge-pathnames (truename path))))
		   (*load-truename* *load-pathname*)
		   (cl::*COMPILER-WARN-ON-UNDEFINED-FUNCTION* nil)
		   (cl::*UNDEFINED-FUNCTIONS* t)
		   (ccl::*source-file* 
                (if ccl::*save-relative-source-file-names*
                    (namestring path)
                    (namestring *load-pathname*)))
		   (ccl::*source-line* nil)
		   (count 0)
		   (eof-value (cons 'eof nil)))
		(with-open-file (istream path 
                        :direction :input 
                        :if-does-not-exist (if if-does-not-exist :error nil))
            
            ;; If ISTREAM is null, it means the open failed but
            ;; the user specified :if-does-not-exist nil, which
            ;; means we just return NIL.
            (when (null istream) (return-from load nil))
            (when verbose 
                (format *standard-output* ";; Loading ~A~%" (namestring *load-pathname*)))
            (do ((x 
					(progn 
						(setq ccl::*source-line* nil)
						(read istream nil eof-value nil)) 
					(progn 
						(setq ccl::*source-line* nil)
						(read istream nil eof-value nil)))) 
				((eq x eof-value))
				(setq count (+ 1 count))
                (process-top-level-form-for-load x print)))
        (unless (eq cl::*UNDEFINED-FUNCTIONS* t)
			(let ((undefined '()))
				(do* ((x cl::*UNDEFINED-FUNCTIONS* (cddr x)))
					((not (consp x)))
					(if (or (not (fboundp (car x)))(eq (symbol-function (car x)) (cadr x)))
						(push (car x) undefined)))
				(when undefined
					(format *error-output* ";;; Warning: the following function(s) are called from forms~%~
											;;; in file \"~A\" but have not yet been defined:~%" path)
					(dolist (s undefined)
						(format *error-output* ";;;     ~S~%" s))))) 
		 count))

;;;
;;; Common Lisp WITH-COMPILATION-UNIT macro.
;;;
;;; This is a temporary implementation, to be replaced by a complete one at some point.
;;;
(defmacro with-compilation-unit (options &rest forms)
   (declare (ignore options))
   `(progn ,@forms))

(in-package :win32)

(defun u1 (stream) (read-byte stream))
(defun u2 (stream) (+ (read-byte stream) (* #x100 (read-byte stream))))
(defun u4 (stream) 
		(+  		 (read-byte stream) 
		(* #x100 	 (read-byte stream)) 
		(* #x10000   (read-byte stream)) 
		(* #x1000000 (read-byte stream))))

(defun o1 (byte stream) (write-byte byte stream))
(defun o2 (word stream) 
	(write-byte (logand word #xff) stream)
	(write-byte (logand (ash word -8) #xff) stream))
(defun o4 (dword stream) 
	(write-byte (logand dword #xff) stream)
	(write-byte (logand (ash dword -8) #xff) stream)
	(write-byte (logand (ash dword -16) #xff) stream)
	(write-byte (logand (ash dword -24) #xff) stream))

;;;
;;; Position a DLL or EXE file for appending a new section
;;;
(defconstant PAGE_SIZE 4096)
(defconstant SizeOfNtSignature 4)
(defconstant IMAGE_SIZEOF_SHORT_NAME 8)		;; max length of section name

;; Round up to a multiple of PAGE_SIZE
(defun page-bytes (size)
	(let ((pages (truncate (+ size (- PAGE_SIZE 1)) PAGE_SIZE)))
		(* pages PAGE_SIZE)))

(defun open-write-exe (os)
	(let* ((length (file-length os)))
		(file-position os length)))		; position at end of file

(defconstant file-alignment #x200)

;;; functions to find various locations within a PE image file
;;; They assume an open file, and will position the file at the
;;; requested spot.
;;;

(defun inc-file-position (stream num)
	(file-position stream (+ (file-position stream) num)))

(defun pe-dos-header (stream)
	(file-position stream 0))		;; DOS header at start of file

(defun pe-image-header (stream)
	(file-position stream (ct::offsetof 'IMAGE_DOS_HEADER 'e_lfanew))
	(let ((temp (u4 stream)))
		(file-position stream (+ temp SizeOfNtSignature))))

(defun pe-optional-header (stream)
	(pe-image-header stream)
	(inc-file-position stream (ct:sizeof 'IMAGE_FILE_HEADER)))

(defun pe-section-headers (stream)
	(pe-optional-header stream)
	(inc-file-position stream (ct:sizeof 'IMAGE_OPTIONAL_HEADER)))

(defun pe-export-directory-info (stream)
	(pe-optional-header stream)
	(inc-file-position stream (ct:offsetof 'IMAGE_OPTIONAL_HEADER 'DataDirectory)))

(defstruct export-info 
	name 
	ordinal 
	address)

(defstruct section-header
	name
	virtual-size
	rva
	size-of-raw-data
	pointer-to-raw-data
	pointer-to-relocations
	pointer-to-line-numbers
	number-of-relocations
	number-of-line-numbers
	characteristics)

(defstruct export-directory
	file-position
	rva
	characteristics
	time-date-stamp
	major-version
	minor-version
	name
	base
	number-of-functions
	number-of-names
	address-of-functions
	address-of-names
	address-of-name-ordinals)

;;; get all the section header information
(defun section-headers (stream)
	(let ((sections '()))
		(pe-image-header stream)
		(inc-file-position stream (ct:offsetof 'IMAGE_FILE_HEADER 'NumberOfSections))
		(let ((num-sections (u2 stream)))
			(pe-section-headers stream)
			(dotimes (i num-sections (nreverse sections))
				(let ((name (make-array 8 :element-type 'character :fill-pointer 0 :adjustable t)))
					(dotimes (i 8)
						(let ((byte (read-byte stream)))
							(unless (= byte 0)
								(vector-push (int-char byte) name))))
					(push
						(make-section-header
							:name name
							:virtual-size (u4 stream)
							:rva (u4 stream)
							:size-of-raw-data (u4 stream)
							:pointer-to-raw-data (u4 stream)
							:pointer-to-relocations (u4 stream)
							:pointer-to-line-numbers (u4 stream)
							:number-of-relocations (u2 stream)
							:number-of-line-numbers (u2 stream)
							:characteristics (u4 stream))
						sections))))))
										
		
;;;
;;; Returns the export directory information
(defun exported-directory (stream)
	(pe-export-directory-info stream)
	(let* ((rva (u4 stream))
		   (size (u4 stream)))
		(let ((section-headers (section-headers stream))
			  (section nil))
			(dolist (header section-headers)
				(when (and (>= rva (section-header-rva header))
						   (<= (+ rva size) 
							  (+ (section-header-rva header)(section-header-virtual-size header))))
					(setf section header)
					(return)))
			(when section
				(file-position stream 
					(+ (- rva (section-header-rva section)) 
						(section-header-pointer-to-raw-data section)))
				(make-export-directory
					:file-position (file-position stream)
					:rva rva
					:characteristics 			(u4 stream)
					:time-date-stamp 			(u4 stream)
					:major-version 				(u2 stream)
					:minor-version 				(u2 stream)
					:name 						(u4 stream)
					:base 						(u4 stream)
					:number-of-functions 		(u4 stream)
					:number-of-names 			(u4 stream)
					:address-of-functions 		(u4 stream)
					:address-of-names 			(u4 stream)
					:address-of-name-ordinals 	(u4 stream))))))

(defun exported-names (stream)
	(let* ((dir (exported-directory stream))
		   (offset (- (export-directory-rva dir) (export-directory-file-position dir)))
		   (num-funcs (export-directory-number-of-functions dir))
		   (num-names (export-directory-number-of-names dir))
		   (addresses (- (export-directory-address-of-functions dir) offset))
		   (names (- (export-directory-address-of-names dir) offset))
		   (ordinals (- (export-directory-address-of-name-ordinals dir) offset)))
		(let ((result '()))
			(file-position stream names)
			(dotimes (i num-names)
				(let* ((position (- (u4 stream) offset))
					   (save-position (file-position stream)))
					(file-position stream position)
					(let ((name (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t)))
						(do ()
							(nil)
							(let ((byte (read-byte stream)))
								(if (= byte 0)
									(return)
									(vector-push-extend (int-char byte) name))))
						(push (make-export-info :name name) result))
					(file-position stream save-position)))
			(setf result (nreverse result))
			(file-position stream ordinals)
			(dolist (x result)
				(setf (export-info-ordinal x) (+ (export-directory-base dir)(u2 stream))))
			(file-position stream addresses)
			(let ((address-vec (make-array num-funcs)))
				(dotimes (i num-funcs)
					(setf (aref address-vec i) (u4 stream)))	
				(dolist (x result)
					(setf (export-info-address x) 
						(aref address-vec (- (export-info-ordinal x) 
								(export-directory-base dir))))))
			result)))
		
(defun finish-exe (os section-name original-length attributes)
	(unless (and (stringp section-name) (< (length section-name) 8))
		(error "Invalid section name for executable file"))
	(let* ((pos (file-position os))
		   (extra (mod pos file-alignment))
		   (bytes-added (- pos original-length))
		   (padded-section bytes-added))
		(if (> extra 0)					; pad to page boundary
			(let ((pad (- file-alignment extra)))
				(dotimes (i pad)
					(write-byte 0 os))
				(incf padded-section pad)
				(incf pos pad)))
		(file-position os (ct::offsetof 'IMAGE_DOS_HEADER 'e_lfanew))
		(let* ((temp (u4 os))
			   (image-header-pos (+ temp SizeOfNtSignature))
			   (optional-header-pos (+ image-header-pos (ct:sizeof 'IMAGE_FILE_HEADER)))
			   (start-of-section-header-pos (+ optional-header-pos (ct:sizeof 'IMAGE_OPTIONAL_HEADER))))
			(file-position os (+ image-header-pos (ct:offsetof 'IMAGE_FILE_HEADER 'NumberOfSections)))
			(let* ((num-sections (u2 os))
				   (new-section-header-pos 
					(+ start-of-section-header-pos (* num-sections (ct:sizeof 'IMAGE_SECTION_HEADER)))))
				;; increment the number of sections
				(file-position os (+ image-header-pos (ct:offsetof 'IMAGE_FILE_HEADER 'NumberOfSections)))
				(incf num-sections)
				(write-byte (mod num-sections #x100) os)
				(write-byte (ash num-sections -8) os)
				(file-position os 
					(+ new-section-header-pos (- (ct:sizeof 'IMAGE_SECTION_HEADER))
						(ct:offsetof 'IMAGE_SECTION_HEADER 'Misc)))
				(let ((prev-virtual-size (u4 os))
					  (prev-virtual-address (u4 os))
					  (header-length (ct:sizeof 'IMAGE_SECTION_HEADER)))
					(file-position os new-section-header-pos)
					;; write out the header info for the new section				
					(ct:with-fresh-foreign-block (section-info 'IMAGE_SECTION_HEADER)
						(dotimes (i header-length)
							(setf (ct:cref (BYTE *) section-info i) 0))	;; clear all fields to zero
						(dotimes (i (length section-name))
							(setf (ct:cref (BYTE *) section-info i) (char-int (char section-name i))))
						(setf (ct:cref IMAGE_SECTION_HEADER section-info Misc) bytes-added)
						(setf (ct:cref IMAGE_SECTION_HEADER section-info VirtualAddress)
							(+ prev-virtual-address (page-bytes prev-virtual-size)))
						(setf (ct:cref IMAGE_SECTION_HEADER section-info SizeOfRawData) padded-section)
						(setf (ct:cref IMAGE_SECTION_HEADER section-info PointerToRawData) original-length)
						(setf (ct:cref IMAGE_SECTION_HEADER section-info Characteristics) attributes)
						(dotimes (i header-length)
							(write-byte (ct:cref (BYTE *) section-info i) os))))
				
				;; update size of image
				(file-position os (+ optional-header-pos (ct:offsetof 'IMAGE_OPTIONAL_HEADER 'SizeOfImage)))
				(let ((fsize (u4 os)))	;; original size
					(file-position os (+ optional-header-pos (ct:offsetof 'IMAGE_OPTIONAL_HEADER 'SizeOfImage)))
					(incf fsize (page-bytes padded-section))
					(write-byte (mod fsize #x100) os)
					(write-byte (mod (ash fsize -8) #x100) os)
					(write-byte (mod (ash fsize -16) #x100) os)
					(write-byte (mod (ash fsize -24) #x100) os))))))

(defwinconstant IMAGE_SCN_CNT_INITIALIZED_DATA 	#x00000040)
(defwinconstant IMAGE_SCN_MEM_SHARED 			#x10000000)
(defwinconstant IMAGE_SCN_MEM_EXECUTE 			#x20000000)
(defwinconstant IMAGE_SCN_MEM_READ	 			#x40000000)
(defwinconstant IMAGE_SCN_MEM_WRITE 			#x80000000)

(defun write-exe-section (path section-name bytes &optional 
				(attributes (logior IMAGE_SCN_CNT_INITIALIZED_DATA IMAGE_SCN_MEM_READ)))
	(with-open-file (os path :direction :io :element-type 'unsigned-byte)
		(open-write-exe os)
		(let ((position (file-position os)))
			(dotimes (i (length bytes))
				(write-byte (aref bytes i) os))
			(finish-exe os section-name position attributes))))

(defun open-read-exe (is section-name)
	(file-position is (ct::offsetof 'IMAGE_DOS_HEADER 'e_lfanew))
	(let* ((temp (u4 is))
		   (image-header-pos (+ temp SizeOfNtSignature))
		   (optional-header-pos (+ image-header-pos (ct:sizeof 'IMAGE_FILE_HEADER)))
		   (start-of-section-header-pos (+ optional-header-pos (ct:sizeof 'IMAGE_OPTIONAL_HEADER)))
		   (sname (make-array IMAGE_SIZEOF_SHORT_NAME :element-type 'byte :initial-element 0)))
		(dotimes (i (length section-name))
			(setf (aref sname i)(char-int (char section-name i))))
		
		(file-position is (+ image-header-pos (ct:offsetof 'IMAGE_FILE_HEADER 'NumberOfSections)))
		(let* ((num-sections (u2 is)))
			(block section-search
				(dotimes (i num-sections)
					(file-position is (+ start-of-section-header-pos (* i (ct:sizeof 'IMAGE_SECTION_HEADER))))
					(dotimes (j (length sname) (return-from section-search))
						(if (/= (read-byte is) (aref sname j))
							(return))))
				;; if we got here, we didn't find a matching section
				(return-from open-read-exe 0))
			;; we found the section
			(let* ((section-pos (- (file-position is) IMAGE_SIZEOF_SHORT_NAME)))
				(file-position is (+ section-pos (ct:offsetof 'IMAGE_SECTION_HEADER 'SizeOfRawData)))
				(let ((section-size (u4 is))
					  (section-pos  (u4 is)))
					(file-position is section-pos)
					section-size)))))
					
(defun read-exe-section (path section-name)
	(with-open-file (is path :direction :input :element-type 'unsigned-byte)
		(let ((size (open-read-exe is section-name)))
			(if (= size 0)
				nil
				(let ((bytes (make-array size :element-type 'byte)))
					(dotimes (i size)
						(setf (aref bytes i) (read-byte is)))
					bytes)))))

(defvar *export-buffer* nil)

(ct:defun-dll c_time ((time_ptr (:unsigned-long *)))
   :return-type :unsigned-long
   :library-name "msvcrt.dll"
   :entry-name "time"
   :linkage-type :c)

(defun w1 (byte) (vector-push-extend byte *export-buffer*))
(defun w2 (word) 
	(vector-push-extend (logand word #xff) 		 	*export-buffer*)
	(vector-push-extend (logand (ash word -8) #xff) 	*export-buffer*))
(defun w4 (dword) 
	(vector-push-extend (logand dword #xff) 			*export-buffer*)
	(vector-push-extend (logand (ash dword -8) #xff) 	*export-buffer*)
	(vector-push-extend (logand (ash dword -16) #xff) 	*export-buffer*)
	(vector-push-extend (logand (ash dword -24) #xff) 	*export-buffer*))

(defun wchars (string)
	(dotimes (i (length string))
		(w1 (char-int (char string i))))
	(w1 0))

(defun exported-symbol-name (symbol)
	(if (consp symbol)
		(second symbol)		; explicitly specified alternative name
		;; otherwise construct one
		(let* ((package-name (substitute #\_ #\- (package-name (symbol-package symbol))))
			   (name (substitute #\_ #\- (symbol-name symbol))))
			(concatenate 'string package-name "__" name))))

(defun update-export-table-address (path rva length)
	(with-open-file (stream path :direction :io :element-type 'unsigned-byte)
		(pe-export-directory-info stream)	;; move file position to export directory record
		(o4 rva stream)
		(o4 length stream)))

(defun update-time-stamp (path)
	(with-open-file (stream path :direction :io :element-type 'unsigned-byte)
		(pe-image-header stream)			;; move to image header
		(inc-file-position stream (ct:offsetof 'IMAGE_FILE_HEADER 'TimeDateStamp))
		(o4 (c_time ct:null) stream)))

(defun write-export-section (output-file exports template-file dll-name)
	(let* ((exported-names nil)
		   (section-headers nil)
		   (exported-directory nil)
		   (*export-buffer* (make-array 40 :element-type 'byte :adjustable t :fill-pointer 0))
		   (dll-main nil)
		   (num-exports (+ 1 (length exports)))
		   (address-table-offset 40)	;; right after the header
		   (name-pointer-table-offset (+ address-table-offset (* num-exports 4)))
		   (ordinal-table-offset (+ name-pointer-table-offset (* num-exports 4)))
		   (export-name-table-offset (+ ordinal-table-offset (* num-exports 2)))
		   (next-rva 0)
		   (last-section nil)
		   (new-exports (make-array (+ 1 (length exports))))
		   (exports-copy nil))
		(declare (ignore exported-directory))
		 
		;; collect all the PE data we need from the DLL
		(with-open-file (is template-file :direction :input :element-type 'unsigned-byte)
			(setf exported-names (exported-names is))
			(setf section-headers (section-headers is))
			(setf exported-directory (exported-directory is)))

		;; the last function should be DllMain
		;;(setf dll-main (find "DllMain" exported-names :key #'export-info-name :test #'string-equal))
		(setf dll-main (car (last exported-names)))

		(setf exports-copy (make-array (+ 1 (length exports))))
		(dotimes (i (length exports))
			(setf (aref exports-copy i) (exported-symbol-name (car (aref exports i)))))
		(setf (aref exports-copy (length exports)) "DllMain")
		(setf exports-copy (sort exports-copy #'string<)) 

		(setf last-section (car (last section-headers)))
		(setf next-rva 
			(page-bytes 
				(+ (section-header-rva last-section)
				   (section-header-virtual-size last-section))))

		;; write the export section header
		(w4 0)										; Export Flags: reserved
		(w4 (c_time ct:null))						; Time/Date Stamp
		(w2 0)										; Major Version
		(w2 0)										; Minor Version
		(w4 (+ export-name-table-offset next-rva))	; Name RVA
		(w4 1)										; Ordinal Base: 1
		(w4 num-exports)							; Address Table Entries
		(w4 num-exports)							; Number of Name Pointers
		(w4 (+ address-table-offset next-rva))		; Export Address Table RVA
		(w4 (+ name-pointer-table-offset next-rva))	; Name Pointer RVA
		(w4 (+ ordinal-table-offset next-rva))		; Ordinal Table RVA
		
		;; make sure we are not trying to export too many functions
		(if (>= (length exports) (length exported-names))
			(error "The number of functions exported by the DLL cannot be greater than ~A"
				(- (length exported-names) 1)))
		
		;; construct the new export list
		(let ((index 0))
			(dotimes (i (length exports-copy))
				(let* ((name (aref exports-copy i))
					   (is-main (string= name "DllMain")))
					(setf (aref new-exports i)
						(make-export-info 
							:name name 
							:address 
								(if is-main 
									(export-info-address dll-main)
									(export-info-address (nth index exported-names)))
							:ordinal (+ 1 (if is-main (length exports) index))))
					(unless is-main
						(incf index)))))

		;(format t "export list: ~A~%" new-exports) ;; debugging

		;; sort the export list by ordinal
		(sort new-exports #'< :key #'export-info-ordinal)

		;; write the address table
		(dotimes (i (length new-exports))
			(w4 (export-info-address (aref new-exports i))))

		;; sort the export list by name
		(sort new-exports #'string< :key #'export-info-name)
		
		;; write export name pointer table
		(let ((offset (+ export-name-table-offset next-rva (length dll-name) 1)))
			(dotimes (i (length new-exports))
				(let ((len (+ 1 (length (export-info-name (aref new-exports i))))))
					(w4 offset)
					(incf offset len))))

		;; write export ordinal table
		(dotimes (i (length new-exports))
			(w2 (- (export-info-ordinal (aref new-exports i)) 1)))

		;; write export name table
		(wchars dll-name)	;; first output dll filename
		(dotimes (i (length new-exports))
			(wchars (export-info-name (aref new-exports i))))

		(write-exe-section output-file ".edata" *export-buffer*)
		(update-export-table-address output-file next-rva (length *export-buffer*))))

;; Write a section named ".corman" which contains the name of the DLL
;; and the name of image file to use.
;; It contains:
;;		name of the kernel DLL (normally CormanLispServer.dll)
;;      name of the image file to load (which can be .img, .exe or .dll)
;;      copyright notice
;;      name of registered owner (or empty string if unregistered)
;;      name of registered organization (or empty string if none)
;; 
;; These are all null-terminated ascii strings.
;;
(defun write-corman-section (output-file dll-file-name image-file-name)
	(let ((*export-buffer* (make-array 40 :element-type 'byte :adjustable t :fill-pointer 0)))
		(multiple-value-bind 
			(registered days name org)
			(cl::registration-info)
			(declare (ignore days))
			(wchars dll-file-name)
			(wchars image-file-name)
			(wchars (cl::copyright-notice-short))
			(wchars (if registered name ""))
			(wchars (if registered org ""))
			(write-exe-section output-file ".corman" *export-buffer*))))

(defun write-def-file (output-file exports &optional verbose)
	(let ((path (merge-pathnames ".def" output-file)))
        (if verbose
            (format t "Creating file ~A.~%" path))
		(with-open-file (os path :direction :output)
			(format os "LIBRARY ~A~%" (pathname-name output-file))
			(format os "EXPORTS~%")
			(dotimes (i (length exports))
				(format os "    ~A~%" 
					(exported-symbol-name (car (aref exports i))))))))

(defun write-h-file (output-file exports &optional verbose)
	(let ((path (merge-pathnames ".h" output-file)))
        (if verbose
            (format t "Creating file ~A.~%" path))
		(with-open-file (os path :direction :output)
			(format os "#ifdef __cplusplus~%")
			(format os "extern \"C\" {~%")
			(format os "#endif // __cplusplus~%")
			(dotimes (i (length exports))
				(if (cadr (aref exports i))
					(format os "    ~A;~%" (cadr (aref exports i)))))
			(format os "#ifdef __cplusplus~%")
			(format os "}~%")
			(format os "#endif // __cplusplus~%"))))

(defun ccl::compile-dll (input-file 
		&key (output-file nil)
			 (verbose *compile-verbose*)
			 (print *compile-print*)
			 (def nil)
			 (h nil)
			 (kernel "CormanLispServer.dll")
			 (image "CormanLisp.img"))
	(if (null output-file)
		(setq output-file
			(if (string-equal (pathname-type input-file) cl::lisp-file-extension)
				(make-pathname 
					:device (pathname-device input-file)
					:directory (pathname-directory input-file)
					:name (pathname-name input-file)
					:type cl::dll-file-extension)
				(pathname 
					(concatenate 'string (namestring input-file) "." cl::dll-file-extension))))
		(setq output-file (truename output-file)))
		(let ((template-file (pathname (concatenate 'string *CORMANLISP-DIRECTORY* "dlltemplate.dll"))))		
			(unless
				(win:CopyFile
					(ct:lisp-string-to-c-string (namestring template-file))
					(ct:lisp-string-to-c-string (namestring output-file))
					nil)
				(error "Could not create output file ~A" output-file))
			(let* ((exports (make-array 10 :adjustable t :fill-pointer 0))
		   		   (dll-name 
					(concatenate 'string 
						(pathname-name output-file) "." 
						(pathname-type output-file)))
				   (ct:*collect-exported-functions* exports))
				(compile-file input-file :output-file output-file :verbose verbose :print print)
				(write-export-section output-file exports output-file dll-name)
				(write-corman-section output-file kernel image)
				(when def
					(write-def-file output-file exports verbose))
				(when h
					(write-h-file output-file exports verbose))
				output-file)))
	
(export '(write-exe-section read-exe-section open-read-exe))
(export '(#:compile-dll) 'ccl)

(setq cl::*COMPILER-WARN-ON-UNDEFINED-FUNCTION* t)


