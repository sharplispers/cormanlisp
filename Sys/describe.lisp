;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		describe.lisp
;;;;	Contents:	DESCRIBE support for Corman Lisp.
;;;;	History:	3/27/97  RGC  Created.
;;;;			    11/30/99 RGC  Fixed bug in description of ratio, complex numbers.
;;;;
;;;;                11/15/02 RGC  Replaced with JP Massar's implementation of 11/15/02.
;;;;
;;;;
;;;;    JP Massar.   11/15/02.  
;;;;        Removed repetitive strings from all the definitions, making them
;;;;        global variables.
;;;;        Changed functions around to conform to hyperspec.  DESCRIBE now simply
;;;;        calls DESCRIBE-OBJECT, which has methods on all Common Lisp standard
;;;;        objects (which used to be the functions) 
;;;;        Changed use of ~A to ~S in many places for readability.  E.g., describing
;;;;        #\Newline would print out a real newline, not '#\Newline'.

;;;;    Specific additions to functionality:
;;;;     -- Integers print out binary, octal and hex representations as well.
;;;;     -- Floats print out result of DECODE-FLOAT as well as value.
;;;;     -- Functions pretty print original lambda definition.
;;;;     -- Arrays print out number of dimensions and list of dimensions.
;;;;     -- Packages print out number of shadowing and external symbols.

(in-package :common-lisp)

(defparameter *dfs-a* "~4T~A:~20T~A~%" "Standard 'Describe' format string using ~A")
(defparameter *dfs-s* "~4T~A:~20T~S~%" "Standard 'Describe' format string using ~S")
(defparameter *dfs-hex* "~4T~A:~20T#x~X~%" "Standard 'Describe' format string using hex")

;;; Symbols

(defmethod describe-object ((x symbol) s)
  (let* ((is-special 
	  (= (logand (%symbol-get-flags x) *symbol-special-flag*) 
	     *symbol-special-flag*))
	 (function (if (fboundp x) (symbol-function x) "#< UNBOUND >")) 
	 (value (if (boundp x) (symbol-value x) "#< UNBOUND >"))) 
    (format s "SYMBOL:~%~?~?~?~?~?~?~?~?~?~?~?"
	    *dfs-a* (list "name" 		(symbol-name x))
	    (if (boundp x) *dfs-s* *dfs-a*) (list "value" 		value)
	    *dfs-a* (list "package" 	(symbol-package x))
	    *dfs-s* (list "property list"   (symbol-plist x))
	    *dfs-a* (list "function" 	function)
	    *dfs-a* (list "function type" 	(uref x symbol-function-type-offset))
	    *dfs-a* (list "constant" 	(constantp x))
	    *dfs-a* (list "special" 	is-special)
	    *dfs-a* (list "jump table" 	(uref x symbol-jump-table-offset))
	    *dfs-a* (list "variable table"  (uref x symbol-var-table-offset))
	    *dfs-hex* (list "heap address" (%uvector-address x)))))

;;; Numbers

(defmethod describe-object ((x integer) s)
  (if (fixnump x) (describe-fixnum x s) (describe-bignum x s)))

(defun describe-fixnum (x s) 
  (format s "INTEGER:~%~?~?~?~?~?"
	  *dfs-a* (list "subclass" 		'fixnum)
	  *dfs-a* (list "value" 			x)
	  "~4T~A:~20T~B~%" (list "binary" x)
	  "~4T~A:~20T~O~%" (list "octal" x)		
	  "~4T~A:~20T~X~%" (list "hex" x)))

(defun describe-bignum (x s) 
  (format s "INTEGER:~%~?~?~?~?~?"
	  *dfs-a* (list "subclass" 		'bignum)
	  *dfs-a* (list "value" 			x)
	  "~4T~A:~20T~X~%" (list "hex" x)
	  *dfs-a* (list "number of cells" (truncate (uref x 1) 2))
	  *dfs-hex* (list "heap address" (%uvector-address x))))

(defmethod describe-object ((x ratio) s) 
  (format s "RATIO:~%~?~?~?~?"
	  *dfs-a* (list "value" 			x)
	  *dfs-a* (list "numerator" 		(numerator x))
	  *dfs-a* (list "denominator" 	(denominator x))
	  *dfs-hex* (list "heap address" (%uvector-address x))))

(defmethod describe-object ((x float) s)
  (multiple-value-bind (significand exponent sign)
      (decode-float x)
    (let ((float-type (cond ((single-float-p x) 'single-float)
			    ((double-float-p x) 'double-float)
			    (t 'short-float))))
      (format s "FLOAT:~%~?~?~?~?~?"
	      *dfs-a* (list "subclass" float-type)	
	      *dfs-a* (list "value" x)
	      *dfs-a* (list "significand" significand)
	      *dfs-a* (list "exponent" exponent)
	      *dfs-a* (list "sign" (if (plusp sign) #\+ #\-))
	      ))))

(defmethod describe-object ((x complex) s) 
  (format s "COMPLEX:~%~?~?~?~?"
	  *dfs-a* (list "value" 			x)
	  *dfs-a* (list "real part" 		(realpart x))
	  *dfs-a* (list "imaginary part" (imagpart x))
	  *dfs-hex* (list "heap address" (%uvector-address x))))

;;; Characters

(defmethod describe-object ((x character) s)
	(format s "CHARACTER:~%~?~?~?"
		*dfs-a* (list "subclass" 		'base-char)
		*dfs-s* (list "value" 			x)
		*dfs-a* (list "character code" (char-int x))))

;;; Cons cells

(defmethod describe-object ((x cons) s) 
	(format s "CONS:~%~?~?~?~?"
		*dfs-s* (list "car" 		(car x))
		*dfs-s* (list "cdr" 		(cdr x))
		*dfs-s* (list "value" 		x)
		*dfs-hex* (list "heap address" (- (lisp-object-id x) x86::cons-tag))))

;;; Function objects (all function objects are compiled)

(defmethod describe-object ((x function) s)
  (let* ((compiled-code-obj 
	  (unless (pl::kernel-function-p x) 
	    (uref x function-code-buffer-offset)))
	 (references 
	  (if compiled-code-obj 
	      (uref compiled-code-obj compiled-code-references-offset)))
	 (properties 
	  (if compiled-code-obj 
	      (uref compiled-code-obj compiled-code-info-offset)))
	 (name (getf properties 'function-name))
	 (lambda-list (getf properties 'lambda-list))
	 (lambda (getf properties 'lambda))
	 (source-file (getf properties 'pl:*source-file*))
	 (source-line (getf properties 'pl:*source-line*)))
    (if (null name)
	(setq name "#< UNKNOWN >"))

    ;; If you have a null lambda-list this used to print out
    ;; that it was "UNKNOWN".
    ;; (if (null lambda-list)
    ;;   (setq lambda-list "#< UNKNOWN >"))

    ;;(if (null lambda)
    ;; (setq lambda "#< UNKNOWN >"))
	 

    ;; Make the original lambda definition pretty print

    (format s "FUNCTION:~%~?~?~?~?~?~?~?~?~?~?"
	    *dfs-a* (list "subclass" 	'compiled-function)
	    *dfs-a* (list "implementation" 
			  (if (pl::kernel-function-p x) 'kernel 'standard))
	    *dfs-a* (list "environment" (function-environment x))
	    *dfs-hex* (list "code address" (execution-address x))
	    *dfs-a* (list "references" references)
	    *dfs-a* (list "name" 		name)
	    *dfs-a* (list "lambda-list" lambda-list)
	    ;; *dfs-a* (list "lambda" lambda)	
	    *dfs-a* (list "source file" source-file)
	    *dfs-a* (list "source line" source-line)
	    *dfs-hex* (list "heap address" (%uvector-address x)))
    (format s "~&Original definition: ")
    (if (null lambda) (format s "#< UNKNOWN >") (pprint lambda s))
    (terpri s)
    ))


;;; Arrays


(defmethod describe-object ((x vector) s)
  (cond 
     ((simple-vector-p x)(describe-simple-vector x s))
     ((simple-char-vector-p x)(describe-simple-char-vector x s))
     ((simple-byte-vector-p x)(describe-simple-byte-vector x s))
     ((simple-bit-vector-p x)(describe-simple-bit-vector x s))
     ((simple-short-vector-p x)(describe-simple-short-vector x s))
     ((simple-double-float-vector-p x)
      (describe-simple-double-float-vector x s))
     (t (call-next-method x s))
     ))


(defun describe-generic-simple-vector (x s name)
  (format s "~A:~%~?~?~?"
	  name
	  *dfs-a* (list "type" 		(array-type x))
	  *dfs-a* (list "number of cells" (array-num-cells x))
	  *dfs-hex* (list "heap address" (%uvector-address x))))
  

(defun describe-simple-vector (x s) 
  (describe-generic-simple-vector x s "SIMPLE-VECTOR"))

(defun describe-simple-char-vector (x s)
  (describe-generic-simple-vector x s "SIMPLE-CHAR-VECTOR"))

(defun describe-simple-byte-vector (x s)
  (describe-generic-simple-vector x s "SIMPLE-BYTE-VECTOR"))

(defun describe-simple-bit-vector (x s)
  (describe-generic-simple-vector x s "SIMPLE-BIT-VECTOR"))

(defun describe-simple-short-vector (x s)
  (describe-generic-simple-vector x s "SIMPLE-SHORT-VECTOR"))

(defun describe-simple-double-float-vector (x s)
  (describe-generic-simple-vector x s "SIMPLE-DOUBLE-FLOAT-VECTOR"))

(defun describe-simple-single-float-vector (x s)
  (describe-generic-simple-vector x s "SIMPLE-SINGLE-FLOAT-VECTOR"))

(defmethod describe-object ((x array) s) 
  (let* ((*print-array* nil)) 
    (format 
     s "ARRAY:~%~?~?~?~?~?~?~?~?"
     *dfs-a* (list "type" 		(array-type x))
     *dfs-a* (list "number of dimensions" (uref x adjustable-array-dimensions-offset))
     *dfs-a* (list "dimensions" (array-dimensions x))
     *dfs-a* (list "fill pointer" 
		   (if (< (uref x adjustable-array-fill-pointer-offset) 0) nil
		     (uref x adjustable-array-fill-pointer-offset)))
     *dfs-a* (list "number of cells" (array-num-cells x))
     *dfs-a* (list "vector" (uref x adjustable-array-vector-offset))
     *dfs-a* (list "displaced index" (uref x adjustable-array-displaced-offset))
     *dfs-a* (list "adjustable" 'T)
     *dfs-hex* (list "heap address" (%uvector-address x)))))


;;; Other Common Lisp data structures.
;;; Streams, packages, hash-tables, readtables and pathnames.

(defmethod describe-object ((x stream) s) 
  (let* ((*print-array* nil)
	 (*dfs-s* "~4T~A:~30T~S~%")
	 (*dfs-hex* "~4T~A:~30T#x~X~%"))
    (format 
     s "STREAM:~%~?~?~?~?~?~?~?~?~?~?~?~?~?~?~?~?~?~?~?~?~?~?"
     *dfs-s* (list "subclass" 	(uref x stream-subclass-offset))
     *dfs-s* (list "name" 		(uref x stream-name-offset))
     *dfs-s* (list "overflow function" (uref x stream-overflow-func-offset))
     *dfs-s* (list "underflow function" (uref x stream-underflow-func-offset))
     *dfs-s* (list "position" 	(uref x stream-position-offset))
     *dfs-s* (list "column" 	(uref x stream-col-position-offset))
     *dfs-s* (list "line" 		(uref x stream-line-number-offset))
     *dfs-hex* (list "input buffer" 	(%uvector-address (uref x stream-input-buffer-offset)))
     *dfs-s* (list "input buffer length" 	(uref x stream-input-buffer-length-offset))
     *dfs-s* (list "input buffer position" (uref x stream-input-buffer-pos-offset))
     *dfs-s* (list "chars in input buffer" (uref x stream-input-buffer-num-offset))
     *dfs-hex* (list "output buffer"	(%uvector-address (uref x stream-output-buffer-offset)))
     *dfs-s* (list "output buffer length" 	(uref x stream-output-buffer-length-offset))
     *dfs-s* (list "output buffer position" (uref x stream-output-buffer-pos-offset))
     *dfs-s* (list "handle" 	(uref x stream-handle-offset))
     *dfs-s* (list "binary" 	(uref x stream-binary-offset))
     *dfs-s* (list "open" 		(uref x stream-open-offset))
     *dfs-s* (list "direction" 	(uref x stream-direction-offset))
     *dfs-s* (list "interactive" (uref x stream-interactive-offset))
     *dfs-s* (list "element type" (uref x stream-element-type-offset))
     *dfs-s* (list "associated streams" (uref x stream-associated-streams-offset))
     *dfs-hex* (list "heap address" (%uvector-address x)))))

(defmethod describe-object ((x package) s) 
  (let* ((*print-array* nil)
	 (shadowing-syms (package-shadowing-symbols x))
	 (external-syms
	  (let ((elist nil)) (do-external-symbols (e x) (push e elist)) elist)
	  ))
    
    (format 
     s "PACKAGE:~%~?~?~?~?~?~?~?~?~?~?~?~?"
     *dfs-s* (list "name" 		(package-name x))
     *dfs-s* (list "nicknames" 	(package-nicknames x))
     *dfs-s* (list "uses packages" (mapcar #'package-name (package-use-list x)))
     *dfs-a* (list "used by" 	(mapcar #'package-name (package-used-by-list x)))
     *dfs-a* (list "capacity" 	(package-capacity x))
     *dfs-a* (list "count" 		(package-count x))
     *dfs-a* (list "table" 		(package-table x))
     *dfs-hex* (list "heap address" (%uvector-address x))
     *dfs-a* (list "# of shadowing syms" (length shadowing-syms))
     *dfs-a* (list "shadowing symbols" shadowing-syms)
     *dfs-a* (list "# of external syms" (length external-syms))
     *dfs-a* (list "external symbols" external-syms)
     )))


(defmethod describe-object ((x hash-table) s)
  (let* ((*print-array* nil)) 
    (format s "HASH-TABLE:~%~?~?~?~?~?~?~?~?~?"
	    *dfs-a* (list "size" 		(hash-table-size x))
	    *dfs-a* (list "count" 		(hash-table-count x))
	    *dfs-a* (list "rehash size" (hash-table-rehash-size x))
	    *dfs-a* (list "rehash threshold" (hash-table-rehash-threshold x))
	    *dfs-a* (list "table" 		(hash-table-table x))
	    *dfs-a* (list "hash function" 	(hash-table-hash-function x))
	    *dfs-a* (list "test" 		(hash-table-test x))
	    *dfs-a* (list "test function" (hash-table-test-function x))
	    *dfs-hex* (list "heap address" (%uvector-address x)))))

(defmethod describe-object ((x readtable) s) 
  (let* ((*print-array* nil)) 
    (format s "READTABLE:~%~?~?~?~?"
	    *dfs-a* (list "read level" (uref x 1))
	    *dfs-a* (list "table" 		(uref x 3))
	    *dfs-a* (list "case" 		(uref x 4))
	    *dfs-hex* (list "heap address" (%uvector-address x)))))


(defmethod describe-object ((pathname pathname) s)
  (format s "PATHNAME:~%~?~?~?~?~?~?~?~?"
	  *dfs-s* (list "host" 		(pathname-host pathname))
	  *dfs-s* (list "device" 	(pathname-device pathname))
	  *dfs-s* (list "directory" 	(pathname-directory pathname))
	  *dfs-s* (list "name" 		(pathname-name pathname))
	  *dfs-s* (list "type" 		(pathname-type pathname))
	  *dfs-a* (list "defaults" 	(pathnames::pathname-internal-defaults pathname))
	  *dfs-a* (list "case" 		(pathnames::pathname-internal-case pathname))
	  *dfs-hex* (list "heap address" (%uvector-address pathname))))

;;;
;;; Structure objects and other, Corman-internal objects.
;;; Plus a catch-all.

(defmethod describe-object ((object t) stream)
  (let ((*print-length* 6))
    (cond 
     ((structurep object)(describe-structure object stream))
     ((foreignp object)	(describe-foreign object stream))
     ((compiled-code-p object) (describe-compiled-code object stream))
     ((foreign-heap-p object) (describe-foreign-heap object stream))
     ((weak-pointer-p object) (describe-weak-pointer object stream))
     (t (format t "Sorry, no description available for ~A" object))
     )))

(defun get-template (struct)
	(let ((template (uref struct 1)))
		(if (symbolp template) ; construct a template
		    (let ((num-slots (1- (uvector-num-slots struct))))
                       (setq template (list template nil nil nil 0 num-slots))
                       (dotimes (i num-slots template)
			    (nconc template (list (intern (format nil "SLOT~A" (+ i 1)) keyword-package) nil t nil nil))))
                    template)))

(defun describe-structure (x s) 
  
  ;; The old code constructed a template if template was a symbol.
  ;; But the format of the template it constructed seemed to be wrong.
  ;; First, why would the template ever be a symbol?  I don't know.
  ;; Some kind of anonymous structure.
  ;; Second, no sense constructing a template when all the old code
  ;; cared about was consing up the names "SLOT1", "SLOT2", etc.
  ;; Back to old code.

  (let* ((template (get-template x))
	 (*print-escape* nil)
	 (num-slots)) 
				
    (setq num-slots (cl::struct-template-num-slots template))

    (format s "STRUCTURE:~%~?~?~?~?"
	    *dfs-a* (list "name" 		(elt template 0))
	    *dfs-a* (list "no. of slots" num-slots)
	    *dfs-a* (list "template" 	(uref x 1))
	    *dfs-hex* (list "heap address" (%uvector-address x)))

    (dotimes (i num-slots)
	(format s *dfs-s*
		(elt template (+ 6 (* i 5)))
		(uref x (+ 2 i))))))

(defun describe-foreign (x s) 
	(format s "FOREIGN POINTER:~%~?~?"
		*dfs-hex* (list "address" 		(foreign-ptr-to-int x))
		*dfs-hex* (list "heap address" (%uvector-address x))))

;; (defconstant compiled-code-code-offset 4)

(defun describe-compiled-code (x s) 
	(let* ((references (uref x compiled-code-references-offset))
		   (properties (uref x compiled-code-info-offset))
		   (name (getf properties 'function-name))
		   (lambda-list (getf properties 'lambda-list))
		   (lambda (getf properties 'lambda))
		   (source-file (getf properties 'pl:*source-file*))
		   (source-line (getf properties 'pl:*source-line*)))
		(if (null name)
			(setq name "#< UNKNOWN >"))
		(if (null lambda-list)
			(setq lambda-list "#< UNKNOWN >"))
		(if (null lambda)
			(setq lambda "#< UNKNOWN >"))
	 
		(format s "COMPILED-CODE:~%~?~?~?~?~?~?~?~?"
			*dfs-hex* (list "code address" 
					(+ (%uvector-address x) (* compiled-code-code-offset 4)))
			*dfs-a* (list "references" references)
			*dfs-s* (list "name" 		name)
			*dfs-a* (list "lambda-list" lambda-list)
			*dfs-a* (list "lambda" 	lambda)
			*dfs-s* (list "source file" source-file)
			*dfs-a* (list "source line" source-line)
			*dfs-hex* (list "heap address" (%uvector-address x)))))

(defun describe-foreign-heap (x s) 
	(format s "FOREIGN HEAP POINTER:~%~?~?~?"
		*dfs-hex* (list "address" 		(foreign-ptr-to-int x))
		*dfs-a*   (list "no. bytes" 	(uref x 2))
		*dfs-hex* (list "heap address" (%uvector-address x))))

(defun describe-weak-pointer (x s) 
	(format s "WEAK POINTER:~%~?~?"
		*dfs-a* (list "object" 		(uref x 1))
		*dfs-hex* (list "heap address" (%uvector-address x))))


;;; The DESCRIBE function


(defun describe (object &optional (s *standard-output*))
  (cond
   ((eq s t) (setq s *terminal-io*))
   ((eq s nil) (setq s *standard-output*))
   ((null (streamp s))
    (error "Stream argument to DESCRIBE is not a stream designator"))
   )
  (describe-object object s)
  (values)
  )

;; (defun nd (x) (new-describe x))

;; (export '(new-describe nd) (find-package :lisp))

