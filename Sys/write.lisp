;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		write.lisp
;;;;	Contents:	Corman Lisp startup code to build the
;;;;				system.
;;;;	History:	9/1/96  RGC  Created.
;;;;				11/30/99 RGC Modified floating point printing
;;;;							 to be ANSI-compliant.
;;;;				4/16/01 RGC  Writing arrays now handles fill-pointers correctly.
;;;;				9/29/01 RGC  Incorporated fix to WRITE-SYMBOL from JP Massar.
;;;;				12/06/01 FAA Updated structure printer for wider slot specifiers.
;;;;                9/22/03  RGC BIT-VECTORs with fill pointers now print correctly.
;;;;

(defvar *standard-output* *standard-output*)
(defvar *print-escape* t)
(defvar *print-radix* nil)
(defvar *print-base* 10)
(defvar *print-circle* nil)
(defvar *print-pretty* nil)
(defvar *print-level* 100)
(defvar *print-length* 512)
(defvar *print-case* :upcase)
(defvar *print-gensym* t)
(defvar *print-array* t)
(defvar *print-readably* nil)
(defvar *print-right-margin* nil)
(defvar *print-miser-width* nil)
(defvar *print-lines* nil)
(defvar *print-pprint-dispatch* nil)
(defvar *printer-eq-forms* nil)						;; not exported
(defvar *printer-eq-forms-index* 0)					;; not exported
(defvar *current-print-level* 0)					;; not exported

(defun output-pretty-list (obj &optional stream need-to-indent) 
		(error "Pretty printer not loaded"))
(defun write-lisp-object (obj))	;; redefined below--avoid warning
(defun format (stream string &rest args) (error "Format not implemented yet")) ;; defined later

(defun gethash (object hashtable &optional default) (error "Not available"))	;; defined later
(defun |(SETF GETHASH)| (value object hashtable)
	(error "Not available"))	;; defined later
(register-setf-function 'gethash '|(SETF GETHASH)|)
(defun make-hash-table () (error "Not available"))	;; defined later
(defun array-dimensions (array) (error "Not available"))	;; defined later
(defun typep (x typespec) (error "Not available"))	;; defined later

;;;
;;; Add support for structures to this
;;;
(defun search-for-circularities (object)
	;; This currently checks arrays (of general type), lists, and structures
    ;;
	(unless 
        (or (consp object)
            (structurep object)
            (typep object '(array t)))
		(return-from search-for-circularities))
	(let ((n (gethash object *printer-eq-forms* 0)))
		(setf (gethash object *printer-eq-forms*) (+ n 1))
		(if (= n 0)
			(cond ((consp object)
		   		   (search-for-circularities (car object))
		  		   (search-for-circularities (cdr object)))
                  ((structurep object)
                   (dotimes (i (uvector-num-slots object))
                        (search-for-circularities (uref object (+ i 1)))))                  
		  		  (t (let ((size (apply '* (array-dimensions object))))
						(dotimes (i size)
						(search-for-circularities (row-major-aref object i)))))))))	

;; returns t if the object was output, nil otherwise
(defun output-circular-object (object)
	(let ((n (gethash object *printer-eq-forms*)))
		(if (or (null n)(and (integerp n)(= n 1)))
			(return-from output-circular-object nil))
		(when (and (integerp n)(> n 1))
			(incf *printer-eq-forms-index*)
			(setf (gethash object *printer-eq-forms*) (list *printer-eq-forms-index*))
			(%output-char #\# *standard-output*)
			(write-lisp-object *printer-eq-forms-index*)
			(%output-char #\= *standard-output*)
			(return-from output-circular-object nil))
		(when (listp n)
			(%output-char #\# *standard-output*)
			(write-lisp-object (car n))
			(%output-char #\# *standard-output*)
			(return-from output-circular-object t))
		nil)) 

(defun write-char (char &optional (output-stream *standard-output*))
	(if (null output-stream)
		(setq output-stream *standard-output*))
	(if (eq t output-stream)
		(setq output-stream *standard-output*))
	(%output-char char output-stream))

(defun write-string (string &optional (output-stream *standard-output*) 
			&key (start 0) end)
	(if (null output-stream)
		(setq output-stream *standard-output*))
	(if (eq t output-stream)
		(setq output-stream *standard-output*))
	(if (null end)
		(setq end (length string)))
	(%output-chars string output-stream start end)
	string)

(defun write-line (string &optional output-stream &key (start 0) end)
	(if (null output-stream)
		(setq output-stream *standard-output*))
	(if (eq t output-stream)
		(setq output-stream *standard-output*))
	(if (null end)
		(setq end (length string)))
	(%output-chars string output-stream start end)
	(terpri output-stream)
	string)


(defun write-integer (object)
	(let* ((print-trailing-point nil)
		  (os *standard-output*)
		  (negative (< object 0))
		  (n (if (< object 0) (- object) object))
		  (digits nil)
		  (base *print-base*))

		(if *print-radix*
			(cond 
				((= base 2) (%output-chars "#b" os 0 2))
				((= base 8) (%output-chars "#o" os 0 2))
				((= base 16) (%output-chars "#x" os 0 2))
				(t (if (/= base 10)
					(let ((*print-base* 10)(*print-radix* nil))
						(%output-char #\# os)
						(write-integer base)
						(%output-char #\r os))
					(setq print-trailing-point t)))))

		(if (= n 0)
			(push #\0 digits))
		(do* (digit)
			 ((<= n 0))
			 (setq digit (mod n base))
			 (setq n (floor n base))
			 (if (< digit 10)
				(push (int-char (+ (char-int #\0) digit)) digits)
				(push (int-char (+ (char-int #\A) (- digit 10))) digits)))
		(if negative (push #\- digits))
		(dolist (x digits)
			(%output-char x os))
		(if print-trailing-point
			(%output-char #\. os))))

(defun write-bignum (object)
	(let* ((print-trailing-point nil)
		  (os *standard-output*)
		  (negative (< object 0))
		  (n (if (< object 0) (- object) object))
		  (digits nil)
		  (base *print-base*))

		(if *print-radix*
			(cond 
				((= base 2) (%output-chars "#b" os 0 2))
				((= base 8) (%output-chars "#o" os 0 2))
				((= base 16) (%output-chars "#x" os 0 2))
				(t (if (/= base 10)
					(let ((*print-base* 10)(*print-radix* nil))
						(%output-char #\# os)
						(write-integer base)
						(%output-char #\r os))
					(setq print-trailing-point t)))))
		
		;; see how many digits to print at once
		(let ((clump-size 4)
			  (factor 1))
			(if (<= base 16)
				(setq clump-size 6))
			(if (<= base 10)
				(setq clump-size 8))
			(dotimes (i clump-size)
				(setq factor (* factor base)))

			(if (= n 0)
				(push #\0 digits))
			(do* (digit 
			      clump)
				 ((<= n 0))
				 (setq clump (mod n factor))
				 (setq n (floor n factor))
				 (dotimes (i clump-size)
					 (if (and (= clump 0) (= n 0))
						 (return))
					 (setq digit (mod clump base))
					 (setq clump (floor clump base))
					 (if (< digit 10)
						(push (int-char (+ (char-int #\0) digit)) digits)
						(push (int-char (+ (char-int #\A) (- digit 10))) digits))))
			(if negative (push #\- digits))
			(dolist (x digits)
				(%output-char x os))
			(if print-trailing-point
				(%output-char #\. os)))))

(defun write-hex-byte (n)
	(let ((high (truncate n 16))
		  (low (mod n 16))
		  (*print-base* 16))
		  (write-integer high)
		  (write-integer low)))
#|		
(defun write-bignum (object)
	(let ((*print-escape* nil)) 
		(write-string-object "#< BIGNUM: #x")
		(let ((*print-base* 16)
			  (bignum-length (* 4 (truncate (uref object 1) 2))))
			(write-integer (%uvector-address object))
			(write-string-object " ")
			(dotimes (i bignum-length)
				(write-hex-byte (%bignum-byte object (- bignum-length i 1)))))
		(write-string-object " >")))
|#

;;;;
;;;;	Common Lisp FIND-SYMBOL function
;;;;
(defun find-symbol (string &optional (package *package*))
	(unless (packagep package)
		(setq package (find-package package)))
	(unless (packagep package)
		(error "Invalid package: ~A" package))
	(multiple-value-bind (sym status)
		(package-find-symbol package string)
		(if status
			(cond 
				((eq status 'internal)(values sym :internal))
				((eq status 'external)(values sym :external))
			    (t (values sym :inherited)))
			(values nil nil))))

(defun external-symbol-p (sym package)
	(eq (cadr (multiple-value-list (find-symbol (symbol-name sym) package)))
		:external))

;; faa20001128a -	Add support for readtable-case.
;;					The following items moved to read.lisp.
;(defconstant *lower-case-a-code* (char-int #\a))
;(defconstant *lower-case-z-code* (char-int #\z))
;(defconstant *upper-case-a-code* (char-int #\A))
;(defconstant *upper-case-z-code* (char-int #\Z))

;(defun alpha-char-p (char) 
;	(or 
;		(and (>= (char-int char) *lower-case-a-code*)
;			 (<= (char-int char) *lower-case-z-code*))
;		(and (>= (char-int char) *upper-case-a-code*)
;			 (<= (char-int char) *upper-case-z-code*))))

;(defun lower-case-p (char) 
;	(and (>= (char-int char) *lower-case-a-code*)
;		 (<= (char-int char) *lower-case-z-code*)))

;(defun upper-case-p (char) 
;	(and (>= (char-int char) *upper-case-a-code*)
;		 (<= (char-int char) *upper-case-z-code*)))
;; faa20001128a -	End change.

(defun special-char-p (char) 
	(if (member char '(#\| #\# #\( #\) #\\ #\: #\;)) t nil))

(defun write-symbol (object)
	(let* ((pack nil)
	       (name-chars nil)
		   (pack-escape nil)
		   (name-escape nil)
		   (package (symbol-package object))
		   (symbol-name (symbol-name object))
		   (os *standard-output*)
		   (escape *print-escape*))
	
		;; if the symbol is in the keyword package, output a colon first
		(if (null package)
			(if *print-gensym*
				(progn
					(push #\# pack)
					(push #\: pack)))
			(if (eq package (find-package :keyword))
				(push #\: pack)
				(multiple-value-bind (symbol status) 
					(find-symbol symbol-name *package*)
					;; If we can't find a symbol of this name in the current package
					;; or the symbol we found isn't the same one we want to print,
					;; then we need to print the package prefix.  JPM.  09/27/01
					(if (or (null status) (not (eq symbol object))) 					
						(let ((package-name	(package-name package))
							  (need-bars nil))
							(dotimes (i (length package-name))
								(let ((c (elt package-name i)))
									(if (or (special-char-p c) (lower-case-p c))
										(setq need-bars t))
									(push c pack)))
							(if (and need-bars escape)
								(progn
									(setq pack (append '(#\|) pack '(#\|)))
									(setq pack-escape t)))
							(if (external-symbol-p object package) 
								(push #\: pack)
								(progn (push #\: pack) (push #\: pack))))))))

 		(let ((need-bars nil))
			(dotimes (i (length symbol-name))
				(let ((c (elt symbol-name i)))
					(if (or (special-char-p c) (lower-case-p c)(whitespace-char c))
						(setq need-bars t))
					(push c name-chars)))
			(if (and need-bars escape)
				(progn
					(setq name-chars (append (list #\|) name-chars (list #\|)))
					(setq name-escape t))))

		(setq name-chars (nreverse name-chars))
		(setq pack (nreverse pack))

		(cond 
			((eq *print-case* :downcase)
			 (if escape (dolist (i pack) (%output-char (if pack-escape i (char-downcase i)) os)))
			 (dolist (i name-chars) (%output-char (if name-escape i (char-downcase i)) os)))
			((eq *print-case* :capitalize)
			 (let ((first-time t))
				(if escape 
					(dolist (i pack) 
						(if first-time 
							(setq first-time nil)
							(setq i (char-downcase i)))
						(%output-char (if (or first-time pack-escape) i (char-downcase i)) os)))
				(setq first-time t)
				(dolist (i name-chars) 
					(if first-time 
						(setq first-time nil)
						(setq i (char-downcase i)))
					(%output-char (if (or first-time name-escape) i (char-downcase i)) os))))
			(t
			 (if escape (dolist (i pack) (%output-char i os)))
			 (dolist (i name-chars) (%output-char i os))))))
			 
(defun write-string-object (object)
	(let ((os *standard-output*)
		  (length (length object)))
		(if (or *print-readably* *print-escape*)
			(progn
				(%output-char #\" os)
				(dotimes (i length)
					(let ((c (elt object i)))
						(if (or (eq c #\\)(eq c #\"))
							(%output-char #\\ os)) ;; need to escape " and \
						(%output-char c os)))
 				(%output-char #\" os))
			(dotimes (i length)
				(%output-char (elt object i) os)))))

(defconstant left-paren #\( ;; )
	) 
(defconstant right-paren  ;; (
	#\))

(defun get-printer-eq-form (form)
	(let ((f (gethash form *printer-eq-forms* 0)))
		(or (listp f)(> f 1))))

(defun write-list (object)
	(let ((os *standard-output*)
		  list)
		(if *print-pretty*
			(progn
				(output-pretty-list object os)
				(return-from write-list)))

		;; check for (quote x) forms and output as 'x
		(if (and (eq (car object) 'quote) (consp (cdr object)))
			(progn
				(%output-char #\' os)
				(write-lisp-object (cadr object))
				(return-from write-list)))
        
        ;; check for (function x) forms and output as #'x
		(if (and (eq (car object) 'function) (consp (cdr object)))
			(progn
				(%output-char #\# os)
				(%output-char #\' os)
				(write-lisp-object (cadr object))
				(return-from write-list)))
		
		(incf *current-print-level*)	;; increment the print level		
	
		(%output-char left-paren os)
		(block print-loop
			(setq list object)
			(do* ((count 0 (+ count 1)))
				((not (consp list)))
				(when (> count 0)
					(%output-char (int-char 32) os)
					(if (and *print-circle* (get-printer-eq-form list))
						(return-from print-loop)))
				(if (and (not *print-readably*) *print-length* (>= *print-length* 0)
							(>= count *print-length*))
					(progn
						(%output-chars "..." os 0 3)
						(%output-char right-paren os)
						(decf *current-print-level*)
						(return-from write-list)))	;; decrement the print level		
				(write-lisp-object (car list))
				(setq list (cdr list))))

		(if list
			(progn
				(%output-chars " . " os 0 3)
				(write-lisp-object list)))
		(%output-char right-paren os) 
		(decf *current-print-level*)))	;; decrement the print level		

;;(defun write-float (object)
;;	(write (%float-to-string object) :escape nil))

(defconstant max-float-decimal-digits 6)
(defconstant float-decimal-constant (expt 10 max-float-decimal-digits))

(defun choose-exp (float)
	(cond 
		((single-float-p float)(if (eq *read-default-float-format* 'single-float) #\e #\f))
		((double-float-p float)(if (eq *read-default-float-format* 'double-float) #\e #\d))
		((short-float-p float)(if (eq *read-default-float-format* 'short-float) #\e #\s))))
	
(defun write-decimal-float (float os exp-flag)
	(let ((f (round (* float float-decimal-constant))))
		(multiple-value-bind (int fraction)
			(truncate f float-decimal-constant)
			(if (minusp f)
				(%output-char #\- os))
			(write-integer (if (minusp int) (- int) int))
			(setf fraction (if (minusp fraction)(- fraction) fraction))
			(%output-char #\. os)
			(if (= fraction 0)
				(progn (%output-char #\0 os) (%output-char #\0 os))
				(let ((frac (/ fraction float-decimal-constant)))
					(dotimes (i max-float-decimal-digits)
						(if (= frac 0)
							(return))
						(setf frac (* frac 10))
						(%output-char (int-char (+ (truncate frac)(char-int #\0))) os)
						(decf frac (truncate frac)))))
			(if exp-flag
				(let ((exp-designator (choose-exp float)))
					(when (not (eql exp-designator #\e))
						(%output-char exp-designator os)
						(%output-char #\0 os)))))))

(defun write-exp-float (float os)
	(let ((exp 0)
		  (magnitude (if (minusp float) (- float) float)))
		(if (>= magnitude 10.0d0)
			(do ()
				((< magnitude 10.0d0))
				(incf exp)
				(setf magnitude (/ magnitude 10.0d0)))
			(if (< magnitude 1.0d0)
				(do ()
					((>= magnitude 1.0d0))
					(decf exp)
					(setf magnitude (* magnitude 10.0d0)))))
		(write-decimal-float (* float (expt 10.0d0 (- exp))) os nil)
		(%output-char (choose-exp float) os)
		(write-integer exp)))	
		
(defun write-float (float)
	(let ((os *standard-output*)
		  (magnitude (if (minusp float) (- float) float))
		  (*print-base* 10)
		  (*print-radix* nil))
		(when (= float 0.0)
			(%output-chars "0.0" os 0 3)
			(let ((exp-signifier (choose-exp float)))
				(unless (eql exp-signifier #\e)
					(%output-char exp-signifier os)
					(%output-char #\0 os))
				(return-from write-float)))
		(if (and (>= magnitude 10e-3)(< magnitude 10e7))
			(write-decimal-float float os t)
			(write-exp-float float os))))

(defun write-complex (object)
	(let ((*print-escape* nil))
		(write-string-object "#C(")
		(write-lisp-object (realpart object))
		(write-string-object " ")
		(write-lisp-object (imagpart object))
		(write-string-object ")")))

;;; this gets redefined later
(defun output-readable-char (ch stream)
	(%output-char ch stream))

(defun write-character (object)
	(let ((os *standard-output*))
		(if (or *print-readably* *print-escape*)
			(output-readable-char object os)
			(%output-char object os))))

(defun write-ratio (object)
	(let ((*print-escape* nil))
		(write-integer (numerator object))
		(write-character #\/)
		(write-integer (denominator object))))

(defun write-function (object)
	(let ((*print-escape* nil)) 
		(write-string-object "#< COMPILED-FUNCTION: #x")
		(let ((*print-base* 16))
			(write-integer (execution-address object)))
		(write-string-object " >")))

(defun write-stream (object)
	(let ((*print-escape* nil))
		(write-string-object "#< ")
		(write-symbol (stream-subclass object))
		(write-string-object ": #x")
		(let ((*print-base* 16))
			(write-integer (%uvector-address object)))
		(write-string-object " >")))

;; redefine this here (from structures.lisp)
(defconstant struct-template-name-offset 0)
(defconstant struct-template-num-slots-offset 5)
(defconstant struct-template-slot1-offset 6)
(defconstant struct-template-slot-size 5)

(defun write-struct (object)
	(let* ((template (uref object 1))
		   (print-function 
			(if (vectorp template) 
				(get (elt template 0) :struct-print))))
		(if print-function
			(funcall print-function object *standard-output* (+ *current-print-level* 1))
			(let* ((save-print-escape *print-escape*)
				   (*print-escape* nil)
				   (keyword-package (find-package "KEYWORD"))	   
				   num-slots)
				(if (symbolp template)
				;; need to construct a template on the fly
					(let ()
						(setq template (list template))
						(push nil template)		; class
						(push nil template)		; type
						(push nil template)		; base
						(push 0 template)		; offset
						(push (- (uvector-num-slots object) 1) template)
						(dotimes (i (uvector-num-slots object))
							(push (intern (format nil "SLOT~A" (+ i 1)) keyword-package) template)
							(push nil template)
                            (push t template)
                            (push nil template)
                            (push nil template))
						(setq template (nreverse template))))
				(setq num-slots (elt template struct-template-num-slots-offset))
				(write-string-object "#S( ")
				(write-lisp-object (elt template struct-template-name-offset))
				(dotimes (i num-slots)
					(write-string-object " ")
					(let ((*print-escape* t))
						(write-lisp-object 
							(intern (symbol-name
									(elt template (+ struct-template-slot1-offset (* i struct-template-slot-size)))) 
									keyword-package)))
					(write-string-object " ")
					(let ((*print-escape* save-print-escape))
						(write-lisp-object (uref object (+ 2 i)))))
				(write-string-object " )")))))

(defun print-object (object stream) ;; this gets replaced by the generic function when clos.lisp loads
	(let ((*print-escape* nil)
		  (*standard-output* stream))
		(write-string-object "#< CLOS instance: #x")
		(let ((*print-base* 16))
			(write-integer (%uvector-address object)))
		(write-string-object " >")))	
	
(defun write-clos-instance (object)
	(print-object object *standard-output*))

(defun write-hashtable (object)
	(let ((*print-escape* nil))
		(write-string-object "#< HASHTABLE: #x")
		(let ((*print-base* 16))
			(write-integer (%uvector-address object)))
		(write-string-object " >")))

(defun write-package (object)
	(let ((*print-escape* nil))
		(write-string-object "#<PACKAGE ")
		(write (package-name object) :escape t)
		(write-string-object ">")))

(defun write-readtable (object)
	(let ((*print-escape* nil))
		(write-string-object "#< READTABLE: #x")
		(let ((*print-base* 16))
			(write-integer (%uvector-address object)))
		(write-string-object " >")))

(defun write-foreign-ptr (object)
	(let ((*print-escape* nil))
		(write-string-object "#< FOREIGN PTR: #x")
		(let ((*print-base* 16))
			(write-integer (foreign-ptr-to-int object)))
		(write-string-object " >")))

(defun write-foreign-heap-ptr (object)
	(let ((*print-escape* nil))
		(write-string-object "#< FOREIGN HEAP PTR: #x")
		(let ((*print-base* 16))
			(write-integer (foreign-ptr-to-int object)))
		(write-string-object ", length = ")
		(write-integer (uref object 2))
		(write-string-object " bytes")
		(write-string-object " >")))

(defun write-foreign-stack-ptr (object)
	(let ((*print-escape* nil))
		(write-string-object "#< FOREIGN PTR (dynamic): #x")
		(let ((*print-base* 16))
			(write-integer (foreign-ptr-to-int object)))
		(write-string-object ", length = ")
		(write-integer (uref object foreign-stack-ptr-size))
		(write-string-object " bytes")
		(write-string-object " >")))

(defun write-weak-ptr (object)
	(let ((*print-escape* nil))
		(write-string-object "#< WEAK PTR: ")
		(write-lisp-object (uref object weak-ptr-offset))
		(write-string-object " >")))

(defun write-compiled-code (object)
	(let ((*print-escape* nil))
		(write-string-object "#< COMPILED CODE: #x")
		(let ((*print-base* 16))
			(write-integer (%uvector-address object)))
		(write-string-object " >")))

(defun cormanlisp::lisp-object-bits (object) 0) ;; defined later
(defun write-unknown-object (object)
	(let ((*print-escape* nil))
		(write-string-object "#< UNKNOWN OBJECT TYPE: #x")
		(let ((*print-base* 16))
			(write-lisp-object (cormanlisp::lisp-object-bits object)))
		(write-string-object " >")))

(defun terminal-output-line-width () nil)

(defun write-array-segment (array index dimension)
	(let* ((need-space nil)
		   (max-line-length nil)
		   (os *standard-output*)
		   (*current-print-level* (+ 1 *current-print-level*))
		   (elements (if (array-has-fill-pointer-p array)
					(length array)
					(array-dimension array (- dimension 1)))))
		(if *print-pretty*
			(setq max-line-length (terminal-output-line-width)))
		(%output-char #\( os)
		(if (< dimension (array-rank array))
			(dotimes (i elements)
				(write-array-segment array index (+ dimension 1)))
			(dotimes (i elements)
				(if need-space
					(if (and max-line-length (> (stream-column os) max-line-length))
						(terpri)
						(%output-char (int-char 32) os)))

				(if (and (not *print-readably*) 
						*print-length* 
						(>= *print-length* 0)
						(>= i *print-length*))
					(progn
						(rplaca index (+ (car index) (- elements i)))
						(%output-chars "..." os 0 3)	
						(return))
					(write-lisp-object (row-major-aref array (car index))))
				(rplaca index (+ (car index) 1))
				(setq need-space t)))
		(%output-char #\) os)))

(defun write-array (object)
	(let* ((length (array-num-cells object))
	       (dimensions (array-rank object))
		   (os *standard-output*))
		(if (stringp object)
			(progn (write-string-object object) (return-from write-array)))
		(if (and (= dimensions 1) (eq (array-type object) 'BIT))
			(progn
				(%output-chars "#*" os 0 2)
				(dotimes (i (length object))
					(write-integer (elt object i)))
				(return-from write-array)))
		(if (and (not *print-readably*) (not *print-array*))
			(let ((*print-escape* nil))
				(write-string-object "#< ARRAY: #x")
				(let ((*print-base* 16))
					(write-integer (%uvector-address object)))
				(write-string-object " >")
				(return-from write-array)))
		(%output-char #\# os)
		(if (/= dimensions 1)
			(progn
				(write-integer dimensions)
				(%output-char #\A os)))
		(if (>= dimensions 1)
			(write-array-segment object (list 0) 1)
			(write-lisp-object (list (row-major-aref object 0))))))

;; need to override warning here
(setq *COMPILER-WARN-ON-UNDEFINED-FUNCTION* nil)
(defun write-builtin-object (object)
	;; if we have reached *print-level*, print as a '#' character
	(if (and *print-level* (> *print-level* 0) (<= *print-level* *current-print-level*))
		(setq object #\#))

	;; handle circularities if necessary
	(if (and *print-circle* (or (consp object)(uvectorp object)))
		(if (output-circular-object object)
			(return-from write-builtin-object object)))
	
	(cond
		((uninitialized-object-p object) (write "#< Uninitialized >" :escape nil)) 
		((consp object)			(write-list object))
		((fixnump object)		(write-integer object))
		((bignump object)		(write-bignum object))
		((single-float-p object)(write-float object))
		((double-float-p object)(write-float object))
		((short-float-p object) (write-float object))
		((symbolp object)		(write-symbol object))
		((stringp object)		(write-string-object object))
		((ratiop object)		(write-ratio object))
		((complexp object)		(write-complex object))
		((characterp object)	(write-character object))
		((clos-instance-p object)(write-clos-instance object))
		((functionp object)		(write-function object))
		((streamp object)		(write-stream object))
		((arrayp object)		(write-array object))
		((structurep object)	(write-struct object))
		((hash-table-p object)	(write-hashtable object))
		((packagep object)		(write-package object))
		((readtablep object)	(write-readtable object))
		((foreignp object)		(write-foreign-ptr object))
		((foreign-heap-p object)(write-foreign-heap-ptr object))
		((foreign-stack-p object)(write-foreign-stack-ptr object))
		((weak-pointer-p object)(write-weak-ptr object))
		((compiled-code-p object)(write-compiled-code object))
		(t (write-unknown-object object)))
	object)
(setq *COMPILER-WARN-ON-UNDEFINED-FUNCTION* t)

;;;
;;; Add a level of indirection here, which helps when we need to modify it
;;; after CLOS loads.
;;; This gets overridden then.
;;;
(defun write-lisp-object (object)
    (write-builtin-object object))
    
(defun invalid-object-p (object) nil)		;; redefined later
(defun invalid-object-string (object) "invalid object") ;; redefined later
(defun broadcast-stream-p (stream) nil)     ;; redefined later
(defun broadcast-stream-streams (stream) nil) ;; redefined later

(defun write (object 
	&key (stream		*standard-output*)
		 (escape		*print-escape*)
		 (radix			*print-radix*)
		 (base			*print-base*)
		 (circle		*print-circle*)
		 (pretty		*print-pretty*)
		 (level			*print-level*)
		 (length		*print-length*)
		 (case			*print-case*)
		 (gensym		*print-gensym*)
		 (array			*print-array*)
		 (readably		*print-readably*)
		 (right-margin	*print-right-margin*)
		 (miser-width	*print-miser-width*)
		 (lines			*print-lines*)
		 (pprint-dispatch *print-pprint-dispatch*))

    (if (broadcast-stream-p stream)
        (dolist (x (broadcast-stream-streams stream))
            (write object :stream x
                :escape escape
                :radix radix
                :base base
                :circle circle
                :pretty pretty
                :level level
                :length length
                :case case
                :gensym gensym
                :array array
                :readably readably
                :right-margin right-margin
                :miser-width miser-width
                :lines lines
                :pprint-dispatch pprint-dispatch))
        (progn
       
    		(if (invalid-object-p object)
    			(write (invalid-object-string object) :stream stream))

        	;; rebind all variables
        	(let* ((*standard-output*		stream)
        		   (*print-escape*			escape)
        		   (*print-radix*			radix)
        		   (*print-base*			base)
        		   (*print-circle*			circle)
        		   (*print-pretty*			pretty)
        		   (*print-level*			level)
        		   (*print-length*			length)
        		   (*print-case*			case)
        		   (*print-gensym*			gensym)
        		   (*print-array*			array)
        		   (*print-readably*		readably)
        		   (*print-right-margin*	right-margin)
        		   (*print-miser-width*		miser-width)
        		   (*print-lines*			lines)
        		   (*print-pprint-dispatch* pprint-dispatch)
        		   (*current-print-level* 0))
        
        		(if (and *print-circle* (= *current-print-level* 0))
        			(let ((*printer-eq-forms* (make-hash-table))
        				  (*printer-eq-forms-index* 0))
        				(search-for-circularities object)	
        				(write-lisp-object object))
        			(write-lisp-object object)))
        	object)))
;
;	Common Lisp 'princ' function.
;
(defun princ (object &optional (output-stream *standard-output*))
	(write object :stream output-stream :escape nil))

;
;	Common Lisp 'prin1' function.
;
(defun prin1 (object &optional (output-stream *standard-output*))
	(write object :stream output-stream :escape t))

;
;	Common Lisp 'print' function.
;
(defun print (object &optional (output-stream *standard-output*))
	(write #\Newline :stream output-stream :escape nil)
	(write object :stream output-stream :escape t)
	(write #\Space :stream output-stream :escape nil)
	object)
;
;	Common Lisp 'pprint' function.
;
(defun pprint (object &optional (output-stream *standard-output*))
	(write #\Newline :stream output-stream :escape nil)
	(write object :stream output-stream :escape t :pretty t)
	(values))

;; redefined later
(defun format (stream string &rest args) 
	(write string :stream stream))

;; redefined later
(defun warn (string &rest args)
	(format *error-output* ";;; Warning: ~A~%" (apply 'format nil string args))) 



