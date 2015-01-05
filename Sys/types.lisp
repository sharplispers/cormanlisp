;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		types.lisp
;;;;	Contents:	Corman Lisp type functions.
;;;;	History:	10/14/96  RGC  Created.
;;;;				4/6/01    RGC  Added code to check lengths of vectors correctly.
;;;;                12/19/02  RGC  Incorporated JP Massar's improvements to DEFTYPE.
;;;;

(in-package :common-lisp)

(defun uvector-type-of (x)
	(let ((tag (uvector-type-bits x)))
		(cond 
			((eq tag uvector-symbol-tag) 'symbol)
			((eq tag uvector-function-tag) 'function)
			((eq tag uvector-kfunction-tag) 'function)
			((eq tag uvector-structure-tag) 
				(let ((template (uref x 1)))
					(if (vectorp template) (elt template 0) 'structure-object)))
			((eq tag uvector-array-tag) 'array)
			((eq tag uvector-stream-tag) 'stream)
			((eq tag uvector-double-float-tag) 'double-float)
			((eq tag uvector-single-float-tag) 'single-float)
			((eq tag uvector-package-tag) 'package)
			((eq tag uvector-hashtable-tag) 'hash-table)
			((eq tag uvector-readtable-tag) 'readtable)
			((eq tag uvector-complex-tag) 'complex)
			((eq tag uvector-ratio-tag) 'ratio)
			((eq tag uvector-bignum-tag) 'bignum)
			(t (error "Cannot determine type of object: ~A" x)))))

(defun type-of (x)
	(let ((tag (tag-bits x)))
		(cond 
			((eq tag 0) 'fixnum)
			((eq tag 4) 'cons)
			((eq tag 1) 'character)
			((eq tag 3) 'short-float)
			((eq tag 7) 'short-float)
			((eq tag 5) (uvector-type-of x))
			(t (error "Cannot determine type of object: ~A" x)))))

(defun install-type-specifier (type-specifier-symbol func)
	(setf (get type-specifier-symbol 'type-discriminator) func))

(defmacro declare-type-specifier (type-specifier-symbol &rest exprs)
	`(install-type-specifier ',type-specifier-symbol #'(lambda ,@exprs)))

;;;; Returns true if the passed object is a symbol which 
;;;; represents a type.
(defun is-symbol-type-specifier (sym)
	(and (symbolp sym) (get sym 'type-discriminator)))


;;;;
;;;;	Type expansion
;;;;
(defun type-expansion (symbol) (get symbol :type-expansion))
(defun |(SETF TYPE-EXPANSION)| (func symbol) (setf (get symbol :type-expansion) func))
(register-setf-function 'type-expansion '|(SETF TYPE-EXPANSION)|) 

(defun typeexpand (x)
	(do ()
		(nil)
		(if (or (symbolp x) (and (consp x)(symbolp (car x))))
			(let* ((type-expansion-func (type-expansion (if (symbolp x) x (car x)))))
				(if type-expansion-func
					(setq x (funcall type-expansion-func (if (symbolp x) (list x) x)))
					(return x)))
			(return x))))

(defun typeexpand-all (form)
	(setq form (typeexpand form))   ;; expand top level form
	(if (and (consp form) (member (car form) '(and or not values)))
		;; expand embedded types
		(let ((num-subforms (- (length form) 1)))
			(dotimes (i num-subforms)
				(setf (nth (+ i 1) form) (typeexpand-all (nth (+ i 1) form))))))
	form)

;;;;
;;;;	Common Lisp TYPEP function
;;;;	
(defun get-type-discriminator (type-symbol type-expression obj)
	(declare (ignore obj))
	(unless (symbolp type-symbol)
		(error "Invalid type specifier: ~A" type-expression))
	(let ((type-func (get type-symbol 'type-discriminator)))
		(unless (functionp type-func)
			(error "Unknown type specifier: ~A" type-expression))
		type-func))

(defun _typep (object type-expression)
	(funcall 
		(get-type-discriminator 
			(if (consp type-expression)(car type-expression) type-expression) 
			type-expression
			object) 
		object 
		type-expression))

(defun typep (object type-expression)
	(_typep object (typeexpand-all type-expression)))

(defconstant keyword-package (find-package "KEYWORD"))

;;;;
;;;;	Common Lisp KEYWORDP function.
;;;;
(defun keywordp (x) (and (symbolp x) (eq (symbol-package x) keyword-package)))

;;;;
;;;;	Common Lisp CHECK-TYPE macro.
;;;;

(defun check-type-body (obj typespec msg form)
	(declare (ignore msg))
	(unless (_typep obj typespec)
		(error "The value of ~A, ~A, is not of type ~A" form obj typespec)))

(defmacro check-type (place typespec &optional string)
	`(check-type-body ,place ',(typeexpand-all typespec) ,string ',place))

;; Auxiliary function.  Could be used in other macro forms and
;; special forms that need to parse doc strings and decls.

;; Returns four values
;; 1.  Doc string or NIL
;; 2.  List of decls (or NIL)
;; 3.  List of body forms (including bad decls from 4 below)
;; 4.  Declaration forms which are not in legal position

(defun parse-doc-decls-body (forms)
    (let ((doc-string nil)
          (declarations nil))
        (flet
            ((decl-form? (x) (and (consp x) (eq (car x) 'declare))))
            (do* ((f forms (cdr f)))
                ((null f)(setq forms f))
                (if (and (typep (car f) 'string) (null doc-string) (cdr f))
                    (setq doc-string (car f))
                    (if (decl-form? (car f))
                        (push (car f) declarations)
                        (progn (setq forms f) (return)))))
            (values
                doc-string
                declarations
                forms
                
                ;;; use do loop instead of 
                ;;; (remove-if-not (function decl-form?) forms)
                ;;; since remove-if-not is not defined yet
                (do ((bad-decls nil)
                     (f forms (cdr f)))
                    ((not (consp f)) (nreverse bad-decls))
                    (if (funcall (function decl-form?) (car f))
                        (push (car f) bad-decls)))))))
		
;;;;
;;;;	Common Lisp DEFTYPE macro.
;;;;

(defmacro deftype (name lambda-list &rest forms)
    (let ((doc-form nil) (lambda-form nil))
        (multiple-value-bind (doc-string decls body-forms bad-decls)
            (parse-doc-decls-body forms)
            (when doc-string
                (setq doc-form 
                    `((setf (documentation ',name 'type) ,doc-string))))
            (when bad-decls
                (dolist (bd bad-decls)
                    (warn "~A (DEFTYPE ~A ...): ~A~%~A~%" 
                        "Declaration found in wrong place in" 
                        name 
                        bd
                        ";;;  (The declaration will be ignored.)")
                    (setq body-forms (remove bd body-forms))))
            (setq lambda-form 
                `(lambda (form &optional env)
                    (declare (ignore env ,@(unless lambda-list '(form))))
                    (block ,name
                        ,@(if lambda-list
                            `((type-destructuring-bind 
                                    ,lambda-list 
                                    (cdr form) 
                                    ,@decls
                                    ,@body-forms))
                            `((locally ,@decls ,@body-forms))))))
            
            `(eval-when (:compile-toplevel :load-toplevel :execute)
                ,@doc-form
                (setf (type-expansion ',name) (function ,lambda-form))
                ',name)))) 

;;;; standard type specifiers
(declare-type-specifier array (x specifier)
	(declare (ignore specifier))
	(arrayp x))	;; redefined later

(declare-type-specifier atom (x specifier)
	(declare (ignore specifier))
	(not (consp x)))

(declare-type-specifier base-char (x specifier)
	(declare (ignore specifier))
	(characterp x))

(declare-type-specifier bignum (x specifier)
	(declare (ignore specifier))
	(bignump x))

(declare-type-specifier bit (x specifier)
	(declare (ignore specifier))
	(or (eq x 1) (eq x 0)))

(declare-type-specifier bit-vector (x specifier)
	(if (or (symbolp specifier)(null (cdr specifier)))
		(bit-vector-p x)
		(if (bit-vector-p x)
			(let ((size (cadr specifier)))
				(if (eq size '*)
					t
					(= (length x) size))))))

(declare-type-specifier character (x specifier)
	(declare (ignore specifier))
	(characterp x))

(declare-type-specifier compiled-function (x specifier)
	(declare (ignore specifier))
	(functionp x))

(declare-type-specifier complex (x specifier)
	(and (complexp x) 
		(let ((type (second specifier)))
			(or (null type)
				(and (typep (realpart x) type) (typep (imagpart x) type))))))

(declare-type-specifier cons (x specifier)
	(declare (ignore specifier))
	(consp x))

(declare-type-specifier double-float (x specifier)
	(declare (ignore specifier))
	(double-float-p x))

(declare-type-specifier single-float (x specifier)
	(declare (ignore specifier))
	(single-float-p x))

(declare-type-specifier short-float (x specifier)
	(declare (ignore specifier))
	(short-float-p x))

(declare-type-specifier long-float (x specifier)
	(declare (ignore specifier))
	(double-float-p x))

(declare-type-specifier extended-character (x specifier)
	(declare (ignore specifier x))
	nil)

(declare-type-specifier fixnum (x specifier)
	(cond
		((symbolp specifier) (fixnump x))
		((not (fixnump x)) nil)
		(t 
		 (let ((low (second specifier))
			   (high (third specifier)))
			(if (eq low '*) (setq low nil))
			(if (eq high '*)(setq high nil))
			(and (if low (>= x low) t) (if high (<= x high) t))))))

(declare-type-specifier float (x specifier)
	(declare (ignore specifier))
	(floatp x))

(declare-type-specifier function (x specifier)
	(declare (ignore specifier))
	(functionp x))

(declare-type-specifier hash-table (x specifier)
	(declare (ignore specifier))
	(hash-table-p x))

(declare-type-specifier integer (x specifier)
	(cond
		((symbolp specifier) (integerp x))
		((not (integerp x)) nil)
		(t 
		 (let ((low (second specifier))
			   (high (third specifier)))
			(if (eq low '*) (setq low nil))
			(if (eq high '*)(setq high nil))
                
            ;; handle exclusive specifiers (listed integer bound)
            (if (and (consp low)(integerp (car low)))
                    (setf low (+ (car low) 1)))
            (if (and (consp high)(integerp (car high)))
                    (setf high (- (car high) 1)))
                
			(and (if low (>= x low) t) (if high (<= x high) t))))))

(declare-type-specifier keyword (x specifier)
	(declare (ignore specifier))
	(and (symbolp x) (eq (symbol-package x) keyword-package)))

(declare-type-specifier list (x specifier)
	(declare (ignore specifier))
	(listp x))
			
(declare-type-specifier nil (x specifier)
	(declare (ignore specifier x))
	nil)

(declare-type-specifier null (x specifier)
	(declare (ignore specifier))
	(null x))

(declare-type-specifier number (x specifier)
	(declare (ignore specifier))
	(numberp x))

(declare-type-specifier package (x specifier)
	(declare (ignore specifier))
	(packagep x))

(declare-type-specifier pathname (x specifier)
	(declare (ignore specifier))
	(pathnamep x))

;; need to override warning here for RANDOM-STATE-P not defined yet
(setq *COMPILER-WARN-ON-UNDEFINED-FUNCTION* nil)
(declare-type-specifier random-state (x specifier)
	(declare (ignore specifier))
	(random-state-p x))
(setq *COMPILER-WARN-ON-UNDEFINED-FUNCTION* t)

(declare-type-specifier ratio (x specifier)
	(declare (ignore specifier))
	(ratiop x))

(declare-type-specifier rational (x specifier)
	(declare (ignore specifier))
	(rationalp x))

(declare-type-specifier readtable (x specifier)
	(declare (ignore specifier))
	(readtablep x))

(declare-type-specifier real (x specifier)
	(declare (ignore specifier))
	(realp x))

(declare-type-specifier sequence (x specifier)
	(declare (ignore specifier))
	(sequencep x))

(declare-type-specifier signed-byte (x specifier)
	(if (or (symbolp specifier)(null (cdr specifier)))
		(integerp x)
		(if (integerp x)
			(let ((size (cadr specifier)))
				(if (eq size '*)
					t
					(let ((limit (expt 2 (- size 1))))
						(and (> size 0) 
							(>= x (- limit))
							(< x limit))))))))

(declare-type-specifier simple-array (x specifier)
	(declare (ignore specifier))
	(arrayp x)) 	;; redefined later

(declare-type-specifier simple-bit-vector (x specifier)
	(if (or (symbolp specifier)(null (cdr specifier)))
		(simple-bit-vector-p x)
		(if (simple-bit-vector-p x)
			(let ((size (cadr specifier)))
				(if (eq size '*)
					t
					(= (length x) size))))))

(declare-type-specifier simple-string (x specifier)
	(if (or (symbolp specifier)(null (cdr specifier)))
		(simple-string-p x)
		(if (simple-string-p x)
			(let ((size (cadr specifier)))
				(if (eq size '*)
					t
					(= (length x) size))))))

(declare-type-specifier simple-vector (x specifier)
	(if (or (symbolp specifier)(null (cdr specifier)))
		(simple-vector-p x)
		(if (simple-vector-p x)
			(let ((size (cadr specifier)))
				(if (eq size '*)
					t
					(= (length x) size))))))

(declare-type-specifier standard-char (x specifier)
	(declare (ignore specifier))
	(characterp x))

(declare-type-specifier stream (x specifier)
	(declare (ignore specifier))
	(streamp x))

(declare-type-specifier string (x specifier)
	(if (or (symbolp specifier)(null (cdr specifier)))
		(stringp x)
		(if (stringp x)
			(let ((size (cadr specifier)))
				(if (eq size '*)
					t
					(= (length x) size))))))

(declare-type-specifier symbol (x specifier)
	(declare (ignore specifier))
	(symbolp x))

(declare-type-specifier t (x specifier)
	(declare (ignore specifier x))
	t)

(declare-type-specifier unsigned-byte (x specifier)
	(if (or (symbolp specifier)(null (cdr specifier)))
		(and (integerp x) (>= x 0))
		(if (and (integerp x) (>= x 0))
			(let ((size (cadr specifier)))
				(if (eq size '*)
					t
					(let ((limit (expt 2 size)))
						(and (> size 0)(< x limit))))))))

(declare-type-specifier vector (x specifier)
	(declare (ignore specifier))
	(vectorp x)) ;; redefined later

;;;; predicating type specifiers

(defun type-specifier-not-implemented (obj specifier)
	(declare (ignore obj))
	(error "Type specifier ~A is not implemented" specifier))

(declare-type-specifier satisfies (x specifier)
	(funcall (second specifier) x))

;;;; type specifiers that combine
(declare-type-specifier member (x specifier)
	(if (member x (cdr specifier) :test #'eql) t))

(declare-type-specifier eql (x specifier)
	(eql x (second specifier)))

(declare-type-specifier not (x specifier)
	(not (typep x (cadr specifier))))

(declare-type-specifier and (x specifier)
	(every #'(lambda (spec) (typep x spec)) (cdr specifier)))

(declare-type-specifier or (x specifier)
	(some #'(lambda (spec) (typep x spec)) (cdr specifier)))

;;;; type specifiers that specialize
(declare-type-specifier values (x specifier)
	(type-specifier-not-implemented x specifier))

;;;; type specifiers that abbreviate
(declare-type-specifier mod (x specifier)
	(and (integerp x)(not (minusp x))(< x (second specifier))))

(declare-type-specifier base-string (x specifier)
	(if (symbolp specifier)
		(stringp x)
		(if (consp specifier)
			(if (or (null (cdr specifier)) (eq (second specifier) '*))
				(stringp x)
				(and (stringp x)(= (length x) (second specifier)))))))

(declare-type-specifier simple-base-string (x specifier)
	(if (symbolp specifier)
		(stringp x)
		(if (consp specifier)
			(if (or (null (cdr specifier)) (eq (second specifier) '*))
				(stringp x)
				(and (stringp x)(= (length x) (second specifier)))))))

(declare-type-specifier boolean (x specifier)
	(declare (ignore specifier))
	(or (eq x 't) (eq x 'nil) x))


	