;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		declarations.lisp
;;;;	Contents:	Corman Lisp declaration functions.
;;;;	History:	3/30/97  RGC  Created.
;;;;
(in-package :common-lisp)

(defun proclaim-specials (syms)
	(dolist (s syms)
		(symbol-set-special-flag s)))

(let ((ftype-proclamations (make-hash-table)))
	(defun proclaim-ftype (ftype syms)
		(dolist (s syms)
			(setf (gethash s ftype-proclamations) ftype)))
	(defun lookup-ftype (sym)
		(gethash sym ftype-proclamations)))

(let ((inline-proclamations (make-hash-table)))
	(defun proclaim-inline (funcs)
		(dolist (x funcs)
			(if (and (consp x) (eq (car x) 'setf))
				(setf x (cl::get-setf-function (second x))))
			(setf (gethash x inline-proclamations) t)))
	(defun proclaim-notinline (funcs)
		(dolist (x funcs)
			(if (and (consp x) (eq (car x) 'setf))
				(setf x (cl::get-setf-function (second x))))
			(setf (gethash x inline-proclamations) nil)))
	(defun inline-function-p (sym)
		(if (and (consp sym) (eq (car sym) 'setf))
			(setf sym (cl::get-setf-function (second sym))))
		(and (symbolp sym)(fboundp sym)(gethash sym inline-proclamations)))
	(defun inline-proclaim-p (sym)
		(if (and (consp sym) (eq (car sym) 'setf))
			(setf sym (cl::get-setf-function (second sym))))
		(and (symbolp sym)(gethash sym inline-proclamations))))	

(defun proclaim-optimize-decls (decls)
	(dolist (x decls)
		(unless (typep (second x) '(integer 0 3))
			(error "Invalid optimizer setting for OPTIMIZE ~A" (car x)))
		(case (car x)
			(speed (setf pl::*compiler-optimize-speed* (second x)))
			(safety (setf pl::*compiler-optimize-safety* (second x)))
			(space (setf pl::*compiler-optimize-space* (second x)))
			(debug (setf pl::*compiler-optimize-debug* (second x))))))
		
;;;
;;;	Common Lisp PROCLAIM function.
;;;
(defun proclaim (decl-spec)
	(let ((sym (car decl-spec)))
		(case sym
			(special (proclaim-specials (cdr decl-spec)))
			(ftype (proclaim-ftype (cadr decl-spec) (cddr decl-spec)))
			(inline (proclaim-inline (cdr decl-spec)))
			(notinline (proclaim-notinline (cdr decl-spec)))
			(optimize (proclaim-optimize-decls (cdr decl-spec)))
			(otherwise nil))
		t))		;not sure what to return here

;;;
;;;	Common Lisp DECLAIM macro.
;;;
(defmacro declaim (&rest decl-specs)
	`(eval-when (:compile-toplevel :load-toplevel :execute)
		(proclaim ',@decl-specs)))

;;; Conform to ANSI spec by allowing the #'(lambda ()) to be
;;; replaced by (lambda ())
;(setq *compiler-warn-on-dynamic-return* nil)	;; disable for this macro only
(defmacro lambda (&whole form &rest bvl-decls-and-body)
	(declare (ignore bvl-decls-and-body))
	`#',form)
;(setq *compiler-warn-on-dynamic-return* t)	;; disable for this macro only

(defun intersection (list-1 list-2 
				&key (key nil)
					(test #'eql)
					(test-not nil))
	(if test-not
		(setq test #'(lambda (x y) (not (funcall test-not x y)))))
	(let ((newlist nil))
		(dolist (x list-1)
			(if key (setq x (funcall key x)))
			(if (member x list-2 :test test :key key)
				(push x newlist)))
		(nreverse newlist)))

(defun inline-expand (expr &optional env)
	(declare (ignore env))
	(if (consp expr)
		(let ((sym (car expr)))
			(if (and (symbolp sym) (inline-function-p sym))
				(let* ((func (symbol-function sym))
					   (lambda (function-lambda-expression func)))
					(unless lambda 
						(warn "Call to function ~S was not inlined because the ~ 
							source definition was not available" sym)
						(return-from inline-expand expr))
					(let* ((lambda-list (second lambda))
						   (forms (cddr lambda)) 
					   	   (arg-forms nil)
						   (arg-exprs (cdr expr)))
						(unless (= (length lambda-list)(length arg-exprs))
							(warn "Call to function ~S was not inlined because the 
								number of passed parameters did not match the number of 
								required arguments" sym)
							(return-from inline-expand expr))	;; only inline if number of args matches						
						(unless (null (function-environment func))
							(warn "Call to function ~S was not inlined because it was defined 
								in a non-null lexical environment" sym)
							(return-from inline-expand expr))	;; only inline if null lexical environment						
						(when (intersection cl::lambda-list-keywords lambda-list)
							(warn "Call to function ~S was not inlined because it can take &optional, &rest, &key, or &aux arguments" 
								sym)
							(return-from inline-expand expr))	;; only inline if only required args are used						
						(do* ((x arg-exprs (cdr x))
							  (lambda-var lambda-list (cdr lambda-var)))
							((null x))
							(push `(,(car lambda-var) ,(car x)) arg-forms))
						(return-from inline-expand `(let ,(nreverse arg-forms) 
								(with-only-lexicals ,lambda-list
									,@forms))))))))
	expr)
