;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		tail-calls.lisp
;;;;	Contents:	Tail recursion elimination optimization.
;;;;	History:	6/3/01   RGC  Created.
;;;;

;;
;; Determines if the passed lambda has a tail-recursive call.
;; If so, it returns a list of all the tail calls, otherwise it
;; returns NIL.
;; Since we don't support tail recursion optimization on any functions
;; which take &optional, &rest, &key or &aux parameters, any such functions
;; will also cause NIL to be returned.
;;
;; This algorithm determines that something is a tail call if the following
;; conditions are met:
;; The form is a direct function-call form (not using APPLY or FUNCALL)
;; and the name of the function is the same name as the passed argument.
;; The form is a terminating form in the lambda (if evaluated, it will always
;; be the last form evaluated, and return the resulting value(s)).
;; 
;; A form is considered a terminating form if it is one of the following:
;; -The form is the last form in the lambda.
;; -The form's value is returned as the return value of the outermost block
;;  in the lambda (using RETURN-FROM).
;; -The form is the 'then' part or the 'else' part of an IF special form,
;;  and the IF form is a terminating form.
;; -The form is the last form in a PROGN, which is a terminating form.
;; -The form is the last form in a BLOCK, TAGBODY, or LET body, and the
;;  BLOCK, TAGBODY, or LET form is a terminating form.
;;
;; The lambda is assumed to have all macros expanded prior to the call
;; i.e. there should be no macros (such as RETURN, COND, etc.) remaining.
;;

(in-package :cl)

(defparameter *optimize-tail-recursion* t)

;;;
;;; Search a block for RETURN-FROM forms which will return from that block.
;;;
;;; When we found a block: 
;;; 	If the target label is NIL, there is no need
;;; 	to analyze this block (no way to return from within it to the target).
;;;		If the target label is not NIL, and this label matches (shadows) the
;;;		target label, we also stop. Otherwise, add this label to the list and
;;; 	search the block.
;;;
;;; When we find a RETURN-FROM form:
;;;		If the label is NIL, and there is only one label in scope,
;;;		then it is a RETURN to the target label. If the label is not
;;;		NIL, then it only returns to the target label if it matches the
;;;		target label.
;;;		In either case, we still need to traverse the evaluated return value
;;;		of the form.

(defun traverse-form (form labels-list result)
	(unless (consp form)			;; we only analyze lists (conses)
		(return-from traverse-form result))
	(case (first form)
		(quote nil)		;; do nothing
		(block
			(let ((target (first (last labels-list))))
				(when (not (or (null target) (eq (second form) target)))
					(dolist (x (cddr form))
						(setq result (traverse-form x (cons (second form) labels-list) result))))))
		(return-from
			(if (null (second form))
				(if (= (length labels-list) 1)
					(push form result))
				(if (eq (second form) (first (last labels-list)))
					(push form result)))
			(setq result (traverse-form (third form) labels-list result)))
		((let let*)
		 	(let ((decls (second form)))
				(dolist (decl decls)
                    ;; if any of the variables being bound are special (globally)
                    ;; then we won't find any tail calls in this form (they have an implicit
                    ;; operation following to clear the binding).
                    ;; TDE: We need to do the same for locally declared specials--but are not checking yet.
                    ;;
                    (if (or (and (symbolp decl) (cl::symbol-special-p decl))
                            (and (consp decl) (symbolp (car decl)) (cl::symbol-special-p (car decl))))
                        nil)
                    
                    ;; traverse any initializing forms
					(if (and (consp decl) (cdr decl))
						(setq result (traverse-form (second decl) labels-list result))))
				(dolist (y (cddr form))
					(setq result (traverse-form y labels-list result)))))
		(lambda
			(let ((params (second form)))
				(dolist (p params)
					(if (and (consp p) (cdr p))
						(setq result (traverse-form (second p) labels-list result))))
				(dolist (y (cddr form))
					(setq result (traverse-form y labels-list result)))))
		(declare nil)	;; ignore declarations			
		(otherwise
			(dolist (x (cdr form))
				(setq result (traverse-form x labels-list result)))))
	result)

(defun block-return-from-forms (block)
	(let ((result '()))
		(dolist (x (cddr block) result)
			(setq result (traverse-form x (list (second block)) result)))))

;; returns all the tail forms in form (appended to the beginning
;; of the passed list).
(defun tail-forms (form form-list)
	(unless (consp form)
		(return-from tail-forms (cons form form-list)))
	(let* ((head (car form)))
		(case head
			(lambda  		(tail-forms (car (last (cddr form))) form-list))
			((progn tagbody)(tail-forms (car (last (cdr form))) form-list))
		 	((let let*)
                (let ((decls (second form)))
    				(dolist (decl decls)
                        ;; if any of the variables being bound are special (globally)
                        ;; then we won't find any tail calls in this form (they have an implicit
                        ;; operation following to clear the binding).
                        ;; TDE: We need to do the same for locally declared specials--but are not checking yet.
                        ;;
                        (if (or (and (symbolp decl) (cl::symbol-special-p decl))
                                (and (consp decl) (symbolp (car decl)) (cl::symbol-special-p (car decl))))
                            (return-from tail-forms form-list))))
                (tail-forms (car (last (cddr form))) form-list))
		 	(block   		(append 
								(mapcar 'third (block-return-from-forms form)) 
								(tail-forms (car (last form)) form-list)))
			(quote   		(cons form form-list))
			(if      		(tail-forms (fourth form) (tail-forms (third form) form-list)))
			(function		(tail-forms (second form) form-list))
			(otherwise 		(cons form form-list)))))

(defun has-embedded-lambdas (form)
    (labels ((search-for-lambdas (x)
                (cond
                    ((not (consp x)) nil)
                    ((is-lambda-form x) (return-from has-embedded-lambdas 't))
                    (t (let ((head (car x)))
                            (cond 
                                ((and (or (eq head 'flet) (eq head 'labels))
                                        (consp (cdr x)) 
                                        (consp (cadr x)))
                                    (return-from has-embedded-lambdas 't))
                                ((eq head 'quote) nil)
                                (t (search-for-lambdas head)(search-for-lambdas (cdr x)))))))))
        (search-for-lambdas form)))
        
(defun ignore-warning (condition)
    (declare (ignore condition))
    (muffle-warning))
  
(defun has-tail-recursive-calls (lambda name)
	(let ((lambda-list (second lambda)))
		(dolist (x lambda-list)
			(if (or (member x '(&optional &rest &key &aux))
					(cl::symbol-special-p x))	;; watch for special variables
				(return-from has-tail-recursive-calls nil)))
        
        ;; If there are any embedded lambdas, implicit capture of 
        ;; lexical variable bindings are changed by tail recursion
        ;; elimination. For now, just bail on tail call optimization
        ;; if we find any embedded lambdas which capture variables from
        ;; outer scope. 
        ;; To determine if this is the case, we first see if there are any
        ;; embedded lambdas. If so, we compile the whole thing by calling
        ;; EVAL on the lambda expression, and use a callback to return
        ;; us a list of captured variables. If there are any in this list,
        ;; then we bail on optimization.
        (if (has-embedded-lambdas (cddr lambda))
            (let ((captured-vars nil))
                (let ((cl::*CAPTURED-LEXICAL-CALLBACK* (lambda (var) (push var captured-vars)))
                      (*OPTIMIZE-TAIL-RECURSION* nil))
                    (handler-bind ((warning #'ignore-warning))
                         (eval lambda)))
                 (if captured-vars
                    (return-from has-tail-recursive-calls nil))))
        
		(let ((tail-forms (tail-forms lambda nil)))
			(let* ((tail-recursive-calls '()))
				(dolist (x tail-forms tail-recursive-calls)
					(if (and (consp x) (eq (car x) name))
						(push x tail-recursive-calls)))))))

(defun transform-tail-call (form start-sym lambda-list)
	(unless (= (length lambda-list) (1- (length form)))
		(error "Invalid number of arguments to recursive call to function ~A" (car form)))
	(let ((decl-forms '())
		  (set-forms '()))
		(do* ((x (cdr form) (cdr x))
			  (g (gensym)(gensym))
			  (i 0 (+ i 1)))
			((null x))
			(push `(x86::set-arg ,i ,g) set-forms)
			(push `(,g ,(car x)) decl-forms))
		`(let* (,@(nreverse decl-forms))
			,@(nreverse set-forms)
			(go ,start-sym))))

(defun remove-tail-recursion (lambda name)
	(let ((tail-recursive-forms (has-tail-recursive-calls lambda name)))
		(unless tail-recursive-forms
			(return-from remove-tail-recursion lambda))		;; don't do anything
		(let ((tag-sym (gensym))
			  (block-sym (gensym))
			  (lambda-list (second lambda))
			  (forms (cddr lambda))
			  (decls '()))
			
			;; skip any DECLARE forms
			(do ((f forms (cdr f)))
				((null f))
				(when (or (not (consp (car f))) (not (eq (caar f) 'declare)))
					(setf forms f)
					(return))
				(push (car f) decls))

			;; if any of the lambda-list variables have been declared special,
			;; bail on optimization
			(dolist (decl decls)
				(dolist (clause (cdr decl))
					(if (and (consp clause) (eq (car clause) 'special))
						(dolist (x (cdr clause))
							(if (member x lambda-list)
								(return-from remove-tail-recursion lambda))))))
                        			
			(dolist (x tail-recursive-forms)
				(setf forms (subst (transform-tail-call x tag-sym lambda-list) x forms :test #'eq)))
			(let ((x 
				`(lambda ,(second lambda) 
					,@(nreverse decls)
					(block ,block-sym 
						(tagbody ,tag-sym 
							(return-from ,block-sym
								(progn ,@forms)))))))
				(when *compile-verbose*
					(format t "Performing tail recursion elimination on function ~A~%" name))
				x))))
 
(defun compiler-optimize-tail-recursion () *optimize-tail-recursion*)

#|
(defun cl::%THROW-SYSTEM-EXCEPTION (ex)
	(if pl::*TRACE-EXCEPTIONS*
		(setf cl::*ERROR-TRACE* (cl::STACK-TRACE)))
	(cl::%THROW_EXCEPTION :system-exception ex 1))
|#

