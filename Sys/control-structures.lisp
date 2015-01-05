;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		control-structures.lisp
;;;;	Contents:	Corman Lisp library functions.
;;;;	History:	3/2/97  RGC  Created.
;;;;				8/21/01 RGC  Added SHIFTF.
;;;;							 Improved ROTATEF.
;;;;

(in-package "COMMON-LISP")

;;; 
;;; Common Lisp GET-SETF-EXPANSION function.
;;;
(defun get-setf-expansion (place &optional environment)
	(declare (ignore environment))
	(let* ((subforms (if (consp place) (cdr place)))
		   (vars '())
		   (vals '())
		   (store-vars (list (gensym)))
		   (writer-form nil)
		   (reader-form nil))
		(if (symbolp place)
			(return-from get-setf-expansion 
				(values 
					nil 
					nil 
					store-vars 
					`(setq ,place ,(car store-vars)) 
					place)))
		(if (and (consp place)		;; check for APPLY special case
				(eq (car place) 'apply)
				(consp (cadr place))
				(or (eq (caadr place) 'quote)(eq (caadr place) 'function)))
			(let ((name (cadadr place)))
				(setq subforms (cdr subforms))
				(dolist (x subforms)
					(push (gensym) vars)
					(push x vals))
				(setf vars (nreverse vars) vals (nreverse vals))
				(setf reader-form `(apply ,(cadr place) ,@vars))
				(setq writer-form
                    (if (setf-function-value-last-p (car place)) 
					   `(apply (quote ,(get-setf-function name)) ,@vars ,(car store-vars))
                       `(apply (quote ,(get-setf-function name)) ,(car store-vars) ,@vars))))
			(let ()
				(dolist (x subforms)
					(push (gensym) vars)
					(push x vals))
				(setf vars (nreverse vars) vals (nreverse vals))
				(setf reader-form (cons (car place) vars))
				(setf writer-form 
                    (if (setf-function-value-last-p (car place))
                        `(,(get-setf-function (car place)) ,@vars ,(car store-vars))
                        `(,(get-setf-function (car place)) ,(car store-vars) ,@vars)))))
		(values vars vals store-vars writer-form reader-form)))

;;;
;;; Common Lisp DEFINE-MODIFY-MACRO function.
;;; Adapted from CMU Lisp.
;;;
(defmacro define-modify-macro (name lambda-list function &optional
                               doc-string)
  "Creates a new read-modify-write macro like PUSH or INCF."
  (let ((other-args nil)
        (rest-arg nil)
        (env (gensym))
        (reference (gensym)))
    (do ((ll lambda-list (cdr ll)) (arg nil))
        ((null ll))
      (setq arg (car ll))
      (cond ((eq arg '&optional))
            ((eq arg '&rest)
             (if (symbolp (cadr ll))
                 (setq rest-arg (cadr ll))
                 (error "Non-symbol &rest arg in definition of ~S."
                        name))
             (if (null (cddr ll))
                 (return nil)
                 (error "Illegal stuff after &rest arg in DEFINE-MODIFY-MACRO.")))
            ((member arg '(&key &allow-other-keys &aux) :test 'eq)
             (error "~S not allowed in DEFINE-MODIFY-MACRO lambda list."
                    arg))
            ((symbolp arg) (push arg other-args))
            ((and (listp arg) (symbolp (car arg)))
             (push (car arg) other-args))
            (t
             (error "Illegal stuff in lambda list of DEFINE-MODIFY-MACRO."))))
    (setq other-args (nreverse other-args))
    `(defmacro ,name (,reference ,@lambda-list &environment ,env)
       ,doc-string
       (multiple-value-bind (dummies vals newval setter getter)
           (get-setf-expansion ,reference ,env)
         (do ((d dummies (cdr d))
              (v vals (cdr v))
              (let-list nil (cons (list (car d) (car v)) let-list)))
             ((null d)
              (push (list (car newval)
                          ,(if rest-arg
                               `(list* ',function
                                       getter
                                       ,@other-args
                                       ,rest-arg)
                               `(list ',function
                                      getter
                                      ,@other-args)))
                    let-list)
              `(let* (common-lisp::%comma (nreverse let-list))
                 ,setter)) )))))

;;;
;;; Common Lisp ROTATEF macro.
;;;
(defmacro rotatef (&rest place-forms)
	(if (null place-forms)
		(return-from rotatef 'nil))
	(let* ((subform-temps '())
		   (new-places '()))
		(dolist (x place-forms)
			(if (consp x)
				(let ((res (cl::%once-only-forms x)))
					(dolist (y (car res))
						(push y subform-temps))
					(push (cdr res) new-places))
				(push x new-places)))
		(setf subform-temps (nreverse subform-temps) 
			  new-places (nreverse new-places))
		(let ((temps '())
			  (place1 (first new-places)))
			(do ((x new-places (cdr x)))
				((null (cdr x))(push (car x) temps)(push place1 temps))
				(push (car x) temps)
				(push (cadr x) temps))
			(if subform-temps
				`(let ,subform-temps
					(psetf ,@(nreverse temps)))
				`(psetf ,@(nreverse temps))))))

;;;
;;; Common Lisp SHIFTF macro.
;;;
(defmacro shiftf (&rest forms)
	(if (null forms)
		(error "Not enough arguments to macro SHIFTF"))
	(let* ((place-forms (butlast forms))
		   (new-value-form (car (last forms)))
		   (subform-temps '())
		   (new-places '()))
		(dolist (x place-forms)
			(if (consp x)
				(let ((res (cl::%once-only-forms x)))
					(dolist (y (car res))
						(push y subform-temps))
					(push (cdr res) new-places))
				(push x new-places)))
		(setf subform-temps (nreverse subform-temps) 
			  new-places (nreverse new-places))
		(let ((temps '())
			  (setf-forms '())
			  (vars '())
			  (var1 nil))
			(dotimes (i (length forms))
				(push (gensym) vars))
			(setf vars (nreverse vars))
			(setf var1 (first vars))
			(dolist (x new-places)
				(push `(,(first vars) ,x) temps)
				(push `(setf ,x ,(second vars)) setf-forms)
				(setf vars (cdr vars)))
			(push `(,(car vars) ,new-value-form) temps)
			(if subform-temps
				`(let ,subform-temps
					(let ,(nreverse temps)
						,@setf-forms
						,var1))
				`(let ,(nreverse temps)
						,@setf-forms
						,var1)))))
