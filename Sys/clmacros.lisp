;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;		File:		clmacros.lisp
;;;;		Contents:	Corman Lisp macro definitions.
;;;;		History:	10/16/96  RGC  Created.
;;;;                    04/02/03  RGC  Mods to CASE, TYPECASE by JP Massar.
;;;;

;;;;
;;;;	Common Lisp CASE macro
;;;;
(defmacro case (key &rest cases)
	(let* ((newsym (gensym))
		   (init `((,newsym ,key)))
		   (expr (list 'cond))
		   keylis)
		(dolist (clause cases)
			(unless (consp clause) (error "Invalid CASE clause: ~A" clause))
			(setq keylis (car clause))
			(if (eq keylis 'otherwise)
				(push (cons t (cdr clause)) expr)
				(if (eq keylis t)
					(push clause expr)
					(let (member)
						(if (not (listp keylis))
							(setq keylis (list keylis)))
						(setq member `(member ,newsym ',keylis))
						(if (consp (cdr clause))
							(push (cons member (cdr clause)) expr)
							(push (list member nil) expr))))))
		`(let ,init ,(nreverse expr))))

(defmacro typecase (keyform &rest clauses)
    (let ((keyform-symbol (gensym "KEYFORM-")))
        `(let ((,keyform-symbol ,keyform))
            (cond
                ,@(mapcar
                    #'(lambda (clause)
                        (unless (listp clause)
                                (error "Bad syntax in TYPECASE clause"))
                        (let ((type (first clause))
                              (actions (or (rest clause) (list nil))))
                            (if (or (eq type t) (eq type 'otherwise))
                                `(t ,@actions)
                                `((typep ,keyform-symbol ',type) ,@actions))))
                    clauses)))))

;;;
;;;	Simple version of Common Lisp LOOP macro.
;;;
(defmacro loop (&rest forms)
	(let ((sym (gensym)))
		`(block nil (tagbody ,sym ,@forms (go ,sym)))))

;;;
;;;	Common Lisp NTH-VALUE macro.
;;;
(defmacro nth-value (n form)
	(let ((sym1 (gensym))
		  (sym2 (gensym)))
		`(let* ((,sym1 ,n)
			    (,sym2 (nth ,sym1 (multiple-value-list ,form))))
			,sym2)))

