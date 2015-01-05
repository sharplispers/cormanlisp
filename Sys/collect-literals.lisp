;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:	collect-literals.lisp
;;;;

(defun is-heap-literal (x) 
	(cond 
		((symbolp x) nil)
		((fixnump x) nil)
		((characterp x) nil)
		((listp x) (if (eq (car x) 'quote) t nil))
		(t t)))

(defvar *collected-literals* nil)

(defun extract-literals (x)
	(if (is-heap-literal x)
		(let ((sym (gensym)))
			(push (list sym x) *collected-literals*)
			(setq x sym))
		(if (consp x)
			(do ((f x (cdr f)))
				((not (consp (cdr f)))
				 (rplaca f (extract-literals (car f)))
				 (if (cdr f) (rplacd f (extract-literals (cdr f)))))
				(rplaca f (extract-literals (car f))))))
	x)

(defun collect-literals (x)
	(let ((*collected-literals* nil))
		(setq x (extract-literals x))
		(if (null *collected-literals*)
			x
			`(let (,@*collected-literals*)
				,x))))
	