;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;		File:		symbols.lisp
;;;;		Contents:	Corman Lisp symbol-related functions.
;;;;		History:	3/2/97  	RGC  Created.
;;;;					11/11/98	RGC  DEFPARAMETER and DEFVAR now handle doc string correctly.
;;;;					2/1/01      RGC  Added Frank Adrian's patch for DEFPARAMETER and DEFVAR
;;;;

(in-package :common-lisp)

(defun get-properties (place indicator-list)
	(do ((n place (cddr n)))
		((< (length n) 2) (values nil nil nil))
		(let ((x (member (car n) indicator-list)))
			(if x
				(return (values (car n) (cadr n) n))))))

;;;
;;;	Common Lisp (SETF SYMBOL-VALUE) macro.
;;;
(defun (setf symbol-value) (val sym)
	(unless (symbolp sym)
		(error "Not a symbol: ~A" sym))
	(if (constantp sym)
		(error "The symbol ~A has been declared constant, and may not be assigned to" sym))
	(rplaca (uref sym symbol-value-offset) val)
	val)
	 
(defun define-constant (sym val doc-string)
	(if (and (constantp sym)(not (equal (symbol-value sym) val)))
		(warn "Redefining constant ~A with previous value ~A and new value ~A"
			sym (symbol-value sym) val))
			 
	(let ()
		(%symbol-set-flags 0 sym)
		(setf (symbol-value sym) val)
		(symbol-set-special-flag sym)
		(symbol-set-constant-flag sym)
		(if doc-string
			(setf (documentation sym 'variable) doc-string))
		sym))

(defun define-special-variable (sym doc-string)			 
	(let ()
		(%symbol-set-flags 0 sym)
		;(setf (symbol-value sym) val)
		(cl::symbol-set-special-flag sym)
		(if doc-string
			(setf (documentation sym 'variable) doc-string))
		sym))
		 
;;;
;;;	Common Lisp DEFCONSTANT macro.
;;;
(defmacro defconstant (sym val &optional doc-string) 
	`(eval-when (:compile-toplevel :load-toplevel :execute) 
        (define-constant ',sym ,val ,doc-string)))

;;;
;;;	Common Lisp DEFPARAMETER macro.
;;;
(defmacro defparameter (sym val &optional doc-string) 
	(let ((_ (gensym)))
		`(let ((,_ (define-special-variable ',sym ,doc-string)))
			(setf (symbol-value ',sym) ,val) ,_)))

;;;
;;;	Common Lisp DEFVAR macro.
;;;
(defmacro defvar (sym &rest val-and-doc-string) 
	(let* ((_res (gensym))
		   (_has-val (not (null val-and-doc-string)))
		   (_doc (when (and _has-val
						(consp val-and-doc-string)
						(consp (cdr val-and-doc-string)))
						(cadr val-and-doc-string))))
		`(let ((,_res (define-special-variable ',sym ,_doc)))
			(when (and (not (boundp ',sym))
					,_has-val) (setf (symbol-value ',sym) ,(car val-and-doc-string)))
			,_res)))
