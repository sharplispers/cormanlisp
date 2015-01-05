;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		trees.lisp
;;;;	Contents:	Corman Lisp tree manipulation functions.
;;;;	History:	6/1/01  RGC  Created.
;;;;

(in-package :common-lisp)

;;;
;;;	Common Lisp SUBST function.
;;;
(defun subst (new old tree &key (key nil) (test #'eql) (test-not nil))
	(if test-not
		(setq test #'(lambda (x y) (not (funcall test-not x y)))))
	(cond ((funcall test old (if key (funcall key tree) tree)) new)
		  ((consp tree)
		   (let ((a (subst new old (car tree) :key key :test test))
				 (d (subst new old (cdr tree) :key key :test test)))
				(if (and (eql a (car tree))(eql d (cdr tree)))		;; avoid unnecessary consing
					tree
					(cons a d))))
		  (t tree)))

;;;
;;;	Common Lisp SUBST-IF function.
;;;
(defun subst-if (new predicate tree &key (key nil))
	(cond ((funcall predicate (if key (funcall key tree) tree)) new)
		  ((consp tree)
		   (let ((a (subst-if new predicate (car tree) :key key))
				 (d (subst-if new predicate (cdr tree) :key key)))
				(if (and (eql a (car tree))(eql d (cdr tree)))		;; avoid unnecessary consing
					tree
					(cons a d))))
		  (t tree)))

;;;
;;;	Common Lisp SUBST-IF-NOT function.
;;;
(defun subst-if-not (new predicate tree &key (key nil))
	(cond ((not (funcall predicate (if key (funcall key tree) tree))) new)
		  ((consp tree)
		   (let ((a (subst-if-not new predicate (car tree) :key key))
				 (d (subst-if-not new predicate (cdr tree) :key key)))
				(if (and (eql a (car tree))(eql d (cdr tree)))		;; avoid unnecessary consing
					tree
					(cons a d))))
		  (t tree)))

;;;
;;;	Common Lisp NSUBST function.
;;;
(defun nsubst (new old tree &key (key nil) (test #'eql) (test-not nil))
	(if test-not
		(setq test #'(lambda (x y) (not (funcall test-not x y)))))
	(cond ((funcall test old (if key (funcall key tree) tree)) new)
		  ((consp tree)
		   (setf (car tree) (nsubst new old (car tree) :key key :test test))
		   (setf (cdr tree) (nsubst new old (cdr tree) :key key :test test))
		   tree)
		  (t tree)))

;;;
;;;	Common Lisp NSUBST-IF function.
;;;
(defun nsubst-if (new predicate tree &key (key nil))
	(cond ((funcall predicate (if key (funcall key tree) tree)) new)
		  ((consp tree)
		   (setf (car tree) (nsubst-if new predicate (car tree) :key key))
		   (setf (cdr tree) (nsubst-if new predicate (cdr tree) :key key))
		   tree)
		  (t tree)))

;;;
;;;	Common Lisp NSUBST-IF-NOT function.
;;;
(defun nsubst-if-not (new predicate tree &key (key nil))
	(cond ((not (funcall predicate (if key (funcall key tree) tree))) new)
		  ((consp tree)
		   (setf (car tree) (nsubst-if-not new predicate (car tree) :key key))
		   (setf (cdr tree) (nsubst-if-not new predicate (cdr tree) :key key))
		   tree)
		  (t tree)))


