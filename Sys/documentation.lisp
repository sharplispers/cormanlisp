;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		documentation.lisp
;;;;	Contents:	Corman Lisp DOCUMENTATION function.
;;;;	History:	2/11/99  RGC  Created.
;;;;
(in-package :common-lisp)

;;;
;;;	Corman Lisp  DOCUMENTATION function
;;;
(defun documentation (symbol &optional (type 'function))
	(if (eq (symbol-package symbol) (find-package :common-lisp))
		(unless (and (boundp 'pl::*hyperspec-loaded*) pl::*hyperspec-loaded*)
			(load (concatenate 'string pl:*cormanlisp-directory* "/sys/hyperspec.lisp"))))

	(let ((doclist (gethash symbol *documentation-registry*))
		  doc-clause)
		
		(if (eq (symbol-package symbol) (find-package :common-lisp))
			(unless (and (boundp 'pl::*hyperspec-loaded*) pl::*hyperspec-loaded*)
				(load (concatenate 'string pl:*cormanlisp-directory* "/sys/hyperspec.lisp"))))

		;; if the requested symbol is in the common-lisp package, and
		;; has documentation of type hyperspec as the first type, then
		;; use a special algorithm to display the information from the hyperspec
		(if (and (eq (car doclist) ':hyperspec) 
				(eq (symbol-package symbol) (find-package 'common-lisp)))
			(setq type ':hyperspec))
		(setq doc-clause (getf doclist type))
		(unless doc-clause 
			(return-from documentation (format nil "No documentation available for ~A ~A" type symbol)))
		(if (eq type ':hyperspec)
			(progn (pl:hyperspec symbol) (values))
			;; else just return the doc string
			doc-clause
		#|
			(progn
				(win::message-box-ok doc-clause 
					(format nil "Documentation for ~A ~A" type symbol))
				(values))
		|# )))
