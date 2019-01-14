;;;; Unofficial patches to Corman Lisp 1.41
;;;;
;;;; Patch done by Chris Double - chris@double.nz
;;;; Available from http://www.double.nz/cl
;;;;
;;;; Additional official patches exist at http://www.corman.net 
;;;; that must be applied before applying this patch.
(in-package :common-lisp)

;; Fix to defpackage import-from keyword.
(defmacro defpackage (name &rest options)
	(if (symbolp name)
		(setq name (symbol-name name)))
	(unless (stringp name)
		(error "Invalid package name: ~A" name))
	(let ((forms nil)
		  (size nil)
		  (nicknames nil)
		  (shadow nil)
		  (shadowing-import-from nil)
		  (use default-packages)
		  (import-from nil)
		  (intern nil)
		  (export nil)
		  (documentation nil)
		  (package (find-package name)))
		(declare (ignore size))

		(do* ((p options (cdr p))
			  (option (caar p) (caar p))
			  (value (cdar p) (cdar p)))
			((null p))
			(ecase option
				(:size (setq size value))
				(:nicknames (setq nicknames value))
				(:shadow (setq shadow value))
				(:shadowing-import-from (setq shadowing-import-from value))
				(:use (setq use value))
				(:import-from (setq import-from value))
				(:intern (setq intern value))
				(:export (setq export value))
				(:documentation (setq documentation value))))
		(unless package
			(push `(unless (find-package ,name)
					(make-package ,name :nicknames ',nicknames :use nil)) forms))		
		(if shadow
			(push `(shadow ',shadow ,name) forms))
		(if shadowing-import-from
			(push `(shadowing-import-from ',shadowing-import-from ,name) forms))
		(if use
			(push `(use-package ',use ,name) forms))
		(if import-from
			(let* ((package (car import-from))
					(symbols (mapcar #'(lambda (x) 
								(find-symbol x package)) (cdr import-from))))
				(push `(import ',symbols ,name) forms)))
		(if intern
			(dolist (sym intern)
				(push `(intern ',sym ,name) forms)))
		(if export
			(dolist (sym export)
				(push `(export-create ',sym ,name) forms)))
		(if documentation
			(push `(setf (documentation ,name 'package) documentation) forms))
		(push `(find-package ,name) forms)

		`(eval-when (:load-toplevel :compile-toplevel :execute)
			,@(nreverse forms))))
(in-package :common-lisp-user)

