;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		synchronized.lisp
;;;;	Contents:	A number of library functions are redefined here
;;;;                to add 
;;;;	History:	4/24/02 RGC  Created.
;;;;
(in-package :cl)

;;; ensure all packages have synchronization objects
(register-load-image-restore-func 
	#'(lambda () 
		(dolist (p (list-all-packages))
			(unless (package-sync p)
				(setf (uref p package-sync-offset) (cl::allocate-critical-section))))))

;;; remove all critical sections from packages when the image is saved
(register-save-image-cleanup-func 
	#'(lambda () 
		(dolist (p (list-all-packages))
			(when (package-sync p)
				(cl::deallocate-critical-section (package-sync p))
				(setf (uref p package-sync-offset) nil)))))

;;;;
;;;;	Common Lisp FIND-SYMBOL function
;;;;
(defun find-symbol (string &optional (package *package*))
	(unless (packagep package)
		(setq package (find-package package)))
	(unless (packagep package)
		(error "Invalid package: ~A" package))
	(multiple-value-bind (sym status)
		(package-find-symbol package string)
		(if status
			(cond 
				((eq status 'internal)(values sym :internal))
				((eq status 'external)(values sym :external))
			    (t (values sym :inherited)))
			(values nil nil))))


