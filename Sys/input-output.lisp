;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		input-output.lisp
;;;;	Contents:	Corman Lisp I/O functions.
;;;;	History:	3/2/97  RGC  Created.
;;;;
(in-package :common-lisp)

(defvar *save-image-cleanup-funcs* (symbol-value '*save-image-cleanup-funcs*))
(defvar *load-image-restore-funcs* (symbol-value '*load-image-restore-funcs*))

(defun register-save-image-cleanup-func (func)
	(setq *save-image-cleanup-funcs* 
		(adjoin func *save-image-cleanup-funcs*)))

(defun register-load-image-restore-func (func)
	(setq *load-image-restore-funcs* 
		(adjoin func *load-image-restore-funcs*)))

;; since the time units per second may vary on a per machine basis
;; we need to make sure this is reset after an image is loaded
(register-load-image-restore-func 
	#'(lambda () 
		(setf internal-time-units-per-second 
			(get-internal-time-units-per-second))
		(setf gc-time-units-per-second 
			internal-time-units-per-second)))

;;; ensure all packages have synchronization objects
(register-load-image-restore-func 
	#'(lambda () 
		(dolist (p (list-all-packages))
			(unless (package-sync p)
				(setf (uref p package-sync-offset) (allocate-critical-section))))))

;;; remove all critical sections from packages when the image is saved
(register-save-image-cleanup-func 
	#'(lambda () 
		(dolist (p (list-all-packages))
			(when (package-sync p)
				(deallocate-critical-section (package-sync p))
				(setf (uref p package-sync-offset) nil)))))
