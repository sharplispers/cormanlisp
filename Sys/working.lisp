;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		working.lisp
;;;;	Contents:	Functions that are currently under development
;;;;	History:	1/27/99  RGC  Created.
;;;;
;;;

(defun function-code-buffer (func)
	(check-type func function)
	(if (ccl::kernel-function-p func)
		nil
		(uref func cl::function-code-buffer-offset)))

(defun kernel-function-lambda-list (func)
	(declare (ignore func))
	nil)		;; not implemented yet

(defun function-info-list (func)
	(let ((cb (function-code-buffer func)))
		(if cb
			(uref cb cl::compiled-code-info-offset))))
	
(defun function-lambda-list (func) (getf (function-info-list func) 'cl::lambda-list))
(defun function-lambda (func) (getf (function-info-list func) 'cl::lambda))
(defun function-source-file (func) (getf (function-info-list func) 'ccl:*source-file*))
(defun function-source-line (func) (getf (function-info-list func) 'ccl:*source-line*))
