;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		stepper.lisp
;;;;	Contents:	Simple step function from Guy Steele's CLTL2, p. 493.
;;;;	History:	10/3/96  RGC  Created.
;;;;

(defvar *hooklevel* 0)
(defun hook (x)
	(let ((*evalhook* 'eval-hook-function))
		(eval x)))

(defun eval-hook-function (form &rest env)
	(let ((*hooklevel* (+ *hooklevel* 1)))
		(format t "~%Form: ~A" form)
		(let ((values (multiple-value-list 
						(evalhook form #'eval-hook-function nil env))))
			(format t "~%Value:~{ ~A~}" values)
			(values-list values))))

;; example
;; (hook '(cons (values 3 2) 'b))
