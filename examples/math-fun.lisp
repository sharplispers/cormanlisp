;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		math-fun.lisp
;;;;	Contents:	Math-related functions.
;;;;	History:	9/24/98  RGC  Created.
;;;;

;;; The fibonacci definition used here is iterative, and avoids the dreaded double
;;; recursion in the following obvious recursive definition:
;;;   (defun fibonacci (x) (if (<= x 1) 1 (+ (fibonacci (- x 1)) (fibonacci (- x 2)))))
;;; That definition takes *forever* to run on numbers over 20 or so.
;;;
(defun fibonacci (x)
	(if (= x 0)
		1
		(let ((result 1)
			  (prev 1))
			(dotimes (i (1- x))
				(psetq result (+ result prev) prev result))
			result)))
