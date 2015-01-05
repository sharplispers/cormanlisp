;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:	pi.lisp
;;;;	Calculates PI to any number of decimal places.
;;;;	Usage:	(calc-pi nn)  where nn is the number of significant digits.
;;;;

;;; For the computation of pi:
;;; Computes N * atan(1.0/k) as a large integer
(defun pi-atan (k n)
	(do* ((a 0)
      	  (w (* n k))
		  (k2 (* k k))
		  (i -1))
		((= w 0) a)
		(setq w (truncate w k2))
		(incf i 2)
		(incf a (truncate w i))
		(setq w (truncate w k2))
		(incf i 2)
		(decf a (truncate w i))))

(defun calc-pi (digits)
	(let* ((n digits)
		   (m (+ n 3))
		   (tenpower (expt 10 m)))
		(values 
			(truncate 
				(- 
					(+ (pi-atan 18 (* tenpower 48)) 
					   (pi-atan 57 (* tenpower 32)))
					(pi-atan 239 (* tenpower 20)))
				1000))))
