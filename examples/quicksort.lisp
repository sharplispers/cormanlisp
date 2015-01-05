;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:				quicksort.lisp
;;;;	Contents:			Quicksort implementation.
;;;;
;;;;	Common Lisp code by	Roger Corman
;;;;

(defun quicksort (vec lo hi comp-func)
    (if (> hi lo)
		(let* ((mid (round (+ lo hi) 2))
			   (i lo)
			   (j (+ hi 1))
			   (p (elt vec mid)))
			(rotatef (elt vec mid) (elt vec lo))	;; swap mid element to first
			(loop
				(loop do (incf i) 
					until (or (> i hi) (funcall comp-func p (elt vec i))))
				(loop do (decf j) 
					until (or (<= j lo) (funcall comp-func (elt vec j) p)))	
				(if (< j i) (return))
				(rotatef (elt vec i)(elt vec j)))
	
			(rotatef (elt vec lo) (elt vec j))  ;;  put partition element in place	
			(quicksort vec lo (- j 1) comp-func)
			(quicksort vec i hi comp-func)))
	vec)

(defun qsort (sequence comp-func)
    (quicksort sequence 0 (- (length sequence) 1) comp-func))

