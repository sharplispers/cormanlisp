;;;;
;;;;	File:				quicksort.lisp
;;;;	Contents:			Quicksort implementation.
;;;;
;;;;	Common Lisp code by	Roger Corman
;;;;
(in-package :ccl)
(export '(quicksort))

(defun _quicksort (vec lo hi comp-func key)
    (if (> hi lo)
		(let* ((mid (round (+ lo hi) 2))
			   (i lo)
			   (j (+ hi 1))
			   (p (elt vec mid)))
			(rotatef (elt vec mid) (elt vec lo))	;; swap mid element to first
			(loop
				(loop do (incf i) 
					until 
					(or 
						(> i hi) 
						(funcall comp-func 
							(if key (funcall key p) p) 
							(if key (funcall key (elt vec i)) (elt vec i)))))
				(loop do (decf j) 
					until 
					(or 
						(<= j lo) 
						(funcall comp-func 
							(if key (funcall key (elt vec j)) (elt vec j)) 
							(if key (funcall key p) p))))	
				(if (< j i) (return))
				(rotatef (elt vec i)(elt vec j)))
	
			(rotatef (elt vec lo) (elt vec j))  ;;  put partition element in place	
			(_quicksort vec lo (- j 1) comp-func key)
			(_quicksort vec i hi comp-func key)))
	vec)

(defun quicksort (sequence comp-func &key (key nil))
    (_quicksort sequence 0 (- (length sequence) 1) comp-func key))

(provide "QUICKSORT")

