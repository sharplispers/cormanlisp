;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		baby.lisp
;;;;	Contents:	Baby talker.
;;;;	History:	4/13/01  RGC  Created.
;;;;

(defparameter *vocabulary* 
	(copy-tree
		'(
			(boo)
			(goo) 
			(gah) 
			(waa!) 
			(hee-hee))))

(defparameter *feedback-percent* 50)  ;; 50

(defun punctuation-p (char) (find char ".,;:`!?#-()\\\""))

(defun print-with-spaces (list)
	"Prints a list formatted as a sentence."
	(let ((s (format nil "~{~a ~}" list)))
		(setf (elt s 0) (char-upcase (elt s 0)))
		(unless (punctuation-p (elt s (- (length s) 2)))
			(setf (elt s (1- (length s))) #\.))
		(write s :escape nil)))

(defun next-double () (- (random 2.0) 1.0))
(defconstant e (exp 1))
(defconstant 2*e (* 2 e))

(let ((next-next-normal nil))
	(defun normal-random ()
		(if next-next-normal
			(prog1 
				next-next-normal
				(setf next-next-normal nil))
			(let (v1 v2 (s 1d0))
				(do* ()
					((< s 1.0))
					(setf v1 (* 2 (next-double)))
					(setf v2 (* 2 (next-double)))
					(setf s (+ (* v1 v1)(* v2 v2))))
				(let ((multiplier (sqrt (/ (* -2 (log s)) s))))
					(setf next-next-normal (* v2 multiplier))
					(* v1 multiplier))))))

;;;
;;; Returns a normalized random double float number in the range
;;; 0 <= n < max
;;; The probability of any given integer in that range is
;;; based on the normal distribution curve i.e. the majority
;;; of returned values will be toward max/2.
;;; The passed number should be a non-negative real number.
;;;
(defun scaled-normal-random (n)
	(* n (/ (+ e (normal-random)) 2*e)))

;;; 
;;; Returns a positive integer in the range 0 <= n < max.
;;; The probability of any given integer in that range is
;;; based on the normal distribution curve i.e. the majority
;;; of returned values will be toward max/2.
;;;
(defun normal-random-integer (max)
	(let ((n (floor (scaled-normal-random max))))
		(if (< n 0)
			0
			(if (>= n max)
				(- max 1)
				n))))

(defmacro do-percent ((percent) &rest clauses)
	"Do this only some percentage of the time, as indicated 
	by the integer percent"
	`(when (< (random 100) ,percent)
		,@clauses))

;;;
;;; Reads the next line of input, returning a list of the words
;;; that were found. Punctuation characters are discarded.
;;;
(defun read-input-line ()
	(let ((input-line (progn (clear-input)(read-line))))
		(read-from-string
			(concatenate 'string 
				"("
				(substitute-if #\space #'punctuation-p input-line)
				")"))))

;;;
;;;	Generates a permutation of the passed vocabulary phrase
;;; (all possible orderings of passed list).
;;;
(defun permute (phrase)
	(if phrase
		(if (= (length phrase) 1)
			(list phrase)
			(let ((result '()))
				(dolist (x phrase)
					(dolist (z
						(mapcar 
							(lambda (y) 
								(cons x y))
							(permute (remove x phrase :count 1))))
						(push z result)))
				(nreverse result)))))

(defun get-random-phrase ()
	(let ((phrase (elt *vocabulary* (random (length *vocabulary*)))))
		(if (> (length phrase) 1)
			(let ((start (random (length phrase)))
				  (end (random (length phrase))))
				(if (> start end)
					(rotatef start end)
					(if (= start end)
						(if (> start 0)
							(decf start)
							(incf end))))
				(subseq phrase start (+ 1 end)))
			phrase)))
									
(defun generate-response ()
	(let ((num-words (max 1 (normal-random-integer 7)))
		  (response nil)
		  (current-phrase nil))
		(dotimes (i num-words (nreverse response))
			(if (null current-phrase)
				(setf current-phrase (get-random-phrase)))
			(push (first current-phrase) response)
			(setf current-phrase (cdr current-phrase)))))

(defun update-vocabulary (input)
	(if input
		(push input *vocabulary*)))

(defun respond (input)
	(update-vocabulary input)
	(let ((response (generate-response)))
		(do-percent (*feedback-percent*) 
			(update-vocabulary response))
		response))

(defun baby ()
	(format t "Talk to baby.~%")
	(do* ((input (read-input-line)(read-input-line))
		  (response (respond input)(respond input)))
		((or
			(equal input '(good bye))
			(equal input '(good-bye))
			(equal input '(bye))
			(equal input '(quit))
			(equal input '(stop))
			(equal input '(exit)))
		 (format t "bye~%"))
		(print-with-spaces response)
		(terpri)))
