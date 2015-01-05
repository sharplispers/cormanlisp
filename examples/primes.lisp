;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;		File:		primes.lisp
;;;;
;;;;		Contents:	Fun with prime numbers.
;;;;					Common Lisp algorithms to generate all the 
;;;;					prime numbers between 1 and n. 
;;;;
;;;;		Author:		Roger Corman
;;;;

;;;	Brute force approach to calculating them (i.e. it
;;;	tries all possibilites).
(defun primes (n1 n2 &key (verbose nil))
	"Generate all the prime numbers between n1 and n2"
	(flet ((output-prime (x) (format t "~D~%" x)(force-output)))
		(do ((i n1 (1+ i))
			 (result '()))
			((> i n2) (nreverse result))
			(block inner
				(do ((j 2 (1+ j)))
					((>= j i) 
					 (progn (if verbose (output-prime i))
					 	(push i result)))
					(if (zerop (mod i j))	; if found a divisor
					 	(return-from inner))))))) 

;; Alternative method: generally much faster!
(defun sieve1 (n)
	(let ((a (make-array n :element-type 'bit :initial-element 0))
		  (result '()))
		(loop for i from 2 to (- n 1) do
			(progn
				(when (= (bit a i) 0)
					(push i result)
					(do ((j (* i i) (+ j i)))
						((>= j n))
						(setf (bit a j) 1)))))
		(nreverse result)))

;;; further optimized
(defun sieve2 (n)
	(declare (fixnum n)(optimize (speed 3)(safety 0)))
	(let ((a (make-array n :element-type 'bit :initial-element 0))
		  (result (list 2)))
		(do ((i 3 (the fixnum (+ i 2))))
			((>= i n)(nreverse result))
			(declare (fixnum i))
			(progn
				(when (= (sbit a i) 0)
					(push i result)
					(do* ((inc (+ i i))
						  (j (* i i) (the fixnum (+ j inc))))
						 ((>= j n))
						(declare (fixnum i j inc))
						(setf (sbit a j) 1)))))))

(defun sieve3 (n)
	(declare (fixnum n)(optimize (speed 3)(safety 0)))
	(let* ((end (truncate n 2))
		   (a (make-array end :element-type 'bit :initial-element 0))
		   (result (list 2)))
		(do* ((i 1 (the fixnum (+ i 1))))
			((>= i end)(nreverse result))
			(declare (fixnum i))
			(progn
				(when (= (sbit a i) 0)
					(let ((p (+ i i 1)))
						(push p result)
						(do* ((j (+ (* 2 i i) i i) (the fixnum (+ j p))))
							 ((>= j end))
							(declare (fixnum i j))
							(setf (sbit a j) 1))))))))

;; fastest so far!
(defun sieve4 (n)
	(declare (fixnum n)(optimize (speed 3)(safety 0)))
	(let* ((end (truncate n 2))
		   (root (isqrt end))
		   (a (make-array end :element-type 'bit :initial-element 0))
		   (result (list 2)))
		(do* ((i 1 (the fixnum (+ i 1))))
			((>= i end)(nreverse result))
			(declare (fixnum i))
			(progn
				(when (= (sbit a i) 0)
					(let* ((2i (+ i i))
						   (p (+ 2i 1)))
						(declare (fixnum 2i p))
						(push p result)
						(if (< i root)
							(do* ((ii (* i i))
								  (j (+ (+ ii ii) 2i) (the fixnum (+ j p))))
								 ((>= j end))
								(declare (fixnum j ii))
								(setf (sbit a j) 1)))))))))

(defun sieve5 (n)
	(declare (fixnum n)(optimize (speed 3)(safety 0)))
	(let* ((a (make-array n :element-type 'bit :initial-element 0))
		   (result (list 2))
		   (root (isqrt n)))
		(declare (fixnum root))
		(do ((i 3 (the fixnum (+ i 2))))
			((>= i n)(nreverse result))
			(declare (fixnum i))
			(progn
				(when (= (sbit a i) 0)
					(push i result)
					(if (< i root)
						(do* ((inc (+ i i))
							  (j (* i i) (the fixnum (+ j inc))))
							 ((>= j n))
							(declare (fixnum j inc))
							(setf (sbit a j) 1))))))))

;;;
;;; Find the nearest prime number to a given passed integer
;;; If two primes are equally close it will choose the lesser one.
;;;
(defun nearest-prime (n) 
    (let ((min most-positive-fixnum)
          (closest -1))
        (dolist (x (sieve5 (+ n 1000)) closest) 
            (when (< (abs (- x n)) min) 
                (setf min (abs (- x n)))
                (setf closest x)))))
