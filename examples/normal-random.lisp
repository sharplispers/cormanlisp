;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		normal-random.lisp
;;;;	Contents:	Normalized random number generator.
;;;;	History:	4/13/01  RGC  Created.
;;;;
#|
Returns the next pseudorandom, Gaussian ("normally") distributed 
double value with mean 0.0 and standard deviation 1.0 from this 
random number generator's sequence. 
The general contract of nextGaussian is that one double value, 
chosen from (approximately) the usual normal distribution with 
mean 0.0 and standard deviation 1.0, is pseudorandomly generated 
and returned.
 

 synchronized public double nextGaussian() {
    if (haveNextNextGaussian) {
            haveNextNextGaussian = false;
            return nextNextGaussian;
    } else {
            double v1, v2, s;
            do { 
                    v1 = 2 * nextDouble() - 1;   // between -1.0 and 1.0
                    v2 = 2 * nextDouble() - 1;   // between -1.0 and 1.0
                    s = v1 * v1 + v2 * v2;
            } while (s >= 1);
            double multiplier = Math.sqrt(-2 * Math.log(s)/s);
            nextNextGaussian = v2 * multiplier;
            haveNextNextGaussian = true;
            return v1 * multiplier;
    }
 }
This uses the polar method of G. E. P. Box, M. E. Muller, and 
G. Marsaglia, as described by Donald E. Knuth in The Art of 
Computer Programming, Volume 2: Seminumerical Algorithms, section 
3.4.1, subsection C, algorithm P. Note that it generates two 
independent values at the cost of only one call to Math.log 
and one call to Math.sqrt.

Returns:
the next pseudorandom, Gaussian ("normally") distributed double value 
with mean 0.0 and standard deviation 1.0 from this random number 
generator's sequence.
|#

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

