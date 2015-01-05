;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  mflop.lisp
;;;;  (C) Nicolas Neuss (Nicolas.Neuss@iwr.uni-heidelberg.de)
;;;;  mflop.lisp is in the public domain.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +N-long+ #x100000)  ; does not fit in secondary cache
(defconstant +N-short+ #x100)    ; fits in primary cache

(defparameter *mflop-delta* 5.0
  "Time interval in seconds over which we measure performance.")

(defun make-double-float-array (size &optional (initial 0.0d0))
   (make-array size :element-type 'double-float :initial-element initial))

(defun ddot (x y n)
  (declare (type fixnum n)
	   (type (simple-array double-float (*)) x y))
  (declare (optimize (safety 0) (space 0) (debug 0) (speed 3)))
  (loop for i fixnum from 0 below n
	summing (* (aref x i) (aref y i)) double-float))

(defun daxpy (x y n)
  (declare (type fixnum n)
	   (type (simple-array double-float (*)) x y))
  (declare (optimize (safety 0) (space 0) (debug 0) (speed 3)))
  (loop with s double-float = 0.3d0
	for i from 0 below n do
	(setf (aref x i) (+ (* s (aref y i))))))

(defun test (fn size)
  (let ((x (make-double-float-array +N-long+))
	(y (make-double-float-array +N-long+)))
    (format
     t "~A-~A: ~$ MFLOPS~%"
     fn
     (if (= size +N-long+) "long" "short")
     (loop with after = 0
	   for before = (get-internal-run-time) then after
	   and count = 1 then (* count 2)
	   do
	   (loop repeat count do (funcall fn x y size))
	   (setq after (get-internal-run-time))
	   (when (> (/ (- after before) internal-time-units-per-second)
		    *mflop-delta*)
	     (return (/ (* 2 size count internal-time-units-per-second)
			(* 1e6 (- after before)))))))))

(defun mflop-test ()
  "Returns several numbers characteristic for floating point efficiency of
your CL implementation.  Please compare these numbers to those obtained by
the C version in mflop.c."
  (test 'ddot +N-long+)
  (test 'ddot +N-short+)
  (test 'daxpy +N-long+)
  (test 'daxpy +N-short+))

#+ignore (mflop-test)