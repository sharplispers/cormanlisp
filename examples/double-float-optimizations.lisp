;;;
;;; Examples using SYS math functions to optimize double-float performance.
;;; Optimizing:
;;;     (dotimes (i 1000000 z)(incf z (* x y)))
;;;  where x, y and z are double-float.

;;; Standard Common Lisp
(time
    (let ((x 1.5d0)(y 2.1d0)(z 0.0d0))
        (dotimes (i 1000000 z)(incf z (* x y)))))

;;;
;;; Using SYS D+, D* functions, specific to double-floats.
;;;
(time
    (let ((x 1.5d0)(y 2.1d0)(z 0.0d0))
        (dotimes (i 1000000 z)(setf z (sys:d+ z (sys:d* x y))))))

;;;
;;; Standard Common Lisp, using arrays of double-float rather than individual numbers (slow)
;;;
(time
    (let ((x (make-array 1 :element-type 'double-float :initial-element 1.5d0))
          (y (make-array 1 :element-type 'double-float :initial-element 2.1d0))
          (z (make-array 1 :element-type 'double-float :initial-element 0d0)))
        (dotimes (i 1000000 (aref z 0))(setf (aref z 0) (+ (aref z 0) (* (aref x 0) (aref y 0)))))))

;;;
;;; Same thing, using SYS DV+, DV* operators, optimized for double-float.
;;;
(time
    (let ((x (make-array 1 :element-type 'double-float :initial-element 1.5d0))
          (y (make-array 1 :element-type 'double-float :initial-element 2.1d0))
          (z (make-array 1 :element-type 'double-float :initial-element 0d0))
          (temp (make-array 1 :element-type 'double-float :initial-element 0d0)))
        (dotimes (i 1000000 (aref z 0))
            (progn
                (sys:dv* x 0 y 0 temp 0)
                (sys:dv+ z 0 temp 0 z 0)))))

;;;
;;; Generates the same code for a double-float number as for an array of double floats of length 1.
;;; This usage is possibly not a good idea, because the actual number in z and temp gets 
;;; changed, and Common Lisp normally treats double-float as an immutable object (i.e if
;;; the value were shared, the shared copy would be changed.
;;;
(time
    (let ((x 1.5d0)
          (y 2.1d0)
          (z 0d0)
          (temp 0d0))
        (dotimes (i 1000000 z)
                (progn
                    (sys:dv* x 0 y 0 temp 0)
                    (sys:dv+ z 0 temp 0 z 0)))))

;;;
;;; A better alternative to the previous--values that will be changed
;;; are stored in arrays.
;;;
(time
    (let ((x 1.5d0)
          (y 2.1d0)
          (z (make-array 1 :element-type 'double-float :initial-element 0d0))
          (temp (make-array 1 :element-type 'double-float :initial-element 0d0)))
        (dotimes (i 1000000 (aref z 0))
            (progn
                (sys:dv* x 0 y 0 temp 0)
                (sys:dv+ z 0 temp 0 z 0)))))
