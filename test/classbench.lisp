(defclass foo () ())

(defclass bar () (a b c))

(defclass baz () (x y))

(defgeneric gf-1-1 (a))
(defmethod gf-1-1 ((a foo)))

(defgeneric gf-1-2 (a))
(defmethod gf-1-2 ((a foo)))
(defmethod gf-1-2 ((a symbol)))

(defgeneric gf-1-3 (a))
(defmethod gf-1-3 ((a integer)))
(defmethod gf-1-3 ((a bar)))
(defmethod gf-1-3 ((a t)))

(defgeneric gf-2-3 (a b))
(defmethod gf-2-3 ((a integer) (b bar)))
(defmethod gf-2-3 ((a foo) (b t)))
(defmethod gf-2-3 ((a t) (b integer)))

(defun bench-gfs ()
  (let ((foo (make-instance 'foo))
		(bar (make-instance 'bar))
		(baz (make-instance 'baz)))
  (dotimes (i 10000)
	(gf-1-1 foo)
	(gf-1-1 foo)
	(gf-1-2 foo)
	(gf-1-2 'quux)
	(gf-1-3 5)
	(gf-1-3 foo)
	(gf-2-3 5 bar)
	(gf-2-3 foo 'quux)
	(gf-2-3 baz 7))))

(defun bench-slot-value ()
  (let ((bar (make-instance 'bar))
		(baz (make-instance 'baz)))
	(dotimes (i 100000)
	  (setf (slot-value bar 'a) 10)
	  (setf (slot-value baz 'y) 20)
	  (slot-value bar 'a)
	  (setf (slot-value bar 'c) 777)
	  (slot-value bar 'c)
	  (slot-value baz 'y))))

