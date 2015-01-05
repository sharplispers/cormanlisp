;;;;
;;;;	File:       ansi-chapter-7.lisp
;;;;   Contents:   Examples from the Hyperspec
;;;;

;;; OBJECTS
(dotests FUNCTION-KEYWORDS
 (defmethod gf1 ((a integer) &optional (b 2)
                 &key (c 3) ((:dee d) 4) e ((eff f)))
   (list a b c d e f))
=>  true ;;#<STANDARD-METHOD GF1 (INTEGER) 36324653>
 (find-method #'gf1 '() (list (find-class 'integer))) 
=>  true ;;#<STANDARD-METHOD GF1 (INTEGER) 36324653>
 (function-keywords *)
=>  (:C :DEE :E EFF), false
 (defmethod gf2 ((a integer))
   (list a b c d e f))
=>  true ;;#<STANDARD-METHOD GF2 (INTEGER) 42701775>
 (function-keywords (find-method #'gf1 '() (list (find-class 'integer))))
=>  (), false
 (defmethod gf3 ((a integer) &key b c d &allow-other-keys)
   (list a b c d e f))
 (function-keywords *)
=>  (:B :C :D), true
)
	
(dotests CHANGE-CLASS

 (defclass position () ()) => true
  
 (defclass x-y-position (position)
     ((x :initform 0 :initarg :x)
      (y :initform 0 :initarg :y))) => true
  
 (defclass rho-theta-position (position)
     ((rho :initform 0)
      (theta :initform 0))) => true
  
 (defmethod update-instance-for-different-class :before ((old x-y-position) 
                                                         (new rho-theta-position)
                                                         &key)
   ;; Copy the position information from old to new to make new
   ;; be a rho-theta-position at the same position as old.
   (let ((x (slot-value old 'x))
         (y (slot-value old 'y)))
     (setf (slot-value new 'rho) (sqrt (+ (* x x) (* y y)))
           (slot-value new 'theta) (atan y x)))) => true
  
;;; At this point an instance of the class x-y-position can be
;;; changed to be an instance of the class rho-theta-position using
;;; change-class:
 
 (setq p1 (make-instance 'x-y-position :x 2 :y 0)) => true
  
 (change-class p1 'rho-theta-position) => true
  
;;; The result is that the instance bound to p1 is now an instance of
;;; the class rho-theta-position.   The update-instance-for-different-class
;;; method performed the initialization of the rho and theta slots based
;;; on the value of the x and y slots, which were maintained by
;;; the old instance.
)
	
(dotests SLOT-VALUE
	 (defclass foo () 
	   ((a :accessor foo-a :initarg :a :initform 1)
	    (b :accessor foo-b :initarg :b)
	    (c :accessor foo-c :initform 3)))
	=>  true ;; RGC #<STANDARD-CLASS FOO 244020371>
	 (setq foo1 (make-instance 'foo :a 'one :b 'two))
	=>  true ;; RGC #<FOO 36325624>
	 (slot-value foo1 'a) =>  ONE
	 (slot-value foo1 'b) =>  TWO
	 (slot-value foo1 'c) =>  3
	 (setf (slot-value foo1 'a) 'uno) =>  UNO
	 (slot-value foo1 'a) =>  UNO
	 (defmethod foo-method ((x foo))
	   (slot-value x 'a))
	=>  true ;; RGC #<STANDARD-METHOD FOO-METHOD (FOO) 42720573>
	 (foo-method foo1) =>  UNO
)

(dotests METHOD-QUALIFIERS
	 (setf temp (defmethod some-gf :before ((a integer)) a))
	=>  true ;; RGC #<STANDARD-METHOD SOME-GF (:BEFORE) (INTEGER) 42736540>
	 (method-qualifiers temp) =>  (:BEFORE)
)

(dotests MAKE-LOAD-FORM
	 (defclass obj ()
	    ((x :initarg :x :reader obj-x)
	     (y :initarg :y :reader obj-y)
	     (dist :accessor obj-dist)))
	=>  true ;; RGC #<STANDARD-CLASS OBJ 250020030>
	 (defmethod shared-initialize :after ((self obj) slot-names &rest keys)
	   (declare (ignore slot-names keys))
	   (unless (slot-boundp self 'dist)
	     (setf (obj-dist self)
	           (sqrt (+ (expt (obj-x self) 2) (expt (obj-y self) 2))))))
	=> true ;; RGC  #<STANDARD-METHOD SHARED-INITIALIZE (:AFTER) (OBJ T) 26266714>
	 (defmethod make-load-form ((self obj) &optional environment)
	   (declare (ignore environment))
	   ;; Note that this definition only works because X and Y do not
	   ;; contain information which refers back to the object itself.
	   ;; For a more general solution to this problem, see revised example below.
	   `(make-instance ',(class-of self)
	                   :x ',(obj-x self) :y ',(obj-y self)))
	=>  true ;; RGC #<STANDARD-METHOD MAKE-LOAD-FORM (OBJ) 26267532>
	 (setq obj1 (make-instance 'obj :x 3.0 :y 4.0)) =>  true ;; RGC #<OBJ 26274136>
	 (obj-dist obj1) =>  5.0
	 (make-load-form obj1) =>  (MAKE-INSTANCE 'OBJ :X '3.0 :Y '4.0)
	
	 ;; Redefine method defined above.
	 (defmethod make-load-form ((self obj) &optional environment)
	    (make-load-form-saving-slots self
	                                 :slot-names '(x y)
	                                 :environment environment))
	=>  true ;; RGC #<STANDARD-METHOD MAKE-LOAD-FORM (OBJ) 42755655>
	#|
	 ;; Try MAKE-LOAD-FORM on object created above.
	 (make-load-form obj1)
	=>  (ALLOCATE-INSTANCE '#<STANDARD-CLASS OBJ 250020030>),
	    (PROGN
	      (SETF (SLOT-VALUE '#<OBJ 26274136> 'X) '3.0)
	      (SETF (SLOT-VALUE '#<OBJ 26274136> 'Y) '4.0)
	      (INITIALIZE-INSTANCE '#<OBJ 26274136>))
	|#
)

(dotests WITH-ACCESSORS
	 (defclass thing ()
	           ((x :initarg :x :accessor thing-x)
	            (y :initarg :y :accessor thing-y)))
	=>  true ;; RGC #<STANDARD-CLASS THING 250020173>
	 (defmethod (setf thing-x) :before (new-x (thing thing))
	   (format t "~&Changing X from ~D to ~D in ~S.~%"
	           (thing-x thing) new-x thing)) => true
	 (setq thing1 (make-instance 'thing :x 1 :y 2)) =>  true ;; RGC #<THING 43135676>
	 (setq thing2 (make-instance 'thing :x 7 :y 8)) =>  true ;; RGC #<THING 43147374>
	 (with-accessors ((x1 thing-x) (y1 thing-y))
	                 thing1
	   (with-accessors ((x2 thing-x) (y2 thing-y))
	                   thing2
	     (list (list x1 (thing-x thing1) y1 (thing-y thing1)
	                 x2 (thing-x thing2) y2 (thing-y thing2))
	           (setq x1 (+ y1 x2))
	           (list x1 (thing-x thing1) y1 (thing-y thing1)
	                 x2 (thing-x thing2) y2 (thing-y thing2))
	           (setf (thing-x thing2) (list x1))
	           (list x1 (thing-x thing1) y1 (thing-y thing1)
	                 x2 (thing-x thing2) y2 (thing-y thing2)))))
	;>>  Changing X from 1 to 9 in #<THING 43135676>.
	;>>  Changing X from 7 to (9) in #<THING 43147374>.
	=>  ((1 1 2 2 7 7 8 8)
	     9
	     (9 9 2 2 7 7 8 8) 
	     (9)
	     (9 9 2 2 (9) (9) 8 8))
)

(dotests WITH-SLOTS
	 (defclass thing ()
	           ((x :initarg :x :accessor thing-x)
	            (y :initarg :y :accessor thing-y)))
	=>  true ;; RGC #<STANDARD-CLASS THING 250020173>
	 (defmethod (setf thing-x) :before (new-x (thing thing))
	   (format t "~&Changing X from ~D to ~D in ~S.~%"
	           (thing-x thing) new-x thing)) => true
	 (setq thing (make-instance 'thing :x 0 :y 1)) =>  true ;; RGC #<THING 62310540>
	 (with-slots (x y) thing (incf x) (incf y)) =>  2
	 (values (thing-x thing) (thing-y thing)) =>  (values 1 2)
	 (setq thing1 (make-instance 'thing :x 1 :y 2)) =>  true ;; RGC #<THING 43135676>
	 (setq thing2 (make-instance 'thing :x 7 :y 8)) =>  true ;; RGC #<THING 43147374>
	 (with-slots ((x1 x) (y1 y))
	             thing1
	   (with-slots ((x2 x) (y2 y))
	               thing2
	     (list (list x1 (thing-x thing1) y1 (thing-y thing1)
	                 x2 (thing-x thing2) y2 (thing-y thing2))
	           (setq x1 (+ y1 x2))
	           (list x1 (thing-x thing1) y1 (thing-y thing1)
	                 x2 (thing-x thing2) y2 (thing-y thing2))
	           (setf (thing-x thing2) (list x1))
	           (list x1 (thing-x thing1) y1 (thing-y thing1)
	                 x2 (thing-x thing2) y2 (thing-y thing2)))))
	;  Changing X from 7 to (9) in #<THING 43147374>.
	=>  ((1 1 2 2 7 7 8 8)
	     9
	     (9 9 2 2 7 7 8 8) 
	     (9)
	     (9 9 2 2 (9) (9) 8 8))
)

(dotests FIND-METHOD
	 (defmethod some-operation ((a integer) (b float)) (list a b))
	=> true ;; RGC #<STANDARD-METHOD SOME-OPERATION (INTEGER FLOAT) 26723357>
	 (find-method #'some-operation '() (mapcar #'find-class '(integer float)))
	=>  true ;; RGC #<STANDARD-METHOD SOME-OPERATION (INTEGER FLOAT) 26723357>
	; (find-method #'some-operation '() (mapcar #'find-class '(integer integer)))
	;;>>  Error: No matching method
	 (find-method #'some-operation '() (mapcar #'find-class '(integer integer)) nil)
	=>  NIL
)

(dotests CLASS-OF
	 (class-of 'fred) =>    true ;; RGC #<BUILT-IN-CLASS SYMBOL 610327300>
	 (class-of 2/3) =>    true ;; RGC #<BUILT-IN-CLASS RATIO 610326642>
	 
	 (defclass book () ()) =>    true ;; RGC #<STANDARD-CLASS BOOK 33424745>
	 (class-of (make-instance 'book)) =>    true ;; RGC #<STANDARD-CLASS BOOK 33424745>
	 
	 (defclass novel (book) ()) =>    true ;; RGC #<STANDARD-CLASS NOVEL 33424764>
	 (class-of (make-instance 'novel)) =>    true ;; RGC #<STANDARD-CLASS NOVEL 33424764>
	
	 (defstruct kons kar kdr) =>  KONS
	 (class-of (make-kons :kar 3 :kdr 4)) =>    true ;; RGC #<STRUCTURE-CLASS KONS 250020317>
)	
	