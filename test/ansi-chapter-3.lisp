;;;;
;;;;	File:      ansi-chapter-3.lisp
;;;;  Contents:   Examples from the Hyperspec
;;;;

;;;
;;;		Chapter 3 Dictionary
;;;

(dotests LAMBDA
	 (funcall (lambda (x) (+ x 3)) 4) =>  7
)

(dotests COMPILE
	(defun foo () "bar") =>  FOO
	(compiled-function-p #'foo) =>  implementation-dependent
	(compile 'foo) =>  FOO 
	(compiled-function-p #'foo) =>  true
	(setf (symbol-function 'foo)
		(compile nil '(lambda () "replaced"))) =>  implementation-dependent ;#<Compiled-Function>
	(foo) =>  "replaced"
)

(dotests EVAL
	(setq form '(1+ a) a 999) =>  999
	(eval form) =>  1000
	(eval 'form) =>  (1+ A)
	(let ((a '(this would break if eval used local value))) (eval form)) =>  1000
)

(dotests EVAL-WHEN
	;; these are too complicated for this file
)

(dotests LOAD-TIME-VALUE
	;; these are too complicated for this file
)

(dotests QUOTE
	 (setq a 1) =>  1
	 (quote (setq a 3)) =>  (SETQ A 3)
	 a =>  1
	 'a =>  A
	 ''a =>  (QUOTE A) 
	 '''a =>  (QUOTE (QUOTE A))
	 (setq a 43) =>  43
	 (list a (cons a 3)) =>  (43 (43 . 3))
	 (list (quote a) (quote (cons a 3))) =>  (A (CONS A 3)) 
	 1 =>  1
	 '1 =>  1
	 "foo" =>  "foo"
	 '"foo" =>  "foo"
	 (car '(a b)) =>  A
	 '(car '(a b)) =>  (CAR (QUOTE (A B)))
	 #(car '(a b)) =>  #(CAR (QUOTE (A B)))
	 '#(car '(a b)) =>  #(CAR (QUOTE (A B)))
)

(dotests COMPILER-MACRO-FUNCTION
	;; No examples
)

(dotests DEFINE-COMPILER-MACRO
	 (defun square (x) (expt x 2)) =>  SQUARE
	 (define-compiler-macro square (&whole form arg)
	   (if (atom arg)
	       `(expt ,arg 2)
	       (case (car arg)
	         (square (if (= (length arg) 2)
	                     `(expt ,(nth 1 arg) 4)
	                     form))
	         (expt   (if (= (length arg) 3)
	                     (if (numberp (nth 2 arg))
	                         `(expt ,(nth 1 arg) ,(* 2 (nth 2 arg)))
	                         `(expt ,(nth 1 arg) (* 2 ,(nth 2 arg))))
	                     form))
	         (otherwise `(expt ,arg 2))))) =>  SQUARE
	 (square (square 3)) =>  81
	 (macroexpand '(square x)) =>  (values (SQUARE X) false) ;; (SQUARE X), false
	 (funcall (compiler-macro-function 'square) '(square x) nil) =>  (EXPT X 2)
	 (funcall (compiler-macro-function 'square) '(square (square x)) nil) =>  (EXPT X 4)
	 (funcall (compiler-macro-function 'square) '(funcall #'square x) nil) =>  (EXPT X 2)
	
	 (defun distance-positional (x1 y1 x2 y2)
	   (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))) =>  DISTANCE-POSITIONAL
	 (defun distance (&key (x1 0) (y1 0) (x2 x1) (y2 y1))
	   (distance-positional x1 y1 x2 y2)) =>  DISTANCE
	 (define-compiler-macro distance (&whole form
	                                  &rest key-value-pairs
	                                  &key (x1 0  x1-p)
	                                       (y1 0  y1-p)
	                                       (x2 x1 x2-p)
	                                       (y2 y1 y2-p)
	                                  &allow-other-keys
	                                  &environment env)
	   (flet ((key (n) (nth (* n 2) key-value-pairs))
	          (arg (n) (nth (1+ (* n 2)) key-value-pairs))
	          (simplep (x)
	            (let ((expanded-x (macroexpand x env)))
	              (or (constantp expanded-x env)
	                  (symbolp expanded-x)))))
	     (let ((n (/ (length key-value-pairs) 2)))
	       (multiple-value-bind (x1s y1s x2s y2s others)
	           (loop for (key) on key-value-pairs by #'cddr
	                 count (eq key ':x1) into x1s
	                 count (eq key ':y1) into y1s
	                 count (eq key ':x2) into x2s
	                 count (eq key ':y1) into y2s
	                 count (not (member key '(:x1 :x2 :y1 :y2)))
	                   into others
	                 finally (return (values x1s y1s x2s y2s others)))
	         (cond ((and (= n 4)
	                     (eq (key 0) :x1)
	                     (eq (key 1) :y1)
	                     (eq (key 2) :x2)
	                     (eq (key 3) :y2))
	                `(distance-positional ,x1 ,y1 ,x2 ,y2))
	               ((and (if x1-p (and (= x1s 1) (simplep x1)) t)
	                     (if y1-p (and (= y1s 1) (simplep y1)) t)
	                     (if x2-p (and (= x2s 1) (simplep x2)) t)
	                     (if y2-p (and (= y2s 1) (simplep y2)) t)
	                     (zerop others))
	                `(distance-positional ,x1 ,y1 ,x2 ,y2))
	               ((and (< x1s 2) (< y1s 2) (< x2s 2) (< y2s 2)
	                     (zerop others))
	                (let ((temps (loop repeat n collect (gensym))))
	                  `(let ,(loop for i below n
	                               collect (list (nth i temps) (arg i)))
	                     (distance
	                       ,@(loop for i below n
	                               append (list (key i) (nth i temps)))))))
	               (t form)))))) =>  DISTANCE
	 (dolist (form
	           '((distance :x1 (setq x 7) :x2 (decf x) :y1 (decf x) :y2 (decf x))
	             (distance :x1 (setq x 7) :y1 (decf x) :x2 (decf x) :y2 (decf x))
	             (distance :x1 (setq x 7) :y1 (incf x))
	             (distance :x1 (setq x 7) :y1 (incf x) :x1 (incf x))
	             (distance :x1 a1 :y1 b1 :x2 a2 :y2 b2)
	             (distance :x1 a1 :x2 a2 :y1 b1 :y2 b2)
	             (distance :x1 a1 :y1 b1 :z1 c1 :x2 a2 :y2 b2 :z2 c2)))
	   (print (funcall (compiler-macro-function 'distance) form nil)))
	;;  (LET ((#:G6558 (SETQ X 7))
	;;        (#:G6559 (DECF X))
	;;        (#:G6560 (DECF X))
	;;        (#:G6561 (DECF X)))
	;;    (DISTANCE :X1 #:G6558 :X2 #:G6559 :Y1 #:G6560 :Y2 #:G6561)) 
	;;  (DISTANCE-POSITIONAL (SETQ X 7) (DECF X) (DECF X) (DECF X)) 
	;;  (LET ((#:G6567 (SETQ X 7))
	;;        (#:G6568 (INCF X)))
	;;    (DISTANCE :X1 #:G6567 :Y1 #:G6568)) 
	;;  (DISTANCE :X1 (SETQ X 7) :Y1 (INCF X) :X1 (INCF X)) 
	;;  (DISTANCE-POSITIONAL A1 B1 A2 B2) 
	;;  (DISTANCE-POSITIONAL A1 B1 A2 B2) 
	;;  (DISTANCE :X1 A1 :Y1 B1 :Z1 C1 :X2 A2 :Y2 B2 :Z2 C2) 
	=>  NIL
)

(dotests DEFMACRO
	 (defmacro mac1 (a b) "Mac1 multiplies and adds" 
	            `(+ ,a (* ,b 3))) =>  MAC1 
	 (mac1 4 5) =>  19 
	 (documentation 'mac1 'function) =>  "Mac1 multiplies and adds" 
	 (defmacro mac2 (&optional (a 2 b) (c 3 d) &rest x) `'(,a ,b ,c ,d ,x)) =>  MAC2 
	 (mac2 6) =>  (6 T 3 NIL NIL) 
	 (mac2 6 3 8) =>  (6 T 3 T (8)) 
	 (defmacro mac3 (&whole r a &optional (b 3) &rest x &key c (d a))
	    `'(,r ,a ,b ,c ,d ,x)) =>  MAC3 
	 (mac3 1 6 :d 8 :c 9 :d 10) =>  ((MAC3 1 6 :D 8 :C 9 :D 10) 1 6 9 8 (:D 8 :C 9 :D 10)) 

	#|
	 (defmacro dm1a (&whole x) `',x)
	 (macroexpand '(dm1a))  =>  (QUOTE (DM1A))
	 (macroexpand '(dm1a a)) is an error.
	 
	 (defmacro dm1b (&whole x a &optional b) `'(,x ,a ,b))
	 (macroexpand '(dm1b))  is an error.
	 (macroexpand '(dm1b q))  =>  (QUOTE ((DM1B Q) Q NIL))
	 (macroexpand '(dm1b q r)) =>  (QUOTE ((DM1B Q R) Q R))
	 (macroexpand '(dm1b q r s)) is an error.
	
	
	 (defmacro dm2a (&whole form a b) `'(form ,form a ,a b ,b))
	 (macroexpand '(dm2a x y)) =>  (QUOTE (FORM (DM2A X Y) A X B Y))
	 (dm2a x y) =>  (FORM (DM2A X Y) A X B Y)
	
	 (defmacro dm2b (&whole form a (&whole b (c . d) &optional (e 5)) 
	                 &body f &environment env)
	   ``(,',form ,,a ,',b ,',(macroexpand c env) ,',d ,',e ,',f))
	 ;Note that because backquote is involved, implementations may differ
	 ;slightly in the nature (though not the functionality) of the expansion.
	 (macroexpand '(dm2b x1 (((incf x2) x3 x4)) x5 x6))
	 =>  (LIST* '(DM2B X1 (((INCF X2) X3 X4))
	                   X5 X6)
	            X1
	            '((((INCF X2) X3 X4)) (SETQ X2 (+ X2 1)) (X3 X4) 5 (X5 X6))),
	     T
	 (let ((x1 5))
	   (macrolet ((segundo (x) `(cadr ,x)))
	     (dm2b x1 (((segundo x2) x3 x4)) x5 x6)))
	 =>  ((DM2B X1 (((SEGUNDO X2) X3 X4)) X5 X6)
	      5 (((SEGUNDO X2) X3 X4)) (CADR X2) (X3 X4) 5 (X5 X6))
	|#
)

(dotests MACRO-FUNCTION
	(defmacro macfun (x) '(macro-function 'macfun)) =>  MACFUN 
	 (not (macro-function 'macfun)) =>  false 
	
	 (macrolet ((foo (&environment env)
	               (if (macro-function 'bar env)
	                  ''yes
	                  ''no)))
	    (list (foo)
	          (macrolet ((bar () :beep))
	             (foo))))
	 
	=>  (NO YES)
)

(dotests MACROEXPAND/MACROEXPAND-1
	 (defmacro alpha (x y) `(beta ,x ,y)) =>  ALPHA
	 (defmacro beta (x y) `(gamma ,x ,y)) =>  BETA
	 (defmacro delta (x y) `(gamma ,x ,y)) =>  DELTA  ;EPSILON (mistake in spec)
	 (defmacro expand (form &environment env)
	   (multiple-value-bind (expansion expanded-p)
	       (macroexpand form env)
	     `(values ',expansion ',expanded-p))) =>  EXPAND
	 (defmacro expand-1 (form &environment env)
	   (multiple-value-bind (expansion expanded-p)
	       (macroexpand-1 form env)
	     `(values ',expansion ',expanded-p))) =>  EXPAND-1
	
	;; Simple examples involving just the global environment
	 (macroexpand-1 '(alpha a b)) =>  (values (BETA A B) true)
	 (expand-1 (alpha a b)) =>  (values (BETA A B) true)
	 (macroexpand '(alpha a b)) =>  (values (GAMMA A B) true)
	 (expand (alpha a b)) =>  (values (GAMMA A B) true)
	 (macroexpand-1 'not-a-macro) =>  (values NOT-A-MACRO false)
	 (expand-1 not-a-macro) =>  (values NOT-A-MACRO false)
	 (macroexpand '(not-a-macro a b)) =>  (values (NOT-A-MACRO A B) false)
	 (expand (not-a-macro a b)) =>  (values (NOT-A-MACRO A B) false)
	
	;; Examples involving lexical environments
	 (macrolet ((alpha (x y) `(delta ,x ,y)))
	   (macroexpand-1 '(alpha a b))) =>  (values (BETA A B) true)
	 (macrolet ((alpha (x y) `(delta ,x ,y)))
	   (expand-1 (alpha a b))) =>  (values (DELTA A B) true)
	 (macrolet ((alpha (x y) `(delta ,x ,y)))
	   (macroexpand '(alpha a b))) =>  (values (GAMMA A B) true)
	 (macrolet ((alpha (x y) `(delta ,x ,y)))
	   (expand (alpha a b))) =>  (values (GAMMA A B) true)
	 (macrolet ((beta (x y) `(epsilon ,x ,y)))
	   (expand (alpha a b))) =>  (values (EPSILON A B) true)
	 (let ((x (list 1 2 3)))
	   (symbol-macrolet ((a (first x)))
	     (expand a))) =>  (values (FIRST X) true)
	 (let ((x (list 1 2 3)))
	   (symbol-macrolet ((a (first x)))
	     (macroexpand 'a))) =>  (values A false)
	 (symbol-macrolet ((b (alpha x y)))
	   (expand-1 b)) =>  (values (ALPHA X Y) true)
	 (symbol-macrolet ((b (alpha x y)))
	   (expand b)) =>  (values (GAMMA X Y) true)
	 (symbol-macrolet ((b (alpha x y))
	                   (a b))
	   (expand-1 a)) =>  (values B true)
	 (symbol-macrolet ((b (alpha x y))
	                   (a b))
	   (expand a)) =>  (values (GAMMA X Y) true)
	
	;; Examples of shadowing behavior
	 (flet ((beta (x y) (+ x y)))
	   (expand (alpha a b))) =>  (values (BETA A B) true)
	 (macrolet ((alpha (x y) `(delta ,x ,y)))
	   (flet ((alpha (x y) (+ x y)))
	     (expand (alpha a b)))) =>  (values (ALPHA A B) false)
	 (let ((x (list 1 2 3)))
	   (symbol-macrolet ((a (first x)))
	     (let ((a x))
	       (expand a)))) =>  (values A false)
)

(dotests DEFINE-SYMBOL-MACRO
	(defvar *things* (list 'alpha 'beta 'gamma)) =>  *THINGS*
	
	(define-symbol-macro thing1 (first *things*)) =>  THING1
	(define-symbol-macro thing2 (second *things*)) =>  THING2
	(define-symbol-macro thing3 (third *things*)) =>  THING3
	
	thing1 =>  ALPHA
	(setq thing1 'ONE) =>  ONE
	*things* =>  (ONE BETA GAMMA)
	(multiple-value-setq (thing2 thing3) (values 'two 'three)) =>  TWO
	thing3 =>  THREE
	*things* =>  (ONE TWO THREE)
	
	(list thing2 (let ((thing2 2)) thing2)) =>  (TWO 2)
)

(dotests SYMBOL-MACROLET
	;;; The following is equivalent to
	;;;   (list 'foo (let ((x 'bar)) x)),
	;;; not
	;;;   (list 'foo (let (('foo 'bar)) 'foo))
	 (symbol-macrolet ((x 'foo))
	   (list x (let ((x 'bar)) x))) 
	=>  (foo bar)
	;;NOT=>  (foo foo) 
	 
	 (symbol-macrolet ((x '(foo x)))
	   (list x))
	=>  ((FOO X))
)

(dotests *MACROEXPAND-HOOK*
	 (defun hook (expander form env)
	    (format t "Now expanding: ~S~%" form)
	    (funcall expander form env)) =>  HOOK 
	 (defmacro machook (x y) `(/ (+ ,x ,y) 2)) =>  MACHOOK 
	 (macroexpand '(machook 1 2)) =>  (values (/ (+ 1 2) 2) true) 
	 (let ((*macroexpand-hook* #'hook)) (macroexpand '(machook 1 2)))
	;;>>  Now expanding (MACHOOK 1 2) 
	=>  (VALUES (/ (+ 1 2) 2) true)
)

(dotests PROCLAIM
 (defun declare-variable-types-globally (type vars)
   (proclaim `(type ,type ,@vars))
   type) => declare-variable-types-globally

 ;; Once this form is executed, the dynamic variable *TOLERANCE*
 ;; must always contain a float.
 (declare-variable-types-globally 'float '(*tolerance*))
=>  FLOAT
)

(dotests DECLAIM
	;; no examples
)

(dotests DECLARE
	;; no examples
)

(dotests IGNORE/IGNORABLE
	;; no examples
)

(dotests DYNAMIC-EXTENT
	;; In this example, the implementation is permitted to stack allocate
	;; the list that is bound to X.
	 (let ((x (list 1 2 3)))
	   (declare (dynamic-extent x))
	   (print x)
	   :done)
	;;>>  (1 2 3)
	=>  :DONE
	 
	;; In this example, the list to be bound to L can be stack-allocated.
	 (defun zap (x y z)
	   (do ((l (list x y z) (cdr l)))
	       ((null l))
	     (declare (dynamic-extent l))
	     (prin1 (car l)))) =>  ZAP
	 (zap 1 2 3)
	;;>>  123
	=>  NIL
	#|
	;; Some implementations might open-code LIST-ALL-PACKAGES in a way
	;; that permits using stack allocation of the list to be bound to L.
	 (do ((l (list-all-packages) (cdr l)))
	     ((null l))
	   (declare (dynamic-extent l))
	   (let ((name (package-name (car l))))
	     (when (string-search "COMMON-LISP" name) (print name))))
	;;>>  "COMMON-LISP"
	;;>>  "COMMON-LISP-USER"
	=>  NIL
	|#
	;; Some implementations might have the ability to stack allocate 
	;; rest lists.  A declaration such as the following should be a cue
	;; to such implementations that stack-allocation of the rest list
	;; would be desirable.
	 (defun add (&rest x)
	   (declare (dynamic-extent x))
	   (apply #'+ x)) =>  ADD
	 (add 1 2 3) =>  6
	
	 (defun zap (n m)
	   ;; Computes (RANDOM (+ M 1)) at relative speed of roughly O(N).
	   ;; It may be slow, but with a good compiler at least it
	   ;; doesn't waste much heap storage.  :-}
	   (let ((a (make-array n)))
	     (declare (dynamic-extent a))
	     (dotimes (i n) 
	       (declare (dynamic-extent i))
	       (setf (aref a i) (random (+ i 1))))
	     (aref a m))) =>  ZAP
	 (< (zap 5 3) 3) =>  true
	
	#|
	The following are in error, since the value of x is used outside of its extent: 
	
	
	 (length (list (let ((x (list 1 2 3)))  ; Invalid
	                (declare (dynamic-extent x))
	                x)))
	
	 (progn (let ((x (list 1 2 3)))  ; Invalid
	          (declare (dynamic-extent x))
	          x)
	        nil)
	|#
)

(dotests TYPE
	 (defun f (x y)
	   (declare (type fixnum x y))
	   (let ((z (+ x y)))
	     (declare (type fixnum z))
	     z)) =>  F
	 (f 1 2) =>  3
	 ;; The previous definition of F is equivalent to
	 (defun f (x y)
	   ;; This declaration is a shorthand form of the TYPE declaration
	   (declare (fixnum x y))
	   ;; To declare the type of a return value, it's not necessary to
	   ;; create a named variable.  A THE special form can be used instead.
	   (the fixnum (+ x y))) =>  F
	 (f 1 2) =>  3
)

(dotests INLINE/NOTINLINE
	;; no tests
)

(dotests FTYPE
	;; no tests
)

(dotests DECLARATION
	;; no tests
)

(dotests OPTIMIZE
	;; no tests
)

(dotests SPECIAL
	(defun declare-eg (y)                 ;this y is special
	 (declare (special y))
	 (let ((y t))                         ;this y is lexical
	      (list y
	            (locally (declare (special y)) y)))) ;this y refers to the
	                                                 ;special binding of y
	=>  DECLARE-EG 
	 (declare-eg nil) =>  (T NIL) 
	
	
	(setf (symbol-value 'x) 6) => 6
	(defun foo (x)                         ;a lexical binding of x
	  (print x)
	  (let ((x (1+ x)))                    ;a special binding of x
	    (declare (special x))              ;and a lexical reference
	    (bar))
	  (1+ x)) => FOO
	(defun bar () 
	  (print (locally (declare (special x))
	           x))) => BAR
	(foo 10) 
	;;>>  10
	;;>>  11
	=>  11
	
	
	(setf (symbol-value 'x) 6)  => 6
	(defun bar (x y)            ;[1] 1st occurrence of x
	  (let ((old-x x)           ;[2] 2nd occurrence of x -- same as 1st occurrence
	        (x y))              ;[3] 3rd occurrence of x
	    (declare (special x))
	    (list old-x x))) => BAR
	(bar 'first 'second) =>  (FIRST SECOND)
	
	 (declaim (special prosp)) =>  implementation-dependent
	 (setq prosp 1 reg 1) =>  1
	 (let ((prosp 2) (reg 2))         ;the binding of prosp is special
	    (set 'prosp 3) (set 'reg 3)   ;due to the preceding proclamation,
	    (list prosp reg))             ;whereas the variable reg is lexical
	=>  (3 2)
	 (list prosp reg) =>  (1 3)
	
	 (declaim (special x)) => T          ;x is always special.
	 (defun example (x y)                                 
	   (declare (special y))
	   (let ((y 3) (x (* x 2)))
	     (print (+ y (locally (declare (special y)) y)))
	     (let ((y 4)) (declare (special y)) (foo x)))) =>  EXAMPLE
)

(dotests LOCALLY
	 (defun sample-function (y)  ;this y is regarded as special
	   (declare (special y))                                
	   (let ((y t))              ;this y is regarded as lexical
	     (list y
	           (locally (declare (special y))
	             ;; this next y is regarded as special
	             y))))
	=>  SAMPLE-FUNCTION
	 (sample-function nil) =>  (T NIL) 
	 (setq x '(1 2 3) y '(4 . 5)) =>  (4 . 5)
	
	;;; The following declarations are not notably useful in specific.
	;;; They just offer a sample of valid declaration syntax using LOCALLY.
	 (locally (declare (inline floor) (notinline car cdr))
	          (declare (optimize space))
	    (floor (car x) (cdr y))) => (values 0 1)
	
	
	;;; This example shows a definition of a function that has a particular set
	;;; of OPTIMIZE settings made locally to that definition.
	 (locally (declare (optimize (safety 3) (space 3) (speed 0)))
	   (defun frob (w x y &optional (z (foo x y)))
	     (mumble x y z w)))
	=>  FROB
	
	;;; This is like the previous example, except that the optimize settings
	;;; remain in effect for subsequent definitions in the same compilation unit.
	 (declaim (optimize (safety 3) (space 3) (speed 0))) => T
	 (defun frob (w x y &optional (z (foo x y)))
	   (mumble x y z w))
	=>  FROB
)

(dotests THE
	;(the symbol (car (list (gensym)))) =>  #:G9876  ;; can't verify gensyms
	(the symbol (car (list 'foo))) =>  foo
	 (the fixnum (+ 5 7)) =>  12
	 (the (values) (truncate 3.2 2)) =>  (values 1 1.2)
	 (the integer (truncate 3.2 2)) =>  (values 1 1.2)
	 (the (values integer) (truncate 3.2 2)) =>  (values 1 1.2)
	 (the (values integer float) (truncate 3.2 2))   =>  (values 1 1.2)
	 (the (values integer float symbol) (truncate 3.2 2)) =>  (values 1 1.2)
	 (the (values integer float symbol t null list) 
	      (truncate 3.2 2)) =>  (values 1 1.2)
	 (let ((i 100))
	    (declare (fixnum i))
	    (the fixnum (1+ i))) =>  101
	 (let* ((x (list 'a 'b 'c))
	        (y 5))
	    (setf (the fixnum (car x)) y)
	    x) =>  (5 B C)
)

(dotests SPECIAL-OPERATOR-P
	 (special-operator-p 'if) =>  true
	 (special-operator-p 'car) =>  false
	 (special-operator-p 'one) =>  false
)

(dotests CONSTANTP
	(constantp 1) =>  true
	(constantp 'temp) =>  false
	(constantp ''temp) =>  true
	(defconstant this-is-a-constant 'never-changing) =>  THIS-IS-A-CONSTANT 
	(constantp 'this-is-a-constant) =>  true
	(constantp "temp") =>  true
	(setq a 6) =>  6 
	(constantp a) =>  true
	(constantp '(sin pi)) =>  implementation-dependent
	(constantp '(car '(x))) =>  implementation-dependent
	(constantp '(eql x x)) =>  implementation-dependent
	(constantp '(typep x 'nil)) =>  implementation-dependent
	(constantp '(typep x 't)) =>  implementation-dependent
	(constantp '(values this-is-a-constant)) =>  implementation-dependent
	(constantp '(values 'x 'y)) =>  implementation-dependent
	(constantp '(let ((a '(a b c))) (+ (length a) 6))) =>  implementation-dependent
)

