;;;;
;;;;	File:       ansi-chapter-5.lisp
;;;;   Contents:   Examples from the Hyperspec
;;;;

;;; DATA AND CONTROL FLOW

(dotests SUBFORM-EVALUATION
	 (let ((ref2 (list '())))
	   (push (progn (princ "1") 'ref-1)
	         (car (progn (princ "2") ref2)))) 
	;;>>  12
	=>  (REF-1)
	
	 (let (x)
	    (push (setq x (list 'a))
	          (car (setq x (list 'b))))
	     x)
	=>  (((A) . B))
)

(dotests PLACES
	 (setq integer #x69) =>  #x69
	 (rotatef (ldb (byte 4 4) integer) 
	          (ldb (byte 4 0) integer)) => NIL
	 integer =>  #x96
	 (setq s (setq r (list (list 'a 1 'b 2 'c 3)))) =>  ((a 1 b 2 c 3))
	 (setf (getf (car r) 'b) 
	       (progn (setq r nil) 6)) =>  6
	 r =>  NIL
	 s =>  ((A 1 B 6 C 3))
)

(dotests APPLY
	 (setq f '+) =>  +
	 (apply f '(1 2)) =>  3
	 (setq f #'-) =>  #'- ; #<FUNCTION ->
	 (apply f '(1 2)) =>  -1
	 (apply #'max 3 5 '(2 7 3)) =>  7
	 (apply 'cons '((+ 2 3) 4)) =>  ((+ 2 3) . 4)
	 (apply #'+ '()) =>  0
	
	 (defparameter *some-list* '(a b c)) => *SOME-LIST*
	 (defun strange-test (&rest x) (eq x *some-list*)) => STRANGE-TEST
	 (apply #'strange-test *some-list*) =>  implementation-dependent
	
	 (defun bad-boy (&rest x) (rplacd x 'y)) => BAD-BOY
	 ;(bad-boy 'a 'b 'c) has undefined consequences.
	 ;(apply #'bad-boy *some-list*) has undefined consequences.
	
	
	 (defun foo (size &rest keys &key double &allow-other-keys)
	   (let ((v (apply #'make-array size :allow-other-keys t keys)))
	     (if double (concatenate (type-of v) v v) v))) => FOO
	 (foo 4 :initial-contents '(a b c d) :double t)
	    =>  #(A B C D A B C D)
)

(dotests DEFUN
	 (defun recur (x)
	  (when (> x 0)
	    (recur (1- x)))) =>  RECUR 
	 (defun ex (a b &optional c (d 66) &rest keys &key test (start 0))
	    (list a b c d keys test start)) =>  EX 
	 (ex 1 2) =>  (1 2 NIL 66 NIL NIL 0)
	 (ex 1 2 3 4 :test 'equal :start 50) 
	=>  (1 2 3 4 (:TEST EQUAL :START 50) EQUAL 50)
	 (ex :test 1 :start 2) =>  (:TEST 1 :START 2 NIL NIL 0)
	
	 ;; This function assumes its callers have checked the types of the
	 ;; arguments, and authorizes the compiler to build in that assumption.
	 (defun discriminant (a b c)
	   (declare (number a b c))
	   "Compute the discriminant for a quadratic equation."
	   (- (* b b) (* 4 a c))) =>  DISCRIMINANT
	 (discriminant 1 2/3 -2) =>  76/9
	
	 ;; This function assumes its callers have not checked the types of the
	 ;; arguments, and performs explicit type checks before making any assumptions. 
	 (defun careful-discriminant (a b c)
	   "Compute the discriminant for a quadratic equation."
	   (check-type a number)
	   (check-type b number)
	   (check-type c number)
	   (locally (declare (number a b c))
	     (- (* b b) (* 4 a c)))) =>  CAREFUL-DISCRIMINANT
	 (careful-discriminant 1 2/3 -2) =>  76/9
)

(dotests FDEFINITION
	;; no examples
)

(dotests FBOUNDP
	 (fboundp 'car) =>  true
	 (fboundp 'nth-value) =>  false
	 (fboundp 'with-open-file) =>  true
	 (fboundp 'unwind-protect) =>  true
	 (defun my-function (x) x) =>  MY-FUNCTION
	 (fboundp 'my-function) =>  true
	 (let ((saved-definition (symbol-function 'my-function)))
	   (unwind-protect (progn (fmakunbound 'my-function)
	                          (fboundp 'my-function))
	     (setf (symbol-function 'my-function) saved-definition)))
	=>  false
	 (fboundp 'my-function) =>  true
	 (defmacro my-macro (x) `',x) =>  MY-MACRO
	 (fboundp 'my-macro) =>  true
	 (fmakunbound 'my-function) =>  MY-FUNCTION
	 (fboundp 'my-function) =>  false
	 (flet ((my-function (x) x))
	   (fboundp 'my-function)) =>  false
)

(dotests FMAKUNBOUND
	(defun add-some (x) (+ x 19)) =>  ADD-SOME
	 (fboundp 'add-some) =>  true
	 (flet ((add-some (x) (+ x 37)))
	    (fmakunbound 'add-some)
	    (add-some 1)) =>  38
	 (fboundp 'add-some) =>  false
)

;;; RGC: LABELS: need to check handling of declarations
(dotests FLET/LABELS/MACROLET
	 (flet ((flet1 (n) (+ n n)))
	    (flet ((flet1 (n) (+ 2 (flet1 n))))
	      (flet1 2))) =>  6
	
	 (defun dummy-function () 'top-level) =>  DUMMY-FUNCTION 
	 (funcall #'dummy-function) =>  TOP-LEVEL 
	 (flet ((dummy-function () 'shadow)) 
	      (funcall #'dummy-function)) =>  SHADOW 
	 (eq (funcall #'dummy-function) (funcall 'dummy-function))
	=>  true 
	 (flet ((dummy-function () 'shadow))
	   (eq (funcall #'dummy-function)
	       (funcall 'dummy-function)))
	=>  false 
	
	 (defun recursive-times (k n)
	   (labels ((temp (n) 
	              (if (zerop n) 0 (+ k (temp (1- n))))))
	     (temp n))) =>  RECURSIVE-TIMES
	 (recursive-times 2 3) =>  6
	
	 (defmacro mlets (x &environment env) 
	    (let ((form `(babbit ,x)))
	      (macroexpand form env))) =>  MLETS
	 (macrolet ((babbit (z) `(+ ,z ,z))) (mlets 5)) =>  10
	
	
	 (flet ((safesqrt (x) (sqrt (abs x))))
	  ;; The safesqrt function is used in two places.
	   (safesqrt (apply #'+ (map 'list #'safesqrt '(1 2 3 4 5 6)))))
	=>  3.291173
	
	 (defun integer-power (n k)     
	   (declare (integer n))         
	   (declare (type (integer 0 *) k))
	   (labels ((expt0 (x k a)
	              (declare (integer x a) (type (integer 0 *) k))
	              (cond ((zerop k) a)
	                    ((evenp k) (expt1 (* x x) (floor k 2) a))
	                    (t (expt0 (* x x) (floor k 2) (* x a)))))
	            (expt1 (x k a)
	              (declare (integer x a) (type (integer 0 *) k))
	              (cond ((evenp k) (expt1 (* x x) (floor k 2) a))
	                    (t (expt0 (* x x) (floor k 2) (* x a))))))
	    (expt0 n k 1))) =>  INTEGER-POWER
	
	 (defun example (y l)
	   (flet ((attach (x)
	            (setq l (append l (list x)))))
	     (declare (inline attach))
	     (dolist (x y)
	       (unless (null (cdr x))
	         (attach x)))
	     l)) => EXAMPLE
	
	 (example '((a apple apricot) (b banana) (c cherry) (d) (e))
	          '((1) (2) (3) (4 2) (5) (6 3 2)))
	=>  ((1) (2) (3) (4 2) (5) (6 3 2) (A APPLE APRICOT) (B BANANA) (C CHERRY))
)

(dotests FUNCALL
	 (funcall #'+ 1 2 3) =>  6
	 (funcall 'car '(1 2 3)) =>  1
	 (funcall 'position 1 '(1 2 3 2 1) :start 1) =>  4
	 (cons 1 2) =>  (1 . 2)
	 (flet ((cons (x y) `(kons ,x ,y)))
	   (let ((cons (symbol-function '+)))
	     (funcall #'cons
	              (funcall 'cons 1 2)
	              (funcall cons 1 2))))
	=>  (KONS (1 . 2) 3)
)

(dotests FUNCTION
	 (defun adder (x) (function (lambda (y) (+ x y)))) => ADDER
	 (setq add3 (adder 3)) => true ;; had to put something here -RGC
	 (funcall add3 5) =>  8
)

(dotests FUNCTION-LAMBDA-EXPRESSION
	(function-lambda-expression #'(lambda (x) x)) => implementation-dependent
	(function-lambda-expression (funcall #'(lambda () #'(lambda (x) x)))) => implementation-dependent
	(function-lambda-expression (funcall #'(lambda (x) #'(lambda () x)) nil)) => implementation-dependent
	(flet ((foo (x) x))
		(setf (symbol-function 'bar) #'foo)
		(function-lambda-expression #'bar)) => implementation-dependent
	(defun foo ()
		(flet ((bar (x) x))
			#'bar)) => FOO
	(function-lambda-expression (foo)) => implementation-dependent
)

(dotests FUNCTIONP
	 (functionp 'append) =>  false
	 (functionp #'append) =>  true
	 (functionp (symbol-function 'append)) =>  true
	 (flet ((f () 1)) (functionp #'f)) =>  true
	 (functionp (compile nil '(lambda () 259))) =>  true
	 (functionp nil) =>  false
	 (functionp 12) =>  false
	 (functionp '(lambda (x) (* x x))) =>  false
	 (functionp #'(lambda (x) (* x x))) =>  true
)

(dotests COMPILED-FUNCTIONP
	 (defun f (x) x) =>  F
	 (compiled-function-p #'f) =>  implementation-dependent
	 (compiled-function-p 'f) =>  false
	 (compile 'f) =>  F
	 (compiled-function-p #'f) =>  true
	 (compiled-function-p 'f) =>  false
	 (compiled-function-p (compile nil '(lambda (x) x))) =>  true
	 (compiled-function-p #'(lambda (x) x)) =>  implementation-dependent
	 (compiled-function-p '(lambda (x) x)) =>  false
)

(dotests CALL-ARGUMENTS-LIMIT
	call-arguments-limit  =>  implementation-dependent
)

(dotests LAMBDA-PARAMETERS-LIMIT
	lambda-parameters-limit  =>  implementation-dependent
)

(dotests DEFCONSTANT
	 (defconstant this-is-a-constant 'never-changing "for a test") =>  THIS-IS-A-CONSTANT
	 this-is-a-constant =>  NEVER-CHANGING
	 (documentation 'this-is-a-constant 'variable) =>  "for a test"
	 (constantp 'this-is-a-constant) =>  true
)

(dotests DEFPARAMETER/DEFVAR
	 (defparameter *p* 1) =>  *P*
	 *p* =>  1
	 (constantp '*p*) =>  false
	 (setq *p* 2) =>  2
	 (defparameter *p* 3) =>  *P*
	 *p* =>  3
	
	 (defvar *v* 1) =>  *V*
	 *v* =>  1
	 (constantp '*v*) =>  false
	 (setq *v* 2) =>  2
	 (defvar *v* 3) =>  *V*
	 *v* =>  2
	
	 (defun foo ()
	   (let ((*p* 'p) (*v* 'v))
	     (bar))) =>  FOO
	 (defun bar () (list *p* *v*)) =>  BAR
	 (foo) =>  (P V)
)

(dotests DESTRUCTURING-BIND
	 (defun iota (n) (loop for i from 1 to n collect i)) => IOTA       ;helper
	 (destructuring-bind ((a &optional (b 'bee)) one two three)
	     `((alpha) ,@(iota 3))
	   (list a b three two one)) =>  (ALPHA BEE 3 2 1)
)

(dotests LET/LET*
	 (setq a 'top) =>  TOP
	 (defun dummy-function () a) =>  DUMMY-FUNCTION
	 (let ((a 'inside) (b a))
	    (format nil "~S ~S ~S" a b (dummy-function))) =>  "INSIDE TOP TOP" 
	 (let* ((a 'inside) (b a))
	    (format nil "~S ~S ~S" a b (dummy-function))) =>  "INSIDE INSIDE TOP" 
	 (let ((a 'inside) (b a))
	    (declare (special a))
	    (format nil "~S ~S ~S" a b (dummy-function))) =>  "INSIDE TOP INSIDE"
)

(dotests PROGV
	 (setq *x* 1) =>  1
	 (progv '(*x*) '(2) *x*) =>  2
	 *x* =>  1
	
	;;Assuming *x* is not globally special,
	
	 (let ((*x* 3)) 
	    (progv '(*x*) '(4) 
	      (list *x* (symbol-value '*x*)))) =>  (3 4)
)

(dotests SETQ
	 ;; A simple use of SETQ to establish values for variables.
	 (setq a 1 b 2 c 3) =>  3
	 a =>  1
	 b =>  2
	 c =>  3
	
	 ;; Use of SETQ to update values by sequential assignment.
	 (setq a (1+ b) b (1+ a) c (+ a b)) =>  7
	 a =>  3
	 b =>  4
	 c =>  7
	
	 ;; This illustrates the use of SETQ on a symbol macro.
	 (let ((x (list 10 20 30)))
	   (symbol-macrolet ((y (car x)) (z (cadr x)))
	     (setq y (1+ z) z (1+ y))
	     (list x y z)))
	=>  ((21 22 30) 21 22)
)	

(dotests PSETQ
	 ;; A simple use of PSETQ to establish values for variables.
	 ;; As a matter of style, many programmers would prefer SETQ 
	 ;; in a simple situation like this where parallel assignment
	 ;; is not needed, but the two have equivalent effect.
	 (psetq a 1 b 2 c 3) =>  NIL
	 a =>  1
	 b =>  2
	 c =>  3
	
	 ;; Use of PSETQ to update values by parallel assignment.
	 ;; The effect here is very different than if SETQ had been used.
	 (psetq a (1+ b) b (1+ a) c (+ a b)) =>  NIL
	 a =>  3
	 b =>  2
	 c =>  3
	
	 ;; Use of PSETQ on a symbol macro.
	 (let ((x (list 10 20 30)))
	   (symbol-macrolet ((y (car x)) (z (cadr x)))
	     (psetq y (1+ z) z (1+ y))
	     (list x y z)))
	=>  ((21 11 30) 21 11)
	
	 ;; Use of parallel assignment to swap values of A and B.
	 (let ((a 1) (b 2))
	   (psetq a b  b a)
	   (values a b))
	=>  (values 2 1)
)

(dotests BLOCK	
	 (block empty) =>  NIL
	 (block whocares (values 1 2) (values 3 4)) =>  (values 3 4)
	 (let ((x 1)) 
	   (block stop (setq x 2) (return-from stop) (setq x 3))
	   x) =>  2
	 (block early (return-from early (values 1 2)) (values 3 4)) =>  (values 1 2)
	 (block outer (block inner (return-from outer 1)) 2) =>  1
	 (block twin (block twin (return-from twin 1)) 2) =>  2
	 ;; Contrast behavior of this example with corresponding example of CATCH.
	 (block b
	   (flet ((b1 () (return-from b 1)))
	     (block b (b1) (print 'unreachable))
	     2)) =>  1
)

(dotests CATCH
	 (catch 'dummy-tag 1 2 (throw 'dummy-tag 3) 4) =>  3
	 (catch 'dummy-tag 1 2 3 4) =>  4
	 (defun throw-back (tag) (throw tag t)) =>  THROW-BACK
	 (catch 'dummy-tag (throw-back 'dummy-tag) 2) =>  T
	
	 ;; Contrast behavior of this example with corresponding example of BLOCK.
	 (catch 'c
	   (flet ((c1 () (throw 'c 1)))
	     (catch 'c (c1) (print 'unreachable))
	     2)) =>  2
)

(dotests GO
	 (tagbody
	   (setq val 2)
	   (go lp)
	   (incf val 3)
	   lp (incf val 4)) =>  NIL
	 val =>  6 
)

(dotests RETURN-FROM
	 (block alpha (return-from alpha) 1) =>  NIL
	 (block alpha (return-from alpha 1) 2) =>  1
	 (block alpha (return-from alpha (values 1 2)) 3) =>  (values 1 2)
	 (let ((a 0))
	    (dotimes (i 10) (incf a) (when (oddp i) (return)))
	    a) =>  2
	 (defun temp (x)
	    (if x (return-from temp 'dummy))
	    44) =>  TEMP
	 (temp nil) =>  44
	 (temp t) =>  DUMMY
	 (block out
	   (flet ((exit (n) (return-from out n)))
	     (block out (exit 1)))
	   2) =>  1
	 (block nil   
	   (unwind-protect (return-from nil 1)
	     (return-from nil 2)))
	=>  2
	 (dolist (flag '(nil t))
	   (block nil
	     (let ((x 5))
	       (declare (special x))
	       (unwind-protect (return-from nil)
	         (print x))))
	   (print 'here))
	;;>>  5
	;;>>  HERE
	;;>>  5
	;;>>  HERE
	=>  NIL
	 (dolist (flag '(nil t))
	   (block nil
	     (let ((x 5))
	       (declare (special x))
	       (unwind-protect
	           (if flag (return-from nil))
	         (print x))))
	   (print 'here))
	;;>>  5
	;;>>  HERE
	;;>>  5
	;;>>  HERE
	=>  NIL
)

(dotests RETURN
	 (block nil (return) 1) =>  NIL
	 (block nil (return 1) 2) =>  1
	 (block nil (return (values 1 2)) 3) =>  (values 1 2)
	 (block nil (block alpha (return 1) 2)) =>  1
	 (block alpha (block nil (return 1)) 2) =>  2
	 (block nil (block nil (return 1) 2)) =>  1
)

(dotests TAGBODY
	 (let (val)
	    (tagbody
	      (setq val 1)
	      (go point-a)
	      (incf val 16)
	     point-c
	      (incf val 04)
	      (go point-b)
	      (incf val 32)
	     point-a
	      (incf val 02)
	      (go point-c)
	      (incf val 64)
	     point-b
	      (incf val 08))
	    val)
	=>  15
	 (defun f1 (flag)
	   (let ((n 1))
	     (tagbody 
	       (setq n (f2 flag #'(lambda () (go out))))
	      out
	       (prin1 n))))
	=>  F1
	 (defun f2 (flag escape)
	   (if flag (funcall escape) 2))
	=>  F2
	 (f1 nil)
	;;>>  2
	=>  NIL
	 (f1 t)
	;;>>  1
	=>  NIL
)

(dotests THROW
	 (catch 'result
	    (setq i 0 j 0)
	    (loop (incf j 3) (incf i)
	          (if (= i 3) (throw 'result (values i j))))) =>  (values 3 9)
	 (catch nil 
	   (unwind-protect (throw nil 1)
	     (throw nil 2))) =>  2
	 (catch 'foo
	         (format t "The inner catch returns ~s.~%"
	                 (catch 'foo
	                     (unwind-protect (throw 'foo :first-throw)
	                         (throw 'foo :second-throw))))
	         :outer-catch)
	;;>>  The inner catch returns :SECOND-THROW
	=>  :OUTER-CATCH
)

(dotests UNWIND-PROTECT
	 (defun dummy-function (x)
	    (setq state 'running)
	    (unless (numberp x) (throw 'abort 'not-a-number))
	    (setq state (1+ x))) =>  DUMMY-FUNCTION
	 (catch 'abort (dummy-function 1)) =>  2
	 state =>  2
	 (catch 'abort (dummy-function 'trash)) =>  NOT-A-NUMBER
	 state =>  RUNNING
	 (catch 'abort (unwind-protect (dummy-function 'trash) 
	                  (setq state 'aborted))) =>  NOT-A-NUMBER
	 state =>  ABORTED

	;;; The following returns 2.
 	 (block nil   
	   (unwind-protect (return 1)
	     (return 2))) => 2
	 
	;;; The following has undefined consequences.
 	 (block a    
	   (block b
	     (unwind-protect (return-from a 1)
	       (return-from b 2)))) => implementation-dependent
	 
	;;; The following returns 2.
	 (catch nil 
	   (unwind-protect (throw nil 1)
	     (throw nil 2))) => 2
	 
	;;; The following has undefined consequences because the catch of B is 
	;;; passed over by the first THROW, hence portable programs must assume 
	;;; its dynamic extent is terminated.  The binding of the catch tag is not
	;;; yet disestablished and therefore it is the target of the second throw.
	 (catch 'a
	   (catch 'b
	     (unwind-protect (throw 'a 1)
	       (throw 'b 2)))) => implementation-dependent
	 
	;;; The following prints "The inner catch returns :SECOND-THROW"
	;;; and then returns :OUTER-CATCH.
	 (catch 'foo
	         (format t "The inner catch returns ~s.~%"
	                 (catch 'foo
	                     (unwind-protect (throw 'foo :first-throw)
	                         (throw 'foo :second-throw))))
	         :outer-catch) => :OUTER-CATCH
	 
	 
	;;; The following returns 10. The inner CATCH of A is passed over, but 
	;;; because that CATCH is disestablished before the THROW to A is executed,
	;;; it isn't seen.
	 (catch 'a
	   (catch 'b
	     (unwind-protect (1+ (catch 'a (throw 'b 1)))
	       (throw 'a 10)))) => 10
	 
	 
	;;; The following has undefined consequences because the extent of
	;;; the (CATCH 'BAR ...) exit ends when the (THROW 'FOO ...)
	;;; commences.
	 (catch 'foo
	   (catch 'bar
	       (unwind-protect (throw 'foo 3)
	         (throw 'bar 4)
	         (print 'xxx)))) => implementation-dependent
	 
	 
	;;; The following returns 4; XXX is not printed.
	;;; The (THROW 'FOO ...) has no effect on the scope of the BAR
	;;; catch tag or the extent of the (CATCH 'BAR ...) exit.
	 (catch 'bar
	   (catch 'foo
	       (unwind-protect (throw 'foo 3)
	         (throw 'bar 4)
	         (print 'xxx)))) => 4
	 
	 
	;;; The following prints 5.
	 (block nil
	   (let ((x 5))
	     (declare (special x))
	     (unwind-protect (return)
	       (print x)))) => NIL ;; RGC          
)

(dotests NIL
	nil =>  NIL 
)

(dotests NOT
	 (not nil) =>  T
	 (not '()) =>  T
	 (not (integerp 'sss)) =>  T
	 (not (integerp 1)) =>  NIL
	 (not 3.7) =>  NIL
	 (not 'apple) =>  NIL
)

(dotests T
	 t =>  T 
	 (eq t 't) =>  true
	 (find-class 't) =>  true ; RGC #<CLASS T 610703333>
	 (case 'a (a 1) (t 2)) =>  1
	 (case 'b (a 1) (t 2)) =>  2
	 (prin1 'hello t)
	;;>>  HELLO
	=>  HELLO
)

(dotests EQ
	 (eq 'a 'b) =>  false
	 (eq 'a 'a) =>  true
	 (eq 3 3) =>  implementation-dependent
	 (eq 3 3.0) =>  false
	 (eq 3.0 3.0) =>  implementation-dependent
	 (eq #c(3 -4) #c(3 -4)) =>  implementation-dependent
	 (eq #c(3 -4.0) #c(3 -4)) =>  false
	 (eq (cons 'a 'b) (cons 'a 'c)) =>  false
	 (eq (cons 'a 'b) (cons 'a 'b)) =>  false
	 (eq '(a . b) '(a . b)) =>  implementation-dependent
	 (progn (setq x (cons 'a 'b)) (eq x x)) =>  true
	 (progn (setq x '(a . b)) (eq x x)) =>  true
	 (eq #\A #\A) =>  implementation-dependent
	 (let ((x "Foo")) (eq x x)) =>  true
	 (eq "Foo" "Foo") =>  implementation-dependent
	 (eq "Foo" (copy-seq "Foo")) =>  false
	 (eq "FOO" "foo") =>  false
	 (eq "string-seq" (copy-seq "string-seq")) =>  false
	 (let ((x 5)) (eq x x)) =>  implementation-dependent
)

(dotests EQL
	 (eql 'a 'b) =>  false
	 (eql 'a 'a) =>  true
	 (eql 3 3) =>  true
	 (eql 3 3.0) =>  false
	 (eql 3.0 3.0) =>  true
	 (eql #c(3 -4) #c(3 -4)) =>  true
	 (eql #c(3 -4.0) #c(3 -4)) =>  false
	 (eql (cons 'a 'b) (cons 'a 'c)) =>  false
	 (eql (cons 'a 'b) (cons 'a 'b)) =>  false
	 (eql '(a . b) '(a . b)) =>  implementation-dependent
	 (progn (setq x (cons 'a 'b)) (eql x x)) =>  true
	 (progn (setq x '(a . b)) (eql x x)) =>  true
	 (eql #\A #\A) =>  true
	 (eql "Foo" "Foo") =>  implementation-dependent
	 (eql "Foo" (copy-seq "Foo")) =>  false
	 (eql "FOO" "foo") =>  false
)

(dotests EQUAL
	 (equal 'a 'b) =>  false
	 (equal 'a 'a) =>  true
	 (equal 3 3) =>  true
	 (equal 3 3.0) =>  false
	 (equal 3.0 3.0) =>  true
	 (equal #c(3 -4) #c(3 -4)) =>  true
	 (equal #c(3 -4.0) #c(3 -4)) =>  false
	 (equal (cons 'a 'b) (cons 'a 'c)) =>  false
	 (equal (cons 'a 'b) (cons 'a 'b)) =>  true
	 (equal #\A #\A) =>  true
	 (equal #\A #\a) =>  false
	 (equal "Foo" "Foo") =>  true
	 (equal "Foo" (copy-seq "Foo")) =>  true
	 (equal "FOO" "foo") =>  false
	 (equal "This-string" "This-string") =>  true
	 (equal "This-string" "this-string") =>  false
)

(dotests EQUALP
	 (equalp 'a 'b) =>  false
	 (equalp 'a 'a) =>  true
	 (equalp 3 3) =>  true
	 (equalp 3 3.0) =>  true
	 (equalp 3.0 3.0) =>  true
	 (equalp #c(3 -4) #c(3 -4)) =>  true
	 (equalp #c(3 -4.0) #c(3 -4)) =>  true
	 (equalp (cons 'a 'b) (cons 'a 'c)) =>  false
	 (equalp (cons 'a 'b) (cons 'a 'b)) =>  true
	 (equalp #\A #\A) =>  true
	 (equalp #\A #\a) =>  true
	 (equalp "Foo" "Foo") =>  true
	 (equalp "Foo" (copy-seq "Foo")) =>  true
	 (equalp "FOO" "foo") =>  true
	
	 (setq array1 (make-array 6 :element-type 'integer
	                            :initial-contents '(1 1 1 3 5 7))) 
	=>  #(1 1 1 3 5 7)
	 (setq array2 (make-array 8 :element-type 'integer
	                            :initial-contents '(1 1 1 3 5 7 2 6)
	                            :fill-pointer 6))
	=>  #(1 1 1 3 5 7)
	 (equalp array1 array2) =>  true
	 (setq vector1 (vector 1 1 1 3 5 7)) =>  #(1 1 1 3 5 7)
	 (equalp array1 vector1) =>  true 
)

(dotests IDENTITY
	 (identity 101) =>  101
	 (mapcan #'identity (list (list 1 2 3) '(4 5 6))) =>  (1 2 3 4 5 6)
)

(dotests COMPLEMENT
	 (funcall (complement #'zerop) 1) =>  true
	 (funcall (complement #'characterp) #\A) =>  false
	 (funcall (complement #'member) 'a '(a b c)) =>  false
	 (funcall (complement #'member) 'd '(a b c)) =>  true
)

(dotests CONSTANTLY
	 (mapcar (constantly 3) '(a b c d)) =>  (3 3 3 3)
	 (defmacro with-vars (vars &body forms)
	   `((lambda ,vars ,@forms) ,@(mapcar (constantly nil) vars)))
	=>  WITH-VARS
	 (macroexpand '(with-vars (a b) (setq a 3 b (* a a)) (list a b)))
	=>  (values ((LAMBDA (A B) (SETQ A 3 B (* A A)) (LIST A B)) NIL NIL) true)
)

(dotests EVERY/SOME/NOTEVERY/NOTANY
	 (every #'characterp "abc") =>  true
	 (some #'= '(1 2 3 4 5) '(5 4 3 2 1)) =>  true
	 (notevery #'< '(1 2 3 4) '(5 6 7 8) '(9 10 11 12)) =>  false
	 (notany #'> '(1 2 3 4) '(5 6 7 8) '(9 10 11 12)) =>  true 
)

(dotests AND
	 (setq temp1 1 temp2 1 temp3 1) =>  1 
	 (and (incf temp1) (incf temp2) (incf temp3)) =>  2 
	 (and (eql 2 temp1) (eql 2 temp2) (eql 2 temp3)) =>  true
	 (decf temp3) =>  1 
	 (and (decf temp1) (decf temp2) (eq temp3 'nil) (decf temp3)) =>  NIL 
	 (and (eql temp1 temp2) (eql temp2 temp3)) =>  true
	 (and) =>  T 
)

(dotests COND
	 (defun select-options ()
	   (cond ((= a 1) (setq a 2))
	         ((= a 2) (setq a 3))
	         ((and (= a 3) (floor a 2)))
	         (t (floor a 3)))) =>  SELECT-OPTIONS
	 (setq a 1) =>  1
	 (select-options) =>  2
	 a =>  2
	 (select-options) =>  3
	 a =>  3
	 (select-options) =>  1
	 (setq a 5) =>  5
	 (select-options) =>  (values 1 2)
)

(dotests IF
	 (if t 1) =>  1
	 (if nil 1 2) =>  2 
	 (defun test ()
	   (dolist (truth-value '(t nil 1 (a b c)))
	     (if truth-value (print 'true) (print 'false))
	     (prin1 truth-value))) =>  TEST
	 (test)
	;;>>  TRUE T
	;;>>  FALSE NIL
	;;>>  TRUE 1
	;;>>  TRUE (A B C)
	=>  NIL
)

(dotests OR
	 (or) =>  NIL 
	 (setq temp0 nil temp1 10 temp2 20 temp3 30) =>  30
	 (or temp0 temp1 (setq temp2 37)) =>  10
	 temp2 =>  20
	 (or (incf temp1) (incf temp2) (incf temp3)) =>  11
	 temp1 =>  11
	 temp2 =>  20
	 temp3 =>  30
	 (or (values) temp1) =>  11
	 (or (values temp1 temp2) temp3) =>  11
	 (or temp0 (values temp1 temp2)) =>  (values 11 20)
	 (or (values temp0 temp1) (values temp2 temp3)) =>  (values 20 30)
)

(dotests WHEN/UNLESS
	 (when t 'hello) =>  HELLO
	 (unless t 'hello) =>  NIL
	 (when nil 'hello) =>  NIL
	 (unless nil 'hello) =>  HELLO
	 (when t) =>  NIL
	 (unless nil) =>  NIL
	 (when t (prin1 1) (prin1 2) (prin1 3))
	;;>>  123
	=>  3
	 (unless t (prin1 1) (prin1 2) (prin1 3)) =>  NIL
	 (when nil (prin1 1) (prin1 2) (prin1 3)) =>  NIL
	 (unless nil (prin1 1) (prin1 2) (prin1 3))
	;;>>  123
	=>  3
	 (let ((x 3))
	   (list (when (oddp x) (incf x) (list x))
	         (when (oddp x) (incf x) (list x))
	         (unless (oddp x) (incf x) (list x))
	         (unless (oddp x) (incf x) (list x))
	         (if (oddp x) (incf x) (list x)) 
	         (if (oddp x) (incf x) (list x)) 
	         (if (not (oddp x)) (incf x) (list x)) 
	         (if (not (oddp x)) (incf x) (list x))))
	=>  ((4) NIL (5) NIL 6 (6) 7 (7))
)

(dotests CASE/CCASE/ECASE
	 (dolist (k '(1 2 3 :four #\v () t 'other))
	    (format t "~S "
	       (case k ((1 2) 'clause1)
	               (3 'clause2)
	               (nil 'no-keys-so-never-seen)
	               ((nil) 'nilslot)
	               ((:four #\v) 'clause4)
	               ((t) 'tslot)
	               (otherwise 'others)))) 
	;;>>  CLAUSE1 CLAUSE1 CLAUSE2 CLAUSE4 CLAUSE4 NILSLOT TSLOT OTHERS 
	=>  NIL
	 (defun add-em (x) (apply #'+ (mapcar #'decode x)))
	=>  ADD-EM
	 (defun decode (x)
	   (ccase x
	     ((i uno) 1)
	     ((ii dos) 2)
	     ((iii tres) 3)
	     ((iv cuatro) 4)))
	=>  DECODE
	 (add-em '(uno iii)) =>  4
	 (add-em '(uno iiii))
	;;>>  Error: The value of X, IIII, is not I, UNO, II, DOS, III,
	;;>>         TRES, IV, or CUATRO.
	;;>>   1: Supply a value to use instead.
	;;>>   2: Return to Lisp Toplevel.
	;;>>  Debug> :CONTINUE 1
	;;>>  Value to evaluate and use for X: 'IV
	=>  5
)

(dotests TYPECASE/CTYPECASE/ETYPECASE
	;;; (Note that the parts of this example which use TYPE-OF 
	;;;  are implementation-dependent.)
	 (defun what-is-it (x)
	   (format t "~&~S is ~A.~%"
	           x (typecase x
	               (float "a float")
	               (null "a symbol, boolean false, or the empty list")
	               (list "a list")
	               (t (format nil "a(n) ~(~A~)" (type-of x))))))
	=>  WHAT-IS-IT
	 (map 'nil #'what-is-it '(nil (a b) 7.0 7 box))
	;;>>  NIL is a symbol, boolean false, or the empty list.
	;;>>  (A B) is a list.
	;;>>  7.0 is a float.
	;;>>  7 is a(n) integer.
	;;>>  BOX is a(n) symbol.
	=>  NIL
	 (setq x 1/3)
	=>  1/3
	 (ctypecase x
	     (integer (* x 4))
	     (symbol  (symbol-value x)))
	;;>>  Error: The value of X, 1/3, is neither an integer nor a symbol.
	;;>>  To continue, type :CONTINUE followed by an option number:
	;;>>   1: Specify a value to use instead.
	;;>>   2: Return to Lisp Toplevel.
	;;>>  Debug> :CONTINUE 1
	;;>>  Use value: 3.7
	;;>>  Error: The value of X, 3.7, is neither an integer nor a symbol.
	;;>>  To continue, type :CONTINUE followed by an option number:
	;;>>   1: Specify a value to use instead.
	;;>>   2: Return to Lisp Toplevel.
	;;>>  Debug> :CONTINUE 1
	;;>>  Use value: 12
	=>  48
	 x =>  12
)

(dotests MULTIPLE-VALUE-BIND
	 (multiple-value-bind (f r) 
	     (floor 130 11)
	   (list f r)) =>  (11 9)
)

(dotests MULTIPLE-VALUE-CALL
	 (multiple-value-call #'list 1 '/ (values 2 3) '/ (values) '/ (floor 2.5))
	=>  (1 / 2 3 / / 2 0.5)
	 (+ (floor 5 3) (floor 19 4))
	=>  5
	 (multiple-value-call #'+ (floor 5 3) (floor 19 4)) ==  (+ 1 2 4 3)
	=>  10
)

(dotests MULTIPLE-VALUE-LIST
	(multiple-value-list (floor -3 4)) =>  (-1 1)
)

(dotests MULTIPLE-VALUE-PROG1
	 (setq temp '(1 2 3)) =>  (1 2 3)
	 (multiple-value-prog1
	    (values-list temp)
	    (setq temp nil)
	    (values-list temp)) =>  (values 1 2 3)
)

(dotests MULTIPLE-VALUE-SETQ
	 (multiple-value-setq (quotient remainder) (truncate 3.2 2)) =>  1
	 quotient =>  1
	 remainder =>  1.2
	 (multiple-value-setq (a b c) (values 1 2)) =>  1
	 a =>  1
	 b =>  2
	 c =>  NIL
	 (multiple-value-setq (a b) (values 4 5 6)) =>  4
	 a =>  4
	 b =>  5
)

(dotests VALUES
	 (values) =>  NIL ;RGC <no values>
	 (values 1) =>  1
	 (values 1 2) =>  (values 1 2)
	 (values 1 2 3) =>  (values 1 2 3)
	 (values (values 1 2 3) 4 5) =>  (values 1 4 5)
	 (defun polar (x y)
	   (values (sqrt (+ (* x x) (* y y))) (atan y x))) =>  POLAR
	 (multiple-value-bind (r theta) (polar 3.0 4.0)
	   (vector r theta))
	=>  #(5.0 0.927295)
)

(dotests VALUES-LIST
	 (values-list nil) =>  NIL ;RGC <no values>
	 (values-list '(1)) =>  1
	 (values-list '(1 2)) =>  (values 1 2)
	 (values-list '(1 2 3)) =>  (values 1 2 3)
)

(dotests MULTIPLE-VALUES-LIMIT
	multiple-values-limit => implementation-dependent
)

(dotests NTH-VALUE
	 (nth-value 0 (values 'a 'b)) =>  A
	 (nth-value 1 (values 'a 'b)) =>  B
	 (nth-value 2 (values 'a 'b)) =>  NIL
	 (let* ((x 83927472397238947423879243432432432)
	        (y 32423489732)
	        (a (nth-value 1 (floor x y)))
	        (b (mod x y)))
	   (values a b (= a b)))
	=>  (values 3332987528 3332987528 true)
)

(dotests PROG/PROG*
	 (setq a 1) =>  1
	 (prog ((a 2) (b a)) (return (if (= a b) '= '/=))) =>  /=
	 (prog* ((a 2) (b a)) (return (if (= a b) '= '/=))) =>  =
	 (prog () 'no-return-value) =>  NIL
	
	 (defun king-of-confusion (w)
	   "Take a cons of two lists and make a list of conses.
	    Think of this function as being like a zipper."
	   (prog (x y z)          ;Initialize x, y, z to NIL
	        (setq y (car w) z (cdr w))
	    loop
	        (cond ((null y) (return x))
	              ((null z) (go err)))
	    rejoin
	        (setq x (cons (cons (car y) (car z)) x))
	        (setq y (cdr y) z (cdr z))
	        (go loop)
	    err
	        (cerror "Will self-pair extraneous items"
	                "Mismatch - gleep!  ~S" y)
	        (setq z y)
	        (go rejoin))) =>  KING-OF-CONFUSION 
	
	 (defun prince-of-clarity (w)
	   "Take a cons of two lists and make a list of conses.
	    Think of this function as being like a zipper."
	   (do ((y (car w) (cdr y))
	        (z (cdr w) (cdr z))
	        (x '() (cons (cons (car y) (car z)) x)))
	       ((null y) x)
	     (when (null z)
	       (cerror "Will self-pair extraneous items"
	              "Mismatch - gleep!  ~S" y)
	       (setq z y)))) =>  PRINCE-OF-CLARITY 
)

(dotests PROG1/PROG2
	 (setq temp 1) =>  1
	 (prog1 temp (print temp) (incf temp) (print temp))
	;;>>  1
	;;>>  2
	=>  1
	 (prog1 temp (setq temp nil)) =>  2
	 temp =>  NIL
	 (prog1 (values 1 2 3) 4) =>  1 
	 (setq temp (list 'a 'b 'c)) => (A B C) ;; RGC
	 (prog1 (car temp) (setf (car temp) 'alpha)) =>  A
	 temp =>  (ALPHA B C)
	 (flet ((swap-symbol-values (x y)
	          (setf (symbol-value x) 
	                (prog1 (symbol-value y)
	                       (setf (symbol-value y) (symbol-value x))))))
	   (let ((*foo* 1) (*bar* 2))
	     (declare (special *foo* *bar*))
	     (swap-symbol-values '*foo* '*bar*)
	     (values *foo* *bar*)))
	=>  (values 2 1)
	 (setq temp 1) =>  1
	 (prog2 (incf temp) (incf temp) (incf temp)) =>  3
	 temp =>  4
	 (prog2 1 (values 2 3 4) 5) =>  2
)

(dotests PROGN
	 (progn) =>  NIL
	 (progn 1 2 3) =>  3
	 (progn (values 1 2 3)) =>  (values 1 2 3)
	 (setq a 1) =>  1
	 (if a
	      (progn (setq a nil) 'here)
	      (progn (setq a t) 'there)) =>  HERE
	 a =>  NIL
)

(dotests DEFINE-MODIFY-MACRO
	 (define-modify-macro appendf (&rest args) 
	    append "Append onto list") =>  APPENDF
	 (setq x '(a b c) y x) =>  (A B C)
	 (appendf x '(d e f) '(1 2 3)) =>  (A B C D E F 1 2 3)
	 x =>  (A B C D E F 1 2 3)
	 y =>  (A B C)
	 (define-modify-macro new-incf (&optional (delta 1)) +) => NEW-INCF
	 (define-modify-macro unionf (other-set &rest keywords) union) => UNIONF
)

(dotests DEFSETF
	 (defun middleguy (x) (nth (truncate (1- (list-length x)) 2) x)) =>  MIDDLEGUY
	 (defun set-middleguy (x v)
	    (unless (null x)
	      (rplaca (nthcdr (truncate (1- (list-length x)) 2) x) v))
	    v) =>  SET-MIDDLEGUY
	 (defsetf middleguy set-middleguy) =>  MIDDLEGUY
	 (setq a (list 'a 'b 'c 'd)
	       b (list 'x)
	       c (list 1 2 3 (list 4 5 6) 7 8 9)) =>  (1 2 3 (4 5 6) 7 8 9)
	 (setf (middleguy a) 3) =>  3
	 (setf (middleguy b) 7) =>  7
	 (setf (middleguy (middleguy c)) 'middleguy-symbol) =>  MIDDLEGUY-SYMBOL
	 a =>  (A 3 C D)
	 b =>  (7)
	 c =>  (1 2 3 (4 MIDDLEGUY-SYMBOL 6) 7 8 9)
	
	;;An example of the use of the long form of defsetf: 
	 (defsetf subseq (sequence start &optional end) (new-sequence)
	   `(progn (replace ,sequence ,new-sequence
	                    :start1 ,start :end1 ,end)
	           ,new-sequence)) =>  SUBSEQ
	
	
	 (defvar *xy* (make-array '(10 10))) => *xy*
	 (defun xy (&key ((x x) 0) ((y y) 0)) (aref *xy* x y)) =>  XY
	 (defun set-xy (new-value &key ((x x) 0) ((y y) 0))
	   (setf (aref *xy* x y) new-value)) =>  SET-XY
	 (defsetf xy (&key ((x x) 0) ((y y) 0)) (store)
	   `(set-xy ,store 'x ,x 'y ,y)) =>  XY
	#|
	 (get-setf-expansion '(xy a b))
	=>  (#:t0 #:t1),
	   (a b),
	   (#:store),
	   ((lambda (&key ((x #:x)) ((y #:y))) 
	      (set-xy #:store 'x #:x 'y #:y))
	    #:t0 #:t1),
	   (xy #:t0 #:t1)
	|#
	 (xy 'x 1) =>  NIL
	 (setf (xy 'x 1) 1) =>  1
	 (xy 'x 1) =>  1
	 (let ((a 'x) (b 'y))
	   (setf (xy a 1 b 2) 3)
	   (setf (xy b 5 a 9) 14))
	=>  14
	 (xy 'y 0 'x 1) =>  1
	 (xy 'x 1 'y 2) =>  3
)

(dotests DEFINE-SETF-EXPANDER
	 (defun lastguy (x) (car (last x))) =>  LASTGUY
	 (define-setf-expander lastguy (x &environment env)
	   "Set the last element in a list to the given value."
	   (multiple-value-bind (dummies vals newval setter getter)
	       (get-setf-expansion x env)
	     (let ((store (gensym)))
	       (values dummies
	               vals
	               `(,store)
	               `(progn (rplaca (last ,getter) ,store) ,store)
	               `(lastguy ,getter))))) =>  LASTGUY
	 (setq a (list 'a 'b 'c 'd)
	       b (list 'x)
	       c (list 1 2 3 (list 4 5 6))) =>  (1 2 3 (4 5 6))
	 (setf (lastguy a) 3) =>  3
	 (setf (lastguy b) 7) =>  7
	 (setf (lastguy (lastguy c)) 'lastguy-symbol) =>  LASTGUY-SYMBOL
	 a =>  (A B C 3)
	 b =>  (7)
	 c =>  (1 2 3 (4 5 LASTGUY-SYMBOL))
)

(dotests GET-SETF-EXPANSION
	 (get-setf-expansion 'x)
	=>  (values NIL NIL true true X) ;; RGC (#:G0001), (SETQ X #:G0001), X 
)

(dotests SETF/PSETF	
	 (setq x (cons 'a 'b) y (list 1 2 3)) =>  (1 2 3) 
	 (setf (car x) 'x (cadr y) (car x) (cdr x) y) =>  (1 X 3) 
	 x =>  (X 1 X 3) 
	 y =>  (1 X 3) 
	 (setq x (cons 'a 'b) y (list 1 2 3)) =>  (1 2 3) 
	 (psetf (car x) 'x (cadr y) (car x) (cdr x) y) =>  NIL 
	 x =>  (X 1 A 3) 
	 y =>  (1 A 3) 
)

(dotests SHIFTF
	 (setq x (list 1 2 3) y 'trash) =>  TRASH
	 (shiftf y x (cdr x) '(hi there)) =>  TRASH
	 x =>  (2 3)
	 y =>  (1 HI THERE)
	
	 (setq x (list 'a 'b 'c)) =>  (A B C)
	 (shiftf (cadr x) 'z) =>  B
	 x =>  (A Z C)
	 (shiftf (cadr x) (cddr x) 'q) =>  Z
	 x =>  (A (C) . Q)
	 (setq n 0) =>  0
	 (setq x (list 'a 'b 'c 'd)) =>  (A B C D)
	 (shiftf (nth (setq n (+ n 1)) x) 'z) =>  B
	 x =>  (A Z C D)
)

(dotests ROTATEF
	 (let ((n 0)
	        (x (list 'a 'b 'c 'd 'e 'f 'g)))
	    (rotatef (nth (incf n) x)
	             (nth (incf n) x)
	             (nth (incf n) x))
	    x) =>  (A C D B E F G)
)