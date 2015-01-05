;;;; testkit.lisp -- test suite definition macros for Corman Lisp.

;;;; Author:  Vassili Bykov <vassili@objectpeople.com>
;;;; Created: 12/26/1998

(in-package :cormanlisp)

(export 'define-test-suite)

(defun %parse-define-test-suite-body (forms)
  (let ((tests (list nil))
	(results nil)
	(state :test))
    (dolist (form forms)
      (case state
	(:test
	 (cond ((and (symbolp form) (string= (symbol-name form) "=>"))
		(setq state :result-expected))
	       (t
		(push form (car tests)))))
	(:result-expected
	 (push form results)
	 (push nil tests)
	 (setq state :test))))
    (when (eq state :result-expected)
      (error "Test result form is expected."))
    (when (car tests)
      (format t "Test suite forms are ignored: ~S~%" (car tests)))
    (values (mapcar #'nreverse (nreverse (cdr tests)))
	    (nreverse results))))

(defmacro define-test-suite (name args &body forms)
  (let ((problems (gensym))
	(foobar (gensym)))
    (multiple-value-bind (tests results)
	(%parse-define-test-suite-body forms)
      `(DEFUN ,name ,args
	(LET ((,problems 0))
	  (FORMAT T "~&Running suite ~A~%" ',name)
	  ,@(mapcar
	     #'(lambda (test expected)
		 (cond ((and (consp expected) (member (car expected) '(lambda function)))
			`(LET ((,foobar (PROGN ,@test)))
			  (UNLESS (FUNCALL ,expected ,foobar)
			    (INCF ,problems)
			    (FORMAT T "~&~2TFailed: ~S~%~4Tresult: ~S~%~4Tdoes not pass: ~S~%"
				    ',test ,foobar ',expected))))
		       ((eq :error expected)
			;; The following should be fixed when exceptions are in place.
			`(LET ((,foobar T))
			  (FORMAT T "~&An error is expected now:~%")
			  (CATCH 'COMMON-LISP::%ERROR
			    ,@test
			    (SETQ ,foobar NIL))
			  (UNLESS ,foobar
			    (INCF ,problems)
			    (FORMAT T "~&~2TFailed: ~S~&~4Texpected error not signaled.~%"
				    ',test))))
		       (t
			`(LET ((,foobar (PROGN ,@test)))
			  (UNLESS (EQUALP ,foobar ',expected)
			    (INCF ,problems)
			    (FORMAT T "~&~2TFailed: ~S~%~4Tresult is: ~S~%~4Tmust be: ~S~%"
				    ',test ,foobar ',expected))))))
	     tests results)
	  (if (zerop ,problems)
	      (format t "~&Suite ~A (~D tests) is successful.~%" ',name ,(length tests))
	      (format t "~&Suite ~A completed, ~D out of ~D tests failed.~%"
		      ',name ,problems ,(length tests)))
	  ,problems)))))

(provide "TESTKIT")

#|  EXAMPLES

(define-test-suite test-concatenate ()
  ;; A test suite is a sequence of test expressions and expected
  ;; results, of the form <test> => <result>.  Each test expression is
  ;; evaluated in the lexical environment of its definition. The value
  ;; of the evaluation is then compared to the expected result. How it
  ;; is compared depends of the expected result form:
  (concatenate 'string "all" " " "together" " " "now")
  => "all together now"
  ;; If the expected result is neither a function nor a list starting
  ;; with the symbol LAMBDA, the result of the test expression is
  ;; compared to the (unevaluated) expected result form using EQUAL.
  ;; If they are equal, the test succeeds.
  (concatenate 'list "ABC" '(d e f) #(1 2 3) #*1011)
  => (#\A #\B #\C D E F 1 2 3 1 0 1 1)
  ;; The above illustrates that the expected test result form is not
  ;; evaluated.
  (concatenate 'string "a" 'bc) => :error
  ;; If the expected test result is a keyword :ERROR, an error is
  ;; expected to be signaled while the test expression is being
  ;; evaluated.  If it is, the test succeeds.  If evaluation of the
  ;; test expression successfully completes without an error being
  ;; signaled, the test fails.
  )

(define-test-suite test-foo (size)
  ;; The SIZE argument of the test suite can be used in test
  ;; expressions and result testing functions (since they are
  ;; evaluated in the lexical environment of their definition).
  (setq list-1 (make-list size))
  (setq list-2 (make-list size))
  ;; The results of the previous two expressions are ignored since
  ;; they are not followed by =>; these expressions just set up the
  ;; scene for the following one.  (In fact, the above two expressions
  ;; and the following test are evaluated as a single PROGN).
  (concatenate 'list list-1 list-2)
  => (lambda (result) (= (* 2 size) (length result)))
  ;; If the expected result expression is a function or a form with
  ;; LAMBDA as the first symbol, the form is compiled and the
  ;; resulting function is is applied to the test result, and the
  ;; returned value is treated as a generalized boolean.  NIL means
  ;; test failure, any other value -- test success.
  (member 'a '(1 2 3 a b c))
  => #'identity
  ;; The above rule also works for the case when the expected result
  ;; is a form with FUNCTION as the first symbol.  Note that the above
  ;; was the correct way to test for the expected result (generalized
  ;; true); while "=> t" would not have succeeded.
  )

(defun test-all ()
  ;; Test suites are actually regular functions, they can be combined
  ;; into larger test units.
  (test-concatenate)
  (test-foo 10)
  (test-foo 100000))

|#
