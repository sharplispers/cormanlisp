;;;;
;;;;	File:       ansi-examples.lisp
;;;;   Contents:   Examples from the Hyperspec
;;;;

(in-package "COMMON-LISP-USER")

(defun passed-test (expr result)
	(format t "PASSED: ~S => ~S~%" expr result))
(defun failed-test (expr result expected-result)
	(format t "**************~%FAILED: ~S => ~S  Expected: ~S~%**************~%" 
		expr result expected-result))

;;; Like equalp, but the second list can contain true, false designators
(defun test-equalp (a b)
	(or (equalp a b)
		(and (listp a) 
			(listp b)
			(every #'(lambda (x y)
					(or (and x (eq y 'true))
						(and (not x) (eq y 'false))
						(equalp x y)))
				a b))))

(defun verify (symbol examples)
	(format t "Testing ~A~%" symbol)
	(handler-bind
		((error (lambda (condition) (warn "An error occurred: ~A~%" condition) (return-from verify))))
		(do* ((x examples (cdddr x))
			  (expr (first x) (first x))
			  (=> (second x) (second x))
			  (expected-result (third x)(third x)))
			((null x)(format t "~%~%")(values))
			(unless (eq => '=>)
				(error "Invalid format for test:~%~S ~S ~S" expr => expected-result))
			(let ((result (multiple-value-list (eval expr))))
				(case expected-result
					(true (if (car result) 
							(passed-test expr (car result)) 
							(failed-test expr (car result) expected-result)))
					(false (if (car result) 
							(failed-test expr (car result) expected-result) 
							(passed-test expr (car result))))
					(implementation-dependent 
						(passed-test expr (if (> (length result) 1)(cons 'values result) (car result))))
					(t (if (and (consp expected-result)(eq (car expected-result) 'values))
							(if (test-equalp result (cdr expected-result))
								(passed-test expr (cons 'values result))
								(failed-test expr (cons 'values result) expected-result))
							(if (equalp (car result) expected-result)
								(passed-test expr (car result))
								(failed-test expr (car result) expected-result)))))))))

(defmacro dotests (symbol &rest examples)
	`(verify ',symbol ',examples))

(load "test/ansi-chapter-2.lisp")
(load "test/ansi-chapter-3.lisp")
(load "test/ansi-chapter-4.lisp")
(load "test/ansi-chapter-5.lisp")
(load "test/ansi-chapter-6.lisp")
(load "test/ansi-chapter-7.lisp")
(load "test/ansi-chapter-8.lisp")

