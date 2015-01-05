;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		win-conditions.lisp
;;;;	Contents:	Conditions specific to Win32.
;;;;	History:	3/20/00  RGC  Created.
;;;;				5/4/01   RGC  Modified top level loop so init.lisp
;;;;							  will be loaded from the lisp kernel directory
;;;;							  regardless of what the current directory is.
;;;;
;;; Define Windows-specific OS conditions
(in-package :win32)
(export '(memory-access-violation-error array-bounds-exceeded-error breakpoint
		datatype-misalignment float-stack-check illegal-instruction in-page-error
		invalid-disposition noncontinuable-exception privileged-instruction
		single-step stack-overflow user-abort))

(define-condition memory-access-violation-error (error))
(define-condition array-bounds-exceeded-error (error))
(define-condition breakpoint (serious-condition))
(define-condition datatype-misalignment (error))
(define-condition float-stack-check (error))
(define-condition illegal-instruction (error))
(define-condition in-page-error (error))
(define-condition invalid-disposition (error))
(define-condition noncontinuable-exception (error))
(define-condition privileged-instruction (error))
(define-condition single-step (serious-condition))
(define-condition stack-overflow (storage-condition))
(define-condition user-abort (serious-condition))

(defun cl::%throw-system-exception (ex)
	(let ((condition-type
				(case ex
					(:exception-access-violation 'memory-access-violation-error)
					(:exception-array-bounds-exceeded 'array-bounds-exceeded-error)
					(:exception-breakpoint 'breakpoint)
					(:exception-datatype-misalignment 'datatype-misalignment)
					(:exception-flt-denormal-operand 'floating-point-invalid-operation)
					(:exception-flt-divide-by-zero 'division-by-zero)
					(:exception-flt-inexact-result 'floating-point-inexact)
					(:exception-flt-invalid-operation 'floating-point-invalid-operation)
					(:exception-flt-overflow 'floating-point-overflow)
					(:exception-flt-stack_check 'float-stack-check)
					(:exception-flt-invalid-operation 'floating-point-invalid-operation)
					(:exception-flt-underflow 'floating-point-underflow)
					(:exception-illegal-instruction 'illegal-instruction)
					(:exception-in-page-error 'in-page-error)
					(:exception-int-divide-by-zero 'division-by-zero)
					(:exception-invalid-disposition 'invalid-disposition)
					(:exception-noncontinuable-exception 'noncontinuable-exception)
					(:exception-priv-instruction 'privileged-instruction)
					(:exception-single-step 'single-step)
					(:exception-stack-overflow 'stack-overflow)
					(:exception-user-abort 'user-abort))))
		(error condition-type "A system error of type ~A occurred" condition-type)))

(in-package :cl)

(let ((toplevel-symbol-shortcuts (make-hash-table)))
    (defun find-toplevel-shortcut (symbol)
        (gethash symbol toplevel-symbol-shortcuts))
    (defun add-toplevel-shortcut (symbol expansion)
        (setf (gethash symbol toplevel-symbol-shortcuts) expansion)))


(defparameter ccl::*shutdown-functions* nil)
(defun ccl::register-shutdown-function (func)
    (unless (or (functionp func) (and (symbolp func) (symbol-function func)))
        (error "REGISTER-SHUTDOWN-FUNCTION expected a function, got ~A~%" func))
    (push func ccl::*shutdown-functions*))

(defun ccl::unregister-shutdown-function (func)
    (unless (or (functionp func) (and (symbolp func) (symbol-function func)))
        (error "UNREGISTER-SHUTDOWN-FUNCTION expected a function, got ~A~%" func))
    (setf ccl::*shutdown-functions*
        (remove func ccl::*shutdown-functions*)))

(defun ccl:lisp-shutdown (message)
    (dolist (x ccl::*shutdown-functions*)
        (funcall x))
    (cl::%lisp-shutdown message))

(export '(
        ccl::register-shutdown-function 
        ccl::unregister-shutdown-function 
        ccl::*shutdown-functions*
        ccl::lisp-shutdown) 
    :ccl)
   
;;; Redefine top-level to use conditions.
(defun top-level ()
	(write (copyright-notice) :escape nil)
	(terpri *standard-output*)
	(force-output)
	(let ((init-path (concatenate 'string ccl::*cormanlisp-directory* "init.lisp")))
		(if (and (not *loading-kernel*)(probe-file init-path))
			(load init-path)))
	(do (expr result (stack-overflow nil))
		(nil)
		(restart-case
			(progn
				(block eval-expression
					(handler-bind 
						((win:stack-overflow 
								(lambda (condition) 
									(format *error-output* "~A~%" condition)
									(force-output *error-output*)
									(setf stack-overflow t)
									(return-from eval-expression condition))))
						(progn
							(setq *read-level* 0)
							(write *top-level-prompt* :escape nil) 
							(setq expr (read *standard-input* nil 'Eof nil))
							(if (eq expr 'quit)
								(return 'quit))
							(if (eq expr 'Eof)
								(return 'Eof))
                            
                            ;; add support for top-level shortcuts
                            (if (symbolp expr) 
                                (let ((expansion (find-toplevel-shortcut expr)))
                                    (if expansion (setf expr expansion))))
                            
							(setq - expr)
							(editor-set-message "Working...")
							(unwind-protect
								(setq result (multiple-value-list (eval expr)))
								(editor-set-default-message))					
							(update-toplevel-globals expr result)
							(if (null result) 
								(force-output)
								(dolist (i result)
									(write i)
									(terpri)
									(force-output))))))
				(if stack-overflow (protect-stack)))
			(abort () :report "Abort to top level." 
				(format *standard-output* "~%;;; Returning to top level loop.~%")
				(go next)))
		next))

(setq *top-level* #'top-level)

