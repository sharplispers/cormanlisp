;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		conditions.lisp
;;;;	Contents:	Conditions functions and declarations.
;;;;	History:	9/23/98  RGC  Created.
;;;;				4/27/01  RGC  Added :initform to arithmetic-error conditions.
;;;;				5/20/01  RGC  ASSERT now evaluates replacement values that are entered
;;;;							  interactively (as the spec examples imply).
;;;;				9/28/01  RGC  Integrated bug fix for ASSERT from JP Massar.
;;;;                4/02/03  RGC  Integrated rewritten CTYPECASE by JP Massar.
;;;;                9/29/03  RGC  HANDLER-BIND now checks that valid types have been
;;;;                              passed to it.
;;;;                              ASSERT now handles the case with no places correctly.
;;;;
;;;;
(in-package "COMMON-LISP")

;;; special variables for private use
(defvar *handler-registry* nil)
(defvar *restart-registry* nil)
(defvar *error-function* nil)
(defvar *debug-condition* nil)

;;;
;;;	Common Lisp *DEBUGGER-HOOK* variable
;;;
(defvar *debugger-hook* nil)

;;;
;;;	Common Lisp *DEBUG-IO* variable
;;;
(defvar *debug-io* *terminal-io*)

;;;
;;; Common Lisp *BREAK-ON-SIGNALS* macro.
;;;
(defvar *break-on-signals* nil)

;;;
;;; Common Lisp DEFINE-CONDITION macro.
;;;
(defmacro define-condition (name parent-types slot-specifiers &rest class-options)
	(if (null parent-types)
		(setf parent-types '(condition)))
	(unless (symbolp name)
		(error "Expected a symbol-name in DEFINE-CONDITION, got ~A" name))
	(let* ((report-clause (first (member ':report class-options :key 'first)))
		   (report-func (second report-clause))
		   (report-definition nil))
		(if (stringp report-func)
			(setq report-func
				`(lambda (condition stream)
					(declare (ignore condition)
						(write-string ,report-func stream)))))
		(if report-func
			(setq report-definition
				`(defmethod print-object ((x ,name) stream)
					(if *print-escape* 
						(call-next-method)
						(funcall (function ,report-func) x stream)))))
					
		`(let ((class
				(defclass ,name ,parent-types ,slot-specifiers
					,(remove ':report class-options :key 'first))))
			,report-definition
			class)))

;;;
;;;	Common Lisp CONDITION base class.
;;;
(defclass condition ()
	((format-control :initarg :format-control :accessor simple-condition-format-control :initform nil)
	 (format-arguments :initarg :format-arguments :accessor simple-condition-format-arguments :initform nil)))

(defmethod print-object ((condition condition) stream)
	(if (simple-condition-format-control condition)
		(apply #'format stream 
				(simple-condition-format-control condition)
				(simple-condition-format-arguments condition))
		(call-next-method)))

(define-condition simple-condition (condition))
(define-condition warning (condition))
(define-condition style-warning (warning))
(define-condition simple-warning (simple-condition warning))
(define-condition serious-condition (condition))
(define-condition storage-condition (serious-condition))
(define-condition error (serious-condition))
(define-condition simple-error (simple-condition error))

(define-condition type-error (error)
	((datum :initarg :datum :accessor type-error-datum)
	 (expected-type :initarg :expected-type :accessor type-error-expected-type))
	(:report
		(lambda (condition stream)
			(format stream "Type error: datum = ~A, expected type = ~A"
				(type-error-datum condition)
				(type-error-expected-type condition)))))

(define-condition simple-type-error (simple-condition type-error))

(define-condition arithmetic-error (error)
	((operation :initarg :operation :accessor arithmetic-error-operation :initform "<unknown>")
	 (operands :initarg :operands :accessor arithmetic-error-operands :initform "<unknown>"))
	(:report 
		(lambda (condition stream)
			(format stream "Arithmetic error: operation = ~A, operands = ~A"
				(arithmetic-error-operation condition)
				(arithmetic-error-operands condition)))))

(define-condition division-by-zero (arithmetic-error)
    ()
    (:report 
		(lambda (condition stream)
			(format stream "Division by zero error: operation = ~A, operands = ~A"
				(arithmetic-error-operation condition)
				(arithmetic-error-operands condition)))))

(define-condition floating-point-inexact (arithmetic-error))
(define-condition floating-point-invalid-operation (arithmetic-error))
(define-condition floating-point-overflow (arithmetic-error)
    ()
    (:report 
		(lambda (condition stream)
			(format stream "Floating point overflow: operation = ~A, operands = ~A"
				(arithmetic-error-operation condition)
				(arithmetic-error-operands condition)))))
    
(define-condition floating-point-underflow (arithmetic-error)
    ()
    (:report 
		(lambda (condition stream)
			(format stream "Floating point underflow: operation = ~A, operands = ~A"
				(arithmetic-error-operation condition)
				(arithmetic-error-operands condition)))))

(define-condition cell-error (error)
	((name :initarg :name :accessor cell-error-name))
	(:report 
		(lambda (condition stream)
			(format stream "Cell error: name = ~A"
				(cell-error-name condition)))))

(define-condition unbound-slot (cell-error)
	()
	(:report 
		(lambda (condition stream)
			(format stream "The slot ~S is unbound"
				(cell-error-name condition)))))

(define-condition unbound-variable (cell-error)
	()
	(:report 
		(lambda (condition stream)
			(format stream "The variable ~S is unbound"
				(cell-error-name condition)))))

(define-condition undefined-function (cell-error)
	()
	(:report 
		(lambda (condition stream)
			(format stream "The function ~S is undefined"
				(cell-error-name condition)))))

(define-condition control-error (error)
	((format-control :initarg :format-control :accessor simple-condition-format-control)
	 (format-arguments :initarg :format-arguments :accessor simple-condition-format-arguments))
	(:report 
		(lambda (condition stream)
			(apply #'format stream 
				(simple-condition-format-control condition)
				(simple-condition-format-arguments condition)))))

(define-condition package-error (error))
(define-condition parse-error (error))
(define-condition print-not-readable (error))
(define-condition program-error (error))

(define-condition stream-error (error))
(define-condition end-of-file (stream-error))
(define-condition reader-error (parse-error stream-error))

(define-condition file-error (error)
	((pathname :initarg :pathname :accessor file-error-pathname))
	(:report 
		(lambda (condition stream)
			(format stream "File error: name = ~A"
				(file-error-pathname condition))
			(when (simple-condition-format-control condition)
				(format stream ". ")
				(call-next-method)))))

;;;
;;; Common Lisp MAKE-CONDITION function.
;;;
(defun make-condition (type &rest slot-initializations)
	(apply #'make-instance type slot-initializations))

;; this will be redefined below
(defun type-specifier-p (specifier) (declare (ignore specifier)) t)

(defun handler-bind-body (bindings forms)
	(let ((binding-forms nil))
		(dolist (x bindings)
            (unless (type-specifier-p (first x))
                (warn "Invalid type specifier in HANDLER-BIND clause: ~A" (first x)))
			(push `(list ',(first x) ,(second x)) binding-forms))		
		`(let ((*handler-registry* 
					(cons ,(cons 'list (nreverse binding-forms)) 
						*handler-registry*)))
			,@forms)))
;;;
;;;	Common Lisp HANDLER-BIND macro.
;;;
(defmacro handler-bind (bindings &rest forms)
	(handler-bind-body bindings forms))

(defun handler-case-body (form handler-forms)
	(let ((no-error-clause (member ':no-error handler-forms :key #'car)))
		(if no-error-clause
			(let* ((handler-forms (remove ':no-error handler-forms :key #'car))
				   (error-return (gensym))
				   (normal-return (gensym)))
				`(block ,error-return
					(multiple-value-call
						#'(lambda ,@(cdar no-error-clause))
						(block ,normal-return
							(return-from ,error-return
								(handler-case (return-from ,normal-return ,form)
									,@handler-forms))))))
			(let ((bind-clauses nil)
				  (target-clauses nil)
				  (block-sym (gensym))
				  (let-temp (gensym)))
				(dolist (x handler-forms)
					(let ((type (first x))
						  (var-form (second x))
						  (body (cddr x))
						  (go-target (gensym)))
						(push `(,type 
								#'(lambda (temp)
									(setq ,let-temp temp)
									(go ,go-target)))
							bind-clauses)
						(push go-target target-clauses)
						(push `(return-from ,block-sym 
									(let (,@(if var-form (list (list (car var-form) let-temp))))
										,@body)) target-clauses)))
				`(block ,block-sym
					(let ((,let-temp nil))
						(tagbody
							(handler-bind ,(nreverse bind-clauses)
								(return-from ,block-sym ,form))
							,@(nreverse target-clauses))))))))

;;; Common Lisp HANDLER-CASE macro
(defmacro handler-case (form &rest handler-forms)
	(handler-case-body form handler-forms))

;;;
;;;	Binds the variable *error-trace* and returns the name of
;;; the calling function. It needs to know the directly calling
;;; function in order to skip it (i.e. ERROR, INVOKE-DEBUGGER)
;;; because this is not the function we are interested in.
;;;
(defun stack-trace-and-calling-function (caller-name trace)
	(let* ((func nil))
		(setq *error-trace* trace)
		;; skip past calls to functions:
		;;    FUNCALL
		;;    caller-name (ERROR, etc.)
		;;	  any function beginning with %
		(do ()
			((null trace))
			(setq func (caar trace))
			(unless (symbolp func)
				(return))
			(if (not 
					(or (eq func caller-name)
						(eq func 'error)
						(eq func 'cerror)
						(eq func 'funcall)
						(eq func 'nil)
						(char= (char (symbol-name func) 0) #\%)))
				(return))
			(setq trace (cdr trace)))
		func))
	
;;;
;;;	Corman Lisp DEBUGGER function.
;;;	Currently, this simply creates a copy of the stack frame,
;;; bound to the variable *error-trace* (and accessible via the
;;; function debug:dump-error-stack), and aborts to the top level.
;;;
(defun debugger () ;; stub, redefined later
	(format *error-output* 
			";;; An error occurred in function ~A:~%;;; ~A~%" 
			*error-function* *debug-condition*)
	(format *error-output* ";;; Aborting to top level.~%")
	(force-output *error-output*)
	(throw 'common-lisp::%error *debug-condition*))

;;;
;;;	Common Lisp INVOKE-DEBUGGER function.
;;;
(defun invoke-debugger (condition)
	(if *debugger-hook*
		(let* ((previous *debugger-hook*)
			   (*debugger-hook* nil))
			(funcall previous condition previous)))
	(let ((*debug-condition* condition)
		  (*error-function* 
				(if *enable-error-trace* 
					(stack-trace-and-calling-function 'invoke-debugger (stack-trace)))))
		(declare (special *debug-condition* *error-function*))
		(debugger)))

;;;
;;;	Redefine this kernel function now, to use condition.
;;;
(defun %unbound-variable (sym)
	(error 'unbound-variable :name sym))	
				
(defun %undefined-function(func-name)
	#'(lambda (&rest x)
		(declare (ignore x))
		(error 'undefined-function :name func-name)))

;;;
;;;	Common Lisp RESTART type.
;;;
(defclass restart () 
	((name :accessor restart-name :initarg :name)
	 (function :accessor restart-function :initarg :function)
	 (test-function :accessor restart-test-function :initarg :test-function)
	 (report-function :accessor restart-report-function :initarg :report-function)
	 (interactive-function :accessor restart-interactive-function :initarg :interactive-function)))

(defun make-restart (name function &key test report interactive)
	(make-instance 'restart 
		:name name 
		:function function 
		:test-function test
		:report-function report 
		:interactive-function interactive))

;;;
;;;	Common Lisp RESTART-BIND macro.
;;;
(defmacro restart-bind (restart-clauses &rest forms)
	(let ((restart-exprs nil))
		(dolist (r restart-clauses)
			(push `(make-restart ',(first r) ,(second r) ,@(cddr r)) restart-exprs))
		`(let ((*restart-registry* 
					(append (list ,@(nreverse restart-exprs)) *restart-registry*)))
			,@forms)))

;;;
;;;	Common Lisp COMPUTE-RESTARTS function.
;;;
(defun compute-restarts (&optional condition)
	(if condition
		(remove-if-not 
			(lambda (restart)
				(let ((test-function (restart-test-function restart))) 
					(or (null test-function)
						(funcall (restart-test-function restart) condition))))
			*restart-registry*)
		*restart-registry*))

;;;
;;;	Common Lisp FIND-RESTART function.
;;;
(defun find-restart (identifier &optional condition)
	(do* ((restarts *restart-registry* (cdr restarts))
		  (restart (car restarts)(car restarts)))
		((null restarts))
		(if (or (eq restart identifier)(eq (restart-name restart) identifier))
			(if condition
				(let ((test-function (restart-test-function restart)))
					(if (or (null test-function)
							(funcall (restart-test-function restart) condition))
						(return restart)))
				(return restart)))))

;;;
;;;	Common Lisp INVOKE-RESTART function.
;;;
(defun invoke-restart (restart &rest arguments)
	(let ((found (find-restart restart)))
		(unless found 
			(error 'control-error 
				:format-control "No active restart of type ~A was found" 
				:format-arguments (list restart)))
		(apply (restart-function found) arguments)))

;;;
;;;	Common Lisp RESTART-CASE macro.
;;; Implementation is based on example expansion in CLTL2 p.906
;;;
(defmacro restart-case (expression &rest clauses)
	(if (null clauses)
		(return-from restart-case `,expression))		;; optimization for no clauses
	(let* ((decls nil)
		   (impls nil)
		   (block-sym (gensym))
		   (temp-sym (gensym)))
		(dolist (clause clauses)
			(let* ((name (first clause))
				   (arglist (second clause))
				   (report nil)
				   (interactive nil)
				   (test nil)
			       (forms nil)
				   (tag-sym (gensym)))
				(do ((x (cddr clause) (cddr x)))
					((null x))
					(cond ((eq (car x) ':report)
						   (setf report 
								(if (stringp (cadr x)) 
									`(function 
										(lambda (stream)
											(write-string ,(cadr x) stream)))
									(cadr x))))
						  ((eq (car x) ':interactive)
						   (setf interactive (if (cadr x) `(function ,(cadr x)))))
						  ((eq (car x) ':test)
						   (setf test (if (cadr x) `(function ,(cadr x)))))
						  (t (setf forms x)(return))))
				;; check for anonymous restart--must include an interactive function
				(if (and (null name) (null interactive))
					(error "An anonymous restart must have an :INTERACTIVE function defined: ~A" clause))
				
				(push `(,name 
						#'(lambda (&rest temp) 
							(setq ,temp-sym temp)
							(go ,tag-sym))
						 :test ,test :interactive ,interactive :report ,report)
					decls)
				(push tag-sym impls)
				(push `(return-from ,block-sym
						(apply #'(lambda ,arglist ,@forms) ,temp-sym))
					impls)))
		`(block ,block-sym
			(let ((,temp-sym nil))
				(tagbody
					(restart-bind ,(nreverse decls)
						(return-from ,block-sym ,expression))
					,@(nreverse impls))))))

;;;
;;;	Common Lisp WITH-SIMPLE-RESTART macro.
;;;
(defmacro with-simple-restart ((restart-name format-control &rest format-arguments)
		&body forms)
	`(restart-case (progn ,@forms)
		(,restart-name ()
			:report (lambda (stream)
				(format stream ,format-control ,@format-arguments))
			(values nil t))))

;;;
;;;	Common Lisp SIGNAL function.
;;;
(defun signal (datum &rest arguments)
	(let ((condition
				(cond ((stringp datum)
					   (funcall 'make-condition 'simple-condition 
						:format-control datum 
						:format-arguments arguments))
					  ((symbolp datum)
					   (apply 'make-condition datum arguments))
					  (t datum)))
		  (handlers *handler-registry*))
		(unless (typep condition 'condition)
			(error "Not a condition: ~A" condition))
		(when (typep condition *break-on-signals*)
			(with-simple-restart (continue-signal "Continue to signal")
				(break "Break: ~A~%~A" condition "BREAK entered because of *BREAK-ON-SIGNALS*.")))
		(do ((x handlers (cdr x)))
			((null x))
			(dolist (y (car x))
				(if (typep condition (first y))
					(let ((*handler-registry* (cdr x)))
						(funcall (second y) condition))))))
	nil)

;;;
;;;	Common Lisp ERROR function.
;;;

(defun error (datum &rest arguments)
	;;(apply 'signal datum arguments)	;; give handlers the first crack at it
	(let ((condition
				(cond ((stringp datum)
					   (funcall 'make-condition 'simple-error 
						:format-control datum 
						:format-arguments arguments))
					  ((symbolp datum)
					   (apply 'make-condition datum arguments))
					  (t datum)))
		  (handlers *handler-registry*))
		(unless (typep condition 'condition)
			(error "Not a condition: ~A" condition))
		(when (typep condition *break-on-signals*)
			(with-simple-restart (continue-signal "Continue to signal")
				(break "Break: ~A~%~A" condition "BREAK entered because of *BREAK-ON-SIGNALS*.")))
		(do ((x handlers (cdr x)))
			((null x))
			(dolist (y (car x))
				(if (typep condition (first y))
					(let ((*handler-registry* (cdr x)))
						(funcall (second y) condition)))))

	;;	(if *ignore-errors*
	;;		(throw 'common-lisp::%error nil))
		
		;; not handled, so invoke debugger
		(invoke-debugger condition)))

(defun create-condition (datum args)
	(let ((condition
			(cond ((null datum)
				   (make-condition 'simple-error 
							:format-control "" :format-arguments '()))
				  ((stringp datum)
				   (make-condition 'simple-error 
					:format-control datum 
					:format-arguments args))
				  ((symbolp datum)
				   (apply 'make-condition datum args))
				  (t datum))))
		condition))

(defun cerror (continue-format-control datum &rest arguments)
	 (restart-case
		(let ((condition (create-condition datum arguments))
			  (handlers *handler-registry*))
			(unless (typep condition 'condition)
				(error "Not a condition: ~A" condition))
			(when (typep condition *break-on-signals*)
				(with-simple-restart (continue-signal "Continue to signal")
					(break "Break: ~A~%~A" condition "BREAK entered because of *BREAK-ON-SIGNALS*.")))
			(do ((x handlers (cdr x)))
				((null x))
				(dolist (y (car x))
					(if (typep condition (first y))
						(let ((*handler-registry* (cdr x)))
							(funcall (second y) condition)))))
	;;		(if *ignore-errors*
	;;			(throw 'common-lisp::%error nil))		
			;; not handled, so invoke debugger
			(invoke-debugger condition))
		(continue (&optional condition) 
			:report (lambda (stream) 
				(apply 'format stream continue-format-control arguments))
			(declare (ignore condition)) 
			(return-from cerror nil)))) 				

;;;
;;; Common Lisp CTYPECASE macro.
;;;
(defmacro ctypecase (keyform &rest clauses)
    (let ((new-symbol (gensym))
          (loop-var (gensym))
          (valid-types '()))
    ;; Rewrote this part, turned DOLIST into a MAPCAR, added error check.
        (setf clauses
            (mapcar
                #'(lambda (clause)
                    (unless (listp clause) (error "Bad CTYPECASE clause: ~S" clause))
                    (let ((type (first clause)) (actions (rest clause)))
                        (push type valid-types)
                        `((typep ,new-symbol ',type)
                            ,@(if (null actions) `((return nil))
                                (progn
                                    (setq actions (copy-list actions))
                                    (let* ((last-cons (last actions))
                                           (last-action (car last-cons)))
                                        (setf (car last-cons) `(return ,last-action)))
                                    actions)))))
                clauses))
        (setf valid-types (nreverse valid-types))
        (setf clauses 
            (append clauses 
                `((t (cerror "Enter a value of a correct type" 'type-error 
                            :datum ,new-symbol :expected-type '(or ,@valid-types))
                     (fresh-line *debug-io*)
                     (format *debug-io* "?~%")
                     (setq ,new-symbol (read *debug-io*))
                     (go ,loop-var)))))
        `(let ((,new-symbol ,keyform))
            (prog ()
                ,loop-var
                (cond ,@clauses)))))

;;;
;;;	Common Lisp INVOKE-RESTART-INTERACTIVELY
;;;
(defun invoke-restart-interactively (restart)
	(let ((found (find-restart restart)))
		(unless found 
			(error 'control-error 
				:format-control "No active restart of type ~A was found" 
				:format-arguments (list restart)))	
		(apply (restart-function found)
            (when (restart-interactive-function found)
                (funcall (restart-interactive-function found))))))
	
(defun assert-body (datum args interactive)
	(restart-bind
		((continue
				(lambda (&optional condition)
					(declare (ignore condition))
					(return-from assert-body nil))
			:report 
			(lambda (stream) 
				(format stream "~A" 
					"You will be prompted for one or more new values."))
			:interactive
			interactive))
		(let ((condition (create-condition datum args)))
			(invoke-debugger condition))))

(defmacro simple-loop (&rest forms)
	(let ((sym (gensym)))
		`(tagbody ,sym (progn ,@forms) (go ,sym))))

;;;
;;; Common Lisp ASSERT macro.
;;;
(defmacro assert (test-form &optional places datum &rest args)
	;;(print (list 'places places 'datum datum))
	(let ((loop-tag (gensym)))
		(if (null places)
            `(block assert 
                (restart-bind
                    ((continue
                        (lambda (&optional condition)
                            (declare (ignore condition))
                            (return-from assert nil))))
                    (unless ,test-form 
                        (let ((condition (create-condition ,(or datum ''error) (list ,@args))))
			                 (error condition)))))
            
            (let ((forms nil)
				  (sym (gensym)))
				(dolist (x places)
					(push `(format *query-io* "Value for ~A: " ',x) forms)
					(push `(let ((,sym (eval (read *query-io*))))
							(setf ,x ,sym))
						forms))
				`(tagbody ,loop-tag
					(unless ,test-form
						(common-lisp::assert-body ,datum (list ,@args)
							(lambda () ,@(nreverse forms)))
						(go ,loop-tag)))))))

;;;
;;;	Common Lisp ABORT function.
;;;
(defun abort (&optional condition) 
	(declare (ignore condition))
	(invoke-restart 'abort))

;;;
;;;	Common Lisp CONTINUE function.
;;;
(defun continue (&optional condition)
	(declare (ignore condition))
	(let ((r (find-restart 'continue))) 
		(if r (invoke-restart r))))

;;;
;;;	Common Lisp MUFFLE-WARNING function.
;;;
(defun muffle-warning (&optional condition) 
	(declare (ignore condition))
	(invoke-restart 'muffle-warning))

;;;
;;;	Common Lisp STORE-VALUE function.
;;;
(defun store-value (value &optional condition)
	(declare (ignore condition))
	(let ((r (find-restart 'store-value))) 
		(if r (invoke-restart r value))))

;;;
;;;	Common Lisp USE-VALUE function.
;;;
(defun use-value (value &optional condition)
	(declare (ignore condition))
	(let ((r (find-restart 'use-value))) 
		(if r (invoke-restart r value))))

(defun conditionp (obj) (typep obj 'condition))


;;;
;;;	Common Lisp BREAK function.
;;;
(defun break (&optional (format-control "Break") &rest format-arguments)
   (with-simple-restart (continue "Return from BREAK.")
		(let ((*debugger-hook* nil))
			(let ((condition 
						(make-condition 'simple-condition
							:format-control format-control
							:format-arguments format-arguments)))
				(format *debug-io* ";;; User break: ~A~%" condition)
				(invoke-debugger condition))))
	nil)

;;;;
;;;;	Common Lisp WARN function.
;;;;
(defun warn (datum &rest args)
	(if (conditionp datum)
		(if (or (not (typep datum 'warning)) args)
			(error 'type-error :datum datum :expected-type 'warning)))
	(let ((condition
			(cond ((null datum)
				   (make-condition 'simple-warning 
							:format-control "" :format-arguments '()))
				  ((stringp datum)
				   (make-condition 'simple-warning 
					:format-control datum 
					:format-arguments args))
				  ((symbolp datum)
				   (apply 'make-condition datum args))
				  (t datum))))
		(with-simple-restart (muffle-warning "Muffle-warning")
			(signal condition)
			(format *error-output* ";;; Warning: ~A~%" condition)
			(force-output *error-output*))))

;;;;
;;;;	Common Lisp IGNORE-ERRORS macro.
;;;;
(defmacro ignore-errors (&rest forms)
	`(handler-case (progn ,@forms)   
		(error (condition) (values nil condition))))

;; redefine this here with new ignore-errors macro call
(defun funcall-ignoring-errors (func &rest args)
	(ignore-errors (apply func args)))

;; trick to see is something is a valid type specifier--if it isn't
;; then the IGNORE-ERRORS form will return a second value
(defun type-specifier-p (specifier)
    (if (cadr (multiple-value-list (ignore-errors (typep 10 specifier)))) nil t))

;;;
;;; CHECK-TYPE-ERROR
;;; Used by CHECK-TYPE macro.
;;;
(define-condition check-type-error (type-error)
	((original-form :initarg :original-form :accessor type-error-original-form)
	 (type-name :initarg :type-name :accessor type-error-type-name))
	(:report
		(lambda (condition stream)
			(format stream "Type error: The value of ~A, ~A is not ~A."
				(type-error-original-form condition)
				(type-error-datum condition)
				(or (type-error-type-name condition)
					(format nil "of type ~A" (type-error-expected-type condition)))))))

(defun read-new-value ()
   (format *debug-io* "Enter a new value: ~%")
   (eval (read *debug-io*)))

;;; Used by CHECK-TYPE expansion.
;;;
(defun handle-invalid-type (obj typespec orig-form type-name setter)
	(restart-case
		(error (make-condition 'check-type-error 
				:datum obj 
				:expected-type typespec 
				:original-form orig-form 
				:type-name type-name))
       (store-value (new-value)
         :report "Enter a value of the correct type."
         :interactive read-new-value  
         (funcall setter new-value))))

;;;;
;;;;	Common Lisp CHECK-TYPE macro.
;;;;	Redefined here to use condition system.
;;;;
(defmacro check-type (place typespec &optional string)
	(let ((obj-sym (gensym))
		  (new-value-sym (gensym))
		  (expanded-type (typeexpand-all typespec)))
		`(do ((,obj-sym ,place ,place))
			((_typep ,obj-sym ',expanded-type))
			(handle-invalid-type ,obj-sym ',expanded-type ',place ,string
				(lambda (,new-value-sym) (setf ,place ,new-value-sym))))))

;;;
;;; Internal function SIGNAL-PROGRAM-ERROR. 
;;; Redefined here to use conditions.
;;;
(defun signal-program-error (format &rest args)
    (error 'program-error 
        :format-control format
		:format-arguments args))

;;;
;;; Override the kernel function %WRONG-NUMBER-OF-ARGS
;;;
(let ()
    (declare (optimize (speed 3)(safety 0)))
    (defun cl::%wrong-number-of-args ()
        (signal-program-error "Wrong number of arguments")))

;;;
;;; Internal function SIGNAL-UNDEFINED-FUNCTION. 
;;; Redefined here to use conditions.
;;;
(defun signal-undefined-function (func-name)
    (error 'undefined-function :name func-name))

;;;
;;; Internal function SIGNAL-TYPE-ERROR.
;;; Redefined here to use conditions.
;;;
(defun signal-type-error (object expected-type)
    (error 'type-error :datum object :expected-type expected-type))

;;;
;;; Redefine this kernel function used by the compiler type checking.
;;; It only gets called by generated code, so it doesn't to check the
;;; number of arguments.
;;;
(defun %check-list (object)
    (declare (optimize (safety 0)(speed 3)))    ;; disable number of argument check
    (if (listp object)
        object
        (signal-type-error object 'list)))

#|
(x86::defcodegen %check-list (form dest) ;; (object)
    (cl::compile-sub-form (second form) :dest-eax-operand t)
    (x86::parse-assembler 
        { 
    		mov		edx, eax
    		and		edx, 7
    		cmp		edx, cons-tag
    		je		short :done
    		cmp		eax, [esi]
     		je		short :done
            push    eax
            push    'cl::list
            mov     ecx, 2
            callf   cl::signal-type-error
            add     esp, 8     
    	:done
        })
    (if (eq dest :dest-stack)
    	(progn
    		(x86::parse-assembler
    			{
    				push	eax
    			})
    		(x86::offset-stack -4)))
    (if (eq dest :dest-eax)
    	(progn
    		(x86::parse-assembler
    			{
    				mov		ecx, 1
    			})))
    t)
|#

;;;
;;; Redefine this kernel function used by the compiler type checking
;;;
(defun cl::%invalid-fixnum (num)
    (signal-type-error num 'fixnum))

;;;
;;; Internal function SIGNAL-ARITHMETIC-ERROR
;;;
(defun signal-arithmetic-error (operation operands)
    (error 'arithmetic-error 
        :operation operation 
        :operands operands))

;;;
;;; Internal function SIGNAL-DIVISION-BY-ZERO-ERROR
;;;
(defun signal-division-by-zero (operands)
    (error 'division-by-zero 
        :operation '/ 
        :operands operands))

;;;
;;; Redefine IN-PACKAGE to use conditions
;;;
(defmacro in-package (name)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (let ((package (find-package ',name)))
       (cond (package
               (setq *package* package))
             (t
               (cerror "Create a new package named ~S."
                       (make-condition 'package-error
                                       :package ',name
                                       :format-control "Package ~S not found."
                                       :format-arguments (list ',name))
                       ',name)
               (setq *package* (make-package ',name)))))))

;;;
;;; Redefine this function (used by the compiler) to defer
;;; constant folding until run time when it generates errors.
;;;
(defun macroexpand-all (x &optional env)
	(if (and (consp x)(eq (car x) 'quote))
		(return-from macroexpand-all x))
		
	;; keep doing compiler macros, macros and inline expansion
	;; until we go one time through the loop and nothing changes
	(do ((save x x))
		(nil)
		(setq x (expand-compiler-macros x))
		(setq x (macroexpand x env))   ;; expand top level form	
		(setq x (inline-expand x env))
		(if (eq save x)
			(return)))
	(if (compiler-fold-constants)
		(if (constantp x)
            ;; handle case where folding constants generates an error--and just defer it until runtime
			(let* ((result-list (multiple-value-list (ignore-errors (eval-constant-expression x))))
                   (result (car result-list)))
                (unless (and (null result) (cdr result-list) (typep (cadr result-list) 'condition))
    				(if (or (consp result) 
    						(and (symbolp result) 
    							(not (keywordp result))
    							(not (eq result t))
    							(not (eq result nil))))
    					(setf result (list 'quote result)))
    				(return-from macroexpand-all result)))))
	(macroexpand-all-except-top x env))

