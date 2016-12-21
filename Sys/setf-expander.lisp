;;; -*- Log: code.log; Mode: Lisp; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;(ext:file-comment
;;;  "$Header: /home/CVS-cmucl/src/code/defmacro.lisp,v 1.29 2003/06/01 19:35:05 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; Written by Blaine Burks.
;;;
;;; Adapted for Corman Lisp by Roger Corman  July 10, 2003
;;;

(in-package :sys)
;;;; Some variable definitions.

;;; Variables for amassing the results of parsing a defmacro.  Declarations
;;; in DEFMACRO are the reason this isn't as easy as it sounds.
;;;
(defvar *arg-tests* ()
  "A list of tests that do argument counting at expansion time.")

(defvar *system-lets* ()
  "Let bindings that are done to make lambda-list parsing possible.")

(defvar *user-lets* ()
  "Let bindings that the user has explicitly supplied.")

(defvar *default-default* nil
  "Unsupplied optional and keyword arguments get this value defaultly.")

;; Temps that we introduce and might not reference.
(defvar *ignorable-vars*)

;;; RGC -- added this helper function for mod to allow better handling of
;;; explicitly named keywords  
(defun is-quoted-key (sym) (and (consp sym) (cdr sym) (eq (car sym) 'quote)))

;;; helper functions for SETF expansion functions
(defun keyword-supplied-p (keyword key-list)
  (do ((remaining key-list (cddr remaining)))
      ((endp remaining))
    (when (or
                (eq keyword (car remaining))
                (and (is-quoted-key (car remaining))
                    (eq keyword (cadr (car remaining)))))
      (return t))))

(defun lookup-keyword (keyword key-list)
  (do ((remaining key-list (cddr remaining)))
      ((endp remaining))
    (when (or
                (eq keyword (car remaining))
                (and (is-quoted-key (car remaining))
                    (eq keyword (cadr (car remaining)))))
      (return (cadr remaining)))))
;;; VERIFY-KEYWORDS -- internal
;;;
;;; Determine if key-list is a valid list of keyword/value pairs.  Do not
;;; signal the error directly, 'cause we don't know how it should be signaled.
;;; 

(defun verify-keywords (key-list valid-keys allow-other-keys)
  (do ((already-processed nil)
       (unknown-keyword nil)
       (remaining key-list (cddr remaining))
       key)
      ((null remaining)
       (if (and unknown-keyword
		(not allow-other-keys)
		(not (lookup-keyword :allow-other-keys key-list)))
	   (values :unknown-keyword (list unknown-keyword valid-keys))
	   (values nil nil)))
      (setf key (car remaining))
        
      ;; kludge here--if the passed key is (QUOTE X) replace with X.
      ;; This handles cases where the key name is being quoted.
      (if (and (consp key) (eq (car key) 'quote))
          (setf key (cadr key)))
    (cond ((not (and (consp remaining) (listp (cdr remaining))))
	   (return (values :dotted-list key-list)))
	  ((null (cdr remaining))
	   (return (values :odd-length key-list)))
	  ((or (eq key :allow-other-keys)
	       (member key valid-keys))
	   (push key already-processed))
	  (t (setf unknown-keyword key)))))

;;;; Stuff to parse DEFMACRO, MACROLET, DEFINE-SETF-METHOD, and DEFTYPE.

;;; PARSE-DEFMACRO returns, as multiple-values, a body, possibly a declare
;;; form to put where this code is inserted, and the documentation for the
;;; parsed body.
;;;
(defun parse-defmacro (lambda-list arg-list-name code name error-kind
				   &key (annonymousp nil)
				   (doc-string-allowed t)
				   ((:environment env-arg-name))
				   (error-fun 'error))
  "Returns as multiple-values a parsed body, any local-declarations that
   should be made where this body is inserted, and a doc-string if there is
   one."
  (multiple-value-bind (body declarations documentation)
		       (parse-body code nil doc-string-allowed)
    (let* ((*arg-tests* ())
	   (*user-lets* ())
	   (*system-lets* ())
	   (*ignorable-vars* ()))
      (multiple-value-bind
	  (env-arg-used minimum maximum)
	  (parse-defmacro-lambda-list lambda-list arg-list-name name
				      error-kind error-fun (not annonymousp)
				      nil env-arg-name)
	(values
	 `(let* ,(nreverse *system-lets*)
	   ,@(when *ignorable-vars*
	       `((declare (ignorable ,@*ignorable-vars*))))
	    ,@*arg-tests*
	    (let* ,(nreverse *user-lets*)
	      ,@declarations
	      ,@body))
	 `(,@(when (and env-arg-name (not env-arg-used))
	       `((declare (ignore ,env-arg-name)))))
	 documentation
	 minimum
	 maximum)))))


(defun parse-defmacro-lambda-list
       (lambda-list arg-list-name name error-kind error-fun
		    &optional top-level env-illegal env-arg-name)
  (let ((path (if top-level `(cdr ,arg-list-name) arg-list-name))
	(now-processing :required)
	(maximum 0)
	(minimum 0)
	(keys ())
	(key-seen nil)
	rest-name restp allow-other-keys-p env-arg-used)
    ;; This really strange way to test for '&whole is neccessary because member
    ;; does not have to work on dotted lists, and dotted lists are legal
    ;; in lambda-lists.
    (when (and (do ((list lambda-list (cdr list)))
		   ((atom list) nil)
		 (when (eq (car list) '&whole) (return t)))
	       (not (eq (car lambda-list) '&whole)))
      (cl::signal-program-error "&Whole must appear first in ~S lambda-list."
                            error-kind))
    (do ((rest-of-args lambda-list (cdr rest-of-args)))
	((atom rest-of-args)
	 (cond ((null rest-of-args) nil)
	       ;; Varlist is dotted, treat as &rest arg and exit.
	       (t (push-let-binding rest-of-args path nil)
		  (setf restp :dotted))))
      (let ((var (car rest-of-args)))
	(cond ((eq var '&whole)
	       (cond ((and (cdr rest-of-args) (symbolp (cadr rest-of-args)))
		      (setf rest-of-args (cdr rest-of-args))
		      ;; For compiler macros, we have to do something
		      ;; special in case the form has a car eq to
		      ;; funcall, as specified in the CLHS.  In this
		      ;; case, we skip over the funcall and pretend
		      ;; that the rest of the form is the actual form.
		      ;;
		      ;; This is a gross hack because we look at
		      ;; error-kind to figure out if we're defining a
		      ;; compiler macro or not.
		      (when (eq error-kind 'define-compiler-macro)
			(push-let-binding arg-list-name arg-list-name
			  t
			  `(progn
			    (not (and (listp ,arg-list-name)
				  (eq 'funcall (car ,arg-list-name)))))
			  `(progn
			    (setf ,arg-list-name (cdr ,arg-list-name)))))
		      (push-let-binding (car rest-of-args) arg-list-name nil))
		     ((and (cdr rest-of-args) (consp (cadr rest-of-args)))
		      (pop rest-of-args)
		      (let* ((destructuring-lambda-list (car rest-of-args))
			     (sub (gensym "WHOLE-SUBLIST")))
			(push-sub-list-binding
			 sub arg-list-name destructuring-lambda-list
			 name error-kind error-fun)
			(parse-defmacro-lambda-list
			 destructuring-lambda-list sub name error-kind error-fun)))
		     (t
		      (defmacro-error "&WHOLE" error-kind name))))
	      ((eq var '&environment)
	       (cond (env-illegal
		      (cl::signal-program-error "&environment not valid with ~S."
                                            error-kind))
		     ((not top-level)
		      (cl::signal-program-error
		       "&environment only valid at top level of lambda-list.")))
	       (cond ((and (cdr rest-of-args) (symbolp (cadr rest-of-args)))
		      (setf rest-of-args (cdr rest-of-args))
		      (append-let-binding (car rest-of-args) env-arg-name nil)
		      (setf env-arg-used t))
		     (t
		      (defmacro-error "&ENVIRONMENT" error-kind name))))
	      ;;
	      ;; This branch implements an extension to Common Lisp
	      ;; that was formerly implemented for &body.  In place of
	      ;; a symbol following &body, there could be a list of up
	      ;; to three elements which will be bound to the body,
	      ;; declarations, and doc-string of the body.
	      ((eq var '&parse-body)
	       (unless (and (cdr rest-of-args)
			    (consp (cadr rest-of-args))
			    (symbolp (caadr rest-of-args)))
		 (cl::signal-program-error "Invalid ~a" '&parse-body))
		(setf rest-of-args (cdr rest-of-args))
		(setf restp t)
		(let ((body-name (caar rest-of-args))
		      (declarations-name (cadar rest-of-args))
		      (doc-string-name (caddar rest-of-args))
		      (parse-body-values (gensym)))
		  (push-let-binding
		   parse-body-values
		   `(multiple-value-list
		     (parse-body ,path ,env-arg-name
				 ,(not (null doc-string-name))))
		   t)
		  (setf env-arg-used t)
		  (when body-name
		    (push-let-binding body-name
				      `(car ,parse-body-values) nil))
		  (when declarations-name
		    (push-let-binding declarations-name
				      `(cadr ,parse-body-values) nil))
		  (when doc-string-name
		    (push-let-binding doc-string-name
				      `(caddr ,parse-body-values) nil))))
	      ;;
	      ((member var '(&rest &body))
	       (cond ((and (cddr rest-of-args)
			   (not (member (caddr rest-of-args) lambda-list-keywords)))
		      (defmacro-error (symbol-name var) error-kind name))
		     ((and (cdr rest-of-args) (symbolp (cadr rest-of-args)))
		      (setf rest-of-args (cdr rest-of-args))
		      (setf restp t)
		      (push-let-binding (car rest-of-args) path nil))
		     ((and (cdr rest-of-args) (consp (cadr rest-of-args)))
		      (pop rest-of-args)
		      (setq restp t)
		      (let* ((destructuring-lambda-list (car rest-of-args))
			     (sub (gensym "REST-SUBLIST")))
			(push-sub-list-binding sub path destructuring-lambda-list
			 name error-kind error-fun)
			(parse-defmacro-lambda-list
			 destructuring-lambda-list sub name error-kind error-fun)))
		     (t
		      (defmacro-error (symbol-name var) error-kind name))))
	      ((eq var '&optional)
	       (setf now-processing :optionals))
	      ((eq var '&key)
	       (setf now-processing :keywords)
	       (setf rest-name (gensym "KEYWORDS-"))
	       (push rest-name *ignorable-vars*)
	       (setf restp t)
	       (setq key-seen t)
	       (push-let-binding rest-name path t))
	      ((eq var '&allow-other-keys)
	       (setf allow-other-keys-p t))
	      ((eq var '&aux)
	       (setf now-processing :auxs))
	      ((listp var)
	       (case now-processing
		 (:required
		  (let ((sub-list-name (gensym "SUBLIST-")))
		    (push-sub-list-binding sub-list-name `(car ,path) var
					   name error-kind error-fun)
		    (parse-defmacro-lambda-list var sub-list-name name
						error-kind error-fun))
		  (setf path `(cdr ,path))
		  (incf minimum)
		  (incf maximum))
		 (:optionals
		  (when (> (length var) 3)
		    (cerror "Ignore extra noise."
			    "More than variable, initform, and suppliedp ~
			    in &optional binding - ~S"
			    var))
		  (push-optional-binding (car var) (cadr var) (caddr var)
					 `(not (null ,path)) `(car ,path)
					 name error-kind error-fun)
		  (setf path `(cdr ,path))
		  (incf maximum))
		 (:keywords
		  (let* ((keyword-given (consp (car var)))
			 (variable (if keyword-given
				       (cadar var)
				       (car var)))
			 (keyword (if keyword-given
				      (caar var)
				      (make-keyword variable)))
			 (supplied-p (caddr var)))
		    (push-optional-binding variable (cadr var) supplied-p
					   `(keyword-supplied-p ',keyword
								,rest-name)
					   `(lookup-keyword ',keyword
							    ,rest-name)
					   name error-kind error-fun)
		    (push keyword keys)))
		 (:auxs (push-let-binding (car var) (cadr var) nil))))
	      ((symbolp var)
	       (case now-processing
		 (:required
		  (incf minimum)
		  (incf maximum)
		  (push-let-binding var `(car ,path) nil)
		  (setf path `(cdr ,path)))
		 (:optionals
		  (incf maximum)
		  (push-let-binding var `(car ,path) nil `(not (null ,path)))
		  (setf path `(cdr ,path)))
		 (:keywords
		  (let ((key (make-keyword var)))
		    (push-let-binding var `(lookup-keyword ,key ,rest-name)
				      nil)
		    (push key keys)))
		 (:auxs
		  (push-let-binding var nil nil))))
	      (t
	       (cl::signal-program-error "Non-symbol in lambda-list - ~S." var)))))
    ;; Generate code to check the number of arguments, unless dotted
    ;; in which case length will not work.
    (unless (eq restp :dotted)
       (push `(unless (<= ,minimum
			  (length (the list ,(if top-level
						 `(cdr ,arg-list-name)
					       arg-list-name)))
			  ,@(unless restp
				    (list maximum)))
		      ,(let ((arg (if top-level
				      `(cdr ,arg-list-name)
				    arg-list-name)))
			 (if (eq error-fun 'error)
			     `(do-arg-count-error ',error-kind ',name ,arg
						  ',lambda-list ,minimum
						  ,(unless restp maximum))
			   `(,error-fun 'defmacro-ll-arg-count-error
				 :kind ',error-kind
				 ,@(when name `(:name ',name))
				 :argument ,arg
				 :lambda-list ',lambda-list
				 :minimum ,minimum
				 ,@(unless restp `(:maximum ,maximum))))))
	     *arg-tests*))
    (when key-seen
      (let ((problem (gensym "KEY-PROBLEM-"))
	    (info (gensym "INFO-")))
	(push `(multiple-value-bind
		     (,problem ,info)
		   (verify-keywords ,rest-name ',keys ',allow-other-keys-p)
		 (when ,problem
		   (,error-fun
		    'defmacro-ll-broken-key-list-error
		    :kind ',error-kind
		    ,@(when name `(:name ',name))
		    :problem ,problem
		    :info ,info)))
	      *arg-tests*)))
    (values env-arg-used minimum (if (null restp) maximum nil))))

;;; We save space in macro definitions by calling this function.
;;;
(defun do-arg-count-error (error-kind name arg lambda-list minimum maximum)
  (let ((fname #|(debug::find-caller-name)|# "Unknown"))
    (error 'defmacro-ll-arg-count-error
	   :kind error-kind
	   :function-name fname
	   :name name
	   :argument arg
	   :lambda-list lambda-list
	   :minimum minimum :maximum maximum)))

(defun push-sub-list-binding (variable path object name error-kind error-fun)
  (let ((var (gensym "TEMP-")))
    (push `(,variable
	    (let ((,var ,path))
	      (if (listp ,var)
		  ,var
		  (,error-fun 'defmacro-bogus-sublist-error
			      :kind ',error-kind
			      ,@(when name `(:name ',name))
			      :object ,var
			      :lambda-list ',object))))
	  *system-lets*)))

(defun push-let-binding (variable path systemp &optional condition
			 (init-form *default-default*))
  (let ((let-form (if condition
		      `(,variable (if ,condition ,path ,init-form))
		      `(,variable ,path))))
    (if systemp
	(push let-form *system-lets*)
	(push let-form *user-lets*))))

(defun append-let-binding (variable path systemp &optional condition
			 (init-form *default-default*))
  (let ((let-form (if condition
		      `(,variable (if ,condition ,path ,init-form))
		      `(,variable ,path))))
    (if systemp
	(setq *system-lets* (nconc *system-lets* (list let-form)))
	(setq *user-lets* (nconc *user-lets* (list let-form))))))

(defun push-optional-binding (value-var init-form supplied-var condition path
					name error-kind error-fun)
  (unless supplied-var
    (setf supplied-var (gensym "SUPLIEDP-")))
  (push-let-binding supplied-var condition t)
  (cond ((consp value-var)
	 (let ((whole-thing (gensym "OPTIONAL-SUBLIST-")))
	   (push-sub-list-binding whole-thing
				  `(if ,supplied-var ,path ,init-form)
				  value-var name error-kind error-fun)
	   (parse-defmacro-lambda-list value-var whole-thing name
				       error-kind error-fun)))
	((symbolp value-var)
	 (push-let-binding value-var path nil supplied-var init-form))
	(t
	 (cl::signal-program-error "Illegal optional variable name: ~S"
	                       value-var))))

(defun make-keyword (symbol)
  "Takes a non-keyword symbol, symbol, and returns the corresponding keyword."
  (intern (symbol-name symbol) cl::KEYWORD-PACKAGE))

(defun defmacro-error (problem kind name)
  (cl::signal-program-error "Illegal or ill-formed ~A argument in ~A~@[ ~S~]."
                        problem kind name))
;;;; Conditions signaled at runtime by the resultant body.

(define-condition defmacro-lambda-list-bind-error (error)
  ((kind :reader defmacro-lambda-list-bind-error-kind
	 :initarg :kind)
   (name :reader defmacro-lambda-list-bind-error-name
	 :initarg :name
	 :initform nil)))

(defun print-defmacro-ll-bind-error-intro (condition stream)
  (if (null (defmacro-lambda-list-bind-error-name condition))
      (format stream
	      "Error while parsing arguments to ~A in ~S:~%"
	      (defmacro-lambda-list-bind-error-kind condition)
	      #|(condition-function-name condition) |# "Unknown")
      (format stream
	      "Error while parsing arguments to ~A ~S:~%"
	      (defmacro-lambda-list-bind-error-kind condition)
	      (defmacro-lambda-list-bind-error-name condition))))

(define-condition defmacro-bogus-sublist-error
		  (defmacro-lambda-list-bind-error)
  ((object :reader defmacro-bogus-sublist-error-object :initarg :object)
   (lambda-list :reader defmacro-bogus-sublist-error-lambda-list
		:initarg :lambda-list))
  (:report
   (lambda (condition stream)
     (print-defmacro-ll-bind-error-intro condition stream)
     (format stream
	     "Bogus sublist:~%  ~S~%to satisfy lambda-list:~%  ~:S~%"
	     (defmacro-bogus-sublist-error-object condition)
	     (defmacro-bogus-sublist-error-lambda-list condition)))))

(define-condition defmacro-ll-arg-count-error (defmacro-lambda-list-bind-error)
  ((argument :reader defmacro-ll-arg-count-error-argument :initarg :argument)
   (lambda-list :reader defmacro-ll-arg-count-error-lambda-list
		:initarg :lambda-list)
   (minimum :reader defmacro-ll-arg-count-error-minimum :initarg :minimum)
   (maximum :reader defmacro-ll-arg-count-error-maximum :initarg :maximum))
  (:report
   (lambda (condition stream)
     (print-defmacro-ll-bind-error-intro condition stream)
     (format stream
	     "Invalid number of elements in:~%  ~:S~%~
	     to satisfy lambda-list:~%  ~:S~%"
	     (defmacro-ll-arg-count-error-argument condition)
	     (defmacro-ll-arg-count-error-lambda-list condition))
     (cond ((null (defmacro-ll-arg-count-error-maximum condition))
	    (format stream "Expected at least ~D"
		    (defmacro-ll-arg-count-error-minimum condition)))
	   ((= (defmacro-ll-arg-count-error-minimum condition)
	       (defmacro-ll-arg-count-error-maximum condition))
	    (format stream "Expected exactly ~D"
		    (defmacro-ll-arg-count-error-minimum condition)))
	   (t
	    (format stream "Expected between ~D and ~D"
		    (defmacro-ll-arg-count-error-minimum condition)
		    (defmacro-ll-arg-count-error-maximum condition))))
     (format stream ", but got ~D."
	     (length (defmacro-ll-arg-count-error-argument condition))))))


(define-condition defmacro-ll-broken-key-list-error
		  (defmacro-lambda-list-bind-error)
  ((problem :reader defmacro-ll-broken-key-list-error-problem
	    :initarg :problem)
   (info :reader defmacro-ll-broken-key-list-error-info :initarg :info))
  (:report (lambda (condition stream)
	     (print-defmacro-ll-bind-error-intro condition stream)
	     (format stream
		     (ecase
			 (defmacro-ll-broken-key-list-error-problem condition)
		       (:dotted-list
			"Keyword/value list is dotted: ~S")
		       (:odd-length
			"Odd number of elements in keyword/value list: ~S")
		       (:duplicate
			"Duplicate keyword: ~S")
		       (:unknown-keyword
			"~{Unknown keyword: ~S; expected one of ~{~S~^, ~}~}"))
		     (defmacro-ll-broken-key-list-error-info condition)))))

;;; Parse-Body  --  Public
;;;
;;;    Parse out declarations and doc strings, *not* expanding macros.
;;; Eventually the environment arg should be flushed, since macros can't expand
;;; into declarations anymore.
;;;
(defun parse-body (body environment &optional (doc-string-allowed t))
  "This function is to parse the declarations and doc-string out of the body of
  a defun-like form.  Body is the list of stuff which is to be parsed.
  Environment is ignored.  If Doc-String-Allowed is true, then a doc string
  will be parsed out of the body and returned.  If it is false then a string
  will terminate the search for declarations.  Three values are returned: the
  tail of Body after the declarations and doc strings, a list of declare forms,
  and the doc-string, or NIL if none."
  (declare (ignore environment))
  (let ((decls ())
	(doc nil))
    (do ((tail body (cdr tail)))
	((endp tail)
	 (values tail (nreverse decls) doc))
      (let ((form (car tail)))
	(cond ((and (stringp form) (cdr tail))
	       (if doc-string-allowed
		   (setq doc form
			 ;; Only one doc string is allowed.
			 doc-string-allowed nil)
		   (return (values tail (nreverse decls) doc))))
	      ((not (and (consp form) (symbolp (car form))))
	       (return (values tail (nreverse decls) doc)))
	      ((eq (car form) 'declare)
	       (push form decls))
	      (t
	       (return (values tail (nreverse decls) doc))))))))

;;;
;;; Common Lisp DEFINE-SETF-EXPANDER macro.
;;;
(defmacro define-setf-expander (access-fn lambda-list &body body)
    "Syntax like DEFMACRO, but creates a Setf-Expansion generator.  The body
     must be a form that returns the five magical values."
    (unless (symbolp access-fn)
        (cl::signal-program-error "~S -- Access-function name not a symbol in DEFINE-SETF-EXPANDER."
            access-fn))
    (let ((whole (gensym "WHOLE-"))
          (environment (gensym "ENV-"))
          (name (make-symbol (concatenate 'string "(SETF " (symbol-name access-fn) ")"))))
        (multiple-value-bind (body local-decs doc)
            (parse-defmacro lambda-list whole body access-fn
                'define-setf-expander
                :environment environment)
            `(eval-when (:compile-toplevel :load-toplevel :execute)
                (remprop ',access-fn 'cl::defstruct-writer)
                (defun ,name (,whole ,environment)
                    (declare (ignore ,environment))
                    ,@local-decs
                    ,body)
                (cl::register-setf-expander-function ',access-fn ',name)
                (setf (documentation ',access-fn 'setf) ',doc)
                ',access-fn))))

;;;; The Collect macro:

;;; Collect-Normal-Expander  --  Internal
;;;
;;;    This function does the real work of macroexpansion for normal collection
;;; macros.  N-Value is the name of the variable which holds the current
;;; value.  Fun is the function which does collection.  Forms is the list of
;;; forms whose values we are supposed to collect.
;;;
(defun collect-normal-expander (n-value fun forms)
  `(progn
    ,@(mapcar #'(lambda (form) `(setq ,n-value (,fun ,form ,n-value))) forms)
    ,n-value))

;;; Collect-List-Expander  --  Internal
;;;
;;;    This function deals with the list collection case.  N-Tail is the pointer
;;; to the current tail of the list, which is NIL if the list is empty.
;;;
(defun collect-list-expander (n-value n-tail forms)
  (let ((n-res (gensym)))
    `(progn
      ,@(mapcar #'(lambda (form)
		    `(let ((,n-res (cons ,form nil)))
		       (cond (,n-tail
			      (setf (cdr ,n-tail) ,n-res)
			      (setq ,n-tail ,n-res))
			     (t
			      (setq ,n-tail ,n-res  ,n-value ,n-res)))))
		forms)
      ,n-value)))


;;; Collect  --  Public
;;;
;;;    The ultimate collection macro...
;;;
(defmacro collect (collections &body body)
  "Collect ({(Name [Initial-Value] [Function])}*) {Form}*
  Collect some values somehow.  Each of the collections specifies a bunch of
  things which collected during the evaluation of the body of the form.  The
  name of the collection is used to define a local macro, a la MACROLET.
  Within the body, this macro will evaluate each of its arguments and collect
  the result, returning the current value after the collection is done.  The
  body is evaluated as a PROGN; to get the final values when you are done, just
  call the collection macro with no arguments.

  Initial-Value is the value that the collection starts out with, which
  defaults to NIL.  Function is the function which does the collection.  It is
  a function which will accept two arguments: the value to be collected and the
  current collection.  The result of the function is made the new value for the
  collection.  As a totally magical special-case, the Function may be Collect,
  which tells us to build a list in forward order; this is the default.  If an
  Initial-Value is supplied for Collect, the stuff will be rplacd'd onto the
  end.  Note that Function may be anything that can appear in the functional
  position, including macros and lambdas."

  (let ((macros ())
	(binds ()))
    (dolist (spec collections)
      (unless (<= 1 (length spec) 3)
	(error "Malformed collection specifier: ~S." spec))
      (let ((n-value (gensym))
	    (name (first spec))
	    (default (second spec))
	    (kind (or (third spec) 'collect)))
	(push `(,n-value ,default) binds)
	(if (eq kind 'collect)
	    (let ((n-tail (gensym)))
	      (if default
		  (push `(,n-tail (last ,n-value)) binds)
		  (push n-tail binds))
	      (push `(,name (&rest args)
			    (collect-list-expander ',n-value ',n-tail args))
		    macros))
	    (push `(,name (&rest args)
			  (collect-normal-expander ',n-value ',kind args))
		  macros))))
    `(macrolet ,macros (let* ,(nreverse binds) ,@body))))

(defun expand-short-defsetf-form (func args)
    `(eval-when (:load-toplevel :compile-toplevel :execute)
        (let ()
            (cl::remove-struct-print ',func)
            (cl::register-setf-function ',func ',(car args) t)
            (setf (documentation ',func 'setf)
                ,(if (and (>= (length args) 2) (stringp (second args)))
                    (second args)
                    nil))
            ',func)))

(defun expand-long-defsetf-form (func set-func-name set-func-def doc)
    `(eval-when (:load-toplevel :compile-toplevel :execute)
        (let ()
            (cl::remove-struct-print ',func)
            ,set-func-def                            
            (cl::register-setf-function ',func ',set-func-name (list t))
            (setf (documentation ',func 'setf) ,doc)
            ',func)))

(defun %defsetf (orig-access-form num-store-vars expander)
    (collect ((subforms) (subform-vars) (subform-exprs) (store-vars))
        (dolist (subform (cdr orig-access-form))
            (if (constantp subform)
                (subforms subform)
                (let ((var (gensym)))
                    (subforms var)
                    (subform-vars var)
                    (subform-exprs subform))))
        (dotimes (i num-store-vars)
            (store-vars (gensym)))
        (values (subform-vars)
            (subform-exprs)
            (store-vars)
            (funcall expander (cons (subforms) (store-vars)))
            `(,(car orig-access-form) ,@(subforms)))))

(defmacro defsetf (access-fn &rest rest)
  "Associates a SETF update function or macro with the specified access
   function or macro.  The format is complex.  See the manual for
   details."
    (if (not (listp (car rest)))
        ;; short form
        (expand-short-defsetf-form access-fn rest)

        (if (and (cdr rest) (listp (cadr rest)))
            ;; long form
            (destructuring-bind
                (lambda-list (&rest store-variables) &body body)
                rest
                (let ((arglist-var (gensym "ARGS-"))
                      (access-form-var (gensym "ACCESS-FORM-"))
                      (env-var (gensym "ENVIRONMENT-"))
                      (name (make-symbol (concatenate 'string "(SETF " (symbol-name access-fn) ")"))))
                    (multiple-value-bind
                        (body local-decs doc)
                        (parse-defmacro 
                            `(,lambda-list ,@store-variables)
                            arglist-var body access-fn 'defsetf
                            :annonymousp t)
                        (expand-long-defsetf-form 
                            access-fn
                            name
                            `(defun ,name (,access-form-var ,env-var)
                                (declare (ignore ,env-var))
                                (%defsetf ,access-form-var ,(length store-variables)
                                    #'(lambda (,arglist-var) ,@local-decs (block ,access-fn ,body))))
                            doc))))
            (error "Ill-formed DEFSETF for ~S." access-fn))))

;;;
;;; Common Lisp PUSH macro.
;;;
(defmacro push (obj place &environment env)
  "Takes an object and a location holding a list.  Conses the object onto
  the list, returning the modified list.  OBJ is evaluated before PLACE."

    (let ((expanded-place (macroexpand place env)))
        
        ;; This special case for place being a symbol isn't strictly needed.
        ;; It's so we can do push (and pushnew) with a kernel.core.
        (if (and (symbolp place) (eq place expanded-place))
            `(setq ,place (cons ,obj ,place))
            (multiple-value-bind (dummies vals newval setter getter)
                (get-setf-expansion expanded-place env)
                (cond
                    ((cdr newval)
                        ;; Handle multiple values
                     (let ((g (mapcar #'(lambda (x) (declare (ignore x))(gensym)) (rest obj))))
                            `(multiple-value-bind ,g ,obj
                                (let* (,@(mapcar #'list dummies vals))
                                    (multiple-value-bind ,newval
                                        (values ,@(mapcar #'(lambda (a b) (list 'cons a b))
                                                g (rest getter)))
                                        ,setter)))))
                    (t
                        ;; A single value
                        (let ((g (gensym)))
                            `(let* ((,g ,obj)
                                    ,@(mapcar #'list dummies vals)
                                    (,@newval (cons ,g ,getter)))
                                ,setter))))))))

;;;
;;;  Common Lisp SUBSEQ function
;;;  We need to redefine this here now that the setf expansion functions are fully supported
;;;
(defun subseq (sequence start &optional end)
  (let ((length (length sequence)))
    (unless end (setq end length))
    (unless (<= 0 start end length)
      (error "Invalid START = ~S, END = ~S arguments" start end))
    (if (vectorp sequence)
	(let* ((elements (- end start))
	       (a (make-array elements 
			      :element-type (array-element-type sequence))))
	  (dotimes (i elements)
	    (setf (elt a i) (elt sequence (+ i start))))
	  a)
	(if (listp sequence)
	    (let* ((elements (- end start))
		   (x (nthcdr start sequence))
		   (newlist nil))
	      (dotimes (i elements)
		(push (car x) newlist)
		(setq x (cdr x)))
	      (nreverse newlist))
	    (error "Invalid sequence: ~S" sequence)))))

