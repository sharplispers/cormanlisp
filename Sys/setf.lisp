;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		setf.lisp
;;;;	Contents:	Setf macro related functions.
;;;;	History:	10/22/96  RGC  Created.
;;;;				7/31/01   RGC  Updated to use SETF registry functions.
;;;;                04/03/03  RGC  Integrated JP Massar's changes to DEFUN.

(defun setf-function-symbol (function-specifier)
	(if (consp function-specifier)
		(let ((print-name (format nil "~:@(~A~)" function-specifier)))
			(values (intern print-name
                (symbol-package (cadr function-specifier)))))
		function-specifier))

(defun expand-symbol-macros (sym)
	(do ((ret (macroexpand-1 sym)(macroexpand-1 sym)))
		((or (not (symbolp ret))(eq sym ret)) ret)
		(setf sym ret)))

(defun get-setf-expander-function (name) 
	(declare (ignore name))
	nil)	;; redefined later

(defun expand-with-expander-func (expander-func place value)
    (multiple-value-bind (temps vals stores store-form access-form)
        (funcall expander-func place nil)
        (declare (ignore access-form))
    	(let ((bindings (mapcar #'list temps vals)))
            (if (= (length stores) 1)
                `(let* ,(append bindings (list `(,(first stores) ,value)))
                    ,store-form)
                `(let* ,bindings
                    (multiple-value-bind ,stores ,value ,store-form))))))
    
;;;
;;;		Common Lisp SETF macro.
;;;    To do: handle case where a local function shadows the registered place name.
;;;
(defmacro setf (&rest forms)
	(let ((form-list nil))
		(do* ((f forms (cddr f))
			  (place (car f) (car f))
			  (value (cadr f) (cadr f)))
			((null f))
			(if (null (cdr f)) (error "Odd number of arguments to setf: ~A" forms))
			(if (symbolp place)
				(setf place (expand-symbol-macros place)))		;; catch any symbol macros
			:try-again
            
            ;; handle THE form as a place (CLHS: Section 5.1.2.4)
            (if (and (consp place) (eq (car place) 'the) (cdr place) (cddr place))
                (setq value (list 'the (cadr place) value) place (caddr place)))
            
			(if (symbolp place)
				(setq form-list (cons `(setq ,place ,value) form-list))
				(if (and (consp place)		;; check for APPLY special case
						(eq (car place) 'apply)
						(consp (cadr place))
						(or (eq (caadr place) 'quote)(eq (caadr place) 'function)))
					(let ((name (cadadr place)))
						(setq form-list (cons `(apply (quote ,(get-setf-function name)) ,value ,@(cddr place)) form-list)))
					(let ((expander-func (get-setf-expander-function (car place)))) ;; see if there is an expander function
						(if expander-func
                            ;; expansion function has been defined with DEFINE-SETUP-EXPANDER
							(setq form-list (cons (expand-with-expander-func expander-func place value) form-list))
                            ;; otherwise try default expansion method
							(let ((expansion-func (get-setf-function (car place))) 
								  (value-last-p (setf-function-value-last-p (car place))))
                                
                                ;; if no expansion was found, try macroexpanding the place form
                                ;; as long as the expansion results in a new form (it was a macro) go
                                ;; back and try again
								(when (null expansion-func)
									(let ((temp (macroexpand-1 place)))
                                        (unless (eq temp place)
                                            (setq place temp)
									        (go :try-again))))
								(if (null expansion-func)
									(error "There is no SETF expansion method defined for ~A" place))
								(if (symbolp expansion-func)
									(if value-last-p
                                        (if (consp value-last-p) ;; if defsetf long form
                                            (multiple-value-bind (dummies vals newval setter getter)
                                                (funcall expansion-func place nil)
                            	                (declare (ignore getter))
                                                (setq form-list
                                                    (cons `(let* (,@(mapcar #'list dummies vals))
                                                        (multiple-value-bind ,newval ,value ,setter))
                                                        form-list)))
										    (setq form-list (cons `(,expansion-func ,@(cdr place) ,value) form-list)))
                                        ;; because we need to pass the value as the first argument, we need to
                                        ;; evaluate all the following arguments first to preserve the left-to-right
                                        ;; evaluation. However, if the expansion symbol is a macro, we will not evaluate
                                        ;; anything because we need to leave this to the macro to deal with.
                                        (if (macro-function expansion-func)
                                            (setq form-list (cons `(,expansion-func ,value ,@(cdr place)) form-list))
                                            (let ((inits '())
                                                  (syms '()))
                                                (dolist (x (cdr place))
                                                    (let ((sym (gensym)))
                                                        (push `(,sym ,x) inits)
                                                        (push sym syms)))
                                                (setq form-list 
                                                    (cons 
                                                        `(let* (,@(nreverse inits))
                                                            (,expansion-func ,value ,@(nreverse syms))) form-list)))))
									(if value-last-p
                                        (if (consp value-last-p) ;; if defsetf long form
                                            
                                            (multiple-value-bind (dummies vals newval setter getter)
                                                (funcall expansion-func place nil)
                            	                (declare (ignore getter))
                                                (setq form-list
                                                    (cons `(let* (,@(mapcar #'list dummies vals))
                                                        (multiple-value-bind ,newval ,value ,setter))
                                                        form-list)))
                                            
                                           ;; (setq form-list (cons (funcall expansion-func place value) form-list))
										    (setq form-list (cons `(funcall ,expansion-func ,@(cdr place) ,value) form-list)))
										(setq form-list (cons `(funcall ,expansion-func ,value ,@(cdr place)) form-list))))))))))
		(if (cdr form-list)
			`(progn ,@(nreverse form-list))
			(car form-list))))

;;
;;	Common Lisp DEFUN macro.
;;	This redefines the built-in special form.
;;
(defmacro defun (name lambda-list &rest forms)
  (let ((doc-form nil) 
        (lambda-form nil) 
        (declarations nil)
        (setf-form nil)
        (original-name name)
        (block-name name))

        (when (and (consp name) (eq (car name) 'setf))
            (unless (and (symbolp (cadr name)) (eql 2 (length name)))
                (error "Invalid function name: ~A" name))
            (setq setf-form (cadr name))
            (setq block-name (cadr name))
            (setq name (setf-function-symbol name)))
        
        ;; look for declarations and doc string
        (do* ((f forms (cdr f)))
            ((null f) (setq forms f))
            (if (and (typep (car f) 'string) (null doc-form) (cdr f))
                (setq doc-form `((setf (documentation ',name 'function) ,(car f))))
                (if (and (consp (car f)) (eq (caar f) 'declare))
                    (push (car f) declarations)
                    (progn (setq forms f) (return)))))
        (setq lambda-form 
            `(lambda ,lambda-list ,@(nreverse declarations) (block ,block-name ,@forms)))

        `(progn ,@doc-form
            (setf (symbol-function ',name) (function ,lambda-form))
            ,@(when setf-form `((cl::register-setf-function ',setf-form ',name))) ',original-name)))

(defun ccl::macro-lambda-list () nil)
(defun (setf ccl::macro-lambda-list) (val list) (declare (ignore val list)) nil)
 
;;
;;	Common Lisp 'defmacro' macro.
;;	This redefines the built-in special form.
;;
(defmacro defmacro (name lambda-list &rest forms)
	(let ((doc-form nil) 
		  (lambda-form nil)
		  (declarations nil)
		  (setf-form nil))

		(if (and (consp name) (eq (car name) 'setf))
			(progn
				(unless (symbolp (cadr name)) (error "Invalid macro: ~A" name))
				(setq setf-form (cadr name))
				(setq name (setf-function-symbol name))))

		;; look for declarations and doc string
		(do* ((f forms (cdr f)))
			((null f) (setq forms f))
			(if (and (typep (car f) 'string) (null doc-form) (cdr f))
				(setq doc-form 
					`((setf (documentation ',name 'function) ,(car f))))
				(if (and (consp (car f)) (eq (caar f) 'declare))
					(push (car f) declarations)
					(progn (setq forms f) (return)))))

		(setq lambda-form 
			`(lambda (form &optional env)
				(declare (ignore env ,@(unless lambda-list '(form)))) 
				(macro-bind ,lambda-list 
					form
					,@(nreverse declarations) 
					(block ,name ,@forms)))) 
		
		(if setf-form 		
			`(progn
				,@doc-form
				(setf (macro-function ',name) (function ,lambda-form))
				(register-setf-function ',setf-form ',name)
				',name) 
			`(progn
				,@doc-form
				(setf (macro-function ',name) (function ,lambda-form))
				(setf (ccl::macro-lambda-list (symbol-function ',name)) ',lambda-list)
				',name)))) 

(defmacro defsetf (sym first &rest rest)
	(if (symbolp first) ;; if short form
		`(progn 
			(register-setf-function ',sym ',first t) 
			',sym)
		`(progn
			(register-setf-function ',sym
				#'(lambda ,(append (car rest) first) (block ,sym ,@(cdr rest))))
			',sym)))

(defun fdefinition (function-specifier)
	(if (consp function-specifier)
		(symbol-function (get-setf-function (cadr function-specifier)))
		(symbol-function function-specifier)))

(defun (setf fdefinition) (value function-specifier)
  	(if (consp function-specifier)
		(let* ((func (cadr function-specifier))
			   (set-sym (get-setf-function func)))
			(unless set-sym
				(progn
					(setq set-sym (setf-function-symbol function-specifier))
					(register-setf-function func set-sym)))
			(setf (symbol-function set-sym) value))
		(setf (symbol-function function-specifier) value)))

;;;;
;;;;	Common Lisp FMAKUNBOUND function.
;;;;		
(defun fmakunbound (function-specifier)
  (if (consp function-specifier)
      (%fmakunbound (get-setf-function (cadr function-specifier)))
      (%fmakunbound function-specifier)))

;;; print-unreadable-object is the standard way in the new Common Lisp
;;; to generate #< > around objects that can't be read back in.  The option
;;; (:identity t) causes the inclusion of a representation of the object's
;;;  identity, typically some sort of machine-dependent storage address.

(defmacro print-unreadable-object
          ((object stream &key type identity) &body body)
  `(let ((.stream. ,stream)
         (.object. ,object))
	 ,@(unless (or type identity) '((declare (ignore .object.))))
     (format .stream. "#<")
     ,(when type
        '(format .stream. "~S" (type-of .object.)))
     ,(when (and type (or body identity))
        '(format .stream. " "))
     ,@body
     ,(when (and identity body)
        '(format .stream. " "))
     ,(when identity
     		'(format .stream. "#x~X" (common-lisp::%uvector-address .object.))
       )
     (format .stream. ">")
     nil))

;;;
;;;	Common Lisp (SETF FIRST) functions
;;;
(defun (setf first) (val x) (setf (car x) val))
(defun (setf second) (val x) (setf (car (cdr x)) val))
(defun (setf third) (val x) (setf (car (cdr (cdr x))) val))
(defun (setf fourth) (val x) (setf (car (cdr (cdr (cdr x)))) val))
(defun (setf fifth) (val x) (setf (car (cdr (cdr (cdr (cdr x))))) val))
(defun (setf sixth) (val x) (setf (car (cdr (cdr (cdr (cdr (cdr x)))))) val))
(defun (setf seventh) (val x) (setf (car (cdr (cdr (cdr (cdr (cdr (cdr x))))))) val))
(defun (setf eighth) (val x) (setf (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr x)))))))) val))
(defun (setf ninth) (val x) (setf (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr x))))))))) val))
(defun (setf tenth) (val x) (setf (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr x)))))))))) val))

;;;
;;;	Common Lisp (SETF REST) function.
;;;
(defun (setf rest)(val x) (setf (cdr x) val))
	
;;;
;;;	Common Lisp (SETF VALUES) macro.
;;;
(defmacro (setf values) (vals &rest places)
	(let ((sym (gensym))
		  (setf-forms nil)
		  (retvals nil)
		  (index 0))
		(dolist (x places)
			(push `(setf ,x (nth ,index ,sym)) setf-forms)
			(push `(nth ,index ,sym) retvals)
			(incf index))
		`(let ((,sym (multiple-value-list ,vals)))
				,@(nreverse setf-forms)
				(values ,@(nreverse retvals)))))

;;;
;;;	Common Lisp PSETF macro.
;;;
(defmacro psetf (&rest pairs)
	(let ((places nil)
		  (value-forms nil)
		  (temp-syms nil)
		  (var-forms nil)
		  (setf-forms nil))
		(do ((x pairs (cddr x)))
			((endp x))
			(if (endp (cdr x))
				(error "Odd number of arguments to PSETF"))
			(push (car x) places)
			(push (cadr x) value-forms)
			(push (gensym) temp-syms))
		
		(setf places (nreverse places))
		(setf value-forms (nreverse value-forms))
		;; no need to reverse the generated symbols
		
		(do ((s temp-syms (cdr s))
			 (v value-forms (cdr v))
			 (p places (cdr p)))
			((endp s))
			(push `(,(car s) ,(car v)) var-forms)
			(push `(setf ,(car p) ,(car s)) setf-forms))
		
		`(let ,(nreverse var-forms)
			,@(nreverse setf-forms)
			nil)))

;;;
;;; SETF function implementations from Frank Adrian
;;;
;(defun (setf subseq) (val seq start &optional (end nil))
; 	(replace seq val :start1 start :end1 end)
;	val)

(defun (setf caar) (val list) (setf (car (car list)) val))
(defun (setf cadr) (val list) (setf (car (cdr list)) val))
(defun (setf cdar) (val list) (setf (cdr (car list)) val))
(defun (setf cddr) (val list) (setf (cdr (cdr list)) val))

(defun (setf caaar) (val list) (setf (car (car (car list))) val))
(defun (setf caadr) (val list) (setf (car (car (cdr list))) val))
(defun (setf cadar) (val list) (setf (car (cdr (car list))) val))
(defun (setf caddr) (val list) (setf (car (cdr (cdr list))) val))
(defun (setf cdaar) (val list) (setf (cdr (car (car list))) val))
(defun (setf cdadr) (val list) (setf (cdr (car (cdr list))) val))
(defun (setf cddar) (val list) (setf (cdr (cdr (car list))) val))
(defun (setf cdddr) (val list) (setf (cdr (cdr (cdr list))) val))

(defun (setf caaaar) (val list) (setf (car (car (car (car list)))) val))
(defun (setf caaadr) (val list) (setf (car (car (car (cdr list)))) val))
(defun (setf caadar) (val list) (setf (car (car (cdr (car list)))) val))
(defun (setf caaddr) (val list) (setf (car (car (cdr (cdr list)))) val))
(defun (setf cadaar) (val list) (setf (car (cdr (car (car list)))) val))
(defun (setf cadadr) (val list) (setf (car (cdr (car (cdr list)))) val))
(defun (setf caddar) (val list) (setf (car (cdr (cdr (car list)))) val))
(defun (setf cadddr) (val list) (setf (car (cdr (cdr (cdr list)))) val))

(defun (setf cdaaar) (val list) (setf (cdr (car (car (car list)))) val))
(defun (setf cdaadr) (val list) (setf (cdr (car (car (cdr list)))) val))
(defun (setf cdadar) (val list) (setf (cdr (car (cdr (car list)))) val))
(defun (setf cdaddr) (val list) (setf (cdr (car (cdr (cdr list)))) val))
(defun (setf cddaar) (val list) (setf (cdr (cdr (car (car list)))) val))
(defun (setf cddadr) (val list) (setf (cdr (cdr (car (cdr list)))) val))
(defun (setf cdddar) (val list) (setf (cdr (cdr (cdr (car list)))) val))
(defun (setf cddddr) (val list) (setf (cdr (cdr (cdr (cdr list)))) val))

(defun (setf logical-pathname-translations) (val host)
	(declare (ignore host))
	#| we don't currently do anything with these |#
	val)

			