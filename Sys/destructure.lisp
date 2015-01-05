;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		destructure.lisp
;;;;	Contents:	
;;;;	History:	11/9/96  RGC  Created.
;;;;				11/22/98 RGC  &environment parameter no longer causes error
;;;;                12/19/02 RGC  Incorporated JP Massar's fix to avoid warning in
;;;;                              DESTRUCTURING-BIND.
;;;;                05/10/04 RGC  Included Carlos Ungil's patch to DESTRUCTURE-BIND to
;;;;                              handle keywords with differing variable names.
;;;;



(defvar lambda-list-keywords '(&optional &rest &key &aux &body &whole &environment &allow-other-keys))
(defvar lambda-states '(:initial :whole :required :optional 
						:rest :key :environment :aux :terminal))

(defun is-lambda-key (sym) (member sym lambda-list-keywords))
(defun is-quoted-key (sym) (and (consp sym) (cdr sym) (eq (car sym) 'quote)))
(defun keyword-test (sym elt) (or (eql sym elt) (and (is-quoted-key elt)(eql sym (cadr elt)))))

(defun destructure-bind (lambda-list form sub-form &optional (start-index 0))
	(let ((state :initial)
		  (list-counter start-index)
		  (expr nil)
		  (new-bindings nil))
		(do* ()
			((not (consp lambda-list)))
			(if (symbolp (car lambda-list))
				(let ((sym (car lambda-list)))
					(cond
						((eq state :initial)
						 (if (is-lambda-key sym)
							(progn
								(setq state :whole)
								(if (eq sym '&whole)
									(setq lambda-list (cdr lambda-list))))
							(setq state :required)))

						((eq state :whole)
						 (if (is-lambda-key sym)
							(setq state :required)
							(progn
								(push `(,sym ,form) new-bindings)
								(setq lambda-list (cdr lambda-list))
								(setq state :required))))

						((eq state :required)
						 (if (is-lambda-key sym)
							(progn
								(setq state :optional)
								(if (eq sym '&optional)
									(setq lambda-list (cdr lambda-list))))
							(progn
								(push `(,sym (nth ,list-counter ,form)) new-bindings)
								(incf list-counter)
								(setq lambda-list (cdr lambda-list)))))

						((eq state :optional)
						 (if (is-lambda-key sym)
							(progn
								(setq state :rest)
								(if (or (eq sym '&rest)(eq sym '&body))
									(setq lambda-list (cdr lambda-list))))
							(progn
								(push `(,sym (nth ,list-counter ,form)) new-bindings)
								(incf list-counter)
								(setq lambda-list (cdr lambda-list)))))

						((eq state :rest)
						 (if (is-lambda-key sym)
							(progn
								(setq state :key)
								(if (eq sym '&key)
									(setq lambda-list (cdr lambda-list))))
							(progn
								(push `(,sym (nthcdr ,list-counter ,form)) new-bindings)
								(setq lambda-list (cdr lambda-list)))))

						((eq state :key)
						 (if (is-lambda-key sym)
							(progn
								(setq state :allow-other-keys)
								(if (eq sym '&allow-other-keys)
									(setq lambda-list (cdr lambda-list))))
							(let ((g (gensym)))
								(push 
									`(,sym 
										(let ((,g (member 
												(intern (symbol-name ',sym) (find-package "KEYWORD")) ,sub-form)))
											(if ,g (cadr ,g)))) new-bindings)
								(setq lambda-list (cdr lambda-list)))))

						((eq state :allow-other-keys)
						 (if (is-lambda-key sym)
							(progn
								(setq state :environment)
								(if (eq sym '&environment)
									(setq lambda-list (cdr lambda-list))))))

						((eq state :environment)
						 (if (is-lambda-key sym)
							(progn
								(setq state :aux)
								(if (eq sym '&aux)
									(setq lambda-list (cdr lambda-list))))
							(progn
								(push `(,sym nil) new-bindings)  ;; not implemented
								(setq lambda-list (cdr lambda-list))
								(setq state :aux))))

						((eq state :aux)
						 (if (is-lambda-key sym)
							(progn
								(setq state :terminal))
							(progn
								(push `(,sym nil) new-bindings)  ;; not implemented
								(setq lambda-list (cdr lambda-list))
								(setq state :aux))))

						(t (error "Invalid lambda list formation"))))

				(let ((inner-list (car lambda-list)))
					(unless (consp inner-list)
						(error "Invalid lambda list formation: not a list or a symbol"))
					(if (or (eq state :initial) (eq state :whole))
						 (setq state :required)) ;; only currently destructure in required arguments

					(cond
						((eq state :required)
						 (let* ((temp-sub-form `(nth ,list-counter ,sub-form))
						        (b (destructure-bind inner-list temp-sub-form temp-sub-form)))
							(dolist (f b)
								(push f new-bindings))
							(incf list-counter)))

						((eq state :optional)
						 (let* ((sym (car inner-list))
							    (init nil)
								(supplied-p nil))
							(unless (symbolp sym)
								(error "Missing symbol in &optional initializer expression"))
							(if (consp (cdr inner-list))
								(progn
									(setq init (cadr inner-list))
									(if (consp (cddr inner-list))
										(setq supplied-p (caddr inner-list)))))
							(if supplied-p
								(progn
									(unless (symbolp supplied-p)
										(error "Invalid form for SUPPLIED-P parameter"))
									(push `(,supplied-p nil) new-bindings)))
							(let ((t3 `(< ,list-counter (length ,form))))
								(if (null supplied-p)
									(setq expr `(,sym (if ,t3 (nth ,list-counter ,form) ,init)))
									(setq expr `(,sym (if ,t3 
														(progn (setq ,supplied-p t) (nth ,list-counter ,form))
														,init))))
								(incf list-counter)
								(push expr new-bindings))))

						((eq state :key)
						 (let* ((sym (car inner-list))
							    (init nil)
                                (named-keyword nil)
								(supplied-p nil)
								(g (gensym)))
							(unless (symbolp sym)
                                (when (consp sym)
                                     (setq named-keyword (car sym))
                                     (setq sym (cadr sym)))
                                (unless (symbolp sym)
                                    (error "Missing symbol or (keyword-name var) list in &key initializer expression")))

							(if (consp (cdr inner-list))
								(progn
									(setq init (cadr inner-list))
									(if (consp (cddr inner-list))
										(setq supplied-p (caddr inner-list)))))
							(if supplied-p
								(progn
									(unless (symbolp supplied-p)
										(error "Invalid form for SUPPLIED-P parameter"))
									(push `(,supplied-p nil) new-bindings)
									(setq expr
										`(,sym 
											(let ((,g (member ',(if named-keyword
                                                                    named-keyword
                                                                    (intern (symbol-name sym)(find-package "KEYWORD"))) 
                                                                    ,sub-form
                                                                :test #'keyword-test)))
												(if ,g (progn (setq ,supplied-p t) (cadr ,g)) ,init)))))									
								(setq expr 
										`(,sym 
											(let ((,g (member ',(if named-keyword 
                                                                    named-keyword       
                                                                    (intern (symbol-name sym)(find-package "KEYWORD"))) 
                                                                    ,sub-form
                                                            :test #'keyword-test)))
												(if ,g (cadr ,g) ,init))))) 
							(push expr new-bindings)))

						((eq state :aux)
						 (let* ((sym (car inner-list))
						        (init nil))
							(unless (symbolp sym)
								(error "Missing symbol in &aux initializer expression"))

							(if (consp (cdr inner-list))
								(setq init (cadr inner-list)))
							(push `(,sym ,init) new-bindings))))

					(setq lambda-list (cdr lambda-list)))))

		;; handle 'dotted' lambda list
		(if (and lambda-list (symbolp lambda-list))
			(push `(,lambda-list (nthcdr ,list-counter ,form)) new-bindings))

		(nreverse new-bindings)))

(defmacro destructuring-bind (lambda-list form &rest forms)
  (let* ((temp-sym (gensym))
	 (bindings (destructure-bind lambda-list temp-sym temp-sym 0)))
    (push `(,temp-sym ,form) bindings)
    `(let* ,bindings 
       ,@(when (null lambda-list) `((declare (ignore ,temp-sym))))
       ,@forms)))

(defmacro macro-bind (lambda-list form &rest forms)
	(let* ((temp-sym (gensym))
		   (bindings (destructure-bind lambda-list temp-sym temp-sym 1)))
		(if bindings 
			(push `(,temp-sym ,form) bindings))
		`(let* ,bindings ,@forms)))

;;
;;	Common Lisp 'defmacro' macro.
;;
(defmacro defmacro (name lambda-list &rest forms)
	(let ((doc-form nil) 
		  (lambda-form nil)
		  (declarations nil))

		;; look for declarations and doc string
		(do* ((f forms (cdr f)))
			((null f) (setq forms f))
			(if (and (stringp (car f)) (null doc-form) (cdr f))
				(setq doc-form 
					`((setf (documentation ',name 'macro) ,(car f))))
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
		`(progn
			,@doc-form
			(setf (macro-function ',name) (function ,lambda-form))
			',name))) 

(defun type-destructure-bind (lambda-list form sub-form)
	(let ((state :required)
		  (list-counter 0)
		  (expr nil)
		  (new-bindings nil)
		  (save-lambda-list lambda-list))
		(do* ()
			((not (consp lambda-list)))
			(if (symbolp (car lambda-list))
				(let ((sym (car lambda-list)))
					(cond
						((eq state :required)
						 (if (is-lambda-key sym)
							(progn
								(setq state :optional)
								(if (eq sym '&optional)
									(setq lambda-list (cdr lambda-list))))
							(progn
								(push `(,sym (type-nthcar ,list-counter ,form)) new-bindings)
								(incf list-counter)
								(setq lambda-list (cdr lambda-list)))))

						((eq state :optional)
						 (if (is-lambda-key sym)
							(progn
								(setq state :rest)
								(if (or (eq sym '&rest)(eq sym '&body))
									(setq lambda-list (cdr lambda-list))))
							(progn
								(push `(,sym (type-nthcar ,list-counter ,form)) new-bindings)
								(incf list-counter)
								(setq lambda-list (cdr lambda-list)))))

						((eq state :rest)
						 (if (is-lambda-key sym)
							(setq state :terminal)
							(progn
								(push `(,sym (type-nthcdr ,list-counter ,form)) new-bindings)
								(incf list-counter)
								(setq lambda-list (cdr lambda-list))
								(setq state :terminal))))

						((or t (eq state :terminal))
						 (error "Invalid lambda list in type specifier: ~A"	save-lambda-list))))

				(let ((inner-list (car lambda-list)))
					(unless (consp inner-list)
						(error "Invalid lambda list formation: not a list or a symbol: ~A" save-lambda-list))
					(cond
						((eq state :required)
						 (let* ((temp-sub-form `(type-nthcar ,list-counter ,sub-form))
						        (b (type-destructure-bind inner-list temp-sub-form temp-sub-form)))
							(dolist (f b)
								(push f new-bindings))
							(incf list-counter)))

						((eq state :optional)
						 (let* ((sym (car inner-list))
							    (init nil)
								(supplied-p nil))
							(unless (symbolp sym)
								(error "Missing symbol in &optional initializer expression"))
							(if (consp (cdr inner-list))
								(progn
									(setq init (cadr inner-list))
									(if (consp (cddr inner-list))
										(setq supplied-p (caddr inner-list)))))
							(if supplied-p
								(progn
									(unless (symbolp supplied-p)
										(error "Invalid form for supplied_p parameter: ~A" supplied-p))
									(push `(,supplied-p nil) new-bindings)))
							(let* ((t1 `(< ,list-counter (length ,form)))
								   (t2 `(type-nthcar ,list-counter ,form))
								   (expr
									(if supplied-p 
										`(,sym (if ,t1 (progn (setq ,supplied-p t) ,t2) ,init))
										`(,sym (if ,t1 ,t2 ,init)))))
								(incf list-counter)
								(push expr new-bindings)))))
					(setq lambda-list (cdr lambda-list)))))

		;; handle 'dotted' lambda list
		(if (and lambda-list (symbolp lambda-list))
			(push `(,lambda-list (type-nthcdr ,list-counter ,form)) new-bindings))

		(nreverse new-bindings)))

(defmacro type-destructuring-bind (lambda-list expr &rest forms)
	(let* ((temp-symbol (gensym))
		   (bindings (type-destructure-bind lambda-list temp-symbol temp-symbol)))
		(if bindings
			(push `(,temp-symbol ,expr) bindings))
		`(let* ,bindings 
			,@forms)))

;;;;
;;;; Used by deftype() macro expansion
(defun type-nthcar (index list)
	(if (minusp index)
		(error "Negative value for index: ~A" index))
	(do ()
		((not (consp list)) '*)
		(if (< index 1)
			(return (car list)))
		(setq list (cdr list))
		(decf index)))

;;;;
;;;; Used by deftype() macro expansion
(defun type-nthcdr (index list)
	(if (minusp index)
		(error "Negative value for index: ~A" index))
	(dotimes (i index)
		(setq list (cdr list)))
	list)
