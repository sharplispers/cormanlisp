;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		structures.lisp
;;;;	Contents:	Corman Lisp Structure facility.
;;;;
;;;;    History:    09/07/07 RGC  Integrated Matthias Hölzl's fixes for SUBTYPEP, OPEN, 
;;;;                              and BOA constructors. 
;;;;

(provide :structures)
(in-package :common-lisp)

(defun intern-structure-class (name superclasses) 
	(declare (ignore name superclasses))) ;; redefined by CLOS

(defun define-struct-template (name class type base initial-offset num-slots &rest fields)
	(apply 'vector name class type base initial-offset num-slots fields))

(defun omit-type-checks () (> ccl:*compiler-optimize-speed* ccl:*compiler-optimize-safety*))

;(defconstant struct-template-name-offset 0)
(defconstant struct-template-class-offset 1)
(defconstant struct-template-type-offset 2)
(defconstant struct-template-base-offset 3)	;; included structure type
(defconstant struct-template-initial-offset-offset 4)
;(defconstant struct-template-num-slots-offset 5)
;(defconstant struct-template-slot1-offset 6)

(defconstant struct-template-slot-name-offset 0)
(defconstant struct-template-slot-default-offset 1)
(defconstant struct-template-slot-type-offset 2)
(defconstant struct-template-slot-ro-p-offset 3)
(defconstant struct-template-slot-inline-p-offset 4)
;(defconstant struct-template-slot-size 5) ;redefined in write.lisp


(defun struct-template-num-slots (template)
	(elt template struct-template-num-slots-offset))
(defun struct-template-first-slot (template)
	(elt template struct-template-num-slots-offset))
(defun struct-template-type (template)
	(elt template struct-template-type-offset))
(defun struct-template-slot-name (template index)
	(elt template (+ struct-template-slot1-offset
					(* index struct-template-slot-size)
					struct-template-slot-name-offset)))
(defun struct-template-slot-default (template index)
	(elt template (+ struct-template-slot1-offset
					(* index struct-template-slot-size)
					struct-template-slot-default-offset)))
(defun struct-template-slot-type (template index)
	(elt template (+ struct-template-slot1-offset
					(* index struct-template-slot-size)
					struct-template-slot-type-offset)))
(defun struct-template-slot-ro-p (template index)
	(elt template (+ struct-template-slot1-offset
					(* index struct-template-slot-size)
					struct-template-slot-ro-p-offset)))
(defun struct-template-slot-inline-p (template index)
	(elt template (+ struct-template-slot1-offset
					(* index struct-template-slot-size)
					struct-template-slot-inline-p-offset)))
(defun struct-template-base (template)
	(elt template struct-template-base-offset))
(defun struct-template-class (template)
	(elt template struct-template-class-offset))

(defun clone-struct (s)
	(let* ((template (uref s 1))
		   (num-slots (elt template struct-template-num-slots-offset))
		   (copy (alloc-uvector (+ 1 num-slots) uvector-structure-tag)))
		(setf (uref copy 1) (uref s 1))
		(dotimes (i num-slots)
			(setf (uref copy (+ i 2)) (uref s (+ i 2))))
		copy))

(defun struct-type (s) 
	(let ((template (uref s 1)))
		(if (vectorp template) (elt template struct-template-name-offset))))

(defun struct-type-p (s name)
	(and (structurep s) 
		(let ((template (uref s 1)))
			(or (eq name (if (vectorp template) 
						(uref template (+ 2 struct-template-name-offset)))) 
				(and (vectorp template)
					(if (member name (struct-template-base template)) t))))))

(defun check-struct-type (s name)
	(unless (struct-type-p s name) 
		(error "The object ~A is not a ~A structure" s name)))

(defun initial-value-expression (struct-template slot-name)
	(let* ((num-slots (elt struct-template struct-template-num-slots-offset))
		   (ret 
				(dotimes (i num-slots nil)
					(if (eq (struct-template-slot-name struct-template i) slot-name)
				 		(return (struct-template-slot-default struct-template i))))))
		(if (functionp ret) 
			`(funcall ,ret)
			ret)))

(defun parse-boa-lambda-list (lambda-list struct-template)
	(let ((state '&required)
		  (result nil)
		  (new-lambda-list nil))
		(dolist (x lambda-list)
			(if (is-lambda-key x)
				(progn
					(setq state (car (is-lambda-key x)))
					(push x new-lambda-list))
				(if (symbolp x)
					(if (or (eq state '&optional)(eq state '&key)(eq state '&aux))
						(let ((init (initial-value-expression struct-template x)))
							(push `(,state ,x ,init) result)
							(push `(,x ,init) new-lambda-list))
						(progn
							(push `(,state ,x) result)
							(push x new-lambda-list)))
					(if (consp x)
						(if (or (eq state '&optional)(eq state '&key)(eq state '&aux))
							(let ((init 
										(if (consp (cdr x))
											(cadr x)
											(initial-value-expression struct-template (car x))))
                                  (supplied-p-var (if (consp (cddr x)) (caddr x))))
								(push `(,state ,(car x) ,init) result)
								(push `(,(car x) ,init ,@(if supplied-p-var (list supplied-p-var))) new-lambda-list))
							(error "Invalid lambda-list ~A for BOA constructor" lambda-list))
						(error "Invalid lambda-list ~A for BOA constructor" lambda-list)))))
		(values (nreverse result) (nreverse new-lambda-list))))
		
;; only support optional and required arguments for now
(defmacro create-boa-constructor (func-name lambda-list struct-template struct-template-sym 
			struct-name struct-type named) 
	(let ((struct-sym (gensym))
		  (slot-initializers nil)
		  (type-checks nil)
		  (parsed-args nil)
		  (new-lambda-list nil)
		  (allocate-form nil)
		  (name-form nil)
		  (num-slots (elt struct-template struct-template-num-slots-offset))
		  (initial-offset (elt struct-template struct-template-initial-offset-offset)))
		
		(multiple-value-setq (parsed-args new-lambda-list) 
			(parse-boa-lambda-list lambda-list struct-template))
		
		;; create type-check forms
		(dotimes (index num-slots)
			(let* ((slot-name (struct-template-slot-name struct-template index))
				   (present (member slot-name parsed-args :key #'second)))
				(when present
					(unless (or (omit-type-checks)
								(eq (struct-template-slot-type struct-template index) t))
						(push `(check-type ,slot-name
							,(struct-template-slot-type struct-template index))
							type-checks)))))
				

		;; create slot initializer forms
		(do* ((index 0 (+ index 1))
			  slot-name
			  default-initializer
			  param)
			((= index num-slots))
			(setf slot-name (struct-template-slot-name struct-template index))
			(setf default-initializer (struct-template-slot-default struct-template index))
			(setq param (car (member slot-name parsed-args :key #'second)))
			(if (functionp default-initializer) 
				(setq default-initializer `(funcall ,default-initializer)))
			(cond 
				((eq struct-type 'list)
				 (case (car param)
					(&required (push `(setf (nth ,(+ index (if named 1 0) initial-offset) ,struct-sym) ,(second param)) 
								slot-initializers))
					(&optional (push `(setf (nth ,(+ index (if named 1 0) initial-offset) ,struct-sym) ,(second param)) 
								slot-initializers))
					(&rest 		(push `(setf (nth ,(+ index (if named 1 0) initial-offset) ,struct-sym) ,(second param)) 
								slot-initializers))
					(&key 		(push `(setf (nth ,(+ index (if named 1 0) initial-offset) ,struct-sym) ,(second param)) 
								slot-initializers))
					(&aux 		(push `(setf (nth ,(+ index (if named 1 0) initial-offset) ,struct-sym) ,(second param)) 
								slot-initializers))
					((nil) 		(push `(setf (nth ,(+ index (if named 1 0) initial-offset) ,struct-sym) ,default-initializer)
							slot-initializers)))
			 	 (setf allocate-form `(make-list ,(+ num-slots (if named 1 0) initial-offset)))
			 	 (setf name-form (if named `((setf (elt ,struct-sym ,initial-offset) ',struct-name))))) 
				((or (eq struct-type 'vector)(and (consp struct-type) (eq (car struct-type) 'vector)))
				 (case (car param)
					(&required 	(push `(setf (elt ,struct-sym ,(+ index (if named 1 0) initial-offset)) 
									,(second param)) slot-initializers))
					(&optional 	(push `(setf (elt ,struct-sym ,(+ index (if named 1 0) initial-offset)) 
									,(second param)) slot-initializers))
					(&rest 		(push `(setf (elt ,struct-sym ,(+ index (if named 1 0) initial-offset)) 
									,(second param)) slot-initializers))
					(&key 		(push `(setf (elt ,struct-sym ,(+ index (if named 1 0) initial-offset)) 
									,(second param)) slot-initializers))
					(&aux 		(push `(setf (elt ,struct-sym ,(+ index (if named 1 0) initial-offset)) 
									,(second param)) slot-initializers))
					((nil) 		(push `(setf (elt ,struct-sym ,(+ index (if named 1 0) initial-offset)) ,default-initializer)
							slot-initializers)))
			 	 (setf allocate-form 
					`(make-array 
						,(+ num-slots (if named 1 0) initial-offset)
						:element-type ',(if (and (consp struct-type)(cadr struct-type)) (cadr struct-type) t)))
			 	 (setf name-form (if named `((setf (elt ,struct-sym ,initial-offset) ',struct-name))))) 
				((null struct-type)
			 	 (case (car param)
					(&required 	(push `(setf (uref ,struct-sym ,(+ index 2)) ,(second param)) slot-initializers))
					(&optional 	(push `(setf (uref ,struct-sym ,(+ index 2)) ,(second param)) slot-initializers))
					(&rest 		(push `(setf (uref ,struct-sym ,(+ index 2)) ,(second param)) slot-initializers))
					(&key 		(push `(setf (uref ,struct-sym ,(+ index 2)) ,(second param)) slot-initializers))
					(&aux 		(push `(setf (uref ,struct-sym ,(+ index 2)) ,(second param)) slot-initializers))
					((nil) 		(push `(setf (uref ,struct-sym ,(+ index 2)) ,default-initializer) slot-initializers)))
			 	 (setf allocate-form `(alloc-uvector ,(+ num-slots 1) uvector-structure-tag))
			 	 (setf name-form `((setf (uref ,struct-sym 1) ,struct-template-sym)))))) 

		`(defun ,func-name ,new-lambda-list
			(let* ((,struct-sym ,allocate-form))
				,@(nreverse type-checks)
				,@name-form
				,@(nreverse slot-initializers)
				,struct-sym))))

(defmacro create-keyword-constructor (func-name struct-template struct-template-sym struct-name struct-type named)
	(let* ((lambda-list nil)
		  (slot-initializers nil)
		  (type-checks nil)
		  (struct-sym (gensym))
		  (allocate-form nil)
		  (name-form nil)
		  (num-slots (elt struct-template struct-template-num-slots-offset))
		  (initial-offset (elt struct-template struct-template-initial-offset-offset)))
		(do* ((slot-index 0 (+ slot-index 1))
			  slot-name
			  default-initializer)
			((= slot-index num-slots))
			(setf slot-name (struct-template-slot-name struct-template slot-index))
			(setf default-initializer (struct-template-slot-default struct-template slot-index))
			(if (functionp default-initializer)
				(setq default-initializer `(funcall ,default-initializer)))
			(push `(,slot-name ,default-initializer) lambda-list))
		(setq lambda-list (cons '&key (nreverse lambda-list)))
		
		;; create type-check forms
		(dotimes (i num-slots)
			(unless (or (omit-type-checks)
						(eq (struct-template-slot-type struct-template i) t))
				(push `(check-type ,(struct-template-slot-name struct-template i)
					,(struct-template-slot-type struct-template i))
					type-checks)))

		
		;; create slot initializer forms and allocation form
		(cond 
			((eq struct-type 'list)
			 (dotimes (i num-slots)
					(push
						`(setf (elt ,struct-sym ,(+ i (if named 1 0) initial-offset))
							,(struct-template-slot-name struct-template i))
						slot-initializers))
			 (setf allocate-form `(make-list ,(+ num-slots (if named 1 0) initial-offset)))
			 (setf name-form (if named `((setf (elt ,struct-sym ,initial-offset) ',struct-name))))) 
			((or (eq struct-type 'vector)(and (consp struct-type) (eq (car struct-type) 'vector)))
			 (dotimes (i num-slots)
					(push 
						`(setf (elt ,struct-sym ,(+ i (if named 1 0) initial-offset)) 
							,(struct-template-slot-name struct-template i))
						slot-initializers))
			 (setf allocate-form 
					`(make-array 
						,(+ num-slots (if named 1 0) initial-offset)
						:element-type ',(if (and (consp struct-type)(cadr struct-type)) (cadr struct-type) t)))
			 (setf name-form (if named `((setf (elt ,struct-sym ,initial-offset) ',struct-name))))) 
			((null struct-type)
			 (dotimes (i num-slots)
					(push `(setf (uref ,struct-sym ,(+ i 2))
							,(struct-template-slot-name struct-template i))
						slot-initializers))
			 (setf allocate-form `(alloc-uvector ,(+ num-slots 1) uvector-structure-tag))
			 (setf name-form `((setf (uref ,struct-sym 1) ,struct-template-sym)))) 
			(t (error "Invalid :type for DEFSTRUCT structure type: ~S" struct-type)))
		 
		`(defun ,func-name ,lambda-list
			(let* ((,struct-sym ,allocate-form))
				,@(nreverse type-checks)
				,@name-form
				,@(nreverse slot-initializers)
				,struct-sym))))

(defun is-vector-structure (structure-type)
	(or (eq structure-type 'vector) (and (consp structure-type) (eq (car structure-type) 'vector))))

(defun build-accessor (structure-name structure-type named-p initial-offset
						accessor-name slot-index included)
	(let ((structure-type-check
				(unless (or (omit-type-checks) structure-type)
					 `(check-struct-type arg ',structure-name)))
		  expr)
		(cond
			((eq structure-type 'list)
				(push `(nth ,(+ slot-index (if named-p 1 0) initial-offset) arg) expr))
			((is-vector-structure structure-type)
				(push `(aref arg ,(+ slot-index (if named-p 1 0) initial-offset)) expr))
			((null structure-type)
				(push `(uref arg ,(+ slot-index 2)) expr))
			(t (error "Invalid :type for DEFSTRUCT structure type: ~S" structure-type)))
		(when structure-type-check (push structure-type-check expr))
		(push '(arg) expr)
		(push accessor-name expr)
		(push 'defun expr)
        
        ;; if the slot is an included slot, do not create an accessor if a 
        ;; function by that name already exists.
        ;; Although this is not 100% what the Hyperspec says, it is as close as 
        ;; we can get for now because we are not keeping track of which accessors
        ;; are associated with a structure.
        (if included (setf expr `(unless (fboundp ',accessor-name) ,expr)))
        
		expr))

(defun build-mutator (structure-name structure-type named-p initial-offset
						accessor-name slot-index slot-type included)
	(let* ((function-designator `(setf ,accessor-name))
		   (type-check (unless (or (omit-type-checks) (eq slot-type t))
						`(check-type val ,slot-type)))
		  (structure-type-check
				(unless (or (omit-type-checks) structure-type)
					 `(check-struct-type arg ',structure-name)))
		  expr)
		(cond
			((eq structure-type 'list)
				(push `(setf (nth ,(+ slot-index (if named-p 1 0) initial-offset) arg) val) expr))
			((is-vector-structure structure-type)
				(push `(setf (aref arg ,(+ slot-index (if named-p 1 0) initial-offset)) val)expr))
			((null structure-type)
				(push `(setf (uref arg ,(+ slot-index 2)) val) expr))
			(t (error "Invalid :type for DEFSTRUCT structure type: ~S" structure-type)))
		(when structure-type-check (push structure-type-check expr))
		(when type-check (push type-check expr))
		(push '(val arg) expr)
		(push function-designator expr)
		(push 'defun expr)
        
        ;; if the slot is an included slot, do not create an accessor if a 
        ;; function by that name already exists.
        ;; Although this is not 100% what the Hyperspec says, it is as close as 
        ;; we can get for now because we are not keeping track of which accessors
        ;; are associated with a structure.
        (if included (setf expr `(unless (fboundp ',function-designator) ,expr)))
        
   		expr))

(defmacro defstruct (name-and-options &rest doc-and-slots)
	(let (	name 
			options 
			doc-string 
			slot-descriptors
			(slot-count 0)
			struct-template-info
			constructor-name
			(boa-constructor-info nil)
			(conc-name nil)
			copier-name
			predicate-name
			accessor-name
			(print-function nil)
			(expressions nil)
			(struct-template-expressions nil)
			(struct-type nil)
			(named nil)
			(initial-offset 0)
			(base nil)
			(base-list nil)
			(included-options nil)
			struct-template
			(struct-template-sym (gensym))
            (num-included-slots 0)) 
			
		(if (symbolp name-and-options)
			(setq name name-and-options)
			(progn
				(if (or (not (consp name-and-options)) (not (symbolp (car name-and-options))))
					(error "Invalid syntax for defstruct name: ~A" name-and-options))
				(setq name (car name-and-options))
				(setq options (cdr name-and-options))))
		
		;(format t "Parsing structure: ~A~%" name)

		(setq conc-name (concatenate 'string (symbol-name name) "-"))

		(dolist (opt options)
			;(format t "option: ~A~%" opt)
			(cond
				((keywordp opt)(if (eq opt ':named) (setf named t)))
				((and (listp opt) (keywordp (car opt)))
				 (case (car opt)
					(:conc-name 
						(if (cdr opt)
							(setq conc-name 
									(if (cadr opt) 
										(symbol-name (cadr opt))
										""))
                                (setq conc-name "")))
					(:constructor 
						(if (cdr opt) 
							(if (cddr opt)
								(push (list (cadr opt) (caddr opt)) boa-constructor-info)
								(setq constructor-name (cadr opt)))))
					(:copier (if (cdr opt) (setq copier-name (cadr opt))))
					(:predicate (if (cdr opt) (setq predicate-name (cadr opt))))
					(:include (setf base (cadr opt) included-options (cddr opt)))
					(:print-function (if (cdr opt) (setq print-function (cadr opt))))
					(:type (setf struct-type (cadr opt)))
					(:initial-offset (setf initial-offset (cadr opt)))
					(otherwise (error "Unknown defstruct option: ~A~%" (car opt)))))
				(t (error "Invalid defstruct option: ~A~%" opt))))	

		(if (and (null struct-type) (/= initial-offset 0))
			(error "If :INITIAL-OFFSET is specified in DEFSTRUCT, then :TYPE must also be specified."))

		(if (stringp (car doc-and-slots))
			(progn
				(setq doc-string (car doc-and-slots))
				(setq slot-descriptors (cdr doc-and-slots)))
			(setq slot-descriptors doc-and-slots))
		
		;; add the doc string with structure attribute	
		(if doc-string
			(push 
				`(setf (documentation ',name 'structure) ,doc-string) 
				expressions))
		
		;; if :include specified, add those slots now
		(if base
			(let* ((included-struct-template (get base ':struct-template)))
				(unless included-struct-template
					(error "Cannot :INCLUDE struct type ~S. No slot information was found." base))
				(setf base-list (cons base (struct-template-base included-struct-template)))
				(setf num-included-slots (struct-template-num-slots included-struct-template))
				(dotimes (i num-included-slots)
					(let* ((name (struct-template-slot-name included-struct-template i))
						   (default (struct-template-slot-default included-struct-template i))
						   (type (struct-template-slot-type included-struct-template i))
						   (ro-p (struct-template-slot-ro-p included-struct-template i))
						   (inline-p (struct-template-slot-inline-p included-struct-template i))
						   (override (member name included-options 
									:key #'(lambda (obj) (if (symbolp obj) obj (car obj))))))
						(if override
							(setf default 
								(if (and (consp (car override))(consp (cdar override)))
									(cadar override))))
						(push name struct-template-info) 
						(push default struct-template-info)
						(push type struct-template-info)
						(push ro-p struct-template-info)
						(push inline-p struct-template-info)
						(incf slot-count)))))

		;; process slot options
		(dolist (opt slot-descriptors)
			;(format t "slot: ~A~%" opt)
			(incf slot-count)
			(cond
				((symbolp opt)  
					(push opt struct-template-info)
					(push nil struct-template-info)
					(push t struct-template-info)
					(push nil struct-template-info)
					(push nil struct-template-info)
					;(format t "parsed-slot: ~A ~A ~A ~A ~A~%"
						;opt nil t nil nil)
					)
				((consp opt)
					(let ((sym (car opt))
						  (slot-initializer (cadr opt))
						  (options (cddr opt))
						  (type t)
						  (read-only nil)
						  (inline nil))
						(if (not (symbolp sym))
							(error "Invalid slot descriptor: ~A~%" sym))
						(if (or (not (constantp slot-initializer)) 
								(functionp slot-initializer))
							(setq slot-initializer (compile-form slot-initializer)))
						(do ((o options (cddr o)))
							((null o) nil)
							(case (car o)
								(:type (setf type (cadr o)))
								(:read-only (setf read-only (cadr o)))
								(:inline (setf inline (cadr o)))
								(otherwise (error "Unknown slot option: ~A" (car o)))))
						;(format t "parsed-slot: ~A ~A ~A ~A ~A~%"
							;sym slot-initializer type read-only inline)
						(push sym struct-template-info)
						(push slot-initializer struct-template-info)
						(push type struct-template-info)
						(push read-only struct-template-info)
						(push inline struct-template-info)))
				(t (error "Invalid slot option: ~A~%" opt))))

		(setq struct-template 
			(apply #'define-struct-template name (intern-structure-class name (mapcar 'find-class base-list)) struct-type 
					base-list initial-offset slot-count (reverse struct-template-info)))

		;; install template		
		(push
			`(setf (get ',name :struct-template) ,struct-template-sym)
			struct-template-expressions)

		;; install print function		
		(when print-function
			(if (and (consp print-function) (eq (car print-function) 'lambda))
				(setq print-function `(function ,print-function))
				(setq print-function `(quote ,print-function)))
			(push
				`(setf (get ',name :struct-print) 
					,print-function)
				expressions))
			
		;; install constructor function
		(setq constructor-name
			(if constructor-name 
				(intern (symbol-name constructor-name))
				(if boa-constructor-info			
					(make-symbol (concatenate 'string "MAKE-" (symbol-name name))) ;; invisible
					(intern (concatenate 'string "MAKE-" (symbol-name name))))))
			
		(push
			`(create-keyword-constructor 
				,constructor-name
				,struct-template
				,struct-template-sym
				,name
				,struct-type
				,named)
				struct-template-expressions)

		(push
			`(setf (get ',name ':struct-constructor) ',constructor-name)
				expressions) 
		
		;; install BOA constructor
		(dolist (boa-info boa-constructor-info)
			(push
				`(create-boa-constructor 
					,(car boa-info) 
					,(cadr boa-info)
					,struct-template
					,struct-template-sym
					,name
					,struct-type
					,named)
				struct-template-expressions))
			
		;; install copier function			
		(setq copier-name
			(if copier-name 
				(intern (symbol-name copier-name))
				(intern (concatenate 'string "COPY-" (symbol-name name)))))
			
		(push
			`(defun ,copier-name (arg) (clone-struct arg))
			expressions)
		
		;; install predicate function			
		(setq predicate-name
			(if predicate-name 
				(intern (symbol-name predicate-name))
				(intern (concatenate 'string (symbol-name name) "-P"))))
			
		(push
            (cond ((and named struct-type)
                   `(defun ,predicate-name (arg) 
                        (and (typep arg ',struct-type)
                            (eq (elt arg 0) ',name))))
                  (struct-type `(fmakunbound ',predicate-name))   ;; don't define a predicate
                  (t `(defun ,predicate-name (arg) (struct-type-p arg ',name))))
			expressions)

		;; install type specifier
		(push
			`(cl::declare-type-specifier ,name (x specifier)
				(declare (ignore specifier))
				(struct-type-p x ',name))
			expressions)
		
		;; install accessor functions
        (do* ((num-slots (struct-template-num-slots struct-template))
              (i 0 (+ i 1)))  ;; skip over any included slots--they already have accessors
            ((= i num-slots))
			(setq accessor-name 
				(intern 
					(concatenate 'string conc-name 
						(symbol-name (struct-template-slot-name struct-template i)))))
			(if (struct-template-slot-inline-p struct-template i)
				(push `(proclaim '(inline ,accessor-name)) expressions))
			(push (build-accessor name struct-type named initial-offset
					accessor-name i (< i num-included-slots))
				expressions)
			(when (not (struct-template-slot-ro-p struct-template i))
				(if (struct-template-slot-inline-p struct-template i)
					(push 
						`(proclaim '(inline ,(setf-function-symbol (list 'setf accessor-name)))) 
						expressions))
				(push (build-mutator name struct-type named initial-offset
						accessor-name i (struct-template-slot-type struct-template i) (< i num-included-slots))
					expressions)))

		(push `',name expressions)	
		`(progn
			(let* ((,struct-template-sym ,struct-template)) 
				,@(reverse struct-template-expressions))
			,@(nreverse expressions))))

;;;
;;; Common Lisp COPY-STRUCTURE function.
;;;
(defun copy-structure (structure)
    (let ((new (alloc-uvector (uvector-num-slots structure) uvector-structure-tag)))
        (dotimes (i (uvector-num-slots structure))
            (setf (uref new (+ i 1)) (uref structure (+ i 1))))
        new))


                
                
