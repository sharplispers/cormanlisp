;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		bootstrap.lisp
;;;;	Contents:	Startup code to enable the rest of the system to be 
;;;;				loaded.
;;;;	History:	11/9/96  RGC  Created.
;;;;				1/7/99   RGC  Incorporated Vassili's DOTIMES corrections.
;;;;				4/27/01  RGC  Added LISP-IMPLEMENTATION-TYPE, LISP-IMPLEMENTATION-VERSION.
;;;;

;; set up defun macro
(set-symbol-macro 
	#'(lambda (x env)
		(declare (ignore env))
		`(progn
			(set-symbol-function 
				(function (lambda ,(car (cdr (cdr x)))
						(block ,(car (cdr x)) ,@(cdr (cdr (cdr x))))))
				',(car (cdr x)))
			',(car (cdr x))))
	'defun)

;;;
;;; Internal function SIGNAL-UNDEFINED-FUNCTION.
;;; This is redefined later after conditions are loaded.
;;;
(defun signal-undefined-function (func-name)
    (error "The function ~A is not defined" func-name))

;;;
;;; Internal function SIGNAL-PROGRAM-ERROR.
;;; This is redefined later after conditions are loaded.
;;;
(defun signal-program-error (format &rest args)
    (apply 'error format args))

;;;
;;; Internal function SIGNAL-TYPE-ERROR.
;;; This is redefined later after conditions are loaded.
;;;
(defun signal-type-error (object expected-type)
    (error "Type error: datum = ~A, expected type = ~A" object expected-type))

(defun %undefined-function(func-name)
	#'(lambda (&rest x)
		(signal-undefined-function func-name)))

(defun cons (x y) (cons x y))	;; inlined
(defun car (x) (car x))			;; inlined
(defun cdr (x) (cdr x))			;; inlined

(defun caar (x) (car (car x)))		
(defun cadr (x) (car (cdr x)))		
(defun cdar (x) (cdr (car x)))		
(defun cddr (x) (cdr (cdr x)))		

(defun caaar (x) (car (car (car x))))		
(defun caadr (x) (car (car (cdr x))))		
(defun cadar (x) (car (cdr (car x))))		
(defun caddr (x) (car (cdr (cdr x))))		
(defun cdaar (x) (cdr (car (car x))))		
(defun cdadr (x) (cdr (car (cdr x))))		
(defun cddar (x) (cdr (cdr (car x))))		
(defun cdddr (x) (cdr (cdr (cdr x))))		

(defun caaaar (x) (car (car (car (car x)))))		
(defun caaadr (x) (car (car (car (cdr x)))))		
(defun caadar (x) (car (car (cdr (car x)))))		
(defun caaddr (x) (car (car (cdr (cdr x)))))		
(defun cadaar (x) (car (cdr (car (car x)))))		
(defun cadadr (x) (car (cdr (car (cdr x)))))		
(defun caddar (x) (car (cdr (cdr (car x)))))		
(defun cadddr (x) (car (cdr (cdr (cdr x)))))		
(defun cdaaar (x) (cdr (car (car (car x)))))		
(defun cdaadr (x) (cdr (car (car (cdr x)))))		
(defun cdadar (x) (cdr (car (cdr (car x)))))		
(defun cdaddr (x) (cdr (car (cdr (cdr x)))))		
(defun cddaar (x) (cdr (cdr (car (car x)))))		
(defun cddadr (x) (cdr (cdr (car (cdr x)))))		
(defun cdddar (x) (cdr (cdr (cdr (car x)))))		
(defun cddddr (x) (cdr (cdr (cdr (cdr x)))))		

(defun rest (x)	(cdr x))		

(defun macroexpand (x &optional env)
	(tagbody loop
		(if (consp x)
			(if (symbolp (car x))
				(if (macro-function (car x))
					(progn 
						(setq x (funcall (macro-function (car x)) x env))
						(go loop))))))
	x)

(defun macroexpand-1 (x &optional env)
	(if (consp x)
		(if (symbolp (car x))
			(if (macro-function (car x))
				(setq x (funcall (macro-function (car x)) x env)))))
	x)

(defun length (x) 
	(if (vectorp x)
		(array-dimension x 0)
		(let ((length 0)) 
			(tagbody loop 
				(if (null x) (return-from length length)) 
				(setq x (cdr x))
				(setq length (+ 1 length))
				(go loop)))))

(defun nth (num list)
	(if (< num 0) (return-from nth nil))
	(tagbody loop
		(if (<= num 0) 
			(return-from nth (car list))
			(setq list (cdr list)))
		(setq num (- num 1))
		(go loop)))

(defun first (x)	(car x))		
(defun second (x)	(cadr x))		
(defun third (x)	(caddr x))		
(defun fourth (x)	(cadddr x))		
(defun fifth (x)	(car (cddddr x)))		
(defun sixth (x)	(nth 5 x))		
(defun seventh (x)	(nth 6 x))		
(defun eighth (x)	(nth 7 x))		
(defun ninth (x)	(nth 8 x))		
(defun tenth (x)	(nth 9 x))		

(defun nthcdr (num list)
	(if (< num 0) (return-from nthcdr nil))
	(tagbody loop
		(if (<= num 0) 
			(return-from nthcdr list)
			(setq list (cdr list)))
		(setq num (- num 1))
		(go loop)))

(defun not (x) (eq x nil))
(defun 1+ (x) (+ x 1))
(defun 1- (x) (- x 1))
(defun atom (x) (not (consp x)))
(defun endp (x) (null x))
		
;; set up defmacro macro
(set-symbol-macro 
	#'(lambda (x env)
		(declare (ignore env))
		(let ((name (cadr x))
			  (lambda-list (caddr x))
			  (forms (cdddr x))
			  (bindings nil)
			  (argnum 0)
			  var
			  (state 'required-args))

			(block prepare-bindings
				(tagbody loop
					(setq var (car lambda-list))
					(if (eq var '&whole)
						(progn
							(setq state 'body-args)
							(setq lambda-list (cdr lambda-list))
							(setq var (car lambda-list))))
					(if (eq var '&rest)
						(progn
							(setq state 'rest-args)
							(setq lambda-list (cdr lambda-list))
							(setq var (car lambda-list))))
					(if (eq var '&optional)
						(progn
							(setq state 'optional-args)
							(setq lambda-list (cdr lambda-list))
							(setq var (car lambda-list))))
					(if (null lambda-list)
						(return-from prepare-bindings nil))
					(if (eq state 'required-args)
						(setq bindings 
							(cons `(,var (nth ,(+ 1 argnum) %x)) bindings)))
					(if (eq state 'optional-args)
						(let (init)
							(if (consp var)
								(progn 
									(setq init (cadr var))
									(setq var (car var))))		
							(setq bindings 
								(cons 
									`(,var 
										(if (> (length %x) ,(+ 1 argnum))
											(nth ,(+ 1 argnum) %x)
											,init)) 
									bindings))))
					(if (eq state 'rest-args)
						(setq bindings 
							(cons `(,var (nthcdr ,(+ 1 argnum) %x)) bindings)))
					(if (eq state 'body-args)
						(setq bindings 
							(cons `(,var %x) bindings)))
					(setq argnum (+ argnum 1))
					(setq lambda-list (cdr lambda-list))
					(go loop)))

			`(progn
				(set-symbol-macro 
					(function (lambda (%x %env) (block ,name (let ,bindings ,@forms))))
					',name)
				',name)))
	'defmacro)

(defmacro return (&rest x) `(return-from nil ,(car x)))

(defun reverse (x) 
	(let ((r nil)) 
		(tagbody loop 
			(if (null x) 
				(return-from reverse r)
				(setq r (cons (car x) r)))
			(setq x (cdr x))
			(go loop))))

#|(defmacro prog (&whole body)
	(let ((lambda-list (cadr body))
		  (forms (cddr body)))
		`(let ,lambda-list
			(block nil
				(tagbody ,@forms)))))
|#

(defmacro prog (&whole body)
	(let ((lambda-list (cadr body))
		  (forms (cddr body))
		  (declarations nil))
	
		;; collect declarations
		(let (form)
			(tagbody loop
				(if (not (consp forms)) (go exit))
				(setq form (car forms))
				(if (not (consp form)) (go exit))
				(if (not (eq (car form) 'declare)) (go exit))
				(setq declarations (cons form declarations))
				(setq forms (cdr forms))
				(go loop)
				exit))

		`(let ,lambda-list
			,@declarations
			(block nil
				(tagbody ,@forms)))))

(defmacro prog* (&whole body)
	(let ((lambda-list (cadr body))
		  (forms (cddr body))
		  (declarations nil))

		;; collect declarations
		(let (form)
			(tagbody loop
				(if (not (consp forms)) (go exit))
				(setq form (car forms))
				(if (not (consp form)) (go exit))
				(if (not (eq (car form) 'declare)) (go exit))
				(setq declarations (cons form declarations))
				(setq forms (cdr forms))
				(go loop)
				exit))

		`(let* ,lambda-list
			,@declarations
			(block nil
				(tagbody ,@forms)))))

(defmacro push (x var) `(setq ,var (cons ,x ,var)))
(defun nreverse (x) (reverse x))

(defmacro psetq (&rest forms)
	(if (= (length forms) 2)
		(return-from  psetq `(progn (setq ,@forms) nil)))
	(let ((val-forms nil)
		  (var-forms nil))
		(prog* ((x forms) var val)
			  loop
			  (if (null x)(return))
			  (setq var (car x))
			  (if (not (consp (cdr x)))
				(signal-program-error "Variable without value in PSETQ form"))
			  (setq val (cadr x))
			(let ((gs (gensym)))
				(push `(,gs ,val) val-forms)
				(push `(setq ,var ,gs) var-forms))
			(setq x (cddr x))
			(go loop))
		`(let ,(nreverse val-forms)
			,@(nreverse var-forms)
			nil))) 

(defmacro do* (varlist return-clause &whole body)
	(let ((local-vars nil)
		  (inc-expressions nil)
		  (label (gensym))
		  (forms (cdddr body))
		  (declarations nil))

		;; collect declarations
		(prog (form)
			loop
			(if (not (consp forms)) (return))
			(setq form (car forms))
			(if (not (consp form)) (return))
			(if (not (eq (car form) 'declare)) (return))
			(setq declarations (cons form declarations))
			(setq forms (cdr forms))
			(go loop))

		;; collect variable and increment expressions
		(prog* ((v varlist) sym)
			loop-label
			(if (null v) (return))
			(setq sym (car v))
			(if (consp sym)
                (progn
                    (if (not (symbolp (car sym)))
                    		(signal-program-error 
                    		 "Improper 'do*' variable - not a symbol: ~A" (car sym)))
    				(if (consp (cdr sym))
    						(progn
    							(push (list (car sym) (cadr sym)) local-vars)
    							(if (consp (cddr sym))
    								(progn
    									(push (car sym) inc-expressions)
    									(push (caddr sym) inc-expressions))))
    					(push (car sym) local-vars)))
				(if (not (symbolp sym))
					(signal-program-error "Improper 'do*' expression--should be a symbol: ~A" sym)
					(push sym local-vars)))
			(setq v (cdr v))
			(go loop-label))

		(setq local-vars (nreverse local-vars))
		(setq inc-expressions `(setq ,@(nreverse inc-expressions)))
		(if (not (consp return-clause))
			(signal-program-error "Invalid return clause in 'do*' expression: ~A" 
				return-clause))
		(setq return-clause 
			`(if ,(car return-clause) (return (progn ,@(cdr return-clause)))))

		`(prog* ,local-vars
			   ,@declarations
			   ,label
			   ,return-clause
			   ,@forms
			   ,inc-expressions
			   (go ,label))))

(defmacro do (varlist return-clause &whole body)
	(let ((local-vars nil)
		  (inc-expressions nil)
		  (label (gensym))
		  (forms (cdddr body))
		  (declarations nil))

		;; collect declarations
		(prog (form)
			loop
			(if (not (consp forms)) (return))
			(setq form (car forms))
			(if (not (consp form)) (return))
			(if (not (eq (car form) 'declare)) (return))
			(setq declarations (cons form declarations))
			(setq forms (cdr forms))
			(go loop))

		;; collect variable and increment expressions
		(prog* ((v varlist) sym)
			loop-label
			(if (null v) (return))
			(setq sym (car v))
			(if (consp sym)
                (progn
	               (if (not (symbolp (car sym)))
                        (signal-program-error "Improper 'do' variable - not a symbol: ~A" (car sym)))
                   (if (consp (cdr sym))
                        (progn
        					(push (list (car sym) (cadr sym)) local-vars)
        					(if (consp (cddr sym))
        						(progn
        							(push (car sym) inc-expressions)
        							(push (caddr sym) inc-expressions))))
        					(push (car sym) local-vars)))
				(if (not (symbolp sym))
					(signal-program-error "Improper 'do' expression--should be a symbol: ~A" sym)
					(push sym local-vars)))
			(setq v (cdr v))
			(go loop-label))

		(setq local-vars (nreverse local-vars))
		(setq inc-expressions `(psetq ,@(nreverse inc-expressions)))
		(if (not (consp return-clause))
			(signal-program-error "Invalid return clause in 'do' expression: ~A" 
				return-clause))
		(setq return-clause 
			`(if ,(car return-clause) (return (progn ,@(cdr return-clause)))))

		`(prog ,local-vars
			   ,@declarations
			   ,label
			   ,return-clause
			   ,@forms
			   ,inc-expressions
			   (go ,label))))

(defmacro dotimes (&whole body)
  (let* ((varform (cadr body))
         (var (car varform))
	     (num (cadr varform))
	     (result (caddr varform))
	     (vartype 'integer)
         (numtype-expr)
	     (countsym (gensym))
	     (forms (cddr body))
	     (var-inc-expr `(+ ,var 1)))

        (if (fixnump num)
            (progn
                (setq vartype 'fixnum)
                (setq numtype-expr `((declare (type fixnum ,countsym))))
	            (setq var-inc-expr `(the fixnum ,var-inc-expr)))
                (if (symbolp num)
                	(if (constantp num)
                	  (if (fixnump (symbol-value num))
                	    (progn 
                	      (setq vartype 'fixnum)
                	      (setq var-inc-expr `(the fixnum ,var-inc-expr)))))))
        `(let ((,countsym ,num))
            ,@numtype-expr
            (do ((,var 0 ,var-inc-expr))
                ((>= ,var ,countsym) ,result)
                (declare (type ,vartype ,var))
                ,@forms))))

(defmacro dolist (&whole body)
	(let* ((varform (cadr body))
		   (var (car varform))
		   (list (cadr varform))
		   (result (caddr varform))
		   (forms (cddr body))
		   (sym (gensym)))
		`(do* ((,sym ,list (cdr ,sym))
			   (,var (car ,sym) (car ,sym)))
			((null ,sym) ,result)
			(declare (type list ,sym))
			,@forms)))

(defmacro time (x)
	`(let ((tm (get-internal-run-time)) 
		   (gtm (get-gc-time))
		   (*print-escape* nil)
		   ret)
		(declare (special *print-escape*))
		(setq ret ,x)
		(setq tm (- (get-internal-run-time) tm))
		(setq gtm (- (get-gc-time) gtm))
;		(setq tm (/ (float tm) 1000000.0))
		(terpri)
		(write "Total Execution time: ")
		(write (truncate tm internal-time-units-per-second))
		(write " seconds, ")
		(write (mod tm internal-time-units-per-second))
		(write " milliseconds")
		(terpri)
		(write "Time spent garbage collecting: ")
		(write (truncate gtm internal-time-units-per-second))
		(write " seconds, ")
		(write (mod gtm internal-time-units-per-second))
		(write " milliseconds")
		(terpri)
;		(format *trace-output* "Execution time: ~A seconds~%" tm)
		ret))		

(setq *symbol-constant-flag* 1)
(setq *symbol-special-flag*  2)

(defun symbol-set-special-flag (sym)
	(%symbol-set-flags 
		(bit-or (%symbol-get-flags sym) (symbol-value '*symbol-special-flag*))
		sym))

(defun symbol-set-constant-flag (sym)
	(%symbol-set-flags  
		(bit-or (%symbol-get-flags sym) (symbol-value '*symbol-constant-flag*))
		sym))

(defmacro defvar (sym val) 
	`(progn 
		(setq ,sym ,val)
		(symbol-set-special-flag ',sym)
		',sym))

(defmacro defparameter (sym val) 
	`(progn 
		(setq ,sym ,val)
		(symbol-set-special-flag ',sym)
		',sym))

(defmacro defconstant (sym val) 
	`(progn 
		(setq ,sym ,val)
		(symbol-set-special-flag ',sym)
		(symbol-set-constant-flag ',sym)
		',sym))

(defconstant *symbol-constant-flag* 1)
(defconstant *symbol-special-flag*  2)

; Some Common Lisp special variables
(defvar *features* '(cormanlisp))	;reinitialized later to keyword
(defvar *modules* nil)
(defvar *read-suppress* nil)
(defvar *top-level* nil)
(defvar *debug* nil)
(defvar *evalhook* nil)
(defvar *applyhook* nil)
(defvar *read-base* 10)
(defvar *print-base* 10)
(defvar *read-level* 0)
(defvar *read-eval* t)
(defvar *package* (symbol-value '*package*))
(defvar *compiler-warn-on-undefined-function* t)
(defvar *compiler-warn-on-unused-variable* nil)
(defvar *compiler-function-name* nil)
(defvar *terminal-io* (symbol-value '*terminal-io*))
(defvar *standard-input* (symbol-value '*standard-input*))
(defvar *standard-output* (symbol-value '*standard-output*))
(defvar *error-output* *standard-output*)
(defvar *trace-output* *terminal-io*)
(defvar *code-jump-table-refs* nil)
(defvar *code-env-table-refs* nil)
(defvar *code-var-table-refs* nil)
(defvar *compile-verbose* nil)
(defvar *compile-print* nil)
(defvar *loading-kernel* t)
(defvar *read-default-float-format* 'single-float)
(defvar *append-refs-to-code* t)
(defvar *compiler-environment* nil)
(defvar *undefined-functions* nil)
(defvar *lexical-macros* nil)
(defvar *lexical-symbol-macros* nil)
(defvar *macroexpand-hook* 'funcall)
(defvar *collected-literals* nil)
(defvar *in-backquote-form* nil)
(defvar *package-list* (symbol-value '*package-list*))
(defvar *readtable* (symbol-value '*readtable*))
(defvar *system-internals* (symbol-value '*system-internals*))
(defvar compiler_cleanups (symbol-value 'compiler_cleanups))
(defvar *compiler-lambda-list* (symbol-value '*compiler-lambda-list*))
(defvar internal-time-units-per-second (symbol-value 'internal-time-units-per-second))
(defvar gc-time-units-per-second (symbol-value 'gc-time-units-per-second))
(defvar *keyword-package*)
(defvar *gc-time-counter* (symbol-value '*gc-time-counter*))
(defvar *compiled-specials* nil)    ;; used by the compiler and tail call optimization

;;; Cond  --  Public
;;;
;;;    COND also turns into IF.
;;;
;; define a separate expander function because our compiler
;; will not allow macros to be recursive (yet)						
(defun %cond-expand ())		;; avoid warning
(defmacro cond (&rest clauses)
	(%cond-expand clauses))

(defun %cond-expand (clauses)
	(if (endp clauses)
		nil
		(let ((clause (first clauses)))
			(if (atom clause)
				(signal-program-error "Cond clause is not a list: ~S." clause))
			(let ((test (first clause))
				  (forms (rest clause)))
				(if (endp forms)
					(let ((n-result (gensym)))
						`(let ((,n-result ,test))
							(if ,n-result
								,n-result
								(cond ,@(rest clauses)))))
					`(if ,test
						(progn ,@forms)
						(cond ,@(rest clauses))))))))
			

(defmacro when (test &rest forms)
;  "First arg is a predicate.  If it is non-null, the rest of the forms are
;  evaluated as a PROGN."
  `(cond (,test nil ,@forms)))

(defmacro unless (test &rest forms)
;  "First arg is a predicate.  If it is null, the rest of the forms are
;  evaluated as a PROGN."
  `(cond ((not ,test) nil ,@forms)))

;;; And, Or  --  Public
;;;
;;;    AND and OR are defined in terms of IF.
;;;
(defun %and-expand ())		;; avoid warning
(defmacro and (&rest forms)
	(%and-expand forms))

(defun %and-expand (forms)
  (cond ((endp forms) t)
	((endp (rest forms)) (first forms))
	(t
	 `(if ,(first forms)
	      (and ,@(rest forms))
	      nil))))

;;;
(defun %or-expand ())		;; avoid warning
(defmacro or (&rest forms)
	(%or-expand forms))

(defun %or-expand (forms)
  (cond ((endp forms) nil)
	((endp (rest forms)) (first forms))
	(t
	 (let ((n-result (gensym)))
	   `(let ((,n-result ,(first forms)))
	      (if ,n-result
		  ,n-result
		  (or ,@(rest forms))))))))

(defmacro multiple-value-list (form)
	`(multiple-value-call #'list ,form))

(defun list-length (x)
	(do ((n 0 (+ n 2))			; counter
		 (fast x (cddr fast))	; Fast pointer: leaps by 2
		 (slow x (cdr slow)))	; Slow pointer: leaps by 1
		(nil)
		; if fast pointer hits the end, return the count
		(when (endp fast) (return n))
		(when (endp (cdr fast)) (return (+ n 1)))
		;; if fast pointer eventually equals slow pointer,
		;; then we must be stuck in a circular list.
		(when (and (eq fast slow) (> n 0)) (return nil))))

(defun compiler-check-args-num   () t)
(defun compiler-check-types      () t)
(defun compiler-fold-constants   () t)
(defun compiler-inline-functions () t)
(defun compiler-check-key-args   () t)
(defun compiler-optimize-tail-recursion () nil)	;; redefine to turn on
(defun remove-tail-recursion (lambda name) lambda)	;; implement this in another file

(defun lisp-implementation-type () "Corman Common Lisp")
(defun lisp-implementation-version () (%cormanlisp-version-string)) ; redefined in 'version.lisp'
(defun lisp-implementation-version-string () (lisp-implementation-version))

(defconstant call-arguments-limit     1024)
(defconstant lambda-parameters-limit   256)
(defconstant multiple-values-limit     256)

