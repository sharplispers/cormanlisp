;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		expand.lisp
;;;;	Contents:	Corman Lisp 3.0 macro expansion code to build the
;;;;				system. Hooks into the compiler.
;;;;	History:	7/23/96  RGC  Created.
;;;;				12/15/98 RGC  Integrated code by Vassili Bykov to fix
;;;;							  SYMBOL-MACROLET and SETQ, PSETQ.
;;;;				12/16/98 RGC  Moved ASSOC, EQL function here from misc.lisp
;;;;							  as they are needed earlier during system building.
;;;;				7/26/99  VB   Modified macroexpand-var-list.
;;;;							  Fixes the problem of not expanding a symbol-macro appearing 
;;;;							  in the init-form of a LET variable list, for example if 
;;;;							  FOO in (let ((a foo)) ...) is a symbol-macro.
;;;;				10/19/99 RGC  MACROEXPAND-ALL performs code inlining as well as 
;;;;							  macro expansion.
;;;;				5/21/01  RGC  Fixed problem with macroexpansion in FLET and LABELS function forms.
;;;;

;; need to override warning here
(setq *COMPILER-WARN-ON-UNDEFINED-FUNCTION* nil)

(defun inline-expand (expr &optional env) 
	;(declare (ignore env)) 
	expr)	;; redefined later

(defun compiler-macro-function (name &optional environment) nil)	;; this is redefined later

(defun macroexpand-lambda-list (lambda-list env)
   (do* ((x lambda-list)
         (form (car x)(car x))
		 (sym (if (consp form) (car form) form)))
       ((null x) lambda-list)
	   (push sym *lexical-symbol-macros*)	;shadow any symbol-macro bindings
       (if (and (consp form) (consp (cdr form)))
          (rplaca (cdr form) (macroexpand-all (cadr form) env)))
       (setq x (cdr x))))

;; this handles the LET* case
(defun macroexpand-var*-list (var-list env)
   (do* ((x var-list)
         (form (car x)(car x)))
       ((null x) var-list)
		;; expand initializer form before adding new variable to scope
       (if (and (consp form) (consp (cdr form)))
          (rplaca (cdr form) (macroexpand-all (cadr form) env)))
	   (push (if (consp form) (car form) form) *lexical-symbol-macros*)	;shadow any symbol-macro bindings
       (setq x (cdr x))))

;; this handles the LET case
(defun macroexpand-var-list (var-list env)
	;; first time through, expand all initializer forms
   (do* ((x var-list)
         (form (car x)(car x)))
       ((null x))
		;; expand initializer form before adding symbol macro to scope
       (if (and (consp form) (consp (cdr form)))
          (rplaca (cdr form) (macroexpand-all (cadr form) env)))
       (setq x (cdr x)))
	;; second time through, add new variable to scope
   (do* ((x var-list)
         (form (car x)(car x)))
       ((null x) var-list)
	   (push (if (consp form) (car form) form) *lexical-symbol-macros*)	;shadow any symbol-macro bindings
       (setq x (cdr x))))

(defun is-lambda-form (x)
	(and (consp x) (eq (car x) 'LAMBDA)))

;;;
;;;	Common Lisp EQL function.
;;;
(defun eql (x y) 
	(or (eq x y) 
		(and (numberp x) 
			 (numberp y) 
			 (= x y) 
			 (number-types-eq x y))))

;;;
;;;	Common Lisp ASSOC function.
;;;
(defun assoc (item alist &key key (test #'eql) test-not)
	(if test-not
		(let ((save-test test))
			(setq test #'(lambda (x y) (not (funcall save-test x y))))))
	(dolist (a alist)
		(if (and (consp a) 
				 (funcall test item 
					(if key (funcall key (car a)) (car a))))
			(return a))))

;;;
;;; MACROLET and SYMBOL-MACROLET special operators are implemented here.
;;; To Do: if symbol-macro symbol is declared special, signal an error
;;; To Do: LET shadows symbol-macro.
;;; [VB] To Do: take care of MULTIPLE-VALUE-SETQ
;;;

(defun %nfixup-setq-lexical-symbol-macros (pairs env)
  ;; [VB 12/15/1998] Mutate PAIRS to replace symbol-macro places with their
  ;; expansions and return a boolean to indicate whether expansions have
  ;; actually occurred.
  (do* ((p pairs (cddr p))
	(var (car p) (car p))
	(fixedp nil))
       ((null p) fixedp)
;    (when (and (symbolp var) (assoc var *lexical-symbol-macros* :test #'eq)) ;)
;    (when (and (symbolp var) (assoc var *lexical-symbol-macros*)) ;)
    (when (and (symbolp var) (get-symbol-macro-expansion var))
      (rplaca p (macroexpand-1 var env))
      (setq fixedp t))))


;;;
;;; MACROLET and SYMBOL-MACROLET special operators are implemented here.
;;; To Do: if symbol-macro symbol is declared special, signal an error
;;; To Do: LET shadows symbol-macro.
;;;
(defun macroexpand-all-except-top (x env)

	(unless (consp x) (return-from macroexpand-all-except-top x))
   
   ;; now expand macros for each element of the form
	(let ((sym (car x)))
		(cond
			((eq sym 'LET)
			 (let ((var-list (cadr x))
				   (forms (cddr x))
				   (*lexical-symbol-macros* *lexical-symbol-macros*))
					(rplaca (cdr x) (macroexpand-var-list var-list env))
					(do ((form (car forms)(car forms)))
						((null forms))
						(rplaca forms (macroexpand-all (car forms) env))
						(setq forms (cdr forms)))))
			((eq sym 'LET*)
			 (let ((var-list (cadr x))
				   (forms (cddr x))
				   (*lexical-symbol-macros* *lexical-symbol-macros*))
					(rplaca (cdr x) (macroexpand-var*-list var-list env))
					(do ((form (car forms)(car forms)))
						((null forms))
						(rplaca forms (macroexpand-all (car forms) env))
						(setq forms (cdr forms)))))
			((eq sym 'LAMBDA)
				(let* ((lambda-list (cadr x))
					   (forms (cddr x))
					   (*lexical-symbol-macros* *lexical-symbol-macros*))
					(rplaca (cdr x) (macroexpand-lambda-list lambda-list env))
					(do ((form (car forms)(car forms)))
						((null forms))
						(rplaca forms (macroexpand-all (car forms) env))
						(setq forms (cdr forms)))))
			((eq sym 'QUOTE))
			((eq sym 'FUNCTION))
			((eq sym 'MACROLET)
				(let ((temp-macro-sym (gensym))
					  (*lexical-macros* *lexical-macros*)
					  (macro-list (cadr x)) 
					  (forms      (cddr x)))
					(dolist (m macro-list)
						(eval `(defmacro ,temp-macro-sym ,(cadr m) ,@(cddr m)))
						(push (list (car m) (symbol-function temp-macro-sym)) *lexical-macros*))
					(rplaca x 'let)
					(rplaca (cdr x) 'nil)
					(do ((form (car forms)(car forms)))
						((null forms))
						(rplaca forms (macroexpand-all (car forms) env))
						(setq forms (cdr forms)))))
			((or (eq sym 'FLET)(eq sym 'LABELS))
				(let ((*lexical-macros* *lexical-macros*)
					  (func-list (cadr x)) 
					  (forms      (cddr x)))
					;; lexically defined functions need to shadow macros,
					;; so we add a macro definition with NIL as the function
					;; to disable the macro.
					(dolist (m func-list)
						(push (list (car m) nil) *lexical-macros*))
					;; expand macros in functions
					(do ((func func-list (cdr func)))
						((null func))
						(let ((func-forms (cddar func)))
							(do ((i func-forms (cdr i)))
								((null i))
								(rplaca i (macroexpand-all (car i) env)))))
					(do ((form (car forms)(car forms)))
						((null forms))
						(rplaca forms (macroexpand-all (car forms) env))
						(setq forms (cdr forms)))))
			((eq sym 'SYMBOL-MACROLET)
				(let ((temp-macro-sym (gensym))
					  (*lexical-symbol-macros* *lexical-symbol-macros*)
					  (macro-list (cadr x)) 
					  (forms      (cddr x)))
					(dolist (m macro-list)
						(push (list (car m) (cadr m)) *lexical-symbol-macros*))
					(rplaca x 'progn)
					(rplaca (cdr x) 'nil)
					(do ((form (car forms)(car forms)))
						((null forms))
						(rplaca forms (macroexpand-all (car forms) env))
						(setq forms (cdr forms)))))
						
      		;; SETQ on SYMBOL-MACROLET'ted symbols must be SETF, so be careful...
      		((and (eq sym 'SETQ) (%nfixup-setq-lexical-symbol-macros (cdr x) env))
			 (rplaca x 'SETF)
       		 (setq x (macroexpand-all x env)))
      		;; ...and same is true for PSETQ.  -- Vassili 12/15/1998
      		((and (eq sym 'PSETQ) (%nfixup-setq-lexical-symbol-macros (cdr x) env))
       		 (rplaca x 'PSETF)
       		 (setq x (macroexpand-all x env)))
			((is-lambda-form sym)
				(let ((forms (cdr x)))
					(rplaca x (macroexpand-all-except-top sym env))
					(do ((form (car forms)(car forms)))
						((null forms))
						(rplaca forms (macroexpand-all (car forms) env))
						(setq forms (cdr forms)))))
			(t
				(let ((forms x))
					(do ((form (car forms)(car forms)))
						((null forms))
						(rplaca forms (macroexpand-all (car forms) env))
						(setq forms (cdr forms)))))))
	x)

;;;
;;;	Expands compiler macros.
;;;
(defun expand-compiler-macros (form &optional env)
	(do ((f form))
		((not (consp f)) f)
		(let ((compiler-macro-func (compiler-macro-function (car f)))
			  (save f))
			(if compiler-macro-func
				;; if there is a lexical function (from FLET or LABELS)
				;; it shadows the compiler macro of the same name.
				(let ((lex-func (member (car f) *lexical-macros*)))
					;; if there is a lexical function, the CADR will be NIL
					(if (or (null lex-func) (cadr lex-func))
						(setq f (funcall *macroexpand-hook* compiler-macro-func f env)))))
			(if (eq f save)
				(return f)))))
	
;; this function now performs code inlining as well
;;
(defun macroexpand-all (x &optional env)
	(if (and (consp x)(eq (car x) 'quote))
		(return-from macroexpand-all x))
	
;	(if (constantp x)
;		(if (symbolp x)
;			(return-from macroexpand-all (list 'quote (symbol-value x)))
;			(if (consp x)
;				(return-from macroexpand-all (list 'quote (apply (car x) (cdr x)))))))
	
	;; keep doing compiler macros, macros and inline expansion
	;; until we go one time through the loop and nothing changes
	(do ((save x x))
		(nil)
		(setq x (expand-compiler-macros x))
		(setq x (macroexpand x env))   ;; expand top level form	
		(setq x (inline-expand x env))
		(if (eq save x)
			(return)))
	(macroexpand-all-except-top x env))

(defun get-macro-definition (sym)
	(dolist (x *lexical-macros*)
		(if (eq (car x) sym) (return-from get-macro-definition (cadr x))))
	(macro-function sym))

;;; returns either the list containing (symbol expansion), or NIL
(defun get-symbol-macro-expansion (sym)
	(do* ((x *lexical-symbol-macros* (cdr x))
		  (form (car x) (car x)))
		((null x) nil)
		(if (eq form sym)
			(return nil)) 
		(if (and (consp form)(eq (car form) sym)) 
			(return form))))

(defun symbol-macro-expansion-func (sym &optional env)
	;(declare (ignore env))
	(let ((form (get-symbol-macro-expansion sym)))
		(if form (cadr form) sym)))

;;;
;;;	Common Lisp MACROEXPAND-1 function.
;;;
(defun macroexpand-1 (x &optional env)
	(if
		(and
			(or (not (consp x))
			 	(not (symbolp (car x)))
			 	(not (get-macro-definition (car x))))
			(or (not (symbolp x))
				(not (get-symbol-macro-expansion x))))
		(values x nil)
		(values
			(if (consp x) 
				(funcall *macroexpand-hook* (get-macro-definition (car x)) x env)
				(funcall *macroexpand-hook* 'symbol-macro-expansion-func x env))
			t)))
	
(defun macroexpand (form &optional env)
	(do* ((x form))
		((and
			 (or (not (consp x))
			 	(not (symbolp (car x)))
			 	(not (get-macro-definition (car x))))
			 (or (not (symbolp x))
				 (not (get-symbol-macro-expansion x))))
			(values x (not (eq x form))))
		(setq x
			(if (consp x)
				(funcall *macroexpand-hook* (get-macro-definition (car x)) x env)
				(funcall *macroexpand-hook* 'symbol-macro-expansion-func x env)))))

(setq *COMPILER-WARN-ON-UNDEFINED-FUNCTION* t)

(defun is-heap-literal (x) 
	(cond 
		((symbolp x) nil)
		((fixnump x) nil)
		((characterp x) nil)
		((listp x) (if (eq (car x) 'quote) (not (symbolp (cadr x))) nil))
		(t t)))

(defun extract-literals (x)
	(if (is-heap-literal x)
		(let ((sym (gensym)))
			(push (list sym x) *collected-literals*)
			(if *in-backquote-form*
				(setq x (list 'quote (list '%comma sym)))
				(setq x sym)))
		(if (consp x)
			(if (eq (car x) 'backquote)
				(let ((*in-backquote-form* nil))
					(do ((f x (cdr f)))
						((not (consp (cdr f)))
						 (rplaca f (extract-literals (car f)))
						 (if (cdr f) (rplacd f (extract-literals (cdr f)))))
						(rplaca f (extract-literals (car f)))))
					
				(do ((f x (cdr f)))
					((not (consp (cdr f)))
					 (rplaca f (extract-literals (car f)))
					 (if (cdr f) (rplacd f (extract-literals (cdr f)))))
					(rplaca f (extract-literals (car f)))))))
	x)

#|
(defun collect-literals (x)
	(let ((*collected-literals* nil))
		(setq x (extract-literals x))
		(if (null *collected-literals*)
			x
			`(let (,@*collected-literals*)
				,x))))
|#
(defun collect-literals (x) x)		;; disable for now

(setq macroexpand-inline nil)


             