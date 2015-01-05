;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		massage.lisp
;;;;	Contents:	Corman Lisp compiler transformation code.
;;;;	History:	3/29/00  RGC  Created.
;;;;				5/21/01  RGC  Fixed problem with macroexpansion in FLET and LABELS function forms.
;;;;                12/1/02  RGC  Fixed a problem with MACROLET and duplicate macro names.
;;;;

;(in-package :common-lisp)

;;; Redefine this now that we can access symbols in other packages.
;;;
;;; MACROLET and SYMBOL-MACROLET special operators are implemented here.
;;; To Do: if symbol-macro symbol is declared special, signal an error
;;; To Do: LET shadows symbol-macro.
;;;
(defvar cormanlisp::*compiler-optimize-speed* 0)
(defvar cormanlisp::*compiler-optimize-safety* 3)
(defvar cormanlisp::*compiler-optimize-debug* 3)
(defvar cormanlisp::*compiler-optimize-space* 0)
(defvar cormanlisp::*compiler-optimize-compilation-speed* 0)
(defvar *collect-lexical-macros* nil)
(defvar *collect-lexical-symbol-macros* nil)

(defun find-opt-declarations (forms)
	(let ((ret nil))
		(dolist (x forms)
			(if (and (consp x)(eq (car x) 'declare))
				(dolist (y x)
					(if (and (consp y)(eq (car y) 'optimize))
						(push y ret)))))
		(nreverse ret)))

(defun process-opt-declarations (decls)
	(dolist (x decls)
		(dolist (y (cdr x))
			(cond 
				((eq y 'speed)
			     (setf cormanlisp::*compiler-optimize-speed* 3))
				((and (consp y)(eq (car y) 'speed))
				 (setf cormanlisp::*compiler-optimize-speed* (or (cadr y) 3)))
				((eq y 'safety)
			     (setf cormanlisp::*compiler-optimize-safety* 3))
				((and (consp y)(eq (car y) 'safety))
				 (setf cormanlisp::*compiler-optimize-safety* (or (cadr y) 3)))))))

(defun macroexpand-all-except-top (x env)
	(unless (consp x) (return-from macroexpand-all-except-top x))
   ;; now expand macros for each element of the form
	(let ((sym (car x)))
		(cond
			((eq sym 'LET)
			 (let* ((var-list (cadr x))
				   (forms (cddr x))
				   (*lexical-symbol-macros* *lexical-symbol-macros*)
                   (opt-declarations (find-opt-declarations forms)))
                    (if opt-declarations
						(let ((cormanlisp::*compiler-optimize-speed* 
									cormanlisp::*compiler-optimize-speed*)
							  (cormanlisp::*compiler-optimize-safety* 
									cormanlisp::*compiler-optimize-safety*)								
							  (cormanlisp::*compiler-optimize-debug* 
									cormanlisp::*compiler-optimize-debug*)
							  (cormanlisp::*compiler-optimize-space* 
									cormanlisp::*compiler-optimize-space*)
							  (cormanlisp::*compiler-optimize-compilation-speed* 
									cormanlisp::*compiler-optimize-compilation-speed*))
							(process-opt-declarations opt-declarations)								
                            (rplaca (cdr x) (macroexpand-var-list var-list env))
                            (do ()
                                ((null forms))
                                (rplaca forms (macroexpand-all (car forms) env))
                                (setq forms (cdr forms))))
                        (progn
                            (rplaca (cdr x) (macroexpand-var-list var-list env))
                            (do ()
                                ((null forms))
                                (rplaca forms (macroexpand-all (car forms) env))
                                (setq forms (cdr forms)))))))
			((eq sym 'LET*)
			 (let* ((var-list (cadr x))
				   (forms (cddr x))
				   (*lexical-symbol-macros* *lexical-symbol-macros*)
                   (opt-declarations (find-opt-declarations forms)))
                    (if opt-declarations
						(let ((cormanlisp::*compiler-optimize-speed* 
									cormanlisp::*compiler-optimize-speed*)
							  (cormanlisp::*compiler-optimize-safety* 
									cormanlisp::*compiler-optimize-safety*)								
							  (cormanlisp::*compiler-optimize-debug* 
									cormanlisp::*compiler-optimize-debug*)
							  (cormanlisp::*compiler-optimize-space* 
									cormanlisp::*compiler-optimize-space*)
							  (cormanlisp::*compiler-optimize-compilation-speed* 
									cormanlisp::*compiler-optimize-compilation-speed*))
							(process-opt-declarations opt-declarations)								
                            (rplaca (cdr x) (macroexpand-var*-list var-list env))
                            (do ()
                                ((null forms))
                                (rplaca forms (macroexpand-all (car forms) env))
                                (setq forms (cdr forms))))
                        (progn
                            (rplaca (cdr x) (macroexpand-var*-list var-list env))
                            (do ()
                                ((null forms))
                                (rplaca forms (macroexpand-all (car forms) env))
                                (setq forms (cdr forms)))))))
            ((eq sym 'LAMBDA)
				(let* ((lambda-list (cadr x))
					   (forms (cddr x))
					   (*lexical-symbol-macros* *lexical-symbol-macros*)
					   (opt-declarations (find-opt-declarations forms)))
					(if opt-declarations
						(let ((cormanlisp::*compiler-optimize-speed* 
									cormanlisp::*compiler-optimize-speed*)
							  (cormanlisp::*compiler-optimize-safety* 
									cormanlisp::*compiler-optimize-safety*)								
							  (cormanlisp::*compiler-optimize-debug* 
									cormanlisp::*compiler-optimize-debug*)
							  (cormanlisp::*compiler-optimize-space* 
									cormanlisp::*compiler-optimize-space*)
							  (cormanlisp::*compiler-optimize-compilation-speed* 
									cormanlisp::*compiler-optimize-compilation-speed*))
							(process-opt-declarations opt-declarations)									
							(rplaca (cdr x) (macroexpand-lambda-list lambda-list env))
							(do ()
								((null forms))
								(rplaca forms (macroexpand-all (car forms) env))
								(setq forms (cdr forms))))
						(progn
							(rplaca (cdr x) (macroexpand-lambda-list lambda-list env))
							(do ()
								((null forms))
								(rplaca forms (macroexpand-all (car forms) env))
								(setq forms (cdr forms)))))))
			((eq sym 'QUOTE))
			((eq sym 'FUNCTION)
             (setf x (copy-tree-unquoted x))  ;; ensure unique source tree for each instance
			 (let ((func (cadr x)))
				(if (is-lambda-form func)
					(progn
						(push *lexical-macros* *collect-lexical-macros*)
						(push func *collect-lexical-macros*)
						(push *lexical-symbol-macros* *collect-lexical-symbol-macros*)
						(push func *collect-lexical-symbol-macros*)))))  ;; save for later

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
					(do ()
						((null forms))
						(rplaca forms (macroexpand-all (car forms) env))
						(setq forms (cdr forms)))))
			((eq sym 'LABELS)
				(let ((*lexical-macros* *lexical-macros*)
					  (func-list (cadr x)) 
					  (forms      (cddr x)))
					;; lexically defined functions need to shadow macros,
					;; so we add a macro definition with NIL as the function
					;; to disable the macro.
					;; shadow all the macros by adding the new function names
					(dolist (m func-list)
						(push (list (car m) nil) *lexical-macros*))
					;; expand macros in functions
					(do ((func func-list (cdr func)))
						((null func))
						(let ((func-forms (cddar func)))
							(do ((i func-forms (cdr i)))
								((null i))
								(rplaca i (macroexpand-all (car i) env)))))
					;; expand body forms
					(do ()
						((null forms))
						(rplaca forms (macroexpand-all (car forms) env))
						(setq forms (cdr forms)))))
			((eq sym 'FLET)
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
					(do ()
						((null forms))
						(rplaca forms (macroexpand-all (car forms) env))
						(setq forms (cdr forms)))))
			((eq sym 'SYMBOL-MACROLET)
				(let ((*lexical-symbol-macros* *lexical-symbol-macros*)
					  (macro-list (cadr x)) 
					  (forms      (cddr x)))
					(dolist (m macro-list)
						(push (list (car m) (cadr m)) *lexical-symbol-macros*))
					(rplaca x 'progn)
					(rplaca (cdr x) 'nil)
					(do ()
						((null forms))
						(rplaca forms (macroexpand-all (car forms) env))
						(setq forms (cdr forms)))))
			
			((or (eq sym 'BLOCK)(eq sym 'RETURN-FROM))
				(do ((forms (cddr x) (cdr forms)))
					((endp forms))
					(rplaca forms (macroexpand-all (car forms) env))))

			((eq sym 'TAGBODY)
				(do ((forms (cdr x) (cdr forms)))
					((endp forms))
					(unless (or (integerp (car forms))(symbolp (car forms)))
						(rplaca forms (macroexpand-all (car forms) env)))))
			
			((eq sym 'GO))
			((eq sym 'DECLARE))
						
      		;; SETQ on SYMBOL-MACROLET'ted symbols must be SETF, so be careful...
      		((and (eq sym 'SETQ) (%nfixup-setq-lexical-symbol-macros (cdr x) env))
			 (rplaca x 'SETF)
       		 (setq x (macroexpand-all x env)))
      		;; ...and same is true for PSETQ.  -- Vassili 12/15/1998
      		((and (eq sym 'PSETQ) (%nfixup-setq-lexical-symbol-macros (cdr x) env))
       		 (rplaca x 'PSETF)
       		 (setq x (macroexpand-all x env)))
			((eq sym 'SETQ)
				(do ((forms (cddr x) (cddr forms)))
					((endp forms))
					(rplaca forms (macroexpand-all (car forms) env))))

			((is-lambda-form sym)
				(let ((forms (cdr x)))
					(rplaca x (macroexpand-all-except-top sym env))
					(do ()
						((null forms))
						(rplaca forms (macroexpand-all (car forms) env))
						(setq forms (cdr forms)))))
			(t
				(let ((forms (cdr x)))
					(do ()
						((null forms))
						(rplaca forms (macroexpand-all (car forms) env))
						(setq forms (cdr forms)))))))
	x)

(defun eval-constant-expression (x)
	(cond ((symbolp x)(symbol-value x))
		  ((not (consp x)) x)
		  ((eq (car x) 'quote) (cadr x))
		  (t (apply (car x)
						(let ((ret nil))
							(dolist (y (cdr x)(nreverse ret))
								(push (eval-constant-expression y) ret)))))))

(defun keywordp (x) (and (symbolp x) (eq (symbol-package x) (find-package "KEYWORD"))))

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
			(let ((result (eval-constant-expression x)))
				(if (or (consp result) 
						(and (symbolp result) 
							(not (keywordp result))
							(not (eq result t))
							(not (eq result nil))))
					(setf result (list 'quote result)))
				(return-from macroexpand-all result))))
	(macroexpand-all-except-top x env))

(defvar constant-functions '(+ sin car eql cdr + - * / sqrt))
;(defvar constant-functions '())

(defun all-constant (list)
	(dolist (x list t)
		(unless (constantp x)
			(return nil))))

(defun constantp (x &optional environment)
	(declare (ignore environment))
	(cond ((symbolp x) (symbol-constant-p x))
		((consp x) 
		 (or (eq (car x) 'quote)
			(and (member (car x) constant-functions)
				(all-constant (cdr x)))))
		  (t t)))



