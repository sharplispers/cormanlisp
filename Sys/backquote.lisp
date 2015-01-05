;;;
;;;		File:		backquote.lisp
;;;		Contents:	Corman Lisp backquote reader macros
;;;		History:	6/19/01  RGC Incorporated Gilbert Perfetti's fix
;;;							     to handle dotted pairs in backquotes.
;;;
;;;		Redefine the backquote facility here to handle
;;;		nested backquotes correctly.
;;;
;;;		Code from Appendix C of Guy Steele's Common Lisp, the Language,
;;;		second edition, pp. 960-967
;;;

(provide :backquote)

;; this gets executed before the macro version of in-package is 
;; defined
(eval-when (:compile-toplevel :load-toplevel :execute)
	(in-package :common-lisp))

;; need to override warning here
(setq *COMPILER-WARN-ON-UNDEFINED-FUNCTION* nil)
 
(defvar *comma* '%COMMA)
(defvar *comma-atsign* '%COMMA-ATSIGN)
(defvar *comma-dot* '%COMMA-DOT)
(defvar *bq-list* '%BQ-LIST)
(defvar *bq-append* '%BQ-APPEND)
(defvar *bq-list** '%BQ-LIST*)
(defvar *bq-nconc* '%BQ-NCONC)
(defvar *bq-clobberable* '%BQ-CLOBBERABLE)
(defvar *bq-quote* '%BQ-QUOTE)
(defvar *bq-quote-nil* (list *bq-quote* nil))
#|
	;; caused these symbols to be interned to make reloading cl.lisp
	;; work better
(defvar *comma* (make-symbol "COMMA"))
(defvar *comma-atsign* (make-symbol "COMMA-ATSIGN"))
(defvar *comma-dot* (make-symbol "COMMA-DOT"))
(defvar *bq-list* (make-symbol "BQ-LIST"))
(defvar *bq-append* (make-symbol "BQ-APPEND"))
(defvar *bq-list** (make-symbol "BQ-LIST*"))
(defvar *bq-nconc* (make-symbol "BQ-NCONC"))
(defvar *bq-clobberable* (make-symbol "BQ-CLOBBERABLE"))
(defvar *bq-quote* (make-symbol "BQ-QUOTE"))
(defvar *bq-quote-nil* (list *bq-quote* nil))
|#
(set-macro-character #\`
	#'(lambda (stream char)
		(declare (ignore char))
		(list 'backquote (read stream t nil t))))
		
(set-macro-character #\,
	#'(lambda (stream char)
		(declare (ignore char))
			(case (peek-char nil stream t nil t)
				(#\@ (read-char stream t nil t)
					(list *comma-atsign* (read stream t nil t)))
				(#\. (read-char stream t nil t)
					(list *comma-dot* (read stream t nil t)))
				(otherwise (list *comma* (read stream t nil t))))))

(defparameter *bq-simplify* t)

(defmacro backquote (x)
	(bq-completely-process x))
				
(defun bq-completely-process (x)
	(let ((raw-result (bq-process x)))
		(bq-remove-tokens (if *bq-simplify*
							(bq-simplify raw-result)
							raw-result))))

(defun bq-process (x)
	(cond ((atom x)
			(list *bq-quote* x))
		  ((eq (car x) 'backquote)
		  	(bq-process (bq-completely-process (cadr x))))
		  ((eq (car x) *comma*) (cadr x))
		  ((eq (car x) *comma-atsign*)
		  	(error ",@~S after `" (cadr x)))
		  ((eq (car x) *comma-dot*)
		  	(error ",.~S after `" (cadr x)))
		  (t (do ((p x (cdr p))
		  		  (q '() (cons (bracket (car p)) q)))
				 ((atom p)
				  (cons *bq-append* 
				  		(nreconc q (list (list *bq-quote* p)))))
				(when (eq (car p) *comma*)
					(unless (null (cddr p)) (error "Malformed ,~S" p))
					(return (cons *bq-append*
						(nreconc q (list (cadr p))))))
				(when (eq (car p) *comma-atsign*)
					(error "Dotted ,@~S" p))
				(when (eq (car p) *comma-dot*)
					(error "Dotted ,.~S" p))))))
					
(defun bracket (x)
	(cond ((atom x)
			(list *bq-list* (bq-process x)))
		  ((eq (car x) *comma*)
		  	(list *bq-list* (cadr x)))
		  ((eq (car x) *comma-atsign*)
		  	(cadr x))
		  ((eq (car x) *comma-dot*)
		  	(list *bq-clobberable* (cadr x)))
		  (t (list *bq-list* (bq-process x)))))
		  
(defun maptree (fn x)
	(if (atom x)
		(funcall fn x)
		(let ((a (funcall fn (car x)))
			  (d (maptree fn (cdr x))))
			(if (and (eql a (car x)) (eql d (cdr x)))
				x
				(cons a d)))))

(defun bq-splicing-frob (x)
	(and (consp x)
		(or (eq (car x) *comma-atsign*)
			(eq (car x) *comma-dot*))))

(defun bq-frob (x)
	(and (consp x)
		(or (eq (car x) *comma*)
			(eq (car x) *comma-atsign*)
			(eq (car x) *comma-dot*))))

(defun bq-simplify (x)
	(if (atom x)
		x
		(let ((x (if (eq (car x) *bq-quote*)
					x
					(maptree #'bq-simplify x))))
			(if (not (eq (car x) *bq-append*))
				x
				(bq-simplify-args x)))))

(defun bq-simplify-args (x)
	(do ((args (reverse (cdr x)) (cdr args))
		 (result
			nil
			(cond ((atom (car args))
				   (bq-attach-append *bq-append* (car args) result))
				  ((and (eq (caar args) *bq-list*)
						(notany #'bq-splicing-frob (cdar args)))
				   (bq-attach-conses (cdar args) result))
				  ((and (eq (caar args) *bq-list**)
						(notany #'bq-splicing-frob (cdar args)))
				   (bq-attach-conses
						(reverse (cdr (reverse (cdar args))))
						(bq-attach-append *bq-append*
							(car (last (car args)))
							result)))
				  ((and (eq (caar args) *bq-quote*)
						(consp (cadar args))
						(not (bq-frob (cadar args)))
						(null (cddar args)))
				   (bq-attach-conses (list (list *bq-quote*
												(caadar args)))
									 result))
				  ((eq (caar args) *bq-clobberable*)
				   (bq-attach-append *bq-nconc* (cadar args) result))
				  (t (bq-attach-append *bq-append* (car args) result)))))
		((null args) result)))

(defun null-or-quoted (x)
	(or (null x) (and (consp x) (eq (car x) *bq-quote*))))

(defun bq-attach-append (op item result)
	(cond ((and (null-or-quoted item) (null-or-quoted result))
		   (list* *bq-quote* (cadr item) (cadr result)))
		  ((or (null result) (equal result *bq-quote-nil*))
		   (if (bq-splicing-frob item) (list op item) item))
		  ((and (consp result) (eq (car result) op))
		   (list* (car result) item (cdr result)))
		  (t (list op item result))))

(defun bq-attach-conses (items result)
	(cond
		((and (every #'null-or-quoted items)
			  (null-or-quoted result))
		 (list *bq-quote* (append (mapcar #'cadr items) (cadr result))))
		((or (null result) (equal result *bq-quote-nil*))
		 (cons *bq-list* items))
		((and (consp result)
			  (or (eq (car result) *bq-list*)
				  (eq (car result) *bq-list**)))
		 (cons (car result) (append items (cdr result))))
		(t (cons *bq-list** (append items (list result))))))


(defun bq-remove-tokens (x)
	(cond
		((eq x *bq-list*) 'list)
		((eq x *bq-append*) 'append)
		((eq x *bq-nconc*) 'nconc)
		((eq x *bq-list**) 'list*)
		((eq x *bq-quote*) 'quote)
		((atom x) x)
		((eq (car x) *bq-clobberable*)
		 (bq-remove-tokens (cadr x)))
		((and (eq (car x) *bq-list**)
			(consp (cddr x))
			(null (cdddr x)))
		 (cons 'cons (maptree #'bq-remove-tokens (cdr x))))
		(t (maptree #'bq-remove-tokens x))))

(setq *COMPILER-WARN-ON-UNDEFINED-FUNCTION* t)













