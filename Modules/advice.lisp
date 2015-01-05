;;;; -*-Mode: Lisp; Syntax: Common-Lisp; fill-column: 76; -*-

;;;; advice.lisp -- advise facility.  Allows one to wrap existing
;;;; functions with code that can filter the function arguments or
;;;; return values, perform extra side effects, etc.

;;;; Copyright (C) 1999 Vassili Bykov.

;;;; Author:       Vassili Bykov <vassili@objectpeople.com>
;;;; Created:      12/12/1998
;;;; Last updated: 01/03/1999
;;;;
;;;; History:	   1/06/98  Roger Corman   Exported CALL-ADVISED-FUNCTION, ADVISED-FUNCTION.

(in-package :cormanlisp)

(export '(advise unadvise symbol-function-advised-p call-advised-function advised-function))

(defvar *advised-symbols* nil
  "A list of symbols that are being advised.")

(defun %register-advice (symbol advice)
  (unless (symbolp symbol) (error "Not a symbol: ~S" symbol))
  (let ((original (or (get symbol 'advice-original)
		      (symbol-function symbol))))
    (unless original
      (error "The symbol ~A is unbound as a function" symbol))
    (pushnew symbol *advised-symbols* :test #'eq)
    (setf (get symbol 'advice-original) original)
    (setf (symbol-function symbol)
	  #'(lambda (&rest args)
	      (funcall advice original args))))
  *advised-symbols*)

(defun %unregister-advice (symbol)
  (let ((original (get symbol 'advice-original)))
    (when original
      (setq *advised-symbols* (delete symbol *advised-symbols* :test #'eq))
      (remprop symbol 'advice-original)
      (setf (symbol-function symbol) original)))
  *advised-symbols*)

(defun %unadvise (symbols)
  (dolist (s symbols)
    (unless (symbolp s)
      (error "Not a symbol: ~S" s))
    (%unregister-advice s)))

(defun symbol-function-advised-p (symbol)
  (get symbol 'advice-original))
  
(defmacro advise (&rest whole)
  (if (null whole)
      ;; just want a list of functions
      '*advised-symbols*
      ;; stuff given--install the advisor
      (destructuring-bind (symbol lambda-list &rest body) whole
	`(%register-advice
	  ',symbol
	  #'(lambda (advised-function %all-arguments)
	      (destructuring-bind ,lambda-list %all-arguments
		(macrolet ((call-advised-function ()
			     '(apply advised-function %all-arguments)))
		  ,@body)))))))


(defmacro unadvise (&rest symbols)
  (if symbols
      `(%unadvise ',symbols)
      '(%unadvise *advised-symbols*)))

(provide "ADVICE")

#| EXAMPLE OF USE:

(defun bad-factorial (n)
  (if (zerop n)
      'BOGUS
      (* n (bad-factorial (1- n)))))

;; Following is an example of an advisor.  It is like an :around method in
;; CLOS: it is called instead of the original function. The original
;; function is called as (CALL-ADVISED-FUNCTION) in the advisor (it is a
;; macro; the proper arguments are passed automatically).  The original
;; function itself is also available as the value of the variable
;; ADVISED-FUNCTION.
;;
;; Advisors are mutually exclusive; installing an advisor on a function that
;; is already being advised discards the old advisor.

(advise bad-factorial (n)
  (let ((value (call-advised-function)))
    (cond ((numberp value)
	   value)
	  (t
	   (format t "A call to ~S with argument = ~S returns ~S; returning 1 instead.~%"
		   advised-function n value)
	   1))))

|#

