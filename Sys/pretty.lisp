;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		pretty.lisp
;;;;	Contents:	Corman Lisp startup code to build the
;;;;				system.
;;;;	History:	2/1/98  RGC  Created.
;;;;
;;;;
;;;;	Pretty printing functions
;;;;

(in-package :common-lisp)
(setq *compiler-warn-on-undefined-function* nil)

(defvar 		*indent-count* 	0)
(defparameter 	*indent-tab* 		4)
(defparameter	*max-line-length*	90)

(defun output-pretty-lambda (s stream)
	(let* ((first (first s))
		   (vars (second s))
		   (exprs (cddr s))) 
		(write first :stream stream)
		(write-char #\Space stream)
		(if (consp vars)
			(output-pretty-list vars stream nil)
			(write vars :stream stream))
		(let ((*indent-count* (+ *indent-count* *indent-tab*)))
			(output-columnar-list exprs stream))))

(defun output-pretty-defining-form (s stream)
	(let ((first (first s))
		  (name (second s))
		  (vars (third s))
		  (exprs (cdddr s)))
		(write first :stream stream)
		(write-char #\Space stream)
		(write name :stream stream)
		(write-char #\Space stream)
		(if (consp vars)
			(output-pretty-list vars stream nil)
			(write vars :stream stream))
		(let ((*indent-count* (+ *indent-count* *indent-tab*)))
			(output-columnar-list exprs stream))))

(defun output-pretty-block (s stream)
	(let ((first (first s))
		  (label (second s))
		  (exprs (cddr s)))
		(write first :stream stream)
		(write-char #\Space stream)
		(write label :stream stream)
		(let ((*indent-count* (+ *indent-count* *indent-tab*)))
			(output-columnar-list exprs stream))))

(defun output-pretty-let (s stream)
	(let ((first (first s))
		  (vars (second s))
		  (exprs (cddr s)))
		(if (and (consp vars) (> (list-length vars) 1))
			(progn
				(write first :stream stream)
				(let ((*indent-count* (+ *indent-count* *indent-tab*)))
					(output-pretty-list vars stream t)
					(output-columnar-list exprs stream)))
			(progn
				(write first :stream stream)
				(write-char #\Space stream)
				(write vars :stream stream)
				(let ((*indent-count* (+ *indent-count* *indent-tab*)))
					(output-columnar-list exprs stream))))))

(defun output-pretty-list1-form (s stream)
	(let ((first (first s)))
		(if (consp first)
			(output-pretty-list first stream nil)
			(write-lisp-object first))
		(let ((*indent-count* (+ *indent-count* *indent-tab*)))
			(output-columnar-list (cdr s) stream))))

(defun output-pretty-list2-form (s stream)
	(do* ((p s (cdr p))
          (count 0 (+ count 1)))
        ((not (consp p)))
        (if (> count 0)	
			(write-char #\Space stream))
		(if (consp (car p))
			(output-pretty-list (car p) stream nil)
			(write-lisp-object (car p)))
        (when (and (cdr p) (not (consp (cdr p))))
			(write " . " :stream stream :escape nil)
			(write (cdr p) :stream stream))))

;;;
;;; OUTPUT-COLUMNAR-LIST
;;;
(defun output-columnar-list (s stream)
	(dolist (n s)
		(write-char #\Newline stream)
		(indent stream)
		(if (consp n)
			(progn
				(output-pretty-list n stream nil))
			(write n :stream stream)))
	(let ((end (last s)))
		(when (cdr end)
			(write " . " :stream stream)
			(write (cdr end) :stream stream))))

;;;
;;;	print-length	
;;;	Returns the number of chars required to print the
;;;	passed expression.
;;;
(defun print-length (s)
	(length 
		(with-output-to-string (stream)
			(write s :stream stream :pretty nil))))

;;;
;;; indent
;;; Outputs the specified number of spaces.
;;;
(defun indent (stream) 
	(dotimes (i *indent-count*)
		(write-char #\Space stream)))

(defun reset-indent () (setq *indent-count* 0))

(defun output-pretty-list (s &optional (stream *standard-output*)(need-to-indent nil))
	(let ((first (car s))
		  (plength (print-length s)))

		(when need-to-indent
			(write #\Newline :stream stream :escape nil)
			(indent stream))
			
		;; check for (quote x) forms and output as 'x
		(if (and (eq first 'quote) (consp (cdr s)) (null (cddr s)))
			(let ((quoted-form (cadr s)))
				(write-char #\' stream)
				(if (consp quoted-form)
					(output-pretty-list quoted-form stream nil)
					(write quoted-form :stream stream))
				(return-from output-pretty-list s)))

		;; check for (backquote x) forms and output as `x
		(if (and (eq first 'cl::backquote) (consp (cdr s)) (null (cddr s)))
			(let ((quoted-form (cadr s)))
				(write-char #\` stream)
				(if (consp quoted-form)
					(output-pretty-list quoted-form stream nil)
					(write quoted-form :stream stream))
				(return-from output-pretty-list s)))

		;; check for (cl::%comma x) forms and output as ,x
		(if (and (eq first 'cl::%comma) (consp (cdr s)) (null (cddr s)))
			(let ((quoted-form (cadr s)))
				(write-char #\, stream)
				(if (consp quoted-form)
					(output-pretty-list quoted-form stream nil)
					(write quoted-form :stream stream))
				(return-from output-pretty-list s)))

        ;; check for (function x) forms and output as #'x
        (if (and (eq first 'function) (consp (cdr s)) (null (cddr s)))
            (let ((quoted-form (cadr s)))
              (write-char #\# stream)
              (write-char #\' stream)
              (if (consp quoted-form)
                  (output-pretty-list quoted-form stream nil)
                  (write quoted-form :stream stream))
              (return-from output-pretty-list s)))
        
        ;; check for (cl::%comma-atsign x) forms and output as ,@x
		(if (and (eq first 'cl::%comma-atsign) (consp (cdr s)) (null (cddr s)))
			(let ((quoted-form (cadr s)))
				(write-char #\, stream)
                (write-char #\@ stream)
				(if (consp quoted-form)
					(output-pretty-list quoted-form stream nil)
					(write quoted-form :stream stream))
				(return-from output-pretty-list s)))
                        
		(incf cl::*current-print-level*)		
		(write-char #\( stream)
        
		(cond ((and (eq first 'lambda) (consp (cdr s))) 
					(output-pretty-lambda s stream))
			  ((and (member first '(defun defmacro defgeneric defclass)) (consp (cdr s)) (consp (cddr s)))
					(output-pretty-defining-form s stream))
			  ((and (eq first 'block) (consp (cdr s)))
					(output-pretty-block s stream))
			  ((and (member first '(let let*)) (consp (cdr s)))
					(output-pretty-let s stream))
			  ((or (consp first) (> (+ plength *indent-count*) *max-line-length*))
					(output-pretty-list1-form s stream))
			  (t (output-pretty-list2-form s stream)))

		(write-char #\) stream)
		(decf cl::*current-print-level*)
		s))

(setq *compiler-warn-on-undefined-function* t)


