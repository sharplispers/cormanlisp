;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		util.lisp
;;;;	Contents:	
;;;;	History:	11/9/96  RGC  Created.
;;;;

;;;
;;;	Common Lisp WITH-OUTPUT-TO-STRING macro.
;;;
(defmacro with-output-to-string ((var &optional string) &rest forms)
	(let ((str-var (gensym)))
		`(let ((,var (make-string-output-stream)) (ret ,string) ,str-var)	
			(unwind-protect
				(progn
					(let ()		; establish a let block to allow declarations
						,@forms)
					(setq ,str-var (get-output-stream-string ,var))
					(if ret
						(dotimes (i (length ,str-var))
							(vector-push-extend (elt ,str-var i) ret))
						(setq ret ,str-var)))
				(close ,var))
			ret)))

;
;	Common Lisp MISMATCH function.
;
(defun mismatch (sequence1 sequence2 
		&key (from-end nil)
			 (test #'eql) 
			 (test-not nil)
			 (key nil)
			 (start1 0) 
			 (start2 0)
			 (end1 (length sequence1))
			 (end2 (length sequence2)))

	(unless (sequencep sequence1)
		(error "Not a sequence: ~A" sequence1))
	(unless (sequencep sequence2)
		(error "Not a sequence: ~A" sequence2))
	(if test-not (setq test #'(lambda (x y) (not (funcall test-not x y)))))

	(if from-end
		;; loop backward
		(do* ((i1 start1 (1+ i1))
			  (i2 start2 (1+ i2)) 
			  x1 x2)
			((and (>= i1 end1) (>= i2 end2)) nil)
			(if (>= i1 end1) (return i1))
			(if (>= i2 end2) (return i1))
			(setq x1 (elt sequence1 i1))
			(setq x2 (elt sequence2 i2))
			(if key 
				(progn
					(setq x1 (funcall key x1))
					(setq x2 (funcall key x2))))
			(unless (funcall test x1 x2)
				(return i1)))

		;;; else go forward
		(do* ((i1 start1 (1+ i1))
			  (i2 start2 (1+ i2))
			  x1 x2)
			((and (>= i1 end1) (>= i2 end2)) nil)
			(if (>= i1 end1) (return i1))
			(if (>= i2 end2) (return i1))
			(setq x1 (elt sequence1 i1))
			(setq x2 (elt sequence2 i2))
			(if key
				(progn
					(setq x1 (funcall key x1))
					(setq x2 (funcall key x2))))
			(unless (funcall test x1 x2)
				(return i1)))))

;
;	Common Lisp SEARCH function.
;
(defun search (sequence1 sequence2 
		&key (from-end nil)
			 (test #'eql) 
			 (test-not nil)
			 (key nil)
			 (start1 0) 
			 (start2 0)
			 (end1 (length sequence1))
			 (end2 (length sequence2)))

	(unless (sequencep sequence1)
		(error "Not a sequence: ~A" sequence1))
	(unless (sequencep sequence2)
		(error "Not a sequence: ~A" sequence2))
	(if test-not (setq test #'(lambda (x y) (not (funcall test-not x y)))))

	(if from-end
		;; loop backward
		(do* ((i (1- end2) (1- i)) 
			  compare)
			((< i start2) nil)
			(setq compare (mismatch sequence1 sequence2 :test test
					:key key :start1 start1 :end1 end1 :start2 i))
			(if (or (null compare) (>= compare end1))
				(return i)))

		;;; else go forward
		(do* ((i start2 (1+ i)) 
			  compare)
			((>= i end2) nil)
			(setq compare (mismatch sequence1 sequence2 :test test
					:key key :start1 start1 :end1 end1 :start2 i))
			(if (or (null compare) (>= compare end1))
				(return i)))))

;;;
;;;	Common Lisp PLUSP function.
;;;
(defun plusp (x) (> x 0))

;;;
;;;	Common Lisp MINUSP function.
;;;
(defun minusp (x) (< x 0))

;;;
;;;	Common Lisp ZEROP function.
;;;
(defun zerop (x) (= x 0))

;;;
;;;	Common Lisp COMPILE function.
;;;
(defun compile (name &optional definition)
	(let ((compiled-function
			(cond
				((functionp definition) definition)
				((and (consp definition)(eq (car definition) 'lambda))
				 (eval `(function ,definition)))
				(t nil))))
		(cond
			((and name (null compiled-function))
			 (values name nil nil))			;nothing to do
			((and (null name) compiled-function)
			 (values compiled-function nil nil))
			((and name compiled-function)
			 (setf (symbol-function name) compiled-function)
			 (values name nil nil)))))

;;;
;;;	Common Lisp COMPILED-FUNCTION-P function.
;;;	In Corman Lisp, all functions are compiled.
;;;
(defun compiled-function-p (obj)
	(functionp obj))

(defmacro with-synchronization (critical-section . body)
    (let ((temp-var (gensym)))
    	`(let ((,temp-var ,critical-section))
            (if ,temp-var
                (unwind-protect 
            		(progn (enter-critical-section ,temp-var)
            			,@body)
            		(leave-critical-section ,temp-var))
                (progn ,@body)))))

(defmacro declare-synchronized-function (sync-object name lambda-list . body)
	(let ((decls '()))
		;; gather any DECLARE forms
		(do ((f body (cdr f)))
			((or (null f) (not (consp (car f))) (not (eq (caar f) 'declare)))
			 (setf body f))
			(push (car f) decls))

		`(defun ,name ,lambda-list
			,@(nreverse decls)
			(with-synchronization ,sync-object ,@body))))

