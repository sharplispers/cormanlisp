;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;	
;;;;	File:		strings.lisp
;;;;	Contents:	Corman Lisp string functions.
;;;;	History:	3/4/97  RGC  Created.
;;;;				4/6/01  RGC  Moved SIMPLE-STRING-P out of this file (to misc.lisp).
;;;;

(in-package "COMMON-LISP")

;;;
;;;	Common Lisp string function.
;;;
(defun string (x)
	(unless (stringp x)
		(if (symbolp x)
			(setq x (symbol-name x))
			(if (characterp x)
				(setq x (coerce (list x) 'string))
				(error "Not a string designator: ~A" x))))
	x)

;;;
;;;	ENSURE-STRING macro is used by this implementation.
;;;
(defmacro ensure-string (var) `(unless (stringp ,var) (setq ,var (string ,var))))

;;;
;;;	Common Lisp string= function.
;;;
(defun string= (x y 
				&key (start1 0) 
					 (end1 nil) 
					 (start2 0) 
					 (end2 nil))
	(ensure-string x)
	(ensure-string y)
	(unless end1 (setq end1 (length x)))
	(unless end2 (setq end2 (length y)))
	(let ((numchars (- end1 start1)))
		(unless (= numchars (- end2 start2))
			(return-from string= nil))
		(do ((i start1 (+ i 1))
			 (j start2 (+ j 1)))
			((= i end1) t)
			(unless (eq (char x i) (char y j))
				(return nil)))))

;;;
;;;	Common Lisp string/= function.
;;;
(defun string/= (x y 
				&key (start1 0) 
					 (end1 nil) 
					 (start2 0) 
					 (end2 nil))
	(ensure-string x)
	(ensure-string y)
	(unless end1 (setq end1 (length x)))
	(unless end2 (setq end2 (length y)))
	(do ((i start1 (+ i 1))
		 (j start2 (+ j 1)))
		((= i end1) (if (= j end2) nil i))
		(if (= j end2)
			(return i))
		(if (char/= (char x i) (char y j))
			(return i))))

;;;
;;;	Common Lisp string< function.
;;;
(defun string< (x y 
				&key (start1 0) 
					 (end1 nil) 
					 (start2 0) 
					 (end2 nil))
	(ensure-string x)
	(ensure-string y)
	(unless end1 (setq end1 (length x)))
	(unless end2 (setq end2 (length y)))
	(do ((i start1 (+ i 1))
		 (j start2 (+ j 1)))
		((= i end1) (if (= j end2) nil i))
		(if (= j end2)
			(return nil))
		(if (char< (char x i) (char y j))
			(return i))
		(if (char> (char x i) (char y j))
			(return nil))))

;;;
;;;	Common Lisp string> function.
;;;
(defun string> (x y 
				&key (start1 0) 
					 (end1 nil) 
					 (start2 0) 
					 (end2 nil))
	(ensure-string x)
	(ensure-string y)
	(unless end1 (setq end1 (length x)))
	(unless end2 (setq end2 (length y)))
	(do ((i start1 (+ i 1))
		 (j start2 (+ j 1)))
		((= i end1) nil)
		(if (= j end2)
			(return i))
		(if (char> (char x i) (char y j))
			(return i))
		(if (char< (char x i) (char y j))
			(return nil))))

;;;
;;;	Common Lisp string<= function.
;;;
(defun string<= (x y 
				&key (start1 0) 
					 (end1 nil) 
					 (start2 0) 
					 (end2 nil))
	(ensure-string x)
	(ensure-string y)
	(unless end1 (setq end1 (length x)))
	(unless end2 (setq end2 (length y)))
	(do ((i start1 (+ i 1))
		 (j start2 (+ j 1)))
		((= i end1) i)
		(if (= j end2)
			(return nil))
		(if (char< (char x i) (char y j))
			(return i))
		(if (char> (char x i) (char y j))
			(return nil))))

;;;
;;;	Common Lisp string>= function.
;;;
(defun string>= (x y 
				&key (start1 0) 
					 (end1 nil) 
					 (start2 0) 
					 (end2 nil))
	(ensure-string x)
	(ensure-string y)
	(unless end1 (setq end1 (length x)))
	(unless end2 (setq end2 (length y)))
	(do ((i start1 (+ i 1))
		 (j start2 (+ j 1)))
		((= i end1) (if (= j end2) i nil))
		(if (= j end2)
			(return i))
		(if (char> (char x i) (char y j))
			(return i))
		(if (char< (char x i) (char y j))
			(return nil))))

;;;
;;;	Common Lisp string-equal function.
;;;
(defun string-equal (x y 
				&key (start1 0) 
					 (end1 nil) 
					 (start2 0) 
					 (end2 nil))
	(ensure-string x)
	(ensure-string y)
	(unless end1 (setq end1 (length x)))
	(unless end2 (setq end2 (length y)))
	(let ((numchars (- end1 start1)))
		(unless (= numchars (- end2 start2))
			(return-from string-equal nil))
		(do ((i start1 (+ i 1))
			 (j start2 (+ j 1)))
			((= i end1) t)
			(unless (char-equal (char x i) (char y j))
				(return nil)))))

;;;
;;;	Common Lisp string-not-equal function.
;;;
(defun string-not-equal (x y 
				&key (start1 0) 
					 (end1 nil) 
					 (start2 0) 
					 (end2 nil))
	(ensure-string x)
	(ensure-string y)
	(unless end1 (setq end1 (length x)))
	(unless end2 (setq end2 (length y)))
	(do ((i start1 (+ i 1))
		 (j start2 (+ j 1)))
		((= i end1) (if (= j end2) nil i))
		(if (= j end2)
			(return i))
		(if (char-not-equal (char x i) (char y j))
			(return i))))

;;;
;;;	Common Lisp STRING-LESSP function.
;;;
(defun string-lessp (x y 
				&key (start1 0) 
					 (end1 nil) 
					 (start2 0) 
					 (end2 nil))
	(ensure-string x)
	(ensure-string y)
	(unless end1 (setq end1 (length x)))
	(unless end2 (setq end2 (length y)))
	(do ((i start1 (+ i 1))
		 (j start2 (+ j 1)))
		((= i end1) (if (= j end2) nil i))
		(if (= j end2)
			(return nil))
		(if (char-lessp (char x i) (char y j))
			(return i))
		(if (char-greaterp (char x i) (char y j))
			(return nil))))

;;;
;;;	Common Lisp STRING-GREATERP function.
;;;
(defun string-greaterp (x y 
				&key (start1 0) 
					 (end1 nil) 
					 (start2 0) 
					 (end2 nil))
	(ensure-string x)
	(ensure-string y)
	(unless end1 (setq end1 (length x)))
	(unless end2 (setq end2 (length y)))
	(do ((i start1 (+ i 1))
		 (j start2 (+ j 1)))
		((= i end1) nil)
		(if (= j end2)
			(return i))
		(if (char-greaterp (char x i) (char y j))
			(return i))
		(if (char-lessp (char x i) (char y j))
			(return nil))))

;;;
;;;	Common Lisp STRING-NOT-GREATERP function.
;;;
(defun string-not-greaterp (x y 
				&key (start1 0) 
					 (end1 nil) 
					 (start2 0) 
					 (end2 nil))
	(ensure-string x)
	(ensure-string y)
	(unless end1 (setq end1 (length x)))
	(unless end2 (setq end2 (length y)))
	(do ((i start1 (+ i 1))
		 (j start2 (+ j 1)))
		((= i end1) i)
		(if (= j end2)
			(return nil))
		(if (char-lessp (char x i) (char y j))
			(return i))
		(if (char-greaterp (char x i) (char y j))
			(return nil))))

;;;
;;;	Common Lisp STRING-NOT-LESSP function.
;;;
(defun string-not-lessp (x y 
				&key (start1 0) 
					 (end1 nil) 
					 (start2 0) 
					 (end2 nil))
	(ensure-string x)
	(ensure-string y)
	(unless end1 (setq end1 (length x)))
	(unless end2 (setq end2 (length y)))
	(do ((i start1 (+ i 1))
		 (j start2 (+ j 1)))
		((= i end1) (if (= j end2) i nil))
		(if (= j end2)
			(return i))
		(if (char-greaterp (char x i) (char y j))
			(return i))
		(if (char-lessp (char x i) (char y j))
			(return nil))))

(setq *compiler-warn-on-undefined-function* nil)	;; FIND not defined yet
(defun string-left-trim (char-bag string)
	(let* ((s (string string))
		   (start-index 0)
		   (length (length s)))
		;; trim off leading characters
		(dotimes (i length)
			(let* ((c (char s start-index)))
				(unless (find c char-bag)
					(return))
			 	(incf start-index)))
		(subseq s start-index (length s))))

(defun string-right-trim (char-bag string)
	(let* ((s (string string))
		  (end-index (1- (length s))))
		;; trim off trailing characters
		(do ((i end-index (1- i)))
			((< i 0))
			(let* ((c (char s end-index)))
				(unless (find c char-bag)
					(return))
			 	(decf end-index)))
		(subseq s 0 (1+ end-index))))
(setq *compiler-warn-on-undefined-function* t)

(defun string-trim (char-bag string)
	(string-left-trim char-bag (string-right-trim char-bag string)))

;;;
;;;	Common Lisp MAKE-STRING function.
;;;
(defun make-string (size &key (initial-element #\Space) element-type)
	(declare (ignore element-type))
	(make-array size :element-type 'character :initial-element initial-element))


