;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		lists.lisp
;;;;	Contents:	Corman Lisp list manipulation functions.
;;;;	History:	2/26/97  RGC  Created.
;;;;

(in-package :common-lisp)

;;;
;;;	Common Lisp NCONC function.
;;;
(defun nconc (&rest lists)
	(do* ((x lists (cdr x))
		  (list (car x)(car x))
		  (ret nil))
		((null x) ret)
		(unless (listp x) (error "Not a list: ~A" x))
		(if ret (rplacd (last ret) list) (setq ret list))
		ret))

;;;
;;;	Common Lisp INTERSECTION function.
;;;
(defun intersection (list-1 list-2 
				&key (key nil)
					(test #'eql)
					(test-not nil))
	(if test-not
		(setq test #'(lambda (x y) (not (funcall test-not x y)))))
	(let ((newlist nil)
		  (kx nil))
		(dolist (x list-1)
			(setq kx (if key (funcall key x) x))
			(if (member kx list-2 :test test :key key)
				(push x newlist)))
		(nreverse newlist)))

;;;
;;;	Common Lisp UNION function.
;;;
(defun union (list-1 list-2 
				&key (key nil)
					(test #'eql)
					(test-not nil))
	(if test-not
		(setq test #'(lambda (x y) (not (funcall test-not x y)))))
	(let ((newlist nil)
		  (kx nil))
		(dolist (x list-1)
			(setq kx (if key (funcall key x) x))
			(unless (member kx list-2 :test test :key key)
				(push x newlist)))
		(append (nreverse newlist) list-2)))

;;;
;;;	Common Lisp SUBSETP function.
;;; Warning: needs work to avoid O2 performance.
;;;
(defun subsetp (list-1 list-2 &key key test test-not)
	(unless (listp list-1)
		(error "Invalid list: ~S" list-1))  ;; list-2 checked by MEMBER
	(if test-not
		(setq test #'(lambda (x y) (not (funcall test-not x y)))))
	(every 
		#'(lambda (x)
			(apply #'member (if key (funcall key x) x) list-2 
				`(,@(if key (list :key key))
				  ,@(if test (list :test test))))) 
		list-1))

;;;
;;;	Common Lisp SET-DIFFERENCE function.
;;;
(defun set-difference (list-1 list-2 
				&key (key nil)
					(test #'eql)
					(test-not nil))
	(if test-not
		(setq test #'(lambda (x y) (not (funcall test-not x y)))))
	(let ((newlist nil)
		  (kx nil))
		(dolist (x list-1)
			(setq kx (if key (funcall key x) x))
			(unless (member kx list-2 :test test :key key)
				(push x newlist)))
		(nreverse newlist)))

;;;
;;;	Common Lisp PUSHNEW macro.
;;;
(defmacro pushnew (val form &rest rest)
	(if (and (consp form) (some #'consp form))
		(let ((retval (%once-only-forms form)))
			`(let ,(car retval) 
				(setf ,(cdr retval) (adjoin ,val ,(cdr retval) ,@rest))))
		`(setf ,form (adjoin ,val ,form ,@rest))))

#|
;;;
;;;	Common Lisp MAPC function.
;;;
(defun mapc (function list &rest more-lists)
	(let ((result nil)
		  (lists (cons list more-lists)))
		(block outer-loop
			(do ((args nil nil)
				 (count 0 (+ count 1)))
				(nil nil)
				(dolist (a lists)
					(if (<= (list-length a) count)
						(return-from outer-loop nil)
						(push (nth count a) args)))
				(apply function (nreverse args))))
		list))

;;;
;;;	Common Lisp MAPL function.
;;;
(defun mapl (function list &rest more-lists)
	(let ((result nil)
		  (lists (cons list more-lists)))
		(block outer-loop
			(do ((args nil nil)
				 (count 0 (+ count 1)))
				(nil nil)
				(dolist (a lists)
					(if (<= (list-length a) count)
						(return-from outer-loop nil)
						(push (nthcdr count a) args)))
				(apply function (nreverse args))))
		list))
|#

(defun pairlis (keys values &optional list)
       (nconc (mapcar #'cons keys values) list))

(defun set-exclusive-or (list1 list2 &rest rest)
  (append (apply #'set-difference list1 list2 rest)
          (apply #'set-difference list2 list1 rest)))

(defun nset-exclusive-or (list1 list2 &rest rest)
  (nconc (apply #'set-difference list1 list2 rest)
         (apply #'set-difference list2 list1 rest)))

;;;
;;;	Common Lisp ACONS function.
;;;
(defun acons (key datum alist) (cons (cons key datum) alist))

;;;
;;;	Common Lisp TAILP function.
;;;
(defun tailp (object list)
   (do ((list list (cdr list)))
       ((atom list) (eql list object))
      (if (eql object list)
          (return t))))

;;;
;;;	Common Lisp LDIFF function.
;;;
(defun ldiff (list object)
  (do ((list list (cdr list))
       (r '() (cons (car list) r)))
      ((atom list)
       (if (eql list object) (nreverse r) (nreconc r list)))
    (when (eql object list)
      (return (nreverse r)))))


#|	;; MAPCAN, MAPLIST and MAPCOM moved to misc.lisp
;;;
;;;	Common Lisp MAPCAN function.
;;;
(defun mapcan (function list &rest more-lists)
	(let ((result nil)
		  (lists (cons list more-lists)))
		(block outer-loop
			(do ((args nil nil)
				 (count 0 (+ count 1)))
				(nil nil)
				(dolist (a lists)
					(if (<= (list-length a) count)
						(return-from outer-loop nil)
						(push (nth count a) args)))
				(push (apply function (nreverse args)) result)))
		(apply 'nconc (nreverse result))))

;;;
;;;	Common Lisp MAPLIST function.
;;;
(defun maplist (function list &rest more-lists)
	(let ((result nil)
		  (lists (cons list more-lists)))
		(block outer-loop
			(do ((args nil nil)
				 (count 0 (+ count 1)))
				(nil nil)
				(dolist (a lists)
					(if (<= (list-length a) count)
						(return-from outer-loop nil)
						(push (nthcdr count a) args)))
				(push (apply function (nreverse args)) result)))
		(nreverse result)))

;;;
;;;	Common Lisp MAPCON function.
;;;
(defun mapcon (function list &rest more-lists)
	(let ((result nil)
		  (lists (cons list more-lists)))
		(block outer-loop
			(do ((args nil nil)
				 (count 0 (+ count 1)))
				(nil nil)
				(dolist (a lists)
					(if (<= (list-length a) count)
						(return-from outer-loop nil)
						(push (nthcdr count a) args)))
				(push (apply function (nreverse args)) result)))
		(apply 'nconc (nreverse result))))
|#

;;;
;;;	Common Lisp ELT function
;;;
(defun elt (sequence index)
	(if (or (not (fixnump index))(< index 0))
		(error "Index out of range: ~A" index))
	(if (uvectorp sequence)
		(let* ((vec sequence)
			  (fill-pointer nil)
			  (type (uvector-type-bits vec)))
			(when (eq type uvector-array-tag)
				(if (> (array-rank sequence) 1) (error "Not a vector: ~A" sequence))
				(setq vec (uref sequence adjustable-array-vector-offset))
				(setq type (uvector-type-bits vec))
				(incf index (uref sequence adjustable-array-displaced-offset))
				(when (array-has-fill-pointer-p sequence)
					(setq fill-pointer (fill-pointer sequence))
					(if (>= index fill-pointer)
						(error "Index out of range: ~A" index)))) 
			(cond 
				((eq type uvector-simple-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (x86::%svref vec index))
				((eq type uvector-simple-char-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (x86::%svchar vec index))
				((eq type uvector-simple-bit-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (x86::%svbit vec index))
				((eq type uvector-simple-byte-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (x86::%svbyte vec index))
				((eq type uvector-simple-double-float-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (x86::%svdouble vec index))				
				((eq type uvector-simple-single-float-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (x86::%svsingle vec index))				                
				((eq type uvector-simple-short-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (x86::%svshort vec index))				                
                (t (error "Not a sequence: ~A" sequence))))
		;; check for a list
		(let ((n index))
			(unless (listp sequence) (error "Not a sequence: ~A" sequence))
			(dolist (i sequence)
				(if (= 0 n)
					(return-from elt i)
					(decf n)))
			(error "Index out of range: ~A" index))))

;;;
;;;	Common Lisp (SETF ELT) function
;;;
(defun (setf elt) (value sequence index)
	(if (or (not (fixnump index))(< index 0))
		(error "Index out of range: ~A" index))
	(if (uvectorp sequence)
		(let* ((vec sequence)
			  (fill-pointer nil)
			  (type (uvector-type-bits vec)))
			(when (eq type uvector-array-tag)
				(if (> (array-rank sequence) 1) (error "Not a vector: ~A" sequence))
				(setq vec (uref sequence adjustable-array-vector-offset))
				(setq type (uvector-type-bits vec))
				(incf index (uref sequence adjustable-array-displaced-offset))
				(when (array-has-fill-pointer-p sequence)
					(setq fill-pointer (fill-pointer sequence))
					(if (>= index fill-pointer)
						(error "Index out of range: ~A" index)))) 
			(cond 
				((eq type uvector-simple-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (setf (x86::%svref vec index) value))
				((eq type uvector-simple-char-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (unless (characterp value)(error "Not a character: ~A" value))
				 (setf (x86::%svchar vec index) value))
				((eq type uvector-simple-bit-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (unless (or (eq value 0)(eq value 1))(error "Not a bit: ~A" value))
				 (setf (x86::%svbit vec index) value))
				((eq type uvector-simple-byte-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (unless (and (fixnump value) (>= value 0)(<= value 255))
					(error "Not a byte: ~A" value))
				 (setf (x86::%svbyte vec index) value))
				((eq type uvector-simple-double-float-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (unless (double-float-p value)
					(if (realp value)
							(setf value (float value 0d0))
							(error "Not a double-float: ~A" value)))
				 (setf (x86::%svdouble vec index) value))				
                ((eq type uvector-simple-single-float-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (unless (single-float-p value)
					(if (realp value)
							(setf value (float value 0f0)))
					(error "Not a single-float: ~A" value))
				 (setf (x86::%svsingle vec index) value))
                ((eq type uvector-simple-short-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (unless (and (fixnump value) (>= value -32768)(<= value 32767))
					(error "Not a short integer: ~A" value))
				 (setf (x86::%svshort vec index) value))
   				(t (error "Not a sequence: ~A" sequence))))
		;; check for a list
		(let ((n index))
			(unless (listp sequence) (error "Not a sequence: ~A" sequence))
			(block loop
				(do* ((i sequence (cdr i)))
					((not (consp i)))
					(if (= 0 n)
						(return-from loop (setf (car i) value))
						(decf n)))
				(error "Index out of range: ~A" index)))))

;;;
;;;	Common Lisp ROW-MAJOR-AREF function
;;;
(defun row-major-aref (array index)
	(if (or (not (fixnump index))(< index 0))
		(error "Index out of range: ~A" index))
	(if (uvectorp array)
		(let* ((vec array)
			   (type (uvector-type-bits array)))
			(when (eq type uvector-array-tag)
				(setq vec (uref array adjustable-array-vector-offset))
				(setq type (uvector-type-bits vec))
				(incf index (uref array adjustable-array-displaced-offset)))
			(cond 
				((eq type uvector-simple-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (x86::%svref vec index))
				((eq type uvector-simple-char-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (x86::%svchar vec index))
				((eq type uvector-simple-bit-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (x86::%svbit vec index))
				((eq type uvector-simple-byte-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (x86::%svbyte vec index))
				((eq type uvector-simple-double-float-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (x86::%svdouble vec index))
				((eq type uvector-simple-single-float-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (x86::%svsingle vec index))
				((eq type uvector-simple-short-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (x86::%svshort vec index))
				(t (error "Not an array: ~A" array))))
		(error "Not an array: ~A" array)))

;;;
;;;	Common Lisp (SETF ROW-MAJOR-AREF) function
;;;
(defun (setf row-major-aref) (value array index)
	(if (or (not (fixnump index))(< index 0))
		(error "Index out of range: ~A" index))
	(if (uvectorp array)
		(let* ((vec array)
			   (type (uvector-type-bits array)))
			(when (eq type uvector-array-tag)
				(setq vec (uref array adjustable-array-vector-offset))
				(setq type (uvector-type-bits vec))
				(incf index (uref array adjustable-array-displaced-offset)))
			(cond 
				((eq type uvector-simple-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (setf (x86::%svref vec index) value))
				((eq type uvector-simple-char-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (unless (characterp value)(error "Not a character: ~A" value))
				 (setf (x86::%svchar vec index) value))
				((eq type uvector-simple-bit-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (unless (or (eq value 0)(eq value 1))(error "Not a bit: ~A" value))
				 (setf (x86::%svbit vec index) value))
				((eq type uvector-simple-byte-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (unless (and (fixnump value) (>= value 0)(<= value 255))
					(error "Not a byte: ~A" value))
				 (setf (x86::%svbyte vec index) value))
				((eq type uvector-simple-double-float-vector-tag)
				 (unless (double-float-p value)
					(if (realp value)
							(setf value (float value 0d0))
							(error "Not a double-float: ~A" value)))
				 (setf (x86::%svdouble vec index) value))
				((eq type uvector-simple-single-float-vector-tag)
				 (unless (single-float-p value)
					(if (realp value)
							(setf value (float value 0f0)))
					(error "Not a single-float: ~A" value))
				 (setf (x86::%svsingle vec index) value))
				((eq type uvector-simple-short-vector-tag)
				 (if (>= index (uref vec 1))(error "Index out of range: ~A" index))
				 (unless (and (fixnump value) (>= value -32768)(<= value 32767))
					(error "Not a short integer: ~A" value))
				 (setf (x86::%svshort vec index) value))
				(t (error "Not an array: ~A" array))))
		(error "Not an array: ~A" array)))

;;;
;;;	Common Lisp ASSOC-IF function.
;;;
(defun assoc-if (predicate alist &key key)
	(dolist (a alist)
		(if (and (consp a) 
				 (funcall predicate 
					(if key (funcall key (car a)) (car a)))) 
			(return a))))

;;;
;;;	Common Lisp ASSOC-IF-NOT function.
;;;
(defun assoc-if-not (predicate alist &key key)
	(dolist (a alist)
		(if (and (consp a) 
				 (not 
					(funcall predicate 
					(if key (funcall key (car a)) (car a))))) 
			(return a))))

;;;
;;;	Common Lisp RASSOC function.
;;;
(defun rassoc (item alist &key key (test #'eql) test-not)
	(if test-not
		(let ((save-test test))
			(setq test #'(lambda (x y) (not (funcall save-test x y))))))
	(dolist (a alist)
		(if (and (consp a) 
				 (funcall test item 
					(if key (funcall key (cdr a)) (cdr a))))
			(return a))))

;;;
;;;	Common Lisp RASSOC-IF function.
;;;
(defun rassoc-if (predicate alist &key key)
	(dolist (a alist)
		(if (and (consp a) 
				 (funcall predicate 
					(if key (funcall key (cdr a)) (cdr a)))) 
			(return a))))

;;;
;;;	Common Lisp RASSOC-IF-NOT function.
;;;
(defun rassoc-if-not (predicate alist &key key)
	(dolist (a alist)
		(if (and (consp a) 
				 (not 
					(funcall predicate 
					(if key (funcall key (cdr a)) (cdr a))))) 
			(return a))))

;;;
;;; Common Lisp TREE-EQUAL function.
;;;
(defun tree-equal (tree-1 tree-2 &key (test #'eql) test-not)
	(if test-not
		(setf test (complement test-not)))
	
	(cond ((and (atom tree-1) (atom tree-2)
				(funcall test tree-1 tree-2))
			t)
		  ((and (consp tree-1) (consp tree-2)
				(tree-equal (car tree-1) (car tree-2) :test test)
				(tree-equal (cdr tree-1) (cdr tree-2) :test test))
			t)
		  (t nil)))	 

;;;
;;; Common Lisp REVAPPEND function.
;;;
(defun revappend (list tail)
	(let* ((result tail))
		(dolist (x list)
			(push x result))
		result))

;;;
;;; Common Lisp COPY-ALIST function.
;;;
(defun copy-alist (alist)
	(let ((result '()))
		(dolist (x alist)
			(push (if (consp x) (cons (car x)(cdr x)) x) result))
		(nreverse result)))
			
;;;
;;; Common Lisp NINTERSECTION function.
;;;
(defun nintersection (list-1 list-2 &key key (test #'eql) test-not)
	(if test-not
		(setf test (complement test-not)))
	;; just call non-destructive version for now
	(intersection list-1 list-2 :key key :test test))

;;;
;;; Common Lisp NSET-DIFFERENCE function.
;;;
(defun nset-difference (list-1 list-2 &key key (test #'eql) test-not)
	(if test-not
		(setf test (complement test-not)))
	;; just call non-destructive version for now
	(set-difference list-1 list-2 :key key :test test))

;;;
;;; Common Lisp NUNION function.
;;;
(defun nunion (list-1 list-2 &key key (test #'eql) test-not)
	(if test-not
		(setf test (complement test-not)))
	;; just call non-destructive version for now
	(union list-1 list-2 :key key :test test))

;;;
;;;	Common Lisp VECTOR-PUSH-EXTEND function.
;;; Redefined here to ensure we use the (SET ELT) function defined in this file.
;;;
(defun vector-push-extend (new-element vector &optional extension)
	(unless (array-has-fill-pointer-p vector) 
		(error "Vector does not have a fill pointer: ~A" vector))
	(let ((pos (fill-pointer vector)))
		(if (>= pos (array-dimension vector 0))
			(setq vector (extend-vector vector extension)))
		(setf (fill-pointer vector) (+ pos 1))
		(setf (elt vector pos) new-element)
		pos))

;;;
;;;	Common Lisp VECTOR-PUSH function.
;;; Redefined here to ensure we use the (SET ELT) function defined in this file.
;;;
(defun vector-push (new-element vector)
	(unless (array-has-fill-pointer-p vector) 
		(error "Vector does not have a fill pointer: ~A" vector))
	(let ((pos (fill-pointer vector)))
		(if (>= pos (array-dimension vector 0))
			(return-from vector-push nil))
		(setf (fill-pointer vector) (+ pos 1))
		(setf (elt vector pos) new-element)
		pos))

