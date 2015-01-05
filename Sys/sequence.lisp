;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;	Portions Copyright (c) 2000 Vassili Bykov

;;;;	File:	  sequence.lisp

;;;;	Contents: SUBSEQ, EVERY, SOME, NOTANY, NOTEVERY, NRECONC,
;;;;	MAKE-SEQUENCE, MAP-INTO, MAP, CONCATENATE

;;;;    To Do:    Proper type handling
;;;;              proper error signaling

;;;;	History:
;;;;    * 9/27/1996   RGC  Created.
;;;;    * 12/27/1999  VB   Rewrote most functions to eliminate
;;;;                       exponential execution time when applied to lists.
;;;;                       Added MAKE-SEQUENCE and MAP-INTO.
;;;;	* 4/22/99	  RGC  Fixed a bug in MAP-INTO.
;;;;	* 4/22/99	  RGC  Fixed a couple bugs in MAKE-SEQUENCE.
;;;;	* 1/31/01	  RGC  Added Chris Double's MAKE-SEQUENCE patch.
;;;;    * 4/18/01     RGC  MAKE-SEQUENCE now handles result type NULL.
;;;;

;;;
;;;  Common Lisp SUBSEQ function
;;;
(defun subseq (sequence start &optional end)
  (let ((length (length sequence)))
    (unless end (setq end length))
    (unless (<= 0 start end length)
      (error "Invalid START = ~S, END = ~S arguments" start end))
    (if (vectorp sequence)
	(let* ((elements (- end start))
	       (a (make-array elements 
			      :element-type (array-element-type sequence))))
	  (dotimes (i elements)
	    (setf (elt a i) (elt sequence (+ i start))))
	  a)
	(if (listp sequence)
	    (let* ((elements (- end start))
		   (x (nthcdr start sequence))
		   (newlist nil))
	      (dotimes (i elements)
		(push (car x) newlist)
		(setq x (cdr x)))
	      (nreverse newlist))
	    (error "Invalid sequence: ~S" sequence)))))

;;;  Common Lisp EVERY function
;;;
(defun every (predicate sequence &rest more-sequences)
  (cond ((vectorp sequence)
	 (let* ((sequences (cons sequence more-sequences))
		(min-length (apply #'min (mapcar #'length sequences))))
	   (dotimes (index min-length)
	     (unless (apply predicate
			    (funcall #'mapcar
				     #'(lambda (vector) (elt vector index))
				     sequences))
	       (return-from every nil))))
	 t)
	((listp sequence)
	 (apply #'mapc
		#'(lambda (&rest args)
		    (unless (apply predicate args)
		      (return-from every nil)))
		sequence
		more-sequences)
	 t)
	(t
	 (error "Invalid sequence: ~S" sequence))))


;;;  Common Lisp SOME function
;;;
(defun some (predicate sequence &rest more-sequences &aux retval)
  (cond ((vectorp sequence)
	 (let* ((sequences (cons sequence more-sequences))
		(min-length (apply #'min (mapcar #'length sequences))))
	   (dotimes (index min-length)
	     (when (setf retval (apply predicate
			  (funcall #'mapcar
				   #'(lambda (vector) (elt vector index))
				   sequences)))
	       (return-from some retval))))
	 nil)
	((listp sequence)
	 (apply #'mapc
		#'(lambda (&rest args)
		    (when (setf retval (apply predicate args))
		      (return-from some retval)))
		sequence
		more-sequences)
	 nil)
	(t
	 (error "Invalid sequence: ~S" sequence))))

;;;  Common Lisp NOTANY function
;;;
(defun notany (predicate sequence &rest more-sequences)
  (not (apply #'some predicate sequence more-sequences)))

;;;  Common Lisp NOTEVERY function
;;;
(defun notevery (predicate sequence &rest more-sequences)
  (not (apply #'every predicate sequence more-sequences)))

;;;  Common Lisp NRECONC function
;;;
(defun nreconc (list tail)
  (do (n)
      ((not (consp list)))
    (setq n list)
    (setq list (cdr list))
    (rplacd n tail)
    (setq tail n))
  tail)

;;;; ---------
;;;;	Common Lisp MAPLIST function.
;;;;
;;;; [VB -- Removed: maplist is defined together with other mapping
;;;; functions.  If needed because of the bootstrap sequence requirements,
;;;; it is better to copy all mappers into the misc.lisp file.]
;;;; ---------

;;;  ****************  MAKE-SEQUENCE  ****************

(defun make-sequence (result-type length &key initial-element)
	(cond ((or (eq result-type 'list)(eq result-type 'cons)(eq result-type 'null))
			(make-list length :initial-element initial-element))
		((or (eq result-type 'vector) (eq result-type 'simple-vector))
			(make-array length :initial-element initial-element))
		((or (eq result-type 'string)(eq result-type 'simple-string)
			 (eq result-type 'base-string)(eq result-type 'simple-base-string))
			(make-array length
				:element-type 'character
				:initial-element (or initial-element #\NUL)))
		((or (eq result-type 'bit-vector)(eq result-type 'simple-bit-vector))
			(make-array length
				:element-type 'bit
				:initial-element (or initial-element 0)))
		((consp result-type)
		 (let ((sym (first result-type))
			   (arg1 (second result-type))) 
			(cond
				((eq sym 'vector)
				 (let ((type (if (or (not (cdr result-type))(eq arg1 '*)) t arg1))
					   (size (third result-type)))
				  	(if (and (null initial-element) (not (eq type t)))
								(setf initial-element
									(if (member type '(character base-char extended-char))
										#\NUL
										0)))
					(if (and size (integerp size) (/= size length))
						(error "~A elements cannot fit into a vector of size ~A" length size))
					(make-array length
						:element-type (if (or (not (cdr result-type))(eq arg1 '*)) t arg1)
						:initial-element initial-element)))
				((eq sym 'simple-vector)
				 (if (and arg1 (integerp arg1) (/= arg1 length))
					(error "~A elements cannot fit into a simple-vector of size ~A" length arg1))
			 	 (make-array length
					:element-type t
					:initial-element initial-element))
				((or (eq sym 'bit-vector) (eq sym 'simple-bit-vector))
				 (if (and arg1 (integerp arg1) (/= arg1 length))
					(error "~A elements cannot fit into a bit-vector of size ~A" length arg1))
			 	 (make-array length
					:element-type 'bit
					:initial-element (or initial-element 0)))
				((eq sym 'array)
		 		 (make-array length
					:element-type (if (or (not (cdr result-type))(eq arg1 '*)) t arg1)
					:initial-element initial-element))
				((or (eq sym 'string)(eq sym 'simple-string)
					(eq sym 'base-string)(eq sym 'simple-base-string))
				 (if (and arg1 (integerp arg1) (/= arg1 length))
					(error "~A elements cannot fit into a string of size ~A" length arg1))
				 (make-array length
					:element-type 'character
					:initial-element (or initial-element #\NUL)))
				(t (error "Cannot create a sequence of type ~A" result-type)))))
		(t (error "Cannot create a sequence of type ~A" result-type))))

(defun create-sequence-type (type length)
  (if (null type)
      nil
      (make-sequence type length)))

;;;   Helper functions for MAP and CONCATENATE:
;;;   CREATE-SEQUENCE-TYPE,
;;;   %MAKE-SETTER-ITERATOR
;;;   %MAKE-GETTER-ITERATOR
;;;
;;; To abstract out the specific kind of a sequence while retaining
;;; reasonable element access speed, we use *iterators*--in other
;;; words, closures with captured mutable state. Each time it is
;;; called, the getter iterator returns a new element; the setter
;;; iterator expects a value and stores it as a sequence element at
;;; ever increasing indices.  (Someone said a closure is a poor man's
;;; object...)  This may not be the cheapest possible way to iterate
;;; over a list or an array but it buys us a simple and uniform
;;; interface to walking over sequences of all types in a linear time.

(defun %make-getter-iterator (sequence &optional (start 0))
  (if (listp sequence)
      (let ((s (nthcdr start sequence)))
	#'(lambda () (pop s)))
      (let ((index (1- start)))
	#'(lambda () (elt sequence (incf index))))))

(defun %make-reverse-getter-iterator (sequence &optional (start 0))
  (if (vectorp sequence)
      (let ((index (- (length sequence) start)))
	#'(lambda ()
	    (elt sequence (decf index))))
      (let ((s (nthcdr start (reverse sequence))))
	#'(lambda ()
	    (pop s)))))

(defun %make-setter-iterator (sequence &optional (start 0))
  (cond ((null sequence) #'(lambda (value)))
	((listp sequence)
	 (let ((s (nthcdr start sequence)))
	   #'(lambda (value)
	       (rplaca s value)
	       (setq s (cdr s)))))
	(t
	 (let ((index (1- start)))
	   #'(lambda (value)
	       (setf (elt sequence (incf index)) value))))))

;;;    Common Lisp MAP-INTO function
;;;
(defun map-into (result-sequence function &rest sequences)
	(when (null result-sequence) (return-from map-into nil))
	(let* ((length (min (apply #'min (length result-sequence) 
							(mapcar #'length sequences))))
			(arg-getters (mapcar #'%make-getter-iterator sequences))
			(result-setter (%make-setter-iterator result-sequence)))
		(dotimes (i length)
			(funcall result-setter
				(apply function (mapcar #'funcall arg-getters))))
		(when (and (vectorp result-sequence)
				(array-has-fill-pointer-p result-sequence))
			(setf (fill-pointer result-sequence) length))
		result-sequence))


;;;    Common Lisp MAP function
;;;
(defun map (result-type function &rest sequences)
  (let* ((length (if sequences (apply #'min (mapcar #'length sequences)) 0))
	 (arg-getters (mapcar #'%make-getter-iterator sequences))
	 (result (create-sequence-type result-type length))
	 (result-setter (%make-setter-iterator result)))
    (dotimes (i length)
      (funcall result-setter
	       (apply function (mapcar #'funcall arg-getters))))
    result))

;;;;	Common Lisp CONCATENATE function.
;;;;
(defun concatenate (result-type &rest sequences)
  (let* ((result (create-sequence-type result-type
				       (apply #'+ (mapcar #'length sequences))))
	 (result-setter (%make-setter-iterator result)))
    (dolist (seq sequences)
      (cond ((listp seq)
	     (do ((s seq (cdr s)))
		 ((null s))
	       (funcall result-setter (car s))))
	    (t
	     (dotimes (i (length seq))
	       (funcall result-setter (elt seq i))))))
    result))
	
	