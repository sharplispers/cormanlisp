;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		array.lisp
;;;;	Contents:	Functions to deal with arrays.
;;;;	History:	9/26/96  RGC  Created.
;;;;				2/17/00  RGC  Incorporated Chris Double's displaced 
;;;;							  array fix (when :DISPLACED-TO is used,
;;;;							  the array elements will not be initialized).
;;;;				4/6/01   RGC  Added new type specifiers for simple-array, vector.
;;;;                12/18/02 RGC  Added ARRAY-IN-BOUNDS-P implementation (based on JP Massar's).
;;;;

(defconstant array-rank-limit				8)
(defconstant array-dimension-limit			#x1000000)	;; 16 meg
(defconstant array-total-size-limit			#x1000000)

(defconstant array-bounds-msg "Array subscript ~A out of bounds: ~A")
(defun check-array-bounds (index max rank)
	(unless (and (fixnump index) (>= index 0) (< index max))
		(error array-bounds-msg rank index)))

(defun array-rank (array)
	(unless (arrayp array) (error "Not an array: ~A" array))
	(if (= (uvector-type-bits array) uvector-array-tag)
		(uref array adjustable-array-dimensions-offset)
		1))

(defun check-array (array dims)
	(unless (eq (array-rank array) dims)
		(error "Expected an array of ~A dimensions: ~A" dims array)))
		
(defun array-dimensions (array) 
	(do ((i (1- (array-rank array)) (1- i))
		 (dims nil))
		((< i 0) dims)
		(push (array-dimension array i) dims)))

(register-setf-function 'row-major-aref '|(SETF ROW-MAJOR-AREF)|)
;;(defsetf row-major-aref |(SETF ROW-MAJOR-AREF)|)
				
;;;; These aref functions do bounds checking on the subscripts,
;;;; but assume the passed object is an array of the correct rank.

(defun aref-rank-0 (array)
	(check-array array 0) 
	(row-major-aref array 0))

(defun (setf aref-rank-0) (value array)
	(check-array array 0) 
	(setf (row-major-aref array 0) value))

(defun aref-rank-1 (array i0) 
	(unless (eq (array-rank array) 1)
		(error "Expected a one-dimensional array: ~S" array)) 
	(let ((dim0 (array-dimension array 0)))
		(check-array-bounds i0 dim0 0)
		(row-major-aref array i0)))

(defun (setf aref-rank-1) (value array i0) 
	(unless (eq (array-rank array) 1)
		(error "Expected a one-dimensional array: ~S" array)) 
	(let ((dim0 (array-dimension array 0)))
		(check-array-bounds i0 dim0 0)
		(setf (row-major-aref array i0) value)))

(defun aref-rank-2 (array i0 i1) 
	(unless (eq (array-rank array) 2)
		(error "Expected a two-dimensional array: ~S" array)) 
	(let ((dim0 (array-dimension array 0))
		  (dim1 (array-dimension array 1)))
		(check-array-bounds i0 dim0 0)
		(check-array-bounds i1 dim1 1)
		(row-major-aref array (+ (* i0 dim1) 
								 i1))))

(defun (setf aref-rank-2) (value array i0 i1) 
	(unless (eq (array-rank array) 2)
		(error "Expected a two-dimensional array: ~S" array)) 
	(let ((dim0 (array-dimension array 0))
		  (dim1 (array-dimension array 1)))
		(check-array-bounds i0 dim0 0)
		(check-array-bounds i1 dim1 1)
		(setf (row-major-aref array (+ (* i0 dim1) i1)) value)))

(defun aref-rank-3 (array i0 i1 i2) 
	(check-array array 3) 
	(let ((dim0 (array-dimension array 0))
		  (dim1 (array-dimension array 1))
		  (dim2 (array-dimension array 2)))
		(check-array-bounds i0 dim0 0)
		(check-array-bounds i1 dim1 1)
		(check-array-bounds i2 dim2 2)
		(row-major-aref array (+ (* i0 dim1 dim2) 
								 (* i1 dim2) 
								 i2))))

(defun (setf aref-rank-3) (value array i0 i1 i2) 
	(check-array array 3) 
	(let ((dim0 (array-dimension array 0))
		  (dim1 (array-dimension array 1))
		  (dim2 (array-dimension array 2)))
		(check-array-bounds i0 dim0 0)
		(check-array-bounds i1 dim1 1)
		(check-array-bounds i2 dim2 2)
		(setf (row-major-aref array (+ (* i0 dim1 dim2) 
								 (* i1 dim2) 
								 i2)) value)))

(defun aref-rank-4 (array i0 i1 i2 i3) 
	(check-array array 4) 
	(let ((dim0 (array-dimension array 0))
		  (dim1 (array-dimension array 1))
		  (dim2 (array-dimension array 2))
		  (dim3 (array-dimension array 3)))
		(check-array-bounds i0 dim0 0)
		(check-array-bounds i1 dim1 1)
		(check-array-bounds i2 dim2 2)
		(check-array-bounds i3 dim3 3)
		(row-major-aref array (+ (* i0 dim1 dim2 dim3)
								 (* i1 dim2 dim3) 
								 (* i2 dim3) 
								 i3))))

(defun (setf aref-rank-4) (value array i0 i1 i2 i3) 
	(check-array array 4) 
	(let ((dim0 (array-dimension array 0))
		  (dim1 (array-dimension array 1))
		  (dim2 (array-dimension array 2))
		  (dim3 (array-dimension array 3)))
		(check-array-bounds i0 dim0 0)
		(check-array-bounds i1 dim1 1)
		(check-array-bounds i2 dim2 2)
		(check-array-bounds i3 dim3 3)
		(setf (row-major-aref array (+ (* i0 dim1 dim2 dim3)
								 (* i1 dim2 dim3) 
								 (* i2 dim3) 
								 i3)) value)))

(defun aref-rank-5 (array i0 i1 i2 i3 i4) 
	(check-array array 5) 
	(let ((dim0 (array-dimension array 0))
		  (dim1 (array-dimension array 1))
		  (dim2 (array-dimension array 2))
		  (dim3 (array-dimension array 3))
		  (dim4 (array-dimension array 4)))
		(check-array-bounds i0 dim0 0)
		(check-array-bounds i1 dim1 1)
		(check-array-bounds i2 dim2 2)
		(check-array-bounds i3 dim3 3)
		(check-array-bounds i4 dim4 4)
		(row-major-aref array (+ (* i0 dim1 dim2 dim3 dim4)
								 (* i1 dim2 dim3 dim4) 
								 (* i2 dim3 dim4) 
								 (* i3 dim4)
								 i4))))

(defun (setf aref-rank-5) (value array i0 i1 i2 i3 i4) 
	(check-array array 5) 
	(let ((dim0 (array-dimension array 0))
		  (dim1 (array-dimension array 1))
		  (dim2 (array-dimension array 2))
		  (dim3 (array-dimension array 3))
		  (dim4 (array-dimension array 4)))
		(check-array-bounds i0 dim0 0)
		(check-array-bounds i1 dim1 1)
		(check-array-bounds i2 dim2 2)
		(check-array-bounds i3 dim3 3)
		(check-array-bounds i4 dim4 4)
		(setf (row-major-aref array (+ (* i0 dim1 dim2 dim3 dim4)
								 (* i1 dim2 dim3 dim4) 
								 (* i2 dim3 dim4) 
								 (* i3 dim4)
								 i4)) value)))

(defun aref-rank-6 (array i0 i1 i2 i3 i4 i5) 
	(check-array array 6) 
	(let ((dim0 (array-dimension array 0))
		  (dim1 (array-dimension array 1))
		  (dim2 (array-dimension array 2))
		  (dim3 (array-dimension array 3))
		  (dim4 (array-dimension array 4))
		  (dim5 (array-dimension array 5)))
		(check-array-bounds i0 dim0 0)
		(check-array-bounds i1 dim1 1)
		(check-array-bounds i2 dim2 2)
		(check-array-bounds i3 dim3 3)
		(check-array-bounds i4 dim4 4)
		(check-array-bounds i5 dim5 5)
		(row-major-aref array (+ (* i0 dim1 dim2 dim3 dim4 dim5)
								 (* i1 dim2 dim3 dim4 dim5) 
								 (* i2 dim3 dim4 dim5) 
								 (* i3 dim4 dim5)
								 (* i4 dim5)
								 i5))))

(defun (setf aref-rank-6) (value array i0 i1 i2 i3 i4 i5) 
	(check-array array 6) 
	(let ((dim0 (array-dimension array 0))
		  (dim1 (array-dimension array 1))
		  (dim2 (array-dimension array 2))
		  (dim3 (array-dimension array 3))
		  (dim4 (array-dimension array 4))
		  (dim5 (array-dimension array 5)))
		(check-array-bounds i0 dim0 0)
		(check-array-bounds i1 dim1 1)
		(check-array-bounds i2 dim2 2)
		(check-array-bounds i3 dim3 3)
		(check-array-bounds i4 dim4 4)
		(check-array-bounds i5 dim5 5)
		(setf (row-major-aref array (+ (* i0 dim1 dim2 dim3 dim4 dim5)
								 (* i1 dim2 dim3 dim4 dim5) 
								 (* i2 dim3 dim4 dim5) 
								 (* i3 dim4 dim5)
								 (* i4 dim5)
								 i5)) value)))

(defun aref-rank-7 (array i0 i1 i2 i3 i4 i5 i6) 
	(check-array array 7) 
	(let ((dim0 (array-dimension array 0))
		  (dim1 (array-dimension array 1))
		  (dim2 (array-dimension array 2))
		  (dim3 (array-dimension array 3))
		  (dim4 (array-dimension array 4))
		  (dim5 (array-dimension array 5))
		  (dim6 (array-dimension array 6)))
		(check-array-bounds i0 dim0 0)
		(check-array-bounds i1 dim1 1)
		(check-array-bounds i2 dim2 2)
		(check-array-bounds i3 dim3 3)
		(check-array-bounds i4 dim4 4)
		(check-array-bounds i5 dim5 5)
		(check-array-bounds i6 dim6 6)
		(row-major-aref array (+ (* i0 dim1 dim2 dim3 dim4 dim5 dim6)
								 (* i1 dim2 dim3 dim4 dim5 dim6) 
								 (* i2 dim3 dim4 dim5 dim6) 
								 (* i3 dim4 dim5 dim6)
								 (* i4 dim5 dim6)
								 (* i5 dim6)
								 i6))))

(defun (setf aref-rank-7) (value array i0 i1 i2 i3 i4 i5 i6) 
	(check-array array 7) 
	(let ((dim0 (array-dimension array 0))
		  (dim1 (array-dimension array 1))
		  (dim2 (array-dimension array 2))
		  (dim3 (array-dimension array 3))
		  (dim4 (array-dimension array 4))
		  (dim5 (array-dimension array 5))
		  (dim6 (array-dimension array 6)))
		(check-array-bounds i0 dim0 0)
		(check-array-bounds i1 dim1 1)
		(check-array-bounds i2 dim2 2)
		(check-array-bounds i3 dim3 3)
		(check-array-bounds i4 dim4 4)
		(check-array-bounds i5 dim5 5)
		(check-array-bounds i6 dim6 6)
		(setf (row-major-aref array (+ (* i0 dim1 dim2 dim3 dim4 dim5 dim6)
								 (* i1 dim2 dim3 dim4 dim5 dim6) 
								 (* i2 dim3 dim4 dim5 dim6) 
								 (* i3 dim4 dim5 dim6)
								 (* i4 dim5 dim6)
								 (* i5 dim6)
								 i6)) value)))

(defconstant aref-funcs 
	(make-array 8 
		:initial-contents
		(list
			'aref-rank-0 
	  		'aref-rank-1 
	  		'aref-rank-2 
	  		'aref-rank-3 
	  		'aref-rank-4 
	  		'aref-rank-5 
	  		'aref-rank-6 
	  		'aref-rank-7)))

(defconstant aref-setter-funcs 
	(make-array 8 
		:initial-contents
		(list
			'|(SETF AREF-RANK-0)| 
	  		'|(SETF AREF-RANK-1)| 
	  		'|(SETF AREF-RANK-2)| 
	  		'|(SETF AREF-RANK-3)| 
	  		'|(SETF AREF-RANK-4)| 
	  		'|(SETF AREF-RANK-5)| 
	  		'|(SETF AREF-RANK-6)| 
	  		'|(SETF AREF-RANK-7)|)))

;;;;
;;;;	Common Lisp AREF function.
;;;;
(defun aref (array &rest subscripts)
	(apply (elt aref-funcs (length subscripts)) array subscripts))

(defun (setf aref) (value array &rest subscripts)
	(apply (elt aref-setter-funcs (length subscripts)) value array subscripts))

(defun array-row-major-index (a &rest subscripts)
   (apply #'+ (maplist #'(lambda (x y)
                            (* (car x) (apply #'* (cdr y))))
                       subscripts
                       (array-dimensions a))))

(defun array-total-size (array)
	(unless (arrayp array)
		(error "Not an array: ~A" array))
	(let ((numdims (array-rank array)))
		(cond 
			((= numdims 0) 1)
			((= numdims 1) (array-dimension array 0))
			(t (let ((result 1))
				(dotimes (i numdims)
					(setq result (* result (uref array (+ adjustable-array-dim1-offset i)))))
				result))))) 
			 
#|
;;;;
;;;; Generalized AREF (possibly slower)
;;;;
(defun aref (array &rest subscripts)
	;; check subscripts
	(unless (arrayp array)
		(error "Not an array: ~A" array))
	(let ((num-subscripts (length subscripts))
	      (offset 0)
		  (dims (array-rank array)))
		(unless (= dims num-subscripts)
			(error "AREF expected ~A subscripts, got ~A" 
				dims num-subscripts))

		;; handle case of array with no dimensions
		(if (null subscripts)
			(return-from aref (uref array array-dim1-offset)))

		;; check each index for bounds and calculate the offset
		(do* ((i 0 (+ i 1))
			 (x subscripts (cdr x))
			 (subscript (car x) (car x)))
			((= i dims))
			(let ((dim (array-dimension array i))
				  (factor 1))
				(do ((n (+ i 1) (+ n 1))) 
					((>= n dims)) 
					(setq factor (* factor (array-dimension array n))))

				(if (or (< subscript 0) (>= subscript dim))
					(error "Array subscript ~A out of bounds: ~A"
						i subscript))
				(setq offset (+ offset (* subscript factor)))))
		(%array-element array offset)))
|#

(defun valid-displaced-array (a type num-cells offset)
	(and (arrayp a)
		(eq (array-type a) type)
		(<= (+ offset num-cells)(array-total-size a))))

;;;
;;;	Common Lisp UPGRADED-ARRAY-ELEMENT-TYPE function.
;;;
(defun upgraded-array-element-type (element-type &optional environment)
    (declare (ignore environment))
	(if (member element-type '(bit byte character double-float single-float short-integer))
		element-type
        (if (or (eq element-type 'base-char)(eq element-type 'standard-char))
            'character
            't)))

;;; 
;;; Common Lisp ARRAY-DISPLACEMENT function.
;;;
(defun array-displacement (array)
	(unless (arrayp array)
		(error "Not an array: ~S" array))
	(if (adjustable-array-p array)
		(values (uref array adjustable-array-vector-offset)
			(uref array adjustable-array-displaced-offset))
		(values nil 0)))

;;;
;;;	Common Lisp MAKE-ARRAY function.
;;;
(defun make-array (dimensions 
	&key (element-type t) 
		 (initial-element nil supplied-initial-element)
		 (initial-contents nil supplied-initial-contents)
		 adjustable 
		 fill-pointer 
		 (displaced-to nil supplied-displaced-to) 
		 (displaced-index-offset 0))
	(if (integerp dimensions)
		(setq dimensions (list dimensions)))

	(setq element-type (upgraded-array-element-type element-type))
	
	;; if we are displacing to an already displaced array, add the offsets
	(when (and displaced-to displaced-index-offset (array-displacement displaced-to))
		(incf displaced-index-offset (uref displaced-to adjustable-array-displaced-offset))
		(setf displaced-to (uref displaced-to adjustable-array-vector-offset)))
	
	(let ((array-type 'simple)
		  (num-dimensions (length dimensions))
		  (header-size 0))
        (when (> num-dimensions array-rank-limit)
            (error "Attempted to create an array of ~D dimensions. The maximum number of allowed dimensions is ARRAY-RANK-LIMIT (~D)."
                num-dimensions
                array-rank-limit))
		(when (or adjustable fill-pointer (/= num-dimensions 1) displaced-to)
			(setq array-type 'adjustable))
		(setq header-size (+ adjustable-array-header-min-size num-dimensions -1))

		(let* ((num-cells (apply #'* dimensions))
		 	  a
		  	 vec
		  	 (d dimensions))
            (if (or (> num-cells array-dimension-limit) 
                    (> num-cells array-total-size-limit))
                (error "Array size limit exceeded"))
			(cond
				(displaced-to
				 (unless 
					(valid-displaced-array displaced-to element-type 
						num-cells displaced-index-offset)
				 	(error "Cannot displace-to array ~A" displaced-to))
				 (setq vec displaced-to)
				 (if (adjustable-array-p vec)
					(setq vec (uref vec adjustable-array-vector-offset))))
				((eq element-type 'character)
				 (setq vec (allocate-char-vector num-cells)))
				((eq element-type 'bit)
				 (setq vec (allocate-bit-vector num-cells)))
				((eq element-type 'byte)
				 (setq vec (allocate-byte-vector num-cells)))
				((eq element-type 'double-float)
				 (setq vec (allocate-double-float-vector num-cells)))
				((eq element-type 'single-float)
				 (setq vec (allocate-single-float-vector num-cells)))
				((eq element-type 'short-integer)
				 (setq vec (allocate-short-vector num-cells)))
				((eq element-type 't)
				 (setq vec (allocate-generic-vector num-cells))))
		
			(if (eq array-type 'adjustable)
				(let ()
					(setq a (alloc-uvector header-size uvector-array-tag))
					(setf (uref a adjustable-array-vector-offset) vec)
					(setf (uref a adjustable-array-fill-pointer-offset) -1)
					(setf (uref a adjustable-array-displaced-offset) displaced-index-offset)
					(setf (uref a adjustable-array-dimensions-offset) num-dimensions)
					(dotimes (i num-dimensions)
						(setf (uref a (+ adjustable-array-dim1-offset i)) (car d))
					(setq d (cdr d))))
				(setq a vec))
					 		
			(if supplied-displaced-to
				(if (or supplied-initial-element supplied-initial-contents)
					(error "You cannot initialize the contents of a :DISPLACED-TO array"))
				(cond (supplied-initial-element (array-initialize-element a initial-element))
					  (supplied-initial-contents (array-initialize-contents a initial-contents))
					  (t (if (eq element-type 'character)
							(array-initialize-element a (int-char 32)))))) ;; init char arrays with spaces
                                                                      ;; other types of arrays should already
                                                                      ;; be zero-filled			
			(if fill-pointer
				(progn
					(if (/= num-dimensions 1)
						(error "Cannot declare a fill pointer in a ~A dimensional array"))
					(if (eq fill-pointer t) (setq fill-pointer num-cells))
					(if (or (< fill-pointer 0) (> fill-pointer num-cells))
						(error "Fill pointer out of range"))
					(setf (uref a adjustable-array-fill-pointer-offset) fill-pointer)))
			a)))

(declare-type-specifier array (x specifier)
    (block nil
    	(if (or (symbolp specifier)(not (cdr specifier)))
    		(return (arrayp x)))
    	(let ((element-type (second specifier))
    		  (dimension-spec 
    				(if (>= (length specifier) 3)
    					(third specifier)
    					'*)))
    		(unless (arrayp x)
    			(return nil))
    		(unless 
    			(or 
    				(eq element-type '*)
    				(eq (upgraded-array-element-type element-type) (array-type x)))
    			(return nil))
    		(cond 
    			((eq dimension-spec '*) t)
    			((integerp dimension-spec)(eq (array-rank x) dimension-spec))
    			((listp dimension-spec)
    			 (do* ((i 0 (+ i 1))
    				   (p dimension-spec (cdr p))
    				   (dim (car p)(car p))
    				   (adims (array-dimensions x)(cdr adims)))
    				  ((endp p) t)
    				(unless (or (eq dim '*)(eq (car adims) dim))
    					(return nil))))
    			(t (error "Invalid array specifier: ~A" specifier))))))

;; reuse array specifier for simple-array--we make no distinction
(install-type-specifier 'simple-array (get 'array 'type-discriminator))

(declare-type-specifier vector (x specifier)
    (block nil
    	(if (or (symbolp specifier)(not (cdr specifier)))
    		(return (vectorp x)))
    	(let ((element-type (second specifier))
    		  (dimension-spec 
    				(if (>= (length specifier) 3)
    					(third specifier)
    					'*)))
    		(unless (vectorp x)
    			(return nil))
    		(unless 
    			(or 
    				(eq element-type '*)
    				(eq (upgraded-array-element-type element-type) (array-type x)))
    			(return nil))
    		(cond 
    			((eq dimension-spec '*) t)
    			((integerp dimension-spec)(eq (length x) dimension-spec))
    			(t (error "Invalid array specifier: ~A" specifier))))))

;;;
;;;	Common Lisp VECTOR function
;;;
(defun vector (&rest objects)
	(make-array (length objects) :initial-contents objects))

;;;
;;; Common Lisp ARRAY-IN-BOUNDS-P function
;;;
(defun array-in-bounds-p (array &rest subscripts)
    (unless (arrayp array) (error "Non-array given to ARRAY-IN-BOUNDS-P: ~A" array))
    (let ((r (array-rank array)) 
          (sl (length subscripts)))
        (unless (= r sl)
            (error "ARRAY-IN-BOUNDS-P: Array provided has rank ~D, but you provided ~D subscripts" r sl))
        
        (do* ((d 0 (1+ d)) 
              (slist subscripts (cdr slist))
              (s (car slist) (car slist)))
            ((null slist))
            (unless (integerp s)
                (error "Non-integer provided as index for ARRAY-IN-BOUNDS-P: ~A" s))
            (unless (and (not (minusp s)) (< s (array-dimension array d)))
                (return-from array-in-bounds-p nil))))
    t)
			
			