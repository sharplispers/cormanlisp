;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		arrays.lisp
;;;;	Contents:	Functions to deal with arrays.
;;;;	History:	7/9/98  RGC  Created.
;;;;				1/31/01 RGC Added Chris Double's ADJUST-ARRAY patch.
;;;;

(in-package :common-lisp)

(defun copy-rank-0 (old-array new-array)
	(setf (aref new-array) (aref old-array)))

(defun copy-rank-1 (old-array new-array)
	(let ((old-dim0 (array-dimension old-array 0))
		  (new-dim0 (array-dimension new-array 0)))
		(dotimes (i (min old-dim0 new-dim0))
			(setf (aref new-array i) (aref old-array i)))))	

(defun copy-rank-2 (old-array new-array)
	(let ((old-dim0 (array-dimension old-array 0))
		  (new-dim0 (array-dimension new-array 0))
		  (old-dim1 (array-dimension old-array 1))
		  (new-dim1 (array-dimension new-array 1)))
		(dotimes (i0 (min old-dim0 new-dim0))
			(dotimes (i1 (min old-dim1 new-dim1))
				(setf (aref new-array i0 i1) (aref old-array i0 i1))))))	

(defun copy-rank-3 (old-array new-array)
	(let ((old-dim0 (array-dimension old-array 0))
		  (new-dim0 (array-dimension new-array 0))
		  (old-dim1 (array-dimension old-array 1))
		  (new-dim1 (array-dimension new-array 1))
		  (old-dim2 (array-dimension old-array 2))
		  (new-dim2 (array-dimension new-array 2)))
		(dotimes (i0 (min old-dim0 new-dim0))
			(dotimes (i1 (min old-dim1 new-dim1))
				(dotimes (i2 (min old-dim2 new-dim2))
					(setf (aref new-array i0 i1 i2) (aref old-array i0 i1 i2)))))))	

(defun copy-rank-4 (old-array new-array)
	(let ((old-dim0 (array-dimension old-array 0))
		  (new-dim0 (array-dimension new-array 0))
		  (old-dim1 (array-dimension old-array 1))
		  (new-dim1 (array-dimension new-array 1))
		  (old-dim2 (array-dimension old-array 2))
		  (new-dim2 (array-dimension new-array 2))
		  (old-dim3 (array-dimension old-array 3))
		  (new-dim3 (array-dimension new-array 3)))
		(dotimes (i0 (min old-dim0 new-dim0))
			(dotimes (i1 (min old-dim1 new-dim1))
				(dotimes (i2 (min old-dim2 new-dim2))
					(dotimes (i3 (min old-dim3 new-dim3))
						(setf (aref new-array i0 i1 i2 i3) (aref old-array i0 i1 i2 i3))))))))	

(defun copy-rank-5 (old-array new-array)
	(let ((old-dim0 (array-dimension old-array 0))
		  (new-dim0 (array-dimension new-array 0))
		  (old-dim1 (array-dimension old-array 1))
		  (new-dim1 (array-dimension new-array 1))
		  (old-dim2 (array-dimension old-array 2))
		  (new-dim2 (array-dimension new-array 2))
		  (old-dim3 (array-dimension old-array 3))
		  (new-dim3 (array-dimension new-array 3))
		  (old-dim4 (array-dimension old-array 4))
		  (new-dim4 (array-dimension new-array 4)))
		(dotimes (i0 (min old-dim0 new-dim0))
			(dotimes (i1 (min old-dim1 new-dim1))
				(dotimes (i2 (min old-dim2 new-dim2))
					(dotimes (i3 (min old-dim3 new-dim3))
						(dotimes (i4 (min old-dim4 new-dim4))
							(setf (aref new-array i0 i1 i2 i3 i4) 
								(aref old-array i0 i1 i2 i3 i4)))))))))	

(defun copy-rank-6 (old-array new-array)
	(let ((old-dim0 (array-dimension old-array 0))
		  (new-dim0 (array-dimension new-array 0))
		  (old-dim1 (array-dimension old-array 1))
		  (new-dim1 (array-dimension new-array 1))
		  (old-dim2 (array-dimension old-array 2))
		  (new-dim2 (array-dimension new-array 2))
		  (old-dim3 (array-dimension old-array 3))
		  (new-dim3 (array-dimension new-array 3))
		  (old-dim4 (array-dimension old-array 4))
		  (new-dim4 (array-dimension new-array 4))
		  (old-dim5 (array-dimension old-array 5))
		  (new-dim5 (array-dimension new-array 5)))
		(dotimes (i0 (min old-dim0 new-dim0))
			(dotimes (i1 (min old-dim1 new-dim1))
				(dotimes (i2 (min old-dim2 new-dim2))
					(dotimes (i3 (min old-dim3 new-dim3))
						(dotimes (i4 (min old-dim4 new-dim4))
							(dotimes (i5 (min old-dim5 new-dim5))
								(setf (aref new-array i0 i1 i2 i3 i4 i5) 
									(aref old-array i0 i1 i2 i3 i4 i5))))))))))	

(defun copy-rank-7 (old-array new-array)
	(let ((old-dim0 (array-dimension old-array 0))
		  (new-dim0 (array-dimension new-array 0))
		  (old-dim1 (array-dimension old-array 1))
		  (new-dim1 (array-dimension new-array 1))
		  (old-dim2 (array-dimension old-array 2))
		  (new-dim2 (array-dimension new-array 2))
		  (old-dim3 (array-dimension old-array 3))
		  (new-dim3 (array-dimension new-array 3))
		  (old-dim4 (array-dimension old-array 4))
		  (new-dim4 (array-dimension new-array 4))
		  (old-dim5 (array-dimension old-array 5))
		  (new-dim5 (array-dimension new-array 5))
		  (old-dim6 (array-dimension old-array 6))
		  (new-dim6 (array-dimension new-array 6)))
		(dotimes (i0 (min old-dim0 new-dim0))
			(dotimes (i1 (min old-dim1 new-dim1))
				(dotimes (i2 (min old-dim2 new-dim2))
					(dotimes (i3 (min old-dim3 new-dim3))
						(dotimes (i4 (min old-dim4 new-dim4))
							(dotimes (i5 (min old-dim5 new-dim5))
								(dotimes (i6 (min old-dim6 new-dim6))
									(setf (aref new-array i0 i1 i2 i3 i4 i5 i6) 
										(aref old-array i0 i1 i2 i3 i4 i5 i6)))))))))))	

(defconstant adjust-array-copy-funcs 
	(make-array 8 
		:initial-contents
		(list
			'copy-rank-0 
	  		'copy-rank-1 
	  		'copy-rank-2 
	  		'copy-rank-3 
	  		'copy-rank-4 
	  		'copy-rank-5 
	  		'copy-rank-6 
	  		'copy-rank-7)))

(defun copy-array-elements (old-array new-array)
	(funcall (elt adjust-array-copy-funcs (array-rank old-array)) 
		old-array new-array))

;; non-adjustable arrays are always of rank 1
(defun adjust-non-adjustable-array (array new-dimensions all-keys)
	(let ((new-array (apply 'make-array new-dimensions 
						:adjustable t all-keys)))
		(unless (member ':initial-contents all-keys) (copy-rank-1 array new-array))
		new-array))
 
;;;
;;;	Common Lisp ADJUST-ARRAY function.
;;;	Currently the keys are not implemented.
;;;
(defun adjust-array (array new-dimensions &rest all-keys 
			&key (element-type (array-element-type array)) 
				 (initial-element nil) 
				 initial-contents 
				 fill-pointer 
				 displaced-to 
				 (displaced-index-offset 0))
	(declare (ignore displaced-index-offset displaced-to initial-element))
	(check-type array array)
	(unless (listp new-dimensions)
		(setf new-dimensions (list new-dimensions)))
	(unless (= (array-rank array)(length new-dimensions))
		(error "Wrong number of dimensions passed to ADJUST-ARRAY"))
	(unless (adjustable-array-p array)
		(return-from adjust-array (adjust-non-adjustable-array array new-dimensions all-keys)))
	(let ((new-array (apply 'make-array new-dimensions :adjustable t 
			:element-type element-type all-keys)))
		(unless initial-contents (copy-array-elements array new-array))

		;; now copy the array data structure itself to the old array
		(setf (uref array adjustable-array-vector-offset)
			(uref new-array adjustable-array-vector-offset))
		(if fill-pointer
			(setf (uref array adjustable-array-fill-pointer-offset)
				(uref new-array adjustable-array-fill-pointer-offset)))
		(setf (uref array adjustable-array-displaced-offset)
			(uref new-array adjustable-array-displaced-offset))
		(dotimes (i (array-rank array))
			(setf (uref array (+ i adjustable-array-dim1-offset))
				(uref new-array (+ i adjustable-array-dim1-offset))))
		array))

(defconstant min-vector-extension 16)

(defun extend-vector (vector extension)
	(if (null extension)
		(setq extension min-vector-extension)
		(setq extension (max min-vector-extension extension)))
	(setq extension (max extension (array-dimension vector 0)))
	(adjust-array vector (+ extension (array-dimension vector 0))))

;;;
;;;	Common Lisp VECTOR-PUSH-EXTEND function.
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
;;;	This overrides the kernel function for initializing arrays
;;;
(defun array-initialize-contents (array contents)
    (let* ((dims (array-rank array))
		   (index 0))
        (when (= dims 0)
            (setf (aref array) contents)
			(return-from array-initialize-contents))	
		(labels ((init-array-dimension (sequence depth)
                    (let* ((seq-length (length sequence))
                           (num-dims (array-rank array)))
                        (if (/= seq-length (array-dimension array (- num-dims depth 1)))
                            (error "Invalid initializer form, incorrect number of items in dimension: ~A" sequence))
                        (if (= depth 0)
                            (if (listp sequence)        ;; optimize for list handling
                                (dolist (x sequence)
                                    (setf (row-major-aref array index) x)
                                    (incf index))                                
                                (dotimes (i seq-length)
                                    (setf (row-major-aref array index) (elt sequence i))
                                    (incf index)))
                            (if (listp sequence)        ;; optimize for list handling
                                (dolist (x sequence)
                                    (init-array-dimension x (1- depth)))
                                (dotimes (i seq-length)
                                    (init-array-dimension (elt sequence i) (1- depth))))))))
            (init-array-dimension contents (1- dims)))))

(defun initialize-simple-generic-vector (vec element)
	(let ((length (uref vec 1)))
		(dotimes (i length)
			(setf (aref vec i) element))))

(defun initialize-simple-character-vector (vec element)
	(unless (characterp element)
		(error "Cannot initialize array. Not a character: ~S" element))
	(let ((length (uref vec 1)))
		(dotimes (i length)
			(setf (aref vec i) element))))

(defun initialize-simple-byte-vector (vec element)
	(unless (and (fixnump element) (>= element 0) (< element 256))
		(error "Cannot initialize array because the initializer is not of type (FIXNUM 0 255): ~S" element))
	(let ((length (uref vec 1)))
		(dotimes (i length)
			(setf (aref vec i) element))))

(defun initialize-simple-bit-vector (vec element)
	(unless (and (fixnump element) (>= element 0) (< element 2))
		(error "Cannot initialize array because the initializer is not of type BIT: ~S" element))
	(let ((length (uref vec 1)))
		(dotimes (i length)
			(setf (aref vec i) element))))

(defun initialize-simple-double-float-vector (vec element)
	(unless (double-float-p element)
		(if (numberp element)
			(setf element (float element 0d0))
			(error "Cannot initialize array. The initial element ~S cannot be coerced to a double-float" element)))
	(let ((length (uref vec 1)))
		(dotimes (i length)
			(setf (aref vec i) element))))

(defun initialize-simple-single-float-vector (vec element)
	(unless (single-float-p element)
		(if (realp element)
			(setf element (float element 0f0))
			(error "Cannot initialize array. The initial element ~S cannot be coerced to a single-float" element)))
	(let ((length (uref vec 1)))
		(dotimes (i length)
			(setf (aref vec i) element))))

(defun initialize-simple-short-integer-vector (vec element)
	(unless (and (realp element) (>= element -32768) (<= element 32767))
		(error "Cannot initialize array. The initial element ~S cannot be coerced to a short-integer" element))
	(let ((length (uref vec 1)))
		(dotimes (i length)
			(setf (aref vec i) element))))

(defun array-initialize-element (array element)
	(let ((vec array))
		(if (adjustable-array-p vec)
			(setq vec (uref vec adjustable-array-vector-offset)))
		(let ((type (array-type vec)))
			(cond
				((eq type 't) (initialize-simple-generic-vector vec element))
				((eq type 'character) (initialize-simple-character-vector vec element))
				((eq type 'byte) (initialize-simple-byte-vector vec element))
				((eq type 'bit) (initialize-simple-bit-vector vec element))
				((eq type 'double-float) (initialize-simple-double-float-vector vec element))
				((eq type 'single-float) (initialize-simple-single-float-vector vec element))
				((eq type 'short-integer) (initialize-simple-short-integer-vector vec element))))))
