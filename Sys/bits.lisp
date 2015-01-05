;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		bits.lisp
;;;;	Contents:	Corman Lisp bit manipulation functions.
;;;;	History:	3/8/00  RGC  Created.
;;;;
(in-package :common-lisp)

(defun bit-array-p (a)
	(and (arrayp a)(eq (array-element-type a) 'bit)))

;; TDE: Optimize these for bit manipulation.
;;
(defun bit (bit-array &rest subscripts)
	(unless (eq (array-element-type bit-array) 'bit)
		(error "Not a bit array"))
	(apply 'aref bit-array subscripts))

(defun (setf bit) (bit bit-array &rest subscripts)
	(unless (eq (array-element-type bit-array) 'bit)
		(error "Not a bit array"))
	(apply (fdefinition '(setf aref)) bit bit-array subscripts))

(defun sbit (bit-array &rest subscripts)
	(unless (eq (array-element-type bit-array) 'bit)
		(error "Not a bit array"))
	(apply 'aref bit-array subscripts))

(defun (setf sbit) (bit bit-array &rest subscripts)
	(unless (eq (array-element-type bit-array) 'bit)
		(error "Not a bit array"))
	(apply (fdefinition '(setf aref)) bit bit-array subscripts))

;;;
;;;	Non standard SVBIT and (SETF SVBIT) functions.
;;;
(defun ccl::svbit (bit-array index)
	(unless (eq (array-element-type bit-array) 'bit)
		(error "Not a bit array"))
	(unless (fixnump index) (error "Invalid index"))
	(x86::%svbit bit-array index))

(defun (setf ccl::svbit) (bit bit-array index)
	(unless (eq (array-element-type bit-array) 'bit)
		(error "Not a bit array"))
	(unless (fixnump index) (error "Invalid index"))
	(unless (or (= bit 0)(= bit 1))
		(error "Not a bit: ~S" bit))
	(setf (x86::%svbit bit-array index) bit))

(define-compiler-macro sbit (array &rest indices)
	(if (= (length indices) 1)
		(if (or (cl::compiler-check-args-num)(cl::compiler-check-types))
			`(ccl::svbit ,array ,(first indices))
			`(x86::%svbit ,array ,(first indices)))
		`(bit ,array ,@indices)))

(define-compiler-macro cl::|(SETF SBIT)| (value array &rest indices)
	(if (= (length indices) 1)
		(if (or (cl::compiler-check-args-num)(cl::compiler-check-types))
			`(setf (ccl::svbit ,array ,(first indices)) ,value)
			`(setf (x86::%svbit ,array ,(first indices)) ,value))
		`(setf (bit ,array ,@indices) ,value)))

(defun equal-rank-and-dimensions (a1 a2)
	(equal (array-dimensions a1) (array-dimensions a2)))

(x86:defasm %bit-and (src1 src2 dest)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		push	ebx
		mov		eax, [ebp + (+ ARGS_OFFSET 8)]	;; eax = src1
		mov		edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = src2
		mov		edi, [ebp + (+ ARGS_OFFSET 0)]	;; edi = dest
		mov		ecx, [eax + (uvector-offset 0)] 	
		shr		ecx, 8							
		dec		ecx								;; ecx = number of 8-byte blocks
		begin-atomic
	:loop
		dec		ecx
		jl		short :done
		mov		ebx, [eax + ecx*8 + (uvector-offset 2)]
		and		ebx, [edx + ecx*8 + (uvector-offset 2)]
		mov		[edi + ecx*8 + (uvector-offset 2)], ebx
		mov		ebx, [eax + ecx*8 + (uvector-offset 3)]
		and		ebx, [edx + ecx*8 + (uvector-offset 3)]
		mov		[edi + ecx*8 + (uvector-offset 3)], ebx
		jmp		short :loop
	:done
		end-atomic
		mov		ecx, 1
		pop		ebx
		pop		edi
		pop		ebp
		ret
	})		

(x86:defasm %bit-andc1 (src1 src2 dest)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		push	ebx
		mov		eax, [ebp + (+ ARGS_OFFSET 8)]	;; eax = src1
		mov		edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = src2
		mov		edi, [ebp + (+ ARGS_OFFSET 0)]	;; edi = dest
		mov		ecx, [eax + (uvector-offset 0)] 	
		shr		ecx, 8							
		dec		ecx								;; ecx = number of 8-byte blocks
		begin-atomic
	:loop
		dec		ecx
		jl		short :done
		mov		ebx, [eax + ecx*8 + (uvector-offset 2)]
		not		ebx
		and		ebx, [edx + ecx*8 + (uvector-offset 2)]
		mov		[edi + ecx*8 + (uvector-offset 2)], ebx
		mov		ebx, [eax + ecx*8 + (uvector-offset 3)]
		not		ebx
		and		ebx, [edx + ecx*8 + (uvector-offset 3)]
		mov		[edi + ecx*8 + (uvector-offset 3)], ebx
		jmp		short :loop
	:done
		end-atomic
		mov		ecx, 1
		pop		ebx
		pop		edi
		pop		ebp
		ret
	})		

(x86:defasm %bit-andc2 (src1 src2 dest)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		push	ebx
		mov		eax, [ebp + (+ ARGS_OFFSET 8)]	;; eax = src1
		mov		edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = src2
		mov		edi, [ebp + (+ ARGS_OFFSET 0)]	;; edi = dest
		mov		ecx, [eax + (uvector-offset 0)] 	
		shr		ecx, 8							
		dec		ecx								;; ecx = number of 8-byte blocks
		begin-atomic
	:loop
		dec		ecx
		jl		short :done
		mov		ebx, [edx + ecx*8 + (uvector-offset 2)]
		not		ebx
		and		ebx, [eax + ecx*8 + (uvector-offset 2)]
		mov		[edi + ecx*8 + (uvector-offset 2)], ebx
		mov		ebx, [edx + ecx*8 + (uvector-offset 3)]
		not		ebx
		and		ebx, [eax + ecx*8 + (uvector-offset 3)]
		mov		[edi + ecx*8 + (uvector-offset 3)], ebx
		jmp		short :loop
	:done
		end-atomic
		mov		ecx, 1
		pop		ebx
		pop		edi
		pop		ebp
		ret
	})		

(x86:defasm %bit-eqv (src1 src2 dest)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		push	ebx
		mov		eax, [ebp + (+ ARGS_OFFSET 8)]	;; eax = src1
		mov		edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = src2
		mov		edi, [ebp + (+ ARGS_OFFSET 0)]	;; edi = dest
		mov		ecx, [eax + (uvector-offset 0)] 	
		shr		ecx, 8							
		dec		ecx								;; ecx = number of 8-byte blocks
		begin-atomic
	:loop
		dec		ecx
		jl		short :done
		mov		ebx, [eax + ecx*8 + (uvector-offset 2)]
		xor		ebx, [edx + ecx*8 + (uvector-offset 2)]
		not		ebx
		mov		[edi + ecx*8 + (uvector-offset 2)], ebx
		mov		ebx, [eax + ecx*8 + (uvector-offset 3)]
		xor		ebx, [edx + ecx*8 + (uvector-offset 3)]
		not		ebx
		mov		[edi + ecx*8 + (uvector-offset 3)], ebx
		jmp		short :loop
	:done
		end-atomic
		mov		ecx, 1
		pop		ebx
		pop		edi
		pop		ebp
		ret
	})		

(x86:defasm %bit-ior (src1 src2 dest)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		push	ebx
		mov		eax, [ebp + (+ ARGS_OFFSET 8)]	;; eax = src1
		mov		edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = src2
		mov		edi, [ebp + (+ ARGS_OFFSET 0)]	;; edi = dest
		mov		ecx, [eax + (uvector-offset 0)] 	
		shr		ecx, 8							
		dec		ecx								;; ecx = number of 8-byte blocks
		begin-atomic
	:loop
		dec		ecx
		jl		short :done
		mov		ebx, [eax + ecx*8 + (uvector-offset 2)]
		or		ebx, [edx + ecx*8 + (uvector-offset 2)]
		mov		[edi + ecx*8 + (uvector-offset 2)], ebx
		mov		ebx, [eax + ecx*8 + (uvector-offset 3)]
		or		ebx, [edx + ecx*8 + (uvector-offset 3)]
		mov		[edi + ecx*8 + (uvector-offset 3)], ebx
		jmp		short :loop
	:done
		end-atomic
		mov		ecx, 1
		pop		ebx
		pop		edi
		pop		ebp
		ret
	})		

(x86:defasm %bit-nand (src1 src2 dest)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		push	ebx
		mov		eax, [ebp + (+ ARGS_OFFSET 8)]	;; eax = src1
		mov		edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = src2
		mov		edi, [ebp + (+ ARGS_OFFSET 0)]	;; edi = dest
		mov		ecx, [eax + (uvector-offset 0)] 	
		shr		ecx, 8							
		dec		ecx								;; ecx = number of 8-byte blocks
		begin-atomic
	:loop
		dec		ecx
		jl		short :done
		mov		ebx, [eax + ecx*8 + (uvector-offset 2)]
		and		ebx, [edx + ecx*8 + (uvector-offset 2)]
		not		ebx
		mov		[edi + ecx*8 + (uvector-offset 2)], ebx
		mov		ebx, [eax + ecx*8 + (uvector-offset 3)]
		and		ebx, [edx + ecx*8 + (uvector-offset 3)]
		not		ebx
		mov		[edi + ecx*8 + (uvector-offset 3)], ebx
		jmp		short :loop
	:done
		end-atomic
		mov		ecx, 1
		pop		ebx
		pop		edi
		pop		ebp
		ret
	})		

(x86:defasm %bit-nor (src1 src2 dest)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		push	ebx
		mov		eax, [ebp + (+ ARGS_OFFSET 8)]	;; eax = src1
		mov		edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = src2
		mov		edi, [ebp + (+ ARGS_OFFSET 0)]	;; edi = dest
		mov		ecx, [eax + (uvector-offset 0)] 	
		shr		ecx, 8							
		dec		ecx								;; ecx = number of 8-byte blocks
		begin-atomic
	:loop
		dec		ecx
		jl		short :done
		mov		ebx, [eax + ecx*8 + (uvector-offset 2)]
		or		ebx, [edx + ecx*8 + (uvector-offset 2)]
		not		ebx
		mov		[edi + ecx*8 + (uvector-offset 2)], ebx
		mov		ebx, [eax + ecx*8 + (uvector-offset 3)]
		or		ebx, [edx + ecx*8 + (uvector-offset 3)]
		not		ebx
		mov		[edi + ecx*8 + (uvector-offset 3)], ebx
		jmp		short :loop
	:done
		end-atomic
		mov		ecx, 1
		pop		ebx
		pop		edi
		pop		ebp
		ret
	})		

(x86:defasm %bit-orc1 (src1 src2 dest)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		push	ebx
		mov		eax, [ebp + (+ ARGS_OFFSET 8)]	;; eax = src1
		mov		edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = src2
		mov		edi, [ebp + (+ ARGS_OFFSET 0)]	;; edi = dest
		mov		ecx, [eax + (uvector-offset 0)] 	
		shr		ecx, 8							
		dec		ecx								;; ecx = number of 8-byte blocks
		begin-atomic
	:loop
		dec		ecx
		jl		short :done
		mov		ebx, [eax + ecx*8 + (uvector-offset 2)]
		not		ebx
		or		ebx, [edx + ecx*8 + (uvector-offset 2)]
		mov		[edi + ecx*8 + (uvector-offset 2)], ebx
		mov		ebx, [eax + ecx*8 + (uvector-offset 3)]
		not		ebx
		or		ebx, [edx + ecx*8 + (uvector-offset 3)]
		mov		[edi + ecx*8 + (uvector-offset 3)], ebx
		jmp		short :loop
	:done
		end-atomic
		mov		ecx, 1
		pop		ebx
		pop		edi
		pop		ebp
		ret
	})		

(x86:defasm %bit-orc2 (src1 src2 dest)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		push	ebx
		mov		eax, [ebp + (+ ARGS_OFFSET 8)]	;; eax = src1
		mov		edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = src2
		mov		edi, [ebp + (+ ARGS_OFFSET 0)]	;; edi = dest
		mov		ecx, [eax + (uvector-offset 0)] 	
		shr		ecx, 8							
		dec		ecx								;; ecx = number of 8-byte blocks
		begin-atomic
	:loop
		dec		ecx
		jl		short :done
		mov		ebx, [edx + ecx*8 + (uvector-offset 2)]
		not		ebx
		or		ebx, [eax + ecx*8 + (uvector-offset 2)]
		mov		[edi + ecx*8 + (uvector-offset 2)], ebx
		mov		ebx, [edx + ecx*8 + (uvector-offset 3)]
		not		ebx
		or		ebx, [eax + ecx*8 + (uvector-offset 3)]
		mov		[edi + ecx*8 + (uvector-offset 3)], ebx
		jmp		short :loop
	:done
		end-atomic
		mov		ecx, 1
		pop		ebx
		pop		edi
		pop		ebp
		ret
	})		

(x86:defasm %bit-xor (src1 src2 dest)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		push	ebx
		mov		eax, [ebp + (+ ARGS_OFFSET 8)]	;; eax = src1
		mov		edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = src2
		mov		edi, [ebp + (+ ARGS_OFFSET 0)]	;; edi = dest
		mov		ecx, [eax + (uvector-offset 0)] 	
		shr		ecx, 8							
		dec		ecx								;; ecx = number of 8-byte blocks
		begin-atomic
	:loop
		dec		ecx
		jl		short :done
		mov		ebx, [eax + ecx*8 + (uvector-offset 2)]
		xor		ebx, [edx + ecx*8 + (uvector-offset 2)]
		mov		[edi + ecx*8 + (uvector-offset 2)], ebx
		mov		ebx, [eax + ecx*8 + (uvector-offset 3)]
		xor		ebx, [edx + ecx*8 + (uvector-offset 3)]
		mov		[edi + ecx*8 + (uvector-offset 3)], ebx
		jmp		short :loop
	:done
		end-atomic
		mov		ecx, 1
		pop		ebx
		pop		edi
		pop		ebp
		ret
	})		

(x86:defasm %bit-not (src dest)
	{
		push	ebp
		mov		ebp, esp
		push	ebx
		mov		eax, [ebp + (+ ARGS_OFFSET 4)]	;; eax = src
		mov		edx, [ebp + (+ ARGS_OFFSET 0)]	;; edx = dest
		mov		ecx, [eax + (uvector-offset 0)] 	
		shr		ecx, 8							
		dec		ecx								;; ecx = number of 8-byte blocks
		begin-atomic
	:loop
		dec		ecx
		jl		short :done
		mov		ebx, [eax + ecx*8 + (uvector-offset 2)]
		not		ebx
		mov		[edx + ecx*8 + (uvector-offset 2)], ebx
		mov		ebx, [eax + ecx*8 + (uvector-offset 3)]
		not		ebx
		mov		[edx + ecx*8 + (uvector-offset 3)], ebx
		jmp		short :loop
	:done
		end-atomic
		mov		ecx, 1
		pop		ebx
		pop		ebp
		ret
	})		

(defun initialize-simple-bit-vector (vec element)
	(declare (ignore vec))	;; only referenced from assembler
	(unless (and (fixnump element) (>= element 0) (< element 2))
		(error "Cannot initialize array because the initializer is not of type BIT: ~S" element))
	{{
		mov		eax, [ebp + (+ ARGS_OFFSET 4)]	;; eax = vector
		mov		ecx, [ebp + (+ ARGS_OFFSET 0)]	;; ecx = element
		mov		edx, [eax + (uvector-offset 0)] ;; edx = header
		begin-atomic							;; untagged edx
		shr		edx, 8							;; edx = size of uvector / 8
		or		ecx, ecx
		jz		short :t1
		mov		ecx, -1
	:t1
		dec		edx
		cmp		edx, 0		
		je		short :done
		mov		[eax + edx*8 + (uvector-offset 0)], ecx
		mov		[eax + edx*8 + (uvector-offset 1)], ecx
		jmp		short :t1
	:done
		end-atomic
		mov		ecx, 1
	}})
			
(defun check-2-bit-arrays (bit-array1 bit-array2)
	(unless (bit-array-p bit-array1)
		(error "Not a bit array: ~S" bit-array1))
	(unless (bit-array-p bit-array2)
		(error "Not a bit array: ~S" bit-array2))
	(unless (equal-rank-and-dimensions bit-array1 bit-array2)
		(error "Bit arrays do not have the same rank and dimensions: ~S, ~S" 
			bit-array1 bit-array2)))

(defun check-1-bit-array (bit-array)
	(unless (bit-array-p bit-array)
		(error "Not a bit array: ~S" bit-array)))

(defun get-dest-bit-array (arg bit-array1)
	(cond ((null arg)
		   (make-array (array-dimensions bit-array1) :element-type 'bit))
		  ((eq arg t) bit-array1)
		  (t (unless (and (bit-array-p arg)(equal-rank-and-dimensions bit-array1 arg))
				(error "Result bit-array des not have the same type or dimensions as operand"))
			arg)))

(defun bit-array-vector (bit-array)
	(if (adjustable-array-p bit-array)
		(uref bit-array adjustable-array-vector-offset)
		bit-array))		
;;;
;;;	Common Lisp BIT-AND function.
;;;
(defun bit-and (bit-array1 bit-array2 &optional opt-arg)
	(check-2-bit-arrays bit-array1 bit-array2)
	(let ((result (get-dest-bit-array opt-arg bit-array1)))
		(%bit-and
			(bit-array-vector bit-array1)
			(bit-array-vector bit-array2)
			(bit-array-vector result))
		result))
		 	 
;;;
;;;	Common Lisp BIT-ANDC1 function.
;;;
(defun bit-andc1 (bit-array1 bit-array2 &optional opt-arg)
	(check-2-bit-arrays bit-array1 bit-array2)
	(let ((result (get-dest-bit-array opt-arg bit-array1)))
		(%bit-andc1
			(bit-array-vector bit-array1)
			(bit-array-vector bit-array2)
			(bit-array-vector result))
		result))

;;;
;;;	Common Lisp BIT-ANDC2 function.
;;;
(defun bit-andc2 (bit-array1 bit-array2 &optional opt-arg)
	(check-2-bit-arrays bit-array1 bit-array2)
	(let ((result (get-dest-bit-array opt-arg bit-array1)))
		(%bit-andc2
			(bit-array-vector bit-array1)
			(bit-array-vector bit-array2)
			(bit-array-vector result))
		result))

;;;
;;;	Common Lisp BIT-EQV function.
;;;
(defun bit-eqv (bit-array1 bit-array2 &optional opt-arg)
	(check-2-bit-arrays bit-array1 bit-array2)
	(let ((result (get-dest-bit-array opt-arg bit-array1)))
		(%bit-eqv
			(bit-array-vector bit-array1)
			(bit-array-vector bit-array2)
			(bit-array-vector result))
		result))

;;;
;;;	Common Lisp BIT-IOR function.
;;;
(defun bit-ior (bit-array1 bit-array2 &optional opt-arg)
	(check-2-bit-arrays bit-array1 bit-array2)
	(let ((result (get-dest-bit-array opt-arg bit-array1)))
		(%bit-ior
			(bit-array-vector bit-array1)
			(bit-array-vector bit-array2)
			(bit-array-vector result))
		result))

;;;
;;;	Common Lisp BIT-NAND function.
;;;
(defun bit-nand (bit-array1 bit-array2 &optional opt-arg)
	(check-2-bit-arrays bit-array1 bit-array2)
	(let ((result (get-dest-bit-array opt-arg bit-array1)))
		(%bit-nand
			(bit-array-vector bit-array1)
			(bit-array-vector bit-array2)
			(bit-array-vector result))
		result))

;;;
;;;	Common Lisp BIT-NOR function.
;;;
(defun bit-nor (bit-array1 bit-array2 &optional opt-arg)
	(check-2-bit-arrays bit-array1 bit-array2)
	(let ((result (get-dest-bit-array opt-arg bit-array1)))
		(%bit-nor
			(bit-array-vector bit-array1)
			(bit-array-vector bit-array2)
			(bit-array-vector result))
		result))

;;;
;;;	Common Lisp BIT-ORC1 function.
;;;
(defun bit-orc1 (bit-array1 bit-array2 &optional opt-arg)
	(check-2-bit-arrays bit-array1 bit-array2)
	(let ((result (get-dest-bit-array opt-arg bit-array1)))
		(%bit-orc1
			(bit-array-vector bit-array1)
			(bit-array-vector bit-array2)
			(bit-array-vector result))
		result))

;;;
;;;	Common Lisp BIT-ORC2 function.
;;;
(defun bit-orc2 (bit-array1 bit-array2 &optional opt-arg)
	(check-2-bit-arrays bit-array1 bit-array2)
	(let ((result (get-dest-bit-array opt-arg bit-array1)))
		(%bit-orc2
			(bit-array-vector bit-array1)
			(bit-array-vector bit-array2)
			(bit-array-vector result))
		result))

;;;
;;;	Common Lisp BIT-XOR function.
;;;
(defun bit-xor (bit-array1 bit-array2 &optional opt-arg)
	(check-2-bit-arrays bit-array1 bit-array2)
	(let ((result (get-dest-bit-array opt-arg bit-array1)))
		(%bit-xor
			(bit-array-vector bit-array1)
			(bit-array-vector bit-array2)
			(bit-array-vector result))
		result))

;;;
;;;	Common Lisp BIT-NOT function.
;;;
(defun bit-not (bit-array &optional opt-arg)
	(check-1-bit-array bit-array)
	(let ((result (get-dest-bit-array opt-arg bit-array)))
		(%bit-not
			(bit-array-vector bit-array)
			(bit-array-vector result))
		result))

;;;
;;; Patch for ENDP function.
;;; (-RGC We should move this somewhere else)
;;;
(defun endp (list)
	(unless (listp list)
		(error 'type-error :datum list :expected-type 'list))
	(null list))