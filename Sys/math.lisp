;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		math.lisp
;;;;	Contents:	Corman Lisp math functions.
;;;;	History:	10/14/96  RGC  Created.
;;;;				12/30/98  RGC  Fixed bugs in INTEGER-DECODE-FLOAT, SCALE-FLOAT,
;;;;							   FLOAT-SIGN
;;;;				7/23/01   RGC  Fixed bug in INTEGER-DECODE-FLOAT with argument = 0
;;;;							   (thanks to Gilbert Perfetti)
;;;;                12/19/02  RGC  Incorporated JP Massar's MASK-FIELD and DEPOSIT-FIELD 
;;;;                               implementations.
;;;;

(in-package "COMMON-LISP")

;;;
;;;	Common Lisp PI constant.
;;;
(defconstant pi 3.14159265358979323846d0)

;;;;
;;;; Common Lisp ZEROP function.
;;;;
(defun zerop (x) (= x 0))

;;;;
;;;; Common Lisp PLUSP function.
;;;;
(defun plusp (x) (> x 0))

;;;;
;;;; Common Lisp MINUSP function.
;;;;
(defun minusp (x) (< x 0))

;;;;
;;;; Common Lisp EVENP function.
;;;;
(defun evenp (x) (= (mod (the integer x) 2) 0))

;;;;
;;;; Common Lisp ODDP function.
;;;;
(defun oddp (x) (/= (mod (the integer x) 2) 0))

;;;
;;;	The following defstruct implements the common lisp functions
;;;		BYTE
;;;		BYTE-SIZE
;;;		BYTE-POSITION
;;;
;;; prints as #< BYTE-SPECIFIER size 100 position 200 >
(defstruct (byte
		(:constructor byte (size position))
		(:print-function print-byte-specifier))
	size
	position)

(defun print-byte-specifier (byte-spec stream depth)
	(declare (ignore depth))
	(format stream "#< BYTE-SPECIFIER size ~A position ~A >" 
		(byte-size byte-spec) 
		(byte-position byte-spec)))

(pl:defasm fixnum-integer-length (num)
	{
		push	ebp
		mov		ebp, esp
		mov		eax, [ebp + ARGS_OFFSET]
		test	eax, eax
		je		:exit
		jg		:t1
		add		eax, 8			;negate number
		je		:exit
		neg		eax
	:t1							;eax = positive integer
		xor		edx, edx
		shr		eax, 3			;untagged integer
		test	eax, #xffff0000
		je		:t2
		add		edx, 16
		shr		eax, 16
	:t2
		test	eax, #xff00
		je		:t3
		add		edx, 8
		shr		eax, 8
	:t3
		test	eax, #xf0
		je		:t4
		add		edx, 4
		shr		eax, 4
	:t4
		test	eax, #x0c
		je		:t5
		add		edx, 2
		shr		eax, 2
	:t5
		test	eax, #x02
		je		:t6
		add		edx, 1
	:t6
		mov		eax, edx
		inc		eax
		shl		eax, 3
	:exit
		mov		ecx, 1
		pop		ebp
		ret
	})

;;;
;;;	Common Lisp INTEGER-LENGTH function.
;;;
(defun integer-length (num)
	(cond
		((fixnump num) (fixnum-integer-length num))
		((bignump num) (bignum-integer-length num))
		(t (error "Not an integer: ~A" num))))

;;;
;;;	%MAKE-SINGLE-FLOAT function
;;;	Uses the bits of the passed integer to create a float.
;;;
(pl:defasm %make-single-float (num)
	{
		push	ebp
		mov		ebp, esp
		mov		ecx, 0
		callf	cl::alloc-single-float
		mov		edx, [ebp + ARGS_OFFSET]
		test	edx, 7
		jne		:bignum
		shr		edx, 3
		mov		[eax + (uvector-offset cl::single-float-offset)], edx
		jmp		:exit
	:bignum
		mov		ecx, [edx + (uvector-offset cl::bignum-first-cell-offset)]
		mov		[eax + (uvector-offset cl::single-float-offset)], ecx
	:exit
		mov		ecx, 1
		pop		ebp
		ret
	})

;;;
;;;	%MAKE-DOUBLE-FLOAT function
;;;	Uses the bits of the passed integer to create a float.
;;;
(pl:defasm %make-double-float (num)
	{
		push	ebp
		mov		ebp, esp
		mov		ecx, 0
		callf	cl::alloc-double-float
		mov		edx, [ebp + ARGS_OFFSET]
		test	edx, 7
		jne		:bignum
		shr		edx, 3
		mov		[eax + (uvector-offset cl::double-float-offset)], edx
		xor		edx, edx
		mov		[eax + (uvector-offset (+ cl::double-float-offset 1))], edx
		jmp		:exit
	:bignum
		mov		ecx, [edx + (uvector-offset cl::bignum-first-cell-offset)]
		mov		[eax + (uvector-offset cl::double-float-offset)], ecx
		mov		ecx, [edx + (uvector-offset (+ cl::bignum-first-cell-offset 1))]
		mov		[eax + (uvector-offset (+ cl::double-float-offset 1))], ecx
	:exit
		mov		ecx, 1
		pop		ebp
		ret
	})

;;;
;;;	%MAKE-SHORT-FLOAT function
;;;	Uses the bits of the passed integer to create a float.
;;;
(pl:defasm %make-short-float (num)
	{
		push	ebp
		mov		ebp, esp
		mov		eax, [ebp + ARGS_OFFSET]
		test	eax, 7
		jne		:bignum
		begin-atomic
		shr		eax, 1
		or		eax, 3
		end-atomic
		jmp		short :exit
	:bignum
		begin-atomic
		mov		eax, [eax + (uvector-offset cl::bignum-first-cell-offset)]
		shl		eax, 2
		or		eax, 3
		end-atomic
	:exit
		mov		ecx, 1
		pop		ebp
		ret
	})

;;;
;;;	%SINGLE-FLOAT-BITS function
;;;	Returns the bits of the passed single float as an integer.
;;;
(pl:defasm %single-float-bits (float)
	{
		push	ebp
		mov		ebp, esp
		mov		edx, [ebp + ARGS_OFFSET]
		mov		eax, [edx + (uvector-offset cl::single-float-offset)]
		test	eax, #xe0000000
		jne		:bignum
		shl		eax, 3
		jmp		:exit
	:bignum
		push	8
		mov		ecx, 1
		callf	cl::alloc-bignum
		add		esp, 4
		mov		edx, [ebp + ARGS_OFFSET]
		mov		ecx, [edx + (uvector-offset cl::single-float-offset)]
		mov		[eax + (uvector-offset cl::bignum-first-cell-offset)], ecx
	:exit
		mov		ecx, 1
		pop		ebp
		ret
	})

;;;
;;;	%DOUBLE-FLOAT-BITS function
;;;	Returns the bits of the passed float as an integer.
;;;
(pl:defasm %double-float-bits (float)
	{
		push	ebp
		mov		ebp, esp
		mov		edx, [ebp + ARGS_OFFSET]
		mov		eax, [edx + (uvector-offset cl::double-float-offset)]
		mov		ecx, [edx + (uvector-offset (+ 1 cl::double-float-offset))] 
		test	eax, #xe0000000
		jne		:bignum
		test	ecx, ecx
		jne		:bignum
		shl		eax, 3
		jmp		:exit
	:bignum
		push	16
		mov		ecx, 1
		callf	cl::alloc-bignum
		add		esp, 4
		mov		edx, [ebp + ARGS_OFFSET]
		mov		ecx, [edx + (uvector-offset cl::double-float-offset)]
		mov		[eax + (uvector-offset cl::bignum-first-cell-offset)], ecx
		mov		ecx, [edx + (uvector-offset (+ 1 cl::double-float-offset))]
		mov		[eax + (uvector-offset (+ cl::bignum-first-cell-offset 1))], ecx
	:exit
		mov		ecx, 1
		pop		ebp
		ret
	})

(pl:defasm fixnum-shift (int shift)
	{
		push	ebp
		mov		ebp, esp
		mov		eax, [ebp + (+ ARGS_OFFSET 4)]		;eax = fixnum
		mov		ecx, [ebp + ARGS_OFFSET]		;eax = shift amount
		test	ecx, ecx			;shift < 0?
		jge		:shiftleft
		neg		ecx
		shr		ecx, 3
		cmp		ecx, 32
		jl		:t1
		mov		eax, 0
		jmp		:exit 
	:t1
		sar		eax, cl
		and		eax, -8
		jmp		:exit
	:shiftleft
		shr		ecx, 3
		jmp		:t2
	:loop
		shl		eax, 1
		jo		:bignumshift
	:t2
		dec		ecx
		jge		:loop
		jmp		:exit
	:bignumshift
		mov		eax, [ebp + (+ ARGS_OFFSET 4)]		;eax = fixnum
		push	eax
		callp	cl::%fixnum-to-bignum
		add		esp, 4
		mov		ecx, [ebp + ARGS_OFFSET]		;eax = shift amount
		push	eax
		push	ecx
		mov		ecx, 2
		callf	cl::bignum-shift
		add		esp, 8
	:exit
		mov		ecx, 1
		pop		ebp
		ret
	})

;;;
;;;	%SHORT-FLOAT-BITS function
;;;	Returns the bits of the passed short float as an integer.
;;;
(pl:defasm %short-float-bits (float)
	{
		push	ebp
		mov		ebp, esp
		mov		eax, [ebp + ARGS_OFFSET]
		test	eax, #xc0000000
		jne		:bignum
		begin-atomic
		shr		eax, 2
		shl		eax, 3
		end-atomic
		jmp		short :exit
	:bignum
		push	8
		mov		ecx, 1
		callf	cl::alloc-bignum
		add		esp, 4
		mov		edx, [ebp + ARGS_OFFSET]
		begin-atomic
		shr		edx, 2
		mov		[eax + (uvector-offset cl::bignum-first-cell-offset)], edx
		end-atomic
	:exit
		mov		ecx, 1
		pop		ebp
		ret
	})

(defconstant most-positive-short-float 				(%make-short-float  #x1fdfffff))
(defconstant least-positive-short-float 			(%make-short-float  #x00000001))
(defconstant least-positive-normalized-short-float 	(%make-short-float  #x00200000))
(defconstant most-positive-single-float 			(%make-single-float #x7f7fffff))
(defconstant least-positive-single-float 			(%make-single-float #x00000001))
(defconstant least-positive-normalized-single-float (%make-single-float #x00800000))
(defconstant most-positive-double-float 			(%make-double-float #x7fefffffffffffff))
(defconstant least-positive-double-float 			(%make-double-float #x0000000000000001))
(defconstant least-positive-normalized-double-float (%make-double-float #x0010000000000000))
(defconstant most-positive-long-float 				most-positive-double-float)
(defconstant least-positive-long-float 				least-positive-double-float)
(defconstant least-positive-normalized-long-float 	least-positive-normalized-double-float)

(defconstant most-negative-short-float 				(%make-short-float  #x3fdfffff))
(defconstant least-negative-short-float 			(%make-short-float  #x20000001))
(defconstant least-negative-normalized-short-float 	(%make-short-float  #x20200000))
(defconstant most-negative-single-float 			(%make-single-float #xff7fffff))
(defconstant least-negative-single-float 			(%make-single-float #x80000001))
(defconstant least-negative-normalized-single-float (%make-single-float #x80800000))
(defconstant most-negative-double-float 			(%make-double-float #xffefffffffffffff))
(defconstant least-negative-double-float 			(%make-double-float #x8000000000000001))
(defconstant least-negative-normalized-double-float (%make-double-float #x8010000000000000))
(defconstant most-negative-long-float 				most-negative-double-float)
(defconstant least-negative-long-float 				least-negative-double-float)
(defconstant least-negative-normalized-long-float 	least-negative-normalized-double-float)
	
;;;
;;;	Common Lisp ASH function.
;;;
(defun ash (int shift)
	(unless (fixnump shift)
		(error "Invalid shift amount: ~A" shift))
	(if (fixnump int)
		(fixnum-shift int shift)
		(if (bignump int)
			(bignum-shift int shift))))


;;;
;;;	Common Lisp DPB function.
;;;
(defun dpb (newbyte bytespec integer)
	(let ((mask (ash 
					(- (ash 1 (byte-size bytespec)) 1) 
					(byte-position bytespec))))
		(logior 
			(logand integer (lognot mask))
			(logand (ash newbyte (byte-position bytespec)) mask))))

;;;
;;;	Common Lisp MASK-FIELD function.
;;;
;;; The hyperspec says
;;; (mask-field bs n) ==  (logand n (dpb -1 bs 0))
;;;
(defun mask-field (bytespec integer)
    (logand integer (dpb -1 bytespec 0)))

;;;
;;;	Common Lisp DEPOSIT-FIELD function.
;;;
;;; CLtL II gives a sort of implememtation on p. 364, though
;;; the discussion is for a constant bytespec, computing a mask
;;; at compile time.
;;;
;;; It says if you compute a mask = (deposit-field -1 bytespec 0)
;;; then 
;;; (deposit-field newbyte bytespec integer) =
;;; (logior (logand newbyte mask) (logand integer (lognot mask)))
;;;
;;; Now, (deposit-field -1 bytespec 0) = (mask-field bytespec -1)
;;;
(defun deposit-field (newbyte bytespec integer)
    (let ((mask (mask-field bytespec -1)))
        (logior (logand newbyte mask) (logand integer (lognot mask)))))
			
;;;
;;;	Common Lisp REM function.
;;;
(defun rem (number divisor)
	(multiple-value-bind (value remainder)
		(truncate number divisor)
		(declare (ignore value))
		remainder))

;;;
;;;	Common Lisp MOD function.
;;;
(defun mod (number divisor)
	(multiple-value-bind (value remainder)
		(floor number divisor)
		(declare (ignore value))
		remainder))

;;;
;;;	Common Lisp ABS function.
;;;
(defun abs (x) 
	(if (complexp x)
		(sqrt (+ (expt (realpart x) 2) (expt (imagpart x) 2)))
		(if (minusp x) (- x) x)))

;;;
;;;	Common Lisp CONJUGATE function.
;;;
(defun conjugate (z) (complex (realpart z) (- (imagpart z))))

;;;
;;;	Common Lisp LCM function.
;;;
(defun lcm (&rest more-integers) 
	(let ((result 1))
		(if (= result 0) (return-from lcm 0))
		(if (null more-integers)
			(abs result)
			(dolist (n more-integers)
				(if (= n 0)
					(return-from lcm 0)
					(setq result (/ (abs (* result n)) (gcd result n))))))
		result))	

;;;
;;;	Common Lisp SIGNUM function.
;;;
(defun signum (x) (if (zerop x) x (/ x (abs x))))

;;;
;;;	Common Lisp TAN function.
;;;
(defun tan (x) (/ (sin x) (cos x)))

(defconstant imag-one #C(0.0 1.0))

;;;
;;;	Common Lisp CIS function.
;;;
(defun cis (x) (exp (* imag-one x)))

;;;
;;;	Common Lisp FFLOOR function.
;;;
(defun ffloor (number &optional (divisor 1))
	(multiple-value-bind (num div) 
		(floor number divisor)
		(values (float num) div)))

;;;
;;;	Common Lisp FCEILING function.
;;;
(defun fceiling (number &optional (divisor 1))
	(multiple-value-bind (num div) 
		(ceiling number divisor)
		(values (float num) div)))

;;;
;;;	Common Lisp FTRUNCATE function.
;;;
(defun ftruncate (number &optional (divisor 1))
	(multiple-value-bind (num div) 
		(truncate number divisor)
		(values (float num) div)))

;;;
;;;	Common Lisp FROUND function.
;;;
(defun fround (number &optional (divisor 1))
	(multiple-value-bind (num div) 
		(round number divisor)
		(values (float num) div)))

;;;
;;;	Common Lisp ASIN function.
;;; arcsin z = -i log (iz+sqrt(1-z^2))
;;;
(defun asin (number)
	(* (- #C(0 1)) 
	   (log (+ (* #C(0 1) number) (sqrt (- 1 (* number number)))))))

;;;
;;;	Common Lisp ACOS function.
;;; arccos z = <PI>/2- arcsin z
;;;
(defun acos (number)
	(- (/ pi 2) (asin number)))

;;;
;;;	Common Lisp ATAN function.
;;; arctan y = log (1+iy) - log ((1-iy)/(2i))
;;; 
(defun atan (y &optional x)
	(if (and (zerop x) (not (complexp y)))
		(return-from atan
			(cond
				((plusp y) 0)
				((minusp y) pi)
				(t (error "ATAN of ~A is undefined" y)))))
	(if x (setq y (/ y x)))
	(let* ((i #C(0 1))
		   (iy (* i y)))
		(- (log (+ 1 iy))(log (/ (- 1 iy) (* 2 i))))))

;;;
;;;	Common Lisp SINH function.
;;;
(defun sinh (number) (/ (- (exp number) (exp (- number))) 2))

;;;
;;;	Common Lisp COSH function.
;;;
(defun cosh (number) (/ (+ (exp number) (exp (- number))) 2))

;;;
;;;	Common Lisp TANH function.
;;;
(defun tanh (number)
	(/ (- (exp number) (exp (- number))) (+ (exp number) (exp (- number)))))

;;;
;;;	Common Lisp ASINH function.
;;;
(defun asinh (x) (log (+ x (sqrt (+ 1.0 (* x x))))))

;;;
;;;	Common Lisp ACOSH function.
;;;
(defun acosh (x) (log (+ x (* (1+ x) (sqrt (/ (1- x) (1+ x)))))))

;;;
;;;	Common Lisp ATANH function.
;;;
(defun atanh (x)
	(when (or (= x 1.0) (= x -1.0))
		(error "logarithmic singularity" x))
	(log (/ (1+ x) (sqrt (- 1.0 (* x x))))))

;;;
;;;	Common Lisp PHASE function.
;;;
(defun phase (x)
	(if (not (numberp x)) (error "Not a number: ~A" x))
	(cond
		((complexp x) 	(atan (imagpart x) (realpart x)))
		((minusp x)		pi)
		(t				0.0)))	 
			
;;;
;;;	Common Lisp LOGTEST function.
;;;
(defun logtest (x y) (not (zerop (logand x y))))

(pl:defasm cl::short-float-exponent-bits (float)
	{
		push	ebp
		mov		ebp, esp
		mov		eax, [ebp + ARGS_OFFSET]
		begin-atomic
		shl		eax, 1
		shr		eax, 24
		shl		eax, 3
		end-atomic
		pop		ebp
		ret
	})

(pl:defasm cl::short-float-sign-bit (float)
	{
		push	ebp
		mov		ebp, esp
		mov		eax, [ebp + ARGS_OFFSET]
		shr		eax, 31
		shl		eax, 3
		pop		ebp
		ret
	})

(pl:defasm cl::short-float-mantissa-bits (float)
	{
		push	ebp
		mov		ebp, esp
		mov		eax, [ebp + ARGS_OFFSET]
		begin-atomic
		shl		eax, 9
		shr		eax, 11
		shl		eax, 3
		end-atomic
		pop		ebp
		ret
	})
 
(defun float-get-exponent-bits (float)
	(if (short-float-p float)
		(short-float-exponent-bits float)
		(if (single-float-p float)
			(logand (ash (%single-float-bits float) -23) #xff)
			(logand (ash (%double-float-bits float) -52) #x7ff))))

(defun float-get-sign-bit (float)
	(if (short-float-p float)
		(short-float-sign-bit float)
		(if (single-float-p float)
			(ash (%single-float-bits float) -31)
			(ash (%double-float-bits float) -63))))

(defun float-get-mantissa-bits (float)
	(if (short-float-p float)
		(+ #x200000 (short-float-mantissa-bits float))
		(if (single-float-p float)
			(+ #x800000 (logand (%single-float-bits float) #x7fffff))
			(+ #x10000000000000 (logand (%double-float-bits float) #xfffffffffffff)))))

(defun float-set-exponent-bits (float exp-bits)
	(if (short-float-p float)
		(let* ((bits (%short-float-bits float)))
			(%make-short-float (+ (logand #xe01fffff bits)(ash exp-bits 21))))
		(if (single-float-p float)
			(let* ((bits (%single-float-bits float)))
				(%make-single-float (+ (logand #x807fffff bits)(ash exp-bits 23))))
			(let* ((bits (%double-float-bits float)))
				(%make-double-float (+ (logand #x800fffffffffffff bits)(ash exp-bits 52)))))))		 	

(defun %create-double-float-from-bits (mantissa exponent sign)
	(let ((n (+ 
				(if (= sign -1) (ash 1 63) 0) 
				(ash (+ exponent #x3ff) 52) 
				(- mantissa #x10000000000000))))	
		(%make-double-float n)))

(defun %create-single-float-from-bits (mantissa exponent sign)
	(let ((n (+ 
				(if (= sign -1) (ash 1 31) 0) 
				(ash (+ exponent #x7f) 23) 
				(- mantissa #x800000))))	
		(%make-single-float n)))

(defun %create-short-float-from-bits (mantissa exponent sign)
	(let ((n (+ 
				(if (= sign -1) (ash 1 29) 0) 
				(ash (+ exponent #x7f) 21) 
				(- mantissa #x200000))))	
		(%make-short-float n)))

;;;
;;;	Common Lisp DECODE-FLOAT function.
;;;
(defun decode-float (float)
	(unless (floatp float) (error "Not a float number: ~A" float))
	(if (= float 0.0)
		(values 0.0 0 1.0)
		(let* ((exp-bits (float-get-exponent-bits float))
			   (normalize (if (double-float-p float) #x3fe #x7e))
			   (delta (- exp-bits normalize)))
			(if (= float 0)
				(values 0.0 0 1.0)
				(values (float-set-exponent-bits float normalize)
				    delta
				    (if (< float 0.0) -1.0 1.0))))))

;;;
;;;	Common Lisp FLOAT-RADIX function.
;;;	All floats use radix 2.
;;;
(defun float-radix (float) 	
	(unless (floatp float) (error "Not a float number: ~A" float))
	2)

;;;
;;;	Common Lisp FLOAT-SIGN function.
;;;
(defun float-sign (float-1 &optional float-2) 
	(unless (floatp float-1) (error "Not a float number: ~A" float-1))
	(setf float-2 
		(if float-2 
			(abs float-2)
			(setf float-2 (float 1 float-1))))
	(let ((sign-bit (float-get-sign-bit float-1)))
		(if (zerop sign-bit)
			float-2
			(- float-2))))
			

;;;
;;;	Common Lisp FLOAT-DIGITS function.
;;;
(defun float-digits (float) 
	(unless (floatp float) (error "Not a float number: ~A" float))
	(if (short-float-p float)
		22
		(if (single-float-p float)
			24
			53)))

;;;
;;;	Common Lisp INTEGER-DECODE-FLOAT function.
;;;
(defun integer-decode-float (float)
	(unless (floatp float) (error "Not a float number: ~A" float))
	(if (= float 0.0)
		(values 0 0 1)
		(let* ((mantissa (float-get-mantissa-bits float))
			   (exponent (if (short-float-p float)
							(- (- (float-get-exponent-bits float) #x7f) 21)
							(if (single-float-p float)
								(- (- (float-get-exponent-bits float) #x7f) 23)
								(- (- (float-get-exponent-bits float) #x3ff) 52))))
			   (sign (if (= (float-get-sign-bit float) 0) 1 -1)))
			(values mantissa exponent sign))))

;;;
;;;	Common Lisp FLOAT-PRECISION function.
;;;
(defun float-precision (float) 
	(unless (floatp float) (error "Not a float number: ~A" float))
	(if (zerop float)
		(return-from float-precision 0))
	(multiple-value-bind (mantissa expt sign)
		(integer-decode-float float)
		(declare (ignore sign))
		(if (short-float-p float)
			(if (= expt -148)
				(integer-length (- mantissa #x200000))
				22)
			(if (single-float-p float)
				(if (= expt -150)
					(integer-length (- mantissa #x800000))
					24)		
				(if (= expt -1075)
					(integer-length (- mantissa #x10000000000000))
					53)))))

;;;
;;;	Common Lisp SCALE-FLOAT function.
;;;
(defun scale-float (float scale)
	(unless (floatp float) (error "Not a float number: ~A" float))
	(multiple-value-bind (mantissa expt sign)
		(integer-decode-float float)
		(cond ((double-float-p float)
			   (incf expt (+ scale 52))
			   (if (< expt #x-3fe)
					(error "Floating point underflow"))
			   (if (> expt #x3ff)
					(error "Floating point overflow"))
			   (%create-double-float-from-bits mantissa expt sign))
			  ((single-float-p float)
			   (incf expt (+ scale 23))
			   (if (< expt #x-7e)
					(error "Floating point underflow"))
			   (if (> expt #x7f)
					(error "Floating point overflow"))
			   (%create-single-float-from-bits mantissa expt sign))
			  ((short-float-p float)
			   (incf expt (+ scale 21))
			   (if (< expt #x-7e)
					(error "Floating point underflow"))
			   (if (> expt #x7f)
					(error "Floating point overflow"))
			   (%create-short-float-from-bits mantissa expt sign)))))
;;;;
;;;	Common Lisp RATIONAL function.
;;;
(defun rational (number)
	(unless (realp number)(error "Not a real number: ~A" number))
	(if (rationalp number) 
		(return-from rational number))
	(multiple-value-bind (mantissa exp sign)
		(integer-decode-float number)
		(cond ((double-float-p number)
			   (* sign (/ (ash mantissa (+ 52 exp)) #x10000000000000)))
		  	  ((single-float-p number)
			   (* sign (/ (ash mantissa (+ 24 exp)) #x1000000)))
		  	  ((short-float-p number)
			   (* sign (/ (ash mantissa (+ 22 exp)) #x400000))))))

;;;
;;;	Common Lisp RATIONALIZE function.
;;;
(defun rationalize (number)
	;; need to work on this one
	(rational number))

;;;
;;; Common Lisp ISQRT function.
;;; Redefines kernel function which was incorrect on some large bignums.
;;;
(defun isqrt (n)
    (declare (optimize (speed 3)(safety 0)))
    (declare (integer n))
    (unless (integerp n)
        (error "Non-integer passed to ISQRT: ~a" n))
    (unless (>= n 0)
        (error "Negative integer passed to ISQRT: ~A" n))
    (if (< n 2)
        n
        (do* ((x n)
              (r 0 (ash r -1))
              (len (integer-length n))
              (m (ash 1 (if (evenp len) len (+ 1 len)))(ash m -2))
              (nr (+ r m)(+ r m)))
            ((= m 0) r)
            (declare (integer x r nr m)(fixnum len))
            (when (<= nr x)
                (decf x nr)
                (setf r (+ nr m))))))


