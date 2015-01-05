;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		math-ops.lisp
;;;;	Contents:	Lisp number handling routines.
;;;;	History:	03/08/06  RGC  Created.
;;;;

(in-package :x86)

(defconstant FixnumID       0)
(defconstant BignumID		1)
(defconstant RatioID		2)
(defconstant ShortFloatID	3)
(defconstant SingleFloatID	4)
(defconstant DoubleFloatID	5)
(defconstant ComplexID		6)
(defconstant NotNumberID	7)
(defconstant NumMaxIDs		8)

(setf num-type-table
    (vector
	NotNumberID    ;; FunctionType					0
	NotNumberID	   ;; KFunctionType				    1
	NotNumberID	   ;; StructureType				    2
	NotNumberID	   ;; ArrayType					    3
	NotNumberID	   ;; SymbolType					4
	NotNumberID	   ;; StreamType					5
	DoubleFloatID  ;; DoubleFloatType				6
	NotNumberID	   ;; PackageType					7
	NotNumberID	   ;; 	HashtableType				8
	NotNumberID	   ;; 	ForeignType					9
	NotNumberID	   ;; 	CompiledCodeType			10
	NotNumberID	   ;; 	ReadTableType				11
	ComplexID      ;; 	ComplexType					12
	RatioID		   ;; 	RatioType					13
	BignumID	   ;; 	BignumType					14
	NotNumberID	   ;; 	ForeignHeapType				15
	NotNumberID	   ;; 	WeakPointerType				16
	NotNumberID	   ;; 	SimpleVectorType			17
	NotNumberID	   ;; 	SimpleCharVectorType		18
	NotNumberID	   ;; 	SimpleByteVectorType		19
	NotNumberID	   ;; 	SimpleShortVectorType		20
	NotNumberID	   ;; 	SimpleDoubleFloatVectorType 21
	NotNumberID	   ;; 	SimpleBitVectorType			22
	NotNumberID	   ;; 	SimpleSingleFloatVectorType 23
	SingleFloatID  ;; 	SingleFloatType				24
	NotNumberID	   ;; 	CLOSInstanceType			25
	NotNumberID	   ;; 	ForeignStackType			26
	NotNumberID	   ;; 	ForeignStackEndType			27
	NotNumberID	   ;; 	Unused						28
	NotNumberID	   ;; 	Unused						29
	NotNumberID	   ;; 	Unused						30
	NotNumberID	   ;; 	Unused						31
    ))
#| 
(defasm num-type (n)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + ARGS_OFFSET]
        mov     edx, eax
        and     eax, 7
        je      short :done
        cmp     eax, uvector-tag
        jne     short :next1
		mov 	edx, [edx - uvector-tag]
    :check-double 
		cmp 	dl, (tag-byte uvector-double-float-tag)
        jne     short :check-single
        mov     eax, (* DoubleFloatID 8)
        jmp     short :done
    :check-single  
		cmp 	dl, (tag-byte uvector-single-float-tag)
        jne     short :check-bignum
        mov     eax, (* SingleFloatID 8)
        jmp     short :done            
     :check-bignum        
		cmp 	dl, (tag-byte uvector-bignum-tag)
        jne     short :check-ratio
        mov     eax, (* BignumID 8)
        jmp     short :done 
     :check-ratio        
		cmp 	dl, (tag-byte uvector-ratio-tag)
        jne     short :check-complex
        mov     eax, (* RatioID 8)
        jmp     short :done                    
     :check-complex        
		cmp 	dl, (tag-byte uvector-complex-tag)
        jne     short :check-none
        mov     eax, (* ComplexID 8)
        jmp     short :done   
    :check-none                 
        mov     eax, (* NotNumberID 8)
        jmp     short :done
    :next1
        and     eax, 3
        cmp     eax, 3
        jne     short :check-none
        mov     eax, (* ShortFloatID 8)
    :done   
        pop     ebp
        ret
    })
|#

(defasm num-type (n)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + ARGS_OFFSET]
        mov     edx, eax
        and     eax, 7      
        je      short :done         ;; fixnum
        cmp     eax, uvector-tag
        je      short :uvector
        and     eax, 3
        cmp     eax, 3
        je      short :short-float
        mov     eax, (* NotNumberID 8)
        jmp     short :done
    :short-float
        mov     eax, (* ShortFloatID 8)
        jmp     short :done
    :uvector 
		mov 	al, [edx - uvector-tag]
        shr     eax, 3
        mov     edx, num-type-table
        mov     eax, [edx + eax*4 + (uvector-offset 2)]   ;; eax = result
    :done   
        pop     ebp
        ret
    })

(defasm alloc-8-byte-uvector (type)
	{
		push	ebp
		mov		ebp, esp
	:try1
	begin-atomic
		mov		eax, [esi + (* thread-heap-qv-offset 4)]
		add		eax, uvector-tag
		lea		edx, [eax + (- 8 uvector-tag)]
		cmp		edx, [esi + (* thread-heap-end-qv-offset 4)]
		jle		:done
    end-atomic
		callp	cl::%load-local-heap
	begin-atomic
		jmp		short :try1
	:done
		mov		[esi + (* thread-heap-qv-offset 4)], edx
        mov     edx, [ebp + ARGS_OFFSET]
        and     edx, #xf8
        or      edx, (+ uvector-header-tag #x100)   ;; set length to 1        
        or      [eax + (uvector-offset 0)], edx
    end-atomic
        mov     ecx, 1
		pop		ebp
		ret
	})    

(defasm alloc-16-byte-uvector (type)
	{
		push	ebp
		mov		ebp, esp
	:try1
	begin-atomic
		mov		eax, [esi + (* thread-heap-qv-offset 4)]
		mov     ecx, eax
		and     ecx, #x0fff         ;; check low 12 bits (page offset)
		cmp     ecx, #x0ff8         ;; the block will span a page boundary
		jne     short :next1
    end-atomic
		callp   cl::%alloc-cons     ;; waste 8 bytes
	begin-atomic
		xor     ecx, ecx
		mov     [eax - 4], ecx      ;; clear cells
		mov     [eax], ecx          ;;
		mov		eax, [esi + (* thread-heap-qv-offset 4)]		
	:next1
		add		eax, uvector-tag
		lea		edx, [eax + (- 16 uvector-tag)]
		cmp		edx, [esi + (* thread-heap-end-qv-offset 4)]
		jle		:done
	end-atomic
		callp	cl::%load-local-heap
	begin-atomic
		jmp		short :try1
	:done
		mov		[esi + (* thread-heap-qv-offset 4)], edx
        mov     edx, [ebp + ARGS_OFFSET]
        and     edx, #xf8
        or      edx, (+ uvector-header-tag #x200)   ;; set length to 2        
        or      [eax + (uvector-offset 0)], edx
    end-atomic
        mov     ecx, 1
		pop		ebp
		ret
  	})    

(defasm alloc-24-byte-uvector (type)
	{
		push	ebp
		mov		ebp, esp
	:try1
	begin-atomic
		mov		eax, [esi + (* thread-heap-qv-offset 4)]
		mov     ecx, eax
		and     ecx, #x0ff0             ;; check low 12 bits (page offset)
		cmp     ecx, #x0ff0             ;; the block will span a page boundary
		jne     short :next1
	end-atomic
		callp   cl::%alloc-cons         ;; waste 8 bytes
	begin-atomic
		xor     ecx, ecx
		mov     [eax - 4], ecx          ;; clear cells
		mov     [eax], ecx              ;;
		test    eax, #x8                ;; was that enough?
		jmp     short :next2            ;; waste 8 more bytes
	end-atomic
		callp   cl::%alloc-cons         ;; waste 8 bytes
	begin-atomic
		xor     ecx, ecx
		mov     [eax - 4], ecx          ;; clear cells
		mov     [eax], ecx              ;;
	:next2
		mov		eax, [esi + (* thread-heap-qv-offset 4)]		
	:next1
		add		eax, uvector-tag
		lea		edx, [eax + (- 24 uvector-tag)]
		cmp		edx, [esi + (* thread-heap-end-qv-offset 4)]
		jle		:done
	end-atomic
		callp	cl::%load-local-heap
    begin-atomic
		jmp		short :try1
	:done
		mov		[esi + (* thread-heap-qv-offset 4)], edx
        mov     edx, [ebp + ARGS_OFFSET]
        and     edx, #xf8
        or      edx, (+ uvector-header-tag #x300)   ;; set length to 3        
        or      [eax + (uvector-offset 0)], edx
    end-atomic
        mov     ecx, 1
		pop		ebp
		ret
  	})

(defun cl::alloc-bignum (size) 
    (let ((bn 
                (if (<= size 2) 
                    (alloc-16-byte-uvector uvector-bignum-tag)
                    (if (<= size 4)
                        (alloc-24-byte-uvector uvector-bignum-tag)
                        (cl::alloc-uvector (+ size 1) uvector-bignum-tag)))))
   		(setf (uref bn 1) (* size 2))
        (dotimes (i size) (setf (uref bn (+ i 2)) 0))
		bn))

(defasm cl::alloc-double-float () 
    {
        push        ebp
        mov         ebp, esp
        push        (* uvector-double-float-tag 8)
        mov         ecx, 1
        callp       alloc-16-byte-uvector    ;; eax = double float node
        add         esp, 4    
        mov         ecx, 1
        pop         ebp
        ret
    })

(defasm cl::%double-float-node ()    ;; synonym, used by FFI
    {
        push        ebp
        mov         ebp, esp
        push        (* uvector-double-float-tag 8)
        mov         ecx, 1
        callp       alloc-16-byte-uvector    ;; eax = double float node
        add         esp, 4    
        mov         ecx, 1
        pop         ebp
        ret
    })

(defasm cl::alloc-single-float () 
    {
        push        ebp
        mov         ebp, esp
        push        (* uvector-single-float-tag 8)
        mov         ecx, 1
        callp       alloc-8-byte-uvector    ;; eax = single float node
        add         esp, 4    
        mov         ecx, 1
        pop         ebp
        ret
    })

(defasm cl::%single-float-node ()    ;; synonym, used by FFI
    {
        push        ebp
        mov         ebp, esp
        push        (* uvector-single-float-tag 8)
        mov         ecx, 1
        callp       alloc-8-byte-uvector    ;; eax = single float node
        add         esp, 4    
        mov         ecx, 1
        pop         ebp
        ret
    })

(defun fixnum-to-ratio (n)
    (let ((r (alloc-16-byte-uvector uvector-ratio-tag)))
        (setf (uref r 1) n (uref r 2) 1)
        r))

(defasm fixnum-to-short-float (n)
	{
		push        ebp
		mov         ebp,esp
        mov         edx, [ebp + ARGS_OFFSET] ;; edx = n
  		begin-atomic
        sar         edx, 3
        push        edx
        fild        [esp]                   ;; push onto fpu stack
		fstp.single [esp]                     ;; eax = 32-bit float
        pop         edx
		mov			eax, edx				;; untagged 32-bit float in eax, edx
		and         edx, #x7FFFFF			;; eax = 23-bit mantissa
		cmp         edx, #x7FFFFE			;; avoid overflow when rounding
		jae         short :t1
		mov         cl, al					;; get low three bits of mantissa
		and         cl, 7
		cmp         cl, 3
		je          short :t2
		cmp         cl, 5
		jle         short :t1
	:t2
		add         eax, 2
	:t1
		or          al, 3
        xor         edx, edx
		end-atomic
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
	})

(defasm fixnum-to-single-float (n)
	{
		push        ebp
		mov         ebp,esp
        mov         edx, [ebp + ARGS_OFFSET] ;; edx = n
  		begin-atomic
        sar         edx, 3
        push        edx
        fild        [esp]                   ;; push onto fpu stack
        add         esp, 4
        xor         edx, edx
  		end-atomic
        push        (* uvector-single-float-tag 8)
        callp       alloc-8-byte-uvector    ;; eax = single float node
        add         esp, 4    
   		fstp.single [eax + (uvector-offset 1)]
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
	})

(defasm fixnum-to-double-float (n)
	{
		push        ebp
		mov         ebp,esp
        mov         edx, [ebp + ARGS_OFFSET] ;; edx = n
  		begin-atomic
        sar         edx, 3
        push        edx
        fild        [esp]                   ;; push onto fpu stack
        add         esp, 4
        xor         edx, edx
  		end-atomic
        push        (* uvector-double-float-tag 8)
        callp       alloc-16-byte-uvector  ;; eax = double float node
        add         esp, 4    
		fstp       [eax + (uvector-offset 2)]
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
	})

(defun fixnum-to-complex (n)
    (let ((r (cl::alloc-uvector 2 uvector-complex-tag)))
        (setf (uref r 1) n (uref r 2) 0)
        r))
    
(defun bignum-to-ratio (n)
    (let ((r (alloc-16-byte-uvector uvector-ratio-tag)))
        (setf (uref r 1) n (uref r 2) 1)
        r))

;; this leaves the double on the fpu stack rather than allocating a node
(defasm bignum-to-double (n)
	{
		push        ebp
		mov         ebp,esp
        mov         edx, [ebp + ARGS_OFFSET] ;; edx = n
        mov         ecx, [edx + (uvector-offset 1)]
        shr         ecx, 4                  ;; ecx = number of bignum cells 
        shl         ecx, 2
        begin-atomic
        push        0
        push        [edx + ecx + (uvector-offset 1)]
        fild.64     [esp]
        add         esp, 8
    :loop1
        sub         ecx, 4
        jle         :next1
        push        32
        fild        [esp]
        fxch
        fscale                              ;; multiply fpu by #x100000000
        fxch
        fstp.single [esp]
        add         esp, 4
        push        0
        push        [edx + ecx + (uvector-offset 1)]
        fild.64     [esp]
        add         esp, 8
        faddp
        jmp         short :loop1
    :next1
        mov         ecx, [edx + (uvector-offset 1)]
        test        ecx, 8                  ;; check sign bit
        je          :done
        fchs                                ;; negate fpu value
    :done
        end-atomic
        mov         eax, [esi]
        mov         ecx, 1
        pop         ebp
        ret
    })               

(defasm double-float-to-fixnum (n)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + ARGS_OFFSET]
        fld     [eax + (uvector-offset 2)]
        frndint
        push    0
        begin-atomic
        fistp.32 [esp]
        pop     eax
        shl     eax, 3
        end-atomic
        mov     ecx, 1
        pop     ebp
        ret
    })

(defasm single-float-to-fixnum (n)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + ARGS_OFFSET]
        fld.single [eax + (uvector-offset 1)]
        frndint
        push    0
        begin-atomic
        fistp.32 [esp]
        pop     eax
        shl     eax, 3
        end-atomic
        mov     ecx, 1
        pop     ebp
        ret
    })

(defasm short-float-to-fixnum (n)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + ARGS_OFFSET]
        begin-atomic
        and     al, #xfc
        push    eax
        fld.single [esp]
        frndint
        fistp.32 [esp]
        pop     eax
        shl     eax, 3
        end-atomic
        mov     ecx, 1
        pop     ebp
        ret
    })

;;
;; Stores a double in a 32-bit bignum
;;
(defasm double-to-single-bignum (n)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + ARGS_OFFSET]
        fld     [eax + (uvector-offset 2)]
        frndint
        push    (* 8 2)
        mov     ecx, 1
        callp   cl::alloc-bignum        ;; eax = bignum
        add     esp, 4
        fistp.64 [eax + (uvector-offset 2)]
        mov     ecx, 1
        pop     ebp
        ret
    })

;;
;; Converts the double to a double or an integer.
;; Assumes the double has already been truncated or rounded as necessary.
;;
(defconstant 2^32d (float #x100000000 0d0))

(defun double-mod (n divisor) (- n (* (truncate (/ n divisor)) divisor)))

(defun mod32d (n) (- n (scale-float (float (ftruncate (scale-float n -32)) 0d0) 32)))
(defun double-float-to-integer (n)
    (if (and (< n most-positive-fixnum)
            (> n most-negative-fixnum))
        (double-float-to-fixnum n)
        ;; bignum conversion
        (multiple-value-bind (mantissa exponent sign)
            (integer-decode-float n)
            (* sign (ash mantissa exponent)))))

(defun single-float-to-integer (n)
    (if (and (< n most-positive-fixnum)
            (> n most-negative-fixnum))
        (single-float-to-fixnum n)
        ;; bignum conversion
        (multiple-value-bind (mantissa exponent sign)
            (integer-decode-float n)
            (* sign (ash mantissa exponent)))))

(defun short-float-to-integer (n)
    (if (and (< n most-positive-fixnum)
            (> n most-negative-fixnum))
        (short-float-to-fixnum n)
        ;; bignum conversion
        (multiple-value-bind (mantissa exponent sign)
            (integer-decode-float n)
            (* sign (ash mantissa exponent)))))
                  
(defasm bignum-to-short-float (n)
	{
		push        ebp
		mov         ebp,esp
        push        [ebp + ARGS_OFFSET]
        callp       bignum-to-double    
        add         esp, 4
  		begin-atomic
        push        ecx
		fstp.single [esp]                   
        pop         edx
		mov			eax, edx				;; untagged 32-bit float in eax, edx
		and         edx, #x7FFFFF			;; eax = 23-bit mantissa
		cmp         edx, #x7FFFFE			;; avoid overflow when rounding
		jae         short :t1
		mov         cl, al					;; get low three bits of mantissa
		and         cl, 7
		cmp         cl, 3
		je          short :t2
		cmp         cl, 5
		jle         short :t1
	:t2
		add         eax, 2
	:t1
		or          al, 3
        xor         edx, edx
		end-atomic
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
	})

(defasm bignum-to-single-float (n)
	{
		push        ebp
		mov         ebp,esp
        push        [ebp + ARGS_OFFSET]
        callp       bignum-to-double    
        add         esp, 4
        push        (* uvector-single-float-tag 8)
        callp       alloc-8-byte-uvector    ;; eax = single float node
        add         esp, 4    
   		fstp.single [eax + (uvector-offset 1)]
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
	})

(defasm bignum-to-double-float (n)
	{
		push        ebp
		mov         ebp,esp
        push        [ebp + ARGS_OFFSET]
        callp       bignum-to-double    
        add         esp, 4
        push        (* uvector-double-float-tag 8)
        callp       alloc-16-byte-uvector  ;; eax = double float node
        add         esp, 4    
   		fstp        [eax + (uvector-offset 2)]
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
	})

(defun bignum-to-complex (n)
    (let ((r (cl::alloc-uvector 2 uvector-complex-tag)))
        (setf (uref r 1) n (uref r 2) 0)
        r))

;; this leaves the double on the fpu stack rather than allocating a node
(defasm ratio-to-double (n)
	{
		push        ebp
		mov         ebp,esp
        mov         edx, [ebp + ARGS_OFFSET] ;; edx = n
        mov         eax, [edx + (uvector-offset 1)] ;; eax = numerator
        test        eax, 7
        jnz         :num-bignum
  		begin-atomic
        sar         eax, 3
        push        eax
        fild        [esp]                   ;; push onto fpu stack
        add         esp, 4
        xor         eax, eax
        end-atomic
        jmp         short :next1
    :num-bignum
        push        eax
        callp       bignum-to-double
        add         esp, 4
    :next1
        mov         edx, [ebp + ARGS_OFFSET] ;; edx = n
        mov         eax, [edx + (uvector-offset 2)] ;; eax = denominator
        test        eax, 7
        jnz         :denom-bignum
  		begin-atomic
        sar         eax, 3
        push        eax
        fild        [esp]                   ;; push onto fpu stack
        add         esp, 4
        xor         eax, eax
        end-atomic
        jmp         short :next2
    :denom-bignum
        push        eax
        callp       bignum-to-double
        add         esp, 4
    :next2
        fdivp
        mov         eax, [esi]
        mov         ecx, 1
        pop         ebp
        ret
    })

(defasm ratio-to-short-float (n)
	{
		push        ebp
		mov         ebp,esp
        push        [ebp + ARGS_OFFSET]
        callp       ratio-to-double    
        add         esp, 4
  		begin-atomic
        push        ecx
		fstp.single [esp]                   
        pop         edx
		mov			eax, edx				;; untagged 32-bit float in eax, edx
		and         edx, #x7FFFFF			;; eax = 23-bit mantissa
		cmp         edx, #x7FFFFE			;; avoid overflow when rounding
		jae         short :t1
		mov         cl, al					;; get low three bits of mantissa
		and         cl, 7
		cmp         cl, 3
		je          short :t2
		cmp         cl, 5
		jle         short :t1
	:t2
		add         eax, 2
	:t1
		or          al, 3
        xor         edx, edx
		end-atomic
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
	})        

(defasm ratio-to-single-float (n)
	{
		push        ebp
		mov         ebp,esp
        push        [ebp + ARGS_OFFSET]
        callp       ratio-to-double    
        add         esp, 4
        push        (* uvector-single-float-tag 8)
        callp       alloc-8-byte-uvector    ;; eax = single float node
        add         esp, 4    
  		fstp.single [eax + (uvector-offset 1)]
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
	})
   
(defasm ratio-to-double-float (n)
	{
		push        ebp
		mov         ebp,esp
        push        [ebp + ARGS_OFFSET]
        callp       ratio-to-double    
        add         esp, 4
        push        (* uvector-double-float-tag 8)
        callp       alloc-16-byte-uvector  ;; eax = double float node
        add         esp, 4    
   		fstp        [eax + (uvector-offset 2)]
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
	})

(defun ratio-to-complex (n)
    (let ((r (cl::alloc-uvector 2 uvector-complex-tag)))
        (setf (uref r 1) n (uref r 2) 0)
        r))

(defasm short-float-to-single-float (n)
	{
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
        begin-atomic
		and         al, #xfc
        push        eax
		fld.single  [esp]
        add         esp, 4
        xor         eax, eax        ;; clear untagged data
        end-atomic
        push        (* uvector-single-float-tag 8)
        callp       alloc-8-byte-uvector    ;; eax = single float node
        add         esp, 4    
   		fstp.single [eax + (uvector-offset 1)]
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
	})

;;;
;;; This (lossy) down-conversion is needed by FLOAT
;;;
(defasm single-float-to-short-float (n)
	{
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
        push        edx
        begin-atomic
		fld.single  [eax + (uvector-offset 1)]
		fstp.single [esp]                     ;; eax = 32-bit float
        pop         edx
		mov			eax, edx				;; untagged 32-bit float in eax, edx
		and         edx, #x7FFFFF			;; eax = 23-bit mantissa
		cmp         edx, #x7FFFFE			;; avoid overflow when rounding
		jae         short :t1
		mov         cl, al					;; get low three bits of mantissa
		and         cl, 7
		cmp         cl, 3
		je          short :t2
		cmp         cl, 5
		jle         short :t1
	:t2
		add         eax, 2
	:t1
		or          al, 3
        xor         edx, edx
		end-atomic
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
  	})

(defasm short-float-to-double-float (n)
	{
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
		and         al, #xfc
        push        eax
		fld.single  [esp]
        add         esp, 4
        push        (* uvector-double-float-tag 8)
        callp       alloc-16-byte-uvector  ;; eax = double float node
        add         esp, 4    
  		fstp        [eax + (uvector-offset 2)]
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
	})

;;;
;;; This (lossy) down-conversion is needed by FLOAT
;;;
(defasm double-float-to-short-float (n)
	{
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
        push        edx
        begin-atomic
		fld         [eax + (uvector-offset 2)]
		fstp.single [esp]
        pop         edx
		mov			eax, edx				;; untagged 32-bit float in eax, edx
		and         edx, #x7FFFFF			;; eax = 23-bit mantissa
		cmp         edx, #x7FFFFE			;; avoid overflow when rounding
		jae         short :t1
		mov         cl, al					;; get low three bits of mantissa
		and         cl, 7
		cmp         cl, 3
		je          short :t2
		cmp         cl, 5
		jle         short :t1
	:t2
		add         eax, 2
	:t1
		or          al, 3
        xor         edx, edx
		end-atomic
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
  	})

(defun short-float-to-complex (n)
    (let ((r (cl::alloc-uvector 2 uvector-complex-tag)))
        (setf (uref r 1) n (uref r 2) 0)
        r))

(defasm single-float-to-double-float (n)
	{
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
		fld.single  [eax + (uvector-offset 1)]
        push        (* uvector-double-float-tag 8)
        callp       alloc-16-byte-uvector  ;; eax = double float node
        add         esp, 4    
   		fstp        [eax + (uvector-offset 2)]
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
	})

;;;
;;; This (lossy) down-conversion is needed by FLOAT
;;;
(defasm double-float-to-single-float (n)
	{
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
		fld         [eax + (uvector-offset 2)]
        push        (* uvector-single-float-tag 8)
        callp       alloc-8-byte-uvector    ;; eax = single float node
        add         esp, 4    
   		fstp.single [eax + (uvector-offset 1)]
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
	})

(defun single-float-to-complex (n)
    (let ((r (cl::alloc-uvector 2 uvector-complex-tag)))
        (setf (uref r 1) n (uref r 2) 0)
        r))

(defun double-float-to-complex (n)
    (let ((r (cl::alloc-uvector 2 uvector-complex-tag)))
        (setf (uref r 1) n (uref r 2) 0)
        r))

(defun not-number (n) n)        ;; just return object

(defconstant number-promotion-functions 
    (vector
        nil      #'fixnum-to-bignum #'fixnum-to-ratio #'fixnum-to-short-float #'fixnum-to-single-float       #'fixnum-to-double-float       #'fixnum-to-complex       #'not-number
        nil      nil                #'bignum-to-ratio #'bignum-to-short-float #'bignum-to-single-float       #'bignum-to-double-float       #'bignum-to-complex       #'not-number        
        nil      nil                nil               #'ratio-to-short-float  #'ratio-to-single-float        #'ratio-to-double-float        #'ratio-to-complex        #'not-number        
        nil      nil                nil               nil                     #'short-float-to-single-float  #'short-float-to-double-float  #'short-float-to-complex  #'not-number                
        nil      nil                nil               nil                     nil                            #'single-float-to-double-float #'single-float-to-complex #'not-number                
        nil      nil                nil               nil                     nil                            nil                            #'double-float-to-complex #'not-number                
        nil      nil                nil               nil                     nil                            nil                            nil                       #'not-number                
    #'not-number #'not-number       #'not-number      #'not-number            #'not-number                   #'not-number                   #'not-number              #'not-number                
     ))                   

;;;
;;; Takes a number of any type, type1 is the fixnum type id of that number, and type2
;;; is the fixnum type id of the numeric type you wish the number to be promoted to.
;;; It should always be true that 0 <= type1 < type2 < NumMaxIDs.
;;;
(defasm promote-number (n type1 type2)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + (+ ARGS_OFFSET 4)]      ;; eax = type1
        mov     edx, [ebp + ARGS_OFFSET]            ;; edx = type2
        shr     edx, 3
        add     eax, edx
        shl     eax, 2
        mov     edx, number-promotion-functions
        mov     eax, [edx + (uvector-offset 2) + eax]   ;; eax = promotion function
        push    [ebp + (+ ARGS_OFFSET 8)]           ;; push number
        mov     ecx, 1
        callfunc  eax
        add     esp, 4
        pop     ebp
        ret
    })

;; redefined below
(defun simplify-ratio (num denom)
    (if (eq num 0)
        (return-from simplify-ratio 0))
    (if (eq denom 0)
        (error "Divide by zero error: ~A / ~A" num denom))
    (when (< denom 0)
        (setf denom (- denom))
        (setf num (- num)))
    (let ((temp (gcd num denom)))
        (unless (eq temp 1)
            (setf num (/ num temp))
            (setf denom (/ denom temp)))
        (if (eq denom 1) 
            num
            (let ((ratio (alloc-16-byte-uvector uvector-ratio-tag)))
                (setf (uref ratio 1) num)
                (setf (uref ratio 2) denom)
                ratio)))) 
        
(defasm add-fixnums (n1 n2)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + (+ ARGS_OFFSET 4)]  ;; eax = n1
        add     eax, [ebp + ARGS_OFFSET]
        jno     short :done
        ;; handle overflow
        push    [ebp + (+ ARGS_OFFSET 4)]
        mov     ecx, 1
        callp   fixnum-to-bignum
        add     esp, 4
        push    eax          ;; bignum n1 on stack
        push    [ebp + ARGS_OFFSET]
        mov     ecx, 1
        callp   fixnum-to-bignum
        add     esp, 4
        push    eax          ;; bignum n2 on stack
        mov     ecx, 2
        callp   add-bignums
        add     esp, 8
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

(defun add-ratios (n1 n2)
    (let* ((num1 (numerator n1))
           (den1 (denominator n1))
           (num2 (numerator n2))
           (den2 (denominator n2)))
        (simplify-ratio (+ (* den2 num1) (* den1 num2)) (* den1 den2))))    

(defasm add-short-floats (x y)
	{
		push		ebp
		mov			ebp, esp
		begin-atomic
        mov         eax, #xfc
		and			[ebp + ARGS_OFFSET], al
		and         [ebp + (+ ARGS_OFFSET 4)], al
		fld.single  [ebp + (+ ARGS_OFFSET 4)]
		fadd.single [ebp + ARGS_OFFSET]
		fstp.single [ebp + ARGS_OFFSET]
		mov         edx, [ebp + ARGS_OFFSET]
		mov			eax, edx				;; untagged 32-bit float in eax, edx
		and         edx,#x7FFFFF			;; edx = 23-bit mantissa
		cmp         edx,#x7FFFFE			;; avoid overflow when rounding
		jae         short :t1
		mov         cl, al					;; get low three bits of mantissa
		and         cl, 7
		cmp         cl, 3
		je          short :t2
		cmp         cl, 5
		jle         short :t1
	:t2
		add         eax, 2
	:t1
		or          al, 3
        mov         [ebp + ARGS_OFFSET], ecx            ;; for safety, wipe out untagged data
		mov         [ebp + (+ ARGS_OFFSET 4)], ecx      ;; for safety, wipe out untagged data       
		end-atomic
        mov         cl, 1
		mov			esp, ebp
		pop			ebp
		ret
    })
 
(defasm add-single-floats (x y)
	{
		push		ebp
		mov			ebp, esp
        mov         edx, [ebp + (+ ARGS_OFFSET 4)]
		fld.single  [edx + (uvector-offset 1)]
        mov         edx, [ebp + ARGS_OFFSET]
		fadd.single [edx + (uvector-offset 1)]
        push        (* uvector-single-float-tag 8)
        callp       alloc-8-byte-uvector    ;; eax = single float node
        add         esp, 4    
   		fstp.single [eax + (uvector-offset 1)]
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
    })    
 
(defasm add-double-floats (x y)
	{
		push		ebp
		mov			ebp, esp
        mov         edx, [ebp + (+ ARGS_OFFSET 4)]
		fld         [edx + (uvector-offset 2)]
        mov         edx, [ebp + ARGS_OFFSET]
		fadd        [edx + (uvector-offset 2)]
        push        (* uvector-double-float-tag 8)
        callp       alloc-16-byte-uvector    ;; eax = double float node
        add         esp, 4    
   		fstp        [eax + (uvector-offset 2)]
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
    })

(defun add-complex (x y)
    (complex (+ (realpart x) (realpart y)) (+ (imagpart x) (imagpart y))))

(defun add-non-numbers (x y)
    (unless (numberp x)
        (cl::signal-type-error x 'number))
    (unless (numberp y)
        (cl::signal-type-error y 'number)))

(defconstant add-functions 
    (vector 
        #'add-fixnums
        #'add-bignums
        #'add-ratios
        #'add-short-floats
        #'add-single-floats
        #'add-double-floats
        #'add-complex 
        #'add-non-numbers))

(defasm add-numbers (x y)
    {
        push    ebp
        mov     ebp, esp
    
        ;; quick check for adding two fixnums
        xor     eax, eax
        mov     al, [ebp + (+ ARGS_OFFSET 4)]
        or      al, [ebp + ARGS_OFFSET]
        and     al, 7
        jne     :next
        
        ;; attempt fixnum add
		mov     eax, [ebp + (+ ARGS_OFFSET 4)]
		add     eax, [ebp + ARGS_OFFSET]
		jno     :done
        push    [ebp + (+ ARGS_OFFSET 4)]
        push    0
        push    8
        callp   promote-number
        add     esp, 12
        mov     [ebp + (+ ARGS_OFFSET 4)], eax      ;; promote x to bignum
        push    [ebp + ARGS_OFFSET]
        push    0
        push    8                 
        callp   promote-number
        add     esp, 12
        mov     [ebp + ARGS_OFFSET], eax            ;; promote y to bignum
        mov     eax, 8
        jmp     short :same
       
    :next    
        push    [ebp + (+ ARGS_OFFSET 4)]
        callp   num-type
        add     esp, 4
        push    eax
        push    [ebp + ARGS_OFFSET]
        callp   num-type
        add     esp, 4                  ;; eax = typeof(y)
        pop     edx                     ;; edx = typeof(x)
        cmp     edx, eax
        jl      short :less
        jg      short :greater
        
        ;; types are the same
    :same
        mov     edx, add-functions
        shr     eax, 1
        mov     eax, [edx + (uvector-offset 2) + eax]   ;; eax = promotion function
        push    [ebp + (+ ARGS_OFFSET 4)]           ;; push x
        push    [ebp + ARGS_OFFSET]                 ;; push y
        mov     ecx, 2
   		callfunc eax
        add     esp, 8
        jmp     short   :done
    
    :less
        push    eax
        push    [ebp + (+ ARGS_OFFSET 4)]
        push    edx
        push    eax
        mov     ecx, 3
        callp   promote-number
        add     esp, 12
        mov     [ebp + (+ ARGS_OFFSET 4)], eax      ;; promote x
        pop     eax                                 ;; eax = typeof(y)
        jmp     short :same
    
    :greater
        push    edx
        push    [ebp + ARGS_OFFSET]
        push    eax
        push    edx
        mov     ecx, 3
        callp   promote-number
        add     esp, 12
        mov     [ebp + ARGS_OFFSET], eax            ;; promote y
        pop     eax                                 ;; eax = typeof(y)
        jmp     short :same        
            
    :done
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })            
#|          
(defun add-numbers (x y)
    (let ((xtype (num-type x))
          (ytype (num-type y)))
        (cond 
            ((eq xtype ytype)
             (funcall (uref add-functions (+ 2 xtype)) x y))
            ((< xtype ytype)
             (funcall (uref add-functions (+ 2 ytype)) (promote-number x xtype ytype) y))
            (t 
             (funcall (uref add-functions (+ 2 xtype)) x (promote-number y ytype xtype))))))
|#
                  
(defasm subtract-fixnums (n1 n2)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + (+ ARGS_OFFSET 4)]  ;; eax = n1
        sub     eax, [ebp + ARGS_OFFSET]
        jno     short :done
        ;; handle overflow
        push    [ebp + (+ ARGS_OFFSET 4)]
        mov     ecx, 1
        callp   fixnum-to-bignum
        add     esp, 4
        push    eax          ;; bignum n1 on stack
        push    [ebp + ARGS_OFFSET]
        mov     ecx, 1
        callp   fixnum-to-bignum
        add     esp, 4
        push    eax          ;; bignum n2 on stack
        mov     ecx, 2
        callp   sub-bignums
        add     esp, 8
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })
    
(defun subtract-ratios (n1 n2)
    (let* ((num1 (numerator n1))
           (den1 (denominator n1))
           (num2 (numerator n2))
           (den2 (denominator n2)))
        (simplify-ratio (- (* den2 num1) (* den1 num2)) (* den1 den2))))

(defasm subtract-short-floats (x y)
	{
		push		ebp
		mov			ebp, esp
		begin-atomic
        mov         eax, #xfc
		and			[ebp + ARGS_OFFSET], al
		and         [ebp + (+ ARGS_OFFSET 4)], al
		fld.single  [ebp + (+ ARGS_OFFSET 4)]
		fsub.single [ebp + ARGS_OFFSET]
		fstp.single [ebp + ARGS_OFFSET]
		mov         edx, [ebp + ARGS_OFFSET]
		mov			eax, edx				;; untagged 32-bit float in eax, edx
		and         edx,#x7FFFFF			;; edx = 23-bit mantissa
		cmp         edx,#x7FFFFE			;; avoid overflow when rounding
		jae         short :t1
		mov         cl, al					;; get low three bits of mantissa
		and         cl, 7
		cmp         cl, 3
		je          short :t2
		cmp         cl, 5
		jle         short :t1
	:t2
		add         eax, 2
	:t1
		or          al, 3
        mov         [ebp + ARGS_OFFSET], ecx            ;; for safety, wipe out untagged data
		mov         [ebp + (+ ARGS_OFFSET 4)], ecx      ;; for safety, wipe out untagged data       
		end-atomic
        mov         cl, 1
		mov			esp, ebp
		pop			ebp
		ret
    })
    
(defasm subtract-single-floats (x y)
	{
		push		ebp
		mov			ebp, esp
        mov         edx, [ebp + (+ ARGS_OFFSET 4)]
		fld.single  [edx + (uvector-offset 1)]
        mov         edx, [ebp + ARGS_OFFSET]
		fsub.single [edx + (uvector-offset 1)]
        push        (* uvector-single-float-tag 8)
        callp       alloc-8-byte-uvector    ;; eax = single float node
        add         esp, 4    
   		fstp.single [eax + (uvector-offset 1)]
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
    })    
 
(defasm subtract-double-floats (x y)
	{
		push		ebp
		mov			ebp, esp
        mov         edx, [ebp + (+ ARGS_OFFSET 4)]
		fld         [edx + (uvector-offset 2)]
        mov         edx, [ebp + ARGS_OFFSET]
		fsub        [edx + (uvector-offset 2)]
        push        (* uvector-double-float-tag 8)
        callp       alloc-16-byte-uvector    ;; eax = double float node
        add         esp, 4    
   		fstp        [eax + (uvector-offset 2)]
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
    })

(defun subtract-complex (x y)
    (complex (- (realpart x) (realpart y)) (- (imagpart x) (imagpart y))))

(defun subtract-non-numbers (x y)
    (unless (numberp x)
        (cl::signal-type-error x 'number))
    (unless (numberp y)
        (cl::signal-type-error y 'number)))

(defconstant subtract-functions 
    (vector 
        #'subtract-fixnums
        #'sub-bignums
        #'subtract-ratios
        #'subtract-short-floats
        #'subtract-single-floats
        #'subtract-double-floats
        #'subtract-complex 
        #'subtract-non-numbers))

(defasm subtract-numbers (x y)
    {
        push    ebp
        mov     ebp, esp
    
        ;; quick check for subtracting two fixnums
        xor     eax, eax
        mov     al, [ebp + (+ ARGS_OFFSET 4)]
        or      al, [ebp + ARGS_OFFSET]
        and     al, 7
        jne     :next
        
        ;; attempt fixnum subtract
		mov     eax, [ebp + (+ ARGS_OFFSET 4)]
		sub     eax, [ebp + ARGS_OFFSET]
		jno     :done
        push    [ebp + (+ ARGS_OFFSET 4)]
        push    0
        push    8
        callp   promote-number
        add     esp, 12
        mov     [ebp + (+ ARGS_OFFSET 4)], eax      ;; promote x to bignum
        push    [ebp + ARGS_OFFSET]
        push    0
        push    8                 
        callp   promote-number
        add     esp, 12
        mov     [ebp + ARGS_OFFSET], eax            ;; promote y to bignum
        mov     eax, 8
        jmp     short :same
       
    :next    
        push    [ebp + (+ ARGS_OFFSET 4)]
        callp   num-type
        add     esp, 4
        push    eax
        push    [ebp + ARGS_OFFSET]
        callp   num-type
        add     esp, 4                  ;; eax = typeof(y)
        pop     edx                     ;; edx = typeof(x)
        cmp     edx, eax
        jl      short :less
        jg      short :greater
        
        ;; types are the same
    :same
        mov     edx, subtract-functions
        shr     eax, 1
        mov     eax, [edx + (uvector-offset 2) + eax]   ;; eax = promotion function
        push    [ebp + (+ ARGS_OFFSET 4)]           ;; push x
        push    [ebp + ARGS_OFFSET]                 ;; push y
        mov     ecx, 2
        callfunc	eax
        add     esp, 8
        jmp     short   :done
    
    :less
        push    eax
        push    [ebp + (+ ARGS_OFFSET 4)]
        push    edx
        push    eax
        mov     ecx, 3
        callp   promote-number
        add     esp, 12
        mov     [ebp + (+ ARGS_OFFSET 4)], eax      ;; promote x
        pop     eax                                 ;; eax = typeof(y)
        jmp     short :same
    
    :greater
        push    edx
        push    [ebp + ARGS_OFFSET]
        push    eax
        push    edx
        mov     ecx, 3
        callp   promote-number
        add     esp, 12
        mov     [ebp + ARGS_OFFSET], eax            ;; promote y
        pop     eax                                 ;; eax = typeof(y)
        jmp     short :same        
            
    :done
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })            

(defasm multiply-fixnums (n1 n2)
    {
        push    ebp
        mov     ebp, esp
    
        mov     eax, [ebp + (+ ARGS_OFFSET 4)]  ;; eax = n1
        begin-atomic
   		shr     eax, 3
		imul    [ebp + ARGS_OFFSET]
        jo      short :overflow
        xor     edx, edx                        ;; clear untagged data
        end-atomic
        jmp     short :done
    :overflow    
        ;; handle overflow
        xor     edx, edx        ; clear untagged data
        end-atomic
        push    [ebp + (+ ARGS_OFFSET 4)]
        mov     ecx, 1
        callp   fixnum-to-bignum
        add     esp, 4
        push    eax             ; bignum n1 on stack
        push    [ebp + ARGS_OFFSET]
        mov     ecx, 1
        callp   fixnum-to-bignum
        add     esp, 4
        push    eax             ; bignum n2 on stack
        mov     ecx, 2
        callp   multiply-bignums
        add     esp, 8
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

(defun multiply-ratios (n1 n2)
    (let* ((num1 (numerator n1))
           (den1 (denominator n1))
           (num2 (numerator n2))
           (den2 (denominator n2)))
        (simplify-ratio (* num1 num2) (* den1 den2))))

(defasm multiply-short-floats (x y)
	{
		push		ebp
		mov			ebp, esp
		begin-atomic
        mov         eax, #xfc
		and			[ebp + ARGS_OFFSET], al
		and         [ebp + (+ ARGS_OFFSET 4)], al
		fld.single  [ebp + (+ ARGS_OFFSET 4)]
		fmul.single [ebp + ARGS_OFFSET]
		fstp.single [ebp + ARGS_OFFSET]
		mov         edx, [ebp + ARGS_OFFSET]
		mov			eax, edx				;; untagged 32-bit float in eax, edx
		and         edx,#x7FFFFF			;; edx = 23-bit mantissa
		cmp         edx,#x7FFFFE			;; avoid overflow when rounding
		jae         short :t1
		mov         cl, al					;; get low three bits of mantissa
		and         cl, 7
		cmp         cl, 3
		je          short :t2
		cmp         cl, 5
		jle         short :t1
	:t2
		add         eax, 2
	:t1
		or          al, 3
        mov         [ebp + ARGS_OFFSET], ecx            ;; for safety, wipe out untagged data
		mov         [ebp + (+ ARGS_OFFSET 4)], ecx      ;; for safety, wipe out untagged data       
		end-atomic
        mov         cl, 1
		mov			esp, ebp
		pop			ebp
		ret
    })

(defasm multiply-single-floats (x y)
	{
		push		ebp
		mov			ebp, esp
        mov         edx, [ebp + (+ ARGS_OFFSET 4)]
		fld.single  [edx + (uvector-offset 1)]
        mov         edx, [ebp + ARGS_OFFSET]
		fmul.single [edx + (uvector-offset 1)]
        push        (* uvector-single-float-tag 8)
        callp       alloc-8-byte-uvector    ;; eax = single float node
        add         esp, 4    
   		fstp.single [eax + (uvector-offset 1)]
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
    })

(defasm multiply-double-floats (x y)
	{
		push		ebp
		mov			ebp, esp
        mov         edx, [ebp + (+ ARGS_OFFSET 4)]
		fld         [edx + (uvector-offset 2)]
        mov         edx, [ebp + ARGS_OFFSET]
		fmul        [edx + (uvector-offset 2)]
        push        (* uvector-double-float-tag 8)
        callp       alloc-16-byte-uvector    ;; eax = double float node
        add         esp, 4    
   		fstp        [eax + (uvector-offset 2)]
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
    })

;; (a + bi) * (c + di) == ac - bd + (bc + ad)i
(defun multiply-complex (x y)
    (let ((realx (realpart x))
          (realy (realpart y))
          (imagx (imagpart x))
          (imagy (imagpart y)))  
        (complex (- (* realx realy) (* imagx imagy))
                 (+ (* imagx realy) (* realx imagy)))))

(defun multiply-non-numbers (x y)
    (unless (numberp x)
        (cl::signal-type-error x 'number))
    (unless (numberp y)
        (cl::signal-type-error y 'number)))

(defconstant multiply-functions 
    (vector 
        #'multiply-fixnums
        #'multiply-bignums
        #'multiply-ratios
        #'multiply-short-floats
        #'multiply-single-floats
        #'multiply-double-floats
        #'multiply-complex 
        #'multiply-non-numbers))

(defasm multiply-numbers (x y)
    {
        push    ebp
        mov     ebp, esp
    
        ;; quick check for multiplying two fixnums
        xor     eax, eax
        mov     al, [ebp + (+ ARGS_OFFSET 4)]
        or      al, [ebp + ARGS_OFFSET]
        and     al, 7
        jne     :next
        
        ;; attempt fixnum multiply
        mov     eax, [ebp + (+ ARGS_OFFSET 4)]  ;; eax = n1
        begin-atomic
   		shr     eax, 3
		imul    [ebp + ARGS_OFFSET]
		jo     short :overflow
        xor     edx, edx        ; clear untagged data
        end-atomic
        jmp     :done
    :overflow    
        ;; handle overflow
        xor     edx, edx        ; clear untagged data
        end-atomic

        push    [ebp + (+ ARGS_OFFSET 4)]
        mov     ecx, 1
        callp   fixnum-to-bignum
        add     esp, 4
        push    eax             ; bignum n1 on stack
        push    [ebp + ARGS_OFFSET]
        mov     ecx, 1
        callp   fixnum-to-bignum
        add     esp, 4
        push    eax             ; bignum n2 on stack
        mov     ecx, 2
        callp   multiply-bignums
        add     esp, 8
        jmp     short :done       
    :next    
        push    [ebp + (+ ARGS_OFFSET 4)]
        callp   num-type
        add     esp, 4
        push    eax
        push    [ebp + ARGS_OFFSET]
        callp   num-type
        add     esp, 4                  ;; eax = typeof(y)
        pop     edx                     ;; edx = typeof(x)
        cmp     edx, eax
        jl      short :less
        jg      short :greater
        
        ;; types are the same
    :same
        mov     edx, multiply-functions
        shr     eax, 1
        mov     eax, [edx + (uvector-offset 2) + eax]   ;; eax = promotion function
        push    [ebp + (+ ARGS_OFFSET 4)]           ;; push x
        push    [ebp + ARGS_OFFSET]                 ;; push y
        mov     ecx, 2
        callfunc	eax
        add     esp, 8
        jmp     short   :done
    
    :less
        push    eax
        push    [ebp + (+ ARGS_OFFSET 4)]
        push    edx
        push    eax
        mov     ecx, 3
        callp   promote-number
        add     esp, 12
        mov     [ebp + (+ ARGS_OFFSET 4)], eax      ;; promote x
        pop     eax                                 ;; eax = typeof(y)
        jmp     short :same
    
    :greater
        push    edx
        push    [ebp + ARGS_OFFSET]
        push    eax
        push    edx
        mov     ecx, 3
        callp   promote-number
        add     esp, 12
        mov     [ebp + ARGS_OFFSET], eax            ;; promote y
        pop     eax                                 ;; eax = typeof(y)
        jmp     short :same        
            
    :done
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })            

;; This could result in a bignum, in the case of most-negative-fixnum
(defasm abs-fixnum (n)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + (+ ARGS_OFFSET 0)]  ; eax = n
        cmp     eax, 0                          ; n1 < 0?
        jge     short :done
        neg     eax        
        jno      short :done
        push    8
        mov     ecx, 1
        callp   cl::alloc-bignum
        add     esp, 4
        mov     edx, 1
        shl     edx, 28
        mov     [eax + (uvector-offset 2)], edx
    :done
        pop     ebp
        ret
    })

(defasm compare-integers (n1 n2)
    {
        push    ebp
        mov     ebp, esp
        mov     edx, [ebp + (+ ARGS_OFFSET 4)]  ; edx = n1
        mov     ecx, [ebp + (+ ARGS_OFFSET 0)]  ; ecx = n2
        xor     eax, eax
        mov     al, dl
        or      al, cl
        and     al, 7
        jne     short :do-bignum
        cmp     edx, ecx
        jg      short :gt
        je      short :eq
        mov     eax, -8
        jmp     short :done
    :gt
        mov     eax, 8
        jmp     short :done
    :eq
        mov     eax, 0
        jmp     short :done
    
    :do-bignum
        mov     eax, edx
        test    al, 7
        jne     :next1
        push    eax
        mov     ecx, 1
        callp   fixnum-to-bignum
        add     esp, 4
    :next1
        push    eax         ; push bignum(n1)
        mov     eax, [ebp + (+ ARGS_OFFSET 0)]  ; eax = n2
        test    al, 7
        jne     :next2
        push    eax
        mov     ecx, 1
        callp   fixnum-to-bignum
        add     esp, 4
    :next2
        push    eax         ; push bignum(n2)
        mov     ecx, 2 
        callp   bignum-compare
        add     esp, 8
    :done
        mov     ecx, 1
        pop     ebp
        ret
    }) 

(defasm zerop-fixnum (n)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + (+ ARGS_OFFSET 0)]
        test    eax, eax
        jne     :next
        mov     eax, [esi + 4]
        jmp     short :done
    :next
        mov     eax, [esi]
    :done
        pop     ebp
        ret
    })

(defun zerop-bignum (n) (= (bignum-significant-length n) 0))
(defun zerop-ratio (n) (= (numerator n) 0))
(defasm zerop-short-float (n)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + (+ ARGS_OFFSET 0)]
        and     al, #xf8
        test    eax, eax
        jne     :next
        mov     eax, [esi + 4]
        jmp     short :done
    :next
        mov     eax, [esi]
    :done
        pop     ebp
        ret
    })

(defasm zerop-single-float (n)
    {
		push		ebp
		mov			ebp, esp
        mov         edx, [ebp + (+ ARGS_OFFSET 0)]
		fld.single  [edx + (uvector-offset 1)]
        ftst
		fstp.single [edx + (uvector-offset 1)]
        fstsw_AX
        test        eax, #x4000
        jne         short :equal
        mov         eax, [esi]
        jmp         short :done
    :equal
        mov         eax, [esi + 4]
    :done
        mov         ecx, 1
        pop         ebp
        ret
    })

(defasm zerop-double-float (n)
    {
		push		ebp
		mov			ebp, esp
        mov         edx, [ebp + (+ ARGS_OFFSET 0)]
		fld         [edx + (uvector-offset 2)]
        ftst
        fstp        [edx + (uvector-offset 2)]
        fstsw_AX
        test        eax, #x4000
        jne         short :equal
        mov         eax, [esi]
        jmp         short :done
    :equal
        mov         eax, [esi + 4]
    :done
        mov         ecx, 1
        pop         ebp
        ret
    })

(defun zerop-complex (n) (and (= (realpart n) 0) (= (imagpart n) 0)))
               
(defun gcd-bignums (n1 n2) (gcd n1 n2))     ;; RGC--redefined below
(defun gcd-fixnums (n1 n2) (gcd n1 n2))     ;; RGC--redefined below

(defun gcd-bignums (n1 n2)
    (setq n1 (abs-bignum n1))
    (setq n2 (abs-bignum n2))
    (if (= n1 0)
        (return-from gcd-bignums n2))
    (if (= n2 0)
        (return-from gcd-bignums n1))
    
    (do ((temp 0))
        (nil)
        (if (and (fixnump n1) (fixnump n2))
            (return-from gcd-bignums (gcd-fixnums n1 n2)))
        (cond
            ((fixnump n1)(setq temp (mod-bignums (fixnum-to-bignum n1) n2)))
            ((fixnump n2)(setq temp (mod-bignums n1 (fixnum-to-bignum n2))))
            (t (setq temp (mod-bignums n1 n2))))
        (if (= temp 0)
            (return-from gcd-bignums n2))
        (setq n1 n2)
        (setq n2 temp)))
  
(defasm gcd-fixnums (n1 n2)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + (+ ARGS_OFFSET 0)]
        callp   abs-fixnum
        add     esp, 4
        mov     ecx, eax                        ; ecx = abs(n2)
        push    ecx
        push    [ebp + (+ ARGS_OFFSET 4)]
        callp   abs-fixnum
        add     esp, 4                          ; eax = abs(n1)
        pop     ecx
        test    eax, eax
        jne     short :next1
        mov     eax, ecx
        jmp     short :done
    :next1            
        test    ecx, ecx
        jne     short :next2
        jmp     short :done
        
        ;; eax = n1, ecx = n2
    :next2
        xor     edx, edx
        mov     dl, al
        or      dl, cl
        and     dl, 7
        jne     :do-bignum
    
        begin-atomic
        shr     eax, 3
        shr     ecx, 3
        
    :loop
        mov     edx, 0
        div     ecx
        cmp     edx, 0
        jne     short :next3
        mov     eax, ecx
        shl     eax, 3
        jmp     short :done
    :next3    
        mov     eax, ecx
        mov     ecx, edx
        jmp     short :loop
    :do-bignum
        test    al, 7
        jne     short :next4
        push    eax
        callp   fixnum-to-bignum
        add     esp, 4
    :next4
        push    eax
        mov     eax, ecx
        test    al, 7
        jne     short :next5
        push    eax
        callp   fixnum-to-bignum
        add     esp, 4
    :next5
        push    eax        
        callp   gcd-bignums
        add     esp, 8
    
    :done
        mov     ecx, 1
        xor     edx, edx
        end-atomic
        pop     ebp
        ret
    })    
               
(defasm divide-and-truncate-integers (n1 n2)
    {
        push    ebp
        mov     ebp, esp
        mov     edx, [ebp + (+ ARGS_OFFSET 4)]  ; edx = n1
        mov     ecx, [ebp + (+ ARGS_OFFSET 0)]  ; ecx = n2
        xor     eax, eax
        mov     al, dl
        or      al, cl
        and     al, 7
        jne     short :do-bignum
        begin-atomic
        mov     eax, edx
        xor     edx, edx
        sar     eax, 3
        cmp     eax, 0
        jge     short :next0
        not     edx         ;; sign extend edx if necessary
    :next0
        sar     ecx, 3
        idiv    ecx
        shl     eax, 3
        xor     ecx, ecx
        xor     edx, edx
        end-atomic
        jmp     short :done
    :do-bignum
        mov     eax, edx
        test    al, 7
        jne     :next1
        push    eax
        mov     ecx, 1
        callp   fixnum-to-bignum
        add     esp, 4
    :next1
        push    eax         ; push bignum(n1)
        mov     eax, [ebp + (+ ARGS_OFFSET 0)]  ; eax = n2
        test    al, 7
        jne     :next2
        push    eax
        mov     ecx, 1
        callp   fixnum-to-bignum
        add     esp, 4
    :next2
        push    eax         ; push bignum(n2)
        mov     ecx, 2 
        callp   divide-bignums
        add     esp, 8
    :done
        mov     ecx, 1
        pop     ebp
        ret
    }) 

(defun gcd-numbers (n1 n2)
    (unless (integerp n1)
        (cl::signal-type-error n1 'integer))
    (unless (integerp n2)
        (cl::signal-type-error n2 'integer))
    (if (and (fixnump n1) (fixnump n2))
        (gcd-fixnums n1 n2)
        (progn
            (if (fixnump n1)
                (setq n1 (fixnum-to-bignum n1)))
            (if (fixnump n2)
                (setq n2 (fixnum-to-bignum n2)))
            (gcd-bignums n1 n2))))

(defun make-ratio (num denom)
    (let ((r (alloc-16-byte-uvector uvector-ratio-tag)))
        (setf (uref r cl::ratio-numerator-offset) num)
        (setf (uref r cl::ratio-denominator-offset) denom)
        r))
                  
(defun simplify-ratio (num denom)
    (if (eq num 0)
        0
        (if (= denom 0)
            (cl::signal-division-by-zero (list num denom))
            (progn
                (when (eq (compare-integers denom 0) -1) 
                    (setq denom (- 0 denom))
                    (setq num (- 0 num)))
                (let ((temp (gcd-numbers num denom)))
                    (unless (eq temp 1)
                        (setq num (divide-and-truncate-integers num temp))
                        (setq denom (divide-and-truncate-integers denom temp)))
                    (if (eq denom 1) 
                        num
                        (let ((ratio (alloc-16-byte-uvector uvector-ratio-tag)))
                            (setf (uref ratio cl::ratio-numerator-offset) num)
                            (setf (uref ratio cl::ratio-denominator-offset) denom)
                            ratio))))))) 

(defun divide-fixnums (n1 n2)
    (simplify-ratio n1 n2))

(defun divide-bignums-2 (n1 n2)
    (simplify-ratio n1 n2))

(defun divide-ratios (n1 n2)
    (let ((num1 (numerator n1))
          (den1 (denominator n1))
          (num2 (numerator n2))
          (den2 (denominator n2)))
        (simplify-ratio (* num1 den2) (* den1 num2))))

(defasm divide-short-floats (x y)
	{
		push		ebp
		mov			ebp, esp
		begin-atomic
        mov         eax, #xfc
		and			[ebp + ARGS_OFFSET], al
		and         [ebp + (+ ARGS_OFFSET 4)], al
		fld.single  [ebp + (+ ARGS_OFFSET 4)]
		fdiv.single [ebp + ARGS_OFFSET]
		fstp.single [ebp + ARGS_OFFSET]
		mov         edx, [ebp + ARGS_OFFSET]
		mov			eax, edx				;; untagged 32-bit float in eax, edx
		and         edx,#x7FFFFF			;; edx = 23-bit mantissa
		cmp         edx,#x7FFFFE			;; avoid overflow when rounding
		jae         short :t1
		mov         cl, al					;; get low three bits of mantissa
		and         cl, 7
		cmp         cl, 3
		je          short :t2
		cmp         cl, 5
		jle         short :t1
	:t2
		add         eax, 2
	:t1
		or          al, 3
        mov         [ebp + ARGS_OFFSET], ecx            ;; for safety, wipe out untagged data
		mov         [ebp + (+ ARGS_OFFSET 4)], ecx      ;; for safety, wipe out untagged data       
		end-atomic
        mov         cl, 1
		mov			esp, ebp
		pop			ebp
		ret
    })

(defasm divide-single-floats (x y)
	{
		push		ebp
		mov			ebp, esp
        push        [ebp + (+ ARGS_OFFSET 0)]
        callp       zerop-single-float
        add         esp, 4
        cmp         eax, [esi]
        je          :next
        push        [ebp + (+ ARGS_OFFSET 4)]
        push        [ebp + (+ ARGS_OFFSET 0)]
        mov         ecx, 2
        callf       list
        add         esp, 8
        push        eax
        mov         ecx, 1
        callf       cl::signal-division-by-zero
    :next
        mov         edx, [ebp + (+ ARGS_OFFSET 4)]
  		fld.single  [edx + (uvector-offset 1)]
        mov         edx, [ebp + ARGS_OFFSET]
		fdiv.single [edx + (uvector-offset 1)]
        push        (* uvector-single-float-tag 8)
        callp       alloc-8-byte-uvector    ;; eax = single float node
        add         esp, 4    
   		fstp.single [eax + (uvector-offset 1)]
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
    })

(defasm divide-double-floats (x y)
	{
		push		ebp
		mov			ebp, esp
        push        [ebp + (+ ARGS_OFFSET 0)]
        callp       zerop-double-float
        add         esp, 4
        cmp         eax, [esi]
        je          :next
        push        [ebp + (+ ARGS_OFFSET 4)]
        push        [ebp + (+ ARGS_OFFSET 0)]
        mov         ecx, 2
        callf       list
        add         esp, 8
        push        eax
        mov         ecx, 1
        callf       cl::signal-division-by-zero
    :next
        mov         edx, [ebp + (+ ARGS_OFFSET 4)]
		fld         [edx + (uvector-offset 2)]
        mov         edx, [ebp + ARGS_OFFSET]
		fdiv        [edx + (uvector-offset 2)]
        push        (* uvector-double-float-tag 8)
        callp       alloc-16-byte-uvector    ;; eax = double float node
        add         esp, 4    
   		fstp        [eax + (uvector-offset 2)]
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
    })

;; (a + bi) / (c + di) == (ac + bd)/(cc + dd) + ((bc - ad)/(cc + dd))i          
(defun divide-complex (x y)
    (let* ((a (realpart x))
           (c (realpart y))
           (b (imagpart x))
           (d (imagpart y))
           (temp (+ (* c c) (* d d))))  
        (complex (/ (+ (* a c) (* b d)) temp)
                 (/ (- (* b c) (* a d)) temp))))

(defun divide-non-numbers (x y)
    (unless (numberp x)
        (cl::signal-type-error x 'number))
    (unless (numberp y)
        (cl::signal-type-error y 'number)))
    
(defconstant divide-functions 
    (vector 
        #'divide-fixnums
        #'divide-bignums-2
        #'divide-ratios
        #'divide-short-floats
        #'divide-single-floats
        #'divide-double-floats
        #'divide-complex 
        #'divide-non-numbers))

(defasm divide-numbers (x y)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + (+ ARGS_OFFSET 4)]
        callp   num-type
        add     esp, 4
        push    eax
        push    [ebp + ARGS_OFFSET]
        callp   num-type
        add     esp, 4                  ;; eax = typeof(y)
        pop     edx                     ;; edx = typeof(x)
        cmp     edx, eax
        jl      short :less
        jg      short :greater
        
        ;; types are the same
    :same
        mov     edx, divide-functions
        shr     eax, 1
        mov     eax, [edx + (uvector-offset 2) + eax]   ;; eax = promotion function
        push    [ebp + (+ ARGS_OFFSET 4)]           ;; push x
        push    [ebp + ARGS_OFFSET]                 ;; push y
        mov     ecx, 2
    	callfunc	eax
        add     esp, 8
        jmp     short   :done
    
    :less
        push    eax
        push    [ebp + (+ ARGS_OFFSET 4)]
        push    edx
        push    eax
        mov     ecx, 3
        callp   promote-number
        add     esp, 12
        mov     [ebp + (+ ARGS_OFFSET 4)], eax      ;; promote x
        pop     eax                                 ;; eax = typeof(y)
        jmp     short :same
    
    :greater
        push    edx
        push    [ebp + ARGS_OFFSET]
        push    eax
        push    edx
        mov     ecx, 3
        callp   promote-number
        add     esp, 12
        mov     [ebp + ARGS_OFFSET], eax            ;; promote y
        pop     eax                                 ;; eax = typeof(y)
        jmp     short :same        
            
    :done
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })

(defasm add (#|&rest nums|#)
    {
        push    ebp
        mov     ebp, esp
        cmp     ecx, 2
        jl      :next1
        jg      :next3
        push    [ebp + (+ ARGS_OFFSET 4)]
        push    [ebp + (+ ARGS_OFFSET 0)]
        callp   add-numbers
        add     esp, 8
        jmp     short :done
    :next1
        cmp     ecx, 1
        jne     short :next2
        mov     eax, [ebp + (+ ARGS_OFFSET 0)]
        jmp     short :done
    :next2      
        mov     eax, 0      ; identity is 0
        jmp     short :done
    :next3
        mov     eax, [ebp + ecx*4 + (- (+ ARGS_OFFSET 0) 4)]
        dec     ecx
        dec     ecx
    :loop
        cmp     ecx, 0
        jl      :done
        push    ecx
        push    eax
        push    [ebp + ecx*4 + (+ ARGS_OFFSET 0)]
        callp   add-numbers
        add     esp, 8
        pop     ecx
        dec     ecx
        jmp     short :loop
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })


;;;
;;; Replace Common Lisp + operator
;;;
(setf (symbol-function '+) (symbol-function 'add))

;;;
;;;	Define code generation for Common Lisp + operator
(defcodegen + (form dest)
	(let ((num-args (- (length form) 1)))
		(cond ((= num-args 2)
			   (cl::compile-sub-form (second form) :dest-stack t)
			   (cl::compile-sub-form (third form) :dest-stack t)
			   (parse-assembler
					{
                        ;; quick check for adding two fixnums
                        xor     eax, eax
                        mov     al, [esp + 4]
                        or      al, [esp + 0]
                        and     al, 7
                        jne     short :next
                        pop     edx
                        pop     eax
                        add     eax, edx
                        jno      short :done
                        sub     eax, edx
                        push    eax
                        push    edx  
                    :next
						callp	x86::add-numbers
						add		esp, 8
                    :done 
					})
			   (x86::offset-stack 8))
			  ((= num-args 1)
			   (cl::compile-sub-form (second form) :dest-eax-operand t))
			  ((= num-args 0)
			   (parse-assembler
					{
						mov		eax, 0		;; fixnum 0
					}))
			  (t
			   (cl::compile-sub-form (second form) :dest-stack t)
			   (dotimes (i (- num-args 1))
				   (unless (zerop i)
					   (parse-assembler
							{
								push	eax
							})
					   (x86::offset-stack -4))
			   		(cl::compile-sub-form (elt form (+ i 2)) :dest-stack t)
				    (parse-assembler
						{
							callp	x86::add-numbers
							add		esp, 8
						})
					(x86::offset-stack 8))))
		(if (eq dest :dest-stack)
			(progn
				(parse-assembler
					{
						push	eax
					})
				(x86::offset-stack -4)))
		(if (eq dest :dest-eax)
			(progn
				(parse-assembler
					{
						mov		ecx, 1
					}))))
	t)

(defasm subtract (#|&rest nums|#)
    {
        push    ebp
        mov     ebp, esp
        cmp     ecx, 2
        jl      :next1
        jg      :next3
        push    [ebp + (+ ARGS_OFFSET 4)]
        push    [ebp + (+ ARGS_OFFSET 0)]
        callp   subtract-numbers
        add     esp, 8
        jmp     short :done
    :next1
        cmp     ecx, 1
        jne     short :next2
        push    0               ;; do negate
        push    [ebp + (+ ARGS_OFFSET 0)]
        mov     ecx, 2
        callp   subtract-numbers
        add     esp, 8
        jmp     short :done
    :next2      
        mov     eax, 0      ; identity is 0
        jmp     short :done
    :next3
        mov     eax, [ebp + ecx*4 + (- (+ ARGS_OFFSET 0) 4)]
        dec     ecx
        dec     ecx
    :loop
        cmp     ecx, 0
        jl      :done
        push    ecx
        push    eax
        push    [ebp + ecx*4 + (+ ARGS_OFFSET 0)]
        callp   subtract-numbers
        add     esp, 8
        pop     ecx
        dec     ecx
        jmp     short :loop
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

;;;
;;; Replace Common Lisp - operator
;;;
(setf (symbol-function '-) (symbol-function 'subtract))

;;;
;;;	Define code generation for Common Lisp - operator
(defcodegen - (form dest)
	(let ((num-args (- (length form) 1)))
		(cond ((= num-args 2)
			   (cl::compile-sub-form (second form) :dest-stack t)
			   (cl::compile-sub-form (third form) :dest-stack t)
			   (parse-assembler
					{
                        ;; quick check for subtract of two fixnums
                        xor     eax, eax
                        mov     al, [esp + 4]
                        or      al, [esp + 0]
                        and     al, 7
                        jne     short :next
                        pop     edx
                        pop     eax
                        sub     eax, edx
                        jno     short :done
                        add     eax, edx
                        push    eax
                        push    edx  
                    :next
						callp	x86::subtract-numbers
						add		esp, 8
                    :done 
					})
			   (x86::offset-stack 8))
			  ((= num-args 1)
               (parse-assembler
                    {
                        push    0
                    })
			   (cl::compile-sub-form (second form) :dest-stack t)
               (parse-assembler
                    {
						callp	x86::subtract-numbers
						add		esp, 8
                    })
                (x86::offset-stack 4))                
			  ((= num-args 0)
			   (parse-assembler
					{
						mov		eax, 0		;; fixnum 0
					}))
			  (t
			   (cl::compile-sub-form (second form) :dest-stack t)
			   (dotimes (i (- num-args 1))
				   (unless (zerop i)
					   (parse-assembler
							{
								push	eax
							})
					   (x86::offset-stack -4))
			   		(cl::compile-sub-form (elt form (+ i 2)) :dest-stack t)
				    (parse-assembler
						{
							callp	x86::subtract-numbers
							add		esp, 8
						})
					(x86::offset-stack 8))))
		(if (eq dest :dest-stack)
			(progn
				(parse-assembler
					{
						push	eax
					})
				(x86::offset-stack -4)))
		(if (eq dest :dest-eax)
			(progn
				(parse-assembler
					{
						mov		ecx, 1
					}))))
	t)
	
(defasm multiply (#|&rest nums|#)
    {
        push    ebp
        mov     ebp, esp
        cmp     ecx, 2
        jl      :next1
        jg      :next3
        push    [ebp + (+ ARGS_OFFSET 4)]
        push    [ebp + (+ ARGS_OFFSET 0)]
        callp   multiply-numbers
        add     esp, 8
        jmp     short :done
    :next1
        cmp     ecx, 1
        jne     short :next2
        mov     eax, [ebp + (+ ARGS_OFFSET 0)]
        jmp     short :done
    :next2      
        mov     eax, 8      ; identity is 1
        jmp     short :done
    :next3
        mov     eax, [ebp + ecx*4 + (- (+ ARGS_OFFSET 0) 4)]
        dec     ecx
        dec     ecx
    :loop
        cmp     ecx, 0
        jl      :done
        push    ecx
        push    eax
        push    [ebp + ecx*4 + (+ ARGS_OFFSET 0)]
        callp   multiply-numbers
        add     esp, 8
        pop     ecx
        dec     ecx
        jmp     short :loop
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

;;;
;;; Replace Common Lisp * operator
;;;
(setf (symbol-function '*) (symbol-function 'multiply))

;;;
;;;	Define code generation for Common Lisp * operator
(defcodegen * (form dest)
	(let ((num-args (- (length form) 1)))
		(cond ((= num-args 2)
			   (cl::compile-sub-form (second form) :dest-stack t)
			   (cl::compile-sub-form (third form) :dest-stack t)
			   (parse-assembler
					{
						callp	x86::multiply-numbers
						add		esp, 8
					})
			   (x86::offset-stack 8))
			  ((= num-args 1)
			   (cl::compile-sub-form (second form) :dest-eax-operand t))
			  ((= num-args 0)
			   (parse-assembler
					{
						mov		eax, 8		;; fixnum 1
					}))
			  (t
			   (cl::compile-sub-form (second form) :dest-stack t)
			   (dotimes (i (- num-args 1))
				   (unless (zerop i)
					   (parse-assembler
							{
								push	eax
							})
					   (x86::offset-stack -4))
			   		(cl::compile-sub-form (elt form (+ i 2)) :dest-stack t)
				    (parse-assembler
						{
							callp	x86::multiply-numbers
							add		esp, 8
						})
					(x86::offset-stack 8))))
		(if (eq dest :dest-stack)
			(progn
				(parse-assembler
					{
						push	eax
					})
				(x86::offset-stack -4)))
		(if (eq dest :dest-eax)
			(progn
				(parse-assembler
					{
						mov		ecx, 1
					}))))
	t)

(defasm divide (#|&rest nums|#)
    {
        push    ebp
        mov     ebp, esp
        cmp     ecx, 2
        jl      short :next1
        jg      short :next3
        push    [ebp + (+ ARGS_OFFSET 4)]
        push    [ebp + (+ ARGS_OFFSET 0)]
        callp   divide-numbers
        add     esp, 8
        jmp     short :done
    :next1
        cmp     ecx, 1
        jne     short :next2
        push    8               ;; push wrapped(1)
        push    [ebp + (+ ARGS_OFFSET 0)]
        mov     ecx, 2
        callp   divide-numbers
        add     esp, 8
        jmp     short :done
    :next2
        callp   x86::_wrong-number-of-args-error          
    :next3
        mov     eax, [ebp + ecx*4 + (- (+ ARGS_OFFSET 0) 4)]
        dec     ecx
        dec     ecx
    :loop
        cmp     ecx, 0
        jl      :done
        push    ecx
        push    eax
        push    [ebp + ecx*4 + (+ ARGS_OFFSET 0)]
        callp   divide-numbers
        add     esp, 8
        pop     ecx
        dec     ecx
        jmp     short :loop
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

;;;
;;; Replace Common Lisp / operator
;;;
(setf (symbol-function '/) (symbol-function 'divide))

;;;
;;;	Define code generation for Common Lisp / operator
(defcodegen / (form dest)
	(let ((num-args (- (length form) 1)))
		(cond ((= num-args 2)
			   (cl::compile-sub-form (second form) :dest-stack t)
			   (cl::compile-sub-form (third form) :dest-stack t)
			   (parse-assembler
					{
						callp	x86::divide-numbers
						add		esp, 8
					})
			   (x86::offset-stack 8))
			  ((= num-args 1)
               (parse-assembler
                    {
                        push    8       ; wrapped(1)
                    })
			   (cl::compile-sub-form (second form) :dest-stack t)
               (parse-assembler
                    {
						callp	x86::divide-numbers
						add		esp, 8
                    })
                (x86::offset-stack 4))                
  			  ((= num-args 0)
			   (parse-assembler
					{
                        callp   x86::_wrong-number-of-args-error          
  					}))
			  (t
			   (cl::compile-sub-form (second form) :dest-stack t)
			   (dotimes (i (- num-args 1))
				   (unless (zerop i)
					   (parse-assembler
							{
								push	eax
							})
					   (x86::offset-stack -4))
			   		(cl::compile-sub-form (elt form (+ i 2)) :dest-stack t)
				    (parse-assembler
						{
							callp	x86::divide-numbers
							add		esp, 8
						})
					(x86::offset-stack 8))))
		(if (eq dest :dest-stack)
			(progn
				(parse-assembler
					{
						push	eax
					})
				(x86::offset-stack -4)))
		(if (eq dest :dest-eax)
			(progn
				(parse-assembler
					{
						mov		ecx, 1
					}))))
	t)

;; This function is called by code generated by the compiler.
;; We redefine it here so it calls our new function.
(defasm cl::%plus_eax_edx ()
    {
        push    ebp
        mov     ebp, esp
        push    edx
        push    eax
        mov     ecx, 2
        callp   x86::add-numbers
        add     esp, 8
        pop     ebp
        ret
    })

;; This function is called by code generated by the compiler.
;; We redefine it here so it calls our new function.
(defasm cl::%minus_eax_edx ()
    {
        push    ebp
        mov     ebp, esp
        push    eax
        push    edx
        mov     ecx, 2
        callp   x86::subtract-numbers
        add     esp, 8
        pop     ebp
        ret
    })

(defasm numeric-compare-fixnums (n1 n2)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + (+ ARGS_OFFSET 4)]
        cmp     eax, [ebp + (+ ARGS_OFFSET 0)]
        jg      short :gt
        jl      short :lt
        mov     eax, 0
        jmp     short :done
    :gt
        mov     eax, 8
        jmp     short :done
    :lt
        mov     eax, -8
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

(defun numeric-compare (n1 n2) (if (< n1 n2) -1 (if (= n1 n2) 0 1)))    ;; redefined later

(defun numeric-compare-ratio (n1 n2)
    (let ((num1 (numerator n1))
          (den1 (denominator n1))
          (num2 (numerator n2))
          (den2 (denominator n2)))
        (numeric-compare (* num1 den2) (* num2 den1))))

(defasm numeric-compare-short-float (n1 n2)
    {
        push    ebp
        mov     ebp, esp
        fld.single [ebp + (+ ARGS_OFFSET 4)]            ;; push n1
        fcomp.single [ebp + (+ ARGS_OFFSET 0)]          ;; cmp n1, n2
        xor     eax, eax
        fstsw_AX
        test     eax, #x4000
        jne		short :equal
    :not-equal
        test    eax, #x0100
        jne     short :less
    :greater
        mov     eax, 8                                  ;; eax = 1 
        jmp     short :done
    :less
        mov     eax, -8                                 ;; eax = -1
        jmp     short :done
    :equal
        mov     eax, 0                                  ;; eax = 0 
    :done
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })

(defasm numeric-compare-single-float (n1 n2)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + (+ ARGS_OFFSET 4)]
        fld.single [eax + (uvector-offset 1)]            ;; push n1
        mov     eax, [ebp + (+ ARGS_OFFSET 0)]
        fcomp.single [eax + (uvector-offset 1)]          ;; cmp n1, n2
        xor     eax, eax
        fstsw_AX
        test     eax, #x4000
        jne		short :equal
    :not-equal
        test    eax, #x0100
        jne     short :less
    :greater
        mov     eax, 8                                  ;; eax = 1 
        jmp     short :done
    :less
        mov     eax, -8                                 ;; eax = -1
        jmp     short :done
    :equal
        mov     eax, 0                                  ;; eax = 0 
    :done
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })

(defasm numeric-compare-double-float (n1 n2)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + (+ ARGS_OFFSET 4)]
        fld     [eax + (uvector-offset 2)]              ;; push n1
        mov     eax, [ebp + (+ ARGS_OFFSET 0)]
        fcomp   [eax + (uvector-offset 2)]              ;; cmp n1, n2
        xor     eax, eax
        fstsw_AX
        test     eax, #x4000
        jne		short :equal
    :not-equal
        test    eax, #x0100
        jne     short :less
    :greater
        mov     eax, 8                                  ;; eax = 1 
        jmp     short :done
    :less
        mov     eax, -8                                 ;; eax = -1
        jmp     short :done
    :equal
        mov     eax, 0                                  ;; eax = 0 
    :done
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })

;;
;; Inequalities (<, <=, >, >=) are not defined for complex type.
;; Only = and /= are defined. When not equal, this function will always
;; return 1.
;;
(defun numeric-compare-complex (n1 n2)
    (if (and (= (realpart n1) (realpart n2)) (= (imagpart n1) (imagpart n2)))
        0
        1))
    
(defun compare-non-numbers (x y)
    (unless (numberp x)
        (cl::signal-type-error x 'number))
    (unless (numberp y)
        (cl::signal-type-error y 'number)))

(defconstant numeric-compare-functions 
    (vector 
        #'numeric-compare-fixnums
        #'bignum-compare
        #'numeric-compare-ratio
        #'numeric-compare-short-float
        #'numeric-compare-single-float
        #'numeric-compare-double-float
        #'numeric-compare-complex 
        #'compare-non-numbers))

(defasm numeric-compare-numbers (x y)
    {
        push    ebp
        mov     ebp, esp
    
        ;; quick check for comparing two fixnums
        xor     eax, eax
        mov     al, [ebp + (+ ARGS_OFFSET 4)]
        or      al, [ebp + ARGS_OFFSET]
        and     al, 7
        jne     :next
        
        ;; attempt fixnum compare
		mov     eax, [ebp + (+ ARGS_OFFSET 4)]
		cmp     eax, [ebp + ARGS_OFFSET]
		jl      short :lt
        jg      short :gt
        mov     eax, 0
        jmp     :done
    :lt
        mov     eax, -8
        jmp     short :done
    :gt
        mov     eax, 8
        jmp     short :done
       
    :next    
        push    [ebp + (+ ARGS_OFFSET 4)]
        callp   num-type
        add     esp, 4
        push    eax
        push    [ebp + ARGS_OFFSET]
        callp   num-type
        add     esp, 4                  ;; eax = typeof(y)
        pop     edx                     ;; edx = typeof(x)
        cmp     edx, eax
        jl      short :less
        jg      short :greater
        
        ;; types are the same
    :same
        mov     edx, numeric-compare-functions
        shr     eax, 1
        mov     eax, [edx + (uvector-offset 2) + eax]   ;; eax = promotion function
        push    [ebp + (+ ARGS_OFFSET 4)]           ;; push x
        push    [ebp + ARGS_OFFSET]                 ;; push y
        mov     ecx, 2
   		callfunc	eax
        add     esp, 8
        jmp     short   :done
    
    :less
        push    eax
        push    [ebp + (+ ARGS_OFFSET 4)]
        push    edx
        push    eax
        mov     ecx, 3
        callp   promote-number
        add     esp, 12
        mov     [ebp + (+ ARGS_OFFSET 4)], eax      ;; promote x
        pop     eax                                 ;; eax = typeof(y)
        jmp     short :same
    
    :greater
        push    edx
        push    [ebp + ARGS_OFFSET]
        push    eax
        push    edx
        mov     ecx, 3
        callp   promote-number
        add     esp, 12
        mov     [ebp + ARGS_OFFSET], eax            ;; promote y
        pop     eax                                 ;; eax = typeof(y)
        jmp     short :same        
            
    :done
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })

(defasm less-than (#|&rest nums|#)
    {
        push    ebp
        mov     ebp, esp
        cmp     ecx, 2
        jl      :next1
        jg      :next3
        push    [ebp + (+ ARGS_OFFSET 4)]
        push    [ebp + (+ ARGS_OFFSET 0)]
        callp   numeric-compare-numbers
        add     esp, 8
        cmp     eax, -8
        jne     short :false
        jmp     short :true
     :next1
        cmp     ecx, 1
        jne     short :next2
        jmp     short :true     ; single arg case always true
    :next2      
        callp   x86::_wrong-number-of-args-error          
    :next3
        mov     eax, [ebp + ecx*4 + (- (+ ARGS_OFFSET 0) 4)]
        dec     ecx
        dec     ecx
    :loop
        cmp     ecx, 0
        jl      :true
        push    ecx
        push    eax
        push    [ebp + ecx*4 + (+ ARGS_OFFSET 0)]
        callp   numeric-compare-numbers
        add     esp, 8
        pop     ecx
        cmp     eax, -8
        jne     short :false
        mov     eax, [ebp + ecx*4 + (+ ARGS_OFFSET 0)]
        dec     ecx
        jmp     short :loop
    :true
        mov     eax, [esi + 4]
        jmp     short :done
    :false
        mov     eax, [esi]
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

(defasm less-than-or-equal (#|&rest nums|#)
    {
        push    ebp
        mov     ebp, esp
        cmp     ecx, 2
        jl      :next1
        jg      :next3
        push    [ebp + (+ ARGS_OFFSET 4)]
        push    [ebp + (+ ARGS_OFFSET 0)]
        callp   numeric-compare-numbers
        add     esp, 8
        cmp     eax, 8
        je     short :false
        jmp     short :true
     :next1
        cmp     ecx, 1
        jne     short :next2
        jmp     short :true     ; single arg case always true
    :next2      
        callp   x86::_wrong-number-of-args-error          
    :next3
        mov     eax, [ebp + ecx*4 + (- (+ ARGS_OFFSET 0) 4)]
        dec     ecx
        dec     ecx
    :loop
        cmp     ecx, 0
        jl      :true
        push    ecx
        push    eax
        push    [ebp + ecx*4 + (+ ARGS_OFFSET 0)]
        callp   numeric-compare-numbers
        add     esp, 8
        pop     ecx
        cmp     eax, 8
        je     short :false
        mov     eax, [ebp + ecx*4 + (+ ARGS_OFFSET 0)]
        dec     ecx
        jmp     short :loop
    :true
        mov     eax, [esi + 4]
        jmp     short :done
    :false
        mov     eax, [esi]
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

(defasm greater-than (#|&rest nums|#)
    {
        push    ebp
        mov     ebp, esp
        cmp     ecx, 2
        jl      :next1
        jg      :next3
        push    [ebp + (+ ARGS_OFFSET 4)]
        push    [ebp + (+ ARGS_OFFSET 0)]
        callp   numeric-compare-numbers
        add     esp, 8
        cmp     eax, 8
        jne     short :false
        jmp     short :true
     :next1
        cmp     ecx, 1
        jne     short :next2
        jmp     short :true     ; single arg case always true
    :next2      
        callp   x86::_wrong-number-of-args-error          
    :next3
        mov     eax, [ebp + ecx*4 + (- (+ ARGS_OFFSET 0) 4)]
        dec     ecx
        dec     ecx
    :loop
        cmp     ecx, 0
        jl      :true
        push    ecx
        push    eax
        push    [ebp + ecx*4 + (+ ARGS_OFFSET 0)]
        callp   numeric-compare-numbers
        add     esp, 8
        pop     ecx
        cmp     eax, 8
        jne     short :false
        mov     eax, [ebp + ecx*4 + (+ ARGS_OFFSET 0)]
        dec     ecx
        jmp     short :loop
    :true
        mov     eax, [esi + 4]
        jmp     short :done
    :false
        mov     eax, [esi]
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

(defasm greater-than-or-equal (#|&rest nums|#)
    {
        push    ebp
        mov     ebp, esp
        cmp     ecx, 2
        jl      :next1
        jg      :next3
        push    [ebp + (+ ARGS_OFFSET 4)]
        push    [ebp + (+ ARGS_OFFSET 0)]
        callp   numeric-compare-numbers
        add     esp, 8
        cmp     eax, -8
        je     short :false
        jmp     short :true
     :next1
        cmp     ecx, 1
        jne     short :next2
        jmp     short :true     ; single arg case always true
    :next2      
        callp   x86::_wrong-number-of-args-error          
    :next3
        mov     eax, [ebp + ecx*4 + (- (+ ARGS_OFFSET 0) 4)]
        dec     ecx
        dec     ecx
    :loop
        cmp     ecx, 0
        jl      :true
        push    ecx
        push    eax
        push    [ebp + ecx*4 + (+ ARGS_OFFSET 0)]
        callp   numeric-compare-numbers
        add     esp, 8
        pop     ecx
        cmp     eax, -8
        je     short :false
        mov     eax, [ebp + ecx*4 + (+ ARGS_OFFSET 0)]
        dec     ecx
        jmp     short :loop
    :true
        mov     eax, [esi + 4]
        jmp     short :done
    :false
        mov     eax, [esi]
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

(defasm numeric-equal (#|&rest nums|#)
    {
        push    ebp
        mov     ebp, esp
        cmp     ecx, 2
        jl      :next1
        jg      :next3
        push    [ebp + (+ ARGS_OFFSET 4)]
        push    [ebp + (+ ARGS_OFFSET 0)]
        callp   numeric-compare-numbers
        add     esp, 8
        cmp     eax, 0
        jne     short :false
        jmp     short :true
     :next1
        cmp     ecx, 1
        jne     short :next2
        jmp     short :true     ; single arg case always true
    :next2      
        callp   x86::_wrong-number-of-args-error          
    :next3
        mov     eax, [ebp + ecx*4 + (- (+ ARGS_OFFSET 0) 4)]
        dec     ecx
        dec     ecx
    :loop
        cmp     ecx, 0
        jl      :true
        push    ecx
        push    eax
        push    [ebp + ecx*4 + (+ ARGS_OFFSET 0)]
        callp   numeric-compare-numbers
        add     esp, 8
        pop     ecx
        cmp     eax, 0
        jne     short :false
        mov     eax, [ebp + ecx*4 + (+ ARGS_OFFSET 0)]
        dec     ecx
        jmp     short :loop
    :true
        mov     eax, [esi + 4]
        jmp     short :done
    :false
        mov     eax, [esi]
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

(defasm numeric-not-equal (#|&rest nums|#)
    {
        push    ebp
        mov     ebp, esp
        cmp     ecx, 2
        jl      :next1
        jg      :next3
        push    [ebp + (+ ARGS_OFFSET 4)]
        push    [ebp + (+ ARGS_OFFSET 0)]
        callp   numeric-compare-numbers
        add     esp, 8
        cmp     eax, 0
        je     short :false
        jmp     short :true
     :next1
        cmp     ecx, 1
        jne     short :next2
        jmp     short :true     ; single arg case always true
    :next2      
        callp   x86::_wrong-number-of-args-error          
    :next3
        mov     eax, [ebp + ecx*4 + (- (+ ARGS_OFFSET 0) 4)]
        dec     ecx
        dec     ecx
    :loop
        cmp     ecx, 0
        jl      :true
        push    ecx
        push    eax
        push    [ebp + ecx*4 + (+ ARGS_OFFSET 0)]
        callp   numeric-compare-numbers
        add     esp, 8
        pop     ecx
        cmp     eax, 0
        je      short :false
        mov     eax, [ebp + ecx*4 + (+ ARGS_OFFSET 0)]
        dec     ecx
        jmp     short :loop
    :true
        mov     eax, [esi + 4]
        jmp     short :done
    :false
        mov     eax, [esi]
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

;;;
;;; Replace Common Lisp = operator
;;;
(setf (symbol-function '=) (symbol-function 'numeric-equal))

;;;
;;; Replace Common Lisp /= operator
;;;
(setf (symbol-function '/=) (symbol-function 'numeric-not-equal))

;;;
;;; Replace Common Lisp < operator
;;;
(setf (symbol-function '<) (symbol-function 'less-than))

;;;
;;; Replace Common Lisp <= operator
;;;
(setf (symbol-function '<=) (symbol-function 'less-than-or-equal))

;;;
;;; Replace Common Lisp > operator
;;;
(setf (symbol-function '>) (symbol-function 'greater-than))

;;;
;;; Replace Common Lisp >= operator
;;;
(setf (symbol-function '>=) (symbol-function 'greater-than-or-equal))

;;;
;;;	Replace BIGNUM-SHIFT in kernel (called by ASH).
;;;
(defun cl::bignum-shift (bn bits)
    (x86::bignum-shift bn bits t))

;;;
;;; Common Lisp FLOAT operator
;;;
(defun cl::float (number &optional prototype)
    (if (and (floatp number) (null prototype))
        number
        (let ((target-type (if prototype (num-type prototype) SingleFloatID))
              (source-type (num-type number)))
            (if (and prototype (or (< target-type ShortFloatID) (> target-type DoubleFloatID)))
                (cl::signal-type-error prototype 'float))
            (unless (< source-type ComplexID)
                (cl::signal-type-error number 'real))
            (cond ((eq source-type target-type) number)
                  ((< source-type target-type)             
                   (promote-number number source-type target-type))
                  (t (if (eq source-type DoubleFloatID)
                        (if (eq target-type SingleFloatID)
                            (double-float-to-single-float number)
                            (double-float-to-short-float number))
                        (single-float-to-short-float number)))))))  
        
(define-compiler-macro cl::max (&whole form &rest args)
    (declare (ignore form))
    (cond
        ((= (length args) 2)
         (let ((t1 (gensym))(t2 (gensym)))
            `(let ((,t1 ,(car args))
                   (,t2 ,(cadr args)))
                (if (>= ,t1 ,t2) 
                    ,t1
                    ,t2))))
        ((= (length args) 1)
            `,(car args))
        (t (let ((t1 (gensym))(t2 (gensym)))
            `(let* ((,t1 ,(car args))
                    (,t2 (cl::max ,@(cdr args))))
                (if (>= ,t1 ,t2) 
                    ,t1
                    ,t2))))))

(define-compiler-macro cl::min (&whole form &rest args)
    (declare (ignore form))
    (cond
        ((= (length args) 2)
         (let ((t1 (gensym))(t2 (gensym)))
            `(let ((,t1 ,(car args))
                   (,t2 ,(cadr args)))
                (if (<= ,t1 ,t2) 
                    ,t1
                    ,t2))))
        ((= (length args) 1)
            `,(car args))
        (t (let ((t1 (gensym))(t2 (gensym)))
            `(let* ((,t1 ,(car args))
                    (,t2 (cl::min ,@(cdr args))))
                (if (<= ,t1 ,t2) 
                    ,t1
                    ,t2))))))

(defasm xor-numbers (i1 i2)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + (+ ARGS_OFFSET 4)]  ; eax = i1
        mov     edx, [ebp + (+ ARGS_OFFSET 0)]  ; edx = i2
        mov     cl, al
        or      cl, dl
        and     cl, 7
        jne     short :do-bignum
        xor     eax, edx
        jmp     short :done
    :do-bignum
        test    eax, 7
        jne     short :next1
        push    eax
        callp   fixnum-to-bignum
        add     esp, 4
    :next1
        push    eax
        mov     eax, [ebp + (+ ARGS_OFFSET 0)]  ; eax = i2
        test    eax, 7
        jne     short :next2
        push    eax
        callp   fixnum-to-bignum
        add     esp, 4
    :next2
        push    eax
        mov     ecx, 2
        callp   xor-bignums
        add     esp, 8
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

;;;
;;; Replace Common Lisp LOGXOR operator
;;;
(defasm cl::logxor ()       ; &rest nums
    {
        push    ebp
        mov     ebp, esp
        cmp     ecx, 2
        jl      :next1
        jg      :next3
        push    [ebp + (+ ARGS_OFFSET 4)]
        push    [ebp + (+ ARGS_OFFSET 0)]
        callp   xor-numbers
        add     esp, 8
        jmp     short :done
    :next1
        cmp     ecx, 1
        jne     short :next2
        mov     eax, [ebp + (+ ARGS_OFFSET 0)]
        jmp     short :done
    :next2      
        mov     eax, 0      ; identity is 0
        jmp     short :done
    :next3
        mov     eax, [ebp + ecx*4 + (- (+ ARGS_OFFSET 0) 4)]
        dec     ecx
        dec     ecx
    :loop
        cmp     ecx, 0
        jl      :done
        push    ecx
        push    eax
        push    [ebp + ecx*4 + (+ ARGS_OFFSET 0)]
        callp   xor-numbers
        add     esp, 8
        pop     ecx
        dec     ecx
        jmp     short :loop
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

;;;
;;;	Define code generation for Common Lisp LOGXOR operator
;;;
(defcodegen logxor (form dest)
	(let ((num-args (- (length form) 1)))
		(cond ((= num-args 2)
			   (cl::compile-sub-form (second form) :dest-stack t)
			   (cl::compile-sub-form (third form) :dest-stack t)
			   (parse-assembler
					{
                        ;; quick check for adding two fixnums
                        xor     eax, eax
                        mov     al, [esp + 4]
                        or      al, [esp + 0]
                        and     al, 7
                        jne     short :next
                        pop     edx
                        pop     eax
                        xor     eax, edx
                        jmp      short :done
                    :next
						callp	x86::xor-numbers
						add		esp, 8
                    :done 
					})
			   (x86::offset-stack 8))
			  ((= num-args 1)
			   (cl::compile-sub-form (second form) :dest-eax-operand t))
			  ((= num-args 0)
			   (parse-assembler
					{
						mov		eax, 0		;; fixnum 0
					}))
			  (t
			   (cl::compile-sub-form (second form) :dest-stack t)
			   (dotimes (i (- num-args 1))
				   (unless (zerop i)
					   (parse-assembler
							{
								push	eax
							})
					   (x86::offset-stack -4))
			   		(cl::compile-sub-form (elt form (+ i 2)) :dest-stack t)
				    (parse-assembler
						{
							callp	x86::xor-numbers
							add		esp, 8
						})
					(x86::offset-stack 8))))
		(if (eq dest :dest-stack)
			(progn
				(parse-assembler
					{
						push	eax
					})
				(x86::offset-stack -4)))
		(if (eq dest :dest-eax)
			(progn
				(parse-assembler
					{
						mov		ecx, 1
					}))))
	t)

(defasm ior-numbers (i1 i2)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + (+ ARGS_OFFSET 4)]  ; eax = i1
        mov     edx, [ebp + (+ ARGS_OFFSET 0)]  ; edx = i2
        mov     cl, al
        or      cl, dl
        and     cl, 7
        jne     short :do-bignum
        or      eax, edx
        jmp     short :done
    :do-bignum
        test    eax, 7
        jne     short :next1
        push    eax
        callp   fixnum-to-bignum
        add     esp, 4
    :next1
        push    eax
        mov     eax, [ebp + (+ ARGS_OFFSET 0)]  ; eax = i2
        test    eax, 7
        jne     short :next2
        push    eax
        callp   fixnum-to-bignum
        add     esp, 4
    :next2
        push    eax
        mov     ecx, 2
        callp   or-bignums
        add     esp, 8
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

;;;
;;; Replace Common Lisp LOGIOR operator
;;;
(defasm cl::logior ()   ; &rest nums
    {
        push    ebp
        mov     ebp, esp
        cmp     ecx, 2
        jl      :next1
        jg      :next3
        push    [ebp + (+ ARGS_OFFSET 4)]
        push    [ebp + (+ ARGS_OFFSET 0)]
        callp   ior-numbers
        add     esp, 8
        jmp     short :done
    :next1
        cmp     ecx, 1
        jne     short :next2
        mov     eax, [ebp + (+ ARGS_OFFSET 0)]
        jmp     short :done
    :next2      
        mov     eax, 0      ; identity is 0
        jmp     short :done
    :next3
        mov     eax, [ebp + ecx*4 + (- (+ ARGS_OFFSET 0) 4)]
        dec     ecx
        dec     ecx
    :loop
        cmp     ecx, 0
        jl      :done
        push    ecx
        push    eax
        push    [ebp + ecx*4 + (+ ARGS_OFFSET 0)]
        callp   ior-numbers
        add     esp, 8
        pop     ecx
        dec     ecx
        jmp     short :loop
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

;;;
;;;	Define code generation for Common Lisp LOGIOR operator
;;;
(defcodegen logior (form dest)
	(let ((num-args (- (length form) 1)))
		(cond ((= num-args 2)
			   (cl::compile-sub-form (second form) :dest-stack t)
			   (cl::compile-sub-form (third form) :dest-stack t)
			   (parse-assembler
					{
                        ;; quick check for adding two fixnums
                        xor     eax, eax
                        mov     al, [esp + 4]
                        or      al, [esp + 0]
                        and     al, 7
                        jne     short :next
                        pop     edx
                        pop     eax
                        or      eax, edx
                        jmp      short :done
                    :next
						callp	x86::ior-numbers
						add		esp, 8
                    :done 
					})
			   (x86::offset-stack 8))
			  ((= num-args 1)
			   (cl::compile-sub-form (second form) :dest-eax-operand t))
			  ((= num-args 0)
			   (parse-assembler
					{
						mov		eax, 0		;; fixnum 0
					}))
			  (t
			   (cl::compile-sub-form (second form) :dest-stack t)
			   (dotimes (i (- num-args 1))
				   (unless (zerop i)
					   (parse-assembler
							{
								push	eax
							})
					   (x86::offset-stack -4))
			   		(cl::compile-sub-form (elt form (+ i 2)) :dest-stack t)
				    (parse-assembler
						{
							callp	x86::ior-numbers
							add		esp, 8
						})
					(x86::offset-stack 8))))
		(if (eq dest :dest-stack)
			(progn
				(parse-assembler
					{
						push	eax
					})
				(x86::offset-stack -4)))
		(if (eq dest :dest-eax)
			(progn
				(parse-assembler
					{
						mov		ecx, 1
					}))))
	t)
 
(defasm and-numbers (i1 i2)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + (+ ARGS_OFFSET 4)]  ; eax = i1
        mov     edx, [ebp + (+ ARGS_OFFSET 0)]  ; edx = i2
        mov     cl, al
        or      cl, dl
        and     cl, 7
        jne     short :do-bignum
        and     eax, edx
        jmp     short :done
    :do-bignum
        test    eax, 7
        jne     short :next1
        push    eax
        callp   fixnum-to-bignum
        add     esp, 4
    :next1
        push    eax
        mov     eax, [ebp + (+ ARGS_OFFSET 0)]  ; eax = i2
        test    eax, 7
        jne     short :next2
        push    eax
        callp   fixnum-to-bignum
        add     esp, 4
    :next2
        push    eax
        mov     ecx, 2
        callp   and-bignums
        add     esp, 8
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

;;;
;;; Replace Common Lisp LOGAND operator
;;;
(defasm cl::logand ()   ; &rest nums
    {
        push    ebp
        mov     ebp, esp
        cmp     ecx, 2
        jl      :next1
        jg      :next3
        push    [ebp + (+ ARGS_OFFSET 4)]
        push    [ebp + (+ ARGS_OFFSET 0)]
        callp   and-numbers
        add     esp, 8
        jmp     short :done
    :next1
        cmp     ecx, 1
        jne     short :next2
        mov     eax, [ebp + (+ ARGS_OFFSET 0)]
        jmp     short :done
    :next2      
        mov     eax, -8      ; identity is -1
        jmp     short :done
    :next3
        mov     eax, [ebp + ecx*4 + (- (+ ARGS_OFFSET 0) 4)]
        dec     ecx
        dec     ecx
    :loop
        cmp     ecx, 0
        jl      :done
        push    ecx
        push    eax
        push    [ebp + ecx*4 + (+ ARGS_OFFSET 0)]
        callp   and-numbers
        add     esp, 8
        pop     ecx
        dec     ecx
        jmp     short :loop
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

;;;
;;;	Define code generation for Common Lisp LOGIOR operator
;;;
(defcodegen logand (form dest)
	(let ((num-args (- (length form) 1)))
		(cond ((= num-args 2)
			   (cl::compile-sub-form (second form) :dest-stack t)
			   (cl::compile-sub-form (third form) :dest-stack t)
			   (parse-assembler
					{
                        ;; quick check for adding two fixnums
                        xor     eax, eax
                        mov     al, [esp + 4]
                        or      al, [esp + 0]
                        and     al, 7
                        jne     short :next
                        pop     edx
                        pop     eax
                        and     eax, edx
                        jmp      short :done
                    :next
						callp	x86::and-numbers
						add		esp, 8
                    :done 
					})
			   (x86::offset-stack 8))
			  ((= num-args 1)
			   (cl::compile-sub-form (second form) :dest-eax-operand t))
			  ((= num-args 0)
			   (parse-assembler
					{
						mov		eax, -8		;; fixnum -1
					}))
			  (t
			   (cl::compile-sub-form (second form) :dest-stack t)
			   (dotimes (i (- num-args 1))
				   (unless (zerop i)
					   (parse-assembler
							{
								push	eax
							})
					   (x86::offset-stack -4))
			   		(cl::compile-sub-form (elt form (+ i 2)) :dest-stack t)
				    (parse-assembler
						{
							callp	x86::and-numbers
							add		esp, 8
						})
					(x86::offset-stack 8))))
		(if (eq dest :dest-stack)
			(progn
				(parse-assembler
					{
						push	eax
					})
				(x86::offset-stack -4)))
		(if (eq dest :dest-eax)
			(progn
				(parse-assembler
					{
						mov		ecx, 1
					}))))
	t)
    
;;;
;;; Replace Common Lisp LOGNOT operator
;;;
(defasm cl::lognot (i)
    {
        push    ebp
        mov     ebp, esp
        cmp     ecx, 1
		jz 		short :next1
		callp 	_wrong-number-of-args-error
	:next1
        mov     eax, [ebp + ARGS_OFFSET]
        test    al, 7
        jne     :bignum
        not     eax
        and     eax, -8
        jmp     short :done
    :bignum
        push    eax
        callp   not-bignum
        add     esp, 4
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })
    
;;;
;;; Common Lisp LOGANDC1 operator
;;;
(defun logandc1 (integer-1 integer-2)
    (logand (lognot integer-1) integer-2))

;;;
;;; Common Lisp LOGANDC2 operator
;;;
(defun logandc2 (integer-1 integer-2)
    (logand integer-1 (lognot integer-2)))


;;;
;;; Common Lisp LOGEQV operator
;;;
(defun logeqv (&rest integers)
    (let ((x -1))
        (dolist (i integers)
            (setq x (lognot (logxor x i))))
        x))

;;;
;;; Common Lisp LOGNAND operator
;;;
(defun lognand (integer-1 integer-2)
    (lognot (logand integer-1 integer-2)))

;;;
;;; Common Lisp LOGNOR operator
;;;
(defun lognor (integer-1 integer-2)
    (lognot (logior integer-1 integer-2)))

;;;
;;; Common Lisp LOGORC1 operator
;;;
(defun logorc1 (integer-1 integer-2)
    (logior (lognot integer-1) integer-2))

;;;
;;; Common Lisp LOGORC2 operator
;;;
(defun logorc2 (integer-1 integer-2)
    (logior integer-1 (lognot integer-2)))

;;;
;;; Redefine this function here to use new math routines.
;;;
(defasm fixnum-shift (int shift)
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
		callp	x86::fixnum-to-bignum
		add		esp, 4
		mov		ecx, [ebp + ARGS_OFFSET]		;eax = shift amount
		push	eax
		push	ecx
		mov		ecx, 2
		callp	x86::bignum-shift-signed
		add		esp, 8
	:exit
		mov		ecx, 1
		pop		ebp
		ret
	})

;;;
;;; Common Lisp ASH operator
;;;
(defun ash (int shift)
	(unless (fixnump shift)
		(cl::signal-type-error shift 'fixnum))
	(if (fixnump int)
		(fixnum-shift int shift)
		(if (bignump int)
			(x86::bignum-shift int shift t)
            (cl::signal-type-error int 'integer))))

;;;
;;; Common Lisp COMPLEX operator
;;;
(defun complex (realpart &optional (imagpart 0))
    (unless (realp realpart)
        (cl::signal-type-error realpart 'real))
    (unless (realp imagpart)
        (cl::signal-type-error imagpart 'real))
    (let ((real-type (num-type realpart))
          (imag-type (num-type imagpart))
          (real-num realpart))
        (if (and (> real-type imag-type) (>= real-type ShortFloatID))
            (setq imagpart (promote-number imagpart imag-type real-type))
            (if (and (< real-type imag-type) (>= imag-type ShortFloatID))
                (setq real-num (promote-number real-num real-type imag-type))))
        (if (and (rationalp real-num)(rationalp imagpart)(zerop imagpart))
            (return-from complex realpart))
        (let ((r (alloc-16-byte-uvector uvector-complex-tag)))
            (setf (uref r 1) real-num (uref r 2) imagpart)
            r)))

;;;
;;;	Common Lisp ABS function.
;;;
(defun abs (x) 
	(if (complexp x)
        (let ((r (realpart x))
              (i (imagpart x)))
            (sqrt (+ (* r r) (* i i))))
		(if (< x 0) (- x) x)))

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
(defun evenp (x) 
    (unless (integerp x)
        (cl::signal-type-error x 'integer))
    (= (mod x 2) 0))

;;;;
;;;; Common Lisp ODDP function.
;;;;
(defun oddp (x) 
    (unless (integerp x)
        (cl::signal-type-error x 'integer))
    (/= (mod x 2) 0))

;;;
;;; Common Lisp ISQRT function.
;;; Redefined here to ensure the newer math operations are taken advantage of.
;;;
(defun isqrt (n)
    (declare (optimize (speed 3)(safety 0)))
    (declare (integer n))
    (unless (integerp n)
        (cl::signal-type-error n 'integer))
    (unless (>= n 0)
        (cl::signal-type-error n '(integer 0 *)))
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

(defasm sqrt-fixnum (n)
    {
        push    ebp
        mov     ebp, esp
        mov     edx, [ebp + ARGS_OFFSET]
  		begin-atomic
        sar     edx, 3
        push    edx
        fild    [esp]                   ;; push onto fpu stack
        add     esp, 4
        xor     edx, edx
  		end-atomic
        push    (* uvector-single-float-tag 8)
        callp   alloc-8-byte-uvector    ;; eax = single float node
        add     esp, 4    
        fsqrt
   		fstp.single [eax + (uvector-offset 1)]
        mov     ecx, 1
        pop     ebp
        ret
    })

(defasm sqrt-bignum (n)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + ARGS_OFFSET]
        callp   bignum-to-single-float
        add     esp, 4
        fld.single [eax + (uvector-offset 1)]
        fsqrt
   		fstp.single [eax + (uvector-offset 1)]
        mov     ecx, 1
        pop     ebp
        ret
    })               

(defasm sqrt-ratio (n)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + ARGS_OFFSET]
        callp   ratio-to-single-float
        add     esp, 4
        fld.single [eax + (uvector-offset 1)]
        fsqrt
   		fstp.single [eax + (uvector-offset 1)]
        mov     ecx, 1
        pop     ebp
        ret
    })               
    
(defasm sqrt-short-float (n)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
        begin-atomic
		and         al, #xfc
        push        eax
		fld.single  [esp]
        fsqrt
		fstp.single [esp]
        pop         edx
		mov			eax, edx				;; untagged 32-bit float in eax, edx
		and         edx, #x7FFFFF			;; eax = 23-bit mantissa
		cmp         edx, #x7FFFFE			;; avoid overflow when rounding
		jae         short :t1
		mov         cl, al					;; get low three bits of mantissa
		and         cl, 7
		cmp         cl, 3
		je          short :t2
		cmp         cl, 5
		jle         short :t1
	:t2
		add         eax, 2
	:t1
		or          al, 3
        xor         edx, edx
		end-atomic
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
    })

(defasm sqrt-single-float (n)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
		fld.single  [eax + (uvector-offset 1)]
        fsqrt
        push        (* uvector-single-float-tag 8)
        callp       alloc-8-byte-uvector    ;; eax = single float node
        add         esp, 4    
		fstp.single [eax + (uvector-offset 1)]
        mov         ecx, 1
        pop         ebp
        ret  
    })

(defasm sqrt-double-float (n)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
		fld         [eax + (uvector-offset 2)]
        fsqrt
        push        (* uvector-double-float-tag 8)
        callp       alloc-16-byte-uvector    ;; eax = single float node
        add         esp, 4    
		fstp        [eax + (uvector-offset 2)]
        mov         ecx, 1
        pop         ebp
        ret  
    })

(defun sqrt-complex (c)
    (let* ((re (float (realpart c) 0d0))
           (im (float (imagpart c) 0d0))
           (x (abs re))
           (y (abs im))
           (r 0d0)
           (w 0d0)
           real-result
           imag-result)
        (if (and (= re 0d0) (= im 0d0))
            (return-from sqrt-complex 0d0))
        (if (>= x y)
            (progn
                (setq r (/ y x))
                (setq w (* (sqrt x) (sqrt (* 0.5d0 (+ 1.0d0 (sqrt (+ 1.0d0 (* r r)))))))))
            (progn
                (setq r (/ x y))
                (setq w (* (sqrt y) (sqrt (* 0.5d0 (+ r (sqrt (+ 1.0d0 (* r r))))))))))
        (if (>= re 0d0)
            (setq real-result w imag-result (/ im (* 2d0 w)))
            (setq imag-result (if (>= im 0) w (- w))
                  real-result (/ im (* 2.0d0 imag-result))))
        (complex real-result imag-result)))

(defun sqrt-non-number (x)
    (unless (numberp x)
        (cl::signal-type-error x 'number)))

(defconstant sqrt-functions 
    (vector 
        #'sqrt-fixnum
        #'sqrt-bignum
        #'sqrt-ratio
        #'sqrt-short-float
        #'sqrt-single-float
        #'sqrt-double-float
        #'sqrt-complex 
        #'sqrt-non-number))

;;;
;;;	Common Lisp SQRT operator
;;;
(defasm sqrt (x)
    {
        push    ebp
        mov     ebp, esp  
        push    [ebp + ARGS_OFFSET]
        push    0
        callp   numeric-compare-numbers
        add     esp, 8
        cmp     eax, -8                                 ;; x < 0?
        jne     :next
    
        ;; negative x, promote to complex
        push    (* uvector-complex-tag 8)
        callp   alloc-16-byte-uvector               ;; eax = complex node
        add     esp, 4    
        mov     edx, [ebp + ARGS_OFFSET]
        mov     [eax + (uvector-offset 1)], edx
        xor     edx, edx
        mov     [eax + (uvector-offset 2)], edx    
        mov     [ebp + ARGS_OFFSET], eax 
    :next
        push    [ebp + ARGS_OFFSET]
        callp   num-type
        add     esp, 4                                  ;; eax = typeof(x)
        mov     edx, sqrt-functions
        shr     eax, 1
        mov     eax, [edx + (uvector-offset 2) + eax]   ;; eax = promotion function
        push    [ebp + ARGS_OFFSET]                     ;; push x
        mov     ecx, 1
   		callfunc	eax
        add     esp, 4
        mov     ecx, 1
        pop     ebp
        ret
    })                

(defun floor-fixnum (n int-flag) (if int-flag n (float n)))
(defun floor-bignum (n int-flag) (if int-flag n (float n)))
(defun floor-ratio (n int-flag)
    (let* ((num (numerator n))
           (denom (denominator n))
           result
           (sign 1))
        (if (< denom 0)
            (setf denom (* denom -1) num (* num -1)))
        (if (< num 0)
            (progn
                (setf sign -1)
                (setf num (- num))    ;; handle floor
                (incf num (- denom 1))))  
        (setq result (divide-and-truncate-integers num denom))
        (if int-flag (* sign result) (float (* sign result)))))

(defasm floor-short-float (n int-flag)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + (+ ARGS_OFFSET 4)]
        begin-atomic
		and         al, #xfc
        push        eax
		fld.single  [esp]
        fstcw       [esp]                   ;; put FPU control word on stack
        fwait
        push        [esp]                   ;; copy control word on stack
        pop         eax                     ;; ax = control word
        and         eax, #xf3ff             ;; mask off control bits 10-11
        or          eax, #x0400             ;; set 10-11 to #b01
        push        eax
        fldcw       [esp]
        frndint                             ;; floor
        pop         eax
        fldcw       [esp]                   ;; restore original       
		fstp.single [esp]
        pop         edx
		mov			eax, edx				;; untagged 32-bit float in eax, edx
		and         edx, #x7FFFFF			;; eax = 23-bit mantissa
		cmp         edx, #x7FFFFE			;; avoid overflow when rounding
		jae         short :t1
		mov         cl, al					;; get low three bits of mantissa
		and         cl, 7
		cmp         cl, 3
		je          short :t2
		cmp         cl, 5
		jle         short :t1
	:t2
		add         eax, 2
	:t1
		or          al, 3
        xor         edx, edx
		end-atomic
        mov         edx, [ebp + (+ ARGS_OFFSET 0)]
        cmp         edx, [esi]
        je          short :done  
        push        eax
        mov         ecx, 1
        callf       short-float-to-integer
        add         esp, 4
    :done
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
    })

(defasm floor-single-float (n int-flag)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + (+ ARGS_OFFSET 4)]
		fld.single  [eax + (uvector-offset 1)]
        push        0
        fstcw       [esp]                   ;; put FPU control word on stack
        fwait
        push        [esp]                   ;; copy control word on stack
        pop         edx                     ;; ax = control word
        and         edx, #xf3ff             ;; mask off control bits 10-11
        or          edx, #x0400             ;; set 10-11 to #b01
        push        edx
        fldcw       [esp]
        frndint                             ;; floor
        fldcw       [esp + 4]               ;; restore original
        add         esp, 8       
        push        (* uvector-single-float-tag 8)
        callp       alloc-8-byte-uvector    ;; eax = single float node
        add         esp, 4    
		fstp.single [eax + (uvector-offset 1)]
        mov         edx, [ebp + (+ ARGS_OFFSET 0)]
        cmp         edx, [esi]
        je          short :done  
        push        eax
        mov         ecx, 1
        callf       single-float-to-integer
        add         esp, 4
    :done
        mov         ecx, 1
  		mov         esp,ebp
		pop         ebp
		ret
    })

(defasm floor-double-float (n int-flag)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + (+ ARGS_OFFSET 4)]
		fld         [eax + (uvector-offset 2)]
        push        0
        fstcw       [esp]                   ;; put FPU control word on stack
        fwait
        push        [esp]                   ;; copy control word on stack
        pop         edx                     ;; ax = control word
        and         edx, #xf3ff             ;; mask off control bits 10-11
        or          edx, #x0400             ;; set 10-11 to #b01
        push        edx
        fldcw       [esp]
        frndint                             ;; floor
        fldcw       [esp + 4]               ;; restore original
        add         esp, 8       
        push        (* uvector-double-float-tag 8)
        callp       alloc-16-byte-uvector   ;; eax = double float node
        add         esp, 4    
		fstp        [eax + (uvector-offset 2)]
        mov         edx, [ebp + (+ ARGS_OFFSET 0)]
        cmp         edx, [esi]
        je          short :done  
        push        eax
        mov         ecx, 1
        callf       double-float-to-integer
        add         esp, 4
    :done
        mov         ecx, 1
  		mov         esp,ebp
		pop         ebp
		ret
    })

(defun floor-complex (c) (cl::signal-type-error c 'real))

(defun floor-non-number (x) (cl::signal-type-error x 'real))

(defconstant floor-functions 
    (vector 
        #'floor-fixnum
        #'floor-bignum
        #'floor-ratio
        #'floor-short-float
        #'floor-single-float
        #'floor-double-float
        #'floor-complex 
        #'floor-non-number))

(defasm floor-number (x)
    {
        push    ebp
        mov     ebp, esp  
        push    [ebp + ARGS_OFFSET]
        callp   num-type
        add     esp, 4                                  ;; eax = typeof(x)
        mov     edx, floor-functions
        shr     eax, 1
        mov     eax, [edx + (uvector-offset 2) + eax]   ;; eax = promotion function
        push    [ebp + ARGS_OFFSET]                     ;; push x
        push    [esi + 4]                               ;; push int-flag = T
        mov     ecx, 2
   		callfunc	eax
        add     esp, 8
        mov     ecx, 1
        pop     ebp
        ret
    })                

(defasm ffloor-number (x)
    {
        push    ebp
        mov     ebp, esp  
        push    [ebp + ARGS_OFFSET]
        callp   num-type
        add     esp, 4                                  ;; eax = typeof(x)
        mov     edx, floor-functions
        shr     eax, 1
        mov     eax, [edx + (uvector-offset 2) + eax]   ;; eax = promotion function
        push    [ebp + ARGS_OFFSET]                     ;; push x
        push    [esi]                                   ;; push int-flag = NIL
        mov     ecx, 2
       	callfunc	eax
        add     esp, 8
        mov     ecx, 1
        pop     ebp
        ret
    })                

;;;
;;;	Common Lisp FLOOR operator
;;;
(defun floor (number &optional (divisor 1))
    (let ((d number))
        (unless (eq divisor 1)
            (setq d (/ number divisor)))
        (let ((result (floor-number d)))
            (values result (- number (* result divisor))))))

;;;
;;;	Common Lisp FFLOOR operator
;;;
(defun ffloor (number &optional (divisor 1))
    (let ((d number))
        (unless (eq divisor 1)
            (setq d (/ number divisor)))
        (let ((result (ffloor-number d)))
            (values result (- number (* result divisor))))))

(defun ceiling-fixnum (n int-flag) (if int-flag n (float n)))
(defun ceiling-bignum (n int-flag) (if int-flag n (float n)))
(defun ceiling-ratio (n int-flag)   ;; rgc -- need to work on this...
    (let* ((num (numerator n))
           (denom (denominator n))
           result
           (sign 1))
        (if (< denom 0)
            (setf denom (* denom -1) num (* num -1)))
        (if (< num 0)
            (progn
                (setf sign -1)
                (setf num (- num)))
            (incf num (- denom 1)))      ;; handle ceiling
        (setq result (divide-and-truncate-integers num denom))
        (if int-flag (* sign result) (float (* sign result)))))

(defasm ceiling-short-float (n int-flag)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + (+ ARGS_OFFSET 4)]
        begin-atomic
		and         al, #xfc
        push        eax
		fld.single  [esp]
        fstcw       [esp]                   ;; put FPU control word on stack
        fwait
        push        [esp]                   ;; copy control word on stack
        pop         eax                     ;; ax = control word
        and         eax, #xf3ff             ;; mask off control bits 10-11
        or          eax, #x0800             ;; set 10-11 to #b10
        push        eax
        fldcw       [esp]
        frndint                             ;; ceiling
        pop         eax
        fldcw       [esp]                   ;; restore original       
		fstp.single [esp]
        pop         edx
		mov			eax, edx				;; untagged 32-bit float in eax, edx
		and         edx, #x7FFFFF			;; eax = 23-bit mantissa
		cmp         edx, #x7FFFFE			;; avoid overflow when rounding
		jae         short :t1
		mov         cl, al					;; get low three bits of mantissa
		and         cl, 7
		cmp         cl, 3
		je          short :t2
		cmp         cl, 5
		jle         short :t1
	:t2
		add         eax, 2
	:t1
		or          al, 3
        xor         edx, edx
		end-atomic
        mov         edx, [ebp + (+ ARGS_OFFSET 0)]
        cmp         edx, [esi]
        je          short :done  
        push        eax
        mov         ecx, 1
        callf       short-float-to-integer
        add         esp, 4
    :done
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
    })

(defasm ceiling-single-float (n int-flag)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + (+ ARGS_OFFSET 4)]
		fld.single  [eax + (uvector-offset 1)]
        push        0
        fstcw       [esp]                   ;; put FPU control word on stack
        fwait
        push        [esp]                   ;; copy control word on stack
        pop         edx                     ;; ax = control word
        and         edx, #xf3ff             ;; mask off control bits 10-11
        or          edx, #x0800             ;; set 10-11 to #b10
        push        edx
        fldcw       [esp]
        frndint                             ;; ceiling
        fldcw       [esp + 4]               ;; restore original
        add         esp, 8       
        push        (* uvector-single-float-tag 8)
        callp       alloc-8-byte-uvector    ;; eax = single float node
        add         esp, 4    
		fstp.single [eax + (uvector-offset 1)]
        mov         edx, [ebp + (+ ARGS_OFFSET 0)]
        cmp         edx, [esi]
        je          short :done  
        push        eax
        mov         ecx, 1
        callf       single-float-to-integer
        add         esp, 4
    :done
        mov         ecx, 1
  		mov         esp,ebp
		pop         ebp
		ret
    })

(defasm ceiling-double-float (n int-flag)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + (+ ARGS_OFFSET 4)]
		fld         [eax + (uvector-offset 2)]
        push        0
        fstcw       [esp]                   ;; put FPU control word on stack
        fwait
        push        [esp]                   ;; copy control word on stack
        pop         edx                     ;; ax = control word
        and         edx, #xf3ff             ;; mask off control bits 10-11
        or          edx, #x0800             ;; set 10-11 to #b10
        push        edx
        fldcw       [esp]
        frndint                             ;; ceiling
        fldcw       [esp + 4]               ;; restore original
        add         esp, 8       
        push        (* uvector-double-float-tag 8)
        callp       alloc-16-byte-uvector   ;; eax = double float node
        add         esp, 4    
		fstp        [eax + (uvector-offset 2)]
        mov         edx, [ebp + (+ ARGS_OFFSET 0)]
        cmp         edx, [esi]
        je          short :done  
        push        eax
        mov         ecx, 1
        callf       double-float-to-integer
        add         esp, 4
    :done
        mov         ecx, 1
  		mov         esp,ebp
		pop         ebp
		ret
    })

(defun ceiling-complex (c) (cl::signal-type-error c 'real))

(defun ceiling-non-number (x) (cl::signal-type-error x 'real))

(defconstant ceiling-functions 
    (vector 
        #'ceiling-fixnum
        #'ceiling-bignum
        #'ceiling-ratio
        #'ceiling-short-float
        #'ceiling-single-float
        #'ceiling-double-float
        #'ceiling-complex 
        #'ceiling-non-number))

(defasm ceiling-number (x)
    {
        push    ebp
        mov     ebp, esp  
        push    [ebp + ARGS_OFFSET]
        callp   num-type
        add     esp, 4                                  ;; eax = typeof(x)
        mov     edx, ceiling-functions
        shr     eax, 1
        mov     eax, [edx + (uvector-offset 2) + eax]   ;; eax = promotion function
        push    [ebp + ARGS_OFFSET]                     ;; push x
        push    [esi + 4]                               ;; push int-flag = T
        mov     ecx, 2
   		callfunc	eax
        add     esp, 8
        mov     ecx, 1
        pop     ebp
        ret
    })                

(defasm fceiling-number (x)
    {
        push    ebp
        mov     ebp, esp  
        push    [ebp + ARGS_OFFSET]
        callp   num-type
        add     esp, 4                                  ;; eax = typeof(x)
        mov     edx, ceiling-functions
        shr     eax, 1
        mov     eax, [edx + (uvector-offset 2) + eax]   ;; eax = promotion function
        push    [ebp + ARGS_OFFSET]                     ;; push x
        push    [esi]                                   ;; push int-flag = NIL
        mov     ecx, 2
        callfunc	eax
        add     esp, 8
        mov     ecx, 1
        pop     ebp
        ret
    })                

;;;
;;;	Common Lisp CEILING operator
;;;
(defun ceiling (number &optional (divisor 1))
    (let ((d number))
        (unless (eq divisor 1)
            (setq d (/ number divisor)))
        (let ((result (ceiling-number d)))
            (values result (- number (* result divisor))))))

;;;
;;;	Common Lisp FCEILING operator
;;;
(defun fceiling (number &optional (divisor 1))
    (let ((d number))
        (unless (eq divisor 1)
            (setq d (/ number divisor)))
        (let ((result (fceiling-number d)))
            (values result (- number (* result divisor))))))

(defun truncate-fixnum (n int-flag) (if int-flag n (float n)))
(defun truncate-bignum (n int-flag) (if int-flag n (float n)))
(defun truncate-ratio (n int-flag) 
    (let* ((num (numerator n))
           (denom (denominator n))
           result
           (sign 1))
        (if (< denom 0)
            (setf denom (* denom -1) num (* num -1)))
        (if (< num 0)
            (progn
                (setf sign -1)
                (setf num (- num))))
        (setq result (divide-and-truncate-integers num denom))
        (if int-flag (* sign result) (float (* sign result)))))

(defasm truncate-short-float (n int-flag)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + (+ ARGS_OFFSET 4)]
        begin-atomic
		and         al, #xfc
        push        eax
		fld.single  [esp]
        fstcw       [esp]                   ;; put FPU control word on stack
        fwait
        push        [esp]                   ;; copy control word on stack
        pop         eax                     ;; ax = control word
        and         eax, #xf3ff             ;; mask off control bits 10-11
        or          eax, #x0c00             ;; set 10-11 to #b11
        push        eax
        fldcw       [esp]
        frndint                             ;; truncate
        pop         eax
        fldcw       [esp]                   ;; restore original       
		fstp.single [esp]
        pop         edx
		mov			eax, edx				;; untagged 32-bit float in eax, edx
		and         edx, #x7FFFFF			;; eax = 23-bit mantissa
		cmp         edx, #x7FFFFE			;; avoid overflow when rounding
		jae         short :t1
		mov         cl, al					;; get low three bits of mantissa
		and         cl, 7
		cmp         cl, 3
		je          short :t2
		cmp         cl, 5
		jle         short :t1
	:t2
		add         eax, 2
	:t1
		or          al, 3
        xor         edx, edx
		end-atomic
        mov         edx, [ebp + (+ ARGS_OFFSET 0)]
        cmp         edx, [esi]
        je          short :done  
        push        eax
        mov         ecx, 1
        callf       short-float-to-integer
        add         esp, 4
    :done
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
    })

(defasm truncate-single-float (n int-flag)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + (+ ARGS_OFFSET 4)]
		fld.single  [eax + (uvector-offset 1)]
        push        0
        fstcw       [esp]                   ;; put FPU control word on stack
        fwait
        push        [esp]                   ;; copy control word on stack
        pop         edx                     ;; ax = control word
        and         edx, #xf3ff             ;; mask off control bits 10-11
        or          edx, #x0c00             ;; set 10-11 to #b11
        push        edx
        fldcw       [esp]
        frndint                             ;; truncate
        fldcw       [esp + 4]               ;; restore original
        add         esp, 8       
        push        (* uvector-single-float-tag 8)
        callp       alloc-8-byte-uvector    ;; eax = single float node
        add         esp, 4    
		fstp.single [eax + (uvector-offset 1)]
        mov         edx, [ebp + (+ ARGS_OFFSET 0)]
        cmp         edx, [esi]
        je          short :done  
        push        eax
        mov         ecx, 1
        callf       single-float-to-integer
        add         esp, 4
    :done
        mov         ecx, 1
  		mov         esp,ebp
		pop         ebp
		ret
    })

(defasm truncate-double-float (n int-flag)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + (+ ARGS_OFFSET 4)]
		fld         [eax + (uvector-offset 2)]
        push        0
        fstcw       [esp]                   ;; put FPU control word on stack
        fwait
        push        [esp]                   ;; copy control word on stack
        pop         edx                     ;; ax = control word
        and         edx, #xf3ff             ;; mask off control bits 10-11
        or          edx, #x0c00             ;; set 10-11 to #b11
        push        edx
        fldcw       [esp]
        frndint                             ;; truncate
        fldcw       [esp + 4]               ;; restore original
        add         esp, 8       
        push        (* uvector-double-float-tag 8)
        callp       alloc-16-byte-uvector   ;; eax = double float node
        add         esp, 4    
		fstp        [eax + (uvector-offset 2)]
        mov         edx, [ebp + (+ ARGS_OFFSET 0)]
        cmp         edx, [esi]
        je          short :done  
        push        eax
        mov         ecx, 1
        callf       double-float-to-integer
        add         esp, 4
    :done
        mov         ecx, 1
  		mov         esp,ebp
		pop         ebp
		ret
    })

(defun truncate-complex (c) (cl::signal-type-error c 'real))

(defun truncate-non-number (x) (cl::signal-type-error x 'real))

(defconstant truncate-functions 
    (vector 
        #'truncate-fixnum
        #'truncate-bignum
        #'truncate-ratio
        #'truncate-short-float
        #'truncate-single-float
        #'truncate-double-float
        #'truncate-complex 
        #'truncate-non-number))

(defasm truncate-number (x)
    {
        push    ebp
        mov     ebp, esp  
        push    [ebp + ARGS_OFFSET]
        callp   num-type
        add     esp, 4                                  ;; eax = typeof(x)
        mov     edx, truncate-functions
        shr     eax, 1
        mov     eax, [edx + (uvector-offset 2) + eax]   ;; eax = promotion function
        push    [ebp + ARGS_OFFSET]                     ;; push x
        push    [esi + 4]                               ;; push int-flag = T
        mov     ecx, 2
        callfunc	eax
        add     esp, 8
        mov     ecx, 1
        pop     ebp
        ret
    })                

(defasm ftruncate-number (x)
    {
        push    ebp
        mov     ebp, esp  
        push    [ebp + ARGS_OFFSET]
        callp   num-type
        add     esp, 4                                  ;; eax = typeof(x)
        mov     edx, truncate-functions
        shr     eax, 1
        mov     eax, [edx + (uvector-offset 2) + eax]   ;; eax = promotion function
        push    [ebp + ARGS_OFFSET]                     ;; push x
        push    [esi]                                   ;; push int-flag = NIL
        mov     ecx, 2
   		callfunc	eax
        add     esp, 8
        mov     ecx, 1
        pop     ebp
        ret
    })                

;;;
;;;	Common Lisp TRUNCATE operator
;;;
(defun truncate (number &optional (divisor 1))
    (let ((d number))
        (unless (eq divisor 1)
            (setq d (/ number divisor)))
        (let ((result (truncate-number d)))
            (values result (- number (* result divisor))))))

;;;
;;;	Common Lisp FTRUNCATE operator
;;;
(defun ftruncate (number &optional (divisor 1))
    (let ((d number))
        (unless (eq divisor 1)
            (setq d (/ number divisor)))
        (let ((result (ftruncate-number d)))
            (values result (- number (* result divisor))))))

(defun round-fixnum (n int-flag) (if int-flag n (float n)))
(defun round-bignum (n int-flag) (if int-flag n (float n)))
(defun round-ratio (n int-flag)
    (let ((x (+ n 1/2)))
        (if (and (= (denominator x) 1) (oddp x))    ;; do even/odd rounding
            (decf x))
        (floor-ratio x int-flag)))

(defasm round-short-float (n int-flag)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + (+ ARGS_OFFSET 4)]
        begin-atomic
		and         al, #xfc
        push        eax
		fld.single  [esp]
        fstcw       [esp]                   ;; put FPU control word on stack
        fwait
        push        [esp]                   ;; copy control word on stack
        pop         eax                     ;; ax = control word
        and         eax, #xf3ff             ;; mask off control bits 10-11
        push        eax
        fldcw       [esp]
        frndint                             ;; round
        pop         eax
        fldcw       [esp]                   ;; restore original       
		fstp.single [esp]
        pop         edx
		mov			eax, edx				;; untagged 32-bit float in eax, edx
		and         edx, #x7FFFFF			;; eax = 23-bit mantissa
		cmp         edx, #x7FFFFE			;; avoid overflow when rounding
		jae         short :t1
		mov         cl, al					;; get low three bits of mantissa
		and         cl, 7
		cmp         cl, 3
		je          short :t2
		cmp         cl, 5
		jle         short :t1
	:t2
		add         eax, 2
	:t1
		or          al, 3
        xor         edx, edx
		end-atomic
        mov         edx, [ebp + (+ ARGS_OFFSET 0)]
        cmp         edx, [esi]
        je          short :done  
        push        eax
        mov         ecx, 1
        callf       short-float-to-integer
        add         esp, 4
    :done
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
    })

(defasm round-single-float (n int-flag)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + (+ ARGS_OFFSET 4)]
		fld.single  [eax + (uvector-offset 1)]
        push        0
        fstcw       [esp]                   ;; put FPU control word on stack
        fwait
        push        [esp]                   ;; copy control word on stack
        pop         edx                     ;; ax = control word
        and         edx, #xf3ff             ;; mask off control bits 10-11
        push        edx
        fldcw       [esp]
        frndint                             ;; round
        fldcw       [esp + 4]               ;; restore original
        add         esp, 8       
        push        (* uvector-single-float-tag 8)
        callp       alloc-8-byte-uvector    ;; eax = single float node
        add         esp, 4    
		fstp.single [eax + (uvector-offset 1)]
        mov         edx, [ebp + (+ ARGS_OFFSET 0)]
        cmp         edx, [esi]
        je          short :done  
        push        eax
        mov         ecx, 1
        callf       single-float-to-integer
        add         esp, 4
    :done
        mov         ecx, 1
  		mov         esp,ebp
		pop         ebp
		ret
    })

(defasm round-double-float (n int-flag)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + (+ ARGS_OFFSET 4)]
		fld         [eax + (uvector-offset 2)]
        push        0
        fstcw       [esp]                   ;; put FPU control word on stack
        fwait
        push        [esp]                   ;; copy control word on stack
        pop         edx                     ;; ax = control word
        and         edx, #xf3ff             ;; mask off control bits 10-11
        push        edx
        fldcw       [esp]
        frndint                             ;; round
        fldcw       [esp + 4]               ;; restore original
        add         esp, 8       
        push        (* uvector-double-float-tag 8)
        callp       alloc-16-byte-uvector   ;; eax = double float node
        add         esp, 4    
		fstp        [eax + (uvector-offset 2)]
        mov         edx, [ebp + (+ ARGS_OFFSET 0)]
        cmp         edx, [esi]
        je          short :done  
        push        eax
        mov         ecx, 1
        callf       double-float-to-integer
        add         esp, 4
    :done
        mov         ecx, 1
  		mov         esp,ebp
		pop         ebp
		ret
    })

(defun round-complex (c) (cl::signal-type-error c 'real))

(defun round-non-number (x) (cl::signal-type-error x 'real))

(defconstant round-functions 
    (vector 
        #'round-fixnum
        #'round-bignum
        #'round-ratio
        #'round-short-float
        #'round-single-float
        #'round-double-float
        #'round-complex 
        #'round-non-number))

(defasm round-number (x)
    {
        push    ebp
        mov     ebp, esp  
        push    [ebp + ARGS_OFFSET]
        callp   num-type
        add     esp, 4                                  ;; eax = typeof(x)
        mov     edx, round-functions
        shr     eax, 1
        mov     eax, [edx + (uvector-offset 2) + eax]   ;; eax = promotion function
        push    [ebp + ARGS_OFFSET]                     ;; push x
        push    [esi + 4]                               ;; push int-flag = T
        mov     ecx, 2
        callfunc	eax
        add     esp, 8
        mov     ecx, 1
        pop     ebp
        ret
    })                

(defasm fround-number (x)
    {
        push    ebp
        mov     ebp, esp  
        push    [ebp + ARGS_OFFSET]
        callp   num-type
        add     esp, 4                                  ;; eax = typeof(x)
        mov     edx, round-functions
        shr     eax, 1
        mov     eax, [edx + (uvector-offset 2) + eax]   ;; eax = promotion function
        push    [ebp + ARGS_OFFSET]                     ;; push x
        push    [esi]                                   ;; push int-flag = NIL
        mov     ecx, 2
        callfunc eax                                    ;; call function
        add     esp, 8
        mov     ecx, 1
        pop     ebp
        ret
    })                

;;;
;;;	Common Lisp ROUND operator
;;;
(defun round (number &optional (divisor 1))
    (let ((d number))
        (unless (eq divisor 1)
            (setq d (/ number divisor)))
        (let ((result (round-number d)))
            (values result (- number (* result divisor))))))

;;;
;;;	Common Lisp FROUND operator
;;;
(defun fround (number &optional (divisor 1))
    (let ((d number))
        (unless (eq divisor 1)
            (setq d (/ number divisor)))
        (let ((result (fround-number d)))
            (values result (- number (* result divisor))))))

(defasm sin-fixnum (n)
    {
        push    ebp
        mov     ebp, esp
        mov     edx, [ebp + ARGS_OFFSET]
  		begin-atomic
        sar     edx, 3
        push    edx
        fild    [esp]                   ;; push onto fpu stack
        add     esp, 4
        xor     edx, edx
  		end-atomic
        push    (* uvector-single-float-tag 8)
        callp   alloc-8-byte-uvector    ;; eax = single float node
        add     esp, 4    
        fsin
   		fstp.single [eax + (uvector-offset 1)]
        mov     ecx, 1
        pop     ebp
        ret
    })

(defasm sin-bignum (n)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + ARGS_OFFSET]
        callp   bignum-to-single-float
        add     esp, 4
        fld.single [eax + (uvector-offset 1)]
        fsin
   		fstp.single [eax + (uvector-offset 1)]
        mov     ecx, 1
        pop     ebp
        ret
    })               

(defasm sin-ratio (n)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + ARGS_OFFSET]
        callp   ratio-to-single-float
        add     esp, 4
        fld.single [eax + (uvector-offset 1)]
        fsin
   		fstp.single [eax + (uvector-offset 1)]
        mov     ecx, 1
        pop     ebp
        ret
    })               
    
(defasm sin-short-float (n)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
        begin-atomic
		and         al, #xfc
        push        eax
		fld.single  [esp]
        fsin
		fstp.single [esp]
        pop         edx
		mov			eax, edx				;; untagged 32-bit float in eax, edx
		and         edx, #x7FFFFF			;; eax = 23-bit mantissa
		cmp         edx, #x7FFFFE			;; avoid overflow when rounding
		jae         short :t1
		mov         cl, al					;; get low three bits of mantissa
		and         cl, 7
		cmp         cl, 3
		je          short :t2
		cmp         cl, 5
		jle         short :t1
	:t2
		add         eax, 2
	:t1
		or          al, 3
        xor         edx, edx
		end-atomic
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
    })

(defasm sin-single-float (n)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
		fld.single  [eax + (uvector-offset 1)]
        fsin
        push        (* uvector-single-float-tag 8)
        callp       alloc-8-byte-uvector    ;; eax = single float node
        add         esp, 4    
		fstp.single [eax + (uvector-offset 1)]
        mov         ecx, 1
        pop         ebp
        ret  
    })

(defasm sin-double-float (n)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
		fld         [eax + (uvector-offset 2)]
        fsin
        push        (* uvector-double-float-tag 8)
        callp       alloc-16-byte-uvector    ;; eax = single float node
        add         esp, 4    
		fstp        [eax + (uvector-offset 2)]
        mov         ecx, 1
        pop         ebp
        ret  
    })

(defun complex-sin (x)
  (let* ((eix (exp (* #c(0.0 1.0) x)))
	 (e-ix (/ 1.0 eix)))
    (/ (- eix e-ix) (* #c(0.0 1.0) 2.0))))

(defun sin-non-number (x)
    (unless (numberp x)
        (cl::signal-type-error x 'number)))

(defconstant sin-functions 
    (vector 
        #'sin-fixnum
        #'sin-bignum
        #'sin-ratio
        #'sin-short-float
        #'sin-single-float
        #'sin-double-float
        #'complex-sin 
        #'sin-non-number))

;;;
;;;	Common Lisp SIN operator
;;;
(defasm cl:sin (x)
    {
        push    ebp
        mov     ebp, esp  
        push    [ebp + ARGS_OFFSET]
        callp   num-type
        add     esp, 4                                  ;; eax = typeof(x)
        mov     edx, sin-functions
        shr     eax, 1
        mov     eax, [edx + (uvector-offset 2) + eax]   ;; eax = promotion function
        push    [ebp + ARGS_OFFSET]                     ;; push x
        mov     ecx, 1
		callfunc	eax
        add     esp, 4
        mov     ecx, 1
        pop     ebp
        ret
    })

(defasm cos-fixnum (n)
    {
        push    ebp
        mov     ebp, esp
        mov     edx, [ebp + ARGS_OFFSET]
  		begin-atomic
        sar     edx, 3
        push    edx
        fild    [esp]                   ;; push onto fpu stack
        add     esp, 4
        xor     edx, edx
  		end-atomic
        push    (* uvector-single-float-tag 8)
        callp   alloc-8-byte-uvector    ;; eax = single float node
        add     esp, 4    
        fcos
   		fstp.single [eax + (uvector-offset 1)]
        mov     ecx, 1
        pop     ebp
        ret
    })

(defasm cos-bignum (n)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + ARGS_OFFSET]
        callp   bignum-to-single-float
        add     esp, 4
        fld.single [eax + (uvector-offset 1)]
        fcos
   		fstp.single [eax + (uvector-offset 1)]
        mov     ecx, 1
        pop     ebp
        ret
    })               

(defasm cos-ratio (n)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + ARGS_OFFSET]
        callp   ratio-to-single-float
        add     esp, 4
        fld.single [eax + (uvector-offset 1)]
        fcos
   		fstp.single [eax + (uvector-offset 1)]
        mov     ecx, 1
        pop     ebp
        ret
    })               
    
(defasm cos-short-float (n)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
        begin-atomic
		and         al, #xfc
        push        eax
		fld.single  [esp]
        fcos
		fstp.single [esp]
        pop         edx
		mov			eax, edx				;; untagged 32-bit float in eax, edx
		and         edx, #x7FFFFF			;; eax = 23-bit mantissa
		cmp         edx, #x7FFFFE			;; avoid overflow when rounding
		jae         short :t1
		mov         cl, al					;; get low three bits of mantissa
		and         cl, 7
		cmp         cl, 3
		je          short :t2
		cmp         cl, 5
		jle         short :t1
	:t2
		add         eax, 2
	:t1
		or          al, 3
        xor         edx, edx
		end-atomic
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
    })

(defasm cos-single-float (n)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
		fld.single  [eax + (uvector-offset 1)]
        fcos
        push        (* uvector-single-float-tag 8)
        callp       alloc-8-byte-uvector    ;; eax = single float node
        add         esp, 4    
		fstp.single [eax + (uvector-offset 1)]
        mov         ecx, 1
        pop         ebp
        ret  
    })

(defasm cos-double-float (n)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
		fld         [eax + (uvector-offset 2)]
        fcos
        push        (* uvector-double-float-tag 8)
        callp       alloc-16-byte-uvector    ;; eax = single float node
        add         esp, 4    
		fstp        [eax + (uvector-offset 2)]
        mov         ecx, 1
        pop         ebp
        ret  
    })

(defun complex-cos (x)
  (let* ((eix (exp (* #c(0.0 1.0) x)))
	 (e-ix (/ 1.0 eix)))
    (/ (+ eix e-ix) 2.0)))

(defun cos-non-number (x)
    (unless (numberp x)
        (cl::signal-type-error x 'number)))

(defconstant cos-functions 
    (vector 
        #'cos-fixnum
        #'cos-bignum
        #'cos-ratio
        #'cos-short-float
        #'cos-single-float
        #'cos-double-float
        #'complex-cos 
        #'cos-non-number))

;;;
;;;	Common Lisp COS operator
;;;
(defasm cl:cos (x)
    {
        push    ebp
        mov     ebp, esp  
        push    [ebp + ARGS_OFFSET]
        callp   num-type
        add     esp, 4                                  ;; eax = typeof(x)
        mov     edx, cos-functions
        shr     eax, 1
        mov     eax, [edx + (uvector-offset 2) + eax]   ;; eax = promotion function
        push    [ebp + ARGS_OFFSET]                     ;; push x
        mov     ecx, 1
		callfunc	eax
        add     esp, 4
        mov     ecx, 1
        pop     ebp
        ret
    })

(defasm tan-fixnum (n)
    {
        push    ebp
        mov     ebp, esp
        mov     edx, [ebp + ARGS_OFFSET]
  		begin-atomic
        sar     edx, 3
        push    edx
        fild    [esp]                   ;; push onto fpu stack
        add     esp, 4
        xor     edx, edx
  		end-atomic
        push    (* uvector-single-float-tag 8)
        callp   alloc-8-byte-uvector    ;; eax = single float node
        add     esp, 4    
        fsincos
        fdivp    st1, st0
   		fstp.single [eax + (uvector-offset 1)]
        mov     ecx, 1
        pop     ebp
        ret
    })

(defasm tan-bignum (n)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + ARGS_OFFSET]
        callp   bignum-to-single-float
        add     esp, 4
        fld.single [eax + (uvector-offset 1)]
        fsincos
        fdivp    st1, st0
   		fstp.single [eax + (uvector-offset 1)]
        mov     ecx, 1
        pop     ebp
        ret
    })               

(defasm tan-ratio (n)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + ARGS_OFFSET]
        callp   ratio-to-single-float
        add     esp, 4
        fld.single [eax + (uvector-offset 1)]
        fsincos
        fdivp    st1, st0
   		fstp.single [eax + (uvector-offset 1)]
        mov     ecx, 1
        pop     ebp
        ret
    })               
    
(defasm tan-short-float (n)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
        begin-atomic
		and         al, #xfc
        push        eax
		fld.single  [esp]
        fsincos
        fdivp    st1, st0
		fstp.single [esp]
        pop         edx
		mov			eax, edx				;; untagged 32-bit float in eax, edx
		and         edx, #x7FFFFF			;; eax = 23-bit mantissa
		cmp         edx, #x7FFFFE			;; avoid overflow when rounding
		jae         short :t1
		mov         cl, al					;; get low three bits of mantissa
		and         cl, 7
		cmp         cl, 3
		je          short :t2
		cmp         cl, 5
		jle         short :t1
	:t2
		add         eax, 2
	:t1
		or          al, 3
        xor         edx, edx
		end-atomic
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
    })

(defasm tan-single-float (n)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
		fld.single  [eax + (uvector-offset 1)]
        fsincos
        fdivp    st1, st0
        push        (* uvector-single-float-tag 8)
        callp       alloc-8-byte-uvector    ;; eax = single float node
        add         esp, 4    
		fstp.single [eax + (uvector-offset 1)]
        mov         ecx, 1
        pop         ebp
        ret  
    })

(defasm tan-double-float (n)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
		fld         [eax + (uvector-offset 2)]
        fsincos
        fdivp    st1, st0
        push        (* uvector-double-float-tag 8)
        callp       alloc-16-byte-uvector    ;; eax = single float node
        add         esp, 4    
		fstp        [eax + (uvector-offset 2)]
        mov         ecx, 1
        pop         ebp
        ret  
    })

(defun complex-tan (x)
  (let* ((eix (exp (* #c(0.0 1.0) x)))
	 (e-ix (/ 1.0 eix)))
    (/ (- eix e-ix) (+ eix e-ix) #c(0.0 1.0))))

(defun tan-non-number (x)
    (unless (numberp x)
        (cl::signal-type-error x 'number)))

(defconstant tan-functions 
    (vector 
        #'tan-fixnum
        #'tan-bignum
        #'tan-ratio
        #'tan-short-float
        #'tan-single-float
        #'tan-double-float
        #'complex-tan 
        #'tan-non-number))

;;;
;;;	Common Lisp TAN operator
;;;
(defasm cl:tan (x)
    {
        push    ebp
        mov     ebp, esp  
        push    [ebp + ARGS_OFFSET]
        callp   num-type
        add     esp, 4                                  ;; eax = typeof(x)
        mov     edx, tan-functions
        shr     eax, 1
        mov     eax, [edx + (uvector-offset 2) + eax]   ;; eax = promotion function
        push    [ebp + ARGS_OFFSET]                     ;; push x
        mov     ecx, 1
		callfunc	eax
        add     esp, 4
        mov     ecx, 1
        pop     ebp
        ret
    })

;;;
;;;	Common Lisp ASIN function.
;;; arcsin z = -i log (iz+sqrt(1-z^2))
;;;
(defun asin (number)
    (let ((result
	       (* (- #C(0 1)) 
	           (log (+ (* #C(0 1) number) (sqrt (- 1 (* number number))))))))
        (if (and (complexp result) (= (imagpart result) 0))
            (realpart result)
            result)))

;;;
;;;	Common Lisp ACOS function.
;;; arccos z = <PI>/2- arcsin z
;;;
(defun acos (number)
    (let ((result (- (/ pi 2) (asin number))))
        (if (and (complexp result) (= (imagpart number) 0))
            (realpart result)
            result)))

(defasm atan-fixnum (n)
    {
        push    ebp
        mov     ebp, esp
        mov     edx, [ebp + ARGS_OFFSET]
  		begin-atomic
        sar     edx, 3
        push    edx
        fild    [esp]                   ;; push onto fpu stack
        add     esp, 4
        xor     edx, edx
  		end-atomic
        push    (* uvector-single-float-tag 8)
        callp   alloc-8-byte-uvector    ;; eax = single float node
        add     esp, 4
        fld1    
        fpatan
   		fstp.single [eax + (uvector-offset 1)]
        mov     ecx, 1
        pop     ebp
        ret
    })

(defasm atan-bignum (n)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + ARGS_OFFSET]
        callp   bignum-to-single-float
        add     esp, 4
        fld.single [eax + (uvector-offset 1)]
        fld1
        fpatan
   		fstp.single [eax + (uvector-offset 1)]
        mov     ecx, 1
        pop     ebp
        ret
    })               

(defasm atan-ratio (n)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + ARGS_OFFSET]
        callp   ratio-to-single-float
        add     esp, 4
        fld.single [eax + (uvector-offset 1)]
        fld1
        fpatan
   		fstp.single [eax + (uvector-offset 1)]
        mov     ecx, 1
        pop     ebp
        ret
    })               
    
(defasm atan-short-float (n)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
        begin-atomic
		and         al, #xfc
        push        eax
		fld.single  [esp]
        fld1
        fpatan
		fstp.single [esp]
        pop         edx
		mov			eax, edx				;; untagged 32-bit float in eax, edx
		and         edx, #x7FFFFF			;; eax = 23-bit mantissa
		cmp         edx, #x7FFFFE			;; avoid overflow when rounding
		jae         short :t1
		mov         cl, al					;; get low three bits of mantissa
		and         cl, 7
		cmp         cl, 3
		je          short :t2
		cmp         cl, 5
		jle         short :t1
	:t2
		add         eax, 2
	:t1
		or          al, 3
        xor         edx, edx
		end-atomic
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
    })

(defasm atan-single-float (n)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
		fld.single  [eax + (uvector-offset 1)]
        fld1
        fpatan
        push        (* uvector-single-float-tag 8)
        callp       alloc-8-byte-uvector    ;; eax = single float node
        add         esp, 4    
		fstp.single [eax + (uvector-offset 1)]
        mov         ecx, 1
        pop         ebp
        ret  
    })

(defasm atan-double-float (n)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
		fld         [eax + (uvector-offset 2)]
        fld1
        fpatan
        push        (* uvector-double-float-tag 8)
        callp       alloc-16-byte-uvector    ;; eax = single float node
        add         esp, 4    
		fstp        [eax + (uvector-offset 2)]
        mov         ecx, 1
        pop         ebp
        ret  
    })

(defun complex-atan (x)
  (let ((result
                (* (- #C(0 1))
                    (log
                        (* (+ 1 (* #C(0 1) x))
                            (sqrt (/ 1 (+ 1 (* x x)))))))))
         (if (and (complexp result) (= (imagpart x) 0))
            (realpart result)
            result)))

(defun atan-non-number (x)
    (unless (numberp x)
        (cl::signal-type-error x 'number)))

(defconstant atan-functions 
    (vector 
        #'atan-fixnum
        #'atan-bignum
        #'atan-ratio
        #'atan-short-float
        #'atan-single-float
        #'atan-double-float
        #'complex-atan 
        #'atan-non-number))

;;;
;;;	Common Lisp ATAN function.
;;; arctan x = -i log  ((1+ix) sqrt(1/(1+x^2)) )
;;; arctan x = 
;;; 
(defun atan (x &optional y)
    (when y
        (unless (realp x)
            (cl::signal-type-error x 'real))
        (unless (realp y)
            (cl::signal-type-error y 'real))
        
        ;; handle case where y is 0
        (when (= y 0)
            (if (= x 0)
                (cl::signal-arithmetic-error 'atan (list x y)))
            (return-from atan (if (> x 0) (/ pi 2) (- (/ pi 2)))))
        (setq x (/ x y)))
    
    (let ((type (num-type x)))
        (funcall (uref atan-functions (+ type 2)) x)))
                
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
		(cl::signal-arithmetic-error 'atanh (list x)))
	(log (/ (1+ x) (sqrt (- 1.0 (* x x))))))

;;;
;;;	Common Lisp 1+ operator
;;;
(defun 1+ (x) (+ x 1))

;;;
;;;	Common Lisp 1- operator
;;;
(defun 1- (x) (- x 1))

(defasm exp-fixnum (n)
    {
        push    ebp
        mov     ebp, esp
        mov     edx, [ebp + ARGS_OFFSET]
  		begin-atomic
        sar     edx, 3
        push    edx
        fild    [esp]                   ;; push onto fpu stack
        add     esp, 4
        xor     edx, edx
  		end-atomic
        push    (* uvector-single-float-tag 8)
        callp   alloc-8-byte-uvector    ;; eax = single float node
        add     esp, 4 
        fldl2e                  ; log2(e) x 
        fmul        st0, st1    ; z = x * log2(e) x 
        fst         st1         ; z z 
        frndint                 ; int(z) z 
        fxch                    ; z int(z) 
        fsub        st0, st1    ; frac(z) int(z) 
        f2xm1                   ; 2^frac(z)-1 int(z) 
        fld1                    ; 1 2^frac(z)-1 int(z) 
        faddp       st1, st0    ; 2^frac(z) int(z) 
        fscale                  ; 2^z int(z) 
        fstp        st1         ; 2^z 
   		fstp.single [eax + (uvector-offset 1)]
        mov     ecx, 1
        pop     ebp
        ret
    })

(defasm exp-bignum (n)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + ARGS_OFFSET]
        callp   bignum-to-single-float
        add     esp, 4
        fld.single [eax + (uvector-offset 1)]
        fldl2e                  ; log2(e) x 
        fmul        st0, st1    ; z = x * log2(e) x 
        fst         st1         ; z z 
        frndint                 ; int(z) z 
        fxch                    ; z int(z) 
        fsub        st0, st1    ; frac(z) int(z) 
        f2xm1                   ; 2^frac(z)-1 int(z) 
        fld1                    ; 1 2^frac(z)-1 int(z) 
        faddp       st1, st0    ; 2^frac(z) int(z) 
        fscale                  ; 2^z int(z) 
        fstp        st1         ; 2^z 
   		fstp.single [eax + (uvector-offset 1)]
        mov     ecx, 1
        pop     ebp
        ret
    })               

(defasm exp-ratio (n)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + ARGS_OFFSET]
        callp   ratio-to-single-float
        add     esp, 4
        fld.single [eax + (uvector-offset 1)]
        fldl2e                  ; log2(e) x 
        fmul        st0, st1    ; z = x * log2(e) x 
        fst         st1         ; z z 
        frndint                 ; int(z) z 
        fxch                    ; z int(z) 
        fsub        st0, st1    ; frac(z) int(z) 
        f2xm1                   ; 2^frac(z)-1 int(z) 
        fld1                    ; 1 2^frac(z)-1 int(z) 
        faddp       st1, st0    ; 2^frac(z) int(z) 
        fscale                  ; 2^z int(z) 
        fstp        st1         ; 2^z 
   		fstp.single [eax + (uvector-offset 1)]
        mov     ecx, 1
        pop     ebp
        ret
    })               
    
(defasm exp-short-float (n)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
        begin-atomic
		and         al, #xfc
        push        eax
		fld.single  [esp]
        fldl2e                  ; log2(e) x 
        fmul        st0, st1    ; z = x * log2(e) x 
        fst         st1         ; z z 
        frndint                 ; int(z) z 
        fxch                    ; z int(z) 
        fsub        st0, st1    ; frac(z) int(z) 
        f2xm1                   ; 2^frac(z)-1 int(z) 
        fld1                    ; 1 2^frac(z)-1 int(z) 
        faddp       st1, st0    ; 2^frac(z) int(z) 
        fscale                  ; 2^z int(z) 
        fstp        st1         ; 2^z 
  		fstp.single [esp]
        pop         edx
		mov			eax, edx				;; untagged 32-bit float in eax, edx
		and         edx, #x7FFFFF			;; eax = 23-bit mantissa
		cmp         edx, #x7FFFFE			;; avoid overflow when rounding
		jae         short :t1
		mov         cl, al					;; get low three bits of mantissa
		and         cl, 7
		cmp         cl, 3
		je          short :t2
		cmp         cl, 5
		jle         short :t1
	:t2
		add         eax, 2
	:t1
		or          al, 3
        xor         edx, edx
		end-atomic
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
    })

(defasm exp-single-float (n)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
		fld.single  [eax + (uvector-offset 1)]
        fldl2e                  ; log2(e) x 
        fmul        st0, st1    ; z = x * log2(e) x 
        fst         st1         ; z z 
        frndint                 ; int(z) z 
        fxch                    ; z int(z) 
        fsub        st0, st1    ; frac(z) int(z) 
        f2xm1                   ; 2^frac(z)-1 int(z) 
        fld1                    ; 1 2^frac(z)-1 int(z) 
        faddp       st1, st0    ; 2^frac(z) int(z) 
        fscale                  ; 2^z int(z) 
        fstp        st1         ; 2^z 
        push        (* uvector-single-float-tag 8)
        callp       alloc-8-byte-uvector    ;; eax = single float node
        add         esp, 4    
		fstp.single [eax + (uvector-offset 1)]
        mov         ecx, 1
        pop         ebp
        ret  
    })

(defasm exp-double-float (n)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + ARGS_OFFSET]
		fld         [eax + (uvector-offset 2)]
        fldl2e                  ; log2(e) x 
        fmul        st0, st1    ; z = x * log2(e) x 
        fst         st1         ; z z 
        frndint                 ; int(z) z 
        fxch                    ; z int(z) 
        fsub        st0, st1    ; frac(z) int(z) 
        f2xm1                   ; 2^frac(z)-1 int(z) 
        fld1                    ; 1 2^frac(z)-1 int(z) 
        faddp       st1, st0    ; 2^frac(z) int(z) 
        fscale                  ; 2^z int(z) 
        fstp        st1         ; 2^z 
        push        (* uvector-double-float-tag 8)
        callp       alloc-16-byte-uvector    ;; eax = single float node
        add         esp, 4    
		fstp        [eax + (uvector-offset 2)]
        mov         ecx, 1
        pop         ebp
        ret  
    })

(defun complex-exp (x)
    (let* ((re (realpart x))
           (im (imagpart x))
           (x (exp re)))
        (complex (* x (cos im)) (* x (sin im))))) 

(defun exp-non-number (x)
    (unless (numberp x)
        (cl::signal-type-error x 'number)))

(defconstant exp-functions 
    (vector 
        #'exp-fixnum
        #'exp-bignum
        #'exp-ratio
        #'exp-short-float
        #'exp-single-float
        #'exp-double-float
        #'complex-exp 
        #'exp-non-number))

;;;
;;;	Common Lisp EXP operator
;;;
(defasm cl:exp (x)
    {
        push    ebp
        mov     ebp, esp  
        push    [ebp + ARGS_OFFSET]
        callp   num-type
        add     esp, 4                                  ;; eax = typeof(x)
        mov     edx, exp-functions
        shr     eax, 1
        mov     eax, [edx + (uvector-offset 2) + eax]   ;; eax = promotion function
        push    [ebp + ARGS_OFFSET]                     ;; push x
        mov     ecx, 1
		callfunc	eax
        add     esp, 4
        mov     ecx, 1
        pop     ebp
        ret
    })

;;;
;;; Common Lisp EXPT function
;;;
(defun expt (base power)
	(unless (numberp base)
		(error 'type-error :datum base :expected-type 'number))
	(unless (numberp power)
		(error 'type-error :datum power :expected-type 'number))
	(if (and (integerp power)
			(or (rationalp base)
				(and (complexp base) 
					(rationalp (realpart base))
					(rationalp (imagpart base)))))
		;; calculate exact result
		(if (< power 0)
			(do ((result 1)
				 (p (- power)))
				((= p 0) (/ 1 result))
				(if (oddp p)
					(setf result (* result base)))
				(setf p (ash p -1))
				(setf base (* base base)))
			(do ((result 1))
				((= power 0) result)
				(if (oddp power)
					(setf result (* result base)))
				(setf power (ash power -1))
				(setf base (* base base))))
		
		;; approximate result
		(if (or (complexp base) 
				(complexp power)
				(and (< base 0) (not (integerp power))))
			(cl::complex-expt base power)    ;; for now, just use definition in math2.lisp
			(exp (* power (log base))))))

;;;
;;; Replace Common Lisp GCD operator
;;;
(defasm cl::gcd ()   ; &rest nums
    {
        push    ebp
        mov     ebp, esp
        cmp     ecx, 2
        jl      :next1
        jg      :next3
        push    [ebp + (+ ARGS_OFFSET 4)]
        push    [ebp + (+ ARGS_OFFSET 0)]
        callp   gcd-numbers
        add     esp, 8
        jmp     short :done
    :next1
        cmp     ecx, 1
        jne     short :next2
        push    [ebp + (+ ARGS_OFFSET 0)]
        callf   abs
        add     esp, 4
        jmp     short :done
    :next2      
        mov     eax, 0      ; identity is 0
        jmp     short :done
    :next3
        mov     eax, [ebp + ecx*4 + (- (+ ARGS_OFFSET 0) 4)]
        dec     ecx
        dec     ecx
    :loop
        cmp     ecx, 0
        jl      short :done
        push    ecx
        push    eax
        push    [ebp + ecx*4 + (+ ARGS_OFFSET 0)]
        mov     ecx, 2
        callp   gcd-numbers
        add     esp, 8
        pop     ecx
        dec     ecx
        jmp     short :loop
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

;;;
;;;	Common Lisp LCM function.
;;;
(defun lcm (&rest integers)
    (if (null integers)
        1
        (let ((first (car integers))
              (rest (cdr integers)))
            (if (null rest)
                (progn
                    (unless (integerp first)
                        (cl::signal-type-error first 'integer))
                    (abs first))
                (progn
                    (if (= first 0)
                        (return-from lcm 0))
                    (dolist (x rest first)
                        (unless (integerp x)
                            (cl::signal-type-error x 'integer))
                        (if (= first 0)
                            (return-from lcm 0))
                        (setq first (/ (abs (* first x)) (gcd first x)))))))))
                    
(defasm log-fixnum (n base)
    {
        push    ebp
        mov     ebp, esp
        mov     edx, [ebp + (+ ARGS_OFFSET 4)]
  		begin-atomic
        sar     edx, 3
        push    edx
        fld1                            ;; push constant 1.0
        fild    [esp]                   ;; push n onto fpu stack
        add     esp, 4
        mov     edx, [ebp + (+ ARGS_OFFSET 0)]
  		end-atomic
        fyl2x                                        ;; log2(x)
        cmp         edx, [esi]                 
        jne         short :next1
        fldl2e                                       ;; push log2(e)
        jmp         short :next2
    :next1
        fld1
        fld         [edx + (uvector-offset 2)]
        fyl2x
    :next2
        fdivp                                         ;; compute log2(n)/log2(base)         
        push    (* uvector-single-float-tag 8)
        callp   alloc-8-byte-uvector    ;; eax = single float node
        add     esp, 4 
   		fstp.single [eax + (uvector-offset 1)]
        mov     ecx, 1
        pop     ebp
        ret
    })

(defasm log-bignum (n base)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + (+ ARGS_OFFSET 4)]
        callp   bignum-to-single-float
        add     esp, 4
        mov     edx, [ebp + (+ ARGS_OFFSET 0)]
        fld1                                    ;; push constant 1.0
        fld.single [eax + (uvector-offset 1)]
        fyl2x                                   ;; log2(x)
        cmp         edx, [esi]                 
        jne         short :next1
        fldl2e                                  ;; push log2(e)
        jmp         short :next2
    :next1
        fld1
        fld         [edx + (uvector-offset 2)]
        fyl2x
    :next2
        fdivp                                   ;; compute log2(n)/log2(base)         
   		fstp.single [eax + (uvector-offset 1)]
        mov     ecx, 1
        pop     ebp
        ret
    })               

(defasm log-ratio (n base)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + (+ ARGS_OFFSET 4)]
        callp   ratio-to-single-float
        add     esp, 4
        mov     edx, [ebp + (+ ARGS_OFFSET 0)]
        fld1                                    ;; push constant 1.0
        fld.single [eax + (uvector-offset 1)]
        fyl2x                                   ;; log2(x)
        cmp         edx, [esi]                 
        jne         short :next1
        fldl2e                                  ;; push log2(e)
        jmp         short :next2
    :next1
        fld1
        fld         [edx + (uvector-offset 2)]
        fyl2x
    :next2
        fdivp                                   ;; compute log2(n)/log2(base)         
   		fstp.single [eax + (uvector-offset 1)]
        mov     ecx, 1
        pop     ebp
        ret
    })               
    
(defasm log-short-float (n base)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + (+ ARGS_OFFSET 4)]
        mov         edx, [ebp + (+ ARGS_OFFSET 0)]
        begin-atomic
		and         al, #xfc
        push        eax
        fld1                                    ;; push constant 1.0
  		fld.single  [esp]
        fyl2x                                   ;; log2(x)
        cmp         edx, [esi]                 
        jne         short :next1
        fldl2e                                  ;; push log2(e)
        jmp         short :next2
    :next1
        fld1
        fld         [edx + (uvector-offset 2)]
        fyl2x
    :next2
        fdivp                                   ;; compute log2(n)/log2(base)         
  		fstp.single [esp]
        pop         edx
		mov			eax, edx				;; untagged 32-bit float in eax, edx
		and         edx, #x7FFFFF			;; eax = 23-bit mantissa
		cmp         edx, #x7FFFFE			;; avoid overflow when rounding
		jae         short :t1
		mov         cl, al					;; get low three bits of mantissa
		and         cl, 7
		cmp         cl, 3
		je          short :t2
		cmp         cl, 5
		jle         short :t1
	:t2
		add         eax, 2
	:t1
		or          al, 3
        xor         edx, edx
		end-atomic
        mov         ecx, 1
		mov         esp,ebp
		pop         ebp
		ret
    })

;; base is assumed to be a double-float (or nil)
(defasm log-single-float (n base)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + (+ ARGS_OFFSET 4)]   ;; eax = n
        mov         edx, [ebp + (+ ARGS_OFFSET 0)]   ;; edx = base
        fld1                                         ;; push constant 1.0
		fld.single  [eax + (uvector-offset 1)]
        fyl2x                                        ;; log2(x)
        cmp         edx, [esi]                 
        jne         short :next1
        fldl2e                                       ;; push log2(e)
        jmp         short :next2
    :next1
        fld1
        fld         [edx + (uvector-offset 2)]
        fyl2x
    :next2
        fdivp                                         ;; compute log2(n)/log2(base)         
        push        (* uvector-single-float-tag 8)
        callp       alloc-8-byte-uvector    ;; eax = single float node
        add         esp, 4    
		fstp.single [eax + (uvector-offset 1)]
        mov         ecx, 1
        pop         ebp
        ret  
    })

(defasm log-double-float (n base)
    {
		push        ebp
		mov         ebp,esp
        mov         eax, [ebp + (+ ARGS_OFFSET 4)]   ;; eax = n
        mov         edx, [ebp + (+ ARGS_OFFSET 0)]   ;; edx = base
        fld1                                         ;; push constant 1.0
  		fld         [eax + (uvector-offset 2)]
        fyl2x                                        ;; log2(x)
        cmp         edx, [esi]                 
        jne         short :next1
        fldl2e                                       ;; push log2(e)
        jmp         short :next2
    :next1
        fld1
        fld         [edx + (uvector-offset 2)]
        fyl2x
    :next2
        fdivp                                         ;; compute log2(n)/log2(base)         
        push        (* uvector-double-float-tag 8)
        callp       alloc-16-byte-uvector    ;; eax = single float node
        add         esp, 4    
		fstp        [eax + (uvector-offset 2)]
        mov         ecx, 1
        pop         ebp
        ret  
    })

;; How can this not take into account the base??
(defun complex-log (n base)
    (if base
        (/ (log n) (log base))
        (complex (log (float (abs n))) (phase n))))   

(defun log-non-number (n base)
    (declare (ignore base))
    (unless (numberp n)
        (cl::signal-type-error n 'number)))

(defconstant log-functions 
    (vector 
        #'log-fixnum
        #'log-bignum
        #'log-ratio
        #'log-short-float
        #'log-single-float
        #'log-double-float
        #'complex-log 
        #'log-non-number))

(defconstant log_neg_1 (complex 0d0 (float pi 0d0)))

;;;
;;;	Common Lisp LOG operator
;;;
(defun log (n &optional base)
    (unless (or (null base)(numberp base))
        (cl::signal-type-error base 'number))
    (if (and base (zerop base))
        	(return-from log (float 0 (float n))))  ;; return 0 of same type as n (if float)
    (if (zerop n)
        (error 'floating-point-underflow :operation 'log :operands (list n base)))
    (if (realp base) 
        (setq base (float base 0d0))      ;; base must be a double-float
        (if (complexp base)
            (return-from log (/ (log n) (log base)))))
    (let ((type (num-type n)))
        (if (and (< type ComplexID) (< n 0))
            ;; handle negative number
            (+ log_neg_1 (log (- n) base))
            (funcall (uref log-functions (+ type 2)) n base))))

;;;
;;;	Common Lisp MOD operator
;;;
(defun mod (number divisor)
    (multiple-value-bind (value remainder)
    			(floor number divisor)
    			(declare (ignore value))
    			remainder))

;;;
;;;	Common Lisp REM operator
;;;
(defun rem (number divisor)
    (multiple-value-bind (value remainder)
    			(truncate number divisor)
    			(declare (ignore value))
    			remainder))

;;;
;;; Common Lisp SIGNUM operator
;;;
(defun signum (number)
    (if (zerop number)
        number
        (if (complexp number)
            (let ((abs (abs number)))
                (if (zerop abs)
                    (complex (signum (realpart number)) (signum (imagpart number)))
                    (/ number abs)))
            (/ number (abs number)))))

;;;
;;;	%MAKE-SINGLE-FLOAT function
;;;	Uses the bits of the passed integer to create a float.
;;; Redefines the function in math.lisp to be more efficient
;;; and safer re GC.
;;;
(defasm cl::%make-single-float (num)
	{
		push	ebp
		mov		ebp, esp
        push    (* uvector-single-float-tag 8)
        callp   alloc-8-byte-uvector    ;; eax = single float node
        add     esp, 4    
   		mov		ecx, [ebp + ARGS_OFFSET]
		test	ecx, 7
    begin-atomic
   		jne		short :bignum
		shr		ecx, 3
		mov		[eax + (uvector-offset cl::single-float-offset)], ecx
		jmp		short :exit
	:bignum
		mov		ecx, [ecx + (uvector-offset cl::bignum-first-cell-offset)]
		mov		[eax + (uvector-offset cl::single-float-offset)], ecx
	:exit
		mov		ecx, 1
    end-atomic
   		pop		ebp
		ret
	})

;;;
;;;	%MAKE-DOUBLE-FLOAT function
;;;	Uses the bits of the passed integer to create a float.
;;; Redefines the function in math.lisp to be more efficient
;;; and safer re GC.
;;;
(defasm cl::%make-double-float (num)
	{
		push	ebp
		mov		ebp, esp
        push    (* uvector-double-float-tag 8)
        callp   alloc-16-byte-uvector    ;; eax = double float node
        add     esp, 4    
   		mov		ecx, [ebp + ARGS_OFFSET]
		test	ecx, 7
    begin-atomic
		jne		:bignum
		shr		ecx, 3
		mov		[eax + (uvector-offset cl::double-float-offset)], ecx
		xor		ecx, ecx
		mov		[eax + (uvector-offset (+ cl::double-float-offset 1))], ecx
		jmp		:exit
	:bignum
   		mov		edx, [ebp + ARGS_OFFSET]
   		mov		ecx, [edx + (uvector-offset cl::bignum-first-cell-offset)]
		mov		[eax + (uvector-offset cl::double-float-offset)], ecx
		mov		ecx, [edx + (uvector-offset (+ cl::bignum-first-cell-offset 1))]
		mov		[eax + (uvector-offset (+ cl::double-float-offset 1))], ecx
	:exit
		mov		ecx, 1
    end-atomic
		pop		ebp
		ret
	})

;;;
;;;	%MAKE-SHORT-FLOAT function
;;;	Uses the bits of the passed integer to create a float.
;;; Redefines the function in math.lisp to be more efficient
;;; and safer re GC.
;;;
(defasm cl::%make-short-float (num)
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
(defasm cl::%single-float-bits (float)
	{
		push	ebp
		mov		ebp, esp
		mov		edx, [ebp + ARGS_OFFSET]
    begin-atomic
		mov		eax, [edx + (uvector-offset cl::single-float-offset)]
		test	eax, #xe0000000
		jne		short :bignum
		shl		eax, 3
    end-atomic
        mov     ecx, 1
		jmp		short :exit
	:bignum
        mov     eax, 0  ;; clear untagged bits
    end-atomic
		push	8
        mov     ecx, 1
		callp	cl::alloc-bignum
		add		esp, 4
		mov		edx, [ebp + ARGS_OFFSET]
    begin-atomic
		mov		ecx, [edx + (uvector-offset cl::single-float-offset)]
		mov		[eax + (uvector-offset cl::bignum-first-cell-offset)], ecx
        mov     ecx, 1
    end-atomic
	:exit
		pop		ebp
		ret
	})

;;;
;;;	%DOUBLE-FLOAT-BITS function
;;;	Returns the bits of the passed float as an integer.
;;;
(defasm cl::%double-float-bits (float)
	{
		push	ebp
		mov		ebp, esp
		mov		edx, [ebp + ARGS_OFFSET]
    begin-atomic
		mov		eax, [edx + (uvector-offset cl::double-float-offset)]
		mov		ecx, [edx + (uvector-offset (+ 1 cl::double-float-offset))] 
		test	eax, #xe0000000
		jne		short :bignum
		test	ecx, ecx
		jne		short :bignum
		shl		eax, 3
    end-atomic
        mov     ecx, 1
		jmp		short :exit
	:bignum
        mov     eax, 0      ;; clear untagged bits
    end-atomic
		push	16
        mov     ecx, 1
		callp	cl::alloc-bignum
		add		esp, 4
		mov		edx, [ebp + ARGS_OFFSET]
    begin-atomic
		mov		ecx, [edx + (uvector-offset cl::double-float-offset)]
		mov		[eax + (uvector-offset cl::bignum-first-cell-offset)], ecx
		mov		ecx, [edx + (uvector-offset (+ 1 cl::double-float-offset))]
		mov		[eax + (uvector-offset (+ cl::bignum-first-cell-offset 1))], ecx
		mov		ecx, 1
    end-atomic
	:exit
  		pop		ebp
		ret
	})

;;;
;;;	%SHORT-FLOAT-BITS function
;;;	Returns the bits of the passed short float as an integer.
;;;
(defasm cl::%short-float-bits (float)
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
		mov		ecx, 1
		jmp		short :exit
	:bignum
		push	8
		mov		ecx, 1
		callp	cl::alloc-bignum
		add		esp, 4
		mov		ecx, [ebp + ARGS_OFFSET]
	begin-atomic
		shr		ecx, 2
		mov		[eax + (uvector-offset cl::bignum-first-cell-offset)], ecx
        mov     ecx, 1
	end-atomic
	:exit
		pop		ebp
		ret
	})   

;;;
;;; Common Lisp NUMBERP operator
;;;
(defasm cl:numberp (x)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + ARGS_OFFSET]
        callp   num-type
        add     esp, 4
        cmp     eax, (* NotNumberID 8)
        jne     short :number
        mov     eax, [esi]
        jmp     short :done
    :number
        mov     eax, [esi + 4]
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

;;;
;;; Common Lisp REALP operator
;;;
(defasm cl:realp (x)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + ARGS_OFFSET]
        callp   num-type
        add     esp, 4
        cmp     eax, (* ComplexID 8)
        jl      short :real
        mov     eax, [esi]
        jmp     short :done
    :real
        mov     eax, [esi + 4]
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

;;;
;;; Common Lisp RATIONALP operator
;;;
(defasm cl:rationalp (x)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + ARGS_OFFSET]
        callp   num-type
        add     esp, 4
        cmp     eax, (* RatioID 8)
        jle      short :rational
        mov     eax, [esi]
        jmp     short :done
    :rational
        mov     eax, [esi + 4]
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

;;;
;;;	Common Lisp CIS function.
;;;
(defun cis (x) 
    (unless (realp x)
        (cl::signal-type-error x 'real))
    (exp (* cl::imag-one x)))

;;;
;;;	Common Lisp CONJUGATE function.
;;;
(defun conjugate (number)
    (unless (numberp number)
        (cl::signal-type-error number 'number))
    (if (realp number)
        number
        (complex (realpart number) (- (imagpart number)))))

;;;
;;;	Common Lisp PHASE function.
;;;
(defun phase (number)
    (unless (numberp number)
        (cl::signal-type-error number 'number))
	(cond
		((complexp number)(atan (imagpart number) (realpart number)))
		((minusp number) pi)
		(t 0.0)))	 

;;;
;;; Common Lisp REALPART operator
;;;
(defun cl:realpart (number)
    (if (complexp number)
        (uref number 1)
        (if (realp number)
            number
            (cl::signal-type-error number 'number))))

;;;
;;; Common Lisp IMAGPART operator
;;;
(defun cl:imagpart (number)
    (if (complexp number)
        (uref number 2)
        (if (realp number)
            (* 0 number)
            (cl::signal-type-error number 'number))))

;;;
;;; Common Lisp UPGRADED-COMPLEX-PART-TYPE operator
;;;
(defun cl:upgraded-complex-part-type (typespec &optional environment)
    (declare (ignore environment))
    typespec)       ;; we can store any numeric type in the complex number
            
;;;;
;;;;	Common Lisp NUMERATOR function.
;;;;
(defun numerator (x) 
	(unless (rationalp x) 
        (cl::signal-type-error x 'rational))
	(if (ratiop x) 
        (uref x cl::ratio-numerator-offset) x))

;;;;
;;;;	Common Lisp DENOMINATOR function.
;;;;
(defun denominator (x) 
	(unless (rationalp x) 
        (cl::signal-type-error x 'rational))
	(if (ratiop x) 
        (uref x cl::ratio-denominator-offset) 1))

;;;;
;;;;	Corman Lisp DOUBLE-FLOAT-P function.
;;;;
(defasm cl::double-float-p (x)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :next1
		callp 	_wrong-number-of-args-error
	:next1
		mov		edx, [ebp + ARGS_OFFSET]		;; edx = argument
		mov		eax, edx
		and		eax, 7
		cmp		eax, uvector-tag
		mov		eax, [esi]
		jne		short :next2
		mov 	edx, [edx - uvector-tag]
		cmp 	dl, (tag-byte cl::uvector-double-float-tag)
		jne 	short :next2
		mov		eax, [esi + t-offset]
	:next2
		pop		ebp
		ret
	})

;;;;
;;;;	Corman Lisp SINGLE-FLOAT-P function.
;;;;
(defasm cl::single-float-p (x)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :next1
		callp 	_wrong-number-of-args-error
	:next1
		mov		edx, [ebp + ARGS_OFFSET]		;; edx = argument
		mov		eax, edx
		and		eax, 7
		cmp		eax, uvector-tag
		mov		eax, [esi]
		jne		short :next2
		mov 	edx, [edx - uvector-tag]
		cmp 	dl, (tag-byte cl::uvector-single-float-tag)
		jne 	short :next2
		mov		eax, [esi + t-offset]
	:next2
		pop		ebp
		ret
	})

;;;;
;;;;	Corman Lisp SHORT-FLOAT-P function.
;;;;
(defasm cl::short-float-p (x)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		edx, [ebp + ARGS_OFFSET]		;; edx = argument
		and		edx, 3
		cmp		edx, 3
		je		short :true
	:false
		mov		eax, [esi]
		jmp		short :t2
	:true
		mov		eax, [esi + t-offset]
	:t2
		pop		ebp
		ret
	})

;;;;
;;;	Common Lisp RATIONAL function.
;;;
(defun rational (number)
	(unless (realp number)
        (cl::signal-type-error number 'number))
	(if (rationalp number) 
		(return-from rational number))
	(multiple-value-bind (mantissa exp sign)
		(integer-decode-float number)
		(cond ((cl::double-float-p number)
			   (* sign (/ (ash mantissa (+ 52 exp)) #x10000000000000)))
		  	  ((cl::single-float-p number)
			   (* sign (/ (ash mantissa (+ 24 exp)) #x1000000)))
		  	  ((cl::short-float-p number)
			   (* sign (/ (ash mantissa (+ 22 exp)) #x400000))))))

;;;
;;;	Common Lisp RATIONALIZE function.
;;;
(defun rationalize (number)
	;; need to work on this one
	(rational number))

(defasm cl::fixnum-integer-length (num)
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
    begin-atomic
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
    end-atomic
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
		((cl::fixnump num) (cl::fixnum-integer-length num))
		((cl::bignump num) (cl::bignum-integer-length num))
		(t (cl::signal-type-error num 'integer))))

;;;
;;;	Common Lisp PARSE-INTEGER operator
;;;
(defun parse-integer (string 
		&key (start 0) 
			 (end (length string))
			 (radix 10)
			 (junk-allowed nil)
		&aux (result 0)
			 (state :initial)
			 (sign 1)
			 c)
    (unless (typep radix '(integer 2 36))
        (cl::signal-type-error radix '(integer 2 36)))
    (unless (stringp string)
        (cl::signal-type-error string 'string))
    
	;; check for leading sign
	(setf c (char string start))
	(if (char= c #\-)
		(progn (setf sign -1) (incf start))
		(if (char= c #\+)
			(incf start)))

	(do* ((i start (+ i 1))
		  (n 0))
		((>= i end))
		(setq c (char string i))
		(setq n (digit-char-p c radix))
		(cond
			(n (progn
				(cond
					((eq state :finished) 
					 (if (not junk-allowed)
						(error 'parse-error "Invalid integer parsed: ~A" string)
						(progn (setq end i) (return)))))
				(setq result (+ (* result radix) n))
				(setq state :collecting)))
			
			((member c (list (int-char 13) (int-char 32) (int-char 9)))	;; '(#\Newline #\Space #\Tab)
				(cond
					((eq state :collecting) (setq state :finished))
					((eq state :initial) nil)	; don't do anything
					((eq state :finished) nil)))
			(t 
				(if (not junk-allowed)
					(error 'parse-error "Invalid integer parsed: ~A" string)  ;; string  
					(progn (setq end i) (return))))))

	(if (eq state :initial)
		(setq result nil)
		(setq result (* result sign)))
	(values result end))

;;;
;;; BOOLE constants are defined in boole.lisp
;;;
(defconstant boole-funcs 
    (vector
        (lambda (i1 i2) (check-type i1 integer) (check-type i2 integer) 0)  ; BOOLE-CLR
        #'logand                                                            ; BOOLE-AND
        #'logandc1                                                          ; BOOLE-ANDC1
        (lambda (i1 i2) (check-type i1 integer) (check-type i2 integer) i2) ; BOOLE-2
        #'logandc2                                                          ; BOOLE-ANDC2
        (lambda (i1 i2) (check-type i1 integer) (check-type i2 integer) i1) ; BOOLE-1        
        #'logxor                                                            ; BOOLE-XOR
        #'logior                                                            ; BOOLE-IOR
        #'lognor                                                            ; BOOLE-NOR
        #'logeqv                                                            ; BOOLE-EQV        
        (lambda (i1 i2) (check-type i2 integer) (lognot i1))                ; BOOLE-C1
        #'logorc1                                                           ; BOOLE-ORC1
        (lambda (i1 i2) (check-type i1 integer) (lognot i2))                ; BOOLE-C2
        #'logorc2                                                           ; BOOLE-ORC2
        #'lognand                                                           ; BOOLE-NAND        
        (lambda (i1 i2) (check-type i1 integer) (check-type i2 integer) -1) ; BOOLE-SET
    ))

;;;
;;; Common Lisp BOOLE operator
;;;
(defun boole (op i1 i2)
    (unless (and (integerp op) (>= op boole-clr) (<= op boole-set))
        (cl::signal-type-error op '(integer 0 15)))
    (funcall (uref boole-funcs (+ op 2)) i1 i2))

;;;
;;;	Common Lisp LOGTEST function.
;;;
(defun logtest (x y) 
    (not (zerop (logand x y))))

;;;
;;;	Common Lisp MASK-FIELD function.
;;;
;;; The hyperspec says
;;; (mask-field bs n) ==  (logand n (dpb -1 bs 0))
;;;
(defun mask-field (bytespec integer)
    (logand integer (dpb -1 bytespec 0)))

;;;
;;; Common Lisp (SETF MASK-FIELD) function
;;;
(defmacro cl::|(SETF MASK-FIELD)| (new-byte bytespec place)
	(if (and (consp place) (some #'consp place))
		(let ((retval (cl::%once-only-forms place))
			  (sym (gensym)))
			`(let ,(car retval)
				(let ((,sym ,new-byte)) 
					(setf ,(cdr retval) (deposit-field ,sym ,bytespec ,(cdr retval)))
					,sym)))
		(let ((sym (gensym)))
			`(let ((,sym ,new-byte))
				(setf ,place (deposit-field ,sym ,bytespec ,place))
				,sym))))

(cl::register-setf-function 'mask-field 'cl::|(SETF MASK-FIELD)|)

(defasm cl::short-float-exponent-bits (float)
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

(defasm cl::short-float-sign-bit (float)
	{
		push	ebp
		mov		ebp, esp
		mov		eax, [ebp + ARGS_OFFSET]
		shr		eax, 31
		shl		eax, 3
		pop		ebp
		ret
	})

(defasm cl::short-float-mantissa-bits (float)
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

(defun cl::float-get-exponent-bits (float)
	(if (cl::short-float-p float)
		(cl::short-float-exponent-bits float)
		(if (cl::single-float-p float)
			(logand (ash (cl::%single-float-bits float) -23) #xff)
			(logand (ash (cl::%double-float-bits float) -52) #x7ff))))

(defun cl::float-set-exponent-bits (float exp-bits)
	(if (cl::short-float-p float)
		(let* ((bits (cl::%short-float-bits float)))
			(cl::%make-short-float (+ (logand #xe01fffff bits)(ash exp-bits 21))))
		(if (cl::single-float-p float)
			(let* ((bits (cl::%single-float-bits float)))
				(cl::%make-single-float (+ (logand #x807fffff bits)(ash exp-bits 23))))
			(let* ((bits (cl::%double-float-bits float)))
				(cl::%make-double-float (+ (logand #x800fffffffffffff bits)(ash exp-bits 52)))))))		 	

(defun cl::float-get-sign-bit (float)
	(if (cl::short-float-p float)
		(cl::short-float-sign-bit float)
		(if (cl::single-float-p float)
			(ash (cl::%single-float-bits float) -31)
			(ash (cl::%double-float-bits float) -63))))

(defun float-get-mantissa-bits (float)
	(if (cl::short-float-p float)
		(+ #x200000 (cl::short-float-mantissa-bits float))
		(if (cl::single-float-p float)
			(+ #x800000 (logand (cl::%single-float-bits float) #x7fffff))
			(+ #x10000000000000 (logand (cl::%double-float-bits float) #xfffffffffffff)))))

(defun %create-double-float-from-bits (mantissa exponent sign)
	(let ((n (+ 
				(if (= sign -1) (ash 1 63) 0) 
				(ash (+ exponent #x3ff) 52) 
				(- mantissa #x10000000000000))))	
		(cl::%make-double-float n)))

(defun %create-single-float-from-bits (mantissa exponent sign)
	(let ((n (+ 
				(if (= sign -1) (ash 1 31) 0) 
				(ash (+ exponent #x7f) 23) 
				(- mantissa #x800000))))	
		(cl::%make-single-float n)))

(defun %create-short-float-from-bits (mantissa exponent sign)
	(let ((n (+ 
				(if (= sign -1) (ash 1 29) 0) 
				(ash (+ exponent #x7f) 21) 
				(- mantissa #x200000))))	
		(cl::%make-short-float n)))

;;;
;;;	Common Lisp DECODE-FLOAT function.
;;;
(defun decode-float (float)
	(unless (floatp float) 
        (cl::signal-type-error float 'float))
	(if (= float 0.0)
		(values 0.0 0 1.0)
		(let* ((exp-bits (cl::float-get-exponent-bits float))
			   (normalize (if (cl::double-float-p float) #x3fe #x7e))
			   (delta (- exp-bits normalize)))
			(if (= float 0)
				(values 0.0 0 1.0)
				(values (cl::float-set-exponent-bits float normalize)
				    delta
				    (if (< float 0.0) -1.0 1.0))))))

;;;
;;;	Common Lisp SCALE-FLOAT function.
;;;
(defun scale-float (float scale)
	(unless (floatp float) 
        (cl::signal-type-error float 'float))
   	(multiple-value-bind (mantissa expt sign)
		(integer-decode-float float)
		(cond ((cl::double-float-p float)
			   (incf expt (+ scale 52))
			   (if (< expt #x-3fe)
					(error 'floating-point-underflow :operation 'scale-float :operands (list float scale)))
			   (if (> expt #x3ff)
					(error 'floating-point-overflow :operation 'scale-float :operands (list float scale)))
			   (cl::%create-double-float-from-bits mantissa expt sign))
			  ((cl::single-float-p float)
			   (incf expt (+ scale 23))
			   (if (< expt #x-7e)
					(error 'floating-point-underflow :operation 'scale-float :operands (list float scale)))
			   (if (> expt #x7f)
					(error 'floating-point-overflow :operation 'scale-float :operands (list float scale)))
			   (cl::%create-single-float-from-bits mantissa expt sign))
			  ((cl::short-float-p float)
			   (incf expt (+ scale 21))
			   (if (< expt #x-7e)
					(error 'floating-point-underflow :operation 'scale-float :operands (list float scale)))
			   (if (> expt #x7f)
					(error 'floating-point-overflow :operation 'scale-float :operands (list float scale)))
			   (cl::%create-short-float-from-bits mantissa expt sign)))))

;;;
;;;	Common Lisp FLOAT-SIGN function.
;;;
(defun float-sign (float-1 &optional float-2) 
	(unless (floatp float-1) 
        (cl::signal-type-error float-1 'float))
  	(setf float-2 
		(if float-2
            (progn 
            	(unless (floatp float-2) 
                    (cl::signal-type-error float-2 'float))
			     (abs float-2))
			(setf float-2 (float 1 float-1))))
	(let ((sign-bit (cl::float-get-sign-bit float-1)))
		(if (zerop sign-bit)
			float-2
			(- float-2))))

;;;
;;;	Common Lisp FLOAT-DIGITS function.
;;;
(defun float-digits (float) 
	(unless (floatp float) 
        (cl::signal-type-error float 'float))
	(if (cl::short-float-p float)
		22
		(if (cl::single-float-p float)
			24
			53)))

;;;
;;;	Common Lisp FLOAT-PRECISION function.
;;;
(defun float-precision (float) 
	(unless (floatp float) 
        (cl::signal-type-error float 'float))
	(if (zerop float)
		(return-from float-precision 0))
	(multiple-value-bind (mantissa expt sign)
		(integer-decode-float float)
		(declare (ignore sign))
		(if (cl::short-float-p float)
			(if (= expt -148)
				(integer-length (- mantissa #x200000))
				22)
			(if (cl::single-float-p float)
				(if (= expt -150)
					(integer-length (- mantissa #x800000))
					24)		
				(if (= expt -1075)
					(integer-length (- mantissa #x10000000000000))
					53)))))

;;;
;;;	Common Lisp INTEGER-DECODE-FLOAT function.
;;;
(defun integer-decode-float (float)
	(unless (floatp float) 
        (cl::signal-type-error float 'float))
	(if (= float 0.0)
		(values 0 0 1)
		(let* ((mantissa (cl::float-get-mantissa-bits float))
			   (exponent (if (cl::short-float-p float)
							(- (- (cl::float-get-exponent-bits float) #x7f) 21)
							(if (cl::single-float-p float)
								(- (- (cl::float-get-exponent-bits float) #x7f) 23)
								(- (- (cl::float-get-exponent-bits float) #x3ff) 52))))
			   (sign (if (= (cl::float-get-sign-bit float) 0) 1 -1)))
			(values mantissa exponent sign))))

;;; Redefine ALLOCATE-C-HEAP and DEALLOCATE-C-HEAP

(ct:defun-dll ct::heap-alloc ((heap :handle)(flags :unsigned-long)(size :unsigned-long))
   :return-type (:void *)
   :library-name "Kernel32.dll"
   :entry-name "HeapAlloc"
   :linkage-type :pascal)

(ct:defun-dll ct::heap-free ((heap :handle)(flags :unsigned-long)(mem (:void *)))
   :return-type :long-bool
   :library-name "Kernel32.dll"
   :entry-name "HeapFree"
   :linkage-type :pascal)

(ct:defun-dll ct::get-process-heap ()
   :return-type :handle
   :library-name "Kernel32.dll"
   :entry-name "GetProcessHeap"
   :linkage-type :pascal)

(defasm ct::c-allocate (size)
    {
        push    ebp
        mov     ebp, esp
        push    ebx
        mov     ecx, 0
        callf   ct::get-process-heap
        push    eax
        push    0
        push    [ebp + ARGS_OFFSET]
        mov     ecx, 3
        callf   ct::heap-alloc
        add     esp, 12
        mov     ebx, eax         ;; ebx = new memory block
        push    (* uvector-foreign-heap-tag 8)    ;; push tagged int
        callp   alloc-16-byte-uvector
        add     esp, 4
        mov     edx, [ebx + (uvector-offset 1)] ;; raw pointer in edx--
                                                ;; should be OK because it cannot 
                                                ;; point into lisp heap
        mov     [eax + (uvector-offset 1)], edx
        mov     edx, [ebp + ARGS_OFFSET]        ;; edx = size (wrapped)
        mov     [eax + (uvector-offset 2)], edx
        xor     edx, edx
        mov     [eax + (uvector-offset 3)], edx
        push    eax
        push    eax
        push    #'ct::free
        mov     ecx, 2
        callf   cl::register-finalization
        add     esp, 8
        pop     eax
        pop     ebx
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })

(defasm ct::c-deallocate (ptr)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + ARGS_OFFSET]
        callf   cl::foreign-heap-p
        add     esp, 4
        cmp     eax, [esi]
        jne     :next1
        push    "Not a foreign heap pointer"
        mov     ecx, 1
        callf   error
    :next1
        mov     ecx, 0
        callf   ct::get-process-heap
        push    eax
        push    0
        push    [ebp + ARGS_OFFSET]
        mov     ecx, 3
        callf   ct::heap-free
        add     esp, 12
        xor     edx, edx
        mov     eax, [ebp + ARGS_OFFSET]
        mov     [eax + (uvector-offset 1)], edx      ;; set pointer to null for safety
        mov     [eax + (uvector-offset 2)], edx
        mov     [eax + (uvector-offset 3)], edx
        mov     eax, [esi]                          ;; return NIL
        mov     ecx, 1
        pop     ebp
        ret
    })

(setf temp (ct::c-allocate 10))
(setf (symbol-function 'ct:malloc) #'ct::c-allocate)
(setf (symbol-function 'ct:free) #'ct::c-deallocate)
(setf (symbol-function 'cl::allocate-c-heap) #'ct::c-allocate)

;;;
;;; add a few missing number constants
;;;

;;; Common Lisp SHORT-FLOAT-EPSILON
(defconstant short-float-epsilon (cl::%make-short-float  #x0D400000))

;;; Common Lisp SHORT-FLOAT-NEGATIVE-EPSILON
(defconstant short-float-negative-epsilon (cl::%make-short-float  #x0D400000))

;;; Common Lisp SINGLE-FLOAT-EPSILON
(defconstant single-float-epsilon (cl::%make-single-float  #x34000000))

;;; Common Lisp SINGLE-FLOAT-NEGATIVE-EPSILON
(defconstant single-float-negative-epsilon (cl::%make-single-float  #x34000000))

;;; Common Lisp DOUBLE-FLOAT-EPSILON
(defconstant double-float-epsilon (cl::%make-double-float #x3CB0000000000000))

;;; Common Lisp DOUBLE-FLOAT-NEGATIVE-EPSILON
(defconstant double-float-negative-epsilon (cl::%make-double-float #x3CB0000000000000))

;;; Common Lisp LONG-FLOAT-EPSILON
(defconstant long-float-epsilon double-float-epsilon)

;;; Common Lisp LONG-FLOAT-NEGATIVE-EPSILON
(defconstant long-float-negative-epsilon double-float-negative-epsilon)

