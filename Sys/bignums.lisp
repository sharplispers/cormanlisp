;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		bignums.lisp
;;;;	Contents:	Lisp Bignum handling routines.
;;;;	History:	03/11/06  RGC  Created.
;;;;

(in-package :x86)

(defasm fixnum-to-bignum (n)
    {
        push    ebp
        mov     ebp, esp
        push    (* 8 1)
        mov     ecx, 1
        callp   cl::alloc-bignum        ;; eax = bignum
        add     esp, 4
        mov     edx, [ebp + ARGS_OFFSET] ;; edx = n
        mov     ecx, #x00008000
        shl     ecx, 16                  ;; ecx = #x80000000 (can't load directly)
        cmp     edx, ecx
        jne     :next1
    begin-atomic
        mov     ecx, #x00008000
        shl     ecx, 13
        mov 	[eax + (uvector-offset 2)], ecx                            
        mov 	ecx, [eax + (uvector-offset 1)]    ;; set sign bit
    end-atomic
        add     ecx, 8
        mov     [eax + (uvector-offset 1)], ecx
        jmp     short :done
    :next1
        test    edx, ecx
        jz      short :next2
        ;; number is negative
        mov 	ecx, [eax + (uvector-offset 1)]
        add     ecx, 8
        mov     [eax + (uvector-offset 1)], ecx
        neg     edx
    :next2
    begin-atomic        ;; untagged data alert
        shr     edx, 3
        mov 	[eax + (uvector-offset 2)], edx
        xor     edx, edx
    end-atomic
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

;; redefined later
(defun subtract-fixnums (n1 n2) (- n1 n2))

(defasm bignum-length (b)
    {
        push    ebp
        mov     ebp, esp
        mov     edx, [ebp + ARGS_OFFSET]
        mov     eax, [edx + (uvector-offset 1)]
        shr     eax, 4
        shl     eax, 3
        pop     ebp
        ret
    })

;;
;; Returns the number of significant words of a bignum
;;
(defasm bignum-significant-length (b)
    {
        push    ebp
        mov     ebp, esp
        mov     edx, [ebp + ARGS_OFFSET]
        mov     eax, [edx + (uvector-offset 1)]
        shr     eax, 4
        dec     eax
        xor     ecx, ecx
    :loop
        cmp     eax, 0
        jle     :next1
        cmp     [edx + (uvector-offset 2) + eax*4], ecx
        jne     :next1
        dec     eax
        jg     :loop 
    :next1
        inc     eax
        shl     eax, 3
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })
    
(defasm bignum-negative (b)
    {
        push    ebp
        mov     ebp, esp
        mov     edx, [ebp + ARGS_OFFSET]
        mov     eax, [edx + (uvector-offset 1)]
        and     eax, 8
        je      short :not-negative
        mov     eax, [esi + 4]
        jmp     short :done
    :not-negative
        mov     eax, [esi]
    :done
        pop     ebp
        ret
    })    

;; sign should be 1 (negative) or 0 (positive)
(defasm bignum-set-sign (b sign)
    {
        push    ebp
        mov     ebp, esp
        mov     edx, [ebp + (+ ARGS_OFFSET 4)]
        mov     eax, [edx + (uvector-offset 1)]
        and     al, #xf0
        mov     ecx, [ebp + (+ ARGS_OFFSET 0)]
        or      ecx, ecx
        jz      short :neg
        or      al, 8
    :neg        
        mov     [edx + (uvector-offset 1)], eax
        mov     eax, edx
        mov     ecx, 1
        pop     ebp
        ret
    })

(defun sub-bignums (b1 b2) (- b1 b2))   ;; redefined later

;;
;; Allocate a new uvector and copy it
;;
(defasm copy-uvector (u)
    {
        push    ebp
        mov     ebp, esp
        push    ebx
        mov     edx, [ebp + ARGS_OFFSET]
        mov     eax, [edx + (uvector-offset 0)]
        mov     edx, eax
        and     eax, #x000000f8             ;; remove tag
        xor     dl, dl
        shr     edx, 4
        sub     edx, 8
        push    edx
        push    eax
        mov     ecx, 2
        callp   cl::alloc-uvector
        add     esp, 8
        ;; now copy original to new one
        mov     edx, [ebp + ARGS_OFFSET]
        mov     ecx, [edx + (uvector-offset 0)]
        xor     cl, cl                 
        shr     ecx, 5
    
        ;; eax = new object
        ;; edx = original object
        ;; ecx = number of 64-bit cells in heap object
    begin-atomic        ;; we don't know what we are copying--it could be untagged
    :loop
        sub     ecx, 8
        jl      short :done
        mov     ebx, [edx + (uvector-offset 0) + ecx]
        mov     [eax + (uvector-offset 0) + ecx], ebx     
        mov     ebx, [edx + (uvector-offset 1) + ecx]
        mov     [eax + (uvector-offset 1) + ecx], ebx
        jmp     short :loop
    :done
    end-atomic
        mov     ecx, 1
        pop     ebx
        pop     ebp
        ret
    })

;;
;; Copy cells of b1 to b2. Assumes b2 is as large or larger than b1.
;;
(defasm copy-bignum (b1 b2)
    {
        push    ebp
        mov     ebp, esp
        push    ebx

        mov     edx, [ebp + (+ ARGS_OFFSET 4)]  ; edx = b1
        mov     ebx, [ebp + (+ ARGS_OFFSET 0)]  ; ebx = b2   
     
        mov     ecx, [edx + (uvector-offset 1)]
        shr     ecx, 4                          ; ecx = num cells
        dec     ecx
    begin-atomic
    
    :loop
        cmp     ecx, 0
        jl      short :next1    
        mov     eax, [edx + ecx * 4 + (uvector-offset 2)]
        mov     [ebx + ecx * 4 + (uvector-offset 2)], eax
        dec     ecx
        jmp     short :loop
    :next1
        ;; copy sign bit
        mov     al, [edx + (uvector-offset 1)]
        and     al, 8                           ; al = sign bit
        mov     dl, [ebx + (uvector-offset 1)]
        and     dl, #xf0
        or      dl, al
        mov     [ebx + (uvector-offset 1)], dl 
        mov     eax, 0
        mov     ecx, 1
    end-atomic
        pop     ebx
        pop     ebp
        ret
    })

;;
;; Copy a range of cells from one bignum to another
;;
(defasm copy-bignum-cells (b1 i1 b2 i2 length)
    {
        push    ebp
        mov     ebp, esp
        push    ebx
    begin-atomic
        mov     edx, [ebp + (+ ARGS_OFFSET 16)]  ; edx = b1
        mov     ecx, [ebp + (+ ARGS_OFFSET 12)]  ; ecx = i1
        shr     ecx, 1                           ; ecx = untagged i1 * 4
        add     edx, ecx
        mov     ebx, [ebp + (+ ARGS_OFFSET 8)]   ; ebx = b2
        mov     ecx, [ebp + (+ ARGS_OFFSET 4)]   ; ecx = i2
        shr     ecx, 1                           ; ecx = untagged i2 * 4
        add     ebx, ecx
        mov     ecx, [ebp + (+ ARGS_OFFSET 0)]   ; ecx = length
        shr     ecx, 3                           ; ecx = untagged length
        dec     ecx
    :loop
        cmp     ecx, 0
        jl     short :done
        mov     eax, [edx + ecx*4 + (uvector-offset 2)]
        mov     [ebx + ecx*4 + (uvector-offset 2)], eax
        dec     ecx
        jmp     short :loop
     
    :done
        mov     eax, 0
        mov     ecx, 1
    end-atomic
        pop     ebx
        pop     ebp
        ret
    })
             
;;;
;;; Return a copy of a bignum with the sign flipped.
;;;
(defasm bignum-negate (b)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + ARGS_OFFSET]
        mov     ecx, 1
        callp   copy-uvector
        add     esp, 4
        mov     edx, [eax + (uvector-offset 1)]
        xor     edx, 8
        mov     [eax + (uvector-offset 1)], edx
        mov     ecx, 1
        pop     ebp
        ret
    })                

;;;
;;; Like bignum-negate, but does not copy the bignum (make sure it is not shared)
;;;
(defasm bignum-flip-sign (b)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + ARGS_OFFSET]
        mov     edx, [eax + (uvector-offset 1)]
        xor     edx, 8
        mov     [eax + (uvector-offset 1)], edx
        mov     ecx, 1
        pop     ebp
        ret
    })  

(defasm bignum-reduce (b)
    {
        push    ebp
        mov     ebp, esp
        push    ebx
        mov     eax, [ebp + ARGS_OFFSET]
        mov     edx, [eax + (uvector-offset 1)]
        mov     ecx, edx
        and     cl, #xf0
        shr     ecx, 4          ;; ecx = num cells
        and     edx, 8          ;; edx = sign bit
        xor     ebx, ebx
    :loop
        dec     ecx
        jle      short :done    ;; always leave at least one cell
        cmp     [eax + (uvector-offset 2) + ecx * 4], ebx
        jz      short :loop
    :done
        inc     ecx
        shl     ecx, 4
        cmp     ecx, 0
        je      short :next1
        or      ecx, edx        ;; if no cells, set sign to positive
    :next1
        mov     [eax + (uvector-offset 1)], ecx
        mov     ecx, 1
        pop     ebx
        mov     esp, ebp
        pop     ebp
        ret
    })

(defasm normalize-bignum (bn)
    {
        push    ebp
        mov     ebp, esp
        push    ebx
        push    [ebp + ARGS_OFFSET] 
        callp   bignum-reduce
        add     esp, 4
        mov     edx, [eax + (uvector-offset 1)]
        mov     ecx, edx
        and     edx, 8      ;; edx = sign bit
        shr     ecx, 4      ;; ecx = untagged length
        cmp     ecx, 1
    begin-atomic  
        jg     short :done  ;; more than one cell, it is already normalized
        mov     ebx, [eax + (uvector-offset 2)] ;; ebx = word
        or      edx, edx
        je      short :positive
        mov     ecx, most-positive-fixnum
        inc     ecx         ; can't load #x10000000 directly
        ;cmp     ebx, #x10000000
        cmp     ebx, ecx
        ja      :done
        neg     ebx
        shl     ebx, 3
        mov     eax, ebx
        jmp     short :done
    :positive
        cmp     ebx, most-positive-fixnum
        ja     :done
        shl     ebx, 3
        mov     eax, ebx
        jmp     short :done
    :done
    end-atomic
        mov     ecx, 1
        pop     ebx
        mov     esp, ebp
        pop     ebp
        ret
    })

(defasm add-bignum-words (b1 b1-len b2 b2-len result)
    {
        push    ebp
        mov     ebp, esp
        push    ebx
        push    esi
        push    edi

        mov     esi, [ebp + (+ ARGS_OFFSET 16)] ;; esi = b1
        mov     ebx, [ebp + (+ ARGS_OFFSET 8)]  ;; ebx = b2
        mov     edi, [ebp + (+ ARGS_OFFSET 0)]  ;; edi = result

        mov     ecx, -1
        mov     edx, [ebp + (+ ARGS_OFFSET 4)]
    begin-atomic  
        shr     edx, 3                          ;; edx = b2-len
        jmp     short :t1
	:loop1
        push    edx
        xor     edx, edx
        mov     eax, [esi + ecx*4 + (uvector-offset 2)]
        add     eax, [ebx + ecx*4 + (uvector-offset 2)]
        adc     [edi + ecx*4 + (uvector-offset 3)], edx
        add     [edi + ecx*4 + (uvector-offset 2)], eax
        adc     [edi + ecx*4 + (uvector-offset 3)], edx
        pop     edx
	:t1
        inc     ecx
        
        cmp     ecx, edx
        jl      short :loop1
        dec     ecx
        mov     edx, [ebp + (+ ARGS_OFFSET 12)]  
        shr     edx, 3                          ;; edx = b1-len
        xor     ebx, ebx
        jmp     short :t2
	:loop2
        mov     eax, [esi + ecx*4 + (uvector-offset 2)]
        add     [edi + ecx*4 + (uvector-offset 2)], eax
        adc     [edi + ecx*4 + (uvector-offset 3)], ebx
	:t2
        inc     ecx
        cmp     ecx, edx
        jl	    short :loop2
    
        xor     edx, edx
        xor     eax, eax
    end-atomic
        pop     edi
        pop     esi
        pop     ebx
        mov     eax, [esi]
        mov     ecx, 1
        pop     ebp
        ret
    })
                                   
(defun add-bignums (b1 b2)
    (let ((b1len (bignum-significant-length b1))
          (b2len (bignum-significant-length b2))
          (negative nil)
          result
          dest-size)
        (if (bignum-negative b1)
            (if (bignum-negative b2)
                (setq negative t)
                (return-from add-bignums (sub-bignums b2 (bignum-negate b1))))
            (if (bignum-negative b2)
                (return-from add-bignums (sub-bignums b1 (bignum-negate b2)))))
        
        (setq dest-size (+ 1 (max b1len b2len)))
        (setq result (cl::alloc-bignum dest-size))
        (dotimes (i dest-size)
            (setf (uref result (+ i 2)) 0))     ;; clear all the cells    
        (if (< b1len b2len)
            (add-bignum-words b2 b2len b1 b1len result)
            (add-bignum-words b1 b1len b2 b2len result))
        (if negative
            (bignum-flip-sign result))
        (normalize-bignum result)))

(defasm sub-bignum-words (b1 b1-len b2 b2-len result)
    {
        push    ebp
        mov     ebp, esp
        push    ebx
        push    esi
        push    edi

        mov     esi, [ebp + (+ ARGS_OFFSET 16)] ;; esi = b1
        mov     ebx, [ebp + (+ ARGS_OFFSET 8)]  ;; ebx = b2
        mov     edi, [ebp + (+ ARGS_OFFSET 0)]  ;; edi = result

    begin-atomic
        mov     ecx, -1
        xor     edx, edx		;;borrow flag
        jmp     short :t1
	:loop1
        mov     eax, [esi + ecx*4 + (uvector-offset 2)]
        sub     eax, edx
        mov     edx, 0
        adc     edx, 0
        sub     eax, [ebx + ecx*4 + (uvector-offset 2)]
        adc     edx, 0
        mov     [edi + ecx*4 + (uvector-offset 2)], eax
  	:t1
        inc     ecx
        push    edx
        mov     edx, [ebp + (+ ARGS_OFFSET 4)]  
        shr     edx, 3                          ;; edx = b2len
        cmp     ecx, edx
        pop     edx
        jl      short :loop1
        dec     ecx
        jmp     short :t2
	:loop2
        mov     eax, [esi + ecx*4 + (uvector-offset 2)]
        sub     eax, edx
        mov     edx, 0
        adc     edx, 0
        mov     [edi + ecx*4 + (uvector-offset 2)], eax       
	:t2
        inc     ecx
        push    edx
        mov     edx, [ebp + (+ ARGS_OFFSET 12)]  
        shr     edx, 3                          ;; edx = b1len
        cmp     ecx, edx
        pop     edx        
        jl	    short :loop2
 
        xor     edx, edx
        xor     eax, eax
    end-atomic   
        pop     edi
        pop     esi
        pop     ebx
        mov     eax, [esi]
        mov     ecx, 1
        mov     esp, ebp
        pop     ebp
        ret
    })

(defasm bignum-abs-compare (b1 b2)
    {
        push    ebp
        mov     ebp, esp
        push    ebx
        push    edi
    
        push    [ebp + (+ ARGS_OFFSET 4)]   ;; push b1
        callp   bignum-significant-length
        add     esp, 4
        push    eax
        push    [ebp + ARGS_OFFSET]         ;; push b2
        callp   bignum-significant-length
        add     esp, 4
        pop     edx                         ;; edx = b1len, eax = b2len
        cmp     edx, eax
        jle     short :next1
    :gt
        mov     eax, 8
        jmp     short :done        
    :next1
        je      short :eq-length
    :lt
        mov     eax, -8
        jmp     short :done
    :eq-length
        mov     ecx, edx
    begin-atomic
        shr     ecx, 1
        sub     ecx, 4
        mov     ebx, [ebp + (+ ARGS_OFFSET 4)]  ;; ebx = b1
        mov     edi, [ebp + ARGS_OFFSET]        ;; edi = b2
    :loop
        mov     eax, [ebx + ecx + (uvector-offset 2)]
        cmp     eax, [edi + ecx + (uvector-offset 2)]
        ja      short :gt
        jb      short :lt
        sub     ecx, 4
        jge     short :loop
        
        xor     eax, eax
    :done
        mov     ecx, 1
    end-atomic
        pop     edi
        pop     ebx
        mov     esp, ebp
        pop     ebp
        ret
    })

(defun bignum-compare (b1 b2)
    (if (bignum-negative b1)
        (if (bignum-negative b2)
            (- 0 (bignum-abs-compare b1 b2))
            -1)
        (if (bignum-negative b2)
            1
            (bignum-abs-compare b1 b2))))

(defun sub-bignums (b1 b2)
    (let ((b1len (bignum-significant-length b1))
          (b2len (bignum-significant-length b2))
          (negative nil)
          result
          dest-size
          (b1sign (bignum-negative b1))
          (b2sign (bignum-negative b2)))
        (when (or (and b1sign (not b2sign)) (and (not b1sign) b2sign))
            (return-from sub-bignums (add-bignums b1 (bignum-negate b2))))
        (setq dest-size (max b1len b2len))
        (setq result (cl::alloc-bignum dest-size))
        (dotimes (i dest-size)
            (setf (uref result (+ i 2)) 0))     ;; clear all the cells    
        (if (>= (bignum-abs-compare b1 b2) 0)
            (progn
                (setf negative b1sign)
                (sub-bignum-words b1 b1len b2 b2len result))
            (progn
                (setf negative (not b1sign))
                (sub-bignum-words b2 b2len b1 b1len result)))
 
        (if negative
            (bignum-flip-sign result))
        (normalize-bignum result)))

(defasm mul-bignum-words (b1 b1len b2 b2len result)
    {
        push    ebp
        mov     ebp, esp
        push    ebx
        push    esi
        push    edi
    
        push    0                               ; [ebp - 16] = i
        push    0                               ; [ebp - 20] = carry
        push    0                               ; [ebp - 24] = overflow
        push    0                               ; [ebp - 28] = temp
    :loop0
        mov     eax, [ebp + (+ ARGS_OFFSET 4)]  ; eax = b2len
    begin-atomic
        shr     eax, 1                          ; eax = untagged(b2len) * 4
        mov     edi, [ebp - 16]                 ; edi = i
        cmp     edi, eax                        ; i < (b2len*4) ?
        jge     :done
        
        xor     eax, eax
		mov     [ebp - 20], eax					; carry = 0
		mov     [ebp - 24], eax					; overflow = 0
   		mov		edi, [ebp - 16]                 ; edi = i
		mov		edx, [ebp + (+ ARGS_OFFSET 8)]  ; edx = b2
		push	[edx + edi + (uvector-offset 2)]
        pop     [ebp - 28]                      ; temp = b2[i]
		mov		ecx, 0							; j = 0
		mov		ebx, [ebp + (+ ARGS_OFFSET 0)]  ; ebx = result    
		jmp		short :end_test
    
	:loop1
		mov		eax, [ebp + (+ ARGS_OFFSET 16)] ; eax = b1
		mov		eax, [eax + ecx + (uvector-offset 2)]; eax = b1[j]
		mul		[ebp - 28]                      ; multiply by temp
		add		eax, [ebp - 24]                 ; add overflow
		adc		edx, 0						    ; edx:eax = b1[j]*b2[i]+overflow
		mov		[ebp - 24], edx                 ; set overflow
		add		eax, [ebp - 20]				    ; add previous carry
        xor     edi, edi
		mov		[ebp - 20], edi                 ; clear carry
		jnc		short :t3
		inc		[ebp - 20]                      ; inc carry flag
	:t3 
        mov		edi, [ebp - 16]                 ; edi = i
		add		edi, ecx                        ; edi = i + j
		add		[ebx + edi + (uvector-offset 2)], eax    ; result[i+j] = eax
		jnc		short :t2
		inc		[ebp - 20]                      ; inc carry flag
	:t2
		add		ecx, 4
	:end_test
        mov     edx, [ebp + (+ ARGS_OFFSET 12)] ; edx = b1len
        shr     edx, 1                          ; edx = untagged(b1len) * 4
   		cmp		ecx, edx
		jl		short :loop1

        mov		edi, [ebp - 16]                 ; edi = i
   		add		edi, ecx                        ; edi = i + j
		mov		eax, [ebx + edi + (uvector-offset 2)]; result[i+j] = eax
		add		eax, [ebp - 24]                 ; overflow
		add		eax, [ebp - 20]                 ; carry
		mov		[ebx + edi + (uvector-offset 2)], eax
        mov     edi, [ebp - 16]                 ; edi = i
        add     edi, 4
        mov     [ebp - 16], edi                 ; i += 4    
        jmp     :loop0
    :done
        add     esp, 16                         ; clear temp vars
        pop     edi
        pop     esi
        pop     ebx
        mov     eax, [esi]
        mov     ecx, 1
    end-atomic
        mov     esp, ebp
        pop     ebp
        ret                     
    })

(defun multiply-bignums (b1 b2)
    (let ((b1len (bignum-significant-length b1))
          (b2len (bignum-significant-length b2))
          (negative nil)
          result
          dest-size
          (b1sign (bignum-negative b1))
          (b2sign (bignum-negative b2)))
        (unless (or (and (not b1sign) (not b2sign)) (and b1sign b2sign))
            (setq negative t))
        (setq dest-size (+ b1len b2len))
        (setq result (cl::alloc-bignum dest-size))
        (dotimes (i dest-size)
            (setf (uref result (+ i 2)) 0))     ;; clear all the cells    
        (if (< b1len b2len)
            (mul-bignum-words b2 b2len b1 b1len result)
            (mul-bignum-words b1 b1len b2 b2len result))
        (if negative
            (bignum-flip-sign result))
        (normalize-bignum result)))              

;; All four args should be single cell bignums.
;; The denom should already have been checked for zero.
;; This does not update the signs of the resulting quotient or remainder.
;; It returns the quotient.
(defasm divide-single-cell-bignums (num denom quotient remainder) 
    {
        push    ebp
        mov     ebp, esp
        push    ebx
        mov     ecx, [ebp + (+ ARGS_OFFSET 12)] ;; ecx = num
        mov     ebx, [ebp + (+ ARGS_OFFSET 8)]  ;; ebx = denom
    begin-atomic
        mov     eax, [ecx + (uvector-offset 2)] ;; eax = untagged 32-bit num
        xor     edx, edx                        ;; edx must be 0
        div     [ebx + (uvector-offset 2)]      ;; eax:edx = quotient/remainder
        mov     ecx, [ebp + (+ ARGS_OFFSET 0)]  ;; ecx = remainder
        mov     [ecx + (uvector-offset 2)], edx ;; store remainder
        mov     ecx, [ebp + (+ ARGS_OFFSET 4)]  ;; ecx = quotient
        mov     [ecx + (uvector-offset 2)], eax ;; store quotient
        xor     edx, edx
        xor     eax, eax
    end-atomic
        mov     eax, ecx
        mov     ecx, 1
        pop     ebx
        pop     ebp
        ret
    })

;;
;; Assume the passed bignum contains only one cell, this function
;; determines if the one cell has any of its upper 16 bits set.
;; If not, it can be used for optimized division algorithm--and this
;; will return the half-word as a (positive) fixnum in this case. Otherwise
;; this function returns NIL.
;;                    
(defasm bignum-fits-in-half-word (b)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + ARGS_OFFSET]
    begin-atomic
        mov     edx, #xffff
        shl     edx, 16
        and     edx, [eax + (uvector-offset 2)]
        jz      short :fits
        mov     eax, [esi]
        jmp     short :done
    :fits            
        mov     eax, [eax + (uvector-offset 2)]
        shl     eax, 3          ; wrap integer
    :done
    end-atomic
        pop     ebp
        ret
    })

;; returns the lower half of the cell at index
(defasm bignum-low-half-word (b index)
    {
        push    ebp
        mov     ebp, esp
        mov     edx, [ebp + (+ ARGS_OFFSET 4)]  ; edx = b
        mov     ecx, [ebp + (+ ARGS_OFFSET 0)]  ; ecx = index
        shr     ecx, 1
        xor     eax, eax
        mov     ax, [edx + ecx + (uvector-offset 2)]
        shl     eax, 3                          ; wrap integer
        mov     ecx, 1
        pop     ebp
        ret
    })

(defasm bignum-high-half-word (b index)
    {
        push    ebp
        mov     ebp, esp
        mov     edx, [ebp + (+ ARGS_OFFSET 4)]  ; edx = b
        mov     ecx, [ebp + (+ ARGS_OFFSET 0)]  ; ecx = index
        shr     ecx, 1
        xor     eax, eax
        mov     ax, [edx + ecx + (+ (uvector-offset 2) 2)]
        shl     eax, 3                          ; wrap integer
        mov     ecx, 1
        pop     ebp
        ret
    })

;;
;; Store the contents of a 32-bit bignum cell. We pass it as
;; two tagged integers representing the high 16 bits and the low 16 bits.
;;
(defasm bignum-set-half-words (b index high low)
    {
        push    ebp
        mov     ebp, esp
        push    ebx
        mov     eax, [ebp + (+ ARGS_OFFSET 12)] ; eax = b
        mov     ebx, [ebp + (+ ARGS_OFFSET 8)]  ; ebx = index 
    begin-atomic
        shr     ebx, 3
        mov     edx, [ebp + (+ ARGS_OFFSET 4)]  ; edx = high
        mov     ecx, [ebp + (+ ARGS_OFFSET 0)]  ; ecx = low
        shr     edx, 3
        mov     [eax + (+ (uvector-offset 2) 2) + ebx*4], dx
        shr     ecx, 3  
        mov     [eax + (+ (uvector-offset 2) 0) + ebx*4], cx 
        mov     ecx, 1
        pop     ebx
    end-atomic
        pop     ebp
        ret
    })

(defasm bignum-set-word (b index n)
    {
        push    ebp
        mov     ebp, esp
        push    ebx
        mov     eax, [ebp + (+ ARGS_OFFSET 8)]  ; eax = b
        mov     ecx, [ebp + (+ ARGS_OFFSET 4)]  ; ecx = index
        mov     edx, [ebp + (+ ARGS_OFFSET 0)]  ; edx = n
        shr     ecx, 3
        test    dl, 7
        jnz     short :bignum
    :fixnum
    begin-atomic
        shr     edx, 3
        mov     [eax + ecx * 4 + (uvector-offset 2)], edx
        xor     edx, edx
    end-atomic
        jmp     short :done
    :bignum
    begin-atomic
        mov     ebx, [edx + ecx * 4 + (uvector-offset 2)]
        mov     [eax + ecx * 4 + (uvector-offset 2)], ebx
        xor     ebx, ebx
    end-atomic
    :done
        mov     ecx, 1
        pop     ebx
        pop     ebp
        ret
    })

;; Compare two cells of bignums--returns -1, 0 or 1
(defasm bignum-cell-compare (b1 i1 b2 i2)
    {
        push    ebp
        mov     ebp, esp
        push    ebx
        push    edi
        mov     eax, [ebp + (+ ARGS_OFFSET 12)] ; eax = b1
        mov     edi, [ebp + (+ ARGS_OFFSET 4)]  ; edi = b2
    begin-atomic
        mov     ecx, [ebp + (+ ARGS_OFFSET 8)]
        shr     ecx, 3                          ; ecx = untagged i1    
        mov     edx, [ebp + (+ ARGS_OFFSET 0)]
        shr     edx, 3                          ; edx = untagged i2
        mov     ebx, [eax + ecx * 4 + (uvector-offset 2)]   ; ebx = untagged cell-1
        cmp     ebx, [edi + edx * 4 + (uvector-offset 2)]
        jz      short :equal
        jb      short :less
        mov     eax, 8
        jmp     short :done
    :equal
        mov     eax, 0
        jmp     short :done
    :less
        mov     eax, -8
    :done
        mov     ecx, 1
        pop     edi
        pop     ebx
        xor     edx, edx
    end-atomic
        pop     ebp
        ret
    })
        
;; Given an index to a bignum cell which has at least one bit set, 
;; return (32 - <the index of the highest bit>).
;; If the cell = #xf0000000, returns 0.
;; If the cell = #x00000001, returns 31
;;
(defasm calc-shift-bits (b index)
    {
        push    ebp
        mov     ebp, esp
        mov     ecx, [ebp + (+ ARGS_OFFSET 4)]  ; ecx = b
        mov     edx, [ebp + (+ ARGS_OFFSET 0)]  ; edx = index
    begin-atomic
        shr     edx, 3
        mov     edx, [ecx + edx * 4 + (uvector-offset 2)]
        xor     eax, eax
    :loop
        test    edx, edx
        jl      short :done
        shl     edx, 1
        inc     eax
        jmp     short :loop
    :done
        shl     eax, 3
        xor     edx, edx
    end-atomic
        mov     ecx, 1
        pop     ebp
        ret
    })


(defasm shift-bignum-words-left (src src-len dest bit-shift word-shift)
    {
        push    ebp
        mov     ebp, esp
        push    esi
        push    edi
        push    ebx
        mov     esi, [ebp + (+ ARGS_OFFSET 16)] ; esi = src
        mov     edi, [ebp + (+ ARGS_OFFSET 8)]  ; edi = dest
        mov     ecx, [ebp + (+ ARGS_OFFSET 4)]  ; ecx = bit-shift
        mov     edx, [ebp + (+ ARGS_OFFSET 0)]  ; edx = word-shift
    begin-atomic
        shr     ecx, 3                          ; untagged int bit-shift
        shr     edx, 2                          ; untagged word-shift * 2
        add     edi, edx                        ; offset dest by word-shift * 2        
        mov     edx, [ebp + (+ ARGS_OFFSET 12)] 
        shr     edx, 2                          ; edx = untagged src-len * 2
    
        xor     ebx, ebx                        ; i = 0
    :loop
        cmp     ebx, edx        ;; i = src-len * 2?
        jge     short :done
        xor     eax, eax
        mov     ax, [esi + ebx * 2 + (uvector-offset 2)]
        shl     eax, cl
        add     [edi + ebx * 2 + (uvector-offset 2)], eax
        inc     ebx
        jmp     short :loop
    :done
        xor     eax, eax
        mov     ecx, 1
        pop     ebx
        pop     edi
        pop     esi
    end-atomic
        pop     ebp
        ret
    })    
     
(defasm shift-bignum-words-right (src src-len dest bit-shift word-shift)
    {
        push    ebp
        mov     ebp, esp
        push    esi
        push    edi
        push    ebx
        push    0                               ; [ebp - 16]
        push    0                               ; [ebp - 20]
        mov     esi, [ebp + (+ ARGS_OFFSET 16)] ; esi = src
        mov     edi, [ebp + (+ ARGS_OFFSET 8)]  ; edi = dest
        mov     ecx, [ebp + (+ ARGS_OFFSET 4)]  ; ecx = bit-shift
    begin-atomic
        shr     ecx, 3                          ; untagged int bit-shift
        mov     edx, [ebp + (+ ARGS_OFFSET 12)] 
        shr     edx, 2                          ; edx = untagged src-len * 2
        mov     [ebp - 16], edx                 ; [ebp - 16] = untagged src-len * 2
        xor     ebx, ebx                        ; i = 0
        
        mov     edx, [ebp + (+ ARGS_OFFSET 0)]  ; edx = word-shift
        test    edx, edx
        jnz     short :next1
        xor     eax, eax
        mov     ax, [esi + (uvector-offset 2)]
        shr     eax, cl 
        mov     bx, [edi + (uvector-offset 2)]       
        add     eax, ebx
        mov     [edi + (uvector-offset 2)], ax
    :next1     
        mov     ebx, 1                          ; i = 1
        mov     edx, [ebp + (+ ARGS_OFFSET 0)]  ; edx = word-shift
        shr     edx, 3
        inc     edx
        mov     [ebp - 20], edx                 ; [ebp - 20] = untagged word-shift + 1
        shl     edx, 1                          ; untagged (word-shift + 1) * 2
        sub     edi, edx                        ; offset dest by (negative) (word-shift + 1) * 2
    :loop
        cmp     ebx, [ebp - 16]                 ; i = src-len * 2?
        jge     short :done
        xor     eax, eax
        mov     ax, [esi + ebx * 2 + (uvector-offset 2)]
        shl     eax, 16
        shr     eax, cl    
        cmp     ebx, [ebp - 20]                 ; i >= (word-shift + 1)
        jl      short :next2
        add     [edi + ebx * 2 + (uvector-offset 2)], eax
        jmp     short :next-loop
    :next2
        mov     edx, [ebp + (+ ARGS_OFFSET 0)] 
        shr     edx, 3                          ; edx = untagged word-shift
        cmp     ebx, edx                        ; i = word-shift?     
        jne     short :next-loop
        xor     edx, edx
        mov     dx, [edi + ebx * 2 + (+ 2 (uvector-offset 2))] 
        shr     eax, 16      
        add     eax, edx
        mov     [edi + ebx * 2 + (+ 2 (uvector-offset 2))], ax           
    :next-loop
        inc     ebx
        jmp     short :loop
    :done
        xor     eax, eax
        xor     edx, edx
        mov     ecx, 1
        add     esp, 8
        pop     ebx
        pop     edi
        pop     esi
    end-atomic
        pop     ebp
        ret
    })
        
(defun shift-bignum-words (b src-len res result-len bits)
    (let (shift-dir
          word-shift
          bit-shift)
        
        ;; make sure destination bits are cleared
        (dotimes (i result-len)
            (setf (uref res (+ i 2)) 0))
        
        (if (< bits 0)
            (setq shift-dir 'right bits (- bits))
            (setq shift-dir 'left))
        
        (multiple-value-setq (word-shift bit-shift)
            (truncate bits 16))
        
        (if (eq shift-dir 'left)
            (shift-bignum-words-left b src-len res bit-shift word-shift)
            (shift-bignum-words-right b src-len res bit-shift word-shift)))) 
                           
(defun bignum-shift (b shift signed-shift)
    (let* ((word-length (bignum-significant-length b))
           (bit-length (* word-length 32))
           (new-length (max (+ bit-length shift) 1))
           (new-word-length (truncate (+ new-length 31) 32))
           (negative (bignum-negative b))
           (result (cl::alloc-bignum new-word-length)))
        (shift-bignum-words b word-length result new-word-length shift)
        (when (and negative signed-shift)
            (setq result (add-bignums result (fixnum-to-bignum 1)))
            (if (bignump result)
                (progn
                    (bignum-flip-sign result)
                    (return-from bignum-shift (normalize-bignum result)))
                (return-from bignum-shift (subtract-fixnums 0 result))))
        (normalize-bignum result)))

(defun bignum-shift-signed (b shift)
    (bignum-shift b shift t))

#|
;; bignum-shift test function
(defun test (max negative)
    (let ((b1 #x123456789a012345))
        (dotimes (i max)
            (let ((shift (if negative (- i) i)))
                (if (/= (bignum-shift b1 shift nil) (ash b1 shift))
                    (format t "!!!!Invalid result: b=~A, shift=~A, result=~A~%" 
                        b1 shift (bignum-shift b1 shift nil))
                    (format t "Valid result: b=~A, shift=~A, result=~A~%"  
                        b1 shift (bignum-shift b1 shift nil)))))))
|#
        
(defun divide-bignums (b1 b2)
    (bignum-divide b1 b2 nil))

(defconstant uint-max-bignum #xffffffff)

;;
;; Double-digit value (A, B) is divided by d and stored in q.
;; Each consists of a bignum and an index.
;;
(defasm dd-quotient (a ai b bi d di q qi)
    {
        push    ebp
        mov     ebp, esp
        push    ebx
        mov     ebx, [ebp + (+ ARGS_OFFSET 28)] ; ebx = a
        mov     ecx, [ebp + (+ ARGS_OFFSET 24)] ; ecx = ai
    begin-atomic
        shr     ecx, 1                          ; ecx = untagged ai * 4
        mov     edx, [ebx + ecx + (uvector-offset 2)]   ; edx = upper 32-bits
        mov     ebx, [ebp + (+ ARGS_OFFSET 20)] ; ebx = b
        mov     ecx, [ebp + (+ ARGS_OFFSET 16)] ; ecx = bi
        shr     ecx, 1                          ; ecx = untagged bi * 4
        mov     eax, [ebx + ecx + (uvector-offset 2)]   ; eax = lower 32-bits
        mov     ebx, [ebp + (+ ARGS_OFFSET 12)] ; ebx = d
        mov     ecx, [ebp + (+ ARGS_OFFSET 8)]  ; ecx = di 
        shr     ecx, 1                          ; ecx = untagged di * 4
        div     [ebx + ecx + (uvector-offset 2)]; divide by d, eax = quotient (32-bits)
        mov     ebx, [ebp + (+ ARGS_OFFSET 4)]  ; ebx = q
        mov     ecx, [ebp + (+ ARGS_OFFSET 0)]  ; ecx = qi
        shr     ecx, 1
        mov     [ebx + ecx + (uvector-offset 2)], eax
        mov     edx, 0
        mov     eax, 0          ; return 0
        mov     ecx, 1
    end-atomic
        pop     ebx
        pop     ebp
        ret
    })        
       
;; Subtract multiple q * b from a, where a and b
;; are values of n digits. The remainder a - q * b
;; will be less than b and must not be negative.
;; The latter condition may require q to be
;; decreased by 1:

(defasm subtract-mul (a ai b bi n q)
    {
        push    ebp
        mov     ebp, esp
        push    ebx
        push    edi
        push    esi
    
        mov     ebx, [ebp + (+ ARGS_OFFSET 20)] ; ebx = a
        mov     ecx, [ebp + (+ ARGS_OFFSET 16)] ; ecx = ai
    begin-atomic
        shr     ecx, 1                          ; ecx = untagged ai * 4
        add     ebx, ecx
        mov     edi, [ebp + (+ ARGS_OFFSET 12)] ; edi = b
        mov     ecx, [ebp + (+ ARGS_OFFSET 8)]  ; ecx = bi
        shr     ecx, 1                          ; ecx = untagged bi * 4
        add     edi, ecx
        mov     ecx, 0                          ; ecx = i = 0
        mov     esi, 0                          ; esi = carry = 0
    :loop1
        mov     edx, [ebp + (+ ARGS_OFFSET 4)]  ; edx = n
        shr     edx, 3                          ; edx = untagged n
        cmp     ecx, edx                        ; i = n?
        je      short :next1
        mov     eax, [edi + ecx*4 + (uvector-offset 2)] ; eax = b[i]
        mov     edx, [ebp + (+ ARGS_OFFSET 0)]  ; edx = q 
        mul     [edx + (uvector-offset 2)]      ; edx:eax = b[i]*q
        sub     [ebx + ecx*4 + (uvector-offset 2)], eax ; a[i] -= Lo
        adc     esi, 0                          ; if (a[i] > d) carry++;
        add     edx, esi                        ; edx = Hi + carry
        mov     esi, 0                          ; carry = 0
        sub     [ebx + ecx*4 + (+ 4 (uvector-offset 2))], edx  ; a[i + 1] -= Hi + carry;
        adc     esi, 0                          ; carry = (a[i + 1] > d);
        inc     ecx
        jmp     short :loop1
    
    :next1
        cmp     esi, 0
        je      short :done
        mov     eax, [ebp + (+ ARGS_OFFSET 0)]  ; eax = q
        mov     edx, [eax + (uvector-offset 2)] ; edx = q[0]
        dec     edx
        mov     [eax + (uvector-offset 2)], edx ; q[0]--
        mov     esi, 0                          ; carry = 0
        mov     ecx, 0                          ; i = 0
    :loop2
        mov     edx, [ebp + (+ ARGS_OFFSET 4)]  ; ecx = n
        shr     edx, 3                          ; ecx = untagged n
        cmp     ecx, edx                        ; i = n?        
        je      short :loop2-done
        mov     edx, [ebx + ecx*4 + (uvector-offset 2)] ; edx = a[i]
        mov     eax, esi                        ; eax = carry
        mov     esi, 0                              
        add     edx, eax                        ; edx = a[i] + carry          
        cmp     edx, eax                        ; d < carry ?
        jae     short :next2
        inc     esi                             ; carry = d < carry;
    :next2
        add     edx, [edi + ecx*4 + (uvector-offset 2)] ; edx = d + b[i]
        jnc     short :next3
        mov     esi, 1                          ; if (a[i] < d) carry = 1;
    :next3
        mov     [ebx + ecx*4 + (uvector-offset 2)], edx ; a[i] = d + b[i]
        inc     ecx
        jmp     short :loop2
    :loop2-done
        xor     eax, eax
        mov     [ebx + ecx*4 + (uvector-offset 2)], eax ; a[n] = 0;
    :done
        xor     eax, eax
        xor     edx, edx
        mov     ecx, 1
        pop     esi
        pop     edi
        pop     ebx
    end-atomic
        pop     ebp
        ret
    })
               
(defun bignum-divide (num denom include-rem)
    (bignum-reduce num)
    (bignum-reduce denom)
    (let* ((rem-neg (bignum-negative num))
          (denom-neg (bignum-negative denom))
          (quot-neg (not (eq rem-neg denom-neg)))
          (num-len (bignum-length num))
          (denom-len (bignum-length denom))
          quotient
          remainder
          half-word-denom
          (dhi 0)
          r
          x
          temp
          second-done
          n
          Lq
          q)
        
        ;; error on division by zero
        (if (eq denom-len 0)
            (cl::signal-division-by-zero (list num denom)))
        
        ;; bail early if num < denom
        (if (< (bignum-abs-compare num denom) 0)
            (return-from bignum-divide
                (if include-rem
                    (cons 0 (normalize-bignum num))
                    0)))
        
        ;; optimize for case where both bignums fit in 32 bits
        (when (and (= num-len 1) (= denom-len 1))
            (setq quotient (cl::alloc-bignum 1))
            (setq remainder (cl::alloc-bignum 1))
            (divide-single-cell-bignums num denom quotient remainder)
            (bignum-set-sign quotient (if quot-neg 1 0))
            (if include-rem
                (progn
                    (bignum-set-sign remainder (if rem-neg 1 0))
                    (return-from bignum-divide
                        (cons (normalize-bignum quotient)
                              (normalize-bignum remainder))))
                (return-from bignum-divide (normalize-bignum quotient))))
        
        ;; Optimize for case where denominator fits into a half word.
        ;; This is much more efficient than the following general-case algorithm.
        (setq half-word-denom (bignum-fits-in-half-word denom))
        (when (and (eq denom-len 1) half-word-denom)
            (setq quotient (cl::alloc-bignum num-len))
            (setq remainder (cl::alloc-bignum 1))
            (do ((i (- num-len 1) (- i 1))
                 dividend
                 q1
                 r
                 q2)
                ((< i 0))
                (setq dividend (logior (ash dhi 16) (bignum-high-half-word num i)))
                (multiple-value-setq (q1 r) (truncate dividend half-word-denom))
                (setq dividend (logior (ash r 16) (bignum-low-half-word num i)))
                (multiple-value-setq (q2 dhi) (truncate dividend half-word-denom))
                (bignum-set-half-words quotient i q1 q2))
            (bignum-reduce quotient)
            (bignum-set-sign quotient (if quot-neg 1 0))
            (bignum-set-word remainder 0 dhi)
            (bignum-set-sign remainder (if rem-neg 1 0))
            (if include-rem
                (progn
                    (bignum-set-sign remainder (if rem-neg 1 0))
                    (return-from bignum-divide
                        (cons (normalize-bignum quotient)
                              (normalize-bignum remainder))))
                (return-from bignum-divide (normalize-bignum quotient))))
        
        (setq r (- denom-len 1))
        (setq x (calc-shift-bits denom r))
        (setq denom (bignum-shift denom x nil))
        (setq num (bignum-shift num x nil))
        
        ;; Possibly second action according to C. J. Mifsud
        (if (and (> r 0) (= (bignum-cell-compare denom r denom (- r 1)) -1))
            (progn
                (setq num-len (bignum-length num))
                (setq temp (cl::alloc-bignum (+ num-len 1)))
                (mul-bignum-words num num-len uint-max-bignum 1 temp)
                (setq num (normalize-bignum temp))
                (setq denom-len (bignum-length denom))
                (setq temp (cl::alloc-bignum (+ denom-len 1)))
                (mul-bignum-words denom denom-len uint-max-bignum 1 temp)
                (setq denom (normalize-bignum temp))
                (setq second-done t))
            (setq second-done nil))
        
        (setq r (- (bignum-length denom) 1))
        (setq n (- (bignum-length num) 1))                        
        (setq Lq (- n r))
        (if (>= (bignum-cell-compare num n denom r) 0)
            (progn
                (setq remainder (cl::alloc-bignum (+ n 2)))
                (incf n)
                (setq quotient (cl::alloc-bignum (+ Lq 1))))
            (progn
                (setq remainder (cl::alloc-bignum (+ n 1)))
                (setq quotient (cl::alloc-bignum Lq))))
        (copy-bignum num remainder)
        (setq q (cl::alloc-bignum 1))
        
        (do ((k n (- k 1)))
            ((<= k r))
            (dd-quotient remainder k remainder (- k 1) denom r q 0)
            (subtract-mul remainder (- k (+ r 1)) denom 0 (+ r 1) q)
            (copy-bignum-cells q 0 quotient (- k (+ r 1)) 1))
        
        (bignum-reduce quotient)
        (bignum-set-sign quotient (if quot-neg 1 0))
        
        (if include-rem
            (progn
                (if second-done
                    (setq remainder (bignum-divide remainder uint-max-bignum nil)))
                (if (> x 0)
                    (setq remainder (ash remainder (- x))) 
                    (if (bignump remainder)
                        (bignum-reduce remainder)))
                (if (bignump remainder)
                    (progn
                        (bignum-set-sign remainder (if rem-neg 1 0))
                        (setq remainder (normalize-bignum remainder)))
                    (if rem-neg
                        (setq remainder (- remainder))))
                (return-from bignum-divide (cons (normalize-bignum quotient) remainder))))
        (normalize-bignum quotient)))          

(defun divide-bignums (b1 b2)
    (bignum-divide b1 b2 nil))

(defun mod-bignums (b1 b2)
    (cdr (bignum-divide b1 b2 t)))

(defasm abs-bignum (n)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + (+ ARGS_OFFSET 0)]  ; eax = n
        xor     edx, edx
        mov     dl, [eax + (uvector-offset 1)]
        test    dl, 8
        je      short :done
        push    eax
        mov     ecx, 1
        callp   copy-uvector
        add     esp, 4
        mov     dl, [eax + (uvector-offset 1)]
        and     dl, #xf0
        mov     [eax + (uvector-offset 1)], dl
    :done
        pop     ebp
        ret
    })

;;
;; BIGNUM-SM-TO-2C  converts (in place) a signed magnitude bignum to
;; a twos complement representation. Note that the sign bit is not
;; modified. It is assumed that the top-most bit of the signed magnitude
;; is free to hold the sign bit in the twos complement representation.
;;
(defasm bignum-sm-to-2c (bn)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + ARGS_OFFSET]
        mov     ecx, [eax + (uvector-offset 1)]
        test    ecx, 8
        je      short :done     ; positive number--nothing to do
        shr     ecx, 4          ; ecx = num cells
        mov     edx, 0          ; i = 0
        
        ;; flip all the bits
    :loop1
        not     [eax + edx*4 + (uvector-offset 2)]
        inc     edx             ; i++
        cmp     edx, ecx        ; i < len?
        jl      short :loop1
    
        ;; add one to the result
        mov     edx, 0          ; i = 0
    :loop2
        inc     [eax + edx*4 + (uvector-offset 2)]
        jne     short :done     ; no carry, exit
        inc     edx             ; i++
        jmp     short :loop2
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })      
        
;;
;; BIGNUM-2C-TO-SM converts (in place) a twos complement bignum
;;	to a signed magnitude representation.
;;
(defasm bignum-2c-to-sm (bn)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + ARGS_OFFSET]
        mov     ecx, [eax + (uvector-offset 1)]
        shr     ecx, 4          ; ecx = num cells
        mov     edx, 1
        ror     edx, 1          ; set high bit of edx
        test    edx, [eax + ecx*4 + (uvector-offset 1)]
        je      short :positive
        mov     edx, 0          ; i = 0
        
        ;; flip all the bits
    :loop1
        not     [eax + edx*4 + (uvector-offset 2)]
        inc     edx             ; i++
        cmp     edx, ecx        ; i < len?
        jl      short :loop1
    
        ;; add one to the result
        mov     edx, 0          ; i = 0
    :loop2
        inc     [eax + edx*4 + (uvector-offset 2)]
        jne     short :negative ; no carry, exit
        inc     edx             ; i++
        jmp     short :loop2
    :negative
        shl     ecx, 4
        or      ecx, 8          ; set sign bit
        jmp     short :done
    :positive
        shl     ecx, 4
    :done
        mov     [eax + (uvector-offset 1)], ecx
        mov     ecx, 1
        pop     ebp
        ret
    })      

;;;
;;;	xorBignumWords()
;;;	Assumes result length >= max(b1-len, b2-len)
;;;	Assumes both bignums are in signed magnitude format.
;;;
(defasm xor-bignum-words (b1 b1-len b2 b2-len result)
    {
        push    ebp
        mov     ebp, esp
        push    ebx
        push    edi
        push    esi
        push    1                             ;; carry1 = [ebp - 16]
        push    1                             ;; carry2 = [ebp - 20]
     
        mov     ecx, [ebp + (+ ARGS_OFFSET 12)]     ;; ecx = b1-len
        cmp     ecx, [ebp + (+ ARGS_OFFSET 4)]     ;; b1-len < b2-len?
        jge     short :next1    ;; ensure b1-len >= b2-len
        ;; swap b1, b2
        mov     eax, [ebp + (+ ARGS_OFFSET 4)]
        mov     [ebp + (+ ARGS_OFFSET 12)], eax
        mov     [ebp + (+ ARGS_OFFSET 4)], ecx
        mov     ecx, [ebp + (+ ARGS_OFFSET 16)]     ;; ecx = b1          
        mov     eax, [ebp + (+ ARGS_OFFSET 8)]      ;; eax = b2
        mov     [ebp + (+ ARGS_OFFSET 8)], ecx
        mov     [ebp + (+ ARGS_OFFSET 16)], eax
    :next1
        mov     edx, [ebp + (+ ARGS_OFFSET 16)]     ;; edx = b1, b1-len > b2-len
        mov     ebx, [ebp + (+ ARGS_OFFSET 8)]      ;; ebx = b2
        mov     eax, [edx + (uvector-offset 1)]
        test    eax, 8                              ;; b1 negative?
        jne     :b1-negative
        ;; b1 is positive
        mov     eax, [ebx + (uvector-offset 1)]
        test    eax, 8
        jne     short :b1-pos-b2-neg
        ;; b1 >=0, b2 >= 0
    begin-atomic
        mov     ecx, 0                              ;; i = 0
        mov     esi, [ebx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b2-len
        mov     edi, [ebp + (+ ARGS_OFFSET 0)]      ;; edi = result
    :loop1
        cmp     ecx, esi
        jge     short :next2
        mov     eax, [edx + ecx*4 + (uvector-offset 2)]
        xor     eax, [ebx + ecx*4 + (uvector-offset 2)]
        mov     [edi + ecx*4 + (uvector-offset 2)], eax
        inc     ecx
        jmp     short :loop1
    :next2
        mov     esi, [edx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b1-len
    :loop2  
        cmp     ecx, esi
        jge     short :next3
        mov     eax, [edx + ecx*4 + (uvector-offset 2)]
        mov     [edi + ecx*4 + (uvector-offset 2)], eax 
        inc     ecx
        jmp     short :loop2                      
    :next3
        add     esp, 8     ;; pop carry1, carry2
        pop     esi
        xor     eax, eax
        xor     edx, edx
    end-atomic
        jmp     :done
    
    :b1-pos-b2-neg          ;; b1 >= 0, b2 < 0
         ;; b1 >=0, b2 >= 0
    begin-atomic
        mov     ecx, 0                              ;; i = 0
        mov     esi, [ebx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b2-len
        mov     edi, [ebp + (+ ARGS_OFFSET 0)]      ;; edi = result
    :loop3
        cmp     ecx, esi
        jge     short :next5
        mov     eax, [ebx + ecx*4 + (uvector-offset 2)] ;; eax = b2[i]
        not     eax
        push    edx
        mov     edx, 0
        add     eax, [ebp - 16]                      ;; eax = ~b2[i] + carry1
        jne     short :next4
        inc     edx
    :next4
        and     [ebp - 16], edx                      ;; carry1 &&= (!tcword)
        pop     edx
        xor     eax, [edx + ecx*4 + (uvector-offset 2)]
        mov     [edi + ecx*4 + (uvector-offset 2)], eax
        inc     ecx
        jmp     short :loop3
    :next5
        mov     esi, [edx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b1-len
    :loop4  
        cmp     ecx, esi
        jge     short :next6
        mov     eax, [edx + ecx*4 + (uvector-offset 2)]
        not     eax
        mov     [edi + ecx*4 + (uvector-offset 2)], eax 
        inc     ecx
        jmp     short :loop4                      
    :next6
        add     esp, 8     ;; pop carry1, carry2
        pop     esi
        xor     eax, eax
        xor     edx, edx
    end-atomic
        push    edi
        callp   bignum-2c-to-sm
        add     esp, 4
        jmp     :done
    
    :b1-negative 
        mov     eax, [ebx + (uvector-offset 1)]
        test    eax, 8
        jne     short :b1-neg-b2-neg    
        ;; b1 < 0, b2 >= 0 
    begin-atomic
        mov     ecx, 0                              ;; i = 0
        mov     esi, [ebx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b2-len
        mov     edi, [ebp + (+ ARGS_OFFSET 0)]      ;; edi = result
    :loop5
        cmp     ecx, esi
        jge     short :next8
        mov     eax, [edx + ecx*4 + (uvector-offset 2)] ;; eax = b1[i]
        not     eax
        push    edx
        mov     edx, 0
        add     eax, [ebp - 16]                      ;; eax = ~b1[i] + carry1
        jne     short :next7
        inc     edx
    :next7
        and     [ebp - 16], edx                      ;; carry1 &&= (!tcword)
        pop     edx
        xor     eax, [ebx + ecx*4 + (uvector-offset 2)]
        mov     [edi + ecx*4 + (uvector-offset 2)], eax
        inc     ecx
        jmp     short :loop5
    :next8
        mov     esi, [edx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b1-len
    :loop6  
        cmp     ecx, esi
        jge     short :next10
        mov     eax, [edx + ecx*4 + (uvector-offset 2)]
        not     eax
        push    edx
        mov     edx, 0
        add     eax, [ebp - 16]                      ;; eax = ~b1[i] + carry1
        jne     short :next9
        inc     edx
    :next9
        and     [ebp - 16], edx                      ;; carry1 &&= (!tcword)
        pop     edx
        mov     [edi + ecx*4 + (uvector-offset 2)], eax 
        inc     ecx
        jmp     short :loop6                      
    :next10
        add     esp, 8     ;; pop carry1, carry2
        pop     esi
        xor     eax, eax
        xor     edx, edx
    end-atomic
        push    edi
        callp   bignum-2c-to-sm
        add     esp, 4
        jmp     :done
    :b1-neg-b2-neg     
        ;; b1 < 0, b2 < 0 
    begin-atomic
        mov     ecx, 0                              ;; i = 0
        mov     esi, [ebx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b2-len
        mov     edi, [ebp + (+ ARGS_OFFSET 0)]      ;; edi = result
    :loop7
        cmp     ecx, esi
        jge     short :next13
        mov     eax, [edx + ecx*4 + (uvector-offset 2)] ;; eax = b2[i]
        not     eax
        push    edx
        mov     edx, 0
        add     eax, [ebp - 16]                      ;; eax = ~b1[i] + carry1
        jne     short :next11
        inc     edx
    :next11
        and     [ebp - 16], edx                      ;; carry1 &&= (!tcword)
        pop     edx
        push    eax                                  ;; save temp result
        mov     eax, [ebx + ecx*4 + (uvector-offset 2)]
        not     eax
        push    edx
        mov     edx, 0
        add     eax, [ebp - 20]                      ;; eax = ~b2[i] + carry2
        jne     short :next12
        inc     edx
    :next12
        and     [ebp - 20], edx                      ;; carry2 &&= (!tcword)
        pop     edx
        xor     eax, [esp + 0]
        add     esp, 4
        mov     [edi + ecx*4 + (uvector-offset 2)], eax
        inc     ecx
        jmp     short :loop7
    :next13
        mov     esi, [edx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b1-len
    :loop8  
        cmp     ecx, esi
        jge     short :next15
        mov     eax, [edx + ecx*4 + (uvector-offset 2)]
        not     eax
        push    edx
        mov     edx, 0
        add     eax, [ebp - 16]                      ;; eax = ~b1[i] + carry1
        jne     short :next14
        inc     edx
    :next14
        and     [ebp - 16], edx                      ;; carry1 &&= (!tcword)
        pop     edx
        not     eax
        mov     [edi + ecx*4 + (uvector-offset 2)], eax 
        inc     ecx
        jmp     short :loop8                     
    :next15
        add     esp, 8     ;; pop carry1, carry2
        pop     esi
        xor     eax, eax
        xor     edx, edx    
    end-atomic
        push    edi
        callp   bignum-2c-to-sm
        add     esp, 4
    :done
        pop     edi
        pop     ebx
        pop     ebp
        ret
    })

(defun xor-bignums (b1 b2)
    (unless (bignump b1)
        (cl::signal-type-error b1 'integer)) 
    (unless (bignump b2)
        (cl::signal-type-error b2 'integer)) 
    (let* ((b1-len (bignum-significant-length b1))
           (b2-len (bignum-significant-length b2))
           (dest-size (if (< b1-len b2-len) b2-len b1-len))
           (result (cl::alloc-bignum dest-size)))
        (xor-bignum-words b1 b1-len b2 b2-len result)
        (normalize-bignum result)))

;;;
;;;	OR-BIGNUM-WORDS
;;;	Assumes result length >= max(b1-len, b2-len)
;;;	Assumes both bignums are in signed magnitude format.
;;;
(defasm or-bignum-words (b1 b1-len b2 b2-len result)
    {
        push    ebp
        mov     ebp, esp
        push    ebx
        push    edi
        push    esi
        push    1                             ;; carry1 = [ebp - 16]
        push    1                             ;; carry2 = [ebp - 20]
     
        mov     ecx, [ebp + (+ ARGS_OFFSET 12)]     ;; ecx = b1-len
        cmp     ecx, [ebp + (+ ARGS_OFFSET 4)]     ;; b1-len < b2-len?
        jge     short :next1    ;; ensure b1-len >= b2-len
        ;; swap b1, b2
        mov     eax, [ebp + (+ ARGS_OFFSET 4)]
        mov     [ebp + (+ ARGS_OFFSET 12)], eax
        mov     [ebp + (+ ARGS_OFFSET 4)], ecx
        mov     ecx, [ebp + (+ ARGS_OFFSET 16)]     ;; ecx = b1          
        mov     eax, [ebp + (+ ARGS_OFFSET 8)]      ;; eax = b2
        mov     [ebp + (+ ARGS_OFFSET 8)], ecx
        mov     [ebp + (+ ARGS_OFFSET 16)], eax
    :next1
        mov     edx, [ebp + (+ ARGS_OFFSET 16)]     ;; edx = b1, b1-len > b2-len
        mov     ebx, [ebp + (+ ARGS_OFFSET 8)]      ;; ebx = b2
        mov     eax, [edx + (uvector-offset 1)]
        test    eax, 8                              ;; b1 negative?
        jne     :b1-negative
        ;; b1 is positive
        mov     eax, [ebx + (uvector-offset 1)]
        test    eax, 8
        jne     short :b1-pos-b2-neg
        ;; b1 >=0, b2 >= 0
    begin-atomic
        mov     ecx, 0                              ;; i = 0
        mov     esi, [ebx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b2-len
        mov     edi, [ebp + (+ ARGS_OFFSET 0)]      ;; edi = result
    :loop1
        cmp     ecx, esi
        jge     short :next2
        mov     eax, [edx + ecx*4 + (uvector-offset 2)]
        or      eax, [ebx + ecx*4 + (uvector-offset 2)]
        mov     [edi + ecx*4 + (uvector-offset 2)], eax
        inc     ecx
        jmp     short :loop1
    :next2
        mov     esi, [edx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b1-len
    :loop2  
        cmp     ecx, esi
        jge     short :next3
        mov     eax, [edx + ecx*4 + (uvector-offset 2)]
        mov     [edi + ecx*4 + (uvector-offset 2)], eax 
        inc     ecx
        jmp     short :loop2                      
    :next3
        add     esp, 8     ;; pop carry1, carry2
        pop     esi
        xor     eax, eax
    end-atomic
        jmp     :done
    
    :b1-pos-b2-neg          ;; b1 >= 0, b2 < 0
         ;; b1 >=0, b2 >= 0
    begin-atomic
        mov     ecx, 0                              ;; i = 0
        mov     esi, [ebx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b2-len
        mov     edi, [ebp + (+ ARGS_OFFSET 0)]      ;; edi = result
    :loop3
        cmp     ecx, esi
        jge     short :next5
        mov     eax, [ebx + ecx*4 + (uvector-offset 2)] ;; eax = b2[i]
        not     eax
        push    edx
        mov     edx, 0
        add     eax, [ebp - 16]                      ;; eax = ~b2[i] + carry1
        jne     short :next4
        inc     edx
    :next4
        and     [ebp - 16], edx                      ;; carry1 &&= (!tcword)
        pop     edx
        or      eax, [edx + ecx*4 + (uvector-offset 2)]
        mov     [edi + ecx*4 + (uvector-offset 2)], eax
        inc     ecx
        jmp     short :loop3
    :next5
        mov     esi, [edx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b1-len
    :loop4  
        cmp     ecx, esi
        jge     short :next6
        mov     eax, 0
        not     eax
        mov     [edi + ecx*4 + (uvector-offset 2)], eax 
        inc     ecx
        jmp     short :loop4                      
    :next6
        add     esp, 8     ;; pop carry1, carry2
        pop     esi
        xor     eax, eax
        xor     edx, edx
    end-atomic
        push    edi
        callp   bignum-2c-to-sm
        add     esp, 4
        jmp     :done
    
    :b1-negative 
        mov     eax, [ebx + (uvector-offset 1)]
        test    eax, 8
        jne     short :b1-neg-b2-neg    
        ;; b1 < 0, b2 >= 0 
    begin-atomic
        mov     ecx, 0                              ;; i = 0
        mov     esi, [ebx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b2-len
        mov     edi, [ebp + (+ ARGS_OFFSET 0)]      ;; edi = result
    :loop5
        cmp     ecx, esi
        jge     short :next8
        mov     eax, [edx + ecx*4 + (uvector-offset 2)] ;; eax = b1[i]
        not     eax
        push    edx
        mov     edx, 0
        add     eax, [ebp - 16]                      ;; eax = ~b1[i] + carry1
        jne     short :next7
        inc     edx
    :next7
        and     [ebp - 16], edx                      ;; carry1 &&= (!tcword)
        pop     edx
        or      eax, [ebx + ecx*4 + (uvector-offset 2)]
        mov     [edi + ecx*4 + (uvector-offset 2)], eax
        inc     ecx
        jmp     short :loop5
    :next8
        mov     esi, [edx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b1-len
    :loop6  
        cmp     ecx, esi
        jge     short :next10
        mov     eax, [edx + ecx*4 + (uvector-offset 2)]
        not     eax
        push    edx
        mov     edx, 0
        add     eax, [ebp - 16]                      ;; eax = ~b1[i] + carry1
        jne     short :next9
        inc     edx
    :next9
        and     [ebp - 16], edx                      ;; carry1 &&= (!tcword)
        pop     edx
        mov     [edi + ecx*4 + (uvector-offset 2)], eax 
        inc     ecx
        jmp     short :loop6                      
    :next10
        add     esp, 8     ;; pop carry1, carry2
        pop     esi
        xor     eax, eax
        xor     edx, edx
    end-atomic
        push    edi
        callp   bignum-2c-to-sm
        add     esp, 4
        jmp     :done
    :b1-neg-b2-neg     
        ;; b1 < 0, b2 < 0 
    begin-atomic
        mov     ecx, 0                              ;; i = 0
        mov     esi, [ebx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b2-len
        mov     edi, [ebp + (+ ARGS_OFFSET 0)]      ;; edi = result
    :loop7
        cmp     ecx, esi
        jge     short :next13
        mov     eax, [edx + ecx*4 + (uvector-offset 2)] ;; eax = b2[i]
        not     eax
        push    edx
        mov     edx, 0
        add     eax, [ebp - 16]                      ;; eax = ~b1[i] + carry1
        jne     short :next11
        inc     edx
    :next11
        and     [ebp - 16], edx                      ;; carry1 &&= (!tcword)
        pop     edx
        push    eax                                  ;; save temp result
        mov     eax, [ebx + ecx*4 + (uvector-offset 2)]
        not     eax
        push    edx
        mov     edx, 0
        add     eax, [ebp - 20]                      ;; eax = ~b2[i] + carry2
        jne     short :next12
        inc     edx
    :next12
        and     [ebp - 20], edx                      ;; carry2 &&= (!tcword)
        pop     edx
        or      eax, [esp + 0]
        add     esp, 4
        mov     [edi + ecx*4 + (uvector-offset 2)], eax
        inc     ecx
        jmp     short :loop7
    :next13
        mov     esi, [edx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b1-len
    :loop8  
        cmp     ecx, esi
        jge     short :next15
        mov     eax, 0
        not     eax
        mov     [edi + ecx*4 + (uvector-offset 2)], eax 
        inc     ecx
        jmp     short :loop8                     
    :next15
        add     esp, 8     ;; pop carry1, carry2
        pop     esi
        xor     eax, eax
        xor     edx, edx
    end-atomic
        push    edi
        callp   bignum-2c-to-sm
        add     esp, 4
    :done
        pop     edi
        pop     ebx
        pop     ebp
        ret
    })

(defun or-bignums (b1 b2)
    (unless (bignump b1)
        (cl::signal-type-error b1 'integer))
    (unless (bignump b2)
        (cl::signal-type-error b2 'integer)) 
    (let* ((b1-len (bignum-significant-length b1))
           (b2-len (bignum-significant-length b2))
           (dest-size (if (< b1-len b2-len) b2-len b1-len))
           (result (cl::alloc-bignum dest-size)))
        (or-bignum-words b1 b1-len b2 b2-len result)
        (normalize-bignum result)))

;;;
;;;	AND-BIGNUM-WORDS
;;;	Assumes result length >= max(b1-len, b2-len)
;;;	Assumes both bignums are in signed magnitude format.
;;;
(defasm and-bignum-words (b1 b1-len b2 b2-len result)
    {
        push    ebp
        mov     ebp, esp
        push    ebx
        push    edi
        push    esi
        push    1                             ;; carry1 = [ebp - 16]
        push    1                             ;; carry2 = [ebp - 20]
     
        mov     ecx, [ebp + (+ ARGS_OFFSET 12)]     ;; ecx = b1-len
        cmp     ecx, [ebp + (+ ARGS_OFFSET 4)]     ;; b1-len < b2-len?
        jge     short :next1    ;; ensure b1-len >= b2-len
        ;; swap b1, b2
        mov     eax, [ebp + (+ ARGS_OFFSET 4)]
        mov     [ebp + (+ ARGS_OFFSET 12)], eax
        mov     [ebp + (+ ARGS_OFFSET 4)], ecx
        mov     ecx, [ebp + (+ ARGS_OFFSET 16)]     ;; ecx = b1          
        mov     eax, [ebp + (+ ARGS_OFFSET 8)]      ;; eax = b2
        mov     [ebp + (+ ARGS_OFFSET 8)], ecx
        mov     [ebp + (+ ARGS_OFFSET 16)], eax
    :next1
        mov     edx, [ebp + (+ ARGS_OFFSET 16)]     ;; edx = b1, b1-len > b2-len
        mov     ebx, [ebp + (+ ARGS_OFFSET 8)]      ;; ebx = b2
        mov     eax, [edx + (uvector-offset 1)]
        test    eax, 8                              ;; b1 negative?
        jne     :b1-negative
        ;; b1 is positive
        mov     eax, [ebx + (uvector-offset 1)]
        test    eax, 8
        jne     short :b1-pos-b2-neg
        ;; b1 >=0, b2 >= 0
    begin-atomic
        mov     ecx, 0                              ;; i = 0
        mov     esi, [ebx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b2-len
        mov     edi, [ebp + (+ ARGS_OFFSET 0)]      ;; edi = result
    :loop1
        cmp     ecx, esi
        jge     short :next2
        mov     eax, [edx + ecx*4 + (uvector-offset 2)]
        and     eax, [ebx + ecx*4 + (uvector-offset 2)]
        mov     [edi + ecx*4 + (uvector-offset 2)], eax
        inc     ecx
        jmp     short :loop1
    :next2
        mov     esi, [edx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b1-len
    :loop2  
        cmp     ecx, esi
        jge     short :next3
        mov     eax, 0
        mov     [edi + ecx*4 + (uvector-offset 2)], eax 
        inc     ecx
        jmp     short :loop2                      
    :next3
        add     esp, 8     ;; pop carry1, carry2
        pop     esi
        xor     eax, eax
        xor     edx, edx
    end-atomic
        jmp     :done
    
    :b1-pos-b2-neg          ;; b1 >= 0, b2 < 0
         ;; b1 >=0, b2 >= 0
    begin-atomic
        mov     ecx, 0                              ;; i = 0
        mov     esi, [ebx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b2-len
        mov     edi, [ebp + (+ ARGS_OFFSET 0)]      ;; edi = result
    :loop3
        cmp     ecx, esi
        jge     short :next5
        mov     eax, [ebx + ecx*4 + (uvector-offset 2)] ;; eax = b2[i]
        not     eax
        push    edx
        mov     edx, 0
        add     eax, [ebp - 16]                      ;; eax = ~b2[i] + carry1
        jne     short :next4
        inc     edx
    :next4
        and     [ebp - 16], edx                      ;; carry1 &&= (!tcword)
        pop     edx
        and     eax, [edx + ecx*4 + (uvector-offset 2)]
        mov     [edi + ecx*4 + (uvector-offset 2)], eax
        inc     ecx
        jmp     short :loop3
    :next5
        mov     esi, [edx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b1-len
    :loop4  
        cmp     ecx, esi
        jge     short :next6
        mov     eax, [edx + ecx*4 + (uvector-offset 2)]
        mov     [edi + ecx*4 + (uvector-offset 2)], eax 
        inc     ecx
        jmp     short :loop4                      
    :next6
        add     esp, 8     ;; pop carry1, carry2
        pop     esi
        xor     eax, eax
        xor     edx, edx
    end-atomic
        jmp     :done
    
    :b1-negative 
        mov     eax, [ebx + (uvector-offset 1)]
        test    eax, 8
        jne     short :b1-neg-b2-neg    
        ;; b1 < 0, b2 >= 0 
    begin-atomic
        mov     ecx, 0                              ;; i = 0
        mov     esi, [ebx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b2-len
        mov     edi, [ebp + (+ ARGS_OFFSET 0)]      ;; edi = result
    :loop5
        cmp     ecx, esi
        jge     short :next8
        mov     eax, [edx + ecx*4 + (uvector-offset 2)] ;; eax = b1[i]
        not     eax
        push    edx
        mov     edx, 0
        add     eax, [ebp - 16]                      ;; eax = ~b1[i] + carry1
        jne     short :next7
        inc     edx
    :next7
        and     [ebp - 16], edx                      ;; carry1 &&= (!tcword)
        pop     edx
        and     eax, [ebx + ecx*4 + (uvector-offset 2)]
        mov     [edi + ecx*4 + (uvector-offset 2)], eax
        inc     ecx
        jmp     short :loop5
    :next8
        mov     esi, [edx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b1-len
    :loop6  
        cmp     ecx, esi
        jge     short :next10
        mov     eax, 0
        mov     [edi + ecx*4 + (uvector-offset 2)], eax 
        inc     ecx
        jmp     short :loop6                      
    :next10
        add     esp, 8     ;; pop carry1, carry2
        pop     esi
        xor     eax, eax
        xor     edx, edx
    end-atomic
        jmp     :done
    
    :b1-neg-b2-neg     
        ;; b1 < 0, b2 < 0 
    begin-atomic
        mov     ecx, 0                              ;; i = 0
        mov     esi, [ebx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b2-len
        mov     edi, [ebp + (+ ARGS_OFFSET 0)]      ;; edi = result
    :loop7
        cmp     ecx, esi
        jge     short :next13
        mov     eax, [edx + ecx*4 + (uvector-offset 2)] ;; eax = b2[i]
        not     eax
        push    edx
        mov     edx, 0
        add     eax, [ebp - 16]                      ;; eax = ~b1[i] + carry1
        jne     short :next11
        inc     edx
    :next11
        and     [ebp - 16], edx                      ;; carry1 &&= (!tcword)
        pop     edx
        push    eax                                  ;; save temp result
        mov     eax, [ebx + ecx*4 + (uvector-offset 2)]
        not     eax
        push    edx
        mov     edx, 0
        add     eax, [ebp - 20]                      ;; eax = ~b2[i] + carry2
        jne     short :next12
        inc     edx
    :next12
        and     [ebp - 20], edx                      ;; carry2 &&= (!tcword)
        pop     edx
        and     eax, [esp + 0]
        add     esp, 4
        mov     [edi + ecx*4 + (uvector-offset 2)], eax
        inc     ecx
        jmp     short :loop7
    :next13
        mov     esi, [edx + (uvector-offset 1)]
        shr     esi, 4                              ;; esi = b1-len
    :loop8  
        cmp     ecx, esi
        jge     short :next15
        mov     eax, [edx + ecx*4 + (uvector-offset 2)]
        not     eax
        push    edx
        mov     edx, 0
        add     eax, [ebp - 16]                      ;; eax = ~b1[i] + carry1
        jne     short :next14
        inc     edx
    :next14
        and     [ebp - 16], edx                      ;; carry1 &&= (!tcword)
        pop     edx
        not     eax
        mov     [edi + ecx*4 + (uvector-offset 2)], eax 
        inc     ecx
        jmp     short :loop8                     
    :next15
        add     esp, 8     ;; pop carry1, carry2
        pop     esi
        xor     eax, eax
        xor     edx, edx
    end-atomic
        push    edi
        callp   bignum-2c-to-sm
        add     esp, 4
    :done
        pop     edi
        pop     ebx
        pop     ebp
        ret
    })

(defun and-bignums (b1 b2)
    (unless (bignump b1)
        (cl::signal-type-error b1 'integer))
    (unless (bignump b2)
        (cl::signal-type-error b2 'integer)) 
    (let* ((b1-len (bignum-significant-length b1))
           (b2-len (bignum-significant-length b2))
           (dest-size (if (< b1-len b2-len) b2-len b1-len))
           (result (cl::alloc-bignum dest-size)))
        (and-bignum-words b1 b1-len b2 b2-len result)
        (normalize-bignum result)))

;;;
;;;	NOT-BIGNUM-WORDS
;;;	Assumes result length >= b1-len
;;;	Assumes bignum is in signed magnitude format.
;;;
(defasm not-bignum-words (b1 b1-len result)
    {
        push    ebp
        mov     ebp, esp
        push    ebx
        push    edi
        push    1                             ;; carry1 = [ebp - 12]
     
        mov     ecx, [ebp + (+ ARGS_OFFSET 4)]     ;; ecx = b1-len
        mov     edx, [ebp + (+ ARGS_OFFSET 8)]     ;; edx = b1
        mov     ecx, 0                              ;; i = 0
        mov     ebx, [edx + (uvector-offset 1)]
        shr     ebx, 4                              ;; ebx = b1-len
        mov     eax, [edx + (uvector-offset 1)]
    begin-atomic
        test    eax, 8                             ;; b1 negative?
        jne     :b1-negative
        ;; b1 is positive
        mov     edi, [ebp + (+ ARGS_OFFSET 0)]      ;; edi = result
    :loop1
        cmp     ecx, ebx
        jge     short :next1
        mov     eax, [edx + ecx*4 + (uvector-offset 2)]
        not     eax
        mov     [edi + ecx*4 + (uvector-offset 2)], eax
        inc     ecx
        jmp     short :loop1
    :next1
        jmp     short :done    
    
    :b1-negative 
       ;; b1 < 0 
        mov     edi, [ebp + (+ ARGS_OFFSET 0)]      ;; edi = result
    :loop2
        cmp     ecx, ebx
        jge     short :done
        mov     eax, [edx + ecx*4 + (uvector-offset 2)] ;; eax = b1[i]
        not     eax
        push    edx
        mov     edx, 0
        add     eax, [ebp - 12]                      ;; eax = ~b1[i] + carry1
        jne     short :next2
        inc     edx
    :next2
        and     [ebp - 12], edx                      ;; carry1 &&= (!tcword)
        pop     edx
        not     eax
        mov     [edi + ecx*4 + (uvector-offset 2)], eax
        inc     ecx
        jmp     short :loop2
    :done
        xor     eax, eax
        xor     edx, edx
    end-atomic
        push    edi
        callp   bignum-2c-to-sm
        add     esp, 8     ;; pop carry1, arg
        pop     edi
        pop     ebx
        pop     ebp
        ret
    })

;;
;;	BIGNUM-EXPAND
;;	Returns a bignum which has been expanded to fill the requested number
;;	of 32-bit words by adding zeros to the beginning. Assumes the passed
;;	word is smaller or the same size as the requested length.
;;
(defasm bignum-expand (b len)
    {
        push    ebp
        mov     ebp, esp
        push    ebx
        push    edi
        push    [ebp + (+ ARGS_OFFSET 0)]       ; push length
        mov     ecx, 1
        callp   cl::alloc-bignum
        add     esp, 4
    begin-atomic
        mov     edi, eax                        ; edi = result
        mov     edx, [ebp + (+ ARGS_OFFSET 4)]  ; edx = b
        mov     ebx, [edx + (uvector-offset 1)] ; ebx = len 
        shr     ebx, 4  
        mov     ecx, 0
    :loop
        cmp     ecx, ebx
        jge     short :next1   
        mov     eax, [edx + ecx*4 + (uvector-offset 2)]
        mov     [edi + ecx*4 + (uvector-offset 2)], eax
        inc     ecx
        jmp     short :loop
    :next1
        ;; copy sign bit
        mov     al, [edx + (uvector-offset 1)]
        and     al, 8                           ; al = sign bit
        mov     dl, [edi + (uvector-offset 1)]
        and     dl, #xf0
        or      dl, al
        mov     [edi + (uvector-offset 1)], dl 
        xor     edx, edx
        mov     eax, edi
        mov     ecx, 1
    end-atomic
        pop     edi
        pop     ebx
        pop     ebp
        ret
    })

(defun not-bignum (b)
    (unless (bignump b)
        (cl::signal-type-error b 'integer))
    (let* ((b-len (bignum-significant-length b))
           (dest-size (+ b-len 1))      ;; need to make sure there is an extra high-order bit
           (result (cl::alloc-bignum dest-size)))
        (setq b (bignum-expand b dest-size))
        (not-bignum-words b dest-size result)
        (normalize-bignum result)))

(defasm cl::bignum-integer-length (b)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + ARGS_OFFSET]
        callp   bignum-significant-length
        add     esp, 4
    begin-atomic
        shr     eax, 3
        dec     eax         ;; eax = bignum-length - 1
        mov     edx, [ebp + ARGS_OFFSET]
        mov     ecx, [edx + eax*4 + (uvector-offset 2)] ;; ecx = hi-word
        shl     eax, 5                                  ;; eax = (len - 1) * 32
        test    ecx, #x-10000  ; test against #xffff0000
        jz      short :next1
        add     eax, 16
        shr     ecx, 16
    :next1
        test    ecx, #x0000ff00
        jz      short :next2
        add     eax, 8
        shr     ecx, 8
    :next2
        test    ecx, #x000000f0
        jz      short :next3
        add     eax, 4
        shr     ecx, 4
    :next3           
        test    ecx, #x0000000c
        jz      short :next4
        add     eax, 2
        shr     ecx, 2
    :next4 
        test    ecx, #x00000002
        jz      short :next5
        add     eax, 1
        shr     ecx, 1
    :next5
        inc     eax
        shl     eax, 3
    end-atomic
        mov     ecx, 1
        pop     ebp
        ret
    })
              
    
                  
