;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		kernel-asm.lisp
;;;;	Contents:	Corman Lisp kernel functions defined in x86 assembler
;;;;	History:	2/7/97  RGC  Created.
;;;;               9/4/01  RGC  Incorporated many enhancements and fixes by
;;;;                            Pavel Grozman.
;;;;               10/19/01 RGC Rewrote MACRO-FUNCTION in lisp to accomodate optional environment.
;;;;

(in-package :x86)

(defconstant _uninitialized 4)
(defconstant nil-offset		0)
(defconstant t-offset		4)

;; (ebp 0)		link to call stack
;; (ebp 4)		return address
;; (ebp 8)		last argument
;; (ebp 12)		2nd to last argument
;; etc.
;;	ecx = no. of arguments
;;

(defasm _not-a-symbol-error (sym)
	{
		push	ebp
		mov		ebp, esp
		push    "Invalid symbol: ~A"
		push    [ebp + ARGS_OFFSET]
		mov 	ecx, 2
		callp 	error
;		add 	esp, 8  ;; not needed
		pop		ebp
		ret
	})

(defasm _not-a-function-error (sym)
	{
		push	ebp
		mov		ebp, esp
		push 	"Invalid function: ~A"
		push 	[ebp + ARGS_OFFSET]
		mov 	ecx, 2
		callp 	error
		pop		ebp
		ret
	})

(defasm _not-a-list-error (sym)
	{
		push	ebp
		mov		ebp, esp
		push 	"Invalid list: ~A"
		push 	[ebp + ARGS_OFFSET]
		mov 	ecx, 2
		callp 	error
		pop		ebp
		ret
	})

(defasm _not-a-character-error (sym)
	{
		push	ebp
		mov		ebp, esp
		push 	"Invalid character: ~A"
		push 	[ebp + ARGS_OFFSET]
		mov 	ecx, 2
		callp 	error
		pop		ebp
		ret
	})

(defasm _unbound-symbol-error (sym)
	{
		push	ebp
		mov		ebp, esp
		push 	"The symbol ~A is unbound"
		push 	[ebp + ARGS_OFFSET]
		mov 	ecx, 2
		callp 	error
		pop		ebp
		ret
	})

(defasm _unbound-function-error (sym)
	{
		push	ebp
		mov		ebp, esp
		push 	"The symbol ~A does not have a function binding"
		push 	[ebp + ARGS_OFFSET]
		mov 	ecx, 2
		callp 	error
		pop		ebp
		ret
	})

(defasm _wrong-number-of-args-error ()
	{
		push	ebp
		mov		ebp, esp
		push   "Wrong number of arguments"
		mov 	ecx, 1
		callp 	error
		pop		ebp
		ret
	})

;;;;	
;;;;	On entry:		eax = list to be copied
;;;;	On exit:		eax = copy, edx = last cons in copy
;;;;
(defasm _copy-list (list)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		push	[esi]		;; variable at [ebp - 8]
		mov		edi, eax	;; edi = original list
		mov		eax, [esi]	;; eax = nil
		mov		ecx, edi
		and		ecx, 7
		cmp		ecx, cons-tag
		jne		short :t10
		callp	cl::%alloc-cons	;; eax = new cons cell
		mov		edx, eax
		mov		ecx, [esi]
		mov		[eax], ecx
		mov		ecx, [edi - 4]	;; set car of new cell
		mov		[eax - 4], ecx
		mov		[ebp - 8], eax
	:t2
		mov		edi, [edi]
		mov		ecx, edi
		and		ecx, 7
		cmp		ecx, cons-tag
		jne		short :t10
		push	eax
		callp	cl::%alloc-cons
		mov		edx, eax
		pop		ecx
		mov		[ecx], eax
		mov		ecx, [edi - 4]
		mov		[eax - 4], ecx
		mov		ecx, [esi]
		mov		[eax], ecx
		jmp		short :t2
	:t10
		mov		eax, [ebp - 8]	
		add		esp, 4
		pop		edi
		mov		esp, ebp
		pop		ebp
		ret
	})

;; assumes symbol in eax
(defmacro x86::tag-byte (tag) `(+ (* ,tag 8) 6))

(defasm check-symbol ()
	{
		push	ebp
		mov		ebp, esp
		mov 	edx, eax
		and 	edx, 7
		cmp 	dl, uvector-tag
		jne 	short :no
		mov 	dl, [eax - uvector-tag]
		cmp		dl, (tag-byte uvector-symbol-tag)
		je 		short :yes
	:no
		push 	eax
		callp 	_not-a-symbol-error
	:yes
		pop		ebp
		ret
	})
#| old version
(defasm check-function ()
	{
		push	ebp
		mov		ebp, esp
		mov 	edx, eax
		and 	edx, 7
		cmp 	edx, uvector-tag
		je 		short :t1
		push 	eax
		callp 	_not-a-function-error
	:t1
		mov 	edx, [eax - uvector-tag]
		shl		edx, 24
		shr		edx, 27
		cmp 	edx, uvector-kfunction-tag
		jle 	short :t2
		push 	eax
		callp 	_not-a-function-error
	:t2
		pop		ebp
		ret
	})
|#
;; assumes symbol in eax

(defasm check-function ()
	{
		push	ebp
		mov		ebp, esp
		mov 	edx, eax
		and 	edx, 7
		cmp 	dl, uvector-tag
		jne 	short :no
		mov		dl, (tag-byte uvector-kfunction-tag)
		cmp 	dl, [eax - uvector-tag]
		jae 	short :yes
	:no
		push 	eax
		callp 	_not-a-function-error
	:yes
		pop		ebp
		ret
	})


;;;
;;;	Common Lisp SYMBOL-VALUE function.
;;;
(defasm symbol-value (sym)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :next1
		callp 	_wrong-number-of-args-error
	:next1
		mov 	eax, [ebp + ARGS_OFFSET]		;; eax = symbol
		callp 	check-symbol
		mov 	edx, eax
		mov 	eax, [eax + (- (* 4 symbol-var-table-offset) uvector-tag)]
		test 	eax, eax
		jz 		short :not-in-symbol-table
		shr		eax, 1
		mov 	eax, [esi + eax]
		jmp 	short :next3
	:not-in-symbol-table
		mov 	eax, [edx + (- (* 4 symbol-value-offset) uvector-tag)]
	:next3
		mov 	eax, [eax - cons-tag]
		cmp 	eax, _uninitialized
		jne 	short :next2
		mov 	eax, [ebp + ARGS_OFFSET]	;; eax = symbol
		push 	eax
		callp 	_unbound-symbol-error
	:next2
;		mov 	ecx, 1
		pop		ebp
		ret
	})

;;;
;;;	Common Lisp SET function.
;;;
(defasm set (sym value)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 2
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov 	eax, [ebp + (+ ARGS_OFFSET 4)]		;; eax = symbol
		callp 	check-symbol
		mov 	edx, eax			;; edx = symbol
		mov 	eax, [ebp + ARGS_OFFSET]		;; eax = value
		mov 	ecx, [edx + (- (* 4 symbol-var-table-offset) uvector-tag)]
		test 	ecx, ecx
		jz 		short :not-in-symbol-table
		shr		ecx, 1
		mov 	edx, [esi + ecx]
		jmp 	short :t2
	:not-in-symbol-table
		mov 	edx, [edx + (- (* 4 symbol-value-offset) uvector-tag)]
	:t2
		mov 	[edx - cons-tag], eax
		mov 	ecx, 1
		pop		ebp
		ret
	})

;;;
;;;	Common Lisp MAKUNBOUND function.
;;;
(defasm makunbound (sym)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov 	eax, [ebp + ARGS_OFFSET]		;; eax = symbol
		callp 	check-symbol
		mov 	ecx, [eax + (- (* 4 symbol-var-table-offset) uvector-tag)]
		test 	ecx, ecx
		jz 		short :not-in-symbol-table
		shr		ecx, 1
		mov 	edx, [esi + ecx]
		jmp 	short :t2
	:not-in-symbol-table
		mov 	edx, [eax + (- (* 4 symbol-value-offset) uvector-tag)]
	:t2
		mov		ecx, _uninitialized
		mov 	[edx - cons-tag], ecx
		mov 	ecx, 1
		pop		ebp
		ret
	})

;;;
;;;	Common Lisp FMAKUNBOUND function.
;;;
(defasm cl::%fmakunbound (sym)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov 	eax, [ebp + ARGS_OFFSET]		;; eax = symbol
		callp 	check-symbol
		xor		ecx, ecx
		mov 	[eax + (- (* 4 symbol-jump-table-offset) uvector-tag)], ecx
		mov 	edx, [eax + (- (* 4 symbol-function-offset) uvector-tag)]
		mov		ecx, _uninitialized
		mov 	[edx - cons-tag], ecx
		mov		ecx, [esi]
		mov		[eax + (- (* 4 symbol-function-type-offset) uvector-tag)], ecx
		mov 	ecx, 1
		pop		ebp
		ret
	})

;;;;
;;;;	Common Lisp BOUNDP function.
;;;;
(defasm boundp (sym)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :next1
		callp 	_wrong-number-of-args-error
	:next1
		mov 	eax, [ebp + ARGS_OFFSET]		;; eax = symbol
		callp 	check-symbol
		mov 	edx, eax
		mov 	eax, [eax + (- (* 4 symbol-var-table-offset) uvector-tag)]
		test 	eax, eax
		jz 		short :not-in-symbol-table
		shr		eax, 1
		mov 	eax, [esi + eax]
		jmp 	short :next3
	:not-in-symbol-table
		mov 	eax, [edx + (- (* 4 symbol-value-offset) uvector-tag)]
	:next3
		mov 	eax, [eax - cons-tag]
		cmp 	eax, _uninitialized
		jne 	short :next4
		mov 	eax, [esi]				;eax = nil
		jmp 	:next2
	:next4
		mov		eax, [esi + t-offset]	;eax = true
	:next2
;		mov 	ecx, 1
		pop		ebp
		ret
	})

;;;;
;;;;	Common Lisp IDENTITY function.
;;;;
(defasm identity (obj)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		je 		short :next
		callp 	_wrong-number-of-args-error
	:next
		mov 	eax, [ebp + ARGS_OFFSET]
;		mov 	ecx, 1
		pop		ebp
		ret
	})

;;;;
;;;;	Common Lisp FUNCALL function.
;;;;
(defasm funcall (func &rest args) 
	{
		push	ebp
		mov		ebp, esp
		push	edi
		push	ecx
		push	ebx
		push	0			;; one cell local storage at [ebp - 16]

		cmp		ecx, 1
		jge		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		eax, [ebp + ecx*4 + 4]		;; eax = function
		mov		edx, eax
		and		edx, 7
		cmp		edx, uvector-tag			;; see if func arg is a uvector
		je		short :t2
		push	eax
		callp	_not-a-function-error
	:t2
		mov		edx, [eax - uvector-tag]	;; edx = uvector header
		shl		edx, 24
		shr		edx, 27
		cmp		edx, uvector-symbol-tag		;; see if it is a symbol
		jne		short :t3
		;; get the function that is bound to the symbol
		mov		eax, [eax + (- (* 4 symbol-function-offset) uvector-tag)]
		mov		eax, [eax - 4]
		mov		edx, eax
		and		edx, 7
		cmp		edx, uvector-tag
		je 		short :t9
		push	[ebp + ecx*4 + 4]
		callp	_not-a-function-error
	:t9
		mov		edx, [eax - uvector-tag]	;; edx = uvector header
		shl		edx, 24
		shr		edx, 27
	:t3 ;; we now know we have a function in eax, and dl is the type

		;; push all the arguments
		mov		[ebp - 16], esp
		mov		ebx, ecx
		dec		ecx
	:t4
		dec		ebx
		jle		short :t5
		push 	[ebp + ebx*4 + 4]
		jmp		short :t4
	:t5
		cmp 	edx, uvector-function-tag
		jne		short :t6
		mov		edi, [eax + (- (* 4 function-environment-offset) uvector-tag)] 
		callfunc	eax	
		jmp		short :t8
	:t6
		cmp 	edx, uvector-kfunction-tag
		jne		short :t7
		mov		edi, [esi]		;; environment for kfunctions is always NIL
		call	[eax + (- (* function-code-buffer-offset 4) uvector-tag)]
		jmp		short :t8
	:t7
		push	eax
		callp	_not-a-function-error
	:t8
		mov		esp, [ebp - 16]
		pop		edi
		pop		ebx
		pop		edi
		pop		edi
		mov		esp, ebp
		pop		ebp
		ret
	})
	
;;;;
;;;;	Common Lisp APPLY function.
;;;;
(defasm apply (func &rest args)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		push	ecx
		push	ebx
		push	0			;; one cell local storage at [ebp - 16]

		cmp		ecx, 2
		jge		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		eax, [ebp + ecx*4 + 4]		;; eax = function
		mov		edx, eax
		and		edx, 7
		cmp		edx, uvector-tag			;; see if func arg is a uvector
		je		short :t2
		push	eax
		callp	_not-a-function-error
	:t2
		mov		edx, [eax - uvector-tag]	;; edx = uvector header
		shl		edx, 24
		shr		edx, 27
		cmp		edx, uvector-symbol-tag	;; see if it is a symbol
		jne		short :t4
		;; get the function that is bound to the symbol
		mov		eax, [eax + (- (* 4 symbol-function-offset) uvector-tag)]
		mov		eax, [eax - 4]
		mov		edx, eax
		and		edx, 7
		cmp		edx, uvector-tag
		je 		short :t3
		push	[ebp + ecx*4 + 4]
		callp	_not-a-function-error
	:t3
		mov		edx, [eax - uvector-tag]	;; edx = uvector header
		shl		edx, 24
		shr		edx, 27
	:t4 ;; we now know we have a function in eax, and dl is the type

		;; push all the arguments except the last
		mov		[ebp - 16], esp
		dec		ecx
		mov		ebx, ecx
		dec		ecx
	:t5
		dec		ebx
		jle		short :t6
		push 	[ebp + ebx*4 + 8]
		jmp		short :t5
	:t6
		;; the last argument is a list of remaining arguments
		mov		edi, [ebp + ARGS_OFFSET]
	:t7
		mov		ebx, edi
		and		ebx, 7
		cmp		ebx, cons-tag			;; is a cons cell?
		jne		short :t8					;; if not, exit
		push	[edi - 4]
		inc		ecx
		mov		edi, [edi]
		jmp		short :t7
	:t8
		cmp 	edx, uvector-function-tag
		jne		short :t9
		mov		edi, [eax + (- (* 4 function-environment-offset) uvector-tag)] 
		callfunc	eax	
		jmp		short :t11
	:t9
		cmp 	edx, uvector-kfunction-tag
		jne		short :t10
		mov		edi, [esi]		;; environment for kfunctions is always NIL
		call	[eax + (- (* function-code-buffer-offset 4) uvector-tag)]
		jmp		short :t11
	:t10
		push	eax
		callp	_not-a-function-error
	:t11
		mov		esp, [ebp - 16]
		pop		edi				;; remove local storage
		pop		ebx
		pop		edi
		pop		edi
		mov		esp, ebp
		pop		ebp
		ret
	})

;;;;
;;;;	Common Lisp SYMBOL-FUNCTION function.
;;;;
(defasm symbol-function (sym)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :next1
		callp 	_wrong-number-of-args-error
	:next1
		mov 	eax, [ebp + ARGS_OFFSET]		;; eax = symbol
		callp 	check-symbol
		mov 	eax, [eax + (- (* 4 symbol-function-offset) uvector-tag)]
		mov 	eax, [eax - cons-tag]
		cmp 	eax, _uninitialized
		jne 	short :next2
		push	[ebp + ARGS_OFFSET]	;; eax = symbol
		callp 	_unbound-function-error
	:next2
;		mov 	ecx, 1
		pop		ebp
		ret	
	})

;;;;
;;;;	Common Lisp FUNCTIONP function.
;;;;
(defasm functionp (x)
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
		cmp 	dl, (tag-byte uvector-kfunction-tag)
		ja 		short :next2
		mov		eax, [esi + t-offset]
	:next2
		pop		ebp
		ret
	})

;;;
;;;	Corman Lisp KERNEL-FUNCTION-P function.
;;;
(defasm ccl::kernel-function-p (x)
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
		cmp 	dl, (tag-byte uvector-kfunction-tag)
		jne 	short :next2
		mov		eax, [esi + t-offset]
	:next2
		pop		ebp
		ret
	})

;;;;
;;;;	Common Lisp MACRO-FUNCTION function.
;;;;
(defun macro-function (symbol &optional environment)
	(declare (ignore environment))
	(if (and (symbolp symbol)
			(fboundp symbol)
			(eq (uref symbol symbol-function-type-offset) 'cl::macro))
		(symbol-function symbol)))
#|
(defasm macro-function (x)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :next1
		callp 	_wrong-number-of-args-error
	:next1
		mov 	eax, [ebp + ARGS_OFFSET]		;; eax = symbol
		callp 	check-symbol
		mov		edx, eax
		mov 	eax, [eax + (- (* 4 symbol-function-offset) uvector-tag)]
		mov 	eax, [eax - cons-tag]
		cmp 	eax, _uninitialized
		jne 	short :t2
		mov		eax, [esi]
		jmp		short :t3
	:t2
		mov 	edx, [edx + (- (* 4 symbol-function-type-offset) uvector-tag)]
		cmp		edx, 'cl::macro
		je		short :t3
		mov		eax, [esi]
	:t3
;		mov 	ecx, 1
		pop		ebp
		ret	
	})
|#
;;;;
;;;;	Common Lisp LIST* function.
;;;;
(defasm list* (x &rest args)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		push	ecx
		push	ecx			;; allocate variable at [ebp - 12]
		cmp		ecx, 1
		jge		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		eax, [ebp + ARGS_OFFSET]		;; get last argument
		mov		edi, 1
	:t2
		cmp		edi, [ebp - 12]
		je		short :t3
		push	eax
		callp	cl::%alloc-cons
		pop		[eax]
		mov		ecx, [ebp + edi*4 + 8]
		mov		[eax - 4], ecx
		inc		edi
		jmp		short :t2
	:t3
		pop		ecx
		pop		ecx
		pop		edi		
		mov 	ecx, 1
		pop		ebp
		ret	
	})

;;;;
;;;;	Common Lisp LIST function.
;;;;
(defasm list (&rest args)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		push	ecx
		push	ecx			;; allocate variable at [ebp - 12]
		mov		eax, [esi]
		mov		edi, 0
	:t2
		cmp		edi, [ebp - 12]
		je		short :t3
		push	eax
		callp	cl::%alloc-cons
		pop		[eax]
		mov		ecx, [ebp + edi*4 + 8]
		mov		[eax - 4], ecx
		inc		edi
		jmp		short :t2
	:t3
		pop		ecx
		pop		ecx	
		pop		edi	
		mov 	ecx, 1
		pop		ebp
		ret	
	})

;;;;
;;;;	Common Lisp APPEND function.
;;;;

(defasm append (&rest args)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		push	ecx
		mov		eax, [esi]
		push	eax			;; allocate variable at [ebp - 12]
		push	eax			;; allocate variable at [ebp - 16]

		cmp		ecx, 0
		je		short :t10
	:t1	
		cmp		ecx, 1
		jne		short :t2
		mov		eax, [ebp + ARGS_OFFSET]
		jmp		short :t10
	:t2
		mov		edi, ecx
		sub		edi, 2
	:t3
		mov		eax, [ebp + edi * 4 + 12]
		callp	_copy-list	;; eax contains start of list
							;; edx contains pointer to last cons
		cmp		eax, [esi]
		je		short :t5
		mov		ecx, [ebp - 16]
		cmp		ecx, [esi]
		je		short :t4
		mov		[ecx], eax
		mov		[ebp - 16], edx
		jmp		short :t5
	:t4
		mov		[ebp - 12], eax
		mov		[ebp - 16], edx
	:t5
		dec		edi
		jnl		short :t3
		mov		eax, [ebp + ARGS_OFFSET]
		mov		ecx, [ebp - 16]
		cmp		ecx, [esi]
		je		short :t10
		mov		[ecx], eax
		mov		eax, [ebp - 12]
	:t10
		add		esp, 8
		pop		ecx
		mov 	ecx, 1
		pop		edi
		pop		ebp
		ret	
	})

;;;;
;;;;	Common Lisp COPY-LIST function.
;;;;
(defasm copy-list (list)
	{
		push	ebp
		mov		ebp, esp
		mov		eax, [ebp + ARGS_OFFSET]
		callp	_copy-list
		mov		ecx, 1
		mov		esp, ebp
		pop		ebp
		ret
	})

;;;;
;;;;	Common Lisp CAR function.
;;;;
(defasm car (x)
	{
		push    ebp                 
		mov     ebp,esp             
		cmp     ecx, 1          
		jz      short :t1                
		callp 	_wrong-number-of-args-error
	:t1 
		mov     eax, [ebp + ARGS_OFFSET]    
		mov     edx, eax             
		and     edx, 7          
		cmp     edx, cons-tag  		;; arg is a cons?        
		jnz     short :t2                
		mov     eax, [eax - 4]    
		jmp     short :t3
	:t2          
		cmp     eax, [esi]  		;; arg = NIL?         
		je      short :t3                
		push    eax                 
		callp   _not-a-list-error 
	:t3       
		pop     ebp                 
		ret
	})

;;;;
;;;;	Common Lisp CDR function.
;;;;
(defasm cdr (x)
	{
		push    ebp                 
		mov     ebp,esp             
		cmp     ecx, 1          
		jz      short :t1                
		callp 	_wrong-number-of-args-error
	:t1 
		mov     eax, [ebp + ARGS_OFFSET]    
		mov     edx, eax             
		and     edx, 7          
		cmp     edx, cons-tag  		;; arg is a cons?        
		jnz     short :t2                
		mov     eax, [eax]    
		jmp     short :t3
	:t2          
		cmp     eax, [esi]  		;; arg = NIL?         
		je      short :t3                
		push    eax                 
		callp   _not-a-list-error 
	:t3       
		pop     ebp                 
		ret
	})

;;;;
;;;;	Common Lisp NULL function.
;;;;
(defasm null (x)
	{
		push    ebp                 
		mov     ebp,esp             
		cmp     ecx, 1          
		jz      short :t1                
		callp 	_wrong-number-of-args-error
	:t1 
		mov     eax, [ebp + ARGS_OFFSET]    
		cmp		eax, [esi]
		mov		eax, [esi]
		jne		short :t2
		mov		eax, [esi + t-offset]
	:t2             
		pop     ebp                 
		ret
	})
 
;;;;
;;;;	Common Lisp EQ function.
;;;;
(defasm eq (x1 x2)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 2
		je 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		eax, [ebp + ARGS_OFFSET]
		cmp		eax, [ebp + (+ ARGS_OFFSET 4)]
		mov		eax, [esi]
		jne		short :t2
		mov		eax, [esi + 4]
	:t2
		mov		ecx, 1
		pop		ebp
		ret
	})

;;;;
;;;;	Common Lisp CONS function.
;;;;
(defasm cons (x1 x2)
	{
		push    ebp                 
		mov     ebp, esp             
		cmp     ecx, 2          
		je      short :t1                
		callp 	_wrong-number-of-args-error
	:t1
		callp	cl::%alloc-cons
		mov		ecx, [ebp + (+ ARGS_OFFSET 4)]
		mov		[eax - 4], ecx
		mov		ecx, [ebp + ARGS_OFFSET]
		mov		[eax], ecx
		mov		ecx, 1
		mov		esp, ebp
		pop		ebp
		ret
	})

;;;;
;;;;	Common Lisp CONSP function.
;;;;
(defasm consp (x)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		eax, [ebp + ARGS_OFFSET]		;; eax = argument
		mov		edx, eax
		and		edx, 7
		cmp		edx, cons-tag
		je		short :t2
		mov		eax, [esi]
		jmp		short :t4
	:t2
		cmp		eax, _uninitialized
		jne		short :t3
		mov		eax, [esi]
		jmp		short :t4
	:t3
		mov		eax, [esi + t-offset]
	:t4
		pop		ebp
		ret
	})

;;;;
;;;;	Common Lisp SYMBOLP function.
;;;;
(defasm symbolp (x)
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
		cmp 	dl, (tag-byte uvector-symbol-tag)
		jne 	short :next2
		mov		eax, [esi + t-offset]
	:next2
		pop		ebp
		ret
	})

;;;;
;;;;	Common Lisp STRINGP function.
;;;;
(defasm stringp (x)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		edx, [ebp + ARGS_OFFSET]		;; edx = argument
		mov		eax, edx
		and		eax, 7
		cmp		eax, uvector-tag
		mov		eax, [esi]
		jne		short :t3
		mov 	cl, [edx - uvector-tag]
		cmp 	cl, (tag-byte uvector-array-tag)
		jne 	short :t2
		mov		ecx, [edx + (uvector-offset cl::adjustable-array-dimensions-offset)]
		cmp		ecx, 8
		jne		:t3									; more than 1 dimension
		mov		edx, [edx + (uvector-offset cl::adjustable-array-vector-offset)]
		mov 	cl, [edx - uvector-tag]
	:t2
		cmp 	cl, (tag-byte uvector-simple-char-vector-tag)
		jne		:t3
		mov		eax, [esi + t-offset]		
	:t3
		mov		ecx, 1
		pop		ebp
		ret
	})

;;;;
;;;;	Common Lisp STREAMP function.
;;;;
(defasm streamp (x)
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
		cmp 	dl, (tag-byte uvector-stream-tag)
		jne 	short :next2
		mov		eax, [esi + t-offset]
	:next2
		pop		ebp
		ret
	})

(defasm cl::foreign-ptr-p (x)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		edx, [ebp + ARGS_OFFSET]		;; edx = argument
		mov		eax, edx
		and		eax, 7
		cmp		eax, uvector-tag
		mov		eax, [esi]
		jne		short :t3
		mov 	edx, [edx - uvector-tag]
		cmp 	dl, (tag-byte uvector-foreign-tag)
		je 		short :t2
		cmp 	dl, (tag-byte uvector-foreign-heap-tag)
		je 		short :t2
		jmp		:t3
	:t2
		mov		eax, [esi + t-offset]
	:t3
		pop		ebp
		ret
	})

;;;;
;;;;	Common Lisp PACKAGEP function.
;;;;
(defasm packagep (x)
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
		cmp 	dl, (tag-byte uvector-package-tag)
		jne 	short :next2
		mov		eax, [esi + t-offset]
	:next2
		pop		ebp
		ret
	})

;;;;
;;;;	Common Lisp READTABLEP function.
;;;;
(defasm readtablep (x)
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
		cmp 	dl, (tag-byte uvector-readtable-tag)
		jne 	short :next2
		mov		eax, [esi + t-offset]
	:next2
		pop		ebp
		ret
	})

;;;;
;;;;	Common Lisp ARRAYP function.
;;;;
(defasm arrayp (x)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		edx, [ebp + ARGS_OFFSET]		;; edx = argument
		mov		eax, edx
		and		eax, 7
		cmp		eax, uvector-tag
		mov		eax, [esi]
		jne		short :t2
		mov 	edx, [edx - uvector-tag]
		cmp 	dl, (tag-byte uvector-array-tag)
		jne		short :t3
		mov		eax, [esi + t-offset]
		jmp		short :t2
	:t3
		cmp		dl, (tag-byte uvector-simple-vector-tag)
		jb		short :t2
		cmp		dl, (tag-byte uvector-simple-single-float-vector-tag)
		ja		short :t2
		mov		eax, [esi + t-offset]
	:t2
		pop		ebp
		ret
	})

;;;;
;;;;	SEQUENCEP function.
;;;;
(defasm sequencep (x)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		edx, [ebp + ARGS_OFFSET]		;; edx = argument
		cmp		edx, [esi]			;; arg = NIL?
		je		short :yes
		mov		eax, edx
		and		eax, 7
		cmp		eax, uvector-tag
		jne		short :check-list	
		mov 	cl, [edx - uvector-tag]
		cmp 	cl, (tag-byte uvector-array-tag)
		jne		short :check-simple-vector		
		mov		ecx, [edx + (- (* 4 cl::adjustable-array-dimensions-offset) uvector-tag)]
		cmp		ecx, 8
		jne		short :no
		jmp		short :yes
	:check-simple-vector
		cmp		cl, (tag-byte uvector-simple-vector-tag)
		jb		short :no
		cmp		cl, (tag-byte uvector-simple-single-float-vector-tag)
		ja		short :no
		jmp		short :yes		
	:check-list
		cmp		eax, cons-tag
		jne		short :no
	:yes
		mov		eax, [esi + t-offset]
		jmp		short :exit
	:no
		mov		eax, [esi]
	:exit
		mov		ecx, 1
		pop		ebp
		ret
	})

;;;;
;;;;	Corman Lisp kernel STRUCTUREP function.
;;;;
(defasm structurep (x)
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
		cmp 	dl, (tag-byte uvector-structure-tag)
		jne 	short :next2
		mov		eax, [esi + t-offset]
	:next2
		pop		ebp
		ret
	})

;;;;
;;;;	Common Lisp INTEGERP function.
;;;;
(defasm integerp (x)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		edx, [ebp + ARGS_OFFSET]		;; edx = argument
		test	edx, 7
		je		short :true
		mov		eax, edx
		and		eax, 7
		cmp		eax, uvector-tag
		jne		short :false
		mov		eax, [edx - uvector-tag]
		cmp		al, (tag-byte uvector-bignum-tag)
		je		short :true
	:false
		mov		eax, [esi]
		jmp		short :t10
	:true
		mov		eax, [esi + t-offset]
	:t10
		pop		ebp
		ret
	})

;;;;
;;;;	Corman Lisp kernel FIXNUMP function.
;;;;
(defasm fixnump (x)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		edx, [ebp + ARGS_OFFSET]		;; edx = argument
		test	edx, 7
		je		short :true
	:false
		mov		eax, [esi]
		jmp		short :t10
	:true
		mov		eax, [esi + t-offset]
	:t10
		pop		ebp
		ret
	})

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
;;;;	Corman Lisp kernel BIGNUMP function.
;;;;
(defasm bignump (x)
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
		cmp 	dl, (tag-byte uvector-bignum-tag)
		jne 	short :next2
		mov		eax, [esi + t-offset]
	:next2
		pop		ebp
		ret
	})

;;;;
;;;;	Common Lisp FLOATP function.
;;;;
(defasm floatp (x)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		edx, [ebp + ARGS_OFFSET]		;; edx = argument
		mov		eax, edx
		and		eax, 3
		cmp		eax, 3
		je		short :exit_t
		mov		eax, edx
		and		eax, 7
		cmp		eax, uvector-tag
		jne		short :exit_nil
		mov 	edx, [edx - uvector-tag]
		cmp 	dl, (tag-byte uvector-double-float-tag)
		je 		short :exit_t
		cmp 	dl, (tag-byte uvector-single-float-tag)
		jne		short :exit_nil
	:exit_t
		mov		eax, [esi + t-offset]
		jmp		short :exit
	:exit_nil
		mov		eax, [esi]
	:exit
		pop		ebp
		ret
	})

;;;;
;;;;	Common Lisp RATIOP function.
;;;;
(defasm ratiop (x)
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
		cmp 	dl, (tag-byte uvector-ratio-tag)
		jne 	short :next2
		mov		eax, [esi + t-offset]
	:next2
		pop		ebp
		ret
	})

;;;;
;;;;	Common Lisp COMPLEXP function.
;;;;
(defasm complexp (x)
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
		cmp 	dl, (tag-byte uvector-complex-tag)
		jne 	short :next2
		mov		eax, [esi + t-offset]
	:next2
		pop		ebp
		ret
	})

;;;
;;;	Corman Lisp COMPILED-CODE-P function.
;;;
(defasm compiled-code-p (x)
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
		cmp 	dl, (tag-byte uvector-compiled-code-tag)
		jne 	short :next2
		mov		eax, [esi + t-offset]
	:next2
		pop		ebp
		ret
	})

;;;
;;;	Corman Lisp WEAK-POINTER-P function.
;;;
(defasm weak-pointer-p (x)
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
		cmp 	dl, (tag-byte uvector-weak-ptr-tag)
		jne 	short :next2
		mov		eax, [esi + t-offset]
	:next2
		pop		ebp
		ret
	})

;;;;
;;;;	Common Lisp LISTP function.
;;;;
(defasm listp (x)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		edx, [ebp + ARGS_OFFSET]		;; edx = argument
		cmp		edx, [esi]			;; arg = NIL?
		jne		short :t2
		mov		eax, [esi + t-offset]
		jmp		short :t10
	:t2
		mov		eax, edx
		and		eax, 7
		cmp		eax, cons-tag
		mov		eax, [esi]
		jne		short :t10
		mov		eax, [esi + t-offset]
	:t10
		pop		ebp
		ret
	})

;;;;
;;;;	Common Lisp CHARACTERP function.
;;;;
(defasm characterp (x)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :next1
		callp 	_wrong-number-of-args-error
	:next1
		mov		eax, [ebp + ARGS_OFFSET]		;; eax = argument
		cmp		al, 1
		mov		eax, [esi]
		jne		short :next2
		mov		eax, [esi + t-offset]
	:next2
		pop		ebp
		ret
	})

;;;;
;;;;	Corman Lisp kernel UVECTORP function.
;;;;
(defasm uvectorp (x)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :next1
		callp 	_wrong-number-of-args-error
	:next1
		mov		eax, [ebp + ARGS_OFFSET]		;; eax = argument
		and		eax, 7
		cmp		eax, uvector-tag
		mov		eax, [esi]
		jne		short :next2
		mov		eax, [esi + t-offset]
	:next2
		pop		ebp
		ret
	})

;;;;
;;;;	Common Lisp VALUES function.
;;;;
(defasm values (&rest args)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		push	ecx
		cmp		ecx, 0
		jne		short :t1
		mov		eax, [esi]
		jmp 	short :t5
	:t1
		cmp		ecx, 1
		jne		short :t2
		mov		eax, [ebp + ARGS_OFFSET]
		jmp		short :t5
	:t2
		;; make a list of the passed values is more than 1	
		push	ecx			;; allocate variable at [ebp - 12]
		mov		eax, [esi]
		mov		edi, 0
	:t3
		cmp		edi, [ebp - 12]
		je		short :t4
		push	eax
		callp	cl::%alloc-cons
		pop		[eax]
		mov		ecx, [ebp + edi*4 + ARGS_OFFSET]
		mov		[eax - 4], ecx
		inc		edi
		jmp		short :t3
	:t4
		pop		ecx
		mov		[esi + 8], eax
		mov		eax, [eax - 4]		
	:t5
		pop		edi
		pop		edi
		pop		ebp
		ret	
	})

;;;;
;;;;	Corman Lisp kernel %SYMBOL-GET-FLAGS function.
;;;;
(defasm %symbol-get-flags (sym)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :next1
		callp 	_wrong-number-of-args-error
	:next1
		mov 	eax, [ebp + ARGS_OFFSET]		;; eax = symbol
		callp 	check-symbol
		mov 	eax, [eax + (- (* 4 symbol-constant-offset) uvector-tag)]
		pop		ebp
		ret	
	})

;;;;
;;;;	Corman Lisp kernel %SYMBOL-SET-FLAGS function.
;;;;
(defasm %symbol-set-flags (flags sym)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 2
		jz 		short :next1
		callp 	_wrong-number-of-args-error
	:next1
		mov 	eax, [ebp + ARGS_OFFSET]		;; eax = symbol
		callp 	check-symbol
		mov		ecx, [ebp + (+ ARGS_OFFSET 4)]
		mov 	[eax + (- (* 4 symbol-constant-offset) uvector-tag)], ecx
		mov		eax, ecx
		mov		ecx, 1
		pop		ebp
		ret	
	})

;;;;
;;;;	Common Lisp RPLACA function.
;;;;
(defasm rplaca (cons value)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 2
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		eax, [ebp + (+ ARGS_OFFSET 4)]		;; eax = cons
		mov		edx, eax
		and		edx, 7
		cmp		edx, cons-tag
		je		short :t2
		push	"Not a cons cell: ~A"
		push	eax
		callf	error
	:t2
		mov		edx, [ebp + ARGS_OFFSET]		;; value
		mov		[eax - 4], edx
		mov		ecx, 1
		pop		ebp
		ret
	})

;;;;
;;;;	Corman Lisp internal %RPLACA function. Like RPLACA,
;;;;    but returns the value (rather than the cons).
;;;;
(defasm cl::%rplaca (cons value)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 2
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		edx, [ebp + (+ ARGS_OFFSET 4)]		;; edx = cons
		mov		eax, edx
		and		eax, 7
		cmp		eax, cons-tag
		je		short :t2
		push	"Not a cons cell: ~A"
		push	edx
		callf	error
	:t2
		mov		eax, [ebp + ARGS_OFFSET]		;; value
		mov		[edx - 4], eax
		mov		ecx, 1
		pop		ebp
		ret
	})

;;;
;;; Common Lisp (SETF CAR) function.
;;;
(defsetf car cl::%rplaca)

;;;;
;;;;	Common Lisp RPLACD function.
;;;;
(defasm rplacd (cons value)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 2
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		eax, [ebp + (+ ARGS_OFFSET 4)]		;; eax = cons
		mov		edx, eax
		and		edx, 7
		cmp		edx, cons-tag
		je		short :t2
		push	"Not a cons cell: ~A"
		push	eax
		callf	error
	:t2
		mov		edx, [ebp + ARGS_OFFSET]		;; value
		mov		[eax], edx
		mov		ecx, 1
		pop		ebp
		ret
	})

;;;;
;;;;	Corman Lisp internal %RPLACD function. Like RPLACD,
;;;;    but returns the value (rather than the cons).
;;;;
(defasm cl::%rplacd (cons value)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 2
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = cons
		mov		eax, edx
		and		eax, 7
		cmp		eax, cons-tag
		je		short :t2
		push	"Not a cons cell: ~A"
		push	edx
		callf	error
	:t2
		mov		eax, [ebp + ARGS_OFFSET]		;; value
		mov		[edx], eax
		mov		ecx, 1
		pop		ebp
		ret
	})
;;;
;;; Common Lisp (SETF CDR) function.
;;;
(defsetf cdr cl::%rplacd)

;;;;
;;;;	Corman Lisp kernel FUNCTION-ENVIRONMENT function.
;;;;
(defasm function-environment (func)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov 	eax, [ebp + ARGS_OFFSET]		;; eax = func
		callp 	check-function
		mov 	eax, [eax + (- (* 4 function-environment-offset) uvector-tag)]
		pop		ebp
		ret	
	})

;;;;
;;;;	Corman Lisp kernel UREF-SET function.
;;;;
(defasm uref-set (value uvector index)
	{
		push    ebp                 
		mov     ebp, esp             
		cmp     ecx, 3          
		je      short :t1                
		callp 	_wrong-number-of-args-error
	:t1
		mov		eax, [ebp + (+ ARGS_OFFSET 8)]		;; eax = value
		mov		edx, [ebp + (+ ARGS_OFFSET 4)]		;; edx = uvector
		mov		ecx, [ebp + ARGS_OFFSET]			;; ecx = index
	begin-atomic
		shr		ecx, 1								;; untagged integer
		mov		[edx + ecx - uvector-tag], eax
		mov		ecx, 1
	end-atomic
		pop		ebp
		ret
	})		

;;;;
;;;;	Corman Lisp kernel UREF function.
;;;;
(defasm uref (uvector index)
	{
		push    ebp                 
		mov     ebp, esp             
		cmp     ecx, 2          
		je      short :t1                
		callp 	_wrong-number-of-args-error
	:t1
		mov		edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = uvector
		mov		ecx, [ebp + ARGS_OFFSET]		;; ecx = index
	begin-atomic
		shr		ecx, 1							;; untagged integer
		mov		eax, [edx + ecx - uvector-tag]
		mov		ecx, 1
	end-atomic
		pop		ebp
		ret
	})		

;;;;
;;;;	Common Lisp CHAR-INT function.
;;;;
(defasm char-int (char)
	{
		push    ebp                 
		mov     ebp, esp             
		cmp     ecx, 1          
		je      short :t1                
		callp 	_wrong-number-of-args-error
	:t1
		mov		eax, [ebp + ARGS_OFFSET]
		cmp		al, 1
		je		short :t2
		push	"Invalid character: ~A"
		push	eax
		mov		ecx, 2
		callf	error
	:t2
		shr		eax, 5
		pop		ebp
		ret
	})

(defasm %char-int (char)
	{
		push    ebp                 
		mov     ebp, esp             
		mov		eax, [ebp + ARGS_OFFSET]
		shr		eax, 5
		pop		ebp
		ret
	})

;;;;
;;;;	Corman Lisp kernel INT-CHAR function.
;;;;
(defasm int-char (int)
	{
		push    ebp                 
		mov     ebp, esp             
		cmp     ecx, 1          
		je      short :t1                
		callp 	_wrong-number-of-args-error
	:t1
		mov		eax, [ebp + ARGS_OFFSET]
		test	al, 7
		jz		short :t2
		push	"Invalid integer: ~A"
		push	eax
		mov		ecx, 2
		callf	error
	:t2
		cmp		eax, 0
		jl		short :t3
		cmp		eax, (* #xffff 8)	;;; Unicode mod
;;		cmp		eax, (* #xff 8)
		jg		short :t3
		jmp		short :t4
	:t3
		push	"Cannot convert to character. ~A is out of range."
		push	eax
		mov		ecx, 2
		callf	error
	:t4
		shl		eax, 5
		inc		eax
		pop		ebp
		ret
	})

(defasm %int-char (int)
	{
		push    ebp                 
		mov     ebp, esp             
		mov		eax, [ebp + ARGS_OFFSET]
		shl		eax, 5
		inc		eax
		pop		ebp
		ret
	})

(defasm nth (n list)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 2
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov 	eax, [ebp + ARGS_OFFSET]			;; eax = list
		mov		ecx, [ebp + (+ ARGS_OFFSET 4)]		;; ecx = index
		test	ecx, 7				;; index is an integer?
		jnz		:invalid-index
		test	ecx, ecx			;; index >= 0?
		jge		:t2
	:invalid-index
		push	"Invalid index: ~A"
		push	ecx
		mov		ecx, 2
		callf	error
	:t2
		mov		edx, eax
		and		edx, 7
		cmp		edx, cons-tag		;; not a cons?
		je		:t3
		cmp		eax, [esi]
		je		:t5
		push	"Not a list: ~A"
		push	eax
		mov		ecx, 2
		callf	error
	:t3
		test	ecx, ecx
		jnz		:t4
		mov		eax, [eax - 4]
		jmp		:t5
	:t4
		mov		eax, [eax]
		sub 	ecx, 8
		jmp		:t2		
	:t5
		mov		ecx, 1
		pop		ebp
		ret
	})

(defasm svref (vector index)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 2
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov 	eax, [ebp + (+ ARGS_OFFSET 4)]		;; eax = vector
		mov		ecx, [ebp + ARGS_OFFSET]		;; ecx = index
		test	ecx, 7				;; index is an integer?
		jnz		:invalid-index
		test	ecx, ecx			;; index >= 0?
		jge		short :t2
	:invalid-index
		push	"Invalid index: ~A"
		push	ecx
		mov		ecx, 2
		callf	error
	:t2
		;; now see if it is a simple vector
		mov 	edx, eax
		and 	edx, 7
		cmp 	edx, uvector-tag
		jne 	short :invalid-simple-vector
		mov 	dl, [eax - uvector-tag]
		cmp 	dl, (tag-byte uvector-simple-vector-tag)
		jne 	short :invalid-simple-vector
		jmp		short :t3		
	:invalid-simple-vector
		push	"Invalid simple vector: ~A"
		push	eax
		mov		ecx, 2
		callf	error
	
	:t3
		mov		edx, [eax + (- 4 uvector-tag)] ;edx = vector length
		cmp		ecx, edx		; index >= length
		jge		short :invalid-index

	begin-atomic
		shr		ecx, 1
		mov		eax, [eax + ecx + (- 8 uvector-tag)]
		mov		ecx, 1
	end-atomic
		pop		ebp
		ret
	})

(defasm (setf svref) (value vector index)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 3
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov 	eax, [ebp + (+ ARGS_OFFSET 4)]		;; eax = vector
		mov		ecx, [ebp + ARGS_OFFSET]		;; ecx = index
		test	ecx, 7				;; index is an integer?
		jnz		:invalid-index
		test	ecx, ecx			;; index >= 0?
		jge		short :t2
	:invalid-index
		push	"Invalid index: ~A"
		push	ecx
		mov		ecx, 2
		callf	error
	:t2
		;; now see if it is a simple vector
		mov 	edx, eax
		and 	eax, 7
		cmp 	eax, uvector-tag
		jne 	short :invalid-simple-vector
		mov 	al, [edx - uvector-tag]
		cmp 	al, (tag-byte uvector-simple-vector-tag)
		jne 	short :invalid-simple-vector
		jmp		short :t3		
	:invalid-simple-vector
		push	"Invalid simple vector: ~A"
		push	edx
		mov		ecx, 2
		callf	error
	
	:t3
		mov		eax, [edx + (- 4 uvector-tag)] ;eax = vector length
		cmp		ecx, eax		; index >= length
		jge		short :invalid-index

		mov		eax, [ebp + (+ ARGS_OFFSET 8)]				;eax = value
	begin-atomic
		shr		ecx, 1
		mov		[edx + ecx + (- 8 uvector-tag)], eax
		mov		ecx, 1
	end-atomic
		pop		ebp
		ret
	})

(defasm %svref (vector index)
	{
		push	ebp
		mov		ebp, esp
		mov 	edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = vector
		mov		ecx, [ebp + ARGS_OFFSET]		;; ecx = index
	begin-atomic
		shr		ecx, 1							;; untagged integer
		mov		eax, [edx + ecx + (uvector-offset 2)]
	end-atomic
		mov		ecx, 1
		pop		ebp
		ret
	})

(defasm (setf %svref) (val vector index)
	{
		push	ebp
		mov		ebp, esp
		mov 	eax, [ebp + (+ ARGS_OFFSET 8)]	;; eax = val
		mov 	edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = vector
		mov		ecx, [ebp + ARGS_OFFSET]		;; ecx = index
	begin-atomic
		shr		ecx, 1							;; untagged integer
		mov		[edx + ecx + (uvector-offset 2)], eax
		mov		ecx, 1
	end-atomic
		pop		ebp
		ret
	})

(defasm %svchar (vector index)
	{
		push	ebp
		mov		ebp, esp
		mov 	edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = vector
		mov		ecx, [ebp + ARGS_OFFSET]		;; ecx = index
	begin-atomic
		shr		ecx, 2							;; untagged integer
		xor		eax, eax
		mov		ax, [edx + ecx + (uvector-offset 2)]
		mov		ecx, 1
	end-atomic
		shl		eax, 8
		inc		eax
		pop		ebp
		ret
	})

(defasm (setf %svchar) (char vector index)
	{
		push	ebp
		mov		ebp, esp
		mov 	eax, [ebp + (+ ARGS_OFFSET 8)]	;; eax = char
		mov 	edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = vector
		mov		ecx, [ebp + ARGS_OFFSET]		;; ecx = index
	begin-atomic
		shr		ecx, 2
		rol		eax, 24
		mov		[edx + ecx + (uvector-offset 2)], ax
		rol		eax, 8
		mov		ecx, 1
	end-atomic
		pop		ebp
		ret
	})

(defasm %svbyte (vector index)
	{
		push	ebp
		mov		ebp, esp
		mov 	edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = vector
		mov		ecx, [ebp + ARGS_OFFSET]		;; ecx = index
		xor		eax, eax
	begin-atomic
		shr		ecx, 3
		mov		al, [edx + ecx + (uvector-offset 2)]
		mov		ecx, 1
	end-atomic
		shl		eax, 3
		pop		ebp
		ret
	})

(defasm (setf %svbyte) (byte vector index)
	{
		push	ebp
		mov		ebp, esp
		mov 	eax, [ebp + (+ ARGS_OFFSET 8)]	;; eax = byte
		mov 	edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = vector
		mov		ecx, [ebp + ARGS_OFFSET]		;; ecx = index
	begin-atomic
		shr		ecx, 3
		shr		eax, 3
		mov		[edx + ecx + (uvector-offset 2)], al
		shl		eax, 3
		mov		ecx, 1
	end-atomic
		pop		ebp
		ret
	})

;;;
;;;	For a simple bit vector, the bit at index n is returned.
;;;
(defasm %svbit (vector index)
	{
		push	ebp
		mov		ebp, esp
		mov 	edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = vector
		mov		ecx, [ebp + ARGS_OFFSET]		;; ecx = index
		xor		eax, eax
	begin-atomic
		shr		ecx, 3
		bt		[edx + (uvector-offset 2)], ecx
		mov		ecx, 1
	end-atomic
		setc    al
		shl		eax, 3
		pop		ebp
		ret
	})

(defasm (setf %svbit) (bit vector index)
	{
		push	ebp
		mov		ebp, esp
		mov 	eax, [ebp + (+ ARGS_OFFSET 8)]	;; eax = bit
		mov 	edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = vector
		mov		ecx, [ebp + ARGS_OFFSET]		;; ecx = index
	begin-atomic
		shr		ecx, 3
		test	al, 8
		jz		short :clear
	:set
		bts		[edx + (uvector-offset 2)], ecx
		jmp		short :done
	:clear
		btr		[edx + (uvector-offset 2)], ecx
	:done
		mov		ecx, 1
	end-atomic
		pop		ebp
		ret
	})

(defasm %svdouble (vector index)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		mov		ecx, 0
		callf	cl::alloc-double-float			;; eax = new double float
		mov 	edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = vector
		mov		ecx, [ebp + ARGS_OFFSET]		;; ecx = index
	begin-atomic
		mov		edi, [edx + ecx + (uvector-offset 2)]
		mov		[eax + (uvector-offset 2)], edi
		mov		edi, [edx + ecx + (uvector-offset 3)]
		mov		[eax + (uvector-offset 3)], edi
	end-atomic
		mov		ecx, 1
		pop		edi
		pop		ebp
		ret
	})

(defasm (setf %svdouble) (double vector index)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		mov 	eax, [ebp + (+ ARGS_OFFSET 8)]	;; eax = double
		mov 	edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = vector
		mov		ecx, [ebp + ARGS_OFFSET]		;; ecx = index
	begin-atomic
		mov		edi, [eax + (uvector-offset 2)]
		mov		[edx + ecx + (uvector-offset 2)], edi
		mov		edi, [eax + (uvector-offset 3)]
		mov		[edx + ecx + (uvector-offset 3)], edi
	end-atomic
		mov		ecx, 1
		pop		edi
		pop		ebp
		ret
	})

(defasm %svsingle (vector index)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		mov		ecx, 0
		callf	cl::alloc-single-float			;; eax = new single float
		mov 	edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = vector
		mov		ecx, [ebp + ARGS_OFFSET]		;; ecx = index
	begin-atomic
		shr		ecx, 1							;; untagged integer
		mov		edi, [edx + ecx + (uvector-offset 2)]
		mov		[eax + (uvector-offset 1)], edi
		mov		ecx, 1
	end-atomic
		pop		edi
		pop		ebp
		ret
	})

(defasm (setf %svsingle) (single vector index)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		mov 	eax, [ebp + (+ ARGS_OFFSET 8)]	;; eax = single
		mov 	edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = vector
		mov		ecx, [ebp + ARGS_OFFSET]		;; ecx = index
	begin-atomic
		shr		ecx, 1							;; untagged integer
		mov		edi, [eax + (uvector-offset 1)]
		mov		[edx + ecx + (uvector-offset 2)], edi
		mov		ecx, 1
	end-atomic
		pop		edi
		pop		ebp
		ret
	})

(defasm %svshort (vector index)
	{
		push	ebp
		mov		ebp, esp
		mov 	edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = vector
		mov		ecx, [ebp + ARGS_OFFSET]		;; ecx = index
	begin-atomic
		shr		ecx, 2							;; untagged integer
		xor		eax, eax
		mov		ax, [edx + ecx + (uvector-offset 2)]
		shl		eax, 16
		sar		eax, 13							;; create signed fixnum
		mov		ecx, 1
	end-atomic
		pop		ebp
		ret
	})

(defasm (setf %svshort) (char vector index)
	{
		push	ebp
		mov		ebp, esp
		mov 	eax, [ebp + (+ ARGS_OFFSET 8)]	;; eax = char
		mov 	edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = vector
		mov		ecx, [ebp + ARGS_OFFSET]		;; ecx = index
	begin-atomic
		shr		ecx, 2							;; untagged integer
		shr		eax, 3
		mov		[edx + ecx + (uvector-offset 2)], ax
		shl		eax, 3
		mov		ecx, 1
	end-atomic
		pop		ebp
		ret
	})

;;;
;;;	Corman Lisp %GET-SYSTEM-GLOBAL function.
;;;	Usage: 		(%GET-SYSTEM-GLOBAL index)
;;;	Returns:	The Lisp object at the requested index in the global
;;;				system internals array.
;;;
(defvar system-internals cl::*system-internals*)	;; assembler doesn't like * in symbol names
(defasm ccl::%get-system-global (index)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		eax, [ebp + ARGS_OFFSET]		;; eax = argument
		mov		edx, 'system-internals
		mov		edx, [edx + (uvector-offset cl::symbol-value-offset)]	; edx = (foreign ptr)
		mov		edx, [edx - cons-tag]									; edx = foreign ptr
		mov		edx, [edx + (uvector-offset foreign-heap-ptr-offset)]	; edx = ptr
		shr		eax, 1
		mov		eax, [edx + eax]
		mov		ecx, 1
		pop		ebp
		ret
	})

(defasm cl::%read-char (s)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		push	ebx
		mov		edi, [ebp + ARGS_OFFSET]	;; edi = stream
		mov		eax, [edi + (uvector-offset cl::stream-input-buffer-pos-offset)]
		cmp		eax, [edi + (uvector-offset cl::stream-input-buffer-num-offset)]
		jne		short :continue
		push	[edi + (uvector-offset cl::stream-underflow-func-offset)]
		push	edi
		mov		ecx, 2
		callf	funcall
		add		esp, 8
		mov		edi, [ebp + ARGS_OFFSET]	;; edi = stream
		mov		eax, [edi + (uvector-offset cl::stream-input-buffer-pos-offset)]
		cmp		eax, [edi + (uvector-offset cl::stream-input-buffer-num-offset)]
		jne		short :continue
		mov		eax, [esi]
		jmp 	short :exit
	:continue
		mov		ebx, [edi + (uvector-offset cl::stream-input-buffer-offset)]	; ebx = buffer
		mov		edx, [edi + (uvector-offset cl::stream-input-buffer-pos-offset)]; edx = index
		shr		edx, 2		;; unicode mod
;;		shr		edx, 3
		xor		eax, eax
		mov		ax, [ebx + edx + (- 8 uvector-tag)]	;; unicode mod
;;		mov		al, [ebx + edx + (- 8 uvector-tag)]
		cmp		eax, 10							;; ascii newline?
		jne		:t1
		mov		ecx, [edi + (uvector-offset cl::stream-line-number-offset)]
		add		ecx, 8
		mov		[edi + (uvector-offset cl::stream-line-number-offset)], ecx
	:t1
		shl		eax, 8
		inc		eax								;; convert to a character
		mov		ebx, [edi + (uvector-offset cl::stream-input-buffer-pos-offset)]
		mov		ecx, [edi + (uvector-offset cl::stream-position-offset)]
		add		ebx, 8
		add		ecx, 8
		mov		[edi + (uvector-offset cl::stream-input-buffer-pos-offset)], ebx
		mov		[edi + (uvector-offset cl::stream-position-offset)], ecx
	:exit
		mov		ecx, 1
		pop		ebx
		pop		edi
		mov		esp, ebp
		pop		ebp
		ret
	})

(pl:defasm cl::%read-char-with-error (stream)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		push	ebx
		mov		edi, [ebp + ARGS_OFFSET]	;; edi = stream
		mov		eax, [edi + (uvector-offset cl::stream-input-buffer-pos-offset)]
		cmp		eax, [edi + (uvector-offset cl::stream-input-buffer-num-offset)]
		jne		short :continue
		push	[edi + (uvector-offset cl::stream-underflow-func-offset)]
		push	edi
		mov		ecx, 2
		callf	funcall
		add		esp, 8
		mov		edi, [ebp + ARGS_OFFSET]	;; edi = stream
		mov		eax, [edi + (uvector-offset cl::stream-input-buffer-pos-offset)]
		cmp		eax, [edi + (uvector-offset cl::stream-input-buffer-num-offset)]
		jne		short :continue
		push	"Unexpected end of file encountered in stream ~A"
		push	edi
		mov		ecx, 2
		callf	error
	:continue
		mov		ebx, [edi + (uvector-offset cl::stream-input-buffer-offset)]	; ebx = buffer
		mov		edx, [edi + (uvector-offset cl::stream-input-buffer-pos-offset)]; edx = index
		shr		edx, 2		;; unicode mod
;;		shr		edx, 3
		xor		eax, eax
		mov		ax, [ebx + edx + (- 8 uvector-tag)]	;; unicode mod
;;		mov		al, [ebx + edx + (- 8 uvector-tag)]
		cmp		eax, 10							;; ascii newline?
		jne		:t1
		mov		ecx, [edi + (uvector-offset cl::stream-line-number-offset)]
		add		ecx, 8
		mov		[edi + (uvector-offset cl::stream-line-number-offset)], ecx
	:t1
		shl		eax, 8
		or		eax, 1							;; convert to a character
		mov		ebx, [edi + (uvector-offset cl::stream-input-buffer-pos-offset)]
		mov		ecx, [edi + (uvector-offset cl::stream-position-offset)]
		add		ebx, 8
		add		ecx, 8
		mov		[edi + (uvector-offset cl::stream-input-buffer-pos-offset)], ebx
		mov		[edi + (uvector-offset cl::stream-position-offset)], ecx
	:exit
		mov		ecx, 1
		pop		ebx
		pop		edi
		mov		esp, ebp
		pop		ebp
		ret
	})

(defasm ccl:peek-byte (addr)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		edx, [ebp + ARGS_OFFSET]
		test	edx, 7					;; fixnum?
		jnz		short :t2
		xor		eax, eax
		begin-atomic
		shr		edx, 3
		mov		al, [edx]
		shl		eax, 3		;; tag fixnum result
		xor		edx, edx
		end-atomic
		jmp		short :done
	:t2	;; look for the first word of a bignum
		mov		eax, edx
		and		eax, 7
		cmp		eax, uvector-tag
		jne		short :not-int-err
		mov 	eax, [edx - uvector-tag]
		and		eax, #xf8
		cmp 	eax, (* uvector-bignum-tag 8)
		jne 	short :not-int-err
		begin-atomic
		mov		edx, [edx + (uvector-offset cl::bignum-first-cell-offset)]
		mov		al, [edx]
		shl		eax, 3		;; tag fixnum result
		xor		edx, edx
		end-atomic
		jmp		short :done
	:not-int-err
		push	"Not an integer: ~A"
		push	edx
		mov		ecx, 2
		callf	error
	:done
		pop		ebp
		ret
	})


(defasm ccl:peek-dword (addr)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		edx, [ebp + ARGS_OFFSET]
		test	edx, 7					;; fixnum?
		jnz		short :t2
		begin-atomic
		shr		edx, 3
		mov		eax, [edx]
		test	eax, #xf0000000
		jne		:bignum1
		shl		eax, 3
		end-atomic
		jmp		:done
	:bignum1
		xor		edx, edx		;; zero out untagged data
		mov		eax, edx
		end-atomic
		push	8
		mov		ecx, 1
		callf	cl::alloc-bignum
		add		esp, 4
		mov		edx, [ebp + ARGS_OFFSET]
		begin-atomic
		shr		edx, 3
		mov		ecx, [edx]
		mov		[eax + (uvector-offset cl::bignum-first-cell-offset)], ecx
		xor		edx, edx
		mov		ecx, 1
		end-atomic
		jmp		short :done

	:t2	;; look for the first word of a bignum
		mov		eax, edx
		and		eax, 7
		cmp		eax, uvector-tag
		jne		short :not-int-err
		mov 	eax, [edx - uvector-tag]
		and		eax, #xf8
		cmp 	eax, (* uvector-bignum-tag 8)
		jne 	short :not-int-err
		begin-atomic
		mov		edx, [edx + (uvector-offset cl::bignum-first-cell-offset)]
		mov		eax, [edx]
		test	eax, #xf0000000
		jne		:bignum2
		shl		eax, 3
		end-atomic
		jmp		short :done
	:bignum2
		xor		edx, edx		;; zero out untagged data
		mov		eax, edx
		end-atomic
		push	8
		mov		ecx, 1
		callf	cl::alloc-bignum
		add		esp, 4
		mov		edx, [ebp + ARGS_OFFSET]
		begin-atomic
		mov		edx, [edx + (uvector-offset cl::bignum-first-cell-offset)]
		mov		ecx, [edx]
		mov		[eax + (uvector-offset cl::bignum-first-cell-offset)], ecx
		xor		edx, edx
		mov		ecx, 1
		end-atomic
		jmp		short :done
	:not-int-err
		push	"Not an integer: ~A"
		push	edx
		mov		ecx, 2
		callf	error
	:done
		pop		ebp
		ret
	})

(defasm ccl:get-qv-reg (addr)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 0
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		test	esi, #xf0000000
		jne		:bignum
	
		begin-atomic
		mov		eax, esi
		shl		eax, 3
		end-atomic
		jmp		short :done
	:bignum
		push	8
		mov		ecx, 1
		callf	cl::alloc-bignum
		add		esp, 4
		begin-atomic
		mov		[eax + (uvector-offset cl::bignum-first-cell-offset)], esi
		end-atomic
	:done
		mov		ecx, 1
		pop		ebp
		ret
	})

(defasm ccl:peek-lisp-object (addr)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		edx, [ebp + ARGS_OFFSET]
		test	edx, 7					;; fixnum?
		jnz		short :t2
		begin-atomic
		shr		edx, 3
		mov		eax, [edx]
		xor		edx, edx
		end-atomic
		jmp		:done

	:t2	;; look for the first word of a bignum
		mov		eax, edx
		and		eax, 7
		cmp		eax, uvector-tag
		jne		short :not-int-err
		mov 	eax, [edx - uvector-tag]
		and		eax, #xf8
		cmp 	eax, (* uvector-bignum-tag 8)
		jne 	short :not-int-err
		begin-atomic
		mov		edx, [edx + (uvector-offset cl::bignum-first-cell-offset)]
		mov		eax, [edx]
		xor		edx, edx
		end-atomic
		jmp		short :done
	:not-int-err
		push	"Not an integer: ~A"
		push	edx
		mov		ecx, 2
		callf	error
	:done
		pop		ebp
		ret
	})

;;
;;	Assumes the passed arg is a uvector, does no arg checking.
(defasm cl::uvector-type-bits (uvec)
	{
		push	ebp
		mov		ebp, esp
		mov		eax, [ebp + ARGS_OFFSET]
		mov		eax, [eax - uvector-tag]
		and		eax, #xf8
		mov		ecx, 1
		pop		ebp
		ret
	})

;;;
;;;	Get the unsigned integer representation of any lisp object.
;;;
(defasm ccl::lisp-object-bits (obj)
	{
		push	ebp
		mov		ebp, esp
		mov		eax, [ebp + ARGS_OFFSET]
		test	eax, #xf0000000
		jne		:bignum2
		shl		eax, 3
		jmp		short :done
	:bignum2
		push	8
		mov		ecx, 1
		callf	cl::alloc-bignum
		add		esp, 4
		mov		edx, [ebp + ARGS_OFFSET]
		mov		[eax + (uvector-offset cl::bignum-first-cell-offset)], edx
	:done
		mov		ecx, 1
		pop		ebp
		ret
	})

(defun cl::%uvector-address (obj)
	(logand (ccl::lisp-object-bits obj) (lognot 7)))	;; trim off tag bits 

;;;;
;;;;	Common Lisp VECTORP function.
;;;;
(defasm vectorp (x)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		edx, [ebp + ARGS_OFFSET]		;; edx = argument
		mov		eax, edx
		and		eax, 7
		cmp		eax, uvector-tag
		jne		short :exit_nil
		mov		eax, edx
		mov 	edx, [edx - uvector-tag]
		cmp 	dl, (tag-byte uvector-simple-vector-tag)
		jb 		short :check_array
		cmp 	dl, (tag-byte uvector-simple-single-float-vector-tag)
		ja 		short :check_array
	:exit_t
		mov		eax, [esi + t-offset]
		jmp		short :exit
	:check_array
		cmp		dl, (tag-byte uvector-array-tag)
		jne		short :exit_nil
		mov		eax, [eax + (uvector-offset cl::adjustable-array-dimensions-offset)]
		cmp		eax, 8		;; dimensions == wrapInteger(1)?
		je		short :exit_t
	:exit_nil
		mov		eax, [esi]
	:exit
		pop		ebp
		ret
	})

;;;
;;; Common Lisp ARRAY-RANK function.
;;;
(defasm array-rank (array)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		edx, [ebp + ARGS_OFFSET]		;; edx = argument
		mov		eax, edx
		and		eax, 7
		cmp		eax, uvector-tag
		je		short :t2
	:not-array
		push	"Not an array: ~S"
		push	edx
		mov		ecx, 2
		callp	error	;; doesn't return
	:t2
		mov 	eax, [edx - uvector-tag]
		cmp 	al, (tag-byte uvector-array-tag)
		jne		short :t3
		mov		eax, [edx + (uvector-offset cl::adjustable-array-dimensions-offset)]
		jmp		short :done
	:t3
		cmp		al, (tag-byte cl::uvector-simple-vector-tag)
		jb		short :not-array
		cmp		al, (tag-byte cl::uvector-simple-single-float-vector-tag)
		ja		short :not-array
		mov		eax, 8				;; eax = wrapped 1
	:done
		pop		ebp
		ret
	})

;;;
;;; Common Lisp ARRAY-DIMENSION function.
;;;
(defasm array-dimension (array dim)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 2
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		edx, [ebp + (+ ARGS_OFFSET 4)]		;; edx = argument
		mov		eax, edx
		and		eax, 7
		cmp		eax, uvector-tag
		je		short :t2
	:not-array
		push	"Not an array: ~S"
		push	edx
		mov		ecx, 2
		callf	error	;; doesn't return
	:invalid-dimension
		push	"Invalid dimension argument: ~S"
		push	eax
		mov		ecx, 2
		callf	error
	:t2
		mov 	eax, [edx - uvector-tag]
		cmp 	al, (tag-byte uvector-array-tag)
		jne		short :t3
		mov		ecx, [edx + (uvector-offset cl::adjustable-array-dimensions-offset)] ; ecx = num dimensions
		mov		eax, [ebp + ARGS_OFFSET]
		cmp		eax, ecx
		jge		short :invalid-dimension
		shr		eax, 1
		mov		eax, [edx + eax + (uvector-offset cl::adjustable-array-dim1-offset)]
		jmp		short :done
	:t3
		cmp		al, (tag-byte cl::uvector-simple-vector-tag)
		jb		short :not-array
		cmp		al, (tag-byte cl::uvector-simple-single-float-vector-tag)
		ja		short :not-array
		mov		eax, [ebp + ARGS_OFFSET]
		cmp		eax, 0
		jne		short :invalid-dimension
		mov		eax, [edx + (uvector-offset 1)]
	:done
		mov		ecx, 1
		pop		ebp
		ret
	})

(defconstant qv-multiple-return-values-index 2)
(defconstant qv-finalization-registry-index  3)
(defconstant qv-weak-ptr-registry-index      4)
(defconstant qv-thread-heap-index            5)
(defconstant qv-thread-heap-end-index        6)

(defasm cl::%push-special-bindings (&rest pairs)
	{
		push	ebp
		mov		ebp, esp
		push	ebx
		push	edi
		mov     ebx, ecx
	:loop
		callp	cl::%alloc-cons
		mov		edx, [ebp + ebx*4 + 4]		;; edx = sym
		mov		edx, [edx + (uvector-offset symbol-var-table-offset)]
		shr		edx, 3			;; edx = untagged integer offset of var table entry
		mov		edi, [esi+edx*4] ;; edi = existing binding
		mov		[eax], edi
		mov		edi, [ebp + ebx*4 + 0]
		mov		[eax - 4], edi	;; push new value	
		mov		[esi+edx*4], eax
		sub		ebx, 2
		jg		short :loop
		mov		ecx, 1
		mov		eax, [esi]		;; return NIL
		pop		edi
		pop		ebx
		pop		ebp
		ret
	})

(defasm cl::%pop-special-bindings (varlist)
	{
		push	ebp
		mov		ebp, esp
		mov		edx, [ebp + ARGS_OFFSET]		;; edx = varlist
	:loop
		cmp		edx, [esi]
		je		short :done
		mov		eax, [edx - 4]					;; eax = sym
		mov		eax, [eax + (uvector-offset symbol-var-table-offset)]
		shr		eax, 3				;; eax = untagged integer offset of var table entry
		mov		ecx, [esi+eax*4] 	;; ecx = existing binding
		mov		ecx, [ecx]			;; ecx = cdr(ecx)
		mov		[esi+eax*4], ecx
		mov		edx, [edx]			;; edx = cdr(edx)
		jmp		short :loop
	:done
		mov		ecx, 1
		mov		eax, [esi]
		pop		ebp
		ret
	})

(defasm cl::char-upcase (char)
    {
        push    ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :next1
		callp 	_wrong-number-of-args-error
	:next1
		mov		eax, [ebp + ARGS_OFFSET]		;; eax = argument
		cmp		al, 1
        je     short :next2
        push    eax
        mov     ecx, 1
        callf   _not-a-character-error
    :next2
        shl     eax, 8
        shr     eax, 16         ;; char in low 16 bits
        cmp     eax, 97         ;; char = 'a'
        jl      short :done
        cmp     eax, (+ 97 25)  ;; char = 'z'
        jg      short :done
        sub     eax, 32
    :done
        shl     eax, 8
        inc     eax
        pop     ebp
        ret
    })

(defasm cl::char-downcase (char)
    {
        push    ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :next1
		callp 	_wrong-number-of-args-error
	:next1
		mov		eax, [ebp + ARGS_OFFSET]		;; eax = argument
		cmp		al, 1
        je     short :next2
        push    eax
        mov     ecx, 1
        callf   _not-a-character-error
    :next2
        shl     eax, 8
        shr     eax, 16         ;; char in low 16 bits
        cmp     eax, 65         ;; char = 'A'
        jl      short :done
        cmp     eax, (+ 65 25)  ;; char = 'Z'
        jg      short :done
        add     eax, 32
    :done
        shl     eax, 8
        inc     eax
        pop     ebp
        ret
    })

;;
;; Assume element is a character, and vec is a simple character vector
;;
(defasm cl::initialize-simple-character-vector (vec element)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + ARGS_OFFSET]
        and     eax, #x00ffff00
        shr     eax, 8
        mov     edx, [ebp + (+ ARGS_OFFSET 4)]
    begin-atomic
        mov     ecx, eax
        shl     ecx, 16
        or      eax, ecx                        ;; store two chars in eax
        mov     ecx, [edx + (uvector-offset 1)] ;; ecx = tagged length 
        add     ecx, 8       
        shr     ecx, 4                          ;; (untagged length + 1) / 2
        dec     ecx
    :loop
        cmp     ecx, 0
        jl      short :exit
        mov     [edx + ecx*4 + (uvector-offset 2)], eax
        dec     ecx
        jmp     short :loop
    :exit
    end-atomic
        mov     eax, [esi]
        mov     ecx, 1
        pop     ebp
        ret
    })

#|
I am commenting this out for now because it only adds the function
for the current thread's QV, but that binding is not shared by other threads.
Therefore it does not work correctly. Probably the way to correct it is to add a level of
indirection (a binding) in the finalization slot of QV.
-RGC  1/11/07

(defasm cl::register-finalization (obj func)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + (+ ARGS_OFFSET 4)]   ;; push obj
        push    [ebp + (+ ARGS_OFFSET 0)]   ;; func
        mov     ecx, 2
        callf   cons
        add     esp, 8
        push    eax
        push    [esi + (* qv-finalization-registry-index 4)]
        mov     ecx, 2
        callf   cons
        add     esp, 8
        mov     [esi + (* qv-finalization-registry-index 4)], eax
        mov     eax, [ebp + (+ ARGS_OFFSET 4)] ;; return obj
        mov     ecx, 1
        pop     ebp
        ret
    })
|#

