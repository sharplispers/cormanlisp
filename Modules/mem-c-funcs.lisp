;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	All rights reserved.
;;;;	-------------------------------
;;;;
;;;;	File:		mem-c-funcs.lisp
;;;;	Contents:	Implementations of C memory manipulation functions.
;;;;	History:	8/11/99  RGC  Created.
;;;;

(in-package :ct)

(export '(MEMSET MEMCPY))

;; C memcpy function
;; The speed could be improved by making sure the 4-byte at a time
;; loop operated at DWORD boundaries.
;;
(defasm ct::%memcpy (dest src count)
	{
		push	ebp
		mov		ebp, esp
		push	esi
		push	edi
		mov		edi, [ebp + (+ ARGS_OFFSET 8)]	;; edi = dest
		mov		esi, [ebp + (+ ARGS_OFFSET 4)]	;; esi = src
		mov		ecx, [ebp + (+ ARGS_OFFSET 0)]	;; ecx = count
		shr		ecx, 3							;; ecx = unwrapped count
	:loop1										;; copy 4 bytes at a time
		cmp		ecx, 3
		jle		short :loop2
		mov		eax, [esi]
		mov		[edi], eax
		add		esi, 4
		add		edi, 4
		sub		ecx, 4
		jmp		short :loop1
	:loop2										;; copy last 0-3 bytes
		cmp		ecx, 0
		je		short :exit
		mov		al, [esi]
		mov		[edi], al
		inc		esi
		inc		edi
		dec		ecx
		jmp		short :loop2
	:exit
		mov		eax, [ebp + (+ ARGS_OFFSET 8)]	;; return dest
		mov		ecx, 1
		pop		edi
		pop		esi
		mov		esp, ebp
		pop		ebp
		ret
	})
			
(defun ct:memcpy (dest src count)
	(unless (ct:cpointerp dest)
		(error "Not a foreign pointer: ~A" dest))
	(unless (ct:cpointerp src)
		(error "Not a foreign pointer: ~A" src))
	(unless (fixnump count)	
		(error "Third argument to memcpy must be a fixnum, got ~A" count))
	(%memcpy (uref dest 1)(uref src 1) count)
	dest)

;; C memset function
;; The speed could be improved by making sure the 4-byte at a time
;; loop operated at DWORD boundaries.
;;
(defasm ct::%memset (dest c count)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		xor		eax, eax
		mov		edi, [ebp + (+ ARGS_OFFSET 8)]	;; edi = dest
		mov		eax,  [ebp + (+ ARGS_OFFSET 4)]	;; eax  = c
		shr		eax, 3							;; eax = unwrapped c
		mov		ecx, [ebp + (+ ARGS_OFFSET 0)]	;; ecx = count
		shr		ecx, 3							;; ecx = unwrapped count
	:loop										;; copy 4 bytes at a time
		cmp		ecx, 0
		je		short :exit
		mov		[edi], al
		inc		edi
		dec		ecx
		jmp		short :loop
	:exit
		mov		eax, [ebp + (+ ARGS_OFFSET 8)]	;; return dest
		mov		ecx, 1
		pop		edi
		mov		esp, ebp
		pop		ebp
		ret
	})
			
(defun ct:memset (dest c count)
	(unless (ct:cpointerp dest)
		(error "Not a foreign pointer: ~A" dest))
	(unless (fixnump c)
		(error "Not a fixnum: ~A" c))
	(unless (fixnump count)	
		(error "Third argument to memcpy must be a fixnum, got ~A" count))
	(%memset (uref dest 1) c count)
	dest)


(provide "MEM-C-FUNCS")
