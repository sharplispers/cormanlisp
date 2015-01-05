;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;; fast-class-of.lisp -- Fast CLASS-OF reimplementation for Corman Lisp.
;;;; This should be loaded AFTER Closette is loaded.  Benchmarks show
;;;; generic function call speed-up by a factor of 2 and slot access
;;;; speed-up by a factor of 3.  NOTE: class metaobjects are captured in
;;;; variables and therefore must not be replaced after this is loaded.

;;;; Portions Copyright (C) 1999 Vassili Bykov.

;;;; Author:       Vassili Bykov <vassili@objectpeople.com>
;;;; Created:      01/21/1999
;;;; Last updated: 01/21/1999
;;;;
;;;; 			   01/23/1999  Roger Corman  Modified to support changes
;;;;						   in version 1.2, and replaces some numeric
;;;;						   constants with symbolic names.
;;;;			   12/06/1999  Added Ratio class.
;;;;			   02/15/2001  Changed the way a structure's class is looked up.
;;;;

(in-package :common-lisp)

(defvar %*magic-class-table*
  (vector
   (find-class 'function)				; 00000
   (find-class 'function)				; 00001
   nil									; 00010 - a structure, never looked up
   (find-class 'array)					; 00011
   nil									; 00100 - a symbol, never looked up
   (find-class 'stream)					; 00101
   (find-class 'float)					; 00110
   (find-class 'package)				; 00111
   (find-class 'hash-table)				; 01000
   (find-class t)						; 01001 - foreign pointer
   (find-class t)						; 01010 - compiled code
   (find-class 'readtable)				; 01011
   (find-class 'complex)				; 01100
   (find-class 'ratio)					; 01101 - ratio
   (find-class 'integer)				; 01110 - bignum
   (find-class t)						; 01111 - foreign heap pointer
   (find-class t)						; 10000 - weak pointer
   (find-class 'vector)					; 10001 - simple vector
   (find-class 'string)					; 10010
   (find-class 'vector)					; 10011 - simple byte vector
   (find-class 'vector)					; 10100 - simple short vector
   (find-class 'vector)					; 10101 - simple double float vector
   (find-class 'bit-vector)				; 10110
   (find-class 'vector)					; 10111 - simple single float vector
   (find-class 'float)					; 11000 - single float
   :reserved							; 11001
   :reserved							; 11010
   :reserved							; 11011
   :reserved							; 11100
   :reserved							; 11101
   :reserved							; 11110
   :reserved							; 11111
   (find-class 'cons)					; dec 32
   (find-class 'integer)				; dec 33
   (find-class 'character)				; dec 34
   (find-class 'float)))				; dec 35

(defun %class-from-magic-cookie (cookie)
  (declare (optimize (speed 3) (safety 0))
		   (type fixnum cookie))
  (svref %*magic-class-table* cookie))

(defvar %t-class (find-class 't))
(defvar %null-class (find-class 'null))
(defvar %symbol-class (find-class 'symbol))

(ccl:defasm class-of (object)
  {
    push  ebp
	mov   ebp, esp
	cmp   ecx, 1
	jz 	  short :args-ok
	callp _wrong-number-of-args-error
  :args-ok
    ;; See what kind of an object we are looking at--generate a magic cookie
    ;; which is either the type ID field of the object's uvector, or some
    ;; other value for immediates and a cons.  The cookie is a machine
    ;; integer, not a Lisp value.
    mov   edx, [ebp + ARGS_OFFSET]
	mov   eax, edx
	and   eax, 7
	cmp   eax, uvector-tag
	jz    short :uvector-tag
	cmp   eax, x86::cons-tag
	jz    short :cons-tag
	cmp   eax, 0
	jz    short :fixnum-tag
	cmp   eax, 1
	jz    short :char-tag
	cmp   eax, 3
	jz    short :short-float-tag
	cmp   eax, 7
	jz    short :short-float-tag
	;; Should never fall through the above, but just in case...
	mov   eax, "Wrong object in CLASS-OF"
	push  eax
	mov   ecx, 1
	callf error
	add   esp, 4
	pop   ebp
	ret
  :uvector-tag
    mov   eax, [edx - uvector-tag]
	shr   eax, 3
	and   eax, #b11111
	jmp   short :got-a-cookie
  :cons-tag
    mov   eax, 32
	jmp   short :got-a-cookie
  :fixnum-tag
    mov   eax, 33
	jmp   short :got-a-cookie
  :char-tag
    mov   eax, 34
	jmp   short :got-a-cookie
  :short-float-tag	
    mov   eax, 35
  :got-a-cookie
	;; The cookie is now in EAX
	cmp   eax, cl::uvector-clos-instance-tag; instance tag
	jz	  :instance
	cmp   eax, cl::uvector-structure-tag; structure tag
	jz    short :structure				; structures are tougher
	cmp   eax, cl::uvector-symbol-tag	; symbol tag
	jz    short :symbol					; so are symbols
	;; None of the above -- lookup the class in the cookie table.
	shl   eax, 3						; make a cookie a fixnum
	push  eax							; CX is already 1
	callf cl::%class-from-magic-cookie
	add   esp, 4
	jmp   short :exit
  :structure
 	mov	  eax, [ebp + ARGS_OFFSET]
	mov   eax, [eax + (uvector-offset 1)]  ;; eax = structure definition vector
	mov	  eax, [eax + (uvector-offset (+ 2 cl::struct-template-class-offset))] ;; (elt template 0)
	jmp   short :exit
  :st1
	mov   eax, [eax + (- 8 uvector-tag)] ; first STD-INSTANCE slot
	jmp   short :exit
  :instance
	mov	  eax, [ebp + ARGS_OFFSET]
	mov   eax, [eax + (uvector-offset cl::clos-instance-class-offset)]
	jmp	  short :exit
  :symbol
    ;; If this is NIL, return class NULL. Otherwise say it is a SYMBOL.
	mov   eax, [ebp + ARGS_OFFSET]
	cmp   eax, [esi]
	jz    short :sy1
    mov   eax, 'cl::%symbol-class
	jmp   short :get-symbol-value-and-exit
    :sy1
	mov   eax, 'cl::%null-class
  :get-symbol-value-and-exit
	push  eax
	mov   ecx, 1
	callp cl::symbol-value
	add   esp, 4
  :exit
	pop   ebp
	ret
  })
	

