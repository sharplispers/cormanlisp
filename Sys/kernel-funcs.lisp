;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		kernel-funcs.lisp
;;;;	Contents:	Functions which replace kernel functions.
;;;;	History:	11/4/98  RGC  Created.
;;;;
(in-package "COMMON-LISP")

(defun %create-closure (func environment)
	(let ((closure (alloc-uvector 2 uvector-function-tag)))
		(setf (uref closure function-environment-offset) environment)
		(setf (uref closure function-code-buffer-offset)
			(uref func function-code-buffer-offset))
		closure))

(set-dispatch-macro-character #\# #\| 
	#'(lambda (stream char int)
		(declare (ignore char int))
		(do* ((c (read-char stream t nil t)(read-char stream t nil t))
			  (level 1))
			((= level 0) nil)
			(when (and (char= c #\#)(char= (peek-char nil stream) #\|))
				(incf level))
			(when (and (char= c #\|)(char= (peek-char nil stream) #\#))
				(decf level)))
		(values)))

;;; 
;;; Redefine kernel SET-SYMBOL-FUNCTION function.
;;;
(defun set-symbol-function (func sym)
	(setf (car (uref sym symbol-function-offset)) func)
	(setf (uref sym symbol-function-type-offset) 'function)
	(update-jump-table sym func (uref func function-environment-offset))
	func)

;;; 
;;; Redefine kernel SET-SYMBOL-MACRO function.
;;;
(defun set-symbol-macro (func sym)
	(setf (car (uref sym symbol-function-offset)) func)
	(setf (uref sym symbol-function-type-offset) 'macro)
	(update-jump-table sym func (uref func function-environment-offset))
	func)

;;;
;;; Returns the page of an arbitrary heap object
;;;
(x86:defasm address-to-page (addr)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + ARGS_OFFSET]
    begin-atomic
        shr     eax, 12
        shl     eax, 3
    end-atomic
        pop     ebp
        ret
    })

(x86:defasm register-untagged-values (uvec index)
    {
        push    ebp
        mov     ebp, esp
        push    edi
        push    ebx
        mov     eax, [ebp + (+ ARGS_OFFSET 0)]      ;; eax = tagged cell index
    begin-atomic
        shr     eax, 1                              ;; eax = byte offset
        mov     ecx, [ebp + (+ ARGS_OFFSET 4)]      ;; ecx = uvec
        add     ecx, eax
        mov     edx, ecx                            ;; ecx, edx = uvec + byte-offset
        shr     ecx, 12                             ;; ecx = page
        shl     edx, 20
        shr     edx, 23                             ;; edx = offset
        mov     eax, [ebp + (+ ARGS_OFFSET 4)]      ;; eax = uvec
        mov     ebx, [eax + (uvector-offset 0)]
        shr     ebx, 8
        shl     ebx, 3                              ;; ebx = uvec length in bytes
        add     eax, ebx
        mov     ebx, eax
        shr     ebx, 12                             ;; ebx = end page
        shl     eax, 20
        shr     eax, 23                             ;; eax = end offset          
        cmp     ecx, ebx                            ;; page = end-page?
        jne     short :next1
        or      edx, edx                            ;; offset == 0?
        jne     short :done
    :next1
        or      edx, edx
        je      short :next2
        inc     ecx                                 ;; inc page
    :next2
        jmp     short :next3
    :next5
        inc     ecx
    :next3
        cmp     ecx, ebx                            ;; page = end-page?
        jae     short :next4
        symval  edi, cl::|*PAGE-TABLE*|
        mov     edi, [edi + (uvector-offset foreign-heap-ptr-offset)]
        mov     edx, #x200
        mov     [edi + ecx*4 + 2], dx            ;; word move
        jmp     short :next5
    :next4
        symval  edi, cl::|*PAGE-TABLE*|
        mov     edi, [edi + (uvector-offset foreign-heap-ptr-offset)]
        mov     [edi + ecx*4 + 2], ax
    :done
        mov     eax, [esi]
        mov     ecx, 1
        pop     ebx
        pop     edi
        xor     edx, edx
    end-atomic
        pop     ebp
        ret
    })

;;;
;;; Override Kernel cl::%STRINGNODE function.
;;; This is passed a foreign pointer to a C-string, and returns a Lisp string.
;;;
(x86:defasm cl::%stringNode (cstr)
    {
        push    ebp
        mov     ebp, esp
        push    ebx
        mov     edx, [ebp + ARGS_OFFSET]
        xor     ecx, ecx
        xor     eax, eax
    begin-atomic
    :loop1
        mov     al, [edx + ecx]
        or      al, al
        je      short :done-counting
        inc     ecx
        jmp     short :loop1
    :done-counting
        shl     ecx, 3      ;; tag count
    end-atomic
        push    ecx
        mov     ecx, 1
        callp   cl::allocate-char-vector   
        add     esp, 4
        mov     ebx, eax                 ;; ebx = char vector
        mov     edx, [ebp + ARGS_OFFSET]
        xor     ecx, ecx
        xor     eax, eax
    begin-atomic
    :loop2
        mov     al, [edx + ecx]
        or      al, al
        je      short :done
        mov     [ebx + ecx*2 + (uvector-offset 2)], ax
        inc     ecx
        jmp     short :loop2
    :done
        mov     eax, ebx
        mov     ecx, 1
    end-atomic
        pop     ebx
        pop     ebp
        ret
    })

;;;
;;; Redfine kernel function cl::%PUSH_CATCHER, used by the code generator
;;;  void pushCatcher(LispObj tag, unsigned long* regs);
;;;  Note that this gets called from C, and arguments are in reverse order.
;;;
;(defconstant CATCH_HEADER_CODE 1)

(x86:defasm cl::%push_catcher (tag regs)
    {
        push    ebp
        mov     ebp, esp
        push    8           ;; CATCH_HEADER_CODE
        push    [ebp + (+ ARGS_OFFSET 0)]   ;; push tag
        push    [ebp + (+ ARGS_OFFSET 4)]   ;; push regs
        mov     ecx, 3
        callp   list
        add     esp, 12
        push    eax
        symval  eax, cl::compiler_runtime
        push    eax
        mov     ecx, 2
        callp   cons
        add     esp, 8
        symset  eax, cl::compiler_runtime
        mov     eax, [esi]
        mov     ecx, 1
        pop     ebp
        ret
    })

;;;
;;; Redfine kernel function cl::%POP_CATCHER, used by the code generator
;;;  void popCatcher();
;;;

(x86:defasm cl::%pop_catcher ()
    {
        push    ebp
        mov     ebp, esp
        symval  eax, cl::compiler_runtime
        mov     eax, [eax]      ;; eax = (cdr eax)
        symset  eax, cl::compiler_runtime    
        mov     eax, [esi]
        mov     ecx, 1
        pop     ebp
        ret
    })

;;;
;;; Override kernel function cl::%POP_SPECIALS
;;; This implementation is the same as %pop_catcher.
;;;
(x86:defasm cl::%pop_specials ()
    {
        push    ebp
        mov     ebp, esp
        symval  eax, cl::compiler_runtime
        mov     eax, [eax]      ;; eax = (cdr eax)
        symset  eax, cl::compiler_runtime    
        mov     eax, [esi]
        mov     ecx, 1
        pop     ebp
        ret
    })

(x86:defasm create-uninitialized-binding ()
    {
        push    ebp
        mov     ebp, esp
        push    x86::_UNINITIALIZED 
        push    [esi]
        mov     ecx, 2
        callp   cons 
        add     esp, 8
        pop     ebp
        ret
    })

;;;
;;; Common Lisp MAKE-SYMBOL function.
;;;
(defun make-symbol (name)
    ;; allocate a symbol
    (let ((sym (alloc-uvector symbol-size uvector-symbol-tag)))
        (setf (uref sym symbol-name-offset)
            (make-array (length name) :initial-contents name :element-type 'character))
        (setf (uref sym symbol-value-offset) (create-uninitialized-binding))
        (setf (uref sym symbol-package-offset) nil)  
        (setf (uref sym symbol-plist-offset) nil)  
        (setf (uref sym symbol-constant-offset) 0)  
        (setf (uref sym symbol-function-type-offset) nil)  
        (setf (uref sym symbol-function-offset) (create-uninitialized-binding))
        (setf (uref sym symbol-jump-table-offset) 0)          
        (setf (uref sym symbol-var-table-offset) 0)
        sym))

;;;
;;; Redefine Kernel cl::%ESTABLISH-SPECIAL-BINDINGS, used by the code generator
;;;

(x86:defasm cl::%establish-special-bindings (bindings)
    {
        push    ebp
        mov     ebp, esp
        push    'cl::special
        push    [ebp + ARGS_OFFSET]
        mov     ecx, 2
        callp   cons
        add     esp, 8
        push    eax
        symval  eax, cl::compiler_runtime
        push    eax
        mov     ecx, 2
        callp   cons
        add     esp, 8
        symset  eax, cl::compiler_runtime
        mov     eax, [esi]
        mov     ecx, 1
        pop     ebp
        ret
    })

;;;
;;; Redefine kernel function CL::UNINITIALIZED-OBJECT-P.
;;;
(x86:defasm cl::uninitialized-object-p (obj)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + ARGS_OFFSET]
        cmp     eax, _uninitialized
        jne     short :no
        mov     eax, [esi + 4]
        jmp     short :done
    :no
        mov     eax, [esi]
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

(x86:defasm cl::uvector-num-slots (uvec)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + ARGS_OFFSET]
        mov     eax, [eax + (uvector-offset 0)]
        mov     al, 0
        shr     eax, 4
        sub     eax, 8
        pop     ebp
        ret
    })
       