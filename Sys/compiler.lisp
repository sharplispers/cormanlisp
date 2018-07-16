;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		compiler.lisp
;;;;	Contents:	Intel x86 code generation related functions.
;;;;	History:	11/20/96  RGC  Created.
;;;;

(in-package :x86)

(defvar *compiler-macro-table* (make-hash-table))
(defvar *code-generator-table* (make-hash-table))

(defparameter cl::*CAPTURED-LEXICAL-CALLBACK* nil)

;;; Common Lisp numeric constants
(defconstant most-positive-fixnum #xfffffff)	;  268435455
(defconstant most-negative-fixnum #x-10000000)	; -268435456

;;
;; some kernel constants--should conform to the values in Lisp.h
;;
(defconstant thread-heap-qv-offset 	        5)
(defconstant thread-heap-end-qv-offset 	    6)
(defconstant stack-marker-index-qv-offset 	7)
(defconstant stack-markers-qv-offset 		8)
(defconstant stack-markers-max 		 (* 30 4))
(defconstant stack-markers-num             60)
(defconstant foreign-cells-num             64)
(defconstant return-values-num            128)
(defconstant foreign-cells-qv-offset (+ stack-markers-qv-offset stack-markers-num))
(defconstant foreign-cells-qv-tos    (+ foreign-cells-qv-offset foreign-cells-num))
(defconstant return-values-qv-offset (+ foreign-cells-qv-offset foreign-cells-num))

;;;
;;; Common Lisp COMPILER-MACRO-FUNCTION function.
;;;
(defun compiler-macro-function (name &optional environment)
	(declare (ignore environment))
	(gethash name *compiler-macro-table*))

;;;
;;; Common Lisp (SETF COMPILER-MACRO-FUNCTION) function.
;;;
(defun (setf compiler-macro-function) (new-function name &optional environment)
	(declare (ignore environment))
	(setf (gethash name *compiler-macro-table*) new-function))

;;;
;;;	Common Lisp DEFINE-COMPILER-MACRO macro.
;;;
(defmacro define-compiler-macro (name lambda-list &rest forms)
	(let ((doc nil)
		  (lambda-form nil)
		  (declarations nil)
		  (form-sym (gensym))
		  (env-sym (gensym)))

		;; look for declarations and doc string
		(do* ((f forms (cdr f)))
			((null f) (setq forms f))
			(if (and (stringp (car f)) (null doc) (cdr f))
				(setq doc (car f))
				(if (and (consp (car f)) (eq (caar f) 'declare))
					(push (car f) declarations)
					(progn (setq forms f) (return)))))

		(setq lambda-form 
			`(lambda (,form-sym &optional ,env-sym) 
				(declare (ignore ,env-sym ,@(unless lambda-list '(form))))
				(cl::macro-bind ,lambda-list 
					(if (eq (car ,form-sym) 'funcall) (cdr ,form-sym) ,form-sym)
					,@(nreverse declarations) 
					(block ,name ,@forms))))
		`(progn (setf (compiler-macro-function ',name) (function ,lambda-form))
                                     (setf (ccl::macro-lambda-list (compiler-macro-function ',name)) ',lambda-list)
                                     ,@(when doc `((setf (ccl::function-documentation (compiler-macro-function ',name)) ,doc)))
			 (setf (documentation ',name 'compiler-macro) ,doc) ',name)))

;;;
;;; Corman Lisp (non-standard) CODE-GENERATOR-FUNCTION function.
;;;
(defun x86::code-generator-function (name &optional environment)
	(declare (ignore environment))
	(gethash name *code-generator-table*))

;;;
;;; Corman Lisp (non-standard) (SETF CODE-GENERATOR-FUNCTION) function.
;;;
(defun (setf x86::code-generator-function) (new-function name &optional environment)
	(declare (ignore environment))
	(setf (gethash name *code-generator-table*) new-function))

(defun offset-stack (num)
	(let* ((buf *compiler-code-buffer*)
		   (index (uref buf code-buffer-stack-index-offset)))
		(setf (uref buf code-buffer-stack-index-offset) (- index num))))

(defun cl::compile-function-call-form (form dest)
	(let* ((funcname (car form))
		  (code-generator-func (x86::code-generator-function funcname)))
		(if code-generator-func
			(funcall code-generator-func form dest)
			nil)))

(defmacro defcodegen (op params &rest exprs)
	(unless (= (length params) 2)
		(error "Wrong number of arguments to code generator function: ~A ~A" 
			op params))
	`(progn 
		(setf (x86::code-generator-function ',op) #'(lambda ,params ,@exprs))
		',op))

(defun expand-assembler-macros (forms)
	(let ((temp nil))
		(dolist (f forms)
			(if (and (consp f) (assembler-macro-function (car f)))
				(dolist (g (expand-assembler-macro f))
					(push g temp))
				(push f temp)))
		(nreverse temp)))

(defun assemble-inline (forms)
	(let ((*compiler-code-branch-targets* nil)
		  (*compiler-code-labels* nil)
		  (*current-statement* nil))
		(declare (special *compiler-code-branch-targets*))
		(declare (special *compiler-code-labels*))
		(declare (special *current-statement*))

		(dolist (f forms)
			(setq *current-statement* f)
			(if (consp f)
				(emit-code (apply (get (car f) 'x86::encoding-func) (cdr f)))
				(if (keywordp f)
					(push 
						(cons f (uref *compiler-code-buffer* code-buffer-code-index-offset)) 
						*compiler-code-labels*)
					(unless (eq f 'inline-assemble)
						(error "Invalid form in assembly block: ~A" f)))))
	
		;; now need to resolve addresses
		(resolve-branch-addresses *compiler-code-labels* *compiler-code-branch-targets*)))

(defmacro parse-assembler (forms)
	(let ((g (gensym)))
		`(let ((,g (mapcar #'parse-assembly-statement (expand-assembler-macros ,forms))))
			(assemble-inline ,g))))

;;;
;;;	%GET-LONG-SAFE
;;;	Call with eax = lisp integer, on return ax = low 16-bits of long value, dx = high 16-bits
;;; Upper 2 bytes of both eax and edx are 0, thereby making these untagged values safe.
;;;
(pl:defasm %get-long-safe ()
	{
		push	ebp
		mov		ebp, esp
		test	eax, 7
		jz		short :fixnum
		mov		edx, eax
		and		edx, 7
		cmp		dl, uvector-tag
		jne		short :error
		mov		edx, eax
		xor     ecx, ecx
		mov		cl, [edx - uvector-tag]
		shr		cl, 3
		cmp		cl, uvector-bignum-tag
		jne		short :error
	:bignum
	begin-atomic
	    mov     ecx, edx
		mov		eax, [ecx + (uvector-offset cl::bignum-first-cell-offset)]
		mov		ecx, [ecx + (uvector-offset cl::bignum-num-cells-offset)]
		and		ecx, 8			;; bignum < 0?
		jz		short :next1
		neg		eax
	:next1
	    mov     edx, eax
	    and     eax, #xffff
	    shr     edx, 16
    end-atomic
		jmp		short :exit
	:fixnum
	begin-atomic
		sar		eax, 3			;; warning--untagged integer
		mov     edx, eax
	    and     eax, #xffff
	    shr     edx, 16
	end-atomic		
		jmp		short :exit
	:error
		push	"Argument ~A is not of type INTEGER"
		push	eax
		mov		ecx, 2
		callf	error
	:exit
		pop		ebp
		ret
	})

;;;
;;;	%GET-FOREIGN-LONG
;;;	Call with eax = lisp integer, ecx = untagged index
;;; Store at foreign stack frame slot, offset index
;;;
(pl:defasm %get-foreign-long ()
	{
		push	ebp
		mov		ebp, esp
        push    ebx
		test	eax, 7
		jz		short :fixnum
		mov		edx, eax
		and		edx, 7
		cmp		dl, uvector-tag
		jne		short :error
		mov		edx, eax
        xor     ebx, ebx
		mov		bl, [edx - uvector-tag]
		shr		bl, 3
		cmp		bl, uvector-bignum-tag
		jne		short :error
	:bignum
    begin-atomic
		mov		eax, [edx + (uvector-offset cl::bignum-first-cell-offset)]
		mov		edx, [edx + (uvector-offset cl::bignum-num-cells-offset)]
		and		edx, 8			;; bignum < 0?
		jz		short :exit
		neg		eax
		jmp		short :exit
	:fixnum
    begin-atomic
		sar		eax, 3			;; warning--untagged integer
		jmp		short :exit
	:error
		push	"Argument ~A is not of type INTEGER"
		push	eax
		mov		ecx, 2
		callf	error
	:exit
        mov     [esi + ecx*4 + (* foreign-cells-qv-tos 4)], eax
        xor     eax, eax
    end-atomic
        pop     ebx
		pop		ebp
		ret
	})

;;;
;;;	%GET-UNSIGNED-LONG-SAFE
;;;	Call with eax = lisp integer, on return ax = low 16-bits of long value, dx = high 16-bits
;;; Upper 2 bytes of both eax and edx are 0, thereby making these untagged values safe.
;;;
(pl:defasm %get-unsigned-long-safe ()
	{
		push	ebp
		mov		ebp, esp
		test	eax, 7
		jz		short :fixnum
		mov		edx, eax
		and		edx, 7
		cmp		dl, uvector-tag
		jne		short :error
		mov		edx, eax
        xor     ecx, ecx
		mov		cl, [edx - uvector-tag]
		shr		cl, 3
		cmp		cl, uvector-bignum-tag
		jne		short :error
	:bignum
        mov     ecx, edx
    begin-atomic
		mov		eax, [ecx + (uvector-offset cl::bignum-first-cell-offset)]
		mov		ecx, [ecx + (uvector-offset cl::bignum-num-cells-offset)]
		and		ecx, 8			;; bignum < 0?
		jz		short :next1
		neg		eax
    :next1
        mov     edx, eax
	    and     eax, #xffff
	    shr     edx, 16
	end-atomic		
		jmp		short :exit
	:fixnum
    begin-atomic
		shr		eax, 3			;; warning--untagged integer
        mov     edx, eax
	    and     eax, #xffff
	    shr     edx, 16
	end-atomic		
		jmp		short :exit
	:error
		push	"Argument ~A is not of type (INTEGER 0 *)"
		push	eax
		mov		ecx, 2
		callf	error
	:exit
		pop		ebp
		ret
	})

;;;
;;;	%GET-FOREIGN-UNSIGNED-LONG
;;;	Call with eax = lisp integer, ecx = untagged index
;;; Store at foreign stack frame slot, offset index
;;;
(pl:defasm %get-foreign-unsigned-long (index)   ;; tagged index
	{
		push	ebp
		mov		ebp, esp
        push    ebx
		test	eax, 7
		jz		short :fixnum
		mov		edx, eax
		and		edx, 7
		cmp		dl, uvector-tag
		jne		short :error
		mov		edx, eax
        xor     ebx, ebx
		mov		bl, [edx - uvector-tag]
		shr		bl, 3
		cmp		bl, uvector-bignum-tag
		jne		short :error
	:bignum
    begin-atomic
		mov		eax, [edx + (uvector-offset cl::bignum-first-cell-offset)]
		mov		edx, [edx + (uvector-offset cl::bignum-num-cells-offset)]
		and		edx, 8			;; bignum < 0?
		jz		short :exit
		neg		eax
		jmp		short :exit
	:fixnum
    begin-atomic
		shr		eax, 3			;; warning--untagged integer
		jmp		short :exit
	:error
		push	"Argument ~A is not of type (INTEGER 0 *)"
		push	eax
		mov		ecx, 2
		callf	error
	:exit
        mov     [esi + ecx*4 + (* foreign-cells-qv-tos 4)], eax
        xor     eax, eax
    end-atomic
        pop     ebx
   		pop		ebp
		ret
	})

;;;
;;;	%GET-FOREIGN-INT64
;;;	Call with eax = lisp integer, ecx = untagged index
;;; Store at foreign stack frame slot, offset index
;;;
(pl:defasm %get-foreign-int64 ()
	{
		push	ebp
		mov		ebp, esp
        push    ebx
		test	eax, 7
		jz		short :fixnum
		mov		edx, eax
		and		edx, 7
		cmp		dl, uvector-tag
		jne		short :error
		mov		edx, eax
        xor     ebx, ebx
		mov		bl, [edx - uvector-tag]
		shr		bl, 3
		cmp		bl, uvector-bignum-tag
		jne		short :error
	:bignum
    begin-atomic
        push    ecx
        mov     ecx, edx
		mov		eax, [ecx + (uvector-offset cl::bignum-first-cell-offset)]
        mov     edx, 0
		mov		ebx, [ecx + (uvector-offset cl::bignum-num-cells-offset)]
        cmp     ebx, 32         ;; bignum-num-cells > 1
        jl      short :next2
 		mov		edx, [ecx + (uvector-offset (+ cl::bignum-first-cell-offset 1))]
    :next2
        pop     ecx       
		and		ebx, 8			;; bignum < 0?
		jz		short :exit
        ;; twos-complement of edx:eax (64-bit number)
        not     eax
        not     edx
        inc     eax
        adc     edx, 0
		jmp		short :exit
	:fixnum
    begin-atomic
		sar		eax, 3			;; warning--untagged integer
        mov     edx, 0
        cmp     eax, 0
        jge     short :next1
        dec     edx             ;; if negative, flip upper bits
    :next1
		jmp		short :exit
	:error
		push	"Argument ~A is not of type INTEGER"
		push	eax
		mov		ecx, 2
		callf	error
	:exit
        mov     [esi + ecx*4 + (* foreign-cells-qv-tos 4)], eax
        mov     [esi + ecx*4 + (+ 4 (* foreign-cells-qv-tos 4))], edx
        xor     eax, eax
        xor     edx, edx
    end-atomic
        pop     ebx
		pop		ebp
		ret
	})

;;;
;;;	%GET-FOREIGN-UINT64
;;;	Call with eax = lisp integer, ecx = untagged index
;;; Store at foreign stack frame slot, offset index
;;; This is currently the same as %GET-FOREIGN-INT64--not sure if anything needs to be changed
;;;
(pl:defasm %get-foreign-uint64 ()
	{
		push	ebp
		mov		ebp, esp
        push    ebx
		test	eax, 7
		jz		short :fixnum
		mov		edx, eax
		and		edx, 7
		cmp		dl, uvector-tag
		jne		short :error
		mov		edx, eax
        xor     ebx, ebx
		mov		bl, [edx - uvector-tag]
		shr		bl, 3
		cmp		bl, uvector-bignum-tag
		jne		short :error
	:bignum
    begin-atomic
        push    ecx
        mov     ecx, edx
		mov		eax, [ecx + (uvector-offset cl::bignum-first-cell-offset)]
        mov     edx, 0
		mov		ebx, [ecx + (uvector-offset cl::bignum-num-cells-offset)]
        cmp     ebx, 32         ;; bignum-num-cells > 1
        jl      short :next2
 		mov		edx, [ecx + (uvector-offset (+ cl::bignum-first-cell-offset 1))]
    :next2
        pop     ecx       
		and		ebx, 8			;; bignum < 0?
		jz		short :exit
        ;; twos-complement of edx:eax (64-bit number)
        not     eax
        not     edx
        inc     eax
        adc     edx, 0
		jmp		short :exit
	:fixnum
    begin-atomic
		sar		eax, 3			;; warning--untagged integer
        mov     edx, 0
        cmp     eax, 0
        jge     short :next1
        dec     edx             ;; if negative, flip upper bits
    :next1
		jmp		short :exit
	:error
		push	"Argument ~A is not of type INTEGER"
		push	eax
		mov		ecx, 2
		callf	error
	:exit
        mov     [esi + ecx*4 + (* foreign-cells-qv-tos 4)], eax
        mov     [esi + ecx*4 + (+ 4 (* foreign-cells-qv-tos 4))], edx
        xor     eax, eax
        xor     edx, edx
    end-atomic
        pop     ebx
		pop		ebp
		ret
	})

;;
;; Copy the foreign args from the QV buffer to the real stack.
;; This can only be done once the stack has transitioned to foreign.
;;
(defcodegen copy-foreign-args (form dest)
	(declare (ignore dest))
	(let ((num-cells (second form)))
        (dotimes (i num-cells)
            (parse-assembler
                {
                    push [esi + (- (* foreign-cells-qv-tos 4) (* (+ i 1) 4))]
                })))
    t)

(defcodegen push-foreign-args-length (form dest)
	(declare (ignore dest))
	(let ((num-cells (second form)))
         (parse-assembler
            {
                push (* 4 num-cells)
            }))
    t)

(defcodegen pop-foreign-args-length (form dest)
	(declare (ignore form dest))
    (parse-assembler
        {
            add     esp, 4
        })
    t)
       
(defcodegen compile-foreign-arg (form dest)
	(declare (ignore dest))
	(let ((arg (second form))
		  (type (third form))
          (index (fourth form)))
 ;       (cl::compile-sub-form index :dest-eax-operand t)
        (parse-assembler
			{
                mov     ecx, index
            })
		(cl::compile-sub-form arg :dest-eax-operand t)
		(cond
			((eq type :long)
			 (parse-assembler
				{
					callp	%GET-FOREIGN-LONG
				}))

			((or (eq type :long-bool) (eq type :short-bool))
			 (parse-assembler
				{
					cmp		eax, [esi]
					jne		:t1
					mov     eax, 0
					jmp		short :t2
				:t1
					mov     eax, 1
				:t2
                    mov     [esi + ecx*4 + (* foreign-cells-qv-tos 4)], eax
   				}))

			((eq type :unsigned-long)
			 (parse-assembler
				{
					callp	%GET-FOREIGN-UNSIGNED-LONG
				}))

			((eq type :int64)
			 (parse-assembler
				{
					callp	%GET-FOREIGN-INT64
				}))
			
            ((eq type :uint64)
			 (parse-assembler
				{
					callp	%GET-FOREIGN-UINT64
				}))
            
			((eq type :short) 
			 (parse-assembler
				{
                    push    ecx
					callp	%GET-FOREIGN-LONG
                    pop     ecx
                begin-atomic
                    mov     eax, [esi + ecx*4 + (* foreign-cells-qv-tos 4)]
   					shl		eax, 16
					sar		eax, 16
                    mov     [esi + ecx*4 + (* foreign-cells-qv-tos 4)], eax
                end-atomic
   				}))

			((eq type :unsigned-short) 
			 (parse-assembler
				{
                    push    ecx
					callp	%GET-FOREIGN-UNSIGNED-LONG
                    pop     ecx
                begin-atomic
                    mov     eax, [esi + ecx*4 + (* foreign-cells-qv-tos 4)]
   					shl		eax, 16
					shr		eax, 16
                    mov     [esi + ecx*4 + (* foreign-cells-qv-tos 4)], eax
                end-atomic
   				}))

			((eq type :wide-char) 
			 (parse-assembler
				{
                    shl     eax, 8
                    shr     eax, 16
                    mov     [esi + ecx*4 + (* foreign-cells-qv-tos 4)], eax
   				}))
            
			((eq type :char) 
			 (parse-assembler
				{
                    push    ecx
					callp	%GET-FOREIGN-LONG
                    pop     ecx
                begin-atomic
                    mov     eax, [esi + ecx*4 + (* foreign-cells-qv-tos 4)]
   					shl		eax, 24
					sar		eax, 24
                    mov     [esi + ecx*4 + (* foreign-cells-qv-tos 4)], eax
                end-atomic
				}))

			((eq type :unsigned-char) 
			 (parse-assembler
				{
                    push    ecx
					callp	%GET-FOREIGN-UNSIGNED-LONG
                    pop     ecx
                begin-atomic
                    mov     eax, [esi + ecx*4 + (* foreign-cells-qv-tos 4)]
   					shl		eax, 24
					shr		eax, 24
                    mov     [esi + ecx*4 + (* foreign-cells-qv-tos 4)], eax
                end-atomic
   				}))

			((eq type :double-float) 
			 (parse-assembler
				{
					mov 	edx, eax
					and 	edx, 7
					cmp 	edx, uvector-tag
					je 		short :t1
					push	"Not a :double-float: ~S"
					push	eax
					mov		ecx, 2
					callf	error
				:t1
					mov 	dl, [eax - uvector-tag]
					shr 	dl, 3
					cmp 	dl, cl::uvector-double-float-tag
					je		:t2
					push	"Not a :double-float: ~S"
					push	eax
					mov		ecx, 2
					callf	error
				:t2
                    fld     [eax + (uvector-offset cl::double-float-offset)]
                    fstp    [esi + ecx*4 + (* foreign-cells-qv-tos 4)]
				}))

			((eq type :single-float) 
			 (parse-assembler
				{
					mov 	edx, eax
					and 	edx, 7
					cmp 	edx, uvector-tag
					je 		short :t1
					push	"Not a :single-float: ~S"
					push	eax
					mov		ecx, 2
					callf	error
				:t1
					mov 	dl, [eax - uvector-tag]
					shr 	dl, 3
					cmp 	dl, cl::uvector-single-float-tag
					je		:t2
					push	"Not a :single-float: ~S"
					push	eax
					mov		ecx, 2
					callf	error
				:t2
                    fld.single  [eax + (uvector-offset cl::single-float-offset)]
                    fstp.single [esi + ecx*4 + (* foreign-cells-qv-tos 4)]
				}))

			((eq type :void) 
			 (error "Type ~A is not supported by PUSH-ARG" type))

			(t
			 (parse-assembler
				{
					mov 	edx, eax
					and 	edx, 7
					cmp 	edx, uvector-tag
					je 		short :t1
					push	"Not a foreign pointer: ~A"
					push	eax
					mov		ecx, 2
					callf	error
				:t1
					mov 	dl, [eax - uvector-tag]
					shr 	dl, 3
					cmp 	dl, cl::uvector-foreign-tag
					je		:t2
					cmp		dl, cl::uvector-foreign-heap-tag
					je		:t2
					push	"Not a foreign pointer: ~A"
					push	eax
					mov		ecx, 2
					callf	error
				:t2
                begin-atomic
                    mov     edx, [eax + (uvector-offset foreign-heap-ptr-offset)]
                    mov     [esi + ecx*4 + (* foreign-cells-qv-tos 4)], edx
                    xor     edx, edx
                end-atomic
				}))))
	t)

(defcodegen save-lisp-registers (form dest)
	(declare (ignore form dest))
	(parse-assembler
		{
			push	esi
			push	ebx
			push	edi
		})
	t)

(defcodegen restore-lisp-registers (form dest)
	(declare (ignore form dest))
	(parse-assembler
		{
			pop		edi
			pop		ebx
			pop		esi
		})
	t)

;;
;;	push-foreign-stack-context() and push-lisp-stack-context() are identical
;;	The garbage collector figures out which is which by assuming they are
;;	alternating.
;;
(defasm-macro lisp-call-foreign ()
	{
		begin-atomic
		mov		eax, [esi + (* stack-marker-index-qv-offset 4)]
		cmp		eax, stack-markers-max
		jge		short :err1
        test    eax, 4          ;; index should not be odd (already in foreign code)
        jz      short :t11000
    :err1
		end-atomic
		mov		ecx, 0
		callf	cl::memory-report
		push	"Error in context switch from lisp calling into foreign code" 
		mov		ecx, 1
		callf	error           
	:t11000
		push	eax		;; just to get esp - 4
		mov		[esi + eax*2 + (* stack-markers-qv-offset 4)], esp
		lea		edi, [esi + eax*2 + (* (+ stack-markers-qv-offset 1) 4)]
		pop		eax		;; just to get esp - 4
        push    0       ;; clear return address cell
        pop     [esi + eax*2 + (* (+ stack-markers-qv-offset 1) 4)]
		add		eax, 4
		mov		[esi + (* stack-marker-index-qv-offset 4)], eax
		end-atomic
	})

(defasm-macro foreign-call-lisp ()
	{
		begin-atomic
		mov		eax, [esi + (* stack-marker-index-qv-offset 4)]
		cmp		eax, stack-markers-max
		jge		short :err2
        test    eax, 4          ;; index should not be even (already in lisp code)
        jnz      short :t11001
    :err2			
        end-atomic
		mov		ecx, 0
		callf	cl::memory-report
		push	"Error in context switch from foreign code calling into lisp"
		mov		ecx, 1
		callf	error
	:t11001
		mov		[esi + eax*2 + (* stack-markers-qv-offset 4)], esp
        push    0       ;; clear return address cell
        pop     [esi + eax*2 + (* (+ stack-markers-qv-offset 1) 4)]
		add		eax, 4
		mov		[esi + (* stack-marker-index-qv-offset 4)], eax
		end-atomic
	})

(defasm-macro foreign-return-to-lisp ()
	{
		begin-atomic
		mov		edx, [esi + (* stack-marker-index-qv-offset 4)]
		cmp		edx, 0
		jle		short :err3
        test    edx, 4          ;; index should not be even (already in lisp code)
        jnz      short :t11002
    :err3			
		end-atomic
		mov		ecx, 0
		callf	cl::memory-report
		push	"Error in context switch from foreign code returning into lisp"
		mov		ecx, 1
		callf	error
	:t11002
		sub		edx, 4
		mov		[esi + (* stack-marker-index-qv-offset 4)], edx
        push    0  
		pop		[esi + edx*2 + (* stack-markers-qv-offset 4)]   ;; clear stack marker
        push    0       ;; clear return address cell
        pop     [esi + edx*2 + (* (+ stack-markers-qv-offset 1) 4)]
        end-atomic
	})

(defasm-macro lisp-return-to-foreign ()
	{
		begin-atomic
		mov		edx, [esi + (* stack-marker-index-qv-offset 4)]
		cmp		edx, 0
		jle		short :err4
        test    edx, 4          ;; index should not be odd (already in foreign code)
        jz      short :t11003
    :err4			
		end-atomic
		mov		ecx, 0
		callf	cl::memory-report
		push	"Error in context switch from lisp returning into foreign code"
		mov		ecx, 1
		callf	error
	:t11003
		sub		edx, 4
		mov		[esi + (* stack-marker-index-qv-offset 4)], edx
        push    0  
		pop		[esi + edx*2 + (* stack-markers-qv-offset 4)]   ;; clear stack marker 
        push    0       ;; clear return address cell
        pop     [esi + edx*2 + (* (+ stack-markers-qv-offset 1) 4)]
        end-atomic
	})

(defcodegen push-foreign-stack-context (form dest)
	(declare (ignore form dest))
	(parse-assembler
		{
            lisp-call-foreign
        })
    t)

(defcodegen push-lisp-stack-context (form dest)
	(declare (ignore form dest))
	(parse-assembler
		{
            foreign-call-lisp
        })
    t)

(defcodegen pop-foreign-stack-context (form dest)
	(declare (ignore form dest))
	(parse-assembler
		{
            foreign-return-to-lisp
        })
    t)

(defcodegen pop-lisp-stack-context (form dest)
	(declare (ignore form dest))
	(parse-assembler
		{
            lisp-return-to-foreign
        })
    t)
   
(defcodegen link-heap-handler (form dest)
	(declare (ignore form dest))
	(parse-assembler
		{
			;;mov		eax, #'cl::%heap_fault_handler
            mov     eax, 'cl::%heap_fault_handler
    		mov 	eax, [eax + (- (* 4 symbol-function-offset) uvector-tag)]
    		mov 	eax, [eax - cons-tag]
            
			push	[eax + (uvector-offset cl::function-code-buffer-offset)]	;; push handler
			fs
			push	[0]		;; push fs:[0]  previous address
			fs
			mov		[0], esp	;; mov  fs:[0], esp
		})
	t)

(defcodegen unlink-heap-handler (form dest)
	(declare (ignore form dest))
	(parse-assembler
		{
			mov		edx, [esp]		;; eax = previous handler
			fs
			mov		[0], edx		;; install previous handler
			add		esp, 8			;; remove from stack
		})
	t)

(defcodegen call-foreign-proc (form dest)
	(declare (ignore dest))
	(let* ((sym (cadr form))
		   (env-offset (* (uref sym symbol-jump-table-offset) 4))
		   (jump-offset	(+ env-offset 4)))
		(if (zerop env-offset)
			(error "The function ~A does not have a jump table offset and cannot be called" sym))
		(parse-assembler
			{
		  	call [esi + jump-offset]
			add-jump-table-ref sym
   			}))
	t)

(defcodegen call-com-method (form dest)
	(declare (ignore dest form))
	(parse-assembler   
		{
			shr ebx, 1	  ;; ebx = fixnum -> int * 4   
			mov edx, [esp];; edx -> interface
			mov edx, [edx];; edx -> vtable
			call [ebx + edx]   
		})
	t)

;;
;; Stores the COM VTABLE offset (tagged fixnum) in the EBX register
;;
(defcodegen compile-com-method-address (form dest)
	(declare (ignore dest))
    (cl::compile-sub-form (second form) :dest-eax-operand t)
	(parse-assembler
		{
            mov ebx, eax
		})
	t)

;; 
;; Call a foreign function whose address is at the top of the stack.
;;
(defcodegen call-foreign-pointer (form dest)
	(declare (ignore form dest))
	(parse-assembler
		{
            pop     eax
            call    eax
		})
	t)

(defcodegen popargs (form dest)
	(declare (ignore dest))
	(let ((arg-type-list (cadr form))
		  (bytes 0))
		(dolist (i arg-type-list)
			(incf bytes
				(case i
					(:long 	4)
					(:double-float 8)
                    (:int64 8)
                    (:uint64 8)
					(otherwise 4))))
		(parse-assembler
			{
				add		esp, bytes
			}))
	t)

(defasm create-foreign-lisp-integer (index) ;; index < 0
    {
        push    ebp
        mov     ebp, esp
    begin-atomic
        mov     ecx, [ebp + ARGS_OFFSET]
        sar     ecx, 3
        mov     eax, [esi + ecx*4 + (* foreign-cells-qv-tos 4)]
        cmp     eax, cl::most-positive-fixnum
        jg      short :bignum
        cmp     eax, cl::most-negative-fixnum
        jl      short :bignum
        shl     eax, 3
        jmp     short :done
    :bignum
        xor     eax, eax
    end-atomic
        push    (* 8 1)             ;; wrapped integer 1
        mov     ecx, 1
        callp   cl::alloc-bignum
        add     esp, 4
        mov     ecx, [ebp + ARGS_OFFSET]
    begin-atomic
        sar     ecx, 3
        mov     edx, [esi + ecx*4 + (* foreign-cells-qv-tos 4)]
        cmp     edx, 0
        jge     :next1
        neg     edx
        mov     [eax + (uvector-offset 2)], edx
        mov     edx, [eax + (uvector-offset 1)]
        xor     edx, 8
        mov     [eax + (uvector-offset 1)], edx
        jmp     short :done
    :next1      
        mov     [eax + (uvector-offset 2)], edx
    :done
        mov     ecx, 1
        xor     edx, edx
    end-atomic
        pop     ebp
        ret
    })

(defasm create-foreign-lisp-unsigned-integer (index) ;; index < 0
    {
        push    ebp
        mov     ebp, esp
        mov     ecx, [ebp + ARGS_OFFSET]
        begin-atomic
        sar     ecx, 3
        mov     eax, [esi + ecx*4 + (* foreign-cells-qv-tos 4)]
        cmp     eax, cl::most-positive-fixnum
        ja      short :bignum       ;; unsigned compare
        shl     eax, 3
        jmp     short :done
    :bignum
        xor     eax, eax
    end-atomic
        push    (* 8 1)             ;; wrapped integer 1
        mov     ecx, 1
        callp   cl::alloc-bignum
        add     esp, 4
    begin-atomic
        mov     ecx, [ebp + ARGS_OFFSET]
        sar     ecx, 3
        mov     edx, [esi + ecx*4 + (* foreign-cells-qv-tos 4)]
        mov     [eax + (uvector-offset 2)], edx
    :done
        mov     ecx, 1
        xor     edx, edx
    end-atomic
        pop     ebp
        ret
    })

(defasm create-foreign-lisp-int64 (index) ;; index < 0
    {
        push    ebp
        mov     ebp, esp
    begin-atomic
        mov     ecx, [ebp + ARGS_OFFSET]
        sar     ecx, 3
        mov     eax, [esi + ecx*4 + (+ 4 (* foreign-cells-qv-tos 4))]   ;; check high dword
        cmp     eax, 0
        jnz     short :check-neg-fixnum         
        mov     eax, [esi + ecx*4 + (* foreign-cells-qv-tos 4)]     ;; high dword is 0
        cmp     eax, cl::most-positive-fixnum
        jg      short :bignum
    :check-neg-fixnum
        mov     eax, [esi + ecx*4 + (+ 4 (* foreign-cells-qv-tos 4))]   ;; check high dword
        inc     eax
        cmp     eax, 0
        jnz     short :bignum
        mov     eax, [esi + ecx*4 + (* foreign-cells-qv-tos 4)]     ;; high dword is 0xffffffff
        cmp     eax, cl::most-negative-fixnum
        jl      short :bignum
        cmp     eax, 0              ;; special case for low dword = 0
        je      short :bignum
        shl     eax, 3
        jmp     short :done
    :bignum
        xor     eax, eax
    end-atomic
        push    (* 8 2)             ;; wrapped integer 2
        mov     ecx, 1
        callp   cl::alloc-bignum
        add     esp, 4
        mov     ecx, [ebp + ARGS_OFFSET]
    begin-atomic
        sar     ecx, 3
        mov     edx, [esi + ecx*4 + (+ 4 (* foreign-cells-qv-tos 4))]
        mov     ecx, [esi + ecx*4 + (* foreign-cells-qv-tos 4)] ;; int64 in edx:ecx
        cmp     edx, 0
        jge     :next1
        ;;; negative number--do 64-bit negation of edx:ecx to get the absolute value
        not     edx
        not     ecx
        add     ecx, 1
        adc     edx, 0
        mov     [eax + (uvector-offset 2)], ecx
        mov     [eax + (uvector-offset 3)], edx
        mov     edx, [eax + (uvector-offset 1)]
        xor     edx, 8
        mov     [eax + (uvector-offset 1)], edx
        jmp     short :done
    :next1      
        mov     [eax + (uvector-offset 2)], ecx
        mov     [eax + (uvector-offset 3)], edx
    :done
        mov     ecx, 1
        xor     edx, edx
    end-atomic
        pop     ebp
        ret
    })

(defasm create-foreign-lisp-uint64 (index) ;; index < 0
    {
        push    ebp
        mov     ebp, esp
        mov     ecx, [ebp + ARGS_OFFSET]
    begin-atomic
        sar     ecx, 3
        mov     eax, [esi + ecx*4 + (+ 4 (* foreign-cells-qv-tos 4))]   ;; check high dword
        cmp     eax, 0
        jnz     short :bignum         
        mov     eax, [esi + ecx*4 + (* foreign-cells-qv-tos 4)]
        cmp     eax, cl::most-positive-fixnum
        ja      short :bignum       ;; unsigned compare
        shl     eax, 3
        jmp     short :done
    :bignum
        xor     eax, eax
    end-atomic
        push    (* 8 2)             ;; wrapped integer 2
        mov     ecx, 1
        callp   cl::alloc-bignum
        add     esp, 4
        mov     ecx, [ebp + ARGS_OFFSET]
    begin-atomic
        sar     ecx, 3
        mov     edx, [esi + ecx*4 + (+ 4 (* foreign-cells-qv-tos 4))]
        mov     ecx, [esi + ecx*4 + (* foreign-cells-qv-tos 4)] ;; uint64 in edx:ecx
        mov     [eax + (uvector-offset 2)], ecx
        mov     [eax + (uvector-offset 3)], edx
    :done
        mov     ecx, 1
        xor     edx, edx
    end-atomic
        pop     ebp
        ret
    })

;;
;; This stores the untagged returned value in eax in the foreign args
;; frame (topmost cell).
;; In the case of floating point, we leave it in the floating point
;; register because it isn't going to trip up the garbage collector.
;;
(defcodegen copy-return-value (form dest)
	(declare (ignore dest))
	(let ((type (cadr form)))
		(unless (member type '(:double-float :single-float :int64 :uint64))
            (parse-assembler
                {
                    mov     [esi + (- (* foreign-cells-qv-tos 4) 4)], eax
                }))
        (if (or (eq type :int64)(eq type :uint64))
            (parse-assembler
                {
                    mov     [esi + (- (* foreign-cells-qv-tos 4) 4)], edx
                    mov     [esi + (- (* foreign-cells-qv-tos 4) 8)], eax
                })))    
    t)    

(defcodegen wrap-foreign-return-value (form dest)
	(declare (ignore dest))
	(let ((type (cadr form)))
		(cond
			((eq type :long)
			 (parse-assembler
				{
                    push    (* -1 8)
					callp 	x86::create-foreign-lisp-integer
                    add     esp, 4
					mov		ecx, 1
				}))

			((eq type :long-bool)
			 (parse-assembler
				{
                begin-atomic
                    mov     eax, [esi + (- (* foreign-cells-qv-tos 4) 4)]
					test	eax, eax
					jz		:t1
					mov		eax, [esi + 4]	;; T
					jmp		short	:t2
				:t1
					mov		eax, [esi]
				:t2
                end-atomic
					mov		ecx, 1
				}))

			((eq type :short-bool)
			 (parse-assembler
				{
                    xor     eax, eax
                    mov     ax, [esi + (- (* foreign-cells-qv-tos 4) 4)]
   					test	eax, eax
					jz		:t1
					mov		eax, [esi + 4]	;; T
					jmp		short	:t2
				:t1
					mov		eax, [esi]
				:t2
                end-atomic
  					mov		ecx, 1
				}))

			((eq type :unsigned-long)
			 (parse-assembler
				{
                    push    (* -1 8)
  					callp 	x86::create-foreign-lisp-unsigned-integer
					add		esp, 4
					mov		ecx, 1
				}))

			((eq type :int64)
			 (parse-assembler
				{
                    push    (* -2 8)
  					callp 	x86::create-foreign-lisp-int64
					add		esp, 4
					mov		ecx, 1
				}))
 			
            ((eq type :uint64)
			 (parse-assembler
				{
                    push    (* -2 8)
  					callp 	x86::create-foreign-lisp-uint64
					add		esp, 4
					mov		ecx, 1
				}))
            
			((eq type :short) 
			 (parse-assembler
				{
                    xor     eax, eax
                    mov     ax, [esi + (- (* foreign-cells-qv-tos 4) 4)]
   					shl		eax, 16
					sar		eax, 13
					mov		ecx, 1
				}))

			((eq type :unsigned-short) 
			 (parse-assembler
				{
                    xor     eax, eax
                    mov     ax, [esi + (- (* foreign-cells-qv-tos 4) 4)]
   					shl		eax, 16
					shr		eax, 13
					mov		ecx, 1
				}))

			((eq type :wide-char) 
			 (parse-assembler
				{
                    xor     eax, eax
                    mov     ax, [esi + (- (* foreign-cells-qv-tos 4) 4)]
   					shl		eax, 8
					inc     eax
					mov		ecx, 1
				}))
            
			((eq type :char) 
			 (parse-assembler
				{
                    xor     eax, eax
                    mov     al, [esi + (- (* foreign-cells-qv-tos 4) 4)]
   					shl		eax, 24
					sar		eax, 21
					mov		ecx, 1
				}))

			((eq type :unsigned-char) 
			 (parse-assembler
				{
                    xor     eax, eax
                    mov     al, [esi + (- (* foreign-cells-qv-tos 4) 4)]
  					shl		eax, 24
					shr		eax, 21
					mov		ecx, 1
				}))
			((eq type :void)
			 (parse-assembler
				{
					mov		eax, [esi]
					mov		ecx, 1
				}))
			((eq type :double-float)
			 (parse-assembler
				{
                    ;; the kernel func expects these 8 bytes of 0 to be passed,
                    ;; but our replacement code in math-ops.lisp will not need this
					push	0
					push	0
					callp	cl::%double-float-node
					add		esp, 8
					fstp	[eax + (uvector-offset cl::double-float-offset)]
					mov		ecx, 1
				}))
			((eq type :single-float)
			 (parse-assembler
				{
                    ;; the kernel func expects these 8 bytes of 0 to be passed,
                    ;; but our replacement code in math-ops.lisp will not need this
					push	0
					push	0
					callp	cl::%single-float-node
					add		esp, 8
					fstp.single	[eax + (uvector-offset cl::single-float-offset)]
					mov		ecx, 1
				}))

			(t		;; assume we have a pointer
			 (parse-assembler
				{
				    xor     ecx, ecx
					callp	cl::%foreignnode
                begin-atomic
                    mov     edx, [esi + (- (* foreign-cells-qv-tos 4) 4)]
					mov		[eax + (uvector-offset cl::foreign-heap-ptr-offset)], edx
                    xor     edx, edx
                end-atomic
					mov		ecx, 1
				}))))
	t)
							
(defcodegen push-eax (form dest)
	(declare (ignore form dest))
	(parse-assembler
		{
			push	eax
		})
	t)

(defcodegen mov-ecx-num (form dest)
	(declare (ignore dest))
	(let ((num (cadr form)))
		(parse-assembler
			{
				mov		ecx, num
			}))
	t)

(defcodegen pop-stack (form dest)
	(declare (ignore dest))
	(let ((num (cadr form)))
		(parse-assembler
			{
				add	esp, (* 4 num)
			}))
	t)

(defcodegen call-values (form dest)
	(declare (ignore form dest))
	(parse-assembler
		{
			callf	values
		})
	t)

(defcodegen inline-assemble (form dest)
	(declare (ignore dest))
	(let ((statements (cadr form)))
		(parse-assembler (eval statements)))
	t)

(defcodegen cl::with-only-lexicals (form dest)
	(push `(cl::with-only-lexicals ,(second form)) cl::compiler_cleanups)
	(cl::compile-sub-form `(let () ,@(cddr form)) dest t)	;; use LET to allow for declarations
	(pop cl::compiler_cleanups)
	t)
	
#|
;;;; make a call to the LIST function whenever FOOBAR is seen by the compiler
(defcodegen foobar (form dest)
	(let* ((args (cdr form))
		  (nargs (length args)))
		(dolist (arg args)
			(cl::compile-sub-form arg :dest-stack t))
		(parse-assembler
			{
				mov		ecx, nargs
				callf	list
				add		esp, (* nargs 4))
			})
		(offset-stack (* nargs 4))
	t)

(defcodegen consp2 (form dest)
	(unless (eq (length form) 2)
		(error "Wrong number of arguments to function CONSP"))
	(let* ((f (cadr form)))
		(cl::compile-sub-form f :dest-eax-operand t)
		(parse-assembler
			{
				mov		edx, eax
				and		edx, 7
				cmp		edx, 4
				jne		short :t1
				mov		eax, [esi + 4]
				jmp		short :t2
			:t1
				mov		eax, [esi]
			:t2
			})
		(if (eq dest :dest-stack)
			(progn
				(parse-assembler
					{
						push	eax
					})
				(offset-stack 4))
			(if (eq dest :dest-eax)
				(parse-assembler
					{
						mov		ecx, 1
					}))))
	t)
|#

#|
(defcodegen f+ (form dest)
	(let ((x (second form))
		  (y (third form)))
		(if (fixnump y)
			(progn
				(cl::compile-sub-form x :dest-eax-operand t)
				(parse-assembler
				{
					mov		edx, eax
				})
				(cl::compile-sub-form y :dest-eax-operand t)
				(parse-assembler
				{
					add		eax, edx
				})
			)
			(progn
				(cl::compile-sub-form x :dest-stack t)
				(cl::compile-sub-form y :dest-eax-operand t)
				(parse-assembler
					{
						pop		edx
						add		eax, edx
					})
				(x86::offset-stack 4)
				(if (eq dest :dest-stack)
					(progn
						(parse-assembler
							{
								push	eax
							})
						(x86::offset-stack -4)))))
		(parse-assembler
			{
				mov		ecx, 1
			}))
	t)
|#

(defcodegen x86::%svref (form dest)
	(let ((vec (second form))
		  (index (third form)))
		(if (fixnump index)
			(progn
				(cl::compile-sub-form vec :dest-eax-operand t)
				(parse-assembler
				{
					mov		edx, eax
				})
				(cl::compile-sub-form index :dest-eax-operand t))
			(progn
				(cl::compile-sub-form vec :dest-stack t)
				(cl::compile-sub-form index :dest-eax-operand t)
				(parse-assembler
					{
						pop		edx
					})
				(x86::offset-stack 4)))
		(parse-assembler
			{
				shr		eax, 1
				mov		eax, [edx + eax + (uvector-offset 2)]
			})
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
;;;	Define code generation for Common Lisp * operator
(defcodegen * (form dest)
	(let ((num-args (- (length form) 1)))
		(cond ((= num-args 2)
			   (cl::compile-sub-form (second form) :dest-stack t)
			   (cl::compile-sub-form (third form) :dest-stack t)
			   (parse-assembler
					{
						callp	cl::%MULTIPLY-2
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
							callp	cl::%MULTIPLY-2
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

;;
;; Assumes the argument is a uvector.
;; Returns the type of uvector as an integer.
;;
(defcodegen cl::uvector-type-bits (form dest)
	(let ((u (second form)))
		(cl::compile-sub-form u :dest-eax-operand t)
		(parse-assembler
			{
				mov		eax, [eax + (uvector-offset 0)]
				and		eax, #xf8
			})
		(if (eq dest :dest-stack)
			(progn
				(parse-assembler
					{
						push	eax
					})
				(x86::offset-stack -4)))
		(if (eq dest :dest-eax)
			(parse-assembler
				{
					mov		ecx, 1
				})))
	t)

;;
;; Assumes a single argument of any type.
;; Returns T if the argument is a uvector, NIL otherwise.
;;
(defcodegen cl::uvectorp (form dest)
	(let ((obj (second form)))
		(cl::compile-sub-form obj :dest-eax-operand t)
		(parse-assembler
			{
				and		eax, 7
				cmp		eax, uvector-tag
				jne		short :no
				mov		eax, [esi + t-offset]
				jmp		short :done
			:no
				mov		eax, [esi]
			:done
			})
		(if (eq dest :dest-stack)
			(progn
				(parse-assembler
					{
						push	eax
					})
				(x86::offset-stack -4)))
		(if (eq dest :dest-eax)
			(parse-assembler
				{
					mov		ecx, 1
				})))
	t)

;;
;; Form to set the lambda parameters (indexed from 0)
;; This will bypass any local variables at inner scope with the
;; same name.
;; eg: (x86::set-arg 0 val)
(defcodegen x86::set-arg (form dest)
	(declare (ignore dest))
	(cl::compile-sub-form (third form) :dest-eax-operand t)
	(let* ((num-args (length cl::*compiler-lambda-list*))
		   (offset (+ (* (- num-args (second form) 1) 4) x86::ARGS_OFFSET))
		   (lexenv cl::compiler_cleanups)
		   (is-heap nil))
		(if (>= (second form) num-args)
			(error "Invalid X86::SET-ARG form: there are only ~A lambda-list arguments"
				num-args))
		
		;; This check is to see if the argument we are targetting has been converted
		;; to a heap binding. If this is the case, we need to store into the CAR
		;; of that binding, rather than the direct register offset. We are not checking
		;; for the name of the binding, since we don't have that available here--just
		;; if there is a lexical binding at that location, and if it is a heap binding.
		(dolist (x lexenv)
			(when (eq (first x) 'lambda)
				(return))
			(when (and
					(eq (first x) 'let)
					(consp (third x))
					(consp (second (third x)))
					(eq (first (third x)) 'cl::ebp)
					(= (car (second (third x))) offset))
				(setf is-heap t)
				(when cl::*COMPILE-VERBOSE*
					(format t "Compiler: found a heap binding for ~A while optimizing tail recursion...~%"
						(second x)))
				(return)))
		
		(if is-heap
			(parse-assembler
				{
					mov		edx, [ebp + offset]
					mov		[edx - cons-tag], eax
				})
			
			(parse-assembler
				{
					mov		[ebp + offset], eax
				}))
		
		(if (eq dest :dest-stack)
			(progn
				(parse-assembler
					{
						push	eax
					})
				(x86::offset-stack -4)))
	t))

;;;
;;; Redefine this kernel function
;;;
(defasm cl::%alloc-cons ()
	{
		push	ebp
		mov		ebp, esp
	:try1
    begin-atomic
		mov		eax, [esi + (* x86::thread-heap-qv-offset 4)]
		add		eax, cons-tag
		lea		edx, [eax + (- 8 cons-tag)]
		cmp		edx, [esi + (* x86::thread-heap-end-qv-offset 4)]
		jle		:done
    end-atomic
		callp	cl::%load-local-heap
		jmp		short :try1
	:done
		mov		[esi + (* x86::thread-heap-qv-offset 4)], edx
    end-atomic
		pop		ebp
		ret
	})

(defasm cl::alloc-uvector (size tag)
    {
        push    ebp
        mov     ebp, esp
        push    [ebp + (+ ARGS_OFFSET 4)]    ; push size
        callp   cl::%alloc-vector-tagged
        add     esp, 4
        mov     edx, [ebp + (+ ARGS_OFFSET 0)] ; edx = tag
        or      [eax + (uvector-offset 0)], edx
        mov     ecx, 1
        pop     ebp
        ret
    })
                           
(in-package :common-lisp)


(define-compiler-macro aref (array &rest indices)
	`(,(elt aref-funcs (length indices)) ,array ,@indices))

(define-compiler-macro cl::|(SETF AREF)| (value array &rest indices)
	`(,(elt aref-setter-funcs (length indices)) ,value ,array ,@indices))

