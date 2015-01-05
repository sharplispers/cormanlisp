;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		ffi.lisp
;;;;	Contents:	Foreign function interface.
;;;;	History:	2/13/97  RGC  Created.
;;;;

(defpackage "C-TYPES"
	(:export 
		"DEFUN-DLL" 
		"DEFUN-COM-METHOD" 
		"DEFUN-THISCALL"
        "DEFUN-POINTER"
        "DEFUN-KERNEL"
		"DEFCSTRUCT" 
		"DEFCTYPE" 
		"CREATE-FOREIGN-PTR"
		"SIZEOF"
		"OFFSETOF"
		"CPOINTER-VALUE"
		"CPOINTERP"
		"DEFUN-CALLBACK"
		"DEFUN-C-CALLBACK"
		"DEFUN-DIRECT-CALLBACK"
		"DEFUN-DIRECT-C-CALLBACK"
		"DEFUN-DLL-EXPORT-FUNCTION"
		"DEFUN-DLL-EXPORT-C-FUNCTION"
		"GET-CALLBACK-PROCINST"
		"CREF"
		"CREATE-C-STRING"
		"C-STRING-LENGTH"
		"CPOINTER="
	    "CPOINTER-NULL"
		"MEMCMP"
        "MEMCPY"
		"MALLOC"
		"FREE"
		"FOREIGN-PTR-TO-INT"
		"INT-TO-FOREIGN-PTR"
		"UNICODE-TO-LISP-STRING"
		"LISP-STRING-TO-UNICODE"
		"C-STRING-TO-LISP-STRING"
		"LISP-STRING-TO-C-STRING"
		"WITH-C-STRUCT"
		"WITH-FRESH-FOREIGN-BLOCK"
		"FOREIGN-STACK-P"
		"ALLOCA"
		"*COLLECT-EXPORTED-FUNCTIONS*"
        "DUMP-C-STRUCT"
        "FOREIGN-HEAP-LENGTH"
        "LISP-BYTES-TO-C-BYTES"
        "C-BYTES-TO-LISP-BYTES"
	)
	(:nicknames :ct))

(in-package "C-TYPES")
(setq cl::*compiler-warn-on-undefined-function* nil)

;;; 	examples of valid C type descriptors:
;;; 	:void
;;;		:char
;;;		:unsigned-char
;;; 	:short
;;;		:unsigned-short
;;;     :wide-char
;;; 	:long
;;;		:unsigned-long
;;;		:short-bool
;;;		:long-bool
;;;		:single-float
;;;		:double-float
;;;		:handle
;;; 	(:long 5)						; array of 5 longs
;;; 	(:long *)						; pointer to long
;;; 	((:long *) *)					; pointer to pointer to long
;;; 	(:struct f1 :short f2 :long)	; struct containing a short and a long

(defstruct foreign-function-record
	dll-name
	foreign-name
	lisp-sym
	foreign-address
	jump-table-address)
		
(defstruct dll-record 
	name 
	handle
	(function-records nil))

(defvar *dlls-loaded* nil)
#|
(defun pl::peek-dword (addr)
	(+ (pl::peek-byte addr)
	   (ash (pl::peek-byte (+ addr 1)) 8)
	   (ash (pl::peek-byte (+ addr 2)) 16)
	   (ash (pl::peek-byte (+ addr 3)) 24)))
|#
(defun foreign-jump-table-capacity () (pl::peek-dword (+ (cl::sys-globals-address) 12)))
(defun foreign-jump-table-num-entries () (pl::peek-dword (+ (cl::sys-globals-address) 8)))

;;
;; forward references
;;
(declaim (ftype (function (t) t) ctype-lookup-alias))
(declaim (ftype (function (t) fixnum) determine-c-struct-size))
(declaim (ftype (function (t) t) valid-c-struct-definition))

(defun foreign-type-error (obj)
    (error "Not a foreign pointer: ~A" obj))

(defun determine-c-type-size (definition)
	(if (symbolp definition)
		(let ((typedef (ctype-lookup-alias definition)))
			(if typedef (setq definition typedef))))

	(cond
		((symbolp definition)
		 (ecase definition
			(:void							0)
			((:char :unsigned-char)			1)
			((:short :unsigned-short 
			  :short-bool :wide-char)		2)
			((:long :unsigned-long 
			  :single-float :long-bool 
			  :handle)						4)
			((:double-float	:int64 :uint64)  8)))
		((not (listp definition)) 
		 (error "Invalid C Type descriptor: ~S" definition))
		((eq (car definition) ':struct)
		 (determine-c-struct-size definition))
		((eq (cadr definition) '*) 			4)		;; pointer type
		((integerp (cadr definition))
		 (* (determine-c-type-size (car definition)) (cadr definition)))
		((and (constantp (cadr definition)) (fixnump (symbol-value (cadr definition))))
		 (* (determine-c-type-size (car definition)) (symbol-value (cadr definition))))
		(t (error "Invalid C Type descriptor: ~S" definition))))

(defconstant simple-c-types
	'(:void :char :unsigned-char
	  :short :unsigned-short
	  :long :unsigned-long 
	  :short-bool :long-bool
	  :single-float :double-float
	  :handle :wide-char :int64 :uint64)) 
 
(defun valid-c-type-definition (definition)
	(cond
		((symbolp definition)
		 (let ((typedef (ctype-lookup-alias definition)))
			(if typedef 
				t
		 		(if (member definition simple-c-types)
					t))))
		((not (listp definition)) nil) 
		((eq (car definition) ':struct)
		 (valid-c-struct-definition definition))
		((eq (cadr definition) '*)
		 (valid-c-type-definition (car definition)))
		((or (fixnump (cadr definition)) 
		 	 (and (constantp (cadr definition)) (fixnump (symbol-value (cadr definition)))))
		 (valid-c-type-definition (car definition)))
		(t nil)))

(defun valid-c-struct-definition (definition)
	(unless (eq (car definition) ':struct) (return-from valid-c-struct-definition nil))
	(do ((p (cdr definition)(cddr p)))
		((null p) t)
		(unless (valid-c-type-definition (cadr p)) (return nil))))
	
(defun determine-c-struct-size (definition)
	(do ((size 0)
		 (p (cdr definition)(cddr p)))
		((null p) size)
		(incf size (determine-c-type-size (cadr p)))))

(defun cstruct-definition-equal-p (def1 def2)
	(equal def1 def2))
		
(defasm set-foreign-func-address (func cptr)
	{
		push	ebp
		mov		ebp, esp
		mov		eax, [ebp + (+ ARGS_OFFSET 4)]	;eax = func
		mov		edx, [ebp + ARGS_OFFSET]		;edx = cptr
		mov		edx, [edx + (uvector-offset cl::foreign-heap-ptr-offset)]
		mov		[eax + (uvector-offset cl::function-code-buffer-offset)], edx
		mov		ecx, 1
		pop		ebp
		ret
	})
		
(defun foreign-ptr-to-function (cptr)
	(let ((func (alloc-uvector cl::function-size cl::uvector-kfunction-tag)))
 		(setf (uref func cl::function-environment-offset) nil)
 		(set-foreign-func-address func cptr)
		func))

(defun get-dll-record (dll-name)
	(dolist (rec *dlls-loaded*)
		(if (string-equal (dll-record-name rec) dll-name)
			(return-from get-dll-record rec)))
	(let* ((handle (load-dll dll-name))
		   (rec (make-dll-record :name dll-name :handle handle)))
		(push rec *dlls-loaded*)
		rec))

;;;
;;;	GET-DLL-HANDLE dll-name
;;;	Returns handle of the DLL if it is already loaded,
;;;	otherwise returns it loads the DLL and returns the handle.
;;;
(defun get-dll-handle (dll-name)
	(let ((rec (get-dll-record dll-name)))
		(if rec (dll-record-handle rec) nil)))

;;;
;;;	GET-DLL-FUNCTION-ADDRESS dll-name dll-function-name
;;;
(defun get-dll-function-address (dll-name dll-function-name)
	(let ((dll-handle (get-dll-handle dll-name)))
		(if dll-handle
			(let ((addr (get-dll-proc-address dll-function-name dll-handle)))
				(if addr 
					(return-from get-dll-function-address addr)
					(error "Could not find function ~S in library ~S" dll-function-name dll-name)))
			(error "Could not open library ~S" dll-name))))
	
;;;
;;; INSTALL-DLL-FUNCTION dll-name dll-function-name lisp-symbol
;;;
;;;
;;; INSTALL-DLL-FUNCTION dll-name dll-function-name lisp-symbol
;;;
(defun install-dll-function (dll-name dll-function-name lisp-symbol)
	(let* ((func-address (get-dll-function-address dll-name dll-function-name))
		   (dll-rec (get-dll-record dll-name))
		   (jump-address (cl::allocate-foreign-jump-table-entry func-address))
		   (func-rec (make-foreign-function-record 
						:dll-name dll-name
						:foreign-name dll-function-name
						:lisp-sym lisp-symbol
						:foreign-address func-address
						:jump-table-address jump-address)))
		(setf (symbol-function lisp-symbol) (foreign-ptr-to-function jump-address))
		(push func-rec (dll-record-function-records dll-rec))
		lisp-symbol)) 

;;;
;;;	GENERATE-DYNAMIC-FUNCTION
;;;	This generates a function which is used as a stub for a dynamically linked
;;; function. When this executes, it loads the actual function, and calls it
;;; with the arguments which were passed to it.
;;;
#|
(defun generate-dynamic-function (dll-name dll-function-name lisp-symbol)
	#'(lambda (&rest args)
			(install-dll-function dll-name dll-function-name lisp-symbol)
			(apply lisp-symbol args)))
|#

(defun generate-dynamic-function (dll_name dll_function_name lisp_symbol)
	(defasm dynamic-func-instance ()
		{
			push	ebp
			mov		ebp, esp
			push	edi

		;; push lisp stack context--enter lisp mode
            foreign-call-lisp
                
			push	ecx
			push	ct::dll_name
			push	ct::dll_function_name
			push	ct::lisp_symbol
			mov		ecx, 3
			callf	ct::install-dll-function
			add		esp, 12
			pop		ecx

		;; pop lisp stack context--back to foreign code
            lisp-return-to-foreign
        
			pop		edi
			mov		eax, ct::lisp_symbol
			mov		eax, [eax + (uvector-offset cl::symbol-jump-table-offset)]
			shr		eax, 1
			pop		ebp
			jmp		near [esi + eax + 4]
			ret
		})
	#'dynamic-func-instance)
#|
(defun generate-dynamic-function (dll_name dll_function_name lisp_symbol)
	(defasm dynamic-func-instance ()
		{
			push	ebp
			mov		ebp, esp
			push	edi

		;; pop foreign stack context--return to lisp mode
			begin-atomic
			mov		edx, [esi + (* stack-marker-index-qv-offset 4)]
			cmp		edx, 0
			jg		:t1
			end-atomic
			push	"Tried to pop foreign stack context but none exist!"
			mov		ecx, 1
			callf	error
		:t1
			sub		edx, 4
			mov		[esi + (* stack-marker-index-qv-offset 4)], edx
			end-atomic

			push	ecx
			push	ct::dll_name
			push	ct::dll_function_name
			push	ct::lisp_symbol
			mov		ecx, 3
			callf	ct::install-dll-function
			add		esp, 12
			pop		ecx

		;; push foreign stack context--back to foreign code
			begin-atomic
			mov		eax, [esi + (* stack-marker-index-qv-offset 4)]
			cmp		eax, stack-markers-max
			jl		:t2
			end-atomic
			push	"Ran out of stack markers!"
			mov		ecx, 1
			callf	error
		:t2
			push	eax		;; just to get esp - 4
			mov		[esi + eax*2 + (* stack-markers-qv-offset 4)], esp
			lea		edi, [esi + eax*2 + (* (+ stack-markers-qv-offset 1) 4)]
			pop		eax		;; just to get esp - 4
			add		eax, 4
			mov		[esi + (* stack-marker-index-qv-offset 4)], eax
			end-atomic

			pop		edi
			mov		eax, ct::lisp_symbol
			mov		eax, [eax + (uvector-offset cl::symbol-jump-table-offset)]
			shr		eax, 1
			pop		ebp
			jmp		near [esi + eax + 4]
			ret
		})
	#'dynamic-func-instance)
|#
(defun install-dynamic-foreign-function (dll-name dll-function-name lisp-symbol)
	(setf (symbol-function lisp-symbol) 
		(generate-dynamic-function dll-name dll-function-name lisp-symbol))
	lisp-symbol)

(defun create-foreign-function-name (dll-function-name)
	(intern (concatenate 'string "%FOREIGN-" dll-function-name)))

(defun stack-frame-bytes (arg-type-list)
	(let ((bytes 0))
        (dolist (i arg-type-list bytes)
            (incf bytes
    			(case i
    				(:long 	4)
    				(:double-float 8)
                    (:int64 8)
                    (:uint64 8)
    				(otherwise 4))))))

(defstruct foreign-call-params-info 
    push-forms
    local-vars
    lambda-list
    frame-cells
    pointer-p)

(defun generate-call-params-info (param-list frame-cells &optional (func-ptr nil))
    (let ((push-forms '())
          (local-vars '())
          (num-cells (- frame-cells))
          (lambda-list '())
          (pointer-p nil))
        
        ;; special case for defun-pointer--skip past the pointer
        (when func-ptr 
            ;; start with foreign func ptr
            (push `(x86::compile-foreign-arg ,func-ptr (:void *) ,num-cells) push-forms)
            (incf num-cells 1)
            (setf pointer-p t))
        
        (dolist (i param-list)
			(let ((var (first i))
				  (ctype (second i)))
				(setq ctype (ctypeexpand-all ctype))
				(if (stringp var)
					(setf var 
						(if (equalp var "")
							(gensym)
							(intern (string-upcase var)))))
				(push var lambda-list)
				(cond
					((equal ctype '(:char *))
					 (let ((t3 (gensym)))
				 		(push `(,t3 (ct::create-c-string ,var)) local-vars)
						(push `(x86::compile-foreign-arg ,t3 ,ctype ,num-cells) push-forms)))
					(t (push `(x86::compile-foreign-arg ,var ,ctype ,num-cells) push-forms)))
                (if (member ctype '(:double-float :int64 :uint64))
                    (incf num-cells 2)
                    (incf num-cells 1))))
        (make-foreign-call-params-info 
            :push-forms push-forms
            :local-vars local-vars
            :lambda-list lambda-list
            :frame-cells frame-cells
            :pointer-p pointer-p)))

(defun foreign-defun-stub (name info stack-cleanup-forms return-type call-form &optional call-setup-form)
    (let ((num-cells (foreign-call-params-info-frame-cells info)))
        `(defun ,name ,(nreverse (foreign-call-params-info-lambda-list info))
    				(let (,@(foreign-call-params-info-local-vars info))
    					(x86::save-lisp-registers)
    					,@(foreign-call-params-info-push-forms info)
                        ,@call-setup-form
    					(x86::push-foreign-stack-context)
                        (x86::push-foreign-args-length 
                            ,(if (foreign-call-params-info-pointer-p info) (- num-cells 1) num-cells))
                                ;; if called by DEFUN-POINTER, the args length is one fewer because pointer is 
                                ;; popped as part of function call
                        (x86::copy-foreign-args ,(foreign-call-params-info-frame-cells info))
       					,call-form
                        (x86::pop-foreign-args-length)
                        (x86::copy-return-value ,return-type)
    					,@stack-cleanup-forms
       					(x86::pop-foreign-stack-context)
       					(x86::restore-lisp-registers)
    					(x86::wrap-foreign-return-value ,return-type)))))
    
(defmacro defun-dll (name param-list
		&key (return-type :long) 
			library-name
			entry-name
			(linkage-type :c))
	(unless entry-name (setq entry-name (symbol-name name)))
	(unless library-name
		(error "You must specify a DLL name in the declaration of ~A" name))
	
	(setq return-type (ctypeexpand-all return-type))
	(let* ((arg-type-list (mapcar #'cadr param-list))
		   (stack-cleanup-forms nil)
		   (lisp-symbol (create-foreign-function-name entry-name))
           (frame-cells (/ (stack-frame-bytes arg-type-list) 4))
           (info (generate-call-params-info param-list frame-cells)))

		;side effect--allocates a jump table entry
		(cl::%create-func-table-entry lisp-symbol)

		(if (eq linkage-type :c)
			(setq stack-cleanup-forms `((x86::popargs ,arg-type-list))))

		`(progn
			(install-dynamic-foreign-function ,library-name ,entry-name ',lisp-symbol)
            ,(foreign-defun-stub name info stack-cleanup-forms return-type 
                `(x86::call-foreign-proc ,lisp-symbol)))))

(defmacro defun-com-method 
	(name 
	 param-list 
	 vtable-index 
	 &key (return-type :unsigned-long))

	(setq return-type (ctypeexpand-all return-type))
	(let* ((arg-type-list (mapcar #'cadr param-list))
           (frame-cells (/ (stack-frame-bytes arg-type-list) 4))
           (info (generate-call-params-info param-list frame-cells)))

		`(progn
            ,(foreign-defun-stub name info nil return-type 
                `(x86::call-com-method ,vtable-index)
                `((x86::compile-com-method-address ,vtable-index))))))

;;;
;;; Like DEFUN-DLL, but passes first argument in ECX. Used for interfacing
;;; to C++, functions which are defined with thiscall (any non-static 
;;; member-function which does not take a variable number of arguments).
;;;
(defmacro defun-thiscall (name param-list 
		&key (return-type :long) 
			library-name
			entry-name)
	(unless entry-name (setq entry-name (symbol-name name)))
	(unless library-name
		(error "You must specify a DLL name in the declaration of ~A" name))
	
	(setq return-type (ctypeexpand-all return-type))
	(let* ((arg-type-list (mapcar #'cadr param-list))
		   (lisp-symbol (create-foreign-function-name entry-name))
           (frame-cells (/ (stack-frame-bytes arg-type-list) 4))
           (info (generate-call-params-info param-list frame-cells)))

		;side effect--allocates a jump table entry
		(cl::%create-func-table-entry lisp-symbol)

		`(progn
			(install-dynamic-foreign-function ,library-name ,entry-name ',lisp-symbol)
            ,(foreign-defun-stub name info nil return-type               	
                `(progn
                    {{
						pop	ecx		;; move first argument into ECX register
					}}
					(x86::call-foreign-proc ,lisp-symbol))))))

;;;
;;; Corman Lisp DEFUN-POINTER
;;; Similar to defun-dll, but allow the foreign function pointer to be passed
;;; as the first argument to the defined function. This should be a foreign
;;; pointer.
;;;
#|
Example:

(setf module (cl::load-dll "msvcrt.dll"))
(setf proc (cl::get-dll-proc-address "strlen" module))

(ct::defun-pointer strlen-ptr ((str (:char *))) :return-type :long :linkage-type :c)
(strlen-ptr proc "corman lisp")
   
|#

(defmacro defun-pointer (name param-list 
		&key (return-type :long) 
			(linkage-type :c))
	
	(setq return-type (ctypeexpand-all return-type))
	(let* ((arg-type-list (mapcar #'cadr param-list))
		   (stack-cleanup-forms nil)
           (func-ptr (gensym))
           (frame-cells (+ (/ (stack-frame-bytes arg-type-list) 4) 1)) ;; add 1 for func ptr
           (info (generate-call-params-info param-list frame-cells func-ptr)))

		(if (eq linkage-type :c)
			(setq stack-cleanup-forms `((x86::popargs ,arg-type-list))))

        ;; add function pointer on as first arg (this means adding it to the end as it will get reversed)
        (setf (foreign-call-params-info-lambda-list info)
            (append (foreign-call-params-info-lambda-list info) (list func-ptr)))
        
		`(progn
            ,(foreign-defun-stub name info stack-cleanup-forms return-type 
                `(x86::call-foreign-pointer)))))

;;;
;;; DEFUN-KERNEL macro is used to call kernel functions via the FFI.
;;; This protects the foreign kernel code from negatively affecting
;;; garbage collection, at the cost of thunking between lisp and foreign code.
;;; The called function is assumed to be defined in the kernel, as a foreign
;;; function. These will typically be in the common-lisp package, internal, with
;;; names beginning with '%'.
;;;
(defmacro defun-kernel (name param-list 
		&key (return-type :long) 
			kernel-name  ;; a symbol
			(linkage-type :c))
	
	(setq return-type (ctypeexpand-all return-type))
	(let* ((arg-type-list (mapcar #'cadr param-list))
		   (stack-cleanup-forms nil)
           (frame-cells (/ (stack-frame-bytes arg-type-list) 4))
           (info (generate-call-params-info param-list frame-cells)))

		(if (eq linkage-type :c)
			(setq stack-cleanup-forms `((x86::popargs ,arg-type-list))))

		`(progn
            ,(foreign-defun-stub name info stack-cleanup-forms return-type 
                `(x86::call-foreign-proc ,kernel-name)))))

(defun uvector-offset (index) (- (* 4 index) x86::uvector-tag))

(defconstant uvector-foreign-ptr-size 1)

(defun create-foreign-ptr ()
	(let ((p (cl::alloc-uvector uvector-foreign-ptr-size cl::uvector-foreign-tag)))
		(setf (uref p cl::foreign-heap-ptr-offset) 0)
		p))

;;; Override kernel function
(defun cl::%foreignnode () (ct:create-foreign-ptr))

(defasm %create-c-string (string)
	{
		push	ebp
		mov		ebp, esp
		push	[esi]							; allocate temp variable
		push	ebx
		push	edi
		mov		edx, [ebp + ARGS_OFFSET]		;edx = string
		push	edx
		mov		ecx, 1
		callf	length
		add		esp, 4							;eax = length of string
		mov		[ebp - 4], eax					; save length in [ebp - 4]
		mov		ecx, eax
		add		ecx, 8							;ecx = string-length + 1
		push	ecx	
		mov		ecx, 1
		callf	allocate-c-heap					;eax = c-string handle
		add		esp, 4
		push	eax
		mov		edi, [eax + (uvector-offset foreign-heap-ptr-offset)] 
									;edi = actual c-string
		mov		edx, [ebp + ARGS_OFFSET]		; edx = string
		mov		eax, [edx + (uvector-offset 0)]
		shr		al, 3
		cmp		al, uvector-simple-char-vector-tag
		je		:t2
		mov		edx, [edx + (uvector-offset cl::adjustable-array-vector-offset)]
	:t2
		mov		ecx, [ebp - 4]					; get string length
;;		add		ecx, (* 3 8)
		shr		ecx, 3				;ecx = number of chars to copy ;; unicode mod
;;		shr		ecx, 5				;ecx = number of words to copy
		xor		ebx, ebx
		xor		eax, eax
		jmp		:t1
	:loop
		mov		ax, [edx + ebx*2 + (uvector-offset 2)]		;; unicode mod	
;;		mov		eax, [edx + ebx*4 + (uvector-offset 2)]		
		mov		[edi + ebx], al								;; unicode mod
;;		mov		[edi + ebx*4], eax
		inc		ebx
	:t1
		dec		ecx
		jge		:loop
		
		;; null terminate the c string
		mov		ecx, [ebp - 4]		; get string length
		shr		ecx, 3
		xor		eax, eax
		mov		[edi + ecx], al
		
		pop		eax					; eax = c-string handle
		mov		ecx, 1
		pop		edi
		pop		ebx
		add		esp, 4
		pop		ebp
		ret
	})

(defun create-c-string (string)
	(if (or (foreignp string)(cl::foreign-heap-p string))
		string
		(if (stringp string)
			(%create-c-string string)
			(error "Cannot convert ~A to a C string" string))))

(defasm c-string-to-lisp-string (c-string)
	{
		push	ebp
		mov		ebp, esp
		mov		eax, [ebp + ARGS_OFFSET]
		push	[eax + (uvector-offset foreign-heap-ptr-offset)]
		callp	cl::%stringNode
		add		esp, 4
		mov		ecx, 1
		pop		ebp
		ret
	})

(defasm create-c-long (fixnum)
	{
		push	ebp
		mov		ebp, esp
		push	ebx
		push	edi
		push	32	
		mov		ecx, 1
		callf	allocate-c-heap		;eax = c-string handle
		add		esp, 4
		push	eax
		mov		edi, [eax + (uvector-offset foreign-heap-ptr-offset)] 
									;edi = pointer to c-long address
		mov		edx, [ebp + ARGS_OFFSET]		;edx = integer
		mov		[edi], edx
		
		pop		eax					; eax = c-string handle
		mov		ecx, 1
		pop		edi
		pop		ebx
		pop		ebp
		ret
	})

(defasm c-string-length (c-string)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		edx, [ebp + ARGS_OFFSET]		;; edx = argument
		mov		edx, [edx - 1]					;; edx = foreign pointer
		xor		eax, eax
		and		edx, edx
		jz		:done
		xor		ecx, ecx
	:t2
		cmp		[edx], cl
		jz		short :done
		inc		eax
		inc		edx
		jmp		short :t2
	:done
		shl		eax, 3
		inc		ecx
		pop		ebp
		ret
	})

;;;;
;;;;	Corman Lisp kernel FOREIGNP function.
;;;;
(defasm foreignp (x)
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
		cmp 	dl, (tag-byte uvector-foreign-tag)
		jne 	short :next2
		mov		eax, [esi + t-offset]
	:next2
		pop		ebp
		ret
	})

;;;;
;;;;	Corman Lisp kernel FOREIGN-HEAP-P function.
;;;;
(defasm foreign-heap-p (x)
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
		cmp 	dl, (tag-byte uvector-foreign-heap-tag)
		jne 	short :next2
		mov		eax, [esi + t-offset]
	:next2
		pop		ebp
		ret
	})

(defun foreign-heap-length (x)
    (unless (cl::foreign-heap-p x)
        (cl::signal-type-error x 'cl::foreign-heap))
    (uref x cl::foreign-heap-length-offset))
            
(defun unlink-dll-function (dll-func-rec)
	(install-dynamic-foreign-function 
		(foreign-function-record-dll-name dll-func-rec)
		(foreign-function-record-foreign-name dll-func-rec)
		(foreign-function-record-lisp-sym dll-func-rec)))

(defun unlink-dll (dll-rec)
	(let ((func-recs (dll-record-function-records dll-rec)))
		(dolist (r func-recs)
			(unlink-dll-function r))
		(unload-dll (dll-record-handle dll-rec))
		(setq *dlls-loaded* (remove dll-rec *dlls-loaded*))))
 
(defun unlink-all-dll-functions ()
	(dolist (rec *dlls-loaded*)
		(unlink-dll rec))
	(cl::clear-foreign-jump-table))

(cl::register-save-image-cleanup-func #'unlink-all-dll-functions)
 
(let ((ctype-table (make-hash-table)))
	(defun ctype-install-alias (name ctype)
		(setf (gethash name ctype-table) ctype))
	(defun ctype-lookup-alias (name)
		(gethash name ctype-table)))

(defmacro defctype (name ctype)
	(let ((sym (gensym)))
		`(progn
			(unless (valid-c-type-definition ',ctype)
				(error "Invalid C type specifier: ~S" ',ctype))
			(let ((,sym (ctypeexpand-all ',ctype)))
				(unless ,sym (error "Invalid C type: ~S" ',ctype))
				(ctype-install-alias ',name ,sym)) 
			',name)))

(defun ctypeexpand (x)
	(if (symbolp x)
		(let ((type (ctype-lookup-alias x)))
			(if type type x))
		x))

(defun ctypeexpand-all (x)
	(if (symbolp x)
		(let ((type (ctype-lookup-alias x)))
			(return-from ctypeexpand-all (if type type x))))
	(unless (consp x) (return-from ctypeexpand-all x))
	(if (eq (car x) ':struct)
		(do ((p (cdr x)(cddr p)))
			((null p) x)
			(setf (cadr p) (ctypeexpand-all (cadr p))))
		(if (or (eq (cadr x) '*) 
				(fixnump (cadr x)) 
				(and (constantp (cadr x)) (fixnump (symbol-value (cadr x)))))
			(setf (car x) (ctypeexpand-all (car x)))
			(error "Invalid C type specifier: ~S" x)))
	x)

(defun determine-coerce-type (specifier)
	(if (symbolp specifier)
		(ecase specifier
			((:short :long :char) :integer)
			((:unsigned-short :unsigned-long :unsigned-char) :unsigned-integer)
			((:single-float) :single-float)
			((:double-float) :double-float)
			((:short-bool :long-bool) :bool)
            ((:wide-char) :wide-char)
			(:handle :pointer))
		(if (eq (cadr specifier) '*)
			:pointer
			:reference)))

;;;
;;;		cstruct-slot-info
;;;		Returns 3 values: 
;;;			offset into c-struct
;;;			size in bytes of result
;;;			how to interpret the result (:integer,
;;;				 :unsigned-integer, :float, :bool,
;;;				 :pointer, :reference or :wide-char)
;;;
(defun cstruct-slot-info (specifier access)
	(let ((position 0)
		  slot-type
		  size
		  coerce)
		(do ((p (cdr specifier)(cddr p)))
			((null p)(error "Cannot access C type ~A with access ~A" specifier access))
			(when (eq (car p) access)
				(setq slot-type (cadr p))
				(return))
			(incf position (determine-c-type-size (cadr p))))
		(setq coerce (determine-coerce-type slot-type))
		(setq size (determine-c-type-size slot-type))
		(values position size coerce)))

(defun cstruct-definition-p (descriptor)
	(and (consp descriptor) (eq (car descriptor) ':struct)))

(defun carray-definition-p (descriptor)
	(and (consp descriptor) 
		(or (fixnump (cadr descriptor))
			(and (constantp (cadr descriptor))
				 (fixnump (symbol-value (cadr descriptor)))))))

(defun cpointer-definition-p (descriptor)
	(and (consp descriptor) (eq (cadr descriptor) '*)))

(defun struct-cref-expand (ctype object access value-object value-type)
	(unless (symbolp access)
		(error "Cannot access C structure with access ~A" access))
	(multiple-value-bind
		(offset size coerce)
		(cstruct-slot-info ctype access)
		`(%cref ,object ,offset ,size ,coerce ,value-object ,value-type)))

(defun struct-set-cref-expand (value ctype object access)
	(unless (symbolp access)
		(error "Cannot access C structure with access ~A" access))
	(multiple-value-bind
		(offset size coerce)
		(cstruct-slot-info ctype access)
		`(%set-cref ,object ,offset ,size ,coerce ,value)))

(defun array-cref-expand (ctype object access value-object value-type)
	(if (eq access '*)
		(setq access 0))
	(let ((size (determine-c-type-size (car ctype))))
		`(%cref 
			,object 
			(* ,access ,size)
			,size 
			,(determine-coerce-type (car ctype))
			 ,value-object 
			 ,value-type)))

(defun array-set-cref-expand (value ctype object access)
	(if (eq access '*)
		(setq access 0))
	(let ((size (determine-c-type-size (car ctype))))
		`(%set-cref 
			,object 
			(* ,access ,size)
			,size 
			,(determine-coerce-type (car ctype))
			 ,value)))
	
(defmacro cref (ctype object access 
				&optional value-object value-type alt-value-object)
	(declare (ignore alt-value-object))
	(setq ctype (ctypeexpand-all ctype))
	(unless (valid-c-type-definition ctype)
		(error "Invalid C type specifier: ~S" ctype))
	(cond
		((cstruct-definition-p ctype)
		 (struct-cref-expand ctype object access value-object value-type))
		((carray-definition-p ctype)
		 (array-cref-expand ctype object access value-object value-type))
		((cpointer-definition-p ctype)
		 (array-cref-expand ctype object access value-object value-type))
		(t (error "Cannot access C type ~A using CREF" ctype))))

(defmacro (setf cref) (value ctype object access)
	(setq ctype (ctypeexpand-all ctype))
	(unless (valid-c-type-definition ctype)
		(error "Invalid C type specifier: ~S" ctype))
	(cond
		((cstruct-definition-p ctype)
		 (struct-set-cref-expand value ctype object access))
		((carray-definition-p ctype)
		 (array-set-cref-expand value ctype object access))
		((cpointer-definition-p ctype)
		 (array-set-cref-expand value ctype object access))
		(t (error "Cannot access C type ~A using CREF" ctype))))

(defmacro defcstruct (name params)
	(let ((args nil))
		(dolist (p params)
			(unless (and (= (length p) 2) (symbolp (car p)))
				(error "Invalid C structure definition: ~S" params))
			(push (car p) args)
			(push (cadr p) args))
		`(defctype ,name ,(cons ':struct (nreverse args)))))

(in-package :x86)

;; the arguments should each be fixnums < #x10000, tagged
(defasm %create-positive-bignum (low-16 high-16)
    {
        push    ebp
        mov     ebp, esp
        push    8       ;; tagged fixnum 1
        mov     ecx, 1
        callp   cl::alloc-bignum
        add     esp, 4
        mov     edx, [ebp + (+ ARGS_OFFSET 4)]
        shr     edx, 3
        mov		[eax + (uvector-offset cl::bignum-first-cell-offset)], edx
        mov     edx, [ebp + (+ ARGS_OFFSET 0)]  
        shl     edx, 13      
        or		[eax + (uvector-offset cl::bignum-first-cell-offset)], edx
        mov     ecx, 1
        pop     ebp
        ret
    })
 
(defasm %create-negative-bignum (low-16 high-16)
    {
        push    ebp
        mov     ebp, esp    
        push    8       ;; tagged fixnum 1
        mov     ecx, 1
        callp   cl::alloc-bignum
        add     esp, 4
        mov     edx, [ebp + (+ ARGS_OFFSET 4)]
    begin-atomic      
        shr     edx, 3
        mov     ecx, [ebp + (+ ARGS_OFFSET 0)]
        shl     ecx, 13
        or      edx, ecx
        neg     edx
        mov		[eax + (uvector-offset cl::bignum-first-cell-offset)], edx
        mov     edx, [eax + (uvector-offset 1)]
    end-atomic
        or      edx, 8                  ;; set sign bit
        mov     [eax + (uvector-offset 1)], edx
        mov     ecx, 1
        pop     ebp
        ret
    })    
    
            
(defcodegen ct::%cref (form dest)
	(let ((obj (second form))
		  (position (third form))
		  (bytes (fourth form))
		  (coerce (fifth form))
		  (value-object (sixth form))
		  (value-type (seventh form)))
		(declare (ignore value-object value-type))
		(cl::compile-sub-form obj :dest-stack t)
		(cl::compile-sub-form position :dest-eax-operand t)

		;; make sure the offset is a fixnum
		(parse-assembler
			{
				test	al, 7
				jz		short :t1
				push	"Invalid index, expected a fixnum: ~A"
				push	eax
				mov		ecx, 2
				callf	error
			:t1
				mov     ecx, eax
				pop		edx
				mov 	edx, [edx + (uvector-offset foreign-heap-ptr-offset)]
			})
		(x86::offset-stack 4)
		;; at this point ecx is the tagged offset (in bytes) and 
        ;; edx is the untagged pointer into a foreign heap. EDX is not tagged but
        ;; this should not matter, as it should not be a pointer into the lisp heap.
        ;;
	
		(ecase coerce
			(:integer
				(case bytes
					(1 
					 (parse-assembler
						{
                            xor     eax, eax
                        begin-atomic
                            sar     ecx, 3
							mov 	al, [edx + ecx]
                            xor     ecx, ecx
                        end-atomic
							shl		eax, 24
							sar		eax, 21
	 					}))
					(2
					 (parse-assembler
						{
                            xor     eax, eax
                        begin-atomic
                            sar     ecx, 3
							mov 	ax, [edx + ecx]
                            xor     ecx, ecx
                        end-atomic
							shl		eax, 16
							sar		eax, 13
	 					}))
					(4
					 (parse-assembler
						{
                        begin-atomic
                            sar     ecx, 3
							mov 	eax, [edx + ecx]
                            xor     ecx, ecx
                            cmp     eax, most-positive-fixnum
                            jg      :pos-bignum
                            cmp     eax, most-negative-fixnum
                            jl      :neg-bignum
                            shl     eax, 3
                        end-atomic
                            jmp     short :next
                        :pos-bignum
                            mov     ecx, eax
                            shr     ecx, 16
                            shl     ecx, 3
                            shl     eax, 16
                            shr     eax, 13     ;; eax = low 16 bits, ecx = high 16 bits
                        end-atomic
                            push    eax
                            push    ecx
                            callp   %create-positive-bignum
                            add     esp, 8
                            jmp     short :next
                        :neg-bignum
                            mov     ecx, eax
                            shr     ecx, 16
                            shl     ecx, 3
                            shl     eax, 16
                            shr     eax, 13     ;; eax = low 16 bits, ecx = high 16 bits
                        end-atomic
                            push    eax
                            push    ecx
                            callp   %create-negative-bignum
                            add     esp, 8
					    :next
	 					}))))

			(:unsigned-integer
				(case bytes
					(1 
					 (parse-assembler
						{
                            xor     eax, eax
                        begin-atomic
                            sar     ecx, 3
   							mov 	al, [edx + ecx]
                            xor     ecx, ecx
                        end-atomic
							shl		eax, 24
							shr		eax, 21
	 					}))
					(2
					 (parse-assembler
						{
                            xor     eax, eax
                        begin-atomic
                            sar     ecx, 3
							mov 	ax, [edx + ecx]
                            xor     ecx, ecx
                        end-atomic
							shl		eax, 16
							shr		eax, 13
	 					}))
					(4
					 (parse-assembler
						{
                        begin-atomic
                            sar     ecx, 3
							mov 	eax, [edx + ecx]
                            xor     ecx, ecx
                            cmp     eax, most-positive-fixnum
                            ja      :bignum
                            shl     eax, 3
                        end-atomic
                            jmp     short :next
                        :bignum
                            mov     ecx, eax
                            shr     ecx, 16
                            shl     ecx, 3
                            shl     eax, 16
                            shr     eax, 13     ;; eax = low 16 bits, ecx = high 16 bits
                        end-atomic
                            push    eax
                            push    ecx
                            callp   %create-positive-bignum
                            add     esp, 8
					    :next
   	 					}))))
				
			(:pointer
				(parse-assembler
					{
                        push    ecx
                        push    edx
                        xor     ecx, ecx
 						callp	ct:create-foreign-ptr
                        pop     edx
                        pop     ecx                                           
                    begin-atomic
                        sar     ecx, 3
						mov		edx, [edx + ecx]
						mov		[eax + (uvector-offset cl::foreign-heap-ptr-offset)], edx
                        xor     ecx, ecx
                        xor     edx, edx
                    end-atomic
	 				}))

			(:single-float
				(parse-assembler
					{
                    begin-atomic
                        shr     ecx, 3
						fld.single [edx + ecx]
                        xor     ecx, ecx
                    end-atomic
						push	0
						push	0
						callp	cl::%single-float-node
						add		esp, 8
						fstp.single	[eax + (uvector-offset cl::single-float-offset)]
	 				}))

			(:double-float
				(parse-assembler
					{
                    begin-atomic
                        shr     ecx, 3
					    fld     [edx + ecx]
                        xor     ecx, ecx
                    end-atomic
						push	0
						push	0
						callp	cl::%double-float-node
						add     esp, 8
						fstp	[eax + (uvector-offset cl::double-float-offset)]
	 				}))

            (:wide-char
				(parse-assembler
					{
                        xor     eax, eax
                    begin-atomic
                        shr     ecx, 3
                        mov		ax, [edx + ecx]
                        xor     ecx, ecx
                    end-atomic
                        shl     eax, 8
                        inc     eax
	 				}))
            
			(:reference
				(parse-assembler
					{
                        push    ecx
                        push    edx
                        xor     ecx, ecx
 						callp	ct:create-foreign-ptr
                        pop     edx
                        pop     ecx 
                    begin-atomic
                        shr     ecx, 3                                          
   						lea		edx, [edx + ecx]
						mov		[eax + (uvector-offset cl::foreign-heap-ptr-offset)], edx
                        xor     ecx, ecx
                    end-atomic
	 				})))
		
		(if (eq dest :dest-stack)
			(progn
				(parse-assembler
					{
						push	eax
					})
				(x86::offset-stack -4))
			(parse-assembler
				{
					mov		ecx, 1
				})))
	t)

(defcodegen ct::%set-cref (form dest)
	(let ((obj (second form))
		  (position (third form))
		  (bytes (fourth form))
		  (coerce (fifth form))
		  (value (sixth form)))

		(cl::compile-sub-form value :dest-stack t)
		(cl::compile-sub-form obj :dest-stack t)
		(cl::compile-sub-form position :dest-eax-operand t)

		;; make sure the offset is a fixnum
		(parse-assembler
			{
				test	al, 7
				jz		short :t1
				push	"Invalid index, expected a fixnum: ~A"
				push	eax
				mov		ecx, 2
				callf	error
			:t1
				shr 	eax, 3
				pop		edx			;; edx = obj
				mov 	edx, [edx + (uvector-offset foreign-heap-ptr-offset)] ;; edx = c object address
				add		edx, eax	;; edx = target address of store operation
				pop		eax			;; eax = value
				push	eax			;; save value
			})
		(x86::offset-stack 4)
		
		(ecase coerce
			(:integer
				(case bytes
					(1 
					 (parse-assembler
						{
							push	edx
							callp	%get-long-safe ;; get low 16 bits in ax
							pop		edx
							mov 	[edx], al
	 					}))
					(2
					 (parse-assembler
						{
							push	edx
							callp	%get-long-safe ;; get low 16 bits in ax
							pop		edx
							mov 	[edx], ax
	 					}))
					(4
					 (parse-assembler
						{
							push	edx
							callp	%get-long-safe ;; get low 16 bits in ax, high bits in dx
                            mov     ecx, edx
							pop		edx
							mov 	[edx], ax
                            mov     [edx + 2], cx
	 					}))))

			(:unsigned-integer
				(case bytes
					(1 
					 (parse-assembler
						{
							push	edx
							callp	%get-unsigned-long-safe ;; get low 16 bits in ax
							pop		edx
							mov 	[edx], al
	 					}))
					(2
					 (parse-assembler
						{
							push	edx
							callp	%get-unsigned-long-safe ;; get low 16 bits in ax
   							pop		edx
							mov 	[edx], ax
	 					}))
					(4
					 (parse-assembler
						{
							push	edx
							callp	%get-unsigned-long-safe ;; get low 16 bits in ax
   							mov     ecx, edx
							pop		edx
							mov 	[edx], ax
                            mov     [edx + 2], cx
	 					}))))
			
			(:single-float
				(parse-assembler
					{
						push	edx
						mov 	edx, eax
						and 	edx, 7
						cmp 	edx, uvector-tag
						je 		short :t1
					:err
						push	"Not a :single-float: ~S"
						push	eax
						mov		ecx, 2
						callf	error
					:t1
						mov 	edx, [eax - uvector-tag]
						shr 	dl, 3
						cmp 	dl, cl::uvector-single-float-tag
						jne		:err
					:t2
						fld.single [eax + (uvector-offset cl::single-float-offset)]
						pop		edx
						fstp.single	[edx]
   	 				}))
			
			(:double-float
				(parse-assembler
					{
						push	edx
						mov 	edx, eax
						and 	edx, 7
						cmp 	edx, uvector-tag
						je 		short :t1
					:err
						push	"Not a :double-float: ~S"
						push	eax
						mov		ecx, 2
						callf	error
					:t1
						mov 	edx, [eax - uvector-tag]
						shr 	dl, 3
						cmp 	dl, cl::uvector-double-float-tag
						jne		:err
					:t2
						pop		edx
						fld     [eax + (uvector-offset cl::double-float-offset)]
						fstp	[edx]
	 				}))
			
            (:wide-char
				(parse-assembler
					{
                        cmp     al, 1
                        je      short :t1
					:err
						push	"Not a character: ~S"
						push	eax
						mov		ecx, 2
						callf	error
					:t1
                        shl     eax, 8
                        shr     eax, 16
                        mov     [edx], ax
                    }))
                                                   	
			(:pointer
				(parse-assembler
					{
						mov		eax, [eax + (uvector-offset cl::foreign-heap-ptr-offset)]
						mov		[edx], eax
	 				})))
		
		(unless (eq dest :dest-stack)
			(progn
				(parse-assembler
					{
						pop		eax
						mov		ecx, 1
					})
				(x86::offset-stack 4))))
	t)

(in-package :c-types)

(pl:defasm cpointer-value (cpointer)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 1
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		edx, [ebp + ARGS_OFFSET]		;; edx = argument

        ;; check type
		mov		eax, edx
		and		eax, 7
		cmp		eax, uvector-tag
		jne		short :type-error
		mov 	eax, [edx - uvector-tag]
		cmp 	al, (tag-byte uvector-foreign-tag)
        je      :next1
 		cmp 	al, (tag-byte uvector-foreign-heap-tag)
        je      :next1
    :type-error
        push    edx
        callp   ct::foreign-type-error
              
    :next1
	begin-atomic
		mov		eax, [edx + (uvector-offset cl::foreign-heap-ptr-offset)]  ;; untagged pointer in eax
		                                                                ;; probably safe, but we'll be careful anyway
        cmp     eax, most-positive-fixnum
        ja      :bignum
        shl     eax, 3
    end-atomic
        jmp     short :next
    :bignum
        mov     ecx, eax
        shr     ecx, 16
        shl     ecx, 3
        shl     eax, 16
        shr     eax, 13     ;; eax = low 16 bits, ecx = high 16 bits
    end-atomic
        push    eax
        push    ecx
        callp   %create-positive-bignum
        add     esp, 8
    :next
		mov		ecx, 1
		pop		ebp
		ret
	})

(pl:defasm |SETF CPOINTER-VALUE| (value cpointer)
	{
		push	ebp
		mov		ebp, esp
		cmp 	ecx, 2
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		edx, [ebp + ARGS_OFFSET]		;; edx = cpointer
    
        ;; check type
		mov		eax, edx
		and		eax, 7
		cmp		eax, uvector-tag
		jne		short :type-error
		mov 	eax, [edx - uvector-tag]
		cmp 	al, (tag-byte uvector-foreign-tag)
        je      :next1
 		cmp 	al, (tag-byte uvector-foreign-heap-tag)
        je      :next1
    :type-error
        push    edx
        callp   ct::foreign-type-error
              
    :next1
		mov		eax, [ebp + (+ ARGS_OFFSET 4)]	;; eax = number
		test	eax, 7
		jnz		short :t2
		mov		ecx, eax
		shr		ecx, 3
		mov		[edx + (uvector-offset cl::foreign-heap-ptr-offset)], ecx
		jmp 	short :t4
	:t2
		mov		ecx, eax
		and		ecx, 7
		cmp		ecx, uvector-tag
		jne		short :err
		mov 	ecx, [eax - uvector-tag]
		shr 	cl, 3
		cmp 	cl, uvector-bignum-tag
		jne 	short :err
		mov 	ecx, [eax + (uvector-offset cl::bignum-first-cell-offset)]
		mov		[edx + (uvector-offset cl::foreign-heap-ptr-offset)], ecx
		jmp 	short :t4
	:err
		push	"Invalid cpointer value: ~A"
		push	eax
		mov		ecx, 2
		callf	error
	:t4
		mov		ecx, 1				
		pop		ebp
		ret
	})
		
(cl::register-setf-function 'cpointer-value '|SETF CPOINTER-VALUE|)

(defun cpointer= (cp1 cp2)
	(= (cpointer-value cp1)(cpointer-value cp2)))

(defun cpointer-null (cp) (declare (special ct:null)) (cpointer= cp ct:null))

;;;
;;;	CormanLisp FOREIGN-PTR-TO-INT function.
;;;
(defun foreign-ptr-to-int (p)
	(cpointer-value p))

;;;
;;;	CormanLisp INT-TO-FOREIGN-PTR function.
;;;
(defun int-to-foreign-ptr (i)
	(if (< i 0) 
		(setf i (+ i #x100000000)))
	(let ((p (create-foreign-ptr)))
		(setf (cpointer-value p) i)
		p))

(defun sizeof (c-type-specifier)
	(determine-c-type-size c-type-specifier))

(defun offsetof (c-type-specifier struct-field)
	(values (ct::cstruct-slot-info (ct::ctypeexpand-all c-type-specifier) struct-field)))

(defun valid-callback-arg-type (ctype)
	(setq ctype (ctypeexpand-all ctype))
	(if (member ctype simple-c-types)
		t 
	  	(and (consp ctype)
			  (consp (cdr ctype)) 
			 (valid-callback-arg-type (first ctype))
			 (or (integerp (second ctype))(eq (second ctype) '*)))))

;;; We only execute the following macro for its side effects
(defmacro ensure-func-table-entry (symbol)
	(if (zerop (uref symbol cl::symbol-jump-table-offset))
		(cl::%create-func-table-entry symbol))
	nil)

;;;
;;; Redefine Common Lisp MULTIPLE-VALUE-BIND macro to 
;;; make the VALUE-FORM be evaluated in the correct lexical context.
;;; From JP Massar.
;;;
(defmacro cl::multiple-value-bind (vars value-form &rest forms)
    (let ((declarations nil))
        ;; look for declarations
        (do* ((f forms (cdr f)))
            ((null f) (setq forms f))
            (if (and (consp (car f)) (eq (caar f) 'declare))
                (push (car f) declarations)
                (progn (setq forms f) (return))))
        
        (let* ((gensyms (mapcar #'(lambda (x) (gensym (symbol-name x))) vars))
               (bindings (mapcar #'list vars gensyms)))
            `(let ,gensyms
                (multiple-value-setq ,gensyms ,value-form)
                (let ,bindings
                    ,@(nreverse declarations) 
                    ,@forms)))))
	
(defmacro define-callback-func (name arg-list body &key (linkage :c) (create-heap-handler nil))
	(let* ((syms nil)
		   (lisp-func nil)
		   (foreign-func nil)
		   (arg-conversion-forms nil)
		   internal-name
	       (docstring nil)
   		   (num-args (length arg-list))
		   (sym-t1 (gensym)))
		(dolist (x arg-list)
			(unless (and (listp x) 
					 	  (= (length x) 2) 
						  (valid-callback-arg-type (second x)))
				(error "Invalid argument specification in DEFUN-CALLBACK form: ~A" x))
			(push (first x) syms))
		(setq syms (nreverse syms))
		(setq internal-name (intern 
			(concatenate 'string "%" (symbol-name name) "-internal")))
        (multiple-value-bind (doc decls body bad-decls)
            (cl::parse-doc-decls-body body)
            (declare (ignore doc))
            (when bad-decls (error "Declarations found in body of callback"))
            (setq lisp-func 
                `(defun ,internal-name ,syms (let () ,@decls (block ,name ,@body))))
            (setq docstring doc))
        (let ((param-offset 0))
			(dolist (x arg-list)
				(let ((arg-type (ctypeexpand-all (second x))))
					(cond
						((eq arg-type :char)
						 (push `(x86::push-signed-char-lisp-arg ,param-offset) arg-conversion-forms))
						((eq arg-type :unsigned-char)
						 (push `(x86::push-unsigned-char-lisp-arg ,param-offset) arg-conversion-forms))
						((eq arg-type :short)
						 (push `(x86::push-signed-short-lisp-arg ,param-offset) arg-conversion-forms))
						((eq arg-type :unsigned-short)
						 (push `(x86::push-unsigned-short-lisp-arg ,param-offset) arg-conversion-forms))
						((eq arg-type :wide-char)
						 (push `(x86::push-wide-char-lisp-arg ,param-offset) arg-conversion-forms))
  						((eq arg-type :long)
						 (push `(x86::push-signed-long-lisp-arg ,param-offset) arg-conversion-forms))
						((eq arg-type :unsigned-long)
						 (push `(x86::push-unsigned-long-lisp-arg ,param-offset) arg-conversion-forms))
						((eq arg-type :short-bool)
						 (push `(x86::push-short-bool-lisp-arg ,param-offset) arg-conversion-forms))
						((eq arg-type :long-bool)
						 (push `(x86::push-long-bool-lisp-arg ,param-offset) arg-conversion-forms))
;						((eq arg-type :handle)
;						 (push `(x86::push-long-handle-lisp-arg ,param-offset) arg-conversion-forms))
						((eq arg-type :single-float)
						 (push `(x86::push-single-float-lisp-arg ,param-offset) arg-conversion-forms))						 
						((eq arg-type :double-float)
						 (push `(x86::push-double-float-lisp-arg ,param-offset) arg-conversion-forms)
						 (incf param-offset 4))
						((eq arg-type :int64)
						 (push `(x86::push-int64-lisp-arg ,param-offset) arg-conversion-forms)
						 (incf param-offset 4))
                        ((eq arg-type :uint64)
						 (push `(x86::push-uint64-lisp-arg ,param-offset) arg-conversion-forms)
						 (incf param-offset 4))
   						(t
						 (push `(x86::push-pointer-lisp-arg ,param-offset) arg-conversion-forms))))
				(incf param-offset 4))
			(setq foreign-func 
				`(defun ,name ,syms
                    ,@(when docstring (list docstring))
   					(declare (optimize (speed 3)(safety 0)))	; disable arg checking
					(declare (ignore ,@syms))					; disable "unused variable" warnings
					(x86::setup-lisp-registers)
					(x86::push-lisp-stack-context)
					,@(if create-heap-handler
						`((x86::link-heap-handler)))
					,@(nreverse arg-conversion-forms)
					(x86::mov-ecx-num ,num-args)
					(x86::call-lisp-proc ,internal-name)
					(x86::pop-lisp-args ,num-args)
					(x86::return-lisp-val-as-c) 
					,@(if create-heap-handler
						`((x86::unlink-heap-handler)))					
					(x86::pop-lisp-stack-context)
                    (x86::copy-foreign-return-val-to-eax)
					(x86::restore-c-registers)
					,@(if (eq linkage :pascal)
						`((x86::return-from-pascal ,num-args)))))
			`(let ((,sym-t1 (cl::create-callback-thunk ',name)))
					 (setf (get ',name 'ct::callback-thunk) ,sym-t1)
					 (ensure-func-table-entry ,internal-name)	;; make sure there is a jump entry 
					 ,lisp-func				;; define lisp function
					 ,foreign-func))))		;; define foreign function

(defvar *collect-exported-functions* nil)	;; used by COMPILE-FILE and COMPILE-DLL
(defvar *callback-registry* 
    (make-hash-table 
        :test #'equal 
        :synchronized t)) ;; maps strings to callback symbols

(defmacro defun-callback (name arg-list &rest body)
	`(define-callback-func ,name ,arg-list ,body :linkage :pascal))

(defmacro defun-c-callback (name arg-list &rest body)
	`(define-callback-func ,name ,arg-list ,body :linkage :c))

(defmacro defun-direct-callback (name arg-list &rest body)
	`(define-callback-func ,name ,arg-list ,body :linkage :pascal :create-heap-handler t))

(defmacro defun-direct-c-callback (name arg-list &rest body)
	`(define-callback-func ,name ,arg-list ,body :linkage :c :create-heap-handler t))

(defun defun-dll-export-body (name arg-list body linkage)
	(let ((prototype nil)
		  (sym name))
		(when (stringp (first body))
			(setf prototype (first body))
			(setf body (cdr body)))
		(when (consp name)
			;; map export string name to symbol			
			(setf (gethash (cadr name) *callback-registry*) (car name))
			(setf sym (car name)))
		`(eval-when (:execute :load-toplevel :compile-toplevel)
			,@(if (consp name)
				`((setf (gethash ',(cadr name) *callback-registry*) ',(car name)))
				nil)
			(if *collect-exported-functions*
				(vector-push-extend ',(list name prototype) *collect-exported-functions*))
			(define-callback-func ,sym ,arg-list ,body :linkage ,linkage :create-heap-handler t))))

(defmacro defun-dll-export-function (name arg-list &rest body)
	(defun-dll-export-body name arg-list body ':pascal))

(defmacro defun-dll-export-c-function (name arg-list &rest body)
	(defun-dll-export-body name arg-list body ':c))

;;; support code generation operators for defun-callback macro
(in-package :x86)

(defcodegen setup-lisp-registers (form dest)
	(declare (ignore form dest))
	(parse-assembler
		{
			push	esi
			push	edi
			push	ebx
			mov		eax, 'cl::%load-qv-reg
			mov		eax, [eax + (uvector-offset symbol-function-offset)]	; get function binding
			mov		eax, [eax - cons-tag]
			call	[eax + (uvector-offset function-code-buffer-offset)]
		})
	t)
				
(defcodegen restore-c-registers (form dest)
	(declare (ignore form dest))
	(parse-assembler
		{
			pop		ebx
			pop		edi
			pop		esi
		})
	t)

(defcodegen call-lisp-proc (form dest)
	(declare (ignore dest))
	(let* ((sym (second form))
		   (env-offset (* (uref sym symbol-jump-table-offset) 4))
		   (jump-offset	(+ env-offset 4)))
		(if (zerop env-offset)
			(error "The function ~A does not have a jump table offset and cannot be called" sym))
		(parse-assembler
			{
				mov		edi, [esi + env-offset]
				add-env-table-ref sym
		  		call 	[esi + jump-offset]
				add-jump-table-ref sym
			}))
	t)

(defcodegen pop-lisp-args (form dest)
	(declare (ignore dest))
	(let* ((numargs (second form))
		   (bytes (* 4 numargs)))
		(parse-assembler
			{
				add		esp, bytes
			}))
	t)

(defcodegen return-lisp-val-as-c (form dest)
	(declare (ignore form dest))
	(parse-assembler
		{
			mov		ecx, eax
			and		ecx, 7			;; if fixnum, return that
			jnz		short :t2
            cmp     eax, 0
            jl      short :neg-integer
            mov     ecx, -1
            callp   x86::%get-foreign-unsigned-long 
            jmp     :exit
        :neg-integer
            mov     ecx, -1
            callp   x86::%get-foreign-long 
            jmp     :exit
		:t2
            cmp     al, 1
            je      :char
			cmp		ecx, uvector-tag
			jne		:t3           ;; not handling return of short floats from callback yet
			mov 	cl, [eax - uvector-tag]
			shr 	cl, 3
            cmp     cl, uvector-bignum-tag
            jne     short :t7
            mov     cl, [eax + (uvector-offset 1)]
            and     cl, 8               ;; negative bignum?
            jnz     short :neg-bignum
            mov     ecx, -1            
            callp   x86::%get-foreign-unsigned-long 
            jmp     short :exit
        :neg-bignum
            mov     ecx, -1
            callp   x86::%get-foreign-long 
            jmp     short :exit                
            
        :t7
			cmp 	cl, uvector-foreign-tag
			jne		short :t4
        begin-atomic
			mov		eax, [eax + (uvector-offset foreign-heap-ptr-offset)]
            mov     [esi + (- (* foreign-cells-qv-tos 4) 4)], eax
            xor     eax, eax
        end-atomic
			jmp		short :exit
		:t4 
			cmp 	cl, uvector-foreign-heap-tag
			jne		short :t5
        begin-atomic
			mov		eax, [eax + (uvector-offset foreign-heap-ptr-offset)]
            mov     [esi + (- (* foreign-cells-qv-tos 4) 4)], eax
            xor     eax, eax
        end-atomic
 			jmp		short :exit
		:t5
			cmp		cl, uvector-double-float-tag
			jne		short :t6
			fld		[eax + (uvector-offset cl::double-float-offset)]
            ;; store 0 in eax, in case caller is not expecting a float they will get that
			mov		eax, 0
            mov     [esi + (- (* foreign-cells-qv-tos 4) 4)], eax
   			jmp		short :exit
		:t6
			cmp		cl, uvector-single-float-tag
			jne		short :t3
			fld.single [eax + (uvector-offset cl::single-float-offset)]
            ;; store 0 in eax, in case caller is not expecting a float they will get that
			mov		eax, 0
            mov     [esi + (- (* foreign-cells-qv-tos 4) 4)], eax
      		jmp		short :exit
        :char
            shl     eax, 8
            shr     eax, 16
            jmp     short :exit
		:t3
			xor		eax, eax
		:exit
		})
	t)

(defcodegen copy-foreign-return-val-to-eax (form dest)
	(declare (ignore form dest))
	(parse-assembler
		{
            mov    eax, [esi + (- (* foreign-cells-qv-tos 4) 4)]
        })
    t)
            
(defcodegen push-signed-char-lisp-arg (form dest)
	(declare (ignore dest))
	(let ((offset (+ ARGS_OFFSET (second form))))
		(parse-assembler
			{
                xor     eax, eax
				mov		al, [ebp + offset]
				shl		eax, 24
				sar		eax, 21
				push	eax
			}))
	t)

(defcodegen push-unsigned-char-lisp-arg (form dest)
	(declare (ignore dest))
	(let ((offset (+ ARGS_OFFSET (second form))))
		(parse-assembler
			{
                xor     eax, eax
				mov		al, [ebp + offset]
   				shl		eax, 24
				shr		eax, 21
				push	eax
			}))
	t)

(defcodegen push-signed-short-lisp-arg (form dest)
	(declare (ignore dest))
	(let ((offset (+ ARGS_OFFSET (second form))))
		(parse-assembler
			{
                xor     eax, eax
   				mov		ax, [ebp + offset]
				shl		eax, 16
				sar		eax, 13
				push	eax
			}))
	t)

(defcodegen push-unsigned-short-lisp-arg (form dest)
	(declare (ignore dest))
	(let ((offset (+ ARGS_OFFSET (second form))))
		(parse-assembler
			{
                xor     eax, eax
				mov		ax, [ebp + offset]
				shl		eax, 16
				shr		eax, 13
				push	eax
			}))
	t)

(defcodegen push-wide-char-lisp-arg (form dest)
	(declare (ignore dest))
	(let ((offset (+ ARGS_OFFSET (second form))))
		(parse-assembler
			{
                xor     eax, eax
				mov		ax, [ebp + offset]
				shl		eax, 8
				inc     eax
				push	eax
			}))
	t)

(defcodegen push-signed-long-lisp-arg (form dest)
	(declare (ignore dest))
	(let ((offset (+ ARGS_OFFSET (second form))))
		(parse-assembler
			{
            ;; top cell gets converted into Lisp integer in eax
            begin-atomic
                mov     eax, [ebp + offset]
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
                callp   cl::alloc-bignum
                add     esp, 4
            begin-atomic
                mov     edx, [ebp + offset]
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
            end-atomic
                        
            push	eax
        	}))
	t)

(defcodegen push-unsigned-long-lisp-arg (form dest)
	(declare (ignore dest))
	(let ((offset (+ ARGS_OFFSET (second form))))
		(parse-assembler
			{
            ;; carefully copy DWORD to foreign region of QV (top cell)
            begin-atomic
                mov     eax, [ebp + offset]
                mov     [esi + (- (* foreign-cells-qv-tos 4) 4)], eax
                xor     eax, eax
            end-atomic
            ;; top cell gets converted into Lisp integer in eax
				push	(* 8 -1)
				callp	x86::create-foreign-lisp-unsigned-integer
				add		esp, 4
				push	eax
  			}))
	t)

(defcodegen push-int64-lisp-arg (form dest)
	(declare (ignore dest))
	(let ((offset (+ ARGS_OFFSET (second form))))
		(parse-assembler
			{
            ;; carefully copy Int64 to foreign region of QV (top cell)
            begin-atomic
                mov     eax, [ebp + offset]
                mov     [esi + (- (* foreign-cells-qv-tos 4) 8)], eax
                mov     eax, [ebp + (+ offset 4)]
                mov     [esi + (- (* foreign-cells-qv-tos 4) 4)], eax
                xor     eax, eax
            end-atomic
            ;; top 2 cells gets converted into Lisp integer in eax
				push	(* 8 -2)
				callp	x86::create-foreign-lisp-int64
				add		esp, 4
				push	eax
  			}))
	t)

(defcodegen push-uint64-lisp-arg (form dest)
	(declare (ignore dest))
	(let ((offset (+ ARGS_OFFSET (second form))))
		(parse-assembler
			{
            ;; carefully copy UInt64 to foreign region of QV (top cell)
            begin-atomic
                mov     eax, [ebp + offset]
                mov     [esi + (- (* foreign-cells-qv-tos 4) 8)], eax
                mov     eax, [ebp + (+ offset 4)]
                mov     [esi + (- (* foreign-cells-qv-tos 4) 4)], eax
                xor     eax, eax
            end-atomic
            ;; top cell gets converted into Lisp integer in eax
				push	(* 8 -2)
				callp	x86::create-foreign-lisp-uint64
				add		esp, 4
				push	eax
  			}))
	t)

(defcodegen push-single-float-lisp-arg (form dest)
	(declare (ignore dest))
	(let ((offset (+ ARGS_OFFSET (second form))))
		(parse-assembler
			{
				mov		ecx, 0
				callf	cl::alloc-single-float
				fld.single [ebp + offset]		
                fstp.single [eax + (uvector-offset cl::single-float-offset)]
				push	eax
			}))
	t)

(defcodegen push-double-float-lisp-arg (form dest)
	(declare (ignore dest))
	(let ((offset (+ ARGS_OFFSET (second form))))
		(parse-assembler
			{
				mov		ecx, 0
				callf	cl::alloc-double-float				
				fld     [ebp + offset]		
				fstp    [eax + (uvector-offset cl::double-float-offset)]   
				push	eax
			}))
	t)

(defcodegen push-pointer-lisp-arg (form dest)
	(declare (ignore dest))
	(let ((offset (+ ARGS_OFFSET (second form))))
		(parse-assembler
			{
				xor		ecx, ecx
				callf	ct::create-foreign-ptr
            begin-atomic
				mov		ecx, [ebp + offset]
				mov		[eax + (uvector-offset cl::foreign-heap-ptr-offset)], ecx 
                xor     ecx, ecx
            end-atomic
				push	eax
			}))
	t)

(defcodegen return-from-pascal (form dest)
	(declare (ignore dest))
	(let ((numbytes (* (second form) 4)))
		(parse-assembler
			{
				mov		esp, ebp
				pop		ebp
				ret		numbytes
			}))
	t)

(ct::ensure-func-table-entry create-foreign-ptr)

(defun ct::get-callback-procinst (callback-func-sym)
	(when (stringp callback-func-sym)
		(setf callback-func-sym (gethash callback-func-sym ct::*callback-registry*)))
	(if (null callback-func-sym)
		(return-from ct::get-callback-procinst nil))	
	(get callback-func-sym 'ct::callback-thunk))

;; This function gets accessed via the COM interface from 
;; foreign threads.
(defun cl::get-callback (name)
	(ct::get-callback-procinst name))

;;;
;;;	CormanLisp CPOINTERP function.
;;;
(defasm ct::cpointerp (p)
	{
		push	ebp
		mov		ebp, esp
		cmp     ecx, 1          
		jz      short :t1                
		callp 	_wrong-number-of-args-error
	:t1
		mov		eax, [ebp + ARGS_OFFSET]	;eax = p
		mov		edx, eax
		and		edx, 7
		cmp		edx, uvector-tag
		jne		short :not-foreign
		mov		dl, [eax - uvector-tag]
		shr		dl, 3
		cmp		dl, uvector-foreign-tag
		je		short :is-foreign
		cmp		dl, uvector-foreign-heap-tag
		je		short :is-foreign
		cmp		dl, uvector-foreign-stack-tag
		je		short :is-foreign
		jmp		short :not-foreign
	:is-foreign
		mov		eax, [esi + t-offset]
		jmp		short :exit
	:not-foreign
		mov		eax, [esi]
	:exit
		pop		ebp
		ret
	})

(defun ct::foreign-stack-p (x)
	(and (uvectorp x)(eq (cl::uvector-type-bits x) uvector-foreign-stack-tag)))
	
(defasm ct::%memcmp (cp1 cp2 count)
	{
		push	ebp
		mov		ebp, esp
		push	esi
		push	edi
		mov		esi, [ebp + (+ ARGS_OFFSET 8)]	;; esi = cp1
		mov		edi, [ebp + (+ ARGS_OFFSET 4)]	;; edi = cp2
		mov		ecx, [ebp + (+ ARGS_OFFSET 0)]	;; ecx = count
		shr		ecx, 3							;; ecx = unwrapped count
	:loop1
		cmp		ecx, 3
		jle		short :loop2
		mov		eax, [esi]
		cmp		eax, [edi]
		jne		:diff
		add		esi, 4
		add		edi, 4
		sub		ecx, 4
		jmp		short :loop1
	:loop2
		cmp		ecx, 0
		je		short :same
		mov		al, [esi]
		cmp		al, [edi]
		jne		:diff
		inc		esi
		inc		edi
		dec		ecx
		jmp		short :loop2
	:diff
		jl		:less
	:greater
		mov		eax, 8			;; eax = 1
		jmp		short :exit
	:less
		mov		eax, -8
		jmp		short :exit
	:same
		mov		eax, 0
	:exit
		mov		ecx, 1
		pop		edi
		pop		esi
		mov		esp, ebp
		pop		ebp
		ret
	})

(in-package :ct)			
(defun ct:memcmp (buf1 buf2 count)
	(unless (ct:cpointerp buf1)
		(error "Not a foreign pointer: ~A" buf1))
	(unless (ct:cpointerp buf2)
		(error "Not a foreign pointer: ~A" buf2))
	(unless (fixnump count)	
		(error "Third argument to memcmp must be a fixnum, got ~A" count))
	(%memcmp (uref buf1 1)(uref buf2 1) count))

(ct:defun-dll memcpy ((dest (:void *))(src (:void *))(count :unsigned-long))
   :return-type (:void *)
   :library-name "msvcrt.dll"
   :entry-name "memcpy"
   :linkage-type :c)

(defun malloc (size) (cl::allocate-c-heap size))
(defun free (cptr) (cl::deallocate-c-heap cptr))

(defun unicode-to-lisp-string (buf)
	(let ((x (make-array 0 
				:element-type 'character 
				:fill-pointer t 
				:adjustable t)))
		(do* ((index 0 (+ index 1))
			  (c (ct:cref (:wide-char *) buf index)
				 (ct:cref (:wide-char *) buf index)))
			((char= c #\nul) x)
			(vector-push-extend c x))))

(defun lisp-string-to-unicode (str  &optional (buf nil))
	(let ((strlength (length str)))
		(unless buf
			(setf buf (ct:malloc (* (+ strlength 1) 2))))
		(do* ((i 0 (+ i 1)))
			((= i strlength)
			 (setf (ct:cref (:wide-char *) buf i) #\nul)
			 buf)
			(setf (ct:cref (:wide-char *) buf i) (elt str i)))))

(defun lisp-string-to-c-string (str &optional (cstr nil))
	(let ((strlength (length str)))
		(unless cstr
			(setf cstr (ct:malloc (+ strlength 1))))
		(dotimes (i strlength)
			(setf (ct:cref (:unsigned-char *) cstr i) (char-int (elt str i))))
		(setf (ct:cref (:unsigned-char *) cstr strlength) 0)
		cstr))

;; Copy a lisp array of bytes to a C byte array.
;; If the optional parameter cbuf is not supplied (or is nil) then a new C byte
;; array of the same length is allocated with ct:malloc. The resulting C array is returned.
;;
(defun lisp-bytes-to-c-bytes (buf &optional (cbuf nil))
	(let ((length (length buf)))
		(unless cbuf
			(setf cbuf (ct:malloc length)))
		(dotimes (i length)
			(setf (ct:cref (:unsigned-char *) cbuf i) (aref buf i)))
		cbuf))

;; Copy a C array of bytes to a Lisp array of bytes.
;; If the optional parameter lisp-buf is supplied, it should be a Lisp array of 'byte
;; of a length greater or equal to the passed length parameter.
;; The lisp array is returned.
(defun c-bytes-to-lisp-bytes (cbuf length &optional (lisp-buf nil))
	(unless lisp-buf
		(setf lisp-buf (make-array length :element-type 'byte)))
	(dotimes (i length)
		(setf (aref lisp-buf i) (ct:cref (:unsigned-char *) cbuf i)))
	lisp-buf)
    
(defmacro with-c-struct ((var expr structure-type) &body body)
	(declare (ignore var))
	(let ((struct-def (ct::ctypeexpand-all structure-type)))
		(unless (and (listp struct-def)
					(eq (car struct-def) ':struct))
			(error "Not a C structure definition: ~S" struct-def))
		(let ((fields
					(let ((j 0)
						  (ret nil))
						(dolist (i (rest struct-def)(nreverse ret))
							(if (evenp j) (push i ret))
							(incf j)))))
			(unless (every #'symbolp fields)
				(error "Invalid structure definition, names are not all symbols: ~A" fields))
			
			`(symbol-macrolet 
				,(mapcar #'(lambda (sym) `(,sym (ct:cref ,structure-type ,expr ,sym))) fields)
				,@body))))

(defmacro with-fresh-foreign-block ((sym type) &rest forms)
	`(let ((,sym (ct:malloc (ct:sizeof ,type))))
		(unwind-protect (progn ,@forms)(ct:free ,sym))))

;;;
;;; Output the contents of a C structure.
;;; The structure and the optional stream are evaluated, but the C structure name is not. 
;;; Example: (dump-c-struct s WNDCLASSEX)
;;;
(defmacro dump-c-struct (struct type &optional (stream t))
    (let ((exptype (ct::ctypeexpand-all type)))
        (unless (ct::valid-c-type-definition exptype)
            (error "Invalid C type specifier: ~S" type))
        (let ((forms '()))
            (do* ((x (cdr exptype) (cddr x))
                  (name (car x) (car x))
                  (field-type (cadr x) (cadr x)))
                ((null x))
                (push `(format ,stream "~4T~A[~A]:~40T~S~%" ',name ',field-type (ct:cref ,type ,struct ,name)) forms))
            `(progn
                (format ,stream "~A:~%" ',type)
                ,@forms
                (format ,stream "~%")
                (values)))))

(defconstant x86::foreign-stack-start-marker (+ (* cl::uvector-foreign-stack-tag 8) x86::uvector-header-tag))
(defconstant x86::foreign-stack-end-marker   (+ (* cl::uvector-foreign-stack-end-tag 8) x86::uvector-header-tag))

(defasm alloca (size)
	{
		begin-atomic
		pop		edx			; edx = return address
		pop		eax			; eax = size in bytes
		shr		eax, 3		; untag
		test	esp, 4		; are we at an 8-byte stack boundary
		jnz		:t1
		push	0			; if so, push a pad word
	:t1
		add		eax, 7
		and		eax, -8		; should be multiple of 8
		mov		ecx, eax
		shr		ecx, 3
		add		ecx, 2		; 4 word header
		shl		ecx, 8
		or		ecx, x86::foreign-stack-end-marker
		push	ecx			; push end marker
		sub		esp, eax	; allocate foreign block
		push	0			; pad
		shl		eax, 3		; tag byte count
		push	eax			; number of bytes
		lea		eax, [esp + 8] ; start of foreign block
		push	eax
		and		ecx, #xffffff7
		or		ecx, x86::foreign-stack-start-marker
		push	ecx
		mov		eax, esp
		or		eax, uvector-tag
		push	0			; dummy arg
		push	edx			; push return address
		mov		ecx, 1
		end-atomic
		ret
	})

;; redefine kernel function
(defasm cl::lisp-object-id (obj)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + ARGS_OFFSET]
        cmp     eax, most-positive-fixnum
        ja      :bignum
        shl     eax, 3
        jmp     short :next
    :bignum
        mov     ecx, eax
        shr     ecx, 16
        shl     ecx, 3
        shl     eax, 16
        shr     eax, 13     ;; eax = low 16 bits, ecx = high 16 bits
        push    eax
        push    ecx
        callp   %create-positive-bignum
        add     esp, 8
    :next
		mov		ecx, 1
		pop		ebp
        ret
    })

;;;
;;; Redefine kernel LOAD-DLL function
;;;
(ct:defun-kernel cl::loadlibrary ((filename (:unsigned-char *)))
   :return-type :handle
   :kernel-name cl::%LoadLibrary
   :linkage-type :pascal)

;(defun cl::load-dll (filename)
;    (ct:foreign-ptr-to-int (cl::loadlibrary (ct:lisp-string-to-c-string filename))))

(make-package :temp-package)
(in-package :temp-package)	;; avoid interning these exported symbols
(defpackage :win32
	(:export )
	(:nicknames :win))

(in-package :win)
(use-package :ct)
(in-package :cl)

(setq cl::*compiler-warn-on-undefined-function* t)
    