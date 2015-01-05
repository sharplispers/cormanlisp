;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		assembler.lisp
;;;;	Contents:	Intel x86 code generation related functions.
;;;;	History:	11/20/96  RGC  Created.
;;;;

(make-package "ASSEMBLER-X86" :nicknames '("X86"))
(in-package :x86)
(export 'inline-assemble)

(defconstant code-buffer-info-offset 				1)
(defconstant code-buffer-cbuf-offset 				2)
(defconstant code-buffer-refs-offset 				3)
(defconstant code-buffer-code-index-offset 			4)
(defconstant code-buffer-ref-index-offset 			5)
(defconstant code-buffer-dynamic-env-size-offset 	6)
(defconstant code-buffer-heap-env-size-offset 		7)
(defconstant code-buffer-stack-index-offset 		8)
(defconstant code-buffer-max-code-bytes-offset 		9)
(defconstant code-buffer-max-refs-offset 			10)
(defconstant code-buffer-size			 			11)

(defconstant code-buffer-max-code-bytes				#x1000)
(defconstant code-buffer-max-refs					#x400)

(defconstant uvector-header-tag						6)
(defconstant uvector-tag							5)
(defconstant cons-tag								4)

(defconstant compiled-code-execution-offset         4)

(defconstant ARGS_OFFSET							8)		;; offset from EBP

(defvar *assembler-x86-readtable* (copy-readtable))
(defvar cormanlisp::*compiler-code-buffer* 
	(symbol-value 'cormanlisp::*compiler-code-buffer*))
(defvar cormanlisp::*compiler-collect-jump-table-refs*)
(defvar cormanlisp::*compiler-collect-var-table-refs*)
	
(defvar *compiler-code-labels*)

(defun is-byte (x) (and (integerp x) (>= x -128) (<= x 127)))
(defun needs-reference (obj) (or (uvectorp obj) (consp obj))) 

(defun grow-code-buffer () 
    (let* ((buf *compiler-code-buffer*)
           (cbuf (uref buf code-buffer-cbuf-offset))
           (size (uref buf code-buffer-max-code-bytes-offset))
           (new-buf (make-array (+ (* size 2) 16) :element-type 'byte :initial-element 0))
           (current (uref buf code-buffer-code-index-offset)))
        (dotimes (i current)
            (setf (elt new-buf i) (elt cbuf i)))
        (setf (uref buf code-buffer-cbuf-offset) new-buf)))
        
(defun assembler-macro-function (sym) (get sym :assembler-macro))
(defun expand-assembler-macro (statement)
	(let* ((func (assembler-macro-function (car statement)))
		   (args (cdr statement))
           (statements (apply func (remove :op-separator args)))
           (result '()))
        (dolist (x statements)
            (if (and (consp x) (assembler-macro-function (car x)))
				(dolist (g (expand-assembler-macro x))
					(push g result))
                (push x result)))
        (nreverse result)))
                
(defun uvector-offset (index) (- (* 4 index) uvector-tag))

(defvar *compiler-code-branch-targets* nil)
(defvar *current-statement* nil)

(defun add-reference (obj code-position)
	(let* ((buf *compiler-code-buffer*)
		   (rbuf (uref buf code-buffer-refs-offset))
		   (rindex (uref buf code-buffer-ref-index-offset)))
		(setf (elt rbuf	rindex) obj)
		(setf (elt rbuf (+ rindex 1)) code-position)
		(setf (uref buf code-buffer-ref-index-offset) (+ rindex 2))))

(defun add-branch-target (label code-position distance)
	(let ((size 4))
		(if (eq distance :short)
			(setq size 1)) 
		(push (list label code-position size) *compiler-code-branch-targets*)))

(defun add-label (label code-position) 
	(push (cons label code-position) *compiler-code-labels*))

(defun emit-code (bytes)
	(let* ((buf *compiler-code-buffer*)
		   (cbuf (uref buf code-buffer-cbuf-offset))
		   (code-index (uref buf code-buffer-code-index-offset)))
;;		(format t "Emitting code: ~{#x~X ~}~%" bytes)
		(dolist (b bytes)
			(if (consp b)
				(if (eq (car b) :reference)
					(add-reference (cadr b) code-index)
					(if (eq (car b) :target)
						(add-branch-target (cadr b) code-index (third b))
						(if (eq (car b) :label)
							(add-label (cadr b) code-index))))
				(progn
					(if (and (< b 0) (>= b -128))
						(setq b (+ b 256)))
					(setf (elt cbuf code-index) b)
					(incf code-index))))
		(setf (uref buf code-buffer-code-index-offset) code-index)
		(if (> code-index (uref buf code-buffer-max-code-bytes-offset))
			(grow-code-buffer))))

(defun make-code-buffer ()
	(let* ((cbuf (make-array code-buffer-max-code-bytes :element-type 'byte))
		   (rbuf (make-array code-buffer-max-refs))
		   (cb (alloc-uvector code-buffer-size uvector-structure-tag)))
		(setf (uref cb code-buffer-info-offset)	'*compiler-code-buffer*)
		(setf (uref cb code-buffer-cbuf-offset)	cbuf)
		(setf (uref cb code-buffer-refs-offset)	rbuf)
		(setf (uref cb code-buffer-code-index-offset) 0)
		(setf (uref cb code-buffer-ref-index-offset) 0)
		(setf (uref cb code-buffer-dynamic-env-size-offset) 0)
		(setf (uref cb code-buffer-heap-env-size-offset) 0)
		(setf (uref cb code-buffer-stack-index-offset) 0)
		(setf (uref cb code-buffer-max-code-bytes-offset) code-buffer-max-code-bytes)
		(setf (uref cb code-buffer-max-refs-offset) (* 2 code-buffer-max-refs))
		cb))

;;;;
;;;;	Compute the twos complement of the number and convert
;;;;	to bytes, low byte first.
;;;;						
(defun long-bytes (x)
	(unless (integerp x)
		(setq x (lisp-object-id x)))
	(let* ((negative (minusp x))
		   (carry 1)
		   (bytes nil))
		(if negative (setq x (- x)))
		(dotimes (i 4)
			(let ((byte (mod x #x100)))
				(if negative
					(progn 
						(setq byte (+ (- 255 byte) carry))
						(if (= byte 256)
							(progn
								(setq byte 0)
								(setq carry 1))
							(setq carry 0))))
				(push byte bytes)
				(setq x (truncate x #x100))))
		(nreverse bytes)))

(defun short-bytes (x)
	(let ((bytes (long-bytes x)))
		(list (first bytes)(second bytes))))

(set-macro-character #\{
	#'(lambda (stream ch)
		(declare (ignore ch))
		(let ((assembler-statements nil)
		      (*readtable* *assembler-x86-readtable*)
			  (*package* :x86)
			  (inline nil)
			  c)
			(setq c (read-char stream))
			(if (char= c #\{)
				(setq inline t)
				(unread-char c stream))
			(do ((token (read stream t nil t) (read stream t nil t))
				 (statement nil))
				((or (eq token '}) (eq token '}}))
				 (if inline
					`(inline-assemble 
						(encode-assembler-statements 
							,(nreverse assembler-statements)))
					`(encode-assembler-statements ,(nreverse assembler-statements))))
				(if (eq token #\Newline)
					(progn
						(unless (null statement)
							(push (nreverse statement) assembler-statements))
						(setq statement nil))
					(push token statement))))))

(set-macro-character #\Newline 
	#'(lambda (stream ch) (declare (ignore stream ch)) #\Newline)
	nil
	*assembler-x86-readtable*)

(set-macro-character #\, 
	#'(lambda (stream ch) (declare (ignore stream ch)) :op-separator)
	nil
	*assembler-x86-readtable*)

(set-macro-character #\+ 
	#'(lambda (stream ch) (declare (ignore stream ch)) '+)
	nil
	*assembler-x86-readtable*)

(set-macro-character #\* 
	#'(lambda (stream ch) (declare (ignore stream ch)) '*)
	nil
	*assembler-x86-readtable*)

(set-macro-character #\[ 
	#'(lambda (stream ch) (declare (ignore stream ch)) :tok-left-brace)
	nil
	*assembler-x86-readtable*)

(set-macro-character #\] 
	#'(lambda (stream ch) (declare (ignore stream ch)) :tok-right-brace)
	nil
	*assembler-x86-readtable*)

;; set up all the 32-bit registers with properties corresponding
;; to the machine encoding for that register
(do ((p '(eax ecx edx ebx esp ebp esi edi) (cdr p))
	 (i 0 (+ i 1)))
	((null p))
	(setf (get (car p) :register-32) i))

;; same for all the 8-bit registers
(do ((p '(al cl dl bl ah ch dh bh)(cdr p))
	 (i 0 (+ i 1)))
	((null p))
	(setf (get (car p) :register-8) i))

;; same for all the 16-bit registers
(do ((p '(ax cx dx bx sp bp si di)(cdr p))
	 (i 0 (+ i 1)))
	((null p))
	(setf (get (car p) :register-16) i))

;; same for all the fpu registers
(do ((p '(st0 st1 st2 st3 st4 st5 st6 st7)(cdr p))
	 (i 0 (+ i 1)))
	((null p))
	(setf (get (car p) :register-fpu) i))

;; mark all branch instructions
(do ((p '(jmp jz jnz je jne jl jg jnl jng jle jnle jge 
				jnge jo jno ja jb jae jbe jnb jna jp jnp jc jnc call)
			(cdr p))
	 (i 0 (+ i 1)))
	((null p))
	(setf (get (car p) :branch-operation) t))

(defun is-register-8 (x) (get x :register-8))
(defun is-register-16 (x) (get x :register-16))
(defun is-register-32 (x) (get x :register-32))
(defun is-register-fpu (x) (get x :register-fpu))
(defun is-branch-operation (x) (get x :branch-operation))

(defun parse-indirect-operand (p statement)
	(let* ((reg1 nil)
		   (reg2 nil)
		   (reg2-multiplier 1)
		   (disp nil)
		   (negative-disp nil))
		(do ((tok (car p) (car p)))
			((null p) (error "Invalid assembler statement: ~A" statement))
			(if (eq tok :tok-right-brace)
				(return `(:register-indirect ,reg1 ,reg2 ,reg2-multiplier 
						,(if negative-disp (- disp) disp))))
			(if (symbolp tok)
				(if (get tok :register-32)
					(if (eq (cadr p) '*)
						(progn
							(if (or reg2 (not (member (caddr p) '(1 2 4 8))))
								(error "Invalid multiplier in assembler statement: ~A" statement))
							(setq reg2 tok)
							(setq reg2-multiplier (caddr p))
							(setq p (cddr p)))
						(if reg1 
							(if reg2
								(error "Invalid assembler statement: ~A" statement)
								(setq reg2 tok)	)
							(setq reg1 tok)))
					(if (eq tok '-)
						(setq negative-disp t)
						(unless (eq tok '+)
							(error "Invalid assembler statement: ~A" statement))))
				(if (integerp tok)
					(if disp
						(error "Invalid assembler statement--too many displacements: ~A" statement)
						(setq disp tok))
					(error "Invalid assembler statement--token ~A in statement ~A is unknown" tok statement)))
			(setq p (cdr p)))))
				
(defun parse-assembly-statement (s)
	(if (eq s 'inline-assemble)
		(return-from parse-assembly-statement s))		;; special flag
	(if (keywordp (car s))
		(return-from parse-assembly-statement `(declare-label ,(car s))))				
	(let* ((operator (car s))
		   (op-1 nil)
		   (op-2 nil)
		   (p (cdr s))
		   (branch-distance nil))

		(if (null p)
			(return-from parse-assembly-statement `(,operator)))

		(if (is-branch-operation operator)
			(if (member (car p) '(short near far))
				(progn
					(if (eq (car p) 'short)
						(setq branch-distance '(:short))
						(setq branch-distance '(:far)))
					(setq p (cdr p))
					(if (null p)
						(error "Invalid ~A statement, no target" operator)))
				(setq branch-distance '(:far))))

		;; get first operand
		(cond
			((and (symbolp (car p)) (get (car p) :register-32))
			 (setq op-1 `(:register-32 ,(car p)))
			 (setq p (cdr p)))
			((and (symbolp (car p)) (get (car p) :register-16))
			 (setq op-1 `(:register-16 ,(car p)))
			 (setq p (cdr p)))
			((and (symbolp (car p)) (get (car p) :register-8))
			 (setq op-1 `(:register-8 ,(car p)))
			 (setq p (cdr p)))
			((and (symbolp (car p)) (get (car p) :register-fpu))
			 (setq op-1 `(:register-fpu ,(car p)))
			 (setq p (cdr p)))
   			((member (car p) '(:tok-left-brace dword word byte))
			 (setq op-1 (parse-indirect-operand (cdr p) s))
			 (do () ((null p)) 
				(if (eq (car p) :tok-right-brace)
					(progn (setq p (cdr p)) (return)))
				(setq p (cdr p))))
			(t (setq op-1 `(:immediate ,(car p))) (setq p (cdr p))))

		(if (null p)
			(return-from parse-assembly-statement `(,operator ,op-1 ,@branch-distance)))

		(unless (eq (car p) :op-separator)
			(error "Invalid assembler statement: ~A" s))
		(setq p (cdr p))

		;; get second operand
		(cond
			((and (symbolp (car p)) (get (car p) :register-32))
			 (setq op-2 `(:register-32 ,(car p)))
			 (setq p (cdr p)))
			((and (symbolp (car p)) (get (car p) :register-16))
			 (setq op-2 `(:register-16 ,(car p)))
			 (setq p (cdr p)))
			((and (symbolp (car p)) (get (car p) :register-8))
			 (setq op-2 `(:register-8 ,(car p)))
			 (setq p (cdr p)))
			((and (symbolp (car p)) (get (car p) :register-fpu))
			 (setq op-2 `(:register-fpu ,(car p)))
			 (setq p (cdr p)))
   			((eq (car p) :tok-left-brace)
			 (setq op-2 (parse-indirect-operand (cdr p) s))
			 (do () ((null p)) 
				(if (eq (car p) :tok-right-brace)
					(progn (setq p (cdr p)) (return)))
				(setq p (cdr p))))
			(t (setq op-2 `(:immediate ,(car p))) (setq p (cdr p))))

		(if p (error "Extraneous token ~A in assembler statement: ~A" p s))
		`(,operator ,op-1 ,op-2)))

(defun resolve-branch-addresses (labels code-offsets)
	(dolist (i code-offsets)
		(let* ((sym (first i))
			   (addr (second i))
			   (size (third i))
			   (cbuf (uref *compiler-code-buffer* code-buffer-cbuf-offset))
			   (label-addr (cdr (assoc sym labels)))
			   (offset 0))
			(if (null label-addr)
				(error "Branch label ~A not found in assembler block" sym))

			;; calculate offset
			(setq offset (- label-addr (+ addr size)))
			(if (and (eq size 1) (or (< offset -128) (>= offset 128)))
				(error "Offset out of range in assembler block: ~A requires a ~A byte branch"
					sym offset))
			(if (eq size 1)
				(progn
					(if (< offset 0)
						(incf offset 256))
					(setf (elt cbuf addr) offset))
				(let ((bytes (long-bytes offset)))
					(dotimes (i 4)
						(setf (elt cbuf (+ addr i)) (elt bytes i))))))))

;;
;;	If it is NIL, return NIL.
;;	Otherwise, reverse the list, and store it into a simple vector.
;;	Returns the vector.
;;  Symbols of the form (:immediate sym-name) are converted to just sym-name.
;;
(defun convert-refs-to-vector (list)
	(if list
		(do* ((x list (cddr x))
			  (ref (car x)(car x))
			  (sym (cadr x)(cadr x))
			  (ret '()))
			((null x) (apply 'vector ret))
			(push ref ret)
			(push sym ret))))
			
(defun assemble (forms name lambda-list)
	(let ((temp nil)
		  (*compiler-code-buffer* (make-code-buffer))
		  (*compiler-code-branch-targets* nil)
		  (*compiler-code-labels* nil)
		  (*current-statement* nil)
		  (cl::*code-jump-table-refs* nil)
		  (cl::*code-var-table-refs* nil)
		  (cl::*code-env-table-refs* nil)
		  (info `(cl::function-name ,name cl::lambda-list ,lambda-list)))
		(declare (special *compiler-code-buffer*))
		(declare (special *compiler-code-branch-targets*))
		(declare (special *compiler-code-labels*))
		(declare (special *current-statement*))
		(if pl::*source-file*
			(setq info (cons 'pl::*source-file* (cons *source-file* info))))
		(if pl::*source-line*
			(setq info (cons 'pl::*source-line* (cons *source-line* info))))

		;; expand any macros
		(dolist (f forms)
			(if (and (consp f) (assembler-macro-function (car f)))
				(dolist (g (expand-assembler-macro f))
					(push g temp))
				(push f temp)))
		(setq forms (nreverse temp))
		(dolist (f forms)
			(setq f (parse-assembly-statement f))
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
		(resolve-branch-addresses *compiler-code-labels* *compiler-code-branch-targets*)
		
		(if cl::*code-jump-table-refs*
			(setf info 
				(cons 'cl::*code-jump-table-refs* 
					(cons (convert-refs-to-vector cl::*code-jump-table-refs*) info))))

		(if cl::*code-env-table-refs*
			(setf info 
				(cons 'cl::*code-env-table-refs* 
					(cons (convert-refs-to-vector cl::*code-env-table-refs*) info))))

		(if cl::*code-var-table-refs*
			(setf info 
				(cons 'cl::*code-var-table-refs* 
					(cons (convert-refs-to-vector cl::*code-var-table-refs*) info))))
		
		(cl::create-compiled-function 
			(uref *compiler-code-buffer* code-buffer-cbuf-offset)
			(uref *compiler-code-buffer* code-buffer-code-index-offset)
			(uref *compiler-code-buffer* code-buffer-refs-offset)
			(truncate (uref *compiler-code-buffer* code-buffer-ref-index-offset) 2)
			nil
			info
			cl::*append-refs-to-code*)))

(defun encode-instruction-element (x)
	(if (symbolp x)
		(if (keywordp x)
			x
			(if (or (is-register-8 x)
					(is-register-32 x)
					(is-register-16 x)
                    (is-register-fpu x)
					(member x '(+ - * short near far))) 
				`',x
				x))
		x))

(defun encode-instruction (s)
	(if (assembler-macro-function (car s))
		`(list ',(car s) ,@(mapcar #'(lambda (x) `',x) (cdr s)))
		`(list ',(car s) ,@(mapcar #'encode-instruction-element (cdr s)))))

(defmacro encode-assembler-statements (statements)
	(let ((temp nil))
		(dolist (s statements)
			(push (encode-instruction s) temp))
		`(list 'inline-assemble ,@(nreverse temp))))

(export 'ccl::defasm (find-package :cormanlisp))

(defmacro ccl::defasm (name lambda-list asm-block)
	(let ((setf-form nil))
		(if (and (consp name) (eq (car name) 'setf))
			(progn
				(unless (symbolp (cadr name)) (error "Invalid function name: ~A" name))
				(setq setf-form (cadr name))
				(setq name (cl::setf-function-symbol name))))

		(if setf-form 		
			`(progn
				(setf (symbol-function ',name) (assemble ,asm-block ',name ',lambda-list))
				(cl::register-setf-function ',setf-form ',name)
				',name) 
			`(progn
				(setf (symbol-function ',name) (assemble ,asm-block ',name ',lambda-list))
				',name))))

(defmacro defop (op params &rest exprs)
	`(progn 
		(setf (get ',op 'x86::encoding-func) #'(lambda ,params (block ,op (let () ,@exprs))))
		',op))

(defmacro defasm-macro (op params &rest exprs)
	`(progn
		(setf (get ',op :assembler-macro) #'(lambda ,params ,@exprs))
		',op))

(defun mod-reg-rm (mod reg rm)
	(+ (* mod 64) (* reg 8) rm))

(defun calc-sib (op)
	(let ((scale (fourth op))
		  (index-reg (third op))
		  (base-reg (second op))
		  (ss nil))
		(if (null index-reg)
			(return-from calc-sib (+ #x20 (get base-reg :register-32))))
		(setq ss
			(case scale
				(1 0)
				(2 1)
				(4 2)
				(8 3)))
		(+ (* ss 64) (* (get index-reg :register-32) 8) (get base-reg :register-32))))
			
(defun mod-reg-rm-byte (op1 op2 &optional imm)
	(let ((type1 (car op1))
		  (type2 (car op2))
		  (width -1)
		  (direction -1)
		  b
		  reg1
		  reg2
		  disp
		  mode
		  (sib nil)
		  (xbytes nil)
		  (short-size nil)
		  (memory-direct nil))
		(cond 
			((eq type1 :register-32)
			 (setq width 1)
			 (setq reg1 (get (cadr op1) :register-32)))
			((eq type1 :register-8)
			 (setq width 0)
			 (setq reg1 (get (cadr op1) :register-8)))
			((eq type1 :register-16)
			 (setq width 1)
			 (setq short-size t)
			 (setq reg1 (get (cadr op1) :register-16))))

		(cond
			((eq type2 :register-32)
			 (if (= width 0)
				(error "Invalid assembler statement: ~A" *current-statement*)
				(progn
					(setq width 1)
					(setq reg2 (get (cadr op2) :register-32)))))
			((eq type2 :register-8)
			 (if (= width 1)
				(error "Invalid assembler statement: ~A" *current-statement*)
				(progn
					(setq width 0)
					(setq reg2 (get (cadr op2) :register-8)))))
			((eq type2 :register-16)
			 (if (= width 0)
				(error "Invalid assembler statement: ~A" *current-statement*)
				(progn
					(setq width 1)
			 		(setq short-size t)
					(setq reg2 (get (cadr op2) :register-16))))))


		(if (= width -1)
			(setq width 1))		;; default to 32-bit operations

		(cond
			;; handle reg-reg case
			((or
				(and (eq type1 :register-32) (eq type2 :register-32))
				(and (eq type1 :register-8) (eq type2 :register-8))
				(and (eq type1 :register-16) (eq type2 :register-16)))
			 (setq direction 1)
			 (setq b (+ #xc0 (* reg1 8) reg2)))

			;; handle mem-reg case
			((and (eq type1 :register-indirect) 
				(or (eq type2 :register-32) (eq type2 :register-8)(eq type2 :register-16)
						(and imm (eq type2 :immediate))))
			 (if (eq type2 :immediate)
			 	(let* ((immed (cadr op2))
				 	   (is-byte (is-byte immed)))
					(setf reg2 imm)
					(if is-byte
						(setq direction 1)
						(setq direction 0))
				 	(setq xbytes (if (or (and (= width 1) (= direction 0)) short-size)
									(if short-size (short-bytes immed)(long-bytes immed)) 
									(list immed)))
					(if (needs-reference immed)
						(push (list :reference immed) xbytes))))
			(setq direction 0)
			(setq disp (fifth op1))
			(if (null disp)
				(setq mode 0)
				(if (is-byte disp)
					(setq mode 1)
					(setq mode 2)))
			(if (or (third op1)#|(eq (second op2) 'esp)|#)
				;; if there is a second register or the base register is esp (4)
				(progn
					(setq sib (calc-sib op1))
					(setq b (+ (* mode 64) (* reg2 8) #x04)))
				(if (and (null (second op1))(null (third op1)))	;; no register specified
					(progn									;; memory direct case
						(setq b (+ 0 (* reg2 8) 5))
						(setq memory-direct t))
					(setq b (+ (* mode 64) (* reg2 8) (get (cadr op1) :register-32)))))
			(if disp
				(if (and (is-byte disp)(not memory-direct))
					(push disp xbytes)
					(setq xbytes 
					  (append
						(if nil ;; short-size -RGC 
							(short-bytes disp) 
							(long-bytes disp))
						xbytes)))))

			;; handle reg-mem case
			((and (eq type2 :register-indirect) 
				(or (eq type1 :register-32) (eq type1 :register-8)(eq type1 :register-16)))
			 (progn
				(setq direction 1)
				(setq disp (fifth op2))
				(if (null disp)
					(setq mode 0)
					(if (is-byte disp)
						(setq mode 1)
						(setq mode 2)))
				(if (or (third op2)	(eq (second op2) 'esp))	;; if there is a second register
					(progn
						(setq sib (calc-sib op2))
						(setq b (+ (* mode 64) (* reg1 8) #x04)))
					(if (and (null (second op2))(null (third op2)))	;; no register specified
						(progn									;; memory direct case
							(setq b (+ 0 (* reg1 8) 5))	;; memory direct case
							(setq memory-direct t))
						(setq b (+ (* mode 64) (* reg1 8) (get (cadr op2) :register-32)))))
				(if disp
					(if (and (is-byte disp)(not memory-direct))
						(setq xbytes (list disp))
						(setq xbytes 
							(if nil ;; short-size -RGC
								(short-bytes disp) 
								(long-bytes disp)))))))

			;; handle reg-immediate case
			((and (or (eq type1 :register-32)(eq type1 :register-8)(eq type1 :register-16))
				 (eq type2 :immediate))
			 (let* ((immed (cadr op2))
				    (is-byte (is-byte immed)))
			 	(setq b (+ #xc0 reg1))
				(if is-byte
					(setq direction 1)
					(setq direction 0))

			 	(setq xbytes (if (or (and (= width 1) (= direction 0)) short-size)
								(if short-size (short-bytes immed)(long-bytes immed)) 
								(list immed)))
				(if (needs-reference immed)
					(push (list :reference immed) xbytes))))

			;; handle mem-immediate case
			((and (eq type1 :register-indirect) (eq type2 :immediate))
			 (error "Addressing mode not supported yet: ~A" *current-statement*)))

		(values b sib direction width xbytes (if short-size '(#x66)))))

(defun mod-reg-rm-byte-single (op)
	(let ((type (first op))
		  (width -1)
		  b
		  reg
		  disp
		  mode
		  (sib nil)
		  (xbytes nil)
		  (memory-direct nil))
		(if (eq type :register-32)
			(progn
				(setq width 1)
				(setq reg (get (second op) :register-32)))
			(if (eq type :register-8)
				(progn
					(setq width 0)
					(setq reg (get (second op) :register-8)))))

		(if (= width -1)
			(setq width 1))		;; default to 32-bit operations

		(cond
			;; handle reg case
			((or (eq type :register-32) (eq type :register-8))
			 (setq b (+ #xc0 reg)))

			;; handle mem case
			((eq type :register-indirect)
			 (progn
				(setq disp (fifth op))
				(if (null disp)
					(setq mode 0)
					(if (is-byte disp)
						(setq mode 1)
						(setq mode 2)))	
				(if (or (third op) (eq (second op) 'esp))	;; if there is a second register
					(progn
						(setq sib (calc-sib op))
						(setq b (+ (* mode 64) #x04)))
					(if (and (null (second op))(null (third op)))	;; no register specified
						(progn									;; memory direct case
							(setq b 5)
							(setq memory-direct t))
						(setq b (+ (* mode 64) (get (cadr op) :register-32)))))
				(if disp
					(if (and (is-byte disp)(not memory-direct))
						(setq xbytes (list disp))
						(setq xbytes (long-bytes disp)))))))

		(values b sib 0 width xbytes)))

(defun standard-2-op (op1 op2 b1 b2 oc1 oc2 inc-mrm)
	;; check for special case first of immed to accumulator
	(if (and (eq (car op1) :register-32) 
			 (eq (cadr op1) 'eax)
			 (eq (car op2) :immediate))
		(return-from standard-2-op `(,b1 ,@(long-bytes (cadr op2))))
		(if (and (eq (car op1) :register-8) 
				 (eq (cadr op1) 'al)
				 (eq (car op2) :immediate))
			(return-from standard-2-op `(,b2 ,(cadr op2)))))
			  
	(let ((opcode))
		(multiple-value-bind (mrm-byte sib direction width extra-bytes)
			(mod-reg-rm-byte op1 op2)
			;; check for immediate to reg/memory
			(if (eq (car op2) :immediate)
				(progn (setq opcode oc1) (incf mrm-byte inc-mrm))
				(setq opcode oc2))
			(setq opcode (+ opcode (* direction 2) width))

			`(,opcode ,mrm-byte ,@(if sib (list sib)) ,@extra-bytes))))

(defun standard-shift-op (op1 op2 b)
	(if (eq (cadr op2) 'cl)
		(multiple-value-bind (mrm-byte sib direction width extra-bytes)
			(mod-reg-rm-byte-single op1)
			(declare (ignore direction width))
			`(,(+ #xd2 width) ,(+ mrm-byte b) ,@(if sib (list sib)) ,@extra-bytes))
		(multiple-value-bind (mrm-byte sib direction width extra-bytes)
			(mod-reg-rm-byte op1 op2)
			(declare (ignore direction))
			;; check for shift by one
			(if (eq (car op2) :immediate)
				(if (eq (cadr op2) 1)
					`(,(+ #xd0 width) ,(+ mrm-byte b) ,@(if sib (list sib)) 
							,@(nreverse (cdr (nreverse extra-bytes))))	;; drop last byte (1)
					`(,(+ #xc0 width) ,(+ mrm-byte b) ,@(if sib (list sib)) ,@extra-bytes))
				(error "Invalid statement: ~A" *current-statement*)))))

(defun bt-op (op1 op2 c2 im)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes) (mod-reg-rm-byte op1 op2)
		(declare (ignore direction width))
		(if (eq (car op2) :immediate) (setq mrm-byte (logior mrm-byte (* im 8))))
		`(#x0f ,(if (eq (car op2) :immediate) #xba c2) ,mrm-byte ,@(if sib (list sib)) ,@extra-bytes)))

(defun standard-1-op (op b)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(,(+ #xf6 width) ,(+ mrm-byte b) ,@(if sib (list sib)) ,@extra-bytes)))

(defun standard-jmp-op (op distance short-code long-code-high long-code-low)
	(let ((target (cadr op)))
		(if (eq distance :short)
			`(,short-code ,(list :target target distance) #x00)
			`(,long-code-high ,long-code-low ,(list :target target distance) 0 0 0 0))))

(defop declare-label (label)
	(list (list :label label)))

;; prefix bytes
(defop cs  () '(#x2e))
(defop ds  () '(#x3e))
(defop es  () '(#x26))
(defop fs  () '(#x64))
(defop gs  () '(#x65))
(defop ss  () '(#x36))
(defop operand-size-prefix  () '(#x66))
(defop address-size-prefix  () '(#x67))
(defop lock  () '(#xf0))
(defop repne  () '(#xf2))
(defop rep () '(#xf3))

(defop db (x) `(,(cadr x)))		;; define byte

(defop aaa () '(#x37))
(defop aad () '(#xd5 #x0a))
(defop aam () '(#xd4 #x0a))
(defop aas () '(#x3f))

(defop adc (op1 op2) (standard-2-op op1 op2 #x15 #x14 #x80 #x10 #x10))
(defop add (op1 op2) (standard-2-op op1 op2 #x05 #x04 #x80 #x00 #x00))
(defop and (op1 op2) (standard-2-op op1 op2 #x25 #x24 #x80 #x20 #x20))

(defop bsf (op1 op2)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte op1 op2)
		(declare (ignore direction width))
		`(#x0f #xbc ,(+ mrm-byte #x00) ,@(if sib (list sib)) ,@extra-bytes)))

(defop bsr (op1 op2)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte op1 op2)
		(declare (ignore direction width))
		`(#x0f #xbd ,(+ mrm-byte #x00) ,@(if sib (list sib)) ,@extra-bytes)))

(defop bt (op1 op2)  (bt-op op1 op2 #xa3 3))
(defop btc (op1 op2) (bt-op op1 op2 #xbb 7))
(defop btr (op1 op2) (bt-op op1 op2 #xb3 6))
(defop bts (op1 op2) (bt-op op1 op2 #xab 5))

(defop call (op distance)
	(declare (ignore distance))
	(let ((target (cadr op)))
		(if (keywordp target)
			`(#xe8 ,(list :target target :far) 0 0 0 0)
			(multiple-value-bind (mrm-byte sib direction width extra-bytes)
				(mod-reg-rm-byte-single op)
				(declare (ignore direction width))
				`(#xff ,(+ mrm-byte #x10) ,@(if sib (list sib)) ,@extra-bytes)))))

(defop cbw () '(#x98))
(defop cdq () '(#x99))
(defop clc () '(#xf8))
(defop cld  () '(#xfc))	;; clears direction flag (bit 10)
(defop cli () '(#xfa))
(defop clts () '(#x0f #x06))
(defop cmc () '(#xf5))

(defop cmp (op1 op2) 
	(let ((result (standard-2-op op1 op2 #x3d #x3c #x80 #x38 #x38)))
		(if (= (car result) #x82)
			(setf (car result) #x80))	;; turn off the s bit--unnecessary
		result))

(defop cpuid () '(#x0f #xa2))
(defop cwd () '(#x99))
(defop cwde () '(#x98))
(defop daa () '(#x27))
(defop das () '(#x2f))

(defop dec (op)
	(if (eq (car op) :register-32)
		(return-from dec (list (+ #x48 (is-register-32 (cadr op))))))
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(,(+ #xfe width) ,(+ mrm-byte 8) ,@(if sib (list sib)) ,@extra-bytes)))

(defop div (op)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(,(+ #xf6 width) ,(+ mrm-byte #x30) ,@(if sib (list sib)) ,@extra-bytes)))

;; load 32-bit integer
(defop fild (op)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#xdb ,(+ mrm-byte #x00) ,@(if sib (list sib)) ,@extra-bytes)))

(defop fild.32 (op)         ;; synonym for fild
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#xdb ,(+ mrm-byte #x00) ,@(if sib (list sib)) ,@extra-bytes)))

;; load 64-bit integer
(defop fild.64 (op)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#xdf ,(+ mrm-byte #x28) ,@(if sib (list sib)) ,@extra-bytes)))

;; load 16-bit integer
(defop fild.16 (op)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#xdf ,(+ mrm-byte #x00) ,@(if sib (list sib)) ,@extra-bytes)))

(defop fld (op)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#xdd ,(+ mrm-byte #x00) ,@(if sib (list sib)) ,@extra-bytes)))

(defop fld.single (op)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#xd9 ,(+ mrm-byte #x00) ,@(if sib (list sib)) ,@extra-bytes)))

(defop fst (op)
    (if (is-register-fpu (cadr op))
        `(#xdd ,(+ #xd0 (get (cadr op) ':register-fpu)))
    	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
    		(mod-reg-rm-byte-single op)
    		(declare (ignore direction width))
    		`(#xdd ,(+ mrm-byte #x10) ,@(if sib (list sib)) ,@extra-bytes))))

(defop fst.single (op)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#xd9 ,(+ mrm-byte #x10) ,@(if sib (list sib)) ,@extra-bytes)))
	
(defop fstp (op)
    (if (is-register-fpu (cadr op))
        `(#xdd ,(+ #xd8 (get (cadr op) ':register-fpu)))
       	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
    		(mod-reg-rm-byte-single op)
    		(declare (ignore direction width))
    		`(#xdd ,(+ mrm-byte #x18) ,@(if sib (list sib)) ,@extra-bytes))))

(defop fstp.single (op)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#xd9 ,(+ mrm-byte #x18) ,@(if sib (list sib)) ,@extra-bytes)))

;; store 32-bit integer
(defop fistp (op)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#xdb ,(+ mrm-byte #x18) ,@(if sib (list sib)) ,@extra-bytes)))

(defop fistp.32 (op)         ;; synonym for fistp
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#xdb ,(+ mrm-byte #x18) ,@(if sib (list sib)) ,@extra-bytes)))

;; store 64-bit integer
(defop fistp.64 (op)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#xdf ,(+ mrm-byte #x38) ,@(if sib (list sib)) ,@extra-bytes)))

;; store 16-bit integer
(defop fistp.16 (op)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#xdf ,(+ mrm-byte #x18) ,@(if sib (list sib)) ,@extra-bytes)))

(defop fsub (op1 &optional op2)
    (if op2
        (cond ((and (eq (cadr op1) 'st0) (is-register-fpu (cadr op2)))
                `(#xd8 ,(+ #xe0 (get (cadr op2) ':register-fpu))))
              ((and (is-register-fpu (cadr op1)) (eq (cadr op2) 'st0))
                `(#xdc ,(+ #xe8 (get (cadr op1) ':register-fpu))))
              (t (error "Invalid parameters to FADD: ~A and ~A" op1 op2)))
      	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
    		(mod-reg-rm-byte-single op1)
    		(declare (ignore direction width))
    		`(#xdc ,(+ mrm-byte #x20) ,@(if sib (list sib)) ,@extra-bytes))))

(defop fsub.single (op)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#xd8 ,(+ mrm-byte #x20) ,@(if sib (list sib)) ,@extra-bytes)))

;; subtract st(0) from st(i) and pop st(0)
(defop fsubp (&optional op1 op2)
    (if (and (not op1) (not op2))
        `(#xde #xe9)
        (cond ((and (is-register-fpu (cadr op1)) (eq (cadr op2) 'st0))
                `(#xde ,(+ #xe8 (get (cadr op1) ':register-fpu))))
              (t (error "Invalid parameters to FSUBP: ~A and ~A" op1 op2)))))

(defop fadd (op1 &optional op2)
    (if op2
        (cond ((and (eq (cadr op1) 'st0) (is-register-fpu (cadr op2)))
                `(#xd8 ,(+ #xc0 (get (cadr op2) ':register-fpu))))
              ((and (is-register-fpu (cadr op1)) (eq (cadr op2) 'st0))
                `(#xdc ,(+ #xc0 (get (cadr op1) ':register-fpu))))
              (t (error "Invalid parameters to FADD: ~A and ~A" op1 op2)))
    	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
    		(mod-reg-rm-byte-single op1)
    		(declare (ignore direction width))
    		`(#xdc ,(+ mrm-byte #x00) ,@(if sib (list sib)) ,@extra-bytes))))

(defop fadd.single (op)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#xd8 ,(+ mrm-byte #x00) ,@(if sib (list sib)) ,@extra-bytes)))

;; add st(0) to st(i) and pop st(0)
(defop faddp (&optional op1 op2)
    (if (and (not op1) (not op2))
        `(#xde #xc1)
        (cond ((and (is-register-fpu (cadr op1)) (eq (cadr op2) 'st0))
                `(#xde ,(+ #xc0 (get (cadr op1) ':register-fpu))))
              (t (error "Invalid parameters to FADDP: ~A and ~A" op1 op2)))))

(defop fmul (op1 &optional op2)
    (if op2
        (cond ((and (eq (cadr op1) 'st0) (is-register-fpu (cadr op2)))
                `(#xd8 ,(+ #xc8 (get (cadr op2) ':register-fpu))))
              ((and (is-register-fpu (cadr op1)) (eq (cadr op2) 'st0))
                `(#xdc ,(+ #xc8 (get (cadr op1) ':register-fpu))))
              (t (error "Invalid parameters to FADD: ~A and ~A" op1 op2)))
      	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
    		(mod-reg-rm-byte-single op1)
    		(declare (ignore direction width))
    		`(#xdc ,(+ mrm-byte #x08) ,@(if sib (list sib)) ,@extra-bytes))))

(defop fmul.single (op)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#xd8 ,(+ mrm-byte #x08) ,@(if sib (list sib)) ,@extra-bytes)))

(defop fmulp () '(#xde #xc9))     ;; multiply st(0) by st(1) and pop st(0)

(defop fdiv (op1 &optional op2)
    (if op2
        (cond ((and (eq (cadr op1) 'st0) (is-register-fpu (cadr op2)))
                `(#xd8 ,(+ #xf0 (get (cadr op2) ':register-fpu))))
              ((and (is-register-fpu (cadr op1)) (eq (cadr op2) 'st0))
                `(#xdc ,(+ #xf8 (get (cadr op1) ':register-fpu))))
              (t (error "Invalid parameters to FDIV: ~A and ~A" op1 op2)))
       	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
            (mod-reg-rm-byte-single op1)
            (declare (ignore direction width))
            `(#xdc ,(+ mrm-byte #x30) ,@(if sib (list sib)) ,@extra-bytes))))

(defop fdiv.single (op)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#xd8 ,(+ mrm-byte #x30) ,@(if sib (list sib)) ,@extra-bytes)))

;; divide st(1) by st(0) and pop st(0)
(defop fdivp (&optional op1 op2)
    (if (and (not op1) (not op2))
        `(#xde #xf9)
        (cond ((and (is-register-fpu (cadr op1)) (eq (cadr op2) 'st0))
                `(#xde ,(+ #xf8 (get (cadr op1) ':register-fpu))))
              (t (error "Invalid parameters to FDIVP: ~A and ~A" op1 op2)))))


(defop fsin ()   '(#xd9 #xfe))
(defop fcos ()   '(#xd9 #xff))
(defop fsincos () '(#xd9 #xfb))
(defop fsqrt ()  '(#xd9 #xfa))
(defop fabs ()   '(#xd9 #xe1))
(defop fchs ()   '(#xd9 #xe0))        ;; change sign
(defop fscale () '(#xd9 #xfd))        ;; scale
(defop ftst ()   '(#xd9 #xe4))        ;; test against 0
(defop fpatan () '(#xd9 #xf3))        ;; Partial Arctangent
(defop fwait () '(#x9b))              ;; FPU wait (CPU waits for FPU)

(defop fcomi  () '(#xdb #xf1))        ;; compare st(0), st(1)
(defop fcomip () '(#xdf #xf1))        ;; compare st(0), st(1)
(defop fucomi () '(#xdb #xe9))        ;; compare st(0), st(1)
(defop fucomip () '(#xdf #xe9))       ;; compare st(0), st(1)
(defop fstsw_AX () '(#xdf #xe0)) ;; mov status flags to ax
(defop fxch ()  '(#xd9 #xc9))        ;; Exchange st(0), st(1)
(defop frndint ()  '(#xd9 #xfc))      ;; Round integer according to control word bits 10-11

;;; constants
(defop fld1 ()   '(#xd9 #xe8))
(defop fldl2t ()   '(#xd9 #xe9))
(defop fldl2e ()   '(#xd9 #xea))
(defop fldlpi ()   '(#xd9 #xeb))
(defop fldlg2 ()   '(#xd9 #xec))
(defop fldln2 ()   '(#xd9 #xed))
(defop fldz ()   '(#xd9 #xee))

(defop fyl2x ()   '(#xd9 #xf1))
(defop fyl2xp1 () '(#xd9 #xf9))

(defop f2xm1() '(#xd9 #xf0))

(defop fcom (op)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#xdc ,(+ mrm-byte #x10) ,@(if sib (list sib)) ,@extra-bytes)))

(defop fcom.single (op)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#xd8 ,(+ mrm-byte #x10) ,@(if sib (list sib)) ,@extra-bytes)))

(defop fcomp (op)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#xdc ,(+ mrm-byte #x18) ,@(if sib (list sib)) ,@extra-bytes)))

(defop fcomp.single (op)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#xd8 ,(+ mrm-byte #x18) ,@(if sib (list sib)) ,@extra-bytes)))

(defop fldcw (op)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#xd9 ,(+ mrm-byte #x28) ,@(if sib (list sib)) ,@extra-bytes)))

(defop fstcw (op)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#xd9 ,(+ mrm-byte #x38) ,@(if sib (list sib)) ,@extra-bytes)))

(defop hlt () '(#xf4))

(defop idiv (op)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(,(+ #xf6 width) ,(+ mrm-byte #x38) ,@(if sib (list sib)) ,@extra-bytes)))

(defop imul (op)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(,(+ #xf6 width) ,(+ mrm-byte #x28) ,@(if sib (list sib)) ,@extra-bytes)))

(defop inc (op)
	(if (eq (car op) :register-32)
		(return-from inc (list (+ #x40 (is-register-32 (cadr op))))))
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(,(+ #xfe width) ,mrm-byte ,@(if sib (list sib)) ,@extra-bytes)))

(defop invd () '(#x0f #x08))
(defop iret () '(#xcf))

(defop jo   (op distance) (standard-jmp-op op distance #x70 #x0f #x80))
(defop jno  (op distance) (standard-jmp-op op distance #x71 #x0f #x81))
(defop jb   (op distance) (standard-jmp-op op distance #x72 #x0f #x82))
(defop jae  (op distance) (standard-jmp-op op distance #x73 #x0f #x83))
(defop jnb  (op distance) (standard-jmp-op op distance #x73 #x0f #x83)) ; synonym for jae
(defop je   (op distance) (standard-jmp-op op distance #x74 #x0f #x84))
(defop jz   (op distance) (standard-jmp-op op distance #x74 #x0f #x84))	; synonym for je
(defop jne  (op distance) (standard-jmp-op op distance #x75 #x0f #x85))
(defop jnz  (op distance) (standard-jmp-op op distance #x75 #x0f #x85)) ; synonym for jne
(defop jbe  (op distance) (standard-jmp-op op distance #x76 #x0f #x86))
(defop jna  (op distance) (standard-jmp-op op distance #x76 #x0f #x86)) ; synonym for jbe
(defop ja   (op distance) (standard-jmp-op op distance #x77 #x0f #x87))
(defop jp   (op distance) (standard-jmp-op op distance #x7A #x0f #x87))
(defop jnp  (op distance) (standard-jmp-op op distance #x7B #x0f #x87))
(defop jl   (op distance) (standard-jmp-op op distance #x7c #x0f #x8c))
(defop jnge (op distance) (standard-jmp-op op distance #x7c #x0f #x8c)) ; synonym for jl
(defop jge  (op distance) (standard-jmp-op op distance #x7d #x0f #x8d))
(defop jnl  (op distance) (standard-jmp-op op distance #x7d #x0f #x8d)) ; synonym for jge
(defop jle  (op distance) (standard-jmp-op op distance #x7e #x0f #x8e))
(defop jng  (op distance) (standard-jmp-op op distance #x7e #x0f #x8e)) ; synonym for jle
(defop jg   (op distance) (standard-jmp-op op distance #x7f #x0f #x8f))
(defop jnle (op distance) (standard-jmp-op op distance #x7f #x0f #x8f)) ; synonym for jg
(defop jc   (op distance) (standard-jmp-op op distance #x72 #x0f #x82))
(defop jnc  (op distance) (standard-jmp-op op distance #x73 #x0f #x83))

(defop jmp (op distance)
	(let ((target (cadr op)))
		(if (keywordp target)
			(if (eq distance :short)
				`(#xeb ,(list :target target distance) #x00)
				`(#xe9 ,(list :target target distance) 0 0 0 0))
			(multiple-value-bind (mrm-byte sib direction width extra-bytes)
				(mod-reg-rm-byte-single op)
				(declare (ignore direction width))
				`(#xff ,(+ mrm-byte #x20) ,@(if sib (list sib)) ,@extra-bytes)))))			

(defop lahf () '(#x9f))

(defop lea (op1 op2)
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte op1 op2)
		(declare (ignore direction width))
		`(#x8d ,mrm-byte ,@(if sib (list sib)) ,@extra-bytes)))

(defop leave () '(#xc9))

(defop mov (op1 op2)
	;; check for special case first of immed to register
	;; if immed is larger than a byte then this will result in a smaller instruction
	(if (and (eq (car op1) :register-32) 
			 (eq (car op2) :immediate))
		(return-from mov `(,(+ #xb8 (get (cadr op1) :register-32))
					,@(if (needs-reference (cadr op2)) (list (list :reference (cadr op2))))
					,@(long-bytes (cadr op2))))
		(if (and (eq (car op1) :register-8) 
			 	 (eq (car op2) :immediate))
			(return-from mov `(,(+ #xb0 (get (cadr op1) :register-8)) ,(cadr op2)))
            (if (and (eq (car op1) :register-16) 
			 	 (eq (car op2) :immediate))
                (return-from mov `(#x66 ,(+ #xb8 (get (cadr op1) :register-16)) ,@(short-bytes (cadr op2)))))))
			  
	(let ((opcode))
		(multiple-value-bind (mrm-byte sib direction width extra-bytes prefix-bytes)
			(mod-reg-rm-byte op1 op2)
			;; check for immediate to reg/memory
			(if (eq (car op2) :immediate)
				(setq opcode #xc4)
				(setq opcode #x88))
			(setq opcode (+ opcode (* direction 2) width))

			`(,@prefix-bytes
			    ,opcode ,mrm-byte 
				,@(if sib (list sib))
				,@(if (and 
						(eq (car op2) :immediate)
						(needs-reference (cadr op2)))
					 (list (list :reference (cadr op2)))) 
				,@extra-bytes))))

(defop mul (op) (standard-1-op op #x20))
(defop neg (op) (standard-1-op op #x18))

(defop nop () '(#x90))

(defop not (op) (standard-1-op op #x10))

(defop oio () '(#x0f 0xff))		; official invalid opcode

(defop or (op1 op2) (standard-2-op op1 op2 #x0d #x0c #x80 #x08 #x08))

(defop pop (op)
	(if (eq (first op) :register-32)
		(return-from pop (list (+ #x58 (get (second op) :register-32)))))
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#x8f ,mrm-byte ,@(if sib (list sib)) ,@extra-bytes)))

(defop popa () '(#x61))
(defop popf  () '(#x9d))
(defop popfd () '(#x9d))

(defop push (op)
	(if (eq (first op) :register-32)
		(return-from push (list (+ #x50 (get (second op) :register-32)))))
	(if (eq (first op) :register-16)
		(return-from push (list #x66 (+ #x50 (get (second op) :register-16)))))
	(if (eq (first op) :immediate)
		(if (is-byte (second op))
			(return-from push `(#x6a ,(second op)))
			(return-from push 
				`(#x68
							,@(if (needs-reference (second op)) (list (list :reference (cadr op))))
							,@(long-bytes (second op))))))	
	(multiple-value-bind (mrm-byte sib direction width extra-bytes)
		(mod-reg-rm-byte-single op)
		(declare (ignore direction width))
		`(#xff ,(+ mrm-byte #x30) ,@(if sib (list sib)) ,@extra-bytes)))

(defop pusha  () '(#x60))
(defop pushf  () '(#x9c))
(defop pushfd () '(#x9c))

(defop rcl (op1 op2) (standard-shift-op op1 op2 #x10))
(defop rcr (op1 op2) (standard-shift-op op1 op2 #x18))

(defop ret (&optional imm16) 
	(if imm16
		(progn
			(unless (consp imm16)
					(error "Invalid immediate in RET instruction: ~A" imm16))
			(let ((type (first imm16))
				  (val (second imm16)))
				(unless (and (eq type :immediate) (fixnump val) (not (minusp val)))
					(error "Invalid immediate in RET instruction: ~A" imm16))
				`(#xc2 ,(mod val #x100) ,(mod (truncate val #x100) #x100))))
		'(#xc3)))

(defop rol (op1 op2) (standard-shift-op op1 op2 #x00))
(defop ror (op1 op2) (standard-shift-op op1 op2 #x08))

(defop sahf  () '(#x9e))

(defop sal (op1 op2) (standard-shift-op op1 op2 #x20))
(defop sar (op1 op2) (standard-shift-op op1 op2 #x38))

(defmacro setcc-op (byte2 name1 &rest synonyms)
	`(progn
		(defop ,name1 (op)
			(multiple-value-bind (mrm-byte sib direction width extra-bytes)
				(mod-reg-rm-byte-single op)
				(declare (ignore direction width))
				`(#x0f ,,byte2 ,mrm-byte ,@(if sib (list sib)) ,@extra-bytes)))
		,@(let ((synonym-forms nil))
			(dolist (name2 synonyms synonym-forms)
				(push 
					`(setf (get ',name2 'x86::encoding-func) (get ',name1 'x86::encoding-func)) 
					synonym-forms)))))

;; define all the Setcc operators
(setcc-op #x90 seto)
(setcc-op #x91 setno)
(setcc-op #x92 setb setc setnae)
(setcc-op #x93 setae setnb setnc)
(setcc-op #x94 sete setz)
(setcc-op #x95 setne setnz)
(setcc-op #x96 setbe setna)
(setcc-op #x97 seta setnbe)
(setcc-op #x98 sets)
(setcc-op #x99 setns)
(setcc-op #x9a setp setpe)
(setcc-op #x9b setnp setpo)
(setcc-op #x9c setl setnge)
(setcc-op #x9d setge setnl)
(setcc-op #x9e setle setng)
(setcc-op #x9f setg setnle)

(defop shl (op1 op2) (standard-shift-op op1 op2 #x20))
(defop shr (op1 op2) (standard-shift-op op1 op2 #x28))

(defop stc  () '(#xf9))
(defop std  () '(#xfd))		; sets direction flag (bit 10)
(defop sti  () '(#xfb))

(defop sub (op1 op2) (standard-2-op op1 op2 #x2d #x2c #x80 #x28 #x28))

(defop test (op1 op2)
	;; check for special case first of immed to accumulator
	(if (and (eq (car op1) :register-32) 
			 (eq (cadr op1) 'eax)
			 (eq (car op2) :immediate))
		(return-from test `(#xa9 ,@(long-bytes (cadr op2))))
		(if (and (eq (car op1) :register-8) 
				 (eq (cadr op1) 'al)
				 (eq (car op2) :immediate))
			(return-from test `(#xa8 ,(cadr op2)))))
	(let ((opcode))
		(multiple-value-bind (mrm-byte sib direction width extra-bytes)
			(mod-reg-rm-byte op1 op2)
			(declare (ignore direction))
			(if (eq (car op2) :immediate)
				(if (= width 1)
					(return-from test `(,(+ #xf6 width) ,mrm-byte ,@(long-bytes (cadr op2))))
					(return-from test `(,(+ #xf6 width) ,mrm-byte ,(cadr op2))))
				(setq opcode (+ #x84 width)))
			`(,opcode ,mrm-byte ,@(if sib (list sib)) ,@extra-bytes))))

(defop wait  () '(#x9b))
(defop wbinvd  () '(#x0f 0x09))

(defop xchg (op1 op2)
	;; check for special case first of reg to accumulator
	(if (and (eq (car op1) :register-32) 
			 (eq (cadr op1) 'eax)
			 (eq (car op2) :register-32))
		(return-from xchg `(,(+ #x90 (get (cadr op2) :register-32))))
		(if (and (eq (car op1) :register-32) 
				 (eq (cadr op2) 'eax)
				 (eq (car op2) :register-32))
			(return-from xchg `(,(+ #x90 (get (cadr op1) :register-32))))))	  
	(let ((opcode #x86))
		(multiple-value-bind (mrm-byte sib direction width extra-bytes)
			(mod-reg-rm-byte op1 op2)
			(declare (ignore direction))
			(setq opcode (+ opcode width))
			`(,opcode ,mrm-byte ,@(if sib (list sib)) ,@extra-bytes))))

(defop xlat  () '(#xd7))

(defop xor (op1 op2) (standard-2-op op1 op2 #x35 #x34 #x80 #x30 #x30))

;; called for side-effect only, emits no code bytes
(defop add-env-table-ref (sym)
	(if pl::*compiler-collect-jump-table-refs*
		(let ((current-ip (uref *compiler-code-buffer* code-buffer-code-index-offset)))
			(setf cl::*code-env-table-refs* 
				(cons (- current-ip 4) 
					(cons (second sym) cl::*code-env-table-refs*)))))
	'())
			
;; called for side-effect only, emits no code bytes
(defop add-jump-table-ref (sym)
	(if pl::*compiler-collect-jump-table-refs*
		(let ((current-ip (uref *compiler-code-buffer* code-buffer-code-index-offset)))
			(setf cl::*code-jump-table-refs* 
				(cons (- current-ip 4) 
					(cons (second sym) cl::*code-jump-table-refs*)))))
	'())

;; called for side-effect only, emits no code bytes
(defop add-var-table-ref (sym)
	(if pl::*compiler-collect-var-table-refs*
		(let ((current-ip (uref *compiler-code-buffer* code-buffer-code-index-offset)))
			(setf cl::*code-var-table-refs* 
				(cons (- current-ip 4) 
					(cons (second sym) cl::*code-var-table-refs*)))))
	'())

;;; Move function environment into EDI
(defasm-macro load-environment (sym)
	(let* ((env-offset (* (uref sym symbol-jump-table-offset) 4)))
		(if (zerop env-offset)
			(error "The function ~A does not have a jump table offset and cannot be called" sym))
		{
			mov  edi, [esi + env-offset]
			add-env-table-ref sym
		}))
			
(defasm-macro callf (sym)
	(let* ((env-offset (* (uref sym symbol-jump-table-offset) 4))
		   (jump-offset	(+ env-offset 4)))
		(if (zerop env-offset)
			(error "The function ~A does not have a jump table offset and cannot be called" sym))
		{
			mov  edi, [esi + env-offset]
			add-env-table-ref sym
		  	call [esi + jump-offset]
			add-jump-table-ref sym
		}))

(defasm-macro callp (sym)
	(let* ((env-offset (* (uref sym symbol-jump-table-offset) 4))
		   (jump-offset	(+ env-offset 4)))
		(if (zerop env-offset)
			(error "The function ~A does not have a jump table offset and cannot be called" sym))
		{
		  	call [esi + jump-offset]
			add-jump-table-ref sym
		}))

;;;
;;; Call into code buffer, assuming the code buffer is in passed register
;;;
(defasm-macro callcode (reg)
	{
    	;begin-atomic      ;; not necessary to actually call this
        	lea		reg, [reg + (- (* compiled-code-execution-offset 4) uvector-tag)]    ;; reg = target address
        end-atomic          ;; this protects the following call statement
       		call	reg
   	})

;;;
;;; Call into code buffer, assuming a lisp function is in passed register
;;;
(defasm-macro callfunc (reg)
	{
		mov		    reg, [reg + (- (* 4 function-code-buffer-offset) uvector-tag)]       ;; reg = code buffer
    	;begin-atomic      ;; not necessary to actually call this
        	lea		reg, [reg + (- (* compiled-code-execution-offset 4) uvector-tag)]    ;; reg = target address
        end-atomic          ;; this protects the following call statement
       		call	reg
   	})

;; loads the symbol value in a register
(defasm-macro symval (reg sym)
	(let* ((var-offset (* (uref sym symbol-var-table-offset) 4)))
		(if (zerop var-offset)
			(error "The variable ~A does not have a variable table offset and cannot be accessd in compiled code" sym))
		{
		  	mov reg, [esi + var-offset]
			add-var-table-ref sym
            mov reg, [reg - cons-tag]
		}))

;; stores a register in the symbol value slot
(defasm-macro symset (reg sym)
	(let* ((var-offset (* (uref sym symbol-var-table-offset) 4)))
		(if (zerop var-offset)
			(error "The variable ~A does not have a variable table offset and cannot be accessd in compiled code" sym))
		{
            push reg
		  	mov reg, [esi + var-offset]
			add-var-table-ref sym
   		  	pop [reg - cons-tag]
		}))

(defasm-macro begin-atomic ()
	{
		std
	})

(defasm-macro end-atomic ()
	{
		cld
	})

(defasm-macro push-eip ()
	{
		call :t_push_eip
	:t_push_eip
	})

;;;
;;; FPU macros
;;;
(defasm-macro fpu-load-double (arg-index)
	{
        mov     edx, [ebp + (+ (* arg-index 4) ARGS_OFFSET)] ;; edx = boxed double
        fld     [edx + (uvector-offset 2)]
   	})

(defasm-macro fpu-retrieve-double ()
	{
		mov		ecx, 0
		callf	cl::alloc-double-float          ;; eax = result cell
        fstp    [eax + (uvector-offset 2)]
   	})

(defasm-macro fpu-store-double (arg-index)
	{
        mov     edx, [ebp + (+ (* arg-index 4) ARGS_OFFSET)] ;; edx = boxed double
        fstp    [edx + (uvector-offset 2)]
   	})

(defasm-macro fpu-add-double (arg-index)
	{
        mov     edx, [ebp + (+ (* arg-index 4) ARGS_OFFSET)] ;; edx = boxed double
        fadd    [edx + (uvector-offset 2)]
   	})

(defasm-macro fpu-subtract-double (arg-index)
	{
        mov     edx, [ebp + (+ (* arg-index 4) ARGS_OFFSET)] ;; edx = boxed double
        fsub    [edx + (uvector-offset 2)]
   	})

(defasm-macro fpu-multiply-double (arg-index)
	{
        mov     edx, [ebp + (+ (* arg-index 4) ARGS_OFFSET)] ;; edx = boxed double
        fmul    [edx + (uvector-offset 2)]
   	})

(defasm-macro fpu-divide-double (arg-index)
	{
        mov     edx, [ebp + (+ (* arg-index 4) ARGS_OFFSET)] ;; edx = boxed double
        fdiv    [edx + (uvector-offset 2)]
   	})

;;;;
;;;; utility functions
;;;;
(defun xaddress (func) 
	(if (symbolp func)
		(setq func (symbol-function func)))
	(let ((*print-base* 16))
		(print (execution-address func))
		(terpri)))

(setq *encoding-assembler-macro* nil)

(in-package :common-lisp)


