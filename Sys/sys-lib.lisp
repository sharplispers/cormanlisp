;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		sys-lib
;;;;	Contents:	Implementation functions.
;;;;	History:	11/11/2002  RGC  Created.
;;;;

(make-package "SYS")
(in-package :sys)

;;;
;;;	Define code generation for Common Lisp * operator
(x86::defcodegen char-aref (form dest)
	(let ((num-args (- (length form) 1)))
		(if (= num-args 2)
            (progn
			   (cl::compile-sub-form (second form) :dest-stack t)
			   (cl::compile-sub-form (third form) :dest-eax-operand t)
			   (x86::parse-assembler
					{
                        mov     ecx, eax                        ;; ecx = index
                        pop     edx                             ;; edx = vector
	                    begin-atomic
		                shr		ecx, 3							;; untagged integer
		                xor		eax, eax
		                mov		ax, [edx + ecx*2 + (uvector-offset 2)]
		                shl		eax, 8
		                inc		eax
                        end-atomic
					})
			   (x86::offset-stack 4))
			   (error "wrong number of args"))
		(if (eq dest :dest-stack)
			(progn
				(x86::parse-assembler
					{
						push	eax
					})
				(x86::offset-stack -4)))
		(if (eq dest :dest-eax)
			(progn
				(x86::parse-assembler
					{
						mov		ecx, 1
					}))))
	t)

(defun %string= (x y)
    (let ((xlen (length x)))
        (unless (= (length y) xlen)
            (return nil))
        (dotimes (i xlen t)
			(unless (eq (char-aref x i) (char-aref y i))
				(return nil)))))
