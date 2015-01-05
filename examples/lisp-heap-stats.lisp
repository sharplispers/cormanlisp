;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		lisp-heap-stats.lisp
;;;;	Contents:	Generates report of Lisp heap usage.
;;;;	Author:		Roger Corman
;;;;

(defun obj-stats (predicate)
	(let ((num 0)(space 0))
		(cl::process-each-heap-block 
			#'(lambda (obj) 
				(when (funcall predicate obj) 
					(incf num)
					(incf space 
						(if (consp obj) 
							8
							(* (ccl:uvector-length obj) 4))))))
		(values num space)))

(defun output-stat (stream obj-name num space)
	(format stream "~20A ~20A ~20A~%" obj-name num space))

(defun closurep (obj)(and (functionp obj)(not (kernel-function-p obj))))
(defun general-array-p (obj) 
	(and (arrayp obj) 
		(eq (array-element-type obj) 't)))
(defun bit-array-p (obj) 
	(and (arrayp obj) 
		(eq (array-element-type obj) 'bit)))
(defun byte-array-p (obj) 
	(and (arrayp obj) 
		(eq (array-element-type obj) 'byte)))
	
(defun report-lisp-heap-stats (&optional (stream *standard-output*))
	(format stream "Object type          Number               Total Bytes~%")
	(format stream "-----------          ------               -----------~%")
	(gc 2)
	(let ((total-num 0)
		 (total-space 0))
		(do* ((x '(cons             consp
				   symbol           symbolp
				   kernel-function  kernel-function-p
				   closure          closurep
				   structure        structurep
				   string           stringp 
				   general-array    general-array-p
				   bit-array        bit-array-p
				   byte-array       byte-array-p
				   stream           streamp
				   float            floatp
				   complex          complexp
				   ratio            cl::ratiop
				   bignum           bignump
				   package          packagep
				   hash-table       hash-table-p
				   foreign-pointer  foreignp
				   foreign-heap     cl::foreign-heap-p
				   compiled-code    cl::compiled-code-p
				   readtable        readtablep
				   weak-pointer     cl::weak-pointer-p
									
				  					) (cddr x)))
			((null x))
			(multiple-value-bind (num space)
				(obj-stats (second x))
				(incf total-num num)
				(incf total-space space)
				(output-stat stream (first x) num space)))
		(format stream "                     ------               -----------~%")
		(output-stat stream " " total-num total-space))
		(values))
	
	