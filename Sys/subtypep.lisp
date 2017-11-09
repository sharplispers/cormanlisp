;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		subtypep.lisp
;;;;	Contents:	Corman Lisp subtypep function.
;;;;	History:	4/23/97  RGC  Created.
;;;;				9/13/99  RGC  Incorporated Vassili's TYPE-OF
;;;;							  fix to return CLOS instance name and
;;;;							  structure names.
;;;;				2/15/01  RGC  Minor cleanup to TYPE-OF function.
;;;;                09/07/07 RGC  Integrated Matthias Hölzl's fixes for SUBTYPEP, OPEN, 
;;;;                              and BOA constructors. 
;;;;
(in-package :common-lisp)

(defconstant subtype-escape-syms '(satisfies or not and member eql))

(defparameter *subtype-table* (make-hash-table))

(defmacro defsubtype (t1 (var) &body body)
    (let ((name (intern (concatenate 'string "SUBTYPEP." (symbol-name t1)))))
        `(progn 
            (setf (gethash ',t1 *subtype-table*) ',name) 
            (defun ,name (,var) ,@body))))

(defsubtype array (type) (member type '(atom)))
(defsubtype atom (type) (declare (ignore type)) nil)
(defsubtype base-char (type) (member type '(character atom)))
(defsubtype base-string (type) (member type '(string vector array sequence atom)))
(defsubtype bignum (type) (member type '(integer rational number real atom)))
(defsubtype bit (type) (member type '(unsigned-byte signed-byte integer rational number real atom)))
(defsubtype bit-vector (type) (member type '(vector array sequence atom)))
(defsubtype broadcast-stream (type) (member type '(stream atom)))
(defsubtype character (type) (eq type 'atom))
(defsubtype compiled-function (type) (member type '(function atom)))
(defsubtype complex (type) (member type '(number atom)))
(defsubtype concatenated-stream (type) (member type '(stream atom)))
(defsubtype cons (type) (member type '(list sequence)))
(defsubtype double-float (type) (member type '(number float real atom)))
(defsubtype echo-stream (type) (member type '(stream atom)))
(defsubtype extended-char (type) (member type '(character atom)))
(defsubtype file-stream (type) (member type '(stream atom)))
(defsubtype fixnum (type) (member type '(integer rational number real atom)))
(defsubtype float (type) (member type '(number real atom)))
(defsubtype function (type) (eq type 'atom))
(defsubtype hash-table (type) (eq type 'atom))
(defsubtype integer (type) (member type '(rational number real atom)))
(defsubtype keyword (type) (member type '(symbol atom)))
(defsubtype list (type) (eq type 'sequence))
(defsubtype logical-pathname (type) (member type '(pathname atom)))
(defsubtype long-float (type) (member type '(number float real atom)))
(defsubtype nil (type) (declare (ignore type)) t)
(defsubtype null (type) (member type '(symbol list sequence)))
(defsubtype number (type) (member type '(atom)))
(defsubtype package (type) (member type '(atom)))
(defsubtype pathname (type) (member type '(atom)))
(defsubtype random-state (type) (member type '(atom)))
(defsubtype ratio (type) (member type '(rational number real atom)))
(defsubtype rational (type) (member type '(number real atom)))
(defsubtype readtable (type) (member type '(atom)))
(defsubtype real (type) (member type '(number atom)))
(defsubtype sequence (type) (declare (ignore type)) nil)
(defsubtype short-float (type) (member type '(number float real atom)))
(defsubtype signed-byte (type) (member type '(integer rational number real atom)))
(defsubtype simple-array (type) (member type '(array atom)))
(defsubtype simple-base-string (type) (member type '(base-string simple-string string vector simple-array array sequence atom)))
(defsubtype simple-bit-vector (type) (member type '(bit-vector vector simple-array array sequence atom)))
(defsubtype simple-string (type) (member type '(string vector simple-array array sequence atom)))
(defsubtype simple-vector (type) (member type '(vector simple-array array sequence atom)))
(defsubtype single-float (type) (member type '(number float real atom)))
(defsubtype standard-char (type) (member type '(base-char character atom)))
(defsubtype stream (type) (member type '(atom)))
(defsubtype string (type) (member type '(vector array sequence atom)))
(defsubtype string-stream (type) (member type '(stream atom)))
(defsubtype symbol (type) (member type '(atom)))
(defsubtype synonym-stream (type) (member type '(stream atom)))
(defsubtype t (type) (declare (ignore type)) nil)
(defsubtype two-way-stream (type) (member type '(stream atom)))
(defsubtype unsigned-byte (type) (member type '(signed-byte integer rational number real atom)))
(defsubtype vector (type) (member type '(array sequence atom)))

(defun find-class (name) (declare (ignore name)) #| implemented later |#)
(defun subclassp (c1 c2) (declare (ignore c1 c2)) #| implemented later |#)

(defun subtypep-symbol-types (type-1 type-2)
	(cond 
		((eq type-1 type-2) (values t t))
		((eq type-2 'atom) (values (not (member type-1 '(cons list))) t)) 
		(t (let ((func (gethash type-1 *subtype-table*)))
                (if func 
                    (values (if (funcall func type-2) t) t)
                    (let ((c1 (find-class type-1 nil))
                          (c2 (find-class type-2 nil)))
                        (if (and c1 c2)
                            (values (subclassp c1 c2) t)
                            (values nil nil))))))))

;; not fully implemented
(defun subtypep-list-types (type-1 type-2)
    (cond ((and (consp type-1) 
                        (or (eq (car type-1) 'integer)
                                (eq (car type-1) 'unsigned-byte)
                                (eq (car type-1) 'signed-byte))
                        (member type-2 '(integer rational number real)))
            (values t t))
        ((eq type-1 nil)(values t t))
        (t (values nil nil))))

;;;
;;;	Common Lisp SUBTYPEP function.
;;;
(defun subtypep (type-1 type-2 &optional environment)
	(declare (ignore environment))	;; not supported
	(setq type-1 (typeexpand-all type-1))
	(setq type-2 (typeexpand-all type-2))
	(cond ((equal type-1 type-2)(values t t))
		  ((eq type-2 't) (values t t))
		  ((eq type-2 '*) (values t t))
		  ((eq type-2 'nil) (values nil t))
		  ((or (consp type-1)(consp type-2))
		   (subtypep-list-types type-1 type-2))
		  (t (subtypep-symbol-types type-1 type-2))))

(defconstant uvector-type-table 
#(
	function			; 0
	function			; 1
	structure			; 2
	array				; 3
	symbol				; 4
	stream				; 5
	double-float		; 6
	package				; 7
	hash-table			; 8
	foreign				; 9
	compiled-code		; 10
	readtable			; 11
	complex				; 12
	ratio				; 13
	bignum				; 14
	foreign-heap		; 15
	weak-ptr			; 16
	simple-vector		; 17
	(vector character) 	; 18
	(vector byte)		; 19
	(vector short)		; 20
	(vector double-float) ; 21
	(vector bit)		; 22
	(vector single-float); 23
	single-float		; 24
	clos-instance		; 25
	0
	0
	0
	0
	0
	0
))

(defconstant type-table 
#(
	fixnum 		
	character
	forward-pointer 
	short-float 
	cons 	
	uvector
	uvector-header 
	short-float
))

(defun class-name (class) (declare (ignore class)) nil)	;; dummy version, real one gets defined in CLOS
;;;;
;;;; Common Lisp TYPE-OF function.
;;;;
(ccl:defasm type-of (obj)
	{
		push	ebp
		mov		ebp, esp
		cmp		ecx, 1
		je		short :arg-ok
		callp 	_wrong-number-of-args-error
	:arg-ok
		mov		edx, [ebp + ARGS_OFFSET]
		;; From now on, OBJ is in EDX
		mov		eax, edx
		and		eax, 7
		cmp		al, 5					; uvector-tag
		jne		:not-uvector
		;; Uvector types
		mov		al, [edx - uvector-tag]
		shr		al, 3
		cmp     al, uvector-clos-instance-tag
		jne     :not-clos-inst
		;; A CLOS instance -- return the class name
		mov   eax, [edx + (uvector-offset cl::clos-instance-class-offset)]
        push  eax
		mov   ecx, 1
		callf class-name
		add   esp, 4
		jmp   short :done
	:not-clos-inst
	    cmp     al, cl::uvector-structure-tag
		jne     :not-structure
		;; A structure -- return the structure name
		mov     eax, [edx + (uvector-offset 1)] ; structure def. vector
		mov     edx, eax
		mov     edx, [edx - uvector-tag]
		shr     edx, 3
                and     edx, #b11111
                cmp     edx, cl::uvector-symbol-tag ; structure def. vector is a symbol
                je      short :ss
		mov     eax, [eax + (uvector-offset 2)] ; structure name (ELT DEF 0)
		jmp     short :done
        :ss
                mov     eax, 'cl::structure-object
                jmp     short :done
	:not-structure
	    ;; One of uvector-types -- lookup in the table
		mov		edx, cl::uvector-type-table
		mov		eax, [edx + eax*4 + (uvector-offset 2)]
		jmp		short :done
	:not-uvector
	    ;; Non-uvector type -- lookup in the table
		mov		edx, cl::type-table
		mov		eax, [edx + eax*4 + (uvector-offset 2)]
	:done
		pop		ebp
		ret
	})
