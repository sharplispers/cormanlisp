;;;;
;;;;	File:		clos.lisp
;;;;	Contents:	Corman Lisp CLOS implementation based on Closette
;;;;	History:	RGC 12/16/98  Added Vassili Bykov's WITH-SLOTS implementation.
;;;;				RGC 9/8/99    Added STANDARD-CLASS-P.
;;;;							  Decreased generic function call overhead by 40%
;;;;							  by building, compiling and caching the generic function
;;;;							  on the fly.
;;;;				RGC 10/14/99  DEFCLASS adds a type descriminator function to
;;;;							  support TYPEP.	
;;;;				RGC 12/06/99  Added RATIO builtin class.
;;;;				RGC 2/15/01	  Modified structures to be integrated better with CLOS.
;;;;							  i.e. CLASS-OF returns a class unique to that structure type.
;;;;				RGC 2/24/01   Added code to patch common lisp structures which are created
;;;;							  prior to CLOS loading to support method dispatching.
;;;;				RGC 7/29/01   Integrated EQL specializer code, optimized per generic function.
;;;;				RGC 10/21/01  Integrated generic functions so they are now first-class
;;;;							  functions, satisfying FUNCTIONP, and callable by FUNCALL and APPLY.
;;;;
;;;;                RGC  9/19/03  Incorporated Frank Adrian's modification to DEFGENERIC to support :documentation option.
;;;;                RGC  8/17/06  Modified method generation to handle &AUX in argument lists
;;;;
;;;; Note from Roger Corman, 7/29/2001:
;;;; This file has been hacked and modified for over 5 years by
;;;; me and others. It no longer bears much resemblance to the original,
;;;; but I will leave the following messages from Xerox in anyway.
;;;;
;;;; Optimizations for use with Corman Lisp 1.0  (May 15, 1998)
;;;; Minor modifications for use with PowerLisp 2.0  (May 15, 1996)
;;;;
;;;; Closette Version 1.0 (February 10, 1991)
;;;; Copyright (c) 1990, 1991 Xerox Corporation. All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works
;;;; based upon this software are permitted.  Any distribution of this
;;;; software or derivative works must comply with all applicable United
;;;; States export control laws.
;;;; 
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.
;;;; 
;;;; Closette is an implementation of a subset of CLOS with a metaobject
;;;; protocol as described in "The Art of The Metaobject Protocol",
;;;; MIT Press, 1991.
;;;;

(provide :clos)
(in-package :common-lisp)

;; need to override warning here  -RGC
(setq *COMPILER-WARN-ON-UNDEFINED-FUNCTION* nil)

(defvar exports
        '(defclass defgeneric defmethod
          find-class class-of
          call-next-method next-method-p
          slot-value slot-boundp slot-exists-p slot-makunbound
          make-instance change-class
          initialize-instance reinitialize-instance shared-initialize
          update-instance-for-different-class
          print-object

          standard-object
          standard-class standard-generic-function standard-method eql-specializer
          class-name

          class-direct-superclasses class-direct-slots
          class-precedence-list class-slots class-direct-subclasses
          class-direct-methods
          generic-function-name generic-function-lambda-list 
          generic-function-methods generic-function-discriminating-function
          generic-function-method-class
          method-lambda-list method-qualifiers method-specializers method-body
          method-environment method-generic-function method-function
          slot-definition-name slot-definition-initfunction 
          slot-definition-initform slot-definition-initargs
          slot-definition-readers slot-definition-writers
          slot-definition-allocation
          ;;
          ;; Class-related metaobject protocol
          ;; 
          compute-class-precedence-list compute-slots
          compute-effective-slot-definition
          finalize-inheritance allocate-instance
          slot-value-using-class slot-boundp-using-class 
          slot-exists-p-using-class slot-makunbound-using-class
          ;;
          ;; Generic function related metaobject protocol
          ;;
          compute-discriminating-function
          compute-applicable-methods-using-classes method-more-specific-p
          compute-effective-method-function compute-method-function
          apply-methods apply-method
		  describe-object
          find-generic-function  ; Necessary artifact of this implementation
          ))


(export exports)

;;; This hash-table supports CLOS EQL specializers.
(defconstant *clos-singleton-specializers* (make-hash-table :synchronized t))

;; fixed position of required-args in generic-function
(defconstant slot-location-generic-function-name 					0)
(defconstant slot-location-generic-function-lambda-list 			1)
(defconstant slot-location-generic-function-required-args 			2)
(defconstant slot-location-generic-function-methods 				3)
(defconstant slot-location-generic-function-method-class 			4)
(defconstant slot-location-generic-function-discriminating-function 5)
(defconstant slot-location-generic-function-classes-to-emf-table 	6)
(defconstant slot-location-generic-function-method-combination  	7)
(defconstant slot-location-generic-function-method-combination-order 8)

;;;
;;; Utilities 
;;;

;;; push-on-end is like push except it uses the other end:

(defmacro push-on-end (value location)
	`(setf ,location (nconc ,location (list ,value))))

;;; (setf getf*) is like (setf getf) except that it always changes the list,
;;;              which must be non-nil.

(defun (setf getf*) (new-value plist key)
	(block body
		(do ((x plist (cddr x)))
			((null x))
			(when (eq (car x) key)
				(setf (car (cdr x)) new-value)
				(return-from body new-value)))
		(push-on-end key plist)
		(push-on-end new-value plist)
		new-value))

;;; mapappend is like mapcar except that the results are appended together:
 
(defun mapappend (fun &rest args)
	(if (some #'null args)
		()
		(append (apply fun (mapcar #'car args))
			(apply #'mapappend fun (mapcar #'cdr args)))))

;;; mapplist is mapcar for property lists:

(defun mapplist (fun x)
	(if (null x)
		()
		(cons (funcall fun (car x) (cadr x))
			(mapplist fun (cddr x)))))

;;; the method table is only used internally--optimize to the max
(proclaim '(optimize (speed 3)(safety 0)))
(defstruct method-table 
	(method-list nil) 
	(cached-method nil) 
	(cached-method-types nil)
	(sync (cl::allocate-critical-section))
	(eql-specializers nil))
(proclaim '(optimize (speed 0)(safety 3)))

(defun clear-method-table (table)
	(with-synchronization (method-table-sync table)
		(setf (method-table-method-list table) nil)
		(setf (method-table-cached-method table) nil)
		(setf (method-table-cached-method-types table) nil))
	table)

(defun add-method-table-method (table types method)
	(with-synchronization (method-table-sync table)
		(setf (method-table-method-list table) 
			(cons types (cons method (method-table-method-list table))))
		(setf (method-table-cached-method table) method)
		(setf (method-table-cached-method-types table) types))
	table)

(defun class-list-matches (class-list eqls-classes)
	(do ((x class-list (cdr x))
		  (y eqls-classes (cdr y)))
		((null x) t)
		(unless (if (consp (car x)) (and (consp (car y)) (eql (caar x) (caar y))) (eq (car x) (car y)))
			(return nil))))

(defun find-method-table-method (table eqls-classes)
	(with-synchronization (method-table-sync table)
		(do ((p (method-table-method-list table) (cddr p)))
				((null p)(return nil))
				(when (class-list-matches (car p) eqls-classes)
					(return (cadr p))))))
					 
;;;
;;; Standard instances
;;;

(defun std-instance-class (x) 
	(if (clos-instance-p x) 
		(clos-instance-class x)
		(error "Not a CLOS instance: ~S" x)))

(defun (setf std-instance-class) (val x) 
	(if (clos-instance-p x) 
		(setf (uref x clos-instance-class-offset) val)
		(error "Not a CLOS instance: ~S" x)))

(defun std-instance-slots (x) 
	(if (clos-instance-p x) 
		(clos-instance-slots x)
		(error "Not a CLOS instance: ~S" x)))

(defun (setf std-instance-slots) (val x) 
	(if (clos-instance-p x) 
		(setf (uref x clos-instance-slots-offset) val)
		(error "Not a CLOS instance: ~S" x)))

(defun allocate-std-instance (class slots)
	(let ((x (alloc-clos-instance)))
		(setf (uref x clos-instance-class-offset) class)
		(setf (uref x clos-instance-slots-offset) slots)
		x))

;;; Standard instance allocation

(defparameter secret-unbound-value (list "slot unbound"))

(defun instance-slot-p (slot)
	(eq (slot-definition-allocation slot) ':instance))

(defun std-allocate-instance (class)
	(allocate-std-instance
		class
		(allocate-slot-storage 
			(count-if #'instance-slot-p (class-slots class))
			secret-unbound-value)))

;;; Simple vectors are used for slot storage.

(defun allocate-slot-storage (size initial-value)
	(make-array size :initial-element initial-value))

(defun allocate-shared-slot-storage (size initial-value)
	(let ((a (make-array size)))
		(dotimes (i size)
			(setf (aref a i)(list initial-value)))
		a))

;;; Standard instance slot access

;;; N.B. The location of the effective-slots slots in the class metaobject for
;;; standard-class must be determined without making any further slot
;;; references.

(defvar the-slots-of-standard-class) ;standard-class's class-slots
(defvar the-class-standard-class)    ;standard-class's class metaobject

(defun find-slot-position (item list)
	(let ((index 0))
		(dolist (x list)
			(if (eq x item)
				(return-from find-slot-position index)
				(if (eq (slot-definition-allocation x) ':instance)
					(incf index))))
		nil))

(defun find-slot-name (slots name)
	(dolist (x slots)
		(if (eq (slot-definition-name x) name)
			(return-from find-slot-name x)))
	nil)

;;;
;;; This now returns null, rather than signal an error, if the slot is not found.
;;; This is to enable further searches for class slots by the functions which
;;; use this.  -RGC  8/28/01
;;;
(defun slot-location (class slot-name)
	(if (and (eq slot-name 'effective-slots)
           (eq class the-class-standard-class))
  ;;    (position 'effective-slots the-slots-of-standard-class :key #'slot-definition-name)
		4	;; for optimization, hard-code this
		(let* ((slots (class-slots class))
			   (position nil))
			(do* ((s slots (cdr s))
				  (x (car s)(car s))
				  (pos 0))
				((null s) position)
				(when (eq (slot-definition-name x) slot-name)
					(return pos))
				(if (eq (slot-definition-allocation x) ':instance)
					(incf pos))))))

(defun shared-slot-location (class slot-name)
	(let* ((slots (class-shared-slot-definitions class))
		   (position nil))
		(do* ((s slots (cdr s))
			  (x (car s)(car s))
			  (pos 0 (+ pos 1)))
			((null s) position)
			(when (eq (slot-definition-name x) slot-name)
				(return pos)))))

;; optimize these by direct calls to inlined uref
(declaim (inline slot-contents (setf slot-contents)))
(defun slot-contents (slots location) (uref slots (+ location 2)) #|(svref slots location)|#)
(defun (setf slot-contents) (new-value slots location)
	(setf (uref slots (+ location 2)) new-value)#|(setf (svref slots location) new-value)|#)

(defun std-slot-value (instance slot-name)
	(let* ((class (class-of instance))
		   (location (slot-location class slot-name))
		   val)
		(if location
			(setf val (slot-contents (std-instance-slots instance) location))
			(progn
				(setf location (shared-slot-location class slot-name))
				(if location
					(setf val (car (slot-contents (class-shared-slots class) location)))
					(error "The slot ~S is missing from the class ~S." slot-name class))))
		(if (eq secret-unbound-value val)
			(error "The slot ~S is unbound in the object ~S." slot-name instance))
		val))

;;; Fast method to determine if a lisp object is a standard object.
;;; By our definition, any object which is not a CLOS instance (lisp
;;; primitive types, for example) are of type standard-class.
;;; CLOS instances are of type standard-class if the classes of their
;;; classes are standard-class.
;;; The number of arguments is not checked.
;;; We assume that the class of any object is a clos instance
;;; (for optimization purposes).
;;;
(ccl:defasm standard-class-p (object)
	{
	    push	ebp
		mov		ebp, esp
		mov		eax, [ebp + ARGS_OFFSET]
		mov		edx, eax
		and   	edx, 7
		cmp   	edx, uvector-tag
		jne		short :t-exit
		mov		edx, [eax - uvector-tag]
		shr		edx, 3
		and		edx, #x1f
		cmp		edx, cl::uvector-clos-instance-tag
		jne		short :t-exit
		mov		eax, [eax + (uvector-offset cl::clos-instance-class-offset)]
		mov		eax, [eax + (uvector-offset cl::clos-instance-class-offset)]
		mov		edx, 'cl::the-class-standard-class
		mov		edx, [edx + (uvector-offset cl::symbol-value-offset)]
		mov		edx, [edx - cons-tag]
		cmp		edx, eax
		jne		short :nil-exit
	:t-exit
		mov		eax, [esi + t-offset]
		jmp		short :exit
	:nil-exit
		mov		eax, [esi]
	:exit
		pop		ebp
		ret
	})

;;; Fast method to determine if a lisp object is a standard generic function.
(ccl:defasm standard-generic-function-p (object)
	{
	    push	ebp
		mov		ebp, esp
		mov		eax, [ebp + ARGS_OFFSET]
		mov		edx, eax
		and   	edx, 7
		cmp   	edx, uvector-tag
		jne		short :nil-exit
		mov		edx, [eax - uvector-tag]
		shr		edx, 3
		and		edx, #x1f
		cmp		edx, cl::uvector-clos-instance-tag
		jne		short :nil-exit
		mov		eax, [eax + (uvector-offset cl::clos-instance-class-offset)]
		mov		edx, 'cl::the-class-standard-gf
		mov		edx, [edx + (uvector-offset cl::symbol-value-offset)]
		mov		edx, [edx - cons-tag]
		cmp		edx, eax
		jne		short :nil-exit
	:t-exit
		mov		eax, [esi + t-offset]
		jmp		short :exit
	:nil-exit
		mov		eax, [esi]
	:exit
		pop		ebp
		ret
	})

;;; now patch (SETF SYMBOL-FUNCTION) to store the generic function
;;; in the symbol's function slot. We call the kernel function 
;;; first to ensure that the jump table gets setup with the
;;; address of the discrimination function closure.
;;;
(defparameter +save-set-symbol-function+ (fdefinition '(setf symbol-function)))
(defun (setf symbol-function) (value symbol)
	(if (standard-generic-function-p value)
		(let ((discrimination-function 
					(uref 
						(uref value cl::clos-instance-slots-offset)
						(+ 2 cl::slot-location-generic-function-discriminating-function))))
			(funcall +save-set-symbol-function+ discrimination-function symbol)
			;; now replace the function slot of the symbol
			(setf (car (uref symbol cl::symbol-function-offset)) value))
		(funcall +save-set-symbol-function+ value symbol)))
			
(defun slot-value (object slot-name)
	(if (standard-class-p object)
		(std-slot-value object slot-name)
		(slot-value-using-class (class-of object) object slot-name)))

;; For fixed known slot positions (optimization) for std-slots
;;
(defun slot-value-with-index (object slot-name index)
	(if (standard-class-p object)
		(let* ((slots (std-instance-slots object))
			   (val (svref slots index)))
			(if (eq secret-unbound-value val)
				(error "The slot ~S is unbound in the object ~S." slot-name object)
				val))
		(slot-value-using-class (class-of object) object slot-name)))

(defun (setf slot-value-with-index) (new-value object slot-name index)
	(if (standard-class-p object)
		(let* ((slots (std-instance-slots object)))
			(setf (svref slots index) new-value))
		(setf-slot-value-using-class 
			new-value (class-of object) object slot-name)))

(defun (setf std-slot-value) (new-value instance slot-name)
	(let* ((class (class-of instance))
		   (location (slot-location class slot-name)))
		(if location
			(setf (slot-contents (std-instance-slots instance) location) new-value)
			(progn
				(setf location (shared-slot-location class slot-name))
				(if location
					(setf (car (slot-contents (class-shared-slots class) location)) new-value)
					(error "The slot ~S is missing from the class ~S." slot-name class))))))

(defun (setf slot-value) (new-value object slot-name)
	(if (standard-class-p object)
		(setf (std-slot-value object slot-name) new-value)
		(setf-slot-value-using-class 
			new-value (class-of object) object slot-name)))

(defun std-slot-boundp (instance slot-name)
	(let* ((class (class-of instance))
		   (location (slot-location class slot-name)))
		(if location
			(not (eq secret-unbound-value (slot-contents (std-instance-slots instance) location)))
			(progn
				(setf location (shared-slot-location class slot-name))
				(if location
					(not (eq secret-unbound-value (car (slot-contents (class-shared-slots class) location))))
					(error "The slot ~S is missing from the class ~S." slot-name class))))))

(defun slot-boundp (object slot-name)
	(if (standard-class-p object)
		(std-slot-boundp object slot-name)
		(slot-boundp-using-class (class-of object) object slot-name)))

(defun std-slot-makunbound (instance slot-name)
	(let* ((class (class-of instance))
		   (location (slot-location class slot-name)))
		(if location
			(setf (slot-contents (std-instance-slots instance) location) secret-unbound-value)
			(progn
				(setf location (shared-slot-location class slot-name))
				(if location
					(setf (car (slot-contents (class-shared-slots class) location)) secret-unbound-value)
					(error "The slot ~S is missing from the class ~S." slot-name class)))))
	instance)

(defun slot-makunbound (object slot-name)
	(if (standard-class-p object)
		(std-slot-makunbound object slot-name)
		(slot-makunbound-using-class (class-of object) object slot-name)))

(defun std-slot-exists-p (instance slot-name)
	(not (null (find slot-name (class-slots (class-of instance))
				:key #'slot-definition-name))))

(defun slot-exists-p (object slot-name)
	(if (standard-class-p object)
		(std-slot-exists-p object slot-name)
		(slot-exists-p-using-class (class-of object) object slot-name)))

;;; class-of

(defun class-of (x)
  (if (clos-instance-p x)
      (std-instance-class x)
      (built-in-class-of x)))

;;; N.B. This version of built-in-class-of is straightforward but very slow.
;;; This is only for booting, a faster method is used later -RGC
;;;
(defun built-in-class-of (x)
  (typecase x
    (null                                          (find-class 'null))
    ((and symbol (not null))                       (find-class 'symbol))
    ((complex *)                                   (find-class 'complex))
    (integer                                 	   (find-class 'integer))
    ((float * *)                                   (find-class 'float))
    (cons                                          (find-class 'cons))
    (character                                     (find-class 'character))
    (hash-table                                    (find-class 'hash-table))
    (package                                       (find-class 'package))
    (pathname                                      (find-class 'pathname))
    (readtable                                     (find-class 'readtable))
    (stream                                        (find-class 'stream))
    (number 									   (find-class 'number))
    ((string *)                                    (find-class 'string))
    ((bit-vector *)                                (find-class 'bit-vector))
    ((vector * *)   							   (find-class 'vector))
    ((array * *)                				   (find-class 'array))
    (sequence							           (find-class 'sequence))
    (function                                      (find-class 'function))
    (t                                             (find-class 't))))

;;; subclassp and sub-specializer-p

(defun subclassp (c1 c2)
	(not (null (find c2 (class-precedence-list c1)))))

(defun sub-specializer-p (c1 c2 c-arg)
	(let ((cpl (class-precedence-list c-arg)))
		(not (null (find c2 (cdr (member c1 cpl)))))))

;;;
;;; Class metaobjects and standard-class
;;;

(defparameter the-defclass-standard-class   ;standard-class's defclass form
	'(defclass standard-class ()
		((name :initarg :name)              ; :accessor class-name
		 (direct-superclasses               ; :accessor class-direct-superclasses
				:initarg :direct-superclasses)
		 (direct-slots)                     ; :accessor class-direct-slots
		 (class-precedence-list)            ; :accessor class-precedence-list
		 (effective-slots)                  ; :accessor class-slots
		 (direct-subclasses :initform ())   ; :accessor class-direct-subclasses
		 (direct-methods :initform ())      ; :accessor class-direct-methods
		 (shared-slots :initform ())	    ; :accessor class-shared-slots
		 (shared-slot-definitions :initform ())))) ; :accessor class-shared-slot-definitions

;;; Defining the metaobject slot accessor function as regular functions 
;;; greatly simplifies the implementation without removing functionality.
 
(defun class-name (class) (std-slot-value class 'name))
(defun (setf class-name) (new-value class) (setf (slot-value class 'name) new-value))

(defun class-direct-superclasses (class) (slot-value class 'direct-superclasses))
(defun (setf class-direct-superclasses) (new-value class)
	(setf (slot-value class 'direct-superclasses) new-value))

(defun class-direct-slots (class) (slot-value class 'direct-slots))
(defun (setf class-direct-slots) (new-value class)
	(setf (slot-value class 'direct-slots) new-value))

(defun class-precedence-list (class) (slot-value class 'class-precedence-list))
(defun (setf class-precedence-list) (new-value class)
	(setf (slot-value class 'class-precedence-list) new-value))

(defun class-slots (class) (slot-value class 'effective-slots))
(defun (setf class-slots) (new-value class)
	(setf (slot-value class 'effective-slots) new-value))

(defun class-direct-subclasses (class) (slot-value class 'direct-subclasses))
(defun (setf class-direct-subclasses) (new-value class)
	(setf (slot-value class 'direct-subclasses) new-value))

(defun class-direct-methods (class) (slot-value class 'direct-methods))
(defun (setf class-direct-methods) (new-value class)
	(setf (slot-value class 'direct-methods) new-value))

(defun class-shared-slots (class) (slot-value class 'shared-slots))
(defun (setf class-shared-slots) (new-value class)
	(setf (slot-value class 'shared-slots) new-value))

(defun class-shared-slot-definitions (class) (slot-value class 'shared-slot-definitions))
(defun (setf class-shared-slot-definitions) (new-value class)
	(setf (slot-value class 'shared-slot-definitions) new-value))

;;; defclass

(defmacro defclass (name direct-superclasses slot-definitions
                    &rest options)
	`(ensure-class ',name
			     :direct-superclasses
			       ,(canonicalize-direct-superclasses direct-superclasses)
			     :direct-slots
			       ,(canonicalize-direct-slots slot-definitions)
				 :shared-slots
			       ,(canonicalize-shared-slots slot-definitions)
				 :shared-slot-initforms
				   ,(canonicalize-shared-slot-initforms slot-definitions)		
			     ,@(canonicalize-defclass-options options)))
	
(defun canonicalize-direct-slots (slot-definitions)
   `(list ,@(apply 'append (mapcar #'canonicalize-direct-slot slot-definitions))))

(defun canonicalize-shared-slots (slot-definitions)
   `(list ,@(apply 'append (mapcar #'canonicalize-shared-slot slot-definitions))))

(defun canonicalize-direct-slot (spec)
	(canonicalize-slot-definition spec ':instance))

(defun canonicalize-shared-slot (spec)
	(canonicalize-slot-definition spec ':class))
	
(defun canonicalize-slot-definition (spec allocation-type)
	(if (symbolp spec)
		(if (not (eq allocation-type ':class))
			`((list :name ',spec)))
		(let ((name (car spec))
			  (initfunction nil)
			  (initform nil)
			  (initargs ())
			  (readers ())
			  (writers ())
			  (other-options ()))
			
			;; filter out any shared slots
			(let* ((alloc-option (member ':allocation spec))
				   (allocation ':instance))
				(if alloc-option
					(setf allocation (cadr alloc-option)))
				(if (or (and (eq allocation-type ':class)(not (eq allocation ':class)))
						(and (not (eq allocation-type ':class))(eq allocation ':class)))
					(return-from canonicalize-slot-definition '())))
			
			(do ((olist (cdr spec) (cddr olist)))
				((null olist))
				(case (car olist)
					(:initform
						(setq initfunction
							`(function (lambda () ,(cadr olist))))
						(setq initform `',(cadr olist)))
					(:initarg (push-on-end (cadr olist) initargs))
					(:reader (push-on-end (cadr olist) readers))
					(:writer (push-on-end (cadr olist) writers))
					(:accessor 
						(push-on-end (cadr olist) readers)
						(push-on-end `(setf ,(cadr olist)) writers))
					(otherwise 
						(push-on-end `',(car olist) other-options)
						(push-on-end `',(cadr olist) other-options))))
			`((list
					:name ',name
					,@(when initfunction
						`(:initform ,initform :initfunction ,initfunction))
					,@(when initargs `(:initargs ',initargs))
					,@(when readers `(:readers ',readers))
					,@(when writers `(:writers ',writers))
					,@other-options)))))

(defun canonicalize-shared-slot-initforms (slot-definitions)
	(let ((forms '()))
		(dolist (spec slot-definitions)
			(if (and (listp spec)(eq (cadr (member ':allocation spec)) ':class))
				(let ((option (member ':initform spec)))
					(push (if option (cadr option) `',secret-unbound-value) forms))))
		`(list ,@(nreverse forms))))

(defun canonicalize-direct-superclasses (direct-superclasses)
  `(list ,@(mapcar #'canonicalize-direct-superclass direct-superclasses)))

(defun canonicalize-direct-superclass (class-name)
  `(find-class ',class-name))

(defun canonicalize-defclass-options (options)
  (mapappend #'canonicalize-defclass-option options))

(defun canonicalize-defclass-option (option)
  (case (car option)
    (:metaclass
      (list ':metaclass
       `(find-class ',(cadr option))))
    (:default-initargs
      (list 
       ':direct-default-initargs
       `(list ,@(mapappend
                  #'(lambda (x) x)
                  (mapplist
                    #'(lambda (key value)
                        `(',key ,value))
                    (cdr option))))))
    (t (list `',(car option) `',(cadr option)))))

;;; find-class

(let ((class-table (make-hash-table :test #'eq :synchronized t)))

  (defun find-class (symbol &optional (errorp t)(environment nil))
        (declare (ignore environment))
        (let ((class (gethash symbol class-table nil)))
            (if (and (null class) errorp)
                (error "No class named ~S." symbol)
                class)))

  (defun (setf find-class) (new-value symbol)
    (setf (gethash symbol class-table) new-value))

  (defun forget-all-classes ()
    (clrhash class-table)
    (values))
 ) ;end let class-table

;;; Ensure class

(defun ensure-class (name &rest all-keys
                          &key (metaclass the-class-standard-class)
                          &allow-other-keys)
	(let ((class (apply (if (eq metaclass the-class-standard-class)
                              #'make-instance-standard-class
                              #'make-instance)
                          	metaclass :name name all-keys)))
		(setf (find-class name) class)
       class))

;;; make-instance-standard-class creates and initializes an instance of
;;; standard-class without falling into method lookup.  However, it cannot be
;;; called until standard-class itself exists.

(defun make-instance-standard-class (metaclass 
		&key name direct-superclasses direct-slots shared-slots shared-slot-initforms &allow-other-keys)
	(declare (ignore metaclass))
	(let ((class (std-allocate-instance the-class-standard-class)))
		(setf (class-name class) name)
		(setf (class-direct-subclasses class) ())
		(setf (class-direct-methods class) ())
		(std-after-initialization-for-classes class
			:direct-slots direct-slots
			:shared-slots shared-slots
			:direct-superclasses direct-superclasses
			:shared-slot-initforms shared-slot-initforms)
		class))

(defun std-after-initialization-for-classes (class 
		&key direct-superclasses direct-slots shared-slots shared-slot-initforms &allow-other-keys)
	;; update class hierarchy
	(let ((supers
				(or direct-superclasses
					(list (find-class 'standard-object)))))
		(setf (class-direct-superclasses class) supers)
		(dolist (superclass supers)
			(push class (class-direct-subclasses superclass))))
	
	(let ((slots
				(mapcar #'(lambda (slot-properties)
						(apply #'make-direct-slot-definition
							slot-properties))
                    direct-slots)))
		(setf (class-direct-slots class) slots)
		(dolist (direct-slot slots)
			(dolist (reader (slot-definition-readers direct-slot))
				(add-reader-method
					class reader (slot-definition-name direct-slot)))
			(dolist (writer (slot-definition-writers direct-slot))
				(add-writer-method
					class writer (slot-definition-name direct-slot)))))

	(setf (class-shared-slot-definitions class) nil)
	(setf (class-shared-slots class) nil)
	
	(let ((inherited-shared-slot-definitions '())
		  (inherited-shared-slot-bindings '()))
		(dolist (c (class-direct-superclasses class))
			(if (slot-boundp c 'shared-slot-definitions)
				(dolist (slot-definition (class-shared-slot-definitions c))
					(push slot-definition inherited-shared-slot-definitions)))
			(if (slot-boundp c 'shared-slots)
				(let ((shared-slots (class-shared-slots c)))
					(dotimes (i (length shared-slots))
						(push (aref shared-slots i) inherited-shared-slot-bindings)))))
		(when (or shared-slots inherited-shared-slot-bindings)				
			;; create shared (class) slots
			(let* ((slots
						(mapcar #'(lambda (slot-properties)
							(apply #'make-direct-slot-definition
								slot-properties))
							shared-slots))
				   (num-new-slots (length slots))
				   (index 0)
				   (slot-storage 
						(allocate-shared-slot-storage 
							(+ num-new-slots (length inherited-shared-slot-bindings)) 
							secret-unbound-value)))
				(setf (class-shared-slots class) slot-storage)
				(setf slots (append slots (nreverse inherited-shared-slot-definitions)))
				(setf inherited-shared-slot-bindings (nreverse inherited-shared-slot-bindings))
				(setf (class-shared-slot-definitions class) slots)
				(dotimes (i (min num-new-slots (length shared-slot-initforms)))
					(setf (car (aref slot-storage i)) (nth i shared-slot-initforms)))
				;; replace bindings with the bindings from the superclasses
				(dotimes (i (length inherited-shared-slot-bindings))
					(setf (aref slot-storage (+ i num-new-slots)) (elt inherited-shared-slot-bindings i)))
				(dolist (shared-slot slots)
					(dolist (reader (slot-definition-readers shared-slot))
						(add-class-slot-reader-method
							class reader (slot-definition-name shared-slot) index))
					(dolist (writer (slot-definition-writers shared-slot))
						(add-class-slot-writer-method
							class writer (slot-definition-name shared-slot) index))
					(incf index)))))
	
	(if (eq (class-of class) the-class-standard-class)
		(std-finalize-inheritance class)
		(finalize-inheritance class))
	(values))

;;; Slot definition metaobjects

;;; N.B. Quietly retain all unknown slot options (rather than signaling an
;;; error), so that it's easy to add new ones.
;;; This is used for shared slots as well. -RGC
(defun make-direct-slot-definition
       (&rest properties
        &key name (initargs ()) (initform nil) (initfunction nil)
             (readers ()) (writers ()) (allocation :instance)
        &allow-other-keys)
  (let ((slot (copy-list properties))) ; Don't want to side effect &rest list
    (setf (getf* slot ':name) name)
    (setf (getf* slot ':initargs) initargs)
    (setf (getf* slot ':initform) initform)
    (setf (getf* slot ':initfunction) initfunction)
    (setf (getf* slot ':readers) readers)
    (setf (getf* slot ':writers) writers)
    (setf (getf* slot ':allocation) allocation)
    slot))

(defun make-effective-slot-definition
       (&rest properties
        &key name (initargs ()) (initform nil) (initfunction nil)
             (allocation :instance)
        &allow-other-keys)
  (let ((slot (copy-list properties)))  ; Don't want to side effect &rest list
    (setf (getf* slot ':name) name)
    (setf (getf* slot ':initargs) initargs)
    (setf (getf* slot ':initform) initform)
    (setf (getf* slot ':initfunction) initfunction)
    (setf (getf* slot ':allocation) allocation)
    slot))

(defun slot-definition-name (slot) (getf slot ':name))
(defun (setf slot-definition-name) (new-value slot)
	(setf (getf* slot ':name) new-value))

(defun slot-definition-initfunction (slot) (getf slot ':initfunction))
(defun (setf slot-definition-initfunction) (new-value slot)
	(setf (getf* slot ':initfunction) new-value))

(defun slot-definition-initform (slot) (getf slot ':initform))
(defun (setf slot-definition-initform) (new-value slot)
	(setf (getf* slot ':initform) new-value))

(defun slot-definition-initargs (slot) (getf slot ':initargs))
(defun (setf slot-definition-initargs) (new-value slot)
	(setf (getf* slot ':initargs) new-value))

(defun slot-definition-readers (slot) (getf slot ':readers))
(defun (setf slot-definition-readers) (new-value slot)
	(setf (getf* slot ':readers) new-value))

(defun slot-definition-writers (slot) (getf slot ':writers))
(defun (setf slot-definition-writers) (new-value slot)
	(setf (getf* slot ':writers) new-value))

(defun slot-definition-allocation (slot)
	(getf slot ':allocation))
(defun (setf slot-definition-allocation) (new-value slot)
	(setf (getf* slot ':allocation) new-value))

;;; finalize-inheritance

(defun std-finalize-inheritance (class) 
  (setf (class-precedence-list class)
        (funcall (if (eq (class-of class) the-class-standard-class)
                     #'std-compute-class-precedence-list
                     #'compute-class-precedence-list)
                 class))
  (setf (class-slots class)
        (funcall (if (eq (class-of class) the-class-standard-class)
                     #'std-compute-slots
                     #'compute-slots)
                 class))
  (values))

;;; Class precedence lists

(defun std-compute-class-precedence-list (class)
  (let ((classes-to-order (collect-superclasses* class)))
    (topological-sort classes-to-order
                      (remove-duplicates
                        (mapappend #'local-precedence-ordering
                                   classes-to-order))
                      #'std-tie-breaker-rule)))

;;; topological-sort implements the standard algorithm for topologically
;;; sorting an arbitrary set of elements while honoring the precedence
;;; constraints given by a set of (X,Y) pairs that indicate that element
;;; X must precede element Y.  The tie-breaker procedure is called when it
;;; is necessary to choose from multiple minimal elements; both a list of 
;;; candidates and the ordering so far are provided as arguments.

(defun topological-sort (elements constraints tie-breaker)
  (let ((remaining-constraints constraints)
        (remaining-elements elements)
        (result ())) 
    (loop
     (let ((minimal-elements 
            (remove-if
             #'(lambda (class)
                 (member class remaining-constraints
                         :key #'cadr))
             remaining-elements)))
       (when (null minimal-elements)
             (if (null remaining-elements)
                 (return-from topological-sort result)
               (error "Inconsistent precedence graph.")))
       (let ((choice (if (null (cdr minimal-elements))
                         (car minimal-elements)
                       (funcall tie-breaker
                                minimal-elements
                                result))))
         (setq result (append result (list choice)))
         (setq remaining-elements
               (remove choice remaining-elements))
         (setq remaining-constraints
               (remove choice
                       remaining-constraints
                       :test #'member)))))))

;;; In the event of a tie while topologically sorting class precedence lists,
;;; the CLOS Specification says to "select the one that has a direct subclass
;;; rightmost in the class precedence list computed so far."  The same result
;;; is obtained by inspecting the partially constructed class precedence list
;;; from right to left, looking for the first minimal element to show up among
;;; the direct superclasses of the class precedence list constituent.  
;;; (There's a lemma that shows that this rule yields a unique result.)

(defun std-tie-breaker-rule (minimal-elements cpl-so-far)
  (dolist (cpl-constituent (reverse cpl-so-far))
    (let* ((supers (class-direct-superclasses cpl-constituent))
           (common (intersection minimal-elements supers)))
      (when (not (null common))
        (return-from std-tie-breaker-rule (car common))))))

;;; This version of collect-superclasses* isn't bothered by cycles in the class
;;; hierarchy, which sometimes happen by accident.

(defun collect-superclasses* (class)
  (labels ((all-superclasses-loop (seen superclasses)
              (let ((to-be-processed
                       (set-difference superclasses seen)))
                (if (null to-be-processed)
                    superclasses
                    (let ((class-to-process
                             (car to-be-processed)))
                      (all-superclasses-loop
                        (cons class-to-process seen)
                        (union (class-direct-superclasses
                                 class-to-process)
                               superclasses)))))))
    (all-superclasses-loop () (list class))))

;;; The local precedence ordering of a class C with direct superclasses C_1,
;;; C_2, ..., C_n is the set ((C C_1) (C_1 C_2) ...(C_n-1 C_n)).

(defun local-precedence-ordering (class)
  (mapcar #'list 
          (cons class
                (butlast (class-direct-superclasses class)))
          (class-direct-superclasses class)))

;;; Slot inheritance

(defun std-compute-slots (class)
  (let* ((all-slots (nreverse (mapappend #'class-direct-slots
                               (reverse (class-precedence-list class)))))
         (all-names (remove-duplicates 
                      (mapcar #'slot-definition-name all-slots))))
    (nreverse (mapcar #'(lambda (name)
                (funcall
                  (if (eq (class-of class) the-class-standard-class)
                      #'std-compute-effective-slot-definition 
                      #'compute-effective-slot-definition)
                  class
                  (remove name all-slots
                          :key #'slot-definition-name
                          :test-not #'eq)))
            all-names))))

(defun std-compute-effective-slot-definition (class direct-slots)
  (declare (ignore class))
  (let ((initer (find-if-not #'null direct-slots
                             :key #'slot-definition-initfunction)))
    (make-effective-slot-definition
      :name (slot-definition-name (car direct-slots))
      :initform (if initer
                    (slot-definition-initform initer)
                    nil)
      :initfunction (if initer
                        (slot-definition-initfunction initer)
                        nil)
      :initargs (remove-duplicates 
                  (mapappend #'slot-definition-initargs
                             direct-slots))
      :allocation (slot-definition-allocation (car direct-slots)))))

;;;
;;; Generic function metaobjects and standard-generic-function
;;;

(defparameter the-defclass-generic-function
  '(defclass generic-function ()))

(defparameter the-defclass-standard-generic-function
 '(defclass standard-generic-function (generic-function)
      ((name :initarg :name)      ; :accessor generic-function-name
       (lambda-list               ; :accessor generic-function-lambda-list
          :initarg :lambda-list)          
       (required-args             ; :accessor generic-function-required-args
          :initarg :required-args)          
       (methods :initform ())     ; :accessor generic-function-methods
       (method-class              ; :accessor generic-function-method-class
          :initarg :method-class)
       (discriminating-function)  ; :accessor generic-function-
                                  ;    -discriminating-function
       (classes-to-emf-table      ; :accessor classes-to-emf-table
          :initform (make-method-table))
       (method-combination :initarg :method-combination :initform 'standard) ; :accessor generic-function-method-combination
       (method-combination-order :initform ':most-specific-first)))) ; :accessor generic-function-method-combination-order

(defvar the-class-gf)           ;generic-function's class metaobject
(defvar the-class-standard-gf)  ;standard-generic-function's class metaobject

(defun generic-function-name (gf)
	(if (eq (class-of gf) the-class-standard-gf) 
		(slot-value-with-index gf 'name slot-location-generic-function-name)
		(slot-value gf 'name)))

(defun (setf generic-function-name) (new-value gf)
	(if (eq (class-of gf) the-class-standard-gf) 
		(setf (slot-value-with-index gf 'name slot-location-generic-function-name) 
			new-value)
		(setf (slot-value gf 'name) new-value)))

(defun generic-function-required-args (gf)
	(if (eq (class-of gf) the-class-standard-gf) 
		(slot-value-with-index gf 'required-args 
			slot-location-generic-function-required-args)
		(slot-value gf 'required-args)))
 
(defun (setf generic-function-required-args) (new-value gf)
	(if (eq (class-of gf) the-class-standard-gf) 
  		(setf (slot-value-with-index gf 
			'required-args slot-location-generic-function-required-args) 
		new-value)
		(setf (slot-value gf 'required-args) new-value)))

(defun generic-function-lambda-list (gf)
	(if (eq (class-of gf) the-class-standard-gf) 
		(slot-value-with-index gf 'lambda-list 
			slot-location-generic-function-lambda-list)
		(slot-value gf 'lambda-list)))

(defun (setf generic-function-lambda-list) (new-value gf)
	(if (eq (class-of gf) the-class-standard-gf)
		(progn
	  		(setf (generic-function-required-args gf) 
				(getf (analyze-lambda-list new-value) ':required-args))
	  		(setf (slot-value-with-index gf 'lambda-list 
				slot-location-generic-function-lambda-list) 
				new-value))
		(setf (slot-value gf 'lambda-list) new-value)))

(defun generic-function-methods (gf)
	(if (eq (class-of gf) the-class-standard-gf) 
		(slot-value-with-index gf 'methods slot-location-generic-function-methods)
		(slot-value gf 'methods)))

(defun (setf generic-function-methods) (new-value gf)
 	(if (eq (class-of gf) the-class-standard-gf) 
 		(setf (slot-value-with-index gf 'methods slot-location-generic-function-methods) 
			new-value)
		(setf (slot-value gf 'methods) new-value)))

(defun generic-function-discriminating-function (gf)
	(if (eq (class-of gf) the-class-standard-gf) 
  		(slot-value-with-index gf 'discriminating-function 
			slot-location-generic-function-discriminating-function)
		(slot-value gf 'discriminating-function)))

(defun (setf generic-function-discriminating-function) (new-value gf)
	(if (eq (class-of gf) the-class-standard-gf) 
  		(setf (slot-value-with-index gf 'discriminating-function 
			slot-location-generic-function-discriminating-function) 
			new-value)
		(setf (slot-value gf 'discriminating-function) new-value)))

(defun generic-function-method-class (gf)
 	(if (eq (class-of gf) the-class-standard-gf) 
 		(slot-value-with-index gf 'method-class 
			slot-location-generic-function-method-class)
		(slot-value gf 'method-class)))

(defun (setf generic-function-method-class) (new-value gf)
	(if (eq (class-of gf) the-class-standard-gf) 
	  	(setf (slot-value-with-index gf 'method-class 
				slot-location-generic-function-method-class) 
			new-value)
		(setf (slot-value gf 'method-class) new-value)))

;;; Internal accessor for effective method function table

(defun classes-to-emf-table (gf)
	(if (eq (class-of gf) the-class-standard-gf) 
  		(slot-value-with-index gf 'classes-to-emf-table 
			slot-location-generic-function-classes-to-emf-table)
		(slot-value gf 'classes-to-emf-table)))

(defun (setf classes-to-emf-table) (new-value gf)
	(if (eq (class-of gf) the-class-standard-gf) 
	  	(setf (slot-value-with-index gf 'classes-to-emf-table 
				slot-location-generic-function-classes-to-emf-table) 
			new-value)
		(setf (slot-value gf 'classes-to-emf-table) new-value)))

(defun (setf generic-function-method-combination) (new-value gf)
	(if (eq (class-of gf) the-class-standard-gf) 
	  	(setf (slot-value-with-index gf 'method-combination 
				slot-location-generic-function-method-combination) 
			new-value)
		(setf (slot-value gf 'method-combination) new-value)))

(defun (setf generic-function-method-combination-order) (new-value gf)
	(if (eq (class-of gf) the-class-standard-gf) 
	  	(setf (slot-value-with-index gf 'method-combination-order 
				slot-location-generic-function-method-combination-order) 
			new-value)
		(setf (slot-value gf 'method-combination-order) new-value)))


;;;
;;; Method metaobjects and standard-method
;;;

(defparameter the-defclass-standard-method
 '(defclass standard-method ()
   ((lambda-list :initarg :lambda-list)     ; :accessor method-lambda-list
    (qualifiers :initarg :qualifiers)       ; :accessor method-qualifiers
    (specializers :initarg :specializers)   ; :accessor method-specializers
    (body :initarg :body)                   ; :accessor method-body
    (environment :initarg :environment)     ; :accessor method-environment
    (generic-function :initform nil)        ; :accessor method-generic-function
    (function))))                           ; :accessor method-function

(defvar the-class-standard-method)    ;standard-method's class metaobject

(defun method-lambda-list (method) (slot-value method 'lambda-list))
(defun (setf method-lambda-list) (new-value method)
  (setf (slot-value method 'lambda-list) new-value))

(defun method-qualifiers (method) (slot-value method 'qualifiers))
(defun (setf method-qualifiers) (new-value method)
  (setf (slot-value method 'qualifiers) new-value))

(defun method-specializers (method) (slot-value method 'specializers))
(defun (setf method-specializers) (new-value method)
  (setf (slot-value method 'specializers) new-value))

(defun method-body (method) (slot-value method 'body))
(defun (setf method-body) (new-value method)
  (setf (slot-value method 'body) new-value))

(defun method-environment (method) (slot-value method 'environment))
(defun (setf method-environment) (new-value method)
  (setf (slot-value method 'environment) new-value))

(defun method-generic-function (method)
  (slot-value method 'generic-function))
(defun (setf method-generic-function) (new-value method)
  (setf (slot-value method 'generic-function) new-value))

(defun method-function (method) (slot-value method 'function))
(defun (setf method-function) (new-value method)
  (setf (slot-value method 'function) new-value))

;;;
;;; Common Lisp DEFGENERIC macro
;;;
(defmacro defgeneric (function-name lambda-list &rest options)
    (flet ((is-method-option (opt) (eq (car opt) :method))
           (method-definition-form (opt) `(defmethod ,function-name ,@(cdr opt))))
        (let* ((method-definitions (mapcar #'method-definition-form (remove-if (complement #'is-method-option) options)))
                (non-method-options (remove-if #'is-method-option options))
                (documentation-form
                    (let ((doc-string (cadr (find-if #'(lambda (opt) (eq (car opt) :documentation)) non-method-options))))
                        (when doc-string `(setf (documentation ',function-name 'function) ,doc-string)))))            
            `(progn 
                (ensure-generic-function
                    ',function-name 
                    :lambda-list ',lambda-list
                    ,@(canonicalize-defgeneric-options non-method-options))
                 ,(when documentation-form documentation-form)
                 ,@method-definitions
                 ))))

(defun canonicalize-defgeneric-options (options)
  (mapappend #'canonicalize-defgeneric-option options))

(defun canonicalize-defgeneric-option (option)
  (case (car option)
    (:generic-function-class
      (list ':generic-function-class
            `(find-class ',(cadr option))))
    (:method-class
      (list ':method-class
            `(find-class ',(cadr option))))
    (:method-combination
      `(:method-combination ',(cadr option)))
    (t (list `',(car option) `',(cadr option)))))

;;; find-generic-function looks up a generic function by name.  It's an
;;; artifact of the fact that our generic function metaobjects can't legally
;;; be stored a symbol's function value. 

(let ((generic-function-table (make-hash-table :test #'equal :synchronized t)))

  (defun find-generic-function (symbol &optional (errorp t))
		(if (consp symbol)
			(if (and (eq (car symbol) 'SETF)(symbolp (cadr symbol)))
				(let ((setf-func (get-setf-function (cadr symbol))))
                    (if (and (symbolp setf-func) (fboundp setf-func))
                        (setf setf-func (symbol-function setf-func)))
					(if (and setf-func (standard-generic-function-p setf-func))
						(return-from find-generic-function setf-func)))
				(error "Invalid generic function name: ~S" symbol))
			(if (fboundp symbol)
				(let ((func (symbol-function symbol)))
					(if (standard-generic-function-p func)
						(return-from find-generic-function func)))))
		(let ((gf (gethash symbol generic-function-table nil)))
			(if (and (null gf) errorp)
				(error "No generic function named ~S." symbol)
				gf)))

  (defun (setf find-generic-function) (func symbol)
		(if (standard-generic-function-p func)
			(if (consp symbol)
				(if (and (eq (car symbol) 'SETF)(symbolp (cadr symbol)))
					(let ((setter-name (cl::setf-function-symbol symbol)))
						(setf (symbol-function setter-name) func)
						(register-setf-function (cadr symbol) setter-name))
					(error "Invalid generic function name: ~S" symbol))
				(setf (symbol-function symbol) func))
    		(setf (gethash symbol generic-function-table) func)))

  (defun forget-all-generic-functions ()
    (clrhash generic-function-table)
    (values))
 ) ;end let generic-function-table

;;; ensure-generic-function

(defun ensure-generic-function
       (function-name
        &rest all-keys
        &key (generic-function-class the-class-standard-gf)
             (method-class the-class-standard-method)
			 lambda-list
        &allow-other-keys)
  (if (find-generic-function function-name nil) 
      (find-generic-function function-name)
      (let ((gf (apply (if (eq generic-function-class the-class-standard-gf)
                           #'make-instance-standard-generic-function
                           #'make-instance)
                       generic-function-class
                       :name function-name
                       :method-class method-class
				       :required-args (getf (analyze-lambda-list lambda-list) ':required-args)
                       all-keys)))
         (setf (find-generic-function function-name) gf)
         gf)))

;;; finalize-generic-function

;;; Same basic idea as finalize-inheritance.  
;;; Takes care of recomputing and storing the discriminating 
;;; function, and clearing the effective method function table.

(defun finalize-generic-function (gf)
  (setf (generic-function-discriminating-function gf)
        (funcall (if (eq (class-of gf) the-class-standard-gf)
                     #'std-compute-discriminating-function
                     #'compute-discriminating-function)
                 gf))
	(unless (standard-generic-function-p gf)
		(setf (fdefinition (generic-function-name gf))
			(generic-function-discriminating-function gf)))
  (clear-method-table (classes-to-emf-table gf))
  (values))

;;; make-instance-standard-generic-function creates and initializes an
;;; instance of standard-generic-function without falling into method lookup.
;;; However, it cannot be called until standard-generic-function exists.

(defun make-instance-standard-generic-function
       (generic-function-class 
        &key 
            name 
            lambda-list 
            method-class 
            (method-combination 'standard)
            (method-combination-order ':most-specific-first)
            &allow-other-keys)
  (declare (ignore generic-function-class))
  (let ((gf (std-allocate-instance the-class-standard-gf)))
    (setf (generic-function-name gf) name)
    (setf (generic-function-lambda-list gf) lambda-list)
    (setf (generic-function-methods gf) ()) 
    (setf (generic-function-method-class gf) method-class) 
    (setf (classes-to-emf-table gf) (make-method-table))
    (setf (generic-function-method-combination gf) method-combination)
    (setf (generic-function-method-combination-order gf) method-combination-order)
    (finalize-generic-function gf)
    gf))

;;; defmethod

(defmacro defmethod (&rest args)
  (multiple-value-bind (function-name qualifiers
                        lambda-list specializers body)
        (parse-defmethod args)
    `(ensure-method (find-generic-function ',function-name nil)
	   :generic-function-name ',function-name
       :lambda-list ',lambda-list
       :qualifiers ',qualifiers
       :specializers ,(canonicalize-specializers specializers) 
       :body ',body
       :environment (funcall (lambda () (cl::capture-compiler-environment)))#| nil |#
	   :eql-specializers (some #'(lambda (x) (and (listp x)(eq (car x) 'EQL))) ',specializers))))

(defun canonicalize-specializers (specializers)
  `(list ,@(mapcar #'canonicalize-specializer specializers)))

(defun canonicalize-specializer (specializer)
  (if (and (listp specializer) (eq (car specializer) 'eql))
      `(intern-eql-specializer ,(cadr specializer) ',(cadr specializer))
      `(find-class ',specializer)))

(defun specalization-vars (specialized-lambda-list)
    (let ((vars '()))
        (dolist (x specialized-lambda-list (nreverse vars))
            (if (member x lambda-list-keywords)
                (return (nreverse vars))
                (if (consp x)
                    (push (car x) vars))))))

(defun create-ignorable-decls (vars)
    `(declare (ignorable ,@vars)))

(defun parse-defmethod (args)
	(let ((fn-spec (car args))
          (qualifiers ())
          (specialized-lambda-list nil)
          (body '())
		  (decls '())
		  (doc-string nil)
          (parse-state :qualifiers))
		(do* ((a (cdr args))
			  (arg (car a)(car a)))
			((null a))
			(cond 
				((eq parse-state :qualifiers)
					(if (and (atom arg) (not (null arg)))
						(progn
							(push-on-end arg qualifiers)
							(setf a (cdr a)))
						(setq parse-state :lambda-list)))
				((eq parse-state :lambda-list)
					(setq specialized-lambda-list arg)
					(setq parse-state :decls-and-doc-string)
					(setf a (cdr a)))
				((eq parse-state :decls-and-doc-string)
					(if (and (null doc-string) (stringp arg))
                        (setq doc-string arg a (cdr a))
    					(if (and (consp arg)(eq (car arg) 'declare))
    						(progn
    							(push-on-end arg decls)
    							(setf a (cdr a)))
                            (setq parse-state :body))))                    
				((eq parse-state :body)
					(push-on-end arg body)
					(setf a (cdr a)))))
		;; watch for case where a literal string is the only item in body--
		;; we don't want to mistake it for a doc-string
		(if (and (null body) doc-string)
			(setf body (list doc-string) doc-string nil))
		(values fn-spec
			qualifiers
			(extract-lambda-list specialized-lambda-list)
			(extract-specializers specialized-lambda-list)
            ;; add IGNORABLE declarations to specializers so they don't cause
            ;; warnings if they are not referenced in generated lambdas
			`(,@(cons (create-ignorable-decls (specalization-vars specialized-lambda-list))
                    decls)
				,@(if doc-string (list doc-string))
				(block
					,(if (consp fn-spec) (cadr fn-spec) fn-spec)
					,@body)))))

;;; Several tedious functions for analyzing lambda lists
(defun gf-required-arglist (gf)
	(generic-function-required-args gf))
 
(defun required-portion (gf args)
	(let ((required-args (generic-function-required-args gf))
		  (new-args nil))
		(dolist (x required-args (nreverse new-args))
			(declare (ignore x))
			(unless (consp args)
      			(error "Too few arguments to generic function ~S." gf))
			(push (car args) new-args)
			(setf args (cdr args)))))

(defun num-required-args (gf) (length (generic-function-required-args gf)))

(defun extract-lambda-list (specialized-lambda-list)
  (let* ((plist (analyze-lambda-list specialized-lambda-list))
         (requireds (getf plist ':required-names))
         (rv (getf plist ':rest-var))
         (ks (getf plist ':key-args))
         (aok (getf plist ':allow-other-keys))
         (opts (getf plist ':optional-args))
         (auxs (getf plist ':auxiliary-args)))
    `(,@requireds 
      ,@(if opts `(&optional ,@opts) ())
      ,@(if rv `(&rest ,rv) ())
      ,@(if (or ks aok) `(&key ,@ks) ())
      ,@(if aok '(&allow-other-keys) ())
      ,@(if auxs `(&aux ,@auxs) ()))))

(defun extract-specializers (specialized-lambda-list)
  (let ((plist (analyze-lambda-list specialized-lambda-list)))
    (getf plist ':specializers)))

(defun analyze-lambda-list (lambda-list)
  (labels ((make-keyword (symbol)
              (intern (symbol-name symbol)
                      (find-package 'keyword)))
           (get-keyword-from-arg (arg)
              (if (listp arg)
                  (if (listp (car arg))
                      (caar arg)
                      (make-keyword (car arg)))
                  (make-keyword arg))))
    (let ((keys ())           ; Just the keywords
          (key-args ())       ; Keywords argument specs
          (required-names ()) ; Just the variable names
          (required-args ())  ; Variable names & specializers
          (specializers ())   ; Just the specializers
          (rest-var nil)
          (optionals ())
          (auxs ())
          (allow-other-keys nil)
          (state :parsing-required))
      (dolist (arg lambda-list)
        (if (member arg lambda-list-keywords)
          (ecase arg
            (&optional
              (setq state :parsing-optional))
            (&rest
              (setq state :parsing-rest))
            (&key
              (setq state :parsing-key))
            (&allow-other-keys
              (setq allow-other-keys 't))
            (&aux
              (setq state :parsing-aux)))
          (case state
            (:parsing-required 
             (push-on-end arg required-args)
             (if (listp arg)
                 (progn (push-on-end (car arg) required-names)
                        (push-on-end (cadr arg) specializers))
                 (progn (push-on-end arg required-names)
                        (push-on-end 't specializers))))
            (:parsing-optional (push-on-end arg optionals))
            (:parsing-rest (setq rest-var arg))
            (:parsing-key
             (push-on-end (get-keyword-from-arg arg) keys)
             (push-on-end arg key-args))
            (:parsing-aux (push-on-end arg auxs)))))
      (list  :required-names required-names
             :required-args required-args
             :specializers specializers
             :rest-var rest-var
             :keywords keys
             :key-args key-args
             :auxiliary-args auxs
             :optional-args optionals
             :allow-other-keys allow-other-keys))))

;;; ensure method

(defun ensure-method (gf &rest all-keys &key eql-specializers &allow-other-keys)
	(if (null gf)
		;; define a generic function on the fly
		(setf gf 
			(ensure-generic-function 
				(getf all-keys :generic-function-name) 
				':lambda-list (getf all-keys :lambda-list))))
	
	;; as soon as we define one method with an EQL specifier, we assume
	;; methods of that generic function may specify this way
	(if eql-specializers
		(setf (method-table-eql-specializers (classes-to-emf-table gf)) t))
	
	(let ((new-method
          	(apply
              (if (eq (generic-function-method-class gf)
                      the-class-standard-method)
                  #'make-instance-standard-method 
                  #'make-instance)
              (generic-function-method-class gf)
              all-keys)))
    (add-method gf new-method)
    new-method))

;;; make-instance-standard-method creates and initializes an instance of
;;; standard-method without falling into method lookup.  However, it cannot
;;; be called until standard-method exists.

(defun make-instance-standard-method (method-class 
                                      &key lambda-list qualifiers 
                                           specializers body environment
									  &allow-other-keys)
  (declare (ignore method-class))
  (let ((method (std-allocate-instance the-class-standard-method)))
    (setf (method-lambda-list method) lambda-list)
    (setf (method-qualifiers method) qualifiers)
    (setf (method-specializers method) specializers)
    (setf (method-body method) body)
    (setf (method-environment method) environment)
    (setf (method-generic-function method) nil)
    (setf (method-function method) 
          (std-compute-method-function method))
    method))

;;; add-method

;;; This version first removes any existing method on the generic function
;;; with the same qualifiers and specializers.

(defun add-method (gf method)
  (let ((old-method
           (find-method gf (method-qualifiers method)
                           (method-specializers method) nil)))
    (when old-method (remove-method gf old-method)))
  (setf (method-generic-function method) gf)
  (push method (generic-function-methods gf))
  (dolist (specializer (method-specializers method))
    (pushnew method (class-direct-methods specializer)))
  (finalize-generic-function gf)
  method)

(defun remove-method (gf method)
  (setf (generic-function-methods gf)
        (remove method (generic-function-methods gf)))
  (setf (method-generic-function method) nil)
  (dolist (class (method-specializers method))
    (setf (class-direct-methods class)
          (remove method (class-direct-methods class))))
  (finalize-generic-function gf)
  method)

(defun find-method (gf qualifiers specializers
                    &optional (errorp t))
  (let ((method
          (find-if #'(lambda (method)
                       (and (equal qualifiers
                                   (method-qualifiers method))
                            (equal specializers
                                   (method-specializers method))))
                   (generic-function-methods gf))))
      (if (and (null method) errorp)
          (error "No such method for ~S." (generic-function-name gf))
          method)))

;;; Reader and write methods

(defun add-reader-method (class fn-name slot-name)
  (ensure-method
    (ensure-generic-function fn-name :lambda-list '(object))
    :lambda-list '(object)
    :qualifiers ()
    :specializers (list class)
    :body `((slot-value object ',slot-name))
    :environment (top-level-environment))
  (values))

(defun add-class-slot-reader-method (class fn-name slot-name index)
	(declare (ignore slot-name))
  (ensure-method
    (ensure-generic-function fn-name :lambda-list '(object))
    :lambda-list '(object)
    :qualifiers ()
    :specializers (list class)
    :body `((let* ((class (class-of object))
				   (val (car (aref (slot-value class 'shared-slots) ,index))))
				(if (eq secret-unbound-value val)
					(error "The class slot ~S is unbound in the class ~S." ',slot-name (class-name class))
					val)))

    :environment (top-level-environment))
  (values))

(defun add-writer-method (class fn-name slot-name)
  (ensure-method
    (ensure-generic-function 
      fn-name :lambda-list '(new-value object))
    :lambda-list '(new-value object)
    :qualifiers ()
    :specializers (list (find-class 't) class)
    :body `((setf (slot-value object ',slot-name)
                 new-value))
    :environment (top-level-environment))
  (values))

(defun add-class-slot-writer-method (class fn-name slot-name index)
	(declare (ignore slot-name))
  (ensure-method
    (ensure-generic-function 
      fn-name :lambda-list '(new-value object))
    :lambda-list '(new-value object)
    :qualifiers ()
    :specializers (list (find-class 't) class)
    :body `((setf (car (aref (slot-value (class-of object) 'shared-slots) ,index))
                 new-value))
    :environment (top-level-environment))
  (values))

;;;
;;; Generic function invocation
;;;

;;; apply-generic-function

(defun apply-generic-function (gf args)
  (apply (generic-function-discriminating-function gf) args))

(defun std-compute-discriminating-function (gf)
    (let ((table (classes-to-emf-table gf)))
        #'(lambda (&rest args)
               (let* ((eqls (if (listp (method-table-eql-specializers table)) (method-table-eql-specializers table) 
                               (let ((eqls (make-list (num-required-args gf))))
                                   (mapc #'(lambda (meth) (do ((specs (method-specializers meth) (cdr specs))
				                               (eqls eqls (cdr eqls))) ((not specs))
                                                              (when (eql-specializer-p (car specs))
                                                                    (pushnew (list (slot-value (car specs) 'object))
									     (car eqls)
									 :test #'(lambda (x y) (eql (car x) (car y)))))))
                                            (generic-function-methods gf))
                                   (setf (method-table-eql-specializers table) eqls))))
                       (req-args (required-portion gf args))
                       (eqls-classes (if eqls (mapcar #'(lambda (arg eqls)
							  (or (car (member arg eqls :key #'car)) (class-of arg))) req-args eqls)
                                               (mapcar #'class-of req-args))))
                   (funcall (or (find-method-table-method table eqls-classes)
                                (let ((classes (if eqls (mapcar #'(lambda (arg eql-class)
								     (if (consp eql-class)
									 (gethash arg *clos-singleton-specializers*) eql-class))
                                                                req-args eqls-classes) eqls-classes)))
                                   (slow-method-lookup gf table req-args classes eqls-classes))) args)))))

(defun slow-method-lookup (gf table args classes eqls-classes)
    (let* ((applicable-methods (compute-applicable-methods-using-classes gf classes))
             (emfun (if (member-if #'primary-method-p applicable-methods)
                             (if (eq (class-of gf) the-class-standard-gf)
                                 (std-compute-effective-method-function gf applicable-methods)
                                 (compute-effective-method-function gf applicable-methods))
                             (error "No primary methods for ~S in the ~S." args gf))))
        (add-method-table-method table eqls-classes emfun) emfun))

;;; compute-applicable-methods-using-classes

(defun compute-applicable-methods-using-classes
       (gf required-classes)
  (sort 
    (copy-list
      (remove-if-not #'(lambda (method)
                         (every #'subclassp
                                required-classes
                                (method-specializers method)))
                     (generic-function-methods gf)))
    #'(lambda (m1 m2) 
        (funcall
          (if (eq (class-of gf) the-class-standard-gf)
              #'std-method-more-specific-p
              #'method-more-specific-p)
          gf m1 m2 required-classes))))

;;; method-more-specific-p
;(setq cl::*compiler-warn-on-dynamic-return* nil)

(defun std-method-more-specific-p (gf method1 method2 required-classes)
	(declare (ignore gf))
	(do* ((specs1 (method-specializers method1)(cdr specs1))
		  (specs2 (method-specializers method2)(cdr specs2))
		  (classes required-classes (cdr classes))
		  (spec1 (car specs1)(car specs1))
		  (spec2 (car specs2)(car specs2))
		  (arg-class (car classes)(car classes)))
		((or (endp specs1)(endp specs2)(endp classes)) nil)
		(unless (eq spec1 spec2)
			(return-from std-method-more-specific-p
				(sub-specializer-p spec1 spec2 arg-class)))))
	
;(setq cl::*compiler-warn-on-dynamic-return* t)

;;; apply-methods and compute-effective-method-function

(defun apply-methods (gf args methods)
  (funcall (compute-effective-method-function gf methods)
           args))

(defun primary-method-p (method)
  (null (method-qualifiers method)))
(defun before-method-p (method)
  (equal '(:before) (method-qualifiers method)))
(defun after-method-p (method)
  (equal '(:after) (method-qualifiers method)))
(defun around-method-p (method)
  (equal '(:around) (method-qualifiers method)))

;;; If the name is a list i.e. (SETF FOO) then this creates a symbolic
;;; representation of the name, suitable as a block label.
(defun generic-func-name (gf)
	(let ((gfn (generic-function-name gf)))
		(if (symbolp gfn)
			gfn
			(make-symbol (format nil "~A" gfn)))))

;; new way in Corman Lisp 1.5 release
(defun std-compute-effective-method-function (gf methods)
	(let ((primaries (remove-if-not #'primary-method-p methods))
		  (around (find-if #'around-method-p methods)))
		(when (null primaries)
			(error "No primary methods for the generic function ~S." gf))
		(if around
			(let ((next-emfun
						(if (eq (class-of gf) the-class-standard-gf)
							(std-compute-effective-method-function gf (remove around methods))
							(compute-effective-method-function gf (remove around methods)))))
				#'(lambda (args)
					(funcall (method-function around) args next-emfun)))
			(let* ((next-emfun (compute-primary-emfun (cdr primaries)))
				   (befores (remove-if-not #'before-method-p methods))
				   (reverse-afters
						(reverse (remove-if-not #'after-method-p methods)))
				   (before-calls 
						(mapcar #'(lambda (before) 
									`(funcall ,(method-function before) args nil))
							befores))
				   (after-calls 
						(mapcar #'(lambda (after) 
									`(funcall ,(method-function after) args nil))
							reverse-afters)))
				(if after-calls
					(compile nil
						`(lambda (args)
							(block ,(generic-func-name gf)  ;; a named block enables the compiler to
								,@before-calls				;; tag the compiled code with the name
								(multiple-value-prog1		;; (for debugging, etc.)
									(funcall ,(method-function (car primaries)) args ,next-emfun)
									,@after-calls))))
					(compile nil
						`(lambda (args)
							(block ,(generic-func-name gf)
								,@before-calls
								(funcall ,(method-function (car primaries)) args ,next-emfun)))))))))

;;; compute an effective method function from a list of primary methods:

(defun compute-primary-emfun (methods)
  (if (null methods)
      nil
      (let ((next-emfun (compute-primary-emfun (cdr methods))))               
        #'(lambda (args)
            (funcall (method-function (car methods)) args next-emfun)))))

;;; apply-method and compute-method-function

(defun apply-method (method args next-methods)
  (funcall (method-function method)
           args
           (if (null next-methods)
               nil
               (compute-effective-method-function
                 (method-generic-function method) next-methods))))

;;; search a tree for a passed symbol or form
(defun search-tree (tree form)
	(if tree
		(if (eq tree form)
			t
			(if (consp tree)
				(or (search-tree (car tree) form)
					(search-tree (cdr tree) form))))))

(defun std-compute-method-function (method)
	(let ((form (macroexpand-all (cons 'progn (method-body method))))
		  (lambda-list (method-lambda-list method)))
		(if (or (search-tree form 'call-next-method) 
				(search-tree form 'next-method-p))
			(compile-in-lexical-environment (method-environment method)
				`(lambda (args next-emfun)
					(flet ((call-next-method (&rest cnm-args)
								(if (null next-emfun)
									(error "No next method for the~@
										generic function ~S."
										(method-generic-function ',method))
									(funcall next-emfun (or cnm-args args))))
						   (next-method-p () (not (null next-emfun))))
						(apply #'(lambda ,(kludge-arglist lambda-list) ,@(cdr form)) args))))
			(compile-in-lexical-environment (method-environment method)
				`(lambda (args next-emfun)
					(declare (ignore next-emfun))
					(apply #'(lambda ,(kludge-arglist lambda-list) ,@(cdr form)) args))))))

;;; N.B. The function kludge-arglist is used to pave over the differences
;;; between argument keyword compatibility for regular functions versus 
;;; generic functions.
;;; RGC--17 Aug 2006--modified to handle &aux lambda list variables.
;;;
(defun kludge-arglist (lambda-list)
    (let ((aux-vars (member '&aux lambda-list)))
        (if aux-vars
            (setq lambda-list (subseq lambda-list 0 (position '&aux lambda-list))))
        (if (and (member '&key lambda-list)
               (not (member '&allow-other-keys lambda-list)))
            (append lambda-list '(&allow-other-keys))
            (if (and (not (member '&rest lambda-list))
                    (not (member '&key lambda-list)))
                (append lambda-list '(&key &allow-other-keys) aux-vars)
                (append lambda-list aux-vars)))))

;;; Run-time environment hacking (Common Lisp ain't got 'em).

(defun top-level-environment ()
  nil) ; Bogus top level lexical environment

(defvar compile-methods nil)      ; by default, run everything interpreted

(defun compile-in-lexical-environment (env lambda-expr)
  (declare (ignore env))
  (if compile-methods
      (compile nil lambda-expr)
      (eval `(function ,lambda-expr ,env))))

;;;;
;;;;	Common Lisp FUNCALL function.
;;;;	Modified here to allow Standard-Generic-Functions as the 
;;;;    first argument.
;;;;
(in-package :x86)
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
	:t11
		mov		edi, [eax + (- (* 4 function-environment-offset) uvector-tag)] 
		callfunc	eax	
		jmp		short :t8
	:t6
		cmp 	edx, uvector-kfunction-tag
		jne		short :t10
		mov		edi, [esi]		;; environment for kfunctions is always NIL
		call	[eax + (- (* function-code-buffer-offset 4) uvector-tag)]
		jmp		short :t8
	:t10 
		cmp		edx, uvector-clos-instance-tag
		jne		short :t7
		mov		edx, [eax + (uvector-offset cl::clos-instance-class-offset)]	;; edx = clos class
		mov		ebx, 'cl::the-class-standard-gf
		mov		ebx, [ebx + (uvector-offset cl::symbol-value-offset)]
		mov		ebx, [ebx - cons-tag]
		cmp		ebx, edx												;; class = the-class-standard-gf?
		jne		short :t7
		mov		eax, [eax + (uvector-offset cl::clos-instance-slots-offset)]	;; eax = clos class slots
		mov		eax, [eax + (uvector-offset (+ 2 cl::slot-location-generic-function-discriminating-function))]
		jmp 	short :t11
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
;;;;	Modified here to allow Standard-Generic-Functions as the 
;;;;    first argument.
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
	:t13
		mov		edi, [eax + (- (* 4 function-environment-offset) uvector-tag)] 
		callfunc	eax	
		jmp		short :t11
	:t9
		cmp 	edx, uvector-kfunction-tag
		jne		short :t12
		mov		edi, [esi]		;; environment for kfunctions is always NIL
		call	[eax + (- (* function-code-buffer-offset 4) uvector-tag)]
		jmp		short :t11
	:t12 
		cmp		edx, uvector-clos-instance-tag
		jne		short :t10
		mov		edx, [eax + (uvector-offset cl::clos-instance-class-offset)]	;; edx = clos class
		mov		ebx, 'cl::the-class-standard-gf
		mov		ebx, [ebx + (uvector-offset cl::symbol-value-offset)]
		mov		ebx, [ebx - cons-tag]
		cmp		ebx, edx												;; class = the-class-standard-gf?
		jne		short :t10
		mov		eax, [eax + (uvector-offset cl::clos-instance-slots-offset)]	;; eax = clos class slots
		mov		eax, [eax + (uvector-offset (+ 2 cl::slot-location-generic-function-discriminating-function))]
		jmp 	short :t13
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
;;;;	Common Lisp FUNCTIONP function.
;;;;	Redefined here to add support for generic functions.
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
		jne		short :nil-exit
		mov 	eax, [edx - uvector-tag]
		cmp 	al, (tag-byte uvector-kfunction-tag)
		jbe 	short :t-exit
		cmp		al, (tag-byte uvector-clos-instance-tag)
		jne		short :nil-exit
		mov		eax, [edx + (uvector-offset cl::clos-instance-class-offset)]	;; eax = clos class
		mov		edx, 'cl::the-class-standard-gf
		mov		edx, [edx + (uvector-offset cl::symbol-value-offset)]
		mov		edx, [edx - cons-tag]
		cmp		eax, edx												;; class = the-class-standard-gf?
		jne		short :nil-exit	
	:t-exit
		mov		eax, [esi + t-offset]
		jmp		short :exit
	:nil-exit
		mov		eax, [esi]
	:exit
		pop		ebp
		ret
	})

(defasm cl::execution-address (func &rest args) 
	{
		push	ebp
		mov		ebp, esp
		push	edi
		push	ebx
		cmp		ecx, 1
		je		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		eax, [ebp + ARGS_OFFSET]	;; eax = argument
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
		mov		eax, [eax + (uvector-offset symbol-function-offset)]
		mov		eax, [eax - 4]
		mov		edx, eax
		and		edx, 7
		cmp		edx, uvector-tag
		je 		short :t9
		push	[ebp + ARGS_OFFSET]
		callp	_not-a-function-error
	:t9
		mov		edx, [eax - uvector-tag]	;; edx = uvector header
		shl		edx, 24
		shr		edx, 27
	:t3 ;; we should have a function in eax, and dl is the type
		cmp 	edx, uvector-function-tag
		jne		short :t6
	:t11
		mov		eax, [eax + (uvector-offset function-code-buffer-offset)]
		lea		eax, [eax + (uvector-offset compiled-code-execution-offset)] ;; eax = exec address
		jmp		short :got-addr
	:t6
		cmp 	edx, uvector-kfunction-tag
		jne		short :t10
		mov		eax, [eax + (uvector-offset function-code-buffer-offset)]
		jmp		short :got-addr
	:t10 
		cmp		edx, uvector-clos-instance-tag
		jne		short :t7
		mov		edx, [eax + (uvector-offset cl::clos-instance-class-offset)]	;; edx = clos class
		mov		ebx, 'cl::the-class-standard-gf
		mov		ebx, [ebx + (uvector-offset cl::symbol-value-offset)]
		mov		ebx, [ebx - cons-tag]
		cmp		ebx, edx												;; class = the-class-standard-gf?
		jne		short :t7
		mov		eax, [eax + (uvector-offset cl::clos-instance-slots-offset)]	;; eax = clos class slots
		mov		eax, [eax + (uvector-offset (+ 2 cl::slot-location-generic-function-discriminating-function))]
		jmp 	short :t11
	:t7
		push	eax
		callp	_not-a-function-error
	:got-addr			;; eax = execution address
		mov		edx, 0
		mov		dl, [eax]
		cmp		dl, #xe9			;; watch for jump table entry: if so, resolve to real address
		jne		short :t8
		add		eax, [eax + 1]
		add		eax, 5
	:t8
		test	eax, #xf0000000
		jne		:bignum
		shl		eax, 3
		jmp		short :exit
	:bignum
		push	eax
		push	8
		mov		ecx, 1
		callf	cl::alloc-bignum		;; allocate 1 cell
		add		esp, 4
		pop		[eax + (uvector-offset cl::bignum-first-cell-offset)]
	:exit
		pop		ebx
		pop		edi
		mov		ecx, 1
		mov		esp, ebp
		pop		ebp
		ret
	})

(in-package :cl)


;;;
;;; Bootstrap
;;;

(progn  ; Extends to end-of-file (to avoid printing intermediate results).
;;(format t "Beginning to bootstrap Closette...")
(forget-all-classes)
(forget-all-generic-functions)
;; How to create the class hierarchy in 10 easy steps:
;; 1. Figure out standard-class's slots.
(setq the-slots-of-standard-class
      (mapcar #'(lambda (slotd)
                  (make-effective-slot-definition
                    :name (car slotd)
                    :initargs
                      (let ((a (getf (cdr slotd) ':initarg)))
                        (if a (list a) ()))
                    :initform (getf (cdr slotd) ':initform)
                    :initfunction
                      (let ((a (getf (cdr slotd) ':initform)))
                        (if a #'(lambda () (eval a)) nil))
                    :allocation ':instance))
              (nth 3 the-defclass-standard-class)))
;; 2. Create the standard-class metaobject by hand.
(setq the-class-standard-class
      (allocate-std-instance
         'tba
         (make-array (length the-slots-of-standard-class)
                     :initial-element secret-unbound-value)))
;; 3. Install standard-class's (circular) class-of link. 
(setf (std-instance-class the-class-standard-class) 
      the-class-standard-class)
;; (It's now okay to use class-... accessor).
;; 4. Fill in standard-class's class-slots.
(setf (class-slots the-class-standard-class) the-slots-of-standard-class)
;; (Skeleton built; it's now okay to call make-instance-standard-class.)
;; 5. Hand build the class t so that it has no direct superclasses.
(setf (find-class 't) 
  (let ((class (std-allocate-instance the-class-standard-class)))
    (setf (class-name class) 't)
    (setf (class-direct-subclasses class) ())
    (setf (class-direct-superclasses class) ())
    (setf (class-direct-methods class) ())
    (setf (class-direct-slots class) ())
    (setf (class-precedence-list class) (list class))
    (setf (class-slots class) ())
    (setf (class-shared-slots class) ())
    class))
;; (It's now okay to define subclasses of t.)
;; 6. Create the other superclass of standard-class (i.e., standard-object).
(defclass standard-object (t) ())
;; 7. Define the full-blown version of standard-class.
(setq the-class-standard-class (eval the-defclass-standard-class))

(declare-type-specifier standard-object (s1 s2)
                      (and (cl::clos-instance-p s1)
                           (cl::subclassp (class-of s1) (find-class s2))))

(declare-type-specifier standard-class (s1 s2)
                      (and (cl::clos-instance-p s1)
                           (cl::subclassp (class-of s1) (find-class s2))))

(declare-type-specifier class (s1 s2)
                      (declare (ignore s2))
                      (typep s1 'standard-class))

(declare-type-specifier standard-generic-function (s1 s2)
                      (and (cl::clos-instance-p s1)
                           (cl::subclassp (class-of s1) (find-class s2))))

(declare-type-specifier generic-function (s1 s2)
                      (and (cl::clos-instance-p s1)
                           (cl::subclassp (class-of s1) (find-class s2))))

(declare-type-specifier standard-method (s1 s2)
                      (and (cl::clos-instance-p s1)
                           (cl::subclassp (class-of s1) (find-class s2))))

(declare-type-specifier method (s1 s2)
                      (and (cl::clos-instance-p s1)
                           (cl::subclassp (class-of s1) (find-class s2))))

(declare-type-specifier method-combination (s1 s2)
                      (and (cl::clos-instance-p s1)
                           (cl::subclassp (class-of s1) (find-class s2))))

;; 8. Replace all (3) existing pointers to the skeleton with real one.
(setf (std-instance-class (find-class 't)) 
      the-class-standard-class)
(setf (std-instance-class (find-class 'standard-object)) 
      the-class-standard-class)
(setf (std-instance-class the-class-standard-class) 
      the-class-standard-class)
;; (Clear sailing from here on in).
;; 9. Define the other built-in classes.
(defclass symbol (t) ())
(defclass sequence (t) ())
(defclass array (t) ())
(defclass number (t) ())
(defclass character (t) ())
(defclass function (t) ())
;; (defclass hash-table (t) ()) defined below as a structure
(defclass package (t) ())
(defclass pathname (t) ())
(defclass readtable (t) ())
(defclass stream (t) ())
(defclass list (sequence) ())
(defclass null (symbol list) ())
(defclass cons (list) ())
(defclass vector (array sequence) ())
(defclass bit-vector (vector) ())
(defclass string (vector) ())
(defclass complex (number) ())
(defclass integer (number) ())
(defclass float (number) ())
(defclass ratio (number) ())

;; 10. Define the other standard metaobject classes.
(setq the-class-gf (eval the-defclass-generic-function))
(setq the-class-standard-gf (eval the-defclass-standard-generic-function))
(setq the-class-standard-method (eval the-defclass-standard-method))
;; Voila! The class hierarchy is in place.
;;(format t "Class hierarchy created.")
;; (It's now okay to define generic functions and methods.)

(defgeneric print-object (instance stream))
(defmethod print-object ((instance standard-object) stream)
  (print-unreadable-object (instance stream :identity t)
     (format stream "~:(~S~)"
                    (class-name (class-of instance))))
  instance)

;;; Slot access

(defgeneric slot-value-using-class (class instance slot-name))
(defmethod slot-value-using-class ((class standard-class) instance slot-name)
	(declare (ignore class))
	(std-slot-value instance slot-name))
	
(defgeneric (setf slot-value-using-class) (new-value class instance slot-name))
(defmethod (setf slot-value-using-class) 
	(new-value (class standard-class) instance slot-name)
	(declare (ignore class))
	(setf (std-slot-value instance slot-name) new-value))
	(values)) ;end progn

;;; N.B. To avoid making a forward reference to a (setf xxx) generic function:
(defun setf-slot-value-using-class (new-value class object slot-name)
  (setf (slot-value-using-class class object slot-name) new-value))

(defclass eql-specializer (standard-class) (object))

(defun eql-specializer-p (x) (eq (class-of x) #.(find-class 'eql-specializer)))

(progn

(defgeneric slot-exists-p-using-class (class instance slot-name))
(defmethod slot-exists-p-using-class
           ((class standard-class) instance slot-name)
	(declare (ignore class))
	(std-slot-exists-p instance slot-name))

(defgeneric slot-boundp-using-class (class instance slot-name))
(defmethod slot-boundp-using-class
           ((class standard-class) instance slot-name)
	(declare (ignore class))
	(std-slot-boundp instance slot-name))

(defgeneric slot-makunbound-using-class (class instance slot-name))
(defmethod slot-makunbound-using-class
           ((class standard-class) instance slot-name)
	(declare (ignore class))
	(std-slot-makunbound instance slot-name))

;;; Instance creation and initialization

(defgeneric allocate-instance (class))
(defmethod allocate-instance ((class standard-class))
  (std-allocate-instance class))

(defgeneric make-instance (class &key))
(defmethod make-instance ((class standard-class) &rest initargs)
  (let ((instance (allocate-instance class)))
    (apply #'initialize-instance instance initargs)
    instance))
(defmethod make-instance ((class symbol) &rest initargs)
  (apply #'make-instance (find-class class) initargs))

(defgeneric initialize-instance (instance &key))
(defmethod initialize-instance ((instance standard-object) &rest initargs)
  (apply #'shared-initialize instance t initargs))

(defgeneric reinitialize-instance (instance &key))
(defmethod reinitialize-instance
           ((instance standard-object) &rest initargs)
  (apply #'shared-initialize instance () initargs))

(defgeneric shared-initialize (instance slot-names &key))
(defmethod shared-initialize ((instance standard-object) 
                              slot-names &rest all-keys)
  (dolist (slot (class-slots (class-of instance)))
    (let ((slot-name (slot-definition-name slot)))
      (multiple-value-bind (init-key init-value foundp)
            (get-properties
              all-keys (slot-definition-initargs slot))
         (declare (ignore init-key))
         (if foundp
             (setf (slot-value instance slot-name) init-value)
             (when (and (not (slot-boundp instance slot-name))
                        (not (null (slot-definition-initfunction slot)))
                        (or (eq slot-names t)
                            (member slot-name slot-names)))
               (setf (slot-value instance slot-name)
                     (funcall (slot-definition-initfunction slot))))))))
  instance)

;;; change-class

(defgeneric change-class (instance new-class &key))
(defmethod change-class
           ((old-instance standard-object)
            (new-class standard-class)
            &rest initargs)
  (let ((new-instance (allocate-instance new-class)))
    (dolist (slot-name (mapcar #'slot-definition-name
                               (class-slots new-class)))
      (when (and (slot-exists-p old-instance slot-name)
                 (slot-boundp old-instance slot-name))
        (setf (slot-value new-instance slot-name) 
              (slot-value old-instance slot-name))))
    (rotatef (std-instance-slots new-instance) 
             (std-instance-slots old-instance))
    (rotatef (std-instance-class new-instance) 
             (std-instance-class old-instance))
    (apply #'update-instance-for-different-class
           new-instance old-instance initargs)
    old-instance))

(defmethod change-class
           ((instance standard-object) (new-class symbol) &rest initargs)
  (apply #'change-class instance (find-class new-class) initargs))

(defgeneric update-instance-for-different-class (old new &key))
(defmethod update-instance-for-different-class 
           ((old standard-object) (new standard-object) &rest initargs)
  (let ((added-slots 
          (remove-if #'(lambda (slot-name)
                         (slot-exists-p old slot-name))
                     (mapcar #'slot-definition-name
                             (class-slots (class-of new))))))
    (apply #'shared-initialize new added-slots initargs)))

;;;
;;;  Methods having to do with class metaobjects.
;;;

(defmethod print-object ((class standard-class) stream)
  (print-unreadable-object (class stream :identity t)
    (format stream "~:(~S~) ~S"
            (class-name (class-of class))
            (class-name class)))
  class)

(defmethod initialize-instance :after ((class standard-class) &rest args)
  (apply #'std-after-initialization-for-classes class args))

;;; Finalize inheritance

(defgeneric finalize-inheritance (class))
(defmethod finalize-inheritance ((class standard-class)) 
  (std-finalize-inheritance class)
  (values))

;;; Class precedence lists

(defgeneric compute-class-precedence-list (class))
(defmethod compute-class-precedence-list ((class standard-class))
  (std-compute-class-precedence-list class))

;;; Slot inheritance

(defgeneric compute-slots (class))
(defmethod compute-slots ((class standard-class)) 
  (std-compute-slots class))

(defgeneric compute-effective-slot-definition (class direct-slots))
(defmethod compute-effective-slot-definition
           ((class standard-class) direct-slots)
  (std-compute-effective-slot-definition class direct-slots))

;;;
;;; Methods having to do with generic function metaobjects.
;;;

(defmethod print-object ((gf standard-generic-function) stream)
  (print-unreadable-object (gf stream :identity t)
     (format stream "~:(~S~) ~S"
             (class-name (class-of gf)) 
             (generic-function-name gf)))
  gf)

(defmethod initialize-instance :after ((gf standard-generic-function) &key)
  (finalize-generic-function gf))

;;;
;;; Methods having to do with method metaobjects.
;;;

(defmethod print-object ((method standard-method) stream)
  (print-unreadable-object (method stream :identity t)
     (format stream "~:(~S~) ~S~{ ~S~} ~S"
                    (class-name (class-of method))
                    (generic-function-name
                      (method-generic-function method))
                    (method-qualifiers method)
                    (mapcar #'(lambda (x) (if (eql-specializer-p x) `(eql ,(class-name x)) (class-name x))) 
                            (method-specializers method))))
  method)

(defmethod initialize-instance :after ((method standard-method) &key)
  (setf (method-function method) (compute-method-function method)))

;;;
;;; Methods having to do with generic function invocation.
;;;

(defgeneric compute-discriminating-function (gf))
(defmethod compute-discriminating-function ((gf standard-generic-function))
  (std-compute-discriminating-function gf))

(defgeneric method-more-specific-p (gf method1 method2 required-classes))
(defmethod method-more-specific-p 
           ((gf standard-generic-function) method1 method2 required-classes)
  (std-method-more-specific-p gf method1 method2 required-classes))

(defgeneric compute-effective-method-function (gf methods))
(defmethod compute-effective-method-function
           ((gf standard-generic-function) methods)
  (std-compute-effective-method-function gf methods))

(defgeneric compute-method-function (method))
(defmethod compute-method-function ((method standard-method))
  (std-compute-method-function method))

;;; describe-object is a handy tool for enquiring minds:

(defgeneric describe-object (object stream))

(defmethod describe-object ((object standard-object) stream)
	(format stream "CLOS OBJECT:~%~?~?~?"
		"~4T~A:~20T~A~%" (list "printed representation" object)
		"~4T~A:~20T~A~%" (list "class" (class-of object))
		"~4T~A:~20T#x~X~%" (list "heap address" (%uvector-address object)))
	(dolist (sn (mapcar #'slot-definition-name
                      (class-slots (class-of object))))
		(format stream "~4T~A: ~:[not bound~;~S~]~%"
            (string-downcase (symbol-name sn)) 
            (slot-boundp object sn)
            (and (slot-boundp object sn)
                 (slot-value object sn))))
  (values))

(defmethod describe-object ((object t) stream)
  (cl:describe object stream)
  (values))

;;(format t "~%Closette is a Knights of the Lambda Calculus production.")

(values)) ;end progn

(defmacro with-slots (slot-entries instance-form &body forms)
	(let ((sym (gensym))
		  (vars '())
		  (slots '()))
		(unless (listp slot-entries)
			(error "Invalid WITH-SLOTS slot entry list: ~S" slot-entries))
		(dolist (varslot slot-entries)
			(typecase varslot
				(symbol
					(push varslot vars)
					(push varslot slots))
				(list
					(unless (= 2 (length varslot))
						(error "Invalid WITH-SLOTS slot entry: ~S" varslot))
					(push (first varslot) vars)
					(push (second varslot) slots))
				(t	(error "Invalid WITH-SLOTS slot entry: ~S" varslot))))
		`(LET ((,sym ,instance-form))
			(SYMBOL-MACROLET 
				,(mapcar #'(lambda (v s) `(,v (slot-value ,sym ',s)))
					(nreverse vars)
					(nreverse slots))
				,@forms))))
		
;;; redefine to add type support for TYPEP
(defmacro defclass (name direct-superclasses slot-definitions
                    &rest options)
	(let ((s1 (gensym))(s2 (gensym)))
	  `(prog1
			(ensure-class ',name
			     :direct-superclasses
			       ,(canonicalize-direct-superclasses direct-superclasses)
			     :direct-slots
			       ,(canonicalize-direct-slots slot-definitions)
			     :shared-slots
			       ,(canonicalize-shared-slots slot-definitions)
				 :shared-slot-initforms
				   ,(canonicalize-shared-slot-initforms slot-definitions)		
			     ,@(canonicalize-defclass-options options))
			(declare-type-specifier ,name (,s1 ,s2)
				(and (or (cl::clos-instance-p ,s1) (structurep ,s1))
					(cl::subclassp (class-of ,s1) (find-class ,s2)))))))

;;;
;;; setup parent class and metaclass for all structures
;;;
(defclass structure-class (cl::standard-class) ())
(defclass structure-object (t) () (:metaclass structure-class))

;;; initialize this variable to ensure structs which have already
;;; been defined have a class
(setf *default-struct-class* (find-class 'structure-object))

(defun create-named-class (name superclasses)
	(ensure-class name
			     :direct-superclasses (append superclasses (list (find-class 'structure-object)))
                 :metaclass 'structure-class
				 :direct-slots nil
				 :shared-slots nil))

(defun struct-template (struct-name)
	(get struct-name :struct-template))

(defun patch-clos (struct-name)
	(setf (elt (struct-template struct-name) 1) 
		(create-named-class struct-name nil)))
	
;; make sure the following common lisp structures (which are defined before
;; this module is loaded) have CLOS definitions
(patch-clos 'hash-table)
(patch-clos 'random-state)
(patch-clos 'byte)

;; this is internal only, but we will patch it just in case...
(patch-clos 'method-table)

;;; EQL specializer support

;;; Returns a CLOS class representing a type that is specific
;;;	for the object. Used in method dispatch to implement EQL 
;;;	specialisers.
(defun intern-eql-specializer (object &optional (intern-form object))
    (let* ((singleton (gethash object *clos-singleton-specializers*))
             (real (and singleton
                         (car (member intern-form (class-precedence-list singleton)
				      :test #'(lambda (x y) (and (eql-specializer-p y) (equal x (class-name y)))))))))
        (or real (let ((newsingle (make-instance #.(find-class 'eql-specializer) :name intern-form
						 :direct-superclasses (list (or singleton (class-of object))))))
                    (setf (slot-value newsingle 'object) object (gethash object *clos-singleton-specializers*) newsingle)))))

;; need to restore warning here
(setq *COMPILER-WARN-ON-UNDEFINED-FUNCTION* t)	;; restore warnings

;;;
;;; Common Lisp WITH-ACCESSORS macro.
;;;
(defmacro with-accessors (slot-entries instance-form &body forms)
	(let ((sym (gensym))
		  (vars '())
		  (slots '()))
		(unless (listp slot-entries)
			(error "Invalid WITH-ACCESSORS slot entry list: ~S" slot-entries))
		(dolist (varslot slot-entries)
			(typecase varslot
				(symbol
					(push varslot vars)
					(push varslot slots))
				(list
					(unless (= 2 (length varslot))
						(error "Invalid WITH-ACCESSORS slot entry: ~S" varslot))
					(push (first varslot) vars)
					(push (second varslot) slots))
				(t	(error "Invalid WITH-ACCESSORS slot entry: ~S" varslot))))
		`(LET ((,sym ,instance-form))
			(SYMBOL-MACROLET 
				,(mapcar #'(lambda (v s) `(,v (,s ,sym)))
					(nreverse vars)
					(nreverse slots))
				,@forms))))

;;;
;;; Handle STRUCTURE-derived classes
;;;
(defmethod print-object ((instance structure-object) stream)
  (let ((*standard-output* stream))
    (cl::write-builtin-object instance)))

;;;
;;; Handle all other Lisp data types
;;;
(defmethod print-object ((instance t) stream)
  (let ((*standard-output* stream))
    (cl::write-builtin-object instance)))

(defun has-print-object-method (obj)
    (let ((gf (find-generic-function 'print-object)))
        (> (length (cl::compute-applicable-methods-using-classes gf (list (class-of obj)))) 1)))

;;;
;;; Hook into WRITE so that PRINT-OBJECT may be overridden for builtin objects.
;;;
(defun write-lisp-object (object)
    (print-object object *standard-output*))





