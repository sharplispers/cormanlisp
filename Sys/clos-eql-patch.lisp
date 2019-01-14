;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		clos-eql-patch.lisp
;;;;	Contents:	CLOS support for EQL specializers
;;;;	History:	RGC 2/15/2001  Modified Chris Double's code.
;;;;

;;;;
;;;; Patch to Corman Lisp 1.41 to enable 
;;;; EQL specialisers with defmethod.
;;;;
;;;; Version 1.4
;;;; Created by Chris Double - chris@double.nz
;;;; Available from http://www.double.nz/cl
;;;;
;;;; This patch is in the public domain and may therefore
;;;; be used in any manner you wish. 
;;;;
;;;; This patch modifies the CLOS implementation in
;;;; Corman Lisp 1.41 to allow EQL specialisers to work.
;;;;
;;;; This change makes method dispatch very inefficient compared
;;;; to the unpatched Corman Lisp 1.41. See the end of this file
;;;; for details. As a result, you may only want to apply it in
;;;; the situations where you really need EQL specialisers.
;;;;
;;;; Notes
;;;; =====
;;;; For each object that EQL specialisation is used on, a 
;;;; new CLOS class is created that represents the most
;;;; specific type for that object. This is modelled on the
;;;; way singleton types work in the Dylan programming language.
;;;; These singleton types are stored in a hash table and will
;;;; not be garbage collected. It would be nice to have some way
;;;; of detecting when the EQL specialiser is no longer used
;;;; (perhaps when the method is removed, or redefined) and the
;;;; object could be removed from the hash table. Maybe weak types
;;;; could be used here somehow.
;;;;
;;;; 09/12/1999 - 1.0 
;;;;              Initial release.
;;;;
;;;; 10/12/1999 - 1.1
;;;;              Changed *SINGLETONS* hashtable to use EQL as the test.
;;;;
;;;; 12/12/1999 - 1.2
;;;;              Fixed problem where an EQL specialiser over a constant
;;;;              or variable would not work. See test at end for example.
;;;;
;;;; 02/03/2000 - 1.3
;;;;              Verified to work with Corman Lisp 1.4. No actual changes made.
;;;;              See end of file for example showing the overhead on generic 
;;;;              function dispatch this patch imposes.
;;;;
;;;; 14/07/2000 - 1.4
;;;;              Verified to work with Corman Lisp 1.41. No actual changes made.
;;;;              See end of file for example showing the overhead on generic 
;;;;              function dispatch this patch imposes.
;;;;
(in-package :common-lisp)

(defparameter *support-eql-specializers* nil) ;; turn this on to allow EQL specializers

(defconstant *singletons* (make-hash-table))
(defconstant *old-findclass* #'find-class)
(defconstant *old-class-of* #'class-of)

(defun get-singleton (object)
	"Return a CLOS class representing a type that is specific
	for the object. Used in method dispatch to implement EQL 
	specialisers."
	(or 
		(gethash object *singletons*)
		(setf (gethash object *singletons*)			
			(ensure-class (gensym)
				:direct-superclasses (list (class-of object))
				:direct-slots (list)))))

(defun find-class (symbol &optional (errorp t))
	(if (and *support-eql-specializers* (listp symbol) (eq (car symbol) 'eql))
		(let ((specialiser (car (cdr symbol))))
			(get-singleton
				(if (and (consp specialiser)
						(eq (car specialiser) 'quote))
					(car (cdr specialiser))
					(if (symbolp specialiser)
						(symbol-value specialiser)
						specialiser))))
		(funcall *old-findclass* symbol errorp)))

(defun class-of (object)
	(if *support-eql-specializers*
		(or
			(gethash object *singletons*)
			(funcall *old-class-of* object))
		(funcall *old-class-of* object)))

#|
(in-package :common-lisp-user)
(defclass base ())
(defclass derived (base))
(defgeneric doit (x))

(defmethod doit (x) 'default)
(defmethod doit ((x base)) 'base)
(defmethod doit ((x derived)) 'derived)
(defmethod doit ((x (eql 10))) 10)
(defmethod doit ((x (eql 'abcd))) 'abcd)
(defmethod doit ((x integer)) 'integer)
(defmethod doit ((x (eql 20))) (list 20 (call-next-method)))
(defmethod doit ((x (eql :a-key))) 'a-key)

(doit 'hello)
(doit (make-instance 'base))
(doit (make-instance 'derived))
(doit 10)
(doit 'abcd)
(doit 21)
(doit 20)
(doit :a-key)

;; Would not work in patch version 1.2 but does in version 1.3
(defconstant junk 25)
(defmethod doit ((x (eql junk)))
	25)
(defmethod doit ((x (eql 'junk)))
	'junk-symbol)

(doit 25)
(doit junk)
(doit 'junk)

|#

#|
(defclass base1 ())
(defclass derived1 (base1))
(defgeneric do-it (x))
(defmethod do-it ((x base1))
	x)
(defmethod do-it ((x derived1))
	(call-next-method)
	x)

(time
	(let ((a (make-instance 'base1))
			(b (make-instance 'derived1)))
		(dotimes (count 100000)
			(do-it a)
			(do-it b))))

;;; In Corman Lisp 1.4:
;;; Before this patch is applied, on a P2 333Mhz, 128MB ram the above
;;; test averages at 3.7 seconds.
;;; After this patch is applied the test averages 9.4 seconds. So it has
;;; significant overhead even if you don't use EQL dispatch.

;;; As a comparison, in Corman Lisp 1.3:
;;; Before this patch is applied, on a P2 333Mhz, 128MB ram the above
;;; test averages at 8.3 seconds.
;;; After this patch is applied, on a P2 333Mhz, 128MB ram the above
;;; test averages at 35 seconds.

|#
