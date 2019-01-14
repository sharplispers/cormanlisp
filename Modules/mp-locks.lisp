;;;; Thread Lock library for Corman Lisp - Version 1.0
;;;;
;;;; Copyright (C) 2000 Christopher Double. All Rights Reserved.
;;;; 
;;;; License
;;;; =======
;;;; This software is provided 'as-is', without any express or implied
;;;; warranty. In no event will the author be held liable for any damages
;;;; arising from the use of this software.
;;;;
;;;; Permission is granted to anyone to use this software for any purpose,
;;;; including commercial applications, and to alter it and redistribute
;;;; it freely, subject to the following restrictions:
;;;;
;;;; 1. The origin of this software must not be misrepresented; you must
;;;;    not claim that you wrote the original software. If you use this
;;;;    software in a product, an acknowledgment in the product documentation
;;;;    would be appreciated but is not required.
;;;;
;;;; 2. Altered source versions must be plainly marked as such, and must
;;;;    not be misrepresented as being the original software.
;;;;
;;;; 3. This notice may not be removed or altered from any source 
;;;;    distribution.
;;;;
;;;; Notes
;;;; =====
;;;; An implementation of some locking code for multiprocessing.
;;;;
;;;; More recent versions of this software may be available at:
;;;;   http://www.double.nz/cl
;;;; 
;;;; Comments, suggestions and bug reports to the author, 
;;;; Christopher Double, at: chris@double.nz
;;;;
;;;; 16/09/2000 - 1.0 
;;;;              Initial release.
(defpackage "MP-LOCKS"
	(:use 
		:COMMON-LISP)
	(:export
		"CRITICAL-SECTION"
		"CRITICAL-SECTION-ENTER"
		"CRITICAL-SECTION-LEAVE"
		"WITH-CRITICAL-SECTION-LOCK"
		"MAKE-CRITICAL-SECTION"
		))

(in-package :mp-locks)

(defvar *cs-list* (make-array 10 :adjustable t :fill-pointer 0)
	"List of Critical Section objects")

(defvar *cs-list-lock* (cl::allocate-critical-section)
	"Lock for access to critical section list")

(defun add-critical-section (cs)
	"Add the given critical section object to the list of critical sections."
	(unwind-protect
		(progn
			(cl::enter-critical-section *cs-list-lock*)
			(block exit
				(loop for n from 0 below (length *cs-list*) do
				(when (null (aref *cs-list* n))
					(setf (aref *cs-list* n) (ccl:make-weak-pointer cs))
					(return-from exit)))
				(vector-push-extend (ccl:make-weak-pointer cs) *cs-list*)))
		(cl::leave-critical-section *cs-list-lock*)))

(defun remove-critical-section (cs)
	"Remove the critical section object from the list of critical sections."
	(unwind-protect
		(progn
			(cl::enter-critical-section *cs-list-lock*)
			(loop for n from 0 below (length *cs-list*) do
				(when (or
						(null (aref *cs-list* n))
						(eql cs (ccl:weak-pointer-obj (aref *cs-list* n))))
					(setf (aref *cs-list* n) nil))))
		(cl::leave-critical-section *cs-list-lock*)))

		
	
#|
;; Add methods for loading/saving critical section list lock.
(progn
	(cl::register-save-image-cleanup-func
		#'(lambda ()
			(cl::deallocate-critical-section *cs-list-lock*)
			(setf *cs-list-lock* nil)))
	(cl::register-load-image-restore-func
		#'(lambda ()
			(setf *cs-list-lock* (cl::allocate-critical-section)))))
|#
	
(defclass critical-section ()
	((handle :initform nil :accessor critical-section-handle))
	(:documentation
		"A critical section object"))

(defmethod print-object ((cs critical-section) stream)
	(format stream "#<~a ~a>"
		(type-of cs)
		(critical-section-handle cs)))

(defmethod initialize-instance :after ((cs critical-section) &allow-other-keys)	
	(setf (critical-section-handle cs) (cl::allocate-critical-section))
	(add-critical-section cs)
	(ccl:register-finalization cs #'(lambda (x) (remove-critical-section x))))		

(defmethod critical-section-enter ((cs critical-section))
	(cl::enter-critical-section (critical-section-handle cs)))

(defmethod critical-section-leave ((cs critical-section))
	(cl::leave-critical-section (critical-section-handle cs)))

(defmacro with-critical-section-lock ((cs) &body body)
	"Acquire and release a critical section around the given body of code."
	`(progn
		(critical-section-enter ,cs)
		(unwind-protect
			(progn
				,@body)
			(critical-section-leave ,cs))))

(defun make-critical-section ()
	"Return a newly constructed critical section"
	(make-instance 'critical-section))

(provide 'mp-locks)




	
