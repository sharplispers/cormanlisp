;;;; Multiprocessing library for Corman Lisp - Version 1.2
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
;;;; An implementation of some multiprocessing code - loosely
;;;; striving to be compatible with CLOCC and/or Allegro CL.
;;;;
;;;; More recent versions of this software may be available at:
;;;;   http://www.double.nz/cl
;;;; 
;;;; This package works as-is on Windows NT 4.0. For Windows 98 some
;;;; patches to Corman Lisp 1.41 are required. See http://www.double.nz/cl
;;;; for details.
;;;;
;;;; Comments, suggestions and bug reports to the author, 
;;;; Christopher Double, at: chris@double.nz
;;;;
;;;; 16/09/2000 - 1.0 
;;;;              Initial release.
;;;;              Attempts to implement some of the Allegro Common Lisp
;;;;              multiprocessing stuff on top of the Corman Lisp threads
;;;;              module. 
;;;;              See http://www.franz.com/support/documentation/5.0.1/doc/cl/contents.htm#12
;;;;              for details on the Allegro multiprocessing package. So far
;;;;              everything mostly works but some features are not yet implemented.
;;;;
;;;; 20/09/2000 - 1.1 
;;;;              Implemented WITH-TIMEOUT macro and added locking in various places.
;;;;
;;;; 06/04/2001 - 1.2
;;;;              Implemented ACL process lock functions.
;;;;
(require 'threads)
(require 'mp-locks)

(defpackage "SYS"
	(:use :COMMON-LISP)
	(:export "*CURRENT-PROCESS*"))

(in-package :sys)

(defvar *current-process* nil)

(defpackage "MP"
	(:use 
		:COMMON-LISP 
		:THREADS
		:MP-LOCKS
		:C-TYPES)
	(:export
		"PROCESS"
		"PROCESS-THREAD-ID"
		"PROCESS-RUN-REASONS"
		"PROCESS-ARREST-REASONS"
		"PROCESS-INITIAL-BINDINGS"
		"PROCESS-FUNCTION"
		"PROCESS-ARGUMENTS"
		"PROCESS-NAME"
		"PROCESS-STATUS"
		"MAKE-PROCESS"
		"PROCESS-PRESET"
		"PROCESS-REVOKE-RUN-REASON"
		"PROCESS-ADD-RUN-REASON"
		"PROCESS-REVOKE-ARREST-REASON"
		"PROCESS-ADD-ARREST-REASON"
		"PROCESS-RUN-FUNCTION"
		"WITHOUT-SCHEDULING"
		"WITHOUT-INTERRUPTS"
		"PROC"
		"PROCESS-ENABLE"
		"PROCESS-DISABLE"
		"PROCESS-KILL"
		"PROCESS-ALLOW-SCHEDULE"
		"WITH-TIMEOUT"
		"PROCESS-LOCK"
		"MAKE-PROCESS-LOCK"
		"PROCESS-UNLOCK"
		"PROCESS-LOCK-LOCKER"
		"WITH-PROCESS-LOCK"
		))

(in-package :mp)

(defvar *active-process-table* (make-hash-table :synchronized t)
	"Contains a mapping between the thread id and the process CLOS object.")
(defvar *table-guard* (make-critical-section)
	"Criticial section to prevent corruption of the *active-process-table*.")
(defvar *system-lock* (make-critical-section)
    "Lock used when performing an action over all processes.")

(defun add-process-to-table (id proc)
	"Add a process to the active process table."
	(with-critical-section-lock (*table-guard*)
	  (setf (gethash id *active-process-table*) proc)))

(defun remove-process-from-table (id)
	"Remove a process from the active process table."
	(with-critical-section-lock (*table-guard*)
	  (remhash id *active-process-table*)))

(defclass process ()
	((thread-id :initform nil :initarg :id :reader process-thread-id)
		(run-reasons :initform nil :initarg :run-reasons)
		(arrest-reasons :initform nil :initarg :arrest-reasons)
		(initial-bindings :initform nil :initarg :initial-bindings :accessor process-initial-bindings)
		(function :initform nil :initarg :function :accessor process-function)
		(arguments :initform nil :initarg :arguments :accessor process-arguments)
		(status :initform :created :initarg :status :reader process-status)
		(name :initform nil :initarg :name :accessor process-name)
		(lock :initform (make-critical-section) :initarg :lock :accessor %process-lock)))

(defmacro %with-process-locked ((proc) &body body)
	"Acquire a lock on the process for performing internal actions."
	(let ((lock-name (gensym)))
	`(let ((,lock-name (%process-lock ,proc)))
		(with-critical-section-lock (,lock-name)
				,@body))))

(defmethod (setf process-thread-id) (value (proc process))
	(%with-process-locked (proc)
		(setf (slot-value proc 'thread-id) value)))

(defmethod (setf process-status) (value (proc process))
	(%with-process-locked (proc)
		(setf (slot-value proc 'status) value)))

(defun check-process-reasons (proc &optional force)
	"Checks the arrest and run reasons for the process and 
	suspends or resumes the process based on the results."
	(if (null (process-run-reasons proc))
		(suspend-process proc force)
	  (if (null (process-arrest-reasons proc))
		  (resume-process proc force)
		(suspend-process proc force))))

(defmethod process-run-reasons ((proc process))
	(slot-value proc 'run-reasons))

(defmethod process-arrest-reasons ((proc process))
	(slot-value proc 'arrest-reasons))

(defmethod (setf process-run-reasons) (value (proc process))
	(prog1
		(%with-process-locked (proc)
			(setf (slot-value proc 'run-reasons) value))
		(check-process-reasons proc)))

(defmethod (setf process-arrest-reasons) (value (proc process))
	(prog1
		(%with-process-locked (proc)
			(setf (slot-value proc 'arrest-reasons) value))
		(check-process-reasons proc)))
  
(defmethod print-object ((proc process) stream)
	(format stream "#<~a ~a>" 'process (process-name proc)))

(defun make-process (&key (name "Anonymous") 
		(reset-action ()) 
		(run-reasons ()) 
		(arrest-reasons ()) 
		(priority 0)
		(quantum 2) 
		resume-hook 
		suspend-hook 
		initial-bindings 
		message-interrupt-function
		stack-allocation 
		run-immediately)
	(declare (ignore quantum priority reset-action))
	(when (or resume-hook suspend-hook message-interrupt-function stack-allocation run-immediately)
		(error "MAKE-PROCESS keyword not implemented."))
	(when initial-bindings
		(warn "initial-bindings keyword to MAKE-PROCESS is not yet implemented."))
	(make-instance 'process
		:name name
		:initial-bindings initial-bindings
		:run-reasons run-reasons
		:arrest-reasons arrest-reasons))

(defmacro without-scheduling (&body body)
	"Suspends all threads and executes the body of code, resuming
	all threads when the body finished executing."
	`(with-critical-section-lock (*system-lock*)
		(do-processes
			#'(lambda (proc)
				(unless (eq proc sys:*current-process*)
					(process-add-arrest-reason proc :without-scheduling))))	   
		(unwind-protect
			(progn
				,@body)
			(do-processes
				#'(lambda (proc)
					(unless (eq proc sys:*current-process*)
						(process-revoke-arrest-reason proc :without-scheduling)))))))

(defmacro without-interrupts (&body body)
	"Currently does the same as WITHOUT-SCHEDULING. Not sure what it
	should do different..."
	`(without-scheduling
		,@body))

;; Warning - process-initial-bindings are currently ignored - not
;; sure how to handle them yet. 
(defun process-preset (proc initial-function &rest initial-args)
	(flet  ((thread-function ()
				(let ((sys:*current-process* proc))
					(setf (process-thread-id proc) th:*current-thread-id*)
					(add-process-to-table (process-thread-id proc) proc)
					(unwind-protect
						(progn
							(check-process-reasons proc t)
							(catch 'cl::%EXIT_THREAD_TAG
								(apply initial-function initial-args)))
						(remove-process-from-table (process-thread-id proc))
						(setf (process-thread-id proc) nil)
						(setf (process-status proc) :completed)))))
		(setf (process-function proc) initial-function)
		(setf (process-arguments proc) initial-args)
		(th:create-thread #'thread-function :report-when-finished nil)
		proc))

(defun process-revoke-run-reason (proc object)
	(setf (process-run-reasons proc) 
		(remove object (process-run-reasons proc))))

(defun process-revoke-arrest-reason (proc object)
	(setf (process-arrest-reasons proc)
		(remove object (process-arrest-reasons proc))))

(defun process-add-run-reason (proc object)
    (setf (process-run-reasons proc)
		(push object (process-run-reasons proc))))

(defun process-add-arrest-reason (proc object)
    (setf (process-arrest-reasons proc)
		(push object (process-arrest-reasons proc))))

(defun process-run-function (name-or-options preset-function &rest preset-arguments)
	(let ((proc (if (stringp name-or-options)
					(make-process :name name-or-options)
					(apply #'make-process name-or-options))))
		(apply #'process-preset proc preset-function preset-arguments)
		(process-enable proc)
		proc))
	
(defun suspend-process (proc &optional force)
	"Suspends the process if it is not already suspended. If FORCE
	is t then force a call to the suspend thread function even if
	the status of the thread is suspended."
	(let ((id (process-thread-id proc)))
		(when (and id (or force
					(eq (process-status proc) :resume)
					(eq (process-status proc) :killed)))
			(unless (eq (process-status proc) :killed)
				(setf (process-status proc) :suspend))
			(suspend-thread id))))

(defun resume-process (proc &optional force)
	"Resumes the process if it is not already resumed. If FORCE
	is t then force a call to the resume thread function even if
	the status of the thread is resumed."
	(let ((id (process-thread-id proc)))
		(when (and id (or force
					(eq (process-status proc) :suspend)
					(eq (process-status proc) :killed)))
			(unless (eq (process-status proc) :killed)
				(setf (process-status proc) :resume))
			(resume-thread id))))

(defun process-enable (proc)
	(setf (process-run-reasons proc) nil)
	(setf (process-arrest-reasons proc) nil)
	(process-add-run-reason proc :enable))
  
(defun process-disable (proc)
	(setf (process-run-reasons proc) nil)
	(setf (process-arrest-reasons proc) nil))

(setq cl::*compiler-warn-on-unused-variable* nil)

(defun do-processes (function)
	"Execute a function on every active process."
	(with-critical-section-lock (*table-guard*)
		(loop for value being the hash-value in *active-process-table* do
			(funcall function value))))

(setq cl::*compiler-warn-on-unused-variable* t)

(defun proc (&optional (stream t))
	"Display a list of active processes."
	(format stream "~&~30A ~12A ~7A ~7A ~10A~%"
		"Name"
		"Id"
		"Run"
		"Arrest"
		"Status")
	(format stream "~30A ~12A ~7A ~7A ~10A~%"
		"----"
		"--"
		"---"
		"------"
		"------")
	(do-processes
		#'(lambda (proc)
			(format stream "~30A ~12A ~7A ~7A ~10A~%"
				(process-name proc)
				(process-thread-id proc)
				(length (process-run-reasons proc))
				(length (process-arrest-reasons proc))
				(process-status proc)))))
  
(defun process-kill (proc)
	(let ((id (process-thread-id proc)))
		(when id
			(setf (process-status proc) :killed)  
			(th:terminate-thread id 0)
			(loop
				(let ((rt (th:resume-thread id)))
					(when (<= rt 1)
						(return)))))))

(defun process-allow-schedule (&optional proc)
	"Should this function restart threads suspended by WITHOUT-SCHEDULING?"
	(declare (ignore proc))
	(sleep 0))

(defparameter *timeout-reaper-count* 0
	"Incrementing count used in name of timeout-reaper threads.")

;; Uses terminate-thread to cancel body in main thread, catching
;; the terminate throw %EXIT_THREAD_TAG. This should be changed
;; to use its own tag so that it doesn't interfere with a real
;; terminate-thread. That requires changes in the C++ kernel
;; though (I think).
;; Timeout is not exactly to the correct seconds due to the
;; DOTIMES on 1 second sleeps. This is to allow the reaper thread
;; to be closed immediately when the terminate-thread is processed.
;; Otherwise it lives until the sleep exits (which could be a long
;; time).
(defmacro with-timeout ((seconds &body timeout-body) &body body)
	`(progn
		(let ((wait-process 
					(make-process 
						:name 
						(format nil 
							"timeout-reaper-~A" 
							(incf *timeout-reaper-count*))))
				(current-process sys:*current-process*)
				(result nil)
				(timeout-done nil)
				(main-body-done nil)
				(sync (make-critical-section)))
			(process-preset wait-process 
				#'(lambda ()
					(dotimes (n ,seconds)
						(declare (ignore n))
						(sleep 1)
						(when main-body-done
							(return)))
					(unless main-body-done
						(with-critical-section-lock (sync)
							(unless main-body-done
								(setq timeout-done t)
								(th:terminate-thread (process-thread-id current-process) 0))))))
			(unless (eq (catch 'cl::%EXIT_THREAD_TAG
						(process-enable wait-process)
						(progn
							(setq result (progn
									,@body))
							(unless timeout-done
								(with-critical-section-lock (sync)
									(unless timeout-done
										(setq main-body-done t)))))
						:no-timeout)
					:no-timeout)
				(setq main-body-done t)
				(setq result (progn
						,@timeout-body)))
			result)))

(defstruct (process-lock (:print-function print-process-lock))
	locker
	waiting
	name
	(cs (make-critical-section))
	whostate
	value)

(defun print-process-lock (lock stream indent)
	(declare (ignore indent))
	(format stream "#<~a ~a>"
		(type-of lock)
		(process-lock-name lock)))

(defun process-lock (lock &optional (lock-value sys:*current-process*) (whostate "Lock") timeout)
	(when timeout
		(warn "TIMEOUT keyword to PROCESS-LOCK not implemented"))
	(critical-section-enter (process-lock-cs lock))
	(setf (process-lock-whostate lock) whostate)
	(setf (process-lock-value lock) lock-value))

(defun process-unlock (lock &optional (lock-value sys:*current-process*))
	(when 
		(or
			(not (equal (process-lock-whostate lock) "Lock"))
			(not (equal (process-lock-value lock) lock-value)))
	;; RGC	(error "PROCESS-UNLOCK: lock-value different or not locked")
		    (warn "PROCESS-UNLOCK: lock-value different or not locked"))
	(critical-section-leave (process-lock-cs lock))
	(setf (process-lock-whostate lock) nil)
	(setf (process-lock-value lock) nil))

(defun process-lock-locker (lock)
	(process-lock-value lock))

(defmacro with-process-lock ((lock &key timeout norecursive seized whostate) &body body)
	(when (or timeout norecursive seized whostate)
		(warn "TIMEOUT, NORECURSIVE, SEIZED and WHOSTATE keyword to WITH-PROCESS-LOCK
			not implemented"))
	`(unwind-protect
		(progn
			(process-lock ,lock)
			,@body)
		(process-unlock ,lock)))
					
(when (null sys:*current-process*)
	(setq sys:*current-process* (make-process :name "main"))
	(setf (process-thread-id sys:*current-process*) th:*current-thread-id*)
	(setf (process-run-reasons sys:*current-process*) (list :main-process))
	(setf (process-status sys:*current-process*) :resume)
	(add-process-to-table th:*current-thread-id* sys:*current-process*))

(provide 'mp)

#|
(process-run-function "sleeper" #'(lambda () (sleep 30)))

(setq p2 (process-run-function "sleeper2" #'(lambda ()
									(dotimes (n 10)
									  (format t "~A~%" n)
									  (force-output)
									  (sleep 5)))))

(process-run-function 
	'(:name "sleeper"
		:initial-bindings '((*test 0)))
	#'(lambda () (sleep 30)))

(proc)

(time
 (with-timeout (10 (format t "timed out~%") (force-output) 5)
   (format t "in main~%")
   (force-output)
   (dotimes (n 5)
	 (sleep 1))
   (format t "out of main~%")
   (force-output) 4))
|#
	
	

	
	
