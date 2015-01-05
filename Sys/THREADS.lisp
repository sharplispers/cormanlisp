;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		threads.lisp
;;;;	Contents:	Thread support for Corman Lisp.
;;;;	History:	7/31/97  RGC  Created.
;;;;

(in-package :threads)
(import '(cl::with-synchronization 
        cl::allocate-critical-section 
        cl::enter-critical-section 
        cl::leave-critical-section
        cl::deallocate-critical-section))
(export '(
        thread-handle 
        suspend-thread 
        resume-thread 
        terminate-thread
		critical-section
        cs  
        initialize 
        enter 
        leave 
        destroy 
        cl::with-synchronization
        cl::allocate-critical-section 
        cl::enter-critical-section
        cl::leave-critical-section
        cl::deallocate-critical-section))

(define-condition exit-thread (condition)
	((return-value :initarg :return-value :accessor return-value :initform nil))
	(:report 
		(lambda (condition stream)
			(format stream "Thread ~D is exiting with return value ~S." 
				ccl:*current-thread-id*
				(return-value condition)))))
	
;;;
;;;	Corman Lisp CREATE-THREAD function.
;;; There are three ways the thread may be exited:
;;;		Normally, via RETURN.
;;;		Because of an error (including a system exception).
;;;		Because EXIT-THREAD condition was signalled.
;;;
(defun create-thread (func &key (report-when-finished t))
	(cl::create-thread
		(lambda ()
			(let ((result nil)
				  (stack-overflow nil)
				  (cl::*top-level* func)
				  (cl::*handler-registry* nil)
				  (cl::*restart-registry* nil))
				(declare (special cl::*top-level* 
						cl::*handler-registry* cl::*restart-registry*))
				(unwind-protect
					(restart-case
						(block call-thread-func
							(handler-bind 
								((win:stack-overflow 
									(lambda (condition) 
										(format *error-output* "~A~%" condition)
										(force-output *error-output*)
										(setf stack-overflow t)
										(return-from call-thread-func condition)))
								 (error 
									(lambda (condition)
										(if cl::*enable-error-trace*
											(let ((cl::*enable-error-trace* nil))
												(setf cl::*error-trace* (cl::stack-trace))))
										(format *error-output* "~%Error in thread ~D: ~A~%" 
												ccl:*current-thread-id* condition)
										(format *error-output* "Aborting the thread.~%")
										(force-output *error-output*)
										(return-from call-thread-func condition)))
								 (th:exit-thread 
									(lambda (condition)
										(setf result (multiple-value-list (return-value condition))) 
										(return-from call-thread-func))))
								(setf result (multiple-value-list (funcall func)))))
						(abort () :report 
								(lambda (stream)
									(format stream "Abort this thread (~D)." ccl:*current-thread-id*)) 
							(format *error-output* "Aborting thread ~D.~%" ccl:*current-thread-id*)
							(force-output *error-output*)
							(return-from create-thread))))
					(when report-when-finished
						(format *error-output* 
							"Thread ~A has terminated and returned value(s) ~S~%" 
							ccl:*current-thread-id* result)
						(force-output *error-output*))))))
	
;;;
;;;	Corman Lisp EXIT-THREAD function.
;;;
(defun exit-thread (return-value)
	(signal 'exit-thread :return-value return-value))

;;; Get the HANDLE associated with the lisp thread id.
;;; Returns NIL if the argument is not currently a valid
;;; lisp thread id.
;;;
(defun thread-handle (thread-id)
	(cl::thread-handle thread-id))

(in-package :win32)

;;(defconstant size-of-critical-section 24)	;; based on structure in winnt.h

#|
#! (:export t :library "KERNEL32" :ignore "APIENTRY" :pascal "WINAPI")
DWORD WINAPI ResumeThread(HANDLE);		
DWORD WINAPI SuspendThread(HANDLE);
BOOL  WINAPI TerminateThread(HANDLE,DWORD);
VOID  WINAPI InitializeCriticalSection(LPCRITICAL_SECTION lpCriticalSection);
VOID  WINAPI EnterCriticalSection(LPCRITICAL_SECTION lpCriticalSection);
VOID  WINAPI LeaveCriticalSection(LPCRITICAL_SECTION lpCriticalSection);
VOID  WINAPI DeleteCriticalSection(LPCRITICAL_SECTION lpCriticalSection);
!#
|#
(in-package :threads)
(defun suspend-thread (thread-id)
	(let ((handle (thread-handle thread-id)))
		(if handle
			(win:SuspendThread handle)
			(error "Not a valid lisp thread identifier: ~D" thread-id))))

(defun resume-thread (thread-id)
	(let ((handle (thread-handle thread-id)))
		(if handle
			(win:ResumeThread handle)
			(error "Not a valid lisp thread identifier: ~D" thread-id))))

(defun terminate-thread (thread-id result)
	(cl::terminate-thread thread-id result))

;;;
;;;	Critical section class
;;;
(defclass critical-section () 
	((cs :accessor cs :initform (cl::allocate-critical-section))))

(defmethod enter ((sec critical-section))
	(cl::enter-critical-section (cs sec)))

(defmethod leave ((sec critical-section))
	(cl::leave-critical-section (cs sec)))

#|
;;;
;;;	Corman Lisp OPEN-THREAD function
;;;
(defun open-thread (thread-id)
	(let ((handle (win:OpenThread win:THREAD_ALL_ACCESS nil thread-id)))
		(unless (ct:cpointer-null handle)
			handle)))
	
;;;
;;;	Corman Lisp SHOW-THREADS function
;;;
(defun show-threads (&optional (stream *standard-output*))
	(let ((thread-ids (cl::get-current-thread-ids)))
		(dolist (id thread-ids)
			;; not done
			)))
|#	

(provide "THREADS")

#|
;; test stuff
(defun doit ()
	(format t "~D: ~D~%" 
		*current-thread-id* 
		(get-internal-run-time))
	(force-output)
	(sleep 2))

(th:create-thread 
	#'(lambda () 
		(loop 
			(doit))))
(require :threads)
(setf c (make-instance 'th:critical-section))
(dotimes (i 3) 
	(th:create-thread 
		#'(lambda () 
			(format t "Thread ~D is waiting on the critical section.~%" *current-thread-id*)
			(force-output)
			(th:enter c)
			(format t "Thread ~D has entered the critical section.~%" *current-thread-id*)
			(force-output)
			(sleep 5)
			(format t "Thread ~D is leaving the critical section.~%" *current-thread-id*)
			(force-output)
			(th:leave c))))

|#
