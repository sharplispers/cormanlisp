;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		profiler.lisp
;;;;	Contents:	Corman Lisp profiler..
;;;;	History:	RGC  3/6/97  Created.
;;;;

(in-package :ccl)
(export '(profiling))

(defvar *profiled-functions* NIL)

(defun %register-profiled-functions (funcs)
	(if (null funcs)
		(return-from %register-profiled-functions (mapcar #'car *profiled-functions*)))

	(dolist (func funcs)
		(unless (symbolp func)
			(error "Not a symbol: ~A" func))
		(unless (symbol-function func)
			(error "The symbol ~A does not have a function associated with it" func))
		(setf *profiled-functions* 
			(adjoin (cons func (symbol-function func)) *profiled-functions*
				:key #'car :test #'eq))
		(let ((total-time 0)
			  (save-function (symbol-function func)))
			(setf (symbol-function func)
				#'(lambda (&rest x)
					(let* ((start-time (get-internal-run-time))
		   			   	   (ret (multiple-value-list (apply save-function x)))
						   (stop-time (get-internal-run-time)))
						(incf total-time (- stop-time start-time))
						(values-list ret))))))
	funcs) 

(defun %unregister-profiled-functions (funcs)
	(dolist (func funcs)
		(unless (symbolp func)
			(error "Not a symbol: ~A" func))
		(unless (symbol-function func)
			(error "The symbol ~A does not have a function associated with it" func))
		(let* ((saved-func (cdar (member func *profiled-functions* :key #'car :test #'eq))))
			(if (null saved-func)
				(error "The function ~A is not being profiled" func))
			(setf (symbol-function func) saved-func)
			(setf *profiled-functions* 
				(remove func *profiled-functions* :key #'car :test #'eq)))))

(defun profiled-function-timing-results (func)
	(let ((environment (cl::function-environment (symbol-function func))))
		(car (uref environment 2))))

(defun generate-profiled-results (funcs)
	(let ((results nil))
		(dolist (f funcs)
			(push f results)
			(push (profiled-function-timing-results f) results))
		(nreverse results)))

(defun report-profiled-results (results)
	(do* ((p results (cddr p))
		  (func (car p) (car p))
		  (timing (cadr p) (cadr p)))
		((null p))
		(format t "~A, ~A~%" func (/ (float timing) internal-time-units-per-second))))

;;;;
;;;;	Corman Lisp PROFILE macro
;;;;
(defmacro profile (&rest funcs)
	`(%register-profiled-functions ',funcs))

;;;;
;;;;	Corman Lisp UNPROFILE macro
;;;;
(defmacro unprofile (&rest funcs)
	(if (null funcs)
		(setf funcs (profile)))
	`(%unregister-profiled-functions ',funcs))

;;;;
;;;;	Corman Lisp PROFILING macro
;;;;
(defmacro profiling (funcs . body)
	(let ((funcs-sym (gensym))
		  (results-sym (gensym))
		  (timings-sym (gensym)))
		`(let ((,funcs-sym ',funcs)
			   (,results-sym nil)
			   (,timings-sym nil))
			(%register-profiled-functions ,funcs-sym)
			(setq ,results-sym (multiple-value-list (progn ,@body)))
			(setq ,timings-sym (generate-profiled-results ,funcs-sym))
			(%unregister-profiled-functions ,funcs-sym)
			(report-profiled-results ,timings-sym)
			(values-list ,results-sym))))

