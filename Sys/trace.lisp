;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		trace.lisp
;;;;	Contents:	Corman Lisp TRACE facility.
;;;;	History:	RGC  12/1/96  Created.
;;;;				RGC  12/16/98 Changed ~A to ~S in output of trace
;;;;							  parameters and return values.
;;;;				RGC  10/31/01 Modified to use *trace-output*.
;;;;                RGC  11/22/02 Modified TRACE to not add a trace function
;;;;                              if it is already being traced.
;;;;                RGC  04/07/08 Incorporated enhancements from G Neil Haven.
;;;;
;;;;

(in-package :common-lisp)

(defvar *trace-level* 0)
(defvar *traced-functions* NIL)

(defvar *trace-enabled* t "when nil, disables tracing")
(defvar *max-trace-level* 15 "trace depth bottoms out")

(defvar *untraceable-functions*
  '(lambda
    apply
    funcall
    eval
    macroexpand
    macroexpand-1
		)
  "Set of functions, when traced, that lead to problems")

(defun traced-function-p (func)
  "returns nil or the original function object for <func>"
  (cdr (member func cl::*traced-functions* :key #'car :test #'eq)))

(defmacro with-trace (tracep &rest decls-and-forms)
  "enables/disables tracing for execution of decls-and-forms"
  `(let* ((*trace-enabled* ,tracep))
    ,@decls-and-forms))

(defmacro with-trace-disabled (&rest decls-and-forms)
  "irrespective of value of *trace-enabled*, disables tracing"
  `(let* ((*trace-enabled* nil))
    ,@decls-and-forms))

(defmacro with-trace-enabled (&rest decls-and-forms)
  "irrespective of value of *trace-enabled*, enables tracing"
  `(let* ((*trace-enabled* t))
    ,@decls-and-forms))

(defun my-register-traced-function (function-name)
  (let* ((orig-function (symbol-function function-name)))
    (setf (symbol-function function-name)
          #'(lambda (&rest x)
							(if *trace-enabled*
									(with-trace-disabled
											(dotimes (i *trace-level*) (write #\Space :escape nil :stream *trace-output*))
										(format *trace-output* "=>(~A~{ ~S~})~%" function-name x)
										(force-output *trace-output*)
										(let* ((*trace-level* (+ *trace-level* 1))
													 (ret))
                      (setq ret (multiple-value-list
																		(with-trace (< *trace-level* *max-trace-level*)
																			(apply orig-function x))))
											(dotimes (i (- *trace-level* 1)) (write #\Space :escape nil :stream *trace-output*))
											(format *trace-output* "(~A~{ ~S~})=>~{ ~S~}~%" function-name x ret)
											(force-output *trace-output*)
											(apply #'values ret)))
									(apply orig-function x))))
    (setf *traced-functions* 
          (adjoin (cons function-name orig-function) *traced-functions*
                  :key #'car :test #'eq))))

(defun my-register-traced-generic-function (func-name)
  (let* ((gf (symbol-function func-name))
				 (methods (generic-function-methods gf))
				 (old-functions '()))
    (dolist (method methods)
      (let ((orig-func (cl::method-function method))
						(method-string (format nil "~A" method)))
				(push method old-functions)
				(push orig-func old-functions)
				(setf (cl::method-function method)
							#'(lambda (&rest x)
									(if *trace-enabled*
											(with-trace-disabled
													(dotimes (i *trace-level*) (write #\Space :escape nil :stream *trace-output*))
												(format *trace-output* "=>(~A~{ ~S~})~%" method-string (butlast x))
												(force-output *trace-output*)
												(let* ((*trace-level* (+ *trace-level* 1))
															 (ret))
                          (setq ret (multiple-value-list
																				(with-trace (< *trace-level* *max-trace-level*)
																					(apply orig-func x))))
													(dotimes (i (- *trace-level* 1)) (write #\Space :escape nil :stream *trace-output*))
													(format *trace-output* "(~A~{ ~S~})=>~{ ~S~}~%" method-string x ret)
													(force-output *trace-output*)
													(apply #'values ret)))
											(apply orig-func x))))))
    (setf *traced-functions* (adjoin (cons func-name (nreverse old-functions)) *traced-functions*
																				 :key #'car :test #'eq))
    (cl::clear-method-table (cl::classes-to-emf-table gf))))

(defun my-register-traced-macro (macro-name)
  (let* ((orig-macro-function (macro-function macro-name)))
    (setf (macro-function macro-name)
					#'(lambda (form &optional env)
              (if *trace-enabled*
									(with-trace-disabled
											(dotimes (i *trace-level*) (write #\Space :escape nil :stream *trace-output*))
										(format *trace-output* "=>(~A~{ ~S~})~%" macro-name (cdr form))
										(force-output *trace-output*)
										(let* ((*trace-level* (+ *trace-level* 1))
													 (ret))
                      (setq ret (multiple-value-list
																		(with-trace (< *trace-level* *max-trace-level*)
																			(funcall orig-macro-function form env))))
											(dotimes (i (- *trace-level* 1)) (write #\Space :escape nil :stream *trace-output*))
											(format *trace-output* "(~A~{ ~S~})=>~{ ~S~}~%" macro-name (cdr form) ret)
											(force-output *trace-output*)
											(apply #'values ret)))
									(funcall orig-macro-function form env))))
    (setf *traced-functions* 
					(adjoin (cons macro-name orig-macro-function) *traced-functions*
									:key #'car :test #'eq))))

(defun my-%register-traced-functions (funcs)
  (dolist (func funcs)
    (cond
      ((not (symbolp func))
       (error "Not a symbol: ~A" func))
      ((null func)
       (error "~A cannot be traced" func))
      ((special-operator-p func)
       (error "The special operator ~A cannot be traced" func))
      ((member func *untraceable-functions*)
       (error "~A cannot be traced" func))
      ((not (symbol-function func))
       (error "~A does not have a function, macro or method associated with it" func))
      ((member func *traced-functions* :key #'car :test #'eq)
       nil)
      ((macro-function func)
       (my-register-traced-macro func))
      ((cl::standard-generic-function-p (symbol-function func))
       (my-register-traced-generic-function func))
      (t
       (my-register-traced-function func))))
  ;; return list of traced functions
  (mapcar #'car *traced-functions*))

(defun my-unregister-traced-generic-function (func-name)
  (let* ((gf (symbol-function func-name))
	 (f (member func-name *traced-functions* :key #'car :test #'eq))
	 (saved-funcs (cdar f)))
    (when f
      (do* ((m saved-funcs (cddr m))
	    (method (car m) (car m))
	    (orig-func (cadr m) (cadr m)))
	   ((null m))
	(setf (cl::method-function method) orig-func))
      (setf *traced-functions* 
	    (remove func-name *traced-functions* :key #'car :test #'eq))
      (cl::clear-method-table (cl::classes-to-emf-table gf)))))

(defun my-unregister-traced-function (func-name)
  (let* ((saved-func (cdar (member func-name *traced-functions* :key #'car :test #'eq))))
    (when saved-func)
    (setf (symbol-function func-name) saved-func)
    (setf *traced-functions* 
	  (remove func-name *traced-functions* :key #'car :test #'eq))))       

(defun my-unregister-traced-macro (macro-name)
  (let* ((saved-macro (cdar (member macro-name *traced-functions* :key #'car :test #'eq))))
    (when saved-macro)
    (setf (macro-function macro-name) saved-macro)
    (setf *traced-functions* 
	  (remove macro-name *traced-functions* :key #'car :test #'eq))))
             
(defun my-%unregister-traced-functions (funcs)
  (dolist (func funcs)
    (cond
      ((not (symbolp func))
       (error "Not a symbol: ~A" func))
      ((null func)
       (error "~A cannot be traced or untraced" func))
      ((special-operator-p func)
       (error "The special operator ~A cannot be traced or untraced" func))
      ((member func *untraceable-functions*)
       (error "~A cannot be traced or untraced" func))
      ((not (symbol-function func))
       (error "~A does not have a function, macro or method associated with it" func))
      ((not (member func *traced-functions* :key #'car :test #'eq))
       nil)
      ((macro-function func)
       (my-unregister-traced-macro func))
      ((cl::standard-generic-function-p (symbol-function func))
       (my-unregister-traced-generic-function func))
      (t
       (my-unregister-traced-function func))))
  ;; return list of traced functions
  (mapcar #'car *traced-functions*))


;;;;
;;;;	Common Lisp TRACE macro
;;;;
(defmacro trace (&rest funcs)
	(with-trace-disabled
			`(with-trace-disabled (my-%register-traced-functions ',funcs))))

;;;;
;;;;	Common Lisp UNTRACE macro
;;;;
(defmacro untrace (&rest funcs)
  (with-trace-disabled
      (if (null funcs)
					(setf funcs (trace)))
    `(with-trace-disabled (my-%unregister-traced-functions ',funcs))))

;;;;
;;;;	Common Lisp ROOM function
;;;;
(defun room	()
	(gc 3)				;; flush all ephemeral heaps
	(format t "~%Total heap size: ~A bytes. Heap available: ~A bytes.~%"
			(heap-capacity) 
			(- (heap-capacity) (heap-currently-used)))
	(format t "Jump table size:	~A entries. Entries available: ~A.~%"
			(jump-table-capacity) 
			(- (jump-table-capacity) (jump-table-used)))
	(format t "Global symbol table size:	~A entries. Entries available: ~A.~%"
			(symbol-table-capacity) 
			(- (symbol-table-capacity) (symbol-table-used))))

(defun funcall-ignoring-errors (func &rest args)
	(ignore-errors (apply func args)))
