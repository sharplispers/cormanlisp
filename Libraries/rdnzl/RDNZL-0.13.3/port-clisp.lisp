;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RDNZL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/rdnzl/port-clisp.lisp,v 1.14 2010/05/18 10:54:28 edi Exp $

;;; Copyright (c) 2004-2010, Vasilis Margioulas, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; CLISP-specific definitions

(in-package :rdnzl)

(defvar *dll-path* nil
  "The name of RDNZL.dll.")

(defmacro ffi-register-module (dll-path &optional module-name)
  "Store the DLL name provided by the argument DLL-PATH."
  (declare (ignore module-name))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *dll-path* ,dll-path)))

(defun ffi-pointer-p (object)
  "Tests whether OBJECT is an FFI pointer."
  (eql (type-of object) 'ffi:foreign-address))

(defun ffi-null-pointer-p (pointer)
  "Returns whether the FFI pointer POINTER is a null pointer."
  (null pointer))

(defun ffi-pointer-address (pointer)
  "Returns the address of the FFI pointer POINTER."
  (ffi:foreign-address-unsigned pointer))

(defun ffi-make-pointer (name)
  "Returns an FFI pointer to the address specified by the name NAME."
  (get-function-pointer name))

(defun ffi-map-type (type-name)
  "Maps type names like FFI-INTEGER to their corresponding names in
the CLISP FFI."
  (ecase type-name
    (ffi-void nil)
    (ffi-void-pointer 'ffi:c-pointer)
    (ffi-const-string 'ffi:c-pointer)
    (ffi-integer 'ffi:int)
    (ffi-boolean 'ffi:boolean)
    (ffi-wide-char 'ffi:uint16)
    (ffi-float 'ffi:single-float)
    (ffi-double 'ffi:double-float)))
      
(defmacro ffi-define-function* ((lisp-name c-name)
                                arg-list
                                result-type)
  "Defines a Lisp function LISP-NAME which acts as an interface
to the C function C-NAME.  ARG-LIST is a list of \(NAME TYPE)
pairs.  All types are supposed to be symbols mappable by
FFI-MAP-TYPE above."
  (cond ((or (eq result-type 'ffi-wide-char)
             (find 'ffi-wide-char arg-list :key #'second :test #'eq))
         ;; define a wrapper if one of the args and/or the return type
         ;; is a __wchar_t because CLISP doesn't handle this
         ;; type automatically
         (with-unique-names (internal-name result)
           `(progn
              (ffi:def-call-out ,internal-name
		  (:name ,c-name)
		(:arguments ,@(mapcar (lambda (name-and-type)
				       (destructuring-bind (name type) name-and-type
					 (list name (ffi-map-type type))))
				     arg-list))
		,@(when (ffi-map-type result-type)
		    `((:return-type ,(ffi-map-type result-type))))
		(:language :stdc)
		(:library ,*dll-path*))
              (defun ,lisp-name ,(mapcar #'first arg-list)
                (let ((,result (,internal-name ,@(loop for (name type) in arg-list
                                                       when (eq type 'ffi-wide-char)
                                                         collect `(char-code ,name)
                                                       else
                                                         collect name))))
                  ,(if (eq result-type 'ffi-wide-char)
                     `(code-char ,result)
                     result))))))
        (t
         `(ffi:def-call-out ,lisp-name
	      (:name ,c-name)
	    (:arguments ,@(mapcar (lambda (name-and-type)
				    (destructuring-bind (name type) name-and-type
				      (list name (ffi-map-type type))))
				  arg-list))
	    ,@(when (ffi-map-type result-type)
		    `((:return-type ,(ffi-map-type result-type))))
	    (:language :stdc)
	    (:library ,*dll-path*)))))

(defgeneric get-function-pointer (name))

(defmacro ffi-define-callable ((c-name result-type)
                               arg-list
                               &body body)
  "Defines a Lisp function which can be called from C.
ARG-LIST is a list of \(NAME TYPE) pairs. All types are supposed
to be symbols mappable by FFI-MAP-TYPE above."
    (with-unique-names (foreign-function)
      `(progn
	 (defun ,c-name ,(mapcar #'first arg-list)
	   ,@body)
	     
	 (let ((,foreign-function (ffi:allocate-deep 
				   '(ffi:c-function
				     (:language :stdc-stdcall)
				     (:arguments ,@(mapcar (lambda (name-and-type)
							     (destructuring-bind (name type) name-and-type
							       (list name (ffi-map-type type))))
						    arg-list))
				     (:return-type ,(ffi-map-type result-type)))
				   nil)))

	   (defmethod get-function-pointer ((name (eql ',c-name)))
	     (ffi:with-c-place (f-function ,foreign-function)
	       (unless f-function 
		 (setf f-function #',c-name))
	       (ffi:foreign-address f-function)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-unicode-string ((var lisp-string) &body body)
    (with-unique-names (str-len ubyte16-array)
      `(let ((,str-len (length ,lisp-string)))
	 (ffi:with-c-var (,ubyte16-array `(ffi:c-array-max ffi:uint16 ,(1+ ,str-len))
					 (map 'vector #'char-code ,lisp-string))
	   (let ((,var (ffi:c-var-address ,ubyte16-array)))
	     ,@body))))))

(defun unicode-string-to-lisp (ubyte16-array)
  (map 'string #'code-char ubyte16-array))

(defmacro ffi-get-call-by-ref-string (function object length-function)
  "Calls the foreign function FUNCTION.  FUNCTION is supposed to call
a C function f with the signature void f\(..., __wchar_t *s) where s
is a result string which is returned by this macro.  OBJECT is the
first argument given to f.  Prior to calling f the length of the
result string s is obtained by evaluating \(LENGTH-FUNCTION OBJECT)."
  (with-rebinding (object)
    (with-unique-names (length temp)
      `(let ((,length (,length-function ,object)))
	 (ffi:with-c-var (,temp `(ffi:c-array-max ffi:uint16 ,(1+ ,length)) #())
	   (,function ,object (ffi:c-var-address ,temp))
	   (unicode-string-to-lisp ,temp))))))

(defmacro ffi-call-with-foreign-string* (function string &optional other-args)
  "Applies the foreign function FUNCTION to the string STRING and
OTHER-ARGS.  OTHER-ARGS \(a list of CONTAINER structures or `native'
Lisp objects) is converted to a foreign array prior to calling
FUNCTION.  STRING may be NIL which means that this argument is skipped
\(i.e. the macro actually needs a better name)."
  (with-rebinding (other-args)
    (with-unique-names (length arg-pointers ffi-arg-pointers arg i
                        arg-pointer foreign-string)
      (declare (ignorable foreign-string))
      `(let* ((,length (length ,other-args))
              (,arg-pointers (make-array ,length :initial-element nil)))
         (unwind-protect
             (let ((,ffi-arg-pointers
                     (loop for ,arg in ,other-args
                           for ,i from 0
                           for ,arg-pointer = (cond
                                                ((container-p ,arg) (pointer ,arg))
                                                (t (setf (aref ,arg-pointers ,i)
                                                           (box* ,arg))))
                           collect ,arg-pointer)))
               ,(cond (string
                       `(with-unicode-string (,foreign-string ,string)
                          (apply #',function ,foreign-string ,ffi-arg-pointers)))
                      (t
                       `(apply #',function ,ffi-arg-pointers))))
           ;; all .NET elements that were solely created (by BOX*)
           ;; for this FFI call are immediately freed
           (dotimes (,i ,length)
             (named-when (,arg-pointer (aref ,arg-pointers ,i))
               (%free-dot-net-container ,arg-pointer))))))))

(defmacro ffi-call-with-args* (function object name args)
  "Applies the foreign function FUNCTION to OBJECT and ARGS.  ARGS \(a
list of CONTAINER structures or `native' Lisp objects) is converted to
a foreign array prior to calling FUNCTION.  If NAME is not NIL, then
it should be a string and the first argument to FUNCTION will be the
corresponding foreign string."
  (with-rebinding (args)
    (with-unique-names (length arg-pointers ffi-arg-pointers arg i
                        arg-pointer foreign-name)
      `(let* ((,length (length ,args))
              (,arg-pointers (make-array ,length :initial-element nil)))
         (unwind-protect
             (progn
               (ffi:with-c-var 
                   (,ffi-arg-pointers `(ffi:c-array ffi:c-pointer ,,length)
		      (apply #'vector
                             (loop for ,arg in ,args
                                   for ,i from 0
                                   for ,arg-pointer = (cond
                                                        ((container-p ,arg) (pointer ,arg))
                                                        (t (setf (aref ,arg-pointers ,i)
                                                                   (box* ,arg))))
                                   collect ,arg-pointer)))
                 ,(cond (name
                         `(with-unicode-string (,foreign-name ,name)
                            (,function ,foreign-name
                                       ,object
                                       ,length
                                       (ffi:c-var-address ,ffi-arg-pointers))))
                        (t `(,function ,object
                                       ,length
                                       (ffi:c-var-address ,ffi-arg-pointers))))))
           ;; all .NET elements that were solely created (by BOX*)
           ;; for this FFI call are immediately freed
           (dotimes (,i ,length)
             (named-when (,arg-pointer (aref ,arg-pointers ,i))
               (%free-dot-net-container ,arg-pointer))))))))

(defun flag-for-finalization (object &optional function)
  "Mark OBJECT such that FUNCTION is applied to OBJECT before OBJECT
is removed by GC."  
  (ext:finalize object function))

(defun register-exit-function (function &optional name)
  "Makes sure the function FUNCTION \(with no arguments) is called
before the Lisp images exits."
  ;; don't know how to do that in CLISP
  (declare (ignore function name)))

(defun full-gc ()
  "Invokes a full garbage collection."
  (ext:gc))
