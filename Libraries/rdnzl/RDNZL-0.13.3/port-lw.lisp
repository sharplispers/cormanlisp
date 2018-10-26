;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RDNZL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/rdnzl/port-lw.lisp,v 1.44 2010/05/18 10:54:28 edi Exp $

;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.

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

;;; LispWorks-specific definitions

(in-package :rdnzl)

(defvar *module-name* nil
  "Holds the last module name defined by FFI-REGISTER-MODULE.
This is only needed for LispWorks.")

(defmacro ffi-register-module (path &optional (module-name path))
  "Loads a C library designated by PATH.  Optionally \(for
LispWorks) registers this library under the name MODULE-NAME."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (fli:register-module ,module-name
                          :real-name ,path)
     (setq *module-name* ,module-name)))

(defun ffi-pointer-p (object)
  "Tests whether OBJECT is an FFI pointer."
  (fli:pointerp object))

(defun ffi-null-pointer-p (pointer)
  "Returns whether the FFI pointer POINTER is a null pointer."
  (fli:null-pointer-p pointer))

(defun ffi-pointer-address (pointer)
  "Returns the address of the FFI pointer POINTER."
  (fli:pointer-address pointer))

(defun ffi-make-pointer (name)
  "Returns an FFI pointer to the address specified by the name NAME."
  (fli:make-pointer :symbol-name (symbol-name name)))

(defun ffi-make-null-pointer ()
  "Returns an FFI NULL pointer."
  fli:*null-pointer*)

(defun ffi-map-type (type-name)
  "Maps type names like FFI-INTEGER to their corresponding names in
the LispWorks FLI."
  (ecase type-name
    (ffi-void :void)
    (ffi-void-pointer :pointer)
    (ffi-const-string '(:reference-pass (:ef-wc-string
                                         :external-format :unicode)))
    (ffi-integer :int)
    (ffi-boolean :boolean)
    (ffi-wide-char :wchar-t)
    (ffi-float :lisp-float)
    (ffi-double :double)))

(defmacro ffi-define-function* ((lisp-name c-name)
                                arg-list
                                result-type)
  "Defines a Lisp function LISP-NAME which acts as an interface
to the C function C-NAME.  ARG-LIST is a list of \(NAME TYPE)
pairs.  All types are supposed to be symbols mappable by
FFI-MAP-TYPE above."
  `(fli:define-foreign-function
      (,lisp-name ,c-name)
      ,(mapcar (lambda (name-and-type)
                 (destructuring-bind (name type) name-and-type
                   (list name (ffi-map-type type))))
               arg-list)
    :result-type ,(ffi-map-type result-type)
    :calling-convention :cdecl
    :language :ansi-c
    ;; use the last module that was registered
    ,@(when *module-name*
        (list :module *module-name*))))

(defmacro ffi-define-callable ((c-name result-type)
                               arg-list
                               &body body)
  "Defines a Lisp function which can be called from C.  ARG-LIST
is a list of \(NAME TYPE) pairs.  All types are supposed to be
symbols mappable by FFI-MAP-TYPE above."
  `(fli:define-foreign-callable
       (,(symbol-name c-name) :result-type ,(ffi-map-type result-type)
                              :calling-convention :cdecl)
       ,(mapcar (lambda (name-and-type)
                  (destructuring-bind (name type) name-and-type
                    (list name (ffi-map-type type))))
                arg-list)
     ,@body))

(defmacro ffi-get-call-by-ref-string (function object length-function)
  "Calls the foreign function FUNCTION.  FUNCTION is supposed to call
a C function f with the signature void f\(..., __wchar_t *s) where s
is a result string which is returned by this macro.  OBJECT is the
first argument given to f.  Prior to calling f the length of the
result string s is obtained by evaluating \(LENGTH-FUNCTION OBJECT)."
  (with-rebinding (object)
    (with-unique-names (length temp)
      `(let ((,length (,length-function ,object)))
        (fli:with-dynamic-foreign-objects ()
          (let ((,temp (fli:allocate-dynamic-foreign-object :type :wchar-t
                                                            :nelems (1+ ,length))))
            (,function ,object ,temp)
            (fli:convert-from-foreign-string ,temp :external-format :unicode)))))))

(defmacro ffi-call-with-foreign-string* (function string &optional other-args)
  "Applies the foreign function FUNCTION to the string STRING and
OTHER-ARGS.  OTHER-ARGS \(a list of CONTAINER structures or `native'
Lisp objects) is converted to a foreign array prior to calling
FUNCTION.  STRING may be NIL which means that this argument is skipped
\(i.e. the macro actually needs a better name)."
  (with-rebinding (other-args)
    (with-unique-names (length arg-pointers ffi-arg-pointers arg i arg-pointer)
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
                       `(apply #',function ,string ,ffi-arg-pointers))
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
    (with-unique-names (length arg-pointers ffi-arg-pointers arg i arg-pointer)
      (declare (ignorable foreign-name element-count byte-count))
      ` (let* ((,length (length ,args))
               (,arg-pointers (make-array ,length :initial-element nil)))
          (unwind-protect
              (fli:with-dynamic-foreign-objects ()
                (let ((,ffi-arg-pointers (fli:allocate-dynamic-foreign-object :type :pointer
                                                                              :nelems ,length)))
                  (loop for ,arg in ,args
                        for ,i from 0
                        for ,arg-pointer = (cond
                                             ((container-p ,arg) (pointer ,arg))
                                             (t (setf (aref ,arg-pointers ,i)
                                                        (box* ,arg))))
                        do (setf (fli:dereference ,ffi-arg-pointers :index ,i)
                                   ,arg-pointer))
                  (,function ,@(if name (list name) nil)
                             ,object
                             ,length
                             ,ffi-arg-pointers)))
            ;; all .NET elements that were solely created (by BOX*)
            ;; for this FFI call are immediately freed
            (dotimes (,i ,length)
              (named-when (,arg-pointer (aref ,arg-pointers ,i))
                (%free-dot-net-container ,arg-pointer))))))))

;; register MAYBE-FREE-CONTAINER-POINTER as a finalization
;; function - needed for LispWorks
(hcl:add-special-free-action 'maybe-free-container-pointer)

(defun flag-for-finalization (object &optional function)
  "Mark OBJECT such that FUNCTION is applied to OBJECT before OBJECT
is removed by GC."
  ;; LispWorks can ignore FUNCTION because it was registered globally
  ;; above
  (declare (ignore function))
  (hcl:flag-special-free-action object))

(defvar *exit-function-registered* nil
  "Whether LW:DEFINE-ACTION was already called for DllForceTerm.")

(defmacro register-exit-function (function &optional name)
  "Makes sure the function FUNCTION \(with no arguments) is called
before the Lisp images exits."
  `(unless *exit-function-registered*
     (lw:define-action "When quitting image"
                       ,name ,function :once)
     (setq *exit-function-registered* t)))

(defun full-gc ()
  "Invokes a full garbage collection."
  (hcl:mark-and-sweep 3))

;; help the LispWorks IDE to find definitions
(dspec:define-form-parser ffi-define-function (c-name)
  `(,ffi-define-function ,(make-lisp-name c-name)))

(dspec:define-dspec-alias ffi-define-function (name)
  `(fli:define-foreign-function ,name))

(dspec:define-form-parser define-rdnzl-call (name)
  `(,define-rdnzl-call ,name))

(dspec:define-dspec-alias define-rdnzl-call (name)
  `(defun ,name))
