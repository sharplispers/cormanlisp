;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RDNZL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/rdnzl/port-acl.lisp,v 1.20 2010/05/18 10:54:28 edi Exp $

;;; Copyright (c) 2004-2010, Charles A. Cox, Dr. Edmund Weitz.  All rights reserved.

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

;;; AllegroCL-specific definitions

(in-package :rdnzl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :foreign))

;; This variable should really evaluate to ':fat-le, but the fat-le
;; external-format was left out of the Allegro CL distribution by
;; mistake.  A patch will be available, but a workaround is to use the
;; equivalent "rdnzl-fat" external-format, the definition of which is
;; included below.
(defparameter *wchar-external-format* '(e-crlf :rdnzl-fat))

;; Begin rdnzl-fat definition.
(in-package :excl)

(def-external-format :rdnzl-fat :nulls 2 :width 2)

(def-char-to-octets-macro :rdnzl-fat (char state
                                           &key put-next-octet external-format)
  (declare (ignore external-format state))
  `(let ((code (char-code ,char)))
     (,put-next-octet (ldb (byte 8 0) code))
     (,put-next-octet (ldb (byte 8 8) code))))

(def-octets-to-char-macro :rdnzl-fat (state-loc &key get-next-octet external-format
                                                     octets-count-loc unget-octets)
  (declare (ignore external-format state-loc unget-octets))
  `(code-char (+ ,get-next-octet
                 (progn (incf ,octets-count-loc)
                        (ash ,get-next-octet 8)))))

;; force auto-compilation.  Suppress the unnecessary notes.
(with-output-to-string (*system-messages*)
  (string-to-octets "foo" :external-format :rdnzl-fat))

(in-package :rdnzl)
;; End rdnzl-fat definition.

(defmacro ffi-register-module (path &optional (module-name path))
  "Loads a C library designated by PATH."
  (declare (ignore module-name))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (load ,path)))

(defgeneric ffi-pointer-p (object)
  (:documentation "Tests whether OBJECT is an FFI pointer."))

(defmethod ffi-pointer-p ((object ff:foreign-pointer))
  t)

(defmethod ffi-pointer-p ((object integer))
  t)

(defmethod ffi-pointer-p ((object t))
  nil)

(defgeneric ffi-null-pointer-p (pointer)
  (:documentation
   "Returns whether the FFI pointer POINTER is a null pointer."))

(defmethod ffi-null-pointer-p ((pointer (eql 0)))
  t)

(defmethod ffi-null-pointer-p ((pointer ff:foreign-pointer))
  (eql 0 (ff:foreign-pointer-address pointer)))

(defmethod ffi-null-pointer-p ((pointer t))
  nil)

(defgeneric ffi-pointer-address (pointer)
  (:documentation "Returns the address of the FFI pointer POINTER."))

(defmethod ffi-pointer-address ((pointer ff:foreign-pointer))
  (ff:foreign-pointer-address pointer))

(defmethod ffi-pointer-address ((pointer integer))
  pointer)

(defun ffi-make-pointer (name)
  "Returns an FFI pointer to the address specified by the name NAME.
Allegro CL Note:  Use only for foreign-callable symbols."
  (ff:register-foreign-callable name :reuse t))

(defun ffi-make-null-pointer ()
  "Returns an FFI NULL pointer."
  0)

(defun ffi-map-type (type-name)
  "Maps type names like FFI-INTEGER to their corresponding names in
the Allegro CL FLI."
  (ecase type-name
    (ffi-void '(:void))
    (ffi-void-pointer '((* :void)))
    (ffi-const-string '((* :void)))
    (ffi-integer '(:int))
    (ffi-boolean '(:int boolean))
    (ffi-wide-char '(:unsigned-short))
    (ffi-float '(:float))
    (ffi-double '(:double))))

(excl:def-fwrapper wchar_t-retval (x)
  (code-char (excl:call-next-fwrapper)))

(defmacro ffi-define-function* ((lisp-name c-name)
                                arg-list
                                result-type)
  "Defines a Lisp function LISP-NAME which acts as an interface
to the C function C-NAME.  ARG-LIST is a list of \(NAME TYPE)
pairs.  All types are supposed to be symbols mappable by
FFI-MAP-TYPE above."
  (flet ((arg-spec (arg-list)
           (mapcar #'(lambda (name-and-type)
                       (destructuring-bind (name type) name-and-type
                         (cons name (ffi-map-type type))))
                   arg-list)))
    `(progn
       (ff:def-foreign-call (,lisp-name ,c-name) ,(arg-spec arg-list)
         :returning ,(ffi-map-type result-type)
         :strings-convert t
         :release-heap :when-ok
         :convention ':c)
       ,@(when (eq result-type 'ffi-wide-char)
           `((excl:fwrap ',lisp-name 'wchar_t-wrapper 'wchar_t-retval))))))

(defmacro ffi-define-callable ((c-name result-type)
                               arg-list
                               &body body)
  "Defines a Lisp which can be called from C.  ARG-LIST is a list
of \(NAME TYPE) pairs.  All types are supposed to be symbols
mappable by FFI-MAP-TYPE above."
  (declare (ignore result-type))
  `(progn
     (ff:defun-foreign-callable ,c-name
         ,(mapcar (lambda (name-and-type)
                    (destructuring-bind (name type) name-and-type
                      (list name (car (ffi-map-type type)))))
                  arg-list)
       ;; the following is overridden by Windows Allegro CL
       ;; (declare (:unwind nil))
       ,@body)
     (ff:register-foreign-callable ',c-name ':reuse t)))

(defmacro ffi-get-call-by-ref-string (function object length-function)
  "Calls the foreign function FUNCTION.  FUNCTION is supposed to call
a C function f with the signature void f\(..., __wchar_t *s) where s
is a result string which is returned by this macro.  OBJECT is the
first argument given to f.  Prior to calling f the length of the
result string s is obtained by evaluating \(LENGTH-FUNCTION OBJECT)."
  (with-rebinding (object)
    (with-unique-names (length temp)
      `(let ((,length (,length-function ,object)))
         (excl::with-dynamic-extent-usb8-array (,temp (* 2 (1+ ,length)))
           (,function ,object ,temp)
           (excl:octets-to-string
            ,temp
            :external-format *wchar-external-format*))))))

(defmacro ffi-call-with-foreign-string* (function string &optional other-args)
  "Applies the foreign function FUNCTION to the string STRING and
OTHER-ARGS.  OTHER-ARGS \(a list of CONTAINER structures or `native'
Lisp objects) is converted to a foreign array prior to calling
FUNCTION.  STRING may be NIL which means that this argument is skipped
\(i.e. the macro actually needs a better name)."
  (with-rebinding (other-args)
    (with-unique-names (length arg-pointers ffi-arg-pointers arg i arg-pointer
                               foreign-string)
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
                       `(excl:with-native-string
                            (,foreign-string ,string
                                             :external-format *wchar-external-format*)
                          (apply #',function ,foreign-string ,ffi-arg-pointers)))
                      (t
                       `(apply #',function ,ffi-arg-pointers))))
           ;; all .NET elements that were solely created (by BOX*)
           ;; for this FFI call are immediately freed
           (dotimes (,i ,length)
             (named-when (,arg-pointer (aref ,arg-pointers ,i))
               (%free-dot-net-container ,arg-pointer))))))))

(defconstant *ffi-args-size* 20)

(defmacro ffi-call-with-args* (function object name args)
  "Applies the foreign function FUNCTION to OBJECT and ARGS.  ARGS \(a
list of CONTAINER structures or `native' Lisp objects) is converted to
a foreign array prior to calling FUNCTION.  If NAME is not NIL, then
it should be a string and the first argument to FUNCTION will be the
corresponding foreign string."
  (with-rebinding (args)
    (with-unique-names (length arg-pointers ffi-arg-pointers arg i arg-pointer
                               foreign-name element-count byte-count)
      (declare (ignorable foreign-name element-count byte-count))
      ` (let* ((,length (length ,args))
               (,arg-pointers (make-array ,length :initial-element nil)))
          (unwind-protect
              (ff:with-stack-fobject (,ffi-arg-pointers
                                      '(:array (* :void) ,*ffi-args-size*))
                (when (> ,length ,*ffi-args-size*)
                  (error "Need more coding here..."))
                (loop for ,arg in ,args
                      for ,i from 0
                      for ,arg-pointer = (cond
                                           ((container-p ,arg) (pointer ,arg))
                                           (t (setf (aref ,arg-pointers ,i)
                                                      (box* ,arg))))
                      do (setf (ff:fslot-value ,ffi-arg-pointers ,i)
                                 ,arg-pointer))
                ,(cond (name
                        `(excl:with-native-string
                             (,foreign-name
                              ,name
                              :external-format *wchar-external-format*)
                           (,function ,foreign-name
                                      ,object
                                      ,length
                                      ,ffi-arg-pointers)))
                       (t
                        `(,function ,object
                                    ,length
                                    ,ffi-arg-pointers))))
            ;; all .NET elements that were solely created (by BOX*)
            ;; for this FFI call are immediately freed
            (dotimes (,i ,length)
              (named-when (,arg-pointer (aref ,arg-pointers ,i))
                          (%free-dot-net-container ,arg-pointer))))))))

(defun flag-for-finalization (object &optional function)
  "Mark OBJECT such that FUNCTION is applied to OBJECT before OBJECT
is removed by GC."
  (excl:schedule-finalization object function))

(defmacro register-exit-function (function &optional name)
  "Makes sure the function FUNCTION \(with no arguments) is called
before the Lisp images exits."
  (declare (ignore name))
  `(push
     ',(list 'funcall function)
     sys:*exit-cleanup-forms*))

(defun full-gc ()
  "Invokes a full garbage collection."
  (excl:gc t))
