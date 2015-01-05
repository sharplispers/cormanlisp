;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RDNZL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/rdnzl_lisp/port-sbcl.lisp,v 1.11 2006/02/01 12:01:10 edi Exp $

;;; Copyright (c) 2004-2006, Dr. Edmund Weitz.  All rights reserved.

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

;;; SBCL-specific definitions

(in-package :rdnzl)

(defconstant +ffi-pointer-size+
  #.(/ (sb-alien:alien-size sb-alien:system-area-pointer) 8)
  "The size of a pointer in octets.")

(defmacro ffi-register-module (path &optional (module-name path))
  "Loads a C library designated by PATH."
  (declare (ignore module-name))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (sb-alien:load-shared-object ,path)))

(defun ffi-pointer-p (object)
  "Tests whether OBJECT is an FFI pointer."
  (sb-sys:system-area-pointer-p object))

(defun ffi-null-pointer-p (pointer)
  "Returns whether the FFI pointer POINTER is a null pointer."
  (zerop (sb-sys:sap-int pointer)))

(defun ffi-pointer-address (pointer)
  "Returns the address of the FFI pointer POINTER."
  (sb-sys:sap-int pointer))

(defun ffi-map-type (type-name)
  "Maps type names like FFI-INTEGER to their corresponding names in
the LispWorks FLI."
  (ecase type-name
    (ffi-void 'sb-alien:void)
    (ffi-void-pointer 'sb-alien:system-area-pointer)
    (ffi-const-string 'sb-alien:system-area-pointer)
    (ffi-integer 'sb-alien:int)
    ;; only needed for WIDE-CHAR fake below
    (ffi-unsigned-short 'sb-alien:unsigned-short)
    (ffi-float 'sb-alien:single-float)
    (ffi-double 'sb-alien:double-float)))

(defmacro ffi-define-function* ((lisp-name c-name)
                                arg-list
                                result-type)
  "Defines a Lisp function LISP-NAME which acts as an interface
to the C function C-NAME.  ARG-LIST is a list of \(NAME TYPE)
pairs.  All types are supposed to be symbols mappable by
FFI-MAP-TYPE above."
  ;; there's a more elegant way to do this - see the code in
  ;; `port-clisp.lisp'
  (cond ((eq result-type 'ffi-boolean)
         (with-unique-names (inner-fn)
           `(progn
              (ffi-define-function* (,inner-fn ,c-name)
                                    ,arg-list
                                    ffi-integer)
              (defun ,lisp-name ,(mapcar #'first arg-list)
                (not (zerop (,inner-fn ,@(mapcar #'first arg-list))))))))
        ((eq result-type 'ffi-wide-char)
         (with-unique-names (inner-fn)
           `(progn
              (ffi-define-function* (,inner-fn ,c-name)
                                    ,arg-list
                                    ffi-unsigned-short)
              (defun ,lisp-name ,(mapcar #'first arg-list)
                (code-char (,inner-fn ,@(mapcar #'first arg-list)))))))
        ((find 'ffi-boolean arg-list :key #'second)
         (with-unique-names (inner-fn)
           `(progn
              (ffi-define-function* (,inner-fn ,c-name)
                                    ,(mapcar (lambda (name-and-type)
                                               (destructuring-bind (name type) name-and-type
                                                 (if (eq type 'ffi-boolean)
                                                   (list name 'ffi-integer)
                                                   name-and-type)))
                                             arg-list)
                                    ,result-type)
              (defun ,lisp-name ,(mapcar #'first arg-list)
                (,inner-fn ,@(mapcar (lambda (name-and-type)
                                       (destructuring-bind (name type) name-and-type
                                         (if (eq type 'ffi-boolean)
                                           `(if ,name 1 0)
                                           name)))
                                     arg-list))))))
        ((find 'ffi-wide-char arg-list :key #'second)
         (with-unique-names (inner-fn)
           `(progn
              (ffi-define-function* (,inner-fn ,c-name)
                                    ,(mapcar (lambda (name-and-type)
                                               (destructuring-bind (name type) name-and-type
                                                 (if (eq type 'ffi-wide-char)
                                                   (list name 'ffi-unsigned-short)
                                                   name-and-type)))
                                             arg-list)
                                    ,result-type)
              (defun ,lisp-name ,(mapcar #'first arg-list)
                (,inner-fn ,@(mapcar (lambda (name-and-type)
                                       (destructuring-bind (name type) name-and-type
                                         (if (eq type 'ffi-wide-char)
                                           `(char-code ,name)
                                           name)))
                                     arg-list))))))
        (t `(sb-alien:define-alien-routine
                (,c-name ,lisp-name) ,(ffi-map-type result-type)
              ,@(mapcar (lambda (name-and-type)
                          (destructuring-bind (name type) name-and-type
                            (list name (ffi-map-type type))))
                        arg-list)))))

(defvar *callbacks* (make-hash-table)
  "A hash table which maps symbols \(function names) to
callbacks.")

(defmacro ffi-define-callable ((c-name result-type)
                               arg-list
                               &body body)
  "Defines a Lisp function which can be called from C.  ARG-LIST
is a list of \(NAME TYPE) pairs.  All types are supposed to be
symbols mappable by FFI-MAP-TYPE above."
  `(setf (gethash ',c-name *callbacks*)
           (sb-alien:alien-sap
            (sb-alien::alien-lambda ,(ffi-map-type result-type)
                                    ,(mapcar (lambda (name-and-type)
                                               (destructuring-bind (name type) name-and-type
                                                 (list name (ffi-map-type type))))
                                             arg-list)
                                    ,@body))))

(defun ffi-make-pointer (name)
  "Returns an FFI pointer to the \(callback) address specified by
the name NAME."
  (gethash name *callbacks*))

(defun ffi-alloc (size)
  "Allocates an `alien' of size SIZE octets and returns a pointer
to it.  Must be freed with FFI-FREE afterwards."
  (sb-alien:alien-sap
   (sb-alien:make-alien (sb-alien:unsigned 8) size)))

(defun ffi-free (pointer)
  "Frees space that was allocated with FFI-ALLOC."
  (sb-alien:free-alien
   (sb-alien:sap-alien pointer (* (sb-alien:unsigned 8)))))

(defun ffi-convert-from-foreign-ucs-2-string (pointer size)
  "Converts the foreign UCS-2 string pointed to by POINTER of
size SIZE octets to a Lisp string."
  (with-output-to-string (out)
    (loop for i from 0 below size by 2
          do (write-char (code-char
                          (+ (sb-sys:sap-ref-8 pointer i)
                             (ash (sb-sys:sap-ref-8 pointer (1+ i)) 8)))
                         out))))

(defmacro ffi-get-call-by-ref-string (function object length-function)
  "Calls the foreign function FUNCTION.  FUNCTION is supposed to
call a C function f with the signature void f\(..., __wchar_t *s)
where s is a result string which is returned by this macro.
OBJECT is the first argument given to f.  Prior to calling f the
length of the result string s is obtained by evaluating
\(LENGTH-FUNCTION OBJECT)."
  (with-rebinding (object)
    (with-unique-names (length temp)
      `(let ((,length (* 2 (,length-function ,object)))
             ,temp)
        (unwind-protect
            (progn
              (setq ,temp (ffi-alloc (+ 2 ,length)))
              (,function ,object ,temp)
              (ffi-convert-from-foreign-ucs-2-string ,temp ,length))
          (when ,temp
            (ffi-free ,temp)))))))

(defmacro with-ucs-2-string ((var lisp-string) &body body)
  "Converts the Lisp string LISP-STRING to a foreign string using
UCS-2 encoding and evaluates BODY with VAR bound to this foreign
string."
  (with-unique-names (size char char-code i)
    `(let (,var)
       (unwind-protect
           (let ((,size (* 2 (length ,lisp-string))))
             (setq ,var (ffi-alloc (+ 2 ,size)))
             (loop for ,i from 0 by 2
                   for ,char across ,lisp-string
                   for ,char-code = (char-code ,char)
                   do (setf (sb-sys:sap-ref-8 ,var ,i) (ldb (byte 8 0) ,char-code)
                            (sb-sys:sap-ref-8 ,var (1+ ,i)) (ldb (byte 8 8) ,char-code)))
             (setf (sb-sys:sap-ref-8 ,var ,size) 0
                   (sb-sys:sap-ref-8 ,var (1+ ,size)) 0)
             ,@body)
         (when ,var
           (ffi-free ,var))))))

(defmacro ffi-call-with-foreign-string* (function string &optional other-args)
  "Applies the foreign function FUNCTION to the string STRING and
OTHER-ARGS.  OTHER-ARGS \(a list of CONTAINER structures or `native'
Lisp objects) is converted to a foreign array prior to calling
FUNCTION.  STRING may be NIL which means that this argument is skipped
\(i.e. the macro actually needs a better name)."
  (with-rebinding (other-args)
    (with-unique-names (length arg-pointers ffi-arg-pointers
                        arg i arg-pointer foreign-string)
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
                       `(with-ucs-2-string (,foreign-string ,string)
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
    (with-unique-names (length arg-pointers ffi-arg-pointers arg i j
                        arg-pointer foreign-name)
      (declare (ignorable foreign-name))
      `(let* ((,length (length ,args))
              (,arg-pointers (make-array ,length :initial-element nil))
              ,ffi-arg-pointers)
         (unwind-protect
             (progn
               (setq ,ffi-arg-pointers
                       (ffi-alloc
                        (* ,length +ffi-pointer-size+)))
               (loop for ,arg in ,args
                     for ,i from 0
                     for ,j from 0 by +ffi-pointer-size+
                     for ,arg-pointer = (cond
                                          ((container-p ,arg) (pointer ,arg))
                                          (t (setf (aref ,arg-pointers ,i)
                                                     (box* ,arg))))
                     do (setf (sb-sys:sap-ref-sap ,ffi-arg-pointers ,j)
                                ,arg-pointer))
               ,(cond (name
                       `(with-ucs-2-string (,foreign-name ,name)
                          (,function ,foreign-name
                                     ,object
                                     ,length
                                     ,ffi-arg-pointers)))
                      (t `(,function ,object
                                     ,length
                                     ,ffi-arg-pointers))))
           (when ,ffi-arg-pointers
             (ffi-free ,ffi-arg-pointers))
           ;; all .NET elements that were solely created (by BOX*)
           ;; for this FFI call are immediately freed
           (dotimes (,i ,length)
             (named-when (,arg-pointer (aref ,arg-pointers ,i))
               (%free-dot-net-container ,arg-pointer))))))))

(defun register-exit-function (function &optional name)
  "Makes sure the function FUNCTION \(with no arguments) is called
before the Lisp images exits."
  ;; don't know how to do that in SBCL
  (declare (ignore function name)))

(defun full-gc ()
  "Invokes a full garbage collection."
  (sb-ext:gc :full t))
