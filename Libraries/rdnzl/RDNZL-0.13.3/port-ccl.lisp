;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RDNZL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/rdnzl/port-ccl.lisp,v 1.31 2010/05/18 10:54:28 edi Exp $

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

;;; Corman-specific definitions

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
  (ct:cpointerp object))

(defun ffi-null-pointer-p (pointer)
  "Returns whether the FFI pointer POINTER is a null pointer."
  (ct:cpointer-null pointer))

(defun ffi-pointer-address (pointer)
  "Returns the address of the FFI pointer POINTER."
  (ct:cpointer-value pointer))

(defun ffi-make-pointer (name)
  "Returns an FFI pointer to the address specified by the name NAME."
  (ct:get-callback-procinst name))

(defun ffi-make-null-pointer ()
  "Returns an FFI NULL pointer."
  (ct:create-foreign-ptr))

(defun ffi-map-type (type-name)
  "Maps type names like FFI-INTEGER to their corresponding names in
the Corman Lisp FFI."
  (ecase type-name
    (ffi-void :void)
    (ffi-void-pointer '(:void *))
    (ffi-const-string '(:void *))
    (ffi-integer :long)
    (ffi-boolean :long-bool)
    (ffi-wide-char :unsigned-short)
    (ffi-float :single-float)
    (ffi-double :double-float)))

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
         ;; is a __wchar_t because Corman Lisp doesn't handle this
         ;; type automatically
         (with-unique-names (internal-name result)
           `(progn
              (ct:defun-dll ,internal-name
                  ,(mapcar (lambda (name-and-type)
                             (destructuring-bind (name type) name-and-type
                               (list name (ffi-map-type type))))
                           arg-list)
                :return-type ,(ffi-map-type result-type)
                :linkage-type :c
                :library-name ,*dll-path*
                :entry-name ,c-name)
              (defun ,lisp-name ,(mapcar #'first arg-list)
                (let ((,result (,internal-name ,@(loop for (name type) in arg-list
                                                       when (eq type 'ffi-wide-char)
                                                         collect `(char-code ,name)
                                                       else
                                                         collect name))))
                  ,(if (eq result-type 'ffi-wide-char)
                     ;; only use lower octet...
                     `(code-char (logand ,result 255))
                     result))))))
        (t
         `(ct:defun-dll ,lisp-name
              ,(mapcar (lambda (name-and-type)
                         (destructuring-bind (name type) name-and-type
                           (list name (ffi-map-type type))))
                       arg-list)
            :return-type ,(ffi-map-type result-type)
            :linkage-type :c
            :library-name ,*dll-path*
            :entry-name ,c-name))))

(defmacro ffi-define-callable ((c-name result-type)
                               arg-list
                               &body body)
  "Defines a Lisp function which can be called from C.  ARG-LIST
is a list of \(NAME TYPE) pairs.  All types are supposed to be
symbols mappable by FFI-MAP-TYPE above."
  (declare (ignore result-type))
  `(ct:defun-direct-c-callback ,c-name
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
      `(let ((,length (,length-function ,object))
             ,temp)
         (unwind-protect
             (progn
               (setq ,temp (ct:malloc (* 2 (1+ ,length))))
               (,function ,object ,temp)
               (copy-seq (ct:unicode-to-lisp-string ,temp)))
           (when ,temp
             (ct:free ,temp)))))))

(defmacro ffi-call-with-foreign-string* (function string &optional other-args)
  "Applies the foreign function FUNCTION to the string STRING and
OTHER-ARGS.  OTHER-ARGS \(a list of CONTAINER structures or `native'
Lisp objects) is converted to a foreign array prior to calling
FUNCTION.  STRING may be NIL which means that this argument is skipped
\(i.e. the macro actually needs a better name)."
  (with-rebinding (other-args)
    (with-unique-names (length arg-pointers ffi-arg-pointers arg i
                        arg-pointer foreign-string)
      ` (let* ((,length (length ,other-args))
               (,arg-pointers (make-array ,length :initial-element nil))
               ,foreign-string)
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
                        `(progn
                           (setq ,foreign-string (ct:lisp-string-to-unicode ,string))
                           (apply #',function ,foreign-string ,ffi-arg-pointers)))
                       (t
                        `(apply #',function ,ffi-arg-pointers))))
            (when ,foreign-string
              (ct:free ,foreign-string))
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
      ` (let* ((,length (length ,args))
               (,arg-pointers (make-array ,length :initial-element nil))
               ,ffi-arg-pointers
               ,foreign-name)
          (unwind-protect
              (progn
                (setq ,ffi-arg-pointers (ct:malloc (* ,length (ct:sizeof '(:void *)))))
                (loop for ,arg in ,args
                      for ,i from 0
                      for ,arg-pointer = (cond
                                           ((container-p ,arg) (pointer ,arg))
                                           (t (setf (aref ,arg-pointers ,i)
                                                      (box* ,arg))))
                      do (setf (ct:cref ((:void *) *) ,ffi-arg-pointers ,i)
                                 ,arg-pointer))
                ,(cond (name
                        `(progn
                           (setq ,foreign-name (ct:lisp-string-to-unicode ,name))
                           (,function ,foreign-name
                                      ,object
                                      ,length
                                      ,ffi-arg-pointers)))
                       (t
                        `(,function ,object
                                    ,length
                                    ,ffi-arg-pointers))))
            (when ,ffi-arg-pointers
              (ct:free ,ffi-arg-pointers))
            (when ,foreign-name
              (ct:free ,foreign-name))
            ;; all .NET elements that were solely created (by BOX*)
            ;; for this FFI call are immediately freed
            (dotimes (,i ,length)
              (named-when (,arg-pointer (aref ,arg-pointers ,i))
                (%free-dot-net-container ,arg-pointer))))))))

(defun flag-for-finalization (object &optional function)
  "Mark OBJECT such that FUNCTION is applied to OBJECT before OBJECT
is removed by GC."
  (ccl:register-finalization object function))

(defun register-exit-function (function &optional name)
  "Makes sure the function FUNCTION \(with no arguments) is called
before the Lisp images exits."
  ;; don't know how to do that in Corman Lisp
  (declare (ignore function name)))

(defun full-gc ()
  "Invokes a full garbage collection."
  (ccl:gc 3))

(defun lf-to-crlf (string)
  "Add #\Return before each #\Newline in STRING."
  (loop with new-string = (make-array (length string)
                                      :element-type 'character
                                      :fill-pointer 0)
        for c across string
        when (char= c #\Newline)
          do (vector-push-extend #\Return new-string)
        do (vector-push-extend c new-string)
        finally (return new-string)))

;; Corman's WITH-STANDARD-IO-SYNTAX doesn't work correctly so we fix
;; it here for our purposes

(defvar *standard-readtable* (copy-readtable nil))
(defvar *standard-pprint-dispatch* (copy-pprint-dispatch nil))

(defmacro with-standard-io-syntax (&body body)
  `(let ((*package* (find-package :user))
	 (*print-array* t)
	 (*print-base* 10)                                  
	 (*print-case* :upcase)
	 (*print-circle* nil)
	 (*print-escape* t)
	 (*print-gensym* t)
	 (*print-length* nil)
	 (*print-level* nil)
	 (*print-lines* nil)
	 (*print-miser-width* nil)
	 (*print-pprint-dispatch* *standard-pprint-dispatch*)
	 (*print-pretty* nil)
	 (*print-radix* nil)
	 (*print-readably* nil)
	 (*print-right-margin* nil)
	 (*read-base* 10)
	 (*read-default-float-format* 'single-float)
	 (*read-eval* t)
	 (*read-suppress* nil)
	 (*readtable* *standard-readtable*))
     ,@body))
