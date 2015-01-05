;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RDNZL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/rdnzl_lisp/container.lisp,v 1.46 2006/02/17 13:06:38 edi Exp $

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

;;; Definition of CONTAINER structure and various functions to deal
;;; with .NET objects.

(in-package :rdnzl)

(defstruct (container
            (:conc-name nil)
            ;; Corman Lisp doesn't know :PRINT-OBJECT
            (:print-function print-container))
  "Simple structure to wrap a pointer to a DotNetContainer object."
  (pointer nil :read-only t)
  (refp nil))

(defun print-container (container stream depth)
  "Prints an unreadable representation of a CONTAINER structure to the
stream STREAM."
  (declare (ignore depth))
  (print-unreadable-object (container stream :type t :identity nil)
    (let ((pointer (pointer container)))
      (unless (ffi-pointer-p pointer)
        (error "~S is not an FFI pointer" pointer))
      (format stream "~A #x~X"
              (if (%dot-net-container-is-null pointer)
                "NULL"
                ;; show name of type
                (get-type-name container))
              ;; show pointer address
              (ffi-pointer-address pointer))))
  container)

(define-condition rdnzl-error (simple-error)
  ((exception :initarg :exception
              :reader rdnzl-error-exception))
  (:report (lambda (condition stream)
             (format stream "~?"
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition))))
  (:documentation "An error of this type is signaled whenever an
exception occured during a call into .NET.  The EXCEPTION slot of this
error object holds a reference \(a CONTAINER) to the corresponding
.NET error object."))

(setf (documentation 'rdnzl-error-exception 'function)
        "Returns the .NET error object \(as a CONTAINER) which was
responsible for this error.")

(defun ref (object)
  "Makes a pass-by-reference type out of OBJECT and returns OBJECT.
If OBJECT is not a CONTAINER it'll be boxed first \(see BOX).  This
function makes only sense if OBJECT is used as an argument to INVOKE!"
  (cond ((container-p object)
         (%ref-dot-net-container-type (pointer object))
         (setf (refp object) t)
         object)
        (t
         (ref (box object)))))

(defun unref (container)
  "Resets CONTAINER to have the underlying type again.  Assumes that
REF was applied to CONTAINER before.  Returns CONTAINER."
  (%unref-dot-net-container-type (pointer container))
  (setf (refp container) nil)
  container)

(defmacro rdnzl-handler-case (form &rest clauses)
  "Like HANDLER-CASE but only for conditions of type RDNZL-ERROR.  The
typespecs are either strings \(naming a .NET error type) or of the
form \(OR string-1 ... string-n).  A :NO-ERROR clause is also
allowed."
  (with-unique-names (e exception)
   `(handler-case ,form
      (rdnzl-error (,e)
        (let ((,exception (rdnzl-error-exception ,e)))
          (cond
            ,@(loop for (typespec var-list . forms) in clauses
                    for exception-var = (or (first var-list) (gensym))
                    for typespec-list = (cond ((eq typespec :no-error) nil)
                                              ((stringp typespec)
                                               (list typespec))
                                              ((and (consp typespec)
                                                    (eq (first typespec) 'or))
                                               (rest typespec))
                                              (t (error "Illegal typespec ~S in RDNZL-HANDLER-CASE"
                                                        typespec)))
                    collect `((or ,@(mapcar (lambda (typespec)
                                              `(invoke (make-type-from-name (resolve-type-name ,typespec))
                                                       "IsAssignableFrom"
                                                       (invoke ,exception "GetType")))
                                            typespec-list))
                              (let ((,exception-var ,exception))
                                (declare (ignorable ,exception-var))
                                ,@forms)))
            (t (error ,e)))))
      ,@(let ((no-error-clause (find :no-error clauses
                                     :key #'first
                                     :test #'eq)))
          (and no-error-clause (list no-error-clause))))))
                     
(defun maybe-free-container-pointer (object)
  "This function is to be invoked whenever a CONTAINER structure is
finalized by the garbage collector."
  (when (container-p object)
    (%free-dot-net-container (pointer object))))

(defmacro wrap-with-container (form)
  "Evaluates FORM and wraps the result with a CONTAINER structure.
Also makes sure the corresponding DotNetContainer object is garbage
collected.  NIL is returned if FORM returns a NULL pointer."
  (with-unique-names (block-name container pointer)
    `(block ,block-name
       (let (,container ,pointer)
         (unwind-protect
             (progn
               (setq ,pointer ,form)
               (when (ffi-null-pointer-p ,pointer)
                 (warn "Returning NIL for NULL FFI pointer.")
                 (return-from ,block-name nil))
               (setq ,container
                       (make-container :pointer ,pointer))
               ,container)
           (when ,container
             #-:sbcl
             (flag-for-finalization ,container
                                    #'maybe-free-container-pointer)
             #+:sbcl
             (sb-ext:finalize ,container
                              (lambda ()
                                (%free-dot-net-container ,pointer)))))))))

(defun make-type-from-name (name)
  "Returns the .NET type with the name NAME - uses the static function
Type::GetType."
  (wrap-with-container
   (ffi-call-with-foreign-string* %make-type-from-name
                                  name)))

(defun get-object-as-string (container)
  "Get a string representation of the object denoted by CONTAINER.
Uses 'ToString' internally."
  (ffi-get-call-by-ref-string %get-dot-net-container-object-as-string
                              (pointer container)
                              %get-dot-net-container-object-string-length))

(defun get-type-name (container)
  "Get the name of the type of the object denoted by CONTAINER.  Uses
'FullName' internally."
  (ffi-get-call-by-ref-string %get-dot-net-container-type-as-string
                              (pointer container)
                              %get-dot-net-container-type-string-length))

(defun box* (object)
  "Like BOX but returns the raw pointer."
  (typecase object
    ((signed-byte 32)
     (%make-dot-net-container-from-int object))
    ((signed-byte 64)
     ;; this is due to a limitation of LispWorks: we have to pass the
     ;; argument as a string
     (ffi-call-with-foreign-string* %make-dot-net-container-from-long
                                    (with-standard-io-syntax ()
                                      (princ-to-string object))))
    (string
     (ffi-call-with-foreign-string* %make-dot-net-container-from-string object))
    (character
     (%make-dot-net-container-from-char object))
    (double-float
     (cond (*coerce-double-floats-to-single*
            (%make-dot-net-container-from-float object))
           (t
            (%make-dot-net-container-from-double object))))
    (float
     (%make-dot-net-container-from-float object))
    (pathname
     (box* (namestring object)))
    (boolean
     (%make-dot-net-container-from-boolean object))
    (otherwise
     (error "Don't know how to convert object ~S of type ~A to a .NET object."
            object (type-of object)))))

(defun box (object)
  "If object is a `native' Lisp object which we know how to convert
return a corresponding DotNetContainer object.  Otherwise raise an
error."
  (wrap-with-container (box* object)))

(defun ensure-container (object)
  "If OBJECT isn't already a CONTAINER then box it."
  (cond
    ((container-p object) object)
    (t (box object))))

(defun unbox (container)
  "If CONTAINER is of a known .NET type which we know how to convert
return the corresponding `native' Lisp object.  Otherwise just return
the container."
  (let ((type-name (get-type-name container)))
    (cond ((string= type-name "System.String")
           (get-object-as-string container))
          ((string= type-name "System.Char")
           (%get-dot-net-container-char-value (pointer container)))
          ((string= type-name "System.Int32")
           (%get-dot-net-container-int-value (pointer container)))
          ((string= type-name "System.Int64")
           (with-standard-io-syntax
             (read-from-string (get-object-as-string container))))
          ((string= type-name "System.Boolean")
           (%get-dot-net-container-boolean-value (pointer container)))
          ((string= type-name "System.Double")
           (%get-dot-net-container-double-value (pointer container)))
          ((string= type-name "System.Single")
           (%get-dot-net-container-single-value (pointer container)))
          (t container))))

(defmacro get-invocation-result (form)
  "Evaluates FORM which is supposed to return a pointer to an
InvocationResult object.  Tries to convert the result into a known
Lisp type, otherwise returns a CONTAINER structure."
  (with-unique-names (block-name invocation-result container)
    `(block ,block-name
       (let (,invocation-result ,container)
         (unwind-protect
             (progn
               (setq ,invocation-result ,form)
               (when (%invocation-result-is-void ,invocation-result)
                 ;; return keyword :VOID if the result was void
                 (return-from ,block-name :void))
               ;; first create a CONTAINER so we can be sure the
               ;; corresponding .NET object will be garbage-collected
               (setq ,container
                       (wrap-with-container
                        (%get-dot-net-container-from-invocation-result ,invocation-result)))
               (when (%invocation-result-is-exception ,invocation-result)
                 (error 'rdnzl-error
                        :exception ,container
                        :format-control ".NET error (~A): ~A"
                        :format-arguments (list (get-type-name ,container)
                                                (property ,container "Message")))))
           (when ,invocation-result
             ;; now free the InvocationResult object which wrapped the
             ;; result we were interested in
             (%free-invocation-result ,invocation-result)))
         (when (%dot-net-container-is-null (pointer ,container))
           (warn "Returning NULL object from .NET call")
           (return-from ,block-name (values nil t)))
         ;; try to convert some known types to native Lisp types
         (unbox ,container)))))

(defmacro ffi-call-with-foreign-string (function name &rest other-args)
  "Like FFI-CALL-WITH-FOREIGN-STRING* but handles the returned
InvocationResult object and accepts an arbitrary number of arguments
greater than one."
  `(get-invocation-result
    (ffi-call-with-foreign-string* ,function
                                   ,name
                                   (list ,@other-args))))

(defmacro ffi-call-with-args (function object name args)
  "Like FFI-CALL-WITH-ARGS* but OBJECT is assumed to be a CONTAINER
structure while each element of ARGS can be a native Lisp object or
such a structure.  The result of calling FUNCTION is assumed to be a
pointer to an InvocationResult which is handled by
GET-INVOCATION-RESULT."
  (with-rebinding (object)
    (with-unique-names (pointer)
      `(let ((,pointer (pointer ,object)))
         (when (%dot-net-container-is-null ,pointer)
           (error "Trying to call function ~S with NULL object ~S."
                  ',function ,object))
         (get-invocation-result
          (ffi-call-with-args* ,function
                               ,pointer
                               ,name
                               ,args))))))

(defun make-type-from-assembly-and-name (assembly name)
  "Returns the .NET type with the name NAME from a specific assembly."
  (ffi-call-with-args %invoke-instance-member
                      assembly "GetType" (list name)))

;; generic functions and TYPECASE are avoided below to make delivered
;; images smaller

(defun invoke (object method-name &rest args)
  "Invokes the method named METHOD-NAME \(a string).  If OBJECT
is a CONTAINER then the method is supposed to be an instance
method of this object.  If OBJECT is a string then the method is
supposed to be a static method of the type named OBJECT which
will be looked up using System.Type::GetType.  Otherwise, OBJECT
should be a two-element list where the first element is a
CONTAINER representing an assembly and the second element is a
string denoting a static method \(which will be looked up in that
specific assembly).  ARGS (either CONTAINER structures or Lisp
objects which can be converted) are the arguments to this
method."
  (let ((result
          (cond ((container-p object)
                 (ffi-call-with-args %invoke-instance-member
                                     object
                                     method-name
                                     args))
                ((stringp object)
                 (ffi-call-with-args %invoke-static-member
                                     (make-type-from-name (resolve-type-name object))
                                     method-name
                                     args))
                ((and (consp object)
                      (container-p (car object))
                      (stringp (cdr object)))
                 (ffi-call-with-args %invoke-static-member
                                     (make-type-from-assembly-and-name (car object) (cdr object))
                                     method-name
                                     args))
                (t (error "Don't know how to invoke ~A on ~S." method-name object)))))
    ;; if some of the arguments were pass-by-reference reset them to
    ;; their underlying types
    (dolist (arg args)
      (when (and (container-p arg)
                 (refp arg))
        (unref arg)))
    result))

(defun property (object property-name &rest args)
  "Returns the property named PROPERTY-NAME \(a string).  If OBJECT is
a CONTAINER then the property is supposed to be an instance property
of this object.  If OBJECT is a string then the property is supposed
to be a static property of the type named OBJECT.  ARGS (either
CONTAINER structures or Lisp objects which can be converted) are the
indexes to this property."
  (cond ((container-p object)
         (ffi-call-with-args %get-instance-property-value
                             object
                             property-name
                             args))
        ((stringp object)
         (ffi-call-with-args %get-static-property-value
                             (make-type-from-name (resolve-type-name object))
                             property-name
                             args))
        (t (error "Don't know how to get property ~A of ~S." property-name object))))

(defun (setf property) (new-value object property-name &rest args)
  "Sets the property named PROPERTY-NAME \(a string) to the new value
NEW-VALUE.  If OBJECT is a CONTAINER then the property is supposed to
be an instance property of this object.  If OBJECT is a string then
the property is supposed to be a static property of the type named
OBJECT.  ARGS (either CONTAINER structures or Lisp objects which can
be converted) are the indexes to this property."
  (cond ((container-p object)
         (ffi-call-with-args %set-instance-property-value
                             object
                             property-name
                             (cons new-value args)))
        ((stringp object)
         (ffi-call-with-args %set-static-property-value
                             (make-type-from-name (resolve-type-name object))
                             property-name
                             (cons new-value args)))
        (t (error "Don't know how to set property ~A of ~S." property-name object)))
  new-value)

(defun field (object field-name)
  "Returns the field named FIELD-NAME \(a string).  If OBJECT is a
CONTAINER then the field is supposed to be an instance field of this
object.  If OBJECT is a string then the field is supposed to be a
static field of the type named OBJECT."
  (cond ((container-p object)
         (ffi-call-with-foreign-string %get-instance-field-value
                                       field-name
                                       object))
        ((stringp object)
         (ffi-call-with-foreign-string %get-static-field-value
                                       field-name
                                       (make-type-from-name (resolve-type-name object))))
        (t (error "Don't know how to get field ~A of ~S." field-name object))))

(defun (setf field) (new-value object field-name)
  "Sets the field named FIELD-NAME \(a string) to the new value
NEW-VALUE.  If OBJECT is a CONTAINER then the field is supposed to be
an instance field of this object.  If OBJECT is a string then the
field is supposed to be a static field of the type named OBJECT."
  (cond ((container-p object)
         (ffi-call-with-foreign-string %set-instance-field-value
                                       field-name
                                       object
                                       new-value))
        ((stringp object)
         (ffi-call-with-foreign-string %set-static-field-value
                                       field-name
                                       (make-type-from-name (resolve-type-name object))
                                       new-value))
        (t (error "Don't know how to set field ~A of ~S." field-name object)))
  new-value)

(defun invoke-constructor (type &rest args)
  "Invokes the constructor \(corresponding to the signature determined
by ARGS) of the .NET type TYPE \(a CONTAINER).  ARGS (either CONTAINER
structures or Lisp objects which can be converted) are the arguments
to this constructor."
  (ffi-call-with-args %invoke-constructor
                      type
                      nil
                      args))

(defun get-array-element (array index)
  "Shortcut for fast access to elements of .NET arrays with rank 1.
Used only internally by DO-RDNZL-ARRAY."
  (get-invocation-result
   (%get-array-element (pointer array)
                       index)))

(defun cast* (container type-name)
  "Like CAST but doesn't try to resolve TYPE-NAME.  TYPE-NAME must be
a string."
  (ffi-call-with-foreign-string %set-dot-net-container-type-from-string
                                type-name
                                container)
  container)

(defun cast (container type)
  "Changes the type of the DotNetContainer object represented by
CONTAINER to TYPE \(a string or a CONTAINER).  Returns CONTAINER."
  (cast* container (cond ((stringp type)
                          (resolve-type-name type))
                         (t
                          (property type "AssemblyQualifiedName")))))

(defun make-null-object* (type-name)
  "Creates a NULL DotNetContainer with the type named by the string
TYPE-NAME."
  (wrap-with-container
   (ffi-call-with-foreign-string* %make-typed-null-dot-net-container
                                  type-name)))

(defun make-null-object (type-name)
  "Like MAKE-NULL-OBJECT* but resolves TYPE-NAME first."
  (make-null-object* (resolve-type-name type-name)))

(defun build-delegate-type (type-name return-type arg-type-array)
  "Build a subtype of DelegateAdapter \(see C++ code) with the
corresponding signature.  TYPE-NAME \(a string) will be the name of
the new type, the other two arguments are CONTAINERs."
  (wrap-with-container
   (ffi-call-with-foreign-string* %build-delegate-type
                                  type-name
                                  (list return-type
                                        arg-type-array))))