;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RDNZL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/rdnzl_lisp/direct.lisp,v 1.9 2006/01/31 15:16:56 edi Exp $

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

;;; Interface for "direct calls" into .NET

(in-package :rdnzl)

(enable-rdnzl-syntax)

(defun find-interface-method (interfaces method-name arg-types binding-attr)
  "A Lisp version of findInterfaceMethod - see InvokeMember.cpp."
  (do-rdnzl-array (interface interfaces)
    (named-when (method-info [GetMethod interface method-name binding-attr
                                                  (make-null-object "System.Reflection.Binder")
                                                  arg-types
                                                  (make-null-object "System.Reflection.ParameterModifier[]")])
      (return-from find-interface-method method-info))
    (named-when (method-info
                 (find-interface-method [GetInterfaces interface] method-name arg-types binding-attr))
      (return-from find-interface-method method-info))))

(defun find-method* (type method-name arg-types binding-attr)
  "A Lisp version of findMethod - see InvokeMember.cpp."
  (or [GetMethod type method-name binding-attr
                      (make-null-object "System.Reflection.Binder")
                      arg-types
                      (make-null-object "System.Reflection.ParameterModifier[]")]
      (and [%IsInterface type]
           (or (find-interface-method [GetInterfaces type] method-name arg-types binding-attr)
               (find-method* (make-type-from-name "System.Object") method-name arg-types binding-attr)))))

(defun find-instance-method (method-name arg-type-names)
  "Finds and returns a MethodInfo object \(or NIL) corresponding to
the instance method with the name METHOD-NAME \(a string) and the
signature ARG-TYPE-NAMES \(a list of strings naming types).  Note that
the first element of ARG-TYPE-NAMES represents the type to which the
method belongs."
  (let ((arg-types (mapcar (lambda (arg-type-name)
                             (make-type-from-name
                              (resolve-type-name arg-type-name)))
                           arg-type-names)))
    (find-method* (first arg-types)
                  method-name
                  (list-to-rdnzl-array (rest arg-types)
                                       "System.Type")
                  (or-enums [$System.Reflection.BindingFlags.Instance]
                            [$System.Reflection.BindingFlags.Public]))))

(defun find-static-method (method-name type-name arg-type-names)
  "Finds and returns a MethodInfo object \(or NIL) corresponding to
the static method of the type named TYPE-NAME \(a string) with the
name METHOD-NAME \(a string) and the signature ARG-TYPE-NAMES \(a list
of strings naming types)."
  (let ((arg-types (mapcar (lambda (arg-type-name)
                             (make-type-from-name
                              (resolve-type-name arg-type-name)))
                           arg-type-names)))
    (find-method* (make-type-from-name (resolve-type-name type-name))
                  method-name
                  (list-to-rdnzl-array arg-types
                                       "System.Type")
                  (or-enums [$System.Reflection.BindingFlags.Static]
                            [$System.Reflection.BindingFlags.Public]))))

(defun find-property (type property-name arg-types binding-attr)
  "Finds a PropertyInfo object.  See corresponding code in
Property.cpp."
  [GetProperty type property-name binding-attr
                    (make-null-object "System.Reflection.Binder")
                    (make-null-object "System.Type")
                    arg-types
                    (make-null-object "System.Reflection.ParameterModifier[]")])

(defun find-instance-property (property-name arg-type-names)
  "Finds and returns a PropertyInfo object \(or NIL) corresponding to
the instance property with the name PROPERTY-NAME \(a string) and the
signature ARG-TYPE-NAMES \(a list of strings naming types).  Note that
the first element of ARG-TYPE-NAMES represents the type to which the
property belongs."
  (let ((arg-types (mapcar (lambda (arg-type-name)
                             (make-type-from-name
                              (resolve-type-name arg-type-name)))
                           arg-type-names)))
    (find-property (first arg-types)
                   property-name
                   (list-to-rdnzl-array (rest arg-types)
                                        "System.Type")
                   (or-enums [$System.Reflection.BindingFlags.Instance]
                             [$System.Reflection.BindingFlags.Public]))))

(defun find-static-property (property-name type-name arg-type-names)
  "Finds and returns a PropertyInfo object \(or NIL) corresponding to
the static property of the type named TYPE-NAME \(a string) with the
name PROPERTY-NAME \(a string) and the signature ARG-TYPE-NAMES \(a
list of strings naming types)."
  (let ((arg-types (mapcar (lambda (arg-type-name)
                             (make-type-from-name
                              (resolve-type-name arg-type-name)))
                           arg-type-names)))
    (find-property type-name
                   property-name
                   (list-to-rdnzl-array arg-types
                                        "System.Type")
                   (or-enums [$System.Reflection.BindingFlags.Static]
                             [$System.Reflection.BindingFlags.Public]))))

(defun find-field (type field-name binding-attr)
  "Finds a FieldInfo object.  See corresponding code in Field.cpp."
  [GetField type field-name binding-attr])

(defun find-instance-field (field-name type-name)
  "Finds and returns a FieldInfo object \(or NIL) corresponding to the
instance field with the name FIELD-NAME \(a string).  TYPE-NAME \(a
string) names the type to which the field belongs."
  (find-field (make-type-from-name (resolve-type-name type-name))
              field-name
              (or-enums [$System.Reflection.BindingFlags.Instance]
                        [$System.Reflection.BindingFlags.Public])))

(defun find-static-field (field-name type-name)
  "Finds and returns a FieldInfo object \(or NIL) corresponding to the
static field with the name FIELD-NAME \(a string).  TYPE-NAME \(a
string) names the type to which the field belongs."
  (find-field (make-type-from-name (resolve-type-name type-name))
              field-name
              (or-enums [$System.Reflection.BindingFlags.Static]
                        [$System.Reflection.BindingFlags.Public])))

(defmacro define-rdnzl-call (lisp-name
                             (&key (dotnet-name (unmangle-name lisp-name))
                                   type-name
                                   (member-kind :method)
                                   doc-string)
                             args)
  "Defines a Lisp function named by the function name LISP-NAME which
can directly \(without the need to search via Reflection) invoke a
.NET method, or get/set the value of a .NET property or field.
DOTNET-NAME is the name of the .NET member, TYPE-NAME is the name of a
.NET type and should only be supplied if a static member is to be
interfaced.  MEMBER-KIND if one of :METHOD, :PROPERTY, or :FIELD.
DOC-STRING is the documentation string of the resulting Lisp
function."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (create-direct-call
      ',lisp-name
      (setf (gethash ',lisp-name *direct-definitions*)
              (list ,member-kind ,dotnet-name ,type-name
                    (list ,@(loop for (nil arg-type-name) in args
                                  collect arg-type-name)))))
     (setf (documentation ',lisp-name 'function)
             ,(or doc-string
                  (format nil "~:[Instance~;Static~] ~A ~A of .NET type ~A
with Lisp lambda list (~{~A~^ ~})"
                          type-name
                          (ecase member-kind
                            ((:method) "method")
                            ((:property) "property")
                            ((:field) "field"))
                          dotnet-name
                          (or type-name (second (first args)))
                          (loop for (arg-name nil) in args
                                collect arg-name))))
     ',lisp-name))

(defun create-direct-call (lisp-name other-args)
  "Called by DEFINE-RDNZL-CALL \(and also by REDEFINE-DIRECT-CALLS) to
actually create the function definition for LISP-NAME based on the
necessary data \(which is simply a transformation of the arguments to
DEFINE-RDNZL-CALL) in OTHER-ARGS."
  (destructuring-bind (member-kind dotnet-name type-name arg-type-names)
      other-args
    (ecase member-kind
      ((:method)
       (cond (type-name
              (let ((method-info (find-static-method dotnet-name type-name arg-type-names)))
                (unless method-info
                  (error "Static method ~A(~{~A~^, ~}) for .NET type ~A not found"
                         dotnet-name arg-type-names type-name))
                (setf (fdefinition lisp-name)
                        (lambda (&rest args)
                          (ffi-call-with-args %invoke-static-member-directly
                                              method-info
                                              nil
                                              args)))))
             (t 
              (let ((method-info (find-instance-method dotnet-name arg-type-names)))
                (unless method-info
                  (error "Instance method ~A(~{~A~^, ~}) for .NET type ~A not found"
                         dotnet-name (rest arg-type-names) (first arg-type-names)))
                (setf (fdefinition lisp-name)
                        (lambda (&rest args)
                          (ffi-call-with-args %invoke-instance-member-directly
                                              method-info
                                              nil
                                              args)))))))
      ((:property)
       (cond (type-name
              (let ((property-info (find-static-property dotnet-name type-name arg-type-names)))
                (unless property-info
                  (error "Static property ~A(~{~A~^, ~}) for .NET type ~A not found"
                         dotnet-name arg-type-names type-name))
                (setf (fdefinition lisp-name)
                        (if (consp lisp-name)
                          (lambda (new-value &rest other-args)
                            (ffi-call-with-args %set-static-property-value-directly
                                                property-info
                                                nil
                                                (cons new-value other-args))
                            new-value)
                          (lambda (&rest args)
                            (ffi-call-with-args %get-static-property-value-directly
                                                property-info
                                                nil
                                                args))))))
             (t 
              (let ((property-info (find-instance-property dotnet-name arg-type-names)))
                (unless property-info
                  (error "Instance property ~A(~{~A~^, ~}) for .NET type ~A not found"
                         dotnet-name (rest arg-type-names) (first arg-type-names)))
                (setf (fdefinition lisp-name)
                        (if (consp lisp-name)
                          (lambda (new-value &rest other-args)
                            (ffi-call-with-args %set-instance-property-value-directly
                                                property-info
                                                nil
                                                (cons new-value other-args))
                            new-value)
                          (lambda (&rest args)
                            (ffi-call-with-args %get-instance-property-value-directly
                                                property-info
                                                nil
                                                args))))))))
      ((:field)
       (cond (type-name
              (let ((field-info (find-static-field dotnet-name type-name)))
                (unless field-info
                  (error "Static field ~A for .NET type ~A not found"
                         dotnet-name type-name))
                (setf (fdefinition lisp-name)
                        (if (consp lisp-name)
                          (lambda (new-value)
                            (ffi-call-with-foreign-string %set-static-field-value-directly
                                                          nil
                                                          field-info
                                                          new-value)
                            new-value)
                          (lambda ()
                            (ffi-call-with-foreign-string %get-static-field-value-directly
                                                          nil
                                                          field-info))))))
             (t
              (let ((field-info (find-instance-field dotnet-name (first arg-type-names))))
                (unless field-info
                  (error "Instance field ~A for .NET type ~A not found"
                         dotnet-name (first arg-type-names)))
                (setf (fdefinition lisp-name)
                        (if (consp lisp-name)
                          (lambda (new-value object)
                            (ffi-call-with-foreign-string %set-instance-field-value-directly
                                                          nil
                                                          field-info
                                                          object
                                                          new-value)
                            new-value)
                          (lambda (object)
                            (ffi-call-with-foreign-string %get-instance-field-value-directly
                                                          nil
                                                          field-info
                                                          object)))))))))))
                
(disable-rdnzl-syntax)
