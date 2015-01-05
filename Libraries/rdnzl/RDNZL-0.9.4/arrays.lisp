;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RDNZL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/rdnzl_lisp/arrays.lisp,v 1.23 2006/01/31 15:16:56 edi Exp $

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

;;; Utility functions for arrays and enumerations

(in-package :rdnzl)

(enable-rdnzl-syntax)

(defmacro do-rdnzl-array ((var array-form &optional result) &body body)
  "ARRAY-FORM should be a form which evaluates to a CONTAINER
structure wrapping a .NET array of rank 1.  BODY will be evaluated
with VAR bound to each element of this array \(as a CONTAINER) in
turn.  Finally, the result of evaluating the form RESULT is returned."
  (with-unique-names (array length i)
    ;; this can later be optimized by iterating directly through an
    ;; FFI array so we don't have the expensive call to INVOKE on each
    ;; iteration - but we don't do that now
    `(let* ((,array ,array-form)
            (,length [%Length ,array]))
       (dotimes (,i ,length)
         (let ((,var (get-array-element ,array ,i)))
           ,@body))
       ,result)))

(defun aref* (array &rest subscripts)
  "Returns the element of the .NET array ARRAY \(a CONTAINER) with the
subscripts SUBSCRIPTS.  Similar to AREF."
  (let* ((element-type [%AssemblyQualifiedName [GetElementType [GetType array]]])
         (value (apply #`GetValue array subscripts)))
    (unbox (cast* value element-type))))

(defun (setf aref*) (new-value array &rest subscripts)
  "Sets the element of the .NET array ARRAY \(a CONTAINER) with the
subscripts SUBSCRIPTS to the new value NEW-VALUE.  Similar to \(SETF
AREF)."
  (apply #`SetValue array new-value subscripts)
  new-value)

(defun make-array-type (base-type dimensions)
  "Synthesizes a .NET array type with base type BASE-TYPE \(a
CONTAINER) and DIMENSIONS dimensions."
  (let* ((base-type-name (get-object-as-string base-type))
         (array-type-name (format nil "~A[~V,,,',A]~A" base-type-name (1- dimensions) ""
                                  (subseq [%AssemblyQualifiedName base-type]
                                          (length base-type-name)))))
    (make-type-from-name array-type-name)))

(defun list-to-rdnzl-array (list &optional (base-type (make-type-from-name "System.Object")))
  "Creates and returns a .NET array of base type BASE-TYPE \(a
CONTAINER or a string) and rank 1 with the elements from the Lisp list
LIST."
  (when (stringp base-type)
    (setq base-type (make-type-from-name (resolve-type-name base-type))))
  (let* ((length (length list))
         ;; this is equivalent to calling NEW (see import.lisp)
         (new-array (invoke-constructor (make-array-type base-type 1)
                                        length)))
    (loop for element in list
          for i from 0
          do (setf (aref* new-array i)
                     (ensure-container element)))
    new-array))

(defun rdnzl-array-to-list (array)
  "Converts a .NET array ARRAY of rank 1 to a Lisp list with the same
elements."
  (let (list)
    (do-rdnzl-array (element array)
      (push element list))
    (nreverse list)))

(defun enum-to-integer (enum)
  "Converts the .NET object ENUM of type System.Enum to a Lisp
integer.  This is a destructive operation on ENUM."
  (unbox (cast* enum "System.Int32")))

(defun integer-to-enum (number type)
  "Converts the Lisp integer NUMBER to a .NET System.Enum object of
type TYPE \(a string or a CONTAINER)."
  (when (stringp type)
    (setq type (make-type-from-name (resolve-type-name type))))
  (cast [System.Enum.ToObject type number]
        type))

(defun or-enums (&rest enums)
  "Combines several .NET objects of type System.Enum with a logical or
and returns the result.  All arguments must be of the same .NET type."
  (let ((type-name [%AssemblyQualifiedName [GetType (first enums)]]))
    (integer-to-enum
     (apply #'logior (mapcar #'enum-to-integer enums)) type-name)))

(disable-rdnzl-syntax)
