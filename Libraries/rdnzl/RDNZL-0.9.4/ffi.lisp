;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RDNZL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/rdnzl_lisp/ffi.lisp,v 1.30 2006/01/31 20:02:55 edi Exp $

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

;;; FFI definitions for all functions exported by RDNZL.dll.  See the
;;; C++ source code for details.

(in-package :rdnzl)

;; load the C++ library which interfaces with the CLR
(ffi-register-module "RDNZL.dll" :rdnzl)

(defmacro ffi-define-function (c-name arg-list result-type)
  "Like FFI-DEFINE-FUNCTION* but automatically creates the Lisp name
from the C name. A name like \"invokeMethod\" is mapped to
\"%INVOKE-METHOD\"."
  `(ffi-define-function* (,(intern
                            (concatenate 'string "%" (mangle-name c-name))
                            :rdnzl)
                          ,c-name)
                         ,arg-list
                         ,result-type))

(ffi-define-function "DllEnsureInit"
    ()
  ffi-void)

(ffi-define-function "DllForceTerm"
    ()
  ffi-void)

(defun dll-ensure-init ()
  "Wrapper for DllEnsureInit which makes sure the function is called
only once."
  (unless *dll-initialized*
    (%dll-ensure-init)
    (setq *dll-initialized* t)))

(defun dll-force-term ()
  "Wrapper for DllForceTerm which makes sure the function is only
called after DllEnsureInit has been called."
  (when *dll-initialized*
    (%dll-force-term)
    (setq *dll-initialized* nil)))

(ffi-define-function "invokeInstanceMember"
    ((method-name ffi-const-string)
     (target ffi-void-pointer)
     (nargs ffi-integer)
     (args ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-function "invokeInstanceMemberDirectly"
    ((method-info ffi-void-pointer)
     (nargs ffi-integer)
     (args ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-function "invokeStaticMember"
    ((method-name ffi-const-string)
     (type ffi-void-pointer)
     (nargs ffi-integer)
     (args ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-function "invokeStaticMemberDirectly"
    ((method-info ffi-void-pointer)
     (nargs ffi-integer)
     (args ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-function "getInstanceFieldValue"
    ((field-name ffi-const-string)
     (target ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-function "getStaticFieldValue"
    ((field-name ffi-const-string)
     (type ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-function "setInstanceFieldValue"
    ((field-name ffi-const-string)
     (target ffi-void-pointer)
     (new-value ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-function "setStaticFieldValue"
    ((field-name ffi-const-string)
     (type ffi-void-pointer)
     (new-value ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-function "getInstanceFieldValueDirectly"
    ((field-info ffi-void-pointer)
     (target ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-function "getStaticFieldValueDirectly"
    ((field-info ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-function "setInstanceFieldValueDirectly"
    ((field-info ffi-void-pointer)
     (target ffi-void-pointer)
     (new-value ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-function "setStaticFieldValueDirectly"
    ((field-info ffi-void-pointer)
     (new-value ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-function "getInstancePropertyValue"
    ((property-name ffi-const-string)
     (target ffi-void-pointer)
     (nargs ffi-integer)
     (args ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-function "setInstancePropertyValue"
    ((property-name ffi-const-string)
     (target ffi-void-pointer)
     (nargs ffi-integer)
     (args ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-function "getStaticPropertyValue"
    ((property-name ffi-const-string)
     (type ffi-void-pointer)
     (nargs ffi-integer)
     (args ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-function "setStaticPropertyValue"
    ((property-name ffi-const-string)
     (type ffi-void-pointer)
     (nargs ffi-integer)
     (args ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-function "getInstancePropertyValueDirectly"
    ((property-info ffi-void-pointer)
     (nargs ffi-integer)
     (args ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-function "setInstancePropertyValueDirectly"
    ((property-info ffi-void-pointer)
     (nargs ffi-integer)
     (args ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-function "getStaticPropertyValueDirectly"
    ((property-info ffi-void-pointer)
     (nargs ffi-integer)
     (args ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-function "setStaticPropertyValueDirectly"
    ((property-info ffi-void-pointer)
     (nargs ffi-integer)
     (args ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-function "refDotNetContainerType"
    ((ptr ffi-void-pointer))
  ffi-void)

(ffi-define-function "unrefDotNetContainerType"
    ((ptr ffi-void-pointer))
  ffi-void)

(ffi-define-function "freeDotNetContainer"
    ((ptr ffi-void-pointer))
  ffi-void)

(ffi-define-function "DotNetContainerIsNull"
    ((ptr ffi-void-pointer))
  ffi-boolean)

(ffi-define-function "makeTypedNullDotNetContainer"
    ((ptr ffi-const-string))
  ffi-void-pointer)

(ffi-define-function "InvocationResultIsVoid"
    ((ptr ffi-void-pointer))
  ffi-boolean)

(ffi-define-function "freeInvocationResult"
    ((ptr ffi-void-pointer))
  ffi-void)

(ffi-define-function "getDotNetContainerFromInvocationResult"
    ((ptr ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-function "getDotNetContainerTypeStringLength"
    ((ptr ffi-void-pointer))
  ffi-integer)

(ffi-define-function "getDotNetContainerTypeAsString"
    ((ptr ffi-void-pointer)
     (s ffi-void-pointer))
  ffi-void)

(ffi-define-function "setDotNetContainerTypeFromString"
    ((type ffi-const-string)
     (ptr ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-function "getDotNetContainerObjectStringLength"
    ((ptr ffi-void-pointer))
  ffi-integer)

(ffi-define-function "getDotNetContainerObjectAsString"
    ((ptr ffi-void-pointer)
     (s ffi-void-pointer))
  ffi-void)

(ffi-define-function "getDotNetContainerIntValue"
    ((ptr ffi-void-pointer))
  ffi-integer)

(ffi-define-function "getDotNetContainerCharValue"
    ((ptr ffi-void-pointer))
  ffi-wide-char)

(ffi-define-function "getDotNetContainerBooleanValue"
    ((ptr ffi-void-pointer))
  ffi-boolean)

(ffi-define-function "getDotNetContainerDoubleValue"
    ((ptr ffi-void-pointer))
  ffi-double)

(ffi-define-function "getDotNetContainerSingleValue"
    ((ptr ffi-void-pointer))
  ffi-float)

(ffi-define-function "makeTypeFromName"
    ((type ffi-const-string))
  ffi-void-pointer)

(ffi-define-function "makeDotNetContainerFromChar"
    ((c ffi-wide-char))
  ffi-void-pointer)

(ffi-define-function "makeDotNetContainerFromString"
    ((s ffi-const-string))
  ffi-void-pointer)

(ffi-define-function "makeDotNetContainerFromBoolean"
    ((b ffi-boolean))
  ffi-void-pointer)

(ffi-define-function "makeDotNetContainerFromInt"
    ((n ffi-integer))
  ffi-void-pointer)

(ffi-define-function "makeDotNetContainerFromLong"
    ((s ffi-const-string))
  ffi-void-pointer)

(ffi-define-function "makeDotNetContainerFromFloat"
    ((n ffi-float))
  ffi-void-pointer)

(ffi-define-function "makeDotNetContainerFromDouble"
    ((n ffi-double))
  ffi-void-pointer)

(ffi-define-function "getArrayElement"
    ((ptr ffi-void-pointer)
     (index ffi-integer))
  ffi-void-pointer)

(ffi-define-function "InvocationResultIsException"
    ((ptr ffi-void-pointer))
  ffi-boolean)

(ffi-define-function "invokeConstructor"
    ((type ffi-void-pointer)
     (nargs ffi-integer)
     (args ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-function "setFunctionPointers"
    ((fp1 ffi-void-pointer)
     (fp2 ffi-void-pointer))
  ffi-void)

(ffi-define-function "buildDelegateType"
    ((type-name ffi-const-string)
     (return-type ffi-void-pointer)
     (arg-types ffi-void-pointer))
  ffi-void-pointer)

(ffi-define-callable 
  (LispCallback ffi-void-pointer)
  ((index ffi-integer)
   (args ffi-void-pointer))
  ;; here the actual callback, the Lisp closure, is called - see
  ;; adapter.lisp
  (funcall (gethash index *callback-hash*) args))

(ffi-define-callable 
  (ReleaseDelegateAdapter ffi-void)
  ((index ffi-integer))
  ;; remove entry from hash table if CLR is done with it
  (remhash index *callback-hash*))
