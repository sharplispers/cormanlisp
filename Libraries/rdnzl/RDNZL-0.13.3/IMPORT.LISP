;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RDNZL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/rdnzl/import.lisp,v 1.64 2010/05/18 10:54:28 edi Exp $

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

;;; Importing types and assemblies, initialization.

(in-package :rdnzl)

(enable-rdnzl-syntax)

(defun import-type (type &optional assembly)
  "Imports the .NET type TYPE, i.e. registers its name as one that can
be abbreviated \(see USE-NAMESPACE) and maybe creates a mapping from
its short name to its assembly-qualified name.  If TYPE is a string
and ASSEMBLY is NIL then the function will try to create the type from
the string with the static method System.Type::GetType.  If TYPE is a
string and ASSEMBLY is an assembly \(a CONTAINER) then instead the
instance method System.Reflection.Assembly::GetType will be used.  If
TYPE is already a .NET object \(i.e. a CONTAINER) then the function
will just register its name.  If ASSEMBLY is a true value then the
name will also be mapped to its assembly-qualified name.  In all cases
the type itself \(as a CONTAINER) will be returned."
  (cond ((container-p type)
         (setf (gethash [%FullName type] *type-hash*)
                 (cond (assembly [%AssemblyQualifiedName type])
                       (t t)))
         type)
        ((stringp type)
         (import-type (cond (assembly
                             (or [GetType assembly type]
                                 (error "Type with name ~S not found in assembly ~S."
                                        type [%FullName assembly])))
                            (t
                             (let ((imported-type (make-type-from-name type)))
                               (when (%dot-net-container-is-null (pointer imported-type))
                                 (error "Type with name ~S not found."
                                        type))
                               imported-type)))
                      assembly))
        (t (error "Don't know how to import type ~S." type))))

(defun new (type &rest other-args)
  "Creates a new .NET object \(a CONTAINER) of the type TYPE.  Calls
the constructor determined by OTHER-ARGS \(a list of Lisp object
and/or CONTAINERs), i.e. by the corresponding signature.  TYPE can be
a string \(naming the type) or a CONTAINER \(representing the type).
If TYPE is a delegate then the second argument to NEW must be a Lisp
closure with a correspoding signature."
  (cond ((or (stringp type)
             (consp type))
         (apply #'new
                (make-type-from-name (resolve-type-name type))
                other-args))
        ((container-p type)
         (cond ([IsAssignableFrom (make-type-from-name "System.Delegate") type]
                ;; it's a delegate
                (let* ((method-info [GetMethod type "Invoke"])
                       (adapter (make-adapter (first other-args)
                                              [%ReturnType method-info]
                                              (mapcar #`%ParameterType
                                                      (rdnzl-array-to-list [GetParameters method-info])))))
                  (invoke-constructor type
                                      adapter
                                      [GetFunctionPointer [%MethodHandle [GetMethod [GetType adapter]
                                                                                    "InvokeClosure"]]])))
               (t (apply #'invoke-constructor
                         type
                         other-args))))
        (t (error "Don't know how to make a new ~S." type))))
         
(defun load-assembly (name)
  "Loads and returns the assembly with the name NAME \(a string), uses
LoadWithPartialName and then Load so we get a meaningful error if the
assembly wasn't found."
  (or [System.Reflection.Assembly.LoadWithPartialName name]
      (let ((assembly-name (new "System.Reflection.AssemblyName")))
        (setf [%Name assembly-name] name)
        [System.Reflection.Assembly.Load assembly-name])))


(defun import-assembly (assembly)
  "Imports all public types of the assembly ASSEMBLY \(a string or a
CONTAINER).  If ASSEMBLY is a string then the assembly is first loaded
with LOAD-ASSEMBLY.  Returns ASSEMBLY as a CONTAINER."
  (cond ((container-p assembly)
         (do-rdnzl-array (type [GetTypes assembly])
           (when [%IsPublic type]
             (import-type type assembly)))
         assembly)
        ((stringp assembly)
         (import-assembly (load-assembly assembly)))
        (t (error "Don't know how to import assembly ~S." assembly))))

(defun import-types (assembly-name &rest type-names)
  "Loads the assembly named ASSEMBLY-NAME and imports \(see function
IMPORT-TYPE) all types listed from this assembly.  The assembly name
is prepended to the type names before importing them.  All arguments
should be strings."
  (let ((assembly (or (load-assembly assembly-name)
                      (error "Assembly ~S not found" assembly-name))))
    (dolist (type-name type-names)
      (import-type (concatenate 'string
                                assembly-name
                                "."
                                type-name)
                   assembly))))

(defun reset-cached-data ()
  "Resets all relevant global special variables to their initial value,
thereby releasing pointers to DotNetContainer objects if necessary.
Also removes all direct call definitions."
  (setq *callback-counter* 0
        *delegate-counter* 0)
  (clrhash *callback-hash*)
  (clrhash *signature-hash*)
  (loop for function-name being the hash-keys in *direct-definitions*
        do (fmakunbound function-name)))

(defun init-rdnzl ()
  "Initializes RDNZL.  This function must be called once before RDNZL
is used."
  ;; see <http://msdn.microsoft.com/library/en-us/vcmex/html/vcconconvertingmanagedextensionsforcprojectsfrompureintermediatelanguagetomixedmode.asp?frame=true>
  (dll-ensure-init)
  ;; inform the DelegateAdapter class about where the Lisp callbacks
  ;; are located
  (%set-function-pointers (ffi-make-pointer 'LispCallback)
                          (ffi-make-pointer 'ReleaseDelegateAdapter))
  ;; reset to a sane state
  (reset-cached-data)
  (reimport-types)
  (redefine-direct-calls)
  ;; see comment for DLL-ENSURE-INIT above
  (register-exit-function #'dll-force-term "Close DLL")
  ;; set Lisp callback pointers back to NULL before the image exits
  (register-exit-function (lambda ()
                            (%set-function-pointers (ffi-make-null-pointer)
                                                    (ffi-make-null-pointer)))
                          "Clear Lisp callbacks")
  (values))

(defun shutdown-rdnzl (&optional no-gc)
  "Prepares RDNZL for delivery or image saving.  After calling this
function RDNZL can't be used anymore unless INIT-RDNZL is called
again.  If NO-GC is NIL \(the default) a full garbage collection is
also performed."
  (reset-cached-data)
  (dll-force-term)
  (unless no-gc
    (full-gc))
  (values))

(defun reimport-types ()
  "Loops through all imported types and tries to associate them with
the correct assembly.  Only relevant for delivery and saved images."
  (let ((assembly-hash (make-hash-table :test #'equal)))
    (loop for type-name being the hash-keys in *type-hash*
          using (hash-value assembly-qualified-name)
          ;; only do this for types which need the assembly-qualified
          ;; name
          when (stringp assembly-qualified-name)
            do (let ((assembly-name (find-partial-assembly-name assembly-qualified-name)))
                 (import-type type-name
                              (or (gethash assembly-name assembly-hash)
                                  (setf (gethash assembly-name assembly-hash)
                                          (load-assembly assembly-name))))))))

(defun redefine-direct-calls ()
  "Loops through all direct call definition which have been stored in
*DIRECT-DEFINITIONS* and re-animates them.  Only relevant for delivery
and saved images."
  (loop for function-name being the hash-keys in *direct-definitions*
        using (hash-value function-data)
        do (create-direct-call function-name function-data)))

;; when loading this file initialize RDNZL
(eval-when (:load-toplevel :execute)
  (init-rdnzl))

(disable-rdnzl-syntax)
