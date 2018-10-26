;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RDNZL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/rdnzl/specials.lisp,v 1.31 2010/05/18 10:54:29 edi Exp $

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

;;; Global special variables (and constants) used by RDNZL.

(in-package :rdnzl)

#+:sbcl
(defmacro defconstant (name form &optional documentation)
  ;; see <http://www.sbcl.org/manual/Defining-Constants.html>
  `(cl:defconstant ,name
     (cond ((boundp ',name) (symbol-value ',name))
           (t ,form))
     ,@(and documentation (list documentation))))

(defvar *used-namespaces* nil
  "A list of namespaces which are `used.'  See USE-NAMESPACE and
related functions.")

(defvar *dll-initialized* nil
  "Whether RDNZL.dll was initialized with DllEnsureInit.")

(defconstant +private-assembly-name+ "RDNZLPrivateAssembly"
  "The name of the assembly which is generated at run time to create
subtypes of DelegateAdapter.")

(defvar *callback-counter* 0
  "The index of the last closure from which a delegate was created -
or 0 if no delegate has been created yet. Used as a key in the
*CALLBACK-HASH* hash table.")

(defvar *callback-hash* (make-hash-table)
  "A hash table which maps integers to closures used as delegates -
see the instance variable indexIntoLisp in DelegateAdapter.cpp.")

(defvar *delegate-counter* 0
  "Counter used to make sure each subtype of DelegateAdapter has a
unique name.")

(defvar *signature-hash* (make-hash-table :test #'equal)
  "A hash table which maps delegate signatures to subtypes of
DelegateAdapter so that we only create one such subtype for each
signature.")

(defvar *type-hash* (make-hash-table :test #'equal)
  "A hash table which maps short type names of `imported' types to
fully qualified type names \(or to T if the type can be retrieved by
Type::GetType without a fully qualified name).")

(defvar *direct-definitions* (make-hash-table :test #'equal)
  "Maps function names \(for direct calls) to data structures which
can be used to re-construct the function.")

(defconstant +whitespace-char-list+
             '(#\Space #\Tab #\Linefeed #\Newline #\Return #\Page)
  "A list of all characters which are considered to be whitespace.")

(defvar *previous-readtables* nil
  "A stack which holds the previous readtables that have been pushed
here by ENABLE-RDNZL-SYNTAX.")

(defvar *coerce-double-floats-to-single* nil
  "If this is true, then BOX will convert a Lisp DOUBLE-FLOAT
value to System.Single.  This is mainly interesting for
LispWorks, where Lisp floats are always DOUBLE-FLOAT.")

(pushnew :rdnzl *features*)

;; stuff for Nikodemus Siivola's HYPERDOC
;; see <http://common-lisp.net/project/hyperdoc/>
;; and <http://www.cliki.net/hyperdoc>
;; also used by LW-ADD-ONS

(defvar *hyperdoc-base-uri* "http://weitz.de/rdnzl/")

(let ((exported-symbols-alist
       (loop for symbol being the external-symbols of :rdnzl
             collect (cons symbol
                           (concatenate 'string
                                        "#"
                                        (string-downcase symbol))))))
  (defun hyperdoc-lookup (symbol type)
    (declare (ignore type))
    (cdr (assoc symbol
                exported-symbols-alist
                :test #'eq))))
