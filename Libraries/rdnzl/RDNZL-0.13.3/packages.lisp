;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/rdnzl/packages.lisp,v 1.35 2010/05/18 10:54:28 edi Exp $

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

;;; Definition of the "RDNZL" package.

(in-package :cl-user)

;; Corman Lisp has problems with uninterned symbols like #:aref*
(defpackage :rdnzl
  (:use :cl)
  #+:sbcl (:shadow :defconstant)
  (:export :*coerce-double-floats-to-single*
           :aref*
           :box
           :cast
           :container-p
           :copy-container
           :define-rdnzl-call
           :disable-rdnzl-syntax
           :do-rdnzl-array
           :do-rdnzl-collection
           :enable-rdnzl-syntax
           :enum-to-integer
           :field
           :import-assembly
           :import-type
           :import-types
           :integer-to-enum
           :invoke
           :init-rdnzl
           #+(or :cormanlisp :ecl) :lf-to-crlf
           :load-assembly
           :list-to-rdnzl-array
           :make-null-object
           :new
           :or-enums
           :property
           :ref
           :rdnzl-array-to-list
           :rdnzl-error
           :rdnzl-error-exception
           :rdnzl-handler-case
           :shutdown-rdnzl
           :unbox
           :unuse-all-namespaces
           :unuse-namespace
           :use-namespace))

(defpackage :rdnzl-user
  (:use :cl :rdnzl)
  (:documentation "This package is intended for playing around
with RDNZL."))