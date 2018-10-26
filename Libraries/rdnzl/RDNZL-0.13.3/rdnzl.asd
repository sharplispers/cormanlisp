;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/rdnzl/rdnzl.asd,v 1.55 2010/05/18 10:56:37 edi Exp $

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

;;; System definition for ASDF - see <http://www.cliki.net/asdf>

(asdf:defsystem :rdnzl
  :serial t
  :version "0.13.3"
  :components ((:file "packages")
               (:file "specials")
               (:file "util")
               #+:allegro (:file "port-acl")    ; AllegroCL-specific stuff here
               #+:cormanlisp (:file "port-ccl") ; Corman-specific stuff here
               #+:clisp (:file "port-clisp")    ; CLISP-specific stuff here
               #+:ecl (:file "port-ecl")        ; ECL-specific stuff here
               #+:lispworks (:file "port-lw")   ; LispWorks-specific stuff here
               #+:sbcl (:file "port-sbcl")      ; SBCL-specific stuff here
               (:file "ffi")
               (:file "container")
               (:file "reader")
               (:file "arrays")
               (:file "adapter")
               (:file "import")
               (:file "direct")))
