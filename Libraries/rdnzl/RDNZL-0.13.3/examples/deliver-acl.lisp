;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RDNZL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/rdnzl/examples/deliver-acl.lisp,v 1.10 2010/05/18 10:55:37 edi Exp $

;;; Copyright (c) 2004-2010, Charles A. Cox.  All rights reserved.

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

;;; Example: How to deliver a RDNZL application with AllegroCL

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :res))

(defparameter *rdnzl-directory* 
              ;; assume this file is in examples/ subdirectory
              (merge-pathnames #p".."
                               (make-pathname :name nil
                                              :type nil
                                              :version nil
                                              :defaults *load-truename*)))

;; make sure RDNZL is loaded so that we can compile apropos.lisp
;; (better to use provide/require for this?)
(unless (find-package ':rdnzl)
  (load (merge-pathnames #p"load.lisp" *rdnzl-directory*)))

(let ((*default-pathname-defaults* *rdnzl-directory*))
  (generate-application
   "apropos" ; application name
   (merge-pathnames #p"examples/apropos/") ; application directory
   ;; list of files to load in the image being built
   (list (merge-pathnames #p"load.lisp")
         (merge-pathnames (compile-file #p"examples/apropos.lisp")))
   ;; extra files used at runtime
   :application-files (list (merge-pathnames #p"rdnzl.dll")
                            (merge-pathnames #p"examples/AproposGui.dll"))
   :discard-compiler t
   :allow-existing-directory t
   :post-load-form '(rdnzl:shutdown-rdnzl)
   :restart-app-function '(lambda ()
                           (rdnzl:init-rdnzl)
                           (rdnzl-user::run-apropos-form)
                           (exit)))
  
  (win:set-default-command-line-arguments #p"examples/apropos/apropos.exe"
                                          ;; suppress console
                                          '("+c")))
