;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RDNZL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/rdnzl_lisp/examples/deliver-lw.lisp,v 1.7 2006/01/31 15:17:00 edi Exp $

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

;;; Example: How to deliver a RDNZL application with LispWorks

;;; Usage: Open up a console window and execute somthing like this:
;;;   "C:\Program Files\LispWorks\lispworks-4450.exe" -init "C:\path\to\RDNZL\examples\deliver-lw.lisp"

(in-package :cl-user)

(defun copy-file (from to)
  (let ((element-type '(unsigned-byte 8))
        (buffer-size 8192))
    (with-open-file (in from
                     :element-type element-type)
      (with-open-file (out to
                       :direction :output
                       :if-exists :supersede
                       :element-type element-type)
        (let ((buf (make-array buffer-size
                               :element-type element-type)))
          (loop
            (let ((pos (read-sequence buf in)))
              (when (zerop pos) (return))
              (write-sequence buf out :end pos))))))))
(compile 'copy-file)

(defparameter *rdnzl-directory* 
              ;; assume this file is in examples/ subdirectory
              (merge-pathnames #p".."
                               (make-pathname :name nil
                                              :type nil
                                              :version nil
                                              :defaults *load-truename*)))

(hcl:change-directory *rdnzl-directory*)
(load "load.lisp")
(load (compile-file "examples/apropos.lisp"))

(defun run ()
  (rdnzl:init-rdnzl)
  (run-apropos-form)
  0)
(compile 'run)

(rdnzl:shutdown-rdnzl)

(defparameter *target-directory* 
              (merge-pathnames "examples/apropos/" *rdnzl-directory*))

(defun target-path (file-name)
  (merge-pathnames file-name *target-directory*))
(compile 'target-path)

(copy-file "RDNZL.dll" (ensure-directories-exist
                        (target-path "RDNZL.dll")))
(copy-file "examples/AproposGUI.dll" (target-path "AproposGUI.dll"))

(hcl:change-directory *target-directory*)

(lw:deliver #'run "apropos"
            ;; we could use 5 here but then APROPOS wouldn't make much
            ;; sense... :)
            4
            :compact t
            :redefine-compiler-p nil
            :keep-symbol-names '(rdnzl::LispCallback rdnzl::ReleaseDelegateAdapter)
            :keep-lisp-reader t
            :console :input)

(quit)