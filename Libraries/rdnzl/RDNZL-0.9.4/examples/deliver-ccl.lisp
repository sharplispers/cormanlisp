;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RDNZL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/rdnzl_lisp/examples/deliver-ccl.lisp,v 1.6 2006/01/31 15:17:00 edi Exp $

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

;;; Example: How to deliver a RDNZL application with Corman Common Lisp

;;; Usage: Start clconsole.exe and from there
;;;   (LOAD "/path/to/RDNZL/examples/deliver-ccl.lisp")

(in-package :cl-user)

(defun copy-file (from to)
  (let ((element-type '(unsigned-byte 8)))
    (with-open-file (in from
                     :element-type element-type)
      (with-open-file (out to
                       :direction :output
                       :if-exists :supersede
                       :element-type element-type)
        (loop for byte = (read-byte in nil nil)
              while byte
                do (write-byte byte out))))))

(defparameter *rdnzl-directory*
              ;; assume this file is in examples/ subdirectory
              (make-pathname :name nil
                             :type nil
                             :version nil
                             :directory (butlast (pathname-directory *load-truename*))
                             :defaults *load-truename*))

(setf (ccl:current-directory) *rdnzl-directory*)

(load "load.lisp")
(load "examples/apropos.lisp")

(defun main ()
  (rdnzl:init-rdnzl)
  (run-apropos-form)
  (shutdown-rdnzl))

(rdnzl:shutdown-rdnzl)

(let ((target-dir
        (merge-pathnames "examples/apropos/" *rdnzl-directory*)))
  (defun target-path (file-name)
    (merge-pathnames file-name target-dir)))

(copy-file "RDNZL.dll" (ensure-directories-exist
                        (target-path "RDNZL.dll")))
(copy-file "examples/AproposGUI.dll" (target-path "AproposGUI.dll"))
(copy-file (concatenate 'string ccl:*cormanlisp-server-directory* "\\msvcr70.dll")
           (target-path "msvcr70.dll"))

(ccl:save-application (namestring (target-path "apropos.exe"))
                      #'main
                      :console nil
                      :static t)