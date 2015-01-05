;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RDNZL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/rdnzl_lisp/examples/apropos.lisp,v 1.6 2006/01/31 21:05:01 edi Exp $

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

(in-package :rdnzl-user)

(enable-rdnzl-syntax)

(import-types "System.Windows.Forms"
              "Application" "DockStyle" "Form" "MessageBox" "KeyPressEventHandler" "TextBox")

(import-types "AproposGUI"
              "AproposControl")

(use-namespace "System.Windows.Forms")
(use-namespace "AproposGUI")

(defun copy-to-clipboard (text-box)
  (let ((selection-start [%SelectionStart text-box])
        (selection-length [%SelectionLength text-box])
        (text-length [%Length (box [%Text text-box])]))
    (setf [%SelectionStart text-box] 0
          [%SelectionLength text-box] text-length)
    [Copy text-box]
    (setf [%SelectionStart text-box] selection-start
          [%SelectionLength text-box] selection-length)))

(let (message-shown)
  (defun fill-list-box (object event)
    (when (char= [%KeyChar event] #\Return)
      (cast object "TextBox")
      (let* ((input-string [%Text object])
             (input-length (length input-string)))
        (when (plusp input-length)
          (let ((apropos-text
                  (with-output-to-string (*standard-output*)
                    (apropos input-string)))
                (list-box [$listBox (cast [%Parent object] "AproposControl")]))
            #+:cormanlisp (setq apropos-text (lf-to-crlf apropos-text))
            (setf [%Text list-box] apropos-text)
            (copy-to-clipboard list-box)
            (unless message-shown
              [MessageBox.Show "The output of APROPOS has been copied to the clipboard."
                               "RDNZL"]
              (setq message-shown t)))
          (setf [%SelectionStart object] 0
                [%SelectionLength object] input-length))))))

(defun run-apropos-form ()
  (let* ((control (new "AproposControl"))
         (form (new "Form")))
    (setf [%Dock control] [$DockStyle.Fill]
          [%ClientSize form] [%ClientSize control]
          [%Text form] "RDNZL Apropos Demo"
          [%Text [$title control]]
            (format nil "RDNZL Apropos Demo (~A)"
                    (lisp-implementation-type)))
    [+KeyPress [$textBox control]
               (new "KeyPressEventHandler" #'fill-list-box)]
    [Add [%Controls form] control]
    [Application.Run form]))

(disable-rdnzl-syntax)