;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RDNZL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/rdnzl/examples/apropos2.lisp,v 1.12 2010/05/18 10:55:37 edi Exp $

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

;;; same as apropos.lisp but using "direct calls"

(in-package :rdnzl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; wrapped in EVAL-WHEN because these must be available when the
  ;; direct calls are defined
  (import-types "System.Windows.Forms"
                "Application" "Control" "Control+ControlCollection" "DockStyle" "Form"
                "MessageBox" "KeyPressEventArgs" "KeyPressEventHandler" "TextBox")
  
  (import-types "AproposGUI"
                "AproposControl")
  
  (use-namespace "System.Windows.Forms")
  (use-namespace "AproposGUI"))

;; an instance property
(define-rdnzl-call controls
    (:member-kind :property)
  ((control "Control")))

;; we can't use the standard name here because LENGTH is an external
;; symbol of the COMMON-LISP package
(define-rdnzl-call string-length
    (:member-kind :property
     :dotnet-name "Length")
  ((string "System.String")))

(define-rdnzl-call text
    (:member-kind :property)
  ((control "Control")))

;; a setter function for an instance property
(define-rdnzl-call (setf text)
    (:member-kind :property)
  ((control "Control")))

(define-rdnzl-call (setf dock)
    (:member-kind :property)
  ((control "Control")))

(define-rdnzl-call client-size
    (:member-kind :property)
  ((control "Control")))

(define-rdnzl-call (setf client-size)
    (:member-kind :property)
  ((control "Control")))

(define-rdnzl-call selection-start
    (:member-kind :property)
  ((text-box "TextBox")))

(define-rdnzl-call (setf selection-start)
    (:member-kind :property)
  ((text-box "TextBox")))

(define-rdnzl-call selection-length
    (:member-kind :property)
  ((text-box "TextBox")))

(define-rdnzl-call (setf selection-length)
    (:member-kind :property)
  ((text-box "TextBox")))

(define-rdnzl-call parent
    (:member-kind :property)
  ((string "TextBox")))

;; an instance method
(define-rdnzl-call copy
    ()
  ((text-box "TextBox")))

(define-rdnzl-call key-char
    (:member-kind :property)
  ((event "KeyPressEventArgs")))

;; an instance field (which should have been called "Title" instead of
;; "title")
(define-rdnzl-call title
    (:member-kind :field
     :dotnet-name "title")
  ((control "AproposControl")))

(define-rdnzl-call list-box
    (:member-kind :field
     :dotnet-name "listBox")
  ((control "AproposControl")))

(define-rdnzl-call text-box
    (:member-kind :field
     :dotnet-name "textBox")
  ((control "AproposControl")))

(define-rdnzl-call add
    ()
  ((collection "Control+ControlCollection")
   (control "Control")))

;; a static method of the .NET type MessageBox
(define-rdnzl-call show
    (:type-name "MessageBox")
  ((text "System.String")
   (caption "System.String")))

(define-rdnzl-call run-form
    (:type-name "Application"
     ;; renamed because deliver-xx.lisp already contains a RUN
     ;; function
     :dotnet-name "Run")
  ((form "Form")))

;; a static field of the .NET type DockStyle (which is an enumeration)
(define-rdnzl-call dock-style/fill
    (:member-kind :field
     :dotnet-name "Fill"
     :type-name "DockStyle")
  ())

(define-rdnzl-call add-key-press
    (:dotnet-name "add_KeyPress")
  ((text-box "TextBox")
   (handler "KeyPressEventHandler")))

(defun copy-to-clipboard (text-box)
  (let ((selection-start (selection-start text-box))
        (selection-length (selection-length text-box))
        (text-length (string-length (box (text text-box)))))
    (setf (selection-start text-box) 0
          (selection-length text-box) text-length)
    (copy text-box)
    (setf (selection-start text-box) selection-start
          (selection-length text-box) selection-length)))

(let (message-shown)
  (defun fill-list-box (object event)
    (when (char= (key-char event) #\Return)
      (cast object "TextBox")
      (let* ((input-string (text object))
             (input-length (length input-string)))
        (when (plusp input-length)
          (let ((apropos-text
                  (with-output-to-string (*standard-output*)
                    (apropos input-string)))
                (list-box (list-box (cast (parent object) "AproposControl"))))
            #+(or :cormanlisp :ecl) (setq apropos-text (lf-to-crlf apropos-text))
            (setf (text list-box) apropos-text)
            (copy-to-clipboard list-box)
            (unless message-shown
              (show "The output of APROPOS has been copied to the clipboard."
                    "RDNZL")
              (setq message-shown t)))
          (setf (selection-start object) 0
                (selection-length object) input-length))))))

(defun run-apropos-form ()
  (let* ((control (new "AproposControl"))
         (form (new "Form")))
    (setf (dock control) (dock-style/fill)
          (client-size form) (client-size control)
          (text form) "RDNZL Apropos Demo"
          (text (title control))
            (format nil "RDNZL Apropos Demo (~A)"
                    (lisp-implementation-type)))
    (add-key-press (text-box control)
                   (new "KeyPressEventHandler" #'fill-list-box))
    (add (controls form) control)
    (run-form form)))