;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		places.lisp
;;;;	Contents:	Corman Lisp SETF functions.
;;;;	History:	10/10/2001  RGC  Created.
;;;;

(in-package :cl)

;;;
;;; Redefine these to get correct order of argument evaluation
;;;
;;;
;;;	Common Lisp (SETF FIRST) etc. functions
;;;
(defun (setf first) (val x) (setf (car x) val))
(defun (setf second) (val x) (setf (car (cdr x)) val))
(defun (setf third) (val x) (setf (car (cdr (cdr x))) val))
(defun (setf fourth) (val x) (setf (car (cdr (cdr (cdr x)))) val))
(defun (setf fifth) (val x) (setf (car (cdr (cdr (cdr (cdr x))))) val))
(defun (setf sixth) (val x) (setf (car (cdr (cdr (cdr (cdr (cdr x)))))) val))
(defun (setf seventh) (val x) (setf (car (cdr (cdr (cdr (cdr (cdr (cdr x))))))) val))
(defun (setf eighth) (val x) (setf (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr x)))))))) val))
(defun (setf ninth) (val x) (setf (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr x))))))))) val))
(defun (setf tenth) (val x) (setf (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr x)))))))))) val))

;;;
;;;	Common Lisp (SETF REST) function.
;;;
(defun (setf rest)(val x) (setf (cdr x) val))

(defun (setf caar) (val list) (setf (car (car list)) val))
(defun (setf cadr) (val list) (setf (car (cdr list)) val))
(defun (setf cdar) (val list) (setf (cdr (car list)) val))
(defun (setf cddr) (val list) (setf (cdr (cdr list)) val))

(defun (setf caaar) (val list) (setf (car (car (car list))) val))
(defun (setf caadr) (val list) (setf (car (car (cdr list))) val))
(defun (setf cadar) (val list) (setf (car (cdr (car list))) val))
(defun (setf caddr) (val list) (setf (car (cdr (cdr list))) val))
(defun (setf cdaar) (val list) (setf (cdr (car (car list))) val))
(defun (setf cdadr) (val list) (setf (cdr (car (cdr list))) val))
(defun (setf cddar) (val list) (setf (cdr (cdr (car list))) val))
(defun (setf cdddr) (val list) (setf (cdr (cdr (cdr list))) val))

(defun (setf caaaar) (val list) (setf (car (car (car (car list)))) val))
(defun (setf caaadr) (val list) (setf (car (car (car (cdr list)))) val))
(defun (setf caadar) (val list) (setf (car (car (cdr (car list)))) val))
(defun (setf caaddr) (val list) (setf (car (car (cdr (cdr list)))) val))
(defun (setf cadaar) (val list) (setf (car (cdr (car (car list)))) val))
(defun (setf cadadr) (val list) (setf (car (cdr (car (cdr list)))) val))
(defun (setf caddar) (val list) (setf (car (cdr (cdr (car list)))) val))
(defun (setf cadddr) (val list) (setf (car (cdr (cdr (cdr list)))) val))

(defun (setf cdaaar) (val list) (setf (cdr (car (car (car list)))) val))
(defun (setf cdaadr) (val list) (setf (cdr (car (car (cdr list)))) val))
(defun (setf cdadar) (val list) (setf (cdr (car (cdr (car list)))) val))
(defun (setf cdaddr) (val list) (setf (cdr (car (cdr (cdr list)))) val))
(defun (setf cddaar) (val list) (setf (cdr (cdr (car (car list)))) val))
(defun (setf cddadr) (val list) (setf (cdr (cdr (car (cdr list)))) val))
(defun (setf cdddar) (val list) (setf (cdr (cdr (cdr (car list)))) val))
(defun (setf cddddr) (val list) (setf (cdr (cdr (cdr (cdr list)))) val))

(defun remove-struct-print (func) (remprop func ':struct-print))


