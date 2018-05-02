;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		version.lisp
;;;;	Contents:	Code to obtain Corman Lisp version information from kernel.
;;;;	History:	02.05.2018  Artem Boldarev  Created.
;;;;
(in-package :cl)

(export '(
        cormanlisp-version
        lisp-implementation-version
        cormanlisp-patch-level))

(defun cormanlisp-version ()
    (multiple-value-bind (major end)
        (parse-integer (cl::%cormanlisp-version-string) :junk-allowed t)
        (multiple-value-bind (minor end)
            (parse-integer (cl::%cormanlisp-version-string) :junk-allowed t :start (1+ end))
            (multiple-value-bind (patch)
                (parse-integer (cl::%cormanlisp-version-string) :junk-allowed t :start (1+ end))
                (values major minor patch)))))

(defun lisp-implementation-version ()
    "Returns Lisp implementation version as a string."
    (multiple-value-bind (major minor)
        (cormanlisp-version)
        (format nil "~A.~A" major minor)))

(defun cormanlisp-patch-level ()
    "Returns Lisp implementation version as an integer."
    (multiple-value-bind (major minor patchlevel)
    (cormanlisp-version)
    (declare (ignore major minor))
    patchlevel))
