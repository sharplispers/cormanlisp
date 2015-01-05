;;;; snooper.lisp -- turns ? into a macro character that causes the
;;;; value of the form that follows it to be printed on
;;;; *snooper-output* every time the form is evaluated.
;;;;
;;;; Author:  Vassili Bykov <vassili@objectpeople.com>, <vassili@magma.ca>
;;;; Created: 11/12/1998

(in-package :cormanlisp)
(provide "SNOOPER")

(defvar *snooper-output* *error-output*)
;; *debug-io* is not available?

(defun expand-snooper (stream char)
    (declare (ignore char))
  (let ((form (read stream t nil t))
        (newsym (gensym)))
    `(let* ((,newsym ,form))
      (format *snooper-output* "?: ~S~%"
       ,newsym)
      ,newsym)))

(set-macro-character #\? #'expand-snooper)
