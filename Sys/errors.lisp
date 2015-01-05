;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		errors.lisp
;;;;	Contents:	Corman Lisp miscellaneous features.
;;;;	History:	2/27/97  RGC  Created.
;;;;				12/15/98 RGC  Integrated Vassili Bykov's implementations
;;;;							  of ECASE, ETYPECASE
;;;;

(in-package :common-lisp)

(defun %nwrap-otherwise-keys (clauses)
  (dolist (clause clauses)
    (let ((key (car clause)))
      (when (or (eq key 't) (eq key 'otherwise))
        (format *error-output* "~&Warning: ~A is used as a key in ~A, ~
                                assuming you mean (~2:*~A)~%" key clause)
        (setf (car clause) (list key))))))

(defmacro ecase (keyform &rest clauses)
  (%nwrap-otherwise-keys clauses)
  `(case ,keyform
    ,@clauses
    (t (error "No ECASE clause matching the key"))))

(defmacro etypecase (keyform &rest clauses)
  (%nwrap-otherwise-keys clauses)
  `(typecase ,keyform
    ,@clauses
    (t (error "No ETYPECASE clause matching the key"))))
