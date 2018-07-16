;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		documentation.lisp
;;;;	Contents:	Corman Lisp DOCUMENTATION function.
;;;;	History:	2/11/99  RGC  Created.
;;;;                    6/28/18  LC   DOCUMENTATION and (SETF DOCUMENTATION) generic functions
;;;;
(in-package :common-lisp)

;;;
;;;	Common Lisp  DOCUMENTATION and (SETF DOCUMENTATION) standard generic functions
;;;

(defgeneric documentation (x doc-type) (:documentation "
Method Signature Extensions:

(x function) (doc-type ignored as if t or function)
(x symbol) (doc-type :hyperspec)
(x metaobject) (doc-type ignored as if t or type)
(x standard-class) (doc-type t or slot-name)
(x package) (doc-type ignored as if t)
"))

(defgeneric (setf documentation) (new-value x doc-type))

;;; before
(defmethod (setf documentation) :before (new-value x doc-type) (declare (ignore x doc-type))
    (unless (typep new-value '(or string null)) (error "~a not a STRING or NIL." new-value)))

;;; function
(defmethod documentation ((x function) doc-type) (declare (ignore doc-type))
    (or (ccl::function-documentation x) (values (parse-doc-decls-body (cddr (ccl:function-lambda x))))))

(defmethod (setf documentation) (new-value (x function) doc-type) (declare (ignore doc-type))
    (setf (ccl::function-documentation x) new-value))

;;; list
(defmethod documentation ((x list) doc-type)
    (and (eq 'setf (car x)) (eq 2 (ignore-errors (length x))) (symbolp (cadr x)) (documentation (setf-function-symbol x) doc-type))) 

(defmethod (setf documentation) (new-value (x list) doc-type)
    (and (eq 'setf (car x)) (eq 2 (ignore-errors (length x))) (symbolp (cadr x))
            (setf (documentation (setf-function-symbol x) doc-type) new-value)))

;;; symbol
(defmethod documentation ((x symbol) doc-type) (getf (gethash x *documentation-registry*) doc-type))

(defmethod (setf documentation) ((new-value string) (x symbol) doc-type)
    (setf (getf (gethash x *documentation-registry*) doc-type) new-value))

(defmethod (setf documentation) ((new-value null) (x symbol) doc-type)
    (when (documentation x doc-type)
              (remf (gethash x *documentation-registry*) doc-type)
              (unless (gethash x *documentation-registry*) (remhash x *documentation-registry*)))
    nil)

;;; metaobject. standard-generic-function standard-method standard-class structure-class
(defmethod documentation ((x metaobject) doc-type) (declare (ignore doc-type)) (class-documentation x))

(defmethod (setf documentation) (new-value (x metaobject) doc-type) (declare (ignore doc-type))
    (setf (class-documentation x) new-value))

;;; package. doc attached to the keyword with name package-name
(defmethod documentation ((x package) doc-type) (declare (ignore doc-type))
    (documentation (intern (package-name x) :keyword) 'package))

(defmethod (setf documentation) (new-value (x package) doc-type) (declare (ignore doc-type))
    (setf (documentation (intern (package-name x) :keyword) 'package) new-value))

;;; standard-class
(defmethod documentation ((x standard-class) doc-type)
    (if (eq doc-type t) (call-next-method) (slot-definition-documentation (find doc-type (class-slots x) :key #'slot-definition-name))))

(defmethod (setf documentation) (new-value (x standard-class) doc-type)
    (if (eq doc-type t) (call-next-method)
        (setf (slot-definition-documentation (find doc-type (class-slots x) :key #'slot-definition-name)) new-value)))

;;; :hyperspec
(defun win::documentation-selection ()) ; defined later

(defmethod documentation ((x symbol) (doc-type (eql :hyperspec)))
    (when (eq (symbol-package x) #.(find-package :cl)) (ignore-errors (win::documentation-selection x :hyperspec))))

(setf (documentation '(setf documentation) 'function) "
Method Signature Extensions:

(new-value string or nil)

(x function) (doc-type ignored as if t or function)
(x metaobject) (doc-type ignored as if t or type)
(x standard-class) (doc-type t or slot-name)
(x package) (doc-type ignored as if t)
" (documentation (fdefinition '(setf documentation)) t) (documentation '(setf documentation) 'function))
