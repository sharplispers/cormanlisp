;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RDNZL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/rdnzl_lisp/adapter.lisp,v 1.28 2006/01/03 18:48:49 edi Exp $

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

(in-package :rdnzl)

(enable-rdnzl-syntax)

(defun wrap-closure (closure return-type arg-types)
  "Generates and returns a wrapper for the Lisp function CLOSURE such
that it can be used as a .NET delegate with the return type
RETURN-TYPE and argument types as in the array ARG-TYPE-ARRAY. Both
RETURN-TYPE and ARG-TYPE-ARRAY are DOT-NET-OBJECTs."
  (let ((arg-type-names (map 'vector #`%AssemblyQualifiedName arg-types))
        ;; remember if the delegate doesn't return a result
        (void-result-p [Equals return-type
                               (make-type-from-name "System.Void")]))
    ;; wrapper starts here
    (lambda (args-pointer &aux completed)
      (unwind-protect 
	  (prog1
            (let ((i 0)
                  args)
              ;; loop through the array of arguments and cast each one
              ;; to the expected type, convert to native Lisp types if
              ;; appropriate
              (do-rdnzl-array (arg (wrap-with-container args-pointer))
                (cast* arg (aref arg-type-names i))
                (incf i)
                (push (unbox arg) args))
              ;; call the actual function
              (let ((result (apply closure (nreverse args))))
                (pointer
                 (cond (void-result-p
                        ;; return a dummy System.Void object in case
                        ;; the delegate doesn't return anything
                        (make-null-object* "System.Void"))
                       (t
                        ;; otherwise wrap the result
                        (ensure-container result))))))
	    (setq completed t))
	;; block throw attempts
	(unless completed
	  (labels ((block-throw (&aux (block t))
		     (unwind-protect
			 (restart-case 
			     (error "Cannot safely throw over a .NET -> Lisp callback.")
			   (continue-throw ()
			     :report "Continue throw anyway."
			     (setq block nil)))
		       (when block
			 (block-throw)))))
	    (block-throw)))))))

(defun make-adapter (closure return-type arg-types)
  "Creates, if necessary, a subtype of DelegateAdapter \(see C++ code)
matching the signature determined by RETURN-TYPE \(a CONTAINER) and
ARG-TYPES \(a list of CONTAINERs).  Then creates and returns a new
instance of this type which is used to wrap the Lisp closure CLOSURE."
  (let* ((arg-type-array (list-to-rdnzl-array arg-types
                                              (make-type-from-name "System.Type")))
         ;; the signature is a tupel of the return type's name and the
         ;; names of the argument types
         (signature (mapcar #`%AssemblyQualifiedName
                            (cons return-type arg-types)))
         ;; first check if we have already cached a type for this
         ;; signature, otherwise create it (via a call into RDNZL.dll)
         (delegate-type (or (gethash signature *signature-hash*)
                            (setf (gethash signature *signature-hash*)
                                    (build-delegate-type (format nil "_LispCallback_~A"
                                                                 (incf *delegate-counter*))
                                                         return-type
                                                         arg-type-array)))))
    (let ((delegate-instance (new delegate-type)))
      ;; initialize the new instance by informing it about the index
      ;; number of this callback
      [init delegate-instance (incf *callback-counter*)]
      ;; wrap the Lisp closure with the code for argument marshalling
      ;; and store it using the same index number
      (setf (gethash *callback-counter* *callback-hash*)
              (wrap-closure closure return-type arg-types))
      delegate-instance)))

(disable-rdnzl-syntax)
