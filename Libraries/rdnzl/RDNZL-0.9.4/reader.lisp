;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RDNZL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/rdnzl_lisp/reader.lisp,v 1.17 2006/01/31 15:16:56 edi Exp $

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

;;; This file defines the special reader syntax for .NET calls.

(in-package :rdnzl)

(define-condition rdnzl-reader-error (simple-condition reader-error)
  ()
  (:report (lambda (condition stream)
             (format stream "RDNZL reader error: ~?"
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition))))
  (:documentation "A reader error which can be signalled by ERROR."))

(defmacro signal-reader-error (stream format-control &rest format-arguments)
  "Like ERROR but signals a SIMPLE-READER-ERROR for the stream
STREAM."
  `(error 'rdnzl-reader-error
          :stream ,stream
          :format-control ,format-control
          :format-arguments (list ,@format-arguments)))

(defun read-rdnzl-token (stream)
  "Tries to emulate how the Lisp reader reads a token with standard
syntax but is case-sensitive.  Returns a string."
  (let ((collector (make-array 0
                               :element-type 'character
                               :fill-pointer t
                               :adjustable t))
        in-multiple-escape-p
        in-single-escape-p
        char-seen-p)
    (loop
      (let ((char (peek-char nil stream nil nil t)))
        (cond (in-multiple-escape-p
               ;; in multiple escape mode, read everything as is but
               ;; don't accept EOF
               (unless char
                 (signal-reader-error stream
                                      "End of file while in multiple~
escape mode \(i.e. after pipe character)."))
               (read-char stream nil nil t)
               (cond ((char= char #\|)
                      ;; end of multiple escape mode
                      (setq in-multiple-escape-p nil))
                     (t
                      (vector-push-extend char collector))))
              (in-single-escape-p
               ;; single escape mode, i.e. last char was backslash -
               ;; read next char as is but don't accept EOF
               (unless char
                 (signal-reader-error stream
                                      "End of file while in single~
escape mode \(i.e. after backslash character)."))
               (setq in-single-escape-p nil)
               (read-char stream nil nil t)
               (vector-push-extend char collector))
              ((null char)
               ;; EOF - return what has been read so far
               (return-from read-rdnzl-token collector))
              ((and (not char-seen-p)
                    (whitespacep char))
               ;; skip whitespace after #\[
               (read-char stream nil nil t))
              ((char= char #\|)
               ;; switch to multiple escape mode
               (setq in-multiple-escape-p t
                     char-seen-p t)
               (read-char stream nil nil t))
              ((char= char #\\)
               ;; switch to single escape mode
               (setq in-single-escape-p t
                     char-seen-p t)
               (read-char stream nil nil t))
              ((or (whitespacep char)
                   (member char '(#\" #\' #\( #\) #\[ #\] #\, #\; #\`)
                           :test #'char=))
               ;; whitespace or terminating macro character, stop
               ;; parsing this token
               (return-from read-rdnzl-token collector))
              (t
               ;; otherwise just consume the character
               (setq char-seen-p t)
               (read-char stream nil nil t)
               (vector-push-extend char collector)))))))

(defun read-and-parse-rdnzl-token (stream)
  "Reads a token like \"%Environment.UserName\" with READ-RDNZL-TOKEN
and dissects it into its parts \(type name and member name) if
necessary.  Also returns the corresponding function \(INVOKE,
PROPERTY, or FIELD) from container.lisp."
  (let ((token (read-rdnzl-token stream))
        (function-name 'invoke))
    (when (string= token "")
      (signal-reader-error stream
                           "Empty token after #\[ character."))
    (when (and (= (length token) 1)
               (member (char token 0) '(#\% #\$ #\+ #\-)
                       :test #'char=))
      (signal-reader-error stream
                           "Illegal token \"~C\" after #\[ character."
                           token))
    (let ((first-char (char token 0)))
      (case first-char
        ((#\%)
         ;; first char #\% means property
         (setf function-name 'property
               token (subseq token 1)))
        ((#\$)
         ;; first char #\$ means field
         (setf function-name 'field
               token (subseq token 1)))
        ((#\+)
         ;; first char #\+ adds "add_"
         (setf token (concatenate 'string "add_"
                                  (subseq token 1))))
        ((#\-)
         ;; first char #\- adds "remove_"
         (setf token (concatenate 'string "remove_"
                                  (subseq token 1))))))
    ;; find last dot (if any) in token
    (let ((dot-pos (position #\. token :test #'char= :from-end t)))
      (cond (dot-pos
             ;; if there is a dot we have a static invocation and the
             ;; part before the dot is the type name
             (when (= dot-pos (1- (length token)))
               (signal-reader-error stream
                                  "Dot at end of token."))
             (let ((type-name (subseq token 0 dot-pos))
                   (member-name (subseq token (1+ dot-pos))))
               (values member-name function-name type-name)))
            (t
             ;; otherwise it's an instance invocation
             (values token function-name))))))
  

(defun rdnzl-list-reader (stream char)
  (declare (ignore char))
  "The reader function for the RDNZL \[] notation."
  ;; read the first token after the opening bracket with
  ;; READ-RDNZL-TOKEN
  (multiple-value-bind (member-name function-name type-name)
      (read-and-parse-rdnzl-token stream)
    ;; now read rest until #\]
    (let ((args (read-delimited-list #\] stream t)))
      (cond (type-name
             ;; static invocation
             (list* function-name type-name member-name args))
            (t
             ;; instance invocation
             (unless args
               ;; we always need at least one argument - the object
               ;; instance itself
               (signal-reader-error stream
                                    "Missing arguments after token \"~A~A\"."
                                    (case function-name
                                      ((invoke) "")
                                      ((property) "%")
                                      ((field) "$"))
                                    member-name))
             (list* function-name (first args) member-name (rest args)))))))

(defun rdnzl-function-reader (stream char arg)
  "The reader function for the RDNZL #` notation.  Always returns a
function object."
  (declare (ignore char arg))
  (cond ((char= #\( (peek-char nil stream t nil t))
         ;; starts with a left parenthesis, so we expect #`(SETF ...)
         (read-char stream t nil t)
         (let ((symbol (read stream t nil t)))
           (unless (eq symbol 'setf)
             (signal-reader-error stream
                                  "Expected CL:SETF after \"#`(\""))
           (multiple-value-bind (member-name function-name type-name)
               (read-and-parse-rdnzl-token stream)
             (unless (char= #\) (peek-char t stream t nil t))
               (signal-reader-error stream
                                    "Expected #\) after \"#`(CL:SETF ~A\"."
                                    (if type-name
                                      (concatenate 'string type-name "." member-name)
                                      member-name)))
             (read-char stream t nil t)
             (cond (type-name
                    `(lambda (new-value &rest args)
                       (apply #'(setf ,function-name)
                              new-value ,type-name ,member-name args)))
                   (t
                    `(lambda (new-value object &rest args)
                       (apply #'(setf ,function-name)
                              new-value object ,member-name args)))))))
        (t
         (multiple-value-bind (member-name function-name type-name)
             (read-and-parse-rdnzl-token stream)
           (cond (type-name
                  `(lambda (&rest args)
                     (apply #',function-name
                            ,type-name ,member-name args)))
                 (t
                  `(lambda (object &rest args)
                     (apply #',function-name
                            object ,member-name args))))))))
  
(defun %enable-rdnzl-syntax ()
  "Internal function used to enable reader syntax and store current
readtable on stack."
  (push *readtable*
        *previous-readtables*)
  (setq *readtable* (copy-readtable))
  (set-syntax-from-char #\] #\) *readtable*)
  ;; make #\[ non-terminating
  (set-macro-character #\[
                       #'rdnzl-list-reader)
  (set-dispatch-macro-character #\# #\` #'rdnzl-function-reader)
  (values))

(defun %disable-rdnzl-syntax ()
  "Internal function used to restore previous readtable." 
  (if *previous-readtables*
    (setq *readtable* (pop *previous-readtables*))
    (setq *readtable* (copy-readtable nil)))
  (values))

(defmacro enable-rdnzl-syntax ()
  "Enables RDNZL reader syntax."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-rdnzl-syntax)))

(defmacro disable-rdnzl-syntax ()
  "Restores the readtable which was active before the last call to
ENABLE-RDNZL-SYNTAX. If there was no such call, the standard readtable
is used."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (%disable-rdnzl-syntax)))
