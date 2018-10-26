;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RDNZL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/rdnzl/util.lisp,v 1.29 2010/05/18 10:54:29 edi Exp $

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

;;; Several utility functions.

(in-package :rdnzl)

#+:lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'lw:with-unique-names))

#-:lispworks
(defmacro with-unique-names ((&rest bindings) &body body)
  "Syntax: WITH-UNIQUE-NAMES ( { var | (var x) }* ) declaration* form*

Executes a series of forms with each VAR bound to a fresh,
uninterned symbol. The uninterned symbol is as if returned by a call
to GENSYM with the string denoted by X - or, if X is not supplied, the
string denoted by VAR - as argument.

The variable bindings created are lexical unless special declarations
are specified. The scopes of the name bindings and declarations do not
include the Xs.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3bshuf30f.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  `(let ,(mapcar #'(lambda (binding)
                     (check-type binding (or cons symbol))
                     (if (consp binding)
                       (destructuring-bind (var x) binding
                         (check-type var symbol)
                         `(,var (gensym ,(etypecase x
                                          (symbol (symbol-name x))
                                          (character (string x))
                                          (string x)))))
                       `(,binding (gensym ,(symbol-name binding)))))
                 bindings)
         ,@body))

#+:lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (macro-function 'with-rebinding)
          (macro-function 'lw:rebinding)))

#-:lispworks
(defmacro with-rebinding (bindings &body body)
  "WITH-REBINDING ( { var | (var prefix) }* ) form*

Evaluates a series of forms in the lexical environment that is
formed by adding the binding of each VAR to a fresh, uninterned
symbol, and the binding of that fresh, uninterned symbol to VAR's
original value, i.e., its value in the current lexical environment.

The uninterned symbol is created as if by a call to GENSYM with the
string denoted by PREFIX - or, if PREFIX is not supplied, the string
denoted by VAR - as argument.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3wv0fya0p.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  (loop for binding in bindings
        for var = (if (consp binding) (car binding) binding)
        for name = (gensym)
        collect `(,name ,var) into renames
        collect ``(,,var ,,name) into temps
        finally (return `(let ,renames
                          (with-unique-names ,bindings
                            `(let (,,@temps)
                              ,,@body))))))

(defun starts-with (string sub-string)
  "Returns true if the string STRING starts with the string
SUB-STRING."
  (let ((mismatch (mismatch string sub-string :test #'char-equal)))
    (or (null mismatch)
        (>= mismatch (length sub-string)))))

(defmacro named-when ((var form) &body body)
  "Executes BODY if FORM evaluates to a true value. During the
execution of BODY VAR is bound to the value returned by FORM."
  `(let ((,var ,form))
     (when ,var
       ,@body)))

(defun use-namespace (namespace)
  "Adds the .NET namespace NAMESPACE \(a string) to the list of
namespaces that will be prefixed when trying to resolve a type name.
After calling this function NAMESPACE will be the first entry in this
list unless it has already been there."
  (pushnew (concatenate 'string namespace ".")
           *used-namespaces*
           :test #'string=)
  (values))

(defun unuse-namespace (namespace)
  "Removes the .NET namespace NAMESPACE \(a string) from the list of
namespaces that will be prefixed when trying to resolve a type name."
  (setq *used-namespaces*
          (delete (concatenate 'string namespace ".")
                  *used-namespaces*
                  :test #'string=))
  (values))

(defun unuse-all-namespaces ()
  "Removes all entries from the list of namespaces that will be
prefixed when trying to resolve a type name."
  (setq *used-namespaces* nil)
  (values))

(defun resolve-type-name (name)
  "If NAME is a string which names a type which has been previously
imported via IMPORT-TYPE, then return its assembly-qualified name.  If
a type named NAME can't be found directly, then also try the `used'
namespaces.  If NAME is a tree of strings, interpret this as a generic
type and resolve each leaf as above, except that for the first \(base)
type the suffix giving the number of parameters is added
automatically"
  (cond ((stringp name)
         (loop for namespace in (cons "" *used-namespaces*)
               for full-name = (concatenate 'string namespace name)
               for hashed-name = (gethash full-name *type-hash*)
               when hashed-name
               do (return (cond ((stringp hashed-name) hashed-name)
                                (t full-name)))
               finally (return name)))
        (t (let ((first-type-name
                   (concatenate 'string (car name)
                                (format nil "`~D" (length (rest name))))))
             (mapcar #'resolve-type-name (cons first-type-name (rest name)))))))

(defun mangle-name (string)
  "Converts the string STRING into another string with case determined
by the current readtable-case and where a hyphen is inserted whenever
the case changes from lower to upper, e.g. \"myCoolFoo\" becomes
\"MY-COOL-FOO\"."
  (symbol-name
   (read-from-string
    (with-output-to-string (out)
      (loop for last-char = #\. then char
            for char across string
            when (and (lower-case-p last-char)
                      (upper-case-p char))
            do (write-char #\- out)
            do (write-char (char-downcase char) out))))))

(defun make-lisp-name (c-name)
  "Makes a Lisp name \(a symbol in the RDNZL package) from a C name."
  (intern (concatenate 'string "%" (mangle-name c-name)) :rdnzl))

(defun unmangle-name* (string)
  "STRING is assumed to be a string consisting solely of single-case
letters and hyphens.  This function will return a string with all
hyphens removed and all characters downcased except for the first one
and those following a hyphen - these are upcased."
  (with-output-to-string (out)
    (loop with upcase = t
          for c across string
          do (cond ((char= c #\-)
                    (setq upcase t))
                   (upcase
                    (write-char (char-upcase c) out)
                    (setq upcase nil))
                   (t
                    (write-char (char-downcase c) out))))))

(defun unmangle-name (function-name)
  "FUNCTION-NAME is assumed to be a function name, i.e. a symbol
or a cons of the form \(SETF symbol).  If the symbol name of this
symbol consists solely of single-case letters appropriate for the
current readtable-case and hyphens then UNMANGLE-NAME* is applied
to it, otherwise the symbol name itself is returned.  Note that
the return value is always a symbol even if the argument was a
cons."
  (let* ((symbol (cond ((consp function-name)
                        (second function-name))
                       (t function-name)))
         (symbol-name (symbol-name symbol)))
    (let ((case-test (case (readtable-case *readtable*)
                       ((:upcase :invert) #'upper-case-p)
                       (t #'lower-case-p))))
      (cond ((every (lambda (c)
                      (or (funcall case-test c)
                          (char= c #\-)))
                    symbol-name)
             (unmangle-name* symbol-name))
            (t symbol-name)))))

(defun find-partial-assembly-name (type-name)
  "Tries to extract the partial assembly name from the
assembly-qualified type name TYPE-NAME."
  (let ((length (length type-name)))
    (flet ((find-comma (start)
             "Finds the position of the first comma within TYPE-NAME
\(starting from position START) which is not preceded by a backslash."
             (loop for i = start then (1+ pos)
                   for pos = (and (< i length)
                                  (position #\, type-name :test #'char= :start i))
                   while (and pos
                              (plusp pos)
                              (char= (char type-name (1- pos)) #\\))
                   finally (return pos))))
    (let* ((first-comma (find-comma 0))
           ;; now skip spaces
           (non-space (and first-comma
                           (position #\Space type-name :test #'char/= :start (1+ first-comma))))
           (second-comma (and non-space
                              (find-comma non-space))))
      (or (and second-comma
               (> second-comma non-space)
               (subseq type-name non-space second-comma))
          (error "Couldn't find partial assembly name in ~S" type-name))))))

(defun whitespacep (chr)
  "Tests whether a character is whitespace."
  (member chr +whitespace-char-list+ :test #'char=))

