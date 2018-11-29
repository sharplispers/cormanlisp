;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		code-formatter.lisp
;;;;	Contents:	Code formatting facility on top of Dorai Sitaram's 'scmindent' (https://github.com/ds26gte/scmindent/).
;;;;	History:	11/29/18  Artem Boldariev  Created.
;;;;

(in-package :ide)

(defun %indent-string (str)
  "Indent string as Lisp code."
  (if (stringp str)
      (with-input-from-string (in str)
        (with-output-to-string (out (make-array 0
                                                :element-type 'character
                                                :adjustable t
                                                :fill-pointer 0))
          (let ((*standard-input* in)
                (*standard-output* out))
            (indent-lines))
          out))
    str))

(defun %indent-last-line (str)
  "Accepts block of a Lips code as string, returns last indented line. Intended for formatting code during typing inside IDE."
  (let* ((indented (%indent-string str))
         (res indented))
    (with-input-from-string (in indented)
      (do ((text (read-line in nil) (read-line in nil)))
          ((null text))
        (setf res text)))
    res))

(defun %prepare-string-for-indention (str)
  (let ((result (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (dotimes (i (length str))
      (let ((c (aref str i)))
        (if (char= c #\Return)
            (progn
              (vector-push-extend #\Return result)
              (vector-push-extend #\Newline result))
          (vector-push-extend c result))))
    result))

#! (:export nil :library "msvcrt")
unsigned long strlen(const char *str);
char *strcpy (char *destination, const char *source );
!#

(ct::defun-direct-c-callback %indent-next-line ((context (:char *)))
  (if (not (ct::cpointer-null context))
      (flet ((heap-strdup (str)
                          (let* ((len (strlen str))
                                 (copy (ct::heap-alloc (ct::get-process-heap) 0 (1+ len))))
                            (if (ct::cpointer-null copy)
                                ct::null
                              (strcpy copy str)))))
        (heap-strdup ; do not forget to call HeapFree(GetProcessHeap()...) in C code
          (ct::lisp-string-to-c-string
            (%indent-last-line (concatenate 'string
                                            (%prepare-string-for-indention
                                                         (ct::c-string-to-lisp-string context))
                                            (format nil "~%"))))))
    ct:null))
