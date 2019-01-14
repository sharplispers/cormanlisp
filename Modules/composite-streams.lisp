;;;; Composite Streams Implementation for Corman Lisp - Version 1.0
;;;;
;;;; Copyright (C) 2004 Espen Wiborg. All Rights Reserved.
;;;;
;;;; License
;;;; =======
;;;; This software is provided 'as-is', without any express or implied
;;;; warranty. In no event will the author be held liable for any damages
;;;; arising from the use of this software.
;;;;
;;;; Permission is granted to anyone to use this software for any purpose,
;;;; including commercial applications, and to alter it and redistribute
;;;; it freely, subject to the following restrictions:
;;;;
;;;; 1. The origin of this software must not be misrepresented; you must
;;;;    not claim that you wrote the original software. If you use this
;;;;    software in a product, an acknowledgment in the product documentation
;;;;    would be appreciated but is not required.
;;;;
;;;; 2. Altered source versions must be plainly marked as such, and must
;;;;    not be misrepresented as being the original software.
;;;;
;;;; 3. This notice may not be removed or altered from any source 
;;;;    distribution.
;;;;
;;;; Notes
;;;; =====
;;;; This package depends on Chris Double's Gray Streams (available
;;;; from http://www.double.nz/cl/).
;;;;
;;;; I insert things into the COMMON-LISP package.  This is not
;;;; exactly comme-il-faut, but composite streams are defined by ANSI
;;;; to be in that package (and the symbols are already exported from
;;;; it).

(in-package :common-lisp)

(require 'gray-streams)

;;;;
;;;; Internal bookkeeping helper
;;;;

(defclass openable-mixin ()
  ((open-p :initform t :accessor open-p))
  (:documentation "Mixin to track openness of composite streams."))

(defmethod gs:stream-open-stream-p ((stream openable-mixin))
  (open-p stream))

(defmethod gs:stream-close :after ((stream openable-mixin))
  "After the main method, flag STREAM as closed."
  (setf (open-p stream) nil))

;;;;
;;;; TWO-WAY-STREAM
;;;;

(defclass two-way-stream (gs:fundamental-character-output-stream
                          gs:fundamental-character-input-stream
                          openable-mixin)
  ((input :initarg :in :reader two-way-stream-input-stream)
   (output :initarg :out :reader two-way-stream-output-stream))
  (:documentation "two-way-stream as per ANSI."))

(defun make-two-way-stream (in out)
  "Make a two-way-stream reading from IN and writing to OUT."
  (check-type in (satisfies input-stream-p))
  (check-type out (satisfies output-stream-p))
  (make-instance 'two-way-stream :in in :out out))

(defmethod print-object ((obj two-way-stream) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "input: ~A output: ~A"
            (two-way-stream-input-stream obj)
            (two-way-stream-output-stream obj))))

(defmethod gs:stream-read-byte ((stream two-way-stream))
  (read-byte (two-way-stream-input-stream stream)))

(defmethod gs:stream-write-byte ((stream two-way-stream) integer)
  (write-byte integer (two-way-stream-output-stream stream)))

(defmethod gs:stream-read-char ((stream two-way-stream))
  (read-char (two-way-stream-input-stream stream)))

(defmethod gs:stream-read-char-no-error ((stream two-way-stream))
  (read-char (two-way-stream-input-stream stream) nil nil))

(defmethod gs:stream-unread-char ((stream two-way-stream) character)
  (unread-char character (two-way-stream-input-stream stream)))

(defmethod gs:stream-read-char-no-hang ((stream two-way-stream))
  (read-char-no-hang (two-way-stream-input-stream stream)))

(defmethod gs:stream-peek-char ((stream two-way-stream))
  (peek-char (two-way-stream-input-stream stream)))

(defmethod gs:stream-listen ((stream two-way-stream))
  (listen (two-way-stream-input-stream stream)))

(defmethod gs:stream-read-line ((stream two-way-stream))
  (read-line (two-way-stream-input-stream stream)))

(defmethod gs:stream-clear-input ((stream two-way-stream))
  (clear-input (two-way-stream-input-stream stream)))

(defmethod gs:stream-write-char ((stream two-way-stream) character)
  (write-char character (two-way-stream-output-stream stream)))
  
(defmethod gs:stream-write-string ((stream two-way-stream) string &optional start end)
  (write-string string (two-way-stream-output-stream stream)
                :start start :end end))

(defmethod gs:stream-terpri ((stream two-way-stream))
  (terpri (two-way-stream-output-stream stream)))

(defmethod gs:stream-fresh-line ((stream two-way-stream))
  (fresh-line (two-way-stream-output-stream stream)))

(defmethod gs:stream-finish-output ((stream two-way-stream))
  (finish-output (two-way-stream-output-stream stream)))

(defmethod gs:stream-force-output ((stream two-way-stream))
  (force-output (two-way-stream-output-stream stream)))

(defmethod gs:stream-clear-output ((stream two-way-stream))
  (clear-output (two-way-stream-output-stream stream)))

(defmethod gs:stream-read-sequence ((stream two-way-stream) sequence start end)
  (read-sequence (two-way-stream-input-stream stream) sequence start end))

(defmethod gs:stream-write-sequence ((stream two-way-stream) sequence start end)
  (write-sequence sequence (two-way-stream-output-stream stream) start end))

(defmethod gs:stream-line-column ((stream two-way-stream))
  (stream-column (two-way-stream-input-stream stream)))

(defmethod gs:stream-close ((stream two-way-stream) &key abort)
  "Sets input and output streams to NIL to ensure that later stream
operations fail."
  (declare (ignore abort))
  (setf (slot-value stream 'input) nil
        (slot-value stream 'output) nil))

;;;;
;;;; SYNONYM-STREAM
;;;;

(defclass synonym-stream (gs:fundamental-stream openable-mixin)
  ((symbol :reader synonym-stream-symbol :initarg :symbol))
  (:documentation "synonym-stream as per ANSI."))
  
(defun make-synonym-stream (symbol)
  "Make a synonym stream for SYMBOL."
  (check-type symbol symbol)
  (make-instance 'synonym-stream :symbol symbol))

(defmethod print-object ((obj synonym-stream) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "for ~S" (synonym-stream-symbol obj))))

(defmethod gs:stream-close ((stream synonym-stream) &key abort)
  "Sets synonym-stream-symbol of STREAM to NIL to ensure that later
stream operations fail."
  (declare (ignore abort))
  (setf (slot-value stream 'symbol) nil))

(defmethod gs:stream-input-stream-p ((stream synonym-stream))
  (input-stream-p (symbol-value (synonym-stream-symbol stream))))

(defmethod gs::stream-input-character-stream-p ((stream synonym-stream))
  (input-character-stream-p (symbol-value (synonym-stream-symbol stream))))

(defmethod gs:stream-output-stream-p ((stream synonym-stream))
  (output-stream-p (symbol-value (synonym-stream-symbol stream))))

(defmethod gs:stream-stream-element-type ((stream synonym-stream))
  (stream-element-type (symbol-value (synonym-stream-symbol stream))))

(defmethod gs:stream-read-byte ((stream synonym-stream))
  (read-byte (symbol-value (synonym-stream-symbol stream))))

(defmethod gs:stream-write-byte ((stream synonym-stream) integer)
  (write-byte integer (symbol-value (synonym-stream-symbol stream))))

(defmethod gs:stream-read-char ((stream synonym-stream))
  (read-char (symbol-value (synonym-stream-symbol stream))))

(defmethod gs:stream-read-char-no-error ((stream synonym-stream))
  (read-char (symbol-value (synonym-stream-symbol stream)) nil nil))

(defmethod gs:stream-unread-char ((stream synonym-stream) character)
  (unread-char character (symbol-value (synonym-stream-symbol stream))))

(defmethod gs:stream-read-char-no-hang ((stream synonym-stream))
  (read-char-no-hang (symbol-value (synonym-stream-symbol stream))))

(defmethod gs:stream-peek-char ((stream synonym-stream))
  (peek-char (symbol-value (synonym-stream-symbol stream))))

(defmethod gs:stream-listen ((stream synonym-stream))
  (listen (symbol-value (synonym-stream-symbol stream))))

(defmethod gs:stream-read-line ((stream synonym-stream))
  (read-line (symbol-value (synonym-stream-symbol stream))))

(defmethod gs:stream-clear-input ((stream synonym-stream))
  (clear-input (symbol-value (synonym-stream-symbol stream))))

(defmethod gs:stream-write-char ((stream synonym-stream) character)
  (write-char character (symbol-value (synonym-stream-symbol stream))))
  
(defmethod gs:stream-write-string ((stream synonym-stream) string &optional start end)
  (write-string string (symbol-value (synonym-stream-symbol stream))
                :start start :end end))

(defmethod gs:stream-terpri ((stream synonym-stream))
  (terpri (symbol-value (synonym-stream-symbol stream))))

(defmethod gs:stream-fresh-line ((stream synonym-stream))
  (fresh-line (symbol-value (synonym-stream-symbol stream))))

(defmethod gs:stream-finish-output ((stream synonym-stream))
  (finish-output (symbol-value (synonym-stream-symbol stream))))

(defmethod gs:stream-force-output ((stream synonym-stream))
  (force-output (symbol-value (synonym-stream-symbol stream))))

(defmethod gs:stream-clear-output ((stream synonym-stream))
  (clear-output (symbol-value (synonym-stream-symbol stream))))

(defmethod gs:stream-read-sequence ((stream synonym-stream) sequence start end)
  (read-sequence (symbol-value (synonym-stream-symbol stream)) sequence start end))

(defmethod gs:stream-write-sequence ((stream synonym-stream) sequence start end)
  (write-sequence sequence (symbol-value (synonym-stream-symbol stream)) start end))

(defmethod gs:stream-line-column ((stream synonym-stream))
  (stream-column (symbol-value (synonym-stream-symbol stream))))

(provide 'composite-streams)
