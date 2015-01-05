;;;; Gray Streams Implementation for Corman Lisp - Version 1.4
;;;;
;;;; Copyright (C) 2000 Christopher Double. All Rights Reserved.
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
;;;; A simple implementation of Gray streams for Corman Lisp 1.42.
;;;; Gray streams are 'clos' based streams as described at:
;;;;
;;;; ftp://parcftp.xerox.com/pub/cl/cleanup/mail/stream-definition-by-user.mail
;;;;
;;;; Some differences exist between this implementation and the 
;;;; specification above. See notes below for details.
;;;;
;;;; More recent versions of this software may be available at:
;;;;   http://www.double.co.nz/cl
;;;;
;;;; Comments, suggestions and bug reports to the author, 
;;;; Christopher Double, at: chris@double.co.nz
;;;;
;;;; 03/03/2001 - 1.0 
;;;;              Initial release.
;;;;
;;;; 20/08/2001 - 1.1
;;;;              Small modifications by Frederic Bastenaire (fba@free.fr) 
;;;;              (lines flagged by  ;; # fb 1.01)
;;;;              - Make it work with the READ function by 
;;;;                defining %read-char, %read-char-with-error
;;;;               and input-character-stream-p
;;;;              - Add nickname GS to package "GRAY-STREAMS" for ease of use
;;;;              - added missing '*' to *old-write-byte* in gray-write-byte
;;;;
;;;; 03/01/2002 - 1.2
;;;;              Fixed bug with GRAY-WRITE-LINE and GRAY-WRITE-STRING
;;;;              that appeared in Corman Lisp 2.0 due to changes to
;;;;              WRITE-LINE and WRITE-STRING.
;;;;
;;;; 04/01/2002 - 1.3
;;;;              Added support for STREAM-READ-SEQUENCE and STREAM-WRITE-SEQUENCE.
;;;;              Fixed STREAM-WRITE-STRING bug.
;;;;
;;;; 11/24/2004 - 1.4
;;;;              Modifications by Espen Wiborg (espenhw@grumblesmurf.org)
;;;;              (lines flagged by ;; # ehw 1.4
;;;;              - Introduced STREAM-READ-CHAR-NO-ERROR
;;;;              - GRAY-%READ-CHAR should call STREAM-READ-CHAR-NO-ERROR
;;;;                to be compatible with %READ-CHAR
;;;;              - Removed the describe-stream business to work in
;;;;                Corman Lisp 2.5
;;;;
;;;; Notes
;;;; =====
;;;; CLOSE is not a generic function in this implementation. Instead,
;;;; the generic is called STREAM-CLOSE and the function CLOSE calls
;;;; STREAM-CLOSE. The same goes for STREAMP, INPUT-STREAM-P, 
;;;; OUTPUT-STREAM-P and STREAM-ELEMENT-TYPE. The generic functions for
;;;; these are STREAM-STREAMP, STREAM-INPUT-STREAM-P, 
;;;; STREAM-OUTPUT-STREAM-P and STREAM-STREAM-ELEMENT-TYPE. 
;;;;
;;;; The standard Corman Lisp streams are not derived from 
;;;; FUNDAMENTAL-STREAM. All the stream functions check to see if the
;;;; stream is an original Corman Lisp stream and forward on to the
;;;; original function implementations.
;;;;
;;;; The string streams are implemented in this file as Gray streams
;;;; but do not replace the Corman Lisp string streams. They are only
;;;; implemented here to test the Gray stream functionality. These methods
;;;; are called:
;;;;    GRAY-MAKE-STRING-OUTPUT-STREAM
;;;;    GRAY-GET-OUTPUT-STREAM-STRING
;;;;    GRAY-MAKE-STRING-INPUT-STREAM
;;;;
;;;; Much of the implementation of the Gray streams below is from the 
;;;; document referenced earlier.
;;;;
(defpackage "GRAY-STREAMS"
	(:use
		"COMMON-LISP")
	(:nicknames "GS") ;; # fb 1.01
	(:export 
		"FUNDAMENTAL-STREAM"
		"STREAM-CLOSE"
		"STREAM-OPEN-STREAM-P"	
		"STREAM-STREAMP"
		"STREAM-INPUT-STREAM-P"
		"STREAM-OUTPUT-STREAM-P"
		"STREAM-STREAM-ELEMENT-TYPE"
		"STREAM-CLOSE"
		"FUNDAMENTAL-OUTPUT-STREAM"
		"FUNDAMENTAL-INPUT-STREAM"
		"FUNDAMENTAL-CHARACTER-STREAM"
		"FUNDAMENTAL-BINARY-STREAM"
		"STREAM-READ-BYTE"
		"STREAM-WRITE-BYTE"
		"FUNDAMENTAL-CHARACTER-INPUT-STREAM"
		"STREAM-READ-CHAR"
		"STREAM-READ-CHAR-NO-ERROR"            ;; # ehw 1.4
		"STREAM-UNREAD-CHAR"
		"STREAM-READ-CHAR-NO-HANG"
		"STREAM-PEEK-CHAR"
        "STREAM-LISTEN"
		"STREAM-READ-LINE"
        "STREAM-CLEAR-INPUT"
		"FUNDAMENTAL-CHARACTER-OUTPUT-STREAM"
		"STREAM-WRITE-CHAR"
		"STREAM-LINE-COLUMN"
		"STREAM-START-LINE-P"
		"STREAM-WRITE-STRING"
		"STREAM-TERPRI"
		"STREAM-FRESH-LINE"
		"STREAM-FINISH-OUTPUT"
		"STREAM-FORCE-OUTPUT"
		"STREAM-CLEAR-OUTPUT"
		"STREAM-ADVANCE-TO-COLUMN"				
        "STREAM-READ-SEQUENCE"
        "STREAM-WRITE-SEQUENCE"
        "FUNDAMENTAL-BINARY-INPUT-STREAM"
		"FUNDAMENTAL-BINARY-OUTPUT-STREAM"					
	))

(in-package :gray-streams)

(defvar *old-read-char* #'read-char)
(defvar *old-peek-char* #'peek-char)
(defvar *old-unread-char* #'unread-char)
(defvar *old-listen* nil)
(defvar *old-read-line* #'read-line)
(defvar *old-clear-input* #'clear-input)
(defvar *old-read-char-no-hang* #'read-char-no-hang)
(defvar *old-write-char* #'write-char)
(defvar *old-fresh-line* #'fresh-line)
(defvar *old-terpri* #'terpri)
(defvar *old-write-string* #'write-string)
(defvar *old-write-line* #'write-line)
(defvar *old-force-output* #'force-output)
(defvar *old-finish-output* #'finish-output)
(defvar *old-clear-output* #'clear-output)
(defvar *old-read-byte* #'read-byte)
(defvar *old-write-byte* #'write-byte)
(defvar *old-%output-char* #'cl::%output-char)
(defvar *old-stream-column* #'cl::stream-column)
(defvar *old-stream-element-type* #'cl::stream-element-type)
(defvar *old-close* #'cl::close)
(defvar *old-input-character-stream-p* #'cl::input-character-stream-p) ;; # fb 1.01
(defvar *old-%read-char* #'cl::%read-char) ;; # fb 1.01
(defvar *old-%read-char-with-error* #'cl::%read-char-with-error) ;; # fb 1.01
(defvar *old-input-stream-p* #'cl::input-stream-p)
(defvar *old-output-stream-p* #'cl::output-stream-p)
(defvar *old-open-stream-p* #'cl::open-stream-p)
(defvar *old-streamp* #'cl::streamp)
(defvar *old-write-stream* #'cl::write-stream)
(defvar *old-read-sequence* #'cl::read-sequence)
(defvar *old-write-sequence* #'cl::write-sequence)

;; ;; Need to load DESCRIBE                             ;; # ehw 1.4
;; (progn                                               ;; # ehw 1.4
;; 	(describe nil nil))	                        ;; # ehw 1.4
;; (defvar *old-describe-stream* #'cl::describe-stream) ;; # ehw 1.4

(defun old-streamp (stream)
	(funcall *old-streamp* stream))

(defclass fundamental-stream ())

(defgeneric stream-close (stream &key abort))
(defgeneric stream-open-stream-p (stream))
(defgeneric stream-streamp (stream))
(defgeneric stream-input-stream-p (stream))
(defgeneric stream-input-character-stream-p (stream)) ;; # fb 1.01
(defgeneric stream-output-stream-p (stream))
(defgeneric stream-stream-element-type (stream))

(defmethod stream-close (stream &key abort)
	(declare (ignore stream abort))
	nil)

(defmethod stream-streamp (s)
	(declare (ignore s))
	nil)

(defmethod stream-streamp ((s fundamental-stream))
	s)

(defclass fundamental-input-stream (fundamental-stream))

(defmethod stream-input-character-stream-p (s)  ;; # fb 1.01
	(and (stream-input-stream-p s)
    	(eq (stream-stream-element-type s) 'character)))

(defmethod stream-input-stream-p (s)
	(declare (ignore s))
	nil)

(defmethod stream-input-stream-p ((s fundamental-input-stream))
	(declare (ignore s))
	t)

(defclass fundamental-output-stream (fundamental-stream))

(defmethod stream-output-stream-p (s)
	(declare (ignore s))
	nil)

(defmethod stream-output-stream-p ((s fundamental-output-stream))
	(declare (ignore s))
	t)

(defclass fundamental-character-stream (fundamental-stream))

(defmethod stream-stream-element-type ((s fundamental-character-stream))
	(declare (ignore s))
	'character)

(defclass fundamental-binary-stream (fundamental-stream))

(defgeneric stream-read-byte (stream))
(defgeneric stream-write-byte (stream integer))

(defclass fundamental-character-input-stream
	(fundamental-input-stream fundamental-character-stream))

(defgeneric stream-read-char (stream))
(defgeneric stream-read-char-no-error (stream))   ;; # ehw 1.4
(defgeneric stream-unread-char (stream character))
(defgeneric stream-read-char-no-hang (stream))
(defgeneric stream-peek-char (stream))
(defgeneric stream-listen (stream))
(defgeneric stream-read-line (stream))
(defgeneric stream-clear-input (stream))

(defmethod stream-peek-char ((stream fundamental-character-input-stream))
	(let ((character (stream-read-char stream)))
		(unless (eq character :eof)
			(stream-unread-char stream character))
		character))

(defmethod stream-listen ((stream  fundamental-character-input-stream))
	(let ((char (stream-read-char-no-hang stream)))
		(and (not (null char))
			(not (eq char :eof))
			(progn
				(stream-unread-char stream char)
				t))))

(defmethod stream-read-line ((stream  fundamental-character-input-stream))
	(let ((line (make-array 64 :element-type 'character
					:fill-pointer 0 :adjustable t)))
		(loop
			(let ((character (stream-read-char stream)))
				(if (eq character :eof)
					(return (values line t))
					(if (eql character #\Newline)
						(return (values line nil))
						(vector-push-extend character line)))))))

(defclass fundamental-character-output-stream
	(fundamental-output-stream fundamental-character-stream))

(defgeneric stream-write-char (stream character))
(defgeneric stream-line-column (stream))
(defgeneric stream-start-line-p (stream))
(defgeneric stream-write-string (stream string &optional start end))
(defgeneric stream-terpri (stream))
(defgeneric stream-fresh-line (stream))
(defgeneric stream-finish-output (stream))
(defgeneric stream-force-output (stream))
(defgeneric stream-clear-output (stream))
(defgeneric stream-advance-to-column (stream column))
(defgeneric stream-read-sequence (stream sequence start end))
(defgeneric stream-write-sequence (stream sequence start end))

(defmethod stream-force-output (stream)
	(declare (ignore stream))
	nil)

(defmethod stream-start-line-p ((stream fundamental-character-output-stream))
	(equal (stream-line-column stream) 0))

(defmethod stream-write-string ((stream fundamental-character-output-stream)
		string &optional (start 0) (end (length string)))
    (let ((start (or start 0))
          (end (or end (length string))))
	  (do ((i start (1+ i)))
		  ((>= i end) string)
		(stream-write-char stream (char string i)))))

(defmethod stream-fresh-line ((stream fundamental-character-output-stream))
	(if (stream-start-line-p stream)
		nil
		(progn
			(stream-terpri stream)
			t)))

(defmethod stream-advance-to-column ((stream fundamental-character-output-stream)
		column)
	(let ((current (stream-line-column stream)))
		(unless (null current)
			(dotimes (i (- current column) t)
				(stream-write-char stream #\Space)))))

;; Implementation for Corman Lisp READ-SEQUENCE
(defmethod stream-read-sequence ((stream  fundamental-character-input-stream) sequence start end)
    (if (null end)
        (setf end (length sequence)))
    (let ((element-type (stream-element-type stream))
		  (eof (cons nil nil)))
		(cond 
			((eq element-type 'character)
				(dotimes (count (- end start) (- end start))
					(let ((c (stream-read-char stream nil eof)))
						(if (eq c eof)
							(return (+ count start)))
						(setf (elt sequence (+ count start)) c))))
			((or (eq element-type 'byte) 
					(eq element-type 'unsigned-byte) 
					(eq element-type 'signed-byte))
				(dotimes (count (- end start) (- end start))
					(let ((b (stream-read-byte stream nil eof)))
						(if (eq b eof)
							(return (+ count start)))
						(setf (elt sequence (+ count start)) b))))
			(t (error "Cannot READ-SEQUENCE on stream of :ELEMENT-TYPE ~A" element-type)))))

;; Implementation for Corman Lisp WRITE-SEQUENCE
(defmethod stream-write-sequence ((stream fundamental-character-output-stream) sequence start end)
	(let ((element-type (stream-element-type stream))
			(start (if start start 0))
			(end (if end end (length sequence))))
		(if (eq element-type 'character)
			(do ((n start (+ n 1)))
				((= n end))
				(stream-write-char (if (typep (elt sequence n) 'number) (ccl:int-char (elt sequence n)) (elt sequence n)) stream))
			(do ((n start (+ n 1)))
				((= n end))
				(stream-write-byte (elt sequence n) stream))))		;; recoded to avoid LOOP, because it isn't loaded yet
	(stream-force-output stream))

(defclass fundamental-binary-input-stream
	(fundamental-input-stream fundamental-binary-stream))

(defclass fundamental-binary-output-stream
	(fundamental-output-stream fundamental-binary-stream))

(defun decode-read-arg (arg)
	(cond ((null arg) *standard-input*)
		((eq arg t) *terminal-io*)
		(t arg)))

(defun decode-print-arg (arg)
	(cond ((null arg) *standard-output*)
		((eq arg t) *terminal-io*)
		(t arg)))

(defun report-eof (stream eof-errorp eof-value)
	(if eof-errorp
		(error 'end-of-file :stream stream)
		eof-value))

(defun check-for-eof (value stream eof-errorp eof-value)
	(if (eq value :eof)
		(report-eof stream eof-errorp eof-value)
		value))

(defun gray-read-char (&optional input-stream (eof-errorp t) eof-value recursive-p)
	(declare (ignore recursive-p))
	(let ((stream (decode-read-arg input-stream)))
		(if (old-streamp stream)
			(funcall *old-read-char* stream eof-errorp eof-value recursive-p)
			(check-for-eof (stream-read-char stream) stream eof-errorp eof-value))))

(defun gray-peek-char (&optional peek-type input-stream (eof-errorp t)
		eof-value recursive-p)
	(declare (ignore recursive-p))
	(let ((stream (decode-read-arg input-stream)))
		(if (old-streamp stream)
			(funcall *old-peek-char* peek-type stream eof-errorp
				eof-value recursive-p)
			(if (null peek-type)
				(check-for-eof (stream-peek-char stream) stream eof-errorp eof-value)
				(loop
					(let ((value (stream-peek-char stream)))
						(if (eq value :eof)
							(return (report-eof stream eof-errorp eof-value))
							(if (if (eq peek-type t)
									(not (member value '(#\space #\tab #\newline
												#\return)))
									(char= peek-type value))
								(return value)
								(stream-read-char stream)))))))))

(defun gray-unread-char (character &optional input-stream)
	(let ((stream (decode-read-arg input-stream)))
		(if (old-streamp stream)
			(funcall *old-unread-char* character stream)	  
			(stream-unread-char stream character))))

(defun gray-listen (&optional input-stream)
	(let ((stream (decode-read-arg input-stream)))
		(if (old-streamp stream)
			(funcall *old-listen* stream)	  
			(stream-listen stream))))

(defun gray-read-line (&optional input-stream (eof-error-p t)
		eof-value recursive-p)
	(declare (ignore recursive-p))
	(let ((stream (decode-read-arg input-stream)))
		(if (old-streamp stream)
			(funcall *old-read-line* stream eof-error-p eof-value recursive-p)
			(multiple-value-bind (string eofp)
				(stream-read-line stream)
				(if eofp
					(if (= (length string) 0)
						(report-eof stream eof-error-p eof-value)
						(values string t))
					(values string nil))))))

(defun gray-clear-input (&optional input-stream)
	(let ((stream (decode-read-arg input-stream)))
		(if (old-streamp stream)
			(funcall *old-clear-input* stream)
			(stream-clear-input stream))))

(defun gray-read-char-no-hang (&optional input-stream (eof-errorp t)
		eof-value recursive-p)
	(declare (ignore recursive-p))
	(let ((stream (decode-read-arg input-stream)))
		(if (old-streamp stream)
			(funcall *old-read-char-no-hang* stream eof-errorp eof-value recursive-p)
			(check-for-eof (stream-read-char-no-hang stream)
				stream eof-errorp eof-value))))

(defun gray-write-char (character &optional output-stream)
	(let ((stream (decode-print-arg output-stream)))
		(if (old-streamp stream)
			(funcall *old-write-char* character stream)
			(stream-write-char stream character))))

(defun gray-fresh-line (&optional output-stream)
	(let ((stream (decode-print-arg output-stream)))
		(if (old-streamp stream)
			(funcall *old-fresh-line* stream)
			(stream-fresh-line stream))))

(defun gray-terpri (&optional output-stream)
	(let ((stream (decode-print-arg output-stream)))
		(if (old-streamp stream)
			(funcall *old-terpri* stream)
			(stream-terpri stream))))

(defun gray-write-string (string &optional output-stream &key (start 0) end)
	(let ((stream (decode-print-arg output-stream)))
		(if (old-streamp stream)
			(funcall *old-write-string* string stream :start start :end end)
			(stream-write-string stream string start end))))

(defun gray-write-line (string &optional output-stream &key (start 0) end)
	(let ((stream (decode-print-arg output-stream)))
		(if (old-streamp stream)
			(funcall *old-write-line* string stream :start start :end end)
			(progn
				(stream-write-string stream string start end)
				(stream-terpri stream)
				string))))

(defun gray-force-output (&optional output-stream)
	(let ((stream (decode-print-arg output-stream)))
		(if (old-streamp stream)
			(funcall *old-force-output* stream)
			(stream-force-output stream))))

(defun gray-finish-output (&optional output-stream)
	(let ((stream (decode-print-arg output-stream)))
		(if (old-streamp stream)
			(funcall *old-finish-output* stream)
			(stream-finish-output stream))))

(defun gray-clear-output (&optional output-stream)
	(let ((stream (decode-print-arg output-stream)))
		(if (old-streamp stream)
			(funcall *old-clear-output* stream)
			(stream-clear-output stream))))

(defun gray-read-byte (binary-input-stream &optional (eof-errorp t) eof-value)
	(if (old-streamp binary-input-stream)
		(funcall *old-read-byte* binary-input-stream eof-errorp eof-value)	
		(check-for-eof (stream-read-byte binary-input-stream)
			binary-input-stream eof-errorp eof-value)))

(defun gray-write-byte (integer binary-output-stream)
	(if (old-streamp binary-output-stream)
		(funcall *old-write-byte* integer binary-output-stream)	
		(stream-write-byte binary-output-stream integer)))

(defclass string-input-stream (fundamental-character-input-stream)
	((string :initarg :string :type string)
		(index :initarg :start :type fixnum)
		(end :initarg :end :type fixnum)))

(defun gray-make-string-input-stream (string &optional (start 0) end)
	(make-instance 'string-input-stream :string string
		:start start :end (or end (length string))))

(defmethod stream-read-char ((stream string-input-stream))
	(with-slots (index end string) stream
		(if (>= index end)
			:eof
			(prog1
				(char string index)
				(incf index)))))

(defmethod stream-unread-char ((stream string-input-stream) character)
	(with-slots (index end string) stream
		(decf index)
		(assert (eql (char string index) character))
		nil))

(defmethod stream-read-line ((stream string-input-stream))
	(with-slots (index end string) stream
		(let* ((endline (position #\newline string :start index :end end))
				(line (subseq string index endline)))
			(if endline
				(progn
					(setq index (1+ endline))
					(values line nil))
				(progn
					(setq index end)
					(values line t))))))

(defclass string-output-stream (fundamental-character-output-stream)
	((string :initform nil :initarg :string)))

(defun gray-make-string-output-stream ()
	(make-instance 'string-output-stream))

(defun gray-get-output-stream-string (stream)
	(with-slots (string) stream
		(if (null string)
			""
			(prog1
				(coerce string 'string)
				(setq string nil)))))

(defmethod stream-write-char ((stream string-output-stream) character)
	(with-slots (string) stream
		(when (null string)
			(setq string (make-array 64 :slement-type 'character
					:fill-pointer 0 :adjustable t)))
		(vector-push-extend character string)
		character))
			  


(defmethod stream-line-column ((stream string-output-stream))
	(with-slots (string) stream
		(if (null string)
			0
			(let ((nx (position #\newline string :from-end t)))
				(if (null nx)
					(length string)
					(- (length string) nx 1))))))

(defun gray-%output-char (character output-stream)
	(let ((stream (decode-print-arg output-stream)))
		(if (old-streamp stream)
			(funcall *old-%output-char* character stream)
			(stream-write-char stream character))))
  
(defun gray-stream-column (&optional input-stream)
	(let ((stream (decode-read-arg input-stream)))
		(if (old-streamp stream)
			(funcall *old-stream-column* stream)
			(stream-line-column stream))))

(defun gray-stream-element-type (stream)
	(if (old-streamp stream)
		(funcall *old-stream-element-type* stream)
		(stream-stream-element-type stream)))

(defun gray-close (stream)
	(if (old-streamp stream)
		(funcall *old-close* stream)
		(stream-close stream :abort nil)))

(defun gray-input-stream-p (stream)
	(if (old-streamp stream)
		(funcall *old-input-stream-p* stream)
		(stream-input-stream-p stream)))

(defun gray-input-character-stream-p (stream)
	(if (old-streamp stream)
		(funcall *old-input-character-stream-p* stream)
		(stream-input-character-stream-p stream)))

(defun gray-output-stream-p (stream)
	(if (old-streamp stream)
		(funcall *old-output-stream-p* stream)
		(stream-output-stream-p stream)))

(defun gray-open-stream-p (stream)
	(if (old-streamp stream)
		(funcall *old-open-stream-p* stream)
		(stream-open-stream-p stream)))

(defun gray-write-stream (stream)
	(if (old-streamp stream)
		(funcall *old-write-stream* stream)
		(cl::write-clos-instance stream)))

;; (defun gray-describe-stream (object stream)                ;; # ehw 1.4
;; 	(if (old-streamp object)                              ;; # ehw 1.4
;; 		(funcall *old-describe-stream* object stream) ;; # ehw 1.4
;; 		(cl::describe-clos-instance object stream)))  ;; # ehw 1.4

(defun gray-streamp (stream)
	(if (old-streamp stream)
		(funcall *old-streamp* stream)
		(stream-streamp stream)))

(defun gray-%read-char (stream)  ;; # fb 1.01
	(if (old-streamp stream)
		(funcall *old-%read-char* stream)
		(stream-read-char-no-error stream))) ;; # ehw 1.4

(defun gray-%read-char-with-error (stream)  ;; # fb 1.01
	(if (old-streamp stream)
		(funcall *old-%read-char-with-error* stream)
		(stream-read-char stream)))

(defun gray-write-sequence (sequence stream &key start end)
    (if (old-streamp stream)
        (funcall *old-write-sequence* sequence stream :start start :end end)
        (stream-write-sequence stream sequence start end)))

(defun gray-read-sequence (sequence stream &key (start 0) (end nil))
    (if (old-streamp stream)
        (funcall *old-read-sequence* sequence stream :start start :end end)
        (stream-read-sequence stream sequence start end)))

(setf (symbol-function 'common-lisp::read-char) #'gray-read-char)
(setf (symbol-function 'common-lisp::peek-char) #'gray-peek-char)
(setf (symbol-function 'common-lisp::unread-char) #'gray-unread-char)
(setf (symbol-function 'common-lisp::read-line) #'gray-read-line)
(setf (symbol-function 'common-lisp::clear-input) #'gray-clear-input)
(setf (symbol-function 'common-lisp::read-char-no-hang) #'gray-read-char-no-hang)
(setf (symbol-function 'common-lisp::write-char) #'gray-write-char)
(setf (symbol-function 'common-lisp::fresh-line) #'gray-fresh-line)
(setf (symbol-function 'common-lisp::terpri) #'gray-terpri)
(setf (symbol-function 'common-lisp::write-string) #'gray-write-string)
(setf (symbol-function 'common-lisp::write-line) #'gray-write-line)
(setf (symbol-function 'common-lisp::force-output) #'gray-force-output)
(setf (symbol-function 'common-lisp::finish-output) #'gray-finish-output)
(setf (symbol-function 'common-lisp::clear-output) #'gray-clear-output)
(setf (symbol-function 'common-lisp::read-byte) #'gray-read-byte)
(setf (symbol-function 'common-lisp::write-byte) #'gray-write-byte)
(setf (symbol-function 'common-lisp::%output-char) #'gray-%output-char)
(setf (symbol-function 'common-lisp::stream-column) #'gray-stream-column)
(setf (symbol-function 'common-lisp::stream-element-type) #'gray-stream-element-type)
(setf (symbol-function 'common-lisp::close) #'gray-close)
(setf (symbol-function 'common-lisp::input-stream-p) #'gray-input-stream-p)
(setf (symbol-function 'common-lisp::input-character-stream-p) #'gray-input-character-stream-p)  ;; # fb 1.01
(setf (symbol-function 'common-lisp::%read-char) #'gray-%read-char)  ;; # fb
(setf (symbol-function 'common-lisp::%read-char-with-error) #'gray-%read-char-with-error)  ;; # fb 1.01
(setf (symbol-function 'common-lisp::output-stream-p) #'gray-output-stream-p)
(setf (symbol-function 'common-lisp::open-stream-p) #'gray-open-stream-p)
(setf (symbol-function 'common-lisp::streamp) #'gray-streamp)
(setf (symbol-function 'common-lisp::write-stream) #'gray-write-stream)
;; (setf (symbol-function 'common-lisp::describe-stream) #'gray-describe-stream) ;; # ehw 1.4
(setf (symbol-function 'common-lisp::read-sequence) #'gray-read-sequence)
(setf (symbol-function 'common-lisp::write-sequence) #'gray-write-sequence)

(provide 'gray-streams)
