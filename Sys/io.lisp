;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		io.lisp
;;;;	Contents:	Corman Lisp I/O functions.
;;;;	History:	8/16/96  RGC  Created.
;;;;

;;;;
;;;;	Common Lisp PEEK-CHAR function.
;;;;

(defun peek-char (&optional 
					(peek-type nil) 
					(input-stream *standard-input*) 
					(eof-error-p t)
					(eof-value nil)
					(recursive-p nil))
	(declare (ignore recursive-p eof-value eof-error-p))
	(let ((ch (%read-char input-stream)))
		(unless peek-type
			(progn
				(unread-char ch input-stream)
				(return-from peek-char ch)))
		(if (characterp peek-type)
			(do ()
				((eq ch peek-type)(progn (unread-char ch input-stream) ch))
				(setq ch (%read-char input-stream)))
			(if (eq peek-type t)
				(do ((readtable *readtable*))
					((eq (readtable-char-type readtable ch) 'whitespace-char-type)
					 (progn (unread-char ch input-stream) ch))
					(setq ch (%read-char input-stream)))
				(error "Invalid PEEK-TYPE: ~A" peek-type)))))
