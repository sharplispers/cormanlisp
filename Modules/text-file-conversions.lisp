;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	All rights reserved.
;;;;	-------------------------------
;;;;
;;;;	File:		text-file-conversions.lisp
;;;;	Contents:	Converts unix text files or Macintosh text files
;;;;				to Windows text file.
;;;; 	Author:		Roger Corman
;;;;	History:	1/06/99  RGC  Created.
;;;;

(in-package :cormanlisp)
(export '(unix-to-windows mac-to-windows))
(provide "TEXT-FILE-CONVERSIONS")

(defun unix-to-windows (filename)
	(let* ((input-path (pathname filename))
		   (output-path (make-pathname 
							:name (concatenate 'string (pathname-name input-path) "-temp")
							:directory (pathname-directory input-path) 
							:device (pathname-device input-path) 
							:type (pathname-type input-path)))
		   (is (open input-path :direction :input :element-type 'integer))
		   (os (open output-path :direction :output :element-type 'integer)))
		(do ((ch (read-byte is nil 'EOF)(read-byte is nil 'EOF)))
			((eq ch 'EOF) (close is)(close os))
			(if (eq ch 10)
				(write-byte 13 os))
			(write-byte ch os))))

(defun mac-to-windows (filename)
	(let* ((input-path (pathname filename))
		   (output-path (make-pathname 
							:name (concatenate 'string (pathname-name input-path) "-temp")
							:directory (pathname-directory input-path) 
							:device (pathname-device input-path) 
							:type (pathname-type input-path))))
	   (with-open-file (is input-path :direction :input )
			(with-open-file (os output-path :direction :output)
				(do ((ch (read-char is nil 'EOF)(read-char is nil 'EOF)))
					((eq ch 'EOF))
					(when (eq ch 13)
						(write-char ch os)
						(setq ch 10))
					(write-char ch os))))))
