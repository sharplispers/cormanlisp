;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;	
;;;;	File:		dribble.lisp
;;;;	Contents:	Corman Lisp dribble function.
;;;;	History:	10/8/98  RGC  Created.
;;;;

(in-package "COMMON-LISP")

(defvar *dribble-file* nil)

;;;
;;; Filter *terminal-io* input and output through these functions.
;;;
(defun terminal-io-underflow (stream)
	(let ((ret (cl::console-underflow-function stream)))
		(if *dribble-file*
			(let ((buf (stream-input-buffer stream))
				  (start (stream-input-buffer-pos stream))
				  (num (stream-input-buffer-num stream)))
				(dotimes (i num)
					(write-char (elt buf (+ start i)) *dribble-file*))))
		ret))

(defun terminal-io-overflow (stream)
	(if *dribble-file*
		(let ((buf (stream-output-buffer stream))
			  (num (stream-output-buffer-pos stream)))
			(dotimes (i num)
				(write-char (elt buf i) *dribble-file*))))
	(cl::console-overflow-function stream))

(setf (uref *terminal-io* stream-underflow-func-offset) #'terminal-io-underflow)
(setf (uref *terminal-io* stream-overflow-func-offset) #'terminal-io-overflow)

;;;
;;;	Common Lisp DRIBBLE function
;;;
(defun dribble (&optional pathname)
	(if *dribble-file*
		(close *dribble-file*))
	(if (null pathname)
		(setf *dribble-file* nil)
		(setf *dribble-file* 
			(open pathname 
				:direction :output
				:if-exists :append
				:if-does-not-exist :create))))

				