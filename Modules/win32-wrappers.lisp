;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	All rights reserved.
;;;;	-------------------------------
;;;;
;;;;	Win32 wrappers
;;;;

(require :winbase)

(defun get-user-name ()
	(let ((namebuf (ct:malloc 128))
		  (sizebuf (ct:malloc (ct:sizeof :long))))
		(setf (ct:cref (win:DWORD *) sizebuf 0) 128)
		(let ((result (win:GetUserName namebuf sizebuf)))
			(if (/= result 0)
				(ct:c-string-to-lisp-string namebuf)))))

