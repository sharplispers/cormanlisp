;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	Windows Hello program.
;;;;    
;;;;    To run from lisp, load this file and enter:
;;;;
;;;;		(th:create-thread #'win::hello)
;;;;
;;;;    To create a single-file executable, load this file and enter:
;;;;    (save-application "hello" #'win::hello :static t)
;;;;

(in-package :win)
(require "GUI")	

(defclass <hello-window> (<frame>)
	((text-rect :accessor hello-text-rect :initform (ct:malloc (sizeof 'RECT)))))

(defun hello ()
    (gui-initialize)
	(let ((window (make-instance '<hello-window>)))
		(create-window window
			:caption "hello"
			:style (logior WS_OVERLAPPEDWINDOW WS_MINIMIZEBOX))			
		(show-window window SW_SHOW)
		(update-window window)
		(standard-message-loop)))

(defmethod handle-message ((window <hello-window>) (message <paint-message>) wparam lparam)
	(declare (ignore wparam lparam))
    (begin-paint window)
	(let ((rect (hello-text-rect window)))
		(GetClientRect (window-hwnd window) rect)
		(DrawText (window-hdc window) 
			(ct:create-c-string "Hello, World") 
			-1 
			rect
			(logior DT_SINGLELINE DT_CENTER DT_VCENTER)))
    (end-paint window)
	0)
