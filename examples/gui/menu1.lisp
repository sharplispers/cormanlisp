;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	Windows Menu example program.
;;;;    
;;;;    To run from lisp, load this file and enter:
;;;;
;;;;		(th:create-thread #'win::menu1)
;;;;
;;;;    To create a single-file executable, load this file and enter:
;;;;    (save-application "menu1" #'win::menu1 :static t)
;;;;

(in-package :win)
(require "GUI")

(defclass <menu-window> (<main-menu-mixin> <frame>))
                
(defun my-open-file ()
	(format t "open file selected~%")
	(force-output))

(defun menu1 ()
    (gui-initialize)
	(let ((window (make-instance '<menu-window>)))
		(create-menu window '(:menu "File") nil 1)
		(create-menu window (list :command "Open" 'my-open-file) "File" 1)
		(create-window window
			:caption "test window"
			:style (logior WS_OVERLAPPEDWINDOW WS_MINIMIZEBOX)
			:width 300
			:height 300)
		(show-window window SW_SHOW)
		(update-window window)
		(standard-message-loop)))