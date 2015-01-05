;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	Windows shapes program.
;;;;    
;;;;    To run from lisp, load this file and enter:
;;;;
;;;;		(th:create-thread #'win::shapes)
;;;;
;;;;    To create a single-file executable, load this file and enter:
;;;;    (save-application "shapes" #'win::shapes :static t)
;;;;

(in-package :win)
(require "GUI")

(defvar *timer-id* 1)
(defvar *refresh-milliseconds* 100)

(defun install-refresh-timer (hwnd)
	(win:SetTimer hwnd *timer-id* *refresh-milliseconds* NULL))
 
(defun uninstall-refresh-timer (hwnd)
	(win:KillTimer hwnd *timer-id*)) 

(defclass <shapes-window> (<main-menu-mixin> <frame>)
	((rect :accessor shapes-rect :initform (ct:malloc (sizeof 'RECT)))
	 (paused-flag :initform nil :accessor paused)))

(defun shapes ()
    (gui-initialize)
	(let* ((window (make-instance '<shapes-window>)))
		(create-menu window '(:menu "File") nil 1)
		(create-menu window 
			(list :command "Pause"  (lambda () (setf (paused window) t))) 	  
			"File" 1)
		(create-menu window 
			(list :command "Resume" (lambda () (setf (paused window) nil)))   
			"File" 2)
		(create-window window
			:caption "Shapes"
			:style (logior WS_OVERLAPPEDWINDOW WS_MINIMIZEBOX))		
	    (install-refresh-timer (window-hwnd window))
   		(show-window window SW_SHOW)
		(update-window window)
		(standard-message-loop)))

(defmethod handle-message ((window <shapes-window>) (message <create-message>) wparam lparam)
	(declare (ignore message wparam lparam))
	0)

(defmethod handle-message ((window <shapes-window>) (message <destroy-message>) wparam lparam)
	(declare (ignore message wparam lparam))
	(uninstall-refresh-timer (window-hwnd window))
	0)

(defmethod handle-message ((window <shapes-window>) (message <paint-message>) wparam lparam)
	(declare (ignore message wparam lparam))
    (begin-paint window)
	(draw-multiple-rects window)
	(end-paint window)
    0)

(defmethod handle-message ((window <shapes-window>)(message <timer-message>) wparam lparam)
	(declare (ignore message wparam lparam))
	(unless (paused window)
		(win:InvalidateRect (window-hwnd window) NULL nil))
	0)

;;; Simplify temporary selection of a Win32 GUI object.
;;; Takes care of selecting object, restoring old selection,
;;; and calling DeleteObject.
(defmacro with-selected-object ((name hdc init) . rest)
	(let ((prev-sym (gensym))
		  (hdc-sym (gensym)))
    	`(let* ((,name ,init)
				(,prev-sym)
				(,hdc-sym ,hdc))
			(setf ,prev-sym (SelectObject ,hdc-sym ,name))
			(unwind-protect (progn ,@rest)
				(SelectObject ,hdc-sym ,prev-sym)
				(DeleteObject ,name)))))

(defun draw-colored-rect (x1 y1 x2 y2 r g b hdc rect)
	(ct:with-c-struct (s rect RECT)
		(setf left x1 top y1 right x2 bottom y2))
	(with-selected-object (brush hdc (CreateSolidBrush (rgb r g b)))
		(FillRect hdc rect brush)))

(defun draw-colored-ellipse (left top right bottom r g b hdc)
	(with-selected-object (brush hdc (CreateSolidBrush (rgb r g b)))
		(Ellipse hdc left top right bottom)))

(defun draw-multiple-rects (window)
	(let ((hdc (window-hdc window))
		  (rect (shapes-rect window))
		  (width (width window))
		  (height (height window)))
		(dotimes (i 100)
			(let* ((left (random width))
				   (top  (random height))
				   (right (min (+ left (random width)) width))
				   (bottom (min (+ top (random height)) height))
				   (r (random 256))
				   (g (random 256))
				   (b (random 256)))
				(if (> (random 2) 0)
					(draw-colored-rect left top right bottom r g b hdc rect)
					(draw-colored-ellipse left top right bottom r g b hdc))))))

