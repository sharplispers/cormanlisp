;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	Windows Bouncing Ball program.
;;;;    
;;;;    To run, load this file and enter:
;;;;
;;;;		(th:create-thread #'win::bounce)
;;;;

(in-package :win)

(require "GUI")

(defvar *timer-id* 1)
(defvar *refresh-milliseconds* 10)

(defstruct ball
	(x-position 0)
	 (y-position 0)
	 (width 0)
	 (height 0)
	 (x-velocity 0)
	 (y-velocity 0))

(defun install-refresh-timer (hwnd)
	(win:SetTimer hwnd *timer-id* *refresh-milliseconds* NULL))
 
(defun uninstall-refresh-timer (hwnd)
	(win:KillTimer hwnd *timer-id*)) 

(defclass <bouncing-window> (<frame>)
	((ball
		:accessor ball
		:initform (make-ball :x-velocity 4 :y-velocity 4)
		:initarg :ball)
     (rect :accessor bouncing-rect :initform (ct:malloc (sizeof 'RECT)))
     (save-x1 :accessor save-x1 :initform 0)
     (save-y1 :accessor save-y1 :initform 0)
     (save-x2 :accessor save-x2 :initform 0)
     (save-y2 :accessor save-y2 :initform 0)))

(defun bounce ()
    (gui-initialize)
	(let* ((window (make-instance '<bouncing-window>)))
		(create-window window
			:caption "bouncing ball"
			:style (logior WS_OVERLAPPEDWINDOW WS_MINIMIZEBOX)
			:width 300
			:height 300)
	    (install-refresh-timer (window-hwnd window))			
		(show-window window SW_SHOW)
		(update-window window)
		(standard-message-loop)))

(defmethod handle-message ((window <bouncing-window>) (message <create-message>) wparam lparam)
	(declare (ignore message wparam lparam))
    (call-next-method)
	0)

(defmethod handle-message ((window <bouncing-window>) (message <destroy-message>) wparam lparam)
	(declare (ignore message wparam lparam))
	(uninstall-refresh-timer (window-hwnd window))
    (call-next-method)
	0)

(defmethod handle-message ((window <bouncing-window>) (message <paint-message>) wparam lparam)
	(declare (ignore message wparam lparam))
    (begin-paint window)
	(redraw-ball window)
    (end-paint window)
	0)

(defmethod handle-message ((window <bouncing-window>) (message <size-message>) wparam lparam)
	(declare (ignore window message wparam lparam))
	(call-next-method)
	(let ((ball (ball window)))
		(setf (ball-width ball) (max (truncate (LOWORD lParam) 20) 1)
			  (ball-height ball) (max (truncate (HIWORD lParam) 20) 1))
		(setf (ball-x-velocity ball) (ball-width ball)
			  (ball-y-velocity ball) (ball-height ball))))

(defmethod handle-message ((window <bouncing-window>) (message <lbuttondown-message>) wparam lparam)
	(declare (ignore message wparam))
	(let ((x (LOWORD lParam))
		  (y (HIWORD lParam)))
		(move-ball-to-position (ball window) x y))					
	0)

(defmethod handle-message ((window <bouncing-window>)(message <timer-message>) wparam lparam)
	(declare (ignore message wparam lparam))
	(win:InvalidateRect (window-hwnd window) NULL nil)
    (call-next-method)
	0)

(defun draw-colored-rect (x1 y1 x2 y2 r g b hdc rect)
	(let ((brush (CreateSolidBrush (rgb r g b))))
		(with-c-struct (s rect RECT)
			(setf left   x1
				  top    y1
				  right  x2
				  bottom y2)
			(FillRect hdc rect brush)
			(DeleteObject brush))))

(defun draw-colored-ellipse (left top right bottom r g b hdc)
	(let ((brush (CreateSolidBrush (rgb r g b)))
		  (prev-object))
		(setf prev-object (SelectObject hdc brush))	
		(Ellipse hdc left top right bottom)
		(SelectObject hdc prev-object)
		(DeleteObject brush)))

(defun draw-ball (ball hdc rect window)
	(let* ((width (ball-width ball))
		   (height (ball-height ball))
		   (x (ball-x-position ball))
		   (y (ball-y-position ball)))
		(draw-colored-rect (save-x1 window) (save-y1 window) 
            (save-x2 window) (save-y2 window) 
            255 255 255 hdc rect)
		(draw-colored-ellipse x y (+ x width) (+ y height) 255 0 0 hdc)
		(setf (save-x1 window) x (save-y1 window) y 
            (save-x2 window) (+ x width) (save-y2 window) (+ y height))))

(defun advance-ball (ball window)
	(let ((x (ball-x-position ball))
		  (y (ball-y-position ball))
		  (ball-width (ball-width ball))
		  (ball-height (ball-height ball))
		  (x-velocity (ball-x-velocity ball))
		  (y-velocity (ball-y-velocity ball))
		  (window-width (width window))
		  (window-height (height window)))
		(incf x x-velocity)
		(incf y y-velocity)
		(when (< x 0)
			(setf (ball-x-velocity ball) (- x-velocity) x 0))
		(when (> (+ x ball-width) window-width)
			(setf (ball-x-velocity ball) (- x-velocity) x (- window-width ball-width)))
		(when (< y 0)
			(setf (ball-y-velocity ball) (- y-velocity) y 0))
		(when (> (+ y ball-height) window-height)
			(setf (ball-y-velocity ball) (- y-velocity) y (- window-height ball-height)))
		(setf (ball-x-position ball) x (ball-y-position ball) y)))
		
(defun redraw-ball (window)
	(let* ((ball (ball window)))
	(draw-ball ball (window-hdc window) (bouncing-rect window) window)
	(advance-ball ball window)
	0))

(defun move-ball-to-position (ball x y)
	(setf (ball-x-position ball) x (ball-y-position ball) y))

