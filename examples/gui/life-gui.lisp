;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	Windows LIFE program.
;;;;    
;;;;    To run from lisp, load this file and enter:
;;;;
;;;;		(th:create-thread #'win::life-gui)
;;;;
;;;;    To create a single-file executable, load this file and enter:
;;;;    (save-application "life-gui" #'win::life-gui :static t)
;;;;

(in-package :win)

(require "GUI")

(defvar *horiz-cells-default* 15)
(defvar *vert-cells-default* 15)
(defvar *timer-id* 1)
(defvar *refresh-milliseconds* 100)

(defclass board ()
	((cells :accessor cells 
			:initform 
			(make-array 
				(list *horiz-cells-default* *vert-cells-default*)
				:initial-element nil))
	 (neighbor-counters :accessor neighbor-counters 
			:initform 
			(make-array 
				(list *horiz-cells-default* *vert-cells-default*)
				:initial-element 0))
	 (view-width :initform 1 :accessor view-width)
	 (view-height :initform 1 :accessor view-height)
	 (paused 
		:accessor paused 
		:initform nil)))

(defun create-board () (make-instance 'board))

(defmethod horiz-cells (board) (array-dimension (cells board) 0))
(defmethod vert-cells  (board) (array-dimension (cells board) 1))

(defun install-refresh-timer (hwnd)
	(win:SetTimer hwnd *timer-id* *refresh-milliseconds* NULL))
 
(defun uninstall-refresh-timer (hwnd)
	(win:KillTimer hwnd *timer-id*)) 

(defclass <life-window> (<main-menu-mixin> <frame>)
	((board
		:accessor board
		:initform (create-board)
		:initarg :board)
	 (rect :accessor life-rect :initform (ct:malloc (sizeof 'RECT)))))

(defun life-gui ()
    (gui-initialize)
	(let* ((window (make-instance '<life-window>)))
		(create-menu window '(:menu "File") nil 1)
		(create-menu window 
			(list :command "Clear"  
				(lambda () 
					(clear-board (board window))
					(redraw-board window)))         
			"File" 1)
		(create-menu window 
			(list :command "Pause"  
				(lambda () (setf (paused (board window)) t))) 	  
			"File" 2)
		(create-menu window 
			(list :command "Resume" 
				(lambda () (setf (paused (board window)) nil)))   
			"File" 3)
		(create-window window
			:caption "life"
			:style (logior WS_OVERLAPPEDWINDOW WS_MINIMIZEBOX)
			:width 300
			:height 300)
    	(setup-board (board window))
    	(install-refresh-timer (window-hwnd window))			
		(show-window window SW_SHOW)
		(update-window window)
		(standard-message-loop)))

(defmethod handle-message ((window <life-window>) (message <create-message>) wparam lparam)
	(declare (ignore message wparam lparam))
    (call-next-method)
	0)

(defmethod handle-message ((window <life-window>) (message <destroy-message>) wparam lparam)
	(declare (ignore message wparam lparam))
	(uninstall-refresh-timer (window-hwnd window))
    (call-next-method)
	0)

(defmethod handle-message ((window <life-window>) (message <paint-message>) wparam lparam)
	(declare (ignore message wparam lparam))
    (begin-paint window)
	(let* ((board (board window)))
		(draw-board board (window-hdc window) (life-rect window))
		(advance-board board))
    (end-paint window)
	0)

(defmethod handle-message ((window <life-window>) (message <size-message>) wparam lparam)
	(declare (ignore window message wparam lparam))
    (call-next-method)
	(let ((board (board window)))
		(setf (view-width board) (max (LOWORD lParam) 1))
		(setf (view-height board) (max (HIWORD lParam) 1)))
	0)

(defmethod handle-message ((window <life-window>) (message <lbuttondown-message>) wparam lparam)
	(declare (ignore message wparam))
    (call-next-method)
	(let ((x (LOWORD lParam))
		  (y (HIWORD lParam)))
		(toggle-cell-at-position (board window) x y))					
	(redraw-board window)
	0)

(defmethod handle-message ((window <life-window>)(message <timer-message>) wparam lparam)
	(declare (ignore message wparam lparam))
    (call-next-method)
	(if (paused (board window))
		(return-from handle-message 0))
	(redraw-board window)
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

;; draw black vertical grid lines
(defun draw-vertical-lines (num cell-width height hdc rect)
	(dotimes (i num)
		(draw-colored-rect (* cell-width i) 0 
			(+ (* cell-width i) 1) height
			0 0 0 hdc rect)))

;; draw black horizontal grid lines
(defun draw-horizontal-lines (num cell-height width hdc rect)
	(dotimes (i num)
		(draw-colored-rect 0 (* cell-height i) 
			width (+ (* cell-height i) 1)
			0 0 0 hdc rect)))

(let ((red 0)(green 0)(blue 0))
	(defun get-rgb ()
		(let* ((r red)
			   (g green)
			   (b blue))
			(incf red 16)
			(when (>= red 256)
				(setf red 0)
				(incf blue 16)) 
			(when (>= blue 256)
				(setf blue 0)
				(incf green 16)) 
			(when (>= green 256)
				(setf green 0))
			(values r g b))))

(defun draw-populated-cells (cells horiz-cells cell-width vert-cells cell-height hdc rect)
	(dotimes (y vert-cells)
		(dotimes (x horiz-cells)
			(multiple-value-bind (r g b)(get-rgb)
				(if (aref cells x y)
					(draw-colored-ellipse 
						(+ 2 (* x cell-width)) 
						(+ 2 (* y cell-height))
						(1- (* (+ x 1) cell-width)) 
						(1- (* (+ y 1) cell-height))
						r g b hdc)
					(draw-colored-rect
						(+ 2 (* x cell-width)) 
						(+ 2 (* y cell-height))
						(1- (* (+ x 1) cell-width)) 
						(1- (* (+ y 1) cell-height))
						255 255 255 hdc rect))))))

(defun draw-board (board hdc rect)
	(let* ((horiz (horiz-cells board))
		   (vert (vert-cells board))
		   (cells (cells board))
		   (cell-width (truncate (view-width board) horiz))
		   (cell-height(truncate (view-height board) vert)))
		(draw-vertical-lines (+ horiz 1) cell-width (* cell-height vert) hdc rect)
		(draw-horizontal-lines (+ vert 1) cell-height (* cell-width horiz) hdc rect)
		(draw-populated-cells cells horiz cell-width vert cell-height hdc rect)))

(defun refresh-board (board hdc rect)
	(let* ((horiz (horiz-cells board))
		   (vert (vert-cells board))
		   (cells (cells board))
		   (cell-width (truncate (view-width board) horiz))
		   (cell-height(truncate (view-height board) vert)))
		(draw-populated-cells cells horiz cell-width vert cell-height hdc rect)))

(defun redraw-board (window)
	(win:InvalidateRect (window-hwnd window) NULL nil))
#|
(defun redraw-board (window)
	(let* ((hwnd (window-hwnd window))
		   (paintstuct (window-paintstruct window))
		   (hdc (BeginPaint hwnd paintstuct))
		   (board (board window)))
	(refresh-board board hdc (life-rect window))
	(advance-board board)
	(EndPaint hwnd paintstuct)
	(return 0)))
|#

(defun clear-board (board)
	(let* ((horiz (horiz-cells board))
		   (vert (vert-cells board))
		   (cells (cells board)))
		(dotimes (y vert)
			(dotimes (x horiz)
				(setf (aref cells x y) nil)))))
	
(defun setup-board (board)
	(clear-board board)
	(setf (aref (cells board) 7 7) t)
	(setf (aref (cells board) 6 7) t)
	(setf (aref (cells board) 8 7) t))

(defun neighbor-count (board x y)
	(let* ((horiz (horiz-cells board))
		   (vert (vert-cells board))
		   (cells (cells board))
		   (count 0))
		(if (and (> x 0) (> y 0) (aref cells (1- x) (1- y)))
			(incf count))	;; upper left
		(if (and (> y 0) (aref cells x (1- y)))
			(incf count))	;; upper center
		(if (and (< x (1- horiz)) (> y 0) (aref cells (1+ x) (1- y)))
			(incf count))	;; upper right
		(if (and (> x 0) (aref cells (1- x) y))
			(incf count))	;; middle left
		(if (and (< x (1- horiz)) (aref cells (1+ x) y))
			(incf count))	;; middle right
		(if (and (> x 0) (< y (1- vert)) (aref cells (1- x) (1+ y)))
			(incf count))	;; lower left
		(if (and (< y (1- vert)) (aref cells x (1+ y)))
			(incf count))	;; lower center
		(if (and (< x (1- horiz)) (< y (1- vert)) (aref cells (1+ x) (1+ y)))
			(incf count))	;; lower right
		count))		

(defun clear-counters (board)
	(let* ((horiz (horiz-cells board))
		   (vert (vert-cells board))
		   (counters (neighbor-counters board)))
		(dotimes (y vert)
			(dotimes (x horiz)
				(setf (aref counters x y) 0)))))

(defun set-counters (board)
	(let* ((horiz (horiz-cells board))
		   (vert (vert-cells board))
		   (counters (neighbor-counters board)))
		(dotimes (y vert)
			(dotimes (x horiz)
				(setf (aref counters x y) (neighbor-count board x y))))))
			
(defun advance-board (board)
	(if (paused board)
		(return-from advance-board))
	(let* ((horiz (horiz-cells board))
		   (vert (vert-cells board))
		   (cells (cells board))
		   (counters (neighbor-counters board)))
		(clear-counters board)
		(set-counters board)
		(dotimes (y vert)
			(dotimes (x horiz)
				(let ((neighbors (aref counters x y)))
					(if (and (aref cells x y)
							(or (< neighbors 2)(> neighbors 3)))
						(setf (aref cells x y) nil)	;; cell dies
					(if (and (not (aref cells x y)) (>= neighbors 3))
						(setf (aref cells x y) t)))))) ;; cell is born
		#|(let ((count 0))
			(dotimes (y vert)
				(dotimes (x horiz)
					(if (aref cells x y) (incf count))))
			(format t "cell count = ~D~%" count)
			(force-output))|#)) 

(defun toggle-cell (board x y)
	(let ((cells (cells board)))
		(setf (aref cells x y)(not (aref cells x y)))))

(defun toggle-cell-at-position (board x y)
	(let* ((horiz (horiz-cells board))
		   (vert (vert-cells board))
		   (cell-width (truncate (view-width board) horiz))
		   (cell-height(truncate (view-height board) vert))
		   (cell-x (truncate x cell-width))
		   (cell-y (truncate y cell-height)))
		(if (>= cell-x horiz)
			(setf cell-x (1- horiz)))
		(if (>= cell-y vert)
			(setf cell-y (1- vert)))
		(toggle-cell board cell-x cell-y)))
