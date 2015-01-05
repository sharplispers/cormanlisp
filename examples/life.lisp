;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		life.lisp
;;;;	Contents:	This is a simple application to show drawing
;;;;				capabilities. It may be saved as an application:
;;;;					
;;;;				example:
;;;;					(load "examples/life.lisp")
;;;;					(save-application "life" #'win:life)
;;;;
;;;;				If you run this from the IDE, be sure to run
;;;;				it in a separate thread, since it calls EXIT-THREAD.
;;;;
;;;;				example:
;;;;					(th:create-thread #'win:life)
;;;;
;;;;	Author:		Roger Corman
;;;;

(in-package :win32)
(export 'life)

(defconstant szAppName "Life")
(defconstant NULL cl::C_NULL)
(defvar *messages-processed* 0)
(defvar *ps* nil)
(defvar *rect* nil)
(defvar hwnd-save nil)
(defvar ps-save nil)
(defvar *app-window* nil)
(defvar *timer-id* 1)

(defvar *local-rect* (ct:malloc (ct:sizeof 'RECT)))
(defvar *hdc* nil)
(defvar *height* 1)
(defvar *width* 1)
(defvar *client-create-struct* nil)
(defvar IDM_FIRSTCHILD 100)

(defvar *horiz-cells* 15)
(defvar *vert-cells* 15)

(defvar *board* nil)
(defvar *neighbor-counters* nil)

(defvar *refresh-milliseconds* 100)

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
	
(defun create-board ()
	(make-array (list *horiz-cells* *vert-cells*) 
		:initial-element nil))

(defun clear-board ()
	(dotimes (y *vert-cells*)
		(dotimes (x *horiz-cells*)
			(setf (aref *board* x y) nil))))
	
(defun setup-board ()
	(clear-board)
	(setf (aref *board* 7 7) t)
	(setf (aref *board* 6 7) t)
	(setf (aref *board* 8 7) t))

(defun neighbor-count (x y)
	(let ((count 0))
		(if (and (> x 0) (> y 0) (aref *board* (1- x) (1- y)))
			(incf count))	;; upper left
		(if (and (> y 0) (aref *board* x (1- y)))
			(incf count))	;; upper center
		(if (and (< x (1- *horiz-cells*)) (> y 0) (aref *board* (1+ x) (1- y)))
			(incf count))	;; upper right
		(if (and (> x 0) (aref *board* (1- x) y))
			(incf count))	;; middle left
		(if (and (< x (1- *horiz-cells*)) (aref *board* (1+ x) y))
			(incf count))	;; middle right
		(if (and (> x 0) (< y (1- *vert-cells*)) (aref *board* (1- x) (1+ y)))
			(incf count))	;; lower left
		(if (and (< y (1- *vert-cells*)) (aref *board* x (1+ y)))
			(incf count))	;; lower center
		(if (and (< x (1- *horiz-cells*)) (< y (1- *vert-cells*)) (aref *board* (1+ x) (1+ y)))
			(incf count))	;; lower right
		count))		

(defun clear-counters ()
	(dotimes (y *vert-cells*)
		(dotimes (x *horiz-cells*)
			(setf (aref *neighbor-counters* x y) 0))))

(defun set-counters ()
	(dotimes (y *vert-cells*)
		(dotimes (x *horiz-cells*)
			(setf (aref *neighbor-counters* x y) (neighbor-count x y)))))
			
(defun advance-board ()
	(clear-counters)
	(set-counters)
	(dotimes (y *vert-cells*)
		(dotimes (x *horiz-cells*)
			(let ((neighbors (aref *neighbor-counters* x y)))
				(if (and (aref *board* x y) 
						(or (< neighbors 2)(> neighbors 3)))
					(setf (aref *board* x y) nil)	;; cell dies
					(if (and (not (aref *board* x y)) (>= neighbors 3))
						(setf (aref *board* x y) t)))))) ;; cell is born
	#|(let ((count 0))
		(dotimes (y *vert-cells*)
			(dotimes (x *horiz-cells*)
				(if (aref *board* x y) (incf count))))
		(format t "cell count = ~D~%" count)
		(force-output))|#) 
						
(defun draw-colored-rect (left top right bottom r g b)
	(let ((brush (CreateSolidBrush (rgb r g b))))
		(setf (cref RECT *local-rect* left) 	left)
		(setf (cref RECT *local-rect* top) 		top)
		(setf (cref RECT *local-rect* right) 	right)
		(setf (cref RECT *local-rect* bottom) 	bottom)
		(FillRect *hdc* *local-rect* brush)
		(DeleteObject brush)))

(defun draw-colored-ellipse (left top right bottom r g b)
	(let ((brush (CreateSolidBrush (rgb r g b)))
		  (prev-object))
		(setf prev-object (SelectObject *hdc* brush))	
		(Ellipse *hdc* left top right bottom)
		(SelectObject *hdc* prev-object)
		(DeleteObject brush)))

(defun draw-board ()
	(let ((cell-width (truncate *width* *horiz-cells*))
		  (cell-height(truncate *height* *vert-cells*)))
		(dotimes (i (+ *horiz-cells* 1))
			(draw-colored-rect (* cell-width i) 0 
				(+ (* cell-width i) 1) (* cell-height *vert-cells*)
				0 0 0))
		(dotimes (i (+ *vert-cells* 1))
			(draw-colored-rect 0 (* cell-height i) 
				(* cell-width *horiz-cells*) (+ (* cell-height i) 1)
				0 0 0))
		
		(dotimes (y *vert-cells*)
			(dotimes (x *horiz-cells*)
				(if (aref *board* x y)
					(draw-colored-ellipse (+ 2 (* x cell-width)) (+ 2 (* y cell-height))
						(1- (* (+ x 1) cell-width)) (1- (* (+ y 1) cell-height))
						0 0 0))))))

(defun refresh-board ()
	(let ((cell-width (truncate *width* *horiz-cells*))
		  (cell-height(truncate *height* *vert-cells*)))		
		(dotimes (y *vert-cells*)
			(dotimes (x *horiz-cells*)
				(if (aref *board* x y)
					(multiple-value-bind (r g b)(get-rgb)
						(draw-colored-ellipse (+ 2 (* x cell-width)) (+ 2 (* y cell-height))
							(1- (* (+ x 1) cell-width)) (1- (* (+ y 1) cell-height))
							r g b))
					(draw-colored-rect (+ 2 (* x cell-width)) (+ 2 (* y cell-height))
						(1- (* (+ x 1) cell-width)) (1- (* (+ y 1) cell-height))
						255 255 255))))))

(defun toggle-cell (x y)
	(setf (aref *board* x y)(not (aref *board* x y))))

(defun toggle-cell-at-position (x y)
	(let* ((cell-width (truncate *width* *horiz-cells*))
		  (cell-height(truncate *height* *vert-cells*))
		  (cell-x (truncate x cell-width))
		  (cell-y (truncate y cell-height)))
		(if (>= cell-x *horiz-cells*)
			(setf cell-x (1- *horiz-cells*)))
		(if (>= cell-y *vert-cells*)
			(setf cell-y (1- *vert-cells*)))
		(toggle-cell cell-x cell-y)))
		
(defun install-refresh-timer ()
	(win:SetTimer *app-window* *timer-id* *refresh-milliseconds* NULL))
 
(defun uninstall-refresh-timer ()
	(win:KillTimer *app-window* *timer-id*)) 

(defun redraw-board (hwnd)
	(setf *hdc* (BeginPaint hwnd *ps*))
	(GetClientRect hwnd *rect*)
	(refresh-board)
	(advance-board)
	(EndPaint hwnd *ps*)
	(return-from redraw-board 0))
					
(ct:defun-callback WndProc ((hwnd HWND)(iMsg UINT)(wParam WPARAM)(lParam LPARAM))
;;	(setq hwnd-save hwnd)
;;	(setq ps-save *ps*)
	(incf *messages-processed*)
	(let ((*hdc* NULL)) 
		 (cond
			((= iMsg WM_CREATE)
				(setup-board)
				(return-from WndProc 0))
			((= iMsg WM_SIZE)
				(setf *width*  (max (LOWORD lParam) 1))
				(setf *height* (max (HIWORD lParam) 1))
				(return-from WndProc 0))
			((= iMsg WM_PAINT)
				(setf *hdc* (BeginPaint hwnd *ps*))
				(GetClientRect hwnd *rect*)
				(draw-board)
				(advance-board)
				(EndPaint hwnd *ps*)
				(return-from WndProc 0))
			((= iMsg WM_LBUTTONDOWN)
				(let ((x (LOWORD lParam))
					  (y (HIWORD lParam)))
					(toggle-cell-at-position x y)))					
			((= iMsg WM_TIMER)
			 (win:InvalidateRect *app-window* NULL nil)
			 (redraw-board hwnd)
	;		 (win:SendMessage *app-window* WM_PAINT 
	;				(ct:foreign-ptr-to-int (win:GetDC *app-window*)) 0)
			 (return-from WndProc 0))
			((= iMsg WM_DESTROY)
				(uninstall-refresh-timer)
				(PostQuitMessage 0)
				(return-from WndProc 0))
			(t	(return-from WndProc (DefWindowProc hwnd iMsg wParam lParam))))))

(defwinstruct CLIENTCREATESTRUCT
	((hWindowMenu 	HANDLE)
	 (idFirstChild 	UINT)
	))

(defun WinMain (hInstance hPrevInstance szCmdLine iCmdShow)
	(declare (ignore hPrevInstance szCmdLine))
	(let ((*app-window* *app-window*)	;; rebind for per-thread copy
		  (*width* *width*)
		  (*height* *height*)
		  (*board* (create-board))
		  (*neighbor-counters* (make-array (list *horiz-cells* *vert-cells*) :initial-element 0))
		  (msg (ct:malloc (sizeof 'MSG)))
		  (wndclass (ct:malloc (sizeof 'WNDCLASSEX)))
		  (*ps* (ct:malloc (sizeof 'PAINTSTRUCT)))
		  (*rect* (ct:malloc (sizeof 'RECT)))
		  (*client-create-struct* (ct:malloc (ct:sizeof 'CLIENTCREATESTRUCT))))

		(setf (cref WNDCLASSEX wndclass cbSize) (sizeof 'WNDCLASSEX))
		(setf (cref WNDCLASSEX wndclass style) (logior CS_HREDRAW CS_VREDRAW))
		(setf (cref WNDCLASSEX wndclass lpfnWndProc) (get-callback-procinst 'WndProc))
		(setf (cref WNDCLASSEX wndclass cbClsExtra) 0)
		(setf (cref WNDCLASSEX wndclass cbWndExtra) 0)
		(setf (cref WNDCLASSEX wndclass hInstance) hInstance)
		(setf (cref WNDCLASSEX wndclass hIcon) (LoadIcon NULL IDI_APPLICATION))
		(setf (cref WNDCLASSEX wndclass hCursor) (LoadCursor NULL IDC_ARROW))
		(setf (cref WNDCLASSEX wndclass hbrBackground) (GetStockObject WHITE_BRUSH))
		(setf (cref WNDCLASSEX wndclass lpszMenuName) NULL)
		(setf (cref WNDCLASSEX wndclass lpszClassName) (ct:create-c-string szAppName))
		(setf (cref WNDCLASSEX wndclass hIconSm) (LoadIcon NULL IDI_APPLICATION))
		(setf (cref CLIENTCREATESTRUCT *client-create-struct* hWindowMenu) 
			(win::getsubmenu (win:get-main-menu) 3))
		(setf (cref CLIENTCREATESTRUCT *client-create-struct* idFirstChild) IDM_FIRSTCHILD)
		
		(RegisterClassEx wndclass)
		(setq *app-window* 
			(CreateWindowEx 0
				(ct:create-c-string szAppName)				;; window class name
				(ct:create-c-string "Life") 				;; window caption
				WS_OVERLAPPEDWINDOW							;; window style
				CW_USEDEFAULT								;; initial x position
				CW_USEDEFAULT								;; initial y position
				300  ;; CW_USEDEFAULT						;; initial x size
				300  ;; CW_USEDEFAULT						;; initial y size
				(cl::get-application-main-window)			;; parent window handle
				NULL										;; window menu handle
				hInstance									;; program instance handle
				*client-create-struct*))					;; creation parameters

		(install-refresh-timer)
		(ShowWindow *app-window* iCmdShow)
		(UpdateWindow *app-window*)
		(do ((ret (GetMessage msg NULL 0 0)(GetMessage msg NULL 0 0)))
			((not ret))
			(TranslateMessage msg)
			(if (= (cref MSG msg message) WM_QUIT)
				(return))
			(DispatchMessage msg))

		(cref MSG msg wParam)))

(defun life ()
	(restart-case
		(handler-bind ((error (lambda (c) (declare (ignore c)) (invoke-restart 'error))))
			(winmain (cl::get-application-instance) 
				null (ct:create-c-string "") SW_SHOW))
		(error () (return-from life))))


