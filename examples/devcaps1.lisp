;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		devcaps1.lisp
;;;;	Contents:	Simple Windows demonstration app, from Petzold's
;;;;				Programming Windows 95
;;;;
;;;;				It may be saved as an application:
;;;;					
;;;;				example:
;;;;					(load "examples/devcaps1.lisp")
;;;;					(save-application "devcaps1" #'win::devcaps1)
;;;;
;;;;                Or run directly from the IDE:
;;;;                    (th:create-thread #'win::devcaps1)
;;;;
(in-package :win32)

(defconstant szAppName "DevCaps1")

(defstruct info index label desc)

(defvar cxChar)
(defvar cyChar)
(defvar cxCaps)

(defvar *devcaps*
	(list
		(make-info :INDEX HORZSIZE 		:LABEL "HORZSIZE"	:DESC "Width in millimeters:")
		(make-info :INDEX VERTSIZE 		:LABEL "VERTSIZE" 	:DESC "Height in millimeters:")
		(make-info :INDEX HORZRES 		:LABEL "HORZRES" 	:DESC "Width in pixels:")
		(make-info :INDEX VERTRES 		:LABEL "VERTRES" 	:DESC "Height in raster lines:")
		(make-info :INDEX BITSPIXEL 	:LABEL "BITSPIXEL" 	:DESC "Color bits per pixel:")
		(make-info :INDEX PLANES 		:LABEL "PLANES" 	:DESC "Number of color planes:")
		(make-info :INDEX NUMBRUSHES 	:LABEL "NUMBRUSHES" :DESC "Number of device brushes:")
		(make-info :INDEX NUMPENS 		:LABEL "NUMPENS" 	:DESC "Number of device pens:")
		(make-info :INDEX NUMMARKERS 	:LABEL "NUMMARKERS" :DESC "Number of device markers:")
		(make-info :INDEX NUMFONTS 		:LABEL "NUMFONTS" 	:DESC "Number of device fonts:")
		(make-info :INDEX NUMCOLORS 	:LABEL "NUMCOLORS" 	:DESC "Number of device colors:")
		(make-info :INDEX PDEVICESIZE 	:LABEL "PDEVICESIZE" :DESC "Size of device structure:")
		(make-info :INDEX ASPECTX 		:LABEL "ASPECTX" 	:DESC "Relative width of pixel:")
		(make-info :INDEX ASPECTY 		:LABEL "ASPECTY" 	:DESC "Relative height of pixel:")
		(make-info :INDEX ASPECTXY 		:LABEL "ASPECTXY" 	:DESC "Relative diagonal of pixel:")
		(make-info :INDEX LOGPIXELSX 	:LABEL "LOGPIXELSX" :DESC "Horizontal dots per inch:")
		(make-info :INDEX LOGPIXELSY 	:LABEL "LOGPIXELSY" :DESC "Vertical dots per inch:")
		(make-info :INDEX SIZEPALETTE 	:LABEL "SIZEPALETTE" :DESC "Number of palette entries:")
		(make-info :INDEX NUMRESERVED 	:LABEL "NUMRESERVED" :DESC "Reserved palette entries:")
		(make-info :INDEX COLORRES 		:LABEL "COLORRES" 	:DESC "Actual color resolution:")
		(make-info :INDEX RASTERCAPS 	:LABEL "RASTERCAPS" :DESC "RASTERCAPS:")
		))

(defvar *ps* nil)
(defvar *tm* nil)
(defvar *app-window* nil)

(ct:defun-callback WndProc-devcaps1 ((hwnd HWND)(iMsg UINT)(wParam WPARAM)(lParam LPARAM))
	(let ((hdc NULL)
		  (buffer))
		 (cond
			((= iMsg WM_CREATE)
			 (setf hdc (GetDC hwnd))
			 (GetTextMetrics hdc *tm*)
			 (setf cxChar (cref TEXTMETRIC *tm* tmAveCharWidth))
			 (setf cxCaps 
				(*
					(if (evenp (cref TEXTMETRIC *tm* tmPitchAndFamily))
						3
						2)
					(truncate cxChar 2)))
			 (setf cyChar 
				(+ (cref TEXTMETRIC *tm* tmHeight) 
				   (cref TEXTMETRIC *tm* tmExternalLeading)))
			 (ReleaseDC hwnd hdc)
			 (return-from WndProc-devcaps1 0))

			((= iMsg WM_PAINT)
			 (setf hdc (BeginPaint hwnd *ps*))
			 (dotimes (i (length *devcaps*))
				(TextOut 
					hdc 
					cxChar 
					(* cyChar (+ i 1)) 
					(create-c-string (info-label (elt *devcaps* i)))
					(length (info-label (elt *devcaps* i))))
				(TextOut 
					hdc 
					(+ cxChar (* 22 cxCaps)) 
					(* cyChar (+ i 1)) 
					(create-c-string (info-desc (elt *devcaps* i)))
					(length (info-desc (elt *devcaps* i))))
				(SetTextAlign hdc (logior TA_RIGHT TA_TOP))
				(setf buffer 
					(format nil "~5D" 
						(GetDeviceCaps hdc (info-index (elt *devcaps* i)))))
				(TextOut 
					hdc 
					(+ cxChar (* 22 cxCaps) (* cxChar 40)) 
					(* cyChar (+ i 1)) 
					(create-c-string buffer)
					(length buffer))
				(SetTextAlign hdc (logior TA_LEFT TA_TOP)))
			 (EndPaint hwnd *ps*)
			 (return-from WndProc-devcaps1 0))

			((= iMsg WM_DESTROY)
		   	 (PostQuitMessage 0)
			 (return-from WndProc-devcaps1 0))
			
			(t	(return-from WndProc-devcaps1 (DefWindowProc hwnd iMsg wParam lParam))))))

(defun WinMain-devcaps1 (hInstance hPrevInstance szCmdLine iCmdShow)
    (declare (ignore hPrevInstance szCmdLine))
	(let ((*app-window* *app-window*)	;; rebind for per-thread copy
		  (msg (ct:malloc (sizeof 'MSG)))
		  (wndclass (ct:malloc (sizeof 'WNDCLASSEX)))
		  (*ps* (ct:malloc (sizeof 'PAINTSTRUCT)))
		  (*tm* (ct:malloc (sizeof 'TEXTMETRIC))))

		(setf (cref WNDCLASSEX wndclass cbSize) (sizeof 'WNDCLASSEX))
		(setf (cref WNDCLASSEX wndclass style) (logior CS_HREDRAW CS_VREDRAW))
		(setf (cref WNDCLASSEX wndclass lpfnWndProc) (get-callback-procinst 'WndProc-devcaps1))
		(setf (cref WNDCLASSEX wndclass cbClsExtra) 0)
		(setf (cref WNDCLASSEX wndclass cbWndExtra) 0)
		(setf (cref WNDCLASSEX wndclass hInstance) hInstance)
		(setf (cref WNDCLASSEX wndclass hIcon) (LoadIcon NULL IDI_APPLICATION))
		(setf (cref WNDCLASSEX wndclass hCursor) (LoadCursor NULL IDC_ARROW))
		(setf (cref WNDCLASSEX wndclass hbrBackground) (GetStockObject WHITE_BRUSH))
		(setf (cref WNDCLASSEX wndclass lpszMenuName) NULL)
		(setf (cref WNDCLASSEX wndclass lpszClassName) (ct:create-c-string szAppName))
		(setf (cref WNDCLASSEX wndclass hIconSm) (LoadIcon NULL IDI_APPLICATION))
		(RegisterClassEx wndclass)
		(setq *app-window* 
			(CreateWindowEx 0
				(ct:create-c-string szAppName)				;; window class name
				(ct:create-c-string "Device Capabilities") 	;; window caption
				WS_OVERLAPPEDWINDOW							;; window style
				CW_USEDEFAULT								;; initial x position
				CW_USEDEFAULT								;; initial y position
				CW_USEDEFAULT								;; initial x size
				CW_USEDEFAULT								;; initial y size
				(cl::get-application-main-window)			;; parent window handle
				NULL										;; window menu handle
				hInstance									;; program instance handle
				NULL))										;; creation parameters

		(ShowWindow *app-window* iCmdShow)
		(UpdateWindow *app-window*)
		(do ((ret (GetMessage msg NULL 0 0)(GetMessage msg NULL 0 0)))
			((not ret))
			(TranslateMessage msg)
			(if (= (cref MSG msg message) WM_QUIT)
				(return))
			(DispatchMessage msg))

		(cref MSG msg wParam)))

(defun devcaps1 ()
	(restart-case
		(handler-bind ((error (lambda (c) (declare (ignore c)) (invoke-restart 'error))))
			(winmain-devcaps1 (cl::get-application-instance) 
				null (ct:create-c-string "") SW_SHOW))
		(error () #'abort)))


