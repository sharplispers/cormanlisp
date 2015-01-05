;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		Hellowin.lisp
;;;;	Contents:	Simple Windows demonstration app, from Petzold's
;;;;				Programming Windows 95
;;;;
;;;;				It may be saved as an application:
;;;;					
;;;;				example:
;;;;					(load "examples/hellowin.lisp")
;;;;					(save-application "hellowin" #'win::hellowin)
;;;;
;;;;				Make sure the sound file hellowin.wav is in the same
;;;;				directory as the application when you run it.
;;;;

(in-package :win32)

(defconstant szAppName "HelloWin")
(defconstant NULL cl::C_NULL)
(defvar *messages-processed* 0)
(defvar *ps* nil)
(defvar *rect* nil)
(defvar hwnd-save nil)
(defvar ps-save nil)
(defvar *app-window* nil)

(ct:defun-callback WndProc ((hwnd HWND)(iMsg UINT)(wParam WPARAM)(lParam LPARAM))
	(incf *messages-processed*)
	(let ((hdc NULL))
		 (cond
			((= iMsg WM_CREATE)
				(PlaySound (create-c-string "hellowin.wav") 
					NULL (logior SND_FILENAME SND_ASYNC))
				(return-from WndProc 0))
			((= iMsg WM_PAINT)
				(setf hdc (BeginPaint hwnd *ps*))
				(GetClientRect hwnd *rect*)
				(DrawText hdc (ct:create-c-string "Hello, Windows 95!") -1 *rect*
					(logior DT_SINGLELINE DT_CENTER DT_VCENTER))
				(EndPaint hwnd *ps*)
				(return-from WndProc 0))
			((= iMsg WM_DESTROY)
				(PostQuitMessage 0)
				(return-from WndProc 0))
			(t	(return-from WndProc (DefWindowProc hwnd iMsg wParam lParam))))))


(defun WinMain (instance hPrevInstance szCmdLine iCmdShow)
	(declare (ignore hPrevInstance szCmdLine))
	(let ((*app-window* *app-window*)	;; rebind for per-thread copy 
		  (msg (ct:malloc (sizeof 'MSG)))
		  (wndclass (ct:malloc (sizeof 'WNDCLASSEX)))
		  (*ps* (ct:malloc (sizeof 'PAINTSTRUCT)))
		  (*rect* (ct:malloc (sizeof 'RECT))))

		(ct:with-c-struct (s wndclass WNDCLASSEX)
			(setf
				cbSize			(sizeof 'WNDCLASSEX)
				style			(logior CS_HREDRAW CS_VREDRAW)
				lpfnWndProc		(get-callback-procinst 'WndProc)
				cbClsExtra		0
				cbWndExtra		0
				hInstance		instance
				hIcon			(LoadIcon NULL IDI_APPLICATION)
				hCursor			(LoadCursor NULL IDC_ARROW)
				hbrBackground	(GetStockObject WHITE_BRUSH)
				lpszMenuName	NULL
				lpszClassName	(ct:create-c-string szAppName)
				hIconSm			(LoadIcon NULL IDI_APPLICATION)))
		(RegisterClassEx wndclass)
		(setq *app-window* 
			(CreateWindowEx 0
				(ct:create-c-string szAppName)				;; window class name
				(ct:create-c-string "The Hello Program") 	;; window caption
;;				(logior WS_OVERLAPPEDWINDOW	WS_CHILD WS_VISIBLE)		;; window style
				WS_OVERLAPPEDWINDOW							;; window style
				CW_USEDEFAULT								;; initial x position
				CW_USEDEFAULT								;; initial y position
				200  ;; CW_USEDEFAULT						;; initial x size
				200  ;; CW_USEDEFAULT						;; initial y size
				(cl::get-application-main-window)			;; parent window handle
				NULL										;; window menu handle
				instance									;; program instance handle
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

(defun hellowin ()
	(restart-case
		(handler-bind ((error (lambda (c) (declare (ignore c)) (invoke-restart 'error))))
			(WinMain (cl::get-application-instance) 
				null (ct:create-c-string "") SW_SHOW))
		(error () (return-from hellowin))))


