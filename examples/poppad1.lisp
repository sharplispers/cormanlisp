;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		Poppad1.lisp
;;;;	Contents:	Simple Windows demonstration app, from Petzold's
;;;;				Programming Windows 95
;;;;
;;;;				It may be saved as an application:
;;;;					
;;;;				example:
;;;;					(load "examples/poppad1.lisp")
;;;;					(save-application "poppad1" #'poppad1)
;;;;

(in-package :win32)
(defconstant szAppName "PopPad1")
(defvar hwndEdit nil)
(defvar ghinstance nil)
(defvar *app-window* nil)
;;
;; Edit Control Styles
;;
(defwinconstant ES_LEFT                #x0000)
(defwinconstant ES_CENTER              #x0001)
(defwinconstant ES_RIGHT               #x0002)
(defwinconstant ES_MULTILINE           #x0004)
(defwinconstant ES_UPPERCASE           #x0008)
(defwinconstant ES_LOWERCASE           #x0010)
(defwinconstant ES_PASSWORD            #x0020)
(defwinconstant ES_AUTOVSCROLL         #x0040)
(defwinconstant ES_AUTOHSCROLL         #x0080)
(defwinconstant ES_NOHIDESEL           #x0100)
(defwinconstant ES_OEMCONVERT          #x0400)
(defwinconstant ES_READONLY            #x0800)
(defwinconstant ES_WANTRETURN          #x1000)
(defwinconstant ES_NUMBER              #x2000)
(defwinconstant EN_ERRSPACE            #x0500)

#! (:library "user32" :export t  :pascal "WINAPI")
HWND WINAPI SetFocus(HWND hWnd);
ATOM WINAPI RegisterClassA(CONST WNDCLASSA* lpWndClass);
!#
	
(ct:defun-callback WndProc ((hwnd HWND)(iMsg UINT)(wParam WPARAM)(lParam LPARAM))
	(let () 
		 (cond
			((= iMsg WM_CREATE)
				(setf hwndEdit 
					(win:CreateWindowEx
						0 
						(ct:create-c-string "RICHEDIT20A")
						null
						(logior WS_CHILD WS_VISIBLE WS_HSCROLL WS_VSCROLL
                              WS_BORDER ES_LEFT ES_MULTILINE
                              ES_AUTOHSCROLL ES_AUTOVSCROLL)
						0
						0
						100
						100
						hwnd
						(ct:int-to-foreign-ptr 1)
						ghinstance
						NULL))
				0)		
			((= iMsg WM_SETFOCUS)
				(SetFocus hwndEdit)
				0)
			((= iMsg WM_SIZE)
				(MoveWindow hwndEdit 0 0 (LOWORD lParam)(HIWORD lParam) t))		
			((= iMsg WM_COMMAND)
				(if (and (= wParam 1)(= (HIWORD lParam) EN_ERRSPACE))
					(MessageBox hwnd "Edit control out of space." 
						szAppName (logior MB_OK MB_ICONSTOP)))
				0)		
			((= iMsg WM_DESTROY)
				(PostQuitMessage 0)
				0)
			(t	(DefWindowProc hwnd iMsg wParam lParam)))))

(defun WinMain (instance hPrevInstance szCmdLine iCmdShow)
	(declare (ignore hPrevInstance szCmdLine))
	(let ((*app-window* *app-window*)	;; rebind for per-thread copy
		  (msg (ct:malloc (sizeof 'MSG)))
		  (wndclass (ct:malloc (sizeof 'WNDCLASSEX))))
		(setf ghinstance instance)
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
				(ct:create-c-string szAppName) 				;; window caption
				WS_OVERLAPPEDWINDOW							;; window style
				CW_USEDEFAULT								;; initial x position
				CW_USEDEFAULT								;; initial y position
				(truncate (GetSystemMetrics SM_CXSCREEN) 2) ;; initial x size
				(truncate (GetSystemMetrics SM_CYSCREEN) 2)	;; initial y size
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

(defun poppad1 ()
	(restart-case
		(handler-bind ((error (lambda (c) (declare (ignore c)) (invoke-restart 'error))))
			(WinMain (cl::get-application-instance) 
				null (ct:create-c-string "") SW_SHOW))
		(error () (return-from poppad1))))
