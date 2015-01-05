;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		sysmets3.lisp
;;;;	Contents:	Simple Windows demonstration app, from Petzold's
;;;;				Programming Windows 95
;;;;
;;;;				It may be saved as an application:
;;;;					
;;;;				example:
;;;;					(load "examples/sysmets3.lisp")
;;;;					(save-application "sysmets3" #'win::sysmets3)
;;;;

(in-package :win32)

(defconstant szAppName "SysMets3")
(defvar *ps* nil)
(defvar *tm* nil)

(defvar cxChar 0)
(defvar cxCaps 0) 
(defvar cyChar 0) 
(defvar cxClient 0)
(defvar cyClient 0)
(defvar iMaxWidth 0)
(defvar iVscrollPos 0)
(defvar iVscrollMax 0)
(defvar iHscrollPos 0)
(defvar iHscrollMax 0)

(defstruct info index label desc)

(defvar *sysmetrics*
	(list
		(make-info :INDEX SM_CXSCREEN 		:LABEL "SM_CXSCREEN"		:DESC "Screen width in pixels")
		(make-info :INDEX SM_CYSCREEN 		:LABEL "SM_CYSCREEN"		:DESC "Screen height in pixels")
		(make-info :INDEX SM_CXVSCROLL 		:LABEL "SM_CXVSCROLL"		:DESC "Vertical scroll arrow width")
		(make-info :INDEX SM_CYHSCROLL 		:LABEL "SM_CYHSCROLL"		:DESC "Horizontal scroll arrow height")
		(make-info :INDEX SM_CYCAPTION 		:LABEL "SM_CYCAPTION"		:DESC "Caption bar height")
		(make-info :INDEX SM_CXBORDER 		:LABEL "SM_CXBORDER"		:DESC "Window border width")
		(make-info :INDEX SM_CYBORDER 		:LABEL "SM_CYBORDER"		:DESC "Window border height")
		(make-info :INDEX SM_CXDLGFRAME 	:LABEL "SM_CXDLGFRAME"		:DESC "Dialog window frame width")
		(make-info :INDEX SM_CYDLGFRAME 	:LABEL "SM_CYDLGFRAME"		:DESC "Dialog window frame height")
		(make-info :INDEX SM_CYVTHUMB 		:LABEL "SM_CYVTHUMB"		:DESC "Vertical scroll thumb height")
		(make-info :INDEX SM_CXHTHUMB 		:LABEL "SM_CXHTHUMB"		:DESC "Horizontal scroll thumb width")
		(make-info :INDEX SM_CXICON 		:LABEL "SM_CXICON"			:DESC "Icon width")
		(make-info :INDEX SM_CYICON 		:LABEL "SM_CYICON"			:DESC "Icon height")
		(make-info :INDEX SM_CXCURSOR 		:LABEL "SM_CXCURSOR"		:DESC "Cursor width")
		(make-info :INDEX SM_CYCURSOR 		:LABEL "SM_CYCURSOR"		:DESC "Cursor height")
		(make-info :INDEX SM_CYMENU 		:LABEL "SM_CYMENU"			:DESC "Menu bar height")
		(make-info :INDEX SM_CXFULLSCREEN 	:LABEL "SM_CXFULLSCREEN"	:DESC "Full screen client area width")
		(make-info :INDEX SM_CYFULLSCREEN 	:LABEL "SM_CYFULLSCREEN"	:DESC "Full screen client area height")
		(make-info :INDEX SM_CYKANJIWINDOW 	:LABEL "SM_CYKANJIWINDOW"	:DESC "Kanji window height")
		(make-info :INDEX SM_MOUSEPRESENT 	:LABEL "SM_MOUSEPRESENT "	:DESC "Mouse present flag")
		(make-info :INDEX SM_CYVSCROLL 		:LABEL "SM_CYVSCROLL"		:DESC "Vertical scroll arrow height")
		(make-info :INDEX SM_CXHSCROLL 		:LABEL "SM_CXHSCROLL"		:DESC "Horizontal scroll arrow width")
		(make-info :INDEX SM_DEBUG 			:LABEL "SM_DEBUG"			:DESC "Debug version flag")
		(make-info :INDEX SM_SWAPBUTTON 	:LABEL "SM_SWAPBUTTON"		:DESC "Mouse buttons swapped flag")
		(make-info :INDEX SM_RESERVED1 		:LABEL "SM_RESERVED1"		:DESC "Reserved")
		(make-info :INDEX SM_RESERVED2 		:LABEL "SM_RESERVED2"		:DESC "Reserved")
		(make-info :INDEX SM_RESERVED3 		:LABEL "SM_RESERVED3"		:DESC "Reserved")
		(make-info :INDEX SM_RESERVED4 		:LABEL "SM_RESERVED4"		:DESC "Reserved")
		(make-info :INDEX SM_CXMIN 			:LABEL "SM_CXMIN"			:DESC "Minimum window width")
		(make-info :INDEX SM_CYMIN 			:LABEL "SM_CYMIN"			:DESC "Minimum window height")
		(make-info :INDEX SM_CXSIZE 		:LABEL "SM_CXSIZE"			:DESC "Minimize/Maximize icon width")
		(make-info :INDEX SM_CYSIZE 		:LABEL "SM_CYSIZE"			:DESC "Minimize/Maximize icon height")
		(make-info :INDEX SM_CXFRAME 		:LABEL "SM_CXFRAME"			:DESC "Window frame width")
		(make-info :INDEX SM_CYFRAME 		:LABEL "SM_CYFRAME"			:DESC "Window frame height")
		(make-info :INDEX SM_CXMINTRACK 	:LABEL "SM_CXMINTRACK "		:DESC "Minimum window tracking width")
		(make-info :INDEX SM_CYMINTRACK 	:LABEL "SM_CYMINTRACK "		:DESC "Minimum window tracking height")
		(make-info :INDEX SM_CXDOUBLECLK 	:LABEL "SM_CXDOUBLECLK"		:DESC "Double click x tolerance")
		(make-info :INDEX SM_CYDOUBLECLK 	:LABEL "SM_CYDOUBLECLK "	:DESC "Double click y tolerance")
		(make-info :INDEX SM_CXICONSPACING 	:LABEL "SM_CXICONSPACING"	:DESC "Horizontal icon spacing")
		(make-info :INDEX SM_CYICONSPACING 	:LABEL "SM_CYICONSPACING "	:DESC "Vertical icon spacing")
		(make-info :INDEX SM_MENUDROPALIGNMENT :LABEL "SM_MENUDROPALIGNMENT" :DESC "Left or right menu drop")
		(make-info :INDEX SM_PENWINDOWS 	:LABEL "SM_PENWINDOWS"		:DESC "Pen extensions installed")
		(make-info :INDEX SM_DBCSENABLED 	:LABEL "SM_DBCSENABLED"		:DESC "Double-Byte Char Set enabled")
		(make-info :INDEX SM_CMOUSEBUTTONS 	:LABEL "SM_CMOUSEBUTTONS "	:DESC "Number of mouse buttons")
		(make-info :INDEX SM_SHOWSOUNDS 	:LABEL "SM_SHOWSOUNDS "		:DESC "Present sounds visually")))

(defvar NUMLINES (length *sysmetrics*))
(dotimes (i NUMLINES)
	(let ((s (elt *sysmetrics* i)))
		(setf (info-label s) (ct:create-c-string (info-label s)))
		(setf (info-desc s) (ct:create-c-string (info-desc s)))))

(ct:defun-callback WndProc-sysmets3 ((hwnd HWND)(iMsg UINT)(wParam WPARAM)(lParam LPARAM))
	(let ((hdc NULL)
		  (buffer)
		  x y
		  (iHscrollInc 0) 
		  (iVscrollInc 0)
		  iPaintBeg iPaintEnd)
	
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
				(setq iMaxWidth (+ (* 40 cxChar)(* 22 cxCaps)))
			 	(return-from WndProc-sysmets3 0))

			((= iMsg WM_SIZE)
			;	(MessageBeep #xffffffff)
				(setf cxClient (LOWORD lParam))
				(setf cyClient (HIWORD lParam))

				(setf iVscrollMax (max 0 (+ NUMLINES (- 2 (truncate cyClient cyChar)))))
				(setf iVscrollPos (min iVscrollPos iVscrollMax))

            	(SetScrollRange hwnd SB_VERT 0 iVscrollMax FALSE)
            	(SetScrollPos hwnd SB_VERT iVscrollPos TRUE)

				(setf iHscrollMax (max 0 (+ 2 (truncate (- iMaxWidth cxClient) cxChar))))
				(setf iHscrollPos (min iHscrollPos iHscrollMax))

            	(SetScrollRange hwnd SB_HORZ 0 iHscrollMax FALSE)
            	(SetScrollPos   hwnd SB_HORZ iHscrollPos TRUE)
				(return-from WndProc-sysmets3 0))
		
			((= iMsg WM_VSCROLL)
				(let ((scrollcmd (LOWORD wParam)))
					(cond
						((= SB_TOP scrollcmd) (setf iVscrollInc (- iVscrollPos)))
						((= SB_BOTTOM scrollcmd) (setf iVscrollInc (- iVscrollMax iVscrollPos)))
						((= SB_LINEUP scrollcmd)(setf iVscrollInc -1))
						((= SB_LINEDOWN scrollcmd) (setf iVscrollInc 1))
						((= SB_PAGEUP scrollcmd) (setf iVscrollInc (min -1 (truncate (- cyClient) cyChar))))
						((= SB_PAGEDOWN scrollcmd) (setf iVscrollInc (max 1 (truncate cyClient cyChar))))
						((= SB_THUMBTRACK scrollcmd) (setf iVscrollInc (- (HIWORD wParam) iVscrollPos)))))
				(setf iVscrollInc (max (- iVscrollPos) (min iVscrollInc (- iVscrollMax iVscrollPos))))

				(unless (zerop iVscrollInc)
					(incf iVscrollPos iVscrollInc)
					(ScrollWindow hwnd 0 (* (- cyChar) iVscrollInc) NULL NULL)
					(SetScrollPos hwnd SB_VERT iVscrollPos TRUE)
					(UpdateWindow hwnd))
				(return-from WndProc-sysmets3 0))

			((= iMsg WM_HSCROLL)
				(let ((scrollcmd (LOWORD wParam)))
					(cond
						((= SB_LINEUP scrollcmd)(setf iHscrollInc -1))
						((= SB_LINEDOWN scrollcmd) (setf iHscrollInc 1))
						((= SB_PAGEUP scrollcmd) (setf iHscrollInc -8))
						((= SB_PAGEDOWN scrollcmd) (setf iHscrollInc 8))
						((= SB_THUMBPOSITION scrollcmd) (setf iHscrollInc (- (HIWORD wParam) iHscrollPos)))))
				(setf iHscrollInc (max (- iHscrollPos) (min iHscrollInc (- iHscrollMax iHscrollPos))))

				(unless (zerop iHscrollInc)
					(incf iHscrollPos iHscrollInc)
					(ScrollWindow hwnd (* (- cxChar) iHscrollInc) 0 NULL NULL)
					(SetScrollPos hwnd SB_HORZ iHscrollPos TRUE)
					(UpdateWindow hwnd))
				(return-from WndProc-sysmets3 0))

			((= iMsg WM_PAINT)
				(setf hdc (BeginPaint hwnd *ps*))

				(setf iPaintBeg 
					(max 0 
						(+ iVscrollPos 
							(- (truncate (cref RECT (cref PAINTSTRUCT *ps* rcPaint) top) cyChar) 1))))
				(setf iPaintEnd 
					(min NUMLINES
						(+ iVscrollPos 
							(truncate (cref RECT (cref PAINTSTRUCT *ps* rcPaint) bottom) cyChar))))

				(do ((i iPaintBeg (+ i 1)))
					((= i iPaintEnd))
					(setf x (* cxChar (- 1 iHscrollPos)))
					(setf y (* cyChar (+ (- 1 iVscrollPos) i)))

					(TextOut 
						hdc 
						x 
						y 
						(info-label (elt *sysmetrics* i))
						(ct:c-string-length (info-label (elt *sysmetrics* i))))

					(TextOut 
						hdc 
						(+ x (* 22 cxCaps)) 
						y 
						(info-desc (elt *sysmetrics* i))
						(ct:c-string-length  (info-desc (elt *sysmetrics* i))))

					(SetTextAlign hdc (logior TA_RIGHT TA_TOP))
					(setf buffer 
						(format nil "~5D" 
							(GetSystemMetrics (info-index (elt *sysmetrics* i)))))
					(TextOut 
						hdc 
						(+ x (* 22 cxCaps) (* cxChar 40)) 
						y
						(create-c-string buffer)
						(length buffer))
					(SetTextAlign hdc (logior TA_LEFT TA_TOP)))
				(EndPaint hwnd *ps*)
				(return-from WndProc-sysmets3 0))

			((= iMsg WM_DESTROY)
				(PostQuitMessage 0)
				(return-from WndProc-sysmets3 0))
			
			(t	(return-from WndProc-sysmets3 (DefWindowProc hwnd iMsg wParam lParam))))))


(defun WinMain-sysmets3 (hInstance hPrevInstance szCmdLine iCmdShow)
    (declare (ignore szCmdLine hPrevInstance))
	(let ((hwnd NULL)
		  (msg (ct:malloc (sizeof 'MSG)))
		  (wndclass (ct:malloc (sizeof 'WNDCLASSEX)))
		  (*ps* (ct:malloc (sizeof 'PAINTSTRUCT)))
		  (*tm* (ct:malloc (sizeof 'TEXTMETRIC))))

		(setf (cref WNDCLASSEX wndclass cbSize) (sizeof 'WNDCLASSEX))
		(setf (cref WNDCLASSEX wndclass style) (logior CS_HREDRAW CS_VREDRAW))
		(setf (cref WNDCLASSEX wndclass lpfnWndProc) (get-callback-procinst 'WndProc-sysmets3))
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
		(setq hwnd 
			(CreateWindowEx 0
				(ct:create-c-string szAppName)				;; window class name
				(ct:create-c-string "Get System Metrics No. 3") ;; window caption
				(logior WS_OVERLAPPEDWINDOW WS_VSCROLL WS_HSCROLL) ;; window style
				CW_USEDEFAULT								;; initial x position
				CW_USEDEFAULT								;; initial y position
				CW_USEDEFAULT								;; initial x size
				CW_USEDEFAULT								;; initial y size
				(cl::get-application-main-window)			;; parent window handle
				NULL										;; window menu handle
				hInstance									;; program instance handle
				NULL))										;; creation parameters

		(ShowWindow hwnd iCmdShow)
		(UpdateWindow hwnd)
		(do ((ret (GetMessage msg NULL 0 0)(GetMessage msg NULL 0 0)))
			((not ret))
			(TranslateMessage msg)
			(if (= (cref MSG msg message) WM_QUIT)
				(return))
			(DispatchMessage msg))

		(cref MSG msg wParam)))

(defun sysmets3 ()
	(restart-case
		(handler-bind ((error (lambda (c) (declare (ignore c)) (invoke-restart 'error))))
			(WinMain-sysmets3 (cl::get-application-instance) 
				null (ct:create-c-string "") SW_SHOW))
		(error () (return-from sysmets3))))
