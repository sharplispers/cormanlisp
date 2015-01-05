;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	All rights reserved.
;;;;	-------------------------------
;;;;
;;;;	File:		gui.lisp
;;;;	Contents:	Graphic User Interface framework for Corman Lisp.
;;;;	History:	7/18/01  RGC  Created.
;;;;				8/15/01  RGC  Enhanced for release with version 1.5.
;;;;
;;;;	This is currently under development and is likely to change
;;;;    substantially in the next release!
;;;;

(in-package :win)
(provide "GUI")
(require :winutil)

(defconstant HWND_TOP       (int-to-foreign-ptr 0))
(defconstant HWND_BOTTOM    (int-to-foreign-ptr 1))
(defconstant HWND_TOPMOST   (int-to-foreign-ptr -1))
(defconstant HWND_NOTOPMOST (int-to-foreign-ptr -2))

#! (:export t :library "comctl32" :pascal "WINAPI" :ignore "WINCOMMCTRLAPI")
#define STATUSCLASSNAMEA        "msctls_statusbar32"
typedef struct tagINITCOMMONCONTROLSEX {
    DWORD dwSize;             // size of this structure
    DWORD dwICC;              // flags indicating which classes to be initialized
} INITCOMMONCONTROLSEX, *LPINITCOMMONCONTROLSEX;
WINCOMMCTRLAPI BOOL WINAPI InitCommonControlsEx(LPINITCOMMONCONTROLSEX);
WINCOMMCTRLAPI void WINAPI InitCommonControls();

#define ICC_LISTVIEW_CLASSES   0x00000001 // listview, header
#define ICC_TREEVIEW_CLASSES   0x00000002 // treeview, tooltips
#define ICC_BAR_CLASSES        0x00000004 // toolbar, statusbar, trackbar, tooltips
#define ICC_TAB_CLASSES        0x00000008 // tab, tooltips
#define ICC_UPDOWN_CLASS       0x00000010 // updown
#define ICC_PROGRESS_CLASS     0x00000020 // progress
#define ICC_HOTKEY_CLASS       0x00000040 // hotkey
#define ICC_ANIMATE_CLASS      0x00000080 // animate
#define ICC_WIN95_CLASSES      0x000000FF
#define ICC_DATE_CLASSES       0x00000100 // month picker, date picker, time picker, updown
#define ICC_USEREX_CLASSES     0x00000200 // comboex
#define ICC_COOL_CLASSES       0x00000400 // rebar (coolbar) control
#define ICC_INTERNET_CLASSES   0x00000800
#define ICC_PAGESCROLLER_CLASS 0x00001000   // page scroller
#define ICC_NATIVEFNTCTL_CLASS 0x00002000   // native font control
#define ICC_STANDARD_CLASSES   0x00004000
#define ICC_LINK_CLASS         0x00008000

!#

#! (:export t :library "user32" :pascal "WINAPI")
/*
 * WM_SIZE message wParam values
 */
#define SIZE_RESTORED       0
#define SIZE_MINIMIZED      1
#define SIZE_MAXIMIZED      2
#define SIZE_MAXSHOW        3
#define SIZE_MAXHIDE        4

/*
 * SetWindowPos Flags
 */
#define SWP_NOSIZE          0x0001
#define SWP_NOMOVE          0x0002
#define SWP_NOZORDER        0x0004
#define SWP_NOREDRAW        0x0008
#define SWP_NOACTIVATE      0x0010
#define SWP_FRAMECHANGED    0x0020  /* The frame changed: send WM_NCCALCSIZE */
#define SWP_SHOWWINDOW      0x0040
#define SWP_HIDEWINDOW      0x0080
#define SWP_NOCOPYBITS      0x0100
#define SWP_NOOWNERZORDER   0x0200  /* Don't do owner Z ordering */
#define SWP_NOSENDCHANGING  0x0400  /* Don't send WM_WINDOWPOSCHANGING */
#define SWP_DRAWFRAME       SWP_FRAMECHANGED
#define SWP_NOREPOSITION    SWP_NOOWNERZORDER
#define SWP_DEFERERASE      0x2000
#define SWP_ASYNCWINDOWPOS  0x4000

/*
 * Window field offsets for GetWindowLong()
 */
#define GWL_WNDPROC         -4
#define GWL_HINSTANCE       -6
#define GWL_HWNDPARENT      -8
#define GWL_STYLE           -16
#define GWL_EXSTYLE         -20
#define GWL_USERDATA        -21
#define GWL_ID              -12

#define WM_QUERYUISTATE                 0x0129

typedef void* HACCEL;

/*
 * Window information snapshot
 */
typedef struct tagWINDOWINFO
{
    DWORD cbSize;
    RECT  rcWindow;
    RECT  rcClient;
    DWORD dwStyle;
    DWORD dwExStyle;
    DWORD dwWindowStatus;
    UINT  cxWindowBorders;
    UINT  cyWindowBorders;
    ATOM  atomWindowType;
    WORD  wCreatorVersion;
} WINDOWINFO, *PWINDOWINFO, *LPWINDOWINFO;

/*
 * Dialog Styles
 */
#define DS_ABSALIGN         0x01L
#define DS_SYSMODAL         0x02L
#define DS_LOCALEDIT        0x20L   /* Edit items get Local storage. */
#define DS_SETFONT          0x40L   /* User specified font for Dlg controls */
#define DS_MODALFRAME       0x80L   /* Can be combined with WS_CAPTION  */
#define DS_NOIDLEMSG        0x100L  /* WM_ENTERIDLE message will not be sent */
#define DS_SETFOREGROUND    0x200L  /* not in win3.1 */
#define DS_3DLOOK           0x0004L
#define DS_FIXEDSYS         0x0008L
#define DS_NOFAILCREATE     0x0010L
#define DS_CONTROL          0x0400L
#define DS_CENTER           0x0800L
#define DS_CENTERMOUSE      0x1000L
#define DS_CONTEXTHELP      0x2000L
#define DS_SHELLFONT        (DS_SETFONT | DS_FIXEDSYS)

/*
 * Button Control Styles
 */
#define BS_PUSHBUTTON       0x00000000L
#define BS_DEFPUSHBUTTON    0x00000001L
#define BS_CHECKBOX         0x00000002L
#define BS_AUTOCHECKBOX     0x00000003L
#define BS_RADIOBUTTON      0x00000004L
#define BS_3STATE           0x00000005L
#define BS_AUTO3STATE       0x00000006L
#define BS_GROUPBOX         0x00000007L
#define BS_USERBUTTON       0x00000008L
#define BS_AUTORADIOBUTTON  0x00000009L
#define BS_OWNERDRAW        0x0000000BL
#define BS_LEFTTEXT         0x00000020L
#define BS_TEXT             0x00000000L
#define BS_ICON             0x00000040L
#define BS_BITMAP           0x00000080L
#define BS_LEFT             0x00000100L
#define BS_RIGHT            0x00000200L
#define BS_CENTER           0x00000300L
#define BS_TOP              0x00000400L
#define BS_BOTTOM           0x00000800L
#define BS_VCENTER          0x00000C00L
#define BS_PUSHLIKE         0x00001000L
#define BS_MULTILINE        0x00002000L
#define BS_NOTIFY           0x00004000L
#define BS_FLAT             0x00008000L
#define BS_RIGHTBUTTON      BS_LEFTTEXT

BOOL WINAPI TranslateMDISysAccel(HWND hWndClient, LPMSG lpMsg);
BOOL WINAPI BringWindowToTop(hWnd);
BOOL WINAPI GetWindowInfo(HWND hwnd, PWINDOWINFO pwi);
int* WINAPI DialogBoxIndirectParamA(HINSTANCE hInstance, LPCDLGTEMPLATEA hDialogTemplate,
    HWND hWndParent, DLGPROC lpDialogFunc, LPARAM dwInitParam);
!#

#! (:export t :library "kernel32" :pascal "WINAPI")

/* Local Memory Flags */
#define LMEM_FIXED          0x0000
#define LMEM_MOVEABLE       0x0002
#define LMEM_NOCOMPACT      0x0010
#define LMEM_NODISCARD      0x0020
#define LMEM_ZEROINIT       0x0040
#define LMEM_MODIFY         0x0080
#define LMEM_DISCARDABLE    0x0F00
#define LMEM_VALID_FLAGS    0x0F72
#define LMEM_INVALID_HANDLE 0x8000

#define LHND                (LMEM_MOVEABLE | LMEM_ZEROINIT)
#define LPTR                (LMEM_FIXED | LMEM_ZEROINIT)

#define NONZEROLHND         (LMEM_MOVEABLE)
#define NONZEROLPTR         (LMEM_FIXED)

typedef unsigned int SIZE_T;

HLOCAL WINAPI LocalAlloc(UINT uFlags, SIZE_T uBytes);
!#

(defwinapi TranslateAccelerator ((hWnd HWND)(hAccTable HACCEL)(lpMsg LPMSG))
   :return-type int
   :library-name "user32.dll"
   :entry-name "TranslateAcceleratorA"
   :linkage-type :pascal)

(defvar *class-name-prefix* "gui-")
(defvar *message-hash-table*)
(defvar *window-table*)
(defvar *trace-messages* nil)

(defstruct rect (x 0)(y 0)(width 0)(height 0))

;;; Names of window classes registered by this framework
(defconstant windows-class-name (concatenate 'string *class-name-prefix* "main"))
(defconstant windows-mdi-frame-class-name (concatenate 'string *class-name-prefix* "mdi-frame"))
(defconstant windows-mdi-child-class-name (concatenate 'string *class-name-prefix* "mdi-child"))

;;
;; Clients should call this first when their application starts.
;;
(defun gui-initialize ()
    (ct::get-dll-handle "RICHED20")
;    (InitCommonControls)
    ;; register common controls classes
    (with-fresh-foreign-block (icc 'INITCOMMONCONTROLSEX)
        (with-c-struct (s icc INITCOMMONCONTROLSEX)
            (setf dwSize (ct:sizeof 'INITCOMMONCONTROLSEX)
                  dwICC ICC_BAR_CLASSES)
            (InitCommonControlsEx icc)))

    (register-class windows-class-name 'gui-wndproc)
    (register-class windows-mdi-frame-class-name 'gui-mdi-frame-wndproc)
    (register-class windows-mdi-child-class-name 'gui-mdi-child-wndproc)
    (setf *message-hash-table* (make-hash-table))
    (setf *window-table* (make-hash-table)))
    
;;;
;;; Map win32 window handles and gui window instances
;;;
;; Finds the CLOS <window> object that exists for the
;; given HWND. If no object exists for the handle then nil is returned.
(defun find-window (hwnd)
	(gethash (ct:foreign-ptr-to-int hwnd) *window-table*))

;; Adds a mapping between the HWND and the CLOS <window>
;; object. The mapping will replace any existing mapping for that
;; HWND in the window table.
(defun map-window (hwnd window)
	(setf (gethash (ct:foreign-ptr-to-int hwnd) *window-table*) 
		window))

;; Remove the mapping for HWND
(defun remove-window (hwnd)
	(remhash (ct:foreign-ptr-to-int hwnd) *window-table*))

;;;
;;; Window classes
;;;

;;;
;;; Base window object
;;;
(defclass <window> ()
	((hwnd :accessor window-hwnd :initarg :hwnd :initform nil)
	 (hdc :accessor window-hdc :initform nil)
	 (paintstruct :accessor window-paintstruct :initform (ct:malloc (sizeof 'PAINTSTRUCT)))
	 (xpos :accessor xpos :initarg :xpos :initform 0)
	 (ypos :accessor ypos :initarg :ypos :initform 0)
	 (width :accessor width :initarg :width :initform 0)
	 (height :accessor height :initarg :height :initform 0)
	 (current-id 	:accessor menu-current-id 		:initform 0)
     (command-table :accessor command-table 		:initform (make-hash-table))))

(defclass <client-window> (<window>) ())

;; A mixin class for subclassed windows. Allows subclassing of standard
;; windows controls.
(defclass <subclassed-window-mixin> (<window>)
	((wndproc :accessor window-wndproc :initarg :wndproc :initform nil)))

(defclass <control-window> (<subclassed-window-mixin>))
(defclass <status-bar-control> (<control-window>))
(defclass <richedit-control> (<control-window>))

;;; Class for main windows frames. When one of these is destroyed 
;;; then the message loop is terminated, which usually results in 
;;; an application closing down.
(defclass <frame> (<window>))

(defclass <main-menu-mixin> ()
	((hmenu 		:accessor menu-hmenu 			:initform (CreateMenu))
	 (current-id 	:accessor menu-current-id 		:initform 0)
	 (command-table :accessor command-table 		:initform (make-hash-table))))

;;;
;;; MDI support
;;;
(defclass <mdi-frame> (<main-menu-mixin> <frame>) 
	((client :accessor mdi-client :initform nil)))

(defclass <mdi-child-window> (<window>) ())


;;;
;;; Handle these operations by passing a message, and letting windows message
;;; handlers do the work. These are overridable for each window type via generic
;;; functions, so these do not need to be generic.
;;;
(defun move-window (window x y)
    (PostMessage (window-hwnd window) WM_MOVE 0 (logior (ash y 16) x)))

(defun size-window (window width height)
    (PostMessage (window-hwnd window) WM_SIZE SIZE_RESTORED (logior (ash height 16) width)))
		
(defun instantiate-message ())		;; forward declaration

;;;
;;; Generic functions on <window>
;;;

;; Methods added to this generic function will perform the 
;; actual message handling. Methods should be specialized on the
;; window and/or message parameter to perform specific handling.
(defgeneric handle-message (window message wparam lparam))

(defun crack-message (window id wparam lparam)
    (let ((message (instantiate-message id)))
        (if *trace-messages*
            (let ((name (class-name (class-of message))))
                (when (member name *trace-messages*)
                    (format *trace-output* "Received message: ~A~%"
                        (format-trace-message window message wparam lparam))
                    (force-output))))
        (handle-message window message wparam lparam)))

;; Generic functions for manipulating windows.
(defgeneric update-window (window))
(defgeneric show-window   (window arg))
(defgeneric set-text (window text))
(defgeneric set-position (window x y width height))
(defgeneric format-trace-message (window message wparam lparam))

;; Create a window
(defgeneric create-window (window &key class-name ex-style caption style x y width height parent menu param))

(defgeneric call-default-handler ((window <window>) message wparam lparam))

;;;
;;; Should return NIL to cause the command to be passed on for further processing.
;;;
(defgeneric process-command ((window <window>) command-id wparam lparam))

;; Message classes
(defclass <message> () ((id :accessor message-id :initarg :id :initform nil)))

(defmethod print-object ((message <message>) stream)
    (print-unreadable-object (message stream :identity t)
        (format stream "~:(~S~) id: ~S"
            (class-name (class-of message)) 
            (message-id message))))
                                                      
(defclass <ncdestroy-message> 	        (<message>))
(defclass <nccreate-message>            (<message>))
(defclass <create-message>              (<message>))
(defclass <destroy-message>             (<message>))
(defclass <size-message> 		        (<message>))
(defclass <move-message> 		        (<message>))
(defclass <paint-message> 		        (<message>))
(defclass <lbuttondown-message>         (<message>))
(defclass <rbuttondown-message>         (<message>))
(defclass <timer-message> 		        (<message>))
(defclass <command-message> 	        (<message>))
(defclass <nccalcsize-message> 	        (<message>))
(defclass <showwindow-message> 	        (<message>))
(defclass <settext-message> 	        (<message>))
(defclass <windowposchanging-message> 	(<message>))
(defclass <childactivate-message>       (<message>))
(defclass <ncpaint-message>             (<message>))
(defclass <erasebkgnd-message>          (<message>))
(defclass <windowposchanged-message>    (<message>))
(defclass <stylechanging-message>       (<message>))
(defclass <stylechanged-message>        (<message>))
(defclass <nchittest-message>           (<message>))
(defclass <setcursor-message>           (<message>))
(defclass <ncmousemove-message>         (<message>))

(defclass <nclbuttondown-message>       (<message>))
(defclass <nclbuttonup-message>         (<message>))
(defclass <nclbuttondblclk-message>     (<message>))
(defclass <ncrbuttondown-message>       (<message>))
(defclass <ncrbuttonup-message>         (<message>))
(defclass <ncrbuttondblclk-message>     (<message>))
(defclass <ncmbuttondown-message>       (<message>))
(defclass <ncmbuttonup-message>         (<message>))
(defclass <ncmbuttondblclk-message>     (<message>))

(defclass <mousemove-message>           (<message>))
(defclass <mouseactivate-message>       (<message>))
(defclass <lbuttondown-message>         (<message>))
(defclass <lbuttonup-message>           (<message>))
(defclass <lbuttondblclk-message>       (<message>))
(defclass <rbuttondown-message>         (<message>))
(defclass <rbuttonup-message>           (<message>))
(defclass <rbuttondblclk-message>       (<message>))
(defclass <mbuttondown-message>         (<message>))
(defclass <mbuttonup-message>           (<message>))
(defclass <mbuttondblclk-message>       (<message>))

(defclass <ncmouseleave-message>        (<message>))

(defclass <setfocus-message>            (<message>))
(defclass <capturechanged-message>      (<message>))
(defclass <keydown-message>             (<message>))
(defclass <keyup-message>               (<message>))
(defclass <char-message>                (<message>))
(defclass <parentnotify-message>        (<message>))
(defclass <notifyformat-message>        (<message>))
(defclass <queryuistate-message>        (<message>))
(defclass <geticon-message>             (<message>))
                                                                                
(defun instantiate-message (id)
	(or (gethash id *message-hash-table*)
		(setf (gethash id *message-hash-table*) 
			(make-instance 
				(cond
					((= id WM_CREATE)                   '<create-message>)
					((= id WM_DESTROY)                  '<destroy-message>)
					((= id WM_NCCREATE) 	            '<nccreate-message>)
					((= id WM_NCDESTROY) 	            '<ncdestroy-message>)
					((= id WM_SIZE) 		            '<size-message>)
					((= id WM_MOVE) 		            '<move-message>)
					((= id WM_PAINT)                    '<paint-message>)
					((= id WM_TIMER)                    '<timer-message>)
					((= id WM_COMMAND) 		            '<command-message>)
					((= id WM_NCCALCSIZE) 	            '<nccalcsize-message>)
					((= id WM_SHOWWINDOW) 	            '<showwindow-message>)
					((= id WM_SETTEXT) 	                '<settext-message>)
                    ((= id WM_WINDOWPOSCHANGING)        '<windowposchanging-message>)
                    ((= id WM_CHILDACTIVATE)            '<childactivate-message>) 
                    ((= id WM_NCPAINT)                  '<ncpaint-message>)
                    ((= id WM_ERASEBKGND)               '<erasebkgnd-message>)
                    ((= id WM_WINDOWPOSCHANGED)         '<windowposchanged-message>)
                    ((= id WM_STYLECHANGING)            '<stylechanging-message>)                                  
                    ((= id WM_STYLECHANGED)             '<stylechanged-message>)                                  
                    ((= id WM_NCHITTEST)                '<nchittest-message>)                                  
                    ((= id WM_SETCURSOR)                '<setcursor-message>)
                                                                                                          
                    ((= id WM_MOUSEMOVE)                '<mousemove-message>)
                    ((= id WM_MOUSEACTIVATE)            '<mouseactivate-message>)

                    ((= id WM_LBUTTONDOWN)              '<lbuttondown-message>)
                    ((= id WM_LBUTTONUP)                '<lbuttonup-message>)
                    ((= id WM_LBUTTONDBLCLK)            '<lbuttondblclk-message>)
                    ((= id WM_RBUTTONDOWN)              '<rbuttondown-message>)
                    ((= id WM_RBUTTONUP)                '<rbuttonup-message>)
                    ((= id WM_RBUTTONDBLCLK)            '<rbuttondblclk-message>)
                    ((= id WM_MBUTTONDOWN)              '<mbuttondown-message>)
                    ((= id WM_MBUTTONUP)                '<mbuttonup-message>)
                    ((= id WM_MBUTTONDBLCLK)            '<mbuttondblclk-message>)
                                                                                
                    ((= id WM_NCMOUSEMOVE)              '<ncmousemove-message>) 
                    ((= id WM_NCLBUTTONDOWN)            '<nclbuttondown-message>) 
                    ((= id WM_NCLBUTTONUP)              '<nclbuttonup-message>)
                    ((= id WM_NCLBUTTONDBLCLK)          '<nclbuttondblclk-message>)
                    ((= id WM_NCRBUTTONDOWN)            '<ncrbuttondown-message>)
                    ((= id WM_NCRBUTTONUP)              '<ncrbuttonup-message>)
                    ((= id WM_NCRBUTTONDBLCLK)          '<ncrbuttondblclk-message>)
                    ((= id WM_NCMBUTTONDOWN)            '<ncmbuttondown-message>)
                    ((= id WM_NCMBUTTONUP)              '<ncmbuttonup-message>)
                    ((= id WM_NCMBUTTONDBLCLK)          '<ncmbuttondblclk-message>)

                    ((= id WM_NCMOUSELEAVE)             '<ncmouseleave-message>)
                    ((= id WM_SETFOCUS)                 '<setfocus-message>)
                    ((= id WM_CAPTURECHANGED)           '<capturechanged-message>)
                    ((= id WM_KEYDOWN)                  '<keydown-message>)
                    ((= id WM_KEYUP)                    '<keyup-message>)
                    ((= id WM_CHAR)                     '<char-message>)
                    ((= id WM_PARENTNOTIFY)             '<parentnotify-message>)
                    ((= id WM_NOTIFYFORMAT)             '<notifyformat-message>)
                    ((= id WM_QUERYUISTATE)             '<queryuistate-message>)
                    ((= id WM_GETICON)                  '<geticon-message>)
                                                                                                                                            
                    (t 						           '<message>))
				:id id))))

(defmethod handle-message ((window <window>) (message <message>) wparam lparam)
	(call-default-handler window message wparam lparam))

(defmethod show-window ((window <window>) arg)
	(ShowWindow (window-hwnd window) arg))

(defmethod update-window ((window <window>))
	(UpdateWindow (window-hwnd window)))

(defmethod call-default-handler ((window <window>) message wparam lparam)
	(DefWindowProc (window-hwnd window) (message-id message) wparam lparam))	

(defmethod process-command ((window <window>) command-id wparam lparam)
	(declare (ignore wparam lparam))
	(let* ((func (gethash command-id (command-table window))))
		(if func
			(progn (funcall func) t)
			nil)))
	
(defmethod handle-message ((window <window>)(message <timer-message>) wparam lparam)
	(declare (ignore window message wparam lparam))
	(call-next-method))	;; default handler does nothing
	
(defmethod handle-message ((window <window>) (message <size-message>) wparam lparam)
	(declare (ignore message wparam))
   	(setf (width window) (LOWORD lParam))
	(setf (height window) (HIWORD lParam))
	(call-next-method))

(defmethod handle-message ((window <client-window>) (message <size-message>) wparam lparam)
	(declare (ignore message wparam lparam))
    (call-next-method))

(defmethod handle-message ((window <control-window>) (message <size-message>) wparam lparam)
	(declare (ignore message wparam lparam))
    (call-next-method))

(defmethod handle-message ((window <window>) (message <move-message>) wparam lparam)
	(declare (ignore message wparam))
	(setf (xpos window) (LOWORD lParam))
	(setf (ypos window) (HIWORD lParam))
	(call-next-method))

(defmethod handle-message ((window <window>) (message <paint-message>) wparam lparam)
	(declare (ignore message wparam lparam))
	(call-next-method))

(defun begin-paint (window)
 	(setf (window-hdc window) 
		(BeginPaint (window-hwnd window) (window-paintstruct window))))

(defun end-paint (window)
 	(EndPaint (window-hwnd window) (window-paintstruct window))
    (setf (window-hdc window) nil))
     
#|
(defmethod handle-message :before ((window <window>) (message <paint-message>) wparam lparam)
	(declare (ignore message wparam lparam))
	(setf (window-hdc window) 
		(BeginPaint (window-hwnd window) (window-paintstruct window))))

(defmethod handle-message :after ((window <window>) (message <paint-message>) wparam lparam)
	(declare (ignore message wparam lparam))
	(EndPaint (window-hwnd window) (window-paintstruct window))
	(setf (window-hdc window) nil))
|#
(defmethod create-window ((window <window>) 
		&key class-name ex-style caption style x y width height parent menu param)
    (let ((hwnd
                (CreateWindowEx (or ex-style 0)
                    (ct:create-c-string (or class-name windows-class-name))
        			(ct:create-c-string (or caption ""))
        			(or style 0)
        			(or x CW_USEDEFAULT)
        			(or y CW_USEDEFAULT)
        			(or width CW_USEDEFAULT)
        			(or height CW_USEDEFAULT)
        			(cond 
                        ((typep parent '<window>)(window-hwnd parent)) 
                        ((null parent) ct:null) 
                        (t parent))
        			(or menu NULL)
        			(cl::get-application-instance)
        			(or param NULL))))
        (when (or (null hwnd) (and (foreignp hwnd)(cpointer= null hwnd)))
			(error "Null window handle"))
        (setf (window-hwnd window) hwnd)
        (map-window hwnd window)    ;; set up mapping from hwnd to the window object 
		(SetWindowText hwnd (ct:create-c-string (or caption "")))
        (let ((rect (get-window-rect window)))
            (setf (xpos window) (rect-x rect) (ypos window) (rect-y rect)
                  (width window) (rect-width rect) (height window) (rect-height rect)))))

(defmethod set-text ((window <window>) text)
    (SetWindowText (window-hwnd window) (ct:create-c-string (or text ""))))

;; Client applications usually call this when handling a resize (WM_SIZE) event
(defmethod set-position ((window <window>) x y width height)
    (setf (xpos window) x 
          (ypos window) y
          (width window) width
          (height window) height)
    (win:MoveWindow (window-hwnd window) x y width height TRUE)) 
   
(defmethod handle-message :after ((window <frame>) (message <ncdestroy-message>) wparam lparam)
	(declare (ignore window message wparam lparam))
	(PostQuitMessage 0))

(defmethod call-default-handler ((window <subclassed-window-mixin>) message wparam lparam)
	(CallWindowProc (window-wndproc window) (window-hwnd window) (message-id message) wparam lparam))

(defmethod handle-message ((window <window>) (message <nccreate-message>) wparam lparam)
	(declare (ignore window message wparam lparam))
    (call-next-method))

(defmethod handle-message ((window <window>) (message <create-message>) wparam lparam)
	(declare (ignore window message wparam lparam))
    (call-next-method))

(defmethod create-window ((window <subclassed-window-mixin>) 
		&key class-name ex-style caption style x y width height parent menu param)
    ;; create the window
    (call-next-method window :class-name class-name :ex-style ex-style
            :caption caption :style style :x x :y y :width width :height height
            :parent parent :menu menu :param param)
    
	(let* ((hwnd (window-hwnd window))
           (orig-wndproc (SetWindowLong hwnd GWL_WNDPROC (ct:foreign-ptr-to-int (get-callback-procinst 'gui-wndproc)))))
        (setf (window-wndproc window) (ct:int-to-foreign-ptr orig-wndproc))
        (SetWindowPos hwnd HWND_TOP 0 0 0 0 (logior SWP_NOMOVE SWP_NOSIZE SWP_NOZORDER SWP_FRAMECHANGED))))

(defmethod handle-message ((window <subclassed-window-mixin>) (message <destroy-message>) wparam lparam)
	(declare (ignore window message wparam lparam))
    (SetWindowLong (window-hwnd window) GWL_WNDPROC (ct:foreign-ptr-to-int (window-wndproc window)))
    (call-next-method))

(defmethod create-menu ((menu-mixin <main-menu-mixin>) option parent position)
	(let* ((key (first option)))
		(case key
			(:menu
				(let ((new-menu (CreateMenu))
	                  (name (second option)))
					(if (null parent)
						;; add to the main menu bar
						(InsertMenu
							(menu-hmenu menu-mixin)
							position 
							(logior MF_ENABLED MF_STRING MF_BYPOSITION MF_POPUP)
							(cl::foreign-ptr-to-int new-menu)
							(create-c-string name))
						;; add submenu to a menu
						(InsertMenu
							(find-named-menu (get-main-menu) parent) 
							position 
							(logior MF_ENABLED MF_STRING MF_BYPOSITION MF_POPUP)
							(cl::foreign-ptr-to-int new-menu)
							(create-c-string name)))))
			(:command
				(let ((name (second option))
					  (func (third option)))
					(if (null parent)
						(error "No menu was specified"))
					(let ((id (incf (menu-current-id menu-mixin)))
						  (c-name (create-c-string name)))
						(InsertMenu
							(find-named-menu (menu-hmenu menu-mixin) parent)
							position
							(logior MF_ENABLED MF_STRING)
							id
							c-name)
						(setf (gethash id (command-table menu-mixin)) func))))
			(otherwise (error "Invalid option in CREATE-MENU-ITEM: ~A" key)))))

(defmethod handle-message ((window <main-menu-mixin>) (message <command-message>) wparam lparam)
	(declare (ignore message))
	(let* ((command-id (LOWORD wparam)))
		(unless (process-command window command-id wparam lparam)
			(call-next-method)))
    0)

(defmethod create-window ((window <main-menu-mixin>) 
		&key class-name ex-style caption style x y width height parent menu param)
	(if menu
		(call-next-method)
		(call-next-method window 
			:class-name class-name
			:ex-style ex-style
			:caption caption 
			:style style 
			:x x 
			:y y 
			:width width 
			:height height 
			:parent parent 
			:menu (menu-hmenu window)
            :param param)))
				
(defmethod create-window ((window <mdi-frame>) 
		&key class-name ex-style caption (style 0) x y width height parent menu param)
    (declare (ignore class-name))
	(call-next-method window 
			:class-name windows-mdi-frame-class-name
			:ex-style ex-style
			:caption caption 
			:style (logior style WS_OVERLAPPEDWINDOW WS_CLIPCHILDREN) 
			:x x 
			:y y 
			:width width 
			:height height 
			:parent parent 
			:menu (or menu (menu-hmenu window))
            :param param)
	(create-menu window '(:menu "Window") nil 1)
    
    ;; create the mdi frame client window
	(with-fresh-foreign-block (ccs 'CLIENTCREATESTRUCT)
		(with-c-struct (s ccs CLIENTCREATESTRUCT)
			(setf hWindowMenu (find-named-menu (menu-hmenu window) "Window") idFirstChild 100)
        	(let ((client (make-instance '<subclassed-window-mixin>)))
                (create-window client 
                    :class-name "MDICLIENT" 
                    :style (logior WS_CHILD WS_CLIPCHILDREN WS_CLIPSIBLINGS WS_VISIBLE WS_MAXIMIZE)
             		:x 0
            		:y 0
            		:width 0
               		:height 0
                    :parent window
                    :menu (ct:int-to-foreign-ptr 1)
                    :param ccs)
                (setf (mdi-client window) client))))
    (DrawMenuBar (window-hwnd window)))

(defmethod create-window ((window <mdi-child-window>) 
		&key class-name (ex-style 0) caption (style 0) x y width height parent menu param)
    (declare (ignore menu class-name))
    (call-next-method window 
			:class-name windows-mdi-child-class-name
			:ex-style (logior WS_EX_MDICHILD ex-style)
			:caption caption 
			:style (logior style WS_OVERLAPPEDWINDOW WS_CLIPCHILDREN) 
			:x (or x CW_USEDEFAULT)
			:y (or y CW_USEDEFAULT) 
			:width (or width CW_USEDEFAULT) 
			:height (or height CW_USEDEFAULT) 
			:parent parent 
			:menu ct:null
            :param param))

(defmethod create-window ((window <status-bar-control>)
		&key class-name (ex-style 0) caption (style 0) x y width height parent menu param)
    (declare (ignore class-name))
    (call-next-method window :class-name STATUSCLASSNAMEA
            :ex-style ex-style :caption caption
            :style (logior style WS_CHILD WS_VISIBLE)
     		:x x :y y :width width :height height
            :menu menu :parent parent :param param))

(defmethod create-window ((window <richedit-control>)
		&key class-name (ex-style 0) caption (style 0) x y width height parent menu param)
    (declare (ignore class-name style))
    (call-next-method window :class-name "RICHEDIT20A"
        :ex-style ex-style :caption caption
        :style (logior WS_CHILD WS_VISIBLE WS_HSCROLL WS_VSCROLL
              WS_BORDER ES_LEFT ES_MULTILINE
              ES_AUTOHSCROLL ES_AUTOVSCROLL)
        :x x :y y :width width :height height
        :menu menu :parent parent :param param))

(defun get-window-rect (window)
	(with-fresh-foreign-block (wi 'WINDOWINFO)
        (GetWindowInfo (window-hwnd window) wi)
            (let ((rect (ct:cref WINDOWINFO wi rcClient)))
            	(with-c-struct (s rect RECT)
                    (make-rect :x left :y top :width (- right left) :height (- bottom top))))))
             
(defmethod call-default-handler ((window <mdi-frame>) message wparam lparam)
	(DefFrameProc 
        (window-hwnd window) 
        (if (mdi-client window) 
            (window-hwnd (mdi-client window))
            ct:null) 
        (message-id message) 
        wparam 
        lparam))

(defmethod call-default-handler ((window <mdi-child-window>) message wparam lparam)
	(DefMDIChildProc (window-hwnd window) (message-id message) wparam lparam))

(defmethod process-command ((window <mdi-frame>) command-id wparam lparam)
	(declare (ignore wparam lparam))
	(let* ((func (gethash command-id (command-table window))))
		(cond 
			(func (funcall func) t)
			(t
				(let ((active-child-hwnd 
							(ct:int-to-foreign-ptr
								(SendMessage (window-hwnd (mdi-client window)) WM_MDIGETACTIVE 0 0))))
					(if (IsWindow active-child-hwnd)
						(SendMessage active-child-hwnd WM_COMMAND wparam lparam)))
				nil))))	;; call DefFrameProc
         
;;;
;;; Main windows procedure
;;;
(ct:defun-callback gui-wndproc ((hwnd HWND)(id UINT)(wParam WPARAM)(lParam LPARAM))
;    (format t "GUI-WNDPROC: ~A ~A ~A ~A~%" (find-window hwnd) (instantiate-message id) wParam lParam)(force-output)	
	(let ((window (find-window hwnd)))
		(if (null window)
			(DefWindowProc hwnd id wParam lParam)
			(prog1
				(crack-message window id wParam lParam)
				(when (= id WM_NCDESTROY)
					(remove-window hwnd))))))

(ct:defun-callback gui-mdi-frame-wndproc ((hwnd HWND)(id UINT)(wParam WPARAM)(lParam LPARAM))
	(let ((window (find-window hwnd)))
		(if (null window)
			(DefFrameProc hwnd ct:null id wParam lParam)
			(prog1
				(crack-message window id wParam lParam)
				(when (= id WM_NCDESTROY)
					(remove-window hwnd))))))

(ct:defun-callback gui-mdi-child-wndproc ((hwnd HWND)(id UINT)(wParam WPARAM)(lParam LPARAM))
	(let ((window (find-window hwnd)))
		(if (null window)
			(DefMDIChildProc hwnd id wParam lParam)
			(prog1
				(crack-message window id wParam lParam)
				(when (= id WM_NCDESTROY)
					(remove-window hwnd))))))

(ct:defun-callback dialog-wndproc ((hwnd HWND)(id UINT)(wParam WPARAM)(lParam LPARAM))
    (declare (ignore hwnd id wParam lParam window))
	(let ((window (find-window hwnd)))
        (declare (ignore window))
        FALSE))

;;; The standard message loop used by applications or threads
(defun standard-message-loop ()
	(let ((msg (ct:malloc (ct:sizeof 'MSG))))
		(do ()
			((not (GetMessage msg NULL 0 0)))
			(TranslateMessage msg)
			(DispatchMessage msg))))

;;; Message loop for MDI applications
(defun mdi-message-loop (mdi-frame-window)
	(let* ((msg (ct:malloc (ct:sizeof 'MSG)))
          (mdi-client (mdi-client mdi-frame-window))
          (mdi-client-hwnd (window-hwnd mdi-client))
          (mdi-frame-hwnd (window-hwnd mdi-frame-window)))
        (declare (ignore mdi-frame-hwnd))
		(do ()
			((not (GetMessage msg NULL 0 0)))
 #|
            (if (and (not (TranslateMDISysAccel mdi-client-hwnd msg))
                     (= (TranslateAccelerator mdi-frame-hwnd hAccel msg))
                    ))
 |#
                (TranslateMDISysAccel mdi-client-hwnd msg)
                (TranslateMessage msg)
			    (DispatchMessage msg))))

(defun register-trace-message (message-list)
    (if (null message-list)
        *trace-messages*
        (dolist (x message-list)
            (setf *trace-messages* (adjoin x *trace-messages* :test 'eq)))))

(defun unregister-trace-message  (message-list)
    (if (null message-list)
        (setf *trace-messages* nil)
        (dolist (x message-list)
            (setf *trace-messages* (remove x *trace-messages* :test 'eq)))))
        
(defmacro trace-message (&rest messages)
    `(register-trace-message ',messages))

(defmacro untrace-message (&rest messages)
    `(unregister-trace-message ',messages))

(defmethod format-trace-message (window (message <message>) wparam lparam)
    (format nil "~A ~A ~A ~A" (class-name (class-of message)) window wparam lparam))

(defmethod format-trace-message (window (message <size-message>) wparam lparam)
    (declare (ignore wparam))
    (format nil "~A ~A width:~A height:~A" (class-name (class-of message)) 
        window (loword lparam) (hiword lparam)))

(defmethod format-trace-message (window (message <move-message>) wparam lparam)
    (declare (ignore wparam))
    (format nil "~A ~A x:~A y:~A" (class-name (class-of message)) 
        window (loword lparam) (hiword lparam)))

