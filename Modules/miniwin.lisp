;;;;
;;;;	File:		miniwin.lisp
;;;;	Contents:	Simple windows class toolkit for Common Lisp.
;;;; 	Author:		Chris Double
;;;;	History:	Mods by Roger Corman
;;;;

(in-package :win)
(provide "MINIWIN")
;;(require "SNOOPER")

#! (:library "user32" :export t  :pascal "WINAPI")
BOOL WINAPI SetWindowTextA(HWND hWnd, LPCSTR lpString);
!#

;; Holds the associations between win32 window handles
;; and miniwin windows objects.
(let ((window-table (make-hash-table)))
	(defun find-window ( hwnd )
		"Finds the CLOS <window> object that exists for the
		 given HWND. If no object exists for the handle then nil is returned."
		(gethash (ct:foreign-ptr-to-int hwnd) window-table))
	
	(defun map-window ( hwnd window )
		"Adds a mapping between the HWND and the CLOS <window>
		 object. The mapping will replace any existing mapping for that
		 HWND in the window table."
		(setf (gethash (ct:foreign-ptr-to-int hwnd) window-table) 
			window))
	
	(defun remove-window ( hwnd )
		"Remove the mapping for HWND"
		(remhash (ct:foreign-ptr-to-int hwnd) window-table)))

;; Base window object
(defclass <window> ()
	((hwnd :accessor window-hwnd :initarg hwnd :initform nil)
	 (width :accessor width :initarg :width :initform 0)
	 (height :accessor height :initarg :height :initform 0)))


(defvar *window-class-atoms* (make-hash-table :test 'equal))
(defvar *class-name-prefix* "miniwin-")

(defun miniwin-class-name ()
	"Register the main windows class necessary for this framework.
     and return the atom used in CreateWindowEx to identify the class."		
	(let ((class-name (concatenate 'string *class-name-prefix* "main")))
		(progn
			(let ((wndclass (ct:malloc (sizeof 'WNDCLASSEX))))
				(with-c-struct (s wndclass WNDCLASSEX)
					(setf 
						cbSize 			(sizeof 'WNDCLASSEX)
						style 			(logior CS_HREDRAW CS_VREDRAW)
						lpfnWndProc 	(get-callback-procinst 'miniwin-wndproc)
						cbClsExtra 		0
						cbWndExtra 		0
						hInstance 		(cl::get-application-instance)
						hIcon 			(LoadIcon NULL IDI_APPLICATION)
						hCursor 		(LoadCursor NULL IDC_ARROW)
						hbrBackground 	(GetStockObject WHITE_BRUSH)
						lpszMenuName 	NULL
						lpszClassName 	(ct:create-c-string class-name)
						hIconSm 		(LoadIcon NULL IDI_APPLICATION)))
				(RegisterClassEx wndclass)	
				(setf (gethash class-name *window-class-atoms*) class-name)))
		(gethash class-name *window-class-atoms*)))

;; The standard message loop used by applications or threads
(defun standard-message-loop ()
	(let ((msg (ct:malloc (sizeof 'MSG))))
		(do ()
			((not (GetMessage msg NULL 0 0)))
			(TranslateMessage msg)
			(DispatchMessage msg))))
		
(defun make-message ())		;; forward declaration

;; Generic functions on <window>
;;
;; Converts the standard windows message parameters into a message
;; object and passes that onto the handle-windows-message generic.
(defgeneric crack-windows-message ( window imsg wparam lparam ))

;; Methods added to this generic function will perform the 
;; actual message handling. Methods should be specialised on the
;; window and/or message parameter to perform specific handling.
(defgeneric handle-windows-message (window message wparam lparam))

;; The default windows handler for messages that our
;; application does not process. Usually calls DefWindowProc 
;; but it could call subclassed windows functions or dialog box
;; procedure defaults depending on the method specialisation.
(defgeneric handle-unknown-message (window message wparam lparam))

;; Generic functions for manipulating windows. Maybe these should
;; be normal functions - Is there any advantage in being able to
;; specialise them?
(defgeneric update-window ( window ))
(defgeneric show-window ( window arg ))

;; Create a window
(defgeneric create-window ( window &key class-name ex-style caption style x y width height parent menu ))

;; Methods added to this generic function will perform handle 
;; timer messages for the window.
(defgeneric handle-timer-message (window))

;; Methods for <window>
(defmethod crack-windows-message ( (window <window>) imsg wparam lparam )
	(handle-windows-message window (make-message imsg wparam lparam)  wparam lparam))

;; Message classes
(defclass <windows-message> ()
	((id :accessor message-id :initarg id :initform nil)))

(defclass <ncdestroy-message> (<windows-message>))
(defclass <nccreate-message> (<windows-message>))
(defclass <create-message> (<windows-message>))
(defclass <destroy-message> (<windows-message>))
(defclass <size-message> (<windows-message>))
(defclass <paint-message> (<windows-message>))
(defclass <lbuttondown-message> (<windows-message>))
(defclass <rbuttondown-message> (<windows-message>))
(defclass <timer-message> (<windows-message>))

(defvar *message-hash-table* (make-hash-table))

(defun make-message (imsg wparam lparam)
	(declare (ignore wparam lparam))
	(or (gethash imsg *message-hash-table*)
		(setf (gethash imsg *message-hash-table*) 
			(make-instance (cond
					((= imsg WM_NCDESTROY) '<ncdestroy-message>)
					((= imsg WM_DESTROY) '<destroy-message>)
					((= imsg WM_NCCREATE) '<nccreate-message>)
					((= imsg WM_CREATE) '<create-message>)
					((= imsg WM_SIZE) '<size-message>)
					((= imsg WM_PAINT) '<paint-message>)
					((= imsg WM_LBUTTONDOWN) '<lbuttondown-message>)
					((= imsg WM_RBUTTONDOWN) '<rbuttondown-message>)
					((= imsg WM_TIMER) '<timer-message>)
					(t '<windows-message>)) 'id imsg))))

(defmethod handle-windows-message ((w <window>) message wparam lparam)
	(handle-unknown-message w message wparam lparam))

(defmethod handle-unknown-message ((window <window>) message wparam lparam)
	(DefWindowProc (window-hwnd window) (message-id message) wparam lparam))

(defmethod show-window ((window <window>) arg)
	(ShowWindow (window-hwnd window) arg))

(defmethod update-window ((window <window>))
	(UpdateWindow (window-hwnd window)))

(defmethod handle-windows-message ((window <window>)(message <timer-message>) wparam lparam)
	(declare (ignore window message wparam lparam)))
		;; default handler does nothing
	
(defmethod handle-windows-message ((window <window>) (message <size-message>) wparam lparam)
	(declare (ignore message wparam))
	(setf (width window) (LOWORD lParam))
	(setf (height window) (HIWORD lParam)))

(defmethod create-window ((window <window>) &key class-name ex-style caption style x y width height parent menu)
	(declare (ignore class-name))
	(let ((current-key (incf *window-create-key*)))
		(setf (gethash current-key *window-create-mapping*) window)
		(CreateWindowEx (or ex-style 0)
			(ct:create-c-string (miniwin-class-name))
			(ct:create-c-string (or caption ""))
			(or style 0)
			(or x CW_USEDEFAULT)
			(or y CW_USEDEFAULT)
			(or width CW_USEDEFAULT)
			(or height CW_USEDEFAULT)
			(or parent NULL)
			(or menu NULL)
			(cl::get-application-instance)
			(ct:int-to-foreign-ptr current-key))
		(SetWindowText (window-hwnd window) (ct:create-c-string caption))
		(when (or (null (window-hwnd window)) 
				(and (foreignp (window-hwnd window))(cpointer= null (window-hwnd window))))
			(error "Null window handle"))))
			
;; A mixin class for main windows frames. When a window that has
;; this class mixed in is destroyed then the message loop is terminated
;; which usually results in an application closing down.
(defclass <main-window-mixin> (<window>))

(defmethod handle-windows-message :after ((window <main-window-mixin>) (message <ncdestroy-message>) wparam lparam)
	(declare (ignore window message wparam lparam))
	(PostQuitMessage 0))

;; A mixin class for subclassed windows. Allows subclassing of standard
;; windows controls.
(defclass <subclassed-window-mixin> (<window>)
	((wndproc :accessor window-wndproc :initarg wndproc :initform nil)))

(defmethod handle-unknown-message ((window <subclassed-window-mixin>) message wparam lparam)
	(CallWindowProc (window-wndproc window) (window-hwnd window) (message-id message) wparam lparam))

(defmethod handle-windows-message ((window <window>) (message <nccreate-message>) wparam lparam)
	(declare (ignore window message wparam lparam))
	TRUE)

(defmethod create-window ( (window <subclassed-window-mixin>) &key class-name ex-style caption style x y width height parent menu )
	(let ((new-class-name (concatenate 'string *class-name-prefix* class-name)))
		(let ((wndclass (ct:malloc (sizeof 'WNDCLASSEX))))
			(when (not (GetClassInfoEx NULL (ct:create-c-string class-name) wndclass))
				(error "GetClassInfoEx-Failed"))
			(setf (window-wndproc window) (cref WNDCLASSEX wndclass win::lpfnWndProc))
;;			(when (null (gethash new-class-name *window-class-atoms*))
				(with-c-struct (s wndclass WNDCLASSEX)
					(setf 
						cbSize 			(sizeof 'WNDCLASSEX)
						style			(logior CS_HREDRAW CS_VREDRAW)
						cbClsExtra		0
						cbWndExtra		0
						hInstance 		(cl::get-application-instance)
						hIcon			(LoadIcon NULL IDI_APPLICATION)
						hCursor			(LoadCursor NULL IDC_ARROW)
						hbrBackground	(GetStockObject WHITE_BRUSH)
						lpszClassName 	(ct:create-c-string new-class-name)
						lpszMenuName 	NULL
						lpfnWndProc 	(get-callback-procinst 'miniwin-wndproc)
						hIconSm			(LoadIcon NULL IDI_APPLICATION)))
				(when (= (RegisterClassEx wndclass) 0)
					(error "registerclassex-failed"))
				(setf (gethash new-class-name *window-class-atoms*) new-class-name)
;;			)
			
			(let ((current-key (incf *window-create-key*)))
				(setf (gethash current-key *window-create-mapping*) window)
				(CreateWindowEx (or ex-style 0)
					(ct:create-c-string new-class-name)
					(ct:create-c-string (or caption ""))
					(or style 0)
					(or x CW_USEDEFAULT)
					(or y CW_USEDEFAULT)
					(or width CW_USEDEFAULT)
					(or height CW_USEDEFAULT)
					(or parent NULL)
					(or menu NULL)
					(cl::get-application-instance)
					(ct:int-to-foreign-ptr current-key))
				(when (null (window-hwnd window))
					(GetLastError))))))

;; Infrastructure functions
;;
;; Main windows procedure
(defvar *window-create-mapping* (make-hash-table))
(defvar *window-create-key* 0)

(ct:defun-callback miniwin-wndproc ((hwnd HWND)(iMsg UINT) (wParam WPARAM) (lParam LPARAM))
	(when (= iMsg WM_NCCREATE)
		(let ((current-key (ct:foreign-ptr-to-int 
						(cref CREATESTRUCT (ct:int-to-foreign-ptr lParam) lpCreateParams))))
			(let ((window (gethash current-key *window-create-mapping*)))
				(setf (window-hwnd window) hwnd)
				(map-window hwnd window))
			(remhash current-key *window-create-mapping*)))
	
	(let ((window (find-window hwnd)))
		(if (null window)
			(DefWindowProc hwnd iMsg wParam lParam)
			(prog1
				(crack-windows-message window iMsg wParam lParam)
				(when (= iMsg WM_NCDESTROY)
					(remove-window hwnd))))))

;;;;
;;;;	Test functions
;;;;
(defclass <test-window> (<main-window-mixin> <window>))

(defun run-test1 ( )
	(let ((window (make-instance '<test-window>)))
		(create-window window
			:caption "test window"
			:style (logior WS_OVERLAPPEDWINDOW WS_MINIMIZEBOX)
			:width 300
			:height 300)
			
		(show-window window SW_SHOW)
		(update-window window)
		(standard-message-loop)))

(defclass <test-window2> (<main-window-mixin> <subclassed-window-mixin> <window>))
;;(defmethod handle-windows-message ((window <test-window2>) (message <rbuttondown-message>) wparam lparam)
	;;(PostQuitMessage 0))

(defun run-test2 ( )
	(let ((window (make-instance '<test-window2>)))
		(create-window window
			:class-name "EDIT"
			:caption "test window"
			:style (logior WS_OVERLAPPEDWINDOW WS_MINIMIZEBOX ES_MULTILINE)
			:width 300
			:height 300)
			
		(show-window window SW_SHOW)
		(update-window window)
		(standard-message-loop)))