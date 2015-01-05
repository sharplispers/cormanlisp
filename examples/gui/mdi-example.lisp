;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	Windows MDI example program.
;;;;    
;;;;    To run from lisp, load this file and enter:
;;;;
;;;;		(th:create-thread #'win::mdi-example)
;;;;
;;;;    To create a single-file executable, load this file and enter:
;;;;    (save-application "mdi-example" #'win::mdi-example :static t)
;;;;

(in-package :win)
(require "GUI")

;;;
;;; Derived windows for this applications
;;;
(defclass <mdi-example-frame> (<mdi-frame>) 
    ((status-bar :accessor status-bar :initform nil)))

(defclass <mdi-example-child> (<mdi-child-window>)
    ((edit-control :accessor edit-control :initform nil)))

(defun create-status-bar (parent-window)
    "Creates a status bar on the passed window, and returns it"
    (let ((status-bar (make-instance '<status-bar-control>)))
        (create-window status-bar  :style (logior WS_CHILD WS_VISIBLE)
            :menu (int-to-foreign-ptr 1) :parent parent-window)
        (BringWindowToTop (window-hwnd status-bar))
        status-bar))

(defun create-rich-edit-control (parent-window)
    "Creates a rich edit control on the passed window, and returns it"
    (let ((edit-control (make-instance '<richedit-control>)))
        (create-window edit-control 
            :style (logior WS_CHILD WS_VISIBLE WS_HSCROLL WS_VSCROLL
              WS_BORDER ES_LEFT ES_MULTILINE
              ES_AUTOHSCROLL ES_AUTOVSCROLL)
            :x 0 :y 0 :width (width parent-window) :height (height parent-window)
            :menu (int-to-foreign-ptr 1) :parent parent-window)
        (BringWindowToTop (window-hwnd edit-control))
        edit-control))

(let ((counter 0))
	(defun create-new-document (client-window)
		(let ((window (make-instance '<mdi-example-child>)))
			(create-window window
					:caption (format nil "roger-~D" (incf counter))
					:parent client-window
					:style (logior WS_VISIBLE))
            (setf (edit-control window) (create-rich-edit-control window))
            (show-window (edit-control window) SW_SHOW)
            (update-window (edit-control window)))))

(defmethod handle-message ((window <mdi-example-child>) (message <size-message>) wparam lparam)
	(declare (ignore wparam))
	(let ((result (call-next-method)))
        (if (edit-control window)
            (let ((width (LOWORD lParam))
                  (height (HIWORD lparam)))
                (set-position (edit-control window) 2 2 (- width 4) (- height 4))))
        result))

(defmethod handle-message ((window <mdi-example-frame>) (message <size-message>) wparam lparam)
	(declare (ignore wparam))
    (let ((width (loword lparam))
          (height (hiword lparam)))
        (set-position (status-bar window) 0 (- height 20) width 20 TRUE)
        (set-position (mdi-client window) 0 0 width (- height 20) TRUE)
        (call-next-method)))

(defmethod create-window ((window <mdi-example-frame>)
		&key class-name ex-style caption style x y width height parent menu param)
    (declare (ignore class-name ex-style caption style x y width height parent menu param))
    (call-next-method)   
    ;; create status bar
    (setf (status-bar window) (create-status-bar window))
    (set-text (status-bar window) "This is the main frame")
    (show-window (status-bar window) SW_SHOW)
    (update-window window))
 
(defmethod handle-message ((window <mdi-example-frame>) (message <create-message>) wparam lparam)
	(declare (ignore window message wparam lparam))
    (call-next-method))
 
(defun mdi-example ()
    (gui-initialize)
	(let ((window (make-instance '<mdi-example-frame>)))
		(create-menu window '(:menu "File") nil 1)
		(create-menu window 
			(list :command "New" 
				(lambda () (create-new-document (mdi-client window))))
			"File" 1)
		(create-window window
			:caption "mdi window"
			:width 500
			:height 500 :style (logior WS_VISIBLE WS_CLIPCHILDREN))
		(show-window window SW_SHOW)
		(update-window window)
		(mdi-message-loop window)))
