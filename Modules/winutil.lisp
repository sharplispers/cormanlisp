;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	All rights reserved.
;;;;	-------------------------------
;;;;
;;;;	File:		winutil.lisp
;;;;	Contents:	Windows utility functions for Corman Lisp.
;;;;	History:	07/11/03  RGC  Created.
;;;;

(in-package :win)
(provide "WINUTIL")

(export '(register-class))

(defvar *window-class-atoms* (make-hash-table :test 'equal))

(defun window-class-registered-p (class-name &optional hinstance)
    (let ((wndclass (ct:malloc (ct:sizeof 'WNDCLASSEX))))
        (unwind-protect
            (progn
                (GetClassInfoEx (or hinstance
                                    (cl::get-application-instance))
                                (ct:create-c-string class-name)
                                wndclass))
            (ct:free wndclass))))
    

;;;
;;; Window class registration
;;;
(defun register-class (class-name &optional (window-proc 'gui-wndproc)
        &key (win-style (logior CS_HREDRAW CS_VREDRAW)))
    (if (eq window-proc t)
        (setf window-proc 'gui-wndproc))
    (if (symbolp window-proc)
        (setf window-proc (get-callback-procinst window-proc)))
    
    ;; see if it has already been registered
    (let ((atom (gethash class-name *window-class-atoms*)))
        (when (and atom
                   (window-class-registered-p class-name (cl::get-application-instance)))
            (return-from register-class atom)))
    
	(let ((wndclass (ct:malloc (ct:sizeof 'WNDCLASSEX))))
		(with-c-struct (s wndclass WNDCLASSEX)
			(setf 
				cbSize 			(sizeof 'WNDCLASSEX)
				style 			win-style
				lpfnWndProc 	window-proc
				cbClsExtra 		0
				cbWndExtra 		0
				hInstance 		(cl::get-application-instance)
				hIcon 			(LoadIcon NULL IDI_APPLICATION)
				hCursor 		(LoadCursor NULL IDC_ARROW)
				hbrBackground 	(GetStockObject WHITE_BRUSH)
				lpszMenuName 	NULL
				lpszClassName 	(ct:create-c-string class-name)
				hIconSm 		(LoadIcon NULL IDI_APPLICATION)))
        (let ((atom (RegisterClassEx wndclass)))
            (when (= atom 0)
                (error "RegisterClassEx() failed"))
            (setf (gethash class-name *window-class-atoms*) atom)
            atom)))

;;;
;;; Given a registered window class, return the window-proc (as a foreign pointer to code).
;;;
(defun get-window-proc (class-name)
	(let ((wndclass (ct:malloc (ct:sizeof 'WNDCLASSEX))))
        (setf (ct:cref WNDCLASSEX wndclass cbSize) (sizeof 'WNDCLASSEX))
        (when (not (GetClassInfoEx NULL (ct:create-c-string class-name) wndclass))
            (error "GetClassInfoEx() failed"))
        (ct:cref WNDCLASSEX wndclass win::lpfnWndProc)))


