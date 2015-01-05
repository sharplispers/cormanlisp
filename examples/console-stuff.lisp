;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;; File:      console.lisp
;;;; Contents:  Console utilities.
;;;; History:   11/14/02  RGC  Created.
;;;;
#|
To create a console application, load this file and then execute:
(ccl:save-application "console-main" #'console-main)
|#

(in-package :win32)
#! (:library "Kernel32" :export t :pascal "WINAPI")
BOOL WINAPI AllocConsole();
BOOL WINAPI ReadConsoleW(HANDLE hConsoleInput, LPVOID lpBuffer,
    DWORD nNumberOfCharsToRead, LPDWORD lpNumberOfCharsRead, LPVOID lpReserved);
BOOL WINAPI WriteConsoleW(HANDLE hConsoleOutput, CONST VOID *lpBuffer,
    DWORD nNumberOfCharsToWrite, LPDWORD lpNumberOfCharsWritten, LPVOID lpReserved);
HANDLE WINAPI GetStdHandle(DWORD nStdHandle);

#define STD_INPUT_HANDLE    (/*(DWORD)-10*/ 0xfffffff6)
#define STD_OUTPUT_HANDLE   (/*(DWORD)-11*/ 0xfffffff5)
#define STD_ERROR_HANDLE    (/*(DWORD)-12*/ 0xfffffff4)
!#

(defpackage "CONSOLE")
(in-package "CONSOLE")
(provide "CONSOLE")

(defconstant console-critical-section (cl::allocate-critical-section))

;; allocate wide char foreign buffer
(defconstant console-text-buffer 
    (ct:malloc (* 2 (cl::stream-input-buffer-length *terminal-io*))))

;; allocate once here, rather than each time functions are called
(defconstant console-num-ptr (ct:malloc (ct:sizeof 'win:DWORD)))

(cl::declare-synchronized-function console-critical-section
	DLL-TERMINAL-IO-UNDERFLOW (stream)
        (force-output stream)
        (let* ((input-handle (win:GetStdHandle win:STD_INPUT_HANDLE))
               (buf (cl::stream-input-buffer stream))
    	       (num (cl::stream-input-buffer-length stream)))     
            (win:ReadConsoleW input-handle 
                console-text-buffer num console-num-ptr ct:null)
            (let ((num-received (ct:cref (win:DWORD *) console-num-ptr 0)))
                (setf (cl::stream-input-buffer-pos stream) 0)
                (setf (cl::stream-input-buffer-num stream) num-received)
                (dotimes (i num-received)
                    (setf (elt buf i) 
                        (int-char (ct:cref (:unsigned-short *) console-text-buffer i))))
                num-received)))

(cl::declare-synchronized-function console-critical-section
    DLL-TERMINAL-IO-OVERFLOW (stream)
        (let ((output-handle (win:GetStdHandle win:STD_OUTPUT_HANDLE))
              (buf (cl::stream-output-buffer stream))
    		  (num (cl::stream-output-buffer-pos stream)))
            (win:WriteConsoleW output-handle (ct:lisp-string-to-unicode buf) 
                num console-num-ptr ct:null)
            (setf (cl::stream-output-buffer-pos stream) 0)
            num))

(defun setup-console ()
    (win:AllocConsole)
    (setf (uref *terminal-io* cl::stream-overflow-func-offset) #'dll-terminal-io-overflow) 
    (setf (uref *terminal-io* cl::stream-underflow-func-offset) #'dll-terminal-io-underflow))

(defun console-prompt (index)
    (format nil "~A-~D>" (package-name *package*) index))

;;; custom top level loop
(defun console-loop ()
    (in-package :common-lisp-user)
	(write (cl::copyright-notice) :escape nil)
	(terpri *standard-output*)
	(force-output)
	(let ((init-path (concatenate 'string ccl::*cormanlisp-directory* "init.lisp")))
		(if (and (not cl::*loading-kernel*)(probe-file init-path))
			(load init-path)))
	(do (expr 
         result 
        (stack-overflow nil)
        (index 0 (+ index 1)))
		(nil)
		(restart-case
			(progn
				(block eval-expression
					(handler-bind 
						((win:stack-overflow 
								(lambda (condition) 
									(format *error-output* "~A~%" condition)
									(force-output *error-output*)
									(setf stack-overflow t)
									(return-from eval-expression condition))))
						(progn
							(setq cl::*read-level* 0)
							(write (console-prompt index) :escape nil) 
							(setq expr (read *standard-input* nil 'Eof nil))
							(if (eq expr 'quit)
								(return 'quit))
                                                        
							(setq - expr)
							(unwind-protect
								(setq result (multiple-value-list (eval expr))))					
							(cl::update-toplevel-globals expr result)
							(if (null result) 
								(force-output)
								(dolist (i result)
									(write i)
									(terpri)
									(force-output))))))
				(if stack-overflow (cl::protect-stack)))
			(abort () :report "Abort to top level." 
				(format *standard-output* "~%;;; Returning to top level loop.~%")
				(go next)))
		next))

(defun main ()
    (setup-console)
    (console-loop))

