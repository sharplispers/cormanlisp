;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		toplevel.lisp
;;;;	Contents:	Corman Lisp 3.0 top level code.
;;;;	History:	11/9/96  RGC  Created.
;;;;				2/23/01  RGC  Updated to 1.43.
;;;;				8/1/01   RGC  Updated to 1.5.
;;;;				05/31/02 RGC  Updated to 2.0.
;;;;							  Modified copyright information to include
;;;;							  licensing information.
;;;;

;;;;
;;;;	Normal top level user input function.
;;;;	This will get executed at startup and for the duration of an
;;;;	interactive session.
;;;;	By default, this function is the value of the variable *top-level*.
;;;;

(defvar + nil)
(defvar ++ nil)
(defvar +++ nil)
(defvar - nil)
(defvar * nil)
(defvar ** nil)
(defvar *** nil)
(defvar / nil)
(defvar // nil)
(defvar /// nil)

(defvar *command-history* '())
(defvar *command-history-max-length* 10)

(defun remove-last-n (n list)
    (if (> (length list) n) (cons (car list) (remove-last-n n (cdr list)))))

(defun truncate-command-history (length)
    (setf *command-history* 
        (remove-last-n (- (length *command-history*) length) *command-history*)))

(defun add-command (cmd)
    (push cmd *command-history*)
    (when (> (length *command-history*) *command-history-max-length*)
        (truncate-command-history *command-history-max-length*)))

(defun recall-command (n)
    (if (and (<= 0 n) (< n (length *command-history*)))
        (let ((*print-case* :downcase)
              (*print-pretty* t))
            (format *terminal-io* "~%~S" (elt *command-history* n))
            (force-output *terminal-io*))))


(defun copyright-notice-short ()
	(format nil "Corman Lisp ~A  (Patch level ~D)~%;; Copyright ~A Corman Technologies Inc. See LICENSE.txt for license information." 
			(cl::lisp-implementation-version-string)
            ccl::*cormanlisp-patch-level*
			(if (= (cl::cormanlisp-client-type) 1) "(c)" (int-char 169))))
	
(defun copyright-notice ()
  (format nil ";; ~A~%~A" 
		  (copyright-notice-short)
		  (multiple-value-bind (registered version name organization days-remaining)
			  (cl::registration-info)
			(declare (ignore registered version organization days-remaining))
			(format nil ";; User: ~A." name))))
				
(defun version-caption ()
    (format nil "Corman Lisp ~A  (Patch level ~D)" 
			(cl::lisp-implementation-version-string)
            ccl::*cormanlisp-patch-level*))

(defvar *top-level-prompt* "")

;;;
;;; Called each time through the toplevel and debugger functions
;;; to update common lisp special variables.
;;;
(defun update-toplevel-globals (expr result)
	;; update top level variables
	(unless (or (member expr '(+++ ++ + - *** ** * /// // /))
                (and (consp expr) (eql (first expr) 'CORMANLISP::EXECUTE-USER-COMMAND)))
		(progn
			(setq +++ ++)
			(setq ++ +)
			(setq + expr)
			(setq *** **)
			(setq ** *)
			(setq * (if (consp result) (car result) result))
			(setq /// //)
			(setq // /)
			(setq / result)
            (add-command expr))))

(defun top-level ()
	(write (copyright-notice) :escape nil)
	(terpri *standard-output*)
	(force-output)
	(if (and (not *loading-kernel*)(probe-file "init.lisp"))
		(load "init.lisp"))
	(do (expr result (normal-exit nil) sys-exception)
		(nil)
		(catch 'common-lisp::%error
			(setq sys-exception
				(catch :system-exception
					(progn
						(setq normal-exit nil)
						(setq *read-level* 0)
						(write *top-level-prompt* :escape nil) 
						(setq expr (read *standard-input* nil 'Eof nil))
						(if (eq expr 'quit)
							(return 'quit))
						(if (eq expr 'Eof)
							(return 'Eof))
						(setq - expr)
						(editor-set-message "Working...")
						(unwind-protect
							(setq result (multiple-value-list (eval expr)))
							(editor-set-default-message))
						
						(update-toplevel-globals expr result)
						(if (null result) 
							(force-output)
							(dolist (i result)
								(write i)
								(terpri)
								(force-output)))
						(setq normal-exit t))))
			(unless normal-exit
				(progn
					(format t "A system exception was caught: ~A~%" sys-exception)
					(force-output)
					(if (eq sys-exception :exception-stack-overflow)
						(protect-stack)))))))

(setq *top-level* #'top-level)
(setq *package* (find-package "COMMON-LISP-USER"))

