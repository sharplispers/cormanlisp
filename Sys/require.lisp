;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;; require.lisp -- module facility for Corman Lisp
;;;;
;;;; Author:        Vassili Bykov <vassili@objectpeople.com>
;;;; Created:       11/15/1998
;;;; Last modified: 01/05/1999
;;;;
;;;; Implements PROVIDE and REQUIRE according to the HyperSpec and
;;;; some stuff around them to support automatic search for modules,
;;;; within the wiggle room the HyperSpec leaves.

(in-package :common-lisp)

(export '(require))
	  ;; PROVIDE is already exported, though we redefine it here

(export '(ccl::register-module-source 
		  ccl::push-module-directory)
		(find-package "CORMANLISP"))

(defun %absolute-pathname-p (pathname)
  (or (and (< 0 (length pathname))
	   (char= #\\ (char pathname 0)))
      (and (< 2 (length pathname))
	   (string= ":\\" (subseq pathname 1 3)))))
	   
(defun %absolutized-pathname (pathname)
  (if (%absolute-pathname-p pathname)
      pathname
      (concatenate 'string 
			(cormanlisp-directory)
			pathname)))

;;; PROVIDE is already defined (misc.lisp).

;;; *MODULES* is already defined there, too.

(defvar *module-default-directories* nil)
(defvar *module-sources* nil)

(defun ccl::register-module-source (module-name pathname-list)
  (when (symbolp module-name)
    (setq module-name (symbol-name module-name)))
  (unless (listp pathname-list)
    (setq pathname-list (list pathname-list)))
  (setq pathname-list (mapcar #'%absolutized-pathname pathname-list))
  (let ((entry (assoc module-name *module-sources* :test #'string=)))
    (if entry
	(setf (cdr entry) pathname-list)
        (push (cons module-name pathname-list)
	      *module-sources*))))

(defun ccl::push-module-directory (string)
  (assert (stringp string))
  (unless (char= #\\ (char string (1- (length string))))
    (setq string (concatenate 'string string "\\")))
  (push (%absolutized-pathname string) *module-default-directories*))

(defun %load-from-registered (module-name)
  (let* ((entry (assoc module-name *module-sources* :test #'string=))
	 (pathnames (cdr entry)))
    (and entry
	 (mapc #'load pathnames))))

(defun %load-from-default (module-name)
	(let ((filename-src (concatenate 'string module-name ".lisp"))
		  (filename-binary (concatenate 'string module-name ".fasl"))
		  fullname)
		(dolist (dir *module-default-directories*)
			(setq fullname (concatenate 'string dir filename-binary))
			(when (probe-file fullname)
				(load fullname)
				(return-from %load-from-default t))
			(setq fullname (concatenate 'string dir filename-src))
			(when (probe-file fullname)
				(load fullname)
				(return-from %load-from-default t)))
		nil))

(defun require (module-name &optional pathname-list)
  (when (symbolp module-name)
    (setq module-name (symbol-name module-name)))
  (unless (find module-name *modules* :test #'string=)
    (when pathname-list
      (mapc #'load pathname-list)
      (return-from require))
    (or (%load-from-registered module-name)
	(%load-from-default module-name)
	(error "Cannot find required feature: ~A" module-name))
    (provide module-name)))


(defun provide (module-name)
  (when (symbolp module-name)
    (setq module-name (symbol-name module-name)))
  (pushnew module-name *modules* :test #'string=)
  module-name)

;;;
;;;	Define threads package and create stub loader.
;;;
(defpackage "THREADS"
	(:export 
		"CREATE-THREAD" 
		"EXIT-THREAD"
	)
	(:nicknames "TH"))

(in-package :threads)
(defun create-thread (&rest args)
	(require "THREADS")
	(apply 'create-thread args))

(defun exit-thread (&rest args)
	(require "THREADS")
	(apply 'exit-thread args))
	
(defvar exit-thread-tag	'cl::%exit_thread_tag)
