;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		save-application.lisp
;;;;	Contents:	Corman Lisp SAVE-APPLICATION implementation.
;;;;	History:	10/2/98  RGC  Created.
;;;;			5/13/99  UR   added AutoCAD ARX support
;;;;                10/28/16 Artem Boldarev Obey current directory

(in-package "CORMANLISP")
(export 'SAVE-APPLICATION)

(defun name-and-extension (filename)
	(let (name
		 (extension (subseq filename (max (- (length filename) 4) 0) (length filename))))
		(cond ((string-equal extension ".EXE")
			   (setq name (subseq filename 0 (- (length filename) 4))))
			  ((string-equal extension ".ARX")
			   (setq name (subseq filename 0 (- (length filename) 4))))	
			  (t (setq name filename extension ".exe")))
		(values name extension)))
;;;
;;; AutoCAD ARX needs a version specifier in the filename, 
;;; either 13, 14 or 15. The application name should carry this name 
;;; to identify it correctly. But the image name can be common for all 
;;; versions so we try to strip the last two numbers.
;;; Returned are the filenames without extension.
;;;
(defun acad-app-and-imagename (appname bootapp)
	(multiple-value-bind (name extension)
		(name-and-extension appname)
		(declare (ignore extension))
	  	(let ((imgname name)
		 	  (version (subseq bootapp 5 7)))
		  (if (eq #\1 (aref name (- (length name) 2)))
			(setq imgname (subseq name 0 (- (length name) 2)))
			(setq appname (concatenate 'string name version)))
  		  (values appname imgname))))

(defun %normalize-directory-name (path-namestring)
    (when (zerop (length path-namestring))
        (return-from %normalize-directory-name path-namestring))
    (if (char= (aref path-namestring
                        (1- (length path-namestring)))
                 #\\)
        path-namestring
        (concatenate 'string path-namestring "\\")))

;;;
;;;	Corman Lisp SAVE-APPLICATION function.
;;;		
(defun save-application (application-name start-function 
						&key (console nil) acad2000 acadr14 acadr13 (static nil))
    ;; accept pathnames as well
    (when (typep application-name 'pathname)
        (setf application-name (namestring application-name)))
	(let ((application-file-name nil)
		  (image-file-name nil)
		  (boot-app-name (cond ((and console static) "clconsoleapp.exe")
							   (console  "clconsole.exe")
							   (acad2000 "cormanlisp15.arx")
							   (acadr14  "cormanlisp14.arx")
							   (acadr13  "cormanlisp13.arx")
							   (static "clbootapp.exe")
							   (T "clboot.exe")))
		  (save-top-level cl::*top-level*))
		(if (or acad2000 acadr14 acadr13)
			(multiple-value-bind (appname imgname)
				(acad-app-and-imagename application-name boot-app-name)
				(setq application-file-name (concatenate 'string appname ".arx"))
				(setq image-file-name (concatenate 'string imgname ".img")))
			(multiple-value-bind (name extension)
				(name-and-extension application-name)
				(setq application-file-name (concatenate 'string name extension))
				(setq image-file-name application-file-name)))
		(if (string= application-file-name image-file-name)
			(format t ";; Creating application ~A~%"
				application-file-name)
			(format t ";; Creating application ~A and image ~A~%"
				application-file-name
				image-file-name))
	
		;; copy the clboot.exe file
		(if (probe-file application-file-name)
			(delete-file application-file-name))	;; delete file if it already exists
		(win32:CopyFile 
			(ct:create-c-string (concatenate 'string
                                              (%normalize-directory-name *cormanlisp-server-directory*)
                                              boot-app-name))
			(ct:create-c-string application-file-name)
			t)
		(setf cl::*top-level*
			#'(lambda ()
				(catch 'common-lisp::%error
					(catch :system-exception
						(funcall start-function)))
				(win32:ExitThread 1)))		;; application should call (ExitThread 0) on normal exit
		(force-output)
		(save-image image-file-name)
		(setf cl::*top-level* save-top-level)
		application-file-name))
