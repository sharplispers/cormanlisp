;;;; ACL excl wrapper library for Corman Lisp - Version 1.1
;;;;
;;;; Copyright (C) 2000 Christopher Double. All Rights Reserved.
;;;; 
;;;; License
;;;; =======
;;;; This software is provided 'as-is', without any express or implied
;;;; warranty. In no event will the author be held liable for any damages
;;;; arising from the use of this software.
;;;;
;;;; Permission is granted to anyone to use this software for any purpose,
;;;; including commercial applications, and to alter it and redistribute
;;;; it freely, subject to the following restrictions:
;;;;
;;;; 1. The origin of this software must not be misrepresented; you must
;;;;    not claim that you wrote the original software. If you use this
;;;;    software in a product, an acknowledgment in the product documentation
;;;;    would be appreciated but is not required.
;;;;
;;;; 2. Altered source versions must be plainly marked as such, and must
;;;;    not be misrepresented as being the original software.
;;;;
;;;; 3. This notice may not be removed or altered from any source 
;;;;    distribution.
;;;;
;;;; Notes
;;;; =====
;;;; A simple implementation of some of the EXCL package from Allegro
;;;; Common Lisp. Intended to be used for porting various ACL packages,
;;;; like AllegroServe. 
;;;;
;;;; More recent versions of this software may be available at:
;;;;   http://www.double.nz/cl
;;;;
;;;; Comments, suggestions and bug reports to the author, 
;;;; Christopher Double, at: chris@double.nz
;;;;
;;;; 20/09/2000 - 1.0 
;;;;              Initial release.
;;;;              Many of the implementations are stubbed out and
;;;;              need to be completed. Includes the IF* macro
;;;;              placed in the public domain by John Foderaro. 
;;;;              See: http://www.franz.com/~jkf/ifstar.txt
;;;;
;;;; 06/03/2001 - 1.1
;;;;              Implemented MATCH-REGEXP.
;;;;              Implemented FILESYS-TYPE.
;;;;  			  Implemented INTERN*.
;;;;  			  Implemented *CURRENT-CASE-MODE*.
;;;;              Implemented ERRORSET.
;;;;              Changed :export form on defpackage so as not
;;;;              to duplicate symbols in current package.
;;;;
(require 'nregex)

(defpackage :excl
	(:use :common-lisp :nregex)
	(:export 
		"IF*"
		"*INITIAL-TERMINAL-IO*"
		"*CL-DEFAULT-SPECIAL-BINDINGS*"
		"FILESYS-SIZE"
		"FILESYS-WRITE-DATE"
		"STREAM-INPUT-FN"
		"MATCH-REGEXP"
		"*CURRENT-CASE-MODE*"
		"INTERN*"
		"FILESYS-TYPE"
		"ERRORSET"
		))

(in-package :excl)

(defvar if*-keyword-list '("then" "thenret" "else" "elseif"))

(defmacro if* (&rest args)
   (do ((xx (reverse args) (cdr xx))
	(state :init)
	(elseseen nil)
	(totalcol nil)
	(lookat nil nil)
	(col nil))
       ((null xx)
	(cond ((eq state :compl)
	       `(cond ,@totalcol))
	      (t (error "if*: illegal form ~s" args))))
       (cond ((and (symbolp (car xx))
		   (member (symbol-name (car xx))
			   if*-keyword-list
			   :test #'string-equal))
	      (setq lookat (symbol-name (car xx)))))

       (cond ((eq state :init)
	      (cond (lookat (cond ((string-equal lookat "thenret")
				   (setq col nil
					 state :then))
				  (t (error
				      "if*: bad keyword ~a" lookat))))
		    (t (setq state :col
			     col nil)
		       (push (car xx) col))))
	     ((eq state :col)
	      (cond (lookat
		     (cond ((string-equal lookat "else")
			    (cond (elseseen
				   (error
				    "if*: multiples elses")))
			    (setq elseseen t)
			    (setq state :init)
			    (push `(t ,@col) totalcol))
			   ((string-equal lookat "then")
			    (setq state :then))
			   (t (error "if*: bad keyword ~s"
					      lookat))))
		    (t (push (car xx) col))))
	     ((eq state :then)
	      (cond (lookat
		     (error
		      "if*: keyword ~s at the wrong place " (car xx)))
		    (t (setq state :compl)
		       (push `(,(car xx) ,@col) totalcol))))
	     ((eq state :compl)
	      (cond ((not (string-equal lookat "elseif"))
		     (error "if*: missing elseif clause ")))
	      (setq state :init)))))

(defvar *initial-terminal-io* *terminal-io*)
(defvar *cl-default-special-bindings* nil)

(defun filesys-size (stream)
	(file-length stream))

(defun filesys-write-date (stream)
	(file-write-date stream))

(defun stream-input-fn (stream)
	stream)

(defun match-regexp (pattern string)
	(regex pattern string))

(defvar *current-case-mode* :case-insensitive-upper)

(defun intern* (s len package)
	(intern (string-upcase (subseq s 0 len)) package))

(defun filesys-type (file-or-directory-name)
	(if (ccl::directory-p file-or-directory-name)
		:directory
		(if (probe-file file-or-directory-name)
			:file
			nil)))

(defmacro errorset (&body form)
	`(let* ((ok nil)
			(results 
				(ignore-errors
					(prog1
						(multiple-value-list
							,@form)
						(setq ok t)))))
		(if ok
			(apply #'values t results)
			nil)))

(provide 'acl-excl)
