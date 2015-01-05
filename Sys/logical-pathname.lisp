;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		logical-pathname.lisp
;;;;	Contents:	Corman Lisp logical-pathname implementation.
;;;;	History:	04/05/06  RGC  Created.
;;;;
(in-package :pathnames)

(defparameter *logical-pathname-hosts* (make-hash-table :test 'equalp))
(setf (gethash "SYS" *logical-pathname-hosts*) t)
(defun logical-host-p (host) (values (gethash host *logical-pathname-hosts*)))

(defun parse-logical-pathname-namestring (string start end junk-allowed host)  
	(declare (ignore host junk-allowed))
	(setq string (subseq string start end))
	(let ((name nil)
		  (type nil)
          (version nil)
          (relative nil)
          (directories '())
		  (found-dot nil)
          (chars '())
          (state ':host))

		(do* ((index 0 (1+ index))
			  (length (length string))
			   c)
			((or (= index length) (eq state :done)))
			(setq c (elt string index))
			(case state
                (:host
                    (case c
                        (#\: 		(setq state :directory)
                                    (if chars (setq host (concatenate 'string (nreverse chars))))
                                    (setq chars '()))
                        (otherwise  (push c chars))))
   				(:directory 
					(case c
						(#\; 	    (if (and (null directories)(null chars))
                                        (setq relative t)
                                        (progn
                                            (push (concatenate 'string (nreverse chars)) directories)
                                            (setq chars nil))))
                        (#\.        (setq name (concatenate 'string (nreverse chars)) chars '() state :type))
						(otherwise	(push c chars))))
				(:type 
					(case c
						(#\. 	    (setq type (concatenate 'string (nreverse chars)) chars '() state :version))
						(otherwise	(push c chars))))
				(:version  (push c chars))))
        
        ;; handle the final chars
        (if chars
            (case state
                (:version (setq version (concatenate 'string (nreverse chars))))
                (:type (setq type (concatenate 'string (nreverse chars))))
                (:directory (setq name (concatenate 'string (nreverse chars))))
                (:host (error "No host marker was found"))))
        
        (when (null name)
            (setq name (car directories))
            (setq directories (cdr directories)))
        
        (setq directories (nreverse directories))
        
		(if (consp directories)
            (if relative 
                (push ':relative directories)
                (push ':absolute directories)))

        ;(if (and type (null name) (not found-dot)) (setq name type type nil))
		(make-pathname :host host :directory directories
			:name name :type type :version version :logical t)))
    