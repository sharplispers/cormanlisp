;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		auto-update.lisp
;;;;	Contents:	Corman Lisp miscellaneous features.
;;;;	History:	08/21/06 RGC  Created.
;;;;
(in-package :ccl)
(export '(
        *cormanlisp-patch-level* 
        auto-update
        patch-rollback 
        *auto-update-enabled*
        *patch-root-directory*
        *patch-server*
        compile-cormanlisp-image
        load-default-image
        *auto-update-level*))

(defparameter *cormanlisp-patch-level* (cormanlisp-patch-level))
(defparameter *patches-available* nil)
(defparameter *patches-installed* nil)
(defparameter *patch-server* "www.cormanlisp.com")
(defparameter *patch-root-directory* "/CormanLisp/patches/3_01f1/")
(defparameter *auto-update-enabled* nil) ; disabled by default
(defparameter *auto-update-level* 1)        ;; version of auto-update
(defparameter *patch* nil)

(defstruct cormanlisp-patch
    level  ;; int
    pathname
    install-func
    uninstall-func) ;; relative file name

(defun make-patch (filename)
    (make-cormanlisp-patch :level (ccl::get-patch-level-from-name filename) :pathname filename))

(defun define-patch (level filename)
    (push (make-cormanlisp-patch :level level :pathname filename) *patches-available*))

(defun local-patches-directory ()
    (merge-pathnames  "patches/" *cormanlisp-directory*))

(defun local-patches-backup-directory (patch-level)
    (merge-pathnames (format nil "patches/backup/3_01/~d/" patch-level)
         *cormanlisp-directory*))

(defun get-patch-level-from-name (pathname)
    (let ((filename (pathname-name pathname)))
        (values (parse-integer filename :start (+ (position #\_ filename :from-end t) 1)
            :junk-allowed t))))

(defun update-patch-index ()
    (ensure-directories-exist (local-patches-directory))
    (sockets:get-http-file 
        *patch-server*
        (concatenate 'string *patch-root-directory* "CormanLisp_3_01_patch_index.lisp")
        (merge-pathnames "CormanLisp_3_01_patch_index.lisp"
            (local-patches-directory))))

(defun download-patch (patch)
    (sockets:get-http-file 
        *patch-server*
        (format nil "~A~D" *patch-root-directory* (cormanlisp-patch-pathname patch))
        (merge-pathnames (cormanlisp-patch-pathname patch)
            (local-patches-directory))))

(defun compile-cormanlisp-image ()
  (format *terminal-io* "Building CormanLisp.img file...~%")
  (win:shell-execute (namestring (merge-pathnames "clconsole.exe" *cormanlisp-directory*))
					 (format nil " -image \"\" -execute \"~a\""
							 (namestring (merge-pathnames "sys\\compile-sys.lisp" *cormanlisp-directory*))))
  ;;(win:shell-execute (namestring (merge-pathnames "makeimg.bat" *cormanlisp-directory*)) "")
  (win:message-box-ok "A console process has been launched which is recompiling the CormanLisp.img file." "Information")
  t)

;; utility function to load default image
;; (handy for reloading default Corman Lisp image after rebuilding)
(defun load-default-image ()
  "Load default Corman Lisp image"
  (load-image (concatenate 'string
                           *cormanlisp-directory*
                           "CormanLisp.img")))

(defun install-patch (patch)
    (ensure-directories-exist (local-patches-backup-directory (cormanlisp-patch-level patch)))
    (let ((filename (download-patch patch)))
        (unless filename
            (format *terminal-io* "Could not load patch: ~A~%" (cormanlisp-patch-pathname patch))
            (return-from install-patch nil))
        (format *terminal-io* "Loading patch level ~D...~%" (cormanlisp-patch-level patch))
        (let ((*patch* patch))
            (load filename)
            (if (cormanlisp-patch-install-func patch)
                (funcall (cormanlisp-patch-install-func patch))))
        (format *terminal-io* "Finished loading patch level ~D.~%" 
            (cormanlisp-patch-level patch))))

(defun patch-upgrade-confirm (new-level)
    (let ((result (eq 
                    (win:message-box-yes-no 
                        (format nil 
                            (concatenate 'string
                                "Your patch level is currently ~D. There are new patches available which will "
                                "bring you up to patch level ~D.~%"
                                "Would you like to have the new patches installed now?")
                            ccl:*cormanlisp-patch-level* 
                            new-level)
                        "Install New Updates")
                    'win:IDYES)))
        (unless result
            (win:message-box-ok
                (concatenate 'string 
                    "If you wish to disable automatic update notification, you can "
                    "set the special variable ccl:*AUTO-UPDATE-ENABLED* to NIL "
                    "in the 'init.lisp' file.")
                "Information"))
        result))

(defun patch-rollback-confirm (new-level)
    (let ((result (eq 
                    (win:message-box-yes-no 
                        (format nil 
                            (concatenate 'string
                                "Your patch level is currently ~D. Your patch level will "
                                "be rolled back to level ~D.~%"
                                "Do you wish to continue with the rollback?")
                            ccl:*cormanlisp-patch-level* 
                            new-level)
                        "Remove Patches")
                    'win:IDYES)))
        result))
       
(defun auto-update ()
    (let ((local-index (update-patch-index))
          (save-patch-level ccl:*cormanlisp-patch-level*))
        (unless local-index
            (format *terminal-io* 
                (concatenate 'string
                    ";; Could not load auto-update index file. "
                    "This may be because you are not connected to the internet, or because you "
                    "need to configure proxy server settings (see the 'init.lisp' file)~%"))
            (return-from auto-update nil))
        (load local-index)
        
        ;; if the patch level was upgraded, recompile the image
        (when (>  ccl:*cormanlisp-patch-level* save-patch-level)
            (compile-cormanlisp-image))))

;;;
;;; You can rollback to any patch level from 0 to ccl:*cormanlisp-patch-level*
;;;
(defun patch-rollback (level)
    (let ((save-patch-level ccl:*cormanlisp-patch-level*))
        (unless (and (>= level 0)
                (< level ccl:*cormanlisp-patch-level*)
                (patch-rollback-confirm level))
            (return-from patch-rollback))
        (dolist (patch ccl::*patches-installed*)
            (if (> (cormanlisp-patch-level patch) level)
                (progn
                    (if (cormanlisp-patch-uninstall-func patch)
                        (funcall (cormanlisp-patch-uninstall-func patch)))
                    (delete-file (cormanlisp-patch-pathname patch))
                    (pop ccl::*patches-installed*)
                    (decf *cormanlisp-patch-level*))))
        
        ;; if the patch level was downgraded, recompile the image
        (when (<  ccl:*cormanlisp-patch-level* save-patch-level)
            (compile-cormanlisp-image)
            (win:message-box-ok
                (format nil "The new patch level is ~D" ccl:*cormanlisp-patch-level*)
                "Information"))))

(defmacro defpatch (level &key (install-func nil) (uninstall-func nil))
    `(progn
        (setf (ccl::cormanlisp-patch-install-func ccl::*patch*) ',install-func)
        (setf (ccl::cormanlisp-patch-uninstall-func ccl::*patch*) ',uninstall-func)
        (setf ccl::*cormanlisp-patch-level* ,level)
        (push ccl::*patch* ccl::*patches-installed*)))  
