(push-module-directory "MODULES")
(push-module-directory "SYS")
;;(setf (current-directory) *cormanlisp-directory*)

;;; Use external WWW-browser for documentation lookups
;;(setf ccl::*use-external-browser* t)

;;;
;;; Various special variables used by the system or the IDE
;;;

;;; Set sockets:*ipv6* to NIL if you wish to disable
;;; IPv6 socket functionality. Valid values are NIL, :only or t.
(setq sockets:*ipv6* (when (sockets:ipv6-installed-p) t))

;;; Set ccl:*auto-update-enabled* to NIL if you wish to disable
;;; automatic checking for updates (patches) by the IDE.
(setf ccl:*auto-update-enabled* nil)

;; for testing only
;;(defparameter *patch-root-directory* "/CormanLisp/patches/3_01f1/")

;;; If you need to connect through a proxy server,
;;; uncomment the following and set the server and port correctly.
;;; Note that internet windows in the IDE use Internet Explorer's proxy
;;; settings. The setting below is only needed for the auto-update feature or
;;; any calls made via the SOCKETS package.
;;;
#|
(setf sockets:*default-proxy-server*
    (make-instance 'sockets:generic-proxy-server 
        :host "proxy.myserver.com"      ;; your proxy server name or IP address
        :port 8080))                    ;; your proxy server port ID
|#

;;;
;;; If running in the IDE, initialize menus
;;;
(when (eq (cormanlisp-client-type) :ide-client)
    (ide::setup-ide-menus))

;;; IDE colorization overrides
;;; To define a custom color:
;;; example: (defconstant red (win:RGB 255 0 0))
;;;
(setf ide:comment-format 
    (ide:make-text-format 
        :color ide:dark-green
        :italic t))

(setf ide:keyword-format 
    (ide:make-text-format 
        :color ide:blue))

(setf ide:lisp-symbol-format 
    (ide:make-text-format 
        :bold t))

(setf ide:string-format 
    (ide:make-text-format 
        :color (win:RGB #xa0 #x00 #x00)
        :italic t))

;; the color of the user's preference will be used if it has been set, and
;; the color of normal-format ignored
(setf ide:normal-format (ide:make-text-format :bold nil :italic nil :color ide:black))

(when (eq (cormanlisp-client-type) :console-client)
	(setf cl::*top-level-prompt* "?")
	(format t "Type :quit to exit.~%")
	(values))

;;; set your own local path for the Hyperspec
;; eg. (setq *hyperspec-local-path* "c:/roger/lisp/HyperSpec/")
;; unless the Hyperspec is already installed at this location
;; it to that location

;; Automatically set HyperSpec path to the one installed with Corman Lisp.
(let ((hyperspec-install-path (merge-pathnames "HyperSpec\\"
											   (namestring *cormanlisp-directory*))))
  (when (probe-file (merge-pathnames "Front\\Contents.htm" hyperspec-install-path))
	(setq *hyperspec-local-path* (namestring hyperspec-install-path))))
  
;;; set your own declaration symbols list
;; eg. (setf ide:*declaration-symbols* '("defun" "define-symbol-macro")) or
;;     (setf ide:*declaration-symbols* (append ide:*declaration-symbols* '("defwinconstant" "defwinapi")))
(setf ide:*declaration-symbols* '("defun" "defconstant" "defparameter" "defvar" "defclass" 
                               "defmacro" "defmethod" "defasm" "defop" "defgeneric"
                               "deftype" "defstruct" "defsetf" "defpackage" "in-package"))

;; Clear out any History menu items left from building the image file
(cl::truncate-command-history 0)

;; Auto-update feature
(when (and ccl:*auto-update-enabled*
        (eq (cormanlisp-client-type) :ide-client))
    (ccl:auto-update))

;; utility function to load default image
;; (handy for reloading default Corman Lisp image after rebuilding)
(defun cl:load-default-image ()
    "Load default Corman Lisp image"
    (load-image (concatenate 'string
            *cormanlisp-directory*
            "CormanLisp.img")))
;; export from CL package
(export (find-symbol "LOAD-DEFAULT-IMAGE" 'cl) 'cl)

