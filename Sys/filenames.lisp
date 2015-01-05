;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		filenames.lisp
;;;;	Contents:	Pathname and logical filename support for Corman Lisp.
;;;;	History:	3/18/97  RGC  Created.
;;;;				11/11/98 RGC  Modified GET-FULL-PATH-NAME to 
;;;;							  accept a PATHNAME as input.
;;;;				2/18/01  RGC  Added DIRECTORY-NAMESTRING, TRUENAME implementations.
;;;;
(in-package :common-lisp)

(defun cormanlisp-directory ()
	(let* ((dir ccl::*cormanlisp-directory*)
		   (last-char (char dir (1- (length dir)))))
		(if (or (char= last-char #\\)(char= last-char #\/))
			(return-from cormanlisp-directory dir)
			(concatenate 'string dir "\\"))))

;;
;; forward references
;;
(declaim (ftype (function (make-pathname 
			&key host device directory name 
				 type version defaults case) pathname) 
			make-pathname))
(declaim (ftype (function (stream) t) stream-name))
(declaim (ftype (function (stream) t) stream-direction))

(defun namestring (pathname) (declare (ignore pathname)) "#< pathname >#")		;redefined below

(make-package :pathnames)
(in-package :pathnames)

;;;
;;;	The Common Lisp (implementation dependent) PATHNAME class.
;;;
(defstruct (pathname-internal 
	(:constructor construct-pathname 
		(host device directory name type version logical))
	(:print-function print-pathname))
	host
	device
	directory
	name
	type
	version
	defaults
	case
    logical)

(defun print-pathname (pathname stream level) 
	(declare (ignore level))
	(format stream "#P\"~A\"" (namestring pathname)))

(defconstant *default-host* nil)

(defvar *default-pathname-defaults*
	(construct-pathname *default-host* nil nil nil nil nil nil)) ;; initialized later

(defun make-pathname-internal (&key 
	(host (pathname-internal-host *default-pathname-defaults*) supplied-host)
	(device nil supplied-device)
	(directory nil supplied-directory)
	(name nil supplied-name)
	(type nil supplied-type)
	(version :unspecific supplied-version)
	(defaults nil)
	(case :local)
    (logical nil))
	(declare (ignore case))
	(when defaults
        (setq defaults (pathname defaults))
		(unless supplied-host (setq host (pathname-internal-host defaults)))
		(unless supplied-device (setq device (pathname-internal-device defaults)))
		(unless supplied-directory (setq directory (pathname-internal-directory defaults)))
		(unless supplied-name (setq name (pathname-internal-name defaults)))
		(unless supplied-type (setq type (pathname-internal-type defaults)))
		(unless supplied-version (setq version (pathname-internal-version defaults))))

	(construct-pathname host device directory name type version logical))

(defun convert-pathname-to-namestring (pathname)
	(let ((device (pathname-internal-device pathname))
		  (directory (pathname-internal-directory pathname))
		  (type (pathname-internal-type pathname))
		  (name (pathname-internal-name pathname)))
		(format nil "~{~A:~}~{~A~}~{~A\\~}~{~A~}~{.~A~}"
			(if device (list device) nil)
			(if (eq (car directory) :absolute)
				'(#\\) nil)
			(cdr directory)
			(if name (list name) nil)
			(if type (list type) nil))))

(defun convert-pathname-to-directory-namestring (pathname)
	(let ((device (pathname-internal-device pathname))
		  (directory (pathname-internal-directory pathname)))
		(format nil "~{~A:~}~{~A~}~{~A\\~}"
			(if device (list device) nil)
			(if (eq (car directory) :absolute)
				'(#\\) nil)
			(cdr directory))))

(defun logical-host-p (host) (declare (ignore host)) nil)		; not support for logical hosts currently
(defun parse-logical-pathname-namestring (thing start end junk-allowed host)
	(declare (ignore thing start end junk-allowed host))
	"")
(defun valid-logical-pathname-namestring (thing start end junk-allowed)
	(declare (ignore thing start end junk-allowed))
	nil)								; not support for logical pathnames currently

;;; Parse backward from the end
(defun parse-physical-pathname-namestring (string start end junk-allowed host)
	(declare (ignore host junk-allowed))
	(setq string (reverse (subseq string start end)))
	(let ((device nil)
		  (directory nil)
		  (name nil)
		  (type nil)
		  (found-dot nil))

		(do* ((index 0 (1+ index))
			  (length (length string))
			   c
			  (state :type))
			((or (= index length) (eq state :done)))
			(setq c (elt string index))
			(case state
				(:type 
					(case c
						((#\\ #\/) 	(setq name type type nil state :directory)
									(push nil directory))
						(#\. 		(setq state :name found-dot t))
						(#\:		(setq name type type nil state :device))
						(otherwise	(push c type))))
				(:name 
					(case c
						((#\\ #\/) 	(push nil directory) (setq state :directory))
						(#\:		(setq state :device))
						(otherwise	(push c name))))
				(:directory 
					(case c
						((#\\ #\/) 	(push nil directory))
						(#\:		(setq state :device))
						(otherwise	(push c (car directory)))))
				(:device
					(unless (alpha-char-p c)
						(error "Invalid volume in pathname: ~A" c))
					(setq state :done)
					(push c device))))

		(setq device (if device (concatenate 'string device)))
		(setq name (if name (concatenate 'string name)))
		(setq type (if type (concatenate 'string type)))
		(if (and (consp directory) (null (car directory)))
			(setf (car directory) :absolute)
			(if directory (push :relative directory)))
		(if directory
			(dotimes (i (1- (length directory)))
				(setf (elt directory (+ i 1)) 
					(concatenate 'string (elt directory (+ i 1))))))
		(if (and type (null name) (not found-dot)) (setq name type type nil))
		(make-pathname :device device :directory directory
			:name name :type type)))
			

(in-package :common-lisp)

;;;
;;;	Common Lisp PATHNAMEP function.
;;;
(defun pathnamep (obj) (pathnames::pathname-internal-p obj))

;;;
;;;	Common Lisp PATHNAME-VERSION function.
;;;
(defun pathname-version (obj) (pathnames::pathname-internal-version obj))

(setf (symbol-function 'make-pathname) #'pathnames::make-pathname-internal)

;;;
;;;	Common Lisp FILE-STREAM-NAME function.
;;;
(defun file-stream-name (stream) (declare (ignore stream)) #| not defined yet |# nil)

;;;
;;;	Common Lisp CREATE-PATHNAME-FROM-STRING function.
;;;
(defun create-pathname-from-string (string) (values (parse-namestring string)))

;;;
;;;	Common Lisp PATHNAME function.
;;;
(defun pathname (pathspec)
	(cond
		((pathnamep pathspec) pathspec)
		((streamp pathspec) (file-stream-name pathspec))
		((stringp pathspec) (create-pathname-from-string pathspec))
		(t (error "Invalid path specifier: ~A" pathspec))))

;;; need to redefine these standard accessor functions to
;;; take a case argument (even though we currently ignore it)

;;;
;;;	Common Lisp PATHNAME-HOST function.
;;;
(defun pathname-host (pathname &key (case :local))
	(declare (ignore case))
	(pathnames::pathname-internal-host (pathname pathname)))

;;;
;;;	Common Lisp PATHNAME-DEVICE function.
;;;
(defun pathname-device (pathname &key (case :local))
	(declare (ignore case))
	(pathnames::pathname-internal-device (pathname pathname)))

;;;
;;;	Common Lisp PATHNAME-DIRECTORY function.
;;;
(defun pathname-directory (pathname &key (case :local))
	(declare (ignore case))
	(pathnames::pathname-internal-directory (pathname pathname)))

;;;
;;;	Common Lisp PATHNAME-NAME function.
;;;
(defun pathname-name (pathname &key (case :local))
	(declare (ignore case))
	(pathnames::pathname-internal-name (pathname pathname)))

;;;
;;;	Common Lisp PATHNAME-TYPE function.
;;;
(defun pathname-type (pathname &key (case :local))
	(declare (ignore case))
	(pathnames::pathname-internal-type (pathname pathname)))
	
;;;
;;;	Common Lisp LOAD-LOGICAL-PATHNAME-TRANSLATIONS function.
;;;
(defun load-logical-pathname-translations (host)
	(declare (ignore host))
	#| we don't currently do anything with these |#
	nil)

;;;
;;;	Common Lisp LOGICAL-PATHNAME-TRANSLATIONS function.
;;;
(defun logical-pathname-translations (host)
	(declare (ignore host))
	#| we don't currently do anything with these |#
	nil)

;;;
;;;	Common Lisp LOGICAL-PATHNAME function.
;;;
(defun logical-pathname (pathspec)
	(declare (ignore pathspec))
	#| not implemented |#
	nil)

;;;
;;;	Common Lisp NAMESTRING function.
;;;
(defun namestring (pathspec)
	(let ((pathname (pathname pathspec)))
		(pathnames::convert-pathname-to-namestring pathname)))

;;;
;;;	Common Lisp DIRECTORY-NAMESTRING function.
;;;
(defun directory-namestring (pathspec)
	(let ((pathname (pathname pathspec)))
		(pathnames::convert-pathname-to-directory-namestring pathname)))

;;;
;;;	Common Lisp PARSE-NAMESTRING function.
;;;
(defun parse-namestring (thing 
		&optional (host nil) 
				  (default-pathname *default-pathname-defaults*)
		&key (start 0)
			 (end nil)
			 (junk-allowed nil))
	(declare (ignore default-pathname))
	(if (streamp thing)
		(setq thing (file-stream-name thing)))

	(if (pathnamep thing)
		(if (equal host (pathname-host thing))
			(return-from parse-namestring (values thing start))
			(error "Invalid host: ~A" host)))

	(unless (stringp thing)
		(error "Invalid pathname: ~A" thing))
	(unless end (setq end (length thing)))

	(cond 
		((pathnames::logical-host-p host)
		 (pathnames::parse-logical-pathname-namestring thing start end junk-allowed host))
		((and (null host) 
			  (pathnames::valid-logical-pathname-namestring thing start end junk-allowed))
		 (pathnames::parse-logical-pathname-namestring thing start end junk-allowed host))
		(t (values 
			(pathnames::parse-physical-pathname-namestring thing start end junk-allowed host)
			end))))

;;;
;;;	Returns the default device for the passed host.
;;;
(defun host-default-device (host) 
	(declare (ignore host))
	nil)

;;;
;;;	Common Lisp MERGE-PATHNAMES function.
;;;
(defun merge-pathnames (pathname &optional 
		(default-pathname *default-pathname-defaults*)
		(default-version :newest))
	(declare (ignore default-version))
	(setf pathname (pathname pathname))
	(setf default-pathname (pathname default-pathname))
	(let ((host nil)
		  (device nil)
		  (directory nil)
		  (name nil)
		  (type nil)
		  (version nil))
		(if (and (pathname-host pathname)
				(null (pathname-device pathname))
				(string-equal (pathname-host pathname)(pathname-host default-pathname)))
			(setf device (pathname-device default-pathname))
			(setf device (host-default-device (pathname-host pathname))))
		(setf host (or (pathname-host pathname)(pathname-host default-pathname)))
		(unless device
			(setf device (or (pathname-device pathname)(pathname-device default-pathname))))
		(setf directory (or (pathname-directory pathname)(pathname-directory default-pathname)))
		(setf name (or (pathname-name pathname)(pathname-name default-pathname)))
		(setf type (or (pathname-type pathname)(pathname-type default-pathname)))
		(if (null (pathname-name pathname))
			(setf version (or (pathname-version pathname)(pathname-version default-pathname)))
			(setf version (or (pathname-version pathname) default-version)))
		(if (and (eq (car (pathname-directory pathname)) ':relative)
				(consp (pathname-directory default-pathname)))
			(progn
				(setf directory
					(copy-list 
						(append (pathname-directory default-pathname)
							(cdr  ;remove :relative from the front
								(pathname-directory pathname)))))
				;; remove redundant <(string|:wild), :back> pairs
				(do ()
					((funcall
						(lambda ()
							(do ((x directory (cdr x)))
								((null x) t)
								(when (and (or (stringp (second x))(eq (second x) ':wild))
											(eq (third x) ':back))
										(setf (cdr x)(cdddr x))
										(return nil)))))))))
		(make-pathname :host host :device device :directory directory
			:name name :type type :version version)))

;; Make sure the default pathname gets set when an image is loaded.
(flet ((init-default-path ()
			(setf cl::*default-pathname-defaults* (cl:pathname (cl::cormanlisp-directory)))))
	(init-default-path)
	(cl::register-load-image-restore-func #'init-default-path))

(ct:defun-dll GetFullPathName ((lpFileName (:unsigned-char *)) 
							(nBufferLength :long) 
							(lpBuffer (:unsigned-char *))
							(lpFilePart ((:unsigned-char *) *)))
   :return-type :long
   :library-name "kernel32.dll"
   :entry-name "GetFullPathNameA"
   :linkage-type :pascal)

(defun get-full-path-name (name)
	(let ((fname (ct:create-c-string (namestring name)))
		  (buf (ct:malloc 256)))
		(GetFullPathName 
			fname 
			256 
			buf 
			(ct::create-foreign-ptr))
		(values (parse-namestring (ct:c-string-to-lisp-string buf)))))

;;;
;;; Common Lisp TRUENAME function.
;;;
(defun truename (path)
	(pathname (cl::get-full-path-name path)))

;;;
;;;	Common Lisp MAKE-PATHNAME-HS function.
;;; JP Massar's implementation.
;;;
(defparameter *portable-pathname-components* nil) ;;; set to true to enforce portability

(defun implementation-specific-device? (device) (declare (ignore device)) nil)
(defun implementation-specific-directory? (directory)
  (and (listp directory)
       (member (first directory) '(:absolute :relative))
       (every #'stringp (rest directory))))
(defun implementation-specific-name? (name) (declare (ignore name)) nil)
(defun semi-standard-version? (version)
    (member version '(:oldest :previous :installed)))

(defun implementation-specific-punctuation? (ch)
    (or (eql ch #\\) (eql ch #\/)))

(defun make-pathname-hs (&key (host nil supplied-host)
                         (device nil supplied-device)
                         (directory nil supplied-directory)
                         (name nil supplied-name)
                         (type nil supplied-type)
                         (version :unspecific supplied-version)
                         (defaults nil supplied-defaults)
                         (case :local))
  (declare (ignore case))
  (when (not supplied-defaults)
    (setq defaults
          (pathnames::construct-pathname (pathname-host *default-pathname-defaults*)
                                         nil
                                         nil
                                         nil
                                         nil
                                         nil
                                         nil)))
  (flet ((illegal-component (which value)
           (error "Illegal ~A component, ~A, to MAKE-PATHNAME."
                  which
                  value))
         (non-portable (format-string &rest format-args)
           (when *portable-pathname-components*
             (let ((string
                    (concatenate 'string
                                 (apply #'format
                                        nil
                                        format-string
                                        format-args)
                                 "~%~A~%~A")))
               (funcall #'warn
                        string
                        "Execute (setq lisp::*portable-pathname-components* nil)"
                        "to suppress this warning in the future.")))))
    (when supplied-host
      (unless (or (stringp host)
                  (eq :unspecific host)
                  (and (listp host) (every #'stringp host)))
        (illegal-component :host host))
      (unless (null host)
        (non-portable "~A~%~A~A~A"
         "Corman Lisp does not use the HOST slot of a PATHNAME."
         "You provided " host
         " as the :host value for MAKE-PATHNAME -- it will be ignored.")))
    (when supplied-device
      (unless (or (null device)
                  (stringp device)
                  (eq :unspecific device)
                  (implementation-specific-device? device))
        (illegal-component :device device))
      (when (implementation-specific-device? device)
        (non-portable
         "Using ~A as the :device value for MAKE-PATHNAME is non-portable."
         device)))
    (when supplied-directory
      (unless (or (null directory)
                  (stringp directory)
                  (and (listp directory) (every #'stringp directory))
                  (member directory '(:wild :unspecific))
                  (implementation-specific-directory? directory))
        (illegal-component :directory directory))
      (when (implementation-specific-directory? directory)
        (non-portable
         "Using ~S as the :directory value for MAKE-PATHNAME is non-portable."
         directory)))
    (when supplied-name
      (unless (or (null name)
                  (stringp name)
                  (eq name :unspecific)
                  (implementation-specific-name? name))
        (illegal-component :name name))
      (when (implementation-specific-name? name)
        (non-portable
         "Using ~A as the :name value for MAKE-PATHNAME is non-portable."
         name)))
    (when supplied-type
      (unless (or (null type) (stringp type) (eq type :unspecific))
        (illegal-component :type type)))
    (when supplied-version
      (unless (or (and (integerp version) (plusp version))
                  (member version '(:wild newest :unspecific))
                  (null version)
                  (semi-standard-version? version))
        (illegal-component :version version))
      (unless (null version)
        (non-portable "~A~%~A~A~A"
         "Corman Lisp does not use the VERSION slot of a PATHNAME."
         "You provided " version
         " as the :version value for MAKE-PATHNAME -- it will be ignored."))
      (when (semi-standard-version? version)
        (non-portable
         "Using ~A as the value of the VERSION slot for MAKE-PATHNAME ~A"
         "is semi-standard." version)))
    (cond ((stringp directory)
           (setq directory (list :absolute directory)))
          ((every #'stringp directory)
           (setq directory (cons :absolute directory)))
          ((eq :wild directory) (setq directory '(:absolute :wild))))
    (when (listp directory)
      (dolist (component (cdr directory))
        (when (find-if #'implementation-specific-punctuation?
                       component)
          (error "MAKE-PATHNAME The directory component ~S contains punctuation"
                 component))))
    (merge-pathnames (pathnames::construct-pathname host
                                                    device
                                                    directory
                                                    name
                                                    type
                                                    version
                                                    nil)
                     defaults)))

