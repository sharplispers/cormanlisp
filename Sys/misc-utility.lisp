;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		misc-utility.lisp
;;;;	Contents:	Corman Lisp miscellaneous features.
;;;;	History:	2/23/01  RGC  Created.
;;;;                11/13/02 RGC  Added DEFINE-SYMBOL-MACRO implementation.
;;;;                12/19/02 RGC  Included JP Massar's redefinition of 
;;;;                              MULTIPLE-VALUE-BIND.
;;;;                06/05/03 RGC  Fixed #. reader macro to respect *READ-EVAL* state.
;;;;
(in-package :ccl)
(export '(
        ccl::get-command-line-args
        ccl::copy-file
        ccl::move-file
        ccl::ensure-writable-file
        ccl::with-writable-file))

(defun parse-command-line (s)
	(flet ((is-white (c) (or (char= c #\space)(char= c #\tab)(char= c #\newline))))
		(let ((args '())
			  (state :white)
			  c
			  (tok '()))
			(dotimes (i (length s))
				(setf c (char s i))
				(case state
					(:white 
						(unless (is-white c)
							(if (char= c #\") 
								(setf state :in-quoted-token)
								(progn
									(setf state :in-token)
									(push c tok)))))
					(:in-quoted-token
						(cond ((char= c #\")
							   (progn
									(push (coerce (nreverse tok) 'string) args)
									(setf tok '())
									(setf state :white)))
							  (t (push c tok))))
					(:in-token
						(cond ((is-white c)
							   (push (coerce (nreverse tok) 'string) args)
							   (setf tok '())
							   (setf state :white))
							  (t (push c tok))))))
			(unless (null tok)
				(push (coerce (nreverse tok) 'string) args))
			(nreverse args)))) 

(defun ccl::get-command-line-args ()
	"Get the command line as alist of arguments, and remove any double-quotes
	 on the beginning or end of the strings."
	(let ((args (parse-command-line (win:get-command-line))))
		(mapcar 
			(lambda (x) 
				(if (char= (char x 0) #\")
					(setf x (remove #\" x :count 1)))
				(if (char= (char x (- (length x) 1)) #\")
					(setf x (remove #\" x :count 1 :from-end t)))
				x)
			args)))

;;; Some more ANSI stuff
;;;
;;;	Common Lisp VECTOR-POP function.
;;; From Pierpaolo BERNARDI, Karsten Poeck
;;;
(in-package :cl)

(defun vector-pop (vector)
	(unless (array-has-fill-pointer-p vector)
		(error 
			(make-condition 'type-error :datum vector
				:expected-type '(satisfies array-has-fill-pointer-p))))
	(when (zerop (fill-pointer vector))
		(error "Can't pop an empty vector"))
	(aref vector (decf (fill-pointer vector))))

;;; Implement global symbol macros.

;;;
;;; Common Lisp DEFINE-SYMBOL-MACRO macro
;;;
(defvar *global-symbol-macros* (make-hash-table))

(defmacro define-symbol-macro (symbol expansion)
    `(progn
        (setf (gethash ',symbol *global-symbol-macros*) ',expansion)
        ',symbol))

;;; Redefine this here to handle global symbol macros.
;;; Returns either the list containing (symbol expansion), or NIL
(defun get-symbol-macro-expansion (sym)
	(do* ((x *lexical-symbol-macros* (cdr x))
		  (form (car x) (car x)))
		((null x) nil)
		(if (eq form sym)
			(return-from get-symbol-macro-expansion nil)) 
		(if (and (consp form)(eq (car form) sym)) 
			(return-from get-symbol-macro-expansion form)))
    (let ((global-expansion (gethash sym *global-symbol-macros*)))
        (if global-expansion (list sym global-expansion))))

;; Used by FBOUNDP to determine if a function is bound to the stub function
;; (in which case it will return NIL).
(defconstant %undefined-func-code-address 
    (cl::function-compiled-code (%undefined-function '#:DUMMY)))

;;; Redefine FBOUNDP here to consider functions defined with a stub
;;; function to be unbound.
(defun fboundp (function-specifier)
	(if (consp function-specifier)
		(setq function-specifier (get-setf-function (cadr function-specifier))))
	(unless (symbolp function-specifier)
		(error "Not a symbol: ~A" function-specifier))
	(or (let ((func (car (uref function-specifier symbol-function-offset))))
            (and 
                (not (uninitialized-object-p func))
                (not (eq (cl::function-compiled-code func) %undefined-func-code-address))))
		(eq (uref function-specifier symbol-function-type-offset) 'special-operator)))

;;; Fix this macro to respect *READ-EVAL*
(set-dispatch-macro-character #\# #\.
	#'(lambda (stream ch arg)
            (declare (ignore arg ch))
            (unless *read-eval*
                (error 'program-error 
                    :format-control "*READ-EVAL* is disabled" :format-arguments '()))
		(let* ((n (read stream t nil t)))
			(unless *read-suppress* (eval n)))))

;;;
;;; Common Lisp WITH-STANDARD-IO-SYNTAX macro.
;;; (JP Massar, Edi Weitz contributions)
;;;
(defun copy-pprint-dispatch (dispatch) dispatch)   ;; redefined in xp.lisp

(defvar *standard-readtable* (copy-readtable nil))
(defvar *standard-pprint-dispatch* (copy-pprint-dispatch nil))

(defmacro with-standard-io-syntax (&body body)
  `(let ((*package* (find-package :user))
	 (*print-array* t)
	 (*print-base* 10)                                  
	 (*print-case* :upcase)
	 (*print-circle* nil)
	 (*print-escape* t)
	 (*print-gensym* t)
	 (*print-length* nil)
	 (*print-level* nil)
	 (*print-lines* nil)
	 (*print-miser-width* nil)
	 (*print-pprint-dispatch* *standard-pprint-dispatch*)
	 ;; *** *PRINT-PRETTY* is supposed to be T
	 ;; according to the Hyperspec, but causes all sorts of
	 ;; problems with printing in Corman when it is T.
	 (*print-pretty* nil)
	 (*print-radix* nil)
	 (*print-readably* nil)
	 (*print-right-margin* nil)
	 (*read-base* 10)
	 (*read-default-float-format* 'single-float)
	 (*read-eval* t)
	 (*read-suppress* nil)
	 (*readtable* *standard-readtable*)
	 )
     ,@body
     ))

;;;
;;; redefine some kernel functions in Lisp
;;;
(defun cl::bit-or (x y) (logior x y))

;;;
;;; Redefine EQUAL to handle foreign pointer comparisons (we define them as EQUAL)
;;;
(defun equal (x y)
	(or (eql x y)
		(and (consp x) (consp y) (equal (car x) (car y)) (equal (cdr x) (cdr y)))
		(and (stringp x) (stringp y) (string= x y))
		(and (bit-vector-p x) (bit-vector-p y) (bit-vector= x y))
		(and (pathnamep x) (pathnamep y) (pathname= x y))
        (and (or (foreignp x)(foreign-heap-p x))(or (foreignp y)(foreign-heap-p y))(ct:cpointer= x y))))

(in-package :cl)
;;;
;;; Common Lisp MACHINE-INSTANCE function (stub)
;;;
(defun machine-instance ()
  nil)

;;;
;;; Common Lisp MACHINE-VERSION function (stub)
;;;
(defun machine-version ()
  nil)

;;;
;;; Common Lisp MACHINE-TYPE function (stub)
;;;
(defun machine-type ()
  nil)

;;;
;;; Common Lisp USER-HOMEDIR-PATHNAME function
;; Thanks to Andy Sloane <andy@guildsoftware.com>
;;;
(in-package :win)
#! (:export t :library "KERNEL32" :ignore "APIENTRY" :pascal "WINAPI")
DWORD WINAPI GetEnvironmentVariableA(LPCSTR,LPSTR,DWORD);
!#
(in-package :cl)
(defun user-homedir-pathname (&optional host)
  (flet ((getenv (name)
           (let ((buffer (ct:malloc 1))
                 (cname (ct:lisp-string-to-c-string name)))
             (let* ((needed-size (win:getenvironmentvariable
                                  cname buffer 0))
                    (buffer1 (ct:malloc (1+ needed-size))))
               (prog1 (if (zerop (win:getenvironmentvariable
                                  cname buffer1 needed-size)) 
                          nil
                          (ct:c-string-to-lisp-string buffer1))
                 (ct:free buffer)
                 (ct:free buffer1))))))
    (cond ((or (stringp host)
               (and (consp host)
                    (every #'stringp host)))
           nil)
          ((or (eq host :unspecific)
               (null host))
           (let ((homedrive (getenv "HOMEDRIVE"))
                 (homepath (getenv "HOMEPATH")))
             (parse-namestring
              (if (and (stringp homedrive)
                       (stringp homepath)
                       (= (length homedrive) 2)
                       (> (length homepath) 0))
                  ;; Added a tail "\\" to make sure the namestring is parsed
                  ;; as a pure directory. -- Chun Tian (binghe), May 2015
                  (concatenate 'string homedrive homepath "\\")
                  "C:\\"))))
          (t
           (error "HOST must be a string, list of strings, NIL or :UNSPECIFIC")))))

(defun wild-pathname-p (pathname &optional field-key)
  (let ((obj (ecase field-key
               ((nil) (namestring pathname))
               ((:host) (pathname-host pathname))
               ((:device) (pathname-device pathname))
               ((:directory) (pathname-directory pathname))
               ((:name) (pathname-name pathname))
               ((:type) (pathname-type pathname))
               ((:version) (pathname-version pathname)))))
    (flet ((wild-p (sym) (or (eq sym :wild)
                             (eq sym :wild-inferiors))))
      (etypecase obj
        (symbol (wild-p obj))
        (list (position-if (lambda (o) (typecase o
                                         (symbol (wild-p o))
                                         (string (string= obj "*"))))
                           obj))
        (string (position #\* obj :test #'char=))))))

;;;
;;; Define some File functions
;;;
(in-package :win)
#! (:export t :library "KERNEL32" :pascal "WINAPI")
WINAPI BOOL CopyFileExA(LPCTSTR lpExistingFileName, LPCTSTR lpNewFileName, LPVOID lpProgressRoutine, LPVOID lpData, LPBOOL pbCancel, DWORD dwCopyFlags);
!#
(in-package :ccl)
(defun copy-file (source dest)
    (win:CopyFileEx (namestring source) (namestring dest) ct:null ct:null ct:null 0))

(defun move-file (source dest)
    (win::MoveFile (ct:lisp-string-to-c-string (namestring source))
        (ct:lisp-string-to-c-string (namestring dest))))

;; Ensures that the file is writable, and returns the previous file attributes (int).
;; This can be used to restore the previous attributes i.e. read-only, after the write is done.
(defun ensure-writable-file (path)
    (let* ((name (namestring path))
           (attribs (ccl::get-file-attributes name)))
        (ccl::set-file-attributes name (logand attribs (lognot win:FILE_ATTRIBUTE_READONLY)))
        attribs))

(defmacro with-writable-file ((path) &body forms)
    (let ((attrs-sym (gensym)))
        `(let ((,attrs-sym (ensure-writable-file ,path)))
            (unwind-protect
                (progn ,@forms)
                (ccl::set-file-attributes ,(namestring path) ,attrs-sym))))) 
                 
(in-package :cl)
(defun signal-reader-error (format &rest args)
    (error 'reader-error 
        :format-control format
		:format-arguments args))

(set-dispatch-macro-character #\# #\:
	#'(lambda (stream ch arg)
        (declare (ignore arg ch))
        (do* ((c (%read-char stream)(%read-char stream))
             (char-type (readtable-char-type *readtable* c)(readtable-char-type *readtable* c))
             (chars '()))
            ((not (eq char-type 'constituent-char-type))
             (progn (unread-char c stream) (make-symbol (concatenate 'string (nreverse chars)))))
            (if (eq c #\:)
                (cl::signal-reader-error "Invalid character #\: found in uninterned symbol"))
            (push (char-upcase c) chars))))