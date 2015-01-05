;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		defpackage.lisp
;;;;	Contents:	defpackage implementation for Corman Lisp.
;;;;	History:	5/29/97  RGC  Imported from 2.0 system.
;;;;				7/15/00  RGC  Fixed a bug with the :IMPORT-FROM option.
;;;;				1/31/01  RGC  Added Chris Double's bug fix for :IMPORT-FROM.
;;;;				2/19/01  RGC  Modified APROPOS to more accurately report the package
;;;;							  that symbols are found in.
;;;;				6/06/01  RGC  Integrated latest changes from Frank Adrian
;;;;							  to fix bugs with package designators.
;;;;				9/28/01  RGC  Integrated code from JP Massar to implement
;;;;							  :SHADOWING-IMPORT-FROM option.
;;;;                12/19/02 RGC  Integrated JP Massar's defect fix for DEFPACKAGE.
;;;;                              :IMPORT-FROM and :SHADOWING-IMPORT-FROM now handle
;;;;                              package designators and symbol designators.
;;;;                2/22/03  RGC  Integrated Karsten Poeck's fix to APROPOS-LIST, DO-SYMBOLS.
;;;;                              Added Karsten's WITH-PACKAGE-ITERATOR macro implementation.
;;;;                9/22/03  RGC  WITH-PACKAGE-ITERATOR now calls FIND-PACKAGE on each package in
;;;;                              the list.
;;;;                4/19/06  RGC  Fixed a problem with DEFPACKAGE and :DOCUMENTATION option--when the name
;;;;                              is a symbol.
;;;;

(in-package :common-lisp)

;;; Author:  JP Massar.
(defun verify-symbol-or-string (x &optional location-symbol)
	(unless (or (symbolp x) (stringp x))
		(if location-symbol
			(error "Within ~A, the object ~A should be a symbol or string, but it is neither."
				(string location-symbol) x)
			(error "The object ~A should be a symbol or string, but it is neither." x)))
	(string x))

(defun shadowing-import-from-function (into from symbol-names)
	(let ((into-package (find-package into))
		  (from-package (find-package from)))
		(unless into-package
			(error "Internal error.  The package named ~A does not exist, but ~
				should already have been defined by DEFPACKAGE."
				(string into)))
		(unless from-package
			(error "The package named ~A does not exist, but you are trying to import ~
				symbols from that package using :shadowing-import-from (within DEFPACKAGE)."
				(string from)))
		(dolist (symbol symbol-names)
			(let* ((name (verify-symbol-or-string symbol :shadowing-import-from))
				   (symbol (find-symbol name from-package)))
				(unless symbol
					(error "The symbol ~A does not exist in the ~A package.~%~ 
						But you told :shadowing-import-from (within DEFPACKAGE) to import it."
						name from))
				(shadowing-import (list symbol) into-package)))))

(defun shadowing-import-from (from-package-name-and-symbol-names-to-import into-package-name)
	(let ((from-package-name (first from-package-name-and-symbol-names-to-import))
		  (symbol-names (rest from-package-name-and-symbol-names-to-import)))
		(shadowing-import-from-function
			(verify-symbol-or-string into-package-name 'defpackage)
			(verify-symbol-or-string from-package-name 'defpackage)
			symbol-names)))

(defun canonicalize-package-designator (pkg-des &optional (allow-nil-for-*package* t))
	(when (null pkg-des)
		(if allow-nil-for-*package* 
			(setq pkg-des *package*)
			(error "Nil not allowed for package designator in this context.")))
	(unless (packagep pkg-des)
        (let* ((pkg-name (string pkg-des))
               (pkg (find-package pkg-name)))
		  (setq pkg-des (if pkg pkg pkg-name))))
	pkg-des)

(defun canonical-package-name (pkg-des)
    (if (packagep pkg-des)
        (package-name (canonicalize-package-designator pkg-des))
        pkg-des))

(defun export-create (sym-name package)
	(let ((string (string sym-name)))
		(multiple-value-bind (sym state)
			(find-symbol string package)
			(unless state
				(setq sym (intern string package) package))
			(export sym package))))

(defun build-import-forms (into-pkg-name specification-list shadowing)
    (let (forms)
      (do* ((still-to-process specification-list (cdr still-to-process))
            (spec (car still-to-process) (car still-to-process)))
            ((null still-to-process) nil)
        (let* ((package (canonical-package-name (car spec)))
               (symbol-names (mapcan 
                               #'(lambda (name)
                                   (let ((sym (find-symbol (string name) package)))
                                     (if sym
                                       (list sym)
                                       (progn (warn
                                                "Symbol ~A was not found in the package named \"~A\" and will not be imported."
                                                name package) nil))))
                               (cdr spec))))
			(push `(,(if shadowing 'shadowing-import 'import) ',symbol-names ,into-pkg-name) forms)))
      `(progn ,@forms)))

;;;
;;;		Common Lisp DEFPACKAGE macro
;;;
(defmacro defpackage (name &rest options)
	(setq name (canonical-package-name name))
	(let ((forms nil)
		  (size nil)
		  (nicknames nil)
		  (shadow nil)
		  (shadowing-import-from nil)
          (use nil)
          (use-supplied-p nil)
   		  (import-from nil)
		  (intern nil)
		  (export nil)
		  (documentation nil)
		  (package (find-package name)))
		(declare (ignore size))

		(do* ((p options (cdr p))
			  (option (caar p) (caar p))
			  (value (cdar p) (cdar p)))
			((null p))
			(case option
				(:size (when size (error "More than one :size option specified.")) (setq size (car value)))
				(:nicknames (setq nicknames (append nicknames (mapcar #'string value))))
				(:shadow (setq shadow (append shadow (mapcar #'string value))))
				(:shadowing-import-from (push value shadowing-import-from))
				(:use 
                    (setq use-supplied-p t)
                    (setq use 
                        (remove-duplicates 
                            (append use 
                                (mapcar #'(lambda (pkg) (canonicalize-package-designator pkg nil)) value)))))
				(:import-from (push value import-from))
				(:intern (setq intern (append intern (mapcar #'string value))))
				(:export (setq export (append export (mapcar #'string value))))
				(:documentation
                    (when documentation
                        (error "More than one :documentation option specified."))
                    (setq documentation (car value)))
                (otherwise (error "Bad defpackage option: ~A." (car p)))))
        (unless use-supplied-p
                  (setq use default-packages))
		(unless package
			(push `(unless (find-package ',name)
                    (make-package ',name 
                        :nicknames ',(remove-duplicates nicknames :test #'string-equal) 
                        :use nil 
                        ,@(when size `(:size ,size)))) forms))
        (setq use (mapcar (lambda (package) (package-name package)) use))   ;; list package names, not packages 		
		(when shadow
		  (push `(shadow ',shadow ',name) forms))
		(when shadowing-import-from
          (push `,(build-import-forms name shadowing-import-from t) forms))
		(when use
		  (push `(use-package ',use ',name) forms))
		(when import-from
          (push `,(build-import-forms name import-from nil) forms))
        (when intern
	      (dolist (sym intern)
			(push `(intern ,sym ',name) forms)))
		(when export
		  (dolist (sym export)
			(push `(export-create ,sym ',name) forms)))
		(when documentation
		  (push `(setf (documentation ',(intern (string name)) 'package) ,documentation) forms))
		(push `(find-package ',name) forms)

		`(eval-when (:load-toplevel :compile-toplevel :execute)
			,@(nreverse forms))))	

;; support function for DO-SYMBOLS, etc.
(defun iterate-over-package (package func &optional external-only)
	(setq package (canonicalize-package-designator package))
	(if (null package)
		(setq package *package*))
	(check-type package package)
	(let* ((table (package-table package))
		   (size (package-capacity package)))
		(declare (ignore table))
		(dotimes (i size)
			(if (package-entry-occupied package i)
				(if (or (null external-only) 
						(eq (package-entry-state package i) 'external))
					(funcall func (package-entry-symbol package i)))))))
					
;;;
;;;		Common Lisp DO-SYMBOLS macro
;;;
(defmacro do-symbols ((var &optional package result-form) &rest forms)
    (let ((current-package (gensym))
          (lambda (gensym)))
        `(let ((,lambda #'(lambda(,var) ,@forms)))
            (iterate-over-package ,package ,lambda)
        ;;; KAP: Also need to iterate over the inherited symbols
        ;;; being the external symbols in the package use list
            (dolist (,current-package (package-use-list ,(or package *package*)))
                (iterate-over-package ,current-package ,lambda t))
            ,result-form)))

;;;
;;;		Common Lisp DO-EXTERNAL-SYMBOLS macro
;;;
(defmacro do-external-symbols ((var &optional package result-form) &rest forms)
	(let ()
		`(progn
			(iterate-over-package ,package #'(lambda (,var) ,@forms) t)
			,result-form)))

;;;
;;;		Common Lisp DO-ALL-SYMBOLS macro
;;;
(defmacro do-all-symbols ((var &optional result-form) &rest forms)
	(let ((sym (gensym)))
		`(progn
			(dolist (,sym (list-all-packages))
				(iterate-over-package ,sym #'(lambda (,var) ,@forms)))
			,result-form)))

;;;
;;;		Common Lisp FIND-ALL-SYMBOLS macro
;;;
(defun find-all-symbols (name &aux (list nil))
	(do-all-symbols (x) 
		(if (string= (symbol-name x) name) (push x list)))
	list)

(defun output-apropos-variable (var package stream)
	(let ((status (nth-value 1 (find-symbol (symbol-name var) package)))
		  (props nil)
		  (origin nil))
		(if (eq (symbol-package var) package)
			(setf origin (symbol-name status))
			(if (symbol-package var)
				(setf origin (format nil "~A, imported from package ~A"
						(symbol-name status)
						(package-name (symbol-package var))))
				(setf origin (symbol-name status))))
		(format stream "    ~A (~A)" (symbol-name var) origin)
		(cond ((special-operator-p var)(push 'special-operator props))
			  ((macro-function var)(push 'macro props))
			  ((fboundp var)(push 'function props)))
		(if (boundp var)
			(push 'variable props))
		(if props
			(format stream "  ~A" (car props)))
		(dolist (x (cdr props))
			(format stream ", ~A" x))
		(format stream "~%")))
					
;;;
;;;		Common Lisp APROPOS function
;;;
(defun apropos (string &optional package)
	(let ((stream *standard-output*))
		(setq string (string-upcase (canonicalize-string-designator string)))
		(check-type string string)
		(let ((packages-to-search
	 	 	  	(if package 
					(list (find-package package)) 
					(list-all-packages))))
			(dolist (package packages-to-search)
				(let ((first-sym t))
					(do-symbols (x package)
						(when (search string (symbol-name x))
							(when first-sym
								(format stream "~%Package: ~A~%" (package-name package))
								(setf first-sym nil))
							(output-apropos-variable x  package stream)
							(force-output stream))))))))

;;;
;;;	Common Lisp APROPOS-LIST function
;;;
(defun apropos-list (string &optional package)
    (setq string (canonicalize-string-designator string))
    (check-type string string)
    (let ((*apropos-list* nil))
        (declare (special *apropos-list*))
        (if
            package
            (do-symbols
                (symbol (find-package package))
                (when (search string (symbol-name symbol)) (push symbol *apropos-list*)))
            (do-all-symbols
                (symbol)
                (when (search string (symbol-name symbol)) (push symbol *apropos-list*))))
        *apropos-list*))
	 
;;;; KAP 2003-01-04
;;;; with-package-iterator for Corman

;;;; ToDo
;;;; Eventually check that every symbol is only listed once
;;;; Verify that the Format Arguments for the condition are correct

(defun mask-access(symbol)
    (case symbol
        (cl::external :external)
        (cl::internal :internal)
        (t (error "Shoudn't happen"))))

(defun get-next-symbol (package last-index internal-p external-p)
    (flet ((state-correct-p (a-state)
                (or
                    (and (eql 'cl::internal a-state) internal-p)
                    (and (eql 'cl::external a-state) external-p))))
        (let ((size (or (package-capacity package) 0))
              (current-index (1+ last-index)))
            (when (>= current-index size)
                (return-from get-next-symbol nil))
            (loop
                (cond ((and (package-entry-occupied package current-index)
                            (state-correct-p (package-entry-state package current-index)))
                        (return
                            (values t (package-entry-symbol package current-index) 
                                (mask-access (package-entry-state package current-index))
                                package current-index)))
                    (t (incf current-index)))
                (unless (< current-index size)
                    (return nil))))))
		     
(defun generate-package-iterator (package-list &rest symbol-states)
    ;;;; First Iterate over the internal/external when needed
    ;;;; If fininished, check if inherited is specified and iterate over the externals of the use packages
    (let ((state :start)
          (current-package nil)
	      (current-use-package nil)
	      (all-packages 
                (if (listp package-list)
                    (mapcar 'find-package package-list)
                    (find-package package-list)))
	      (use-packages nil)
	      (index nil)
	      (i-p (find :internal symbol-states))
	      (e-p (find :external symbol-states))
	      (ih-p (find :inherited symbol-states)))
        (unless (and (or i-p e-p ih-p)(subsetp symbol-states (list :internal :external :inherited)))
            (signal-program-error "Incorrect States passed ~{~s ~}." symbol-states))
        #'(lambda()
            (loop
                (case state
                    (:start
                        (when (null all-packages) (return nil))
                        (when (and all-packages (not (listp all-packages)))
                            (setq all-packages (list all-packages)))
                        (setq current-package (pop all-packages)
                            index -1 
                            state :check-internal-external))
                    (:check-internal-external
                        (if (or i-p e-p)
                            (multiple-value-bind
                                (found symbol access apackage new-index)
                                (get-next-symbol current-package index i-p e-p)
                                (declare (ignore apackage))
                                (cond 
                                    (found (setq index new-index) 
                                        (return (values found symbol access apackage)))
                                    (t (setq state :check-inherited))))
                            (setq state :check-inherited)))
                    (:check-inherited
                        (cond 
                            (ih-p (setq use-packages (package-use-list current-package)
                                    current-use-package (pop use-packages)
                                    index -1
                                    state :do-inherited))
                            (t (setq state :start))))
                    (:do-inherited 
                        (if (null current-use-package)
                            (setq state :start)
                            (multiple-value-bind
                                (found symbol access apackage new-index)
                                (get-next-symbol current-use-package index nil t)
                                (declare (ignore apackage access))
                                (cond 
                                    (found (setq index new-index) 
                                        (return (values found symbol :inherited current-package)))
                                    (t (setq current-use-package (pop use-packages)
                                            index -1)))))))))))

;;;
;;; Common Lisp WITH-PACKAGE-ITERATOR macro.
;;; Implementation by Karsten Poeck.
;;;
(defmacro with-package-iterator ((lambda package-list &rest symbol-states) &body body)
    (let* ((function (gensym))
           (ps (gensym)))
        `(let* ((,ps ,package-list)
                (,function (generate-package-iterator ,ps ,@symbol-states)))
            (macrolet ((,lambda () '(funcall ,function))) 
                ,@body))))



