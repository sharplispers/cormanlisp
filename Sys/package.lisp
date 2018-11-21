;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		package.lisp
;;;;	Contents:	Corman Lisp 3.0 package functions.
;;;;	History:	10/17/96  RGC  Created.
;;;;				12/10/99  RGC  Modified PACKAGE-FIND-SYMBOL to not follow
;;;;							   inheritance of packages more than one level.
;;;;							   This should conform to ANSI.
;;;;				02/18/01  RGC  Added WIN32 to *features*.
;;;;				06/06/01  RGC  Integrated latest changes from Frank Adrian
;;;;							   to fix bugs with package designators.
;;;;				05/20/02  RGC  Fixed a couple non-standard behaviors in INTERN.
;;;;                12/19/02  RGC  Removed warning from IMPORT.
;;;;                2/22/03   RGC  Integrated Karsten Poeck's mod to PACKAGE-FIND-SYMBOL.
;;;;                9/19/03   RGC  Integrated Frank Adrian's enhancement to MAKE-PACKAGE (added size option)
;;;;
(in-package :common-lisp)

(defun set-package-capacity (capacity p) 
	(setf (uref p package-capacity-offset) capacity)) 

;; all package sizes must be prime integers
(defvar *package-sizes* '(613 701 809 907 1009 1201 1301 1511 2003 3001	4001 5003 
					6007 7001 8009 9001 10007
					11003 12007 13001 14009 15013 16001 17011 18013 19001 20011 25013
					30011 35023 40009 50021 60013 70001 80021 90001 100003 150001 500009 1000003 1499977))
(defun calc-package-size (requested-size)
	(dolist (x *package-sizes*)
		(if (>= x requested-size)
			(return-from calc-package-size x)))
	nil)

(defvar *deleted-symbol* (make-symbol "*deleted-symbol*"))	;placeholder symbol        
    
;; double the package capacity
(defun package-grow (package)
;	(format t "Growing package: ~A~%" package)
;	(format t "Package count = ~A~%" (package-count package))
;	(format t "Package capacity = ~A~%" (package-capacity package))
	(let* ((current-table (package-table package))
	       (current-capacity (package-capacity package))
		   (new-capacity (calc-package-size (* 2 current-capacity))))
		(unless new-capacity (error "Could not grow the package: ~A" package))
		(set-package-table (make-array (* new-capacity 3) :initial-element 0) package)
		(set-package-capacity new-capacity package)
		(set-package-count 0 package)

		(dotimes (i current-capacity)
			(let ((index (* i 3)))
				(if (stringp (elt current-table index))
					(package-add-symbol package 
						(elt current-table index)				;; string
						(elt current-table (+ 1 index))			;; symbol
						(if (= (elt current-table (+ 2 index)) 0)
							'internal 'external))))))	;; state
;	(format t "Done growing: ~A~%" package)
;	(format t "Package count = ~A~%" (package-count package))
;	(format t "Package capacity = ~A~%" (package-capacity package))
	package)

#|	;; defined in uvector.lisp
(defconstant package-name-offset				1)
(defconstant package-nicknames-offset			2)
(defconstant package-use-list-offset			3)
(defconstant package-used-by-list-offset		4)
(defconstant package-shadowing-symbols-offset	5)
(defconstant package-capacity-offset			6)
(defconstant package-count-offset				7)
(defconstant package-table-offset				8)
(defconstant package-sync-offset				9)
|#
(defun package-add-shadowing-symbol-to-list (package symbol)
	(setf (uref package package-shadowing-symbols-offset)
		(adjoin symbol (uref package package-shadowing-symbols-offset))))	

(defun remove-item (item list)
	(let ((ret '()))
		(dolist (x list (nreverse ret))
			(unless (eql x item)
				(push x ret)))))

(defun package-add-used-package-to-list (package used-package)
	(setf (uref package package-use-list-offset)
		(adjoin used-package (uref package package-use-list-offset))))	

(defun package-remove-used-package-from-list (package used-package)
	(setf (uref package package-use-list-offset)
		(remove-item used-package (uref package package-use-list-offset))))	

(defun package-add-used-by-package-to-list (package used-by-package)
	(setf (uref package package-used-by-list-offset)
		(adjoin used-by-package (uref package package-used-by-list-offset))))	

(defun package-remove-used-by-package-from-list (package used-by-package)
	(setf (uref package package-used-by-list-offset)
		(remove-item used-by-package (uref package package-used-by-list-offset))))	

;;;
;;;	Corman Lisp ADD-USED-PACKAGE function.
;;;
(defun add-used-package (package package-to-use)
	(unless (packagep package) 
		(setq package (find-package package)))
	(unless (packagep package-to-use) 
		(setq package-to-use (find-package package-to-use)))
	(unless (packagep package)
		(error "Not a package: ~A" package))
	(unless (packagep package-to-use)
		(error "Not a package: ~A" package-to-use))

	;; check for name conflicts
	(let ((size (package-capacity package-to-use)))
		(dotimes (i size)
			(if (package-entry-occupied package-to-use i)
				(multiple-value-bind (sym state)
					(package-find-symbol package (package-entry-string package-to-use i))
					(declare (ignore sym))
					(if (eq state :inherited)
						(package-add-shadowing-symbol-to-list 
							package 
							(package-entry-symbol package-to-use i)))))))
	(package-add-used-package-to-list package package-to-use)
	(package-add-used-by-package-to-list package-to-use package))   

;;;
;;;	Corman Lisp REMOVE-USED-PACKAGE function.
;;;
(defun remove-used-package (package package-to-unuse)
	(unless (packagep package-to-unuse) 
		(setq package-to-unuse (find-package package-to-unuse)))
	(unless (packagep package-to-unuse)
		(error "Not a package: ~A" package-to-unuse))
	(package-remove-used-package-from-list package package-to-unuse)
	(package-remove-used-by-package-from-list package-to-unuse package))   

;;;
;;;	Common Lisp USE-PACKAGE function.
;;;
(defun use-package (packages-to-use &optional (package *package*))
	(let ((p (find-package package)))
		(check-type p package)
		(unless (listp packages-to-use)
			(setq packages-to-use (list packages-to-use)))
		(dolist (package-to-use packages-to-use)
			(add-used-package p package-to-use)))
	t)

;;;
;;;	Common Lisp UNUSE-PACKAGE function.
;;;
(defun unuse-package (packages-to-unuse &optional (package *package*))
	(let ((p (find-package package)))
		(check-type p package)
		(unless (listp packages-to-unuse)
			(setq packages-to-unuse (list packages-to-unuse)))
		(dolist (package-to-unuse packages-to-unuse)
			(remove-used-package p package-to-unuse)))
	t)

;;;;
;;;;	Common Lisp MAKE-PACKAGE function.
;;;;
(defconstant default-packages (list (find-package :common-lisp) (find-package :cormanlisp)))

(defun make-package (package-name &key nicknames (use default-packages) (size 613))
	(setq package-name (canonicalize-string-designator package-name))
	(let* ((package (alloc-uvector 8 uvector-package-tag))
           (psize (calc-package-size size))
		   (table (make-array (* psize 3))))
		(setf (uref package package-name-offset) package-name)
		(setf (uref package package-nicknames-offset) nicknames)
		(setf (uref package package-used-by-list-offset) nil)
		(setf (uref package package-shadowing-symbols-offset) nil)
		(setf (uref package package-capacity-offset) psize)
		(setf (uref package package-count-offset) 0)
		(setf (uref package package-table-offset) table)
        (setf (uref package package-sync-offset) (allocate-critical-section))
		(setq *package-list* (cons package *package-list*))
		(setf (uref package package-use-list-offset) nil)
		(use-package use package)
		package))
		
;;;;
;;;;	Common Lisp PACKAGE-NAME function.
;;;;
(defun package-name (package)
	(let ((p (find-package package)))
		(check-type p package)
		(uref p package-name-offset)))
		 
;;;;
;;;;	Common Lisp PACKAGE-NICKNAMES function.
;;;;
(defun package-nicknames (package) 
	(let ((p (find-package package)))
		(check-type p package)
		(uref p package-nicknames-offset)))

;;;;
;;;;	Common Lisp RENAME-PACKAGE function.
;;;;
(defun rename-package (package new-name &optional new-nicknames) 
	(let ((p (find-package package)))
		(setq new-name (canonicalize-string-designator new-name))
		(setf (uref p package-name-offset) new-name)
		(setf (uref p package-nicknames-offset)
			(mapcar #'canonicalize-string-designator new-nicknames))))

;;;;
;;;;	Common Lisp PACKAGE-USE-LIST function.
;;;;
(defun package-use-list (package) 
	(let ((p (find-package package)))
		(check-type p package)
		(uref p package-use-list-offset)))

;;;;
;;;;	Common Lisp PACKAGE-USED-BY-LIST function.
;;;;
(defun package-used-by-list (package) 
	(let ((p (find-package package)))
		(check-type p package)
		(uref p package-used-by-list-offset)))

;;;;
;;;;	Common Lisp PACKAGE-SHADOWING-SYMBOLS function.
;;;;
(defun package-shadowing-symbols (package) 
	(let ((p (find-package package)))
		(check-type p package)
		(uref p package-shadowing-symbols-offset)))

;;;;
;;;;	Common Lisp DELETE-PACKAGE function.
;;;;
(defun cerror (continue-format-control datum &rest arguments)
    (declare (ignore continue-format-control datum arguments)))  ;; defined later

(defun delete-package (package)
    ;; attempt to find the named package
    (let ((p (find-package package)))
        (unless (packagep p)
            (cerror "Skip call to DELETE-PACKAGE and continue"
                "DELETE-PACKAGE could not find a package named ~A" package)
            (return-from delete-package nil))
        
        ;; see if the package is used by any other packages
        (let ((used-by (uref p package-used-by-list-offset)))
            (when used-by
                (cerror "Remove all USE-PACKAGE references to package ~A and continue"
                    "DELETE-PACKAGE was called on package ~A, which is used by package(s):~%~{~A~%~}"
                    p
                    used-by)
                (dolist (pack used-by)
                    (unuse-package p pack)))
            
            ;; if package has already been deleted, just return nil
            (unless (member p *package-list*)
                (return-from delete-package nil))
            
            ;; remove from *package-list*
            (setq *package-list* (remove p *package-list*))
            t)))
                
;;;
;;;	Common Lisp IMPORT function.
;;;
(defun import (symbols &optional (package *package*))
	(let ((p (find-package package)))
		(check-type p package)
		(unless (listp symbols)
			(setq symbols (list symbols)))

        (with-synchronization (package-sync p)
    		;; import each symbol in the list
    		(dolist (symbol symbols)
    			;;check for name conflict
    			(multiple-value-bind (sym state)
    				(package-find-symbol p (symbol-name symbol))
    				(declare (ignore sym))
    				;; if there is a conflict with an inherited symbol,
    				;; add the imported symbol as a shadowing symbol
    				(cond
    					((eq state :inherited)
    					 (package-add-shadowing-symbol-to-list p symbol)
    					 (package-add-symbol p (symbol-name symbol) symbol 'internal)
    					 (unless (symbol-package symbol)
    						(set-symbol-package p symbol)))
    					;; if there is a conflict with an internal symbol
    					;; then do not import the symbol
    					((or (eq state 'internal)(eq state 'external)) nil)
    					 ;;(warn "A symbol named ~A is already present in the package ~A, ~A"
    					 ;;	(symbol-name symbol) p "and will not be imported")
    					(t 
    					 (package-add-symbol p (symbol-name symbol) symbol 'internal)
    					 (unless (symbol-package symbol)
    						(set-symbol-package p symbol))))))))
	t)

;;;;
;;;;	Common Lisp INTERN function (redefined here)
;;;;
(defun intern (str &optional (package *package*))
	(unless (packagep package)
		(setq package (find-package package)))
	(unless (packagep package)
		(error "Invalid package: ~A" package))
    (with-synchronization (package-sync package)
        (let* ((ret (multiple-value-list (package-find-symbol package str)))
		       (state (cadr ret))
		       (keyword nil)
		        sym)
    		(if (eq state 'common-lisp::internal) 
    			(return-from intern (values (car ret) ':internal))
    			(if (eq state 'common-lisp::external)
    				(return-from intern (values (car ret) ':external))
    				(if (eq state ':inherited)
    					(return-from intern (values (car ret) ':inherited)))))
    		(if (string= (package-name package) "KEYWORD")
    			(progn (setq state 'EXTERNAL) (setq keyword t))
    			(setq state 'INTERNAL))
    		(setq sym (package-add-symbol package str (make-symbol str) state))
    		(if keyword 
    			(progn
    				(set-symbol-value sym sym)	;; keywords evaluate to themselves
    				(symbol-set-constant-flag sym)
    				(symbol-set-special-flag sym)))
    		(values sym nil))))

;;;
;;;	Common Lisp UNINTERN function.
;;;
(defun unintern (symbol &optional (package *package*))
	(let ((p (find-package package)))
		(check-type p package)
		(check-type symbol symbol)
        (with-synchronization (package-sync p)
       		(let ((index (package-find-symbol-index p (symbol-name symbol))))
    			(when (>= index 0)
    				(let* ((sym (package-entry-symbol p index)))
    					(when (eq sym symbol)	;if the symbol exists in the package
    						;; if this is the symbol's home package,
    						;; cause it to not have a home package by setting
    						;; its home package to NIL
    						(if (eq (symbol-package sym) p)
    							(set-symbol-package nil sym))
    
    						;; clear this hash table position by storing a
    						;; dummy symbol in it (to avoid having to rehash everything)
    						(set-package-entry-symbol *deleted-symbol* p index)
    						(set-package-entry-string (symbol-name *deleted-symbol*) p index)
    						(set-package-entry-state 'internal p index)
    						
    						;; if the symbol was a shadowing symbol, remove it
    						(if (member sym (package-shadowing-symbols p))
    							(setf (uref package package-shadowing-symbols-offset)
    								(remove sym (package-shadowing-symbols p))))
    						(return-from unintern t)))))))
	nil)

;;;
;;;	Common Lisp SHADOWING-IMPORT function.
;;;
(defun shadowing-import (symbols &optional (package *package*))
	(let ((p (find-package package)))
		(check-type p package)
		(unless (listp symbols)
			(setq symbols (list symbols)))

        (with-synchronization (package-sync p)
    		;; import each symbol in the list
    		(dolist (symbol symbols)
    			;;check for name conflict
    			(multiple-value-bind (sym state)
    				(package-find-symbol p (symbol-name symbol))
    				;; if there is a conflict with an inherited symbol,
    				;; add the imported symbol as a shadowing symbol
    				(cond
    					((eq state :inherited)
    					 (package-add-shadowing-symbol-to-list p symbol)
    					 (package-add-symbol p (symbol-name symbol) symbol 'internal)
    					 (unless (symbol-package symbol)
    						(set-symbol-package p symbol)))
    					;; if there is a conflict with an internal symbol
    					;; then unintern the existing symbol before importing
    					;; the new one
    					((or (eq state 'internal)(eq state 'external))
    					 (unintern sym p)
    					 (package-add-symbol p (symbol-name symbol) symbol 'internal))
    					(t 
    					 (package-add-symbol p (symbol-name symbol) symbol 'internal)
    					 (unless (symbol-package symbol)
    						(set-symbol-package p symbol))))))))
	t)

;;;
;;;	If a conflict is found, return the package, state and symbol.
;;; If no conflict, return nil, nil and nil.
;;;
(defun check-for-exporting-conflicts (symbol inheriting-packages)
	(dolist (package inheriting-packages)
		(multiple-value-bind (sym state)
			(package-find-symbol package (symbol-name symbol))
			(unless (or (null state) (eq symbol sym))
				(return-from check-for-exporting-conflicts (values package state sym)))))
	(values nil nil nil))

;;;
;;;	Common Lisp EXPORT function.
;;;
(defun export (symbols &optional (package *package*))
	(let ((p (find-package package)))
		(check-type p package)
		(unless (listp symbols)
			(setq symbols (list symbols)))

		;; export each symbol in the list
        (with-synchronization (package-sync p)
            (dolist (symbol symbols)
    			;;check for name conflicts
    			(multiple-value-bind (sym state)
    				(package-find-symbol p (symbol-name symbol))
    				;; if there is a conflict with an inherited symbol,
    				;; add the imported symbol as a shadowing symbol
    				(cond
    					((eq state 'external))	;; nothing to do
    					((eq state 'internal)	;; if an internal symbol
    					 (multiple-value-bind (found-in-package found-in-state found-symbol)
    						(check-for-exporting-conflicts symbol (package-used-by-list p))
    						(cond
    							((null found-in-state))		;no problem
    							((eq found-in-state :inherited)
    								 (error "Cannot export symbol ~A~A~A~A~A~A~A~A"
    									(symbol-name symbol)
    									" from package " p
    									", because package " found-in-package
    									" already already inherits a symbol named " (symbol-name symbol)
    									" from another package"))
    							;; else make the existing symbol shadow the new one we are exporting
    							(t (package-add-shadowing-symbol-to-list found-in-package found-symbol))))
    					 (let ((index (package-find-symbol-index p (symbol-name sym))))
    						(set-package-entry-state 'external p index)))
    
    					;; if the symbol is inherited, then import it and call this function
    					;; recursively
    					((eq state :inherited)
    					 (import symbol p)
    					 (export symbol p))
    					(t 
    					 (error "Cannot export the symbol ~A because it is not accessible in the package ~A"
    							symbol p)))))))
	t)		

;;;
;;;	Common Lisp SHADOW function.
;;;
(defun shadow (symbol-names &optional (package *package*))
	(let ((p (find-package package)))
		(check-type p package)
		(unless (listp symbol-names)
			(setq symbol-names (list symbol-names)))
        (with-synchronization (package-sync p)
            (dolist (symbol symbol-names)
    			(multiple-value-bind (sym state)
    				(package-find-symbol p (canonicalize-string-designator symbol))
    				(unless (or (eq state 'internal)(eq state 'external))
    					(package-add-symbol 
    						p 
    						(canonicalize-string-designator symbol) 
    						(setf sym (make-symbol (canonicalize-string-designator symbol))) 
    						'internal))
    				(package-add-shadowing-symbol-to-list p sym)))))
	t)

;;;
;;;	Use this function to call on inherited packages so we don't go
;;; more than one level (inheritance of packages via USE_PACKAGE
;;; is not transitive.
(defun package-find-symbol-without-inherited (package str)
	(let ((index (package-find-symbol-index package str)))
		(if (= index -1)
			(values nil nil)
			(values 
				(package-entry-symbol package index)
				(package-entry-state package index)))))
			
;;
;;  Returns 2 values: the symbol (or nil), and 'internal, 'external or :inherited
;;	Redefines this here to look through inherited packages.
;; 
(defun package-find-symbol (package str)
    (when (symbolp str)
        (setq str (symbol-name str)))
   	(let ((index (package-find-symbol-index package str)))
		(if (= index -1)
			(progn
				(dolist (inherited-package (package-use-list package))
					(multiple-value-bind (sym status)
						(package-find-symbol-without-inherited inherited-package str)
						(if (eq status 'external)
							(return-from package-find-symbol 
								(values sym :inherited)))))
				(values nil nil))
			(values 
				(package-entry-symbol package index)
				(package-entry-state package index)))))

;;;;
;;;;	Common Lisp GENTEMP function.
;;;;
(let ((gentemp-counter 0))
	(defun gentemp (&optional (prefix "T") (package *package*))
		(let ((p (find-package package)))
			(check-type p package)
			(do ((sym-name (format nil "~A~D" prefix gentemp-counter)
						   (format nil "~A~D" prefix gentemp-counter)))
				((null (find-symbol sym-name p))
				 (values (intern sym-name p)))
				(incf gentemp-counter)))))

(defun load (path)
	(let* ((*package* *package*)
		   (*print-level* *print-level*)
		   (*read-level* *read-level*)
		   (cormanlisp::*source-file* path)
		   (cormanlisp::*source-line* nil)
		   (istream (open-input-file path))
		   (count 0)
		   (eof-value (cons 'eof nil)))
		(do ((x 
				(progn 
					(setq cormanlisp::*source-line* nil)
					(read istream nil eof-value nil)) 
				(progn 
					(setq cormanlisp::*source-line* nil)
					(read istream nil eof-value nil)))) 
			((eq x eof-value))
			(setq count (+ 1 count))
			(if *load-without-eval*
				(progn (write x :stream *standard-output*)(terpri))
				(progn
					(setq x (eval x))
					(if *load-verbose*
						(progn (write x :stream *standard-output*)(terpri))))))	
		 (close istream)
		 count))

(load "sys/cl-symbols.lisp")
(load "sys/pl-symbols.lisp")
(in-package :common-lisp)

;;; set up initial packages: 
;;; COMMON-LISP, COMMON-LISP-USER, KEYWORD, CORMANLISP

(let ((p (find-package :common-lisp)))
	(setf (uref p package-nicknames-offset) (list "CL" "LISP"))
	(setf (uref p package-use-list-offset) nil)
	(setf (uref p package-used-by-list-offset) 
		(list (find-package :common-lisp-user)(find-package :cormanlisp))))

(let ((p (find-package :common-lisp-user)))
	(setf (uref p package-nicknames-offset) (list "CL-USER" "USER"))
	(setf (uref p package-use-list-offset)  
		(list (find-package :common-lisp) (find-package :cormanlisp)))
	(setf (uref p package-used-by-list-offset) nil))

(let ((p (find-package :cormanlisp)))
	(setf (uref p package-nicknames-offset) (list "CCL" "PL"))
	(setf (uref p package-use-list-offset) (list (find-package :common-lisp)))
	(setf (uref p package-used-by-list-offset) (list (find-package :cl-user))))

(let ((p (find-package :keyword)))
	(setf (uref p package-nicknames-offset) nil)
	(setf (uref p package-use-list-offset) nil)
	(setf (uref p package-used-by-list-offset) nil))

;;;;
;;;;	Common Lisp FIND-SYMBOL function
;;;;    Redefined here to add synchronization.
;;;;
(defun find-symbol (string &optional (package *package*))
	(unless (packagep package)
		(setq package (or (find-package package)
                          (error "The name \"~A\" does not designate any package." package))))
	(unless (packagep package)
		(error "Invalid package: ~A" package))
    (with-synchronization (package-sync package)
    	(multiple-value-bind (sym status)
    		(package-find-symbol package string)
    		(if status
    			(cond 
    				((eq status 'internal)(values sym :internal))
    				((eq status 'external)(values sym :external))
    			    (t (values sym :inherited)))
    			(values nil nil)))))

;;;
;;; Create SYS package.
;;;
(make-package "SYS")

;;; redefine these for better error reporting and type checking
(defun symbol-name (sym) 
	(unless (symbolp sym) (error "Not a symbol: ~A" sym))
	(uref sym symbol-name-offset)) 
(defun set-symbol-name (val sym) 
	(unless (symbolp sym) (error "Not a symbol: ~A" sym))
	(uref-set val sym symbol-name-offset)) 
(defun set-symbol-value (val sym) 
	(unless (symbolp sym) (error "Not a symbol: ~A" sym))
	(rplaca (uref sym symbol-value-offset) val) 
	val) 
(defun symbol-package (sym) 
	(unless (symbolp sym) (error "Not a symbol: ~A" sym))
	(uref sym symbol-package-offset)) 
(defun set-symbol-package (val sym) 
	(unless (symbolp sym) (error "Not a symbol: ~A" sym))
	(uref-set val sym symbol-package-offset)) 
(defun symbol-plist (sym) 
	(unless (symbolp sym) (error "Not a symbol: ~A" sym))
	(uref sym symbol-plist-offset)) 
(defun set-symbol-plist (val sym) 
	(unless (symbolp sym) (error "Not a symbol: ~A" sym))
	(uref-set val sym symbol-plist-offset)) 

(setq *COMPILER-WARN-ON-UNDEFINED-FUNCTION* nil)
(defun singleton-string (char)
	(make-string 1 :initial-element char))		;; make-string is defined later
(setq *COMPILER-WARN-ON-UNDEFINED-FUNCTION* t)
	
(setq *features* '(:cormanlisp :pl :common-lisp :win32 :os-windows :x86 :32-bit :little-endian :hardware-gc))

