;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		read.lisp
;;;;	Contents:	Corman Lisp startup code to build the
;;;;				system.
;;;;	History:	8/1/96  RGC  Created.
;;;;				6/6/01  RGC  Integrated latest changes from Frank Adrian
;;;;							 to fix bugs with package designators.
;;;;				8/1/01  RGC  Fixed handling of escaped : in symbols.
;;;;

(defvar *standard-input* *standard-input*)	;; make it special

;;
;; If bound, *READ-HOOK* should be a function (or name a function) 
;; which takes the following 5 arguments:
;;      stream:     the stream being read
;;      startpos:   the beginning character position of the object being read
;;      endpos:     the end character position of the object being read
;;      object:     the resulting object
;;      commentp:   true if the object is a comment string
;;
;;      As a special case, when a comment is scanned, the *READ-HOOK* function
;;      will be called with the object returned as nil and the commentp flag true.
;;      
(defvar *read-hook* nil)

(defconstant whitespace-char-type							0)
(defconstant constituent-char-type							1)
(defconstant terminating-macro-char-type					2)
(defconstant non-terminating-macro-char-type				3)
(defconstant dispatching-terminating-macro-char-type		4)
(defconstant dispatching-non-terminating-macro-char-type	5)
(defconstant single-escape-char-type						6)
(defconstant multiple-escape-char-type						7)
(defconstant illegal-char-type								8)

(defconstant dot-marker (cons 0 0))         ;; used to flag a dot marker in a list

(let ((x (gensym)))
	(defun make-eq-form-placeholder (n)
		(list x n))
	(defun eq-form-placeholder-p (form)
		(and (consp form) (eq (car form) x)))
	(defun eq-form-placeholder-index (ph) (second ph)))

(defvar *read-eq-forms* nil)	;; for #= and ## read macros

(defconstant readtable-table-offset 3)
(defun readtable-table (readtable) (uref readtable readtable-table-offset))
(defun readtable-char-type (readtable char)
	(let ((readtable-vector (readtable-table readtable)))
		(elt readtable-vector (* 2 (char-int char)))))
(defun readtable-char-func (readtable char)
	(let ((readtable-vector (readtable-table readtable)))
		(elt readtable-vector (1+ (* 2 (char-int char))))))

;;;
;;; Common Lisp GETF function.
;;;
(defun getf (plist property &optional default)
	(do* ((p plist (cddr p)))
		 ((null p) default)
		 (if (eq (car p) property)
			(return (cadr p)))))

(defun get-read-eq-form (int) (getf *read-eq-forms* int))

(defun readtable-char-dispatch-func (readtable char1 char2)
	(let* ((dispatch-vector (readtable-char-func readtable char1)))
		(elt dispatch-vector (char-int char2))))

;;
;; Common Lisp DIGIT-CHAR-P function
;;

(defconstant *lower-case-a-code* (char-int #\a))
(defconstant *lower-case-z-code* (char-int #\z))
(defconstant *upper-case-a-code* (char-int #\A))
(defconstant *upper-case-z-code* (char-int #\Z))

(defun alpha-char-p (char) 
	(or 
		(and (>= (char-int char) *lower-case-a-code*)
			 (<= (char-int char) *lower-case-z-code*))
		(and (>= (char-int char) *upper-case-a-code*)
			 (<= (char-int char) *upper-case-z-code*))))

(defun lower-case-p (char) 
	(and (>= (char-int char) *lower-case-a-code*)
		 (<= (char-int char) *lower-case-z-code*)))

(defun upper-case-p (char) 
	(and (>= (char-int char) *upper-case-a-code*)
		 (<= (char-int char) *upper-case-z-code*)))

(defconstant zero-code (char-int #\0))
(defconstant nine-code (char-int #\9))
(defconstant A-code (char-int #\A))
(defconstant Z-code (char-int #\Z))
(defun digit-char-p (char &optional (radix 10))
	(let ((code (char-int (char-upcase char))))
		(if (and (>= code zero-code)(<= code nine-code))
			(let ((weight (- code zero-code)))
				(if (< weight radix) weight nil))
			(if (and (>= code A-code) (<= code Z-code))
				(let ((weight (+ 10 (- code A-code))))
					(if (< weight radix) weight nil))
				nil))))

(defun valid-integer-chars (chars)
	(let ((digits 0)
		  (negative nil)
		  (value 0)
		  (rbase *read-base*)
		  d)
		(if (eq (car chars) #\+)
			(setq chars (cdr chars))
			(if (eq (car chars) #\-)
				(progn
					(setq negative t)
					(setq chars (cdr chars)))))
		(do ((char (car chars)))
			((null chars))
			(setq d (digit-char-p char rbase))
			(if (null d)
				(return))
			(setq value (+ (* value rbase) d))
			(setq digits (+ 1 digits))
			(setq chars (cdr chars))
			(setq char (car chars)))

		(if (eq (car chars) #\.)
			(setq chars (cdr chars)))		;; allow trailing '.'

		(if (> digits 0)
			(progn
				(if negative
					(setq value (- value)))
				(return-from valid-integer-chars (list value chars)))
			(return-from valid-integer-chars (list nil nil)))))
	
(defun valid-integer (chars)
	(let* ((result (valid-integer-chars chars))
		   (n1 (car result))
		   (remaining-chars (cadr result)))
		(if (and n1 (null remaining-chars))
			n1
			nil)))

(defun valid-ratio (chars)
	(let* ((result (valid-integer-chars chars))
		   (numerator (car result))
		   (remaining-chars (cadr result)))
		(if (and numerator remaining-chars (eq (car remaining-chars) #\/))
			(let ((denominator (valid-integer (cdr remaining-chars))))
				(if denominator (/ numerator denominator) nil)))))			

(defun create-number-from-chars (chars)
	(let ((n (valid-integer chars)))
		(unless n (setq n (%chars-to-float chars)))
		(unless n (setq n (valid-ratio chars)))
		n))
	
(defun package-entry-occupied (package index)
	(let ((package-vector (package-table package)))
		(stringp (elt package-vector (* index 3)))))

(defun package-entry-string (package index)
	(let ((package-vector (package-table package)))
		(elt package-vector (* index 3))))

(defun set-package-entry-string (str package index)
	(let ((package-vector (package-table package)))
		(setelt str package-vector (* index 3))))

(defun package-entry-symbol (package index)
	(let ((package-vector (package-table package)))
		(elt package-vector (+ (* index 3) 1))))

(defun set-package-entry-symbol (sym package index)
	(let ((package-vector (package-table package)))
		(setelt sym package-vector (+ (* index 3) 1))))

(defun package-entry-state (package index)
	(let ((package-vector (package-table package)))
		(if (= (elt package-vector (+ (* index 3) 2)) 1)
			'external
			'internal)))

(defun set-package-entry-state (state package index)
	(let ((package-vector (package-table package)))
		(if (eq state 'external)
			(setelt 1 package-vector (+ (* index 3) 2))
			(setelt 0 package-vector (+ (* index 3) 2)))))

;;;;	Define package-grow later.
(defun package-grow (package)
	(error "PACKAGE-GROW function not implemented--package capacity exceeded"))

;;;
;;;	Corman Lisp INPUT-CHARACTER-STREAM-P function.
;;;	Redefined later.
;;;
(defun input-character-stream-p (s)
	(and (streamp s) (not (uref s stream-binary-offset))))

;;
;;	Common Lisp LIST-ALL-PACKAGES function
;;
(defun list-all-packages () *package-list*)

(defun singleton-string (char)
	(error "Forward definition only."))

(defun canonicalize-string-designator (obj)
	(cond
		((stringp obj) obj)
		((symbolp obj) (symbol-name obj))
		((characterp obj) (singleton-string obj))
		(t (error "Illegal object as string designator: ~A." obj))))

;;
;;	Common Lisp FIND-PACKAGE function
;;
(defun find-package (name)
	(if (packagep name)
		(return-from find-package name))
	(setq name (canonicalize-string-designator name))
	(let ((packages (list-all-packages)))
		(dolist (p packages)
			(if (string= name (package-name p))
				(return p))
			(dolist (nickname (package-nicknames p))
				(if (string= nickname name)
					(return-from find-package p))))))

(defun package-find-symbol-index (package str)
	(let ((table-size (package-capacity package))
		  (h (package-hash-index package str))
		  offset)
		(if (not (package-entry-occupied package h))
			(return-from package-find-symbol-index -1)
			(if (not (string= str (package-entry-string package h)))
				(progn
					(setq offset (- (- table-size 1) h))
					(if (= offset 0) (setq offset 1))
					(do ()
						(nil)
						(setq h (- h offset))
						;; if not found there, look from the end
						(if (< h 0) (setq h (+ h table-size)))
						(if (not (package-entry-occupied package h))
							(return -1))
						(if (string= str (package-entry-string package h))
							(return h))))
				(return-from package-find-symbol-index h)))))

;;
;; returns 2 values: the symbol (or nil), and 'internal, 'external or nil
;; 
(defun package-find-symbol (package str)
	(let ((index (package-find-symbol-index package str)))
		(if (= index -1)
			(values nil nil)
			(values 
				(package-entry-symbol package index)
				(package-entry-state package index)))))

(defun package-add-symbol (package str sym state)
	(let* ((table-size (package-capacity package))
		   (count (package-count package))
		   h
		   offset)

		(if (> count (/ table-size 2))
			(progn
				(package-grow package)		;; grow if necessary
				(setq table-size (package-capacity package))
				(setq count (package-count package))))

		(setq h (package-hash-index package str))

		(if (and (package-entry-occupied package h)
				(not (string= (package-entry-string package h) str)))
			(progn
				(setq offset (- (- table-size 1) h))
				(if (= offset 0) (setq offset 1))
 				(do ()
					(nil)
					(setq h (- h offset))
					(if (< h 0) (setq h (+ h table-size)))
					(if (or (not (package-entry-occupied package h))
							(string= (package-entry-string package h) str))
						(return)))))
		(if (not (package-entry-occupied package h))
			(set-package-entry-string str package h))
		(set-package-entry-symbol sym package h) 
		(set-package-entry-state state package h) 
		(set-package-count (+ (package-count package) 1) package)
		(unless (symbol-package sym)
			(set-symbol-package package sym))	;; if no home package, make this its home
		sym))

;;;;
;;;;	Common Lisp INTERN function
;;;;
(defun intern (str &optional (package *package*))
	(unless (packagep package)
		(setq package (find-package package)))
	(unless (packagep package)
		(error "Invalid package: ~A" package))
	(let* ((ret (multiple-value-list (package-find-symbol package str)))
		   (state (cadr ret))
		   (keyword nil)
		   sym)
		(if state (return-from intern (values (car ret) state)))
		(if (string= (package-name package) "KEYWORD")
			(progn (setq state 'EXTERNAL) (setq keyword t))
			(setq state 'INTERNAL))
		(setq sym (package-add-symbol package str (make-symbol str) state))
		(if keyword 
			(progn
				(set-symbol-value sym sym)	;; keywords evaluate to themselves
				(symbol-set-constant-flag sym)
				(symbol-set-special-flag sym)))
		(values sym state)))

(defun create-symbol-from-chars (chars)
	(let ((package-chars nil)
		  (symbol-chars nil)
		  (package-markers 0)
		  (package-name nil)
		  (symbol-name nil)
		  (found-package-marker nil)
		  (in-keyword-package nil)
		  c)
		(dolist (i chars)
			(if (eq i #\:)		;; look for unescaped colon
				(progn 
					(setq found-package-marker t)
					(return))))
		(unless found-package-marker
			(let ((ch-list nil))
				(dolist (x chars)
					(setq ch-list (cons (if (consp x) (car x) x) ch-list)))
				(return-from create-symbol-from-chars (intern (coerce (nreverse ch-list) 'string) *package*))))
					
		(do ()
			((null chars))
			(setq c (car chars))
			(if (eq c #\:)
				(return))
			(setq package-chars (cons (if (consp c) (car c) c) package-chars))
			(setq chars (cdr chars)))
	
		(if (eq c #\:)
			(progn
				(setq package-markers (+ 1 package-markers))
				(setq chars (cdr chars))
				(setq c (car chars))
				(if (eq c #\:)
					(progn
						(setq package-markers (+ 1 package-markers))
						(setq chars (cdr chars))
						(setq c (car chars))))))

		(do ((c (car chars)(car chars)))
			((null chars))
			(setq symbol-chars (cons (if (consp c) (car c) c) symbol-chars))
			(setq chars (cdr chars)))

		(if (> package-markers 0)
			(progn
				(if package-chars
					(setq package-name (coerce (nreverse package-chars) 'string))
					(setq package-name "KEYWORD" in-keyword-package t))
				(setq symbol-name (coerce (nreverse symbol-chars) 'string)))
			(setq symbol-name (coerce (nreverse package-chars) 'string)))

		(if package-name
			(let ((package (find-package package-name)))
				(if (null package)
					(error "Could not find the requested package ~A" package-name))
				(let ((state 
						(cadr (multiple-value-list (package-find-symbol package symbol-name)))))
					(if (and (eq state 'internal) (= package-markers 1) (not in-keyword-package))
						(error "The symbol ~A is not exported by package ~A"
							symbol-name (package-name package))))
				(intern symbol-name package))
			(intern symbol-name *package*))))
		
(defun whitespace-char (ch &optional (readtable *readtable*)) 
	(eq (readtable-char-type readtable ch) 'whitespace-char-type))
			
(defun read-expression (&optional 
				(stream *standard-input*) 
				(eof-error t) 
				(eof-value nil)
				(recursive-p nil))

	(unless (input-character-stream-p stream)
		(error "Expected an input character stream, got ~A" stream))
				
	(do (  (state 1)
		   (readtable *readtable*)
			x
			y
			z
			func
			ret
		   (chars nil)
			char-type
			call-ret
			third-dispatch-arg
		   (token nil)
		   (*read-level* (if recursive-p (+ *read-level* 1) *read-level*)))
		   (nil)
			(cond
				((= state 1)
				 (progn
					(setq x (%read-char stream))
					(if (null x) 
						(if eof-error
							(error "End-of-file encountered reading from stream ~A" stream)
							(return eof-value)))
					(setq char-type (readtable-char-type readtable x))

					(cond
						((eq char-type 'illegal-char-type)
						 (error "Illegal character found in input"))
						((eq char-type 'whitespace-char-type) nil)
						((or (eq char-type 'terminating-macro-char-type)
							 (eq char-type 'non-terminating-macro-char-type))
						 (return 
							(funcall (readtable-char-func readtable x) stream x)))
						((or (eq char-type 'dispatching-terminating-macro-char-type)
							 (eq char-type 'dispatching-non-terminating-macro-char-type))
						 (setq y (%read-char-with-error stream))
						 (setq y (char-upcase y))
						 (if (digit-char-p y)
							(progn
								(do ((dispatch-int 0))
									((not (digit-char-p y)))
									(setq dispatch-int (+ (* 10 dispatch-int) (digit-char-p y)))
									(setq y (%read-char-with-error stream))
									(setq y (char-upcase y))
									(setq third-dispatch-arg dispatch-int)))
							(setq third-dispatch-arg nil))
						 (setq func (readtable-char-dispatch-func readtable x y))
						 (if (not (functionp func))
							(error "Invalid input form"))
						 (return 
							(funcall func stream y third-dispatch-arg)))
						((eq char-type 'single-escape-char-type)
						 (setq y (%read-char-with-error stream))
						 (setq token (cons y token))
						 (setq state 8))
						((eq char-type 'multiple-escape-char-type)
						 (setq state 9))
						((eq char-type 'constituent-char-type)
						 (setq x (char-upcase x))
						 (setq token (cons x token))
						 (setq state 8))
						(t "Invalid character type"))))

				((= state 8)
				 (setq y (%read-char stream))
				 (if (null y)
					(setq state 10)
					(progn
						(setq char-type (readtable-char-type readtable y))
						(cond
							((or
								(eq char-type 'constituent-char-type)
								(eq char-type 'non-terminating-macro-char-type)
								(eq char-type 'dispatching-non-terminating-macro-char-type))
							 (progn
								(setq y (char-upcase y))
								(setq token (cons y token))))
							((eq char-type 'single-escape-char-type)
							 (setq z (%read-char-with-error stream))
							 (setq token (cons z token)))
							((eq char-type 'multiple-escape-char-type)
							 (setq state 9))
							((eq char-type 'illegal-char-type)
							 (error "Illegal character found in input"))
							((or (eq char-type 'terminating-macro-char-type)
								(eq char-type 'dispatching-terminating-macro-char-type))
							 (unread-char y stream)
							 (setq state 10))
							((eq char-type 'whitespace-char-type)
							 (setq state 10))))))

				((= state 9)
				 (setq y (%read-char-with-error stream))
				 (setq char-type (readtable-char-type readtable y))
				 (cond
					((or
						(eq char-type 'constituent-char-type)
						(eq char-type 'terminating-macro-char-type)
						(eq char-type 'non-terminating-macro-char-type)
						(eq char-type 'dispatching-terminating-macro-char-type)
						(eq char-type 'dispatching-non-terminating-macro-char-type)
						(eq char-type 'whitespace-char-type))
					 (setq token (cons y token)))
					((eq char-type 'single-escape-char-type)
					 (setq z (%read-char-with-error stream))
					 (setq token (cons z token)))
					((eq char-type 'multiple-escape-char-type)
					 (setq state 8))
					((eq char-type 'illegal-char-type)
					 (error "Illegal character found in input"))))

				((= state 10)
				 (if *read-suppress* (return nil))
				 (setq token (nreverse token))
				 (setq ret (create-number-from-chars token))
				 (if (null ret)
					(setq ret (create-symbol-from-chars token)))
				 (return ret)))))


(defun patch-up-eq-forms (form patchups)
	(if (consp form)
		(progn
			(if (eq-form-placeholder-p (car form))
				(let ((n (get-read-eq-form (eq-form-placeholder-index (car form)))))
					(unless n 
						(error "Cannot find definition of #~D# in form being read"
							(eq-form-placeholder-index (car form))))
					(rplaca form n))
				(patch-up-eq-forms (car form) patchups))
			(if (eq-form-placeholder-p (cdr form))
				(let ((n (get-read-eq-form (eq-form-placeholder-index (cdr form)))))
					(unless n 
						(error "Cannot find definition of #~D# in form being read"
							(eq-form-placeholder-index (cdr form))))
					(rplacd form n))
				(patch-up-eq-forms (cdr form) patchups)))))

(defun stream-position (stream) 0)  ;; this gets defined later
						
(defun read (&optional 
				(stream *standard-input*) 
				(eof-error t) 
				(eof-value nil)
				(recursive-p nil))

    (cond ((eq stream t) (setq stream *terminal-io*))
          ((eq stream nil) (setq stream *standard-input*))
          ((not (streamp stream)) 
           (error "Stream argument to READ is not a stream: ~S." stream)))
    
    (let ((startpos (stream-position stream)))
    	(if (not recursive-p)
    		(let ((*read-eq-forms* nil))		
    
    			;; this loop allows skipping over read expressions which return
    			;; no values i.e. commented expressions
    			(do ((ret))
    			    (nil)
    				(setq ret (multiple-value-list 
    						(read-expression stream eof-error eof-value recursive-p)))
    				(if (consp ret) 
    					(progn
    						;; look for read-eq-form-placeholder structures
    						(patch-up-eq-forms (car ret) *read-eq-forms*)
                            (if *read-hook*
                                (funcall *read-hook* stream startpos 
                                    (stream-position stream) (car ret) nil))
    						(return (car ret)))
                        (if *read-hook*
                            (funcall *read-hook* stream startpos 
                                (stream-position stream) nil t)))))
    			
    		;; if recursive, do no rebind *read-eq-forms*
    		(do ((ret))
    		    (nil)
    			(setq ret (multiple-value-list 
    					(read-expression stream eof-error eof-value recursive-p)))
    			(if (consp ret)
                    (progn
                        (if *read-hook*
                            (funcall *read-hook* stream startpos 
                                (stream-position stream) (car ret) nil))
                        (return (car ret)))
                    (if *read-hook* 
                        (funcall *read-hook* stream startpos 
                            (stream-position stream) nil t)))))))

;;;
;;; faa20001128a -	Add support for readtable-case.
;;; At this point, the reader knows how to handle keywords, so we can
;;; relatively easily add support for readtable-case by redefining the
;;; function read-expression.  Of course there are a few helper functions
;;; and the accessors for readtable case defined before the big enchilada
;;; itself.
;;;

(defconstant readtable-case-offset 4)
(defun readtable-case (readtable) (uref readtable readtable-case-offset))
(uref-set :upcase *readtable* readtable-case-offset)

(defun case-coerce (char rc)
	"Coerce the characters for the given readtable-case."
	(cond ((eq rc :upcase) (char-upcase char))
		((eq rc :downcase) (char-downcase char))
		(t char)))

(defun case-invert (char)
	"If the character's case is upper, return lower-case version.
	Same for lower case.  If no case, leave unchanged."
	(cond ((upper-case-p char) (char-downcase char))
		((lower-case-p char) (char-upcase char))
		(t char)))

(defun update-cases-seen (char cs)
	"Track the cases seen so far in adding characters to the token."
	(cond ((upper-case-p char)
			(cond ((null cs) :upper)
				((eq cs :lower) :both)
				(t cs)))
		((lower-case-p char)
			(cond ((null cs) :lower)
				((eq cs :upper) ':both)
				(t cs)))
		(t cs)))

(defun need-to-invert-p (cs)
	"We only need to invert the token if the case has been constant."
	(or (eq cs :upper) (eq cs :lower)))

;; As we accumulate characters in a list, we will list characters 
;; i.e. (#\c) when we want to escape them.
(defun read-expression (&optional 
				(stream *standard-input*) 
				(eof-error t) 
				(eof-value nil)
				(recursive-p nil)
		        (interpret-numerically t))
	(unless (input-character-stream-p stream)
		(error "Expected an input character stream, got ~A" stream))
				
	(do (  (state 1)
		   (readtable *readtable*)
			x
			y
			z
			func
			char-type
			third-dispatch-arg
		   (token nil)
		   (rc (readtable-case *readtable*))
		   (cases-seen nil)
		   (itoken nil)
		   (has-escape nil)
		   (ret nil)
		   (*read-level* (if recursive-p (+ *read-level* 1) *read-level*)))
		   (nil)
			(cond
				((= state 1)
				 (progn
					(setq x (%read-char stream))
					(if (null x) 
						(if eof-error
							(error "End-of-file encountered reading from stream ~A" stream)
							(return eof-value)))
					(setq char-type (readtable-char-type readtable x))

					(cond
						((eq char-type 'illegal-char-type)
						 (error "Illegal character found in input"))
						((eq char-type 'whitespace-char-type) nil)
						((or (eq char-type 'terminating-macro-char-type)
							 (eq char-type 'non-terminating-macro-char-type))
						 (return 
							(funcall (readtable-char-func readtable x) stream x)))
						((or (eq char-type 'dispatching-terminating-macro-char-type)
							 (eq char-type 'dispatching-non-terminating-macro-char-type))
						 (setq y (%read-char-with-error stream))
						 (setq y (char-upcase y))
						 (if (digit-char-p y)
							(progn
								(do ((dispatch-int 0))
									((not (digit-char-p y)))
									(setq dispatch-int (+ (* 10 dispatch-int) (digit-char-p y)))
									(setq y (%read-char-with-error stream))
									(setq y (char-upcase y))
									(setq third-dispatch-arg dispatch-int)))
							(setq third-dispatch-arg nil))
						 (setq func (readtable-char-dispatch-func readtable x y))
						 (if (not (functionp func))
							(error "Invalid input form"))
						 (return 
							(funcall func stream y third-dispatch-arg)))
						((eq char-type 'single-escape-char-type)
						 (setq y (%read-char-with-error stream))
						 (setq token (cons (list y) token))		;; escape it
						 (setq itoken (cons (list y) itoken))	;; escape it
						 (setq has-escape t)
						 (setq state 8))
						((eq char-type 'multiple-escape-char-type)
						 (setq has-escape t)
						 (setq state 9))
						((eq char-type 'constituent-char-type)
						 (setq x (case-coerce x rc))
						 (setq itoken (cons (case-invert x) itoken))
						 (setq cases-seen (update-cases-seen x cases-seen))
						 (setq token (cons x token))
						 (setq state 8))
						(t "Invalid character type"))))

				((= state 8)
				 (setq y (%read-char stream))
				 (if (null y)
					(setq state 10)
					(progn
						(setq char-type (readtable-char-type readtable y))
						(cond
							((or
								(eq char-type 'constituent-char-type)
								(eq char-type 'non-terminating-macro-char-type)
								(eq char-type 'dispatching-non-terminating-macro-char-type))
							 (progn
								(setq y (case-coerce y rc))
								(setq itoken (cons (case-invert y) itoken))
								(setq cases-seen (update-cases-seen y cases-seen))
								(setq token (cons y token))))
							((eq char-type 'single-escape-char-type)
							 (setq z (%read-char-with-error stream))
							 (setq has-escape t)
							 (setq itoken (cons (list z) itoken))
							 (setq token (cons (list z) token)))
							((eq char-type 'multiple-escape-char-type)
							 (setq has-escape t)
							 (setq state 9))
							((eq char-type 'illegal-char-type)
							 (error "Illegal character found in input"))
							((or (eq char-type 'terminating-macro-char-type)
								(eq char-type 'dispatching-terminating-macro-char-type))
							 (unread-char y stream)
							 (setq state 10))
							((eq char-type 'whitespace-char-type)
							 (setq state 10))))))

				((= state 9)
				 (setq y (%read-char-with-error stream))
				 (setq char-type (readtable-char-type readtable y))
				 (cond
					((or
						(eq char-type 'constituent-char-type)
						(eq char-type 'terminating-macro-char-type)
						(eq char-type 'non-terminating-macro-char-type)
						(eq char-type 'dispatching-terminating-macro-char-type)
						(eq char-type 'dispatching-non-terminating-macro-char-type)
						(eq char-type 'whitespace-char-type))
					 (setq itoken (cons (list y) itoken))	;; escape it
					 (setq token (cons (list y) token)))	;; escape it
					((eq char-type 'single-escape-char-type)
					 (setq z (%read-char-with-error stream))
					 (setq has-escape t)
					 (setq itoken (cons (list z) itoken))	;; escape it
					 (setq token (cons (list z) token)))	;; escape it
					((eq char-type 'multiple-escape-char-type)
					 (setq state 8))
					((eq char-type 'illegal-char-type)
					 (error "Illegal character found in input"))))

				((= state 10)
				 (if *read-suppress* (return nil))
				 (setq token
					(nreverse
						(if (and (eq (readtable-case *readtable*) :invert)
								(need-to-invert-p cases-seen))
							itoken
							token)))
				 (when (and interpret-numerically (not has-escape))
					(setq ret (create-number-from-chars token)))
                
                 ;; Check for dot in list. If the dot was escaped, we will treat it
                 ;; as a regular symbol.
                 (if (and (eq (car token) #\.)(null (cdr token)) (not has-escape))
                    (setq ret dot-marker))
				 (if (null ret)
					(setq ret (create-symbol-from-chars token)))
				 (return ret)))))

(defvar *load-verbose* nil)
(defvar *load-without-eval* nil)
(defvar cormanlisp::*source-file* nil)
(defvar cormanlisp::*source-line* nil)
(defvar cormanlisp::*save-debug-info* nil)
(defvar cormanlisp::*source-lines* nil)
(defvar cormanlisp::*save-relative-source-file-names* nil)

;;;; This will get redefined later
(defun load (path)
	(let ((istream (open-input-file path))
		  (count 0)
		  (eof-value (cons 'eof nil))
		  (cormanlisp::*source-file* path))
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

(defvar cormanlisp::*compiler-optimize-speed* (symbol-value 'cormanlisp::*compiler-optimize-speed*))
(defvar cormanlisp::*compiler-optimize-safety* (symbol-value 'cormanlisp::*compiler-optimize-safety*))
(defvar cormanlisp::*compiler-optimize-safety* (symbol-value 'cormanlisp::*compiler-optimize-safety*))

(defun compiler-check-args-num () 
	(< cormanlisp::*compiler-optimize-speed*
		cormanlisp::*compiler-optimize-safety*))		
(defun compiler-check-types ()
	(< cormanlisp::*compiler-optimize-speed*
		cormanlisp::*compiler-optimize-safety*))
(defun compiler-check-key-args () 
	(< cormanlisp::*compiler-optimize-speed*
		cormanlisp::*compiler-optimize-safety*))
(defvar cormanlisp::*compiler-fold-constants* t)		
(defun compiler-fold-constants () cormanlisp::*compiler-fold-constants*)
(defun compiler-inline-functions () t)
(defun compiler-eliminate-tail-recursion () nil)


