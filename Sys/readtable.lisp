;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		readtable.lisp
;;;;	Contents:	Corman Lisp startup code to build the
;;;;				system.
;;;;	History:	8/1/96  RGC  Created.
;;;;				2/1/01  RGC  Added support for #nnR (Frank Adrian).
;;;;				4/16/01 RGC  Fixed reading of structs to work correctly.
;;;;				5/17/01 RGC  Fixed a problem with READ-DELIMITED-LIST with
;;;;							 lists ending with ". NIL".
;;;;                11/7/02 RGC  Incorporated a fix from Pavel Grozman for #\# #\\ reader macro.
;;;;

(defvar *readtable* *readtable*)
(defvar *common-lisp-readtable* (symbol-value '*common-lisp-readtable*))

(defconstant readtable-read-level-offset			1)
(defconstant readtable-backquote-processing-offset	2)
; (defconstant readtable-table-offset					3) ;; defined earlier

;; faa20001128a -	Add support for readtable-case.
;; Move this to read.lisp.
;(defconstant readtable-case-offset					4) ;; defined earlier
;; faa20001128a -	End change.

;;;
;;;	Corman Lisp INPUT-CHARACTER-STREAM-P function.
;;;
(defun input-character-stream-p (s)
	(and (streamp s)
		(not (uref s stream-binary-offset))
		(let ((direction (uref s stream-direction-offset)))
			(or (eq direction :input)
				(eq direction :bidirectional)))))

;;;
;;;	Corman Lisp OUTPUT-CHARACTER-STREAM-P function.
;;;
(defun output-character-stream-p (s)
	(and (streamp s)
		(not (uref s stream-binary-offset))
		(let ((direction (uref s stream-direction-offset)))
			(or (eq direction :output)
				(eq direction :bidirectional)))))

;;
;; Common Lisp COPY-READTABLE function
;;
(defun copy-readtable (&optional (from-readtable *readtable*) 
	to-readtable)
	(if (null from-readtable)
		(setq from-readtable *common-lisp-readtable*))
	(let (dest)
		(if (readtablep to-readtable)
			(setq dest to-readtable)
			(setq dest (alloc-uvector 4 uvector-readtable-tag)))
		(setf (uref dest readtable-read-level-offset) 
			(uref from-readtable readtable-read-level-offset))
		(setf (uref dest readtable-backquote-processing-offset)
			(uref from-readtable readtable-backquote-processing-offset))
		(setf (uref dest readtable-case-offset)
			(uref from-readtable readtable-case-offset))
		(setf (uref dest readtable-table-offset)
			(make-array '(512) :initial-contents (uref from-readtable readtable-table-offset)))
		dest))
 
;;
;; Common Lisp SET-SYNTAX-FROM-CHAR function
;;
(defun set-syntax-from-char (to-char from-char 
	&optional (to-readtable *readtable*)
		(from-readtable nil))
	(if (null from-readtable)
		(setq from-readtable *common-lisp-readtable*))
	(let ((src-table (uref from-readtable readtable-table-offset))
		  (dst-table (uref to-readtable readtable-table-offset))
		  (src-index (* (char-int from-char) 2))
		  (dst-index (* (char-int to-char) 2)))
		(setf (elt dst-table dst-index) (elt src-table src-index))
		(setf (elt dst-table (+ dst-index 1)) (elt src-table (+ src-index 1)))
		t))

;;
;; Common Lisp SET-MACRO-CHARACTER function
;;
(defun set-macro-character (char function 
	&optional (non-terminating-p nil) (readtable *readtable*))
	(unless (readtablep readtable) (setq readtable *readtable*))
	(unless (functionp function) (error "Non-function passed to SET-MACRO-CHARACTER"))
	(let ((table (uref readtable readtable-table-offset))
		  (index (* (char-int char) 2)))
		(if non-terminating-p 
			(setf (elt table index) 'NON-TERMINATING-MACRO-CHAR-TYPE)
			(setf (elt table index) 'TERMINATING-MACRO-CHAR-TYPE))
		(setf (elt table (+ index 1)) function)
		t))

	
;;
;; Common Lisp GET-MACRO-CHARACTER function
;;
(defun get-macro-character (char &optional (readtable *readtable*))
	(unless (readtablep readtable) (setq readtable *common-lisp-readtable*))
	(let ((table (uref readtable readtable-table-offset))
		  (index (* (char-int char) 2)))
		(if (eq (elt table index) 'NON-TERMINATING-MACRO-CHAR-TYPE)
			(values (elt table (+ 1 index)) t)
			(if (eq (elt table index) 'TERMINATING-MACRO-CHAR-TYPE)
				(values (elt table (+ 1 index)) nil)
				nil))))

;;
;; Common Lisp MAKE-DISPATCH-MACRO-CHARACTER function
;;
(defun make-dispatch-macro-character (char 
	&optional (non-terminating-p nil) (readtable *readtable*))
	(unless (readtablep readtable) (setq readtable *readtable*))
	(let* ((table (uref readtable readtable-table-offset))
		   (index (* (char-int char) 2))
		   (dispatch-table (make-array '(256) 
			:initial-element 
				#'(lambda (stream subchar arg) 
					(error "Invalid dispatch macro character")))))
		(if non-terminating-p
			(setf (elt table index) 'DISPATCHING-NON-TERMINATING-MACRO-CHAR-TYPE)
			(setf (elt table index) 'DISPATCHING-TERMINATING-MACRO-CHAR-TYPE))
		(setf (elt table (+ index 1)) dispatch-table)
		t))
	
;;
;; Common Lisp SET-DISPATCH-MACRO-CHARACTER function
;;
(defun set-dispatch-macro-character (disp-char sub-char function 
	&optional (readtable *readtable*))
	(unless (readtablep readtable) (setq readtable *readtable*))
	(unless (functionp function) (error "Non-function passed to SET-DISPATCH-MACRO-CHARACTER"))
	(if (digit-char-p sub-char)
		(error "Attempted to create a dispatching macro with a decimal digit"))
	(setq sub-char (char-upcase sub-char)) 
	(let* ((table (uref readtable readtable-table-offset))
		   (index (* (char-int disp-char) 2))
		   (dispatch-table (elt table (+ index 1))))
		(unless (vectorp dispatch-table)
			(error "Not a dispatching macro"))
		(setf (elt dispatch-table (char-int sub-char)) function)
		t))  

;;
;; Common Lisp GET-DISPATCH-MACRO-CHARACTER function
;;
(defun get-dispatch-macro-character (disp-char sub-char 
	&optional (readtable *readtable*))
	(unless (readtablep readtable) (setq readtable *common-lisp-readtable*))
	(if (digit-char-p sub-char)
		(return-from get-dispatch-macro-character nil))
	(setq sub-char (char-upcase sub-char)) 
	(let* ((table (uref readtable readtable-table-offset))
		   (index (* (char-int disp-char) 2))
		   (dispatch-table (elt table (+ index 1))))
		(elt dispatch-table (char-int sub-char))))

(set-dispatch-macro-character #\# #\A 
	#'(lambda (stream ch arg)
		(if (null arg) (setq arg 1))
		(let* ((dimensions nil)
			   (initform (read stream t nil t))
			   (f initform))
			(dotimes (i arg)
				(push (length f) dimensions)
				(setq f (car f)))
			(make-array (nreverse dimensions) :initial-contents initform))))
			
(set-dispatch-macro-character #\# #\(   ;)
	#'(lambda (stream ch length)
		(declare (ignore ch))
		(unread-char #\( stream)  		;)
		(let ((initform (read stream t nil t)))
			(if (integerp length)
				(let ((a (make-array length :initial-element nil))
					  (e (pop initform)))
					(dotimes (i length)
						(setf (elt a i) e)
						(if initform (setq e (pop initform))))
					a)
				(make-array (length initform) :initial-contents initform)))))
				
(set-dispatch-macro-character #\# #\C 
	#'(lambda (stream ch arg)
        (declare (ignore ch arg))
		(let* ((initform (read stream t nil t)))
            (unless *read-suppress*
                (complex (car initform) (cadr initform))))))

(set-dispatch-macro-character #\# #\X 
	#'(lambda (stream ch arg)
		(let* ((*read-base* 16)
			   (n (read stream t nil t)))
			n)))

(set-dispatch-macro-character #\# #\B
	#'(lambda (stream ch arg)
		(let* ((*read-base* 2)
			   (n (read stream t nil t)))
			n)))

(set-dispatch-macro-character #\# #\O
	#'(lambda (stream ch arg)
		(let* ((*read-base* 8)
			   (n (read stream t nil t)))
			n)))

(set-dispatch-macro-character #\# #\R
     #'(lambda (stream ch arg)
          (declare (ignore ch))
          (if (or (< arg 2) (> arg 36))
               (error "Bad argument for radix: ~A." arg))
          (let* ((*read-base* arg)
                  (n (read stream t nil t)))
               n)))

(set-dispatch-macro-character #\# #\S
	#'(lambda (stream ch arg)
		(declare (ignore arg ch))
		(let* ((initform (read stream t nil t))
			   struct-name
			   struct-constructor)
			(unless (listp initform)
				(error "Invalid form for structure: #S~A" initform))
			(setq struct-name (car initform))
			(unless struct-name
				(error "Invalid form for structure: #S~A" initform))
			(setq struct-constructor (get struct-name ':STRUCT-CONSTRUCTOR))
			(unless struct-constructor
				(error "Cannot find structure constructor for structure ~A" struct-name))
			
			(apply struct-constructor (cdr initform)))))

(set-dispatch-macro-character #\# #\.
	#'(lambda (stream ch arg)
		(let* ((n (read stream t nil t)))
			(unless *read-suppress* (eval n)))))

;;;; This currently causes read-time evaluation, but needs to be
;;;; modified for load-time evaluation when we add support for
;;;; COMPILE-FILE.
(set-dispatch-macro-character #\# #\,
	#'(lambda (stream ch arg)
		(let* ((n (read stream t nil t)))
			(eval n))))

(defvar *named-characters* 
	(list 
		:Space (int-char 32) 
		:Tab (int-char 9) 
		:Newline (int-char 10)
		:Nul (int-char 0)))

;;;
;;;	This gets redefined later.
;;;
(defun find-named-character (sym)
	(do ((i *named-characters* (cddr i)))
		((or (null i) (string= (symbol-name (car i))(symbol-name sym)))
		 (cadr i))))

(set-dispatch-macro-character #\# #\\ 
	#'(lambda (stream ch arg)
		(declare (ignore arg))
		(unread-char ch stream)							; back up to backslash
		(let* ((*readtable* *common-lisp-readtable*)	; ensure standard handling of backslash (escape)
			   (sym (read stream t nil t))
			   (name (symbol-name sym)))
            (unless *read-suppress*
      			(if (> (length name) 1)
    				(let ((char (find-named-character (make-symbol (string-upcase sym)))))
    					(if (null char)
    						(error "Could not find a character associated with the name ~A" (string-upcase sym))
    						char))
    				(char name 0))))))

(set-dispatch-macro-character #\# #\:
	#'(lambda (stream ch arg)
		(let* ((sym (read-expression stream t nil t nil)))
			(make-symbol (symbol-name sym)))))
;; faa20001128b -	End change.

;(setq *readtable* (copy-readtable *common-lisp-readtable*))

(defun white-space-char (c) 
	(eq (readtable-char-type *readtable* c) 'whitespace-char-type))

(defun constituent-char (c) 
	(eq (readtable-char-type *readtable* c) 'constituent-char-type))

(defun read-char-skip-white-space (stream)
	(do ((c (%read-char stream)(%read-char stream)))
		((or (null c)(not (white-space-char c))) c)))

(defun reverse-dotted-list (list cdr)
	(dolist (x list)
		(push x cdr))
	cdr)

(defun read-delimited-list (char &optional (stream *standard-input*) recursive-p)
	(declare (ignore recursive-p))
    (cond ((eq stream t) (setq stream *terminal-io*))
          ((eq stream nil) (setq stream *standard-input*))
          ((not (streamp stream)) 
           (error "Stream argument to READ-DELIMITED-LIST is not an input stream: ~S." stream)))
    (unless (input-character-stream-p stream)
        (error "Expected an input character stream, got ~A" stream))
	(if (null cormanlisp::*source-line*)
		(setq cormanlisp::*source-line* (uref stream stream-line-number-offset)))
	(do ((list nil)
		 (item nil)
		 c
		 found-dot
		 (found-cdr nil)
		 (list-cdr nil)
         (startpos nil))
		((eq c char)(if found-cdr (reverse-dotted-list list list-cdr)(nreverse list)))
		(setq c (read-char-skip-white-space stream))
		(if (null c)
			(error "End-of-file encountered"))
		(if (eq c char)
			(if (and found-dot (null found-cdr))
				(error "The list ends with a dot: ~A" (nreverse list)))
			(progn
				(unread-char c stream)
                (setq startpos (stream-position stream))
				(setq item (multiple-value-list (read-expression stream t nil t)))
                (if *read-hook*
                    (funcall *read-hook* stream startpos 
                        (stream-position stream) (car item) (if item nil t)))
				(if item
					(if (eq (car item) dot-marker)
						(if found-dot 
							(error "The list contains two dots: ~A" (nreverse list))
							(setq found-dot t))
						(if found-dot
							(setq list-cdr (car item) found-cdr t)
							(push (car item) list))))))))

(set-macro-character #\( #'(lambda (stream ch) (read-delimited-list #\) stream)))
(set-macro-character #\) #'(lambda (stream ch) '|)| ))
(set-macro-character #\' #'(lambda (stream ch) `(quote ,(read stream t nil t))))

(defconstant ascii-newline-char (int-char 10))
(defconstant ascii-return-char  (int-char 13))

(set-macro-character #\; 
	#'(lambda (stream ch)
		(declare (ignore ch))
		(do ((c (%read-char stream) (%read-char stream)))
			(nil)
			(when (or (eq c ascii-newline-char) (eq c ascii-return-char))
				(unread-char c stream)
				(return))
			(if (null c)
				(return)))
		(values)))
		
;
;	Set up the reader macro which allows for #+ and #- conditional reads
;

;; need to override warning here
(setq *COMPILER-WARN-ON-UNDEFINED-FUNCTION* nil)
(defun %features-member (feature-list)
	(if (symbolp feature-list)
		(return-from %features-member (member feature-list *features*)))
	(if (consp feature-list)
		(cond
			((eq (car feature-list) :and) 
			 (every #'%features-member (cdr feature-list)))
			((eq (car feature-list) :or)  
			 (some #'%features-member (cdr feature-list)))	
			((eq (car feature-list) :not) 
			 (notany #'%features-member (cdr feature-list)))
			(t (error "~A is not a valid feature" feature-list)))
		(error "~A is not a valid feature" feature-list)))

(setq *COMPILER-WARN-ON-UNDEFINED-FUNCTION* t)

(set-dispatch-macro-character #\# #\+ 
	#'(lambda (stream char int)
		(declare (ignore char int))
		(let ((feature nil))
			(let* ((*package* (find-package :keyword)))	;default to keyword package
				(setf feature (read stream t nil t)))
			(if (%features-member feature)
				(read stream t nil t)

    			; else need to skip over the next expression
    			(let ((*read-suppress* t))
    				  (read stream t nil t)
                    (values))))))

(set-dispatch-macro-character #\# #\- 
	#'(lambda (stream char int)
		(declare (ignore char int))
		(let ((feature nil))
			(let* ((*package* (find-package :keyword)))	;default to keyword package
				(setf feature (read stream t nil t)))
			(if (not (%features-member feature))
				(read stream t nil t)

    			; else need to skip over the next expression
                (let ((*read-suppress* t))
    				  (read stream t nil t)
                    (values))))))

(set-dispatch-macro-character #\# #\< 
	#'(lambda (stream ch arg)
		(do ((c (read-char stream t nil t)(read-char stream t nil t))
			 (chars (list #\< #\#)))
			((char= c #\>)(error "Attempted to read unreadable object: ~A"
							(coerce (nreverse (push c chars)) 'string)))
			(push c chars))))

(set-dispatch-macro-character #\# #\* 
	#'(lambda (stream char int)
		(declare (ignore char))
		(let ((bit-list nil))
			(do ((c (read-char stream nil nil t) (read-char stream nil nil t)))
				((or (null c) (not (or (char= c #\0) (char= c #\1))))
				 (if c
					 (if (constituent-char c)
						(error "Invalid bit vector specifier, contained the digit ~S" c)
					 	(unread-char c stream)))
				 (let ((bits (length bit-list)))
					(if (null int)
						(setq int bits))
					(if (> bits int)
						(error "Too many initializers for bit array of length ~D" int))
					(if (and (= bits 0) (> int 0))
						(error "Not enough initializers for bit array of length ~D" int))
					(let ((last-bit (car bit-list)))
						(dotimes (i (- int bits))
							(push last-bit bit-list))))
				 (make-array int 
					:element-type 'bit 
					:initial-contents (nreverse bit-list)))
				(push (if (char= c #\0) 0 1) bit-list))))) 

(set-dispatch-macro-character #\# #\' 
	#'(lambda (stream char int)
		(let ((ret (read stream t nil t)))
			`(function ,ret))))

(defun set-read-eq-forms (int form)
	(setf (getf *read-eq-forms* int) form))

(set-dispatch-macro-character #\# #\= 
	#'(lambda (stream char int)
		(let ((ret (read stream t nil t)))
			(set-read-eq-forms int ret)
			ret)))

(set-dispatch-macro-character #\# #\# 
	#'(lambda (stream char int)
		(make-eq-form-placeholder int)))

(set-macro-character #\"
  #'(lambda (stream ch)
	  (declare (ignore ch))
	  (let ((chars nil)
			(eof (cons nil nil)))
		(flet ((get-char ()
				 (let ((ch (read-char stream nil eof t)))
				   (when (eq ch eof)
					 (error "Unterminated string reading: ~S"
							stream))
				   ch)))
		  (do ((ch (get-char) (get-char))
			   (count 0 (1+ count)))
			  ((char= ch #\")
			   (make-array count
						   :element-type 'character
						   :initial-contents (nreverse chars)))
			(push (if (char= ch #\\) (get-char) ch)
				  chars))))))

;; need to override warning here
(setq *COMPILER-WARN-ON-UNDEFINED-FUNCTION* nil)
(set-dispatch-macro-character #\# #\P 
	#'(lambda (stream char int)
		(unless (char= (read-char stream) #\")
			(error "Invalid pathname specification"))
		(let ((char-list nil))
			(do ((c (read-char stream) (read-char stream)))
				((char= c #\")
				 (values (parse-namestring (make-array (length char-list) 
					:element-type 'character 
					:initial-contents (nreverse char-list)))))
				(push c char-list))))) 
(setq *COMPILER-WARN-ON-UNDEFINED-FUNCTION* t)


