;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		characters.lisp
;;;;	Contents:	Character functions.
;;;;	History:	3/4/97  RGC  Created.
;;;;               10/17/01 RGC Added PAGE, LINEFEED characters.
;;;;               12/18/02 RGC Added implementation of CHARACTER function.
;;;;                            Added some compiler macros to speed up character processing.
;;;;                            Added implementation of DIGIT-CHAR function.
;;;;                            Added implementation of BOTH-CASE-P function.
;;;;

(in-package :common-lisp)
(make-package "CHARACTER-NAMES" :nicknames nil :use nil)
(defconstant char-code-limit 256)
(defconstant character-names-table (make-array char-code-limit :initial-element nil))

(defun sys::check-character (c) 
	(unless (characterp c)
		(error "Not a character: ~A" c))
	c)

(defmacro defcharname (name char)
	`(progn
		(sys::check-character ,char)
		(set (intern ,name :character-names) ,char)
		(setf (elt character-names-table (char-int ,char)) ,name)))

(defcharname "NUL" 					(int-char 0))
(defcharname "NULL" 				(int-char 0))
(defcharname "CONTROL-A" 			(int-char 1))
(defcharname "CONTROL-B" 			(int-char 2))
(defcharname "CONTROL-C" 			(int-char 3))
(defcharname "CONTROL-D" 			(int-char 4))
(defcharname "CONTROL-E" 			(int-char 5))
(defcharname "CONTROL-F" 			(int-char 6))
(defcharname "CONTROL-G" 			(int-char 7))
(defcharname "CONTROL-H" 			(int-char 8))
(defcharname "CONTROL-I" 			(int-char 9))
(defcharname "CONTROL-J" 			(int-char 10))
(defcharname "CONTROL-K" 			(int-char 11))
(defcharname "CONTROL-L" 			(int-char 12))
(defcharname "CONTROL-M" 			(int-char 13))
(defcharname "CONTROL-N" 			(int-char 14))
(defcharname "CONTROL-O" 			(int-char 15))
(defcharname "CONTROL-P" 			(int-char 16))
(defcharname "CONTROL-Q" 			(int-char 17))
(defcharname "CONTROL-R" 			(int-char 18))
(defcharname "CONTROL-S" 			(int-char 19))
(defcharname "CONTROL-T" 			(int-char 20))
(defcharname "CONTROL-U" 			(int-char 21))
(defcharname "CONTROL-V" 			(int-char 22))
(defcharname "CONTROL-W" 			(int-char 23))
(defcharname "CONTROL-X" 			(int-char 24))
(defcharname "CONTROL-Y" 			(int-char 25))
(defcharname "CONTROL-Z" 			(int-char 26))

(defcharname "^A" 					(int-char 1))
(defcharname "^B" 					(int-char 2))
(defcharname "^C" 					(int-char 3))
(defcharname "^D" 					(int-char 4))
(defcharname "^E" 					(int-char 5))
(defcharname "^F" 					(int-char 6))
(defcharname "^G" 					(int-char 7))
(defcharname "^H" 					(int-char 8))
(defcharname "^I" 					(int-char 9))
(defcharname "^J" 					(int-char 10))
(defcharname "^K" 					(int-char 11))
(defcharname "^L" 					(int-char 12))
(defcharname "^M" 					(int-char 13))
(defcharname "^N" 					(int-char 14))
(defcharname "^O" 					(int-char 15))
(defcharname "^P" 					(int-char 16))
(defcharname "^Q" 					(int-char 17))
(defcharname "^R" 					(int-char 18))
(defcharname "^S" 					(int-char 19))
(defcharname "^T" 					(int-char 20))
(defcharname "^U" 					(int-char 21))
(defcharname "^V" 					(int-char 22))
(defcharname "^W" 					(int-char 23))
(defcharname "^X" 					(int-char 24))
(defcharname "^Y" 					(int-char 25))
(defcharname "^Z" 					(int-char 26))

(defcharname "BACKSPACE" 			(int-char 8))
(defcharname "TAB" 					(int-char 9))
(defcharname "LINEFEED" 			(int-char 10))
(defcharname "NEWLINE" 				(int-char 10))	; second one specified will be used by writer
(defcharname "FORM-FEED" 			(int-char 12))
(defcharname "PAGE" 			    (int-char 12))  ; second one specified will be used by writer
(defcharname "RETURN" 				(int-char 13))
(defcharname "SPACE" 				(int-char 32))
(defcharname "ESCAPE" 				(int-char 27))
(defcharname "LEFT-ARROW" 			(int-char 28))
(defcharname "RIGHT-ARROW" 			(int-char 29))
(defcharname "UP-ARROW" 			(int-char 30))
(defcharname "DOWN-ARROW" 			(int-char 31))
(defcharname "RUBOUT" 				(int-char 127))
;; etc.

(defun output-readable-char (ch stream)
	(declare (ignore stream))
	(format t "#\\~A" 
		(let ((name (elt character-names-table (char-int ch))))
			(if name name ch))))

(defun find-named-character (sym)
	(let ((char-sym (find-symbol (symbol-name sym) :character-names)))
		(if (and char-sym (boundp char-sym))(symbol-value char-sym) nil)))
;;;
;;; Common Lisp CHAR-NAME function.
;;;
(defun char-name (char)
	(let ((name (elt character-names-table (char-int char))))
			(if name name)))

;;;
;;; Common Lisp NAME-CHAR function.
;;;
(defun name-char (obj)
    (if (symbolp obj)
        (setf obj (symbol-name obj))
        (if (characterp obj)
            (setf obj (string obj))))
    (unless (stringp obj)
        (signal-type-error obj '(or string symbol character)))
    (let ((char-sym (find-symbol obj :character-names)))
        (if (and char-sym (boundp char-sym))
            (symbol-value char-sym))))
	
;;;
;;;	Common Lisp char= function.
;;;
(defun char= (&rest characters)
	(unless characters (return-from char= t))
	(let ((c (sys::check-character (car characters))))
		(dolist (x (cdr characters))
 			(unless (eq c (sys::check-character x)) 
				(return-from char= nil)))
		t))
		
;;;
;;;	Common Lisp char/= function.
;;;	TD: This is not completely implemented when there are more than
;;;	two arguments i.e. (char/= #\1 #\2 #\2) -> t
;;;
(defun char/= (&rest characters)
	(unless characters (return-from char/= t))
	(let ((c (sys::check-character (car characters))))
		(dolist (x (cdr characters))
 			(unless (not (eq c (sys::check-character x))) 
				(return-from char/= nil)))
		t))

;;;
;;;	Common Lisp char< function.
;;;
(defun char< (&rest characters)
	(unless characters (return-from char< t))
	(let ((c (char-int (car characters))))
		(dolist (x (cdr characters))
 			(unless (< c (char-int x)) 
				(return-from char< nil))
			(setf c (char-int x)))
		t))

;;;
;;;	Common Lisp char> function.
;;;
(defun char> (&rest characters)
	(unless characters (return-from char> t))
	(let ((c (char-int (car characters))))
		(dolist (x (cdr characters))
 			(unless (> c (char-int x)) 
				(return-from char> nil))
			(setf c (char-int x)))
		t))

;;;
;;;	Common Lisp char<= function.
;;;
(defun char<= (&rest characters)
	(unless characters (return-from char<= t))
	(let ((c (char-int (car characters))))
		(dolist (x (cdr characters))
 			(unless (<= c (char-int x)) 
				(return-from char<= nil))
			(setf c (char-int x)))
		t))

;;;
;;;	Common Lisp char>= function.
;;;
(defun char>= (&rest characters)
	(unless characters (return-from char>= t))
	(let ((c (char-int (car characters))))
		(dolist (x (cdr characters))
 			(unless (>= c (char-int x)) 
				(return-from char>= nil))
			(setf c (char-int x)))
		t))

;;;
;;;	Common Lisp char-equal function.
;;;
(defun char-equal (&rest characters)
	(unless characters (return-from char-equal t))
	(let ((c (char-upcase (car characters))))
		(dolist (x (cdr characters))
 			(unless (eq c (char-upcase x))
				(return-from char-equal nil)))
		t))
		
;;;
;;;	Common Lisp char-not-equal function.
;;;
(defun char-not-equal (&rest characters)
	(unless characters (return-from char-not-equal t))
	(let ((c (char-upcase (car characters))))
		(dolist (x (cdr characters))
 			(unless (not (eq c (char-upcase x)))
				(return-from char-not-equal nil)))
		t))

;;;
;;;	Common Lisp char-lessp function.
;;;
(defun char-lessp (&rest characters)
	(unless characters (return-from char-lessp t))
	(let ((c (char-int (char-upcase (car characters)))))
		(dolist (x (cdr characters))
 			(unless (< c (char-int (char-upcase x))) 
				(return-from char-lessp nil))
			(setf c (char-int (char-upcase x))))
		t))

;;;
;;;	Common Lisp char-greaterp function.
;;;
(defun char-greaterp (&rest characters)
	(unless characters (return-from char-greaterp t))
	(let ((c (char-int (char-upcase (car characters)))))
		(dolist (x (cdr characters))
 			(unless (> c (char-int (char-upcase x))) 
				(return-from char-greaterp nil))
			(setf c (char-int (char-upcase x))))
		t))

;;;
;;;	Common Lisp char-not-lessp function.
;;;
(defun char-not-lessp (&rest characters)
	(unless characters (return-from char-not-lessp t))
	(let ((c (char-int (char-upcase (car characters)))))
		(dolist (x (cdr characters))
 			(unless (>= c (char-int (char-upcase x))) 
				(return-from char-not-lessp nil))
			(setf c (char-int (char-upcase x))))
		t))

;;;
;;;	Common Lisp char-not-greaterp function.
;;;
(defun char-not-greaterp (&rest characters)
	(unless characters (return-from char-not-greaterp t))
	(let ((c (char-int (char-upcase (car characters)))))
		(dolist (x (cdr characters))
 			(unless (<= c (char-int (char-upcase x))) 
				(return-from char-not-greaterp nil))
			(setf c (char-int (char-upcase x))))
		t))

;;;
;;;	Common Lisp GRAPHIC-CHAR-P function.
;;;
(defun graphic-char-p (ch)
	(if (member ch '(#\Space #\Newline #\Return #\Backspace #\Tab
		#\Escape #\Left-arrow #\Right-arrow #\Up-arrow #\Down-arrow))
		nil
		t))

;;;
;;;	Common Lisp (SETF CHAR) function.
;;;
(defun (setf char) (char string index)
	(setf (elt string index) char))

;;;
;;;	Common Lisp SCHAR function.
;;;
(defun schar (string index) (elt string index))

;;;
;;;	Common Lisp (SETF SCHAR) function.
;;;
(defun (setf schar) (char string index)
	(setf (elt string index) char))

;;;
;;;	Common Lisp CODE-CHAR function.
;;;
(defun code-char (code) (int-char code))

;;;
;;; Common Lisp CHARACTER function.
;;;
(defun character (c)
    (if (characterp c)
        c
        (if (and (stringp c) (= (length c) 1))
            (aref c 0)
            (if (symbolp c)
                (let ((name (symbol-name c)))
                    (if (= (length name) 1)
                        (aref name 0)
                        (error "The object ~S cannot be coerced to a character" c)))
                (error "The object ~S cannot be coerced to a character" c)))))

;;;
;;; Common Lisp DIGIT-CHAR function.
;;;
(defun digit-char (weight &optional (radix 10))
    (unless (and (integerp radix) (>= radix 2) (<= radix 36))
        (error "Illegal radix specified for DIGIT-CHAR: ~S" radix))
    (unless (and (integerp weight) (>= weight 0))
        (error "Illegal weight specified for DIGIT-CHAR: ~S" weight))
    (if (< weight radix)
        (if (< weight 10)
            (int-char (+ (char-int #\0) weight))
            (int-char (+ (char-int #\A) (- weight 10))))))
 
;;;
;;; Common Lisp BOTH-CASE-P function.
;;;
(defun both-case-p (x) (or (lower-case-p x) (upper-case-p x)))
   
(defun sys::invalid-char (x) (error "The object ~A is not a character" x))

;;;
;;; Define some compiler macros for better performance.
;;;
(define-compiler-macro sys::check-character (form)
    (if (>= ccl:*COMPILER-OPTIMIZE-SPEED* ccl:*COMPILER-OPTIMIZE-SAFETY*)
        form
        (let ((tempsym (gensym)))
            `(let ((,tempsym ,form))
                 (unless (characterp ,tempsym) 
                    (sys::invalid-char ,tempsym))
                ,tempsym))))

(define-compiler-macro char= (&rest chars)
    (cond ((null chars)
            (warn "Function CHAR= called with no arguments, which will cause a runtime error")
            `(signal-program-error "CHAR= called with no arguments"))
          ((null (cdr chars))
           (if (characterp (car chars))
               't
               `(progn (sys::check-character ,(car chars)) 't)))
          (t
            (let ((expr 
                        `(eq ,(if (characterp (car chars)) 
                                (car chars) 
                                `(sys::check-character ,(car chars)))
                             ,(if (characterp (cadr chars)) 
                                (cadr chars) 
                                `(sys::check-character ,(cadr chars))))))
                (do ((x (cddr chars) (cdr x)))
                    ((null x))
                    (setf expr 
                        `(eq ,expr ,(if (characterp (car x)) 
                                (car x) 
                                `(sys::check-character ,(car x))))))
                expr))))

