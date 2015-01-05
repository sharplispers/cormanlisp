;;; Thu Oct 14 15:08:52 1993 by Mark Kantrowitz <mkant@tok>
;;; infix.lisp -- 37548 bytes

;;; **************************************************************************
;;; Infix ********************************************************************
;;; **************************************************************************
;;;
;;; This is an implementation of an infix reader macro. It should run in any
;;; valid Common Lisp and has been tested in Allegro CL 4.1, Lucid CL 4.0.1,
;;; MCL 2.0 and CMU CL. It allows the user to type arithmetic expressions in
;;; the traditional way (e.g., 1+2) when writing Lisp programs instead of
;;; using the normal Lisp syntax (e.g., (+ 1 2)).  It is not intended to be a
;;; full replacement for the normal Lisp syntax. If you want a more complete
;;; alternate syntax for Lisp, get a copy Apple's MLisp or Pratt's CGOL.
;;;
;;; Although similar in concept to the Symbolics infix reader (#<DIAMOND>), 
;;; no real effort has been made to ensure compatibility beyond coverage 
;;; of at least the same set of basic arithmetic operators. There are several 
;;; differences in the syntax beyond just the choice of #I as the macro 
;;; character. (Our syntax is a little bit more C-like than the Symbolics 
;;; macro in addition to some more subtle differences.) 
;;;
;;; We initially chose $ as a macro character because of its association
;;; with mathematics in LaTeX, but unfortunately that character is already
;;; used in MCL. We switched to #I() because it was one of the few options
;;; remaining.
;;;
;;; Written by Mark Kantrowitz, School of Computer Science,
;;; Carnegie Mellon University, March 1993.
;;;
;;; Copyright (c) 1993 by Mark Kantrowitz. All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted, so long as the following
;;; conditions are met:
;;;      o no fees or compensation are charged for use, copies, 
;;;        distribution or access to this software
;;;      o this copyright notice is included intact.
;;; This software is made available AS IS, and no warranty is made about
;;; the software or its performance.
;;;
;;; In no event will the author(s) or their institutions be liable to you for
;;; damages, including lost profits, lost monies, or other special, incidental
;;; or consequential damages, arising out of or in connection with the use or
;;; inability to use (including but not limited to loss of data or data being
;;; rendered inaccurate or losses sustained by third parties or a failure of
;;; the program to operate as documented) the program, or for any claim by
;;; any other party, whether in an action of contract, negligence, or 
;;; other tortious action.
;;;
;;; Please send bug reports, comments and suggestions to mkant@cs.cmu.edu.
;;;
;;; The current version of this software and a variety of related utilities
;;; may be obtained from the Lisp Utilities Repository by anonymous ftp
;;; from ftp.cs.cmu.edu [128.2.206.173] in the directory
;;;     /afs/cs.cmu.edu/user/mkant/Public/Lisp/
;;; You must cd to this directory in one atomic operation, as some of
;;; the superior directories on the path are protected from access by
;;; anonymous ftp. If your site runs the Andrew File System, you can just
;;; access the files directly without bothering with FTP.
;;;
;;; If you wish to be added to the Lisp-Utilities@cs.cmu.edu mailing list,
;;; send email to Lisp-Utilities-Request@cs.cmu.edu with your name, email
;;; address, and affiliation. This mailing list is primarily for
;;; notification about major updates, bug fixes, and additions to the Lisp
;;; Utilities Repository. The mailing list is intended to have low traffic.
;;;

;;; ********************************
;;; Documentation ******************
;;; ********************************
;;;
;;; Syntax:
;;;
;;;   Begin the reader macro with #I( and end it with ). For example,
;;;      #I( x^^2 + y^^2 )
;;;   is equivalent to the Lisp form
;;;      (+ (expt x 2) (expt y 2))
;;;   but much easier to read according to some folks.
;;;
;;;   If you want to see the expansion, type a quote before the #I form
;;;   at the Lisp prompt:
;;;     > '#I(if x<y<=z then f(x)=x^^2+y^^2 else f(x)=x^^2-y^^2)
;;;     (IF (AND (< X Y) (<= Y Z))
;;;         (SETF (F X) (+ (EXPT X 2) (EXPT Y 2)))
;;;         (SETF (F X) (- (EXPT X 2) (EXPT Y 2))))
;;;
;;;
;;; Operators:
;;;
;;;    NOTE: == is equality, = is assignment (C-style).
;;;
;;;     \                   quoting character:  x\-y  -->  x-y
;;;     !                   lisp escape    !(foo bar) -->  (foo bar)
;;;     ;                   comment
;;;     x = y               assignment                     (setf x y)
;;;     x += y              increment                      (incf x y)
;;;     x -= y              decrement                      (decf x y)
;;;     x *= y              multiply and store             (setf x (* x y))
;;;     x /= y              divide and store               (setf x (/ x y))
;;;     x|y                 bitwise logical inclusive or   (logior x y)
;;;     x^y                 bitwise logical exclusive or   (logxor x y)
;;;     x&y                 bitwise logical and            (logand x y)
;;;     x<<y                left shift                     (ash x y)
;;;     x>>y                right shift                    (ash x (- y))
;;;     ~x                  ones complement (unary)        (lognot x)
;;;     x and y             conjunction                    (and x y)
;;;     x && y              conjunction                    (and x y)
;;;     x or y              disjunction                    (or x y)
;;;     x || y              disjunction                    (or x y)
;;;     not x               negation                       (not x)
;;;     x^^y                exponentiation                 (expt x y)
;;;     x,y                 sequence                       (progn x y)
;;;     (x,y)               sequence                       (progn x y)
;;;                         also parenthesis (x+y)/z -->   (/ (+ x y) z)
;;;     f(x,y)              functions                      (f x y)
;;;     a[i,j]              array reference                (aref a i j)
;;;     x+y x*y             arithmetic                     (+ x y) (* x y) 
;;;     x-y x/y             arithmetic                     (- x y) (/ x y) 
;;;     -y                  value negation                 (- y)
;;;     x % y               remainder                      (mod x y)
;;;     x<y x>y             inequalities                   (< x y) (> x y)
;;;     x <= y  x >= y      inequalities                   (<= x y) (>= x y)
;;;     x == y              equality                       (= x y) 
;;;     x != y              equality                       (not (= x y))
;;;     if p then q         conditional                    (when p q)
;;;     if p then q else r  conditional                    (if p q r) 
;;;

;;; Precedence:
;;;
;;;    The following precedence conventions are obeyed by the infix operators:
;;;      [ ( !
;;;      ^^
;;;      ~
;;;      * / %
;;;      + -
;;;      << >>
;;;      < == > <= != >=
;;;      &
;;;      ^
;;;      |
;;;      not
;;;      and
;;;      or
;;;      = += -= *= /=
;;;      , 
;;;      if
;;;      then else
;;;      ] )
;;;
;;;    Note that logical negation has lower precedence than numeric comparison
;;;    so that "not a<b" becomes (not (< a b)), which is different from the
;;;    C precedence conventions. You can change the precedence conventions by
;;;    modifying the value of the variable *operator-ordering*.
;;;

;;; ********************************
;;; To Do **************************
;;; ********************************
;;;
;;;    Write some more test cases.
;;;    Write some more syntactic optimizations.
;;;    Would really like ~x to be (not x), but need it for (lognot x). 

;;; ********************************
;;; Change Log *********************
;;; ********************************
;;;
;;;  9-MAR-93 mk    Created
;;; 12-MAR-93 mk    Fixed defpackage form for Lucid.
;;; 14-OCT-93 mk    Changed macro character from #$ to #I(). Suggested by
;;;                 Scott McKay.


;;; ********************************
;;; Implementation Notes ***********
;;; ********************************
;;;
;;; Initially we tried implementing everything within the Lisp reader,
;;; but found this to not be workable. Parameters had to be passed in
;;; global variables, and some of the processing turned out to be 
;;; indelible, so it wasn't possible to use any kind of lookahead.
;;; Center-embedded constructions were also a problem, due to the lack
;;; of an explicit stack.
;;;
;;; So we took another tack, that used below. The #I macro binds the
;;; *readtable* to a special readtable, which is used solely for tokenization
;;; of the input. Then the problem is how to correctly parenthesize the input.
;;; We do that with what is essentially a recursive-descent parser. An 
;;; expression is either a prefix operator followed by an expression, or an 
;;; expression followed by an infix operator followed by an expression. When 
;;; the latter expression is complex, the problem becomes a little tricky. 
;;; For example, suppose we have
;;;      exp1 op1 exp2 op2
;;; We need to know whether to parenthesize it as
;;;      (exp1 op1 exp2) op2
;;; or as
;;;      exp1 op1 (exp2 op2 ...)
;;; The second case occurs either when op2 has precedence over op1 (e.g.,
;;; * has precedence over +) or op2 and op1 are the same right-associative
;;; operator (e.g., exponentiation). Thus the algorithm is as follows:
;;; When we see op1, we want to gobble up exp2 op2 exp3 op3 ... opn expn+1
;;; into an expression where op2 through opn all have higher precedence
;;; than op1 (or are the same right-associative operator), and opn+1 doesn't.
;;; This algorithm is implemented by the GATHER-SUPERIORS function. 
;;; 
;;; Because + and - are implemented in the infix readtable as terminating
;;; macro cahracters, the exponentiation version of Lisp number syntax
;;;    1e-3 == 0.001
;;; doesn't work correctly -- it parses it as (- 1e 3). So we add a little
;;; cleverness to GATHER-SUPERIORS to detect when the tokenizer goofed. 
;;; Since this requires the ability to lookahead two tokens, we use a
;;; stack to implement the lookahead in PEEK-TOKEN and READ-TOKEN.
;;;
;;; Finally, the expression returned by GATHER-SUPERIORS sometimes needs to
;;; be cleaned up a bit. For example, parsing a<b<c would normally return
;;; (< (< a b) c), which obviously isn't correct. So POST-PROCESS-EXPRESSION
;;; detects this and similar cases, replacing the expression with (< a b c).
;;; For cases like a<b<=c, it replaces it with (and (< a b) (<= b c)).
;;; 

;;; ********************************
;;; Package Cruft ******************
;;; ********************************

(defpackage "INFIX" (:use #-:lucid "COMMON-LISP" 
			  #+:lucid "LISP" #+:lucid "LUCID-COMMON-LISP"))
(in-package "INFIX")
(export 'test-infix)

(pushnew :infix *features*)

(eval-when (compile load eval)
  (defparameter *version* "1.1  14-OCT-93")

  (defun infix-copyright (&optional (stream *standard-output*))
    "Prints an INFIX copyright notice and header upon startup."
    (format stream "~%;;; ~V,,,'*A" 73 "*")
    (format stream "~%;;;   Infix notation for Common Lisp.")
    (format stream "~%;;;   Version ~A." *version*)
    (format stream "~%;;;   Written by Mark Kantrowitz, ~
                            CMU School of Computer Science.")
    (format stream "~%;;;   Copyright (c) 1993. All rights reserved.")
    (format stream "~%;;;   May be freely redistributed, provided this ~
                            notice is left intact.")
    (format stream "~%;;;   This software is made available AS IS, without ~
                            any warranty.")
    (format stream "~%;;; ~V,,,'*A~%" 73 "*")
    (force-output stream))

  (infix-copyright))

;;; ********************************
;;; Readtable **********************
;;; ********************************

(defparameter *infix-readtable* (copy-readtable nil))
(defparameter *normal-readtable* (copy-readtable nil))

(defun infix-reader (stream subchar arg)
  (declare (ignore arg subchar))
  (let ((*readtable* *infix-readtable*)
        (*normal-readtable* *readtable*))
    (read-char stream) ; get rid of opening left parenthesis
    (read-infix stream)))
(set-dispatch-macro-character #\# #\I #'infix-reader *readtable*) ; was #\# #\$

(defmacro infix-error (format-string &rest args)
  `(error ,format-string ,@args))

(defun read-infix (stream)
  (let* ((result (gather-superiors '\) stream)) ; %infix-end-token%
	 (next-token (read-token stream)))
    (unless (same-token-p next-token '\)) ; %infix-end-token%
      (infix-error "Infix expression ends with ~A." next-token))
    result))

(defun read-regular (stream)
  (let ((*readtable* *normal-readtable*))
    (read stream t nil t)))


;;; ********************************
;;; Reader Code ********************
;;; ********************************

(defun same-operator-p (x y)
  (same-token-p x y))

(defun same-token-p (x y)
  (and (symbolp x)
       (symbolp y) 
       (string-equal (symbol-name x) (symbol-name y))))

;;; Peeking Token Reader

(defvar *peeked-token* nil)
(defun read-token (stream)
  (if *peeked-token*
      (pop *peeked-token*)
      (read stream t nil t)))
(defun peek-token (stream)
  (unless *peeked-token*
    (push (read stream t nil t) *peeked-token*))
  (car *peeked-token*))

;;; Hack to work around + and - being terminating macro characters,
;;; so 1e-3 doesn't normally work correctly.

(defun fancy-number-format-p (left operator stream)
  (when (and (symbolp left)
	     (find operator '(+ -) :test #'same-operator-p))
    (let* ((name (symbol-name left))
	   (length (length name)))
      (when (and (valid-numberp (subseq name 0 (1- length)))
		 ;; Exponent, Single, Double, Float, or Long
		 (find (subseq name (1- length))
		       '("e" "s" "d" "f" "l")
		       :test #'string-equal))
	(read-token stream)
	(let ((right (peek-token stream)))
	  (cond ((integerp right)
		 ;; it is one of the fancy numbers, so return it
		 (read-token stream)
		 (let ((*readtable* *normal-readtable*))
		   (read-from-string (format nil "~A~A~A" 
					     left operator right))))
		(t
		 ;; it isn't one of the fancy numbers, so unread the token
		 (push operator *peeked-token*)
		 ;; and return nil
		 nil)))))))

(defun valid-numberp (string)
  (let ((saw-dot nil))
    (dolist (char (coerce string 'list) t)
      (cond ((char= char #\.)
	     (if saw-dot
		 (return nil)
		 (setq saw-dot t)))
	    ((not (find char "01234567890" :test #'char=))
	     (return nil))))))

;;; Gobbles an expression from the stream.

(defun gather-superiors (previous-operator stream)
  "Gathers an expression whose operators all exceed the precedence of
   the operator to the left."
  (let ((left (get-first-token stream)))
    (loop
      (setq left (post-process-expression left))
      (let ((peeked-token (peek-token stream)))
	(let ((fancy-p (fancy-number-format-p left peeked-token stream)))
	  (when fancy-p
	    ;; i.e., we've got a number like 1e-3 or 1e+3 or 1f-1
	    (setq left fancy-p
		  peeked-token (peek-token stream))))
	(unless (or (operator-lessp previous-operator peeked-token)
		    (and (same-operator-p peeked-token previous-operator)
			 (operator-right-associative-p previous-operator)))
	  ;; The loop should continue when the peeked operator is
	  ;; either superior in precedence to the previous operator,
	  ;; or the same operator and right-associative.
	  (return left)))
      (setq left (get-next-token stream left)))))

(defun get-first-token (stream)
  (let ((token (read-token stream)))
    (if (token-operator-p token)
	;; It's an operator in a prefix context.
	(apply-token-prefix-operator token stream)
	;; It's a regular token
	token)))

(defun apply-token-prefix-operator (token stream)
  (let ((operator (get-token-prefix-operator token)))
    (if operator
	(funcall operator stream)
	(infix-error "~A is not a prefix operator" token))))

(defun get-next-token (stream left)
  (let ((token (read-token stream)))
    (apply-token-infix-operator token left stream)))

(defun apply-token-infix-operator (token left stream)
  (let ((operator (get-token-infix-operator token)))
    (if operator
	(funcall operator stream left)
	(infix-error "~A is not an infix operator" token))))

;;; Fix to read-delimited-list so that it works with tokens, not
;;; characters.

(defun infix-read-delimited-list (end-token delimiter-token stream)
  (do ((next-token (peek-token stream) (peek-token stream))
       (list nil))
      ((same-token-p next-token end-token)
       ;; We've hit the end. Remove the end-token from the stream.
       (read-token stream)
       ;; and return the list of tokens.
       ;; Note that this does the right thing with [] and ().
       (nreverse list))
    ;; Ignore the delimiters.
    (when (same-token-p next-token delimiter-token)
      (read-token stream))
    ;; Gather the expression until the next delimiter.
    (push (gather-superiors delimiter-token stream) list)))


;;; ********************************
;;; Precedence *********************
;;; ********************************

(defparameter *operator-ordering* 
    '(( \[ \( \! )			; \[ is array reference
      ( ^^ )				; exponentiation
      ( ~ )				; lognot 
      ( * /  % )			; % is mod
      ( + - )
      ( << >> )
      ( < == > <= != >= )
      ( & )				; logand
      ( ^ )				; logxor
      ( \| )				; logior
      ( not )
      ( and )
      ( or )
      ;; Where should setf and friends go in the precedence?
      ( = += -= *= /= )
      ( \, )				; progn (statement delimiter)
      ( if )
      ( then else )
      ( \] \) )
      ( %infix-end-token% ))		; end of infix expression
  "Ordered list of operators of equal precedence.")

(defun operator-lessp (op1 op2)
  (dolist (ops *operator-ordering* nil)
    (cond ((find op1 ops :test #'same-token-p)
	   (return nil))
	  ((find op2 ops :test #'same-token-p)
	   (return t)))))

(defparameter *right-associative-operators* '(^^ =))
(defun operator-right-associative-p (operator)
  (find operator *right-associative-operators*))


;;; ********************************
;;; Define Operators ***************
;;; ********************************

(defvar *token-operators* nil)
(defvar *token-prefix-operator-table* (make-hash-table))
(defvar *token-infix-operator-table* (make-hash-table))
(defun token-operator-p (token)
  (find token *token-operators*))
(defun get-token-prefix-operator (token)
  (gethash token *token-prefix-operator-table*))
(defun get-token-infix-operator (token)
  (gethash token *token-infix-operator-table*))

(eval-when (compile load eval)
  (defmacro define-token-operator (operator-name &key
						 (prefix nil prefix-p)
						 (infix nil infix-p))
    `(progn
       (pushnew ',operator-name *token-operators*)
       ,(when prefix-p
	  `(setf (gethash ',operator-name *token-prefix-operator-table*)
		 #'(lambda (stream)
		     ,@(cond ((and (consp prefix)
				   (eq (car prefix) 'infix-error))
			      ;; To avoid ugly compiler warnings.
			      `((declare (ignore stream))
				,prefix))
			     (t
			      (list prefix))))))
       ,(when infix-p
	  `(setf (gethash ',operator-name *token-infix-operator-table*)
		 #'(lambda (stream left)
		     ,@(cond ((and (consp infix)
				   (eq (car infix) 'infix-error))
			      ;; To avoid ugly compiler warnings.
			      `((declare (ignore stream left))
				,infix))
			     (t
			      (list infix)))))))))

;;; Readtable definitions for characters, so that the right token is returned.
(eval-when (compile load eval)
  (defmacro define-character-tokenization (char function)
    `(set-macro-character ,char ,function nil *infix-readtable*)))


;;; ********************************
;;; Operator Definitions ***********
;;; ********************************

(define-token-operator and
    :infix  `(and ,left ,(gather-superiors 'and stream)))
(define-token-operator or
    :infix  `(or ,left ,(gather-superiors 'or stream)))
(define-token-operator not
    :prefix `(not ,(gather-superiors 'not stream)))

(define-token-operator if
    :prefix (let* ((test (gather-superiors 'if stream))
		   (then (cond ((same-token-p (peek-token stream) 'then)
				(read-token stream)
				(gather-superiors 'then stream))
			       (t
				(infix-error "Missing THEN clause."))))
		   (else (when (same-token-p (peek-token stream) 'else)
			   (read-token stream)
			   (gather-superiors 'else stream))))
	      (cond ((and test then else)
		     `(if ,test ,then ,else))
		    ((and test then)
		     ;; no else clause
		     `(when ,test ,then))
		    ((and test else)
		     ;; no then clause
		     `(unless ,test ,else))
		    (t
		     ;; no then and else clauses --> always NIL
		     nil))))

(define-token-operator then
    :prefix (infix-error "THEN clause without an IF."))
(define-token-operator else
    :prefix (infix-error "ELSE clause without an IF."))

(define-character-tokenization #\+
    #'(lambda (stream char)
	(declare (ignore char))
	(cond ((char= (peek-char nil stream t nil t) #\=)
	       (read-char stream t nil t)
	       '+=)
	      (t
	       '+))))
(define-token-operator +
    :infix `(+ ,left ,(gather-superiors '+ stream))
    :prefix (gather-superiors '+ stream))
(define-token-operator +=
    :infix `(incf ,left ,(gather-superiors '+= stream)))

(define-character-tokenization #\-
    #'(lambda (stream char)
	(declare (ignore char))
	(cond ((char= (peek-char nil stream t nil t) #\=)
	       (read-char stream t nil t)
	       '-=)
	      (t
	       '-))))
(define-token-operator -
    :infix `(- ,left ,(gather-superiors '- stream))
    :prefix `(- ,(gather-superiors '- stream)))
(define-token-operator -=
    :infix `(decf ,left ,(gather-superiors '-= stream)))

(define-character-tokenization #\*
    #'(lambda (stream char)
	(declare (ignore char))
	(cond ((char= (peek-char nil stream t nil t) #\=)
	       (read-char stream t nil t)
	       '*=)
	      (t
	       '*))))
(define-token-operator *
    :infix `(* ,left ,(gather-superiors '* stream)))
(define-token-operator *=
    :infix `(,(if (symbolp left) 
		  'setq
		  'setf)
	      ,left 
	      (* ,left ,(gather-superiors '*= stream))))

(define-character-tokenization #\/
    #'(lambda (stream char)
	(declare (ignore char))
	(cond ((char= (peek-char nil stream t nil t) #\=)
	       (read-char stream t nil t)
	       '/=)
	      (t
	       '/))))
(define-token-operator /
    :infix `(/ ,left ,(gather-superiors '/ stream))
    :prefix `(/ ,(gather-superiors '/ stream)))
(define-token-operator /=
    :infix `(,(if (symbolp left) 
		  'setq
		  'setf)
	      ,left 
	      (/ ,left ,(gather-superiors '/= stream))))

(define-character-tokenization #\^
    #'(lambda (stream char)
	(declare (ignore char))
	(cond ((char= (peek-char nil stream t nil t) #\^)
	       (read-char stream t nil t)
	       '^^)
	      (t
	       '^))))
(define-token-operator ^^
    :infix `(expt ,left ,(gather-superiors '^^ stream)))
(define-token-operator ^
    :infix `(logxor ,left ,(gather-superiors '^ stream)))

(define-character-tokenization #\|
    #'(lambda (stream char)
	(declare (ignore char))
	(cond ((char= (peek-char nil stream t nil t) #\|)
	       (read-char stream t nil t)
	       'or)
	      (t
	       '\|))))
(define-token-operator \|
    :infix `(logior ,left ,(gather-superiors '\| stream)))

(define-character-tokenization #\&
    #'(lambda (stream char)
	(declare (ignore char))
	(cond ((char= (peek-char nil stream t nil t) #\&)
	       (read-char stream t nil t)
	       'and)
	      (t
	       '\&))))
(define-token-operator \&
    :infix `(logand ,left ,(gather-superiors '\& stream)))

(define-character-tokenization #\%
    #'(lambda (stream char)
	(declare (ignore stream char))
	'\%))
(define-token-operator \%
    :infix `(mod ,left ,(gather-superiors '\% stream)))

(define-character-tokenization #\~
    #'(lambda (stream char)
	(declare (ignore stream char))
	'\~))
(define-token-operator \~
    :prefix `(lognot ,(gather-superiors '\~ stream)))

(define-character-tokenization #\,
    #'(lambda (stream char)
	(declare (ignore stream char))
	'\,))
(define-token-operator \,
    :infix `(progn ,left ,(gather-superiors '\, stream)))

(define-character-tokenization #\=
    #'(lambda (stream char)
	(declare (ignore char))
	(cond ((char= (peek-char nil stream t nil t) #\=)
	       (read-char stream t nil t)
	       '==)
	      (t
	       '=))))
(define-token-operator ==
    :infix `(= ,left ,(gather-superiors '== stream)))
(define-token-operator =
    :infix `(,(if (symbolp left) 
		  'setq
		  'setf)
	      ,left
	      ,(gather-superiors '= stream)))

(define-character-tokenization #\<
    #'(lambda (stream char)
	(declare (ignore char))
	(cond ((char= (peek-char nil stream t nil t) #\=)
	       (read-char stream t nil t)
	       '<=)
	      ((char= (peek-char nil stream t nil t) #\<)
	       (read-char stream t nil t)
	       '<<)
	      (t
	       '<))))
(define-token-operator <
    :infix `(< ,left ,(gather-superiors '< stream)))
(define-token-operator <=
    :infix `(<= ,left ,(gather-superiors '<= stream)))
(define-token-operator <<
    :infix `(ash ,left ,(gather-superiors '<< stream)))

(define-character-tokenization #\>
    #'(lambda (stream char)
	(declare (ignore char))
	(cond ((char= (peek-char nil stream t nil t) #\=)
	       (read-char stream t nil t)
	       '>=)
	      ((char= (peek-char nil stream t nil t) #\>)
	       (read-char stream t nil t)
	       '>>)
	      (t
	       '>))))
(define-token-operator >
    :infix `(> ,left ,(gather-superiors '> stream)))
(define-token-operator >=
    :infix `(>= ,left ,(gather-superiors '>= stream)))
(define-token-operator >>
    :infix `(ash ,left (- ,(gather-superiors '>> stream))))

(define-character-tokenization #\!
    #'(lambda (stream char)
	(declare (ignore char))
	(cond ((char= (peek-char nil stream t nil t) #\=)
	       (read-char stream t nil t)
	       '!=)
	      (t
	       '!))))
(define-token-operator !=
    :infix `(not (= ,left ,(gather-superiors '!= stream))))
(define-token-operator !
    :prefix (read-regular stream))

(define-character-tokenization #\[
    #'(lambda (stream char)
	(declare (ignore stream char))
	'\[))
(define-token-operator \[
    :infix (let ((indices (infix-read-delimited-list '\] '\, stream)))
	     (if (null indices)
		 (infix-error "No indices found in array reference.")
		 `(aref ,left ,@indices))))

(define-character-tokenization #\(
    #'(lambda (stream char)
	(declare (ignore stream char))
	'\())
(define-token-operator \(
    :infix `(,left ,@(infix-read-delimited-list '\) '\, stream))
    :prefix (let ((list (infix-read-delimited-list '\) '\, stream)))
	      (if (null (rest list))
		  ;; only one element in list. works correctly if list is NIL
		  (first list)
		  ;; several elements in list
		  `(progn ,@list))))

(define-character-tokenization #\]
    #'(lambda (stream char)
	(declare (ignore stream char))
	'\]))
(define-token-operator \]
    :infix (infix-error "Extra close brace \"]\" in infix expression"))

(define-character-tokenization #\)
    #'(lambda (stream char)
	(declare (ignore stream char))
	'\)))
(define-token-operator \)
    :infix (infix-error "Extra close paren \")\" in infix expression"))

#|
;;; Commented out because no longer using $ as the macro character.
(define-character-tokenization #\$
    #'(lambda (stream char)
	(declare (ignore stream char))
	'%infix-end-token%))
(define-token-operator %infix-end-token%
    :infix (infix-error "Prematurely terminated infix expression")
    :prefix (infix-error "Prematurely terminated infix expression"))
|#

(define-character-tokenization #\;
    #'(lambda (stream char)
	(declare (ignore char))
	(do ((char (peek-char nil stream t nil t)
		   (peek-char nil stream t nil t)))
	    ((or (char= char #\newline) (char= char #\return)
		 ;; was #\$
;		 (char= char #\))
		 )
	     ;; Gobble characters until the end of the line or the
	     ;; end of the input.
	     (cond ((or (char= char #\newline) (char= char #\return))
		    (read-char stream)
		    (read stream t nil t))
		   (t
		    ;; i.e., return %infix-end-token%
		    (read stream t nil t))))
	  (read-char stream))))


;;; ********************************
;;; Syntactic Modifications ********
;;; ********************************

;;; Post processes the expression to remove some unsightliness caused
;;; by the way infix processes the input. Note that it is also required
;;; for correctness in the a<b<=c case. 

(defun post-process-expression (expression)
  (if (and (consp expression)
	   (= (length expression) 3))
      (destructuring-bind (operator left right) expression
	(cond ((and (consp left)
		    (same-operator-p (first left) operator)
		    (find operator '(+ * / - and or < > <= >= progn)
			  :test #'same-operator-p))
	       ;; Flatten the expression if possible
	       (cond ((and (eq operator '-)
			   (= (length left) 2))
		      ;; -a-b --> (+ (- a) (- b)). 
		      `(+ ,left (- ,right)))
		     ((and (eq operator '/)
			   (= (length left) 2))
		      ;; ditto with /
		      `(/ (* ,(second left) ,right)))
		     (t 
		      ;; merges a+b+c as (+ a b c).	     
		      (append left (list right)))))
	      ((and (consp left)
		    (eq operator '-)
		    (eq (first left) '+))
	       ;; merges a+b-c as (+ a b (- c)).
	       (append left (list `(- ,right))))
	      ((and (consp left)
		    (find operator '(< > <= >=))
		    (find (first left) '(< > <= >=)))
	       ;; a<b<c --> a<b and b<c
	       `(and ,left
		     (,operator ,(first (last left))
				,right)))
	      (t
	       expression)))
      expression))


;;; ********************************
;;; Test Infix *********************
;;; ********************************

;;; Invoke with (infix:test-infix). 
;;; Prints out all the tests that fail and a count of the number of failures.

(defparameter *test-cases*
    ;; Note that in strings, we have to slashify \ as \\.
    '(("1 * +2"         (* 1 2))
      ("1 * -2"         (* 1 (- 2)))
      ("1 * /2"         (* 1 (/ 2)))
      ("/2"             (/ 2))
      ("not true"       (not true))
      ("foo\\-bar"     foo-bar)
      ("a + b-c"       (+ a b (- c)))
      ("a + b\\-c"      (+ a b-c))
      ("f\\oo"          |FoO|)
      ("!foo-bar * 2"  (* foo-bar 2))
      ("!(foo bar baz)" (foo bar baz))
      ("!foo-bar "     foo-bar)
      ;; The following now longer gives an eof error, since the close
      ;; parenthesis terminates the token.
      ("!foo-bar"      foo-bar)		; eof error -- ! eats the close $
      ("a+-b"          (+ a (- b)))
      ("a+b"           (+ a b))
      ("a+b*c"         (+ a (* b c)))
      ("a+b+c"         (+ a b c))
      ("a+b-c"         (+ a b (- c)))
      ("a+b-c+d"       (+ a b (- c) d))
      ("a+b-c-d"       (+ a b (- c) (- d)))
      ("a-b"           (- a b))
      ("a*b"           (* a b))
      ("a*b*c"         (* a b c))
      ("a*b+c"         (+ (* a b) c))
      ("a/b"           (/ a b))
      ("a^^b"          (expt a b))
      ("foo/-bar"      (/ foo (- bar)))
      ("1+2*3^^4"       (+ 1 (* 2 (expt 3 4))))
      ("1+2*3^^4+5"     (+ 1 (* 2 (expt 3 4)) 5))
      ("2*3^^4+1"       (+ (* 2 (expt 3 4)) 1))
      ("2+3^^4*5"       (+ 2 (* (expt 3 4) 5)))
      ("2^^3^^4"        (expt 2 (expt 3 4)))
      ("x^^2 + y^^2"    (+ (expt x 2) (expt y 2)))
      ("(1+2)/3"       (/ (+ 1 2) 3))
      ("(a=b)"         (setq a b))
      ("(a=b,b=c)"     (progn (setq a b) (setq b c)))
      ("1*(2+3)"       (* 1 (+ 2 3)))
      ("1+2/3"         (+ 1 (/ 2 3)))
      ("a,b"           (progn a b))
      ("a,b,c"         (progn a b c))
      ("foo(a,b,(c,d))" (foo a b (progn c d)))
      ("foo(a,b,c)"    (foo a b c))
      ("(a+b,c)"       (progn (+ a b) c))
      ("1"             1)
      ("-1"            (- 1))
      ("+1"            1)
      ("1."            1)
      ("1.1"           1.1)
      ("1e3"           1000.0)
      ("1e-3"          0.001)
      ("1f-3"          1f-3)
      ("1e-3e"         (- 1e0 3e0))
      ("!1e-3 "        0.001)
      ("a and b and c" (and a b c))
      ("a and b or c"  (or (and a b) c))
      ("a and b"       (and a b))
      ("a or b and c"  (or a (and b c)))
      ("a or b"        (or a b))
      ("a<b and b<c"   (and (< a b) (< b c)))
      ("if (if a then b else c) then e" (when (if a b c) e))
      ("if 1 then 2 else 3+4"  (if 1 2 (+ 3 4)))
      ("(if 1 then 2 else 3)+4"  (+ (if 1 2 3) 4))
      ("if a < b then b else a"   (if (< a b) b a))
      ("if a and b then c and d else e and f"
       (if (and a b) (and c d) (and e f)))
      ("if a or b then c or d else e or f" (if (or a b) (or c d) (or e f)))
      ("if a then (if b then c else d) else e"  (if a (if b c d) e))
      ("if a then (if b then c) else d"  (if a (when b c) d))
      ("if a then b else c"       (if a b c))
      ("if a then b"              (when a b))
      ("if a then if b then c else d else e" (if a (if b c d) e))
      ("if a then if b then c else d"  (when a (if b c d)))
      ("if if a then b else c then e" (when (if a b c) e))
      ("if not a and not b then c" (when (and (not a) (not b)) c))
      ("if not a then not b else not c and d"
       (if (not a) (not b) (and (not c) d)))
      ("not a and not b" (and (not a) (not b)))
      ("not a or not b" (or (not a) (not b)))
      ("not a<b and not b<c"   (and (not (< a b)) (not (< b c))))
      ("not a<b"       (not (< a b)))
      ("a[i,k]*b[j,k]"          (* (aref a i k) (aref b j k)))
      ("foo(bar)=foo[bar,baz]"  (setf (foo bar) (aref foo bar baz)))
      ("foo(bar,baz)"           (foo bar baz))
      ("foo[bar,baz]"           (aref foo bar baz))
      ("foo[bar,baz]=barf"      (setf (aref foo bar baz) barf))
      ("max = if a < b then b else a"   (setq max (if (< a b) b a)))
      ("a < b < c"     (< A B C))
      ("a < b <= c"    (and (< a b) (<= b c)))
      ("a <= b <= c"   (<= A B C))
      ("a <= b <= c"   (<= A B C))
      ("a!=b and b<c"  (and (not (= a b)) (< b c)))
      ("a!=b"          (not (= a b)))
      ("a<b"           (< a b))
      ("a==b"          (= a b))
      ("a*b(c)+d"      (+ (* a (b c)) d))
      ("a+b(c)*d"      (+ a (* (b c) d)))
      ("a+b(c)+d"      (+ a (b c) d))
      ("d+a*b(c)"      (+ d (* a (b c))))
      ("+a+b"          (+ a b))
      ("-a+b"          (+ (- a) b))
      ("-a-b"          (+ (- a) (- b)))
      ("-a-b-c"        (+ (- a) (- b) (- c)))
      ("a*b/c"         (/ (* a b) c))
      ("a+b-c"         (+ a b (- c)))
      ("a-b-c"         (- a b c))
      ("a/b*c"         (* (/ a b) c))
      ("a/b/c"         (/ a b c))
      ("/a/b"          (/ (* a b)))
      ("a^^b^^c"         (expt a (expt b c)))
      ("a(d)^^b^^c"      (expt (a d) (expt b c)))
      ("a<b+c<d"       (< a (+ b c) d))
      ("1*~2+3"        (+ (* 1 (lognot 2)) 3)) 
      ("1+~2*3"        (+ 1 (* (lognot 2) 3))) 
      ("1+~2+3"        (+ 1 (lognot 2) 3)) 
      ("f(a)*=g(b)"    (setf (f a) (* (f a) (g b))))
      ("f(a)+=g(b)"    (incf (f a) (g b)))
      ("f(a)-=g(b)"    (decf (f a) (g b)))
      ("f(a)/=g(b)"    (setf (f a) (/ (f a) (g b))))
      ("a&b"           (logand a b))
      ("a^b"           (logxor a b))
      ("a|b"           (logior a b))
      ("a<<b"          (ash a b))
      ("a>>b"          (ash a (- b)))
      ("~a"            (lognot a))
      ("a&&b"          (and a b))
      ("a||b"          (or a b))
      ("a%b"           (mod a b))

      ;; Comment character -- must have carriage return after semicolon.
      ("x^^2   ; the x coordinate
        + y^^2 ; the y coordinate" :error)
      ("x^^2   ; the x coordinate
        + y^^2 ; the y coordinate
        "              (+ (expt x 2) (expt y 2)))

      ;; Errors
      ("foo(bar,baz"   :error)		; premature termination
      ;; The following no longer gives an error
      ("foo(bar,baz))" (foo bar baz))	; extra close parenthesis
      ("foo[bar,baz]]" :error)		; extra close bracket
      ("[foo,bar]"     :error)		; AREF is not a prefix operator
      ("and a"         :error)		; AND is not a prefix operator
      ("< a"           :error)		; < is not a prefix operator
      ("=bar"         :error)		; SETF is not a prefix operator
      ("*bar"          :error)		; * is not a prefix operator
      ("a not b"       :error)		; NOT is not an infix operator
      ("a if b then c" :error)		; IF is not an infix operator
      (""              :error)		; premature termination (empty clause)
      (")a"            :error)		; left parent is not a prefix operator
      ("]a"            :error)		; left bracket is not a prefix operator
      ))

(defun test-infix (&optional (tests *test-cases*))
  (let ((count 0))
    (dolist (test tests)
      (destructuring-bind (string result) test
	(unless (test-infix-case string result)
	  (incf count))))
    (format t "~&~:(~R~) test~p failed." count count)
    (values)))

(defun test-infix-case (string result)
  (multiple-value-bind (value error)
      (let ((*package* (find-package "INFIX")))
	(ignore-errors
	 (values (read-from-string (concatenate 'string "#I(" string ")")
				   t nil))))
    (cond (error
	   (cond ((eq result :error)
		  t)
		 (t
		  (format t "~&Test #I(~A) failed with ERROR." string)
		  nil)))
	  ((eq result :error)
	   (format t "~&Test #I(~A) failed. ~
                           ~&   Expected ERROR ~
                           ~&   but got ~A." 
		   string value)
	   nil)
	  ((not (equal value result))
	   (format t "~&Test #I(~A) failed. ~
                           ~&   Expected ~A ~
                           ~&   but got ~A." 
		   string result value)
	   nil)
	  (t
	   t))))  

;;; *EOF*
