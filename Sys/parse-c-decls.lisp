;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;	Portions Copyright (c) 2000 Vassili Bykov
;;;; 
;;;;	File:		parse-c-decls.lisp
;;;;	Contents:	Functions to parse C declarations pasted
;;;;                    from header files.
;;;;
;;;;	History:
;;;;     3/17/1998 RGC  Created.
;;;;    11/17/1998 VB   Fully rewritten to handle #defines, typedefs,
;;;;                    structs, C and C++ comments, a bunch of translation
;;;;                    options.
;;;;    11/18/1998 VB   Added support for string constants and configurable
;;;;                    linkage tokens (:pascal), HANDLE as a built-in type,
;;;;                    assorted preprocessor directives.
;;;;    11/23/1998 VB   Added support for "forward" struct declarations and
;;;;                    typedef structs referring to such declarations, the
;;;;                    use of constants as array size, and #LISP escapes.
;;;;    11/27/1998 VB   Changed the handling of :export - generate one export
;;;;                    expression per C block section rather than one per each
;;;;                    C declaration.

(in-package :c-types)

(export 'transcribe-file)

;;; The following variables control translation.  They are bound to
;;; appropriate values taken from the translation parameters, so these default
;;; values are normally always shadowed, but they make testing easier.

(defvar *target-library* nil)
(defvar *name-translations* nil)
(defvar *ignored-names* nil)
(defvar *pascal-hints* nil)
(defvar *automagical-ansi-funs* t)
(defvar *export-all* nil)
(defvar *translate-verbose* nil)

;;;  Error reporting.  Whenever possible, *CURRENT-DEFINITION* is bound to a
;;;  string describing the current C definition being translated.

(defvar *current-definition* "<no current definition>")

(defmacro syntax-error (message &rest args)
  ;; It is a macro so that ERROR is called directly from the function
  ;; reporting the error (the function name is displayed in the error banner).
  `(error "Invalid syntax in C declaration ~A: ~?"
    *current-definition*
    ,message
    (list ,@args)))

(defun syntax-warning (message &rest args)
  (format *error-output* "~&;;; Warning translating <~A>:~%;;; ~?~%"
	  *current-definition*
	  message args))

(defvar *exported-syms* nil)

(defun maybe-export-sym (symbol)
  (when *export-all*
    (push symbol *exported-syms*)))


;;;
;;;  THE TOKENS AND SOME C KNOWLEDGE
;;;

;;; Instead of defining a special token struct, we use Lisp objects as tokens:
;;;
;;;  C keyword        => Lisp keyword, e.g. int => :int
;;;  C syntax element => Lisp char, e.g. ] => #\]
;;;  C identifier     => Lisp string, .e.g. WM_CREATE => "WM_CREATE"
;;;  C lit. number    => Lisp number, e.g. 0x10L => 16
;;;
;;; This usually allows us to use EQL to compare tokens in general case, or EQ
;;; to test whether a token is a specific keyword, and sometimes use CASE
;;; instead of COND.

(defmacro define-c-keywords (const-symbol &rest defs)
  (let ((pairs (mapcar #'(lambda (def)
			   (cond ((stringp def)
				  (cons def
					(intern (string-upcase def) :keyword)))
				 ((consp def)
				  (cons (car def) (cadr def)))
				 (t
				  (error "invalid C keyword specifier"))))
		       defs)))
    `(defconstant ,const-symbol
      (let ((ht (make-hash-table :test #'equal)))
	,@(mapcar #'(lambda (def)
		      `(setf (gethash ,(car def) ht) ,(cdr def)))
		  pairs)
	ht))))

(define-c-keywords +c-keywords+
    "void" "char" "int" "float" "HANDLE" 
    "signed" "unsigned" "short" "long" "single" "double"
    "_pascal" "PASCAL" "const" "CONST"
    "near" "NEAR" "far" "FAR"
    "typedef" "struct"
    ("__int64" :int64)
    ("#define" :define)
    ("#LISP" :lisp) ; lisp escape from a C block
    ;; All other preprocessor directives are junk that will be ignored with
    ;; a warning.
    ("#if" :if) ("#ifdef" :ifdef)
    ("#ifndef" :ifndef) ("#else" :else) ("#endif" :endif)
    "interface")

(defconstant +preprocessor-junk+
  '(:if :ifdef :ifndef :else :endif))

;; MAKE-TOKEN below is on the bottleneck path (originating in read-token).
;; It is executed for each word read from the input.  

(defun make-token (string)
  ;; If the string is a known C keyword, return the corresponding Lisp
  ;; keyword.  Otwerwise, if the string looks like a number representation,
  ;; return the number.  Otherwise return the string and consider it an
  ;; identifier.
  (declare (optimize (speed 3) (safety 0) (space 0))
	   (simple-string string))
  (or (gethash string +c-keywords+)
      (if (digit-char-p (char string 0))
	  ;; If it is a number, recognize the hexadecimal and octal prefixes
	  ;; and allow junk at the end (may be the length or unsignedness
	  ;; indicator).
	  (let* (start
		 radix)
	    (declare (fixnum start radix))
	    (if (eql #\0 (char string 0))
		(if (eql #\x (and (> (length string) 1) (char string 1)))
		    (setq start 2 radix 16)
		  (setq start 0 radix 8))
	      (setq start 0 radix 10))
	    (parse-integer string :start start :radix radix :junk-allowed t))
	string)))


(defun id-token-p (tok)
  ;; T if TOK represents a C identifier like a #define'd constant or a
  ;; typedef'ed type name.  Those are always strings.
  (stringp tok))

(defun term-token-p (tok)
  ;; T if TOK represents something that can appear as a term in an expression.
  ;; We allow identifiers or literal numbers.
  (or (stringp tok) (numberp tok)))

(defun string-token-p (tok)
  (and (consp tok)
       (eq (car tok) :string)))

(defun string-token-string (tok)
  (cdr tok))

(defun prettified-token-string (tokens)
  (with-output-to-string (s)
    (mapl #'(lambda (tail)
	      (let ((tok (car tail))
		    (lastp (null (cdr tail))))
		(princ (typecase tok
			 (symbol (string-downcase (symbol-name tok)))
			 (t tok))
		       s)
		(unless lastp (princ #\Space s))))
	  tokens)))

(defun intern-id (tok)
  ;; Convert an identifier to a symbol in the proper package, translating it
  ;; if this name has translation rules.
  (assert (stringp tok))
  (or (cadr (assoc tok *name-translations* :test #'string=))
      (intern (string-upcase tok))))

(defun intern-term (tok)
  ;; This is a term of an expression, that is, either an identifier or a
  ;; literal number.
  (typecase tok
    (string (intern-id tok))
    (number tok)))

(defun has-ansi-suffix-p (name)
  (assert (stringp name))
  (let ((len (length name)))
    (and (> len 2)
	 (char= (char name (1- len)) #\A)
	 (lower-case-p (char name (- len 2))))))

(defun fun-symbol-name (name)
  (assert (stringp name))
  (if (and *automagical-ansi-funs*
	   (has-ansi-suffix-p name))
      (subseq name 0 (- (length name) 1))
      name))

(defun intern-fun-name (tok)
  (or (cadr (assoc tok *name-translations* :test #'string=))
      (intern (string-upcase (fun-symbol-name tok)))))


;;;
;;;  TOKENIZING A C STREAM AND GROUPING TOKENS INTO "THINGS"
;;;

(declaim #|(inline c-constituent-char-p)|#
	 (ftype (function (character) boolean) c-constituent-char-p))

(defun c-constituent-char-p (x)
  (declare (optimize (speed 3) (safety 0))
	   (character x))
  (or (alphanumericp x) (eql x #\_) (eql x #\#))) ; allow # for #define

(defun skip-white-space (stream want-newline-p)
  (do ((c (read-char stream t nil t)(read-char stream t nil t)))
      ((and (not (member c '(#\Space #\Tab #\Return)))
	    (or want-newline-p (not (eql c #\Newline))))
       (unread-char c stream))))

;; Assume we just read a #\/ from STREAM.  If it is the beginning of a
;; comment, skip it.  Return a boolean indicating whether we've skipped a
;; comment or not (meaning the initial #\/ was meaningful).  Leave STREAM
;; where it was if there was no comment, or just past the comment if there was
;; one.
;;
(defun maybe-skip-comment (stream)
  (case (peek-char nil stream t nil t)
    (#\/
     (peek-char #\Newline stream t nil t)
     t)
	(#\*
     (do ((ch (peek-char #\* stream t nil t)
	      (peek-char #\* stream t nil t)))
	 	 (nil)
		(declare (ignore ch))
       	 (read-char stream t nil t)	; skip this *
       	 (when (eql #\/ (peek-char nil stream t nil t))
	 		(read-char stream t nil t)
	 		(return t))))
    (t
     nil)))

;; THIS FUNCTION IS ON THE BOTTLENECK EXECUTION PATH.

(defun read-token (stream &optional want-newline-p)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (skip-white-space stream want-newline-p)
  (do ((c (read-char stream t nil t) (read-char stream t nil t))
       (token nil)
       (in-token nil))
      (nil)
    (cond ((c-constituent-char-p c)
	   (setq in-token t)
	   (push c token)
	   )
	  ((eql c #\")
	   (unread-char c stream)
	   (return (cons :string (read stream t nil t))))
	  ((eql c #\!)
	   (setq c (read-char stream t nil t))
	   (unless (eq c #\#)
	     (syntax-error "Invalid character following #\!: ~S" c))
	   (return :end))
	  (t
	   (when in-token
	     (unread-char c stream)
	     (setq in-token nil)
	     (return 
	       (make-token (concatenate 'string (nreverse token)))))
	   (return
	     (if (and (eql c #\/ ) (maybe-skip-comment stream))
		 (read-token stream want-newline-p)
	         c))))))


;; This is what we mean by a "C thing": a C statement or a preprocessor
;; #define directive.  The difference is that a statement is terminated by a
;; semicolon while #define is terminated by a newline and may be continued
;; over multiple lines with a backslash.
;;
(defun next-statement-or-directive (stream)
  (let ((tokens (list (read-token stream))))
    (when (eq (car tokens) :end)
      (return-from next-statement-or-directive nil))
    (cond ((or (eq (car tokens) :define)
	       (member (car tokens) +preprocessor-junk+))
	   ;; Looking at a preprocessor directive; gobble it all up to the
	   ;; first newline.  Be ready to deal with a \ line continuation.
	   (do ((tok (read-token stream t) (read-token stream t)))
	       ((or (null tok)
		    (eql tok #\Newline)))
	     (if (eql tok #\\)
		 ;; the directive may be continued on the next line with \
		 (unless (eql (read-token stream t) #\Newline)
		   (syntax-error "Newline expected after a backslash"))
	       (push tok tokens))))
	  ((eq (car tokens) :lisp)
	   (return-from next-statement-or-directive tokens))
	  (t
	   ;; Looking at a C statement; read all up to a semicolon.  Let the
	   ;; semicolons within braces slip through so that a struct or COM
	   ;; interface definition is read in as a whole.
	   (do ((tok (read-token stream) (read-token stream))
		(brace-level 0))
	       ((and (eql tok #\;)
		     (zerop brace-level)))
	     (cond ((eql tok #\{) (incf brace-level))
		   ((eql tok #\}) (decf brace-level)))
	     (push tok tokens))))
    (nreverse tokens)))



;;;
;;;  PREPROCESSOR #define
;;;

;; Current bummers: no operator precedence, only some operators (see
;; OPERATOR-TOKEN-P) are supported.  Good enough for Win32 headers...

(defconstant +c-operators+
  '((#\+ . +)
    (#\- . -)
    (#\* . *)
    (#\/ . /)
    (#\% . mod)
    (#\& . logand)
    (#\| . logior)
    (#\^ . logxor)))

(defun operator-token-p (token)
  (assoc token +c-operators+))

(defun translate-operator (token)
  (cdr (assoc token +c-operators+)))

;; A classical recursive descent parser for expressions in definitions to
;; convert them to prefix notation.  The term nonterminal is broken up into
;; several nonterminals to implement operator priority and left-factored to
;; allow recursive descent parsing.
;;
(defun translate-arithmetic-expression (tokens)
  (let* ((input tokens)
	 (lookahead (car tokens)))
    (labels  
	((match (token)
	   (unless (eql token (car input))
	     (syntax-error "~A expected, got: ~A" token lookahead))
	   (setq input (cdr input))
	   (setq lookahead (car input)))
	 (binop-combine (form ops head-fun tail-fun)
	   (if (member lookahead ops)
	       (let ((op (translate-operator lookahead)))
		 (match lookahead)
		 (funcall tail-fun
			  (list op form (funcall head-fun))))
	     form))
	 (expr ()
	   (expr-rest (term0)))
	 (expr-rest (form)
	   (binop-combine form '(#\|) #'term0 #'expr-rest))
	 (term0 ()
	   (term0-rest (term1)))
	 (term0-rest (form)
	   (binop-combine form '(#\^) #'term1 #'term0-rest))
	 (term1 ()
	   (term1-rest (term2)))
	 (term1-rest (form)
	   (binop-combine form '(#\&) #'term2 #'term1-rest))
	 (term2 ()
	   (term2-rest (term3)))
	 (term2-rest (form)
	   (binop-combine form '(#\+ #\-) #'term3 #'term2-rest))
	 (term3 ()
	   (term3-rest (term4)))
	 (term3-rest (form)
	   (binop-combine form '(#\* #\/ #\%) #'term4 #'term3-rest))
	 (term4 ()
	   (cond ((eql #\( lookahead)
		  (prog2
		      (match #\( )
		      (expr)
		    (match #\) )))
		 ((eql #\~ lookahead)
		  (match #\~)
		  (list 'lognot (term2)))
		 ((eql #\- lookahead)
		  (match #\-)
		  (list (translate-operator #\-) (term2)))
		 ((term-token-p lookahead)
		  (prog1
		      (intern-term lookahead)
		    (match lookahead)))
		 (t
		  (syntax-error "A term is expected, got ~A" lookahead)))))
      (prog1
	  (expr)
	(when lookahead
	  (syntax-error "Cannot parse past token ~A" lookahead))))))

(defun translate-string-expression (tokens)
  ;; A string expression may only be a sequence of strings which all get
  ;; concatenated to yield the result.
  (unless (every #'string-token-p tokens)
    (syntax-error "Expect one or more literal strings, got: ~S" tokens))
  (with-output-to-string (s)
    (dolist (tok tokens)
      (princ (string-token-string tok) s))))

(defun translate-expression (tokens)
  (if (string-token-p (car tokens))
    (translate-string-expression tokens)
    (translate-arithmetic-expression tokens)))

(defun translate-sharpdefine (tokens)
  (let ((const-name (cadr tokens))
	(expr (cddr tokens)))
    (when (or (not (id-token-p const-name))
	      (null expr))
      (syntax-error "Invalid #define directive"))
    (setq const-name (intern-id const-name))
    (maybe-export-sym const-name)
    `(defconstant ,const-name ,(translate-expression expr))))
    


;;;
;;;  TYPEDEFS
;;;

(defconstant +predefined-types+
  '(((:void) :void)
    ((:char) :char)
    ((:signed :char) :char)
    ((:unsigned :char) :unsigned-char)
    ((:int) :long)
    ((:signed :int) :long)
    ((:unsigned :int) :unsigned-long)
    ((:short :int) :short)
    ((:long :int) :long)
    ((:handle) :handle)
    ((:signed :short :int) :short)
    ((:unsigned :short :int) :unsigned-short)
    ((:signed :long :int) :long)
    ((:unsigned :long :int) :unsigned-long)
    ((:float) :single-float)
    ((:single :float) :single-float)
    ((:double :float) :double-float)
    ((:int64) :int64)
    ((:signed :int64) :int64)
    ((:unsigned :int64) :uint64)))

(defconstant +predefined-base-types+
  '(:void :char :int :float :handle :int64))

(defconstant +predefined-type-modifiers+
  '(:signed :unsigned :long :short :single :double))

(defconstant +ignored-typedef-tokens+
  '(:const :pascal :_pascal :near :far))

(defun type-symbols-to-lisp-type (symbol-list)
  (or (cadr (assoc symbol-list +predefined-types+ :test #'equal))
      (if (= 1 (length symbol-list))
	  (car symbol-list)
	(syntax-error "Invalid non-predefined type reference: ~A"
		      symbol-list))))

(defun make-type-form (type-description pointer-level)
  (let ((type-form (type-symbols-to-lisp-type type-description)))
    (dotimes (i pointer-level)
      (declare (ignore i))
      (setq type-form (list type-form '*)))
    type-form))

(defun ignored-token-p (token)
  (or (and (keywordp token)
	   (member token +ignored-typedef-tokens+))
      (and (stringp token)
	   (member token *ignored-names* :test #'string=))))

;; Parse: <type-ref> "*"*
;;        <type-ref> ::= {predefined C type} | <id>
;;
(defun parse-type-prefix (tokens)
  ;; => (VALUES type-form rest-list)
  (let* ((input tokens)
	 (lookahead (car tokens))
	 (base-type nil)
	 (modifiers nil)
	 (pointer-level 0))
    (labels
	((skip-garbage ()
	   (do ()
	       ((not (ignored-token-p lookahead)))
	     (setq input (cdr input))
	     (setq lookahead (car input))))
	 (match (tok)
	   (unless (eql tok lookahead)
	     (syntax-error "~A expected, got: ~A" tok lookahead))
	   (setq input (cdr input))
	   (setq lookahead (car input))
	   (skip-garbage))
	 (src-type ()
	   (cond ((id-token-p lookahead)
		  (setq base-type (list (intern-id lookahead)))
		  (match lookahead))
		 ((member lookahead +predefined-type-modifiers+)
		  (modifier)
		  (if (member lookahead +predefined-type-modifiers+)
		      (src-type)
		    (when (member lookahead +predefined-base-types+)
		      (predefined-type))))
		 ((member lookahead +predefined-base-types+)
		  (predefined-type))
		 (t
		  (syntax-error "Unexpected token ~A" lookahead))))
	 (modifier ()
	   (setq modifiers (append modifiers (list lookahead)))
	   (match lookahead))
	 (predefined-type ()
	   (setq base-type (list lookahead))
	   (match lookahead))
	 (pointers ()
	   (do () 
	       ((not (eql lookahead #\*)))
	     (incf pointer-level)
	     (match #\*))))
      (skip-garbage)
      (src-type)
      (pointers))
    (unless (or base-type modifiers)
      (syntax-error "No valid type declaration found"))
    (unless base-type
      (cond
       ((intersection modifiers '(:signed :unsigned :short :long))
	(setq base-type '(:int)))
       ((intersection modifiers '(:single :double))
	(setq base-type '(:float)))
       (t (error "No valid type declaration found"))))
    (values (make-type-form (delete-if #'null (append modifiers base-type))
			    pointer-level)
	    input)))

;; Parse: <type-prefix> <id>? <array-spec>?
;;        <type-prefix> -- see above
;;        <array-spec>  ::= [ <integer> ]
;;
(defun parse-variable-declaration (tokens)
  ;; => (VALUES var-name-symbol type-definition-form)
  (multiple-value-bind (type-form input)
      (parse-type-prefix tokens)
    (let ((lookahead (car input))
	  (var-name nil)
	  (array-size nil))
      (labels
	  ((match (token)
	     (unless (eql lookahead token)
	       (syntax-error "~A expected, got: ~A" token lookahead))
	     (setq input (cdr input))
	     (setq lookahead (car input)))
	   (var-name ()
	     (when (id-token-p lookahead)
	       (setq var-name (intern-id lookahead))
	       (match lookahead)))
	   (dimension ()
	     (when (eql lookahead #\[ )
	       (match #\[ )
	       (array-size)
	       (match #\] ))
	     (unless (null lookahead)
	       (syntax-error "Unexpected tokens at statement end")))
	   (array-size ()
	     (unless (or (numberp lookahead) (id-token-p lookahead))
	       (syntax-error "Invalid array length specifier."))
	     (setq array-size (intern-term lookahead))
	     (match lookahead)))
	(var-name)
	(dimension)
	(match nil))
      (values var-name
	      (if array-size
		(list type-form array-size)
		type-form)))))

(defun translate-typedef-scalar (tokens)
  (multiple-value-bind (new-type-name type-def-form)
      (parse-variable-declaration (cdr tokens))
    (maybe-export-sym new-type-name)
    `(c-types:defctype ,new-type-name ,type-def-form)))

;; Parse:  struct <tag>? { <field-def>* } <var-def>*
;;         <tag> ::= <id>
;;         <field-def> ::= {variable-declaration} ;
;;         <var-def> ::= "*"* <id>
;;
(defun parse-struct-declaration (tokens)
  ;; => (VALUES tag-or-nil field-def-list var-defs-or-nil)
  (let ((input tokens)
	(lookahead (car tokens))
	(tag nil)
	(fields nil)
	(vars nil))
    (labels
	((match (tok)
	   (unless (eql tok lookahead)
	     (syntax-error "~A expected, got: ~A" tok lookahead))
	   (setq input (cdr input)
		 lookahead (car input)))
	 (tag-id ()
	   (when (id-token-p lookahead)
	     (setq tag (intern-id lookahead))
	     (match lookahead)))
	 (fields ()
	   (do ((fdefs nil)
		(ftoks nil))
	       ((or (null lookahead) (eql lookahead #\} ))
		(when ftoks
		  (syntax-warning
		   "Trailing semicolon missing in struct (inserted)")
		  (push (nreverse ftoks) fdefs))
		(setq fields
		      (mapcar
		       #'(lambda (decl)
			   (let ((parsed
				  (multiple-value-list
				      (parse-variable-declaration decl))))
			     (unless (car parsed)
			       (syntax-error "Field name is missing"))
			     parsed))
		       (nreverse fdefs))))
	     (cond
	       ((eql lookahead #\;)
		(unless ftoks
		  (syntax-error "Empty field declaration"))
		(push (nreverse ftoks) fdefs)
		(setq ftoks nil)
		(match #\;))
	       (t
		(push lookahead ftoks)
		(match lookahead)))))
	 (vars ()
	   (do ((vdefs nil)
		(vtoks nil)
		(have-name-p nil))
	       ((null lookahead)
		(when (and vdefs (null vtoks))
		  (syntax-error "Empty variable declaration"))
		(push vtoks vdefs)
		(setq vars vdefs))
	     (cond
	       ((eql lookahead #\,)
		(unless (symbolp (car vtoks))
		  (syntax-error "Empty variable declaration"))
		(push vtoks vdefs)
		(setq vtoks nil
		      have-name-p nil))
	       (t
		(when have-name-p
		  (syntax-error "Unexpected token: ~A" lookahead))
		(cond
		  ((member lookahead '(:near :far :const))) 
		  ((id-token-p lookahead)
		   (push (intern-id lookahead) vtoks)
		   (setq have-name-p t))
		  ((eql lookahead #\* )
		   (push #\* vtoks))
		  (t
		   (syntax-error "Unexpected token: ~A" lookahead)))))
	     (match lookahead))))
      (match :struct)
      (tag-id)
      (when (eql lookahead #\{)		; It is OK for the field list
	(match #\{ )			; to be missing.
	(fields)
	(match #\} ))
      (vars))
    (values tag fields vars)))

;; Translate a "regular" struct declaration (not typedef), assigning the
;; definition to the symbol named as its tag.
;;
(defun translate-struct (tokens)
  (multiple-value-bind (tag fields vars) 
                       (parse-struct-declaration tokens)
	(declare (ignore vars))
    (when (null tag)
      (syntax-error "No defined type name."))
    (maybe-export-sym tag)
    `(c-types:defcstruct ,tag ,fields)))

;; Translate a "typedef"ed struct declaration.  This may introduce several
;; types at once: one for the struct, others for pointers to it.  The type for
;; the struct may not be introduced if the field list is missing, then we
;; assume this is just a struct tag reference to define a few types.
;;
(defun translate-typedef-struct (tokens)
  (multiple-value-bind (tag fields vars)
                       (parse-struct-declaration (cdr tokens))
    (let* ((principal-name (find-if #'(lambda (def) (null (cdr def)))
				    vars))
	   (other-vars (remove principal-name vars))
	   (struct-name (car principal-name))
	   (defs nil))
      (when (and fields (null struct-name))
	(syntax-error "No principal name for the struct ~A." tag))
      (cond (fields
	     (push `(c-types:defcstruct ,struct-name ,fields) defs))
	    (tag
	     (setq struct-name tag))
	    (t
	     (syntax-error "Invalid structure type declaration")))
      (dolist (var other-vars)
	(push `(c-types:defctype ,(car var)
		,(make-type-form (list struct-name) (1- (length var))))
	      defs))
      (mapc #'(lambda (def) (maybe-export-sym (cadr def))) defs)
      (if (cdr defs)
	  (list* 'progn (nreverse defs))
	(car defs)))))

(defun translate-typedef (tokens)
  (if (eq (cadr tokens) :struct)
    (translate-typedef-struct tokens)
    (translate-typedef-scalar tokens)))



;;;
;;;  FUNCTION PROTOTYPES
;;;

;;  Parse: <type-prefix> <id> ( <params> )
;;         <params> ::= <var-def> <more-params>
;;         <more-params> ::= e | , <params>
;;
(defun parse-fun-declaration (tokens)
  ;; => (VALUES fun-name params return-type dll-entry-name linkage)
  (multiple-value-bind (return-type input)
      (parse-type-prefix tokens)
    ;; At this point looking at the function name.
    (let ((lookahead (car input))
	  name
	  entry-name
	  (linkage (if (find-if #'(lambda (elt)
				    (or (member elt '(:pascal :_pascal))
					(and (id-token-p elt)
					     (member elt *pascal-hints*
						     :test #'string=))))
				tokens)
		     :pascal
		     :c))
	  (params nil))
      (labels
	  ((skip-garbage ()
	   (do ()
	       ((not (ignored-token-p lookahead)))
	     (setq input (cdr input))
	     (setq lookahead (car input))))
	   (match (tok)
	     (unless (eql tok lookahead)
	       (syntax-error "~A expected, got: ~A" tok lookahead))
	     (setq input (cdr input))
	     (setq lookahead (car input))
	     (skip-garbage))
	   (fun-name ()
	     (unless (id-token-p lookahead)
	       (syntax-error "Function name expected, got: ~A" lookahead))
	     (setq entry-name lookahead)
	     (setq name (intern-fun-name lookahead))
	     (match lookahead))
	   (params ()
	     (do ((pdefs nil)
		  (ptoks nil))
		 ((or (null lookahead) (eql lookahead #\) ))
		  (when ptoks
		    (push (nreverse ptoks) pdefs))
		  (setq params
			(mapcar
			 #'(lambda (decl)
			     (let ((definition
				       (multiple-value-list
					   (parse-variable-declaration decl))))
			       ;; If there is no explicit parameter name,
			       ;; replace it with "" which defun-dll recognizes
			       ;; as no parameter name.
			       (when (null (car definition))
				 (setf (car definition) ""))
			       definition))
			 (nreverse pdefs))))
	       (cond
		 ((eql lookahead #\, )
		  (unless ptoks
		    (syntax-error "Empty parameter declaration"))
		  (push (nreverse ptoks) pdefs)
		  (setq ptoks nil)
		  (match #\, ))
		 (t
		  (push lookahead ptoks)
		  (match lookahead))))))
	(fun-name)
	(match #\( )
	(params)
	(match #\) )
	(match nil))
      (values name (delete-if (lambda (decl) (eq (second decl) :void)) params) return-type entry-name linkage))))
      
(defun translate-function-prototype (tokens)
  (unless *target-library*
    (syntax-error "No target library specified"))
  (multiple-value-bind (name params return-type entry-name linkage)
      (parse-fun-declaration tokens)
    (maybe-export-sym name)
    `(c-types:defun-dll ,name ,params
      :return-type ,return-type
      :library-name ,*target-library*
      :entry-name ,entry-name
      :linkage-type ,linkage)))



;;;
;;; COM INTERFACES
;;;

(defun interface-method-list (interface-name)
  (get interface-name 'interface-method-list))

(defun (setf interface-method-list) (method-list interface-name)
  (setf (get interface-name 'interface-method-list) method-list))

(defun translate-com-declaration (interface-name base-index-sym index tokens)
  (multiple-value-bind (name params return-type entry-name linkage)
      (parse-fun-declaration tokens)
	(declare (ignore entry-name linkage))
    (let ((name (intern-id (concatenate 'string
			       (symbol-name interface-name)
			       "-"
			       (symbol-name name)))))
      (maybe-export-sym name)
      `(c-types:defun-com-method
	,name
	,(cons '(win32::interface *) params)
	(+ ,base-index-sym ,index)
	:return-type ,return-type))))

(defun translate-interface (tokens)
  (let ((input tokens)
	(lookahead (car tokens))
	(name nil)
	(parent-name nil)
	(method-toks nil))
    (labels
	((match (tok)
	   (unless (eql tok lookahead)
	     (syntax-error "~A expected, got: ~A" tok lookahead))
	   (setq input (cdr input))
	   (setq lookahead (car input)))
	 (interface-name ()
	   (unless (id-token-p lookahead)
	     (syntax-error "Interface name expected, got: ~A" lookahead))
	   (setq name (intern-id lookahead))
	   (match lookahead)
	   (when (eq lookahead #\:)
	     (parent-interface)))
	 (parent-interface ()
	   (match #\: )
	   (unless (id-token-p lookahead)
	     (syntax-error "Parent interface name expected, got: ~A"
			   lookahead))
	   (setq parent-name (intern-id lookahead))
	   (match lookahead))
	 (methods ()
	   (do ((mdefs nil)
		(mtoks nil))
	       ((or (null lookahead) (eql lookahead #\} ))
		(when mtoks
		  (syntax-warning
		   "No trailing semicolon in interface method list ~
                    (inserted)")
		  (push (nreverse mtoks) mdefs))
		(setq method-toks (nreverse mdefs)))
	     (cond
	       ((eql lookahead #\;)
		(unless mtoks
		  (syntax-error "Empty method declaration"))
		(push (nreverse mtoks) mdefs)
		(setq mtoks nil)
		(match #\;))
	       (t
		(push lookahead mtoks)
		(match lookahead))))))
      (match :interface)
      (interface-name)
      (match #\{ )
      (methods)
      (match #\} )
      (match nil))
    ;; Parsed everything, now generate the definitions from mdefs.  The
    ;; definition is a bit tricky: we have to install the method lists into
    ;; the symbols at the execution time, not at the macroexpansion.  This
    ;; means method indices are not available at the expansion time.
    (let* ((base-index (gensym))
	   (rel-index -1)
	   (method-forms (mapcar #'(lambda (toks)
				     (translate-com-declaration
				      name
				      base-index
				      (incf rel-index)
				      toks))
				 method-toks)))
      (maybe-export-sym name)
      `(let ((,base-index ,(if parent-name 
			       `(length (interface-method-list ',parent-name))
			       0)))
				(defctype ,name win::interface)
	,@method-forms
	(setf (interface-method-list ',name)
	 (append ,(if parent-name
		      `(interface-method-list ',parent-name)
		      nil)
	  ',(mapcar #'cadr method-forms)))))))
 


;;;
;;; TOP LEVEL INTERFACE
;;;

(defun issue-cppjunk-warning ()
  (syntax-warning "Ignoring this preprocessor directive. ~
                   Make sure only the correct directives are processed.")
  nil)

(defun translate-c-thing (tokens)
  ;; Process TOKENS which are a list of tokens making up one C declaration or
  ;; preprocessor directive.  Return a list of forms with an equivalent Lisp
  ;; definition.
  (let ((*current-definition* (prettified-token-string tokens))
	(head (car tokens)))
    (when *translate-verbose*
      (format *standard-output* "~&;;; Translating: ~A~%"
	      *current-definition*))
    (let ((form
	   (cond ((eq head :define) (translate-sharpdefine tokens))
		 ((eq head :typedef) (translate-typedef tokens))
		 ((eq head :struct) (translate-struct tokens))
		 ((eq head :interface) (translate-interface tokens))
		 ((member head +preprocessor-junk+)
		  (issue-cppjunk-warning))
		 (t			; otherwise it better be a function
		  (translate-function-prototype tokens)))))
      (when *translate-verbose*
	(format *standard-output* "~&;;; Result: ~S~%"
		form))
      form)))

(defun canonicalize-name-list (list)
  (when (or (stringp list) (symbolp list))
    (setq list (list list)))
  (mapcar #'(lambda (elt)
	      (typecase elt
		(string elt)
		(symbol (symbol-name elt))
		(t (syntax-error "Invalid entry in name list: ~S" elt))))
	  list))

(defun canonicalize-translation-alist (alist)
  (dolist (entry alist)
    (unless (and (consp entry)
		 (stringp (car entry))
		 (symbolp (cadr entry)))
      (syntax-error "Invalid entry in the translation alist: ~S"
		    entry)))
  alist)

(defun canonicalize-translation-params (params)
  (cond ((listp params))
	((stringp params)
	 (setq params (list :library params
			    :ignore "WINUSERAPI"
			    :pascal "WINAPI"
			    :export t)))
	((eq :interface params)
	 (setq params nil))
	(t
	 (syntax-error "Invalid translation params: ~S" params)))
  (do* ((tail params (cddr tail))
	(key (car tail) (car tail))
	(value (cadr tail) (cadr tail))
	(canonical nil))
       ((null tail)
	(unless (assoc :auto-ansi canonical)
	  (push (cons :auto-ansi t) canonical))
	canonical)
    (case key
      (:library
       (unless (stringp value)
	 (syntax-error "A string is expected, got ~S" value)))
      ((:trim-last :ignore :pascal)
       (unless (or (listp value) (stringp value) (symbolp value))
	 (syntax-error "A name or list of names is expected, got ~S" value))
       (setq value (canonicalize-name-list value)))
      (:translate
       (unless (listp value)
	 (syntax-error "A list is expected, got ~S" value))
       (setq value (canonicalize-translation-alist value)))
      ((:auto-ansi :export :verbose))
      (otherwise
       (syntax-error "Unexpected translation parameter: ~S" key)))
    (push (cons key value) canonical)))
     
(defun translations-from-name-list (names)
  (mapcar
   #'(lambda (funname)
       (unless (stringp funname)
	 (syntax-error "Invalid ANSI function name ~S" funname))
       (list funname 
	     (intern 
	      (string-upcase ; NSTRING-UPCASE would do but it is broken
	       (subseq funname 0 (1- (length funname)))))))
   names))


(defun sharpbang (stream ch arg)
  (declare (ignore ch arg))
  (let* ((header (read stream t nil t))
	 (tps (canonicalize-translation-params header))
	 (*target-library*
	  (let ((lname (cdr (assoc :library tps))))
	     (and lname
		  (if (find #\. lname)
		      lname
		    (concatenate 'string lname ".dll")))))
	 (*name-translations*
	  (append (cdr (assoc :translate tps))
		  (translations-from-name-list (cdr (assoc :trim-last tps)))))
	 (*translate-verbose* (cdr (assoc :verbose tps)))
	 (*automagical-ansi-funs* (cdr (assoc :auto-ansi tps)))
	 (*pascal-hints* (cdr (assoc :pascal tps)))
	 (*ignored-names* (append (cdr (assoc :ignore tps))
				  *pascal-hints*))
	 (*export-all* (cdr (assoc :export tps)))
	 (*exported-syms* nil))
    (do ((translation-forms nil)
	 (thing
	  (if (and (keywordp header) (null tps))
	      ;; That is, there were no parameters, we got the first
	      ;; "interface" token--put it back before proceeding.
	       (cons :interface (next-statement-or-directive stream))
	     (next-statement-or-directive stream))
	   (next-statement-or-directive stream)))
	 ((null thing)
	  (cons
	   'PROGN
	   (if *exported-syms*
	       (cons `(EXPORT ',(nreverse *exported-syms*))
		     (nreverse translation-forms))
	       (nreverse translation-forms))))
      (push (if (eq (car thing) :lisp)
		(read stream t nil t)
	        (translate-c-thing thing))
	    translation-forms))))


(set-dispatch-macro-character #\# #\! #'sharpbang)

(defun cl::format-universal-time (time stream)
	(declare (ignore time stream)) nil) ;; forward declaration

(defun transcribe-file (in-file out-file 
				&optional (package :common-lisp-user)
				          (prettyp t))
  ;; Open IN-FILE, read all forms in it and write them prettily formatted into
  ;; OUT-FILE.  The output file will therefore have regular Lisp FFI
  ;; declarations in place of C declaration.  The original formatting and the
  ;; comments are, of course, lost.  The optinal PRETTYP parameter, a
  ;; generalized boolean, determines whether the output file forms are printed
  ;; prettily or not (default is T).
  (when (string-equal in-file out-file)
    (error "Input and output file names are the same."))
  (let ((*package* (find-package package)))
    (with-open-file (in in-file
			:direction :input
			:if-does-not-exist :error)
      (with-open-file (out out-file
			   :direction :output
			   :if-exists :overwrite)
	(format out
		";;; ~A -- Automatically generated FFI definition file.~@
                 ;;; Source: ~A~@
                 ;;; Transcribed: "
		out-file in-file)
	(cl::format-universal-time (get-universal-time) out)
	(terpri out) (terpri out)
	(do ((form (read in nil) (read in nil)))
	    ((null form))
	  (write form
		 :stream out :readably t :pretty prettyp
		 :right-margin 80 :miser-width 60)
	  (terpri out))))))

;;;; EOF

