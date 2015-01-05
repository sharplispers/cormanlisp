;;;;
;;;;	File:       ansi-chapter-2.lisp
;;;;   Contents:   Examples from the Hyperspec
;;;;

(dotests INVALID-NUMBER-FORMAT
	(type-of '\256) => symbol
	(type-of '25\64) => symbol   
	(type-of '1.0\E6) => symbol   
	(type-of  '|100|)  => symbol  
	(type-of ' 3\.14159)  => symbol  
	(type-of  '|3/4|)  => symbol   
	(type-of '3\/4) => symbol   
	(type-of  '5||) => symbol 
	(type-of  '/) => symbol     
	(type-of  '/5) => symbol     
	(type-of  '+) => symbol  
	(type-of  '1+) => symbol  
	(type-of  '1-) => symbol    
	(type-of  'foo+) => symbol  
	(type-of  'ab.cd) => symbol 
	(type-of  '_) => symbol  
	(type-of  '^) => symbol   
	(type-of  '^/-) => symbol
)

(dotests RATIO-FORMAT
	(every #'rationalp
	'(
	2/3                 ;This is in canonical form                  
	4/6                 ;A non-canonical form for 2/3               
	-17/23              ;A ratio preceded by a sign                 
	-30517578125/32768  ;This is (-5/2)^15                          
	10/5                ;The canonical form for this is 2           
	#o-101/75           ;Octal notation for -65/61                  
	#3r120/21           ;Ternary notation for 15/7                  
	#Xbc/ad             ;Hexadecimal notation for 188/173           
	#xFADED/FACADE      ;Hexadecimal notation for 1027565/16435934  
	)) => true
)

(dotests QUOTE-READ-MACRO
	 'foo =>  FOO
	 ''foo =>  (QUOTE FOO)
	 (car ''foo) =>  QUOTE
)

(dotests SEMICOLON-READ-MACRO
	 (+ 3 ; three
	    4)
	=>  7    
)

(dotests BACKQUOTE
	(setf x '(a b c)) => (a b c)
	 `(x ,x ,@x foo ,(cadr x) bar ,(cdr x) baz ,@(cdr x))
	=>  (x (a b c) a b c foo b bar (b c) baz b c)
)

(dotests SHARPSIGN-VERTICAL-BAR-READ-MACRO
;;; In this example, some debugging code is commented out with #|...|#
;;; Note that this kind of comment can occur in the middle of a line
;;; (because a delimiter marks where the end of the comment occurs)
;;; where a semicolon comment can only occur at the end of a line 
;;; (because it comments out the rest of the line).
 (defun add3 (n) #|(format t "~&Adding 3 to ~D." n)|# (+ n 3)) => ADD3

;;; The examples that follow show issues related to #| ... |# nesting.

;;; In this first example, #| and |# always occur properly paired,
;;; so nesting works naturally.
 (defun mention-fun-fact-1a ()
   (format t "CL uses ; and #|...|# in comments."))
=>  MENTION-FUN-FACT-1A
 (mention-fun-fact-1a)
;;>>  CL uses ; and #|...|# in comments.
=>  NIL
 #| (defun mention-fun-fact-1b ()
      (format t "CL uses ; and #|...|# in comments.")) |#
 (fboundp 'mention-fun-fact-1b) =>  NIL

;;; In this example, vertical-bar followed by sharpsign needed to appear
;;; in a string without any matching sharpsign followed by vertical-bar
;;; having preceded this.  To compensate, the programmer has included a
;;; slash separating the two characters.  In case 2a, the slash is 
;;; unnecessary but harmless, but in case 2b, the slash is critical to
;;; allowing the outer #| ... |# pair match.  If the slash were not present,
;;; the outer comment would terminate prematurely.
 (defun mention-fun-fact-2a ()
   (format t "Don't use |\# unmatched or you'll get in trouble!"))
=>  MENTION-FUN-FACT-2A
 (mention-fun-fact-2a)
;;>>  Don't use |# unmatched or you'll get in trouble!
=>  NIL
 #| (defun mention-fun-fact-2b ()
      (format t "Don't use |\# unmatched or you'll get in trouble!") |#
 (fboundp 'mention-fun-fact-2b) =>  NIL

;;; In this example, the programmer attacks the mismatch problem in a
;;; different way.  The sharpsign vertical bar in the comment is not needed
;;; for the correct parsing of the program normally (as in case 3a), but 
;;; becomes important to avoid premature termination of a comment when such 
;;; a program is commented out (as in case 3b).
 (defun mention-fun-fact-3a () ; #|
   (format t "Don't use |# unmatched or you'll get in trouble!"))
=>  MENTION-FUN-FACT-3A
 (mention-fun-fact-3a)
;;>>  Don't use |# unmatched or you'll get in trouble!
=>  NIL
 #|
 (defun mention-fun-fact-3b () ; #|
   (format t "Don't use |# unmatched or you'll get in trouble!"))
 |#
 (fboundp 'mention-fun-fact-3b) =>  NIL
)
	
	
	
