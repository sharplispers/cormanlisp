;;;;
;;;;	File:       ansi-chapter-6.lisp
;;;;   Contents:   Examples from the Hyperspec
;;;;

;;; ITERATION
(dotests LOOP-DESTRUCTURING  ;; 6.1.1.7
	;; Collect values by using FOR constructs.
	 (loop for numlist in '((1 2 4.0) (5 6 8.3) (8 9 10.4))
	       for a of-type integer = (first numlist)
	       and b of-type integer = (second numlist)
	       and c of-type float = (third numlist)
	       collect (list c b a))
	=>  ((4.0 2 1) (8.3 6 5) (10.4 9 8))
	;; Destructuring simplifies the process.
	 (loop for (a b c) of-type (integer integer float) in
	       '((1 2 4.0) (5 6 8.3) (8 9 10.4))
	       collect (list c b a))
	=>  ((4.0 2 1) (8.3 6 5) (10.4 9 8))
	 
	;; If all the types are the same, this way is even simpler.
	 (loop for (a b c) of-type float in
	       '((1.0 2.0 4.0) (5.0 6.0 8.3) (8.0 9.0 10.4))
	       collect (list c b a))
	=>  ((4.0 2.0 1.0) (8.3 6.0 5.0) (10.4 9.0 8.0))
	;; Initialize and declare variables in parallel by using the AND construct.
	 (loop with (a b) of-type float = '(1.0 2.0)
	       and (c d) of-type integer = '(3 4)
	       and (e f)
	       return (list a b c d e f))
	=>  (1.0 2.0 3 4 NIL NIL)
		
	 (loop for (a nil b) = '(1 2 3)
	       do (return (list a b)))
	=>  (1 3)
	
	 (loop for (x . y) = '(1 . 2)
	       do (return y))
	=>  2
	 (loop for ((a . b) (c . d)) of-type ((float . float) (integer . integer)) in
	       '(((1.2 . 2.4) (3 . 4)) ((3.4 . 4.6) (5 . 6)))
	       collect (list a b c d))
	=>  ((1.2 2.4 3 4) (3.4 4.6 5 6))
)

(dotests FOR-AS-ARITHMETIC  ;; 6.1.2.1.1
	(let ((x 1)) (loop for i from x by (incf x) to 10 collect i))
	=>  (1 3 5 7 9)
	(let ((x 1)) (loop for i by (incf x) from x to 10 collect i))
	=>  (2 4 6 8 10)
	;; Print some numbers.
	 (loop for i from 1 to 3
	       do (print i))
	;;>>  1
	;;>>  2
	;;>>  3
	=>  NIL
	 
	;; Print every third number.
	 (loop for i from 10 downto 1 by 3
	       do (print i))
	;;>>  10 
	;;>>  7 
	;;>>  4 
	;;>>  1 
	=>  NIL
	 
	;; Step incrementally from the default starting value.
	 (loop for i below 3
	       do (print i))
	;;>>  0
	;;>>  1
	;;>>  2
	=>  NIL
)	

(dotests FOR-AS-IN-LIST
	 (loop for item in '(1 2 3) do (print item))
	;;>>  1
	;;>>  2
	;;>>  3
	=>  NIL
	 
	;; Print every other item in a list.
	 (loop for item in '(1 2 3 4 5) by #'cddr
	       do (print item))
	;;>>  1
	;;>>  3
	;;>>  5
	=>  NIL
	 
	;; Destructure a list, and sum the x values using fixnum arithmetic.
	 (loop for (item . x) of-type (t . fixnum) in '((A . 1) (B . 2) (C . 3))
	       unless (eq item 'B) sum x)
	=>  4
)

(dotests FOR-AS-ON-LIST		;; 6.1.2.1.3.1
	;; Collect successive tails of a list.
	 (loop for sublist on '(a b c d)
	       collect sublist)
	=>  ((A B C D) (B C D) (C D) (D))
	 
	;; Print a list by using destructuring with the loop keyword ON.
	 (loop for (item) on '(1 2 3)
	       do (print item))
	;;>>  1 
	;;>>  2 
	;;>>  3 
	=>  NIL
)

(dotests FOR-AS-EQUALS-THEN ;; 6.1.2.1.4.1
	;; Collect some numbers.
	 (loop for item = 1 then (+ item 10)
	       for iteration from 1 to 5
	       collect item)
	=>  (1 11 21 31 41)
)

(dotests FOR-AS-PACKAGE ;; 6.1.2.1.7.1
	 (let ((*package* (make-package "TEST-PACKAGE-1")))
	   ;; For effect, intern some symbols
	   (read-from-string "(THIS IS A TEST)")
	   (export (intern "THIS"))
	   (loop for x being each present-symbol of *package*
	          do (print x)))
		;;>>  A 
		;;>>  TEST 
		;;>>  THIS
		;;>>  IS 
		=>  NIL
)

(dotests LOCAL-VARIABLE-INITIALIZATION
	 (loop with a = 1 
	       with b = (+ a 2) 
	       with c = (+ b 3)
	       return (list a b c))
	=>  (1 3 6)
	 (loop with a = 1 
	       and b = 2 
	       and c = 3
	       return (list a b c))
	=>  (1 2 3)
)

(dotests WITH-CLAUSE
	;; These bindings occur in sequence.
	 (loop with a = 1 
	       with b = (+ a 2) 
	       with c = (+ b 3)
	       return (list a b c))
	=>  (1 3 6)
	 
	;; These bindings occur in parallel.
	 (setq a 5 b 10)
	=>  10
	 (loop with a = 1
	       and b = (+ a 2)
	       and c = (+ b 3)
	       return (list a b c))
	=>  (1 7 13)
	 
	;; This example shows a shorthand way to declare local variables 
	;; that are of different types.
	 (loop with (a b c) of-type (float integer float)
	       return (format nil "~A ~A ~A" a b c))
	=>  "0.0 0 0.0"
	 
	;; This example shows a shorthand way to declare local variables 
	;; that are the same type.
	 (loop with (a b c) of-type float 
	       return (format nil "~A ~A ~A" a b c))
	=>  "0.0 0.0 0.0"
)	

(dotests COLLECT-CLAUSE
	;; Collect all the symbols in a list.
	 (loop for i in '(bird 3 4 turtle (1 . 4) horse cat)
	       when (symbolp i) collect i)
	=>  (BIRD TURTLE HORSE CAT)
	 
	;; Collect and return odd numbers.
	 (loop for i from 1 to 10
	       if (oddp i) collect i)
	=>  (1 3 5 7 9)
	 
	;; Collect items into local variable, but don't return them.
	 (loop for i in '(a b c d) by #'cddr
	       collect i into my-list
	       finally (print my-list))
	;;>>  (A C) 
	=>  NIL
)

(dotests APPEND-CLAUSE
	;; Use APPEND to concatenate some sublists.
	  (loop for x in '((a) (b) ((c)))
	        append x)
	=>  (A B (C))
)

(dotests NCONC-CLAUSE
	;; NCONC some sublists together.  Note that only lists made by the
	;; call to LIST are modified.
	  (loop for i upfrom 0 
	        as x in '(a b (c))
	        nconc (if (evenp i) (list x) nil))
	=>  (A (C))
)

(dotests COUNT-CLAUSE ;; 6.1.3.3
	 (loop for i in '(a b nil c nil d e)
	       count i)
	=>  5
)

(dotests MAXIMIZE-CLAUSE
	 (loop for i in '(2 1 5 3 4)
	       maximize i)
	=>  5 
	;; In this example, FIXNUM applies to the internal variable that holds
	;; the maximum value.
	 (setq series '(1.2 4.3 5.7))
	=>  (1.2 4.3 5.7)
	 (loop for v in series 
	       maximize (round v) of-type fixnum)
	=>  6
)

(dotests MINIMIZE-CLAUSE
	 (loop for i in '(2 1 5 3 4)
	       minimize i)
	=>  1
	;; In this example, FIXNUM applies to the variable RESULT.
	 (loop for v of-type float in series
	       minimize (round v) into result of-type fixnum
	       finally (return result))
	=>  1
)

(dotests SUM-CLAUSE
	 (loop for i of-type fixnum in '(1 2 3 4 5)
	       sum i)
	=>  15
	 (setq series '(1.2 4.3 5.7))
	=>  (1.2 4.3 5.7)
	 (loop for v in series 
	       sum (* 2.0 v))
	=>  22.4
)

(dotests REPEAT-CLAUSE
	 (loop repeat 3
	       do (format t "~&What I say three times is true.~%"))
	;;>>  What I say three times is true.
	;;>>  What I say three times is true.
	;;>>  What I say three times is true.
	=>  NIL
	 (loop repeat -15
	   do (format t "What you see is what you expect~%"))
	=>  NIL
)

(dotests ALWAYS-CLAUSE
	 (loop for i from 0 to 10
	       always (< i 11))
	=>  T
	 ;;; The FINALLY clause is not evaluated in these examples.
	 (loop for i from 0 to 10
	       always (< i 9)
	       finally (print "you won't see this"))
	=>  NIL
 )

(dotests NEVER-CLAUSE
	 (loop for i from 0 to 10
	       never (> i 11))
	=>  T
	
	 (loop never t
	       finally (print "you won't see this"))
	=>  NIL
 )

(dotests THEREIS-CLAUSE
	;; If I exceeds 10 return I; otherwise, return NIL.
	;; The THEREIS construct terminates this loop.
	 (loop for i from 0
	       thereis (when (> i 10) i) )
	=>  11
	
	 (loop thereis "Here is my value"
	       finally (print "you won't see this"))
	=>  "Here is my value"
	;; The FOR construct terminates this loop, so the FINALLY clause 
	;; is evaluated.
	 (loop for i from 1 to 10
	       thereis (> i 11)
	       finally (prin1 'got-here))
	;;>>  GOT-HERE
	=>  NIL
)

(dotests WHILE-CLAUSE
	;; Collect the length and the items of STACK.
	 (let ((stack '(a b c d e f)))
	   (loop for item = (length stack) then (pop stack)
	         collect item
	         while stack))
	=>  (6 A B C D E F)
	 
	;; Use WHILE to terminate a loop that otherwise wouldn't terminate.
	;; Note that WHILE occurs after the WHEN.
	 (loop for i fixnum from 3
	       when (oddp i) collect i
	       while (< i 5))
	=>  (3 5)
)

(dotests DO-CLAUSE ;; 6.1.5.1
	;; Print numbers and their squares.
	;; The DO construct applies to multiple forms.
	 (loop for i from 1 to 3
	       do (print i)
	          (print (* i i)))
	;;>>  1 
	;;>>  1 
	;;>>  2 
	;;>>  4 
	;;>>  3 
	;;>>  9 
	=>  NIL
)

(dotests WHEN-CLAUSE
	#|
	;; Signal an exceptional condition.
	 (loop for item in '(1 2 3 a 4 5)
	       when (not (numberp item))
	        return (cerror "enter new value" "non-numeric value: ~s" item))
	Error: non-numeric value: A
	 
	;; The previous example is equivalent to the following one.
	 (loop for item in '(1 2 3 a 4 5)
	       when (not (numberp item))
	        do (return 
	            (cerror "Enter new value" "non-numeric value: ~s" item)))
	Error: non-numeric value: A
	|#
	
	;; This example parses a simple printed string representation from 
	;; BUFFER (which is itself a string) and returns the index of the
	;; closing double-quote character.
	 (let ((buffer "\"a\" \"b\""))
	   (loop initially (unless (char= (char buffer 0) #\")
	                     (loop-finish))
	         for i of-type fixnum from 1 below (length (the string buffer))
	         when (char= (char buffer i) #\")
	          return i))
	=>  2
	 
	;; The collected value is returned.
	 (loop for i from 1 to 10
	       when (> i 5)
	         collect i
	       finally (prin1 'got-here))
	;;>>  GOT-HERE
	=>  (6 7 8 9 10) 
	
	;; Return both the count of collected numbers and the numbers.
	 (loop for i from 1 to 10
	       when (> i 5)
	         collect i into number-list
	         and count i into number-count
	       finally (return (values number-count number-list)))
	=>  (values 5 (6 7 8 9 10))
)

(dotests NAMED-CLAUSE
	;; Just name and return.
	 (loop named max
	       for i from 1 to 10
	       do (print i)
	       do (return-from max 'done))
	;;>>  1 
	=>  DONE
)

(dotests MISC-CLAUSES
	 (let ((i 0))                     ; no loop keywords are used
	    (loop (incf i) (if (= i 3) (return i)))) =>  3
	 (let ((i 0)(j 0))
	    (tagbody
	      (loop (incf j 3) (incf i) (if (= i 3) (go exit)))
	      exit)
	    j) =>  9
	
	;;In the following example, the variable x is stepped before y is stepped; 
	;; thus, the value of y reflects the updated value of x: 
	 (loop for x from 1 to 10 
	       for y = nil then x 
	       collect (list x y))
	=>  ((1 NIL) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9) (10 10))
	
	;;In this example, x and y are stepped in parallel: 
	 (loop for x from 1 to 10 
	       and y = nil then x 
	       collect (list x y))
	=>  ((1 NIL) (2 1) (3 2) (4 3) (5 4) (6 5) (7 6) (8 7) (9 8) (10 9))
)	

(dotests CLAUSE-GROUPING
	;; Group conditional clauses.
	 (loop for i in '(1 324 2345 323 2 4 235 252)
	       when (oddp i)
	         do (print i)
	         and collect i into odd-numbers
	         and do (terpri)
	       else                              ; I is even.
	         collect i into even-numbers
	       finally
	         (return (values odd-numbers even-numbers)))
	;;>>  1 
	;;>>  
	;;>>  2345 
	;;>>  
	;;>>  323 
	;;>>  
	;;>>  235 
	=>  (values (1 2345 323 235) (324 2 4 252))
	
	;; Collect numbers larger than 3.
	 (loop for i in '(1 2 3 4 5 6)
	       when (and (> i 3) i)
	       collect it)                      ; IT refers to (and (> i 3) i).
	=>  (4 5 6)
	 
	;; Find a number in a list.
	 (loop for i in '(1 2 3 4 5 6)
	       when (and (> i 3) i)
	       return it)
	=>  4
	     
	;; The above example is similar to the following one.
	 (loop for i in '(1 2 3 4 5 6)
	       thereis (and (> i 3) i))
	=>  4
	
	
	;; Nest conditional clauses.
	 (let ((list '(0 3.0 apple 4 5 9.8 orange banana)))
	   (loop for i in list
	         when (numberp i)
	           when (floatp i)
	             collect i into float-numbers
	           else                                  ; Not (floatp i)
	             collect i into other-numbers
	         else                                    ; Not (numberp i)
	           when (symbolp i) 
	             collect i into symbol-list
	           else                                  ; Not (symbolp i)
	             do (error "found a funny value in list ~S, value ~S~%" list i)
	         finally (return (values float-numbers other-numbers symbol-list))))
	=>  (values (3.0 9.8) (0 4 5) (APPLE ORANGE BANANA))
	
	;; Without the END preposition, the last AND would apply to the
	;; inner IF rather than the outer one.
	 (loop for x from 0 to 3 
	       do (print x)
	       if (zerop (mod x 2))
	         do (princ " a")
	          and if (zerop (floor x 2))
	                do (princ " b")
	                end
	          and do (princ " c"))
	;;>>  0  a b c
	;;>>  1 
	;;>>  2  a c
	;;>>  3 
	=>  NIL
)

(dotests DO/DO*
	 (do ((temp-one 1 (1+ temp-one))
	       (temp-two 0 (1- temp-two)))
	      ((> (- temp-one temp-two) 5) temp-one)) =>  4
	
	 (do ((temp-one 1 (1+ temp-one))
	       (temp-two 0 (1+ temp-one)))     
	      ((= 3 temp-two) temp-one)) =>  3
	
	 (do* ((temp-one 1 (1+ temp-one))
	        (temp-two 0 (1+ temp-one)))
	       ((= 3 temp-two) temp-one)) =>  2                     
#|	
	 (do ((j 0 (+ j 1)))
	     (nil)                       ;Do forever.
	   (format t "~%Input ~D:" j)
	   (let ((item (read)))
	     (if (null item) (return)   ;Process items until NIL seen.
	         (format t "~&Output ~D: ~S" j item))))
	;;>>  Input 0: banana
	;;>>  Output 0: BANANA
	;;>>  Input 1: (57 boxes)
	;;>>  Output 1: (57 BOXES)
	;;>>  Input 2: NIL
	=>  NIL
|#	
	 (setq a-vector (vector 1 nil 3 nil)) => #(1 nil 3 nil) ;; RGC
	 (do ((i 0 (+ i 1))     ;Sets every null element of a-vector to zero.
	      (n (array-dimension a-vector 0)))
	     ((= i n))
	   (when (null (aref a-vector i))
	     (setf (aref a-vector i) 0))) =>  NIL
	a-vector =>  #(1 0 3 0)
	
	 (defun ribcage-lookup (sym ribcage)           
	        (do ((r ribcage (cdr r)))
	            ((null r) nil)
	          (do ((s (caar r) (cdr s))
	               (v (cdar r) (cdr v))) 
	              ((null s))
	            (when (eq (car s) sym)
	              (return-from ribcage-lookup (car v)))))) =>  RIBCAGE-LOOKUP
)

(dotests DOTIMES
	 (dotimes (temp-one 10 temp-one)) =>  10
	 (setq temp-two 0) =>  0
	 (dotimes (temp-one 10 t) (incf temp-two)) =>  T
	 temp-two =>  10
	
	;;; True if the specified subsequence of the string is a
	;;; palindrome (reads the same forwards and backwards).
	 (defun palindromep (string &optional
	                           (start 0)
	                           (end (length string)))
	   (dotimes (k (floor (- end start) 2) t)
	    (unless (char-equal (char string (+ start k))
	                        (char string (- end k 1)))
	      (return nil)))) => PALINDROMEP
	 (palindromep "Able was I ere I saw Elba") =>  T
	 (palindromep "A man, a plan, a canal--Panama!") =>  NIL
	 (remove-if-not #'alpha-char-p          ;Remove punctuation.
	               "A man, a plan, a canal--Panama!")
	=>  "AmanaplanacanalPanama"
	 (palindromep
	  (remove-if-not #'alpha-char-p
	                "A man, a plan, a canal--Panama!")) =>  T
	 (palindromep
	  (remove-if-not
	   #'alpha-char-p
	   "Unremarkable was I ere I saw Elba Kramer, nu?")) =>  T
	 (palindromep
	  (remove-if-not
	   #'alpha-char-p
	   "A man, a plan, a cat, a ham, a yak,
	                  a yam, a hat, a canal--Panama!")) =>  T
)

(dotests DOLIST
	 (setq temp-two '()) =>  NIL
	 (dolist (temp-one '(1 2 3 4) temp-two) (push temp-one temp-two)) =>  (4 3 2 1)
	
	 (setq temp-two 0) =>  0
	 (dolist (temp-one '(1 2 3 4)) (incf temp-two)) =>  NIL
	 temp-two =>  4
	
	 (dolist (x '(a b c d)) (prin1 x) (princ " ")) 
	;;>>  A B C D 
	=>  NIL
)

(dotests LOOP
	;; An example of the simple form of LOOP.
	 (defun sqrt-advisor ()
	   (loop (format t "~&Number: ")
	         (let ((n (parse-integer (read-line) :junk-allowed t)))
	           (when (not n) (return))
	           (format t "~&The square root of ~D is ~D.~%" n (sqrt n)))))
	=>  SQRT-ADVISOR
	#|
	 (sqrt-advisor)
	;;>>  Number: 5<NEWLINE>
	;;>>  The square root of 5 is 2.236068.
	;;>>  Number: 4<NEWLINE>
	;;>>  The square root of 4 is 2.
	;;>>  Number: done<NEWLINE>
	=>  NIL
	|#
	;; An example of the extended form of LOOP.
	 (defun square-advisor ()
	   (loop as n = (progn (format t "~&Number: ")
	                       (parse-integer (read-line) :junk-allowed t))
	         while n
	         do (format t "~&The square of ~D is ~D.~%" n (* n n))))
	=>  SQUARE-ADVISOR
	#|
	 (square-advisor)
	;;>>  Number: 4<NEWLINE>
	;;>>  The square of 4 is 16.
	;;>>  Number: 23<NEWLINE>
	;;>>  The square of 23 is 529.
	;;>>  Number: done<NEWLINE>
	=>  NIL
	|#
	;; Another example of the extended form of LOOP.
	 (loop for n from 1 to 10
	       when (oddp n)
	         collect n)
	=>  (1 3 5 7 9)
)

(dotests LOOP-FINISH
	;; Terminate the loop, but return the accumulated count.
	 (loop for i in '(1 2 3 stop-here 4 5 6)
	       when (symbolp i) do (loop-finish)
	       count i)
	=>  3
	 
	;; The preceding loop is equivalent to:
	 (loop for i in '(1 2 3 stop-here 4 5 6)
	       until (symbolp i)
	       count i)
	=>  3
	
	;; While LOOP-FINISH can be used can be used in a variety of 
	;; situations it is really most needed in a situation where a need
	;; to exit is detected at other than the loop's `top level'
	;; (where UNTIL or WHEN often work just as well), or where some 
	;; computation must occur between the point where a need to exit is
	;; detected and the point where the exit actually occurs.  For example:
	 (defun tokenize-sentence (string)
	   (macrolet ((add-word (wvar svar)
	                `(when ,wvar
	                   (push (coerce (nreverse ,wvar) 'string) ,svar)
	                   (setq ,wvar nil))))
	     (loop with word = '() and sentence = '() and endpos = nil
	           for i below (length string)
	           do (let ((char (aref string i)))
	                (case char
	                  (#\Space (add-word word sentence))
	                  (#\. (setq endpos (1+ i)) (loop-finish))
	                  (otherwise (push char word))))
	           finally (add-word word sentence)
	                   (return (values (nreverse sentence) endpos)))))
	=>  TOKENIZE-SENTENCE
	 
	 (tokenize-sentence "this is a sentence. this is another sentence.")
	=>  (values ("this" "is" "a" "sentence") 19)
	 
	 (tokenize-sentence "this is a sentence")
	=>  (values ("this" "is" "a" "sentence") NIL)
)

