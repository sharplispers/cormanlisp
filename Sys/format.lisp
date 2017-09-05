;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		format.lisp
;;;;	Contents:	Common Lisp FORMAT function implementation
;;;;	Author:		Roger Corman
;;;;	History:	??/??/96	RGC  Created.
;;;;				9/3/99		RGC  Implemented ~P modifiers (: and @).
;;;;				1/31/01     RGC  Added Chris Double's fix for FORMAT ~@[...] directive
;;;;				12/14/01    RGC  Fixed a bug with ~[ expression (reported by jmarshall)
;;;;

(in-package :common-lisp)
(provide :format)

;; need to override warning here
(setq *COMPILER-WARN-ON-UNDEFINED-FUNCTION* nil)

(defun char-name (char) ())		;; stub, defined later

(defun format (dest control-string &rest arguments)
	(let ((return-value nil))
		;; check for dest equal to t or nil
		(cond 
			((null dest) 
			 (progn 
				(setf dest (make-string-output-stream))
				(setf return-value dest)))
			((eq dest t) (setf dest *standard-output*)))
		(if (stringp dest)
			(with-output-to-string (stream dest)
				(if (functionp control-string)
					(apply control-string stream arguments)
					(catch '%format-up-and-out
						(%format-list stream control-string arguments))))				
			(catch '%format-up-and-out
				(if (functionp control-string)
					(apply control-string dest arguments)
					(%format-list dest control-string arguments))))
		(if return-value (get-output-stream-string return-value))))

;;;
;;; This is like FORMAT, but for use by FORMATTER.
;;;
(defun format-internal (dest control-string &rest arguments)
	(let ((arg-index 
				(catch '%format-up-and-out
					(%format-list dest control-string arguments))))
		(nthcdr arg-index arguments)))

(defun %format-list (dest control-string arguments &optional (arg-index 0))
  ;; scan control string and dispatch to output functions
  (do ((index 0)
       (orig-arg-index arg-index)
       (length (length control-string))
       (atsign-modifier nil nil)
       (colon-modifier nil nil)
       dispatch-func
       (parameters nil)
       control
       args-used
       char)
      ((>= index length) (- arg-index orig-arg-index))
    (setf char (char control-string index))
    (if (char= char #\~)
        ;; process directive
        (progn
          ;; get parameters
          (incf index)
          (multiple-value-setq (parameters index args-used) 
            (%get-params control-string index arguments arg-index))
          (incf arg-index args-used)

          ;; check for modifiers
          (dotimes (i 2)
            (if (>= index length) (return))
            (setq char (char control-string index))
            (cond ((char= char #\@)(setq atsign-modifier t))
                  ((char= char #\:)(setq colon-modifier t))
                  (t (return)))
            (incf index))

          ;; the next character should be the format
          ;; directive character
          (if (>= index length)
              (error "Invalid format directive: ~A" control-string))
          (setq char (char control-string index))
          (incf index)
          (setf dispatch-func 
                (%get-format-dispatch-func char))
          (if (null dispatch-func)
              (error "Invalid format directive : character ~S in control string ~S" 
                     char control-string))
          (setq control (list control-string index))
          (setq arg-index 
                (apply dispatch-func 
                       dest 
                       arguments arg-index 
                       atsign-modifier colon-modifier 
                       control
                       parameters))
          (setq index (cadr control))) 		

        ;; just output the character
        (progn
          (write-char char dest)
          (incf index)))))

(setq *COMPILER-WARN-ON-UNDEFINED-FUNCTION* t)

;;;
;;;
;;;	Returns two values: the list of params found and the
;;; updated index.
;;
(defun %get-params (control-string index arguments arg-index &aux (params nil)(args-used 0))
	(do ((int nil nil)
		 c
		 (length (length control-string)))
		((>= index length))
		(if (char= (char control-string index) #\Newline)
			(return))
		(if (eql (char control-string index) #\')
			(progn
				(setq int (char control-string (+ index 1)))
				(incf index 2))
			(if (eql (char-upcase (char control-string index)) #\V)
				(progn
					(setq int (elt arguments arg-index))
					(incf arg-index)
					(incf args-used)
					(incf index)) 
				(if (eql (char control-string index) #\#)
					(progn
						(setq int (- (length arguments) arg-index))
						(incf index))
					(multiple-value-setq (int index) 
						(parse-integer control-string :start index
							:junk-allowed t)))))
		(setq c (char control-string index))
		(if int 
			(push int params)
			(if (char= c #\,) 
				(push nil params)))
		(if (char= c #\,) (incf index) (return)))
	(values (nreverse params) index args-used))

(defun %format-integer (stream int radix atsign-modifier colon-modifier 
				mincol padchar commachar)

		;; initialize defaults
		(unless mincol (setq mincol 0))
		(setq padchar (if padchar (if (integerp padchar) (int-char padchar) padchar) #\Space))
		(setq commachar (if commachar (if (integerp commachar) (int-char commachar) commachar) #\,))

		(let ((*print-base* radix)
			  (*print-radix* nil)
			  s
			  (length 0)
			  sign)

			(if (and atsign-modifier (plusp int))
				(progn (setf sign #\+) (incf length))
				(if (minusp int)
					(progn (setf sign #\-) (incf length) (setf int (- int)))))

			(setq s (with-output-to-string (x) (princ int x)))
			(incf length (length s))
			(if colon-modifier 
				(incf length (truncate (1- (length s)) 3)))
			(if (< length mincol)
				(dotimes (i (- mincol length))
					(write-char padchar stream)))

			(if sign (write-char sign stream))

			(if colon-modifier
				(dotimes (i (length s))
					(write-char (char s i) stream)
					(let* ((digits-left (- (length s) (1+ i)))
						   (digit-pos (mod digits-left 3)))
						(if (and (zerop digit-pos) (plusp digits-left))
							(write-char commachar stream))))
				(princ s stream))))  

(defconstant *format-cardinals*
	#( "zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten"
	  "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety" "hundred"
	  "thousand" "million" "billion" "trillion"))

(defun %format-cardinal-number (int stream)
		(if (zerop int) (return-from %format-cardinal-number (princ "zero" stream)))
		(if (minusp int) 
			(progn (princ "negative " stream) (setq int (- int))))
		(cond
			((< int 20)
			 (princ (nth int '("zero" "one" "two" "three" "four" "five" 
					"six" "seven" "eight" "nine" "ten" "eleven" "twelve" "thirteen"
					"fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen")) 
				stream))
			((< int 100)
			 (princ (nth (- (truncate int 10) 2) '("twenty" "thirty" "forty"
							"fifty" "sixty" "seventy" "eighty" "ninety")) stream)
			 (if (plusp (mod int 10)) 
				(progn 
					(write-char #\- stream)
					(%format-cardinal-number (mod int 10) stream))))
			((< int 1000)
			 (%format-cardinal-number (truncate int 100) stream)
			 (princ " hundred" stream)
			 (if (plusp (mod int 100))
				(progn	
					(write-char #\Space stream)		 
					(%format-cardinal-number (mod int 100) stream))))
			((< int 1000000)
			 (%format-cardinal-number (truncate int 1000) stream)
			 (princ " thousand" stream)
			 (if (plusp (mod int 1000))
				(progn	
					(write-char #\Space stream)		 
					(%format-cardinal-number (mod int 1000) stream))))
#|
			((< int 1000000000)
			 (%format-cardinal-number (truncate int 1000000) stream)
			 (princ " million" stream)
			 (if (plusp (mod int 1000000))
				(progn	
					(write-char #\Space stream)		 
					(%format-cardinal-number (mod int 1000000) stream))))
|#
			(t (princ "billions"))))

(defun %format-ordinal-number (int stream)
	(princ "Sorry" stream))

(defun %format-roman-numeral (int stream)
	(princ "Sorry" stream))

(defun %format-old-roman-numeral (int stream)
	(princ "Sorry" stream))
		
;;; Format dispatch functions take a stream, argument list,
;;; @-modifier and :-modifier arguments, followed by any passed
;;; parameters. Any passed parameters which are nil should be
;;; assumed to be requesting the default. The dispatch functions
;;; should return the remaining argument list (missing the
;;; arguments that they processed.
;;;

(defvar *format-functions* #256())

(defun %set-format-dispatch-func (char func)
	(let ((index (char-code (char-upcase char))))
		(setf (elt *format-functions* index) func)))

(defun %get-format-dispatch-func (char)
	(let ((index (char-code (char-upcase char))))
		(elt *format-functions* index)))

(%set-format-dispatch-func #\A 
	#'(lambda (stream args index atsign-modifier colon-modifier control
				&optional mincol colinc 
						minpad padchar)
		(setq args (nthcdr index args))
		(if (null args) 
			(error "Not enough args for ~AA format directive" #\~))

		;; initialize defaults
		(unless mincol (setq mincol 0))
		(unless colinc (setq colinc 1))
		(unless minpad (setq minpad 0))
		(setq padchar (if padchar (if (integerp padchar) (int-char padchar) padchar) #\Space))

		(let ((*print-escape* nil)
		  	  (arg (car args)))
			(if (and (null arg) colon-modifier)
				(setq arg "()"))
			(if atsign-modifier
				;; needto output to string to insert padding in front
				(let ((s (with-output-to-string (x) (princ arg x))) 
					  length)
					(dotimes (i minpad) (write-char padchar stream))
					(setq length (length s))
 					(incf length minpad)
					(do ()
						((>= length mincol))
						(dotimes (i colinc) (write-char padchar stream))
						(incf length colinc))
					(princ s stream))
				(let (length (start-pos (stream-column stream)))
					(princ arg stream)
					(setq length (- (stream-column stream) start-pos))
					(if (< length 0) (setq length 0))
					(dotimes (i minpad) (write-char padchar stream))
 					(incf length minpad)
					(do ()
						((>= length mincol))
						(dotimes (i colinc) (write-char padchar stream))
						(incf length colinc)))))
			(1+ index)))

(%set-format-dispatch-func #\S 
	#'(lambda (stream args index atsign-modifier colon-modifier control
				&optional mincol colinc 
						minpad padchar)
		(setq args (nthcdr index args))
		(if (null args) 
			(error "Not enough args for ~AS format directive" #\~))

		;; initialize defaults
		(unless mincol (setq mincol 0))
		(unless colinc (setq colinc 1))
		(unless minpad (setq minpad 0))
		(setq padchar (if padchar (if (integerp padchar) (int-char padchar) padchar) #\Space))

		(let ((*print-escape* t)
		  	  (arg (car args)))
			(if (and (null arg) colon-modifier)
				(setq arg "()"))
			(if atsign-modifier
				;; need to output to string to insert padding in front
				(let ((s (with-output-to-string (x) (prin1 arg x))) 
					  length)
					(dotimes (i minpad) (write-char padchar stream))
					(setq length (length s))
 					(incf length minpad)
					(do ()
						((>= length mincol))
						(dotimes (i colinc) (write-char padchar stream))
						(incf length colinc))
					(princ s stream))
				(let (length (start-pos (stream-column stream)))
					(prin1 arg stream)
					(setq length (- (stream-column stream) start-pos))
					(if (< length 0) (setq length 0))
					(dotimes (i minpad) (write-char padchar stream))
 					(incf length minpad)
					(do ()
						((>= length mincol))
						(dotimes (i colinc) (write-char padchar stream))
						(incf length colinc)))))
			(1+ index)))

(%set-format-dispatch-func #\D 
	#'(lambda (stream args index atsign-modifier colon-modifier control 
				&optional mincol padchar commachar)
        (block nil
    		(let ((save-args args))
    			(setq args (nthcdr index args))
    			(if (null args) 
    				(error "Not enough args for ~~D format directive"))
    	
    			;; if not an integer use ~A output
    			(if (not (integerp (car args)))
    				(let ((*print-base* 10))
    					(return (funcall (%get-format-dispatch-func #\A)
    							stream save-args index atsign-modifier
    							colon-modifier control mincol 1 padchar commachar))))
    	
    			(%format-integer stream (car args) 10 atsign-modifier colon-modifier
    					mincol padchar commachar)
    			(1+ index)))))

(%set-format-dispatch-func #\B 
	#'(lambda (stream args index atsign-modifier colon-modifier control 
				&optional mincol padchar commachar)
        (block nil
    		(setq args (nthcdr index args))
    		(if (null args) 
    			(error "Not enough args for ~AB format directive" #\~))
    
    		;; if not an integer use ~A output
    		(if (not (integerp (car args)))
    			(let ((*print-base* 2))
    				(return (apply (%get-format-dispatch-func #\A)
    						stream args atsign-modifier
    						colon-modifier mincol nil nil padchar))))
    
    		(%format-integer stream (car args) 2 atsign-modifier colon-modifier
    				mincol padchar commachar)
    		(1+ index))))

(%set-format-dispatch-func #\O 
	#'(lambda (stream args index atsign-modifier colon-modifier control 
				&optional mincol padchar commachar)
        (block nil
    		(setq args (nthcdr index args))
    		(if (null args) 
    			(error "Not enough args for ~AO format directive" #\~))
    
    		;; if not an integer use ~A output
    		(if (not (integerp (car args)))
    			(let ((*print-base* 8))
    				(return (apply (%get-format-dispatch-func #\A)
    						stream args atsign-modifier
    						colon-modifier mincol nil nil padchar))))
    
    		(%format-integer stream (car args) 8 atsign-modifier colon-modifier
    				mincol padchar commachar)
    		(1+ index))))

(%set-format-dispatch-func #\X 
	#'(lambda (stream args index atsign-modifier colon-modifier control 
				&optional mincol padchar commachar)
        (block nil
    		(setq args (nthcdr index args))
    		(if (null args) 
    			(error "Not enough args for ~AX format directive" #\~))
    
    		;; if not an integer use ~A output
    		(if (not (integerp (car args)))
    			(let ((*print-base* 16))
    				(return (apply (%get-format-dispatch-func #\A)
    						stream args atsign-modifier
    						colon-modifier mincol nil nil padchar))))
    
    		(%format-integer stream (car args) 16 atsign-modifier colon-modifier
    				mincol padchar commachar)
    		(1+ index))))

(%set-format-dispatch-func #\R 
	#'(lambda (stream args index atsign-modifier colon-modifier control 
				&optional radix mincol padchar commachar)
        (block nil
    		(setq args (nthcdr index args))
    		(if (null args) 
    			(error "Not enough args for ~AR format directive" #\~))
    		
    		(if radix
    			;; if not an integer use ~A output
    			(progn
    				(if (not (integerp (car args)))
    					(let ((*print-base* radix))
    						(return (apply (%get-format-dispatch-func #\A)
    									args atsign-modifier
    									colon-modifier mincol nil nil padchar))))
    				(unless (and (plusp radix) (<= radix 36))
    					(error "Invalid radix specified: ~A" radix))
    				(%format-integer stream (car args) radix atsign-modifier colon-modifier
    					mincol padchar commachar))
    			(progn
    				(if (not (integerp (car args)))
    					(return (apply (%get-format-dispatch-func #\A)
    									args atsign-modifier
    									colon-modifier mincol nil nil padchar)))
    				(cond
    					((and atsign-modifier colon-modifier) 
    				 	 (%format-old-roman-numeral (car args) stream))
    					(atsign-modifier (%format-roman-numeral (car args) stream))
    					(colon-modifier (%format-ordinal-number (car args) stream))
    					(t (%format-cardinal-number (car args) stream)))))
    		(1+ index))))

(%set-format-dispatch-func #\~ 
	#'(lambda (stream args index atsign-modifier colon-modifier control 
				&optional num)
		(unless num (setq num 1))
		(dotimes (i num)
			(write-char #\~ stream))
		index))

(%set-format-dispatch-func #\% 
	#'(lambda (stream args index atsign-modifier colon-modifier control 
				&optional num)
		(unless num (setq num 1))
		(dotimes (i num)
			(write-char #\Newline stream))
		index))

(%set-format-dispatch-func #\P
	#'(lambda (stream args index atsign-modifier colon-modifier control)
		(when colon-modifier 
			(decf index)
			(if (< index 0)
				(error "No preceding argument for :P modifer to format string")))
		(setq args (nthcdr index args))
		(if (not (eql (car args) 1))
			(if atsign-modifier
				(write-string "ies" stream)
				(write-char #\s stream))
			(if atsign-modifier
				(write-string "y" stream)))			
		(1+ index)))

(%set-format-dispatch-func #\Newline 
	#'(lambda (stream args index atsign-modifier colon-modifier control )
		index))

;;; redefined later for proper floating point handling
(%set-format-dispatch-func #\F 
	#'(lambda (stream args index atsign-modifier colon-modifier control 
				&optional width digits scale overflow-char padchar)
		(setq args (nthcdr index args))
		(if (null args) 
			(error "Not enough args for ~~F format directive"))

		;; initialize defaults
		(unless width (setq width -1))
		(unless digits (setq digits 1))
		(unless scale (setq scale 0))
		(setq overflow-char 
			(if overflow-char 
				(if (integerp overflow-char) (int-char overflow-char) overflow-char) 
				#\Space))
		(setq padchar (if padchar (if (integerp padchar) (int-char padchar) padchar) #\Space))

		(print-float (car args) stream :fixed width digits
				scale padchar atsign-modifier)
		(1+ index)))

;;; redefined later for proper floating point handling
(%set-format-dispatch-func #\G 
	#'(lambda (stream args index atsign-modifier colon-modifier control 
				&optional width digits exp-digits scale overflow-char padchar
					exponent-char)
		(setq args (nthcdr index args))
		(if (null args) 
			(error "Not enough args for ~~G format directive"))

		;; initialize defaults
		(unless width (setq width -1))
		(unless digits (setq digits 1))
		(unless exp-digits (setq exp-digits 2))
		(unless scale (setq scale 0))
		(setq overflow-char 
			(if overflow-char 
				(if (integerp overflow-char) (int-char overflow-char) overflow-char) #\Space))
		(setq padchar (if padchar (if (integerp padchar) (int-char padchar) padchar) #\Space))
		(setq exponent-char 
			(if exponent-char 
				(if (integerp exponent-char) (int-char exponent-char) exponent-char) #\E))

		(print-float (car args) stream :general width digits
				scale padchar atsign-modifier)
		(1+ index)))

;;; redefined later for proper floating point handling
(%set-format-dispatch-func #\E 
	#'(lambda (stream args index atsign-modifier colon-modifier control 
				&optional width digits exp-digits scale overflow-char padchar
					exponent-char)
		(setq args (nthcdr index args))
		(if (null args) 
			(error "Not enough args for ~~E format directive"))

		;; initialize defaults
		(unless width (setq width -1))
		(unless digits (setq digits 1))
		(unless exp-digits (setq exp-digits 2))
		(unless scale (setq scale 0))
		(setq overflow-char 
			(if overflow-char 
				(if (integerp overflow-char) (int-char overflow-char) overflow-char) #\Space))
		(setq padchar (if padchar (if (integerp padchar) (int-char padchar) padchar) #\Space))
		(setq exponent-char 
			(if exponent-char 
				(if (integerp exponent-char) (int-char exponent-char) exponent-char) #\E))

		(print-float (car args) stream :exponential width digits
				scale padchar atsign-modifier)
		(1+ index)))

(%set-format-dispatch-func #\? 
	#'(lambda (stream args index atsign-modifier colon-modifier control 
				&optional width digits scale overflow-char padchar)
		(setq args (nthcdr index args))
		(if (or (null args) (null (cdr args)))
			(error "Not enough args for ~~? format directive"))
		(let ((str (make-string-output-stream)))
			(%format-list str (car args) (cadr args))
			(write-string (get-output-stream-string str) stream))
		(+ index 2)))

(defun find-matching-right-brace (string startpos)
	(do ((i startpos (+ i 1))
		 (endpos (length string))
		 (nesting 1)
		 tilda-pos)
		((= i (- endpos 1)))
		(when (char= (char string i) #\~)
			(setf tilda-pos i)
			(incf i)
			(let ((c (char string i)))
				;; skip past any modifier characters
				(do ()
					((or (>= i (- endpos 1))
							(not (member c '(#\@ #\:)))))	;; any others allowed here?
					(incf i)
					(setf c (char string i)))
				(cond ((char= c #\{)(incf nesting))
					  ((char= c #\})(decf nesting)(if (zerop nesting)(return tilda-pos))))))))
					
(%set-format-dispatch-func #\{ 
	#'(lambda (stream args index atsign-modifier colon-modifier control)
        (block nil
    		(setq args (nthcdr index args))
    		(unless args 
    			(error "Not enough args for ~~{ format directive"))
    		(unless (or (listp (car args)) atsign-modifier)
    			(error "Invalid format argument--should be a list"))
    	
    		(let ((end-brace-index (find-matching-right-brace (car control) (cadr control)))
    			  string)
    			(if end-brace-index
    				(setq string (subseq (car control) (cadr control) end-brace-index))
    				(error "Missing ~~} following ~~{ in format string"))
    			(setf (cadr control) (+ 2 end-brace-index))
    			(cond 
    				((and colon-modifier atsign-modifier)
    					(return 
    						(do ((arg-index 0))
    							((>= arg-index (length args)) (+ index arg-index))
    							(%format-list stream string (nth arg-index args))
    							(incf arg-index))))
    				(colon-modifier
    					(return
    						(do ((arg-index 0))
    							((>= arg-index (length (car args))) (1+ index))
    							(%format-list stream string (nth arg-index (car args)))
    							(incf arg-index))))
    				(atsign-modifier 					
    					(return 
    						(do ((arg-index 0))
    							((>= arg-index (length args)) (+ index arg-index))
    							(incf arg-index 
    								(%format-list stream string (nthcdr arg-index args))))))
    				(t 
    					(catch '%format-up-and-out
    						(do ((arg-index 0))
    							((>= arg-index (length (car args))) (1+ index))
    							(incf arg-index 
    								(%format-list stream string 
    									(nthcdr arg-index (car args))))))
    					(1+ index)))))))

;; case conversion
(defun paren-dispatch-func (stream args index atsign-modifier colon-modifier control)
     (block nil
       (setq args (nthcdr index args)) ;; skip unnecessary arguments
    
       ;; collect the characters up until a closing parentheses
       (let ((close-paren-index (search "~)" (car control) :start2 (cadr control)))
             string
             (string-stream (make-string-output-stream)))
         (if close-paren-index
             (setq string (subseq (car control) (cadr control) close-paren-index))
             (error "Missing ~~) following ~~( in format string"))
         (setf (cadr control) (+ 2 close-paren-index))
         (setf index (catch '%format-up-and-out (%format-list string-stream string args)))
         (setq string (get-output-stream-string string-stream))
         (cond 
           ((and colon-modifier atsign-modifier)
            (progn
              (setq string (string-upcase string))
              (write-string string stream)
              (return index)))
           (colon-modifier
            (progn
              (setq string (string-capitalize string))
              (write-string string stream)
              (return index)))
           ;; Done. need to fix this to only capitalize the first word
           (atsign-modifier 					
            (progn
              (setq string (string-upcase (string-downcase string) :end 1))
              (write-string string stream)
              (return index)))
           (t 
            (progn
              (setq string (string-downcase string))
              (write-string string stream)
              (return index)))))))

(%set-format-dispatch-func #\( 
 'paren-dispatch-func)

;; locates either ~; or ~:;
;; returns its position, and true if a : modifier was found, nil otherwise
(defun find-format-option-separators (string startpos)
	(do ((i startpos (+ i 1))
		 (endpos (length string))
		 tilda-pos
		 (colon-modifier nil))
		((> i (- endpos 2)))
		(when (char= (char string i) #\~)
			(setf tilda-pos i)
			(incf i)
			(let ((c (char string i)))
				;; skip past any modifier characters
				(when (char= c #\:)
					(setf colon-modifier t)
					(incf i)
					(setf c (char string i)))
				(if (char= c #\;)
					(return (values tilda-pos colon-modifier)))))))

;;; Returns the index of the next format directive, or NIL if none found.
;;; The index returned will be of the actual character i.e. "~A" will return 1 (the index of #\A)
;;;
(defun find-format-directive (control-string index)
    (let ((length (length control-string)))
        (do* ((ch (char control-string index) (char control-string index)))
            ((char= ch #\~))
            (incf index)
            (if (= index length) (return-from find-format-directive nil)))
        (incf index)    ;; skip tilda character
    
        ;; scan past parameters
        (do* ((ch (char control-string index) (char control-string index)))
            ((not (or (digit-char-p ch)(char= ch #\,))))
            (incf index)
            (if (= index length) (return-from find-format-directive nil)))
        
        ;; scan past modifiers
        (do* ((ch (char control-string index) (char control-string index)))
            ((not (or (char= ch #\:)(char= ch #\@))))
            (incf index)
            (if (= index length) (return-from find-format-directive nil)))
        
        (if (= index length) nil index)))     

;;; Using the string between the ~[ and ~] or ~< and ~>, return a list of the
;; control strings separated by ~;
;;;
(defun %expr-list (string)
    (let ((position 0)
          (size (length string)) 
          (substrs '()) 
          (start 0) 
          (nesting-stack '())
          (colon-modifier-active nil))
        (do ()
            ((= position size))
            (setq position (find-format-directive string position))
            (unless position
                (let ((colon-modifier (and (> size 0)(char= (char string (- size 1)) #\:))))
                    (push (list (subseq string start size) colon-modifier-active) substrs))
                (return))
            (let ((ch (char string position)))
                (cond
                    ((or (char= ch #\[) (char= ch #\<) (char= ch #\{))
                     (push ch nesting-stack))
                    ((char= ch #\])
                     (unless (char= (pop nesting-stack) #\[)
                            (error "Error in control string: encountered an unexpected #\] character")))
                    ((char= ch #\})
                     (unless (char= (pop nesting-stack) #\{)
                            (error "Error in control string: encountered an unexpected #\} character")))
                    ((char= ch #\>)
                     (unless (char= (pop nesting-stack) #\<)
                            (error "Error in control string: encountered an unexpected #\> character")))
                    ((and (char= ch #\;) (null nesting-stack))
                     (let ((colon-modifier (and (> position 0)(char= (char string (- position 1)) #\:))))
                            (push (list (subseq string start (- position (+ 1 (if colon-modifier 1 0)))) colon-modifier-active) substrs)
                            (setq start (+ position 1))
                            (setf colon-modifier-active colon-modifier))))))
        (nreverse substrs)))

(defun format-choose-selection-from-list (selection-list index)
	(if (< index (length selection-list))
		(car (nth index selection-list))
		(progn	;; look for an else selector (indicated by ~:;)
			(dolist (x selection-list)
				(if (eq (cdr x) 't)
					(return-from format-choose-selection-from-list (car x))))
			(error "Not enough items in the FORMAT selector list.Items = ~S, index = ~D"
				selection-list index))))
;;
;; Look for terminating ~] following ~[. Watch for modifiers, nesting issues.
;;
(defun find-matching-right-bracket (string startpos)
	(do ((i startpos (+ i 1))
		 (endpos (length string))
		 (nesting 1)
		 tilda-pos)
		((= i (- endpos 1)))
		(when (char= (char string i) #\~)
			(setf tilda-pos i)
			(incf i)
			(let ((c (char string i)))
				;; skip past any modifier characters
				(do ()
					((or (>= i (- endpos 1))
							(not (member c '(#\@ #\: #\#)))))	;; any others allowed here?
					(incf i)
					(setf c (char string i)))
				(cond ((char= c #\[)(incf nesting))
					  ((char= c #\])(decf nesting)(if (zerop nesting)(return tilda-pos))))))))

;;;
;;; We found a ~<, so now we search for a matching ~>
;;;

(defun find-matching-greater-than (string position)
    (let ((size (length string)) 
          (substrs '()) 
          (nesting-stack '()))
        (do ()
            ((= position size))
            (setq position (find-format-directive string position))
            (unless position
                (return-from find-matching-greater-than nil))
            (let ((ch (char string position)))
                (cond
                    ((or (char= ch #\[) (char= ch #\<) (char= ch #\{))
                     (push ch nesting-stack))
                    ((char= ch #\])
                     (unless (char= (pop nesting-stack) #\[)
                            (error "Error in control string: encountered an unexpected #\] character")))
                    ((char= ch #\})
                     (unless (char= (pop nesting-stack) #\{)
                            (error "Error in control string: encountered an unexpected #\} character")))
                    ((and (char= ch #\>) nesting-stack)
                     (unless (char= (pop nesting-stack) #\<)
                            (error "Error in control string: encountered an unexpected #\> character")))
                    ((and (char= ch #\>) (null nesting-stack))
                     (do ((ch (char string position)(char string position)))
                         ((char= ch #\~) (return-from find-matching-greater-than position))
                         (decf position))))))))
						
;; conditional expressions
;; Note: despite some work, these expressions still do not nest correctly.
;; The inner ~[~] semicolons will be used as separators for the outer
;; braces. -RGC 10/1/99
(%set-format-dispatch-func #\[ 
	#'(lambda (stream args index atsign-modifier colon-modifier control &optional (num nil))
	;	(setq args (nthcdr index args));; skip unnecessary arguments
		(if (and (< (- (length args) index) 1) (null num))
			(error "Not enough args for ~~[ format directive"))
		;; collect the characters up until a closing brace
		(let ((close-brace-index (find-matching-right-bracket (car control) (cadr control)))
				string
				conditional-exprs
				selector
				(string-stream (make-string-output-stream)))
			(if close-brace-index
				(setq string (subseq (car control) (cadr control) close-brace-index))
				(error "Missing ~~] following ~~[ in format string"))
			(setf (cadr control) (+ 2 close-brace-index))
			(setf conditional-exprs (%expr-list string))
			(setf selector (or num (nth index args)))
			(unless (or num atsign-modifier) 
                (incf index))
			(cond 
				((and colon-modifier atsign-modifier)
					(error "~:@[ not allowed in format control string"))
				(atsign-modifier
					(if selector
						(let ((ctstring (format-choose-selection-from-list conditional-exprs 0)))
							(incf index (%format-list string-stream ctstring args index))
							(setq string (get-output-stream-string string-stream))
							(write-string string stream))
						(incf index)))
				(colon-modifier 					
					(let ((ctstring (format-choose-selection-from-list conditional-exprs
									(if selector 1 0))))
                       ; (format t "ctstring=~S, args=~A~%" ctstring args)(force-output)
						(incf index (%format-list string-stream ctstring args index))
						(setq string (get-output-stream-string string-stream))
						(write-string string stream)))
				(t 
					(let ((ctstring (format-choose-selection-from-list conditional-exprs selector)))
						(incf index (%format-list string-stream ctstring args index))
						(setq string (get-output-stream-string string-stream))
						(write-string string stream))))
			index)))


(%set-format-dispatch-func #\^
	#'(lambda (stream args index atsign-modifier colon-modifier control)
		(setq args (nthcdr index args))
		(unless args (throw '%format-up-and-out index))
		index))

;; need to override warning here for FRESH-LINE not defined yet
(setq *COMPILER-WARN-ON-UNDEFINED-FUNCTION* nil)
(%set-format-dispatch-func #\& 
	#'(lambda (stream args index atsign-modifier colon-modifier control 
				&optional num)
		(unless num (setq num 1))
		(if (>= num 1)
			(progn 
				(fresh-line stream)
				(dotimes (i (1- num))
					(terpri stream))))
		index))
(setq *COMPILER-WARN-ON-UNDEFINED-FUNCTION* t)

(%set-format-dispatch-func #\| 
	#'(lambda (stream args index atsign-modifier colon-modifier control 
				&optional num)
		(unless num (setq num 1))
		(dotimes (i num)
			(write-char (int-char 12) stream))
		index))

(%set-format-dispatch-func #\Newline 
	#'(lambda (stream args index atsign-modifier colon-modifier control)
		;; if atsign, process the newline
		(if atsign-modifier
			(terpri stream))
		;; skip whitespace
		(unless colon-modifier
			(do ((c (when (< (cadr control) (length (car control))) (char (car control) (cadr control))) 
				(when (< (cadr control) (length (car control))) (char (car control) (cadr control)))))
				((not (and c (or (char= c #\Space) (char= c #\Tab)))))
				(incf (cadr control))))
		index))

(%set-format-dispatch-func #\T 
	#'(lambda (stream args index atsign-modifier colon-modifier control 
				&optional colnum colinc)
		(unless colnum (setq colnum 1))
		(unless colinc (setq colinc 1))
		(if atsign-modifier
			(progn
				(dotimes (i colnum)
					(write-char #\Space stream))
				(dotimes (i (- colinc (mod (stream-column stream) colinc)))
					(write-char #\Space stream)))
			(let ((current-position (stream-column stream)))
				(if (> colnum current-position)
					(dotimes (i (- colnum current-position))
						(write-char #\Space stream))
					(if (> colinc 0)
						(dotimes (i (- colinc (mod (- current-position colnum) colinc)))
							(write-char #\Space stream))))))
		index))

(%set-format-dispatch-func #\C 
	#'(lambda (stream args index atsign-modifier colon-modifier control)
		(setq args (nthcdr index args))
		(if (null args) 
			(error "Not enough args for ~~C format directive"))
		(cond ((and atsign-modifier colon-modifier)
			   (let ((char-name (char-name (car args))))
					(if char-name 
						(write-string char-name stream) 
						(write-char (car args) stream))))
			  (colon-modifier
			   (let ((char-name (char-name (car args))))
					(if char-name 
						(write-string char-name stream) 
						(write-char (car args) stream))))			
			  (atsign-modifier (write (car args) :stream stream :escape t))
			  (t (write-char (car args) stream)))
		(1+ index)))

(%set-format-dispatch-func #\* 
	#'(lambda (stream args index atsign-modifier colon-modifier control 
				&optional num)
        (block nil
    		(unless num (if atsign-modifier (setq num 0) (setq num 1)))
    		(if atsign-modifier
    			(return num))
    		(if colon-modifier (return (- index num)))
    		(return (+ index num)))))

(%set-format-dispatch-func #\W 
	#'(lambda (stream args index atsign-modifier colon-modifier control)
		(declare (ignore control))
		(let ((*print-pretty* *print-pretty*)
			  (*print-level* *print-level*)
			  (*print-length* *print-length*))
			(if colon-modifier
				(setf *print-pretty* t))
			(if atsign-modifier
				(setf *print-length* nil *print-level* nil))
			(setq args (nthcdr index args))
			(write (car args) :stream stream)
			(1+ index))))

(defvar *error-trace* nil)
(defvar *ignore-errors* nil)

;(defun function-name (func)
;	(multiple-value-bind (lambda environment name)
;		(function-lambda-expression func)
;		name))

;;;;
;;;;	Common Lisp ERROR function.
;;;;    This gets redefined after conditions package loads.
;;;;
(defvar *enable-error-trace* t)

(defun error (msg &rest args)
;	(if *debug* (dump-heap "dumpheap.txt"))
	(if *ignore-errors*
		(throw 'common-lisp::%error nil)
		(progn
			(setq *evalhook* nil *applyhook* nil)
			(let* ((trace (if *enable-error-trace* (stack-trace)))
				   (func nil)
				   (errmsg (apply #'format nil msg args)))
				(setq *error-trace* trace)
				;; skip past calls to functions:
				;;    FUNCALL
				;;	  ERROR
				;;	  any function beginning with %
				(do ()
					((null trace))
					(setq func (caar trace))
					(unless (symbolp func)
						(return))
					(if (not 
							(or (eq func 'error)
								(eq func 'funcall)
								(char= (char (symbol-name func) 0) #\%)))
						(return))
					(setq trace (cdr trace)))

				(format *error-output* 
					";;; An error occurred in function ~A:~%;;; ~A~%" 
					(caar trace) errmsg)
				(force-output *error-output*)
				(throw 'common-lisp::%error nil)))))

;;;;
;;;;	Common Lisp CERROR function.
;;;;    This gets redefined after conditions package loads.
;;;;
(defun cerror (continue-format-control datum &rest arguments)
    (declare (ignore continue-format-control))
    (apply 'error datum arguments))    
    
;;;
;;;	Common Lisp BREAK function.
;;;	This is currently identical to ERROR in its implementation,
;;; except that it is not affected by IGNORE-ERRORS.
;;;
(defun break (msg &rest args)
	(progn
		(setq *evalhook* nil *applyhook* nil)
		(let* ((trace (if *enable-error-trace* (stack-trace)))
			   (func nil)
			   (errmsg (apply #'format nil msg args)))
			(setq *error-trace* trace)
			;; skip past calls to functions:
			;;    FUNCALL
			;;	  ERROR
			;;	  any function beginning with %
			(do ()
				((null trace))
				(setq func (caar trace))
				(unless (symbolp func)
					(return))
				(if (not 
						(or (eq func 'break)
							(eq func 'funcall)
							(char= (char (symbol-name func) 0) #\%)))
					(return))
				(setq trace (cdr trace)))

			(format *error-output* 
				";;; User break encountered in function ~A:~%;;; ~A~%" 
				(caar trace) errmsg)
			(force-output *error-output*)
			(throw 'common-lisp::%error nil))))

;;;;
;;;;	Common Lisp WARN function.
;;;;
(defun warn (msg &rest args)
	(let ((errmsg (apply #'format nil msg args)))
		(format *error-output* ";;; Warning: ~A~%" errmsg)
		(force-output *error-output*)))

;;;;
;;;;	Common Lisp FORMATTER macro.
;;;;
(defun formatter (string)
	#'(lambda (*standard-output* &rest arguments)
      	(apply #'format-internal *standard-output* string arguments)))

(defun justify-strings (stream strings mincol colinc minpad padchar)
  (let ((total-chars 0)
        (padding 0)
        (num-strs (length strings))
        (str (car strings)))
    (dolist (x strings) (incf total-chars (length x)))
    (setq padding (- mincol total-chars))
    (if (< padding 0) (setq padding 0))
    (dotimes (i (length str)) (%output-char (char str i) stream)
            (decf mincol))
    (if (> num-strs 1)
        (let ((pad (truncate padding (- num-strs 1))))
          (if (< pad minpad) (setq pad minpad))
          (dotimes (i pad) (%output-char padchar stream)(decf mincol))
          (justify-strings stream (cdr strings) (max mincol 0) colinc minpad
           padchar)))))

(%set-format-dispatch-func #\<
                           #'(lambda (stream args index
                                      atsign-modifier colon-modifier
                                      control &optional mincol colinc
                                      minpad padchar)
                               (declare
                                (ignore colon-modifier
                                 atsign-modifier))
                               (setq args (nthcdr index args))
                               (let ((greater-than-index
                                      (find-matching-greater-than (car control)
                                                                  (cadr control)))
                                     string
                                     embedded-exprs)
                                 (if greater-than-index
                                     (setq string
                                           (subseq (car control)
                                                   (cadr control)
                                                   greater-than-index))
                                     (error "Missing ~~> following ~< in format string"))
                                 (setf (cadr control)
                                       (+ 2 greater-than-index))
                                 (setf embedded-exprs
                                       (%expr-list string))
                                 (unless mincol (setf mincol 0))
                                 (unless colinc (setf colinc 1))
                                 (unless minpad (setf minpad 0))
                                 (setq padchar
                                       (if padchar
                                           (if (integerp padchar)
                                               (int-char padchar)
                                               padchar)
                                           #\SPACE))
                                 (let ((new-strs 'nil))
                                   (dolist (x embedded-exprs)
                                     (let ((string-stream
                                            (make-string-output-stream)))
                                       (incf index
                                             (%format-list string-stream
                                                             (car x)
                                                             (nthcdr index
                                                                     args)))
                                       (push (get-output-stream-string string-stream)
                                             new-strs)))
                                   (setq new-strs
                                         (nreverse new-strs))
                                   (if (= (length new-strs) 1)
                                       (push "" new-strs))
                                   (justify-strings stream
                                                    new-strs
                                                    mincol
                                                    colinc
                                                    minpad
                                                    padchar)
                                   index))))













