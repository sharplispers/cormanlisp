;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; parse.cl
;;
;;
;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA 
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the AllegroServe
;; prequel found in license-allegroserve.txt.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License is in the file 
;; license-lgpl.txt that was distributed with this file.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;; Suite 330, Boston, MA  02111-1307  USA
;;

;;
;; $Id: parse.cl,v 1.22 2000/05/16 14:01:25 jkf Exp $

;; Description:
;;   parsing and encoding code  

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-


(in-package :net.aserve)


;; parseobj -- used for cons-free parsing of strings
(defconstant parseobj-size 20)

(defstruct parseobj
  (start (make-array parseobj-size))  ; first charpos
  (end   (make-array parseobj-size))  ; charpos after last
  (next  0) ; next index to use
  (max  parseobj-size)
  )

(defvar *parseobjs* nil) 

(defun allocate-parseobj ()
  (let (res)
    (mp::without-scheduling 
      (if* (setq res (pop *parseobjs*))
	 then (setf (parseobj-next res) 0)
	      res
	 else (make-parseobj)))))

(defun free-parseobj (po)
  (mp::without-scheduling
    (push po *parseobjs*)))

(defun add-to-parseobj (po start end)
  ;; add the given start,end pair to the parseobj
  (let ((next (parseobj-next po)))
    (if* (>= next (parseobj-max po))
       then ; must grow it
	    (let ((ostart (parseobj-start po))
		  (oend   (parseobj-end   po)))
	      (let ((nstart (make-array (+ 10 (length ostart))))
		    (nend   (make-array (+ 10 (length ostart)))))
		(dotimes (i (length ostart))
		  (setf (svref nstart i) (svref ostart i))
		  (setf (svref nend   i) (svref oend   i)))
		(setf (parseobj-start po) nstart)
		(setf (parseobj-end   po) nend)
		(setf (parseobj-max   po) (length nstart))
		)))
  
    (setf (svref (parseobj-start po) next) start)
    (setf (svref (parseobj-end   po) next) end)
    (setf (parseobj-next po) (1+ next))
    next))

;;;;;;


















(defun parse-http-command (buffer end)
  ;; buffer is a string buffer, with 'end' bytes in it.  
  ;; return 3 values
  ;;	command  (kwd naming it or nil if bogus)
  ;;    url      uri object
  ;;    protocol  (kwd naming it or nil if bogus)
  ;;
  (let ((blankpos)
	(cmd)
	(urlstart))

    ; search for command first
    (dolist (possible *http-command-list* 
	      (return-from parse-http-command nil) ; failure
	      )
      (let ((str (car possible)))
	(if* (buffer-match buffer 0 str)
	   then ; got it
		(setq cmd (cdr possible))
		(setq urlstart (length (car possible)))
		(return))))
    
    
    (setq blankpos (find-it #\space buffer urlstart end))
    
    (if* (eq blankpos urlstart)
       then ; bogus, no url
	    (return-from parse-http-command nil))
    
    
    (if* (null blankpos)
       then ; must be http/0.9
	    (return-from parse-http-command (values cmd 
						    (parse-uri
						     (buffer-substr buffer
								   urlstart
								   end))
						    :http/0.9)))
    
    (let ((url (buffer-substr buffer urlstart blankpos))
	  (prot))
      
      ; parse url and if that fails get out right away
      (if* (null (setq url (parse-uri url)))
	 then (return-from parse-http-command nil))
      
      (if* (buffer-match buffer (1+ blankpos) "HTTP/1.")
	 then (if* (eq #\0 (schar buffer (+ 8 blankpos)))
		 then (setq prot :http/1.0)
	       elseif (eq #\1 (schar buffer (+ 8 blankpos)))
		 then (setq prot :http/1.1)))
      
      (values cmd url prot))))


(eval-when (compile load eval)
  (defun dual-caseify (str)
    ;; create a string with each characater doubled
    ;; but with upper case following the lower case
    (let ((newstr (make-string (* 2 (length str)))))
      (dotimes (i (length str))
	(setf (schar newstr (* 2 i)) (schar str i))
	(setf (schar newstr (1+ (* 2 i))) (char-upcase (schar str i))))
      newstr)))


(defparameter *header-to-slot*
    ;; headers that are stored in specific slots, we create
    ;; a list of objects to help quickly parse those slots
    '#.(let (res)
	(dolist (head *fast-headers*)
	  (push (cons
		 (dual-caseify (concatenate 'string (car head) ":"))
		 (third head))
		res))
	res))
      
(defun read-request-headers (req sock buffer)
  ;; read in the headers following the command and put the
  ;; info in the req object
  ;; if an error occurs, then return nil
  ;;
  (let ((last-value-slot nil)
	(last-value-assoc nil)
	(end))
    (loop
      (multiple-value-setq (buffer end)(read-sock-line sock buffer 0))
      (if* (null end) 
	 then ; error
	      (return-from read-request-headers nil))
      (if* (eq 0 end)
	 then ; blank line, end of headers
	      (return t))
    
      (if* (eq #\space (schar buffer 0))
	 then ; continuation of previous line
	      (if* last-value-slot
		 then ; append to value in slot
		      (setf (slot-value req last-value-slot)
			(concatenate 
			    'string
			  (slot-value req last-value-slot)
			  (buffer-substr buffer 0 end)))
	       elseif last-value-assoc
		 then (setf (cdr last-value-assoc)
			(concatenate 'string
			  (cdr last-value-assoc) (buffer-substr buffer 0 end)))
		 else ; continuation with nothing to continue
		      (return-from read-request-headers nil))
	 else ; see if this is one of the special header lines
	    
	      (setq last-value-slot nil)
	      (dolist (possible *header-to-slot*)
		(if* (buffer-match-ci buffer 0 (car possible))
		   then ; store in the slot
			(setf (slot-value req (cdr possible))
			  (concatenate 
			      'string
			    (or (slot-value req (cdr possible)) "")
			    (buffer-substr buffer
					   (1+ (ash (the fixnum 
						      (length (car possible)))
						    -1))
					   end)))
					
			(setq last-value-slot (cdr possible))
			(return)))
	    
	      (if* (null last-value-slot)
		 then ; wasn't a built in header, so put it on
		      ; the alist
		      (let ((colonpos (find-it #\: buffer 0 end))
			    (key)
			    (value))
			  
			(if* (null colonpos)
			   then ; bogus!
				(return-from read-request-headers nil)
			   else (setq key (buffer-substr
					   buffer
					   0
					   colonpos)
				      value
				      (buffer-substr
				       buffer
				       (+ 2 colonpos)
				       end)))
			; downcase the key
			(dotimes (i (length key))
			  (let ((ch (schar key i)))
			    (if* (upper-case-p ch)
			       then (setf (schar key i) 
				      (char-downcase ch)))))
			
			; now add or append
			
			(let* ((alist (request-headers req))
			       (ent (assoc key alist :test #'equal)))
			  (if* (null ent)
			     then (push (setq ent (cons key "")) alist)
				  (setf (request-headers req) alist))
			  (setf (cdr ent)
			    (concatenate 'string
			      (cdr ent)
			      value))
			  
			  (setq last-value-assoc ent)
			  )))))))



			






   
    
    
;------- header value parsing
;
; most headers value this format
;    value
;    value1, value2, value3
;    value; param=val; param2=val
;
; notes: in the comma separated lists, it's legal to use more than one
;	   comma between values in which case the intermediate "null" values
;          are ignored e.g   a,,b is the same as a,b
;
;        the semicolon introduces a parameter, it doesn't end a value.
;        the semicolon has a higher binding power than the comma,
;	 so    A; b=c; d=e, F
;           is two values, A and F, with A having parameters b=c and d=e.
;
;        A header value that doesn't follow the above rules in 
;	 the one for set-cookie
;	    set-cookie: val=yes; expires=Fri, 01-Jan-2010 08:00:00 GMT; path=/
;        note how it starts off with param=val, and then the value
;	 can have commas in it, thus we don't use comm as a separator

(defconstant ch-alpha 0)
(defconstant ch-space 1)
(defconstant ch-sep   2)  ; separators

(defvar *syntax-table*
    (let ((arr (make-array 256 :initial-element ch-alpha)))
#|      
      ; the default so we don't have to set it
      #+ignore (do ((code (char-code #\!) (1+ code)))
	  ((> code #.(char-code #\~)))
	(setf (svref arr code) ch-alpha))
|#      
      (setf (svref arr (char-code #\space)) ch-space)
#|
		      (setf (svref arr (char-code #\ff)) ch-space)
|#		
      (setf (svref arr (char-code #\tab)) ch-space)
      (setf (svref arr (char-code #\return)) ch-space)
      (setf (svref arr (char-code #\newline)) ch-space)
      
      (setf (svref arr (char-code #\,)) ch-sep)
      (setf (svref arr (char-code #\;)) ch-sep)
      (setf (svref arr (char-code #\()) ch-sep)
      
      arr))



(defun header-value-nth (parsed-value n)
  ;; return the nth value in the list of header values
  ;; a value is either a string or a list (:param value  params..)
  ;;
  ;; nil is returned if we've asked for a non-existant element
  ;; (nil is never a valid value).
  ;;
  (if* (and parsed-value (not (consp parsed-value)))
     then (error "bad parsed value ~s" parsed-value))
  
  (let ((val (nth n parsed-value)))
    (if* (atom val)
       then val
       else ; (:param value ...)
	    (cadr val))))
  

(defun header-value-member (val parsed-value)
  ;; test to see if the given value is a member of the list
  ;; of values in the parsed value.  parse the value if needed
  (setq parsed-value (ensure-value-parsed parsed-value))
  (dolist (par parsed-value)
    (if* (consp par)
       then (setq par (cadr par)))
    (if* (equalp val par)
       then (return t))))

(defun ensure-value-parsed (str &optional singlep)
  ;; parse the header value if it hasn't been parsed.
  ;; a parsed value is a cons.. easy to distinguish
  (if* (consp str) 
     then str
     else (parse-header-value str singlep)))


	      

(defun parse-header-value (str &optional singlep (start 0) (end (length str)))
  ;; scan the given string and return either a single value
  ;; or a list of values.
  ;; A single value is a string or (:param value paramval ....) for
  ;; values with parameters.  A paramval is either a string or
  ;; a cons of two strings (name . value)  which are the parameter
  ;; and its value.
  ;;
  ;; if singlep is true then we expect to see a single value which
  ;; main contain commas.  This is seen when Netscape sends
  ;; an if-modified-since header and it may in fact be a bug in 
  ;; Netscape (since parameters aren't defined for if-modified-since's value)
  ;;

  ;; split by comma first
  (let (po res)
    
    (if* singlep
       then ; don't do the comma split, make everything
	    ; one string
	    (setq po (allocate-parseobj))
	    (setf (svref (parseobj-start po) 0) start)
	    (setf (svref (parseobj-end  po) 0) end)
	    (setf (parseobj-next po) 1)
       else (setq po (split-string str #\, t nil nil start end)))
    
		    
    
    ; now for each split, by semicolon
    
    (dotimes (i (parseobj-next po))
      (let ((stindex (parseobj-next po))
	    (params)
	    (thisvalue))
	(split-string str #\; t nil po
		      (svref (parseobj-start po) i)
		      (svref (parseobj-end   po) i))
	; the first object we take whole
	(setq thisvalue (trimmed-parseobj str po stindex))
	(if* (not (equal thisvalue ""))
	   then ; ok, it's real, look for params
		(do ((i (1+ stindex) (1+ i))
		     (max (parseobj-next po))
		     (paramkey nil nil)
		     (paramvalue nil nil))
		    ((>= i max)
		     (setq params (nreverse params))
		     )
		  
		  ; split the param by =
		  (split-string str #\= t 1 po
				(svref (parseobj-start po) i)
				(svref (parseobj-end   po) i))
		  (setq paramkey (trimmed-parseobj str po max))
		  (if* (> (parseobj-next po) (1+ max))
		     then ; must have been an equal
			  (setq paramvalue (trimmed-parseobj str po
							     (1+ max))))
		  (push (if* paramvalue
			   then (cons paramkey paramvalue)
			   else paramkey)
			params)
		  
		  (setf (parseobj-next po) max))
		
		(push (if* params
			 then `(:param ,thisvalue
				       ,@params)
			 else thisvalue)
		      res))))
    
    (free-parseobj po)
    
    (nreverse res)))
    

(defun assoc-paramval (key paramvals)
  ;; search the paramvals for the given key.
  ;; this takes into account that paramvals isn't an assoc
  ;; list since the items my be strings or (string . string)
  ;; Also we use equalp as the test
  ;;
  (dolist (val paramvals)
    (if* (stringp val)
       then (if* (equalp key val)
	       then (return val))
     elseif (equal (car val) key)
       then (return val))))

  
		
		
(defun trimmed-parseobj (str po index)
  ;; return the string pointed to by the given index in 
  ;; the parseobj -- trimming blanks around both sides
  ;;
  ;; if surrounded by double quotes, trim them off too
  
  (let ((start (svref (parseobj-start po) index))
	(end   (svref (parseobj-end   po) index)))
    
    ;; trim left
    (loop
      (if* (>= start end)
	 then (return-from trimmed-parseobj "")
	 else (let ((ch (schar str start)))
		(if* (eq ch-space (svref *syntax-table*
				       (char-code ch)))
		   then (incf start)
		   else (return)))))
    
    ; trim right
    (loop
      (decf end)
      (let ((ch (schar str end)))
	(if* (not (eq ch-space (svref *syntax-table* (char-code ch))))
	   then (incf end)
		(return))))
    
    ; trim matching double quotes
    (if* (and (> end (1+ start))
	      (eq #\" (schar str start))
	      (eq #\" (schar str (1- end))))
       then (incf start)
	    (decf end))
    
    ; make string
    (let ((newstr (make-string (- end start))))
      (dotimes (i (- end start))
	(setf (schar newstr i) 
	  (schar str (+ start i))))
      
      newstr)))
    
    
		  
		  
				
		  
    

(defun split-string (str split &optional 
			       magic-parens 
			       count 
			       parseobj
			       (start 0) 
			       (end  (length str)))
  ;; divide the string where the character split occurs
  ;; return the results in parseobj object
  (let ((po (or parseobj (allocate-parseobj)))
	(st start)
	)
    ; states
    ; 0 initial, scanning for interesting char or end
    (loop
      (if* (>= start end)
	 then (add-to-parseobj po st start)
	      (return)
	 else (let ((ch (schar str start)))
		
		(if* (eq ch split)
		   then ; end this one
			(add-to-parseobj po st start)
			(setq st (incf start))
			(if* (and count (zerop (decf count)))
			   then ; get out now
				(add-to-parseobj po st end)
				(return))
		 elseif (and magic-parens (eq ch #\())
		   then ; scan until matching paren
			(let ((count 1))
			  (loop
			    (incf start)
			    (if* (>= start end)
			       then (return)
			       else (setq ch (schar str start))
				    (if* (eq ch #\))
				       then (if* (zerop (decf count))
					       then (return))
				     elseif (eq ch #\()
				       then (incf count)))))
			   
			(if* (>= start end)
			   then (add-to-parseobj po st start)
				(return))
		   else (incf start)))))
    po))


(defun split-on-character (str char &key count)
  ;; given a string return a list of the strings between occurances
  ;; of the given character.
  ;; If the character isn't present then the list will contain just
  ;; the given string.
  (let ((loc (position char str))
	(start 0)
	(res))
    (if* (null loc)
       then ; doesn't appear anywhere, just the original string
	    (list str)
       else ; must do some work
	    (loop
	      (push (subseq str start loc) res)
	      (setq start (1+ loc))
	      (if* count then (decf count))
	      (setq loc (position char str :start start))
	      (if* (or (null loc)
		       (eql 0 count))
		 then (if* (< start (length str))
			 then (push (subseq str start) res)
			 else (push "" res))
		      (return (nreverse res)))))))

    


(defun split-into-words (str)
  ;; split the given string into words (items separated by white space)
  ;;
  (let ((state 0)
	(i 0)
	(len (length str))
	(start nil)
	(res)
	(ch)
	(spacep))
    (loop
      (if* (>= i len)
	 then (setq ch #\space)
	 else (setq ch (char str i)))
      (setq spacep (eq ch-space (svref *syntax-table* (char-code ch))))
      
      (case state
	(0  ; looking for non-space
	 (if* (not spacep)
	    then (setq start i
		       state 1)))
	(1  ; have left anchor, looking for space
	 (if* spacep
	    then (push (subseq str start i) res)
		 (setq state 0))))
      (if* (>= i len) then (return))
      (incf i))
    (nreverse res)))
		 
#|
;; this isn't needed while the web server is running, it just
;; needs to be run periodically as new mime types are introduced.
#+ignore
(defun generate-mime-table (&optional (file "/etc/mime.types"))
  ;; generate a file type to mime type table based on file type
  (let (res)
    (with-open-file (p file :direction :input)
      (loop
	(let ((line (read-line p nil nil)))
	  (if* (null line) then (return))
	  (if* (and (> (length line) 0)
		    (eq #\# (schar line 0)))
	     thenret ; comment
	     else ; real stuff
		  (let ((data (split-into-words line)))
		    (if* data then (push data res)))))))
    (nreverse res)))
|#  
  
(defun match-head-p (val1 val2)
  ;; return t if val1 is a prefix of val2
  ;; val1 and val2 are simple strings
  (let ((len1 (length val1))
	(len2 (length val2)))
    (if* (<= len1 len2)
       then (dotimes (i len1 t)
	      (if* (not (eq (schar val1 i) (schar val2 i)))
		 then (return nil))))))
	    
(defun match-tail-p (val1 val2)
  ;; return t if val1 is a suffix of val2
  ;; val1 and val2 are simple strings
  (let ((len1 (length val1))
	(len2 (length val2)))
    (if* (<= len1 len2)
       then (let ((diff (- len2 len1)))
	      (dotimes (i len1 t)
		(if* (not (eq (schar val1 i) (schar val2 (+ diff i))))
		   then (return nil)))))))
		
  
			     

    





	

      
	      
	      
  
  
       

