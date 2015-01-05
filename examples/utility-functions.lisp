;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		utility-functions.lisp
;;;;	Contents:	Useful sample Common Lisp functions
;;;;	History:	3/9/98  RGC  Created
;;;;

;;;; from Larry Hunter
(defun lex-string (string &optional (whitespace-chars '(#\space #\newline)))
	"Separates a string at whitespace and returns a list of strings"
	(flet ((whitespace-char? (char)
			(member char whitespace-chars :test #'char=)))
		(let ((tokens nil))
			(do* ((token-start 
				  	(position-if-not #'whitespace-char? string) 
					(when token-end
						(position-if-not #'whitespace-char? string 
							:start (1+ token-end))))
				  (token-end 
					(position-if #'whitespace-char? string :start (or token-start 0))
				  	(when token-start
						(position-if #'whitespace-char? string :start token-start))))
				((null token-start) (nreverse tokens))
				(push (subseq string token-start token-end) tokens)))))

(defun string-list (string)
	(with-input-from-string (s string) 
		(do* ((eof '(cons 0 0))
			  (result nil)
			  (token (read s nil eof)(read s nil eof)))
			((eq token eof)(nreverse result))
			(push token result))))

(defun compare-binary-files (path1 path2)
	(let ((f1 (open path1 :element-type 'unsigned-byte))
		  (f2 (open path2 :element-type 'unsigned-byte)))
		(when (/= (file-length f1)(file-length f2))
			(format t "Files are of different lengths. ~
				 ~A is ~D bytes long, ~A is ~D bytes long.~%"
				(namestring path1)
				(file-length f1)
				(namestring path2)
				(file-length f2))
			(return-from compare-binary-files))
		(unwind-protect
			(do* ((c1 (read-byte f1 nil 'eof)(read-byte f1 nil 'eof))
	 			  (c2 (read-byte f2 nil 'eof)(read-byte f2 nil 'eof))
	 			  (index 0 (+ index 1)))
				((or (eq c1 'eof)(eq c2 'eof)))
				(if (/= c1 c2)
					(progn
						(format t "Difference at position ~D, first file = ~D, second file = ~D~%"
							index c1 c2)
						(return))))
			(close f1)
			(close f2))))

(defmacro cond-every (&rest clauses)  
	(loop for (cond . forms) in clauses
		collect 
			(if (eql cond 't)
				`(progn . ,forms)
				`(when ,cond ,@forms))
		into code
		finally (return `(progn . ,code))))

;;; full-fledged version ala position
;;; BUG FIX: 24-08-1999 (bp): :from-end t caused sub-sequences to be reversed 
;;; copyright notice: this function is in the PUBLIC DOMAIN

(defun split-sequence (delimiter seq
      &key
      (empty-marker nil keep-empty-subseqs)
      (from-end nil)
      (start 0)
      (end nil)
      (test nil test-supplied)
      (test-not nil test-not-supplied)
      (key nil key-supplied)
      &aux
      (len (length seq)))
 
	"Return list of subsequences in SEQ delimited by DELIMITER.
     If an EMPTY-MARKER is supplied, empty subsequences will be
     represented by EMPTY-MARKER, otherwise they will be discarded.
     All other keywords work analogously to POSITION."
 
  	(declare (optimize (speed 3)(safety 0)(space 0)(debug 0)))
 
  	(unless end (setq end len))
 
	(when from-end
		(setf seq (reverse seq))
		(psetf start (- len end)
			end (- len start)))
 
	(loop with other-keys = 
		(nconc (when test-supplied (list :test test))
			(when test-not-supplied (list :test-not test-not))
			(when key-supplied (list :key key)))
		for left = start then (+ right 1)
		for right =
			(min 
				(or (apply #'position delimiter seq :start left other-keys) 
					len)
				end)
		if (< left right)
			collect 
				(let ((subseq (subseq seq left right)))
					(if from-end
						(nreverse subseq)
						subseq))
		else 
		when keep-empty-subseqs 
		collect empty-marker
		until (eq right end)))

;;
;; A handy function for prompting for a string--works well in either IDE or console
;; Outputs the prompt, and then reads the entire line (IDE). If the prompt is at the 
;; beginning of the returned string it is stripped off and the rest returned.
;; In the console it works just like a normal (read-line)
;;
(defun prompt-for-string (&optional (prompt "Enter a string"))
    (if (eq (ccl:cormanlisp-client-type) :ide-client)
        (let (string)
            (format *terminal-io* "~&~A: " prompt)
            (setf string (read-line *terminal-io*))
            (format *terminal-io* "~&")      ;; force a newline if necessary
            (if (and (>= (length string) (length prompt))
                    (string= (subseq string 0 (length prompt)) prompt))
                (subseq string (+ 2 (length prompt)))
                string))
        (progn
            (format *terminal-io* "~&~A: " prompt)
            (read *terminal-io*))))

;;;
;;; FOREACH macro
;;; Reproduces functionality of C# foreach operator.
;;; Iterates over each element of a sequence which matches the named
;;; type.
;;;
#|
examples:
(foreach single-float (x #(10 20.3 30 40.5 50))
    (format t "single-float: ~A~%" x))

(foreach integer (x '(10 20.3 30 40.5 50))
    (format t "integer: ~A~%" x))                   
|# 
(defmacro foreach (type (var sequence) &body body)
    (let ((seqsym (gensym))
          (indexsym (gensym)))
        `(let ((,seqsym ,sequence)
              (,var))
            (if (vectorp ,seqsym)
                (dotimes (,indexsym (length ,seqsym))
                    (setq ,var (elt ,seqsym ,indexsym))
                    (if (typep ,var ',type)
                        (let ()
                            ,@body)))
                (dolist (,var ,seqsym)
                    (if (typep ,var ',type)
                        (let ()
                            ,@body)))))))
