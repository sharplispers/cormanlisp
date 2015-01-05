;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;	
;;;;	File:		streams.lisp
;;;;	Contents:	Corman Lisp 3.0 stream functions.
;;;;	History:	3/31/97  RGC  Created.
;;;;				1/31/01  RGC  Integrated Chris Double's patch to READ-SEQUENCE
;;;;							  and implementation of WRITE-SEQUENCE.
;;;;				7/11/01  RGC  OPEN should now give an error when a file being opened
;;;;							  for input does not exist.
;;;;				9/21/01  RGC  Incorporated Pavel Grozman's speedup to string-streams
;;;;							  (eliminates buffer).
;;;;							  Fixed FILE-POSITION set option to work with string-streams.
;;;;                12/9/02  RGC  Reimplemented kernel function %OUTPUT-CHARS, which
;;;;                              fixes a problem where stream-position was not being
;;;;                              updated with WRITE-LINE and WRITE-STRING.
;;;;                12/19/02 RGC  Fixed a problem with :IF-DOES-NOT-EXIST in OPEN.
;;;;                02/16/06 RGC  OPEN: Integrated Karsten Poeck's patch to include :error case as
;;;;                              valid argument for :if-exists.
;;;;                09/07/07 RGC  Integrated Matthias Hölzl's fixes for SUBTYPEP, OPEN, 
;;;;                              and BOA constructors. 
;;;;

(in-package :common-lisp)

;;;
;;;	Stream subclasses:
;;;		FILE-STREAM
;;;		CONCATENATED-STREAM
;;;		ECHO-STREAM
;;;		STRING-STREAM
;;;		SYNONYM-STREAM
;;;		TWO-WAY-STREAM
;;;		BROADCAST-STREAM
;;;		CONSOLE-STREAM
;;;
;;;		*debug-io*
;;;		*error-output*
;;;		*query-io*
;;;		*standard-input*
;;;		*standard-output*
;;;		*terminal-io*
;;;		*trace-output*
;;;
;;;	(defconstant stream-name-offset 				1)
;;;	(defconstant stream-underflow-func-offset 		2)
;;;	(defconstant stream-position-offset 			3)
;;;	(defconstant stream-col-position-offset 		4)
;;;	(defconstant stream-input-buffer-offset 		5)
;;;	(defconstant stream-input-buffer-length-offset 	6)
;;;	(defconstant stream-input-buffer-pos-offset 	7)
;;;	(defconstant stream-input-buffer-num-offset 	8)
;;;	(defconstant stream-handle-offset 				9)
;;;	(defconstant stream-subclass-offset 			10)
;;;	(defconstant stream-binary-offset 				11)
;;;	(defconstant stream-open-offset 				12)
;;;	(defconstant stream-direction-offset 			13)
;;;	(defconstant stream-interactive-offset 			14)
;;;	(defconstant stream-element-type-offset 		15)
;;;	(defconstant stream-associated-streams-offset	16)
;;;	(defconstant stream-overflow-func-offset		17)
;;;	(defconstant stream-output-buffer-offset 		18)
;;;	(defconstant stream-output-buffer-length-offset 19)
;;;	(defconstant stream-output-buffer-pos-offset 	20)
;;;	(defconstant stream-line-number-offset 			21)
;;;
;;;	(defconstant stream-size						21)

#|
(defstruct standard-stream
	(name)
	(underflow-function nil)
	(position)
	(column)
	(input-buffer)
	(input-buffer-length)
	(input-buffer-position)
	(intput-buffer-count)
	(handle)
	(subclass 'file-stream)
	(binary nil)			;character stream by default
	(open nil)
	(direction :input)		;nil, :input, :output, or :bidirectional
	(interactive nil)
	(element-type 'character)
	(associated-streams nil)
	(overflow-function nil)
	(output-buffer)
	(output-buffer-length)
	(output-buffer-position))
|#

(defconstant stream-buffer-size  2048)

(defun check-stream (s) (if (streamp s) s (error "Invalid stream: ~A" s)))

(defun stream-name (s) 					(uref (check-stream s) stream-name-offset))
(defun stream-underflow-function (s) 	(uref (check-stream s) stream-underflow-func-offset))
(defun stream-position (s) 				(uref (check-stream s) stream-position-offset))
(defun stream-col-position (s) 			(uref (check-stream s) stream-col-position-offset))
(defun stream-input-buffer (s) 			(uref (check-stream s) stream-input-buffer-offset))
(defun stream-input-buffer-length (s) 	(uref (check-stream s) stream-input-buffer-length-offset))
(defun stream-input-buffer-pos (s) 		(uref (check-stream s) stream-input-buffer-pos-offset))
(defun stream-input-buffer-num (s) 		(uref (check-stream s) stream-input-buffer-num-offset))
(defun stream-handle (s) 				(uref (check-stream s) stream-handle-offset))
(defun stream-subclass (s) 				(uref (check-stream s) stream-subclass-offset))
(defun stream-binary (s) 				(uref (check-stream s) stream-binary-offset))
(defun stream-open (s) 					(uref (check-stream s) stream-open-offset))
(defun stream-direction (s) 			(uref (check-stream s) stream-direction-offset))
(defun stream-interactive (s) 			(uref (check-stream s) stream-interactive-offset))
(defun stream-element-type (s) 			(uref (check-stream s) stream-element-type-offset))
(defun stream-associated-streams (s) 	(uref (check-stream s) stream-associated-streams-offset))
(defun stream-overflow-function (s) 	(uref (check-stream s) stream-overflow-func-offset))
(defun stream-output-buffer (s) 		(uref (check-stream s) stream-output-buffer-offset))
(defun stream-output-buffer-length (s) 	(uref (check-stream s) stream-output-buffer-length-offset))
(defun stream-output-buffer-pos (s) 	(uref (check-stream s) stream-output-buffer-pos-offset))
(defun stream-line-number (s) 			(uref (check-stream s) stream-line-number-offset))

(defun (setf stream-input-buffer-pos) (v s) 	(setf (uref s stream-input-buffer-pos-offset) v))
(defun (setf stream-input-buffer-num) (v s) 	(setf (uref s stream-input-buffer-num-offset) v))
(defun (setf stream-output-buffer-pos)(v s) 	(setf (uref s stream-output-buffer-pos-offset) v))
(defun (setf stream-position)  		  (v s) 	(setf (uref s stream-position-offset) v))
(defun (setf stream-col-position)  	  (v s) 	(setf (uref s stream-col-position-offset) v))
(defun (setf stream-binary)  		  (v s) 	(setf (uref s stream-binary-offset) v))
(defun (setf stream-line-number)  	  (v s) 	(setf (uref s stream-line-number-offset) v))
(defun (setf stream-open)  	          (v s) 	(setf (uref s stream-open-offset) v))

(defun file-stream-p (stream)
	(and (streamp stream) (eq (stream-subclass stream) 'file-stream)))

(defun string-stream-p (stream)
	(and (streamp stream) (eq (stream-subclass stream) 'string-stream)))

(defun broadcast-stream-p (stream)
    (and (streamp stream) (eq (stream-subclass stream) 'broadcast-stream)))

;;;
;;;	Common Lisp INPUT-STREAM-P function.
;;;
(defun input-stream-p (stream)
	(check-type stream stream)
	(let ((direction (stream-direction stream)))
		(or (eq direction :input)(eq direction :bidirectional))))

;;;
;;;	Common Lisp OUTPUT-STREAM-P function.
;;;
(defun output-stream-p (stream)
	(check-type stream stream)
	(let ((direction (stream-direction stream)))
		(or (eq direction :output)(eq direction :bidirectional))))

;;;
;;;		Common Lisp MAKE-BROADCAST-STREAM function
;;;
(defun make-broadcast-stream (&rest streams)
    (dolist (x streams)
        (unless (output-stream-p x)
            (signal-type-error x 'output-stream)))
        
	(let* ((s (alloc-uvector stream-size uvector-stream-tag)))
        (setf 
            (uref s stream-name-offset) nil
		    (uref s stream-underflow-func-offset)     nil
            (uref s stream-overflow-func-offset) 	  nil
            (uref s stream-position-offset) 		  0
		    (uref s stream-col-position-offset)       0
			(uref s stream-line-number-offset)        0
			(uref s stream-handle-offset)             nil
			(uref s stream-subclass-offset) 		  'broadcast-stream
			(uref s stream-binary-offset) 		      nil
			(uref s stream-open-offset) 		      t
			(uref s stream-direction-offset) 	      ':output
		    (uref s stream-interactive-offset) 	      nil
		    (uref s stream-element-type-offset)       nil
		    (uref s stream-associated-streams-offset) streams)
        s))

;;;
;;;		Common Lisp BROADCAST-STREAM-STREAMS function
;;;
(defun broadcast-stream-streams (stream)
    (unless (broadcast-stream-p stream)
         (signal-type-error stream 'broadcast-stream))
    (uref stream stream-associated-streams-offset))

;;;
;;;		Common Lisp PROBE-FILE function
;;;
(defun probe-file (file)
	(let* ((truename (truename (pathname file)))
		   (ret (win:CreateFile (ct:create-c-string (namestring truename))
					win:GENERIC_READ 
					win:FILE_SHARE_READ 
					NULL
					win:OPEN_EXISTING 
					win:FILE_ATTRIBUTE_NORMAL 
					NULL)))
		(if (ct:cpointer= ret (ct:int-to-foreign-ptr win:INVALID_HANDLE_VALUE))
			(return-from probe-file nil))
		(win:CloseHandle ret)
		truename))

;; these are defined later
(defun rename-file (path1 path2) (declare (ignore path1 path2)) nil) 
(defun file-position (stream &optional pos) (declare (ignore stream pos)) nil)
(defun file-length (stream) (declare (ignore stream)) nil)

(defun open (filespec 
		&key (direction ':input)
			 (element-type 'character)
			 (if-exists ':new-version)
			 (if-does-not-exist nil supplied-if-does-not-exist)
			 (external-format :default))
	(declare (ignore external-format))
	(unless supplied-if-does-not-exist
		(cond
			((or (eq direction :input)
				 (eq if-exists :overwrite)
				 (eq if-exists :append))
			 (setq if-does-not-exist :error))
			((or (eq direction :output)(eq direction :io))
			 (setq if-does-not-exist :create))
			((eq direction :probe)
			 (setq if-does-not-exist nil))))

	;; verify all the arguments are legit
	(let* ((pn (pathname filespec))
		   (file-exists (probe-file pn))
		   (overwrite nil)
		   (append nil))
		(unless (member direction '(:input :output :io :probe))
			(error "Invalid :DIRECTION specified in OPEN: ~A" direction))
		
		(if (eq direction :io)
			(setf direction :bidirectional))
         
        ;;;KAP 2006-02-16, add :error, should be consistent with case if-exists
		(unless (member if-exists 
					'(:error :new-version :rename :rename-and-delete
						:overwrite :append :supersede nil))
			(error "Invalid :IF-EXISTS specified in OPEN: ~A" if-exists))

		(unless (member if-does-not-exist '(:error :create nil))
			(error "Invalid :IF-DOES-NOT-EXIST specified in OPEN: ~A" if-does-not-exist))

		(unless (member element-type 
			'(character integer fixnum signed-byte unsigned-byte 
				(signed-byte 8) (unsigned-byte 8) :default) :test 'equal)
			(error "Unsupported :ELEMENT-TYPE specified in OPEN: ~A" element-type))
        
        (when (and (null file-exists) (eql if-does-not-exist nil))
            (return-from open nil))
        		
		;; process if-exists parameter
		(if (and file-exists
				(or (eq direction ':bidirectional)(eq direction ':output)))
			(case if-exists
				(:error 			(error (make-condition 'FILE-ERROR :pathname pn 
											:format-control "File already exists.")))
				(:new-version nil)	; do nothing--just proceed
				(:rename 			(rename-file pn (concatenate 'string (namestring pn) ".bak")))
				(:rename-and-delete (rename-file pn (concatenate 'string (namestring pn) ".bak")))
				(:overwrite 		(setf overwrite t))
				(:append nil 		(setf append t))
				(:supersede nil)	; do nothing--just proceed
				((nil) 				(return-from open nil))))
				
		(let* ((s (alloc-uvector stream-size uvector-stream-tag))
			   access attributes share-mode create-mode)
			(setf (uref s stream-name-offset) 			pn
				  (uref s stream-underflow-func-offset) nil
				  (uref s stream-overflow-func-offset) 	nil
				  (uref s stream-position-offset) 		0
				  (uref s stream-col-position-offset) 	0
				  (uref s stream-line-number-offset) 	0
				  (uref s stream-handle-offset) 		nil
				  (uref s stream-subclass-offset) 		'file-stream
				  (uref s stream-binary-offset) 		(subtypep element-type 'integer)
				  (uref s stream-open-offset) 			t
				  (uref s stream-direction-offset) 		direction
				  (uref s stream-interactive-offset) 	nil
				  (uref s stream-element-type-offset) 	element-type
				  (uref s stream-associated-streams-offset) nil)
			
			(if (or (eq direction :input)(eq direction :bidirectional))
				(let* ((buf 
						(make-array stream-buffer-size :element-type 
							(if (stream-binary s) 'byte 'character))))
					(setf (uref s stream-underflow-func-offset) 	'file-underflow-function
						  (uref s stream-input-buffer-offset) 		buf
						  (uref s stream-input-buffer-length-offset)(length buf)
						  (uref s stream-input-buffer-pos-offset) 	0
						  (uref s stream-input-buffer-num-offset) 	0))
				(progn
					(setf (uref s stream-input-buffer-offset) 		nil
						  (uref s stream-input-buffer-length-offset) 0
						  (uref s stream-input-buffer-pos-offset) 	0
						  (uref s stream-input-buffer-num-offset) 	0)))

			(if (or (eq direction :output)(eq direction :bidirectional))
				(let* ((buf 
							(make-array stream-buffer-size :element-type 
								(if (stream-binary s) 'byte 'character))))
					(setf (uref s stream-overflow-func-offset) 'file-overflow-function)
					(setf (uref s stream-output-buffer-offset) buf)
					(setf (uref s stream-output-buffer-length-offset) (length buf))
					(setf (uref s stream-output-buffer-pos-offset) 0))
				(progn
					(setf (uref s stream-output-buffer-offset) nil)
					(setf (uref s stream-output-buffer-length-offset) 0)
					(setf (uref s stream-output-buffer-pos-offset) 0)))
 
			;; set access, attributes, share-mode, create-mode
			(case direction
				(:input (setq access win:GENERIC_READ)
						(setq attributes win:FILE_ATTRIBUTE_NORMAL)
						(setq share-mode win:FILE_SHARE_READ)
						(setq create-mode win:OPEN_EXISTING))
				(:output (setq access win:GENERIC_WRITE)
						(setq attributes win:FILE_ATTRIBUTE_NORMAL)
						(setq share-mode win:FILE_SHARE_READ)
						(setq create-mode 
							(if (or overwrite append) 
								win:OPEN_ALWAYS 
								win:CREATE_ALWAYS)))
				(:bidirectional 	
						(setq access (logior win:GENERIC_READ win:GENERIC_WRITE))
						(setq attributes win:FILE_ATTRIBUTE_NORMAL)
						(setq share-mode win:FILE_SHARE_READ)
						(setq create-mode win:OPEN_EXISTING))
				(:probe (setq access 0)
						(setq attributes win:FILE_ATTRIBUTE_NORMAL)
						(setq share-mode win:FILE_SHARE_READ)
						(setq create-mode win:OPEN_EXISTING)))
				
			(setf (uref s stream-handle-offset)
				(cl::foreign-ptr-to-int 
					(win:CreateFile (ct:create-c-string (namestring pn))
						access 
						share-mode 
						NULL
						create-mode 
						attributes 
						NULL)))
			(if (= (uref s stream-handle-offset) win:INVALID_HANDLE_VALUE)
				(error (make-condition 'FILE-ERROR :pathname pn 
						:format-control "Could not open stream ~A for ~A access."
						:format-arguments (list filespec direction))))
			(if append (file-position s (file-length s)))	;; position at end of file
			s)))
 
(defun get-next-character (s)
	(if (= (stream-input-buffer-pos s) (stream-input-buffer-num s))
		(funcall (stream-underflow-function s) s))
	(if (= (stream-input-buffer-pos s) (stream-input-buffer-num s))
		'EOF
		(let ((retval (elt (stream-input-buffer s) (stream-input-buffer-pos s))))
			(incf (stream-input-buffer-pos s))
			(incf (stream-position s))
			retval)))

(defun put-character (s ch)
	(if (= (stream-output-buffer-pos s) (stream-output-buffer-length s))
		(funcall (stream-overflow-function s) s))
	(setf (elt (stream-output-buffer s) (stream-output-buffer-pos s)) ch)
	(incf (stream-output-buffer-pos s))
	(incf (stream-position s))
	(incf (stream-col-position s))
	(if (eq ch #\Newline)
		(setf (stream-col-position s) 0)))

;;;
;;;	Common Lisp INTERACTIVE-STREAM-P function.
;;;
(defun interactive-stream-p (stream)
	(check-type stream stream)
	(stream-interactive stream))

;;;
;;;	Corman Lisp BINARY-STREAM-P function.
;;;
(defun binary-stream-p (stream)
	(check-type stream stream)
	(stream-binary stream))

;;;
;;;	Common Lisp OPEN-STREAM-P function.
;;;
(defun open-stream-p (stream)
	(check-type stream stream)
	(stream-open stream))

;;;
;;;	Common Lisp STREAM-ELEMENT-TYPE function.
;;;
(defun stream-element-type (stream)
	(check-type stream stream)
    (if (broadcast-stream-p stream)
        (let ((last-stream (car (last (broadcast-stream-streams stream)))))
            (if last-stream (stream-element-type last-stream) t))
	   (uref stream stream-element-type-offset)))

;;;
;;;	Common Lisp READ-BYTE function.
;;;
(defun read-byte (stream &optional 
		(eof-error-p t)
		(eof-value nil))
;	(check-type stream stream)
	(unless (and (binary-stream-p stream)(input-stream-p stream))
		(error "Expected a binary input stream, got ~A" stream))
	(if (= (stream-input-buffer-pos stream)
		   (stream-input-buffer-num stream))
		(funcall (stream-underflow-function stream) stream))
	(if (= (stream-input-buffer-pos stream)
		   (stream-input-buffer-num stream))
		(if eof-error-p 
			(error "End of file encountered in stream ~A" stream)
			(return-from read-byte eof-value)))
	(let ((retval (elt (stream-input-buffer stream)
					(stream-input-buffer-pos stream))))
		(incf (stream-input-buffer-pos stream))
		(incf (stream-position stream))
		retval))

;;; discard any contents of the input buffer
(defun empty-input-buffer (stream)
	(unless (string-stream-p stream)
		(when (input-stream-p stream)
			(setf (stream-input-buffer-pos stream) 0)
			(setf (stream-input-buffer-num stream) 0)))
    
    ;; If it is a console stream, need to try to discard
    ;; any pending input as well.
    (if (eq (stream-subclass stream) 'cl::CONSOLE-STREAM)
        (do ()
            ((not (cl::console-chars-available)))
            (cl::console-underflow-function stream)
            (setf (stream-input-buffer-pos stream) 0)
            (setf (stream-input-buffer-num stream) 0))))

(defun flush-output-buffer (stream)
	(if (output-stream-p stream)
		(funcall (stream-overflow-function stream) stream)))

;;;
;;;	Returns boolean representing success or failure.
;;; File must be a STREAM.
;;;	Position can be :start, :end, or a non-negative integer.
;;;
(defun set-file-position (stream position)
	(empty-input-buffer stream)
	(flush-output-buffer stream)
	(if (file-stream-p stream)
		(let ((ret
			  	(cond ((integerp position)
				       (win:SetFilePointer (stream-handle stream) position NULL win:FILE_BEGIN))
				      ((eq position ':start)
				       (win:SetFilePointer (stream-handle stream) 0 NULL win:FILE_BEGIN))
				      ((eq position ':end)
				       (win:SetFilePointer (stream-handle stream) 0 NULL win:FILE_END)))))
			(if (= ret -1)
				nil 
				(setf (stream-position stream) ret)))
		(if (string-stream-p stream)
			(let ((offset (- (uref stream stream-input-buffer-pos-offset)
							 (stream-position stream)))) ;; buffer offset into string
				(if (eq position ':start)
					(setf position 0)
					(if (eq position ':end)
						(setf position (- (uref stream stream-input-buffer-num-offset) offset))))
			   (let ((offset (- (uref stream stream-input-buffer-pos-offset)
								(stream-position stream)))) ;; buffer offset into string
					(if (or (< position 0) 
							(>= (+ position offset) (uref stream stream-input-buffer-num-offset)))
						(error "Invalid stream position specified for string stream"))
					(setf (uref stream stream-input-buffer-pos-offset)(+ position offset))
					(setf (stream-position stream) position)))
			nil)))

;;;
;;;	Common Lisp FILE-POSITION function.
;;;
(defun file-position (stream &optional (position-spec nil supplied-position))
    (if (broadcast-stream-p stream)
        (if supplied-position
            (dolist (x (broadcast-stream-streams stream) position-spec)
                (set-file-position x position-spec))
            (let ((last-stream (car (last (broadcast-stream-streams stream)))))
                (if last-stream (file-position last-stream) 0)))
    	(if supplied-position
    		(set-file-position stream position-spec)
    		(stream-position stream))))

;;;
;;;	Common Lisp FILE-STRING-LENGTH function.
;;;
(defun file-string-length (stream object)
    (declare (ignore stream object))
    ;; TO DO: implement this
    nil)

;;;
;;;	Common Lisp FILE-STRING-LENGTH function.
;;;
(defun stream-external-format (stream)
    (declare (ignore stream))
    ;; TO DO: implement this
    ':DEFAULT)
    
;;;
;;;	Common Lisp WRITE-BYTE function.
;;;
(defun write-byte (byte stream)
;	(check-type stream stream)
;	(check-type byte (integer 0 255))
    (if (broadcast-stream-p stream)
        (dolist (x (broadcast-stream-streams stream))
            (write-byte byte x))
        (progn
        	(unless (and (binary-stream-p stream)(output-stream-p stream))
        		(error "Expected a binary output stream, got ~A" stream))
        	(if (= (stream-output-buffer-pos stream)
        		   (stream-output-buffer-length stream))
        		(funcall (stream-overflow-function stream) stream))
        	(setf (elt (stream-output-buffer stream) (stream-output-buffer-pos stream)) byte)
        	(incf (stream-output-buffer-pos stream))
        	(incf (stream-position stream))
        	byte)))

;;;
;;;	Common Lisp PEEK-CHAR function.
;;;
(defun peek-char (&optional 
					(peek-type nil) 
					(s *standard-input*) 
					(eof-error-p t)
					(eof-value nil)
					(recursive-p nil))
	(declare (ignore recursive-p))
	;; handle t, nil
	(if (symbolp s)
		(if (null s) 
			(setq s *standard-input*)
			(if (eq s t)
				(setq s *terminal-io*))))

	(if (or (binary-stream-p s)(not (input-stream-p s)))
		(error "Expected a character input stream, got ~A" s))
	(if (= (stream-input-buffer-pos s) (stream-input-buffer-num s))
		(funcall (stream-underflow-function s) s))
	(if (= (stream-input-buffer-pos s) (stream-input-buffer-num s))
		(if eof-error-p 
			(error "End of file encountered in stream ~A" s)
			(return-from peek-char eof-value)))
	(let ((ch (elt (stream-input-buffer s) (stream-input-buffer-pos s))))
		(unless peek-type
			(return-from peek-char ch))
		(if (characterp peek-type)
			(do ()
				((char= ch peek-type) ch)
                (%read-char s)                      ; discard character
                (if (= (stream-input-buffer-pos s) (stream-input-buffer-num s))
                    (funcall (stream-underflow-function s) s))
	            (if (= (stream-input-buffer-pos s) (stream-input-buffer-num s))
                    (if eof-error-p               
                        (error "End of file encountered in stream ~A" s)
			            (return-from peek-char eof-value)))
				(setq ch (elt (stream-input-buffer s) (stream-input-buffer-pos s))))
			(if (eq peek-type t)
				(do ()
					((not (whitespace-char ch)) ch)
                    (%read-char s)                      ; discard character
                    (if (= (stream-input-buffer-pos s) (stream-input-buffer-num s))
                        (funcall (stream-underflow-function s) s))
	                (if (= (stream-input-buffer-pos s) (stream-input-buffer-num s))
                        (if eof-error-p               
                            (error "End of file encountered in stream ~A" s)
			                (return-from peek-char eof-value)))
                    (setq ch (elt (stream-input-buffer s) (stream-input-buffer-pos s))))
				(error "Invalid PEEK-TYPE: ~A" peek-type)))))

;;;
;;;	Common Lisp READ-CHAR function.
;;;
(defvar memoized-input-character-stream *terminal-io*)
(defvar memoized-output-character-stream *terminal-io*)
(defconstant ascii-newline-code 10)
(defun __read-char (s eof-error-p eof-value)
	(if (= (stream-input-buffer-pos s)
		   (stream-input-buffer-num s))
		(progn
			(funcall (stream-underflow-function s) s)
			(if (= (stream-input-buffer-pos s)
		   			(stream-input-buffer-num s))
				(if eof-error-p 
					(error "End of file encountered in stream ~A" s)
					(return-from __read-char eof-value)))))
	(let ((retval (elt (stream-input-buffer s)
					(stream-input-buffer-pos s))))
		(incf (stream-input-buffer-pos s))
		(incf (stream-position s))
		(if (char= retval #\Newline)
			(incf (stream-line-number s)))
		retval))

(defun read-char (&optional 
					(s *standard-input*) 
					(eof-error-p t)
					(eof-value nil)
					(recursive-p nil))
	(declare (ignore recursive-p))
	;; if the stream is the memoized stream, skip checks
	(unless (eq s memoized-input-character-stream)
		;; handle t, nil
		(if (symbolp s)
			(if (null s) 
				(setq s *standard-input*)
				(if (eq s t)
					(setq s *terminal-io*))))
		(unless (input-character-stream-p s)
			(error "Expected a character input stream, got ~A" s))
		(setq memoized-input-character-stream s))
;	(if (or (binary-stream-p s)(not (input-stream-p s)))
;		(error "Expected a character input stream, got ~A" s))
	(__read-char s eof-error-p eof-value))

;;;
;;;	Common Lisp READ-CHAR-NO-HANG function.
;;;
(defun read-char-no-hang (&optional 
					(s *standard-input*) 
					(eof-error-p t)
					(eof-value nil)
					(recursive-p nil))
	;; handle t, nil
	(if (symbolp s)
		(if (null s) 
			(setq s *standard-input*)
			(if (eq s t)
				(setq s *terminal-io*))))
	;; if not a console stream, just call read-char
	(unless (eq (stream-subclass s) 'console-stream)
		(return-from read-char-no-hang (read-char s eof-error-p eof-value recursive-p)))
	
	(unless (input-stream-p s)
		(error "Expected a character input stream, got ~A" s))
	(if (= (stream-input-buffer-pos s)
		   (stream-input-buffer-num s))
		(if (> (console-input-chars-available) 0)
			(funcall (stream-underflow-function s) s)
			(return-from read-char-no-hang nil)))
	(if (= (stream-input-buffer-pos s)
		   (stream-input-buffer-num s))
		(return-from read-char-no-hang nil))

	(let ((retval (elt (stream-input-buffer s)
					(stream-input-buffer-pos s))))
		(incf (stream-input-buffer-pos s))
		(incf (stream-position s))
		(if (char= retval #\Newline)
			(incf (stream-line-number s)))
		retval))

;;;
;;;	Common Lisp TERPRI function.
;;;
(defun terpri (&optional (stream *standard-output*)) 
	(write-char #\Newline stream)
	nil)

;;;
;;;	Common Lisp FRESH-LINE function.
;;;
(defun fresh-line (&optional (stream *standard-output*))
	;; handle t, nil
	(if (symbolp stream)
		(if (null stream) 
			(setq stream *standard-output*)
			(if (eq stream t)
				(setq stream *terminal-io*))))
    (if (broadcast-stream-p stream)
        (let ((result nil))
            (dolist (x (broadcast-stream-streams stream))
                (setq result (fresh-line x)))
            result)
        (progn
            (unless (output-stream-p stream)
                (cl::signal-type-error stream 'output-stream))
	        (if (or (not (output-stream-p stream)) (binary-stream-p stream))
                (cl::signal-type-error stream 'output-stream))
        	(if (zerop (stream-col-position stream))
        		nil
        		(progn
        			(write-char #\Newline stream)
        			t)))))
    
;;;
;;;	Common Lisp UNREAD-CHAR function.
;;;
(defun unread-char (ch &optional (stream *standard-input*))
	;; handle t, nil
	(if (symbolp stream)
		(if (null stream) 
			(setq stream *standard-input*)
			(if (eq stream t)
				(setq stream *terminal-io*))))
	(if (or (binary-stream-p stream)(not (input-stream-p stream)))
		(error "Expected a character input stream, got ~A" stream))
	(if (= (stream-input-buffer-pos stream) 0)
		(error "Could not UNREAD character ~S with stream ~A" ch stream))
	(if (char= ch #\Newline)
		(decf (stream-line-number stream)))
		
	(decf (stream-input-buffer-pos stream))
	(decf (stream-position stream))
	nil)

;;;
;;;	Common Lisp WRITE-CHAR function.
;;;
(defun __write-char (ch stream)
	(if (= (stream-output-buffer-pos stream)
		   (stream-output-buffer-length stream))
		(funcall (stream-overflow-function stream) stream))
	(setf (elt (stream-output-buffer stream) (stream-output-buffer-pos stream)) ch)
	(incf (stream-output-buffer-pos stream))
	(incf (stream-position stream))
	(incf (stream-col-position stream))
	(if (eq ch #\Newline)
		(setf (stream-col-position stream) 0))
	ch)
	
(defun write-char (ch &optional (stream *standard-output*))
    (if (broadcast-stream-p stream)
        (dolist (x (broadcast-stream-streams stream))
            (write-char ch x))
        (progn	
            ;; if the stream is the memoized stream, skip checks
        	(unless (eq stream memoized-output-character-stream)
        		;; handle t, nil
        		(if (symbolp stream)
        			(if (null stream) 
        				(setq stream *standard-output*)
        				(if (eq stream t)
        					(setq stream *terminal-io*))))
        		(if (or (not (output-stream-p stream)) (binary-stream-p stream))
        			(error "Expected a character output stream, got ~A" stream))
        		(setq memoized-output-character-stream stream))
        	(__write-char ch stream))))
	
;;;
;;;	Common Lisp READ-LINE function.
;;;
(defun read-line (&optional
					(s *standard-input*) 
					(eof-error-p t)
					(eof-value nil)
					(recursive-p nil))
	(declare (ignore recursive-p))
	;; handle t, nil
	(if (symbolp s)
		(if (null s) 
			(setq s *standard-input*)
			(if (eq s t)
				(setq s *terminal-io*))))
	(if (or (binary-stream-p s)(not (input-stream-p s)))
		(error "Expected a character input stream, got ~A" s))
	(let ((str (make-array 256 :element-type 'character :fill-pointer t)))
		(setf (fill-pointer str) 0)
		(do ((ch (read-char s nil nil t)(read-char s nil nil t)))
			((eql ch #\Newline)
			 (values 
				(concatenate 'string str)
				nil))
			(if (null ch)		; if end of file
				(if eof-error-p
					(error "End of file encountered in stream ~A" s)
					(if (> (length str) 0)
						(return-from read-line (values (concatenate 'string str) t))
						(return-from read-line (values eof-value t)))))
			(vector-push-extend ch str))))

(pl:defasm cl::__read-char (s eof-error-p eof-value)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		push	ebx
		mov		edi, [ebp + (+ ARGS_OFFSET 8)]	;; edi = stream
		mov		eax, [edi + (uvector-offset cl::stream-input-buffer-pos-offset)]
		cmp		eax, [edi + (uvector-offset cl::stream-input-buffer-num-offset)]
		jne		short :continue
		push	[edi + (uvector-offset cl::stream-underflow-func-offset)]
		push	edi
		mov		ecx, 2
		callf	funcall
		add		esp, 8
		mov		edi, [ebp + (+ ARGS_OFFSET 8)]	;; edi = stream
		mov		eax, [edi + (uvector-offset cl::stream-input-buffer-pos-offset)]
		cmp		eax, [edi + (uvector-offset cl::stream-input-buffer-num-offset)]
		jne		short :continue
		mov		eax, [ebp + (+ ARGS_OFFSET 4)]	;; eax = eof-error-p
		cmp		eax, [esi]						;; eof-error-p = nil?
		je		short :no_error
		push	"End of file encountered in stream ~A"
		push	edi
		mov		ecx, 2
		callf	error
	:no_error
		mov		eax, [ebp + ARGS_OFFSET]		;; eax = eof-value
		jmp 	short :exit
	:continue
		mov		ebx, [edi + (uvector-offset cl::stream-input-buffer-offset)]	; ebx = buffer
		mov		edx, [edi + (uvector-offset cl::stream-input-buffer-pos-offset)]; edx = index
		shr		edx, 3
		xor		eax, eax
		mov		ax, [ebx + edx*2 + (- 8 uvector-tag)]	;; unicode mod
;;		mov		al, [ebx + edx + (- 8 uvector-tag)]
		cmp		eax, 10							;; ascii newline?
		jne		:t1
		mov		ecx, [edi + (uvector-offset cl::stream-line-number-offset)]
		add		ecx, 8
		mov		[edi + (uvector-offset cl::stream-line-number-offset)], ecx
	:t1
		shl		eax, 8
		or		eax, 1							;; convert to a character
		mov		ebx, [edi + (uvector-offset cl::stream-input-buffer-pos-offset)]
		mov		ecx, [edi + (uvector-offset cl::stream-position-offset)]
		add		ebx, 8
		add		ecx, 8
		mov		[edi + (uvector-offset cl::stream-input-buffer-pos-offset)], ebx
		mov		[edi + (uvector-offset cl::stream-position-offset)], ecx
	:exit
		mov		ecx, 1
		pop		ebx
		pop		edi
		mov		esp, ebp
		pop		ebp
		ret
	})

(pl:defasm cl::__write-char (ch stream)
	{
		push	ebp
		mov		ebp, esp
		push	edi
		push	ebx
		mov		edi, [ebp + ARGS_OFFSET]	;; edi = stream
		mov		eax, [edi + (uvector-offset cl::stream-output-buffer-pos-offset)]
		cmp		eax, [edi + (uvector-offset cl::stream-output-buffer-length-offset)]
		jne		short :continue
		push	[edi + (uvector-offset cl::stream-overflow-func-offset)]
		push	edi
		mov		ecx, 2
		callf	funcall
		add		esp, 8
		mov		edi, [ebp + ARGS_OFFSET]	;; edi = stream
	:continue
		mov		ebx, [edi + (uvector-offset cl::stream-output-buffer-offset)]	; ebx = buffer
		mov		edx, [edi + (uvector-offset cl::stream-output-buffer-pos-offset)]; edx = index
		shr		edx, 3
		mov		ecx, [ebp + (+ ARGS_OFFSET 4)]		;; eax = character
		mov		eax, ecx
		shr		ecx, 8
		mov		[ebx + edx*2 + (- 8 uvector-tag)], cx		;; unicode mod
;;		mov		[ebx + edx + (- 8 uvector-tag)], cl
		mov		ebx, [edi + (uvector-offset cl::stream-output-buffer-pos-offset)]
		mov		ecx, [edi + (uvector-offset cl::stream-position-offset)]
		mov		edx, [edi + (uvector-offset cl::stream-col-position-offset)]
		add		ebx, 8
		add		ecx, 8
		add		edx, 8
		mov		[edi + (uvector-offset cl::stream-output-buffer-pos-offset)], ebx
		mov		[edi + (uvector-offset cl::stream-position-offset)], ecx
		mov		[edi + (uvector-offset cl::stream-col-position-offset)], edx
		mov		ecx, eax
		shr		ecx, 8
		cmp		ecx, (char-int #\Newline)
		jne		:exit
		xor		ecx, ecx
		mov		[edi + (uvector-offset cl::stream-col-position-offset)], ecx
	:exit
		mov		ecx, 1
		pop		ebx
		pop		edi
		mov		esp, ebp
		pop		ebp
		ret
	})

(defun string-underflow-function (s)
        (declare (ignore s))
        0)

(defun make-string-input-stream (string &optional (start 0) end)
    (unless end (setf end (length string)))
	
    ;; verify all the arguments are legit
    (check-type string string)
    (check-type start integer)
    (check-type end integer)

	;; only a simple string may be used as buffer
	(when (adjustable-array-p string)
		(let ((offset (uref string adjustable-array-displaced-offset)))
			(incf start offset)
			(incf end offset))
			(setq string (uref string adjustable-array-vector-offset)))

	(let* ((s (alloc-uvector stream-size uvector-stream-tag)))
		(setf (uref s stream-name-offset) nil)
		(setf (uref s stream-underflow-func-offset) #'cl::string-underflow-function)
		(setf (uref s stream-overflow-func-offset) nil)
		(setf (stream-position s) start)
		(setf (stream-col-position s) 0)
		(setf (uref s stream-handle-offset) (list string start end))
		(setf (uref s stream-subclass-offset) 'string-stream)
		(setf (uref s stream-binary-offset) nil)
		(setf (uref s stream-line-number-offset) 0)
		(setf (uref s stream-open-offset) t)
		(setf (uref s stream-direction-offset) :input)
		(setf (uref s stream-interactive-offset) nil)
		(setf (uref s stream-element-type-offset) 'character)
		(setf (uref s stream-associated-streams-offset) nil)
		(setf (uref s stream-output-buffer-offset) nil)
		(setf (uref s stream-output-buffer-length-offset) 0)
		(setf (uref s stream-output-buffer-pos-offset) 0)
		(setf (uref s stream-input-buffer-offset) string)
		(setf (uref s stream-input-buffer-length-offset) end)
		(setf (uref s stream-input-buffer-pos-offset) start)
		(setf (uref s stream-input-buffer-num-offset) end)
		s))

;;; redefine this now to print the file name 
(defun write-stream (object)
	(format t "#< ~A ~S ~A ~A >"
		(stream-subclass object)
		(if (eq (stream-subclass object) 'file-stream)
			(namestring (stream-name object))
			(stream-name object))
		:direction (stream-direction object)))

;;;
;;;	Common Lisp WITH-INPUT-FROM-STRING function.
;;;
(defmacro with-input-from-string 
	((var string &key (index nil) start end) 
		. forms)
	(let ((rv-sym (gensym)))
		`(let ((,var (make-string-input-stream 
						,string 
						,@(if start (list start))
						,@(if end (list end))))
                ,rv-sym)
			(unwind-protect
				(setq ,rv-sym (multiple-value-list (progn ,@forms)))
				(close ,var))
			,@(if index (list `(setf ,index (stream-position ,var))))
			(apply #'values ,rv-sym))))

;;;
;;;	Common Lisp CLEAR-INPUT function.
;;;
(defun clear-input (&optional (s *standard-input*))
	;; handle t, nil
	(if (symbolp s)
		(if (null s) 
			(setq s *standard-input*)
			(if (eq s t)
				(setq s *terminal-io*))))
	(check-type s stream)
	(empty-input-buffer s)
	nil)

;
;	Common Lisp READ-FROM-STRING function.
;	To do: handle eof-error, eof-value, preserve-whitespace settings	
;
(defun read-from-string (string &optional eof-error eof-value 
			&key (start 0) end preserve-whitespace 
			&aux string-stream expr position)
	(declare (ignore preserve-whitespace))
	(if (not (typep string 'string)) (error "Not a string"))
	(if (not end) (setq end (length string)))
	(setq string-stream (make-string-input-stream string start end))
	(setq expr (read string-stream eof-error eof-value))
	(setq position (file-position string-stream))
    (if (eq position 'Eof)
        (setq position end))
	(values expr position))	

;;;
;;;		Common Lisp WRITE-TO-STRING function
;;;
(defun write-to-string (object &rest keys)
				;;		&key array base case 
				;;		circle escape gensym length level 
				;;		lines miser-width pprint-dispatch 
				;;		pretty radix readably right-margin)
	(with-output-to-string (string)
		(apply 'write object :stream string keys)))

;;;
;;;		Common Lisp PRIN1-TO-STRING function
;;;
(defun prin1-to-string (object)
	(with-output-to-string (string)
		(write object :stream string :escape t)))

;;;
;;;		Common Lisp PRINC-TO-STRING function
;;;
(defun princ-to-string (object)
	(with-output-to-string (string)
		(write object :stream string :escape nil :readably nil)))

(defun stream-win-handle (stream)
	(unless (file-stream-p stream)
		(return-from stream-win-handle nil))
	(ct:int-to-foreign-ptr (cl::stream-handle stream)))

;;;
;;;		Common Lisp FILE-LENGTH function
;;;
(defun file-length (stream)
    (if (broadcast-stream-p stream)
        (let ((last-stream (car (last (broadcast-stream-streams stream)))))
            (if last-stream (file-length last-stream) 0))
    	(let ((handle (stream-win-handle stream)))
    		(unless handle
    			(cl::signal-type-error stream 'file-stream))
    		(win:GetFileSize handle ct:null))))

(defun low-word (integer) (mod integer #x100000000))
(defun high-word (integer) (truncate integer #x100000000))

(defun get-file-times (stream)
	(let ((creation-time (ct:malloc (ct:sizeof 'win:FILETIME)))
		  (last-access-time (ct:malloc (ct:sizeof 'win:FILETIME)))
		  (last-write-time (ct:malloc (ct:sizeof 'win:FILETIME))))
		(win:GetFileTime (cl::stream-win-handle stream) 
			creation-time last-access-time last-write-time)
		(values
			(+ (ct:cref win:FILETIME creation-time win::dwLowDateTime)
			   (ash (ct:cref win:FILETIME creation-time win::dwHighDateTime) 32))
			(+ (ct:cref win:FILETIME last-access-time win::dwLowDateTime)
			   (ash (ct:cref win:FILETIME last-access-time win::dwHighDateTime) 32))
			(+ (ct:cref win:FILETIME last-write-time win::dwLowDateTime)
			   (ash (ct:cref win:FILETIME last-write-time win::dwHighDateTime) 32)))))

(defun set-file-times (stream creation last-access last-write)
	(let ((creation-time (ct:malloc (ct:sizeof 'win:FILETIME)))
		  (last-access-time (ct:malloc (ct:sizeof 'win:FILETIME)))
		  (last-write-time (ct:malloc (ct:sizeof 'win:FILETIME))))
		(setf (ct:cref win:FILETIME creation-time win::dwLowDateTime) (low-word creation))
		(setf (ct:cref win:FILETIME creation-time win::dwHighDateTime)(high-word creation))
		(setf (ct:cref win:FILETIME last-access-time win::dwLowDateTime)(low-word last-access))
		(setf (ct:cref win:FILETIME last-access-time win::dwHighDateTime)(high-word last-access))
		(setf (ct:cref win:FILETIME last-write-time win::dwLowDateTime)(low-word last-write))
		(setf (ct:cref win:FILETIME last-write-time win::dwHighDateTime)(high-word last-write))	
		(win:SetFileTime (cl::stream-win-handle stream) 
			creation-time last-access-time last-write-time)))
#|
(defun file-time-to-system-timex (file-time)
	(let ((systemtime (ct:malloc (ct:sizeof 'win:SYSTEMTIME)))
		(filetime (ct:malloc (ct:sizeof 'win:FILETIME))))
		(setf (ct:cref win:FILETIME filetime win::dwLowDateTime) (low-word file-time))
		(setf (ct:cref win:FILETIME filetime win::dwHighDateTime)(high-word file-time))
		(win:FileTimeToSystemTime filetime systemtime)
		(values 
			(ct:cref win:SYSTEMTIME systemtime win::wYear)
			(ct:cref win:SYSTEMTIME systemtime win::wMonth)
			(ct:cref win:SYSTEMTIME systemtime win::wDayOfWeek)
			(ct:cref win:SYSTEMTIME systemtime win::wDay)
			(ct:cref win:SYSTEMTIME systemtime win::wHour)
			(ct:cref win:SYSTEMTIME systemtime win::wMinute)
			(ct:cref win:SYSTEMTIME systemtime win::wSecond)
			(ct:cref win:SYSTEMTIME systemtime win::wMilliseconds))))
|#
(defun format-file-time (file-time &optional (stream *standard-output*))
	(cl::format-universal-time (cl::file-time-to-universal-time file-time)
		stream))

(defun touch-file (file)
	(with-open-file (f file :direction :io)
		(multiple-value-bind (creation access modify)
			(get-file-times f)
			(declare (ignore access modify))
			(set-file-times f
				creation
				(cl::universal-time-to-file-time (get-universal-time))
				(cl::universal-time-to-file-time (get-universal-time))))))
#|				
(defun set-stream-output-buffer (stream buffer)
	(check-type stream stream)
	(check-type buffer (vector byte))
	(force-output stream)
	(setf (uref stream stream-output-buffer-offset) buffer)
	(setf (stream-output-buffer-pos stream) 0)
	(setf (uref stream stream-output-buffer-length-offset) (length buffer))
	stream)

;; (set-stream-output-buffer *terminal-io* (make-array 1 :element-type 'byte))
|#
;;;
;;;	Common Lisp SLEEP function.
;;;
(defun sleep (seconds) (win:%Sleep (truncate (* seconds 1000))))

;;;
;;;	Corman Lisp FILE-STREAM-NAME function.
;;;
(defun file-stream-name (stream)
	(unless (cl::file-stream-p stream)
		(error "Not a FILE-STREAM: ~S" stream))
	(cl::stream-name stream))

;;;
;;;	Common Lisp DELETE-FILE function.
;;;
(defun delete-file (filespec)
	(if (streamp filespec)
		(if (file-stream-p filespec)
			(progn
				(close filespec)
				(setf filespec (file-stream-name filespec)))
			(error "Not a FILE-STREAM: ~S" filespec)))
	(let ((path (pathname filespec)))
		(win:DeleteFile (ct:lisp-string-to-c-string (namestring path)))))

;;;
;;;	Common Lisp FORCE-OUTPUT function.
;;;
(defun force-output (&optional (output-stream *standard-output*))
    (if (broadcast-stream-p output-stream)
        (dolist (x (broadcast-stream-streams output-stream))
            (force-output x))	;; handle t, nil
        (progn
        	(if (null output-stream) 
        		(setq output-stream *standard-output*)
        		(if (eq output-stream t)
        			(setq output-stream *terminal-io*)))
        	(let ((overflow-func (stream-overflow-function output-stream)))
        		(if overflow-func (funcall overflow-func output-stream)))
        	nil)))

;;;
;;;	Common Lisp FINISH-OUTPUT function.
;;;	For now, no different from FORCE-OUTPUT.
;;;
(defun finish-output (&optional (output-stream *standard-output*))
	(force-output output-stream))

;;;
;;;	Common Lisp CLEAR-OUTPUT function.
;;;	For now, no different from FORCE-OUTPUT.
;;;
(defun clear-output (&optional (output-stream *standard-output*))
	(force-output output-stream))

;; Given a string of text, convert to bytes, and convert line feeds
;; to CR/LF pairs.
(defun expand-line-feeds (string len)
	;; first pass, calculate size of output buffer
	(let* ((size len))
		(dotimes (i len)
			(if (char= (char string i) #\Newline)
				(incf size)))
		(let ((buf (make-array size :element-type 'byte)))
			(do ((i 0 (+ i 1))
				 (j 0 (+ j 1)))
				((= i len))
				(let ((c (char string i)))
					(when (char= c #\Newline)
						(setf (elt buf j) 13 (elt buf (incf j)) 10))
					(setf (elt buf j)(char-int (char string i)))))
			buf)))

;;; Overrides the kernel function.
(defun file-overflow-function (s)
	(let (bytes-to-write
	      output-bytes)
		(if (stream-binary s)
			(setf output-bytes (stream-output-buffer s)
				  bytes-to-write (stream-output-buffer-pos s))
			(setf output-bytes 
				(expand-line-feeds (stream-output-buffer s) 
					(stream-output-buffer-pos s))
				  bytes-to-write (length output-bytes)))
		(ct:with-fresh-foreign-block (bytes-written ':unsigned-long)
			(ct:with-fresh-foreign-block (buf `(byte ,bytes-to-write))
				;; copy bytes to foreign buffer
				(dotimes (i bytes-to-write)
					(setf (ct:cref (byte *) buf i) (elt output-bytes i)))
				(unless (or (= bytes-to-write 0)
							(win:WriteFile (ct:int-to-foreign-ptr (stream-handle s))
						 buf bytes-to-write bytes-written NULL))
					(error "Could not write to file ~A. ~A"
						s (win:system-error-text (win:GetLastError))))
				(setf (stream-output-buffer-pos s) 0)
				(ct:cref (:unsigned-long 1) bytes-written 0)))))			

(defun compress-line-feeds (byte-buf char-buf bytes)
	(do ((i 0 (+ i 1))
		 (count 0 (+ count 1)))
		((>= i bytes) count)
		(let ((b (ct:cref (byte *) byte-buf i)))
			(when (= b 13)
				(incf i)
				(if (>= i bytes)
					(return count))
				(setf b (ct:cref (byte *) byte-buf i)))			
			(setf (elt char-buf count)
				(int-char b)))))
						
;;; Overrides the kernel function.
(defun file-underflow-function (s)
	(let* ((streambuf (stream-input-buffer s))
		   (bytes-to-read (length streambuf))
			input-bytes)
		(ct:with-fresh-foreign-block (bytes-read ':unsigned-long)
			(ct:with-fresh-foreign-block (buf `(byte ,bytes-to-read))
				(unless (win:ReadFile (ct:int-to-foreign-ptr (stream-handle s))
							 buf bytes-to-read bytes-read NULL)
						(error "Could not read from file ~A, error code = ~A"
							s (win:GetLastError)))
				(if (stream-binary s)
					(progn
						(setf input-bytes (ct:cref (:unsigned-long 1) bytes-read 0))
						(dotimes (i input-bytes)
							(setf (elt streambuf i) (ct:cref (byte *) buf i))))
					(setf input-bytes 
						(compress-line-feeds buf streambuf
							(ct:cref (:unsigned-long 1) bytes-read 0))))
				(setf (stream-input-buffer-pos s) 0
					  (stream-input-buffer-num s) input-bytes)))
		input-bytes))					

;;;
;;;	Common Lisp READ-SEQUENCE function.
;;;
(defun read-sequence (sequence stream &key (start 0) (end nil))
	(check-type sequence sequence)
	(check-type stream stream)
	(check-type start (fixnum 0 *))
	(check-type end (or null (fixnum 0 *)))
	(if (null end)
		(setf end (length sequence)))
	(let ((element-type (stream-element-type stream))
		  (eof (cons nil nil)))
		(cond 
			((eq element-type 'character)
				(dotimes (count (- end start) (- end start))
					(let ((c (read-char stream nil eof)))
						(if (eq c eof)
							(return (+ count start)))
						(setf (elt sequence (+ count start)) c))))
			((or (eq element-type 'byte) 
					(eq element-type 'unsigned-byte) 
					(eq element-type 'signed-byte))
				(dotimes (count (- end start) (- end start))
					(let ((b (read-byte stream nil eof)))
						(if (eq b eof)
							(return (+ count start)))
						(setf (elt sequence (+ count start)) b))))
			(t (error "Cannot READ-SEQUENCE on stream of :ELEMENT-TYPE ~A" element-type)))))

;;;
;;; Chris Double's simple write-sequence implementation of Common Lisp WRITE-SEQUENCE
;;;
(defun write-sequence (sequence stream &key start end)
	(let ((element-type (stream-element-type stream))
			(start (if start start 0))
			(end (if end end (length sequence))))
		(if (eq element-type 'character)
			(do ((n start (+ n 1)))
				((= n end))
				(write-char (elt sequence n) stream))
			(do ((n start (+ n 1)))
				((= n end))
				(write-byte (elt sequence n) stream))))		;; recoded to avoid LOOP, because it isn't loaded yet
			;(loop for n from start below end do
			;	(write-char (elt sequence n) stream))
			;(loop for n from start below end do
			;	(write-byte (elt sequence n) stream))
	(force-output stream))

;;;
;;; Common Lisp FILE-WRITE-DATE function.
;;;
(defun file-write-date (pathspec)
	(with-open-file (stream pathspec :direction :input)
		(multiple-value-bind (creation-time access-time write-time)
			(cl::get-file-times stream)
			(declare (ignore creation-time access-time))
			(cl::file-time-to-universal-time write-time))))

;;;
;;; Corman Lisp CLEAR-INPUT-BUFFER function.
;;;	Causes the input buffer of a stream to be cleared (emptied).
;;;	Any buffered characters are discarded.
;;;
(defun clear-input-buffer (stream)
	(setf (stream-input-buffer-pos stream) (stream-input-buffer-num stream)))

(defconstant INVALID_FILE #xffffffff)

;;;
;;; Corman Lisp GET-FILE-ATTRIBUTES function.
;;;	Gets the file attributes (as an integer) of the passed file name or path.
;;;
(defun ccl::get-file-attributes (name)
	(win:getfileattributes (ct:lisp-string-to-c-string (namestring name))))

;;;
;;; Corman Lisp SET-FILE-ATTRIBUTES function.
;;;	Sets the file attributes (as an integer) of the passed file name or path.
;;;
(defun ccl::set-file-attributes (name attributes)
	(win:setfileattributes (ct:lisp-string-to-c-string (namestring name)) attributes))

;;;
;;; Corman Lisp DIRECTORY-P function.
;;;	Returns T if the passed file name or path represents a directory (a real
;;; one on disk) or NIL if it represents a file name or does not exist.
;;;
(defun ccl::directory-p (name)
	(let ((attrs (ccl::get-file-attributes name)))
		(and (/= attrs INVALID_FILE)
			 (/= (logand attrs win:FILE_ATTRIBUTE_DIRECTORY) 0))))
(export '(ccl::directory-p) (find-package :ccl))

(register-load-image-restore-func
	(lambda () (clear-input-buffer *terminal-io*)))

;;;
;;; Redefine kernel function %OUTPUT-CHARS. This fixes a bug
;;; in the kernel function where the stream position was not being 
;;; updated properly.
;;;
(defun cl::%output-chars (str stream start end)
    (do ((index start (+ index 1)))
        ((>= index end) str)
        (cl::__write-char (elt str index) stream)))

;;;
;;; Common Lisp LISTEN function
;;;
(defun listen (&optional (s *standard-input*))
	;; handle t, nil
	(if (symbolp s)
		(if (null s) 
			(setq s *standard-input*)
			(if (eq s t)
				(setq s *terminal-io*))))
    
	;; if not a console stream, just call PEEK-CHAR
	(unless (eq (stream-subclass s) 'console-stream)
        (return-from listen (characterp (peek-char nil s nil 'eof))))

	(unless (input-stream-p s)
		(error "Expected a character input stream, got ~A" s))
        
	(if (= (stream-input-buffer-pos s)
		   (stream-input-buffer-num s))
		(if (> (console-input-chars-available) 0)
            t
            nil)
        t))

;; redefine kernel function
(defun cl::open-input-stream (path)
    (open path))

;;;
;;; Redefine Kernel cl::%OUTPUT-CHAR function.
;;;
(defun cl::%output-char (char stream)
    (unless (characterp char)
        (cl::signal-type-error char 'character))
    (unless (streamp stream)
        (cl::signal-type-error stream 'stream))
    (unless (output-stream-p stream)
        (error "The stream ~A is not an output stream" stream))
    (if (= (stream-output-buffer-pos stream)
           (stream-output-buffer-length stream))
        (funcall (stream-overflow-function stream) stream))
    (setf (aref (stream-output-buffer stream) (stream-output-buffer-pos stream)) char)
    (incf (stream-output-buffer-pos stream))
    (if (char= char #\Newline)
        (setf (stream-col-position stream) 0)
        (incf (stream-col-position stream)))
    char)

;;;
;;; Common Lisp CLOSE function (redefine kernel function
;;;
(defun close (stream &key (abort nil))
    (declare (ignore abort))    ;; we don't currently do anything with this
    (unless (streamp stream)
        (signal-type-error stream 'stream))
    (if (and (cl::file-stream-p stream) (cl::stream-open stream))
        (progn
            (if (output-stream-p stream)
                (force-output stream))
            (let ((ret (win:CloseHandle (ct:int-to-foreign-ptr (cl::stream-handle stream)))))
                (setf (cl::stream-open stream) nil)
                ret))
        t)) 

(declare-type-specifier file-stream (x specifier)
    (declare (ignore specifier))
    (file-stream-p x))

(declare-type-specifier string-stream (x specifier)
    (declare (ignore specifier))
    (string-stream-p x))

;;;;
;;;; Some non-standard functions to read and write 64-bit binary double-floats.
;;;;
(in-package :ccl)

(defun double-float-to-bytes (double-float)
    (let ((bits (cl::%double-float-bits double-float))
          (bytes (make-array 8 :element-type 'unsigned-byte)))
        (dotimes (i 8)
            (setf (aref bytes i) (logand (ash bits (* i -8)) #xff)))
        bytes))


(defun bytes-to-double-float (bytes)
    (let ((bits 0))
        (dotimes (i 8)
            (incf bits (ash (aref bytes i) (* i 8))))
        (cl::%make-double-float bits)))

(defun write-double-float (double-float &optional (stream *standard-output*))
    "Writes a double float as a 64-bit binary value, low byte first."
    (let ((bytes (double-float-to-bytes double-float)))
        (dotimes (i (length bytes))
            (write-byte (aref bytes i) stream))
        double-float))

(defun read-double-float (&optional (stream *standard-input*))
    "Reads and returns a double float from a stream, as a 64-bit binary value (low-byte first)"
    (let ((bytes (make-array 8 :element-type 'unsigned-byte)))
        (dotimes (i 8)
            (setf (aref bytes i) (read-byte stream)))
        (bytes-to-double-float bytes)))

(export '(ccl::write-double-float ccl::read-double-float))

