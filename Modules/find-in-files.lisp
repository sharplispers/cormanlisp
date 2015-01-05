;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	All rights reserved.
;;;;	-------------------------------
;;;
;;;	File: 		find-in-files.lisp
;;;	Contents:	File searching utility function.
;;;	History:		5/22/01  RGC  Created.
;;;
	
(in-package :ccl)
(provide "FIND-IN-FILES")
(export 'find-in-files)

(defconstant ascii-return  13)
(defconstant ascii-newline 10)

(defconstant upcase-int
	(let ((a (make-array 256)))
		(dotimes (i 256)
			(setf (aref a i) i))
		(do ((i (char-int #\a) (+ i 1)))
			((> i (char-int #\z)))
			(setf (aref a i) (- i 32)))
		a))

(defconstant downcase-int
	(let ((a (make-array 256)))
		(dotimes (i 256)
			(setf (aref a i) i))
		(do ((i (char-int #\A) (+ i 1)))
			((> i (char-int #\Z)))
			(setf (aref a i) (+ i 32)))
		a))

(defmacro upcase (c) `(svref upcase-int ,c))
(defmacro compare-no-case (c1 c2) 
	`(or (= ,c1 ,c2) (= ,c1 (svref upcase-int ,c2)(= ,c1 (svref downcase-int ,c2)))))

;;; search-string is the string we are scanning, and find-string is
;;; the string we are looking for
;;; The source of this routine is kmpsearch() in Sedgewick's
;;; "Algorithms" reference, pp. 282-283, the Knuth-Morris-Pratt algorithm.
;;;
(defun kmpsearch-lisp (find-string search-string &key (ignore-case t))
	(let ((next (make-array (length find-string))))
		;; initialize the 'next' table
		(setf (svref next 0) -1)
		(do ((i  0 (+ i 1))
			 (j -1 (+ j 1)))
			((>= i (length find-string)))
			(if ignore-case
				(do ()
					((or (< j 0) 
						(char= (char-upcase (char find-string i))
							   (char-upcase (char find-string j)))))
					(setf j (svref next j)))
				(do ()
					((or (< j 0) 
						(char= (char find-string i)
							   (char find-string j))))
					(setf j (svref next j))))
			(setf (svref next i) j))
		
		;; start the search
		(do ((i  0 (+ i 1))
			 (j  0 (+ j 1)))
			((or (>= j (length find-string))
				 (>= i (length search-string)))
				(if (= j (length find-string))(- i j) nil))
			(if ignore-case
				(do ()
					((or (< j 0) 
						(char= (char-upcase (char search-string i))
							   (char-upcase (char find-string j)))))
					(setf j (svref next j)))
				(do ()
					((or (< j 0) 
						(char= (char search-string i)
							   (char find-string j))))
					(setf j (svref next j)))))))
;;;
;;; KMPSEARCH-FOREIGN
;;;	Returns NIL if not found.
;;;	Otherwise, returns:
;;;    file position
;;;    line number
;;;    start of line position
;;;			
(defun kmpsearch-foreign (find-string search-string start end 
		&key (ignore-case t) (curr-line-number 0))
	(declare (optimize (speed 3)(safety 0)))
	(let ((next (make-array (length find-string)))
		  (find (make-array (length find-string))))
		;; convert chars to bytes
		(dotimes (i (length find-string))
			(setf (aref find i) (char-int (char find-string i))))
		
		;; initialize the 'next' table
		(setf (svref next 0) -1)
		(do ((i  0 (+ i 1))
			 (j -1 (+ j 1)))
			((>= i (length find-string)))
			(if ignore-case
				(do ()
					((or (< j 0) 
						(char= (char-upcase (char find-string i))
							   (char-upcase (char find-string j)))))
					(setf j (svref next j)))
				(do ()
					((or (< j 0) 
						(char= (char find-string i)
							   (char find-string j))))
					(setf j (svref next j))))
			(setf (svref next i) j))
		
		;; start the search
		(do* ((i  0 (the fixnum (+ i 1)))
			  (j  0 (the fixnum (+ j 1)))
			  (curr-line-pos 0)
			  (string-length (length find-string))
			  (length (- end start))
			  (s (+ i start)(the fixnum (+ i start))))
			((or (>= j string-length)(>= i length))
				(if (= j string-length)
					(values (+ (- i j) start) curr-line-number curr-line-pos) 
					nil))
			(declare (fixnum i j s length string-length curr-line-pos))
			(when (= (the fixnum (ct:cref (:unsigned-char *) search-string s)) ascii-newline)
				(incf curr-line-number)
				(setf curr-line-pos (+ s 1)))
			(if ignore-case
				(do ((c (upcase (ct:cref (:unsigned-char *) search-string s))))
					((or (< j 0)(= c (the fixnum (upcase (svref find j))))))
					(declare (fixnum c))
					(setf j (svref next j)))
				(do ((c (ct:cref (:unsigned-char *) search-string s)))
					((or (< j 0)(= c (the fixnum (svref find j)))))
					(declare (fixnum c))
					(setf j (svref next j)))))))

(defun extract-line (start-address position end)
	(let ((length 0))
		(do* ((p position (+ p 1))
			  (c (ct:cref (:unsigned-char *) start-address p)
				 (ct:cref (:unsigned-char *) start-address p)))
			((or (= p (- end 1))(= c ascii-return))(setf length (- p position))))
		(let ((line (make-array length :element-type 'character)))
			(do* ((i 0 (+ i 1))
				  (p position (+ p 1))
				  (c (ct:cref (:unsigned-char *) start-address p)
					 (ct:cref (:unsigned-char *) start-address p)))
				((= i length))
				(setf (char line i) (int-char c)))
			line)))

(defstruct (search-file-reference
		(:print-function print-search-file-reference))
	pathname 
	position
	line-text
	line-number
	line-position)

(defun print-search-file-reference (obj stream level)
	(declare (ignore level))
	(format stream "File ~A, Line ~A: ~A~%" 
		(string-upcase (namestring (search-file-reference-pathname obj)))
		(+ (search-file-reference-line-number obj) 1)
		(search-file-reference-line-text obj)))	
	
;;;
;;;	Returns:
;;;    The line contents that contained the beginning of the string
;;;	   The file position of the found string
;;;    The line number of the found string
;;;    The file position of the beginning of the line
;;;    -or- returns NIL if not found.
;;;
				 
(defun search-file (path string &key (ignore-case t))
	(multiple-value-bind (addr length)
		(ccl:map-file path)
		(unwind-protect
			(when addr
				(do ((refs nil)
					 (start 0)
					 (end length)
					 (line-number 0)
					 result
					 start-of-line-position)
					(nil)
					(let ((ref (make-search-file-reference :pathname (pathname path))))
						(multiple-value-setq (result line-number start-of-line-position)
							(kmpsearch-foreign string addr start end 
								:ignore-case ignore-case
								:curr-line-number line-number))
						(if result
							(setf (search-file-reference-line-text ref)
								(extract-line addr start-of-line-position end))
							(return-from search-file (nreverse refs)))
						(setf start (+ result 1))
						(setf (search-file-reference-position ref) result)
						(setf (search-file-reference-line-number ref) line-number)
						(setf (search-file-reference-line-position ref) start-of-line-position)
						(push ref refs))))
			(if addr (ccl:unmap-file addr))))) 
				 				
(defun display-results (file-refs &optional (stream *standard-output*))
	(dolist (x file-refs)
		(format stream "~A" x)
		(force-output stream)))	

(defun flatten (list)
	(let ((new-list '()))
		(dolist (x list (nreverse new-list))
			(if (atom x)
				(push x new-list)
				(dolist (y (flatten x))
					(push y new-list))))))

(defun find-in-files (string path &key (ignore-case t))
	(dolist (x (flatten (directory path :recurse t)))
		(ccl:editor-set-message (format nil "Searching file: ~A" (namestring x)))
		(display-results (search-file x string :ignore-case ignore-case))))


