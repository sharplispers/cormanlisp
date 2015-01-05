
(require 'find-in-files)  ;; for kmpsearch-list

(defstruct (download-record 
        (:constructor make-download-record (name organization country email add-to-list)))
    name 
    organization 
    country 
    email 
    add-to-list)

(defun split-string (string)
	"Separates a string at commas"
	(let ((tokens nil)
          (curr (make-array 256 :element-type 'character :fill-pointer 0)))
        (dotimes (i (length string))
            (let ((c (char string i)))
                (if (char= c #\,)
                    (progn
                        (push (copy-seq curr) tokens)
                        (setf (fill-pointer curr) 0))
                    (vector-push-extend c curr))))
        (push (copy-seq curr) tokens)
        (nreverse tokens)))

(defun trim-spaces (string &optional (whitespace-chars '(#\space #\tab #\newline)))
    "Eliminates white space at the beginning and end of a string"
    (if (= (length string) 0)
        (return-from trim-spaces string))
	(flet ((whitespace-char? (char)
			(member char whitespace-chars :test #'char=)))
        (let* ((start 0)
               (len (length string))
               (i 0))
            (do (c)
                ((= i len)(return-from trim-spaces (subseq string 0 0)))    ;; return empty string
                (setf c (char string i))
                (unless (whitespace-char? c)
                    (setf start i)
                    (return))
                (incf i))
            (do* ((i (- len 1) (- i 1))
                  (c (char string i) (char string i)))
                ((not (whitespace-char? c))(subseq string start (+ i 1)))))))

(defun read-download-record (stream)
    (let* ((s (read-line stream nil 'eof))
            rec)
        (if (eq s 'eof)
            (return-from read-download-record nil))
        (setf rec (apply 'make-download-record (mapcar 'trim-spaces (split-string s))))
        (if (string-equal (download-record-add-to-list rec) "true")
            (setf (download-record-add-to-list rec) t)
            (setf (download-record-add-to-list rec) nil))
        rec))

(defun read-download-records (path)
    (let ((records '()))
        (with-open-file (stream path :direction :input)
            (do ((rec (read-download-record stream)(read-download-record stream)))
                ((null rec)(nreverse records))
                (push rec records)))))

(defun check-for-extra-commas (path)
    (let ((line 1))
        (with-open-file (stream path)
            (do ((s (read-line stream nil 'eof)(read-line stream nil 'eof)))
                ((eq s 'eof))
                (if (/= (count #\, s) 4)
                    (format t "File ~A, Line ~D: ~A~%" (namestring path) line s))
                (incf line)))))

(defun valid-email-address (string)
    (unless (= (count #\@ string) 1)
        (return-from valid-email-address nil))
    (let* ((pos (position #\@ string))
           (name (subseq string 0 pos))
           (domain (subseq string (+ pos 1))))
        (if (and (> (length name) 0) 
                (> (length domain) 2) 
                (find #\. domain)
                (> (position #\. domain) 0)
                (< (position #\. domain) (- (length domain) 1)))
             t nil)))

(defun fix-commas (string)
    (do () 
        ((null (ccl::kmpsearch-lisp "%2c" string)) string)
        (let ((pos (ccl::kmpsearch-lisp "%2c" string)))
            (setf string (concatenate 'string (subseq string 0 pos) "," (subseq string (+ pos 3)))))))

(defun fix-record-commas (rec)
    (setf (download-record-name rec) (fix-commas (download-record-name rec)))
    (setf (download-record-organization rec) (fix-commas (download-record-organization rec)))    
    (setf (download-record-email rec) (fix-commas (download-record-email rec)))
    rec)

(defun create-tabbed-file (path records)
    (with-open-file (output path :direction :output)
        (dolist (x records)
            (format output "~A" (download-record-name x)) (write-char #\tab output)
            (format output "~A" (download-record-email x)) (write-char #\tab output)
            (format output "~A" (download-record-organization x)) (write-char #\tab output)
            (format output "~A" (download-record-country x)) (write-char #\tab output)
            (format output "~A" (if (download-record-add-to-list x) "yes" "no")) (write-char #\tab output)
            (terpri output))))
                            
#|
(setf download-records (read-download-records "../cormanlisp.downloads"))

(check-for-extra-commas "../cormanlisp.downloads")

(with-open-file (output "cormanlisp-downloads.lisp" :direction :output)
    (format output "(setf *download-records* '(~%") ;; ))
    (dolist (x *download-records*)
        (format output "~S~%" x))   ; ((
    (format output "))~%")
    )

(setf download-records (remove-if (lambda (x) (= (length (download-record-email x)) 0))
         *download-records*))

|#

