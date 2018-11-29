;;;;; Stolen from https://github.com/sbcl/sbcl/blob/master/tools-for-build/rtf.lisp

;;;; Generate RTF out of a regular text file, splitting
;;;; paragraphs on empty lines.
;;;;
;;;; Used to generate License.rtf out of COPYING for the
;;;; Windows installer.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(defun read-lines (pathname &optional reverse)
  (let ((lines)
        (oldpos))
    (with-open-file (f pathname :external-format :ascii)
      (do ((text (read-line f nil) (read-line f nil)))
          ((null text))
        (if reverse
            (push text lines)
            (if lines
                (setf (cdr oldpos) (cons text nil)
                      oldpos (cdr oldpos))
                (setf oldpos
                      (push text lines))))))
    lines))

(defun looks-like-numbered-list-member (str)
  (multiple-value-bind (i p) (parse-integer str :junk-allowed t)
    (and i (char= #\. (aref str p)))))

(defun cleanup-string (str)
  (let ((result (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (str (string-trim '(#\Space #\Tab #\Return) str)))
    (dotimes (i (length str))
      (let ((c (aref str i)))
        (cond
          ((char= c #\{)
           (vector-push-extend #\\ result)
           (vector-push-extend #\' result)
           (vector-push-extend #\7 result)
           (vector-push-extend #\b result))
          ((char= c #\})
           (vector-push-extend #\\ result)
           (vector-push-extend #\' result)
           (vector-push-extend #\7 result)
           (vector-push-extend #\d result))
          ((char= c #\\)
           (vector-push-extend #\\ result)
           (vector-push-extend #\' result)
           (vector-push-extend #\5 result)
           (vector-push-extend #\c result))
          ((> (char-code c) 127)
           (vector-push-extend #\\ result)
           (vector-push-extend #\' result)
           (let ((code-text (format nil "~A" (char-code c))))
             (dotimes (i (length code-text))
               (vector-push-extend (aref code-text i) result))))
          (t (vector-push-extend c result)))))
    result))

    
(defun lines-to-paragraphs (lines)
  (let ((pars))
    (dolist (l lines)
      (let ((text (cleanup-string l)))
        (cond
          ((and (plusp (length text))
                (null pars))
           (push text pars))
          ((plusp (length text))
           (cond
             ;; some heuristics to detect numbered lists 
             ((and (car pars)
                   (looks-like-numbered-list-member text))
              (push text pars))
             ((car pars)
              (setf (car pars) (concatenate 'string (car pars) " " text)))
             (t (setf (car pars) text))))
          (t (when (car pars)
               (push nil pars))))))
    (nreverse (if (car pars)
                  pars
                  (cdr pars)))))


(defun write-rtf (pars pathname)
  (with-open-file (f pathname :direction :output :external-format :ascii
                     :if-exists :supersede)
    ;; \rtf0 = RTF 1.0
    ;; \ansi = character set
    ;; \deffn = default font
    ;; \fonttbl = font table
    ;; \fs = font size in half-points
    (format f "{\\rtf1\\ansi~
                \\deffn0~
                {\\fonttbl\\f0\\fswiss Helvetica;}~
                \\fs20~
                ~{~A\\par\\par ~}}"      ; each par used to end with
                                         ; ~%, but resulting Rtf looks
                                         ; strange (WinXP, WiX 3.0.x,
                                         ; ?)
                         pars)))

(defun cleanup-hyperspec-terms (pars)
  (mapcar #'(lambda (par)
              (cond
                ((string= par "------------------------------------------------------------------------ AUTHORSHIP INFORMATION ------------------------------------------------------------------------")
                 "\\i AUTHORSHIP INFORMATION\\i0")
                ((string= par "------------------------------------------------------------------------ IMPORTANT LEGAL NOTICES ------------------------------------------------------------------------")
                 "\\i IMPORTANT LEGAL NOTICES\\i0")
                (t par)))
          pars))

(defun find-zlib-license (lines &aux (copyright-lines nil) (found-copyright nil))
  (dolist (l lines)
    (let ((text (cleanup-string l)))
      (if found-copyright
          (push text copyright-lines)
          (setf found-copyright (string= "Copyright notice:" text)))))
  (nreverse copyright-lines))

;; generate RTF file from TXT files
(defun generate-license-rtf (corman-license hyperspec-license openssl-license distorm-license zlib-license to)
  (let ((corman-pars (lines-to-paragraphs (read-lines corman-license)))
        (hyperspec-pars (lines-to-paragraphs (read-lines hyperspec-license)))
        (openssl-pars (lines-to-paragraphs (read-lines openssl-license)))
        (distorm-pars (lines-to-paragraphs (read-lines distorm-license)))
        (zlib-pars (lines-to-paragraphs (find-zlib-license (read-lines zlib-license)))))
    (write-rtf (concatenate 'list
                            '("\\b 1. Corman Lisp terms of use:\\b0")
                            corman-pars
                            '("\\b 2. ZLib terms of use:\\b0")
                            zlib-pars
                            '("\\b 3. diStorm terms of use:\\b0")
                            distorm-pars
                            '("\\b 4. HyperSpec terms of use:\\b0")
                            (cleanup-hyperspec-terms hyperspec-pars)
                            '("\\b 5. OpenSSL terms of use:\\b0")
                            openssl-pars
                            '("\\b P.S. Certain code in the 'Modules' and 'Libraries' subdirectories carries different licensing terms. See the individual modules and libraries for details after the installation.\\b0"))
               to)))

(generate-license-rtf
    (concatenate 'string *cormanlisp-directory* "LICENSE.txt")
    (concatenate 'string *cormanlisp-directory* "HyperSpec-Legalese.text")
    (concatenate 'string *cormanlisp-directory* "LICENSE.OpenSSL.txt")
    (concatenate 'string *cormanlisp-directory* "CormanLispServer\\distorm\\COPYING")
    (concatenate 'string *cormanlisp-directory* "zlib\\README")
    (concatenate 'string *cormanlisp-directory* ".\\installer\\LICENSE.rtf"))


