;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------

(defun file-to-console (path)
    (with-open-file (in path :direction :input)
        (do* ((eof-val (cons 0 0))
              (c (read-char in nil eof-val)(read-char in nil eof-val)))
            ((eq c eof-val))
            (write-char c *terminal-io*))))

(defun dir-to-console (path)
    (let ((temp-name "tmp000"))
        (win:shell (format nil "dir ~s >~a" (namestring path) temp-name))
        (file-to-console temp-name)
        (delete-file temp-name)))

(define-symbol-macro :pwd (ccl:current-directory))

(define-symbol-macro :cd 
    (let ((path (read)))
        (if (symbolp path)
            (setf path (symbol-name path)))
        (if (string= path "..")
            (setf path "..\\"))
        (setf (ccl:current-directory) path)))

(define-symbol-macro :lsd 
    (let ((path (pathname (read))))
        (dir-to-console path)))

(define-symbol-macro :ls 
    (let ((path "."))
        (dir-to-console path)))

