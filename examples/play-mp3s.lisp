;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		play-mp3s.lisp
;;;;	Contents:	Touch utility.
;;;;	History:	12/6/02  RGC  Created.
;;;;
;;;;	To create the console application, load this file and use
;;;;    this command:
;;;;        (save-application "play-mp3s" #'main :console t :static t)
;;;;

                                                                        
(defvar *mp3-list-path* (merge-pathnames "mp3-list.lisp"))
(defvar *mp3-bat-file* (merge-pathnames "mp3-playlist.bat"))

;; Path of the Windows multi-media player application.
;; This may be different on different OS versions.
(defvar player "C:\\Program Files\\Windows Media Player\\wmplayer.exe")

;;;
;;; Utility function--splices sublists into outer list.
;;;
(defun flatten (list)
	(let ((new-list '()))
		(dolist (x list (nreverse new-list))
			(if (atom x)
				(push x new-list)
				(dolist (y (flatten x))
					(push y new-list))))))

(defun ends-with (string ending)
    (string-equal (subseq string (- (length string) (length ending))) ending))

;;;
;;; Saves the passed list of files in the output path *mp3-list-path*.
;;; They can later be read by the lisp reader.
;;; Returns T.
;;;
(defun save-mp3-list (mp3-list)
    (with-open-file (out *mp3-list-path* :direction :output)
        (format out ";;;;~%")
        (format out ";;;; MP3 List cached by play-mp3s.exe~%")
        (format out ";;;; Copyright (c) 2003 Corman Technologies~%")            
        (format out ";;;;~%")
        (format out "(~%") ;; )
        (dolist (x mp3-list)
            (format out "    ~A~%" x))  ; (
        (format out ")~%")))        

;;;
;;; Reads a previously saved mp3 file list from the path *mp3-list-path*.
;;; Returns the list of mp3 pathnames.
;;;
(defun load-mp3-list ()
    (with-open-file (in *mp3-list-path* :direction :input)
        (read in)))

;;;
;;; Searches a directory root for files with a .mp3 extension.
;;; Returns a list of files found. If any files were found, the 
;;; list gets written to path *mp3-list-path* as a side-effect.
;;;
(defun find-mp3s (root)
    (format t "Searching for MP3 files beginning at ~A...please be patient.~%" root)
    (force-output)
    (unless (or (ends-with root "\\") (ends-with root "/"))
        (setf root (concatenate 'string root "\\")))
    (let ((result (flatten (directory (concatenate 'string root "*.mp3") :recurse t))))
        (if result (save-mp3-list result))
        result))
                                                                        
(defun random-playlist (mp3s num)
    (let ((list '())
          (len (length mp3s)))
        (dotimes (i num)
            (push (elt mp3s (random len)) list))
        list))

(defun write-random-playlist (mp3s num)
    (let ((list (random-playlist mp3s num)))
        (with-open-file (file *mp3-bat-file* :direction :output)
            (format file "\"~A\"" player)
            (dolist (x list)
                (format file " \"~A\"" (namestring x)))
            (format file "~%"))))
                               
(defun play (num &optional args)
    (let ((mp3s (if (and args (stringp (car args)))
                    (find-mp3s (car args))
                    (load-mp3-list))))
        (if (null mp3s)
            (format t "No mp3 files were found to play.~%")
            (progn
                (write-random-playlist mp3s num)
                (win::system (format nil "~S" (namestring *mp3-bat-file*)))))))

(defun display-usage-info () 
	(format t "Usage: play-mp3s number [path]~%")
	(format t "~10tnumber~20tA number between 1 and 99~%")
    (format t "~10tpath~20tThe root path to begin a search i.e. f:\\mp3!\\Coil~%"))

(defun main ()
	(format t "~%Play-MP3s by Roger Corman~%Copyright (c) 2003 Corman Technologies~%~%")
    (setf *RANDOM-STATE* (make-random-state t))
	(let ((args (ccl:get-command-line-args)))
		(if (null (cdr args))
			(display-usage-info)
            (let ((num (read-from-string (cadr args))))
                (if (and (integerp num) (> num 0) (< num 100))
                    (play num (cddr args))
                    (display-usage-info))))
        (force-output)
		(win:exitprocess 0)))

                                                                                                                                                