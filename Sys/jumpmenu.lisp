;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		jumpmenu.lisp
;;;;	Contents:	Corman Lisp Jump Menu facility.
;;;;	History:	EPS  07/12/06  Created.

(in-package :ide)

(defun jumpmenu-read-symbol-token (stream)
    (let ((chars nil)
          (start (cl::stream-position stream)))
        (do ((ch (cl::__read-char stream nil ':eof)(cl::__read-char stream nil ':eof)))
            ((or (not (characterp ch))(not (cl::constituent-char ch)))
             (cond ((eq ch ':eof) ch)
                   ((null chars) nil)
                   (t (unread-char ch stream)
                      (list ':symbol (coerce (nreverse chars) 'string)
                            start (cl::stream-position stream)))))
            (push ch chars))))

(defun jumpmenu-read-token (stream)
    (let ((ch (cl::read-char-skip-white-space stream)))
        (cond ((not (characterp ch)) ':eof)
              ((char= ch #\;) nil)
              ((char= ch #\#) nil)
              ((char= ch #\") nil)
              ((cl::constituent-char ch)(unread-char ch stream)(jumpmenu-read-symbol-token stream)))))

(defun jumpable-symbol-name-p (sym)
    (member sym *declaration-symbols* :test #'equalp))

(defun create-jumpmenu-from-buffer (stream offset)
    (let ((jumpmenu '()))
        (do* ((form (jumpmenu-read-token stream)(jumpmenu-read-token stream))
              (curr-package nil)) 
            ((eq ':eof form))
            (if form
                (progn
                    (let ((type (first form))
                          (string (second form))
                          (start (+ (third form) offset)))
                        (cond
                            ((eq type ':symbol)
                             (if (jumpable-symbol-name-p string)
                                (progn
                                    ;; get the next symbol
                                    ;; if it's null continue
                                    ;; if it's :eof return
                                    ;; otherwise collect it.
                                    (let ((form2 (jumpmenu-read-token stream)))
                                        (if (and form2 (not (eq form2 ':eof)))
                                                (progn
                                                    (let ((type2 (first form2))
                                                          (string2 (second form2))
                                                          (end2 (+ (fourth form2) offset)))
                                                        (if (and (equalp string "in-package")
                                                                (or (eq type2 ':symbol) (eq type2 ':string)))
                                                            (setq curr-package string2))
                                                        (if (eq type2 ':symbol)
                                                            (push (list string string2 start end2 curr-package) jumpmenu))))))))))))))
        (nreverse jumpmenu)))

(defun create-jumpmenu-from-window (hwnd start end)
    (let* ((text (get-editor-buffer-contents hwnd))
           (in (make-string-input-stream 
                    (subseq text start 
                        (min end (get-editor-buffer-length hwnd))))))                                               
        (create-jumpmenu-from-buffer in start)))                     

(defparameter *jump-menu-handle* nil)

(defun add-jumpmenu-to-menubar () #| forward declaration |#)

(defun create-jumpmenu-menu ()
    (win:create-dynamic-menu-item 
        (list :menu "&Declarations"
            (lambda (hmenu)
                (declare (ignore hmenu))
                (add-jumpmenu-to-menubar))) 
        nil 
        4))

(defun format-message (sym-str package)
    (if (and (stringp package) (or (char= (char package 0) #\')(char= (char package 0) #\:)))
        (setf package (subseq package 1)))
    (if (stringp package)
        (setf package (string-upcase package)))
    (multiple-value-bind (sym status)
        (find-symbol (string-upcase sym-str) package)
        (if status
            (format nil "Package: ~A    ~A" (package-name (symbol-package sym)) (win::lookup-lambda-list-impl sym))))) 
           
(defun add-jumpmenu-to-menubar ()  
    (let* ((hwnd (current-edit-window-handle))
           (jumpmenu (sort (create-jumpmenu-from-window hwnd 0 (get-editor-buffer-length hwnd)) 'string-lessp :key 'second))
           (menu-handle 
                (or *jump-menu-handle* (create-jumpmenu-menu))))
        (setf *jump-menu-handle* menu-handle)
        (loop for i from 1 to (length jumpmenu) do
            (let ((item (elt jumpmenu (- i 1))))
                (win:create-dynamic-menu-item 
                    (list :command (concatenate 'string (string-capitalize (first item)) " &" (string-capitalize (second item)))
                        (lambda (id)
                            (declare (ignore id))
                            (set-current-selection hwnd (third item) (fourth item))
                            (values)))
                    menu-handle 
                    i
                    :message (lambda (id)
                        (declare (ignore id))
                        (format-message (second item) (fifth item))))))))

(export 'add-jumpmenu-to-menubar)

#|
(defun on-init-menu-impl (menu-handle)
    (if (ct:cpointer= menu-handle *jump-menu-handle*)
        (let ((count (win:GetMenuItemCount menu-handle)))
            (loop for i from (- count 1) downto 0 do
                (win:RemoveMenu menu-handle i win:MF_BYPOSITION))
            (add-jumpmenu-to-menubar))))

(defun on-init-menu-popup-impl (menu-handle uint bool)
    (declare (ignore uint bool))
    (if (ct:cpointer= menu-handle *jump-menu-handle*)
        (let ((count (win:GetMenuItemCount menu-handle)))
            (loop for i from (- count 1) downto 0 do
                (win:remove-menu-item menu-handle i))
            (add-jumpmenu-to-menubar))))
|#
