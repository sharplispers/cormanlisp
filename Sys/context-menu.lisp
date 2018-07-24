;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		context-menu.lisp
;;;;	Contents:	Handles IDE context menu.
;;;;	History:	9/22/03  RGC  Added Make Uppercase, Make Lowercase options.
;;;;

(in-package :win32)

#! (:library "User32" :ignore "WINUSERAPI" :export t :pascal "WINAPI")
/*
 * Flags for TrackPopupMenu
 */
#define TPM_LEFTBUTTON  	0x0000L
#define TPM_RIGHTBUTTON 	0x0002L
#define TPM_LEFTALIGN   	0x0000L
#define TPM_CENTERALIGN 	0x0004L
#define TPM_RIGHTALIGN  	0x0008L
#define TPM_TOPALIGN        0x0000L
#define TPM_VCENTERALIGN    0x0010L
#define TPM_BOTTOMALIGN     0x0020L
#define TPM_HORIZONTAL      0x0000L     /* Horz alignment matters more */
#define TPM_VERTICAL        0x0040L     /* Vert alignment matters more */
#define TPM_NONOTIFY        0x0080L     /* Don't send any notification msgs */
#define TPM_RETURNCMD       0x0100L
#define TPM_RECURSE         0x0001L
#define TPM_HORPOSANIMATION 0x0400L
#define TPM_HORNEGANIMATION 0x0800L
#define TPM_VERPOSANIMATION 0x1000L
#define TPM_VERNEGANIMATION 0x2000L
#define TPM_NOANIMATION     0x4000L
#define TPM_LAYOUTRTL       0x8000L

WINUSERAPI HMENU WINAPI CreatePopupMenu();
WINUSERAPI int WINAPI TrackPopupMenu(HMENU hMenu, unsigned int uFlags, int x, int y, 
	int nReserved, HWND hWnd, CONST RECT *prcRect);
WINUSERAPI BOOL WINAPI AttachThreadInput(DWORD idAttach, DWORD idAttachTo, BOOL fAttach);
!#

;;;; from Larry Hunter
(defun ccl::lex-string (string &optional (whitespace-chars '(#\space #\newline)))
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

(defun pretty-print-selection (form)
	(let ((*print-case* :downcase)
		  (*print-pretty* t))
		(cl::editor-replace-selection (format nil "~S" form))))

;;
;; adding documentation to the context menu
;;
(defun documentation-selection (symbol &optional (type 'function))
    (if (eq (symbol-package symbol) (find-package :common-lisp))
        (unless (and (boundp 'pl::*hyperspec-loaded*) pl::*hyperspec-loaded*)
            (load (concatenate 'string pl:*cormanlisp-directory* "/sys/hyperspec.lisp"))))
    
    (let ((doclist (gethash symbol cl::*documentation-registry*))
           doc-clause)
        
        ;; if the requested symbol is in the common-lisp package, and
        ;; has documentation of type hyperspec as the first type (LC: Is this convenient?), then
        ;; use a special algorithm to display the information from the hyperspec
        (if (and (eq (car doclist) ':hyperspec) 
                (eq (symbol-package symbol) (find-package 'common-lisp)))
            (setq type ':hyperspec))
        (setq doc-clause (getf doclist type))
        (unless doc-clause 
            (return-from documentation-selection (format nil "No documentation available for ~A ~A" type symbol)))
        (if (eq type ':hyperspec)
            (progn (pl:hyperspec symbol) (values))
            ;; else just return the doc string
            doc-clause
            #|
            (progn
            (win::message-box-ok doc-clause 
            (format nil "Documentation for ~A ~A" type symbol))
            (values))
            |# )))

;; eps,
;; adding macroexpand to the context menu
;;
(defun macroexpand-selection (form)
    (let ((*print-case* :downcase)
          (*print-pretty* t))
        (format *terminal-io* "~%~S" (macroexpand form))
        (force-output *terminal-io*)))

;; eps,
;; adding macroexpand-1 to the context menu
;;
(defun macroexpand-1-selection (form)
    (let ((*print-case* :downcase)
          (*print-pretty* t))
        (format *terminal-io* "~%~S" (macroexpand-1 form))
        (force-output *terminal-io*)))

(defun get-file-and-line (selection)
    (let* ((file-mark-string "File ")
           (line-mark-string "Line")
           (file-mark (search file-mark-string selection))
           (comma-mark (search "," selection)))
        (if (and (integerp file-mark)(integerp comma-mark) (> comma-mark file-mark))
            (let* ((path-string (subseq selection (+ file-mark (length file-mark-string)) comma-mark))
                   (line-string (subseq selection (+ comma-mark 1)))
                   (line-string-tokens (ccl::lex-string line-string '(#\Space #\newline))))
                (if (equal (first line-string-tokens) line-mark-string)
                    (values path-string (first (ccl::lex-string (second line-string-tokens) '(#\:)))))))))  ;; strip trailing #\:

;;;
;;; If the selected form is suitable for this menu option, add it
;;; to the context menu, push an appropriate function on the
;;; func-list, and return the new func-list.
;;; Otherwise do not add the option, and return the passed func-list.
;;;
(defun add-documentation-option (menu func-list selected-form)
    (when (symbolp selected-form)
		(win::AppendMenu menu
			(logior win::MF_ENABLED win::MF_STRING)
            (+ 1 (length func-list))
			(ct:create-c-string (format nil "Documentation for ~A" selected-form)))
		(push #'(lambda () 
                (format *terminal-io* "~%~A~%" (documentation-selection selected-form))
                (force-output *terminal-io*)) func-list))
    func-list)

;;; Same comments as above	
(defun add-lookup-source-option (menu func-list selected-form)
    (when (and (symbolp selected-form)
            (or
                (and (fboundp selected-form)
                    (functionp (symbol-function selected-form))
		    (not (typep (symbol-function selected-form) 'generic-function))
                    (ccl:function-source-file (symbol-function selected-form)))
                (and (x86::code-generator-function selected-form)
                    (ccl:function-source-file (x86::code-generator-function selected-form)))))
        (win::AppendMenu menu
			(logior win::MF_ENABLED win::MF_STRING)
            (+ 1 (length func-list))
   			(ct:create-c-string (format nil "Lookup source for ~A, package ~A" 
                    (symbol-name selected-form)
                    (package-name (symbol-package selected-form)))))
		(push #'(lambda () (db:find-source selected-form)) func-list))
    func-list)

;;; Same comments as above	
(defun add-pretty-print-option (menu func-list selected-form)
    (win::AppendMenu menu
        (logior win::MF_ENABLED win::MF_STRING)
        (+ 1 (length func-list))
        (ct:create-c-string (format nil "Pretty-Print Selection")))
    (push #'(lambda () (pretty-print-selection selected-form)) func-list)
    func-list)

;; eps,
;; adding macroexpand to context menu
;; same comments as above.
;;
(defun add-macroexpand-option (menu func-list selected-form)
    (win::AppendMenu menu
        (logior win::MF_ENABLED win::MF_STRING)
        (+ 1 (length func-list))
        (ct:create-c-string (format nil "Macroexpand Selection")))
    (push #'(lambda () (macroexpand-selection selected-form)) func-list)
    func-list)

;; eps,
;; adding macroexpand-1 to context menu
;; same comments as above.
;;
(defun add-macroexpand-1-option (menu func-list selected-form)
    (win::AppendMenu menu
        (logior win::MF_ENABLED win::MF_STRING)
        (+ 1 (length func-list))
        (ct:create-c-string (format nil "Macroexpand-1 Selection")))
    (push #'(lambda () (macroexpand-1-selection selected-form)) func-list)
    func-list)

(defun add-lisp-variable-display (menu func-list selected-form)
    (win::AppendMenu menu
        (logior win::MF_ENABLED win::MF_STRING)
        (+ 1 (length func-list))
        (ct:create-c-string (format nil "Add to Lisp Variables Panel")))
    (push #'(lambda () (ide:add-lisp-display-variable (format nil "~A" selected-form) selected-form)) func-list)
    func-list)

(defun add-uppercase-option (menu func-list selected-form selection)
    (declare (ignore selected-form))
    (win::AppendMenu menu
        (logior win::MF_ENABLED win::MF_STRING)
        (+ 1 (length func-list))
        (ct:create-c-string (format nil "Make Uppercase")))
    (push #'(lambda () (cl::editor-replace-selection (string-upcase selection))) func-list)
    func-list)

(defun add-lowercase-option (menu func-list selected-form selection)
    (declare (ignore selected-form))
    (win::AppendMenu menu
        (logior win::MF_ENABLED win::MF_STRING)
        (+ 1 (length func-list))
        (ct:create-c-string (format nil "Make Lowercase")))
    (push #'(lambda () (cl::editor-replace-selection (string-downcase selection))) func-list)
    func-list)

;;; Same comments as above	
(defun add-file-and-line-option (menu func-list selected-form selection)
    (declare (ignore selected-form))
    (multiple-value-bind (file line)
        (get-file-and-line selection)
        (when (and (stringp file) (stringp line) (probe-file file)
                (integerp (read-from-string line)))
            (let ((lineno (parse-integer line :junk-allowed t)))
                (win::AppendMenu menu
                    (logior win::MF_ENABLED win::MF_STRING)
                    (+ 1 (length func-list))
                    (ct:create-c-string (format nil "Edit file ~A, line ~D" file lineno)))    
                (push 
                    #'(lambda ()
                        (ed file)
                        (ide::set-selection file (- lineno 1) 0 (- lineno 1) 200)) func-list))))
    func-list)

(defun add-colorize-window-option (menu func-list selected-form selection)
    (declare (ignore selected-form selection))
    (win::AppendMenu menu
        (logior win::MF_ENABLED win::MF_STRING)
        (+ 1 (length func-list))
        (ct:create-c-string (format nil "Colorize Window")))
    (push #'(lambda () (ide::colorize-current-window)) func-list)
    func-list)

;;;
;;; Returns T iff the passed string is composed wholly of constituent chars,
;;; possibly including #\: (package separators).
;;;
(defun string-is-symbol (str)
    (with-input-from-string (s str)
        (do ((ch (read-char s nil nil)(read-char s nil nil))
             (i 0 (+ i 1)))
            ((null ch) t)
            (unless (cl::constituent-char ch)
                (return-from string-is-symbol nil)))))

(defun potential-symbols (str)
    (if (find #\: str)
        (return-from potential-symbols nil))
    (setf str (string-upcase str))
    (let ((syms '()))
        (dolist (package (list-all-packages))
            (multiple-value-bind (sym status)
                (find-symbol str package)
                (if (or (eq status ':internal)(eq status ':external))
                    (push sym syms))))
        (nreverse syms)))

(defun ccl::ide-context-menu (x y win)
	(let* ((menu (CreatePopupMenu))
		   (selection (ide::get-current-selection-text))
		   (func-list nil)
            selected-form
            err
            alt-symbols)    ;; from other packages
        (when (null selection)
            (setf func-list (add-colorize-window-option menu func-list nil selection))
            (return-from ccl::ide-context-menu))
        (multiple-value-setq (selected-form err)
			(ignore-errors (read-from-string selection)))
        (when (not (typep err 'error))
            (setf alt-symbols (potential-symbols selection))
            (setf func-list (add-documentation-option menu func-list selected-form))
            (setf func-list (add-lookup-source-option menu func-list selected-form))
            (dolist (sym (remove selected-form alt-symbols))
                (setf func-list (add-lookup-source-option menu func-list sym)))  
            (setf func-list (add-pretty-print-option menu func-list selected-form))
            (setf func-list (add-file-and-line-option menu func-list selected-form selection))
            (setf func-list (add-macroexpand-option menu func-list selected-form))
            (setf func-list (add-macroexpand-1-option menu func-list selected-form))
            (setf func-list (add-lisp-variable-display menu func-list selected-form)))
        (setf func-list (add-uppercase-option menu func-list selected-form selection))
        (setf func-list (add-lowercase-option menu func-list selected-form selection))
        (setf func-list (add-colorize-window-option menu func-list selected-form selection))      		        
        (setf func-list (nreverse func-list))
		(if (null func-list) (return-from ccl::ide-context-menu))	;; no menu items	
		(let ((ret
			(TrackPopupMenu menu (logior TPM_LEFTALIGN TPM_RIGHTBUTTON TPM_NONOTIFY TPM_RETURNCMD)
				x y 0 win ct:null)))
			(ignore-errors (funcall (nth (- ret 1) func-list)))
			ret)))
    
(ct:defun-direct-c-callback pl::on-context-menu ((x :long)(y :long)(win (:void *)))
	(ccl::ide-context-menu x y win))

(ct:defun-direct-c-callback pl::on-heap-size ((generation :long)(capacity (:long *))(used (:long *)))
	(multiple-value-bind (percent total current)
		(cl::heap-used generation)
		(setf (ct:cref (:long *) capacity 0) total)
		(setf (ct:cref (:long *) used 0) current)
		percent))

;;; Given a string, see if it represents a known symbol. Don't intern
;;; the symbol, if it doesn't exist. This is used to lookup arbitrary strings
;;; from the IDE, and we don't want to intern stuff that doesn't normally
;;; get evaluated.
;;; If found, return the symbol, otherwise return the integer 0.
;;;
(defun lookup-symbol (string)
	(let ((package-chars nil)
		  (symbol-chars nil)
		  (package-markers 0)
		  (package-name nil)
		  (symbol-name nil)
		  (marker-pos (position #\: string))
		  (length (length string)))
		
		(unless marker-pos
			(multiple-value-bind (sym found)
				(find-symbol (string-upcase string) *package*)
				(return-from lookup-symbol (if found sym 0))))
		
		;; handle explicit package
		(setq package-chars (subseq string 0 marker-pos))						
		(incf package-markers)
		(incf marker-pos)
		(when (and (< marker-pos length) (eq (char string marker-pos) #\:))
			(incf package-markers)
			(incf marker-pos))
		(setf symbol-chars (string-upcase (subseq string marker-pos length)))

		(if (> (length package-chars) 0)
			(setq package-name (string-upcase package-chars))
			(setq package-name "KEYWORD"))
		
		(if (> (length symbol-chars) 0)
			(setq symbol-name (string-upcase symbol-chars))
			(return-from lookup-symbol 0))

		(let ((package (find-package package-name)))
			(if (null package)
				(return-from lookup-symbol 0))
			(multiple-value-bind (sym state)
				(find-symbol symbol-name package)
				(if (and (= package-markers 1) (not (eq state ':external)))
					0
					(if state sym 0))))))

;;
;; Converts any newlines in a string to CR-Newline pairs.
;;
(defun format-newlines-for-windows (str)
    (let ((count 0))
        (dotimes (i (length str))
            (if (eq (aref str i) #\newline)
                (incf count)))
        (if (= count 0)
            str        
            (let ((new-str (make-array (+ (length str) count) :element-type 'character :fill-pointer 0)))
                (dotimes (i (length str))
                    (let ((ch (aref str i)))
                        (if (eq ch #\newline)
                            (vector-push #\return new-str))
                        (vector-push ch new-str)))
                new-str))))
                    
;; Returns a string representation of the passed Lisp object, suitable for inclusion
;; in a tooltip
;;
(defun format-for-tooltip (value)
    (let ((os (make-string-output-stream)))
        ;; avoid processing extremely long lists by shortening them to 9 elements
        (if (and (consp value) (> (length value) 8))
            (setf value (subseq value 0 9)))
        (write value :stream os :length 8 :circle t :pretty t :level 3)
        (let* ((str (get-output-stream-string os)))
            (when (> (length str) 128)
                (setq str (concatenate 'string (subseq str 0 128) "...")))  ;; return maximum of 128 chars
            ;; replace any newline with cr-newline
            (format-newlines-for-windows str))))

(defun lookup-lambda-list-impl (sym)
    (cond 
        ((and (symbolp sym)(x86::code-generator-function sym))
         (format nil "~A: ~A" sym "CODE-GENERATOR"))            
        ((and (symbolp sym)(fboundp sym))
		 (let ((func (symbol-function sym))
			   (lambda-list nil)
			   (function-type "Function"))
			(if (macro-function sym)
				(setf lambda-list (ccl::macro-lambda-list func) function-type "Macro")
				(if (cl::standard-generic-function-p func)
					(setf lambda-list (generic-function-lambda-list func) function-type "Generic-Function")
					(setf lambda-list (ccl:function-lambda-list func))))
                (let ((astr (format nil "~{~A ~}" lambda-list)))
				    (format nil "~A ~A[~A]" sym (string-downcase astr) function-type))))
        ((and (symbolp sym)(boundp sym))
         (let ((val (symbol-value sym)))
            (format nil "~A: ~A" sym (format-for-tooltip val))))))
                          
(ct:defun-direct-c-callback ccl::lookup-lambda-list ((symName (:char *))(buf (:char *))(bufLength :long))
	(setf (ct:cref (:char *) buf 0) 0)     ;; initialize to empty string
    (ignore-errors
        (let* ((sym (lookup-symbol (ct:c-string-to-lisp-string symName)))
               (str (lookup-lambda-list-impl sym)))
            (if (stringp str)
                (do* ((n 0 (+ n 1))
    		          (len (min (length str) (- bufLength 1))))
                    ((= n len)(setf (ct:cref (:char *) buf n) 0) n)
                    (setf (ct:cref (:char *) buf n) (char-int (char str n))))
                0))))

(ct:defun-direct-c-callback ccl::load_file ((path (:char *)))
	(load (ct:c-string-to-lisp-string path)))

(ct:defun-direct-c-callback ccl::version_caption ((buf (:char *)))
	(ct:lisp-string-to-c-string (cl::version-caption) buf))
	
					

		
