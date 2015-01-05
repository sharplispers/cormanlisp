;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		open-file.lisp
;;;;	Contents:	Implementation of GET-OPEN-FILE-NAME function.
;;;;	History:	6/2/98  RGC  Created.
;;;;

(in-package :win32)
(export 'get-open-file-name)

(defwintype LPOFNHOOKPROC farproc)

(defwinconstant OFN_READONLY                 #x00000001)
(defwinconstant OFN_OVERWRITEPROMPT          #x00000002)
(defwinconstant OFN_HIDEREADONLY             #x00000004)
(defwinconstant OFN_NOCHANGEDIR              #x00000008)
(defwinconstant OFN_SHOWHELP                 #x00000010)
(defwinconstant OFN_ENABLEHOOK               #x00000020)
(defwinconstant OFN_ENABLETEMPLATE           #x00000040)
(defwinconstant OFN_ENABLETEMPLATEHANDLE     #x00000080)
(defwinconstant OFN_NOVALIDATE               #x00000100)
(defwinconstant OFN_ALLOWMULTISELECT         #x00000200)
(defwinconstant OFN_EXTENSIONDIFFERENT       #x00000400)
(defwinconstant OFN_PATHMUSTEXIST            #x00000800)
(defwinconstant OFN_FILEMUSTEXIST            #x00001000)
(defwinconstant OFN_CREATEPROMPT             #x00002000)
(defwinconstant OFN_SHAREAWARE               #x00004000)
(defwinconstant OFN_NOREADONLYRETURN         #x00008000)
(defwinconstant OFN_NOTESTFILECREATE         #x00010000)
(defwinconstant OFN_NONETWORKBUTTON          #x00020000)
(defwinconstant OFN_NOLONGNAMES              #x00040000)     ;; force no long names for 4.x modules
(defwinconstant OFN_EXPLORER                 #x00080000)     ;; new look commdlg
(defwinconstant OFN_NODEREFERENCELINKS       #x00100000)
(defwinconstant OFN_LONGNAMES                #x00200000)     ;; force long names for 3.x modules

(defwintype LPCTSTR 	lpstr)

(defwinstruct OPENFILENAME
((lStructSize			DWORD)
 (hwndOwner				HWND)
 (hInstance				HINSTANCE)
 (lpstrFilter			LPCTSTR)
 (lpstrCustomFilter		LPTSTR)
 (nMaxCustFilter		DWORD)
 (nFilterIndex			DWORD)
 (lpstrFile				LPTSTR)
 (nMaxFile				DWORD)
 (lpstrFileTitle		LPTSTR)
 (nMaxFileTitle			DWORD)
 (lpstrInitialDir		LPCTSTR)
 (lpstrTitle			LPCTSTR)
 (Flags					DWORD)
 (nFileOffset			WORD)
 (nFileExtension		WORD)
 (lpstrDefExt			LPCTSTR)
 (lCustData				DWORD)
 (lpfnHook				LPOFNHOOKPROC)
 (lpTemplateName		LPCTSTR)
))

(defvar *open-file-info* (ct:malloc (ct:sizeof 'OPENFILENAME)))
(defwinconstant _MAX_PATH		260)
(defwinconstant _MAX_FNAME		256)
(defwinconstant _MAX_EXT		256)


;; BOOL  APIENTRY     GetOpenFileName(LPOPENFILENAMEA);
(defwinapi GetOpenFileName
	((s (OPENFILENAME *)))
	:return-type BOOL
	:library-name "comdlg32.dll"
	:entry-name "GetOpenFileNameA"
	:linkage-type :pascal)

;; DWORD CommDlgExtendedError()
(defwinapi CommDlgExtendedError
	()
	:return-type DWORD
	:library-name "comdlg32.dll"
	:entry-name "CommDlgExtendedError"
	:linkage-type :pascal)

;; take all the filter strings passed, and concatenate them all
;; together into a c string with null bytes terminating each one
(defun make-filter-string (filters)
	(let ((index 0)
		  (cstr (ct:malloc 
			(+ (reduce #'+ (mapcar #'length filters)) (length filters) 1))))
		(dolist (f filters)
			(dotimes (i (length f))
				(setf (cref LPCSTR cstr index) (char-int (elt f i)))
				(incf index))
			(setf (cref LPCSTR cstr index) 0)
			(incf index))
		(setf (cref LPCSTR cstr index) 0)
		cstr))

(defun init-open-file (hwnd filters)
	(setf (cref OPENFILENAME *open-file-info* lStructSize) (ct:sizeof 'OPENFILENAME))
	(setf (cref OPENFILENAME *open-file-info* hwndOwner) hwnd)
	(setf (cref OPENFILENAME *open-file-info* hInstance) ct:null)
	(setf (cref OPENFILENAME *open-file-info* lpstrFilter) (make-filter-string filters))
	(setf (cref OPENFILENAME *open-file-info* lpstrCustomFilter) ct:null)
	(setf (cref OPENFILENAME *open-file-info* nMaxCustFilter) 0)
	(setf (cref OPENFILENAME *open-file-info* nFilterIndex) 0)
	(setf (cref OPENFILENAME *open-file-info* lpstrFile) ct:null)
	(setf (cref OPENFILENAME *open-file-info* nMaxFile) _MAX_PATH)
	(setf (cref OPENFILENAME *open-file-info* lpstrFileTitle) ct:null)
	(setf (cref OPENFILENAME *open-file-info* nMaxFileTitle) (+ _MAX_FNAME _MAX_EXT))
	(setf (cref OPENFILENAME *open-file-info* lpstrInitialDir) ct:null)
	(setf (cref OPENFILENAME *open-file-info* lpstrTitle) ct:null)
	(setf (cref OPENFILENAME *open-file-info* Flags) 0)
;	(setf (cref OPENFILENAME *open-file-info* nFileOffset) 0)
;	(setf (cref OPENFILENAME *open-file-info* nFileExtension) 0)
	(setf (cref OPENFILENAME *open-file-info* lpstrDefExt) (ct:create-c-string "txt"))
	(setf (cref OPENFILENAME *open-file-info* lCustData) 0)
	(setf (cref OPENFILENAME *open-file-info* lpfnHook) ct:null)
	(setf (cref OPENFILENAME *open-file-info* lpTemplateName) ct:null))

;;
;;	Returns: 
;;		NIL    (if the user cancelled)
;;		The pathname of the selected file (if successful)
;;
(defun get-open-file-name (&optional (filetypes '("All Files (*.*)" "*.*")))
	"GET-OPEN-FILE-NAME &optional filetypes
	 Example: (GET-OPEN-FILE-NAME 	
				'(\"Text Files (*.TXT)\" 	\"*.txt\"
				\"All Files (*.ASC)\" \"*.*\"))"
						
	(let ((file-name (ct:malloc _MAX_PATH))
		  (title-name (ct:malloc (+ _MAX_FNAME _MAX_EXT))))

		(init-open-file (cl::get-application-main-window) filetypes)
	
		(setf (cref OPENFILENAME *open-file-info* lpstrFile) file-name)
		(setf (cref OPENFILENAME *open-file-info* lpstrFileTitle) title-name)
		(setf (cref OPENFILENAME *open-file-info* Flags) 
			(logior OFN_HIDEREADONLY OFN_CREATEPROMPT))

		(setf (cref LPSTR file-name 0) 0)
		(setf (cref LPSTR title-name 0) 0)
		(if (GetOpenFileName *open-file-info*)
			(pathname (ct:c-string-to-lisp-string file-name)))))


