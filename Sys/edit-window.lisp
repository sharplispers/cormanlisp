;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;; Code in this module controls syntax coloring in the Corman Lisp IDE.
;;;; IDE:COLORIZE-WINDOW is automatically called by the IDE .5 seconds after
;;;; the last keystroke or screen buffer update occurred.
;;;;
;;;; It may be modified while the editor is running, but be extremely careful
;;;; if you do--a syntax error could cause the editor to go into a loop
;;;; and crash. Better is to turn off auto colorizing in the preferences dialog
;;;; while making changes, and then calling the colorize function explicitly:
;;;;  (IDE:COLORIZE-CURRENT-WINDOW)
;;;; to try it out.
;;;;
;;;; Changes to settings such as text colors can be set in the init.lisp file.
;;;;

(defpackage "IDE"
  (:export 
        "CURRENT-EDIT-FRAME-WINDOW-HANDLE"
		"CURRENT-EDIT-WINDOW-HANDLE" 
		"SET-SELECTION" 
		"GET-CURRENT-SELECTION-TEXT"
        "GET-SELECTION-TEXT"
        "GET-CURRENT-SELECTION"
        "SET-CURRENT-SELECTION"
        "SHOW-SELECTION"
        "HIDE-SELECTION"
        "GET-RICHEDIT-EVENT-MASK"
        "SET-RICHEDIT-EVENT-MASK"
        "GET-SCROLL-INFO"
        "SET-SCROLL-INFO"
        "DISABLE-EDITOR-REDRAW"
        "ENABLE-EDITOR-REDRAW"
        "GET-FIRST-VISIBLE-LINE"
        "SCROLL-LINES"
        "ON-COLORIZE"
        "COLORIZE-CURRENT-WINDOW"
        "MAKE-TEXT-FORMAT"
        "BLACK"
        "DARK-GREEN"
        "DARK-MAROON"
        "BLUE"
        "COMMENT-FORMAT"
        "KEYWORD-FORMAT"
        "STRING-FORMAT"
        "LISP-SYMBOL-FORMAT"
        "NORMAL-FORMAT"
        "SET-CURRENT-WINDOW-ZOOM"
        "ADD-LISP-DISPLAY-VARIABLE"
        "REMOVE-LISP-DISPLAY-VARIABLE"
        "ON-INIT-MENU-POPUP"
        "ADD-SYMBOL-MENU-TO-MENUBAR"
        "ADD-COMMAND-HISTORY-MENU-TO-MENUBAR"))        

(in-package :ide)

(defparameter *declaration-symbols* nil)  ;; redefine in init.lisp
(export '*declaration-symbols*)

;;;
;;; Some Richedit definitions
;;;
(in-package :win32)

#! (:library "kernel32" :export t)

#define CP_ACP                    0           // default to ANSI code page

/* Flags for the GETTEXTLENGTHEX data structure							*/
#define GTL_DEFAULT		0	/* do the default (return # of chars)		*/
#define GTL_USECRLF		1	/* compute answer using CRLFs for paragraphs*/
#define GTL_PRECISE		2	/* compute a precise answer					*/
#define GTL_CLOSE		4	/* fast computation of a "close" answer		*/
#define GTL_NUMCHARS	8	/* return the number of characters			*/
#define GTL_NUMBYTES	16	/* return the number of _bytes_				*/

/* EM_GETTEXTLENGTHEX info; this struct is passed in the wparam of the msg */
typedef struct _gettextlengthex
{
	DWORD	flags;			/* flags (see GTL_XXX defines)				*/
	UINT	codepage;		/* code page for translation (CP_ACP for default,
							   1200 for Unicode)					    */
} GETTEXTLENGTHEX;
    
/* Flags for the GETEXTEX data structure */
#define GT_DEFAULT		0
#define GT_USECRLF		1
#define GT_SELECTION	2

typedef BOOL* LPBOOL;
    
/* EM_GETTEXTEX info; this struct is passed in the wparam of the message */
typedef struct _gettextex
{
	DWORD	cb;				/* count of bytes in the string				*/
	DWORD	flags;			/* flags (see the GT_XXX defines)			*/
	UINT	codepage;		/* code page for translation (CP_ACP for sys default,
						       1200 for Unicode, -1 for control default)*/
	LPCSTR	lpDefaultChar;	/* replacement for unmappable chars			*/
	LPBOOL	lpUsedDefChar;	/* pointer to flag set when def char used	*/
} GETTEXTEX;

/* Event notification masks */
#define ENM_NONE				0x00000000
#define ENM_CHANGE				0x00000001
#define ENM_UPDATE				0x00000002
#define ENM_SCROLL				0x00000004
#define ENM_SCROLLEVENTS		0x00000008
#define ENM_DRAGDROPDONE		0x00000010
#define ENM_PARAGRAPHEXPANDED	0x00000020          
#define ENM_KEYEVENTS			0x00010000
#define ENM_MOUSEEVENTS			0x00020000
#define ENM_REQUESTRESIZE		0x00040000
#define ENM_SELCHANGE			0x00080000
#define ENM_DROPFILES			0x00100000
#define ENM_PROTECTED			0x00200000
#define ENM_CORRECTTEXT			0x00400000		/* PenWin specific */
!#

#! (:library "user32" :export t :pascal WINAPI)
#define SIF_RANGE           0x0001
#define SIF_PAGE            0x0002
#define SIF_POS             0x0004
#define SIF_DISABLENOSCROLL 0x0008
#define SIF_TRACKPOS        0x0010
#define SIF_ALL             (SIF_RANGE | SIF_PAGE | SIF_POS | SIF_TRACKPOS)

typedef struct tagSCROLLINFO
{
    UINT    cbSize;
    UINT    fMask;
    int     nMin;
    int     nMax;
    UINT    nPage;
    int     nPos;
    int     nTrackPos;
}   SCROLLINFO, *LPSCROLLINFO;
typedef SCROLLINFO CONST *LPCSCROLLINFO;

int     WINAPI SetScrollInfo(HWND, int, LPCSCROLLINFO, BOOL);
BOOL    WINAPI GetScrollInfo(HWND, int, LPSCROLLINFO);
!#

;; DEFINE_GUIDXXX(IID_ITextDocument,0x8CC497C0,0xA1DF,0x11CE,0x80,0x98,
;;                0x00,0xAA,0x00,0x47,0xBE,0x5D);
(defwinconstant IID_ITextDocument 
	(string-guid "{8CC497C0-A1DF-11CE-8098-00AA0047BE5D}" (allocate-guid)))

(defwinconstant IID_ITextRange 
	(string-guid "{8CC497C2-A1DF-11CE-8098-00AA0047BE5D}"  (allocate-guid)))

(defwinconstant IID_ITextFont
	(string-guid "{8CC497C3-A1DF-11CE-8098-00AA0047BE5D}"  (allocate-guid)))

(defwinconstant IID_ITextPara
	(string-guid "{8CC497C4-A1DF-11ce-8098-00AA0047BE5D}"  (allocate-guid)))

#! (:export t)
interface ITextDocument : IDispatch
{
    HRESULT GetName(BSTR* pName);
    HRESULT GetSelection(ITextSelection** ppSel);
    HRESULT GetStoryCount(long* pCount);
    HRESULT GetStoryRanges(ITextStoryRanges** ppStories);
    HRESULT GetSaved(long* pValue);
    HRESULT SetSaved(long Value);
    HRESULT GetDefaultTabStop(float* pValue);
    HRESULT SetDefaultTabStop(float Value);
    HRESULT New();
    HRESULT Open(VARIANT* pVar, long Flags, long CodePage);
    HRESULT Save(VARIANT* pVar, long Flags, long CodePage);
    HRESULT Freeze(long* pCount);
    HRESULT Unfreeze(long* pCount);
    HRESULT BeginEditCollection();
    HRESULT EndEditCollection();
    HRESULT Undo(long Count, long* prop);
    HRESULT Redo(long Count, long* prop);
    HRESULT Range(long cp1, long cp2, ITextRange** ppRange);
    HRESULT RangeFromPoint(long x, long y, ITextRange** ppRange);        
};
!#

#! (:export t)
interface ITextRange : IDispatch
{
    HRESULT GetText(BSTR* pbstr);
    HRESULT SetText(BSTR* pbstr);
    HRESULT GetChar(long* pch);
    HRESULT SetChar(long  ch);
    HRESULT GetDuplicate(ITextRange** ppRange);  
    HRESULT GetFormattedText(ITextRange** ppRange); 
    HRESULT SetFormattedText(ITextRange* pRange);
    HRESULT GetStart(long* pcpFirst); 
    HRESULT SetStart(long cpFirst);
    HRESULT GetEnd(long* pcpLim);    
    HRESULT SetEnd(long cpLim);    
    HRESULT GetFont(ITextFont** pFont);   
    HRESULT SetFont(ITextFont* pFont);   
    HRESULT GetPara(ITextPara** pPara);
    HRESULT SetPara(ITextPara* pPara);
    HRESULT GetStoryLength(long *pcch);
    HRESULT GetStoryType(long *pValue);
    HRESULT Collapse(long bStart);
    HRESULT Expand(long Unit, long *pDelta);
    HRESULT GetIndex(long Unit, long *pIndex);
    HRESULT SetIndex(long Unit, long Index, long Extend);
    HRESULT SetRange(long cpActive, long cpOther);
    HRESULT InRange(ITextRange *pRange, long *pb);
    HRESULT InStory(ITextRange *pRange, long *pb);
    HRESULT IsEqual(ITextRange *pRange, long *pb);
    HRESULT Select();
    HRESULT StartOf(long Unit, long Extend, long *pDelta);
    HRESULT EndOf(long Unit, long Extend, long *pDelta);
    HRESULT Move(long Unit, long Count, long *pDelta);
    HRESULT MoveStart(long Unit, long Count, long *pDelta);
    HRESULT MoveEnd(long Unit, long Count, long *pDelta);
    HRESULT MoveWhile(VARIANT *Cset, long Count, long *pDelta);
    HRESULT MoveStartWhile(VARIANT *Cset, long Count, long *pDelta);
    HRESULT MoveEndWhile(VARIANT *Cset, long Count, long *pDelta);
    HRESULT MoveUntil(VARIANT *Cset, long Count, long *pDelta);
    HRESULT MoveStartUntil(VARIANT *Cset, long Count, long *pDelta);
    HRESULT MoveEndUntil(VARIANT *Cset, long Count, long *pDelta);
    HRESULT FindText(BSTR bstr, long cch, long Flags, long *pLength);
    HRESULT FindTextStart(BSTR bstr, long cch, long Flags, long *pLength);
    HRESULT FindTextEnd(BSTR bstr, long cch, long Flags, long *pLength);
    HRESULT Delete(long Unit, long Count, long *pDelta);
    HRESULT Cut(VARIANT *pVar);
    HRESULT Copy(VARIANT *pVar);
    HRESULT Paste(VARIANT *pVar, long Format);
    HRESULT CanPaste(VARIANT *pVar, long Format, long *pb);
    HRESULT CanEdit(long *pbCanEdit);
    HRESULT ChangeCase(long Type);
    HRESULT GetPoint(long Type, long *px, long *py);
    HRESULT SetPoint(long x, long y, long Type, long Extend);
    HRESULT ScrollIntoView(long Value);
    HRESULT GetEmbeddedObject(IUnknown **ppv);
};
!#

#! (:export t)
interface ITextFont : IDispatch
{
    HRESULT GetDuplicate(ITextFont **ppFont);
    HRESULT SetDuplicate(ITextFont *pFont);        
    HRESULT CanChange(long *pB);        
    HRESULT IsEqual(ITextFont *pFont,long *pB);
    HRESULT Reset(long Value);        
    HRESULT GetStyle(long *pValue);        
    HRESULT SetStyle(long Value);
    HRESULT GetAllCaps(long *pValue);
    HRESULT SetAllCaps(long Value);
    HRESULT GetAnimation(long *pValue);
    HRESULT SetAnimation(long Value);
    HRESULT GetBackColor(long *pValue);
    HRESULT SetBackColor(long Value);
    HRESULT GetBold(long *pValue);        
    HRESULT SetBold(long Value);        
    HRESULT GetEmboss(long *pValue);        
    HRESULT SetEmboss(long Value);        
    HRESULT GetForeColor(long *pValue);
    HRESULT SetForeColor(long Value);
    HRESULT GetHidden(long *pValue);
    HRESULT SetHidden(long Value);
    HRESULT GetEngrave(long *pValue);
    HRESULT SetEngrave(long Value);        
    HRESULT GetItalic(long *pValue);        
    HRESULT SetItalic(long Value);        
    HRESULT GetKerning(float *pValue);        
    HRESULT SetKerning(float Value);        
    HRESULT GetLanguageID(long *pValue);        
    HRESULT SetLanguageID(long Value);        
    HRESULT GetName(BSTR *pbstr);        
    HRESULT SetName(BSTR bstr);        
    HRESULT GetOutline(long *pValue);        
    HRESULT SetOutline(long Value);        
    HRESULT GetPosition(float *pValue);       
    HRESULT SetPosition(float Value);        
    HRESULT GetProtected(long *pValue);
    HRESULT SetProtected(long Value);
    HRESULT GetShadow(long *pValue);        
    HRESULT SetShadow(long Value);        
    HRESULT GetSize(float *pValue);        
    HRESULT SetSize(float Value);        
    HRESULT GetSmallCaps(long *pValue);        
    HRESULT SetSmallCaps(long Value);        
    HRESULT GetSpacing(float *pValue);
    HRESULT SetSpacing(float Value);
    HRESULT GetStrikeThrough(long *pValue);        
    HRESULT SetStrikeThrough(long Value);
    HRESULT GetSubscript(long *pValue);        
    HRESULT SetSubscript(long Value);
    HRESULT GetSuperscript(long *pValue);        
    HRESULT SetSuperscript(long Value);        
    HRESULT GetUnderline(long *pValue);        
    HRESULT SetUnderline(long Value);        
    HRESULT GetWeight(long *pValue);        
    HRESULT SetWeight(long Value);        
};
!#

#! (:export t)
interface ITextPara : IDispatch
{
    HRESULT GetDuplicate(ITextPara** ppPara);
    HRESULT SetDuplicate(ITextPara* pPara);
    HRESULT CanChange(long* pB);
    HRESULT IsEqual(ITextPara* pPara, long* pB);
    HRESULT Reset(long Value);
    HRESULT GetStyle(long* pValue);
    HRESULT SetStyle(long Value);
    HRESULT GetAlignment(long* pValue);
    HRESULT SetAlignment(long Value);
    HRESULT GetHyphenation(long* pValue);
    HRESULT SetHyphenation(long Value);
    HRESULT GetFirstLineIndent(float* pValue);
    HRESULT GetKeepTogether(long* pValue);
    HRESULT SetKeepTogether(long Value);
    HRESULT GetKeepWithNext(long* pValue);
    HRESULT SetKeepWithNext(long Value);
    HRESULT GetLeftIndent(float* pValue);
    HRESULT GetLineSpacing(float* pValue);
    HRESULT GetLineSpacingRule(long* pValue);
    HRESULT GetListAlignment(float* pValue);
    HRESULT SetListAlignment(long Value);
    HRESULT GetListLevelIndex(long* pValue);
    HRESULT SetListLevelIndex(long Value);
    HRESULT GetListStart(long* pValue);
    HRESULT SetListStart(long Value);
    HRESULT GetListTab(float* pValue);
    HRESULT SetListTab(float Value);
    HRESULT GetListType(long* pValue);
    HRESULT SetListType(long Value);
    HRESULT GetNoLineNumber(long* pValue);
    HRESULT SetNoLineNumber(long Value);
    HRESULT GetPageBreakBefore(long* pValue);
    HRESULT SetPageBreakBefore(long Value);
    HRESULT GetRightIndent(float* pValue);
    HRESULT SetRightIndent(float Value);
    HRESULT SetIndents(float StartIndent, float LeftIndent, float RightIndent);
    HRESULT SetLineSpacing(long LineSpacingRule, float LineSpacing);
    HRESULT GetSpaceAfter(float* pValue);
    HRESULT SetSpaceAfter(float Value);
    HRESULT GetSpaceBefore(float* pValue);
    HRESULT SetSpaceBefore(float Value);
    HRESULT GetWidowControl(long* pValue);
    HRESULT SetWidowControl(long Value);
    HRESULT GetTabCount(long* pCount);
    HRESULT AddTab(float tbPos, long tbAlign, long tbLeader);
    HRESULT ClearAllTabs();
    HRESULT DeleteTab(float tbPos);
    HRESULT GetTab(long iTab, float* ptbPos, long* ptbAlign, long* ptbLeader);        
};
!#

(defwinconstant tomFalse        0)
(defwinconstant tomTrue         -1)
(defwinconstant tomUndefined    -9999999)
(defwinconstant tomToggle       -9999998)
(defwinconstant tomAutoColor    -9999997)
(defwinconstant tomDefault      -9999996)
(defwinconstant tomSuspend      -9999995)
(defwinconstant tomResume       -9999994)

(defwinconstant tomLineSpaceSingle	 0)
(defwinconstant tomLineSpace1pt5	 1)
(defwinconstant tomLineSpaceDouble	 2)
(defwinconstant tomLineSpaceAtLeast	 3)
(defwinconstant tomLineSpaceExactly	 4)
(defwinconstant tomLineSpaceMultiple 5)
          
(in-package :ide)

(defvar *local-CHARRANGE* (ct:malloc (ct:sizeof 'win32:CHARRANGE)))
(defvar *local-textbuf*   (ct:malloc #x4000))
(defvar *local-SCROLLINFO* (ct:malloc (ct:sizeof 'win:SCROLLINFO)))
(defvar *local-interface-ptr* (ct:malloc (ct:sizeof '(win:interface *))))
(defvar *local-CHARFORMAT* (ct:malloc (ct:sizeof 'win:CHARFORMAT)))
(defvar *local-long-ptr*  (ct:malloc (ct:sizeof 'win:long)))
(defvar *local-BSTR-ptr*  (ct:malloc (ct:sizeof 'win:BSTR)))
(defvar *local-GETTEXTLENGTHEX* (ct:malloc (ct:sizeof 'win:GETTEXTLENGTHEX)))
(defvar *local-GETTEXTEX* (ct:malloc (ct:sizeof 'win:GETTEXTEX)))

(defparameter *text-window-text-buffer* nil)

(defun get-text-buffer (size)
    (when (null *text-window-text-buffer*)
        (setf *text-window-text-buffer* (ct:malloc size))
        (return-from get-text-buffer *text-window-text-buffer*))
    (when (> size (ct:foreign-heap-length *text-window-text-buffer*))
        (ct:free *text-window-text-buffer*)
        (setf *text-window-text-buffer* (ct:malloc size)))
    *text-window-text-buffer*)

(defconstant black (win:RGB 0 0 0))
(defconstant dark-green (win:RGB 0 128 0))
(defconstant blue (win:RGB 0 0 255))
(defconstant dark-maroon (win:RGB #x40 #x00 #x00))

(defconstant cl-package (find-package ':common-lisp))

(defstruct text-format 
    (bold nil)
    (italic nil)
    (color black))

(defparameter comment-format (make-text-format :color dark-green))
(defparameter keyword-format (make-text-format :color blue))
(defparameter lisp-symbol-format (make-text-format :bold t))
(defparameter string-format (make-text-format :color dark-maroon))

;; the color of the user's preference will be used if it has been set, and
;; the color of normal-format ignored
(defparameter normal-format 
    (make-text-format :bold nil :italic nil :color black))

     
(defun get-preferences-text-color ()
    (let ((color 0)
          (registry-value
             (win32:registry-lookup "HKEY_CURRENT_USER"
                                    "Software"
                                    "Corman Technologies"
                                    "Corman Lisp"
                                    (lisp-implementation-version)
                                    "Editor Preferences"
                                    "TextColor"
                                    "")))
        (if registry-value
            (setq color (parse-integer registry-value :radix 16)))
        color))
        
;;;
;;; Corman Lisp CURRENT-EDIT-FRAME-WINDOW-HANDLE function.
;;; Returns the frame window handle of the editor window currently active.
;;;
(defun current-edit-frame-window-handle ()
	(win32:FindWindowEx 
		(win32:FindWindowEx (cl::get-application-main-window) null null null)
		null 
		null 
		null))

;;;
;;; Corman Lisp CURRENT-EDIT-WINDOW-HANDLE function.
;;; Returns the window handle of the editor window currently active.
;;;
(defun current-edit-window-handle ()
	(win32:FindWindowEx 
		(current-edit-frame-window-handle)
		null 
		null 
		null))

;;;
;;; Corman Lisp SET-SELECTION function.
;;; Activates (opening it if necessary) the requested editor window and
;;; selects the requested lines.
;;;
(defun set-selection (filename line1 col1 line2 col2)
	(let* ((richedit-window (ed filename))
		   (range *local-CHARRANGE*)
		   line1-pos
		   line2-pos
		   pos1
		   pos2)
		(if (zerop (cl::foreign-ptr-to-int richedit-window))
			(error "Could not access edit window ~A" filename))

		;; loop until something other than -1 is returned, while we wait for
		;; the window to respond
		(do ()
			((/= (win32:SendMessage richedit-window win32::EM_GETLINECOUNT 0 0) 1)))

		(setq line1-pos (win32:SendMessage richedit-window win32::EM_LINEINDEX line1 0))
		(setq line2-pos (win32:SendMessage richedit-window win32::EM_LINEINDEX line2 0))
		(setf pos1 (+ line1-pos col1))
		(setf pos2 (+ line2-pos col2))
		(if (= line1 line2)
			(let ((next-line-pos (win32:SendMessage richedit-window win32::EM_LINEINDEX (+ line1 1) 0)))
				(setq pos2 (min pos2 (- next-line-pos 1)))))
		(setf (ct:cref win32:CHARRANGE range win32::cpMin) pos1)
		(setf (ct:cref win32:CHARRANGE range win32::cpMax) pos2)
		(win32:SendMessage richedit-window 
			win32::EM_EXSETSEL 0 (cl::foreign-ptr-to-int range))))

;;;
;;; Corman Lisp SET-CURRENT-SELECTION function.
;;; Sets the beginning and ending index of the current selection.
;;; 
(defun set-current-selection (hwnd min max)
	(let* ((range *local-CHARRANGE*))
		(if (ct:cpointer-null hwnd)
			(return-from set-current-selection nil))
   		(setf (ct:cref win32:CHARRANGE range win32::cpMin) min)
		(setf (ct:cref win32:CHARRANGE range win32::cpMax) max)
		(win32:SendMessage hwnd 
			win32::EM_EXSETSEL 0 (cl::foreign-ptr-to-int range))))

;;;
;;; Corman Lisp GET-CURRENT-SELECTION function.
;;; Returns two values, the beginning and ending index of the current selection.
;;; 
(defun get-current-selection (hwnd)
	(let ((range *local-CHARRANGE*))
		(if (ct:cpointer-null hwnd)
			(return-from get-current-selection nil))
		(win32:SendMessage hwnd 
			win32:EM_EXGETSEL 0 (ct:foreign-ptr-to-int range))
        (values 
            (ct:cref win32:CHARRANGE range win::cpMin)
            (ct:cref win32:CHARRANGE range win::cpMax))))

;;;
;;; Corman Lisp SHOW-SELECTION function.
;;; Makes the current selection of the active edit window visible.
;;;
(defun show-selection (hwnd)
 	(win32:SendMessage hwnd win:EM_HIDESELECTION 0 0))

;;;
;;; Corman Lisp HIDE-SELECTION function.
;;; Makes the current selection of the active edit window hidden.
;;;
(defun hide-selection (hwnd)
	(win32:SendMessage hwnd win:EM_HIDESELECTION 1 0))

;;;
;;; Corman Lisp GET-SELECTION-TEXT function.
;;; Returns the selected text as a lisp string. Returns NIL if no text is selected.
;;;
(defun get-selection-text (hwnd)
    (multiple-value-bind (startpos endpos)
        (get-current-selection hwnd)
        (let ((selection-length (- endpos startpos)))
			(if (<= selection-length 0)
				(return-from get-selection-text nil))
			(let ((textbuf (get-text-buffer (1+ selection-length))))
				(win:SendMessage hwnd 
					win:EM_GETSELTEXT 0 (cl::foreign-ptr-to-int textbuf))
				(ct:c-string-to-lisp-string textbuf)))))         

;;;
;;; Corman Lisp GET-CURRENT-SELECTION-TEXT function.
;;; Returns the selected text as a lisp string. Returns NIL if no text is selected.
;;;
(defun get-current-selection-text ()
	(let ((richedit-window (current-edit-window-handle)))
        (get-selection-text richedit-window)))

;;;
;;; Corman Lisp GET-RICHEDIT-EVENT-MASK function.
;;; Returns the event mask of the active edit window as an integer.
;;;
(defun get-richedit-event-mask (hwnd)
    (win32:SendMessage hwnd win:EM_GETEVENTMASK 0 0)) 

;;;
;;; Corman Lisp SET-RICHEDIT-EVENT-MASK function.
;;; Sets the event mask of the active edit window, given the mask as an integer.
;;;
(defun set-richedit-event-mask (hwnd mask)
    (win:SendMessage hwnd win:EM_SETEVENTMASK 0 mask)) 

;;;
;;; Corman Lisp GET-SCROLL-INFO function.
;;; Returns scrollbar info as a foreign heap object of type win:SCROLLINFO.
;;; 
(defun get-scroll-info (hwnd)
	(let ((scrollinfo *local-SCROLLINFO*))
		(if (ct:cpointer-null hwnd)
			(return-from get-scroll-info nil))
        (setf (ct:cref win:SCROLLINFO scrollinfo win::cbSize) (ct:sizeof 'win:SCROLLINFO))
        (setf (ct:cref win:SCROLLINFO scrollinfo win::fMask) 
            (logior win:SIF_PAGE win:SIF_POS win:SIF_RANGE win:SIF_TRACKPOS))
		(win:GetScrollInfo hwnd win:SB_VERT scrollinfo)
        scrollinfo))

;;;
;;; Corman Lisp SET-SCROLL-INFO function.
;;; Sets scrollbar info using a foreign heap object of type win:SCROLLINFO.
;;; 
(defun set-scroll-info (hwnd scrollinfo)
	(if (ct:cpointer-null hwnd)
		(return-from set-scroll-info nil))
    (setf (ct:cref win:SCROLLINFO scrollinfo win::cbSize) (ct:sizeof 'win:SCROLLINFO))
    (setf (ct:cref win:SCROLLINFO scrollinfo win::fMask) 
        (logior win:SIF_PAGE win:SIF_POS win:SIF_RANGE win:SIF_TRACKPOS))
	(win:SetScrollInfo hwnd win::SB_VERT scrollinfo win:FALSE))

;;;
;;; Corman Lisp DISABLE-EDITOR-REDRAW function.
;;; Disables redraw on the current active window.
;;;
(defun disable-editor-redraw (hwnd)
    (win32:SendMessage hwnd win:WM_SETREDRAW 0 0))  

;;;
;;; Corman Lisp ENABLE-EDITOR-REDRAW function.
;;; Enables redraw on the current active window.
;;;
(defun enable-editor-redraw (hwnd)
    (win32:SendMessage hwnd win:WM_SETREDRAW 1 0))  

;;;
;;; Corman Lisp GET-FIRST-VISIBLE-LINE function.
;;; Enables redraw on the current active window.
;;;
(defun get-first-visible-line (hwnd)
    (win:SendMessage hwnd win:EM_GETFIRSTVISIBLELINE 0 0))  

;;;
;;; Corman Lisp SCROLL-LINES function.
;;; Scrolls the edit window the requested number of lines.
;;;
(defun scroll-lines (hwnd lines)
    (win:SendMessage hwnd win:EM_LINESCROLL 0 lines))  

(defun get-richedit-ole-interface (hwnd)
    (let ((interfaceptr *local-interface-ptr*))
        (win:SendMessage hwnd win:EM_GETOLEINTERFACE 0 (ct:foreign-ptr-to-int interfaceptr))
        (ct:cref ((win:interface *)*) interfaceptr 0)))

(defun get-ITextDocument-interface (richedit-interface)
    (let* ((interfaceptr *local-interface-ptr*)
           (result (win:IUNKNOWN-QUERYINTERFACE richedit-interface win::IID_ITextDocument interfaceptr)))
        (if (= result win:S_OK) (ct:cref ((win:interface *)*) interfaceptr 0) nil)))

(defun suspend-undo (itextdocument)
    (if itextdocument (win:ITEXTDOCUMENT-UNDO itextdocument win:tomSuspend ct:null)))

(defun resume-undo (itextdocument)
    (if itextdocument (win:ITEXTDOCUMENT-UNDO itextdocument win:tomResume ct:null)))

;;; 
;;; Make sure that SAVE-IMAGE does not waste memory storing this temporary buffer,
;;; which during the course of execution will grow to accomodate the largest size
;;; text file that is opened for editing (if using the IDE).
;;;
(cl::register-save-image-cleanup-func 
    #'(lambda ()
        (unless (null *text-window-text-buffer*)
            (ct:free *text-window-text-buffer*)
            (setf *text-window-text-buffer* nil))))
                
(defun get-editor-buffer-length (hwnd)
    (let ((textlength *local-GETTEXTLENGTHEX*))
        (ct:with-c-struct (x textlength win:GETTEXTLENGTHEX)
            (setf win::flags win:GTL_DEFAULT
                  win::codepage win::CP_ACP))
        (win:SendMessage hwnd win:EM_GETTEXTLENGTHEX 
            (ct:foreign-ptr-to-int textlength) 0)))

(defun get-editor-buffer-contents (hwnd)
    (let* ((length (get-editor-buffer-length hwnd))
           (buf (get-text-buffer (+ length 1)))
           (gettextex *local-GETTEXTEX*))
        (ct:with-c-struct (x gettextex win:GETTEXTEX)
            (setf win::cb (+ length 1)
                  win::flags win:GT_DEFAULT
                  win::codepage win::CP_ACP
                  win::lpDefaultChar ct:null
                  win::lpUsedDefChar ct:null))
        (win:SendMessage hwnd win:EM_GETTEXTEX 
            (ct:foreign-ptr-to-int gettextex) 
            (ct:foreign-ptr-to-int buf))
        (ct:c-string-to-lisp-string buf)))

(defun colorize-read-comment-token (stream)
    (let ((chars nil)
          (start (cl::stream-position stream)))
        (do ((ch (cl::__read-char stream nil ':eof)(cl::__read-char stream nil ':eof)))
            ((or (not (characterp ch))(char= ch #\Newline)(char= ch #\Return))
             (list ':comment (coerce (nreverse chars) 'string)
                    start (cl::stream-position stream)))
            (push ch chars))))

(defun colorize-read-symbol-token (stream)
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

(defun colorize-read-pound-token (stream)
    (let* ((chars nil)
          (start (cl::stream-position stream))
          (ch (cl::__read-char stream nil ':eof)))
        (setf ch (cl::__read-char stream nil ':eof))
        (if (not (characterp ch)) (return-from colorize-read-pound-token nil))
        (if (char= ch #\|)
            (do ((ch (cl::__read-char stream nil ':eof)(cl::__read-char stream nil ':eof)))
                ((not (characterp ch)) nil)
                (if (char= ch #\|)
                    (progn
                        (setf ch (cl::__read-char stream  nil ':eof))
                        (if (not (characterp ch)) (return nil))
                        (if (char= ch #\#)
                            (progn
                                (push #\| chars)
                                (push ch chars)
                                (return (list ':comment (coerce (nreverse chars) 'string)
                                            start (cl::stream-position stream))))
                            (unread-char ch stream))))
                (push ch chars))
            (progn 
                (cl::__read-char stream nil ':eof)      ;; skip dispatch character and exit
                nil))))

(defun colorize-read-string-token (stream)
    (let ((chars nil)
          (start (cl::stream-position stream)))
        (push (read-char stream) chars)
        (do ((ch (cl::__read-char stream nil ':eof)(cl::__read-char stream nil ':eof)))
            ((or (not (characterp ch))(char= ch #\"))
             (list ':string (coerce (nreverse chars) 'string)
                    start (cl::stream-position stream)))
            (push ch chars))))
    
;;
;; COLORIZE-READ
;; simpler than READ, just returns strings of 1) symbols 2) comments 3) literal strings
;; and skips everything else. Follows basic default lisp reader syntax.
;; Returns ':eof when done. Returns NIL for junk that was skipped.
;;
(defun colorize-read-token (stream)
    (let ((ch (cl::read-char-skip-white-space stream)))
        (cond ((not (characterp ch)) ':eof)
              ((char= ch #\;)(unread-char ch stream)(colorize-read-comment-token stream))
              ((char= ch #\#)(unread-char ch stream)(colorize-read-pound-token stream))
              ((char= ch #\")(unread-char ch stream)(colorize-read-string-token stream))
              ((cl::constituent-char ch)(unread-char ch stream)(colorize-read-symbol-token stream)))))

(defun common-lisp-symbol-name-p (string) 
    (and (stringp string) 
        (eq (nth-value 1 
                (find-symbol (string-upcase string) cl-package)) 
            ':external)))

(defun keyword-name-p (string)
    (and (stringp string)
        (char= (char string 0) #\:)))

(defun format-text (ITextRange start end 
        &key (bold nil supplied-bold)
             (italic nil supplied-italic)
             (color black supplied-color))
    (win:ITextRange-SetStart ITextRange start)
    (win:ITextRange-SetEnd ITextRange end)
    (win:with-com-interface (ITextFont *local-interface-ptr*)
        (win:ITextRange-GetFont ITextRange ITextFont)
        (if supplied-bold
            (win:ITextFont-SetBold ITextFont (if bold win:tomTrue win:tomFalse)))
        (if supplied-italic
            (win:ITextFont-SetItalic ITextFont (if italic win:tomTrue win:tomFalse)))
        (if supplied-color
            (win:ITextFont-SetForeColor ITextFont color))))
    
(defun format-text-bold (ITextRange start end &optional (bold t))
    (win:ITextRange-SetStart ITextRange start)
    (win:ITextRange-SetEnd ITextRange end)
    (win:with-com-interface (ITextFont *local-interface-ptr*)
        (win:ITextRange-GetFont ITextRange ITextFont)
        (win:ITextFont-SetBold ITextFont (if bold win:tomTrue win:tomFalse))))

(defun format-text-color (ITextRange start end color)
    (win:ITextRange-SetStart ITextRange start)
    (win:ITextRange-SetEnd ITextRange end)
    (win:with-com-interface (ITextFont *local-interface-ptr*)
        (win:ITextRange-GetFont ITextRange ITextFont)
        (win:ITextFont-SetForeColor ITextFont color)))

(defun format-commenting (ITextRange start end)
    (format-text ITextRange start end 
        :bold (text-format-bold comment-format)
        :italic (text-format-italic comment-format)
        :color (text-format-color comment-format)))

(defun format-string (ITextRange start end)
    (format-text ITextRange start end 
        :bold (text-format-bold string-format)
        :italic (text-format-italic string-format)
        :color (text-format-color string-format)))

(defun format-common-lisp-symbol (ITextRange start end)
    (format-text ITextRange start end 
        :bold (text-format-bold lisp-symbol-format)
        :italic (text-format-italic lisp-symbol-format)
        :color (text-format-color lisp-symbol-format)))

(defun format-declared-symbol (ITextRange start end)
    (format-text ITextRange start end 
        :bold (text-format-bold lisp-symbol-format)
        :italic (text-format-italic lisp-symbol-format)
        :color (text-format-color lisp-symbol-format)))

(defun format-keyword (ITextRange start end)
    (format-text ITextRange start end 
        :bold (text-format-bold keyword-format)
        :italic (text-format-italic keyword-format)
        :color (text-format-color keyword-format)))

(defun format-normal (ITextRange start end)
    (format-text ITextRange start end 
        :bold (text-format-bold normal-format)
        :italic (text-format-italic normal-format)
        :color (or (get-preferences-text-color)
            (text-format-color normal-format))))
    
(defun clear-formatting (ITextRange)
    (format-normal ITextRange 0 10000000)) ;; some large value for end

(defparameter *line-spacing-value* (ct:malloc (ct:sizeof '(:single-float *))))
(defparameter *font-height-value* (ct:malloc (ct:sizeof '(:single-float *))))
(defparameter *font-height* 0)
(defparameter *editor-buffer-length* 0)
(defparameter *freeze-count* (ct:malloc (ct:sizeof '(:long *))))

(defmacro show-com-result (expr)
    (let ((sym (gensym)))
        `(let ((,sym ,expr))
            (format t "Expression ~A returned ~A~%" ',expr ,sym)
            (force-output t))))
;;;
;;; Select the whole buffer, retrieve the current line spacing,
;;; then set the line spacing rule to exact size.
;;;
(defun set-line-spacing (ITextRange)
    (win:ITextRange-SetRange ITextRange 0 *editor-buffer-length*)
    
    ;; look up the font height--add 2 to that to derive the line height
    (win:with-com-interface (ITextFont *local-interface-ptr*)
        (win:ITextRange-GetFont ITextRange ITextFont)
        (win:ItextFont-GetSize ITextFont *font-height-value*)
        (setq *font-height* (ct:cref (:single-float *) *font-height-value* 0)))
    
    (win:with-com-interface (ITextPara *local-interface-ptr*)
        (win:ITextRange-GetPara ITextRange ITextPara)
        (win:ITextPara-SetLineSpacing ITextPara win:tomLineSpaceExactly (+ 3 *font-height*))))
                      
(defun colorize-buffer (ITextDocument stream offset)
    (win:with-com-interface (ITextRange *local-interface-ptr*)
        (win:ITextDocument-Range ITextDocument 0 0 ITextRange)
        (clear-formatting ITextRange)
        (set-line-spacing ITextRange)
        (do* ((form (colorize-read-token stream)(colorize-read-token stream))
              (last-token-was-declaration nil)) 
            ((eq ':eof form))
            (if form
                (let ((type (first form))
                      (string (second form))
                      (start (+ (third form) offset))
                      (end (+ (fourth form) offset)))
                    (cond
                        ((eq type ':comment) 
                         (format-commenting ITextRange start end)
                         (setq last-token-was-declaration nil))
                        ((eq type ':string) 
                         (format-string ITextRange start end)
                         (setq last-token-was-declaration nil))
                        ((eq type ':symbol)
                         (cond (last-token-was-declaration
                                (format-declared-symbol ITextRange start end)
                                (setq last-token-was-declaration nil))
                               ((common-lisp-symbol-name-p string)
                                (format-common-lisp-symbol ITextRange start end)
                                (setq last-token-was-declaration 
                                    (member string ide:*declaration-symbols* :test 'string-equal)))
                               ((keyword-name-p string)
                                (format-keyword ITextRange start end)
                                (setq last-token-was-declaration nil))))))))))

#| threaded method--doesn't seem to be much faster

(defun colorize-window (hwnd)
   	(let* ((text (get-editor-buffer-contents hwnd))
           (in (make-string-input-stream text)))
        (let ((IRichEdit (get-richedit-ole-interface hwnd)))
            (th:create-thread
                (lambda ()
                    (ignore-errors
                        (let ((*local-interface-ptr* (ct:malloc (ct:sizeof '(win:interface *))))
                              (long-ptr (ct:malloc (ct:sizeof 'win:long))))
                            (unwind-protect
                                (win:with-com-interface (ITextDocument *local-interface-ptr*)
                                    (win:IUNKNOWN-QUERYINTERFACE IRichEdit win::IID_ITextDocument ITextDocument)
                                    (suspend-undo ITextDocument) 
                                    (ITextDocument-Freeze ITextDocument long-ptr) 
                                    (colorize-buffer ITextDocument in)
                                    (ITextDocument-Unfreeze ITextDocument long-ptr) 
                                    (resume-undo ITextDocument) 
                                    nil)
                                (win:IUnknown-Release IRichEdit)))))
                nil))))
|#


#|
;;; this always seems to return NIL, finding that the document never 
;;; has a name, so is not too useful
(defun get-document-name (ITextDocument)
    (win:ITextDocument-GetName ITextDocument *local-BSTR-ptr*)
    (let ((bstr nil))
        (unwind-protect
            (progn
                (setf bstr (ct:cref (win:BSTR *) *local-BSTR-ptr* 0))
                (unless (ct:cpointer-null bstr) (win:bstr-to-lisp-string bstr)))
             (if nil (win:SysFreeString bstr)))))
|#
                   
(defun colorize-window (hwnd start end)
    (setq *editor-buffer-length* (get-editor-buffer-length hwnd))
    (let* ((text (get-editor-buffer-contents hwnd))
           (in (make-string-input-stream 
                    (subseq text start 
                        (min end *editor-buffer-length*)))))
        (win:with-com-interface (IRichEdit *local-interface-ptr*)
            (win:SendMessage hwnd win:EM_GETOLEINTERFACE 0 (ct:foreign-ptr-to-int IRichEdit))
            (win:with-com-interface (ITextDocument *local-interface-ptr*)
                (win:IUNKNOWN-QUERYINTERFACE IRichEdit win::IID_ITextDocument ITextDocument)
                (suspend-undo ITextDocument)
                (win:ITextDocument-Freeze ITextDocument *freeze-count*)
                (unwind-protect
                    (let ((save-scroll-info (get-scroll-info hwnd)))
                        (multiple-value-bind (save-start save-end)
                            (get-current-selection hwnd)
                            (colorize-buffer ITextDocument in start)
                            (set-current-selection hwnd save-start save-end))
                        (set-scroll-info hwnd save-scroll-info))
                    (win:ITextDocument-Unfreeze ITextDocument *freeze-count*))
                (resume-undo ITextDocument) 
                nil))))

(defun colorize-current-window ()
    (let ((hwnd (current-edit-window-handle)))
        (colorize-window hwnd 0 (get-editor-buffer-length hwnd))))

;;;
;;; This is what gets exported and called via DirectCall.
;;;
(ct:defun-direct-c-callback on-colorize ((hwnd win:HWND) (start :long) (end :long))
	(ignore-errors
		(colorize-window hwnd start end)))

;; (ide:set-current-window-zoom 1 1)  ;; to restore normal zoom
(defun set-current-window-zoom (num denom)
    (win32:SendMessage (current-edit-window-handle) win:EM_SETZOOM num denom))
   
(defparameter lisp-display-variables '())
(defun add-lisp-display-variable (name expr) 
    (setf lisp-display-variables
        (append lisp-display-variables (list (list name expr)))))

(defun remove-lisp-display-variable (name) 
    (setf lisp-display-variables 
        (remove name lisp-display-variables :key 'car :test 'equalp)))
  
(defun format-symbol (sym stream)
    (if (not (boundp sym))
        (format stream "<Unbound>")
        (format stream "~A" (symbol-value sym))))
    
(defun format-variable-or-expression (x stream)
    (let ((name (car x))
          (expr (cadr x)))
        (if (symbolp expr)
            (progn
                (format stream "~A: " name)
                (format-symbol expr stream)
                (format stream "~%"))
            (format stream "~A: ~A~%" name (ignore-errors (eval expr))))))
        
(defun lisp-variable-display-text ()
    (with-output-to-string (s)
        (dolist (x lisp-display-variables)
            (format-variable-or-expression x s))))

(ct:defun-direct-c-callback display-lisp-vars ((buf (:char *))(bufLength :long))
	(setf (ct:cref (:char *) buf 0) 0)     ;; initialize to empty string
    (let ((var-string (lisp-variable-display-text)))
        (if (> (length var-string) (- bufLength 1))
            (setf var-string (subseq var-string 0 (- bufLength 1))))
        (ct:lisp-string-to-c-string var-string buf)
        (length var-string)))

(add-lisp-display-variable "*package*" '(package-name *package*))

;;
;; Set up windows message handlers for IDE
;;
(defun on-init-menu-impl (handle) (declare (ignore handle)))  ;; redefined later

(ct:defun-direct-c-callback on-init-menu ((menu-handle win:HMENU))
    (on-init-menu-impl menu-handle))

(defun on-init-menu-popup-impl (menu-handle uint bool)
    (declare (ignore uint bool))
    (win::execute-menu-popup-init menu-handle))

(ct:defun-direct-c-callback on-init-menu-popup ((menu-handle win:HMENU)
                                                (uint win:UINT)
                                                (bool win:UINT))
    (on-init-menu-popup-impl menu-handle uint bool))

(defun on-uninit-menu-popup-impl (menu-handle)
    (declare (ignore uint bool))
    (win::execute-menu-popup-uninit menu-handle))

(ct:defun-direct-c-callback on-uninit-menu-popup ((menu-handle win:HMENU))
    (on-uninit-menu-popup-impl menu-handle))

(defun on-menu-select-impl (menu-handle item-id flags)
    (declare (ignore menu-handle item-id flags))
    (let ((message (win::user-command-message item-id)))
        (if message
            (cl::editor-set-message message))))

(ct:defun-direct-c-callback on-menu-select ((menu-handle win:HMENU)(item-id win:UINT)(flags win:UINT))
    (on-menu-select-impl menu-handle item-id flags))

(defparameter *symbols-menu-handle* nil)

(defun escape-ampersand (str)
    (if (find #\& str)
        (let ((new-chars '()))
            (dotimes (i (length str))
                (let ((ch (elt str i)))
                    (if (char= ch #\&)
                        (push ch new-chars))    ;; push an extra ampersand
                    (push ch new-chars)))
            (concatenate 'string (nreverse new-chars)))
        str))
 
(defun populate-symbol-submenu (hmenu package)
    (let ((count (win:GetMenuItemCount hmenu)))
        (loop for i from (- count 1) downto 0 do
            (win:remove-menu-item hmenu i))
        (let ((i 0)
              #|(columns 1)|#)
            (win32::insert-dynamic-menu-item
                            hmenu
                            i
                            (concatenate 'string "Switch to package: " (package-name package))
                            (lambda (id)
                                (declare (ignore id))
                                (setf *package* package)
                                (values)))
            (incf i)         
            (let ((syms '()))
                (do-symbols (sym package)
                    (multiple-value-bind (s status)
                        (find-symbol sym package)
                        (declare (ignore s))
                        (when (eq status :external)
                            (push sym syms))))
                (when (> (length syms) 0)
                    (win32::insert-dynamic-menu-item
                                hmenu
                                i
                                "Separator" 
                                (lambda (id)
                                    (declare (ignore id))
                                    (values))
                                :separator t)
                    (incf i))
                (dolist (sym (sort syms 'string-lessp :key 'symbol-name))
                    (let ((s sym)
                          (insert-break nil))
                   ;     (when (and (> i 0) (zerop (mod i 20)) (< columns 3))
                   ;         (incf columns)
                   ;         (setf insert-break t))
                        (win32::insert-dynamic-menu-item
                            hmenu
                            i
                            (escape-ampersand (symbol-name sym)) 
                            (lambda (id)
                                (declare (ignore id))
                                (describe s *terminal-io*)
                                (values))
                            :message (lambda (id)
                                (declare (ignore id))
                                (win::lookup-lambda-list-impl s))
                            :break insert-break)
                        (incf i)))))))

(defun populate-symbol-menu (hmenu)
    (win:remove-all-menu-items hmenu)
    (let ((index 0)
          (packages 
                (sort (list-all-packages)
                    (lambda (x y) 
                        (string< (package-name x)(package-name y))))))
        (dolist (package packages)
            (let* ((p package))
				(win:create-dynamic-menu-item 
					(list :menu 
                        (package-name package)
                        (lambda (hmenu)
                            (populate-symbol-submenu hmenu p)))
                    hmenu
                    index))
                (incf index))))

(defun add-symbol-menu-to-menubar ()
    (win:create-menu-item 
       (list :menu "&Symbols" 'populate-symbol-menu)
       nil
       5))

(defvar *cmd-display-max-length* 65)

(defun truncate-cmd-display-text (cmd-text len)
    (if (and (> len 0) (< len (length cmd-text)))
        (concatenate 'string (subseq cmd-text 0 len) "...")
        cmd-text))

(defun populate-command-history-menu (hmenu)
    (let ((i 0))
        (dolist (cmd cl::*command-history*)
            (let ((index i))
                (win::insert-dynamic-menu-item
                    hmenu
                    index
                    (escape-ampersand (truncate-cmd-display-text (format nil "~S" cmd) *cmd-display-max-length*))
                    (lambda (id)
                        (declare (ignore id))
                        (cl::recall-command index)
                        (values))))
            (incf i))))

(defun add-command-history-menu-to-menubar ()
    (win:create-dynamic-menu-item 
       (list :menu "H&istory" 'populate-command-history-menu)
       nil
       6))
