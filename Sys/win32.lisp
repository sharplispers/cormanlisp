;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		win32.lisp
;;;;	Contents:	Win32 API definitions.
;;;;	Author:		Roger Corman
;;;;	Created:	10/9/97
;;;;				2/21/01  Added WIN:SHELL and WIN:GET-COMMAND-LINE
;;;;				7/20/01  Added declaration of SetWindowText.
;;;;				7/20/01  Added structs for MDI support.
;;;;				7/24/01  Added time zone struct, function.
;;;;                04/03/03 Incorporated JP Massar's improvements to DEFWIN* macros.
;;;;                05/10/06 Changed type of ATOM to :unsigned-short (as it should be)
;;;;

(in-package :win32)
(export '(defwintype defwinstruct defwinconstant defwinapi))
(export '(shell-execute))

(defun win-import-export (symbol)
  (let ((winpackage (find-package :win32)))
    (unless (find-symbol (symbol-name symbol) winpackage)
      (import symbol winpackage))
    (export symbol winpackage)
    ))


(defmacro defwintype (name definition) 
  `(progn (win-import-export ',name) (ct:defctype ,name ,definition)))

(defmacro defwinstruct (name fields) 
  `(progn (win-import-export ',name) (ct:defcstruct ,name ,fields)))

(defmacro defwinconstant (name &rest args) 
  `(progn (win-import-export ',name) (defconstant ,name ,@args)))

(defmacro defwinapi (name &rest args) 
  `(progn (win-import-export ',name) (ct:defun-dll ,name ,@args)))

(defconstant cl::C_NULL				(cl::int-to-foreign-ptr 0))
(defwinconstant NULL cl::c_null)

;;; error codes
(defwinconstant ERROR_SUCCESS   0)
(defwinconstant NO_ERROR 		0)
(defwinconstant ERROR_NO_MORE_ITEMS      259)

(defwintype int 	:long)
(defwintype UINT 	:unsigned-long)
(defwintype BOOL 	:long-bool)
(defwintype BYTE 	:unsigned-char)
(defwintype char 	:char)
(defwintype USHORT 	:unsigned-short)
(defwintype WORD 	:unsigned-short)
(defwintype short 	:short)
(defwintype DWORD 	:unsigned-long)
(defwintype long 	:long)
(defwintype ULONG 	:unsigned-long)
(defwintype HFILE 	int)
(defwintype HANDLE 	:handle)
(defwintype HWND 	handle)
(defwintype LPARAM 	long)
(defwintype LRESULT long)
(defwintype WPARAM 	long)
(defwintype HPEN 	handle)
(defwintype HBRUSH 	handle)
(defwintype HBITMAP handle)
(defwintype HMENU 	handle)
(defwintype HICON 	handle)
(defwintype HCURSOR hicon)
(defwintype FARPROC (:void *))
(defwintype WNDPROC farproc)
(defwintype WNDENUMPROC farproc)
(defwintype HINSTANCE handle)
(defwintype LPSTR 	(char *))
(defwintype LPTSTR 	lpstr)
(defwintype LPCSTR 	lpstr)
(defwintype VOID 	:void)
(defwintype LPVOID 	(:void *))
(defwintype PVOID 	(:void *))
(defwintype ATOM 	:unsigned-short)  ;; note: previous to Corman Lisp 2.6, this type
                                      ;; was incorrectly specified as :short
(defwintype HDC 	handle)
(defwintype HGDIOBJ (:void *))
(defwintype LPINT  (:long *))
(defwintype HKEY	LPVOID)
(defwintype LPDWORD	(DWORD *))
(defwintype ACCESS_MASK DWORD)
(defwintype PACCESS_MASK (ACCESS_MASK *))
(defwintype wchar_t :wide-char)
(defwintype WCHAR   wchar_t)
(defwintype HLOCAL HANDLE)

(defwinconstant FALSE				0)
(defwinconstant TRUE				1)

(defun HIWORD (x) (ash x -16))
(defun LOWORD (x) (logand x #xffff))

(defun RGB (r g b) 
	(+ (ash (mod b 256) 16) (ash (mod g 256) 8) (mod r 256)))
(export 'RGB)

(defwintype COLORREF :unsigned-long)

(defwinstruct POINT
  ((x               long)
   (y               long)
  ))

(defwinstruct RECT
  ((left            long)
   (top             long)
   (right           long)
   (bottom          long)
  ))

(defwinstruct MSG
  ((hwnd            HWND)
   (message         UINT)
   (wparam          WPARAM)
   (lparam          LPARAM)
   (time            DWORD)
   (pt              POINT)
  ))

(defwintype LPMSG (MSG *))

(defwinstruct WNDCLASSEX
  ((cbSize			UINT)
   (style           UINT)
   (lpfnWndProc     WNDPROC)
   (cbClsExtra      int)
   (cbWndExtra      int)
   (hInstance       HANDLE)
   (hIcon           HICON)
   (hCursor         HCURSOR)
   (hbrBackground   HBRUSH)
   (lpszMenuName    LPCSTR)
   (lpszClassName   LPCSTR)
   (hIconSm			HICON)
  ))
(defwintype LPWNDCLASSEX (WNDCLASSEX *))

(defwinstruct PAINTSTRUCT
  ((hdc             HDC)
   (ferase          BOOL)
   (rcpaint         RECT)
   (frestore        BOOL)
   (fincupdate      BOOL)
   (rgbreserved     (BYTE 32))
  ))
(defwintype LPPAINTSTRUCT (PAINTSTRUCT *))

(defwinstruct WINDOWPLACEMENT
  ((length 				UINT)
   (flags				UINT)
   (showCmd				UINT)
   (ptMinPosition		POINT)
   (ptMaxPosition		POINT)
   (rcNormalPosition	RECT)
  ))
(defwintype LPWINDOWPLACEMENT (WINDOWPLACEMENT *))

(defwinstruct FILETIME
	((dwLowDateTime  DWORD)
	 (dwHighDateTime DWORD)
	))
(defwintype LPFILETIME (FILETIME *))

(defwinstruct SYSTEMTIME
	((wYear  		WORD)
	 (wMonth 		WORD)
	 (wDayOfWeek 	WORD)
	 (wDay 			WORD)
	 (wHour 		WORD)
	 (wMinute 		WORD)
	 (wSecond 		WORD)
	 (wMilliseconds WORD)
	))
(defwintype LPSYSTEMTIME (SYSTEMTIME *))

(defwinstruct COORD
	((X SHORT)
     (Y SHORT)
	))
(defwintype PCOORD (COORD *))

(defwinstruct CREATESTRUCT
  ((lpCreateParams	LPVOID)
   (hInstance       HINSTANCE)
   (hMenu		    HMENU)
   (hwndParent      HWND)
   (cy      		int)
   (cx 				int)
   (y 				int)
   (x				int)
   (style 			LONG)
   (lpszName 		LPCSTR)
   (lpszClass 		LPCSTR)
   (dwExStyle		DWORD)
  ))

(defwinstruct CLIENTCREATESTRUCT
	((hWindowMenu	HANDLE)
	 (idFirstChild	UINT)))
(defwintype LPCLIENTCREATESTRUCT (CLIENTCREATESTRUCT *))

(defwinstruct MDICREATESTRUCT
	((szClass		LPCSTR)
	 (szTitle		LPCSTR)
	 (hOwner		HANDLE)
	 (x				int)
	 (y				int)
	 (cx			int)
	 (cy			int)
	 (style			DWORD)
	 (lParam		LPARAM)))		;; app-defined stuff
(defwintype LPMDICREATESTRUCT (MDICREATESTRUCT *))

(defwinconstant LF_FACESIZE 32)

;; CHARFORMAT masks
(defwinconstant CFM_BOLD		#x00000001)
(defwinconstant CFM_ITALIC		#x00000002)
(defwinconstant CFM_UNDERLINE	#x00000004)
(defwinconstant CFM_STRIKEOUT	#x00000008)
(defwinconstant CFM_PROTECTED	#x00000010)
(defwinconstant CFM_LINK		#x00000020)
(defwinconstant CFM_SIZE		#x80000000)
(defwinconstant CFM_COLOR		#x40000000)
(defwinconstant CFM_FACE		#x20000000)
(defwinconstant CFM_OFFSET		#x10000000)
(defwinconstant CFM_CHARSET		#x08000000)

;; CHARFORMAT effects
(defwinconstant CFE_BOLD		#x0001)
(defwinconstant CFE_ITALIC		#x0002)
(defwinconstant CFE_UNDERLINE	#x0004)
(defwinconstant CFE_STRIKEOUT	#x0008)
(defwinconstant CFE_PROTECTED	#x0010)
(defwinconstant CFE_LINK		#x0020)
(defwinconstant CFE_AUTOCOLOR	#x40000000)		
    
(defwinstruct CHARFORMAT
   ((cbSize         UINT) 
    (dwMask         DWORD)
    (dwEffects      DWORD)
    (yHeight        LONG)
    (yOffset        LONG) 
    (crTextColor    COLORREF) 
    (bCharSet       BYTE) 
    (bPitchAndFamily BYTE) 
    (pad1           BYTE) 
    (pad2           BYTE) 
    (szFaceName     (:char LF_FACESIZE))))
 
(defwinconstant SCF_SELECTION		#x0001)
(defwinconstant SCF_WORD			#x0002)
(defwinconstant SCF_DEFAULT			#x0000)		;; Set default charformat or paraformat
(defwinconstant SCF_ALL				#x0004)		;; Not valid with SCF_SELECTION or SCF_WORD
(defwinconstant SCF_USEUIRULES		#x0008)		;; Modifier for SCF_SELECTION; says that
										        ;;  format came from a toolbar, etc., and
										        ;;  hence UI formatting rules should be
										        ;;  used instead of literal formatting
(defwinconstant SCF_ASSOCIATEFONT	#x0010)		;; Associate fontname with bCharSet (one
										        ;;  possible for each of Western, ME, FE, Thai)
(defwinconstant SCF_NOKBUPDATE		#x0020)		;; Do not update the KB layput for this change
        									    ;; even if autokeyboard is on.
   
(defwinstruct TIME_ZONE_INFORMATION
	((Bias LONG)
	 (StandardName (WCHAR 32))
	 (StandardDate SYSTEMTIME)
	 (StandardBias LONG)
	 (DaylightName (WCHAR 32))
	 (DaylightDate SYSTEMTIME)
	 (DaylightBias LONG)))
(defwintype LPTIME_ZONE_INFORMATION (TIME_ZONE_INFORMATION *))

(defwinconstant TIME_ZONE_ID_INVALID  	#xFFFFFFFF)
(defwinconstant TIME_ZONE_ID_UNKNOWN    0)
(defwinconstant TIME_ZONE_ID_STANDARD   1)
(defwinconstant TIME_ZONE_ID_DAYLIGHT   2)
			
;;;
;;;	Window messages
;;;
(defwinconstant WM_NULL 			#x0000)
(defwinconstant WM_CREATE 			#x0001)
(defwinconstant WM_DESTROY 			#x0002)
(defwinconstant WM_MOVE 			#x0003)
(defwinconstant WM_SIZE 			#x0005)
(defwinconstant WM_ACTIVATE 		#x0006)
(defwinconstant WM_SETFOCUS			#x0007)
(defwinconstant WM_KILLFOCUS       	#x0008)
(defwinconstant WM_ENABLE          	#x000A)
(defwinconstant WM_SETREDRAW       	#x000B)
(defwinconstant WM_SETTEXT         	#x000C)
(defwinconstant WM_GETTEXT       	#x000D)
(defwinconstant WM_GETTEXTLENGTH 	#x000E)
(defwinconstant WM_PAINT         	#x000F)
(defwinconstant WM_CLOSE         	#x0010)
(defwinconstant WM_QUERYENDSESSION 	#x0011)
(defwinconstant WM_QUIT          	#x0012)
(defwinconstant WM_QUERYOPEN     	#x0013)
(defwinconstant WM_ERASEBKGND    	#x0014)
(defwinconstant WM_SYSCOLORCHANGE	#x0015)
(defwinconstant WM_ENDSESSION    	#x0016)
(defwinconstant WM_SHOWWINDOW    	#x0018)
(defwinconstant WM_WININICHANGE  	#x001A)
(defwinconstant WM_DEVMODECHANGE 	#x001B)
(defwinconstant WM_ACTIVATEAPP   	#x001C)
(defwinconstant WM_FONTCHANGE    	#x001D)
(defwinconstant WM_TIMECHANGE    	#x001E)
(defwinconstant WM_CANCELMODE    	#x001F)
(defwinconstant WM_SETCURSOR     	#x0020)
(defwinconstant WM_MOUSEACTIVATE 	#x0021)
(defwinconstant WM_CHILDACTIVATE 	#x0022)
(defwinconstant WM_QUEUESYNC     	#x0023)

(defwinconstant WM_PAINTICON                    #x0026)
(defwinconstant WM_ICONERASEBKGND               #x0027)
(defwinconstant WM_NEXTDLGCTL                   #x0028)
(defwinconstant WM_SPOOLERSTATUS                #x002A)
(defwinconstant WM_DRAWITEM                     #x002B)
(defwinconstant WM_MEASUREITEM                  #x002C)
(defwinconstant WM_DELETEITEM                   #x002D)
(defwinconstant WM_VKEYTOITEM                   #x002E)
(defwinconstant WM_CHARTOITEM                   #x002F)
(defwinconstant WM_SETFONT                      #x0030)
(defwinconstant WM_GETFONT                      #x0031)
(defwinconstant WM_SETHOTKEY                    #x0032)
(defwinconstant WM_GETHOTKEY                    #x0033)
(defwinconstant WM_QUERYDRAGICON                #x0037)
(defwinconstant WM_COMPAREITEM                  #x0039)
(defwinconstant WM_COMPACTING                   #x0041)
(defwinconstant WM_COMMNOTIFY                   #x0044)
(defwinconstant WM_WINDOWPOSCHANGING            #x0046)
(defwinconstant WM_WINDOWPOSCHANGED             #x0047)
(defwinconstant WM_POWER                        #x0048)
(defwinconstant PWR_OK              			1)
(defwinconstant PWR_FAIL            			-1)
(defwinconstant PWR_SUSPENDREQUEST  			1)
(defwinconstant PWR_SUSPENDRESUME   			2)
(defwinconstant PWR_CRITICALRESUME  			3)
(defwinconstant WM_COPYDATA                     #x004A)
(defwinconstant WM_CANCELJOURNAL                #x004B)
(defwinconstant WM_NOTIFY                       #x004E)
(defwinconstant WM_INPUTLANGCHANGEREQUEST       #x0050)
(defwinconstant WM_INPUTLANGCHANGE              #x0051)
(defwinconstant WM_TCARD                        #x0052)
(defwinconstant WM_HELP                         #x0053)
(defwinconstant WM_USERCHANGED                  #x0054)
(defwinconstant WM_NOTIFYFORMAT                 #x0055)
(defwinconstant NFR_ANSI                             1)
(defwinconstant NFR_UNICODE                          2)
(defwinconstant NF_QUERY                             3)
(defwinconstant NF_REQUERY                           4)
(defwinconstant WM_CONTEXTMENU                  #x007B)
(defwinconstant WM_STYLECHANGING                #x007C)
(defwinconstant WM_STYLECHANGED                 #x007D)
(defwinconstant WM_DISPLAYCHANGE                #x007E)
(defwinconstant WM_GETICON                      #x007F)
(defwinconstant WM_SETICON                      #x0080)
(defwinconstant WM_NCCREATE                     #x0081)
(defwinconstant WM_NCDESTROY                    #x0082)
(defwinconstant WM_NCCALCSIZE                   #x0083)
(defwinconstant WM_NCHITTEST                    #x0084)
(defwinconstant WM_NCPAINT                      #x0085)
(defwinconstant WM_NCACTIVATE                   #x0086)
(defwinconstant WM_GETDLGCODE                   #x0087)
(defwinconstant WM_NCMOUSEMOVE                  #x00A0)
(defwinconstant WM_NCLBUTTONDOWN                #x00A1)
(defwinconstant WM_NCLBUTTONUP                  #x00A2)
(defwinconstant WM_NCLBUTTONDBLCLK              #x00A3)
(defwinconstant WM_NCRBUTTONDOWN                #x00A4)
(defwinconstant WM_NCRBUTTONUP                  #x00A5)
(defwinconstant WM_NCRBUTTONDBLCLK              #x00A6)
(defwinconstant WM_NCMBUTTONDOWN                #x00A7)
(defwinconstant WM_NCMBUTTONUP                  #x00A8)
(defwinconstant WM_NCMBUTTONDBLCLK              #x00A9)
(defwinconstant WM_KEYFIRST                     #x0100)
(defwinconstant WM_KEYDOWN                      #x0100)
(defwinconstant WM_KEYUP                        #x0101)
(defwinconstant WM_CHAR                         #x0102)
(defwinconstant WM_DEADCHAR                     #x0103)
(defwinconstant WM_SYSKEYDOWN                   #x0104)
(defwinconstant WM_SYSKEYUP                     #x0105)
(defwinconstant WM_SYSCHAR                      #x0106)
(defwinconstant WM_SYSDEADCHAR                  #x0107)
(defwinconstant WM_KEYLAST                      #x0108)
(defwinconstant WM_IME_STARTCOMPOSITION         #x010D)
(defwinconstant WM_IME_ENDCOMPOSITION           #x010E)
(defwinconstant WM_IME_COMPOSITION              #x010F)
(defwinconstant WM_IME_KEYLAST                  #x010F)


(defwinconstant WM_INITDIALOG                   #x0110)
(defwinconstant WM_COMMAND                      #x0111)
(defwinconstant WM_SYSCOMMAND                   #x0112)
(defwinconstant WM_TIMER                        #x0113)
(defwinconstant WM_HSCROLL                      #x0114)
(defwinconstant WM_VSCROLL                      #x0115)
(defwinconstant WM_INITMENU                     #x0116)
(defwinconstant WM_INITMENUPOPUP                #x0117)
(defwinconstant WM_MENUSELECT                   #x011F)
(defwinconstant WM_MENUCHAR                     #x0120)
(defwinconstant WM_ENTERIDLE                    #x0121)

(defwinconstant WM_CTLCOLORMSGBOX               #x0132)
(defwinconstant WM_CTLCOLOREDIT                 #x0133)
(defwinconstant WM_CTLCOLORLISTBOX              #x0134)
(defwinconstant WM_CTLCOLORBTN                  #x0135)
(defwinconstant WM_CTLCOLORDLG                  #x0136)
(defwinconstant WM_CTLCOLORSCROLLBAR            #x0137)
(defwinconstant WM_CTLCOLORSTATIC               #x0138)

(defwinconstant WM_MOUSEFIRST                   #x0200)
(defwinconstant WM_MOUSEMOVE                    #x0200)
(defwinconstant WM_LBUTTONDOWN                  #x0201)
(defwinconstant WM_LBUTTONUP                    #x0202)
(defwinconstant WM_LBUTTONDBLCLK                #x0203)
(defwinconstant WM_RBUTTONDOWN                  #x0204)
(defwinconstant WM_RBUTTONUP                    #x0205)
(defwinconstant WM_RBUTTONDBLCLK                #x0206)
(defwinconstant WM_MBUTTONDOWN                  #x0207)
(defwinconstant WM_MBUTTONUP                    #x0208)
(defwinconstant WM_MBUTTONDBLCLK                #x0209)
(defwinconstant WM_MOUSEWHEEL                   #x020A)
(defwinconstant WM_MOUSELAST                    #x020A)
(defwinconstant WHEEL_DELTA                     120)        ; Value for rolling one detent
(defwinconstant WHEEL_PAGESCROLL                #xffffffff) ; Scroll one page
(defwinconstant WM_PARENTNOTIFY                 #x0210)
(defwinconstant MENULOOP_WINDOW                 0)
(defwinconstant MENULOOP_POPUP                  1)
(defwinconstant WM_ENTERMENULOOP                #x0211)
(defwinconstant WM_EXITMENULOOP                 #x0212)
(defwinconstant WM_NEXTMENU                     #x0213)
(defwinconstant WM_SIZING                       #x0214)
(defwinconstant WM_CAPTURECHANGED               #x0215)
(defwinconstant WM_MOVING                       #x0216)
(defwinconstant WM_POWERBROADCAST               #x0218)
(defwinconstant WM_DEVICECHANGE                 #x0219)

(defwinconstant WM_IME_SETCONTEXT               #x0281)
(defwinconstant WM_IME_NOTIFY                   #x0282)
(defwinconstant WM_IME_CONTROL                  #x0283)
(defwinconstant WM_IME_COMPOSITIONFULL          #x0284)
(defwinconstant WM_IME_SELECT                   #x0285)
(defwinconstant WM_IME_CHAR                     #x0286)
(defwinconstant WM_IME_KEYDOWN                  #x0290)
(defwinconstant WM_IME_KEYUP                    #x0291)
(defwinconstant WM_MDICREATE                    #x0220)
(defwinconstant WM_MDIDESTROY                   #x0221)
(defwinconstant WM_MDIACTIVATE                  #x0222)
(defwinconstant WM_MDIRESTORE                   #x0223)
(defwinconstant WM_MDINEXT                      #x0224)
(defwinconstant WM_MDIMAXIMIZE                  #x0225)
(defwinconstant WM_MDITILE                      #x0226)
(defwinconstant WM_MDICASCADE                   #x0227)
(defwinconstant WM_MDIICONARRANGE               #x0228)
(defwinconstant WM_MDIGETACTIVE                 #x0229)
(defwinconstant WM_MDISETMENU                   #x0230)
(defwinconstant WM_ENTERSIZEMOVE                #x0231)
(defwinconstant WM_EXITSIZEMOVE                 #x0232)
(defwinconstant WM_DROPFILES                    #x0233)
(defwinconstant WM_MDIREFRESHMENU               #x0234)
(defwinconstant WM_NCMOUSEHOVER                 #x02A0)
(defwinconstant WM_MOUSEHOVER                   #x02A1)
(defwinconstant WM_NCMOUSELEAVE                 #x02A2)
(defwinconstant WM_MOUSELEAVE                   #x02A3)
(defwinconstant WM_CUT                          #x0300)
(defwinconstant WM_COPY                         #x0301)
(defwinconstant WM_PASTE                        #x0302)
(defwinconstant WM_CLEAR                        #x0303)
(defwinconstant WM_UNDO                         #x0304)
(defwinconstant WM_RENDERFORMAT                 #x0305)
(defwinconstant WM_RENDERALLFORMATS             #x0306)
(defwinconstant WM_DESTROYCLIPBOARD             #x0307)
(defwinconstant WM_DRAWCLIPBOARD                #x0308)
(defwinconstant WM_PAINTCLIPBOARD               #x0309)
(defwinconstant WM_VSCROLLCLIPBOARD             #x030A)
(defwinconstant WM_SIZECLIPBOARD                #x030B)
(defwinconstant WM_ASKCBFORMATNAME              #x030C)
(defwinconstant WM_CHANGECBCHAIN                #x030D)
(defwinconstant WM_HSCROLLCLIPBOARD             #x030E)

(defwinconstant WM_QUERYNEWPALETTE              #x030F)
(defwinconstant WM_PALETTEISCHANGING            #x0310)
(defwinconstant WM_PALETTECHANGED               #x0311)
(defwinconstant WM_HOTKEY                       #x0312)
(defwinconstant WM_PRINT                        #x0317)
(defwinconstant WM_PRINTCLIENT                  #x0318)
(defwinconstant WM_HANDHELDFIRST                #x0358)
(defwinconstant WM_HANDHELDLAST                 #x035F)
(defwinconstant WM_AFXFIRST                     #x0360)
(defwinconstant WM_AFXLAST                      #x037F)
(defwinconstant WM_PENWINFIRST                  #x0380)
(defwinconstant WM_PENWINLAST                   #x038F)


;;;
;;; WM_ACTIVATE state values
;;;
(defwinconstant WA_INACTIVE     0)
(defwinconstant WA_ACTIVE       1)
(defwinconstant WA_CLICKACTIVE  2)

;;
;; ShowWindow() Commands
;;
(defwinconstant SW_HIDE             0)
(defwinconstant SW_SHOWNORMAL       1)
(defwinconstant SW_NORMAL           1)
(defwinconstant SW_SHOWMINIMIZED    2)
(defwinconstant SW_SHOWMAXIMIZED    3)
(defwinconstant SW_MAXIMIZE         3)
(defwinconstant SW_SHOWNOACTIVATE   4)
(defwinconstant SW_SHOW             5)
(defwinconstant SW_MINIMIZE         6)
(defwinconstant SW_SHOWMINNOACTIVE  7)
(defwinconstant SW_SHOWNA           8)
(defwinconstant SW_RESTORE          9)
(defwinconstant SW_SHOWDEFAULT      10)
(defwinconstant SW_MAX              10)

;;
;; Class styles
;;
(defwinconstant CS_VREDRAW          #x0001)
(defwinconstant CS_HREDRAW          #x0002)
(defwinconstant CS_KEYCVTWINDOW     #x0004)
(defwinconstant CS_DBLCLKS          #x0008)
(defwinconstant CS_OWNDC            #x0020)
(defwinconstant CS_CLASSDC          #x0040)
(defwinconstant CS_PARENTDC         #x0080)
(defwinconstant CS_NOKEYCVT         #x0100)
(defwinconstant CS_NOCLOSE          #x0200)
(defwinconstant CS_SAVEBITS         #x0800)
(defwinconstant CS_BYTEALIGNCLIENT  #x1000)
(defwinconstant CS_BYTEALIGNWINDOW  #x2000)
(defwinconstant CS_GLOBALCLASS      #x4000)

;;
;; Standard Icon IDs
;;
(defun MAKEINTRESOURCE (n) (cl::int-to-foreign-ptr n))

(defwinconstant IDI_APPLICATION     (MAKEINTRESOURCE 32512))
(defwinconstant IDI_HAND            (MAKEINTRESOURCE 32513))
(defwinconstant IDI_QUESTION        (MAKEINTRESOURCE 32514))
(defwinconstant IDI_EXCLAMATION     (MAKEINTRESOURCE 32515))
(defwinconstant IDI_ASTERISK        (MAKEINTRESOURCE 32516))
(defwinconstant IDI_WINLOGO         (MAKEINTRESOURCE 32517))

;;
;; Standard Cursor IDs
;;
(defwinconstant IDC_ARROW           (MAKEINTRESOURCE 32512))
(defwinconstant IDC_IBEAM           (MAKEINTRESOURCE 32513))
(defwinconstant IDC_WAIT            (MAKEINTRESOURCE 32514))
(defwinconstant IDC_CROSS           (MAKEINTRESOURCE 32515))
(defwinconstant IDC_UPARROW         (MAKEINTRESOURCE 32516))
(defwinconstant IDC_SIZE            (MAKEINTRESOURCE 32640))
(defwinconstant IDC_ICON            (MAKEINTRESOURCE 32641))
(defwinconstant IDC_SIZENWSE        (MAKEINTRESOURCE 32642))
(defwinconstant IDC_SIZENESW        (MAKEINTRESOURCE 32643))
(defwinconstant IDC_SIZEWE          (MAKEINTRESOURCE 32644))
(defwinconstant IDC_SIZENS          (MAKEINTRESOURCE 32645))
(defwinconstant IDC_SIZEALL         (MAKEINTRESOURCE 32646))
(defwinconstant IDC_NO              (MAKEINTRESOURCE 32648))
(defwinconstant IDC_APPSTARTING     (MAKEINTRESOURCE 32650))
(defwinconstant IDC_HELP            (MAKEINTRESOURCE 32651))

;; Stock Logical Objects 
(defwinconstant WHITE_BRUSH         0)
(defwinconstant LTGRAY_BRUSH        1)
(defwinconstant GRAY_BRUSH          2)
(defwinconstant DKGRAY_BRUSH        3)
(defwinconstant BLACK_BRUSH         4)
(defwinconstant NULL_BRUSH          5)
(defwinconstant HOLLOW_BRUSH        NULL_BRUSH)
(defwinconstant WHITE_PEN           6)
(defwinconstant BLACK_PEN           7)
(defwinconstant NULL_PEN            8)
(defwinconstant OEM_FIXED_FONT      10)
(defwinconstant ANSI_FIXED_FONT     11)
(defwinconstant ANSI_VAR_FONT       12)
(defwinconstant SYSTEM_FONT         13)
(defwinconstant DEVICE_DEFAULT_FONT 14)
(defwinconstant DEFAULT_PALETTE     15)
(defwinconstant SYSTEM_FIXED_FONT   16)
(defwinconstant DEFAULT_GUI_FONT    17)
(defwinconstant STOCK_LAST          17)

;;
;; Window Styles
;;
(defwinconstant WS_OVERLAPPED       #x00000000)
(defwinconstant WS_POPUP            #x80000000)
(defwinconstant WS_CHILD            #x40000000)
(defwinconstant WS_MINIMIZE         #x20000000)
(defwinconstant WS_VISIBLE          #x10000000)
(defwinconstant WS_DISABLED         #x08000000)
(defwinconstant WS_CLIPSIBLINGS     #x04000000)
(defwinconstant WS_CLIPCHILDREN     #x02000000)
(defwinconstant WS_MAXIMIZE         #x01000000)
(defwinconstant WS_CAPTION          #x00C00000)
(defwinconstant WS_BORDER           #x00800000)
(defwinconstant WS_DLGFRAME         #x00400000)
(defwinconstant WS_VSCROLL          #x00200000)
(defwinconstant WS_HSCROLL          #x00100000)
(defwinconstant WS_SYSMENU          #x00080000)
(defwinconstant WS_THICKFRAME       #x00040000)
(defwinconstant WS_GROUP            #x00020000)
(defwinconstant WS_TABSTOP          #x00010000)

(defwinconstant WS_MINIMIZEBOX      #x00020000)
(defwinconstant WS_MAXIMIZEBOX      #x00010000)

(defwinconstant WS_TILED            WS_OVERLAPPED)
(defwinconstant WS_ICONIC           WS_MINIMIZE)
(defwinconstant WS_SIZEBOX          WS_THICKFRAME)

(defwinconstant CW_USEDEFAULT       #x80000000)

;;
;; Dialog Box Command IDs
;;
(defwinconstant IDOK               1)
(defwinconstant IDCANCEL           2)
(defwinconstant IDABORT            3)
(defwinconstant IDRETRY            4)
(defwinconstant IDIGNORE           5)
(defwinconstant IDYES              6)
(defwinconstant IDNO               7)
(defwinconstant IDCLOSE         	8)
(defwinconstant IDHELP          	9)

;;
;; Common Window Styles
;;
(defwinconstant WS_OVERLAPPEDWINDOW 
				(logior 
					WS_OVERLAPPED
                    WS_CAPTION
                    WS_SYSMENU
                    WS_THICKFRAME
                    WS_MINIMIZEBOX
                    WS_MAXIMIZEBOX))
(defwinconstant WS_TILEDWINDOW      WS_OVERLAPPEDWINDOW)

(defwinconstant WS_POPUPWINDOW 
				(logior WS_POPUP
                        WS_BORDER
                        WS_SYSMENU))

(defwinconstant WS_CHILDWINDOW WS_CHILD)

;;
;; Extended Window Styles
;;
(defwinconstant WS_EX_DLGMODALFRAME     #x00000001)
(defwinconstant WS_EX_NOPARENTNOTIFY    #x00000004)
(defwinconstant WS_EX_TOPMOST           #x00000008)
(defwinconstant WS_EX_ACCEPTFILES       #x00000010)
(defwinconstant WS_EX_TRANSPARENT       #x00000020)

(defwinconstant WS_EX_MDICHILD          #x00000040)
(defwinconstant WS_EX_TOOLWINDOW        #x00000080)
(defwinconstant WS_EX_WINDOWEDGE        #x00000100)
(defwinconstant WS_EX_CLIENTEDGE        #x00000200)
(defwinconstant WS_EX_CONTEXTHELP       #x00000400)

(defwinconstant WS_EX_RIGHT             #x00001000)
(defwinconstant WS_EX_LEFT              #x00000000)
(defwinconstant WS_EX_RTLREADING        #x00002000)
(defwinconstant WS_EX_LTRREADING        #x00000000)
(defwinconstant WS_EX_LEFTSCROLLBAR     #x00004000)
(defwinconstant WS_EX_RIGHTSCROLLBAR    #x00000000)

(defwinconstant WS_EX_CONTROLPARENT     #x00010000)
(defwinconstant WS_EX_STATICEDGE        #x00020000)
(defwinconstant WS_EX_APPWINDOW         #x00040000)


(defwinconstant WS_EX_OVERLAPPEDWINDOW  (logior WS_EX_WINDOWEDGE WS_EX_CLIENTEDGE))
(defwinconstant WS_EX_PALETTEWINDOW     (logior WS_EX_WINDOWEDGE WS_EX_TOOLWINDOW WS_EX_TOPMOST))

;;
;; DrawText() Format Flags
;;
(defwinconstant DT_TOP              #x00000000)
(defwinconstant DT_LEFT             #x00000000)
(defwinconstant DT_CENTER           #x00000001)
(defwinconstant DT_RIGHT            #x00000002)
(defwinconstant DT_VCENTER          #x00000004)
(defwinconstant DT_BOTTOM           #x00000008)
(defwinconstant DT_WORDBREAK        #x00000010)
(defwinconstant DT_SINGLELINE       #x00000020)
(defwinconstant DT_EXPANDTABS       #x00000040)
(defwinconstant DT_TABSTOP          #x00000080)
(defwinconstant DT_NOCLIP           #x00000100)
(defwinconstant DT_EXTERNALLEADING  #x00000200)
(defwinconstant DT_CALCRECT         #x00000400)
(defwinconstant DT_NOPREFIX         #x00000800)
(defwinconstant DT_INTERNAL         #x00001000)
(defwinconstant DT_EDITCONTROL      #x00002000)
(defwinconstant DT_PATH_ELLIPSIS    #x00004000)
(defwinconstant DT_END_ELLIPSIS     #x00008000)
(defwinconstant DT_MODIFYSTRING     #x00010000)
(defwinconstant DT_RTLREADING       #x00020000)
(defwinconstant DT_WORD_ELLIPSIS    #x00040000)

(defwinconstant ES_LEFT             #x0000)
(defwinconstant ES_CENTER           #x0001)
(defwinconstant ES_RIGHT            #x0002)
(defwinconstant ES_MULTILINE        #x0004)
(defwinconstant ES_UPPERCASE        #x0008)
(defwinconstant ES_LOWERCASE        #x0010)
(defwinconstant ES_PASSWORD         #x0020)
(defwinconstant ES_AUTOVSCROLL      #x0040)
(defwinconstant ES_AUTOHSCROLL      #x0080)
(defwinconstant ES_NOHIDESEL        #x0100)
(defwinconstant ES_OEMCONVERT       #x0400)
(defwinconstant ES_READONLY         #x0800)
(defwinconstant ES_WANTRETURN       #x1000)
(defwinconstant ES_NUMBER           #x2000)

;;;
;;;	CreateFile WIN32 interface
;;;

;;; access codes
(defwinconstant GENERIC_READ		#x80000000)
(defwinconstant GENERIC_WRITE		#x40000000)

;;; share codes
(defwinconstant FILE_SHARE_READ	#x00000001) 
(defwinconstant FILE_SHARE_WRITE	#x00000002)  
(defwinconstant FILE_SHARE_DELETE	#x00000004)  

;;; creation constants
(defwinconstant CREATE_NEW          1)
(defwinconstant CREATE_ALWAYS       2)
(defwinconstant OPEN_EXISTING       3)
(defwinconstant OPEN_ALWAYS         4)
(defwinconstant TRUNCATE_EXISTING   5)

;;; file attributes
(defwinconstant FILE_ATTRIBUTE_READONLY         #x00000001)  
(defwinconstant FILE_ATTRIBUTE_HIDDEN           #x00000002)  
(defwinconstant FILE_ATTRIBUTE_SYSTEM           #x00000004)  
(defwinconstant FILE_ATTRIBUTE_DIRECTORY        #x00000010)  
(defwinconstant FILE_ATTRIBUTE_ARCHIVE          #x00000020)  
(defwinconstant FILE_ATTRIBUTE_ENCRYPTED        #x00000040)  
(defwinconstant FILE_ATTRIBUTE_NORMAL           #x00000080)  
(defwinconstant FILE_ATTRIBUTE_TEMPORARY        #x00000100)  
(defwinconstant FILE_ATTRIBUTE_SPARSE-FILE      #x00000200)  
(defwinconstant FILE_ATTRIBUTE_REPARSE_POINT    #x00000400)  
(defwinconstant FILE_ATTRIBUTE_COMPRESSED       #x00000800)  
(defwinconstant FILE_ATTRIBUTE_OFFLINE          #x00001000)  
(defwinconstant FILE_ATTRIBUTE_NOT_CONTENT_INDEXED #x00002000)  
(defwinconstant INVALID_HANDLE_VALUE			#xffffffff)

;;; seek origins
(defwinconstant FILE_BEGIN						0)
(defwinconstant FILE_CURRENT					1)
(defwinconstant FILE_END						2)

;;;
;;; Virtual Keys, Standard Set
;;;
(defwinconstant VK_LBUTTON          #x01)
(defwinconstant VK_RBUTTON          #x02)
(defwinconstant VK_CANCEL           #x03)
(defwinconstant VK_MBUTTON          #x04)  ;; NOT contiguous with L & RBUTTON
(defwinconstant VK_XBUTTON1         #x05)
(defwinconstant VK_XBUTTON2         #x06)
;;; #x07 : unassigned
(defwinconstant VK_BACK             #x08)
(defwinconstant VK_TAB              #x09)
;; #x0A - #x0B : reserved
(defwinconstant VK_CLEAR            #x0c)
(defwinconstant VK_RETURN           #x0d)
(defwinconstant VK_SHIFT            #x10)
(defwinconstant VK_CONTROL          #x11)
(defwinconstant VK_MENU             #x12)
(defwinconstant VK_PAUSE            #x13)
(defwinconstant VK_CAPITAL          #x14)
(defwinconstant VK_KANA             #x15)
(defwinconstant VK_JUNJA            #x17)
(defwinconstant VK_FINAL            #x18)
(defwinconstant VK_KANJI            #x19)
(defwinconstant VK_ESCAPE           #x1B)
(defwinconstant VK_CONVERT          #x1C)
(defwinconstant VK_NONCONVERT       #x1D)
(defwinconstant VK_ACCEPT           #x1E)
(defwinconstant VK_MODECHANGE       #x1F)
(defwinconstant VK_SPACE            #x20)
(defwinconstant VK_PRIOR            #x21)
(defwinconstant VK_NEXT             #x22)
(defwinconstant VK_END              #x23)
(defwinconstant VK_HOME             #x24)
(defwinconstant VK_LEFT             #x25)
(defwinconstant VK_UP               #x26)
(defwinconstant VK_RIGHT            #x27)
(defwinconstant VK_DOWN             #x28)
(defwinconstant VK_SELECT           #x29)
(defwinconstant VK_PRINT            #x2A)
(defwinconstant VK_EXECUTE          #x2B)
(defwinconstant VK_SNAPSHOT         #x2C)
(defwinconstant VK_INSERT           #x2D)
(defwinconstant VK_DELETE           #x2E)
(defwinconstant VK_HELP             #x2F)
;;; VK_0 - VK_9 are the same as ASCII '0' - '9' (#x30 - #x39)
;;; #x40 : unassigned
;;; VK_A - VK_Z are the same as ASCII 'A' - 'Z' (#x41 - #x5A)
(defwinconstant VK_LWIN             #x5B)
(defwinconstant VK_RWIN             #x5C)
(defwinconstant VK_APPS             #x5D)
;; #x5E : reserved
(defwinconstant VK_SLEEP            #x5F)
(defwinconstant VK_NUMPAD0          #x60)
(defwinconstant VK_NUMPAD1          #x61)
(defwinconstant VK_NUMPAD2          #x62)
(defwinconstant VK_NUMPAD3          #x63)
(defwinconstant VK_NUMPAD4          #x64)
(defwinconstant VK_NUMPAD5          #x65)
(defwinconstant VK_NUMPAD6          #x66)
(defwinconstant VK_NUMPAD7          #x67)
(defwinconstant VK_NUMPAD8          #x68)
(defwinconstant VK_NUMPAD9          #x69)
(defwinconstant VK_MULTIPLY         #x6A)
(defwinconstant VK_ADD              #x6B)
(defwinconstant VK_SEPARATOR        #x6C)
(defwinconstant VK_SUBTRACT         #x6D)
(defwinconstant VK_DECIMAL          #x6E)
(defwinconstant VK_DIVIDE           #x6F)
(defwinconstant VK_F1               #x70)
(defwinconstant VK_F2               #x71)
(defwinconstant VK_F3               #x72)
(defwinconstant VK_F4               #x73)
(defwinconstant VK_F5               #x74)
(defwinconstant VK_F6               #x75)
(defwinconstant VK_F7               #x76)
(defwinconstant VK_F8               #x77)
(defwinconstant VK_F9               #x78)
(defwinconstant VK_F10              #x79)
(defwinconstant VK_F11              #x7A)
(defwinconstant VK_F12              #x7B)
(defwinconstant VK_F13              #x7C)
(defwinconstant VK_F14              #x7D)
(defwinconstant VK_F15              #x7E)
(defwinconstant VK_F16              #x7F)
(defwinconstant VK_F17              #x80)
(defwinconstant VK_F18              #x81)
(defwinconstant VK_F19              #x82)
(defwinconstant VK_F20              #x83)
(defwinconstant VK_F21              #x84)
(defwinconstant VK_F22              #x85)
(defwinconstant VK_F23              #x86)
(defwinconstant VK_F24              #x87)
;; #x88 - 0x8F : unassigned
(defwinconstant VK_NUMLOCK          #x90)
(defwinconstant VK_SCROLL          #x91)

(defwinapi CreateWindowEx (
	(dwExStyle dword)
	(lpClassName LPCSTR)
	(lpWindowName LPCSTR)
	(dwStyle DWORD)
	(x int)
	(y int)
	(nWidth int)
	(nHeight int)
	(hWndParent HWND)
	(hMenu HMENU)
	(hInstance HANDLE)
	(lpParam LPVOID))
	:return-type HWND
	:library-name "User32.dll"
	:entry-name "CreateWindowExA"
	:linkage-type :pascal)

(defwinapi CreateWindow (
	(lpClassName LPCSTR)
	(lpWindowName LPCSTR)
	(dwStyle DWORD)
	(x int)
	(y int)
	(nWidth int)
	(nHeight int)
	(hWndParent HWND)
	(hMenu HMENU)
	(hInstance HANDLE)
	(lpParam LPVOID))
	:return-type HWND
	:library-name "User32.dll"
	:entry-name "CreateWindowA"
	:linkage-type :pascal)

(defwinapi CreateMDIWindow (
	(lpClassName LPCSTR)
	(lpWindowName LPCSTR)
	(dwStyle DWORD)
	(x int)
	(y int)
	(nWidth int)
	(nHeight int)
	(hWndParent HWND)
	(hMenu HMENU)
	(hInstance HANDLE)
	(lpParam LPVOID))
	:return-type HWND
	:library-name "User32.dll"
	:entry-name "CreateMDIWindowA"
	:linkage-type :pascal)

(defwinapi GetWindowLong ((hWnd HWND) (index int))
	:return-type LONG  
	:library-name "user32.dll"  
	:entry-name "GetWindowLongA"   
	:linkage-type :pascal)

(defwinapi SetWindowLong ((hWnd HWND) (index int) (value LONG))
	:return-type LONG  
	:library-name "user32.dll"  
	:entry-name "SetWindowLongA"   
	:linkage-type :pascal)

(defwinapi CallWindowProc ((wndproc WNDPROC) (hWnd HWND ) 
		(Msg UINT) (wParam WPARAM) (lParam LPARAM))
	:return-type LRESULT   
	:library-name "user32.dll"
	:entry-name "CallWindowProcA"
	:linkage-type :pascal)

(defwinapi GetClassInfoEx ((hinstance HINSTANCE) (lpclassname LPCTSTR ) 
		(lpwndclass LPWNDCLASSEX))
	:return-type BOOL   
	:library-name "user32.dll"
	:entry-name "GetClassInfoExA"
	:linkage-type :pascal)

#! (:library "User32" :ignore "WINUSERAPI" :export t :pascal "WINAPI")
WINUSERAPI BOOL WINAPI DestroyWindow(HWND hWnd);
WINUSERAPI HDC  WINAPI BeginPaint(HWND hWnd, LPPAINTSTRUCT lpPaint);
WINUSERAPI BOOL WINAPI EndPaint(HWND hWnd, CONST LPPAINTSTRUCT lpPaint);
WINUSERAPI HDC  WINAPI GetWindowDC(HWND hWnd);
WINUSERAPI int  WINAPI ReleaseDC(HWND hWnd, HDC hDC);
WINUSERAPI BOOL WINAPI GetUpdateRect(HWND hWnd, LPRECT lpRect, BOOL bErase);
WINUSERAPI int  WINAPI GetUpdateRgn(HWND hWnd, HRGN hRgn, BOOL bErase);
WINUSERAPI int  WINAPI SetWindowRgn(HWND hWnd, HRGN hRgn, BOOL bRedraw);
WINUSERAPI int  WINAPI GetWindowRgn(HWND hWnd, HRGN hRgn);
WINUSERAPI int  WINAPI ExcludeUpdateRgn(HDC hDC, HWND hWnd);
WINUSERAPI BOOL WINAPI InvalidateRect(HWND hWnd, CONST LPRECT lpRect, BOOL bErase);
WINUSERAPI BOOL WINAPI ValidateRect(HWND hWnd, CONST LPRECT lpRect);
WINUSERAPI BOOL WINAPI InvalidateRgn(HWND hWnd, HRGN hRgn, BOOL bErase);
WINUSERAPI BOOL WINAPI ValidateRgn(HWND hWnd, HRGN hRgn);
WINUSERAPI BOOL WINAPI RedrawWindow(HWND hWnd, CONST LPRECT lprcUpdate, HRGN hrgnUpdate, UINT flags);
WINUSERAPI BOOL WINAPI GetClientRect(HWND hWnd, LPRECT lpRect);
WINUSERAPI BOOL WINAPI GetWindowRect(HWND hWnd, LPRECT lpRect);
WINUSERAPI BOOL WINAPI AdjustWindowRect(LPRECT lpRect, DWORD dwStyle, BOOL bMenu);
WINUSERAPI BOOL WINAPI AdjustWindowRectEx(LPRECT lpRect, DWORD dwStyle, BOOL bMenu, DWORD dwExStyle);
WINUSERAPI VOID WINAPI PostQuitMessage(int nExitCode);
WINUSERAPI BOOL WINAPI IsWindow(HWND hWnd);
WINUSERAPI BOOL WINAPI IsMenu(HMENU hMenu);
WINUSERAPI BOOL WINAPI IsChild(HWND hWndParent, HWND hWnd);
WINUSERAPI BOOL WINAPI DestroyWindow(HWND hWnd);
WINUSERAPI BOOL WINAPI ShowWindow(HWND hWnd, int nCmdShow);
WINUSERAPI BOOL WINAPI ShowWindowAsync(HWND hWnd, int nCmdShow);
WINUSERAPI BOOL WINAPI FlashWindow(HWND hWnd, BOOL bInvert);
WINUSERAPI BOOL WINAPI ShowOwnedPopups(HWND hWnd, BOOL fShow);
WINUSERAPI BOOL WINAPI OpenIcon(HWND hWnd);
WINUSERAPI BOOL WINAPI CloseWindow(HWND hWnd);
WINUSERAPI BOOL WINAPI MoveWindow(HWND hWnd, int X, int Y, int nWidth, int nHeight, BOOL bRepaint);
WINUSERAPI BOOL WINAPI SetWindowPos(HWND hWnd, HWND hWndInsertAfter, int X, int Y, int cx, int cy, UINT uFlags);
WINUSERAPI BOOL WINAPI GetWindowPlacement(HWND hWnd, LPWINDOWPLACEMENT lpwndpl);
WINUSERAPI BOOL WINAPI SetWindowPlacement(HWND hWnd, CONST LPWINDOWPLACEMENT lpwndpl);
WINUSERAPI BOOL WINAPI UpdateWindow(HWND hWnd);
WINUSERAPI HWND WINAPI SetActiveWindow(HWND hWnd);
WINUSERAPI HWND WINAPI GetForegroundWindow();
WINUSERAPI BOOL WINAPI PaintDesktop(HDC hdc);
WINUSERAPI BOOL WINAPI SetForegroundWindow(HWND hWnd);
WINUSERAPI HWND WINAPI WindowFromDC(HDC hDC);
WINUSERAPI HDC  WINAPI GetDC(HWND hWnd);
WINUSERAPI HDC  WINAPI GetDCEx(HWND hWnd, HRGN hrgnClip, DWORD flags);
WINUSERAPI HWND WINAPI GetWindow(HWND hWnd, UINT uCmd);
WINUSERAPI BOOL WINAPI GetClientRect(HWND hWnd, LPRECT lpRect);
WINUSERAPI BOOL WINAPI GetWindowRect(HWND hWnd, LPRECT lpRect);
WINUSERAPI BOOL WINAPI AdjustWindowRect(LPRECT lpRect, DWORD dwStyle, BOOL bMenu);
WINUSERAPI BOOL WINAPI AdjustWindowRectEx(LPRECT lpRect, DWORD dwStyle, BOOL bMenu, DWORD dwExStyle);
WINUSERAPI HWND WINAPI GetTopWindow(HWND hWnd);
WINUSERAPI BOOL WINAPI EnumWindows(WNDENUMPROC lpEnumFunc, LPARAM lParam);
WINUSERAPI BOOL WINAPI EnumThreadWindows(DWORD dwThreadId, WNDENUMPROC lpfn, LPARAM lParam);
WINUSERAPI int  WINAPI SetScrollPos(HWND hWnd, int nBar, int nPos, BOOL bRedraw);
WINUSERAPI int  WINAPI GetScrollPos(HWND hWnd, int nBar);
WINUSERAPI BOOL WINAPI SetScrollRange(HWND hWnd, int nBar, int nMinPos, int nMaxPos, BOOL bRedraw);
WINUSERAPI BOOL WINAPI GetScrollRange(HWND hWnd, int nBar, LPINT lpMinPos, LPINT lpMaxPos);
WINUSERAPI BOOL WINAPI ShowScrollBar(HWND hWnd, int wBar, BOOL bShow);
WINUSERAPI BOOL WINAPI EnableScrollBar(HWND hWnd, UINT wSBflags, UINT wArrows);
WINUSERAPI int  WINAPI GetSystemMetrics(int nIndex);
WINUSERAPI BOOL WINAPI LockWindowUpdate(HWND hWndLock);
WINUSERAPI BOOL WINAPI ScrollWindow(HWND hWnd, int XAmount, int YAmount, LPRECT lpRect, LPRECT lpClipRect);
WINUSERAPI BOOL WINAPI ScrollDC(HDC hDC, int dx, int dy, LPRECT lprcScroll, 
							LPRECT lprcClip, HRGN hrgnUpdate, LPRECT lprcUpdate);
WINUSERAPI int  WINAPI ScrollWindowEx(HWND hWnd, int dx, int dy, LPRECT prcScroll, LPRECT prcClip,
							HRGN hrgnUpdate, LPRECT prcUpdate, UINT flags);
WINUSERAPI BOOL WINAPI MessageBeep(UINT uType);
WINUSERAPI int  WINAPI FillRect(HDC hDC, CONST LPRECT lprc, HBRUSH hbr);
WINUSERAPI UINT WINAPI SetTimer(HWND hWnd, UINT nIDEvent, UINT uElapse, TIMERPROC lpTimerFunc);
WINUSERAPI BOOL WINAPI KillTimer(HWND hWnd, UINT uIDEvent);
WINUSERAPI BOOL WINAPI RemoveMenu(HMENU hMenu, UINT uPosition, UINT uFlags);
!#

#! (:library "GDI32" :ignore "WINUSERAPI" :export t :pascal "WINAPI")
WINUSERAPI BOOL WINAPI Ellipse(HDC hDC, int left, int top, int right, int bottom);
WINUSERAPI HGDIOBJ WINAPI SelectObject(HDC hdc, HGDIOBJ hgdiobj); 
!#

;; int DrawText(HDC hDC, LPCTSTR lpString, int nCount, LPRECT lpRect, UINT uFormat); 
(defwinapi DrawText
	((hDC HDC)
	 (lpString LPCTSTR)
	 (nCount int)
	 (lpRect LPRECT)
	 (uFormat UINT))
	:return-type int
	:library-name "User32.dll"
	:entry-name "DrawTextA"
	:linkage-type :pascal)

;; ATOM RegisterClassEx(CONST WNDCLASSEX *lpwcx);
(defwinapi RegisterClassEx
	((lpwcx WNDCLASSEX))
	:return-type DWORD #| ATOM |# ;; it appears we are really getting back a 32-bit value
	:library-name "User32.dll"
	:entry-name "RegisterClassExA"
	:linkage-type :pascal)

;; BOOL GetMessage(LPMSG lpMsg, HWND hWnd, UINT wMsgFilterMin, UINT wMsgFilterMax); 
(defwinapi GetMessage
	((lpMsg LPMSG)
	 (hWnd HWND)
	 (wMsgFilterMin UINT)
	 (wMsgFilterMax UINT))
	:return-type BOOL
	:library-name "User32.dll"
	:entry-name "GetMessageA"
	:linkage-type :pascal)

;; BOOL TranslateMessage(CONST MSG* lpMsg); 
(defwinapi TranslateMessage
	((lpMsg MSG))
	:return-type BOOL
	:library-name "User32.dll"
	:entry-name "TranslateMessage"
	:linkage-type :pascal)

;; BOOL PostMessage(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam); 
(defwinapi PostMessage
	((hWnd HWND)
     (Msg UINT)
     (wParam WPARAM)
     (lParam LPARAM))
	:return-type BOOL
	:library-name "User32.dll"
	:entry-name "PostMessageA"
	:linkage-type :pascal)

;; LRESULT SendMessage(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam); 
(defwinapi SendMessage
	((hWnd HWND)
     (Msg UINT)
     (wParam WPARAM)
     (lParam LPARAM))
	:return-type LRESULT
	:library-name "User32.dll"
	:entry-name "SendMessageA"
	:linkage-type :pascal)

;; LONG DispatchMessage(CONST MSG* lpMsg); 
(defwinapi DispatchMessage
	((lpMsg MSG))
	:return-type LONG
	:library-name "User32.dll"
	:entry-name "DispatchMessageA"
	:linkage-type :pascal)

;; HICON LoadIcon(HINSTANCE hInstance, LPCTSTR lpIconName);
(defwinapi LoadIcon
	((hInstance HINSTANCE)
	 (lpIconName LPCTSTR))
	:return-type HICON
	:library-name "User32.dll"
	:entry-name "LoadIconA"
	:linkage-type :pascal)

;; HCURSOR LoadCursor(HINSTANCE hInstance, LPCTSTR lpCursorName);
(defwinapi LoadCursor
	((hInstance HINSTANCE)
	 (lpIconName LPCTSTR))
	:return-type HCURSOR
	:library-name "User32.dll"
	:entry-name "LoadCursorA"
	:linkage-type :pascal)

;; HGDIOBJ GetStockObject(int fnObject);
(defwinapi GetStockObject
	((fnObject int))
	:return-type HGDIOBJ
	:library-name "gdi32.dll"
	:entry-name "GetStockObject"
	:linkage-type :pascal)

;;; constants for use with GetStdHandle
(defwinconstant STD_INPUT_HANDLE  #xfffffff6) ;; Standard input handle 
(defwinconstant STD_OUTPUT_HANDLE #xfffffff5) ;; Standard output handle 
(defwinconstant STD_ERROR_HANDLE  #xfffffff4) ;; Standard error handle

(defwinconstant size-of-critical-section 24)	;; based on structure in winnt.h

#! (:library "Kernel32" :ignore "WINUSERAPI" :export t :pascal "WINAPI")
BOOL WINAPI CloseHandle(HANDLE hObject);
HANDLE WINAPI GetStdHandle(DWORD nStdHandle); 
BOOL WINAPI SetConsoleScreenBufferSize(HANDLE hConsoleOutput, COORD dwSize); 
BOOL WINAPI SetConsoleTitleA(LPCTSTR lpConsoleTitle); 
DWORD WINAPI GetFileSize(HANDLE hFile, LPDWORD lpFileSizeHigh); 
BOOL WINAPI GetFileTime(HANDLE hFile, LPFILETIME lpCreationTime, LPFILETIME lpLastAccessTime, LPFILETIME lpLastWriteTime);
BOOL WINAPI SetFileTime(HANDLE hFile, CONST FILETIME *lpCreationTime, CONST FILETIME *lpLastAccessTime, CONST FILETIME *lpLastWriteTime);
BOOL WINAPI FileTimeToSystemTime(CONST FILETIME *lpFileTime, LPSYSTEMTIME lpSystemTime);
BOOL WINAPI SystemTimeToFileTime(CONST SYSTEMTIME *lpSystemTime, LPFILETIME lpFileTime);
BOOL WINAPI IsBadReadPtr(CONST VOID *lp, UINT ucb);
BOOL WINAPI IsBadWritePtr(LPVOID lp, UINT ucb);
VOID WINAPI Sleep(DWORD dwMilliseconds);
BOOL WINAPI DeleteFileA(LPCSTR lpFileName);
VOID WINAPI ExitProcess(UINT uExitCode);
DWORD WINAPI ResumeThread(HANDLE);		
DWORD WINAPI SuspendThread(HANDLE);
BOOL  WINAPI TerminateThread(HANDLE,DWORD);
VOID  WINAPI InitializeCriticalSection(LPCRITICAL_SECTION lpCriticalSection);
VOID  WINAPI EnterCriticalSection(LPCRITICAL_SECTION lpCriticalSection);
VOID  WINAPI LeaveCriticalSection(LPCRITICAL_SECTION lpCriticalSection);
VOID  WINAPI DeleteCriticalSection(LPCRITICAL_SECTION lpCriticalSection);
BOOL WINAPI WriteFile(HANDLE hFile, LPCVOID lpBuffer, DWORD nNumberOfBytesToWrite, LPDWORD lpNumberOfBytesWritten, LPOVERLAPPED lpOverlapped);
BOOL WINAPI ReadFile(HANDLE hFile, LPVOID lpBuffer, DWORD nNumberOfBytesToRead, LPDWORD lpNumberOfBytesRead, LPOVERLAPPED lpOverlapped);
LPSTR WINAPI GetCommandLineA();
DWORD WINAPI GetFileAttributesA(LPCSTR lpFileName);
BOOL WINAPI SetFileAttributesA(LPCSTR lpFileName, DWORD dwFileAttributes);
DWORD WINAPI GetTimeZoneInformation(LPTIME_ZONE_INFORMATION lpTimeZoneInformation);
LPVOID WINAPI GetProcAddress(HANDLE module, LPCSTR procName);
!#

#! (:library "user32" :ignore "WINUSERAPI" :export t  :pascal "WINAPI")
BOOL WINAPI SetWindowTextA(HWND hWnd, LPCSTR lpString);
LRESULT WINAPI DefWindowProcA(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam);
LRESULT WINAPI DefFrameProcA(HWND hWnd, HWND hWndMDIClient, UINT uMsg, WPARAM wParam, LPARAM lParam);
LRESULT WINAPI DefMDIChildProcA(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
!#

;;;
;;;	HANDLE GetCurrentProcess()
;;;
(defwinapi GetCurrentProcess ()
   :return-type HANDLE
   :library-name "kernel32.dll"
   :entry-name "GetCurrentProcess"
   :linkage-type :pascal)

;;;
;;;	DWORD GetCurrentProcessId()
;;;
(defwinapi GetCurrentProcessId ()
   :return-type DWORD
   :library-name "kernel32.dll"
   :entry-name "GetCurrentProcessId"
   :linkage-type :pascal)

;;;
;;;	HANDLE GetCurrentThread()
;;;
(defwinapi GetCurrentThread ()
   :return-type HANDLE
   :library-name "kernel32.dll"
   :entry-name "GetCurrentThread"
   :linkage-type :pascal)

;;;
;;;	DWORD GetCurrentThreadId()
;;;
(defwinapi GetCurrentThreadId ()
   :return-type DWORD
   :library-name "kernel32.dll"
   :entry-name "GetCurrentThreadId"
   :linkage-type :pascal)

;;;
;;; VOID WINAPI Sleep(DWORD dwMilliseconds);
;;;
(defwinapi %Sleep ((milliseconds DWORD))
   :return-type :void
   :library-name "kernel32.dll"
   :entry-name "Sleep"
   :linkage-type :pascal)

;;
;; MessageBox() Flags
;;
(defwinconstant MB_OK                       #x00000000)
(defwinconstant MB_OKCANCEL                 #x00000001)
(defwinconstant MB_ABORTRETRYIGNORE         #x00000002)
(defwinconstant MB_YESNOCANCEL              #x00000003)
(defwinconstant MB_YESNO                    #x00000004)
(defwinconstant MB_RETRYCANCEL              #x00000005)


(defwinconstant MB_ICONHAND                 #x00000010)
(defwinconstant MB_ICONQUESTION             #x00000020)
(defwinconstant MB_ICONEXCLAMATION          #x00000030)
(defwinconstant MB_ICONASTERISK             #x00000040)
(defwinconstant MB_USERICON                 #x00000080)
(defwinconstant MB_ICONWARNING              MB_ICONEXCLAMATION)
(defwinconstant MB_ICONERROR                MB_ICONHAND)


(defwinconstant MB_ICONINFORMATION          MB_ICONASTERISK)
(defwinconstant MB_ICONSTOP                 MB_ICONHAND)

(defwinconstant MB_DEFBUTTON1               #x00000000)
(defwinconstant MB_DEFBUTTON2               #x00000100)
(defwinconstant MB_DEFBUTTON3               #x00000200)
(defwinconstant MB_DEFBUTTON4               #x00000300)

(defwinconstant MB_APPLMODAL                #x00000000)
(defwinconstant MB_SYSTEMMODAL              #x00001000)
(defwinconstant MB_TASKMODAL                #x00002000)
(defwinconstant MB_HELP                     #x00004000)

(defwinconstant MB_NOFOCUS                  #x00008000)
(defwinconstant MB_SETFOREGROUND            #x00010000)
(defwinconstant MB_DEFAULT_DESKTOP_ONLY     #x00020000)

(defwinconstant MB_TOPMOST                  #x00040000)
(defwinconstant MB_RIGHT                    #x00080000)
(defwinconstant MB_RTLREADING               #x00100000)

(defwinconstant MB_SERVICE_NOTIFICATION     #x00200000)
(defwinconstant MB_SERVICE_NOTIFICATION_NT3X #x00040000)

(defwinconstant MB_TYPEMASK                 #x0000000F)
(defwinconstant MB_ICONMASK                 #x000000F0)
(defwinconstant MB_DEFMASK                  #x00000F00)
(defwinconstant MB_MODEMASK                 #x00003000)
(defwinconstant MB_MISCMASK                 #x0000C000)

;; int MessageBox(HWND hWnd, LPCTSTR lpText, LPCTSTR lpCaption, UINT uType);
(defwinapi MessageBox 
	((hWnd HWND)
	 (lpText LPCTSTR)
	 (lpCaption LPCTSTR)
	 (uType UINT))
   :return-type int
   :library-name "user32.dll"
   :entry-name "MessageBoxA"
   :linkage-type :pascal)

(defun dialog-id-name (id)
	(cond ((= id win:IDOK) 		'win:IDOK)
		((= id win:IDCANCEL) 	'win:IDCANCEL)
		((= id win:IDABORT) 	'win:IDABORT)
		((= id win:IDRETRY) 	'win:IDRETRY)
		((= id win:IDIGNORE) 	'win:IDIGNORE)
		((= id win:IDYES) 		'win:IDYES)
		((= id win:IDNO) 		'win:IDNO)
		((= id win:IDCLOSE) 	'win:IDCLOSE)
		((= id win:IDHELP) 		'win:IDHELP)))

(defun message-box-ok (message &optional caption)
	(dialog-id-name
		(messagebox (cl::get-application-main-window)
			(ct:create-c-string	message)
			(if caption (ct:create-c-string caption) cl::c_null)
			MB_OK)))

(defun message-box-ok-cancel (message &optional caption)
	(dialog-id-name
		(messagebox (cl::get-application-main-window)
			(ct:create-c-string	message)
			(if caption (ct:create-c-string caption) cl::c_null)
			MB_OKCANCEL)))

(defun message-box-yes-no (message &optional caption)
	(dialog-id-name
		(messagebox (cl::get-application-main-window)
			(ct:create-c-string	message)
			(if caption (ct:create-c-string caption) cl::c_null)
			MB_YESNO)))

(defun message-box-yes-no-cancel (message &optional caption)
	(dialog-id-name
		(messagebox (cl::get-application-main-window)
			(ct:create-c-string	message)
			(if caption (ct:create-c-string caption) cl::c_null)
			MB_YESNOCANCEL)))

;;
;; GetWindow() Constants
;;
(defwinconstant GW_HWNDFIRST        0)
(defwinconstant GW_HWNDLAST         1)
(defwinconstant GW_HWNDNEXT         2)
(defwinconstant GW_HWNDPREV         3)
(defwinconstant GW_OWNER            4)
(defwinconstant GW_CHILD            5)
(defwinconstant GW_MAX              5)

;; this is actually a synonym for GetWindow() in Win32
;; HWND GetNextWindow(HWND hWnd, UINT wCmd);
(defwinapi GetNextWindow 
	((hWnd HWND)
	 (wCmd UINT))
   :return-type HWND
   :library-name "user32.dll"
   :entry-name "GetWindow"
   :linkage-type :pascal)

;; BOOL TextOut(HDC hdc, int nXStart, int nYStart, LPCTSTR lpString, int cbString);
(defwinapi TextOut 
	((hdc HDC)
	 (nXStart int)
	 (nYStart int)
	 (lpString LPCTSTR)
	 (cbString int))
   :return-type BOOL
   :library-name "gdi32.dll"
   :entry-name "TextOutA"
   :linkage-type :pascal)

 
(defstruct rect left top right bottom)
(defun get-window-rect (hwnd)
	(let ((crect (ct:malloc (sizeof 'RECT)))
		  (rect (make-rect)))
		(if (GetWindowRect hwnd crect)
			(progn
				(setf (rect-left rect)(cref RECT crect left))
				(setf (rect-top rect)(cref RECT crect top))
				(setf (rect-right rect)(cref RECT crect right))
				(setf (rect-bottom rect)(cref RECT crect bottom))
				rect))))

;; int GetWindowText(HWND hWnd, LPTSTR lpString, int nMaxCount);
(defwinapi GetWindowText 
	((hWnd HWND)
	 (lpString LPTSTR)
	 (nMaxCount int))
   :return-type int
   :library-name "user32.dll"
   :entry-name "GetWindowTextA"
   :linkage-type :pascal)

(defun get-window-text (hwnd &optional (max-length 256))
	(let ((cstr (ct:malloc max-length)))
		(unless (zerop (GetWindowText hwnd cstr max-length))
			(ct:c-string-to-lisp-string cstr))))

;; HWND FindWindow(LPCTSTR lpClassName, LPCTSTR lpWindowName);
(defwinapi FindWindow 
	((lpClassName LPCTSTR )
	 (lpWindowName LPCTSTR))
   :return-type HWND
   :library-name "user32.dll"
   :entry-name "FindWindowA"
   :linkage-type :pascal)

;; HWND FindWindowEx(HWND hwndParent, HWND hwndChildAfter, LPCTSTR lpszClass, 
;;		LPCTSTR lpszWindow);
(defwinapi FindWindowEx 
	((hwndParent HWND)
	 (hwndChildAfter HWND)
	 (lpClassName LPCTSTR)
	 (lpWindowName LPCTSTR))
   :return-type HWND
   :library-name "user32.dll"
   :entry-name "FindWindowExA"
   :linkage-type :pascal)
 
;;	DWORD GetLastError();
(defwinapi GetLastError
	()
   :return-type DWORD
   :library-name "kernel32.dll"
   :entry-name "GetLastError"
   :linkage-type :pascal)

(defwinconstant SND_SYNC            #x0000)  	;; play synchronously (default)
(defwinconstant SND_ASYNC           #x0001)  	;; play asynchronously
(defwinconstant SND_NODEFAULT       #x0002)  	;; silence (!default) if sound not found
(defwinconstant SND_MEMORY          #x0004)  	;; pszSound points to a memory file
(defwinconstant SND_LOOP            #x0008)		;; loop the sound until next sndPlaySound
(defwinconstant SND_NOSTOP          #x0010)		;; don't stop any currently playing sound
(defwinconstant SND_NOWAIT			#x00002000);; don't wait if the driver is busy
(defwinconstant SND_ALIAS       	#x00010000);; name is a registry alias
(defwinconstant SND_ALIAS_ID	 	#x00110000);; alias is a predefined ID
(defwinconstant SND_FILENAME    	#x00020000);; name is file name
(defwinconstant SND_RESOURCE    	#x00040004);; name is resource name or atom
(defwinconstant SND_PURGE           #x0040)	;; purge non-static events for task
(defwinconstant SND_APPLICATION     #x0080)  	;; look for application specific association

;; BOOL PlaySound(LPCSTR pszSound, HMODULE hmod, DWORD fdwSound);
(defwinapi PlaySound
	((pszSound LPCSTR)(hmod HMODULE)(fdwSound DWORD))
	:return-type BOOL
	:library-name "winmm.dll"
	:entry-name "PlaySoundA"
	:linkage-type :pascal)

;; typedef BOOL (CALLBACK* WNDENUMPROC)(HWND, LPARAM);
(defvar *wnd-enum-proc* nil)

(ct:defun-callback WndEnumProc ((hwnd HWND)(lParam LPARAM))
	(funcall *wnd-enum-proc* hwnd lParam)
	t)

;; LRESULT SendMessage(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam);
(defwinapi SendMessage 
	((hWnd HWND )
	 (Msg UINT)
	 (wParam WPARAM)
	 (lParam LPARAM))
   :return-type LRESULT
   :library-name "user32.dll"
   :entry-name "SendMessageA"
   :linkage-type :pascal)

(defwinconstant WM_USER                 #x0400)

;;;
;;; Edit Control Messages
;;;
(defwinconstant EM_GETSEL               #x00B0)
(defwinconstant EM_SETSEL               #x00B1)
(defwinconstant EM_GETRECT              #x00B2)
(defwinconstant EM_SETRECT              #x00B3)
(defwinconstant EM_SETRECTNP            #x00B4)
(defwinconstant EM_SCROLL               #x00B5)
(defwinconstant EM_LINESCROLL           #x00B6)
(defwinconstant EM_SCROLLCARET          #x00B7)
(defwinconstant EM_GETMODIFY            #x00B8)
(defwinconstant EM_SETMODIFY            #x00B9)
(defwinconstant EM_GETLINECOUNT         #x00BA)
(defwinconstant EM_LINEINDEX            #x00BB)
(defwinconstant EM_SETHANDLE            #x00BC)
(defwinconstant EM_GETHANDLE            #x00BD)
(defwinconstant EM_GETTHUMB             #x00BE)
(defwinconstant EM_LINELENGTH           #x00C1)
(defwinconstant EM_REPLACESEL           #x00C2)
(defwinconstant EM_GETLINE              #x00C4)
(defwinconstant EM_LIMITTEXT            #x00C5)
(defwinconstant EM_CANUNDO              #x00C6)
(defwinconstant EM_UNDO                 #x00C7)
(defwinconstant EM_FMTLINES             #x00C8)
(defwinconstant EM_LINEFROMCHAR         #x00C9)
(defwinconstant EM_SETTABSTOPS          #x00CB)
(defwinconstant EM_SETPASSWORDCHAR      #x00CC)
(defwinconstant EM_EMPTYUNDOBUFFER      #x00CD)
(defwinconstant EM_GETFIRSTVISIBLELINE  #x00CE)
(defwinconstant EM_SETREADONLY          #x00CF)
(defwinconstant EM_SETWORDBREAKPROC     #x00D0)
(defwinconstant EM_GETWORDBREAKPROC     #x00D1)
(defwinconstant EM_GETPASSWORDCHAR      #x00D2)
(defwinconstant EM_SETMARGINS           #x00D3)
(defwinconstant EM_GETMARGINS           #x00D4)
(defwinconstant EM_SETLIMITTEXT         EM_LIMITTEXT)
(defwinconstant EM_GETLIMITTEXT         #x00D5)
(defwinconstant EM_POSFROMCHAR          #x00D6)
(defwinconstant EM_CHARFROMPOS          #x00D7)

(defwinconstant EM_SETFONTSIZE          (+ WM_USER #x00DF))
(defwinconstant EM_GETZOOM              (+ WM_USER #x00E0))
(defwinconstant EM_SETZOOM              (+ WM_USER #x00E1))

;; RichEdit messages
;;(defwinconstant EM_GETLIMITTEXT			(+ WM_USER 37))
;;(defwinconstant EM_POSFROMCHAR			(+ WM_USER 38))
;;(defwinconstant EM_CHARFROMPOS			(+ WM_USER 39))
;;(defwinconstant EM_SCROLLCARET			(+ WM_USER 49))
(defwinconstant EM_CANPASTE				(+ WM_USER 50))
(defwinconstant EM_DISPLAYBAND			(+ WM_USER 51))
(defwinconstant EM_EXGETSEL				(+ WM_USER 52))
(defwinconstant EM_EXLIMITTEXT			(+ WM_USER 53))
(defwinconstant EM_EXLINEFROMCHAR		(+ WM_USER 54))
(defwinconstant EM_EXSETSEL				(+ WM_USER 55))
(defwinconstant EM_FINDTEXT				(+ WM_USER 56))
(defwinconstant EM_FORMATRANGE			(+ WM_USER 57))
(defwinconstant EM_GETCHARFORMAT		(+ WM_USER 58))
(defwinconstant EM_GETEVENTMASK			(+ WM_USER 59))
(defwinconstant EM_GETOLEINTERFACE		(+ WM_USER 60))
(defwinconstant EM_GETPARAFORMAT		(+ WM_USER 61))
(defwinconstant EM_GETSELTEXT			(+ WM_USER 62))
(defwinconstant EM_HIDESELECTION		(+ WM_USER 63))
(defwinconstant EM_PASTESPECIAL			(+ WM_USER 64))
(defwinconstant EM_REQUESTRESIZE		(+ WM_USER 65))
(defwinconstant EM_SELECTIONTYPE		(+ WM_USER 66))
(defwinconstant EM_SETBKGNDCOLOR		(+ WM_USER 67))
(defwinconstant EM_SETCHARFORMAT		(+ WM_USER 68))
(defwinconstant EM_SETEVENTMASK			(+ WM_USER 69))
(defwinconstant EM_SETOLECALLBACK		(+ WM_USER 70))
(defwinconstant EM_SETPARAFORMAT		(+ WM_USER 71))
(defwinconstant EM_SETTARGETDEVICE		(+ WM_USER 72))
(defwinconstant EM_STREAMIN				(+ WM_USER 73))
(defwinconstant EM_STREAMOUT			(+ WM_USER 74))
(defwinconstant EM_GETTEXTRANGE			(+ WM_USER 75))
(defwinconstant EM_FINDWORDBREAK		(+ WM_USER 76))
(defwinconstant EM_SETOPTIONS			(+ WM_USER 77))
(defwinconstant EM_GETOPTIONS			(+ WM_USER 78))
(defwinconstant EM_FINDTEXTEX			(+ WM_USER 79))
(defwinconstant EM_GETWORDBREAKPROCEX	(+ WM_USER 80))
(defwinconstant EM_SETWORDBREAKPROCEX	(+ WM_USER 81))

;; Richedit v2.0 messages
(defwinconstant EM_SETUNDOLIMIT			(+ WM_USER 82))
(defwinconstant EM_REDO					(+ WM_USER 84))
(defwinconstant EM_CANREDO				(+ WM_USER 85))
(defwinconstant EM_GETUNDONAME			(+ WM_USER 86))
(defwinconstant EM_GETREDONAME			(+ WM_USER 87))
(defwinconstant EM_STOPGROUPTYPING		(+ WM_USER 88))
(defwinconstant EM_SETTEXTMODE			(+ WM_USER 89))
(defwinconstant EM_GETTEXTMODE			(+ WM_USER 90))

;; enum for use with EM_GET/SETTEXTMODE
(defwinconstant TM_PLAINTEXT			1)
(defwinconstant TM_RICHTEXT				2)
(defwinconstant TM_SINGLELEVELUNDO		4)
(defwinconstant TM_MULTILEVELUNDO		8)
(defwinconstant TM_SINGLECODEPAGE		16)
(defwinconstant TM_MULTICODEPAGE		32)

(defwinconstant EM_AUTOURLDETECT		(+ WM_USER 91))
(defwinconstant EM_GETAUTOURLDETECT		(+ WM_USER 92))
(defwinconstant EM_SETPALETTE			(+ WM_USER 93))
(defwinconstant EM_GETTEXTEX			(+ WM_USER 94))
(defwinconstant EM_GETTEXTLENGTHEX		(+ WM_USER 95))

;; Far East specific messages
(defwinconstant EM_SETPUNCTUATION		(+ WM_USER 100))
(defwinconstant EM_GETPUNCTUATION		(+ WM_USER 101))
(defwinconstant EM_SETWORDWRAPMODE		(+ WM_USER 102))
(defwinconstant EM_GETWORDWRAPMODE		(+ WM_USER 103))
(defwinconstant EM_SETIMECOLOR			(+ WM_USER 104))
(defwinconstant EM_GETIMECOLOR			(+ WM_USER 105))
(defwinconstant EM_SETIMEOPTIONS		(+ WM_USER 106))
(defwinconstant EM_GETIMEOPTIONS		(+ WM_USER 107))
(defwinconstant EM_CONVPOSITION 		(+ WM_USER 108))
(defwinconstant EM_SETLANGOPTIONS		(+ WM_USER 120))
(defwinconstant EM_GETLANGOPTIONS		(+ WM_USER 121))
(defwinconstant EM_GETIMECOMPMODE		(+ WM_USER 122))

;; Options for EM_SETLANGOPTIONS and EM_GETLANGOPTIONS
(defwinconstant IMF_AUTOKEYBOARD		#x0001)
(defwinconstant IMF_AUTOFONT			#x0002)
(defwinconstant IMF_IMECANCELCOMPLETE	#x0004)	;; high completes the comp string when aborting, 
												;;low cancels.
(defwinconstant IMF_IMEALWAYSSENDNOTIFY #x0008)

;; Values for EM_GETIMECOMPMODE
(defwinconstant ICM_NOTOPEN				#x0000)
(defwinconstant ICM_LEVEL3				#x0001)
(defwinconstant ICM_LEVEL2				#x0002)
(defwinconstant ICM_LEVEL2_5			#x0003)
(defwinconstant ICM_LEVEL2_SUI			#x0004)

(defwinstruct CHARRANGE
  ((cpMin             LONG)
   (cpMax             LONG)))

;; BOOL GetTextMetrics(HDC hdc, LPTEXTMETRIC lptm);
(defwinapi GetTextMetrics ((hWnd HWND)(hDC HDC))
   :return-type BOOL
   :library-name "gdi32.dll"
   :entry-name "GetTextMetricsA"
   :linkage-type :pascal)

;; UINT SetTextAlign(HDC hdc, UINT fMode);
(defwinapi SetTextAlign ((hdc HDC)(fMode UINT))
   :return-type UINT
   :library-name "gdi32.dll"
   :entry-name "SetTextAlign"
   :linkage-type :pascal)

;; BOOL TextOut(HDC hdc, int nXStart, int nYStart, LPCTSTR lpString, int cbString);
(defwinapi TextOut 
	((hdc HDC)
	 (nXStart int)
	 (nYStart int)
	 (lpString LPCTSTR)
	 (cbString int))
   :return-type BOOL
   :library-name "gdi32.dll"
   :entry-name "TextOutA"
   :linkage-type :pascal)

;; int GetDeviceCaps(HDC hdc, int nIndex); 
(defwinapi GetDeviceCaps 
	((hdc HDC)
	 (nIndex int))
   :return-type int
   :library-name "gdi32.dll"
   :entry-name "GetDeviceCaps"
   :linkage-type :pascal)

(defwinstruct TEXTMETRIC
  (
	(tmHeight			LONG)
	(tmAscent           LONG)
	(tmDescent          LONG)
	(tmInternalLeading  LONG)
	(tmExternalLeading  LONG)
	(tmAveCharWidth     LONG)
	(tmMaxCharWidth     LONG)
	(tmWeight           LONG)
	(tmOverhang         LONG)
	(tmDigitizedAspectX LONG)
	(tmDigitizedAspectY LONG)
 	(tmFirstChar        BYTE)
	(tmLastChar         BYTE)
	(tmDefaultChar      BYTE)
 	(tmBreakChar        BYTE)
	(tmItalic           BYTE)
	(tmUnderlined       BYTE)
	(tmStruckOut        BYTE)
	(tmPitchAndFamily   BYTE)
	(tmCharSet          BYTE)
))

;; Device Parameters for GetDeviceCaps()
(defwinconstant DRIVERVERSION 	0)     ;; Device driver version
(defwinconstant TECHNOLOGY    	2)     ;; Device classification
(defwinconstant HORZSIZE      	4)     ;; Horizontal size in millimeters
(defwinconstant VERTSIZE      	6)     ;; Vertical size in millimeters
(defwinconstant HORZRES       	8)     ;; Horizontal width in pixels
(defwinconstant VERTRES       	10)    ;; Vertical height in pixels  
(defwinconstant BITSPIXEL     	12)    ;; Number of bits per pixel      
(defwinconstant PLANES        	14)    ;; Number of planes
(defwinconstant NUMBRUSHES    	16)    ;; Number of brushes the device has
(defwinconstant NUMPENS       	18)    ;; Number of pens the device has
(defwinconstant NUMMARKERS    	20)    ;; Number of markers the device has
(defwinconstant NUMFONTS      	22)    ;; Number of fonts the device has
(defwinconstant NUMCOLORS     	24)    ;; Number of colors the device supports
(defwinconstant PDEVICESIZE   	26)    ;; Size required for device descriptor
(defwinconstant CURVECAPS     	28)    ;; Curve capabilities
(defwinconstant LINECAPS      	30)    ;; Line capabilities
(defwinconstant POLYGONALCAPS 	32)    ;; Polygonal capabilities
(defwinconstant TEXTCAPS      	34)    ;; Text capabilities
(defwinconstant CLIPCAPS      	36)    ;; Clipping capabilities
(defwinconstant RASTERCAPS    	38)    ;; Bitblt capabilities
(defwinconstant ASPECTX       	40)    ;; Length of the X leg
(defwinconstant ASPECTY       	42)    ;; Length of the Y leg
(defwinconstant ASPECTXY      	44)    ;; Length of the hypotenuse
(defwinconstant LOGPIXELSX    	88)    ;; Logical pixels/inch in X
(defwinconstant LOGPIXELSY    	90)    ;; Logical pixels/inch in Y
(defwinconstant SIZEPALETTE  	104)   ;; Number of entries in physical palette
(defwinconstant NUMRESERVED  	106)   ;; Number of reserved entries in palette
(defwinconstant COLORRES     	108)   ;; Actual color resolution 

;; Printing related DeviceCaps. These replace the appropriate Escapes

(defwinconstant PHYSICALWIDTH   110) ;; Physical Width in device units
(defwinconstant PHYSICALHEIGHT  111) ;; Physical Height in device units
(defwinconstant PHYSICALOFFSETX 112) ;; Physical Printable Area x margin
(defwinconstant PHYSICALOFFSETY 113) ;; Physical Printable Area y margin
(defwinconstant SCALINGFACTORX  114) ;; Scaling factor x
(defwinconstant SCALINGFACTORY  115) ;; Scaling factor y

;; Text Alignment Options
(defwinconstant TA_NOUPDATECP                0)
(defwinconstant TA_UPDATECP                  1)
(defwinconstant TA_LEFT                      0)
(defwinconstant TA_RIGHT                     2)
(defwinconstant TA_CENTER                    6)

(defwinconstant TA_TOP                       0)
(defwinconstant TA_BOTTOM                    8)
(defwinconstant TA_BASELINE                  24)
(defwinconstant TA_RTLREADING                256)
(defwinconstant TA_MASK       (+ TA_BASELINE TA_CENTER TA_UPDATECP TA_RTLREADING))

;; Display driver specific

(defwinconstant VREFRESH        116)  ;; Current vertical refresh rate of the
                             		  ;; display device (for displays only) in Hz
(defwinconstant DESKTOPVERTRES  117)  ;; Horizontal width of entire desktop in pixels                
(defwinconstant DESKTOPHORZRES  118)  ;; Vertical height of entire desktop in pixels
(defwinconstant BLTALIGNMENT    119) ;; Preferred blt alignment

;;
;; GetSystemMetrics() codes
;;
(defwinconstant SM_CXSCREEN             0)
(defwinconstant SM_CYSCREEN             1)
(defwinconstant SM_CXVSCROLL            2)
(defwinconstant SM_CYHSCROLL            3)
(defwinconstant SM_CYCAPTION            4)
(defwinconstant SM_CXBORDER             5)
(defwinconstant SM_CYBORDER             6)
(defwinconstant SM_CXDLGFRAME           7)
(defwinconstant SM_CYDLGFRAME           8)
(defwinconstant SM_CYVTHUMB             9)
(defwinconstant SM_CXHTHUMB             10)
(defwinconstant SM_CXICON               11)
(defwinconstant SM_CYICON               12)
(defwinconstant SM_CXCURSOR             13)
(defwinconstant SM_CYCURSOR             14)
(defwinconstant SM_CYMENU               15)
(defwinconstant SM_CXFULLSCREEN         16)
(defwinconstant SM_CYFULLSCREEN         17)
(defwinconstant SM_CYKANJIWINDOW        18)
(defwinconstant SM_MOUSEPRESENT         19)
(defwinconstant SM_CYVSCROLL            20)
(defwinconstant SM_CXHSCROLL            21)
(defwinconstant SM_DEBUG                22)
(defwinconstant SM_SWAPBUTTON           23)
(defwinconstant SM_RESERVED1            24)
(defwinconstant SM_RESERVED2            25)
(defwinconstant SM_RESERVED3            26)
(defwinconstant SM_RESERVED4            27)
(defwinconstant SM_CXMIN                28)
(defwinconstant SM_CYMIN                29)
(defwinconstant SM_CXSIZE               30)
(defwinconstant SM_CYSIZE               31)
(defwinconstant SM_CXFRAME              32)
(defwinconstant SM_CYFRAME              33)
(defwinconstant SM_CXMINTRACK           34)
(defwinconstant SM_CYMINTRACK           35)
(defwinconstant SM_CXDOUBLECLK          36)
(defwinconstant SM_CYDOUBLECLK          37)
(defwinconstant SM_CXICONSPACING        38)
(defwinconstant SM_CYICONSPACING        39)
(defwinconstant SM_MENUDROPALIGNMENT    40)
(defwinconstant SM_PENWINDOWS           41)
(defwinconstant SM_DBCSENABLED          42)
(defwinconstant SM_CMOUSEBUTTONS        43)
(defwinconstant SM_CXFIXEDFRAME SM_CXDLGFRAME)
(defwinconstant SM_CYFIXEDFRAME SM_CYDLGFRAME)
(defwinconstant SM_CXSIZEFRAME  SM_CXFRAME)
(defwinconstant SM_CYSIZEFRAME  SM_CYFRAME)
(defwinconstant SM_SECURE               44)
(defwinconstant SM_CXEDGE               45)
(defwinconstant SM_CYEDGE               46)
(defwinconstant SM_CXMINSPACING         47)
(defwinconstant SM_CYMINSPACING         48)
(defwinconstant SM_CXSMICON             49)
(defwinconstant SM_CYSMICON             50)
(defwinconstant SM_CYSMCAPTION          51)
(defwinconstant SM_CXSMSIZE             52)
(defwinconstant SM_CYSMSIZE             53)
(defwinconstant SM_CXMENUSIZE           54)
(defwinconstant SM_CYMENUSIZE           55)
(defwinconstant SM_ARRANGE              56)
(defwinconstant SM_CXMINIMIZED          57)
(defwinconstant SM_CYMINIMIZED          58)
(defwinconstant SM_CXMAXTRACK           59)
(defwinconstant SM_CYMAXTRACK           60)
(defwinconstant SM_CXMAXIMIZED          61)
(defwinconstant SM_CYMAXIMIZED          62)
(defwinconstant SM_NETWORK              63)
(defwinconstant SM_CLEANBOOT            67)
(defwinconstant SM_CXDRAG               68)
(defwinconstant SM_CYDRAG               69)
(defwinconstant SM_SHOWSOUNDS           70)
(defwinconstant SM_CXMENUCHECK          71)
(defwinconstant SM_CYMENUCHECK          72)
(defwinconstant SM_SLOWMACHINE          73)
(defwinconstant SM_MIDEASTENABLED       74)
(defwinconstant SM_MOUSEWHEELPRESENT    75)
(defwinconstant SM_CMETRICS             76)

;;
;;	Some windows definitions
;;

;;
;; Scroll Bar Constants
;;
(defwinconstant SB_HORZ             0)
(defwinconstant SB_VERT             1)
(defwinconstant SB_CTL              2)
(defwinconstant SB_BOTH             3)

;;
;; Scroll Bar Commands
;;
(defwinconstant SB_LINEUP           0)
(defwinconstant SB_LINELEFT         0)
(defwinconstant SB_LINEDOWN         1)
(defwinconstant SB_LINERIGHT        1)
(defwinconstant SB_PAGEUP           2)
(defwinconstant SB_PAGELEFT         2)
(defwinconstant SB_PAGEDOWN         3)
(defwinconstant SB_PAGERIGHT        3)
(defwinconstant SB_THUMBPOSITION    4)
(defwinconstant SB_THUMBTRACK       5)
(defwinconstant SB_TOP              6)
(defwinconstant SB_LEFT             6)
(defwinconstant SB_BOTTOM           7)
(defwinconstant SB_RIGHT            7)
(defwinconstant SB_ENDSCROLL        8)

;; BOOL Rectangle(HDC hdc, int nLeftRect, int nTopRect, int nRightRect, int nBottomRect);
(defwinapi Rectangle 
	((hDC HDC)
	 (nLeftRect int)
	 (nTopRect int)
	 (nRightRect int)
	 (nBottomRect int))
   :return-type BOOL
   :library-name "gdi32.dll"
   :entry-name "Rectangle"
   :linkage-type :pascal)
 
;; HBRUSH CreateSolidBrush(COLORREF crColor);
(defwinapi CreateSolidBrush 
	((crColor COLORREF))
   :return-type HBRUSH
   :library-name "gdi32.dll"
   :entry-name "CreateSolidBrush"
   :linkage-type :pascal)

;; BOOL DeleteObject(HGDIOBJ hObject);
(defwinapi DeleteObject 
	((hObject HGDIOBJ))
   :return-type BOOL
   :library-name "gdi32.dll"
   :entry-name "DeleteObject"
   :linkage-type :pascal)

;;; Windows Registry API

(defwinconstant HKEY_CLASSES_ROOT 		#x80000000)
(defwinconstant HKEY_CURRENT_USER 		#x80000001)
(defwinconstant HKEY_LOCAL_MACHINE 	#x80000002)
(defwinconstant HKEY_USERS 			#x80000003) 
(defwinconstant HKEY_PERFORMANCE_DATA  #x80000004)
(defwinconstant HKEY_CURRENT_CONFIG    #x80000005)
(defwinconstant HKEY_DYN_DATA          #x80000006)

;;;
;;;  The following are masks for the predefined standard access types
;;;
(defwinconstant DELETE                           #x00010000)
(defwinconstant READ_CONTROL                     #x00020000)
(defwinconstant WRITE_DAC                        #x00040000)
(defwinconstant WRITE_OWNER                      #x00080000)
(defwinconstant SYNCHRONIZE                      #x00100000)
(defwinconstant STANDARD_RIGHTS_REQUIRED         #x000F0000)
(defwinconstant STANDARD_RIGHTS_READ             READ_CONTROL)
(defwinconstant STANDARD_RIGHTS_WRITE            READ_CONTROL)
(defwinconstant STANDARD_RIGHTS_EXECUTE          READ_CONTROL)
(defwinconstant STANDARD_RIGHTS_ALL              #x001F0000)
(defwinconstant SPECIFIC_RIGHTS_ALL              #x0000FFFF)

;;;
;;; Registry Specific Access Rights.
;;;
(defwinconstant KEY_QUERY_VALUE         #x0001)
(defwinconstant KEY_SET_VALUE           #x0002)
(defwinconstant KEY_CREATE_SUB_KEY      #x0004)
(defwinconstant KEY_ENUMERATE_SUB_KEYS  #x0008)
(defwinconstant KEY_NOTIFY              #x0010)
(defwinconstant KEY_CREATE_LINK         #x0020)

(defwinconstant SYNCHRONIZE 			 #x00100000)

(defwinconstant KEY_READ (logand
						(logior 
							STANDARD_RIGHTS_READ
                            KEY_QUERY_VALUE
                            KEY_ENUMERATE_SUB_KEYS
                            KEY_NOTIFY)
                        (lognot SYNCHRONIZE)))


(defwinconstant KEY_WRITE (logand
							(logior
								STANDARD_RIGHTS_WRITE
                                KEY_SET_VALUE
                                KEY_CREATE_SUB_KEY)     
                        	(lognot SYNCHRONIZE)))

(defwinconstant KEY_EXECUTE	(logand
             					KEY_READ
                        		(lognot SYNCHRONIZE)))

(defwinconstant KEY_ALL_ACCESS  (logand
								(logior
       								STANDARD_RIGHTS_ALL
                                  	KEY_QUERY_VALUE
                                  	KEY_SET_VALUE
                                  	KEY_CREATE_SUB_KEY
                                  	KEY_ENUMERATE_SUB_KEYS
                                  	KEY_NOTIFY
                                  	KEY_CREATE_LINK)
                                 (lognot SYNCHRONIZE)))

#! (:library "advapi32" :export t :pascal "WINAPI")
#define REG_NONE                    ( 0 )   // No value type
#define REG_SZ                      ( 1 )   // Unicode nul terminated string
#define REG_EXPAND_SZ               ( 2 )   // Unicode nul terminated string
                                            // (with environment variable references)
#define REG_BINARY                  ( 3 )   // Free form binary
#define REG_DWORD                   ( 4 )   // 32-bit number
#define REG_DWORD_LITTLE_ENDIAN     ( 4 )   // 32-bit number (same as REG_DWORD)
#define REG_DWORD_BIG_ENDIAN        ( 5 )   // 32-bit number
#define REG_LINK                    ( 6 )   // Symbolic Link (unicode)
#define REG_MULTI_SZ                ( 7 )   // Multiple Unicode strings
#define REG_RESOURCE_LIST           ( 8 )   // Resource list in the resource map
#define REG_FULL_RESOURCE_DESCRIPTOR ( 9 )  // Resource list in the hardware description
#define REG_RESOURCE_REQUIREMENTS_LIST ( 10 )
#define REG_QWORD                   ( 11 )  // 64-bit number
#define REG_QWORD_LITTLE_ENDIAN     ( 11 )  // 64-bit number (same as REG_QWORD)
!#

(defwintype REGSAM ACCESS_MASK)

(defwinapi RegEnumKeyEx
	((hKey HKEY)
	 (dwIndex DWORD)
	 (lpName LPSTR)
	 (lpcbName LPDWORD)
	 (lpReserved LPDWORD)
	 (lpClass LPSTR)
	 (lpcbClass LPDWORD)
	 (lpftLastWriteTime PFILETIME))
   :return-type LONG
   :library-name "advapi32.dll"
   :entry-name "RegEnumKeyExA"
   :linkage-type :pascal)

(defwinapi RegOpenKeyEx
	((hKey HKEY)
     (lpSubKey LPCSTR)
     (ulOptions DWORD)
     (samDesired REGSAM)
     (phkResult PHKEY)) 
   :return-type LONG
   :library-name "advapi32.dll"
   :entry-name "RegOpenKeyExA"
   :linkage-type :pascal)

(defwinapi RegQueryValueEx
	((hKey HKEY)
	 (lpValueName LPCSTR)
	 (lpReserved LPDWORD)
	 (lpType LPDWORD)
	 (lpData LPBYTE)
	 (lpcbData LPDWORD))
   :return-type LONG
   :library-name "advapi32.dll"
   :entry-name "RegQueryValueExA"
   :linkage-type :pascal)

(defwinapi RegSetValueEx
	((hKey HKEY)
	 (lpValueName LPCSTR)
	 (Reserved DWORD)
	 (dwType DWORD)
	 (lpData LPBYTE)
	 (cbData DWORD))
   :return-type LONG
   :library-name "advapi32.dll"
   :entry-name "RegSetValueExA"
   :linkage-type :pascal)

(defwinapi OutputDebugString
	((lpOutputString LPCTSTR))
   :return-type VOID
   :library-name "kernel32.dll"
   :entry-name "OutputDebugStringA"
   :linkage-type :pascal)

(defwinapi CopyFile
	((lpExistingFileName LPCSTR)
	 (lpNewFileName LPCSTR)
	 (bFailIfExists BOOL))
   :return-type BOOL
   :library-name "kernel32.dll"
   :entry-name "CopyFileA"
   :linkage-type :pascal)

#! (:library "advapi32" :export t :pascal "WINAPI")
LONG WINAPI RegCloseKey(HKEY hKey);
#define UNLEN 256
BOOL WINAPI GetUserNameA(char* lpBuffer, LPDWORD nSize);
!#

#! (:library "Kernel32" :export t :pascal "WINAPI")
VOID WINAPI ExitThread(DWORD dwExitCode);
#define MAX_COMPUTERNAME_LENGTH 15
BOOL WINAPI GetComputerNameA(char* lpBuffer, LPDWORD nSize);
VOID WINAPI DebugBreak();
!#

(defconstant max-user-name-length UNLEN)
(defconstant max-computer-name-length MAX_COMPUTERNAME_LENGTH)
(defun get-user-name () 
	(let* ((buf (ct:malloc (+ max-user-name-length 1)))
		   (num (ct:malloc (ct:sizeof 'DWORD)))
		   ret)
		(setf (ct:cref (DWORD *) num 0) (+ max-user-name-length 1))
		(setf ret (GetUserName buf num))
		(if ret (ct:c-string-to-lisp-string buf))))

(defun get-computer-name () 
	(let* ((buf (ct:malloc (+ max-computer-name-length 1)))
		   (num (ct:malloc (ct:sizeof 'DWORD)))
		   ret)
		(setf (ct:cref (DWORD *) num 0) (+ max-computer-name-length 1))
		(setf ret (GetComputerName buf num))
		(if ret (ct:c-string-to-lisp-string buf))))

#|
(setf *wnd-enum-proc* #'(lambda (hwnd param) 
	(format t "window = ~A, param = ~A~%" 
		(get-window-text hwnd)
		 param)))
(EnumWindows (get-callback-procinst 'WndEnumProc) 0)
(get-window-text (GetForegroundWindow))
|#

(defwinapi CreateFile ((file-name (:unsigned-char *)) 
							(access :long) 
							(mode :long) 
							(security (:void *))
							(creation :long)
							(attributes :long)
							(template (:void *))) 
	:return-type HANDLE
	:library-name "kernel32.dll"
	:entry-name "CreateFileA"
	:linkage-type :pascal)

(defwinapi SetFilePointer ((handle :long) 
							(distanceToMove :long) 
							(distanceToMoveHigh (:void *)) 
							(MoveMethod :long)) 
	:return-type :long
	:library-name "kernel32.dll"
	:entry-name "SetFilePointer"
	:linkage-type :pascal)

(ct:defun-dll system ((command-line (:char *)))
   :return-type :long
   :library-name "msvcrt.dll"
   :entry-name "system"
   :linkage-type :c)

(defun shell (command-string)
	(system (ct:lisp-string-to-c-string command-string)))

(defun get-command-line () (ct:c-string-to-lisp-string (GetCommandLine)))

;; HINSTANCE ShellExecuteA(HWND hwnd, LPCSTR lpOperation, LPCSTR lpFile, 
;;               LPCSTR lpParameters, LPCSTR lpDirectory, INT nShowCmd);
(defwinapi ShellExecute 
    ((hwnd :long)
     (lpOperation LPCSTR)
     (lpFile LPCSTR)
     (lpParameters LPCSTR)
     (lpDirectory LPCSTR)
     (nShowCmd :long))
    :return-type HINSTANCE
    :library-name "Shell32"    
    :entry-name "ShellExecuteA"
    :linkage-type :pascal)

(defun shell-execute (command params &optional (show t))
    (win:ShellExecute
        0 ;(cl::get-application-main-window)
        (ct:lisp-string-to-c-string "open")
        (ct:lisp-string-to-c-string command)
        (ct:lisp-string-to-c-string params)
        (ct:lisp-string-to-c-string "")
        (logior
            (if show win:SW_SHOWNORMAL win:SW_HIDE))))
      
(defwinapi RemoveDirectory
	((lpPathName LPCSTR)) 
   :return-type BOOL
   :library-name "Kernel32"
   :entry-name "RemoveDirectoryA"
   :linkage-type :pascal)

(defun namestring (x) x)        ;; redefined later
(defun pathname (x) x)          ;; redefined later
(defun delete-directory (path)
    "Deletes an empty directory. 
     Returns true if successful, false otherwise."
    (win:RemoveDirectory 
        (ct:lisp-string-to-c-string (namestring (pathname path)))))

(defun MAKELANGID (p s) (logior (ash s 10) p))
(defun PRIMARYLANGID (lgid) (logand lgid #x3ff))
(defun SUBLANGID (lgid) (ash lgid -10))
(defwinconstant LANG_NEUTRAL 		#x00)
(defwinconstant SUBLANG_DEFAULT 	#x01)
(defwinconstant SUBLANG_SYS_DEFAULT #x02)
(defwinconstant LANG_SYSTEM_DEFAULT (MAKELANGID LANG_NEUTRAL SUBLANG_SYS_DEFAULT))
(defwinconstant LANG_USER_DEFAULT 	(MAKELANGID LANG_NEUTRAL SUBLANG_DEFAULT))

(defwinconstant FORMAT_MESSAGE_ALLOCATE_BUFFER #x00000100)
(defwinconstant FORMAT_MESSAGE_IGNORE_INSERTS  #x00000200)
(defwinconstant FORMAT_MESSAGE_FROM_STRING     #x00000400)
(defwinconstant FORMAT_MESSAGE_FROM_HMODULE    #x00000800)
(defwinconstant FORMAT_MESSAGE_FROM_SYSTEM     #x00001000)
(defwinconstant FORMAT_MESSAGE_ARGUMENT_ARRAY  #x00002000)
(defwinconstant FORMAT_MESSAGE_MAX_WIDTH_MASK  #x000000FF)

(defwinstruct GUID
	((Data1  DWORD)
	 (Data2  WORD)
     (Data3  WORD)
     (Data4  (BYTE 4))
	))
(defwintype LPGUID (GUID *))

#! (:library "Kernel32" :ignore "WINUSERAPI" :export t :pascal "WINAPI")
DWORD WINAPI FormatMessageA(DWORD dwFlags,LPCVOID lpSource,DWORD dwMessageId,DWORD dwLanguageId,
			LPSTR lpBuffer, DWORD nSize,void *Arguments);
HLOCAL WINAPI LocalFree(HLOCAL hMem);
!#

(defun system-error-text (error-id)
	(let* ((ptr (ct:malloc (ct:sizeof '(LPSTR *))))
		   (length 
			(FormatMessage
				(logior FORMAT_MESSAGE_ALLOCATE_BUFFER 
						FORMAT_MESSAGE_IGNORE_INSERTS FORMAT_MESSAGE_FROM_SYSTEM)
				ct:null
				error-id
				LANG_USER_DEFAULT
				ptr
				0
				ct:null))
			message)
		(if (> length 0)
			(setf message (ct:c-string-to-lisp-string (ct:cref (LPSTR *) ptr 0)))
			(LocalFree (ct:cref (LPSTR *) ptr 0)))
		message))		
				 
(export '(shell get-command-line system))
(export '(message-box-ok message-box-ok-cancel message-box-yes-no message-box-yes-no-cancel))
(export '(get-computer-name get-user-name system-error-text delete-directory))

(defun cl::get-dll-proc-addressx (proc-name module)
    (let* ((dll-handle (ct::int-to-foreign-ptr module))
           (name (ct:lisp-string-to-c-string proc-name))
           (proc (win:GetProcAddress dll-handle name)))
        (if (ct:cpointer-null proc)
		  (error "Could not find the procedure ~A, error code = ~D" proc-name (win:GetLastError)))
        proc))
  
#|
LispFunction(Get_DLL_Proc_Address)
{
	LISP_FUNC_BEGIN(2);
	LispObj procname = LISP_ARG(0);
	LispObj dll_handle = LISP_ARG(1);
	HMODULE module = 0;
	FARPROC proc = 0;
	LispObj fp = 0;
	fp = foreignNode();
	checkString(procname);
	checkLispInteger(dll_handle);
	if (isBignum(dll_handle))
	{
		module = (HMODULE)UVECTOR(dll_handle)[BIGNUM_FIRST_CELL];
		if (bignumNegative(dll_handle))
			module = (HMODULE)-((long)module);
	}
	else
		module = (HMODULE)integer(dll_handle);

	proc = GetProcAddress(module, (char*)byteArrayStart(nullTerminate(procname))); 
	if (!proc)
		Error("Could not find the procedure ~A, error code = ~A", procname,
			createLispInteger(GetLastError()));
	UVECTOR(fp)[FOREIGN_PTR] = (LispObj)proc;
	ret = fp;
	LISP_FUNC_RETURN(ret);
}
|#
