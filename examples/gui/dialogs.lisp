;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
(in-package :win)
(require "GUI")

(defconstant FW_DONTCARE         0)
(defconstant DEFAULT_CHARSET     1)

(defconstant template-max-size 1024)
(defun lobyte (n) (logand n #xff))
(defun hibyte (n) (logand (ash n -8) #xff))

(defun add-byte (n a) (vector-push-extend n a))
(defun add-word (n a) (add-byte (lobyte n) a)(add-byte (hibyte n) a))
(defun add-dword(n a) (add-word (loword n) a)(add-word (hiword n) a))
(defun add-wide-string (s a)
    (loop for c across s do     ; Copy the string
        (add-word (char-int c) a))
    (add-dword 0 a))               ; null terminate

(defun dword-boundary (a)
    (let* ((mod (mod (length a) 4))
           (pad (if (= mod 0) 0 (- 4 mod))))
        (dotimes (i pad)
            (add-byte 0 a))))

(defun create-dialog-template ()
    (let ((a (make-array template-max-size :element-type 'byte :adjustable t :fill-pointer 0))
          (style (logior DS_MODALFRAME WS_POPUP WS_VISIBLE WS_CAPTION WS_SYSMENU DS_SETFONT))
          (num-items 1)
          (title "Dialog"))
        
        (add-word 1 a)                  ; DlgVer
        (add-word #xffff a)             ; Signature
        (add-dword 0 a)                 ; HelpID
        (add-dword 0 a)                 ; lExtendedStyle
        (add-dword style a)             ; lStyle        
        (add-word num-items a)          ; NumberOfItems
        (add-word 210 a)                ; x
        (add-word 10 a)                 ; y
        (add-word 100 a)                ; cx
        (add-word 100 a)                ; cy
        (add-word 0 a)                  ; menu
        (add-word 0 a)                  ; Class 
        (add-wide-string title a)       ; Copy the title of the dialog box
        (add-word 18 a)                 ; Point size
        (add-word FW_DONTCARE a)        ; Weight
        (add-byte FALSE a)              ; Italic
        (add-byte DEFAULT_CHARSET a)    ; charset
        (add-wide-string "Times New Roman" a) ; typeface
        
        ;; first item
        (dword-boundary a)              ; pad to dword boundary
        (add-dword 0 a)                 ; lHelpID
        (add-dword 0 a)                 ; lExtendedStyle        
        (add-dword 
            (logior BS_PUSHBUTTON WS_VISIBLE 
                WS_CHILD WS_TABSTOP) a) ; lStyle        
        (add-word 10 a)                 ; x
        (add-word 60 a)                 ; y
        (add-word 80 a)                 ; cx
        (add-word 20 a)                 ; cy
        (add-word IDOK a)               ; Control ID
        (add-wide-string "BUTTON" a)    ; name of class
        (add-wide-string "OK" a)        ; text of item
        
        (dword-boundary a)              ; pad to dword boundary
        (let ((template (LocalAlloc 0 (length a))))
            (dotimes (i (length a))
                (setf (ct:cref (BYTE *) template i) (aref a i)))
            (DialogBoxIndirectParam (cl::get-application-instance) 
                template (cl::get-application-main-window)
                (get-callback-procinst 'dialog-wndproc)
                0))))
            

                                                        
#|/* Allocate some memory. */ 
pdlgtemplate = p = (PWORD) LocalAlloc (LPTR, 1000);/* Start to fill in the dlgtemplate information, addressing by WORDs. */ 
lStyle = DS_MODALFRAME | WS_POPUP | WS_VISIBLE | WS_CAPTION |
WS_SYSMENU| DS_SETFONT;
*p++ = 1;          // DlgVer
*p++ = 0xFFFF;     // Signature
*p++ = 0;          // LOWORD HelpID
*p++ = 0;          // HIWORD HelpID
*p++ = 0;          // LOWORD (lExtendedStyle)
*p++ = 0;          // HIWORD (lExtendedStyle)
*p++ = LOWORD (lStyle);
*p++ = HIWORD (lStyle);
*p++ = 2;          // NumberOfItems
*p++ = 210;        // x
*p++ = 10;         // y
*p++ = 100;        // cx
*p++ = 100;        // cy
*p++ = 0;          // Menu
*p++ = 0;          // Class/* Copy the title of the dialog box. */ 
nchar = nCopyAnsiToWideChar (p, TEXT("Dialog"));
p += nchar;   /* Font information because of DS_SETFONT. */ 
      *p++ = 18;  // Point size 
      *p++ = FW_DONTCARE;  // Weight
      *p++ = MAKEWORD( FALSE, DEFAULT_CHARSET );  // italic flag and charset.nchar = nCopyAnsiToWideChar (p, TEXT("Times New Roman"));  // Face name
p += nchar;
        
/* Make sure the first item starts on a DWORD boundary. */ 
p = lpwAlign (p);/* Now start with the first item. */ 
lStyle = BS_PUSHBUTTON | WS_VISIBLE | WS_CHILD | WS_TABSTOP;
*p++ = 0;          // LOWORD (lHelpID)
*p++ = 0;          // HIWORD (lHelpID)
*p++ = 0;          // LOWORD (lExtendedStyle)
*p++ = 0;          // HIWORD (lExtendedStyle)
*p++ = LOWORD (lStyle);
*p++ = HIWORD (lStyle);
*p++ = 10;         // x
*p++ = 60;         // y
*p++ = 80;         // cx
*p++ = 20;         // cy
*p++ = IDOK;       // LOWORD (Control ID)
*p++ = 0;      // HOWORD (Control ID)* Fill in class i.d., this time by name. */ 
nchar = nCopyAnsiToWideChar (p, TEXT("BUTTON"));/* Copy the text of the first item. */ 
nchar = nCopyAnsiToWideChar (p, TEXT("OK"));
p += nchar;*p++ = 0;  // Advance pointer over nExtraStuff WORD.
        
        /* make sure the second item starts on a DWORD boundary. */ 
p = lpwAlign (p);lStyle = WS_VISIBLE | WS_CHILD;
*p++ = 0;             // LOWORD (lHelpID)
*p++ = 0;             // HIWORD (lHelpID)
*p++ = 0;             // LOWORD (lExtendedStyle)
*p++ = 0;             // HIWORD (lExtendedStyle)
*p++ = LOWORD (lStyle);
*p++ = HIWORD (lStyle);
*p++ = 20;            // x
*p++ = 5;             // y
*p++ = 65;            // cx
*p++ = 45;            // cy
*p++ = 57;            // LOWORD (Control ID)
*p++ = 0;               // HOWORD (Control ID)// The class name of the custom control.
nchar = nCopyAnsiToWideChar (p, TEXT("ACustomControl"));
p += nchar;/* Copy the text of the second item, null terminate the string. */ 
nchar = nCopyAnsiToWideChar (p, TEXT(""));
p += nchar;*p++ = 8;  // Number of bytes of extra data.*p++ = 0xA1;   //Extra data.
*p++ = 0xA2;
*p++ = 0xA3;
*p++ = 0xA4;DialogBoxIndirect (ghInst, (LPDLGTEMPLATE) pdlgtemplate, hwnd,
(DLGPROC) About);
LocalFree (LocalHandle (pdlgtemplate));
/////////////////////////////////////////////////////////////////////////// 
// 
// 
Helper routines taken from the WIN32SDK DYNDLG sample.
/////////////////////////////////////////////////////////////////////////// 
// 
// 
LPWORD lpwAlign ( LPWORD lpIn)
{
  ULONG ul;  ul = (ULONG) lpIn;
  ul +=3;
  ul >>=2;
  ul <<=2;
  return (LPWORD) ul;
}int nCopyAnsiToWideChar (LPWORD lpWCStr, LPSTR lpAnsiIn)
{
  int nChar = 0;  do {
    *lpWCStr++ = (WORD) *lpAnsiIn;
    nChar++;
  } while (*lpAnsiIn++);  return nChar;
} 
|#
