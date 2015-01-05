;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	All rights reserved.
;;;;	-------------------------------
(in-package :win32)
#! (:export t :library "KERNEL32")

#define WINVER 0x0400
#define MAX_PATH 260
#define FALSE 0
#define TRUE 1

typedef unsigned long DWORD;
/*
typedef int WINBOOL;
typedef int *PWINBOOL;
typedef int *LPWINBOOL;
*/
#LISP (progn
        (export '(WINBOOL PWINBOOL LPWINBOOL))
        (defctype WINBOOL :long-bool)
        (defctype PWINBOOL (:long-bool *))
        (defctype LPWINBOOL (:long-bool *)))
typedef WINBOOL BOOL;
typedef BOOL *PBOOL;
typedef BOOL *LPBOOL;
typedef unsigned char BYTE;
typedef unsigned short WORD;
typedef float FLOAT;
typedef FLOAT *PFLOAT;
typedef BYTE *PBYTE;
typedef BYTE *LPBYTE;
typedef int *PINT;
typedef int *LPINT;
typedef WORD *PWORD;
typedef WORD *LPWORD;
typedef long *LPLONG;
typedef DWORD *PDWORD;
typedef DWORD *LPDWORD;
typedef void *PVOID;
typedef void *LPVOID;
typedef CONST void *PCVOID;
typedef CONST void *LPCVOID;
typedef int INT;
typedef unsigned int UINT;
typedef unsigned int *PUINT;
typedef unsigned int *LPUINT;

typedef UINT WPARAM;
typedef LONG LPARAM;
typedef LONG LRESULT;
typedef LONG HRESULT;
typedef WORD ATOM;
typedef HANDLE HGLOBAL;
typedef HANDLE HLOCAL;
typedef HANDLE GLOBALHANDLE;
typedef HANDLE LOCALHANDLE;
typedef void *HGDIOBJ;
typedef HANDLE HACCEL;
typedef HANDLE HACCEL;
typedef HANDLE HBITMAP;
typedef HANDLE HBRUSH;
typedef HANDLE HCOLORSPACE;
typedef HANDLE HDC;
typedef HANDLE HGLRC;
typedef HANDLE HDESK;
typedef HANDLE HENHMETAFILE;
typedef HANDLE HFONT;
typedef HANDLE HICON;
typedef HANDLE HKEY;
typedef HKEY *PHKEY;
typedef HANDLE HMENU;
typedef HANDLE HMETAFILE;
typedef HANDLE HINSTANCE;
typedef HINSTANCE HMODULE;
typedef HANDLE HPALETTE;
typedef HANDLE HPEN;
typedef HANDLE HRGN;
typedef HANDLE HRSRC;
typedef HANDLE HSTR;
typedef HANDLE HTASK;
typedef HANDLE HWND;
typedef HANDLE HWINSTA;
typedef HANDLE HKL;
typedef int HFILE;
typedef HICON HCURSOR;
typedef DWORD COLORREF;
//typedef int (WINAPI *FARPROC)();
//typedef int (WINAPI *NEARPROC)();
//typedef int (WINAPI *PROC)();
typedef VOID *FARPROC;
typedef VOID *NEARPROC;
typedef VOID *PROC;

typedef struct tagRECT {
	LONG left;
	LONG top;
	LONG right;
	LONG bottom;
} RECT,RECTL,*LPRECT,*LPRECTL;
typedef const RECT *LPCRECT;
typedef const RECT *LPCRECTL;
typedef struct tagPOINT {
	LONG x;
	LONG y;
} POINT,POINTL,*PPOINT,*LPPOINT,*PPOINTL,*LPPOINTL;
typedef struct tagSIZE {
	LONG cx;
	LONG cy;
} SIZE,SIZEL,*PSIZE,*LPSIZE,*PSIZEL,*LPSIZEL;
typedef struct tagPOINTS {
	SHORT x;
	SHORT y;
} POINTS,*PPOINTS,*LPPOINTS;

!#

(provide "WINDEF")
