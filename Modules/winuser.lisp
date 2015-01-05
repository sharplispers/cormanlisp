;;;
;;;;	File:	winuser.lisp
;;;;
(in-package :win32)

(defun MAKEINTRESOURCE (num) (ct:int-to-foreign-ptr num))

#! (:library "USER32" :ignore "WINUSERAPI" :export t :pascal "WINAPI")
/*
 * Resource Loading Routines
 */

WINUSERAPI
HBITMAP
WINAPI
LoadBitmapA(
    HINSTANCE hInstance,
    LPCSTR lpBitmapName);
WINUSERAPI
HBITMAP
WINAPI
LoadBitmapW(
    HINSTANCE hInstance,
    LPCWSTR lpBitmapName);

WINUSERAPI
HCURSOR
WINAPI
LoadCursorA(
    HINSTANCE hInstance,
    LPCSTR lpCursorName);
WINUSERAPI
HCURSOR
WINAPI
LoadCursorW(
    HINSTANCE hInstance,
    LPCWSTR lpCursorName);

WINUSERAPI
HCURSOR
WINAPI
LoadCursorFromFileA(
    LPCSTR    lpFileName);
WINUSERAPI
HCURSOR
WINAPI
LoadCursorFromFileW(
    LPCWSTR    lpFileName);

WINUSERAPI
HCURSOR
WINAPI
CreateCursor(
    HINSTANCE hInst,
    int xHotSpot,
    int yHotSpot,
    int nWidth,
    int nHeight,
    CONST VOID *pvANDPlane,
    CONST VOID *pvXORPlane);

WINUSERAPI
BOOL
WINAPI
DestroyCursor(
    HCURSOR hCursor);

!#

(provide "WINUSER")