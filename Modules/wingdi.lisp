;;;;
;;;;	File:	wingdi.lisp
;;;;
(in-package :win32)
#! (:library "GDI32" :ignore "WINUSERAPI" :export t :pascal "WINAPI")
typedef struct tagBITMAP
  {
    LONG        bmType;
    LONG        bmWidth;
    LONG        bmHeight;
    LONG        bmWidthBytes;
    WORD        bmPlanes;
    WORD        bmBitsPixel;
    LPVOID      bmBits;
  } BITMAP, *PBITMAP, NEAR *NPBITMAP, FAR *LPBITMAP;

typedef struct tagBITMAPINFOHEADER{
        DWORD      biSize;
        LONG       biWidth;
        LONG       biHeight;
        WORD       biPlanes;
        WORD       biBitCount;
        DWORD      biCompression;
        DWORD      biSizeImage;
        LONG       biXPelsPerMeter;
        LONG       biYPelsPerMeter;
        DWORD      biClrUsed;
        DWORD      biClrImportant;
} BITMAPINFOHEADER, FAR *LPBITMAPINFOHEADER, *PBITMAPINFOHEADER;

/* Ternary raster operations */
#define SRCCOPY             0x00CC0020 /* dest = source                   */
#define SRCPAINT            0x00EE0086 /* dest = source OR dest           */
#define SRCAND              0x008800C6 /* dest = source AND dest          */
#define SRCINVERT           0x00660046 /* dest = source XOR dest          */
#define SRCERASE            0x00440328 /* dest = source AND (NOT dest )   */
#define NOTSRCCOPY          0x00330008 /* dest = (NOT source)             */
#define NOTSRCERASE         0x001100A6 /* dest = (NOT src) AND (NOT dest) */
#define MERGECOPY           0x00C000CA /* dest = (source AND pattern)     */
#define MERGEPAINT          0x00BB0226 /* dest = (NOT source) OR dest     */
#define PATCOPY             0x00F00021 /* dest = pattern                  */
#define PATPAINT            0x00FB0A09 /* dest = DPSnoo                   */
#define PATINVERT           0x005A0049 /* dest = pattern XOR dest         */
#define DSTINVERT           0x00550009 /* dest = (NOT dest)               */
#define BLACKNESS           0x00000042 /* dest = BLACK                    */
#define WHITENESS           0x00FF0062 /* dest = WHITE                    */

/* DIB color table identifiers */
#define DIB_RGB_COLORS      0 /* color table in RGBs */
#define DIB_PAL_COLORS      1 /* color table in palette indices */

!#

#! (:library "GDI32" :ignore "WINGDIAPI" :export t :pascal "WINAPI")
WINGDIAPI BOOL  WINAPI BitBlt(HDC, int, int, int, int, HDC, int, int, DWORD);
WINGDIAPI BOOL   WINAPI StretchBlt(HDC, int, int, int, int, HDC, int, int, int, int, DWORD);
WINGDIAPI HBITMAP WINAPI CreateBitmap(int, int, UINT, UINT, CONST VOID *);
WINGDIAPI HDC     WINAPI CreateCompatibleDC(HDC);
WINGDIAPI BOOL WINAPI DeleteDC(HDC);
WINGDIAPI HBITMAP WINAPI CreateBitmapIndirect(CONST BITMAP *);
WINGDIAPI int   WINAPI GetObjectA(HGDIOBJ, int, LPVOID);
WINGDIAPI int   WINAPI GetObjectW(HGDIOBJ, int, LPVOID);
WINGDIAPI int   WINAPI SetDIBitsToDevice(HDC, int, int, DWORD, DWORD, int,
        int, UINT, UINT, CONST VOID *, CONST BITMAPINFO *, UINT);
!#

(provide "GDI")
