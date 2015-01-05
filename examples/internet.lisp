;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		internet.lisp
;;;;	Contents:	Functions which open and read from URLs.
;;;;	History:	6/2/98  RGC  Created.
;;;;

(in-package :win32)

(defwintype HINTERNET LPVOID)

(defwinconstant INTERNET_OPEN_TYPE_PRECONFIG    0)   ;; use registry configuration
(defwinconstant INTERNET_OPEN_TYPE_DIRECT       1)   ;; direct to net
(defwinconstant INTERNET_OPEN_TYPE_PROXY        3)   ;; via named proxy

;; HINTERNET InternetOpenA(LPCSTR lpszAgent, DWORD dwAccessType,
;; 				LPCSTR lpszProxy, LPCSTR lpszProxyBypass,
;;				DWORD dwFlags);
(defwinapi InternetOpen
	((lpszAgent LPCSTR)
	 (dwAccessType DWORD)
	 (lpszProxy LPCSTR)
	 (lpszProxyBypass LPCSTR)
	 (dwFlags DWORD))
	:return-type HINTERNET
	:library-name "wininet.dll"
	:entry-name "InternetOpenA"
	:linkage-type :pascal)

;; HINTERNET InternetOpenUrl(HINTERNET hInternetSession, LPCTSTR lpszUrl,
;;				LPCTSTR lpszHeaders, DWORD dwHeadersLength,
;;				DWORD dwFlags, DWORD dwContext );
(defwinapi InternetOpenUrl
	((hInternetSession HINTERNET)
	 (lpszUrl LPCTSTR)
	 (lpszHeaders LPCTSTR)
	 (dwHeadersLength DWORD)
	 (dwFlags DWORD)
	 (dwContext DWORD))
	:return-type HINTERNET
	:library-name "wininet.dll"
	:entry-name "InternetOpenUrlA"
	:linkage-type :pascal)

#! (:library "WinInet" :export t :pascal "WINAPI")
BOOL WINAPI InternetReadFile(HINTERNET hFile, LPVOID lpBuffer, 
			DWORD dwNumberOfBytesToRead, LPDWORD lpNumberOfBytesRead);
!#


#|
(setq ihandle
	(InternetOpen 
		(ct:create-c-string "cormanlisp")
		INTERNET_OPEN_TYPE_PRECONFIG
		ct:null
		ct:null
		0))

(setq urlhandle 
	(InternetOpenUrl
		ihandle
		(ct:create-c-string "http://www.apple.com")
		ct:null
		0
		0
		0))

(setq buffer (ct:malloc 1024))
(setq bytes-read (ct:malloc (ct:sizeof '(DWORD *))))
(setq result (InternetReadFile urlhandle buffer 1000 bytes-read))

|#