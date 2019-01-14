;;;; SSL socket library for Corman Lisp - Version 1.5
;;;;
;;;; Copyright (C) 1999 Christopher Double. All Rights Reserved.
;;;; 
;;;; License
;;;; =======
;;;; This software is provided 'as-is', without any express or implied
;;;; warranty. In no event will the author be held liable for any damages
;;;; arising from the use of this software.
;;;;
;;;; Permission is granted to anyone to use this software for any purpose,
;;;; including commercial applications, and to alter it and redistribute
;;;; it freely, subject to the following restrictions:
;;;;
;;;; 1. The origin of this software must not be misrepresented; you must
;;;;    not claim that you wrote the original software. If you use this
;;;;    software in a product, an acknowledgment in the product documentation
;;;;    would be appreciated but is not required.
;;;;
;;;; 2. Altered source versions must be plainly marked as such, and must
;;;;    not be misrepresented as being the original software.
;;;;
;;;; 3. This notice may not be removed or altered from any source 
;;;;    distribution.
;;;;
;;;; See the end of this file for additional license details regarding the
;;;; OpenSSL libraries.
;;;;
;;;; Notes
;;;; =====
;;;; This library uses the OpenSSL libraries (http://www.openssl.org).
;;;; It was tested with openssl-0.9.4 and the DLL files for Windows
;;;; should be included with this library. 
;;;;
;;;; See the examples at the end of the file for useage. It requires the 
;;;; SOCKETS package by Christopher Double. More recent versions of this 
;;;; software, including the SOCKETS package, may be available at:
;;;;   http://www.double.nz/cl
;;;;
;;;; Comments, suggestions and bug reports to the author, 
;;;; Christopher Double, at: chris@double.nz
;;;;
;;;; This product includes software developed by the OpenSSL Project for 
;;;; use in the OpenSSL Toolkit (http://www.openssl.org/),
;;;; cryptographic software written by Eric Young (eay@cryptsoft.com) and
;;;; software written by Tim Hudson (tjh@cryptsoft.com).
;;;; Modified by Artem Boldarev (artem.boldarev@gmail.com). 
;;;;
;;;; 18/12/1999 - 1.0 
;;;;              Initial release.
;;;;              Initial implementation of client SSL sockets. No error
;;;;              handling so not really very useful for production stuff.
;;;;              Need to improve the read-socket-line code -1 byte buffers
;;;;              make the Winsock lame list in the Winsock FAQ!
;;;;
;;;; 19/12/1999 - 1.1
;;;;              Updated library to work with various changes in SOCKETS library.
;;;;
;;;; 02/03/2000 - 1.2
;;;;              Updated to work nicely with Corman Lisp 1.4 and equivalent
;;;;              changes in the SOCKETS package.
;;;;
;;;; 31/08/2000 - 1.3
;;;;              Updated to work with Corman Lisp 1.41 and SOCKETS library 
;;;;              version 1.5.
;;;;
;;;; 05/09/2000 - 1.4
;;;;              Added support for SSL sockets through a proxy server in the
;;;;              same manner as the support added to the SOCKETS package.
;;;; 30/05/2018 - 1.5 (Artem Boldarev)
;;;;              Added support for OpenSSL 1.1.x.
;;;;              Added support for TLS sockets, removed support for SSLv2, SSLv3 sockets (as these protocols are obsolete)
;;;;              Made the library initialisation safer.
;;;;
(require 'SOCKETS)

(defpackage "SSL-SOCKETS"
	(:use 
		:COMMON-LISP 
		:WIN
		:C-TYPES
		:SOCKETS)
	(:export 
		"START-SSL-SOCKETS"
		"SSL-SOCKET-MIXIN"
		"CLIENT-SSL-SOCKET"
		"PROXY-CLIENT-SSL-SOCKET"
		"SSL-SOCKET-METHOD"
		"SSL-SOCKET-CTX"
		"SSL-SOCKET-HANDLE"
		"MAKE-CLIENT-SSL-SOCKET"
		"WITH-CLIENT-SSL-SOCKET"))

(in-package :ssl-sockets)

; /* old name: ssleay32 */
#! (:export t :library "libssl-1_1")
//void SSL_load_error_strings();
//int SSL_library_init();
//void *SSLv2_client_method();	/* SSLv2 */
//void *SSLv3_client_method();	/* SSLv3 */
//void *SSLv23_client_method();	/* SSLv3 but can rollback to v2 */
//void *TLSv1_client_method();    /* TLSv1 */
void *TLS_client_method();    /* TLS */
void *SSL_CTX_new(void *meth);
void *SSL_new(void *ctx);
int	SSL_set_fd(void *s, int fd);
int	SSL_connect(void *ssl);
int	SSL_write(void *ssl,const char *buf,int num);
int	SSL_read(void *ssl,char *buf,int num);
int SSL_shutdown(void *s);
void SSL_free(void *ssl);
void SSL_CTX_free(void * ctx);
!#

(defun start-ssl-sockets () ;; a mock function
	"Initialize the SSL libraries."
    t)

(defclass ssl-socket-mixin (base-socket)
	((ssl-method :initform nil :accessor ssl-socket-method)
		(ssl-ctx :initform nil :accessor ssl-socket-ctx)
		(ssl-handle :initform nil :accessor ssl-socket-handle))
	(:documentation
		"Mixin class used for adding SSL support to a socket."))

(defclass client-ssl-socket (client-socket ssl-socket-mixin)
	()
	(:documentation
		"SSL Socket used for client programming."))

(defclass proxy-client-ssl-socket (client-socket ssl-socket-mixin proxy-socket-mixin)
	()
	(:documentation 
		"SSL socket that tunnels through a proxy server."))
  
(defmethod initialize-instance :after ((s ssl-socket-mixin) &key host port &allow-other-keys)
    (declare (ignore host port))
    (let* ((method (TLS_client_method))
           (ctx (ssl_ctx_new method))
		(handle (ssl_new ctx)))
		(ssl_set_fd handle (socket-descriptor s))
		(ssl_connect handle)
		(setf (ssl-socket-method s) method)
		(setf (ssl-socket-ctx s) ctx)
		(setf (ssl-socket-handle s) handle)))

;; Note that call-next-method must not be called
(defmethod do-ffi-write-socket ((s ssl-socket-mixin) buffer length)
	(ssl_write (ssl-socket-handle s) buffer length))

;; Note that call-next-method must not be called
(defmethod do-ffi-read-socket ((s ssl-socket-mixin) buffer length)
	(ssl_read (ssl-socket-handle s) buffer length))

(defmethod close-socket :before ((s ssl-socket-mixin))
	(when (ssl-socket-handle s)
		(ssl_shutdown (ssl-socket-handle s))))

(defmethod close-socket :after ((s ssl-socket-mixin))
	(when (ssl-socket-handle s)
		(ssl_free (ssl-socket-handle s))
		(ssl_ctx_free (ssl-socket-ctx s))
		(setf (ssl-socket-handle s) nil)
		(setf (ssl-socket-ctx s) nil)))

(defun make-client-ssl-socket (&key host port (proxy *default-proxy-server*))
  "Create and return an ssl client socket attached to the HOST and PORT."
  (if proxy
    (make-instance 'proxy-client-ssl-socket
                   :host (proxy-server-host proxy)
                   :port (proxy-server-port proxy)
                   :real-host host :real-port port
                   :proxy proxy)
    (make-instance 'client-ssl-socket :host host :port port)))

(defmacro with-client-ssl-socket ((socket &key host port proxy) &body body)
	"Ensures that the SOCKET is closed when scope of WITH-SSL-CLIENT-SOCKET
	has ended."
	(let ((p-name (gensym)))	  
		`(let* ((,p-name (if ,proxy ,proxy *default-proxy-server*))
				(,socket (make-client-ssl-socket :host ,host :port ,port :proxy ,p-name)))
			(unwind-protect
				(progn
					,@body)
				(close-socket ,socket)))))

(provide 'SSL-SOCKETS)

#|
;;Examples:
(in-package :cl-user)
(use-package :sockets)
(use-package :ssl-sockets)

;;
;; Slow way
(with-client-ssl-socket (s :host "wikipedia.org" :port 443)
	(write-socket-line s "GET / HTTP/1.1")
	(write-socket-line s "")	
	(loop as line = (read-socket-line s nil :eof)
		until (eq line :eof)
		do (format t "~A~%" line) (force-output)))

;; Faster way
(with-client-ssl-socket (s :host "wikipedia.org" :port 443)
	(write-socket-line s "GET / HTTP/1.1")
	(write-socket-line s "")
	(let ((content-length 0))
		(loop as line = (read-socket-line s)
			while (and line (> (length line) 0))
			do 
			(when (equal 
					(string-upcase (subseq line 0 (search ":" line)))
					"CONTENT-LENGTH")
				(setq content-length
					(parse-integer line :start (+ (search ":" line) 1)))))
		;; Read contents
		(read-socket s content-length)))

;; Streams faster way
(with-client-ssl-socket (s :host "wikipedia.org" :port 443)
	(with-socket-stream (stream s)
		(write-line "GET / HTTP/1.0" stream)
		(write-line "" stream)
		(force-output stream)
		(let ((content-length 0))
			(loop as line = (read-line stream nil :eof)
				while (and (not (eq line :eof)) (> (length line) 0))
				do 
			(when (equal (string-upcase (subseq line 0 (search ":" line))) "CONTENT-LENGTH")
					(setq content-length
						(parse-integer line :start (+ (search ":" line) 1)))))
			(format t "Total: ~A~%" content-length)
			;; Read contents
			(let ((contents (make-string content-length)))
				(read-sequence contents stream)
				contents))))

;; Using Proxy
(let ((*default-proxy-server* 
			(make-instance 'generic-proxy-server :host "proxy.myserver.com" :port 8080)))
	(with-client-ssl-socket (s :host "wikipedia.org" :port 443)
		(with-socket-stream (stream s)
			(write-line "GET / HTTP/1.0" stream)
			(write-line "" stream)
			(force-output stream)
			(let ((content-length 0))
				(loop as line = (read-line stream nil :eof)
					while (and (not (eq line :eof)) (> (length line) 0))
					do 
				(when (equal (string-upcase (subseq line 0 (search ":" line))) "CONTENT-LENGTH")
						(setq content-length
							(parse-integer line :start (+ (search ":" line) 1)))))
				(format t "Total: ~A~%" content-length)
				;; Read contents
				(let ((contents (make-string content-length)))
					(read-sequence contents stream)
					contents)))))

;; I need some SSL website examples! 
|#

#|
  OpenSSL License
  ---------------

/* ====================================================================
 * Copyright (c) 1998-1999 The OpenSSL Project.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer. 
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. All advertising materials mentioning features or use of this
 *    software must display the following acknowledgment:
 *    "This product includes software developed by the OpenSSL Project
 *    for use in the OpenSSL Toolkit. (http://www.openssl.org/)"
 *
 * 4. The names "OpenSSL Toolkit" and "OpenSSL Project" must not be used to
 *    endorse or promote products derived from this software without
 *    prior written permission. For written permission, please contact
 *    openssl-core@openssl.org.
 *
 * 5. Products derived from this software may not be called "OpenSSL"
 *    nor may "OpenSSL" appear in their names without prior written
 *    permission of the OpenSSL Project.
 *
 * 6. Redistributions of any form whatsoever must retain the following
 *    acknowledgment:
 *    "This product includes software developed by the OpenSSL Project
 *    for use in the OpenSSL Toolkit (http://www.openssl.org/)"
 *
 * THIS SOFTWARE IS PROVIDED BY THE OpenSSL PROJECT ``AS IS'' AND ANY
 * EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE OpenSSL PROJECT OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 * ====================================================================
 *
 * This product includes cryptographic software written by Eric Young
 * (eay@cryptsoft.com).  This product includes software written by Tim
 * Hudson (tjh@cryptsoft.com).
 *
 */

 Original SSLeay License
 -----------------------

/* Copyright (C) 1995-1998 Eric Young (eay@cryptsoft.com)
 * All rights reserved.
 *
 * This package is an SSL implementation written
 * by Eric Young (eay@cryptsoft.com).
 * The implementation was written so as to conform with Netscapes SSL.
 * 
 * This library is free for commercial and non-commercial use as long as
 * the following conditions are aheared to.  The following conditions
 * apply to all code found in this distribution, be it the RC4, RSA,
 * lhash, DES, etc., code; not just the SSL code.  The SSL documentation
 * included with this distribution is covered by the same copyright terms
 * except that the holder is Tim Hudson (tjh@cryptsoft.com).
 * 
 * Copyright remains Eric Young's, and as such any Copyright notices in
 * the code are not to be removed.
 * If this package is used in a product, Eric Young should be given attribution
 * as the author of the parts of the library used.
 * This can be in the form of a textual message at program startup or
 * in documentation (online or textual) provided with the package.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *    "This product includes cryptographic software written by
 *     Eric Young (eay@cryptsoft.com)"
 *    The word 'cryptographic' can be left out if the rouines from the library
 *    being used are not cryptographic related :-).
 * 4. If you include any Windows specific code (or a derivative thereof) from 
 *    the apps directory (application code) you must include an acknowledgement:
 *    "This product includes software written by Tim Hudson (tjh@cryptsoft.com)"
 * 
 * THIS SOFTWARE IS PROVIDED BY ERIC YOUNG ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * 
 * The licence and distribution terms for any publically available version or
 * derivative of this code cannot be changed.  i.e. this code cannot simply be
 * copied and put under another distribution licence
 * [including the GNU Public Licence.]
 */
|#
