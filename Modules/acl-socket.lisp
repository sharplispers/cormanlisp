;;;; ACL socket wrapper library for Corman Lisp - Version 1.1
;;;;
;;;; Copyright (C) 2000 Christopher Double. All Rights Reserved.
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
;;;; Notes
;;;; =====
;;;; A simple wrapper around the SOCKETS package to present an interface
;;;; similar to the Allegro Common Lisp SOCKET package. Based on a package
;;;; by David Bakhash for LispWorks. For documentation on the ACL SOCKET
;;;; package see:
;;;;
;;;;   http://www.franz.com/support/documentation/5.0.1/doc/cl/socket.htm
;;;;
;;;; More recent versions of this software may be available at:
;;;;   http://www.double.nz/cl
;;;;
;;;; Comments, suggestions and bug reports to the author, 
;;;; Christopher Double, at: chris@double.nz
;;;;
;;;; 17/09/2000 - 1.0 
;;;;              Initial release.
;;;;
;;;; 20/09/2000 - 1.1
;;;;              Added SOCKET-CONTROL function.
;;;;
;;;; 27/02/2001 - 1.2
;;;;              Added ability to create SSL sockets. Doesn't use
;;;;              same interface as Allegro 6 - need to look into
;;;;              how that works.
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sockets)
  (require :ssl-sockets))

(sockets:start-sockets)
(ssl-sockets:start-ssl-sockets)

(defpackage socket
  (:use "COMMON-LISP")
  (:export 
		"MAKE-SOCKET"
		"ACCEPT-CONNECTION"
		"DOTTED-TO-IPADDR"
		"IPADDR-TO-DOTTED"
		"IPADDR-TO-HOSTNAME"
		"LOOKUP-HOSTNAME"
		"REMOTE-HOST"
		"LOCAL-HOST"
		"LOCAL-PORT"
		"SOCKET-CONTROL"
		))

(in-package :socket)

(defmethod accept-connection ((server-socket sockets::server-socket)
			      &key (wait t))
	(unless wait
		(error "WAIT keyword to ACCEPT-CONNECTION not implemented."))
	(sockets:make-socket-stream 
		(sockets:accept-socket server-socket)))

(defun make-socket (&key (remote-host "0.0.0.0") ;;localhost?
		type
			 local-port
			 remote-port 
			 (connect :active)
			 (format :text)
		ssl
        reuse-address)
    (check-type remote-host string)
	(when (eq type :datagram)
		(error ":DATAGRAM keyword to MAKE-SOCKET not implemented."))
	(when (eq format :binary)
		(warn ":BINARY keyword to MAKE-SOCKET partially implemented."))
	(when reuse-address
		(warn ":REUSE-ADDRESS keyword to MAKE-SOCKET not implemented."))
	
	(ecase connect
		(:passive
			(sockets:make-server-socket 
				:host remote-host
				:port local-port))
		(:active			
			(sockets:make-socket-stream
				(if ssl
					(ssl-sockets:make-client-ssl-socket
						:host remote-host
						:port remote-port)
					(sockets:make-client-socket
						:host remote-host
						:port remote-port))))))
					

(defun dotted-to-ipaddr (dotted &key errorp)
	(when errorp
		(warn ":ERRORP keyword to DOTTED-TO-IPADDR not supported."))
	(sockets:host-to-ipaddr dotted))

(defun ipaddr-to-dotted (ipaddr &key values)
	(when values
		(error ":VALUES keyword to IPADDR-TO-DOTTED not supported."))
	(sockets:ipaddr-to-dotted ipaddr))

(defun ipaddr-to-hostname (ipaddr &key ignore-cache)
	(when ignore-cache
		(warn ":IGNORE-CACHE keyword to IPADDR-TO-HOSTNAME not supported."))
	(sockets:ipaddr-to-name ipaddr))

(defun lookup-hostname (host &key ignore-cache)
	(when ignore-cache
		(warn ":IGNORE-CACHE keyword to IPADDR-TO-HOSTNAME not supported."))
	(if (stringp host)
		(sockets:host-to-ipaddr host)
		(dotted-to-ipaddr (ipaddr-to-dotted host))))	

(defun remote-host (socket-or-stream)
	(let ((socket (if (typep socket-or-stream 'sockets:base-socket)
					socket-or-stream
					(sockets:stream-socket-handle socket-or-stream))))
		(sockets::socket-host-ipaddr socket)))

(defun local-host (socket-or-stream)
	(let ((socket (if (typep socket-or-stream 'sockets:base-socket)
					socket-or-stream
					(sockets:stream-socket-handle socket-or-stream))))
		(sockets::socket-host-ipaddr socket)))

(defun local-port (socket-or-stream)
	(let ((socket (if (typep socket-or-stream 'sockets:base-socket)
					socket-or-stream
					(sockets:stream-socket-handle socket-or-stream))))
		(sockets:socket-port socket)))

(defun socket-control (stream &key output-chunking output-chunking-eof input-chunking)
	(declare (ignore stream output-chunking output-chunking-eof input-chunking))
	(warn "SOCKET-CONTROL function not implemented."))

(provide 'acl-socket)

#|
(in-package :cl-user)
(require 'acl-socket)
(require 'mp)
(use-package :socket)
(use-package :mp)

(let ((stream (make-socket :remote-host "www.double.nz" :remote-port 80)))
	(write-line "GET / HTTP/1.0" stream)
	(write-line "" stream)
	(force-output stream)
	(loop as line = (read-line stream nil :eof)
		until (eq line :eof)
		do (format t "~A~%" line))
	(close stream))

(defun start-server-test (&optional (port 8002))
	(let ((s (make-socket :connect :passive :remote-host "0.0.0.0" :local-port port)))
		(unwind-protect
			(loop
				(let ((stream (accept-connection s)))
					(when (equal (read-line stream) "quit")
						(return))
					(write-line "ABCDEFGHIJKLMNOPQRSTUVWXYZ" stream)
					(write-line "01234567890" stream)
					(force-output stream)
					(close stream)))
			(close s))))

(process-run-function "server-test" #'start-server-test)

(let ((stream (make-socket :remote-host "localhost" :remote-port 8002)))
	(write-line "GO" stream)
	(force-output stream)
	(format t "~A~%" (read-line stream))
	(format t "~A~%" (read-line stream))
	(close stream))

(let ((stream (make-socket :remote-host "localhost" :remote-port 8002)))
	(write-line "quit" stream)
	(force-output stream))
	
|#
