;;;; Simple socket library for Corman Lisp - Version 1.9
;;;;
;;;; Copyright (C) 2000 Christopher Double. See LICENSE.txt for license information.
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
;;;; See the examples at the end of the file for useage. It requires
;;;; at least version 1.4 of Corman Lisp available at http://www.cormanlisp.com
;;;;
;;;; More recent versions of this software may be available at:
;;;;   http://www.double.co.nz/cl
;;;;
;;;; Comments, suggestions and bug reports to the author, 
;;;; Christopher Double, at: chris@double.co.nz
;;;;
;;;; 05/12/1999 - 1.0 
;;;;              Initial release.
;;;;              It's quite rough and quickly thrown together but does 
;;;;              allow using sockets with Corman Lisp 1.3. The API needs 
;;;;              work and I'm open to suggestions on better ways of doing 
;;;;              things. Stream integration would be nice. The intent of 
;;;;              the software was purely an example of using the 
;;;;              WINSOCK API but it may prove useful outside of that.
;;;;
;;;; 06/12/1999 - 1.1
;;;;              Added exports to package definition. 
;;;;              Removed INTERNET-ADDRESS class and associated generic 
;;;;              functions, replacing with IPADDR-TO-NAME, and similar
;;;;              functions working on the ipaddr directly. 
;;;;              HOST-TO-IPADDR can now take host name or dotted address.
;;;;              Added example of a server in a thread.
;;;;
;;;; 18/12/1999 - 1.2
;;;;              Changed finalization registration of socket class to
;;;;              call CLOSE-SOCKET on the object passed to the finalization
;;;;              function rather than the closure over the argument to the
;;;;              INITIALIZE-INSTANCE method. 
;;;;              Changed READ-SOCKET-LINE to have the similar arguments and eof 
;;;;              behaviour as READ-LINE.
;;;;              Added a buffer to READ-SOCKET-LINE to remove the previous
;;;;              behavior of 1 byte reads.
;;;;              Added DO-FFI-READ-SOCKET generic function to enable sharing
;;;;              of READ-SOCKET-LINE code between SSL sockets and standard sockets.
;;;;              Ditto with DO-FFI-WRITE-SOCKET.
;;;;              Moved to a simpler license.
;;;;              Added WITH-CLIENT-SOCKET, WITH-SERVER-SOCKET,
;;;;              WITH-SERVER-ACCEPT and START-SOCKET-SERVER.
;;;;
;;;; 19/12/1999 - 1.3
;;;;              Added READ-SOCKET and WRITE-SOCKET. Added example using these 
;;;;              methods.
;;;;
;;;; 02/03/2000 - 1.4
;;;;              Updated for Corman Lisp 1.4. The changes were to add the
;;;;              require for the winsock package, various ignore delcarations
;;;;              and use provide with the name SOCKETS. By putting this file in
;;;;              the modules subdirectory of your corman lisp implementation you
;;;;              can load it automatically using (require 'SOCKETS).
;;;;              A future update will improve the error handling with the new
;;;;              features available in Corman Lisp 1.4.
;;;;
;;;; 30/08/2000 - 1.5
;;;;              Added support for socket streams. Calling MAKE-SOCKET-STREAM on
;;;;              a socket will return a stream that can be used with the standard
;;;;              common lisp functions like READ-LINE, etc. The stream is buffered
;;;;              for both reads and writes, so when writing you will need to use
;;;;              FORCE-OUTPUT on the stream to send the data over the socket.
;;;;
;;;;              I also improved the code that handles the read buffer for sockets.
;;;;              The stream integration requires some modifications to some standard
;;;;              common lisp functions (READ-LINE, WRITE-LINE, CLOSE). These are
;;;;              overridden and the original functions called when the network required
;;;;              portion is performed. For example, CLOSE can be used on the socket
;;;;              stream as a result of this.
;;;;
;;;;              When creating a socket stream, don't use the original socket otherwise
;;;;              the buffering will get confused. The stream and socket use seperate
;;;;              buffers. I'll probably fix this some day.
;;;;
;;;;              Some new examples have been added to show stream use. 
;;;;
;;;;              Added a WINSOCK-ERROR condition for when errors occur. Added 
;;;;              READ-SOCKET-CHAR. Added convenience macro, WITH-SOCKET-STREAM to create
;;;;              a stream for a socket and close it at the end of the macro scope.
;;;;
;;;; 05/08/2000 - 1.6
;;;;              Added support for using client sockets by tunnelling
;;;;              through a proxy server.  Currently only 'generic'
;;;;              proxy support is implemented. That is, servers that
;;;;              provide socket and SSL tunnelling via the CONNECT
;;;;              method. I also haven't implemented authentication
;;;;              (if the proxy requires you to logon).  It does work
;;;;              for the generic proxy I use and has been tested with
;;;;              port 443 (SSL) and port 80 (Standard HTTP). See the
;;;;              examples at the end of the file for an example of
;;;;              how to use a proxy (hint: set *DEFAULT-PROXY-SERVER*
;;;;              to be an instance of GENERIC-PROXY-SERVER containing
;;;;              your proxy details).  The proxy functionality is
;;;;              experimental and will probably change as I add
;;;;              authentication and other support.
;;;;
;;;;              Added WITH-SOCKETS-STARTED macro to call
;;;;              START-SOCKETS and STOP-SOCKETS around the supplied
;;;;              body.
;;;;
;;;;              Fixed problem in POPULATE-SOCKET-READ-BUFFER that
;;;;              could cause blocking on a socket even though data
;;;;              had already been loaded in the buffer that the
;;;;              caller could use.
;;;;
;;;;              Uncovered a bug in READ-SEQUENCE in Corman Lisp 1.41
;;;;              that causes one too many bytes to be read causing
;;;;              blocking sockets when reading sequences. Unless you
;;;;              have patched CL 1.41 to fix this problem I suggest
;;;;              not using READ-SEQUENCE (See
;;;;              http://www.double.co.nz/cl) for an unofficial patch
;;;;              to READ-SEQUENCE to fix the problem.
;;;;
;;;; 10/08/2000 - 1.7
;;;;              Added REMOTE-SOCKET-IPADDR to REMOTE-SOCKET to allow
;;;;              retrieval of the IP address of the remote machine.
;;;;
;;;; 16/08/2000 - 1.8
;;;;              Added WITH-THREADED-SERVER-ACCEPT and
;;;;              START-THREADED-SOCKET-SERVER which spawn a new thread
;;;;              to process the remote socket. Increased the buffer size
;;;;              of sockets for better performance. Changed
;;;;              SOCKET-STREAM-OVERFLOW-FUNCTION to check to see if the full
;;;;              buffer is being sent, and if so, don't copy the contents to
;;;;              a temporary buffer. This is the most common case and improves
;;;;              write performance quite a bit.
;;;;              Changed the way DO-FFI-WRITE-SOCKET worked. It now explicitly
;;;;              allocates a C buffer rather than relying on automatic string
;;;;              conversion between lisp/c types. This stops a memory corruption
;;;;              that was occuring.
;;;;              Automatically calls START-SOCKETS when loaded.
;;;;
;;;; 17/08/2000 - 1.9
;;;;              Changed the redefinition of CLOSE to close SOCKETS as well as
;;;;              streams.
;;;;
;;;; 21/08/2006   Roger Corman
;;;;              Integrated into Corman Lisp image file (CormanLisp.img) so
;;;;              clients no longer will need to use REQUIRE to use it.
;;;;              They will have to call START-SOCKETS exlplicitly however.
;;;;
;;;; 11/05/2007   Erik Huelsmann (checked in by Roger Corman)
;;;;              Fixed CR/LF issues to work the same as file streams.
;;;;              Removed redundant read-line, write-line functions which are not
;;;;              needed here (the standard versions of those functions will support sockets).
;;;;
(require 'WINSOCK)

(defpackage "SOCKETS"
	(:use 
		:COMMON-LISP 
		:WIN
		:C-TYPES
		:WINSOCK)
	(:export 
		"START-SOCKETS"
		"STOP-SOCKETS"
		"WITH-SOCKETS-STARTED"
		"HOST-TO-IPADDR"
		"IPADDR-TO-NAME"
		"IPADDR-TO-DOTTED"
		"BASE-SOCKET"
		"SOCKET-DESCRIPTOR"
		"REMOTE-SOCKET"
		"PROXY-SOCKET-MIXIN"
		"PROXY-CLIENT-SOCKET"
		"PROXY-INITIALIZED"
		"*DEFAULT-PROXY-SERVER*"
		"PROXY-SERVER"
		"GENERIC-PROXY-SERVER"
		"PROXY-SERVER-HOST"
		"PROXY-SERVER-PORT"		
		"LOCAL-SOCKET"
		"SOCKET-HOST"
		"SOCKET-PORT"
		"CLIENT-SOCKET"
		"SERVER-SOCKET"
		"ACCEPT-SOCKET"
		"CLOSE-SOCKET"
		"WRITE-SOCKET-LINE"
		"DO-FFI-READ-SOCKET"
		"DO-FFI-WRITE-SOCKET"
		"READ-SOCKET-LINE"
		"READ-SOCKET"
		"READ-SOCKET-CHAR"
		"WRITE-SOCKET"
		"MAKE-CLIENT-SOCKET"
		"MAKE-SERVER-SOCKET"
		"WITH-CLIENT-SOCKET"
		"WITH-SERVER-SOCKET"
		"WITH-SERVER-ACCEPT"
		"WITH-THREADED-SERVER-ACCEPT"
		"START-SOCKET-SERVER"
		"START-THREADED-SOCKET-SERVER"
		"MAKE-SOCKET-STREAM"
		"WITH-SOCKET-STREAM"
		"STREAM-SOCKET-HANDLE"
		"REMOTE-SOCKET-CLASS"
		"REMOTE-SOCKET-IPADDR"
        "SOCKET-STREAM"
        "SOCKET-STREAM-P"
        "GET-HTTP-FILE"
		))

(in-package :sockets)

(defmacro with-c-buffer ((buffer length) &body body)
	"Helper macro to automatically free a malloced buffer."
	`(let ((,buffer (malloc ,length)))
		(unwind-protect
			(progn
				,@body)
			(free ,buffer))))

(defparameter *socket-buffer-length* 20000
	"Size of buffer used to read data from the socket stream.")
	
(defun make-word ( low-byte high-byte )
	(logior (logand low-byte #xff) (ash (logand high-byte #xff) 8)))

(define-condition winsock-error (error)
	((original-error-code :initarg :original-error-code :initform nil :reader winsock-original-error-code)
		(last-error-code :initarg :last-error-code :initform nil :reader winsock-last-error-code))
	(:report (lambda (condition stream)
			(format stream "Winsock error number ~A (WSALastError=~A)." 
				(winsock-original-error-code condition)
				(winsock-last-error-code condition)))))

(defun handle-winsock-error (&optional original-code)
	"Handle the result of a winsock function returning an error value."
	(cerror "Winsock Error"
		'winsock-error 
		:original-error-code original-code
		:last-error-code (WSAGetLastError)))

(defun default-winsock-error-test (x)
	"Tests against the standard 0 success code."
	(not (= x 0)))

(defmacro with-winsock-error-handling ((&key (error-test #'default-winsock-error-test)) winsock-call &body body)
	(let ((result-code (gensym)))
		`(let ((,result-code ,winsock-call))
			(if (funcall ,error-test ,result-code)
				(handle-winsock-error ,result-code)
				(progn ,result-code ,@body)))))

(defmacro with-invalid-socket-check (() winsock-call &body body)
	"Checks the result of the winsock call and signals an error if it
	is an INVALID_SOCKET. Otherwise processes the body forms."
	(declare (ignore nil))
	`(with-winsock-error-handling 
		(:error-test #'(lambda (x) (= x INVALID_SOCKET)))
		,winsock-call
		,@body))

(defmacro with-socket-error-check (() winsock-call &body body)
	"Checks the result of the winsock call and signals an error if it
	is a SOCKET_ERROR. Otherwise processes the body forms."
	(declare (ignore nil))
	`(with-winsock-error-handling 
		(:error-test #'(lambda (x) (= x SOCKET_ERROR)))
		,winsock-call
		,@body))

(defmacro with-winsock-pointer-expected (() winsock-call &body body)
	"Checks that the result of the winsock call is a non-null pointer.
	If it is null, then raises an error and does not process the body
	of the macro."
	(declare (ignore nil))
	(let ((pointer-result (gensym)))
		`(let ((,pointer-result ,winsock-call))
			(if (cpointer-null ,pointer-result)
				(handle-winsock-error)
				(progn ,pointer-result ,@body)))))
	
(defvar *sockets-started* nil
	"Set to T when START-SOCKETS is called.")

(defun start-sockets ()
	"Initialize the winsock libraries."
	(unless *sockets-started*
		(with-fresh-foreign-block (wsa 'WSADATA)
			(with-winsock-error-handling ()
				(WSAStartup (make-word 1 1) wsa)))
		(setq *sockets-started* t)))
		
(defun stop-sockets ()
	"Shutdown the winsock libraries."
	(when *sockets-started*
		(with-winsock-error-handling ()
			(WSACleanup))
		(setq *sockets-started* nil)))

(defmacro with-sockets-started (&body body)
	"Helper macro to automatically start and stop sockets."
	`(progn
		(start-sockets)
		(unwind-protect
			(progn
				,@body)
			(stop-sockets))))

(defun host-to-ipaddr (dotted-or-name)
	"Return the ipaddr given a host name or dotted IP address."
	(let* ((name (lisp-string-to-c-string dotted-or-name))
			(ipaddr (inet_addr name)))
		(when (and (= ipaddr INADDR_NONE)
				(not (equal dotted-or-name "255.255.255.255")))
			(setq ipaddr
				(let* ((he (with-winsock-pointer-expected () 
								(gethostbyname name)))
						(addr-list (cref hostent he winsock::h_addr_list))
						(addr0 (cref ((:unsigned-long *) *) addr-list 0)))
					(cref (:unsigned-long *) addr0 0))))
		ipaddr))

(defun ipaddr-to-name (ipaddr)
	"Given an ipaddr, lookup the host name"
	(with-fresh-foreign-block (temp-ipaddr :unsigned-long)
		(setf (cref (:unsigned-long *) temp-ipaddr 0) ipaddr)
		(c-string-to-lisp-string
			(cref hostent
				(with-winsock-pointer-expected ()
					(gethostbyaddr temp-ipaddr (sizeof :unsigned-long) AF_INET))
				winsock::h_name))))
					
			
(defun ipaddr-to-dotted (ipaddr)
	"Given ipaddr, return the dotted name."
	(c-string-to-lisp-string 
		(with-winsock-pointer-expected ()
			(inet_ntoa (int-to-foreign-ptr ipaddr)))))

(defclass base-socket ()
	((socket-descriptor 
			:initform nil 
			:initarg :descriptor 
			:accessor socket-descriptor)
		(read-buffer :initform nil :accessor socket-read-buffer)
		(read-complete :initform nil :accessor socket-read-complete))	
	(:documentation
		"On finalization the socket will be closed if CLOSE-SOCKET
		has not already been called."))

(defclass remote-socket (base-socket) 
	((address :initform nil :initarg :address :accessor remote-socket-ipaddr))
	(:documentation
		"The socket returned by an ACCEPT-SOCKET call. This socket is
		used to communicate with the remote host."))

(defclass local-socket (base-socket)
	((host-ipaddr :initform nil :accessor socket-host-ipaddr)
		(port :initform nil :accessor socket-port))
	(:documentation
		"Base class for sockets that are created and used on the client machine.
		Takes the keywords :HOST and :PORT on creation of the instance. :HOST can
		be a hostname or dotted ip address."))
		
(defclass client-socket (local-socket) ()
	(:documentation
		"Socket used for client programming."))

(defclass server-socket (local-socket) ()
	(:documentation
		"Socket used for writing servers. Allows calling of ACCEPT-SOCKET and returning
		of the remote-socket for communicating with the incoming connection."))

(defvar *default-proxy-server* nil)

(defclass proxy-server ()
	((host :initform nil :initarg :host :accessor proxy-server-host)
		(port :initform nil :initarg :port :accessor proxy-server-port))
	(:documentation
		"Base class for holding proxy server information. Derived classes
		should exist to implement the protocol specific to the type of
		proxy server."))

(defclass generic-proxy-server (proxy-server)
	()
	(:documentation
		"Implements the proxy server protocol for those proxy servers that
		support tunnelling via the CONNECT method."))

(defgeneric proxy-server-connect (server socket host port))

(defmethod proxy-server-connect ((server generic-proxy-server) s host port)
	(declare (ignore server))
	(write-socket-line s
		(format nil "CONNECT ~A:~A HTTP/1.0" host port))
	(write-socket-line s "")
	(read-socket-line s) ;; Verify here?
	(loop as line = (read-socket-line s)
		until (= (length line) 0)))

(defclass proxy-socket-mixin ()
	((initialized :initform nil :accessor proxy-initialized))
	(:documentation 
		"Class to mixin with other socket classes to provide the ability
		to work through a proxy server."))

(defclass proxy-client-socket (client-socket proxy-socket-mixin)
	()
	(:documentation
		"A standard socket that is used through a proxy server."))
  
(defmethod initialize-instance :after ((s base-socket) &allow-other-keys)
	(ccl:register-finalization s #'(lambda (x) (close-socket x))))

(defmethod initialize-instance ((s local-socket) &key host port &allow-other-keys)
    (call-next-method)
	(setf (socket-host-ipaddr s) (host-to-ipaddr host))
	(setf (socket-port s) (or port 0))
	(setf (socket-descriptor s)	
		(with-invalid-socket-check ()
			(socket AF_INET SOCK_STREAM 0))))

(defmethod initialize-instance :after ((s local-socket) &allow-other-keys)
  (with-fresh-foreign-block (sin-local 'sockaddr_in)
    (with-fresh-foreign-block (size 'int)
      (setf (cref (int *) size 0) (sizeof 'sockaddr_in))
      (with-socket-error-check ()
          (getsockname (socket-descriptor s) sin-local size)
        (setf (socket-port s)
              (ntohs (cref sockaddr_in sin-local winsock::sin_port)))))))

(defmethod initialize-instance ((s client-socket) &key host port &allow-other-keys)
	(declare (ignore port host))
	(call-next-method)
	(with-fresh-foreign-block (sin-remote 'sockaddr_in)
		(setf (cref sockaddr_in sin-remote winsock::sin_family) AF_INET)
		(let ((sin0 (cref sockaddr_in sin-remote winsock::sin_addr)))
			(setf (cref in_addr sin0 winsock::S_addr) (socket-host-ipaddr s))
			(setf (cref sockaddr_in sin-remote winsock::sin_port) (htons (socket-port s)))
			(with-socket-error-check ()
				(connect (socket-descriptor s) sin-remote (sizeof 'sockaddr_in))))))

(defmethod initialize-instance ((s server-socket) &key host port &allow-other-keys)
	(declare (ignore port host))
	(call-next-method)
	(with-fresh-foreign-block (sin-local 'sockaddr_in)
		(setf (cref sockaddr_in sin-local winsock::sin_family) AF_INET)
		(let ((sin0 (cref sockaddr_in sin-local winsock::sin_addr)))
			(setf (cref in_addr sin0 winsock::S_addr) (socket-host-ipaddr s)))
		(setf (cref sockaddr_in sin-local winsock::sin_port) (htons (socket-port s)))
		(with-socket-error-check () 
			(bind (socket-descriptor s) sin-local (sizeof 'sockaddr_in)))
		(with-socket-error-check ()
			(listen (socket-descriptor s) SOMAXCONN))))

(defmethod initialize-instance :after ((s proxy-socket-mixin) &key real-host real-port proxy &allow-other-keys)
	(proxy-server-connect proxy s real-host real-port)
	(setf (proxy-initialized s) t))

(defgeneric remote-socket-class (s)
	(:documentation "Given a server socket, return the class
		used for the remote socket for that server socket type."))

(defmethod remote-socket-class ((s server-socket))
	(declare (ignore s))
	'remote-socket)

(defgeneric accept-socket (s)
	(:documentation 
		"Block until a connection is received on the port for this server
		socket. When a connection is received, return a REMOTE-SOCKET for
		communicating with the remote host."))

(defmethod accept-socket ((s server-socket))
	(with-fresh-foreign-block (sin-remote 'sockaddr_in)
		(with-fresh-foreign-block (size-addr 'long)
			(setf (cref (:unsigned-long *) size-addr 0) (sizeof 'sockaddr_in))
			(let ((as (with-invalid-socket-check ()
					(accept (socket-descriptor s) sin-remote size-addr))))								
				(make-instance (remote-socket-class s) 
					:descriptor as
					:address (cref (:unsigned-long *)sin-remote 1))))))

(defgeneric close-socket (s)
	(:documentation 
		"Close the socket connection. This function does not need
		to be called explicitly as it will be called during finalization of the
		object if required."))

(defmethod close-socket ((s base-socket))
	(let ((descriptor (socket-descriptor s)))
		(when descriptor
		    (shutdown descriptor 1)
			(with-c-buffer (buffer (+ 1 *socket-buffer-length*))
				(loop
					(let ((result (recv descriptor buffer *socket-buffer-length* 0)))
						(when (or (= result SOCKET_ERROR) (= result 0))
							(return)))))
			(closesocket descriptor)
			(setf (socket-descriptor s) nil))))

(defgeneric do-ffi-write-socket (s buffer length)
	(:documentation
		"Perform FFI function to write socket data. Extracted out to allow sharing
		of commonality between standard sockets and SSL sockets. BUFFER should be
		a C buffer."))

(defmethod do-ffi-write-socket ((s base-socket) buffer length)
  (with-c-buffer (c-buffer (+ length 1))
		(dotimes (n length)
            (let ((x (elt buffer n)))
                (setf (ct:cref (:unsigned-char *) c-buffer n) (if (characterp x)(char-int x) x))))
		(with-socket-error-check ()
			(send (socket-descriptor s) c-buffer length 0))))

(defmethod do-ffi-write-socket :around ((s proxy-socket-mixin) buffer length)
	(if (proxy-initialized s)
		(call-next-method)
		(with-c-buffer (c-buffer (+ length 1))
			(dotimes (n length)
				(setf (ct:cref (:unsigned-char *) c-buffer n) (char-int (elt buffer n))))
			(with-socket-error-check ()
				(send (socket-descriptor s) c-buffer length 0)))))

(defgeneric write-socket (s string)
	(:documentation
		"Send a string of bytes across the socket."))

(defmethod write-socket ((s base-socket) string)
	(do-ffi-write-socket s string (length string)))
	
(defgeneric write-socket-line (s string)
	(:documentation
		"Send a string across the socket, terminating with a carriage 
		return and line feed."))

(defmethod write-socket-line ((s base-socket) line)
	(let* ((line-buffer (concatenate 'string line (list #\Return #\Newline)))
			(line-length (length line-buffer)))
		(do-ffi-write-socket s line-buffer line-length)))

(defgeneric read-socket (s length &optional eof-error-p eof-value)
	(:documentation
		"Read a number of bytes from the socket and return as a string."))

(defgeneric read-socket-char (s &optional eof-error-p eof-value)
	(:documentation
		"Read a single character from the socket and return it."))

(defmethod read-socket ((s base-socket) len &optional eof-error-p eof-value)
	(declare (ignore eof-error-p))
	(when (< (length (socket-read-buffer s)) len)
		(populate-socket-read-buffer s
			:block t
			:len (- len (length (socket-read-buffer s)))))
	(if (zerop (length (socket-read-buffer s)))
		eof-value
		(let ((len (min len (length (socket-read-buffer s)))))
			(prog1
				(subseq (socket-read-buffer s) 0 len)
				(setf (socket-read-buffer s)
					(subseq
						(socket-read-buffer s) len))))))

(defmethod read-socket-char ((s base-socket) &optional eof-error-p eof-value)
	(declare (ignore eof-error-p))
	(when (< (length (socket-read-buffer s)) 1)
		(populate-socket-read-buffer s
			:block t
			:len 1))
	(if (zerop (length (socket-read-buffer s)))
		eof-value
		(prog1
			(elt (socket-read-buffer s) 0)
			(setf (socket-read-buffer s)
				(subseq
					(socket-read-buffer s) 1)))))

(defun socket-data-available (s)
	"Return the number of bytes available to be read on 
	the socket without blocking."
	(with-fresh-foreign-block (argp 'ULONG)
		(setf (cref (:unsigned-long *) argp 0) 0)
		(ioctlsocket (socket-descriptor s) 1074030207 argp)
		(cref (:unsigned-long *) argp 0)))

(defun c-buffer-to-string (buffer bytes)
  "Create a lisp string from the BYTES first bytes of BUFFER.
 BUFFER may contain null bytes."
  (coerce (loop for i below bytes
             collect (code-char (cref (:unsigned-char *) buffer i)))
          'string))

(defun populate-socket-read-buffer (s &key block len)
	"Fill the socket read buffer with data from the socket.
	Attempt to read at least LEN bytes into the buffer. The
	function will block waiting for data if BLOCK is T. If
	LEN is not supplied, read as much data is available from
	the socket without blocking."
	(when (and (> (length (socket-read-buffer s)) 0)
			(or (null len)
				(<= len (length (socket-read-buffer s)))))
		(return-from populate-socket-read-buffer))	
	(with-c-buffer (buffer (+ *socket-buffer-length* 1))
		(loop
			(let ((bytes (do-ffi-read-socket s buffer *socket-buffer-length*)))
				(when (<= bytes 0)
					(setf (socket-read-complete s) t)
					(return))
				(setf (socket-read-buffer s)
					(concatenate 'string
						(socket-read-buffer s)
						(c-buffer-to-string buffer bytes)))
				(when (and len (<= (decf len bytes) 0))
					(return))
				(when (and (not block) (zerop (socket-data-available s)))
					(return))))))

(defun populate-socket-binary-read-buffer (s &key block len)
	"Fill the socket read buffer with binary data from the socket.
	Attempt to read at least LEN bytes into the buffer. The
	function will block waiting for data if BLOCK is T. If
	LEN is not supplied, read as much data is available from
	the socket without blocking."
    (let ((socket-buffer (socket-read-buffer s)))
    	(when (and (> (length socket-buffer) 0)
    			(or (null len)
    				(<= len (length socket-buffer))))
    		(return-from populate-socket-binary-read-buffer))	
    	(with-c-buffer (buffer (+ *socket-buffer-length* 1))
    		(loop
    			(let ((bytes (do-ffi-read-socket s buffer *socket-buffer-length*)))
    				(when (<= bytes 0)
    					(setf (socket-read-complete s) t)
    					(return))
                    ;; copy the bytes we received to the read-buffer
                    (if (null socket-buffer)
                        (setf socket-buffer (make-array *socket-buffer-length* :fill-pointer 0)))
                    (dotimes (i bytes)
                        (vector-push-extend (ct:cref (:unsigned-char *) buffer i) socket-buffer))
    				(when (and len (<= (decf len bytes) 0))
    					(return))
    				(when (and (not block) (zerop (socket-data-available s)))
    					(return)))))))

(defgeneric read-socket-line (s &optional eof-error-p eof-value)
	(:documentation
		"Read a line of text data from the socket and return as a string."))

(defgeneric do-ffi-read-socket (s buffer length)
	(:documentation
		"Perform FFI function to read socket. Extracted out to allow sharing
		of commonality between standard sockets and SSL sockets."))

(defmethod do-ffi-read-socket ((s base-socket) buffer length)
	(with-socket-error-check ()
		(recv (socket-descriptor s) buffer length 0)))

(defmethod do-ffi-read-socket :around ((s proxy-socket-mixin) buffer length)
	(if (proxy-initialized s)
		(call-next-method)
		(with-socket-error-check ()
			(recv (socket-descriptor s) buffer length 0))))

;; Much of the READ-SOCKET-LINE code is based on READ-LINE from the Corman
;; Lisp implementation.
(defmethod read-socket-line ((s base-socket) &optional eof-error-p eof-value)	
	(declare (ignore eof-error-p))
	(let ((str (make-array 256 :element-type 'character :fill-pointer t)))
		(setf (fill-pointer str) 0)
		(do ((ch (read-socket-char s nil :eof) (read-socket-char s nil :eof)))
			((and (not (eq ch :eof)) (eql ch #\Newline)) (concatenate 'string str))
			(if (eq ch :eof)
				(if (> (length str) 0)
					(return-from read-socket-line (concatenate 'string str))
					(return-from read-socket-line eof-value)))
			(when (not (eql ch #\Return))
				(vector-push-extend ch str)))))

(defun make-client-socket (&key host port (proxy *default-proxy-server*))
	"Create and return a client socket attached to the HOST and PORT."
	(if proxy
		(make-instance 'proxy-client-socket
			:host (proxy-server-host proxy)
			:port (proxy-server-port proxy)
			:real-host host
			:real-port port
			:proxy proxy)
		(make-instance 'client-socket :host host :port port)))

(defun make-server-socket (&key host port)
	"Create and return a sever socket listening on the HOST and PORT."
	(make-instance 'server-socket :host host :port port))

(defmacro with-client-socket ((socket &key host port proxy) &body body)
	"Ensures that the SOCKET is closed when scope of WITH-CLIENT-SOCKET
	has ended."
	(let ((p-name (gensym)))
		`(let* ((,p-name (if ,proxy ,proxy *default-proxy-server*))
				(,socket (make-client-socket :host ,host :port ,port :proxy ,p-name)))
			(unwind-protect
				(progn
					,@body)
				(close-socket ,socket)))))

(defmacro with-server-socket ((socket &key host port) &body body)
	"Ensures that the SOCKET is closed when scope of WITH-SERVER-SOCKET
	has ended."
	`(let ((,socket (make-server-socket :host ,host :port ,port)))
		(unwind-protect
			(progn
				,@body)
			(close-socket ,socket))))

(defmacro with-server-accept ((remote-socket server-socket) &body body)
	"Ensures that the REMOTE-SOCKET is closed when scope of WITH-REMOTE-SOCKET
	has ended."
	`(let ((,remote-socket (accept-socket ,server-socket)))
		(unwind-protect
			(progn
				,@body)
			(close-socket ,remote-socket))))

(defmacro with-threaded-server-accept ((remote-socket server-socket) &body body)
	"Ensures that the REMOTE-SOCKET is closed when scope of WITH-REMOTE-SOCKET
	has ended."
	`(let ((,remote-socket (accept-socket ,server-socket)))
		(th:create-thread #'(lambda ()
				(unwind-protect
					(progn
						,@body)
					(close-socket ,remote-socket))))))

(defmacro start-socket-server ((server-socket remote-socket) &body body)
	"Starts an ACCEPT-SOCKET loop, and evaluates the BODY with the
	REMOTE-SOCKET bound to the result of the ACCEPT-SOCKET call. The loop
	continues indefinitely or until a RETURN expression."
	`(loop
		(with-server-accept (,remote-socket ,server-socket)
			,@body)))

(defmacro start-threaded-socket-server ((server-socket remote-socket) &body body)
	"Starts an ACCEPT-SOCKET loop, and evaluates the BODY with the
	REMOTE-SOCKET bound to the result of the ACCEPT-SOCKET call. The loop
	continues indefinitely or until a RETURN expression."
	`(loop
		(with-threaded-server-accept (,remote-socket ,server-socket)
			,@body)))

(defun socket-stream-underflow-function (s)
	"Function that gets called by the Corman Lisp stream functions when
	the stream buffer is exhausted."
    ;; File streams have 2 buffers too, but their second buffer
    ;; is only temporary in the overflow function...
	(let* ((buffer (cl::stream-input-buffer s))
			(socket (cl::stream-handle s)))
		(populate-socket-read-buffer socket :block nil)
		(let* ((socket-buffer (socket-read-buffer socket))
				(real-length (min (length socket-buffer)
						(cl::stream-input-buffer-length s))))
            (let ((compressed-length 0))
			    (dotimes (i real-length) ;; same as file stream's compress-line-feeds
                    (let ((c (elt socket-buffer i)))
                        (when (eq c #\Return)
                            (setf (elt buffer compressed-length) c)
                            (incf compressed-length))))
                (setf (cl::stream-input-buffer-pos s) 0)
			    (setf (cl::stream-input-buffer-num s) compressed-length)
			    (setf (socket-read-buffer socket) (subseq (socket-read-buffer socket) compressed-length))))))

(defun socket-stream-binary-underflow-function (s)
	"Function that gets called by the Corman Lisp stream functions when
	the socket stream binary input buffer is exhausted."
    ;; File streams have 2 buffers too, but their second buffer
    ;; is only temporary in the overflow function...
	(let* ((buffer (cl::stream-input-buffer s))
			(socket (cl::stream-handle s)))
		(populate-socket-binary-read-buffer socket :block nil)
		(let* ((socket-buffer (socket-read-buffer socket))
				(real-length (min (length socket-buffer)
						(cl::stream-input-buffer-length s))))
            (let ((compressed-length 0))
			    (dotimes (i real-length) ;; copy bytes from socket-buffer to stream-buffer
                    (let ((c (elt socket-buffer i)))
                        (setf (elt buffer i) c)))
                (setf (cl::stream-input-buffer-pos s) 0)
			    (setf (cl::stream-input-buffer-num s) real-length)
			    (setf (socket-read-buffer socket) (subseq (socket-read-buffer socket) real-length))))))
			
(defun socket-stream-overflow-function (s)
	"Called by the Corman Lisp library when the stream output buffer 
	is full."
	(let* ((buffer (cl::stream-output-buffer s))
			(buffer-length (cl::stream-output-buffer-pos s))
			(socket (cl::stream-handle s)))
	  (if (= buffer-length (cl::stream-output-buffer-length s))
		  (write-socket socket buffer)
		(let ((new-buffer (make-array ;; create a buffer and estimate the number of new-lines to expand,
                                      ;; based on the average line length of 40 (lines ranging from 0
                                      ;; to 80 characters); include the est. #newline expansions in the
                                      ;; size of the allocated buffer.
                                      (+ buffer-length (ceiling (/ buffer-length 40)))
                                      :element-type 'character
                                      :adjustable t :fill-pointer 0)))
		  (dotimes (i buffer-length)
            (let ((c (char buffer i)))
                (when (eq c #\Newline)  ;; expand Newline-s
                   (vector-push-extend #\Return new-buffer))
                (vector-push-extend c new-buffer)))
		  (write-socket socket new-buffer)))
	  (setf (cl::stream-output-buffer-pos s) 0)))

(defun socket-stream-binary-overflow-function (s)
	"Called by the Corman Lisp library when the binary socket stream output buffer 
	is full."
	(let* ((buffer (cl::stream-output-buffer s))
			(buffer-length (cl::stream-output-buffer-pos s))
			(socket (cl::stream-handle s)))
	  (if (= buffer-length (cl::stream-output-buffer-length s))
		  (write-socket socket buffer)
		(let ((new-buffer (make-array ;; create a buffer and estimate the number of new-lines to expand,
                                      ;; based on the average line length of 40 (lines ranging from 0
                                      ;; to 80 characters); include the est. #newline expansions in the
                                      ;; size of the allocated buffer.
                                      (+ buffer-length (ceiling (/ buffer-length 40)))
                                      :element-type 'byte
                                      :adjustable t :fill-pointer 0)))
		  (dotimes (i buffer-length)
            (let ((b (elt buffer i)))
                (vector-push-extend b new-buffer)))
		  (write-socket socket new-buffer)))
	  (setf (cl::stream-output-buffer-pos s) 0)))

;;;
;;; Socket streams default to text, binary optional via switch
;;;
(defun make-socket-stream (s &key (binary nil))
	"Given a socket return a stream that allows reads and writes
	to that socket."
	(let ((stream (cl::alloc-uvector cl::stream-size cl::uvector-stream-tag)))
		(setf (cl::uref stream cl::stream-name-offset) nil)
		(setf (cl::uref stream cl::stream-subclass-offset) 'socket-stream)
		(setf (cl::uref stream cl::stream-underflow-func-offset) 
            (if binary #'socket-stream-binary-underflow-function #'socket-stream-underflow-function))
		(setf (cl::uref stream cl::stream-overflow-func-offset) 
            (if binary #'socket-stream-binary-overflow-function #'socket-stream-overflow-function))
		(setf (cl::uref stream cl::stream-position-offset) 0)
		(setf (cl::uref stream cl::stream-col-position-offset) 0)
		(setf (cl::uref stream cl::stream-handle-offset) s)
		(setf (cl::uref stream cl::stream-binary-offset) binary)
		(setf (cl::uref stream cl::stream-line-number-offset) 0)
		(setf (cl::uref stream cl::stream-open-offset) t)
		(setf (cl::uref stream cl::stream-direction-offset) :bidirectional)
		(setf (cl::uref stream cl::stream-interactive-offset) nil)
		(setf (cl::uref stream cl::stream-element-type-offset) (if binary 'byte 'character))
		(setf (cl::uref stream cl::stream-associated-streams-offset) nil)
		(setf (cl::uref stream cl::stream-output-buffer-offset) 
            (make-array (+ *socket-buffer-length* 0) 
                :element-type (if binary 'byte 'character)))
		(setf (cl::uref stream cl::stream-output-buffer-length-offset) *socket-buffer-length*)
		(setf (cl::uref stream cl::stream-output-buffer-pos-offset) 0)
		(setf (cl::uref stream cl::stream-input-buffer-offset) 
            (make-array *socket-buffer-length* 
                :element-type (if binary 'byte 'character)))
		(setf (cl::uref stream cl::stream-input-buffer-length-offset) *socket-buffer-length*)
		(setf (cl::uref stream cl::stream-input-buffer-pos-offset) 0)
		(setf (cl::uref stream cl::stream-input-buffer-num-offset) 0)
		stream))

(defun stream-socket-handle (stream)
	"If the STREAM is a SOCKET-STREAM, return the socket handle
	that the stream works on."
	(cl::stream-handle stream))

(defmacro with-socket-stream ((stream socket) &body body)
	"Creates a socket stream for the given SOCKET and closes it
	when the scope of the macro ends."
	`(let ((,stream (make-socket-stream ,socket)))
		(unwind-protect
			(progn
				,@body)
			(close ,stream))))

(defun socket-stream-p (obj)
    (and (streamp obj)
        (eq (cl::stream-subclass obj) 'socket-stream)))
   
;;;
;;; Redefined Common Lisp CLOSE function to work with sockets
;;;
(in-package :cl)
(defun close (stream &key (abort nil))
    (declare (ignore abort))    ;; we don't currently do anything with this
    (unless (streamp stream)
        (signal-type-error stream 'stream))
    (cond ((and (cl::file-stream-p stream) (cl::stream-open stream))
           (if (output-stream-p stream)
                (force-output stream))
           (let ((ret (win:CloseHandle (ct:int-to-foreign-ptr (cl::stream-handle stream)))))
                (setf (cl::stream-open stream) nil)
                ret))
         ((typep stream 'sockets:base-socket)
			(ignore-errors
				(sockets:close-socket stream)))
         ((sockets:socket-stream-p stream)
			(ignore-errors
				(force-output stream)
				(sockets:close-socket (cl::stream-handle stream)))))
    t)

(in-package :sockets)

;;;
;;; Clients must call START-SOCKETS explicitly
;;;		
;(unless *sockets-started*
;	(start-sockets))

(provide 'SOCKETS)

;;;
;;; Function GET-HTTP-FILE
;;;
;;; Example: 
#|
 (sockets:get-http-file 
       "www.cormanlisp.com" 
       "/CormanLisp/patches/2_5/time.lisp" 
       "temp.txt")
|#
;;;
(defun get-http-file (server local-url local-name)
    (handler-bind ((winsock-error (lambda (c) (declare (ignore c)) (return-from get-http-file nil))))
        (start-sockets)
        (with-client-socket (s :host server :port 80)
        	(write-socket-line s (format nil "GET ~A HTTP/1.0" local-url))
        	(write-socket-line s "")
            (let ((firstline (read-socket-line s)))
                (if (or (not (stringp firstline))
                        (= (length firstline) 0)
                        (= (parse-integer firstline :start (1+ (position #\space firstline)) :junk-allowed t)
                            404))
                    (return-from get-http-file nil))
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
                    (with-open-file (file local-name :direction :output :element-type 'unsigned-byte)
                        (do (str)
                            ((= content-length 0) nil)
                            (setf str (read-socket s content-length))
                            (decf content-length (length str))
                            (if (= (length str) 0)
                                (return))
                            (dotimes (i (length str))
                                (write-byte (char-int (char str i)) file))))
                    local-name)))))

#|
 
;;Example of reading from an HTTP server.
(start-sockets)
(let ((s (make-client-socket :host "www.double.co.nz" :port 80)))
  (write-socket-line s "GET / HTTP/1.0")
  (write-socket-line s "")
  (loop as line = (read-socket-line s nil :eof)
	  until (eq line :eof)
	  do (format t "~A~%" line))
  (close-socket s))

;; Same example using stream support
(let ((s (make-client-socket :host "www.double.co.nz" :port 80)))
	(with-socket-stream (stream s)
		(write-line "GET / HTTP/1.0" stream)
		(write-line "" stream)
		(force-output stream)
		(loop as line = (read-line stream nil :eof)
			until (eq line :eof)
			do (format t "~A~%" line))))

;; Same example using a proxy
(let* ((*default-proxy-server*
			(make-instance 'generic-proxy-server :host "proxy.myserver.com" :port 8080))
		(s (make-client-socket :host "www.double.co.nz" :port 80)))
	(with-socket-stream (stream s)
		(write-line "GET / HTTP/1.0" stream)
		(write-line "" stream)
		(force-output stream)
		(loop as line = (read-line stream nil :eof)
			until (eq line :eof)
			do (format t "~A~%" line))))

;; Reading from an HTTP server, using Content-Length and SOCKET-READ.
;; Loops through the headers looking for Content-Length. When all the
;; headers are read, read the content data using the length previously
;; provided.
(with-client-socket (s :host "www.double.co.nz" :port 80)
	(write-socket-line s "GET / HTTP/1.0")
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
				
;; Same example using stream support
(with-client-socket (s :host "www.double.co.nz" :port 80)
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

;; Same example using a proxy
(let ((*default-proxy-server*
			(make-instance 'generic-proxy-server :host "proxy.myserver.com" :port 8080)))
	(with-client-socket (s :host "www.double.co.nz" :port 80)
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
	   
;; NNTP - retrieves help text from nntp server and uses with-client-socket
(with-client-socket (ns :host "news.xtra.co.nz" :port 119)
	(format t "~A~%" (read-socket-line ns))
	(write-socket-line ns "help")
	(loop as line = (read-socket-line ns)
		while (and  line (not (eql (elt line 0) #\.)))
		do 	(format t "~A~%" line)))

;; Same example using streams
(with-client-socket (s :host "news.xtra.co.nz" :port 119)
	(with-socket-stream (stream s)		
		(format t "~A~%" (read-line stream))
		(write-line "help" stream)
		(force-output stream)
		(loop as line = (read-line stream nil :eof)
			while (and  (not (eq line :eof)) (not (eql (elt line 0) #\.)))
			do 	(format t "~A~%" line))))

;; Server test - echoes lines back to client until 'quit' is received, then closes down.
(defun start-server-test (&optional (port 8001))
	(with-server-socket (s :host "0.0.0.0" :port port)
		(with-server-accept (remote-socket s)
			(format t "Connection made ~A ~A~%" 
				(socket-descriptor remote-socket) (socket-descriptor s))
			(loop 
				(let ((value (read-socket-line remote-socket nil :eof)))
				  (when (eq value :eof)
					(return))
					(format t "~A~%" value)
					(force-output *standard-output*)
					(write-socket-line remote-socket value)
					(when (equal value "quit")
						(return)))))))
(start-server-test)

;; Start the above server in a seperate thread, and connect to it on the current thread.
(th:create-thread #'start-server-test)
(with-client-socket (s :host "localhost" :port 8001)
	(write-socket-line s "Hello Socket World!")
	(format t "Got: ~A~%" (read-socket-line s))
	(write-socket-line s "quit"))

;; with streams...
(th:create-thread #'start-server-test)
(with-client-socket (s :host "localhost" :port 8001)
	(with-socket-stream (stream s)
		(write-line "Hello Socket World!" stream)
		(force-output stream)
		(format t "Got: ~A~%" (read-line stream))
		(write-line "quit" stream)))

;; Another server test - uses start-socket-server and streams
(defun start-server-test2 (&optional (port 8000))
	(with-server-socket (s :host "0.0.0.0" :port port)
		(start-socket-server (s rs)
			(let ((stream (make-socket-stream rs)))
				(when (equal (read-line stream) "quit")
					(return-from start-server-test2))
				(write-line 
					(coerce (make-list 80 :initial-element #\Z) 'string)
					stream)
				(write-line 
					(coerce (make-list 80 :initial-element #\A) 'string)
					stream)
				(force-output stream)))))
(th:create-thread #'start-server-test2)

;; Use the server
(with-client-socket (s :host "localhost" :port 8000)
	(write-socket-line s "GO")
	(format t "~A~%" (read-socket-line s))
	(format t "~A~%" (read-socket-line s)))

;; Use the server with streams
(with-client-socket (s :host "localhost" :port 8000)
	(with-socket-stream (stream s)
		(write-line "GO" stream)
		(force-output stream)
		(format t "~A~%" (read-line stream))
		(format t "~A~%" (read-line stream))))

;; Close the server
(with-client-socket (s :host "localhost" :port 8000)
	(write-socket-line s "quit"))

|#