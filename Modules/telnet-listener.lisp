;;;; Telnet Listener for Corman Lisp - Version 1.0
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
;;;; An implementation of a simple telnet listener daemon. The daemon
;;;; runs as a lisp process looking for telnet requests on a given port.
;;;; It then spawns a process to handle the request. This process provides
;;;; a simple lisp listener over the telnet connection. Entering :QUIT into
;;;; the listener closes that listener connection.
;;;;
;;;; More recent versions of this software may be available at:
;;;;   http://www.double.co.nz/cl
;;;; 
;;;; Comments, suggestions and bug reports to the author, 
;;;; Christopher Double, at: chris@double.co.nz
;;;;
;;;; 16/09/2000 - 1.0 
;;;;              Initial release.
;;;;              Requires the multiprocessing and sockets packages.
;;;;              Very simple listener. Needs more work to enable 
;;;;              rubout handling, more vt100 translation, etc.
;;;;
(require 'sockets)
(require 'mp)

(defpackage "TELNET-LISTENER"
	(:use 
		:COMMON-LISP 
		:SOCKETS
		:MP)
	(:export
	 "START-TELNET-LISTENER-DAEMON"
	 "STOP-TELNET-LISTENER-DAEMON"))

(in-package :telnet-listener)

;; Create a telnet stream. Translates VT100 escape sequences as required.
(defun telnet-stream-underflow-function (s)
	(let* ((buffer (cl::stream-input-buffer s))
			(other-stream (cl::stream-handle s))
			(ch (read-char other-stream nil :eof)))
		(if (eq ch :eof)
			(progn
				(setf (cl::stream-input-buffer-pos s) 0)
				(setf (cl::stream-input-buffer-num s) 0))
			(progn
				(write-char ch s)
				(force-output s)
				(setf (elt buffer 0) ch)
				(setf (cl::stream-input-buffer-pos s) 0)
				(setf (cl::stream-input-buffer-num s) 1)))))

(defun telnet-stream-overflow-function (s)
	(let* ((buffer (cl::stream-output-buffer s))
			(other-stream (cl::stream-handle s))
			(ch (elt buffer 0)))
		(cond
			((char= ch #\Newline) (format other-stream "~AE" #\Escape)) ;; vt100 newline
			(t (write-char ch other-stream)))
		(force-output other-stream)
		(setf (cl::stream-output-buffer-pos s) 0)))

(defun make-telnet-stream (s)
	"Creates a telnet stream which translates vt100 escape sequences and forwards
	the result onto another stream."
	(let ((stream (cl::alloc-uvector cl::stream-size cl::uvector-stream-tag)))
		(setf (cl::uref stream cl::stream-name-offset) nil)
		(setf (cl::uref stream cl::stream-subclass-offset) 'telnet-stream)
		(setf (cl::uref stream cl::stream-underflow-func-offset) #'telnet-stream-underflow-function)
		(setf (cl::uref stream cl::stream-overflow-func-offset) #'telnet-stream-overflow-function)
		(setf (cl::uref stream cl::stream-position-offset) 0)
		(setf (cl::uref stream cl::stream-col-position-offset) 0)
		(setf (cl::uref stream cl::stream-handle-offset) s)
		(setf (cl::uref stream cl::stream-binary-offset) nil)
		(setf (cl::uref stream cl::stream-line-number-offset) 0)
		(setf (cl::uref stream cl::stream-open-offset) t)
		(setf (cl::uref stream cl::stream-direction-offset) :bidirectional)
		(setf (cl::uref stream cl::stream-interactive-offset) nil)
		(setf (cl::uref stream cl::stream-element-type-offset) 'character)
		(setf (cl::uref stream cl::stream-associated-streams-offset) nil)
		(setf (cl::uref stream cl::stream-output-buffer-offset) (make-array 1 :element-type 'character))
		(setf (cl::uref stream cl::stream-output-buffer-length-offset) 1)
		(setf (cl::uref stream cl::stream-output-buffer-pos-offset) 0)
		(setf (cl::uref stream cl::stream-input-buffer-offset) (make-array 1 :element-type 'character))
		(setf (cl::uref stream cl::stream-input-buffer-length-offset) 1)
		(setf (cl::uref stream cl::stream-input-buffer-pos-offset) 0)
		(setf (cl::uref stream cl::stream-input-buffer-num-offset) 0)
		stream))

(defun telnet-toplevel ()
	"Toplevel read-eval-print loop for the telnet listener."
	(loop
		for form = (progn 
			(format *standard-output* "~&>")
			(force-output)
			(read *standard-input* nil :eof))
		until (eq form :eof)
		do
		(let ((eval-result
					(multiple-value-list
						(ignore-errors (eval form)))))
			(if (and (> (length eval-result) 1)
					(null (first eval-result))
					(typep (second eval-result) 'condition))
				(format t "Condition: ~A~%" (second eval-result))
				(progn
					(format t "~&=> ~A~%~{   ~A~%~}" (car eval-result) (cdr eval-result))
					(when (eq :quit (first eval-result))
						(return))))
			(force-output)))
	(values))

(defvar *telnet-listener-count* 0
	"Could of number of listeners opened so far. Used
	for appending to the process name.")

(defvar *mp-server-quit-table* (make-hash-table)
	"Mapping between port and a flag to indicate whether the
	server on that port should close after the next connect.")

(defun telnet-listener-daemon (port)
	"Listens for telnet connections on a given port, spawning
	processes to handle the connections as they occur. Runs
	indefinitely or until it finds :QUIT in the 
	*mp-server-quit-table* under its port number."
	(let ((s (make-server-socket ::host "0.0.0.0" :port port)))
		(setf (gethash port *mp-server-quit-table*) :running)
		(unwind-protect
			(progn
				(loop
					(when (eq (gethash port *mp-server-quit-table*) :quit)
						(return nil))
					(let ((rs (accept-socket s)))
						(process-run-function
							(format nil "telnet-listener-~S" (incf *telnet-listener-count*))
							#'(lambda ()
								(unwind-protect
									(progn
										(let ((stream (make-socket-stream rs)))
											(let* ((*standard-output* (make-telnet-stream stream))
													(*standard-input* (make-telnet-stream stream)))
												(declare (special *standard-output* *standard-input*))
												(telnet-toplevel))))
									(close-socket rs)))))))
			(remhash port *mp-server-quit-table*)
			(close-socket s))))

(defun start-telnet-listener-daemon (port)
	"Start a telnet listener daemon as a process."
	(process-run-function
		(format nil "telnet-listener-daemon-~A" port)
		#'(lambda () (telnet-listener-daemon port))))

(defun stop-telnet-listener-daemon (port)
	"Stop the telnet listener daemon running on the given port."
	(when (eq (gethash port *mp-server-quit-table*) :running)
		(setf (gethash port *mp-server-quit-table*) :quit)
		(process-run-function
			(format nil "telnet-listener-closer-~A" port)
			#'(lambda ()		 
				(ignore-errors
					(with-client-socket (s :host "127.0.0.1" :port port)))))))
	
(provide 'telnet-listener)

#|
(require 'telnet-listener)

(use-package :telnet-listener)

(start-telnet-listener-daemon 8001)
(mp:proc)

;; use 'telnet localhost 8001' to get a listener
;; Multiple 'telnet localhost 8001' will create multiple listeners.

|#