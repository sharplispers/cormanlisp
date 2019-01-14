;;;; URI library for Corman Lisp - Version 1.0
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
;;;; More recent versions of this software may be available at:
;;;;   http://www.double.nz/cl
;;;;
;;;; Comments, suggestions and bug reports to the author, 
;;;; Christopher Double, at: chris@double.nz
;;;;
;;;; 16/09/2000 - 1.0 
;;;;              Initial release.
;;;;              A quick stab at implementing the URI package that Allegro
;;;;              Common Lisp has. I've started with only the features used
;;;;              by AllegroServe so I can port that package to Corman Lisp.
;;;;              Remaining functions yet to be implemented. See
;;;;              http://www.franz.com/support/tech_corner/uri.php3 for details
;;;;              on the Allegro Common Lisp URI package.
;;;; 23/06/2002   RGC
;;;;              Added local declarations for path-start, path-end, so they
;;;               wouldn't refer to the special variables.
;;;;
(defpackage :net.uri
	(:use :common-lisp)
	(:export 
		"URI"
		"URI-SCHEME"
		"URI-HOST"
		"URI-PORT"
		"URI-PATH"
		"URI-QUERY"
		"URI-FRAGMENT"
		"URI-PLIST"
		"URI-AUTHORITY"
		"RENDER-URI"
		"PARSE-URI"
		"MERGE-URIS"
		"ENOUGH-URI"
		"URI-PARSED-PATH"
		"COPY-URI"
		))

(in-package :net.uri)

(defclass uri ()
	((scheme :initform nil :initarg :scheme :accessor uri-scheme)
		(host :initform nil :initarg :host :accessor uri-host)
		(port :initform nil :initarg :port :accessor uri-port)
		(path :initform nil :initarg :path :accessor uri-path)
		(query :initform nil :initarg :query :accessor uri-query)
		(fragment :initform nil :initarg :fragment :accessor uri-fragment)
		(plist :initform nil :initarg :plist :accessor uri-plist)))

(defgeneric merge-uris (uri base-uri &optional place))
(defgeneric enough-uri (uri base))
(defgeneric uri-parsed-path (uri))

(defun scan-uri-internal (name)
	(let (scheme-start
			scheme-end
			host-start
			host-end
			port-start
			port-end
			fragment-start
			fragment-end
			query-start
			query-end
            path-start
            path-end
			(start 0)
			(end (length name)))
		(labels ((scan-for-scheme (start end)
					(loop for n from start below end do
						(let ((ch (elt name n)))
							(when (eql ch #\:)
								(setq scheme-start start)
								(setq scheme-end n)
								(return-from scan-for-scheme (+ n 1)))
							(when (or (eql ch #\/) (eql ch #\#))
								(return-from scan-for-scheme n)))))
				(scan-for-fragment (start end)
					(loop for n downfrom (- end 1) downto start do
						(let ((ch (elt name n)))
							(when (eql ch #\#)
								(setq fragment-start (+ n 1))))))
				(scan-for-query (start end)
					(loop for n downfrom (- end 1) downto start do
						(let ((ch (elt name n)))
							(when (eql ch #\?)
								(setq query-start (+ n 1))))))			 
				(scan-for-host (start end)
					(if (eql (elt name start) #\/)
						(if (and (> (- end start) 1) (eql (elt name (+ 1 start)) #\/))
							(progn
								(setq host-start (+ 2 start))
								(loop for n from host-start below (or fragment-start query-start end) do
									(let ((ch (elt name n)))
										(when (eql ch #\/)
											(setq host-end n)
											(return (+ host-end 1)))
										(when (eql ch #\:)
											(setq host-end n)
											(setq port-start (+ 1 n))
											(setq port-end (position #\/ name :start port-start))
											(return (+ port-end 1))))))
							(setq start (+ start 1)))
						start)))
			(scan-for-fragment start end)
			(scan-for-query start end)
			(scan-for-scheme start end)
			(scan-for-host (if scheme-end (+ 1 scheme-end) start) (or fragment-start end))
			(setq path-start (if port-end
					(+ 1 port-end)
					(if host-end
						(+ 1 host-end)
						start)))
			(setq path-end (if fragment-start
					(- fragment-start 1)
					(if query-start
						(- query-start 1)
						end)))
			(values
				scheme-start scheme-end
				host-start host-end
				port-start port-end
				path-start path-end
				query-start query-end
				fragment-start fragment-end))))

(defun uri-authority (uri)
	(format nil "~A:~A" (uri-host uri) (uri-port uri)))

(defun render-uri (uri stream)
	(format 
		stream 		
		(with-output-to-string (new-stream)
			(let ((scheme (uri-scheme uri))
					(host (uri-host uri))
					(port (uri-port uri))
					(path (uri-path uri)))
				(when scheme 
					(format new-stream "~A://" scheme))
				(when host
					(format new-stream "~A" host))
				(when port
					(format new-stream ":~A" port))
				(when path
					(format new-stream "~A" path))))))

(defmethod print-object ((uri uri) stream)
	(if *print-escape*
		(format stream "#<~a ~a>" 'uri (render-uri uri nil))
		(render-uri uri stream)))

(defun string-to-keyword (string)
	"Convert a string to an appropriate keyword"
	(when string
		(intern (string-upcase string) (find-package 'keyword))))
		
(defun parse-uri (name &key (class 'uri))
	(multiple-value-bind (scheme-start
			scheme-end
			host-start
			host-end
			port-start
			port-end
			path-start
			path-end
			query-start
			query-end
			fragment-start
			fragment-end)
		(scan-uri-internal name)
		(let ((scheme
					(string-to-keyword 
						(and scheme-start (subseq name scheme-start scheme-end))))
				(path 
					(and path-start (subseq name path-start path-end))))
			(make-instance class
				:scheme scheme			
				:host (and host-start (subseq name host-start host-end))
				:port (and port-start (subseq name port-start port-end))
				:path 
				(if (= (length path) 0) 
					"/" 
					(if (eql (elt path 0) #\/)
						path
						(concatenate 'string "/" path)))
				:query (and query-start (subseq name query-start query-end))
				:fragment (and fragment-start (subseq name fragment-start fragment-end))))))

(defmethod merge-uris (uri base-uri &optional place)
	(declare (ignore uri base-uri place))
	(error "MERGE-URIS not implemented."))

(defmethod enough-uri (uri base)
	(declare (ignore uri base))
	(error "ENOUGH-URI not implemented."))

(defmethod uri-parsed-path (uri)
	(declare (ignore uri))
	(error "URI-PARSED-PATH not implemented."))

(defmethod (setf uri-parsed-path) (value uri)
	(declare (ignore value uri))
	(error "SETF URI-PARSED-PATH not implemented."))

(defun copy-uri (uri &key place scheme host port path query fragment plist)
	(when place
		(error "PLACE keyword of COPY-URI not implemented."))
	(make-instance 'uri
		:scheme (or scheme (uri-scheme uri))
		:host (or host (uri-host uri))
		:port (or port (uri-port uri))
		:path (or path (uri-path uri))
		:query (or query (uri-query uri))
		:fragment (or fragment (uri-fragment uri))
		:plist (or plist (uri-plist uri))))

(provide 'uri)
