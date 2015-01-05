;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; main.cl
;;
;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA 
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the AllegroServe
;; prequel found in license-allegroserve.txt.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License is in the file 
;; license-lgpl.txt that was distributed with this file.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;; Suite 330, Boston, MA  02111-1307  USA
;;
;;
;; $Id: main.cl,v 1.53 2000/08/10 15:48:50 jkf Exp $

;; Description:
;;   aserve's main loop

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-


(defpackage :net.aserve
  (:use :common-lisp :excl :net.html.generator)
  (:export
		"AUTHORIZE"
		"AUTHORIZER"
		"BASE64-DECODE"
		"BASE64-ENCODE"
		"COMPUTE-STRATEGY"
		"COMPUTED-ENTITY"
   ;; don't export, these should be private
   ; #:debug-off		
   ; #:debug-on			
		"DENIED-REQUEST"
		"FAILED-REQUEST"
		"FORM-URLENCODED-TO-QUERY"
		"GET-BASIC-AUTHORIZATION"
		"GET-COOKIE-VALUES"
		"GET-MULTIPART-HEADER"
		"GET-MULTIPART-SEQUENCE"
		"GET-REQUEST-BODY"
		"HANDLE-REQUEST"
		"HEADER-SLOT-VALUE"
		"HTTP-REQUEST"
		"LOCATOR"
		"LOCATION-AUTHORIZER"
		"LOCATION-AUTHORIZER-PATTERNS"
		"PASSWORD-AUTHORIZER"
		"PROCESS-ENTITY"
		"PUBLISH"
		"PUBLISH-FILE"
		"PUBLISH-DIRECTORY"
		"QUERY-TO-FORM-URLENCODED"
		"REPLY-HEADER-SLOT-VALUE"
		"SET-BASIC-AUTHORIZATION"
		"STANDARD-LOCATOR"
		"UNPUBLISH-LOCATOR"
		"REQUEST-METHOD"
		"REQUEST-PROTOCOL"
		"REQUEST-PROTOCOL-STRING"
		"REQUEST-QUERY"
		"REQUEST-QUERY-VALUE"
		"REQUEST-RAW-REQUEST"
		"REQUEST-RAW-URI"
		"REQUEST-SOCKET"
		"REQUEST-URI"
		"REQUEST-WSERVER"
		"REQUEST-REPLY-CODE"
		"REQUEST-REPLY-DATE"
		"REQUEST-REPLY-CONTENT-LENGTH"
		"REQUEST-REPLY-CONTENT-TYPE"
		"REQUEST-REPLY-PLIST"
		"REQUEST-REPLY-PROTOCOL-STRING"
		"REQUEST-REPLY-STRATEGY"
		"REQUEST-REPLY-STREAM"
		"SET-COOKIE-HEADER"
		"SHUTDOWN"
		"SPLIT-INTO-WORDS"
		"START"
		"URIDECODE-STRING"
		"URIENCODE-STRING"
		"UNPUBLISH"
		"URL-ARGUMENT"
		"URL-ARGUMENT-ALIST"
		"WITH-HTTP-RESPONSE"
		"WITH-HTTP-BODY"
		"WSERVER"
		"WSERVER-ENABLE-CHUNKING"
		"WSERVER-ENABLE-KEEP-ALIVE"
		"WSERVER-LOCATORS"
		"WSERVER-LOG-FUNCTION"
		"WSERVER-LOG-STREAM"
		"WSERVER-SOCKET"
		"*ASERVE-VERSION*"
		"*HTTP-RESPONSE-TIMEOUT*"
		"*MIME-TYPES*"
		"*RESPONSE-ACCEPTED*"
		"*RESPONSE-BAD-REQUEST*"
		"*RESPONSE-CONTINUE*"
		"*RESPONSE-CREATED*"
		"*RESPONSE-FOUND*"
		"*RESPONSE-INTERNAL-SERVER-ERROR*"
		"*RESPONSE-NOT-FOUND*"
		"*RESPONSE-NOT-MODIFIED*"
		"*RESPONSE-OK*"
		"*RESPONSE-MOVED-PERMANENTLY*"
		"*RESPONSE-SEE-OTHER*"
		"*RESPONSE-TEMPORARY-REDIRECT*"
		"*RESPONSE-UNAUTHORIZED*"
		"*WSERVER*"))
(in-package :net.aserve)

#+cormanlisp
(import 'cl::fixnump)

(defparameter *aserve-version* '(1 1 25))


(provide :aserve)

(defparameter *aserve-version-string*
    ;; for when we need it in string format
    (format nil "~d.~d.~d" 
	    (car *aserve-version*)
	    (cadr *aserve-version*)
	    (caddr *aserve-version*)))
	    
;;;;;;;  debug support 

(defparameter *debug-all* nil)	; all of the debugging switches
(defparameter *debug-log* nil)  ; all debugging switches that write info
				; to the *debug-stream*
(defparameter *debug-current*  nil)	; current switches set

(defparameter *debug-stream* *initial-terminal-io*)

(defmacro define-debug-kind (name class what)
  `(progn (ecase ,class
	    (:all (pushnew ,name *debug-all*))
	    (:log (pushnew ,name *debug-log*)
		  (pushnew ,name *debug-all*)))
	  (setf (get ,name 'debug-description) ,what)))

(define-debug-kind :notrap :all 
  "If set than errors in handlers cause a break loop to be entered")

(define-debug-kind :xmit   :log
  "If set then most of the traffic between clients and servers is also sent to the debug stream")

(define-debug-kind :info   :log
  "General information")

    

(defun debug-on (&rest args)
  ;; add the given debug kinds to the log list
  (if* (null args)
     then (note-debug-set)
     else (dolist (arg args)
	    (case arg
	      (:all (setq *debug-current* *debug-all*))
	      (:log (setq *debug-current*
		      (union *debug-current* *debug-log*)))
	      (t (pushnew arg *debug-current*))))))

(defun debug-off (&rest args)
  ;; turn off the debugging
  (if* (null args)
     then (note-debug-set)
     else (dolist (arg args)
	    (case arg
	      (:all (setq *debug-current* nil))
	      (:log (setq *debug-current*
		      (set-difference *debug-current* *debug-log*)))
	      (t (setq *debug-current* (remove arg *debug-current*)))))))

(defun note-debug-set ()
  ;; describe what debugging switches exist and if they are on
  ;; and off
  (dolist (kind *debug-all*)
    (format t "~7s ~4a  ~a~%" 
	    kind
	    (if* (member kind *debug-current*)
	       then "on"
	       else "off")
	    (get kind 'debug-description))))

	    

(defmacro debug-format (kind &rest args)
  ;; do the format to *debug-stream* if the kind of this info
  ;; is matched by the value of *debug-current*
  `(if* (member ,kind *debug-current* :test #'eq)
      then (format *debug-stream* "d> (~a): " (mp:process-name sys:*current-process*))
	   (format *debug-stream* ,@args)))


(defmacro format-dif (debug-key &rest args)
  ;; do the format and also do the same format to the 
  ;; debug stream if the given debug keyword is set
  ;; do the format and then send to *initial-terminal-io*
  `(progn (format ,@args)
	  (if* (member ,debug-key *debug-current* :test #'eq)
	     then (format *debug-stream* "x>(~a): " 
			  (mp:process-name sys:*current-process*))
		  (format *debug-stream* ,@(cdr args)))))

(defmacro if-debug-action (kind &rest body)
  ;; only do if the debug value is high enough
  `(progn (if* (member ,kind *debug-current* :test #'eq)
	     then ,@body)))

;;;;;;;;;;; end debug support ;;;;;;;;;;;;

#|
;; foreign function imports
#+unix
(ff:def-foreign-call (setuid "setuid") ((x :int)) :returning :int)
#+unix
(ff:def-foreign-call (setgid "setgid") ((x :int)) :returning :int)
|#


	
;;;;;;;;;;;;;  end special vars


(defclass wserver ()
  ;; all the information contained in a web server
  (
   ;;
   ;;-- user visible slots --
   ;; (accessors exported)
   
   (socket 		;; listening socket 
    :initarg :socket
    :accessor wserver-socket)
     
   (enable-keep-alive ;; do keep alive if it's possible
    :initform t
    :initarg :enable-keep-alive
    :accessor wserver-enable-keep-alive)
     
   (enable-chunking  ;; do chunking if it's possible
    :initform t
    :initarg :enable-chunking
    :accessor wserver-enable-chunking)
     
   (locators
    ;; list of locators objects in search order
    :initform (list (make-instance 'locator-exact
		      :name :exact)
		    (make-instance 'locator-prefix
		      :name :prefix)) 
    :accessor wserver-locators)
   
   (log-function
    ;; function to call after the request is done to 
    ;; do the logging
    :initarg :log-function
    :initform nil	; no logging initially
    :accessor wserver-log-function)
   
   (log-stream
    ;; place for log-function to store stream to log to if
    ;; it makes sense to do so
    :initarg :log-stream
    :initform  t 	; initially to *standard-output*
    :accessor wserver-log-stream)
   
   ;;
   ;; -- internal slots --
   ;;
   
   (terminal-io  ;; stream active when we started server
    :initform *terminal-io*
    :initarg  :terminal-io
    :accessor wserver-terminal-io)
   
   (worker-threads  ;; list of threads that can handle http requests
    :initform nil
    :accessor wserver-worker-threads)
     
   (free-workers    ;; estimate of the number of workers that are idle
    :initform 0
    :accessor wserver-free-workers)
     
   (accept-thread   ;; thread accepting connetions and dispatching
    :initform nil
    :accessor wserver-accept-thread)
     
   (invalid-request
    ;; entity to invoke given a request that can't be
    ;; satisfied
    :initform nil  ; will build on demand if not present
    :accessor wserver-invalid-request)
   
   (denied-request
    ;; entity to invoke given a request that was denied
    :initform nil  ; will build on demand if not present
    :accessor wserver-denied-request)
   
   (ipaddrs
    ;; list of the ip addresses by which this wserver has been contacted
    :initform nil
    :accessor wserver-ipaddrs
   )))


     
     
     
     


;;;;;; macros 

(defmacro with-http-response ((req ent
				&key (timeout '*http-response-timeout*)
				     (check-modified t)
				     (response '*response-ok*)
				     content-type
				     )
			       &rest body)
  ;;
  ;; setup to response to an http request
  ;; do the checks that can shortciruit the request
  ;;
  (let ((g-req (gensym))
	(g-ent (gensym))
	(g-timeout (gensym))
	(g-check-modified (gensym)))
    `(let ((,g-req ,req)
	   (,g-ent ,ent)
	   (,g-timeout ,timeout)
	   (,g-check-modified ,check-modified)
	   )
       (catch 'with-http-response
	 (compute-strategy ,g-req ,g-ent)
	 (up-to-date-check ,g-check-modified ,g-req ,g-ent)
	 (mp::with-timeout ((if* (and (fixnump ,g-timeout)
				      (> ,g-timeout 0))
			       then ,g-timeout
			       else 9999999)
			    (timedout-response ,g-req ,g-ent))
	   ,(if* response
	       then `(setf (request-reply-code ,g-req) ,response))
	   ,(if* content-type
	       then `(setf (request-reply-content-type ,g-req) ,content-type)
	       else `(setf (request-reply-content-type ,g-req) (content-type ,g-ent)))
	   ,@body
	   )))))


(defmacro with-http-body ((req ent &key format headers)
			  &rest body)
  (let ((g-req (gensym))
	(g-ent (gensym))
	(g-format (gensym))
	(g-headers (gensym))
	)
    `(let ((,g-req ,req)
	   (,g-ent ,ent)
	   (,g-format ,format)
	   (,g-headers ,headers)
	   )
       (declare (ignore-if-unused ,g-req ,g-ent ,g-format))
       ,(if* body 
	   then `(compute-response-stream ,g-req ,g-ent))
       (if* ,g-headers
	  then (bulk-set-reply-headers ,g-req ,g-headers)
	       (setf (request-reply-headers ,g-req)
		 (append ,g-headers (request-reply-headers ,g-req))))
       (send-response-headers ,g-req ,g-ent :pre)
       (if* (not (member :omit-body (request-reply-strategy ,g-req)))
	  then (let ((*html-stream* (request-reply-stream ,g-req)))
		 (progn ,@body)))
       
       (if* (member :keep-alive (request-reply-strategy ,g-req))
	  then ; force the body to be read so we can continue
	       (get-request-body ,g-req))
       (send-response-headers ,g-req ,g-ent :post))))
			  


; safe versions during multiprocessing

(defmacro atomic-incf (var)
  `(mp:without-scheduling (incf ,var)))

(defmacro atomic-decf (var)
  `(mp:without-scheduling (decf ,var)))


;;;;;;;;; end macros

	       
       


(eval-when (compile load eval)
  ;; these are the common headers and are stored in slots in 
  ;; the objects
  ;; the list consists of  ("name" . name)
  ;; where name is symbol naming the accessor function
  (defparameter *fast-headers*
      (let (res)
	(dolist (name '("connection" 
			"date" 
			"transfer-encoding"
			"accept"
			"host"
			"user-agent"
			"content-length"))
	  (push (list name   ;; string name
		      (read-from-string (format nil "reply-~a" name)) ;; symbol name
		      ;; accessor name
		      (read-from-string 
			    (format nil "request-header-~a" name))) res))
	res))
  
  (defparameter *fast-reply-headers*
      ;; list of headers for the reply that at stored in slots of
      ;; the http request object
      (let (res)
	(dolist (name '("date" 
			"content-type"
			"content-length"))
	  (push (list name   ;; string name
		      
		      ;; symbol naming slot
		      (read-from-string 
		       (concatenate 'string (symbol-name :reply-) name))
		      
		      ;; accessor name
		      (read-from-string 
			    (format nil "request-reply-~a" name))) res))
	res))
  
  )

    
	


(defmacro header-slot-value (obj name)
  ;; name is a string naming the header value (all lower case)
  ;; retrive the slot's value from the http-request obj obj.
  (let (ent)
    (if* (setq ent (assoc name *fast-headers* :test #'equal))
       then ; has a fast accesor
	    `(,(third ent) ,obj)
       else ; must get it from the alist
	    `(cdr (assoc ,name (request-headers ,obj) :test #'equal)))))

(defsetf header-slot-value (obj name) (newval)
  ;; set the header value regardless of where it is stored
  (let (ent)
    (if* (setq ent (assoc name *fast-headers* :test #'equal))
       then `(setf (,(third ent) ,obj) ,newval)
       else (let ((genvar (gensym))
		  (nobj (gensym)))
	      `(let* ((,nobj ,obj)
		      (,genvar (assoc ,name (request-headers ,nobj) 
				      :test #'equal)))
		 (if* (null ,genvar)
		    then (push (setq ,genvar (cons ,name nil))
			       (request-headers ,nobj)))
		 (setf (cdr ,genvar) ,newval))))))

(defmacro reply-header-slot-value (obj name)
  ;; name is a string naming the header value (all lower case)
  ;; retrive the slot's value from the http-request obj obj.
  (let (ent)
    (if* (setq ent (assoc name *fast-reply-headers* :test #'equal))
       then ; has a fast accesor
	    `(,(third ent) ,obj)
       else ; must get it from the alist
	    `(cdr (assoc ,name (request-reply-headers ,obj) :test #'equal)))))

(defsetf reply-header-slot-value (obj name) (newval)
  ;; set the header value regardless of where it is stored
  (let (ent)
    (if* (setq ent (assoc name *fast-reply-headers* :test #'equal))
       then `(setf (,(third ent) ,obj) ,newval)
       else (let ((genvar (gensym))
		  (nobj (gensym)))
	      `(let* ((,nobj ,obj)
		      (,genvar (assoc ,name (request-reply-headers ,nobj) 
				      :test #'equal)))
		 (if* (null ,genvar)
		    then (push (setq ,genvar (cons ,name nil))
			       (request-reply-headers ,nobj)))
		 (setf (cdr ,genvar) ,newval))))))

(defmacro header-slot-value-integer (obj name)
  ;; if the header value exists and has an integer value
  ;; return two values: the value of the integer and t
  ;; else return nil
  
  `(header-decode-integer (header-slot-value ,obj ,name)))



    


(defclass http-header-mixin ()
  ;; List of all the important headers we can see in any of the protocols.
  ;; 
  #.(let (res)
      ;; generate a list of slot descriptors for all of the 
      ;; fast header slots
      (dolist (head *fast-headers*)
	(push `(,(third head) :accessor ,(third head) 
			    :initform nil
			    :initarg
			    ,(intern (symbol-name (second head)) :keyword))
	      res))
      res))
   
	   
	   
   







(defclass http-request (http-header-mixin)
  ;;
  ;; incoming request and information about the reply we send to it
  ;;
  (
   ;;
   ;; -- external slots --
   ;;  (accessors exported)
   
   (method  ;; keyword giving the command in this request :get .. etc.
    :initarg :method
    :reader request-method)
   
   (uri  ;; uri object holding the current request with the scheme, host
         ;; and port filled in.
    :initarg :uri
    :accessor request-uri)

   (raw-uri  ;; uri object holding the actual uri from the command
    :initarg :raw-uri
    :accessor request-raw-uri)
   
   (protocol ;; symbol naming the http protocol  (e.g. :http/1.0)
    :initarg :protocol
    :reader request-protocol)
   
   (protocol-string ;; string naming the protcol requested
    :initarg :protocol-string
    :reader request-protocol-string)
   
   (socket ;; the socket we're communicating through
    :initarg :socket
    :reader request-socket)
   
   (wserver ;; wserver object for web server this request came to
    :initarg :wserver
    :reader request-wserver)
   
   (raw-request  ;; the actual command line from the browser
    :initarg :raw-request
    :reader request-raw-request)
   
   ;;
   ;; -- internal slots --
   ;;
   
   (query-alist 
    ;; list of conses (name . value) for the query part of the 
    ;; current uri.  This slot is filled in when information
    ;; is first requested by  the  request-query function
    :initform :empty
    :accessor request-query-alist)
   
   
   (headers ;; alist of headers *not* stored in slots
    ;* use header-slot-value to retrieve header values 
    ;  rather than looking here since not all headers are stored 
    ;  here
    :initform nil
    :accessor request-headers)
   
   (request-body 
    ;; if we've read the request body then this 
    ;; is the string holding it.
    :initform nil
    :accessor request-request-body)
   
   
   

   ;; response
   (reply-code   ;; one of the *response-xx* objects
    :initform nil
    :accessor request-reply-code)
   
   (reply-date
    :initform (get-universal-time)  ; when we're responding
    :reader request-reply-date)
   
   (reply-headers  ;; alist of headers to send out
    :initform nil
    :accessor request-reply-headers)
   
   (reply-content-type ;; mime type of the response
    :initform nil
    :accessor request-reply-content-type)
   
   (reply-stream   ;; stream to which to send response
    :initform nil
    :accessor request-reply-stream)
   
   (reply-content-length
    :initform nil  ;; nil means "i don't know"
    :accessor request-reply-content-length)
   
   (reply-strategy  ;; list of strategy objects
    :initform nil
    :accessor request-reply-strategy)
   
   (reply-plist    ;; general stuff in a property list form
    :initform nil
    :accessor request-reply-plist)

   (reply-protocol-sring
    ;; A web server announces the highest minor level of the 
    ;; major level of the protocol that was requested by the client.
    ;; Thus for now we're always http/1.1
    :initform "HTTP/1.1"
    :accessor request-reply-protocol-string)
   )
  
  
		
  )


(defstruct (response (:constructor make-resp (number desc)))
  number
  desc)

(defparameter *response-continue* (make-resp 100 "Continue"))
(defparameter *response-ok* (make-resp 200 "OK"))
(defparameter *response-created* (make-resp 201 "Created"))
(defparameter *response-accepted* (make-resp 202 "Accepted"))

(defparameter *response-moved-permanently* (make-resp 301 "Moved Permanently"))
(defparameter *response-found* (make-resp 302 "Found"))
(defparameter *response-see-other* (make-resp 303 "See Other"))
(defparameter *response-not-modified* (make-resp 304 "Not Modified"))
(defparameter *response-temporary-redirect* 
    (make-resp 307 "Temporary Redirect"))
(defparameter *response-bad-request* (make-resp 400 "Bad Request"))
(defparameter *response-unauthorized* (make-resp 401 "Unauthorized"))
(defparameter *response-not-found* (make-resp 404 "Not Found"))

(defparameter *response-internal-server-error*
    (make-resp 500 "Internal Server Error"))
(defparameter *response-not-implemented* (make-resp 501 "Not Implemented"))

(defparameter *responses*
    (list *response-continue*
	  *response-ok*
	  *response-created*
	  *response-accepted*
	  *response-moved-permanently*
	  *response-found*
	  *response-see-other*
	  *response-not-modified*
	  *response-temporary-redirect*
	  *response-bad-request*
	  *response-unauthorized*
	  *response-not-found*))

(defvar *crlf* (make-array 2 :element-type 'character :initial-contents
			   '(#\return #\linefeed)))

(defvar *read-request-timeout* 20)
(defvar *read-request-body-timeout* 60)
(defvar *http-response-timeout* 120) ; amount of time for an http response

(defvar *thread-index*  0)      ; globalcounter to gen process names

(defvar *wserver*)   ; set to last server created

				    
			      
(defun start (&key (port 80) 
		   (listeners 5)
		   (chunking t)
		   (keep-alive t)
		   (server *wserver*)
		   debug      ; set debug level
		   setuid
		   setgid
		   )
  ;; -exported-
  ;;
  ;; start the web server
  ;; return the server object
#|	
  #+mswindows
  (declare (ignore setuid setgid))
|#  
  (declare (ignore debug))  ; for now

  (if* (eq server :new)
     then (setq server (make-instance 'wserver)))
  
  
  ; shut down existing server
	
  (shutdown server) 
	
  
  
  (let* ((main-socket (socket:make-socket :connect :passive
					  :local-port port
					  :reuse-address t
					  :format :bivalent)))
	

#|
    #+unix
    (progn
      (if* (fixnump setgid) then (setgid setgid))
      (if* (fixnump setuid) then (setuid setuid)))
|#    
    (setf (wserver-socket server) main-socket)
    (setf (wserver-terminal-io server) *terminal-io*)
    (setf (wserver-enable-chunking server) chunking)
    (setf (wserver-enable-keep-alive server) keep-alive)
    
	
    
    (let ((*wserver* server)) ; bind it too for privacy
      (if* (or (null listeners) (eq 0 listeners))
	 then 	
(start-simple-server)
       elseif (and (fixnump listeners) (> listeners 0))
	 then (start-lisp-thread-server listeners)
	 else (error "listeners should be nil or a non-negative fixnum, not ~s"
		     listeners)))
    
    server
    ))

(defun shutdown (&optional (server *wserver*))
  ;; shutdown the neo server
  ; first kill off old processes if any
  (let ((proc (wserver-accept-thread server)))
    (if* proc
       then ; we want this thread gone and the socket closed 
	    ; so that we can reopen it if we want to.
	    (mp:process-kill proc)
	    (mp:process-allow-schedule)
	    (let ((oldsock (wserver-socket server)))
	      (if* oldsock then (ignore-errors (close oldsock))))
	    (setf (wserver-accept-thread server) nil)))
  
  (dolist (th (wserver-worker-threads server))
    (mp:process-kill th)
    (mp:process-allow-schedule))
  
  (setf (wserver-worker-threads server) nil)
  )



(defun start-simple-server ()
	
  ;; do all the serving on the main thread so it's easier to
  ;; debug problems
  (let ((main-socket (wserver-socket *wserver*))
	(ipaddrs (wserver-ipaddrs *wserver*)))
    (unwind-protect
	(loop
		
	  (restart-case
	      (let ((sock (socket:accept-connection main-socket))
		    (localhost))
		

		#-cormanlisp (if* (not (member (setq localhost (socket:local-host sock))
				  ipaddrs))
		   then ; new ip address by which this machine is known
			(push localhost ipaddrs)
			(setf (wserver-ipaddrs *wserver*) ipaddrs))
		
		(process-connection sock)
	    	
)

	    (:loop ()  ; abort out of error without closing socket
	      nil)))
      (close main-socket))))

	
(defun start-lisp-thread-server (listeners)
  ;; start a server that consists of a set of lisp threads for
  ;; doing work and a lisp thread for accepting connections
  ;; and farming out the work
  
  ; create worker threads
  (setf (wserver-free-workers *wserver*) 0)
  (dotimes (i listeners) (make-worker-thread))
  
  
  ; create accept thread
  (setf (wserver-accept-thread *wserver*)
    (mp:process-run-function 
     (list :name (format nil "aserve-accept-~d" (incf *thread-index*))
	   :initial-bindings
	   `((*wserver*  . ',*wserver*)
	     #+ignore (*debug-io* . ',(wserver-terminal-io *wserver*))
	     ,@excl:*cl-default-special-bindings*))
     #'http-accept-thread)))

(defun make-worker-thread ()
  (let* ((name (format nil "~d-aserve-worker" (incf *thread-index*)))
	 (proc (mp:make-process :name name
				:initial-bindings
				`((*wserver*  . ',*wserver*)
				  #+ignore (*debug-io* . ',(wserver-terminal-io 
						   *wserver*))
				  ,@excl:*cl-default-special-bindings*)
				)))
    (mp:process-preset proc #'http-worker-thread)
    (push proc (wserver-worker-threads *wserver*))
    (atomic-incf (wserver-free-workers *wserver*))
    ))


(defun http-worker-thread ()
  ;; made runnable when there is an socket on which work is to be done
  (loop

    (let ((sock (car (mp:process-run-reasons sys:*current-process*))))
      (restart-case
	  (if* (not (member :notrap *debug-current* :test #'eq))
	     then (handler-case (process-connection sock)
		    (error (cond)
		      (logmess (format nil "~s: got error ~a~%" 
				       (mp:process-name sys:*current-process*)
				       cond))))
	     else (process-connection sock))
	(abandon ()
	    :report "Abandon this request and wait for the next one"
	  nil))
      (atomic-incf (wserver-free-workers *wserver*))
      (mp:process-revoke-run-reason sys:*current-process* sock))
    
    ))

(defun http-accept-thread ()
  ;; loop doing accepts and processing them
  ;; ignore sporatic errors but stop if we get a few consecutive ones
  ;; since that means things probably aren't going to get better.
  (let* ((error-count 0)
	 (workers nil)
	 (server *wserver*)
	 (main-socket (wserver-socket server))
	 (ipaddrs (wserver-ipaddrs server)))
    (unwind-protect

	(loop
	  (handler-case
	      (let ((sock (socket:accept-connection main-socket))
		    (localhost))
		
		; track all the ipaddrs by which we're reachable
		#-cormanlisp (if* (not (member (setq localhost (socket:local-host sock))
				  ipaddrs))
		   then ; new ip address by which this machine is known
			(push localhost ipaddrs)
			(setf (wserver-ipaddrs *wserver*) ipaddrs))
		
		
		(setq error-count 0) ; reset count
	
		; find a worker thread
		; keep track of the number of times around the loop looking
		; for one so we can handle cases where the workers are all busy
		(let ((looped 0))
		  (loop
		    (if* (null workers) 
		       then (case looped
			      (0 nil)
			      ((1 2 3) (logmess "all threads busy, pause")
				       (sleep 1))
			     
			      (4 (logmess "forced to create new thread")
				 (make-worker-thread))
		    
			      (5 (logmess "can't even create new thread, quitting")
				 (return-from http-accept-thread nil)))
			   
			    (setq workers (wserver-worker-threads server))
			    (incf looped))
		    (if* (null (mp:process-run-reasons (car workers)))
		       then (atomic-decf (wserver-free-workers server))
			    (mp:process-add-run-reason (car workers) sock)
			    (pop workers)
			    (return) ; satisfied
			    )
		    (pop workers))))
	  
	    (error (cond)
	      (logmess (format nil "accept: error on accept ~s" cond))
	      (if* (> (incf error-count) 4)
		 then (logmess "accept: too many errors, bailing")
		      (return-from http-accept-thread nil)))))
      (ignore-errors (progn
		       (mp:without-scheduling
			 (if* (eql (wserver-socket server) main-socket)
			    then (setf (wserver-socket server) nil)))
		       (close main-socket))))))
      
  
    
    

(defun start-cmd ()
  ;; start using the command line arguments
  (let ((port 8001))
    (do* ((args (cdr (sys:command-line-arguments)) (cdr args))
	  (arg (car args) (car args)))
	((null args))
      (if* (equal "-f" arg)
	 then (load (cadr args))
	      (pop args)
       elseif (equal "-p" arg)
	 then (setq port (read-from-string (cadr args)))
	      (pop args)
       elseif (equal "-I" arg)
	 then (pop args)
	 else (warn "unknown arg ~s" arg)))
    (dotimes (i 20)
      (handler-case (progn (start :port port) (loop (sleep 100000)))
	(error (cond)
	  (format t " got error ~a~%" cond)
	  (format t "restarting~%"))))))


(defun process-connection (sock)
  ;; read an http request from the socket and process
  ;; it.
  ;; If the response indicates 'keep alive' then loop around for
  ;; another request.
  ;; When this function returns the given socket has been closed.
  ;;
  (unwind-protect
      (let ((req))
	;; get first command
	(loop
	  (mp:with-timeout (*read-request-timeout* 
			    (debug-format :info "request timed out on read~%")
			    ; this is too common to log, it happens with
			    ; every keep alive socket when the user stops
			    ; clicking
			    ;;(log-timed-out-request-read sock)
			    (return-from process-connection nil))
	    (setq req (read-http-request sock)))
	  (if* (null req)
	     then ; end of file, means do nothing
		  ; (logmess "eof when reading request")
		  ; end this connection by closing socket
		  (return-from process-connection nil)
	     else ;; got a request
		  (handle-request req)
		  (log-request req)
		  (let ((sock (request-socket req)))
		    (if* (member :keep-alive
				 (request-reply-strategy req)
				 :test #'eq)
		       then ; continue to use it
			    (debug-format :info "request over, keep socket alive~%")
			    (force-output sock)
		       else (return))))))
    (ignore-errors (close sock))))


(defun read-http-request (sock)
  ;; read the request from the socket and return and http-request
  ;; object
  
  (let ((buffer (get-request-buffer))
	(req)
	(end)
	(raw-cmd))
    
    (unwind-protect
	(progn
	  (loop
	    ; loop until a non blank line is seen and is stored in
	    ; the buffer
	    ;
	    ; we handle the case of a blank line before the command
	    ; since the spec says that we should (even though we don't have to)
      
	    (multiple-value-setq (buffer end)
	      (read-sock-line sock buffer 0))
      
	    (if* (null end)
	       then ; eof or error before crlf
		    (return-from read-http-request nil))
      
	    
	    (debug-format  :info "got line of size ~d: " end)
	    (if-debug-action :info
			     (dotimes (i end) (write-char (schar buffer i) 
							  *initial-terminal-io*))
			     (terpri *initial-terminal-io*) (force-output *initial-terminal-io*))
      
	    (if* (not (eql 0 end))
	       then (return) ; out of loop
		    ))
	  
	  (setq raw-cmd (buffer-substr buffer 0 end))
	  
	  (multiple-value-bind (cmd uri protocol)
	      (parse-http-command buffer end)
	    (if* (or (null cmd) (null protocol))
	       then ; no valid command found
		    (return-from read-http-request nil))

	    (if* (null (net.uri:uri-path uri))
	       then (setf (net.uri:uri-path uri) "/"))
	    
	    (setq req (make-instance 'http-request
			:method cmd
			:uri (net.uri:copy-uri uri)
			:raw-uri uri
			:protocol protocol
			:protocol-string (case protocol
					   (:http/1.0 "HTTP/1.0")
					   (:http/1.1 "HTTP/1.1")
					   (:http/0.9 "HTTP/0.9"))
			:socket sock
			:wserver *wserver*
			:raw-request raw-cmd
			))
	    
	    
	    (if* (and (not (eq protocol :http/0.9))
		      (null (read-request-headers req sock buffer)))
	       then (debug-format :info "no headers, ignore~%")
		    (return-from read-http-request nil))
	    
	    ; insert the host name and port into the uri
	    (let ((host (request-header-host req)))
	      (if* host
		 then (let ((colonpos (find-it #\: host 0 (length host)))
			    (uri (request-uri req))
			    (port))
			(if* colonpos
			   then ; host:port
				(setq 
				    port (string-to-number
					  host (1+ colonpos)
					  (length host))
				    host (buffer-substr host
							0 colonpos)))
			(if* (null (uri-host uri))
			   then (setf (uri-host uri) host)
				(if* port
				   then (setf (uri-port uri) port)))
			
			(setf (uri-scheme uri) :http)  ; always http
			))))
	  
	    
	  req  ; return req object
	  )
    
      ; cleanup forms
      (if* buffer then (free-request-buffer buffer)))))


				      
    
		    
      
   
    
    
    




(defvar *http-command-list*
    '(("GET " . :get)
      ("HEAD " . :head)
      ("POST " . :post)
      ("PUT "  . :put)
      ("OPTIONS " . :options)
      ("DELETE " .  :delete)
      ("TRACE "  .  :trace)
      ("CONNECT " . :connect)))
  


    
    
    
#+ignore
(defmethod read-entity-body ((req http-request-1.1) sock)
  ;; http/1.1 offers up the chunked transfer encoding
  ;; we can't handle that at present...
  (declare (ignore sock)) ; but they are passed to the next method
  (if* (equalp "chunked" (request-header-transfer-encoding req))
     then (with-http-response (*response-not-implemented*
			       req "text/html" :close t)
	    (format (response req) "chunked transfer is not supported yet"))
	  t
     else ; do the http/1.0 thing
	  (call-next-method)))


  
	  
	  
	  
			     
#+ignore
(defmethod read-entity-body ((req http-request-1.0) sock)
  ;; Read the message following the request header, if any.
  ;; Here's my heuristic
  ;;   if Content-Length is given then it specifies the length.
  ;;   if there is no Content-Length but there is a Connection: Keep-Alive
  ;;      then we assume there is no entity body
  ;;  
  ;;   if there is no Content-Length and no Connection: Keep-Alive
  ;;      then we read what follows and assumes that the body
  ;;	  (this is the http/0.9 behavior).
  ;;
  (let ((cl (request-header-content-length req)))
    (if* cl
       then (let ((len (content-length-value req))
		  (ret))
	      (setq ret (make-string len))
	      (let ((got-len (read-sequence ret sock)))
		(if* (not (eql got-len len))
		   then ; failed to get all the data
			nil
		   else (setf (body req) ret)
			t)))
       else ; no content length
	    (if* (not (equalp (request-header-connection req) "keep-alive"))
	       then (call-next-method) ; do the 0.9 thing
	       else ; there is no body
		    t))))


#+ignore
(defmethod read-entity-body ((req http-request-0.9) sock)
  ;; read up to end of file, that will be the body
  (let ((ans (make-array 2048 :element-type 'character
			 :fill-pointer 0))
	(ch))
    (loop (if* (eq :eof (setq ch (read-char sock nil :eof)))
	     then (setf (body req) ans)
		  (return t)
	     else (vector-push-extend ans ch)))))

	    

(defmethod get-request-body ((req http-request))
  ;; return a string that holds the body of the http-request
  ;;  cache it for later too
  (or (request-request-body req)
      (setf (request-request-body req)
	(if* (member (request-method req) '(:put :post))
	   then (multiple-value-bind (length believe-it)
		    (header-slot-value-integer req "content-length")
		  (if* believe-it
		     then ; we know the length
			  (prog1 (let ((ret (make-string length)))
				   (read-sequence-with-timeout 
				    ret length 
				    (request-socket req)
				    *read-request-body-timeout*))
	    
			    ; netscape (at least) is buggy in that 
			    ; it sends a crlf after
			    ; the body.  We have to eat that crlf.  
			    ; We could check
			    ; which browser is calling us but it's 
			    ; not clear what
			    ; is the set of buggy browsers 
			    (let ((ch (read-char-no-hang (request-socket req)
							 nil nil)))
			      (if* (eq ch #\return)
				 then ; now look for linefeed
				      (setq ch (read-char-no-hang 
						(request-socket req) nil nil))
				      (if* (eq ch #\linefeed)
					 thenret 
					 else (unread-char 
					       ch (request-socket req)))
			       elseif ch
				 then (unread-char ch (request-socket req)))))
				      
				      
		     else ; no content length given
			  (if* (equalp "keep-alive" 
				       (header-slot-value req "connection"))
			     then ; must be no body
				  ""
			     else ; read until the end of file
				  (mp:with-timeout 
				      (*read-request-body-timeout* 
				       nil)
				    (let ((ans (make-array 
						2048 
						:element-type 'character
						:fill-pointer 0))
					  (sock (request-socket req))
					  (ch))
				      (loop (if* (eq :eof 
						     (setq ch (read-char 
							       sock nil :eof)))
					       then (return  ans)
					       else (vector-push-extend ans ch))))))))
	   else "" ; no body
		))))



;; multipart code
;; used when enctype=multipart/form-data is used

(defstruct mp-info 
  ;; multipart information
  buffer 	; string buffer
  ebuf		; index after last valid char in buffer
  eline         ; index after last valid char not crlf in buffer
  left		; bytes left after buffer exhausted
  leftoff	; place to start again inside this buffer
  boundary	; boundary string
  socket	; socket stream connected to browser
  seen-end-of-segment	; reached the boundary
  seen-end-of-all-segments ; reached all the boundaries
  )



(defmethod start-multipart-capture ((req http-request))
  ;; begin the grab of the body.
  ;; user doesn't have to call this, it's called automatically
  (let* ((ctype (header-slot-value req "content-type"))
	 (parsed (and ctype (parse-header-value ctype)))
	 (boundary (and (equalp "multipart/form-data" (cadar parsed))
			(cdr (assoc "boundary" (cddar parsed) :test #'equal))))
	 (len (header-slot-value-integer req "content-length")))
    
    (if* (null boundary)
       then ; not in the right form, give up
	    (return-from start-multipart-capture nil))
    
    
    (setf (getf (request-reply-plist req) 'mp-info)
      (make-mp-info :buffer (get-request-buffer)
		    :ebuf 0		    :left len
		    ;; keep boundary case insensitive
		    :boundary (concatenate 'string "--" 
					   (string-downcase boundary))
		    :socket   (request-socket req)
		    ))))


    

(defmethod get-multipart-header ((req http-request))
  ;; return an alist holding the header info for the next request.
  ;; or nil if this is the end of the line
  
  (let ((mp-info (getf (request-reply-plist req) 'mp-info)))
    
    (if* (null mp-info)
       then (start-multipart-capture req)
	    ; satisify normal requests for the body with an empty string
	    (setf (request-request-body req) "") 
	    (setq mp-info (getf (request-reply-plist req) 'mp-info)))
    
    (if* (null mp-info)
       then ; no headers left
	    (return-from get-multipart-header nil))
    
    (if* (mp-info-seen-end-of-all-segments mp-info)
       then ; already at eof
	    (return-from get-multipart-header nil))
    
    ; read header lines
    (let ((headers)
	  (heads)
	  (buffer (mp-info-buffer mp-info)))
      (tagbody again
	(loop
	  (setf (mp-info-seen-end-of-segment mp-info) nil)
	  
	  (case (get-multipart-line mp-info)
	    ((:eof :boundary-end)
	     (setf (mp-info-seen-end-of-all-segments mp-info) t)
	     (return-from get-multipart-header nil))
	    
	    (:boundary (go again))
	    
	    (:data (if* (eq (mp-info-eline mp-info) 0)
		      then (return) ; end of headers
		      else (if* (member (schar buffer 0) '(#\space 
							   #\tab))
			      then ; header contintuation
				   (if* headers
				      then (setf (car headers)
					     (concatenate 'string
					       (car headers)
					       " "
					       (buffer-substr buffer 
							      0 
							      (mp-info-eline
							       mp-info)))))
			      else ; new header
				   (push (buffer-substr buffer 
							0 
							(mp-info-eline mp-info))
					 headers)))))))

      (setf (mp-info-seen-end-of-segment mp-info) nil)
      
      ; now parse headers
      (dolist (head headers)
	(let ((colonpos (find-it #\: head 0 (length head))))
	  (if* colonpos
	     then (let ((name (buffer-substr head 0 colonpos))
			(value (buffer-substr head (1+ colonpos)
					      (length head))))
		    (push (cons name
				(parse-header-value value))
			  heads)))))
      heads)))
	  
      
      

(defun get-multipart-line (mp-info)
  ;; read the next line into the give buffer
  ;; return a code describing the line
  (let* ((buffer (mp-info-buffer mp-info))
	 (len    (mp-info-left mp-info))
	 (i 0)
	 (size (length buffer))
	 (fd (mp-info-socket mp-info))
	 (ch))

    (if* (mp-info-seen-end-of-all-segments mp-info)
       then (return-from get-multipart-line :eof))
    
    (if* (mp-info-seen-end-of-segment mp-info)
       then (return-from get-multipart-line :boundary))
    
    (loop 
      (if* (and len (<= len 0))
	 then (setq ch 'eof)
	      (return))
    
      (if* (>= i size)
	 then ; no more buffer space
	      (return))
    
      (setq ch (read-char fd nil :eof))
      ;(if* (not (eq ch :eof)) then (write-char ch))
      (if* len then (decf len))
      (if* (eq :eof ch)
	 then (return) ; out of the loop
	 else (setf (schar buffer i) ch)
	      (incf i)
	      (if* (eq ch #\newline)
		  then (return))))
  
    ; out of the loop at end of line or end of file
    ; see if it matches the boundary
  
    (setf (mp-info-left mp-info) len)
    (setf (mp-info-ebuf mp-info) i)
    (setf (mp-info-leftoff mp-info) nil)
  
    (let ((eline i))
      ; find postition before the crlf, if any
      (if* (> eline 0)
	 then (if* (eq (schar buffer (1- eline)) #\newline)
		 then (decf eline)
		      (if* (and (> eline 0)
				(eq (schar buffer (1- eline)) #\return))
			 then (decf eline))))
      (setf (mp-info-eline mp-info) eline))
		    
		    
  
    (let ((boundary (mp-info-boundary mp-info)))
      (if* (>= i (length boundary))
	 then ; could match the boundary
	      (if* (dotimes (i (length boundary) t)
		     (if* (not (eq (char-downcase (schar buffer i))
				   (schar boundary i)))
			then (return nil)))
		 then ; matches the boundary at least, check for
		      ; end of boundary which is two extra hypens
		      ; at the end
		      (if* (>= i (+ 2 (length boundary)))
			 then (if* (and (eq #\- (schar buffer 
						       (length boundary)))
					(eq #\- (schar buffer 
						       (1+ (length boundary)))))
				 then (setf 
					  (mp-info-seen-end-of-all-segments 
					   mp-info) t)
				      :boundary-end
				 else (setf (mp-info-seen-end-of-segment 
					     mp-info) t)
				      :boundary)
			 else(setf (mp-info-seen-end-of-segment mp-info) t)
			      :boundary)
		 else :data)
       elseif (and (eq i 0) (eq ch :eof))
	 then (setf (mp-info-seen-end-of-all-segments mp-info) t)
	      :eof
	 else :data))))
      
		
    
(defmethod get-multipart-sequence ((req http-request)
				   buffer
				   &key (start 0) 
					(end (length buffer))
					(raw nil raw-p))
  ;; fill the buffer with the next line of data.
  ;; start at 'start' and go no farther than (1- end) in the buffer
  ;; return the index of the first character not place in the buffer.
  (let (mp-info text-grab)
    
    (typecase buffer
      ((array character (*))
       (setq text-grab t))
      ((array (unsigned-byte 8) (*))
       (setq text-grab nil 
	     raw (if* (not raw-p) then t else raw)))
      (t (error "get-multipart-sequence should be given a (simple-array character (*)) or (array (unsigned-byte 8) (*)), not this sequence: ~s" buffer)))
  
  
    (setq mp-info (getf (request-reply-plist req) 'mp-info))
    (if* (null mp-info)
       then ; haven't grabbed the header yet, do that and toss it, and
	    ; start the flow of information
	    (if* (null (get-multipart-header req))
	       then ; no data to read
		    (return-from get-multipart-sequence nil))
	    (setq mp-info (getf (request-reply-plist req) 'mp-info)))
  
    (if* (null (mp-info-leftoff mp-info))
       then (case (get-multipart-line mp-info)
	      ((:boundary :eof :boundary-end)
	       (return-from get-multipart-sequence nil))
	      (:data (setf (mp-info-leftoff mp-info) 0))))
  
    (do ((dest start (1+ dest))
	 (leftoff (mp-info-leftoff mp-info)
		  (1+ leftoff))
	 (eline   (mp-info-eline mp-info))
	 (ebuf    (mp-info-ebuf  mp-info))
	 (s-buffer  (mp-info-buffer mp-info)))
	((>= dest end)
	 ; ran out of buffer
	 (if* raw
	    then (if* (< leftoff ebuf)
		    then (setf (mp-info-leftoff mp-info) leftoff)
		    else (setf (mp-info-leftoff mp-info) nil))
	    else (if* (<= leftoff eline)
		    then (setf (mp-info-leftoff mp-info) leftoff)
		    else (setf (mp-info-leftoff mp-info) nil)))
	 dest)

      (if* raw
	 then (if* (< leftoff ebuf)
		 then (let ((ch (schar s-buffer leftoff)))
			(if* text-grab 
			   then (setf (aref buffer dest) ch)
			   else (setf (aref buffer dest) (char-code ch))))
		 else (setf (mp-info-leftoff mp-info) nil)
		      (return-from get-multipart-sequence dest))
	 else ; non raw, do eol processing
	      
	      (let (ch)
		(if* (< leftoff eline)
		   then (setq ch (schar s-buffer leftoff))
		   else (setq ch #\newline))
		;(format t "lo ~d  dest ~s  ch ~s~%" leftoff dest ch)
		(if* text-grab 
		   then (setf (aref buffer dest) ch)
		   else (setf (aref buffer dest) (char-code ch)))
		(if* (>= leftoff eline)
		   then (setf (mp-info-leftoff mp-info) nil)
			(return-from get-multipart-sequence (1+ dest))))))
    ))
  



;; end multipart code







		      
	      
(defun read-sequence-with-timeout (string length sock timeout)
  ;; read length bytes into sequence, timing out after timeout
  ;; seconds
  ;; return nil if things go wrong.
  (mp:with-timeout (timeout nil)
    (let ((got 0))
      (loop
	(let ((this (read-sequence string sock :start got)))
	  (if* (<= this 0)
	     then (return nil) ; eof too early
	     else (setq  got    this)
		  (if* (>= got length ) then (return string))))))))
		
    
      

(defun read-sock-line (sock buffer start)
  ;; read a line of data into the socket buffer, starting at start.
  ;; return  buffer and index after last character in buffer.
  ;; get bigger buffer if needed.
  ;; If problems occur free the passed in buffer and return nil.
  ;;
  
  (let ((max (length buffer))
	(prevch))
    (loop
      (let ((ch (read-char sock nil :eof)))
	(if* (eq ch :eof)
	   then (debug-format :info "eof on socket~%")
		(free-request-buffer buffer)
		(return-from read-sock-line nil))
      
	(if* (eq ch #\linefeed)
	   then (if* (eq prevch #\return)
		   then (decf start) ; back up to toss out return
			)
		(setf (schar buffer start) #\nul) ; null terminate
		
		; debug output
		; dump out buffer
		(debug-format :info "read on socket: ")
		(if-debug-action :info
				 (dotimes (i start)
				   (write-char (schar buffer i) *initial-terminal-io*))
				 (terpri *initial-terminal-io*))
		;; end debug
			
		(return-from read-sock-line (values buffer start))
	   else ; store character
		(if* (>= start max)
		   then ; must grow the string
			(let ((new-buffer (get-request-buffer (+ max 1024))))
			  (if* (null new-buffer)
			     then ;; too large, give up
				  (free-request-buffer buffer)
				  (return-from read-sock-line nil)
			     else ; got it
				  (dotimes (i start)
				    (setf (schar new-buffer i) 
				      (schar buffer i)))
				  (setq max (length new-buffer))
				  (free-request-buffer buffer)
				  (setq buffer new-buffer))))
		;  buffer is big enough
		(setf (schar buffer start) ch)
		(incf start))
	
	(setq prevch ch)))))
      
		      
				  
(defmethod request-query ((req http-request) &key (post t) (uri t))
  ;; decode if necessary and return the alist holding the
  ;; args to this url.  In the alist items the value is the 
  ;; cdr of the alist item.
  ;;
  ;; If uri is true then we look for query information in the uri
  ;; (following a question mark)
  ;; If post is true and this is a post request then we look for
  ;; query information in the body of the query.
  ;; If both are true (and this is a post) then we look both places.
  ;;
  ;;
  (let ((alist (request-query-alist req))
	(signature (cons post uri)))
    
    (if* (not (eq alist :empty))
       then (let ((given-sig (getf (request-reply-plist req) 
				   'request-query-sig)))
	      (if* (equal given-sig signature)
		 then ; same args as before, cached value is legit
		      (return-from request-query alist))))
    
    (let (res)
      (if* uri
	 then (let ((arg (uri-query (request-uri req))))
		(if* arg
		   then (setq res (form-urlencoded-to-query arg)))))
	      
      (if* post
	 then (if* (eq (request-method req) :post)
		 then (setf res
			(append res
				(form-urlencoded-to-query
				 (get-request-body req))))))
      (setf (getf (request-reply-plist req) 'request-query-sig)
	signature)
      (setf (request-query-alist req) res))))
			

(defun request-query-value (key req &key (post t) (uri t) (test #'equal))
  ;; access the value of the given key in the request's 
  ;; request query.  We do this so often that it's useful
  ;; to make this a function
  (cdr (assoc key (request-query req :post post :uri uri) :test test)))


	

(defun header-decode-integer (val)
  ;; if val is a string holding an integer return its value
  ;; and t,
  ;; else nil
  (if* val 
     then (let (ans)
	    (ignore-errors (setq ans (read-from-string val)))
	    (if* (integerp ans)
	       then (values ans t)))))


(defun date-to-universal-time (date)
  ;; convert  a date string to lisp's universal time
  ;; we accept all 3 possible date formats

  (flet ((cvt (str start-end)
	   (let ((res 0))
	     (do ((i (car start-end) (1+ i))
		  (end (cdr start-end)))
		 ((>= i end) res)
	       (setq res 
		 (+ (* 10 res)
		    (- (char-code (schar str i)) #.(char-code #\0))))))))
    ;; check preferred type first (rfc1123 (formerly refc822)):
    ;;  	Sun, 06 Nov 1994 08:49:37 GMT
    (multiple-value-bind (ok whole
			  day
			  month
			  year
			  hour
			  minute
			  second)
	(match-regexp 
	 "[A-Za-z]+, \\([0-9]+\\) \\([A-Za-z]+\\) \\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\) GMT"
	 date
	 :return :index)
      (declare (ignore whole))
      (if* ok
	 then (return-from date-to-universal-time
		(encode-universal-time
		 (cvt date second)
		 (cvt date minute)
		 (cvt date hour)
		 (cvt date day)
		 (compute-month date (car month))
		 (cvt date year)
		 0))))
    
    ;; now second best format (but used by Netscape sadly):
    ;;		Sunday, 06-Nov-94 08:49:37 GMT
    ;;
    (multiple-value-bind (ok whole
			  day
			  month
			  year
			  hour
			  minute
			  second)
	(match-regexp
	 
	 "[A-Za-z]+, \\([0-9]+\\)-\\([A-Za-z]+\\)-\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\) GMT"
	 date
	 :return :index)
      
      (declare (ignore whole))
      
      (if* ok
	 then (return-from date-to-universal-time
		(encode-universal-time
		 (cvt date second)
		 (cvt date minute)
		 (cvt date hour)
		 (cvt date day)
		 (compute-month date (car month))
		 (cvt date year) ; cl does right thing with 2 digit dates
		 0))))
    
    
    ;; finally the third format, from unix's asctime
    ;;     Sun Nov  6 08:49:37 1994
    (multiple-value-bind (ok whole
			  month
			  day
			  hour
			  minute
			  second
			  year
			  )
	(match-regexp
	 
	 "[A-Za-z]+ \\([A-Za-z]+\\) +\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\) \\([0-9]+\\)"
	 date
	 :return :index)
      
      (declare (ignore whole))
      
      (if* ok
	 then (return-from date-to-universal-time
		(encode-universal-time
		 (cvt date second)
		 (cvt date minute)
		 (cvt date hour)
		 (cvt date day)
		 (compute-month date (car month))
		 (cvt date year)
		 0))))
      
      
    ))
    
    
    
    
	  
(defun compute-month (str start)
  ;; return the month number given a 3char rep of the string
  
  (case (schar str start)
    (#\A  
     (if* (eq (schar str (1+ start)) #\p)
	then 4 ; april
	else 8 ; august
	     ))
    (#\D 12) ; dec
    (#\F 2 ) ; feb
    (#\J
     (if* (eq (schar str (1+ start)) #\a)
	then 1 ; jan
      elseif (eq (schar str (+ 2 start)) #\l)
	then 7 ; july
	else 6 ; june
	     ))
    (#\M
     (if* (eq (schar str (+ 2 start)) #\r)
	then 3 ; march
	else 5 ; may
	     ))
    (#\N 11) ; nov
    (#\O 10)  ;oct
    (#\S 9) ; sept
    ))
    
     

(defun maybe-universal-time-to-date (ut-or-string)
  ;; given a ut or a string, only do the conversion on the string
  (if* (stringp ut-or-string) 
     then ut-or-string
     else (universal-time-to-date ut-or-string)))

(defun universal-time-to-date (ut)
  ;; convert a lisp universal time to rfc 1123 date
  ;;
  (let ((*print-pretty* nil))
    (multiple-value-bind
	(sec min hour date month year day-of-week dsp time-zone)
	(decode-universal-time ut 0)
      (declare (ignore time-zone dsp))
      (format nil "~a, ~2,'0d ~a ~d ~2,'0d:~2,'0d:~2,'0d GMT"
	      (svref
	       '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
	       day-of-week)
	      date
	      (svref
	       '#(nil "Jan" "Feb" "Mar" "Apr" "May" "Jun"
		  "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
	       month
	       )
	      year
	      hour
	      min
	      sec))))

	    


;; ----- scratch buffer resource:
 
(defvar *rq-buffers* nil)
(defparameter *max-buffer-size* #.(* 5 1024)) ; no line should be this long
    
(defun get-request-buffer (&optional size)
  ;; get a string buffer.
  ;; if size is important, it is specified.
  ;; if the size is too great, then refuse to create one and return
  ;; nil
  (mp:without-scheduling 
    (if* size
       then ; must get one of at least a certain size
	    (if* (> size *max-buffer-size*)
	       then (return-from get-request-buffer nil))
	    
	    (dolist (buf *rq-buffers*)
	      (if* (>= (length buf) size)
		 then (setq *rq-buffers* (delete buf *rq-buffers* :test #'eq))
		      (return-from get-request-buffer  buf)))
	    
	    ; none big enough
	    (make-array size :element-type 'character)
       else ; just get any buffer
	    (if* (pop *rq-buffers*)
	       thenret
	       else (make-array 2048 :element-type 'character)))))


(defun free-request-buffer (buffer)
  ;; return buffer to the free pool
  (mp:without-scheduling (push buffer *rq-buffers*)))


	    
;;-----------------


(defun string-to-number (string start end)
  ;; convert the string into a number.
  ;; the number is decimal
  ;; this is faster than creating a string input stream and
  ;; doing a lisp read
  ;; string must be a simple string
  (let ((ans 0))
    (do ((i start (1+ i)))
	((>= i end)
	 ans)
      (let ((digit (- (char-code (schar string i)) #.(char-code #\0))))
	(if* (<= 0 digit 9)
	   then (setq ans (+ (* ans 10) digit))
	   else (return ans))))))

		
	
;;-------------------
;; authorization

(defmethod get-basic-authorization ((req http-request))
  ;; return the basic authorization information for this request, if any
  ;; 
  ;; basic authorization is used when a name/password dialog is
  ;; put up by the browser
  ;;
  ;; if authorization info in found this request, return two values
  ;;  name
  ;;  password
  ;;
  (let ((auth-value (header-slot-value req "authorization")))
    (if* auth-value
       then (let ((words (split-into-words auth-value)))
	      (if* (equalp (car words) "basic")
		 then (setq auth-value 
			(split-on-character (base64-decode (cadr words)) #\:))
		      (values-list auth-value))))))
		      
	      
(defmethod set-basic-authorization ((req http-request) realm)
  ;; declare that you want to get authentication information
  ;; for the given realm.
  ;; This must be called after with-http-response and before
  ;; with-http-body
  (setq realm (string realm))
  (setf (reply-header-slot-value req "www-authenticate")
    (format nil "Basic realm=~s" realm)))
    


;=======

(defun bulk-set-reply-headers (req headers)
  ;; given an alist list of headers to set, set the header info
  ;; in the correct place (given fast vrs slow headers)
  (let ((fast-headers *fast-reply-headers*)
	(current-headers (request-reply-headers req)))
    (dolist (header headers)
      (let ((this (car header))
	    (ent))
	(if* (setq ent (assoc this fast-headers :test #'equal))
	   then ; a fast one
		(setf (slot-value req (second ent)) (cdr header))
	   else ; a slow one
		(if* (null (setq ent (assoc this 
					    current-headers :test #'equal)))
		   then ; not present yet
			(push (setq ent (cons this nil)) current-headers))
		(setf (cdr ent) (cdr header)))))
    (setf (request-reply-headers req) current-headers)))
	

(defun code-to-response (code)
  ;; return response object for the given code
  (let ((obj (find code *responses* :key #'response-number)))
    (if* (null obj)
       then (push (setq obj (make-resp code "unknown code")) *responses*))
    obj))
  
