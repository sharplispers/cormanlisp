;;
;; chat.cl
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
;; $Id: chat.cl,v 1.5 2000/08/04 16:05:01 jkf Exp $

;; Description:
;;   aserve chat program

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-



(defpackage :user (:use :net.aserve :excl :common-lisp :net.uri
			:net.aserve.client
			:net.html.generator))
(in-package :user)

(defvar *chat-home-package* :user) ; :user for now while debugging
(defvar *chat-home*)     ; home dir for chat info
(defvar *default-count* 10)
(defvar *default-secs*  10)

(defvar *do-dnscheck* nil) ; translate ip to dns names

(defvar *chat-hook* nil) ; invoked when the chat page is accessed

;; parameters
;
; one set of of paraamter is the page style of the top frame
;   call (set-style xxx) where xxx is one of the *xxx-style* values
;  (set-style *normal-style*)
;  (set-style *white-style*)
;
; setting *background-image* to an image url will put that url on
; the background of the top window
; e.g.
;  (setq *background-image* "http://www.franz.com/~jkf/aserveback4.gif")
;  (setq *background-image* nil)
; set *recent-first* to true to make the newest messages show first
;
; set *show-style* to 1 for normal, 2 for tables
; (setq *show-style* 1)
; (setq *show-style* 2)
;



(defparameter *bottom-frames-bgcolor* "#dddddd") ; gray
(defparameter *bottom-frames-private* "#ff5555") ; for private messaging

(defparameter *private-font-color*  "#ff4444") ; red
(defparameter *public-font-color* "#ffcc66") ; gold

(defstruct color-style
  bgcolor
  font-color
  vlink-color
  link-color
  alink-color)

(defparameter *normal-style* 
    (make-color-style 
     :bgcolor 		"#000000" ; black
     :font-color        "#ffcc66" ; gold
     :vlink-color 	"#ffaaaa" ; red
     :link-color 	"#aaffaa" ; green
     :alink-color       "#aaaaff" ; blue
     ))

(defparameter *white-style* 
    (make-color-style 
     :bgcolor 		"#ffffff" ; white
     :font-color        "#000000" ; black
     :vlink-color 	"#ff0000" ; red
     :link-color 	"#0000ff" ; blue
     :alink-color       "#00aa00" ; green
     ))



(defvar *top-frame-bgcolor* )
(defvar *top-frame-font-color*)
(defvar *top-frame-vlink-color*)
(defvar *top-frame-link-color*)
(defvar *top-frame-alink-color*)

(defvar *background-image* nil)

(defvar *max-active-time* #.(* 2 60)) ; after 2 minutes no longer active


(defvar *recent-first* t)   ; if true show most recent messages first

(defvar *show-style* 1)     ; 1 = tables, 2 = just entries

;
; query attribute usage:
;  u = controller ustring
;  c = chat ustring
;  s = secret key (specific to the object being accessed)
;  x = user uid
;  pp = uid of person sending message to, * means all
;  purl = picture url
;  z = lurk
;  y = delete message


(defclass master-chat-controller ()
  ((controllers :initform nil
		; list of chat-controller instances
		:initarg :controllers
		:accessor controllers)
   (ustrings :initform nil 
	     :initarg :ustrings
	     :accessor ustrings)
   (master-lock :initform (mp:make-process-lock :name "chat master")
		;; used when doing serious altering to chat info
		:reader master-lock)
   (secret-key :initarg :secret-key
	       ;; to unlock the setup-chat
	       :initform (make-unique-string)
	       :reader secret-key)
   (users :initform nil
	  :initarg :users
	  ;; list of user objects
	  :accessor users)
   ))


(defvar *master-controller* nil) ; the master-controller instance



(defclass chat-controller ()
  ;; describes a whole set of chats
  
  ((chats :initform nil
	  ; list of chat instances
	  :initarg :chats
	  :accessor chats)
   (owner-name :initarg :owner-name
	       :reader owner-name)
   (controller-name :initarg :controller-name
		    :reader controller-name)
   (ustring :initarg :ustring :accessor ustring) ; un
   (ustrings :initform nil
	     ;; ustrings of all the chats
	     :initarg :ustrings
	     :accessor ustrings)
   (secret-key :initarg :secret-key
	       ;; knowing this key gives you access to 
	       ;; changing the parameters of the chat
	       :accessor secret-key)
   (controller-uri :initarg :controller-uri
		   ;; uri to reach this controller page
		   :accessor controller-uri)
   (controller-query-string :initarg :controller-query-string
			    ; u=xxxxx&s=xxxxx  specify this controller and
			    ;	the secret key for this controller
			    :reader controller-query-string)
   ))
   
   

(defclass chat ()
  ((name :initarg :name
	 :reader chat-name
	 )
   
   (state :initform :open
	  ; :open or :closed
	  :initarg :state
	  :accessor chat-state)
   
   (ustring :initarg :ustring
	    :accessor ustring)

   (filename :initarg :filename
	     ;; name of file holding chat info.
	     ;; should be just a name, no directory stuff, so
	     ;; it can be relative to the chat home
	     :accessor chat-filename)
   
   (secret-key :initarg :secret-key
	       ;; to do admin things to this chat
	       :initform (make-unique-string)
	       :reader secret-key)
   
   (chat-query-string :initarg :chat-query-string
		      ;; u=xxxx&c=yyyyyy  indentifies chat
		      :reader chat-query-string)
   (chat-owner-query-string :initarg :chat-owner-query-string
		      ;; u=xxxx&c=yyyyyy&s=xxxx  indentifies chat
			    :reader chat-owner-query-string)
   
   
   (messages :initform (make-array 100)
	     :accessor chat-messages)
   (message-next :initform 0
		 ;; index in the messages array of place
		 ;; to store next message
		 :accessor chat-message-next)
   (message-number :initform 0
		   :initarg :message-number
		   ;; message number of the next message
		   :accessor chat-message-number)
   (message-archive :initform 0
		    :initarg :message-archive
		     ;; 1+ last message number archived so far
		     :accessor chat-message-archive)
   
   
   (message-lock :initform (mp:make-process-lock :name "chat-message")
		 ; grab this before changing the above
		 :accessor chat-message-lock)
   

   ;; list of people monitoring this chat
   (viewers :initform (make-viewers)
	    :accessor chat-viewers)
   ))

(defstruct user 
  handle	; official handle of the user
  password	; password string
  ustring		; unique string of this user
  )


(defstruct viewers 
  (lock (mp:make-process-lock :name "viewers-lock"))
  list	; list of viewent
  )

(defstruct viewent
  time	; time of last read, nil to make this one unused
  user	; if user access, then user object
  ipaddr ; if random access then ipaddr
  hostname ; string if we've figured it out
  )



(defstruct message
  number  ; is unique for each chat
  ipaddr  ; integer ip address of the source
  dns     ; dns name corresponding to the ip address
  handle  ; from handle (for unlogged in user)
  real    ; true if this is a real handle of a logged in user
  to	  ; if non nil then a list of uids who are the target of this message
          ; if nil then this goes to no-one
          ; if t then this goes to everyone
  time
  body)


;; roles
; master-controller - can create controllers.  has a secret key (s)
; controller - can create chats, each has a public key (u) and
;	       a private key (s).  
; chat - is a collection of posted messages.  has a public key (c)
;	 and a controller public key (u) and a secret key (s)
;	 Most access the chat using u and c.  If you also know s then
;	 you have owner priviledges to the chat
;


;; pages
;
; url		set		what
;
; setup-chat	-		if no chat setup yet, bring up first page
;				with link to first controller page page
; setup-chat	s		s has master control key, bring up page of
;				existing chat controllers and form for
;				craeting new one.  This is the master controller
;				page.
; new-controller s,name,controllername
;				posted from setup-chat
;				s is the master controller secret key
;				name and controllername are used to build
;				new controller object.
; controller	u,s		u has controller public string, s has
;				controller private string, show chats by
;				this controller and offer to create a new one
; create-chat	u,s,name,filename   create a new chat under the controller
;				denoted by u.  s has the controller private
;				string (secret key)
; chat		u,c,[s]		build frameset for the given chat.
;				s is the chat secret key [if given.]
; chatlogin	u,c,x,[s]	login to a existing user or create a new user
;
; chatloginnew  u,c,[s],handle,password,password2
;				define new user
;
; chatlogincurrent u,c,[s],handle,password
;				login as an existing user
;
; 
;
;


; top level published urls




; functions
(defun start-chat (&key port home restart (listeners 10))
  ;; start the chat system going
  (declare (special socket::*dns-configured*))
  
  (unpublish :all t) ; useful during debugging, remove afterwards
  
  (if* (not (stringp home))
     then (error "must specify :home value as a string naming a directory (no trailing slash)"))
  
  (setq *chat-home* home)
  
  
  
  (setq *master-controller* nil)
  
  (if* (not restart)
     then (load-existing-chat *chat-home*)
	  )
  
  (if* *master-controller*
     then ; we have an existing chat setup
	  (publish-chat-links)
	  (start-chat-archiver *master-controller*)
	  )
  
  (publish :path "/setup-chat" :function 'setup-chat)

  ; setup for reverse dns lookups.  don't do reverse lookups if we
  ; have to use the C library
  #+(version>= 6 0)
  (if* (and (boundp 'socket::*dns-configured*)
	    socket::*dns-configured*)
     thenret
     else (socket:configure-dns :auto t)
	  (setq *do-dnscheck* socket::*dns-configured*
		socket::*dns-mode* :acldns))
  
  
  (if* port then (net.aserve:start :port port :listeners listeners))
  )

(defun publish-chat-links ()

  ; debugging only.  builds link to the master controller page 
  (publish :path "/xyzz" :function 'quick-return-master)
  
  
  ; post'ed from form in setup-chat
  (publish :path "/new-controller" :function 'new-controller)

  (publish :path "/controller" :function 'existing-controller)

  ; get'ed from the controller page when user asks to create a chat
  (publish :path "/create-chat" :function 'create-chat)


  (publish :path "/chat"  :function 'chat)
  (publish :path "/chattop" :function 'chattop)

  (publish :path "/chatenter" :function 'chatenter)

  (publish :path "/chatcontrol" :function 'chatcontrol)
  
  (publish :path "/chatlogin" :function 'chatlogin)
  
  (publish :path "/chatloginnew" :function 'chatloginnew)
  
  (publish :path "/chatlogincurrent" 
	   :function 'chat-login-current)
  
  (publish :path "/chatviewers" :function 'chatviewers)
  )


(defun load-existing-chat (home)
  ;; read in and build the chat information
  (declare (special user::value1))
  
  (let ((master-file (concatenate 'string home "/cmaster.cl")))
    (if* (probe-file master-file)
       then (load master-file)
	    (if* (boundp 'user::value1)
	       then (setq *master-controller* user::value1))
	    ;; now read in chat data
	    (dolist (controller (controllers *master-controller*))
	      (dolist (chat (chats controller))
		(with-open-file (p (archive-filename chat)
				 :direction :input)
		  (do ((message (read p nil :eof) (read p nil :eof)))
		      ((eq message :eof)
		       ; use everything is archived we've read
		       (setf (chat-message-archive chat) 
			 (chat-message-number chat))
		       )
		    (if* message
		       then (add-chat-message chat message)))))))))
    
(defun dump-existing-chat (home)
  (mp:with-process-lock ((master-lock *master-controller*))
    (labels ((dump-master-chat-controller (masterc)
	       `(make-instance 'master-chat-controller
		  :ustrings ',(ustrings masterc)
		  :secret-key ',(secret-key masterc)
		  :controllers
		  (list ,@(mapcar #'dump-chat-controller 
				  (controllers masterc)))
		  :users ',(users masterc)
		  ))
	     
	     (dump-chat-controller (controller)
	       `(make-instance 'chat-controller
		  :chats
		  (list ,@(mapcar #'dump-chat (chats controller)))
		  :owner-name ',(owner-name controller)
		  :controller-name ',(controller-name controller)
		  :ustring ',(ustring controller)
		  :ustrings ',(ustrings controller)
		  :secret-key ',(secret-key controller)
		  :controller-uri ',(controller-uri controller)
		  :controller-query-string
		  ',(controller-query-string controller)))
	   
	     (dump-chat (chat)
	       `(make-instance 'chat
		  :name ',(chat-name chat)
		  :state ',(chat-state chat)
		  :ustring ',(ustring chat)
		  :filename ',(chat-filename chat)
		  :secret-key ',(secret-key chat)
		  :chat-query-string ',(chat-query-string chat)
		  :chat-owner-query-string ',(chat-owner-query-string chat)
		  ))
	     
	     )
	     
				 
      (let ((new-master-file (concatenate 'string home "/ncmaster.cl"))
	    (master-file (concatenate 'string home "/cmaster.cl"))
	    (value))

	(setq value
	  `(setq user::value1
	     ,(dump-master-chat-controller *master-controller*)))
			
	(with-open-file (p new-master-file 
			 :direction :output
			 :if-exists :supersede)
	  (let ((*package* (find-package *chat-home-package*)))
	    (format p ";;Automatically generated, do not edit~%")
	    (print `(in-package ,*chat-home-package*) p)
	    (pprint value p)
	    (terpri p)))
    
	; success, so make it the official one
	(ignore-errors (delete-file master-file))
	(rename-file new-master-file master-file)))))


    
    
      
      
      
  

(defun quick-return-master (req ent)
  ;; quick hack to get us to the master controller while debugging
  (if* (null *master-controller*)
     then (ancient-link-error req ent)
     else (with-http-response (req ent)
	    (with-http-body (req ent)
	      (html 
	       (:html
		(:body "The master controllers is "
		       ((:a href 
			    (format nil "setup-chat?s=~a"
				    (secret-key *master-controller*)))
			"here"))))))))
			   


(defun setup-chat (req ent)
  ;; this is the first function called to start a whole chat
  ;; system going (building a master controller) and is also
  ;; the function used by the master controller to specify new
  ;; controllers.
  (if* (null *master-controller*)
     then (setq *master-controller* (make-instance 'master-chat-controller))
	  (dump-existing-chat *chat-home*)
	  (do-first-setup-page req ent)
	  (start-chat-archiver *master-controller*)
   elseif (not (equal (secret-key *master-controller*) 
		      (request-query-value "s" req)))
     then (with-http-response (req ent)
	    (with-http-body (req ent)
	      (html
	       (:html 
		(:head (:title "illegal access"))
		(:body "You are attempting to gain illegal access to this "
		       "chat control.  Stop doing this.")))))
		       
	    
     else (with-http-response (req ent)
	    (with-http-body (req ent)
	      (html (:head (:title "Chat Setup"))
		    (:body (:h1 "Chat Setup")
			   
			   (if* (controllers *master-controller*)
			      then (html (:h2 "Existing Chat Controllers")
					 (display-chat-controllers 
					  (controllers *master-controller*))))
			   
			   (:h2 "New Chat Controller")
			   " This page is used to create a chat controller which "
			   "then can be use to create chats."
			   " Just fill out the form below and click on submit "
			   " and you'll be taken to a new controller page. "
			   ((:form :action "new-controller"
				   :method "POST")
			    ((:input :type "hidden"
				     :name "s" 
				     :value (secret-key *master-controller*)))
			    ((:input :type "text"
				     :name "name"
				     :size 30
				     :maxlength 30))
			    "Your Name"
			    :br
		    
			    ((:input :type "text"
				     :name "controllername"
				     :size 30
				     :maxlength 30))
			    "Name for this collection of chats"
			    :br
		   
		    
			    ((:input :type "submit")))))))))


(defun display-chat-controllers (controllers)
  ;; display a table of chat controllers
  (html 
   ((:table :border "1" :cellspacing 1 :cellpadding 3)
    ((:tr :bgcolor "#9999ff")
     (:th "Owner Name")
     (:th "Collection Name")
     (:th "Link"))
    (dolist (controller controllers)
      (html (:tr (:td (:princ-safe (owner-name controller)))
		 (:td (:princ-safe (controller-name controller)))
		 (:td ((:a :href (format nil "controller?~a"
					 (controller-query-string 
					  controller)))
		       "Go To Page"))))))))
		      
(defun do-first-setup-page (req ent)
  ;; called when setup-chat is done for the first time 
  ;; gives the special url that can be used by the chat superadmin
  ;; to give chat controllers to others
  
  (publish-chat-links)
  
  (with-http-response (req ent)
    (with-http-body (req ent)
      (html
       (:html
	(:head (:title "First Setup"))
	(:body (:h1 "First Setup")
	       "This is the first access to this chat setup and you "
	       "are now the chat super-adminstrator."
	       " This "
	       ((:a href 
		    (format nil "setup-chat?s=~a"
			    (secret-key *master-controller*)))
		"link")
	       " will take you to a page where you can create chat"
	       "controller who then can create chats"
	       " Once you follow the link to the page be sure to bookmark "
	       " the page since this will be the only way to "
	       " exert your superadminstrator powers.")))))
  
  )
	       



(defun new-controller (req ent)
  
  (if* (or (not (eq (request-method req) :post))
	   (not (equal (secret-key *master-controller*) 
		       (request-query-value "s" req))))
     then ; someone's playing around
	  (return-from new-controller
	    (ancient-link-error req ent)))
	    
  (with-http-response (req ent)
    (let ((query (request-query req)))
      (let ((controller 
	     (new-chat-controller
	      :owner-name (cdr (assoc "name" query :test #'equalp))
	      :controller-name (cdr (assoc "controllername" query 
					   :test #'equalp))
	      :secret-key (make-unique-string))))
	(mp:with-process-lock ((master-lock *master-controller*))
	  (push controller (controllers *master-controller*)))

	(dump-existing-chat *chat-home*)
	(with-http-body (req ent)
	  (html 
	   (:html
	    (:head (:title "Created New Controller"))
	    (:body
	     "A new controller page has been created, go to "
	     ((:a :href (format nil "controller?~a"
				(controller-query-string 
				 controller)))
	      "here")
	     " to see the page"))))))))

(defun existing-controller (req ent)
  ;; when an owner visits his control page
  (let ((controller (controller-from-req req)))
    (if* (or (null controller)
	     (not (equal (secret-key controller)
			 (cdr (assoc "s" (request-query req) 
				     :test #'equalp)))))
       then (ancient-link-error req ent)
       else (with-http-response (req ent)
	      (with-http-body (req ent)
		(display-controller-page controller))))))
  


(defun display-controller-page (controller)
  ;; display the html for the controller page
  (html 
   (:html 
    (:head (:title "Controller for " 
		   (:princ-safe (controller-name controller))))
    (:body 
     (:h1 "Controller for " 
	  (:princ-safe (controller-name controller)))
     (:h2 "Owner is " (:princ-safe
		       (owner-name controller)))
     (if* (null (chats controller))
	then (html (:h2 "There are no chats defined yet"))
	else (display-chat-list (chats controller) t))
		       
     ((:form :action 
	     (concatenate 'string
	       "create-chat?"
	       (controller-query-string controller))
			       
	     :method "POST")
      :hr
      (:h2 "Create New Chat")
      ((:input :type "text"
	       :name "name"
	       :size 30)
       "  Enter Name for Chat")
      :br
      ((:input :type "text"
	       :name "filename"
	       :value (format nil "chat-~a.txt" (make-unique-string))
	       :size 30))
       "  File where messages are stored"
      :br
      ((:input :type "submit"
	       :value "Create Chat")))))))
			       
		       
	   


(defun display-chat-list (chats owner-p)
  ;; display the characteristics of the chats in a table
  (html ((:table :border "1" :cellspacing 1 :cellpadding 3)
	 ((:tr :bgcolor "#9999ff")
	  (:th "Chat name")
	  (:th "State")
	  (:th "Link")
	  (if* owner-p
	     then (html (:th "Owner Link")))
	  )
	 (dolist (chat chats)
	   (html (:tr
		  (:td (:princ-safe (chat-name chat)))
		  (:td (:princ-safe (chat-state chat)))
		  (:td
		   ((:a :href (concatenate 'string
				"chat?"
				(chat-query-string chat)))
		    "Go to Chat"))
		  (if* owner-p
		     then (html (:td
				 ((:a :href (concatenate 'string
					      "chat?"
					      (chat-owner-query-string chat)))
				  "Go to Chat as owner"))))))))))
    
(defun new-chat-controller (&key owner-name controller-name secret-key)
  ;; create a new chat controller object
  (let (ustring)
    
    ; create a unique string to indentify this controller
    (loop
      (setq ustring (make-unique-string))
      (mp:without-scheduling
	(if* (not (member ustring 
			  (ustrings *master-controller*)
			  :test #'equal))
	   then (push ustring (ustrings *master-controller*))
		(return))))
    
    (let ((controller (make-instance 'chat-controller
			:owner-name owner-name
			:controller-name controller-name
			:secret-key secret-key
			:ustring ustring
			:controller-uri (compute-controller-uri ustring)
			:controller-query-string
			(format nil "u=~a&s=~a" 
				ustring
				secret-key))))
      controller)))

      
      
(defun compute-controller-uri (ustring)
  (format nil "controller?u=~a" ustring))


(defun make-unique-string ()
  ;; make a unique string that's not one of the other strings
  ;; want it to around five characters long
  
  (let ((time (get-universal-time)))
    ; randomize things
    (dotimes (i (logand time #xf)) (random 10))
    (dotimes (i (logand time #x1f)) (random 10))
    (setq time (logxor time (random 11881376)))
    (setq time (logxor time (random
			     (load-time-value
			      (get-universal-time)))))
    ; make sure it's at least 5 digits base 26
    (if* (< time #.(expt 26 5))
       then (incf time #.(expt 26 5)))
    ;
    (string-downcase (format nil "~26r" time))))



(defun create-chat (req ent)
  ;; create a new chat for the given controller
  (let ((controller (controller-from-req req)))
    (if* (or (null controller)
	     (not (equal (secret-key controller)
			 (request-query-value "s" req))))
       then (ancient-link-error req ent)
       else (let (ustring)
	      (loop
		(setq ustring (make-unique-string))
		(mp:without-scheduling
		  (if* (not (member ustring (ustrings controller) 
				    :test #'equal))
		     then (push ustring (ustrings controller))
			  (return))))
      
	      (let ((chat (make-new-chat controller
					 :name (request-query-value "name" req)
					 :filename 
					 (request-query-value "filename" req)
					 :ustring ustring)))
		(mp:without-scheduling
		  (push chat (chats controller)))
		(dump-existing-chat *chat-home*)
		(with-http-response (req ent)
		  (with-http-body (req ent)
		    (display-controller-page controller))))))))

(defun ancient-link-error (req ent)
  (with-http-response (req ent)
    (with-http-body (req ent)
      (html "This link is ancient and won't work any more"))))


(defun controller-from-req (req)
  ;; locate controller named by request
  (let ((ustring (request-query-value "u" req)))
    (if* ustring
       then (dolist (controller (controllers *master-controller*))
	      (if* (equal ustring (ustring controller))
		 then (return controller))))))

(defun chat-from-req (req)
  ;; find the chat object given the req
  (let ((controller (controller-from-req req)))
    (if* controller
       then (let ((chat-ustring (cdr (assoc "c" (request-query req)
					    :test #'equalp))))
	      (if* chat-ustring
		 then (dolist (chat (chats controller))
			(if* (equal chat-ustring (ustring chat))
			   then (return chat))))))))

(defun user-from-req (req)
  ;; find the user object from this request
  (let ((val (request-query-value "x" req)))
    (if* val
       then (user-from-ustring val))))



(defun user-from-ustring (ustring)
  ;; find user object based on unique string
  (find ustring (users *master-controller*)
	:key #'user-ustring :test #'equal))


(defun make-new-chat (controller &key name filename ustring)
  ;; make a new chat object
  (let ((secret-key (make-unique-string)))
    (make-instance 'chat 
      :name name
      :ustring ustring
      :filename filename
      :secret-key secret-key
      :chat-query-string (format nil "u=~a&c=~a"
				 (ustring controller)
				 ustring)
      :chat-owner-query-string 
      (format nil "u=~a&c=~a&s=~a"
	      (ustring controller)
	      ustring
	      secret-key)
      :secret-key secret-key)))



; chat frames:
;
;  chattop 
;  chatviewers chatenter chatcontrol

(defun chat (req ent)
  ;; generate the chat frames
  (format t "start chat~%") (force-output)
  (let ((chat (chat-from-req req))
	(user (user-from-req req))
	(qstring))
    
    (if* *chat-hook*
       then (if* (funcall *chat-hook* req ent)
	       then (return-from chat)))
    
    
    (if* (null chat)
       then (ancient-link-error req ent)
       else (setq qstring 
	      (add-lurk
	       req
	       (add-secret req
			   (add-user req (chat-query-string chat)))))
	    
	    (format t "qstring ~s~%" qstring) (force-output)
	    (with-http-response  (req ent)
	      (with-http-body (req ent)
		(html 
		 (:html
		  (:head (:title "chat - "
				 (:princ-safe (chat-name chat))))
		  
		  ((:frameset :rows "*,160")
		   ((:frame :src 
			    (format nil "chattop?~a&count=~d&secs=~d"
				    qstring
				    *default-count*
				    *default-secs*)
			    :name "chattop")
		    ((:frameset :cols 
				(if* user 
				   then "15%,*,20%"
				   else "*,20%"))
		     (if* user 
			then (html ((:frame :src
					    (concatenate 'string
					      "chatviewers?"
					      qstring)))))
		     ((:frame :src
			      (concatenate 'string
				"chatenter?"
				qstring)
			      :name "chatenter"))
		     ((:frame :src
			      (concatenate 'string
				"chatcontrol?"
				qstring))))
		    (:noframes
		     "This chat program requires a browser that supports frames"
		     ))))))))))




	      
	
    
(defun add-user (req current-string)
  ;; if a user has been specified in the chat
  ;; the add it's x string to the current string
  (let ((val (request-query-value "x" req)))
    (if* val
       then (format nil "~a&x=~a" current-string val)
       else current-string)))

(defun add-secret (req current-string)
  ;; if a secret string has been defined then add it onto the 
  ;; current string
  (let ((val (request-query-value "s" req)))
    (if* val
       then (format nil "~a&s=~a" current-string val)
       else current-string)))

(defun add-reverse (req current-string)
  ;; if a reverse value has been defined then add it onto the 
  ;; current string
  (let ((val (request-query-value "rv" req)))
    (if* val
       then (format nil "~a&rv=~a" current-string val)
       else current-string)))

(defun add-lurk (req current-string)
  ;; if a lurk has been defined then add it onto the 
  ;; current string
  (let ((val (request-query-value "z" req)))
    (if* val
       then (format nil "~a&z=~a" current-string val)
       else current-string)))

(defun chattop (req ent)
  ;; put out the top part of the chat
  (let* ((chat (chat-from-req req))
	 (user (user-from-req req))
	 (is-owner
	  (equal (and chat (secret-key chat)) 
		 (request-query-value "s" req)))
	 (qstring))
    
    (if* (null chat)
       then (return-from chattop (ancient-link-error req ent)))

    (let ((delete (request-query-value "y" req)))
      (if* delete
	 then (delete-chat-message chat (compute-integer-value delete))))
    
    (let* ((count (or (compute-integer-value
		       (request-query-value "count" req))
		      10))
	   (secs  (or (compute-integer-value
		       (request-query-value "secs" req))
		      0)))
      
      (if* (null (request-query-value "z" req))
	 then (track-viewer chat user req))
      
      (with-http-response (req ent)
	(setq qstring 
	  (format nil "~a&count=~d&secs=~d"
		  (add-lurk
		   req
		   (add-reverse 
		    req
		    (add-secret 
		     req
		     (add-user 
		      req 
		      (chat-query-string chat)))))
		  count 
		  secs))
	(with-http-body (req ent)
	  (html 
	   (:html
	    (:head
	     (:title "chat frame")
	     (if* (and secs (> secs 0))
		then ; setup for auto refresh
		     (html ((:meta :http-equiv "Refresh"
				   :content 
				   (format nil "~d;url=chattop?~a"
					   secs
					   qstring)))))
	      
	     ((:body :if* *background-image*
		     :background *background-image*
		     :if* (not *background-image*)
		     :bgcolor *top-frame-bgcolor*
		     :text *top-frame-font-color*
		     :link *top-frame-link-color*
		     :vlink *top-frame-vlink-color*
		     :alink *top-frame-alink-color*
		     )
	      (show-chat-info chat count 
			      (not (equal "1" (request-query-value
					       "rv"
					       req)))
			      (if* user then (user-handle user))
			      (if* is-owner then qstring)))))))))))

		     
(defun chatenter (req ent)
  ;;
  ;; this is the window where you enter the post and your handle.
  ;;
  (let* ((chat (chat-from-req req))
	 (user (user-from-req req))
	 (pp (or (request-query-value "pp" req) "*")) ; who to send to
	 (purl (request-query-value "purl" req))
	 (kind :multiline)
	 (to-user (user-from-ustring pp))
	 (qstring))
    (if* (null chat)
       then (return-from chatenter 
	      (ancient-link-error req ent)))
    
    (let* ((body (request-query-value "body" req))
	   (handle (request-query-value  "handle" req)))
	   
      (setq qstring 
	(add-secret req
		    (add-user req
			      (chat-query-string chat))))
      

	      
      (if* (and body (not (equal "" body)))
	 then ; user added content to the chat
	      (add-chat-data chat req handle body user to-user purl))
      
      (with-http-response (req ent)
	(with-http-body (req ent)
	  (html
	   (:html
	    ((:body :bgcolor 
		    (if* to-user 
		       then *bottom-frames-private*
		       else *bottom-frames-bgcolor*))
	     ((:form :action (concatenate 'string
			       "chatenter?"
			       qstring)
		     :method "POST")
	      (:center
	       (if* (eq kind :multiline)
		  then (html
			(:table
			 (:tr
			  (:td
			   (:center
			    (if* user
			       then (html 
				     (if* to-user
					then (html 
					      "Private msg from: ")
					else (html "From: "))
				     (:b 
				      (:princ-safe
				       (user-handle user)))
				     " to "
				     ((:font :size "+2")
				      (if* to-user
					 then (html
					       (:princ-safe
						(user-handle
						 to-user)))
					 else (html "all"))))
				    
			       else (html
				     "Your Name" 
				     ((:input :name "handle"
					      :type "text"
					      :tabindex 3
					      :size 20
					      :value (if* handle then handle else "")))))
			    " -- " 
			    ((:a :href (format nil "chatlogin?~a" qstring)
				 :target "_top")
			     "Login")
			    " -- &nbsp;&nbsp;&nbsp;"
			    
			    ((:input :name "send"
				     :tabindex 2
				     :value "Send"
				     :type "submit")))))
			 (:tr
			  (:td 
			   ((:textarea :name "body"
				       :tabindex 1
				       :cols 50
				       :rows 5))
			   ((:input :type "hidden"
				    :name "pp"
				    :value pp))))
			 (:tr
			  (:td
			   (:center
			    ((:input :type "text"
				     :size 40
				     :maxlength 100
				     :value (or purl "")
				    :name "purl"))
			   " Picture Url")))))
		  else ; single line
		       (html 
			(:table
			 (:tr
			  ((:td :colspan 1)
			   (:center
			    "Your Name" 
			    ((:input :name "handle"
				     :type "text"
				     :size 20
				     :value (if* handle then handle else "")))
			    ((:input :name "send"
				     :value "Post Message"
				     :type "submit")))))
			 (:tr 
			  (:td
			   ((:input :type "text"
				    :name "body"
				    :size 60
				    :maxsize 10000)))))))))
	      
	     ))))))))




(defun chatcontrol (req ent)
  ; control the updating
  (let ((chat (chat-from-req req))
	(qstring))
    
    (if* (null chat)
       then (return-from chatcontrol (ancient-link-error req ent)))
    
    (let* ((count (or (request-query-value "count" req) *default-count*))
	   (secs  (or (request-query-value "secs" req) *default-secs*)))
      
      (setq qstring 
	(add-lurk
	 req
	 (add-secret req
		     (add-user req (chat-query-string chat)))))
      (with-http-response (req ent)
	(with-http-body (req ent)
	  (html
	   (:html
	    ((:body :bgcolor *bottom-frames-bgcolor*)
	     ((:form :action
		     (concatenate 'string
		       "chattop?"
		       qstring
		       )
		     :target "chattop"
		     :method "POST")
	      ((:input :type "text"
		       :name "secs"
		       :size 3
		       :value secs)
	       "Seconds")
	      :br
	      ((:input :type "text"
		       :name "count"
		       :size 4 
		       :value count))
	      "messages"
	      :br
	      ((:input :type "checkbox"
		       :name "rv"
		       :value "1"))
	      " Reversed"
	      :br
		       
	      ((:input :type "submit"
		       :name "submit"
		       :value "Update Messages")))))))))))
		     

(defun compute-integer-value (string)
  ;; compute the string to a number
  ;; if there's any junk return nil if we haven't seen good stuff yet
  (and (stringp string)
       (let ((ans 0))
	 (do ((i 0 (1+ i))
	      (end (length string))
	      (seen-digit)
	      )
	     ((>= i end)
	      (if* seen-digit 
		 then ans
		 else nil))
	   (let ((digit (- (char-code (schar string i)) #.(char-code #\0))))
	     (if* (<= 0 digit 9)
		then (setq ans (+ (* ans 10) digit))
		     (setq seen-digit t)
		else (if* seen-digit
			then (return ans)
			else (return nil))))))))

  
    
(defun add-chat-data (chat req handle body user to-user purl)
  ;; purl is picture url value
  (multiple-value-bind (prefix link) 
      (if* (and (stringp purl) (not (equal "" purl)))
	 then (scan-for-http purl))
    (declare (ignore prefix))
    
    (if* link
       then (if* (and (consp link)
		      (consp (car link))
		      (eq :img (caar link)))
	       thenret  ; valid image url
	       else (setq link nil)))
    
    (let* ((cvted-body (string-to-lhtml body))
	   (ipaddr (socket:remote-host
		    (request-socket req)))
	   (dns (or #+ignore (socket:ipaddr-to-hostname ipaddr)
		    (socket:ipaddr-to-dotted ipaddr)))
	 
	   (message 
	    (make-message
	     :number (chat-message-number chat)
	     :ipaddr ipaddr
	     :dns dns
	     :handle (if* user then (user-handle user) else handle)
	     :to (if* to-user then (list (user-handle to-user)) else t)
	     :real (if* user then t else nil)
	     :time (compute-chat-date)
	     :body (if* link
		      then (cons link cvted-body)
		      else cvted-body))))
				     
      (mp:with-process-lock ((chat-message-lock chat))
	(add-chat-message chat message)))))

(defun compute-chat-date ()
  ; return string to use as time for this message
  ; quick hack - hardwire in pdt
  (multiple-value-bind (sec min hour)
      (decode-universal-time (get-universal-time))
    (format nil "~d:~2,'0d:~2,'0d PDT" hour min sec)))

(defun add-chat-message (chat message)
  ;; add the message to the messages of the chat.
  ;; assume that we've got the lock to do this.
  (let ((messages (chat-messages chat))
	(message-next (chat-message-next chat)))
	    
    (if* (>= message-next (length messages))
       then ; must grow messages
	    (let ((nmessages (make-array (+ (length messages) 200))))
	      (dotimes (i (length messages))
		(setf (svref nmessages i) (svref messages i)))
	      (setf (chat-messages chat) nmessages)
	      (setq messages nmessages)))
    (setf (svref messages message-next)  message)
    (setf (chat-message-next chat) (1+ message-next))
    (setf (chat-message-number chat) 
      (1+ (message-number message)))))
  


(defun delete-chat-message (chat messagenum)
  ;; remove the given message by setting the to field to nil
  (let ((message (find-chat-message chat messagenum)))
    (if* message
       then (setf (message-to message) nil))))


(defun find-chat-message (chat number)
  ;; find the message with the given number
  (let* ((messages (chat-messages chat))
	 (len (and messages (length messages))))
    (if* messages
       then ; find first message
	    (dotimes (i len)
	      (let ((message (svref messages i)))
		(if* (null message)
		   then (return nil)
		 elseif (eql (message-number message) number)
		   then (return message)))))))
		      
  
  
(defun show-chat-info (chat count recent-first handle ownerp)
  ;; show the messages for all and those private ones for handle
  ;; handle is only non-nil if this is a legit logged in handle
  (let ((message-next (chat-message-next chat))
	(messages (chat-messages chat))
	(first-message)
	(last-message)
	(message-increment))
    (if* (zerop message-next)
       then (html (:b "There are no messsages in this chat"))
     elseif (<= count 0)
       thenret ; nothing to show
       else (if* recent-first
	       then (setq first-message (1- message-next)
			  last-message (max 0
					    (- message-next count))
			  message-increment -1)
	       else (setq last-message (1- message-next)
			  first-message (max 0
					     (- message-next count))
			  message-increment 1))

	    (if* recent-first
	       then ; tag most recent message
		    (html ((:div :id "recent"))))
	    
	    (do ((i first-message (+ i message-increment)))
		(nil)
	    
	      (let ((message (svref messages i)))
		(if* (null message)
		   then (warn "null message at index ~d" i)
		 elseif (if* (or (eq t (message-to message))
				 (member handle (message-to message)
					 :test #'equal))
			   then ;; to everyone or us
				nil	 ; don't skip
			 elseif (and (equal (message-handle message)
					    handle)
				     (message-to message))
			   then ;; from us to someone, anyone
				nil ; don't skip
			   else t ; skip
				)
		   thenret ; skip this message
		 elseif (eq *show-style* 1)
		   then
			(html :newline 
			      ((:font :color 
				      (if* (consp (message-to message))
					 then *private-font-color*
					 else *public-font-color*))
			       
			       (:b (:i (:princ-safe (message-handle message))))
			       (if* (not (message-real message))
				  then (html " (unverified)"))
			       ((:font :size 1)
				" -- ("
				(:princ (message-time message))
				(if* (consp (message-to message))
				   then (html " to: "
					      (:princ-safe (message-to message))))
				")")
			      
			       " <!-- "
			       (:princ (message-number message)) 
			       " "
			       (:princ (message-dns message))
			       " --> "
			       (if* ownerp
				  then (html
					((:a :href 
					     (format nil "chattop?y=~a&~a"
						     (message-number message)
						     ownerp))
					 "Delete")))
			       :newline
			       :br
			       (html-print-list (message-body message)
						*html-stream*)
			       :br)
			      :newline)
		   else 
			(html
			 :newline
			 ((:table :border 1 :width "100%" :frame "box")
			  (:tr
			   ((:td :width "10%")
			    (:b (:i (:princ-safe (message-handle message))))
			    :br
			    ((:font :size 1) (:princ (message-time message)))
			    " <!-- "
			    (:princ (message-number message)) 
			    " "
			    (:princ (message-dns message))
			    " --> "
			    )
			   (:td
			    (html-print-list (message-body message)
					     *html-stream*)))))))
		 
	      (if* (eql i last-message) then (return)))
	    
	    (if* (not recent-first)
	       then ; tag most recent message
		    (html ((:div :id "recent")))))))


(defun chatlogin (req ent)
  ;; response function for /chatlogin?ucstring"
  (let ((chat (chat-from-req req)))
    (if* chat
       then (do-chat-login req ent 
			   (add-secret req
				       (add-user req
						 (chat-query-string chat)))
			   nil)
       else (ancient-link-error req ent))))


(defun do-chat-login (req ent qstring failure)
  ;; put up a login screen for this chat
  (with-http-response (req ent)
    (with-http-body (req ent)
      (html
       (:html
	(:head (:title "Login to Chat"))
	(:body
	 (if* failure
	    then (html (:blink 
			(:b "Error: " (:princ-safe failure) :br :br))))
	 
	 (:h1 "Login as an existing user")
	 ((:form :action (format nil "chatlogincurrent?~a" qstring)
		 :target "_top"
		 :method "POST")
	  ((:input :type "text" :size "15" :name "handle")) "Your Handle" :br
	  ((:input :type "text" :size "15" :name "password")) "Your password" :br
	  ((:input :type "submit" :name "submit" :value "login")))
	 :hr
	 (:h1 "Create a new account and login")
	 ((:form :action (format nil "chatloginnew?~a" qstring)
		 :method "POST")
	  ((:input :type "text" :size "15" :name "handle")) "Your Handle" :br
	  ((:input :type "text" :size "15" :name "password")) "Your password" :br
	  ((:input :type "text" :size "15" :name "password2")) "Type your password again" :br
	  ((:input :type "submit" :name "submit" :value "New Account")))))))))


(defun chat-login-current (req ent)
  ;; handle a post to  chatlogincurrent 
  (let ((chat (chat-from-req req))
	(handle (request-query-value "handle" req))
	(password (request-query-value "password" req)))
    ; locate the handle
    (let ((user (find handle (users *master-controller*)
		      :key #'user-handle :test #'equal)))
      (if* (null user)
	 then (return-from chat-login-current
		(do-chat-login req ent 
			       (add-secret req
					   (add-user req
						     (chat-query-string chat)))
			       "That user name is unknown")))
      (if* (not (equal password (user-password user)))
	 then (return-from chat-login-current
		(do-chat-login req ent 
			       (add-secret req
					   (add-user req
						     (chat-query-string chat)))
			       "That password is incorrect")))
      
      ; worked, do a redirect
      (with-http-response (req ent :response *response-moved-permanently*)
	(setf (reply-header-slot-value req "location")
	  (format  nil "chat?~a&x=~a"
		   (add-secret req 
			       (chat-query-string chat))
		   (user-ustring user)))
	(with-http-body (req ent)
	  (html "redirect"))))))

      
      
(defun chatloginnew (req ent)
  ;; response function when a new user is being defined
  (let* ((handle (request-query-value "handle" req))
	 (password (request-query-value "password" req))
	 (password2 (request-query-value "password2" req))
	 (chat (chat-from-req req))
	 (qstring (and chat (chat-query-string chat))))
    
    (if* (null chat)
       then (return-from chatloginnew (ancient-link-error req ent)))
    
    
    (if* (equal "" password)
       then (return-from chatloginnew
	      (do-chat-login req ent qstring "No password given")))
    
    (if* (not (equal password password2))
       then (return-from chatloginnew
	      (do-chat-login req ent qstring "Passwords don't match")))
    
    (dolist (user (users *master-controller*))
      (if* (equal (user-handle user) handle)
	 then (return-from chatloginnew
		(do-chat-login req ent qstring "That user name exists"))))
    
    ; add new user
    (let (new-ustring)
      (mp:with-process-lock ((master-lock *master-controller*))
	(loop 
	  (setq new-ustring (make-unique-string))
	  (if* (dolist (user (users *master-controller*) t)
		 (if* (equal new-ustring (user-ustring user))
		    then ; already in use
			 (return nil)))
	     then (return)))
	; leave the loop with new-ustring being unique among users
	(push (make-user :handle handle
			 :password password
			 :ustring new-ustring)
	      (users *master-controller*))
	(dump-existing-chat *chat-home*))
      
      ; go to the chat as the user
      (with-http-response (req ent :response
			       *response-moved-permanently*)
	(setf (reply-header-slot-value req "location")
	  (format nil "chat?~a&x=~a" 
		  (add-secret req qstring) new-ustring))
	(with-http-body (req ent) 
	  "move to the chat")))))
							   
    
    
      
    
    
    

    
    
    
  
      
      
    
      
	
	
	 
       
(defun string-to-lhtml (form)
  ;; convert the string to a list of lhtml forms
  ;;
  ;; break the text into lines separated by :br's.
  ;; look for http://'s in the lines and replace them with
  ;; links or inline images
  
  (let (res (i 0) (start 0) (max (length form)))
    (loop
      ; we go around one last time when (eq i max) in which
      ; case we pretent there's a linefeed at the end
      (let ((ch 
	     (if* (>= i max)
		then #\linefeed
		else (schar form i))))
	
	(if* (or (eq ch #\return) (eq ch #\linefeed))
	   then ; end of line
		(if* (not (eq start i))
		   then (let ((line (subseq form start i)))
			  (loop
			    (if* (or (null line) 
				     (equal line ""))
			       then (return))
			    (multiple-value-bind (pref link rest)
				(scan-for-http line)
				(if* link
				   then (push pref res)
					(push link res)
					(setq line rest)
				   else (push pref res)
					(setq line nil))))))
		(push :br res)
		
		(incf i)
		(if* (and (eq ch #\return)
			  (< i max)
			  (eq (schar form i) #\linefeed))
		   then (incf i) ; past following linefeed
			)
		
		(setq start i)
	   else (incf i))
	    
	(if* (> i max) then (return))))
    (nreverse res)))
      
(defun scan-for-http (line)
  ;; look for http:// in the line and if found return it as
  ;; a link or image lhtml
  ;;
  
  (multiple-value-bind (ok whole)
      (match-regexp "http://[^ 	>]+" line :return :index)
    (if* ok
       then ; found one
	    (let (http)
	      (setq http (subseq line (car whole) (cdr whole)))
	    
	      (values
	       ; value 1 -- everything before the http
	       (subseq line 0 (car whole)) 
	       
	       ; value 2 - the link 
	     
	       (do ((i (1- (length http)) (1- i)))
		   ((< i 0)
		    ; didn't find a . .. set to a link
		    `((:a :href ,http :target "_blank") (:princ-safe ,http)))
		 
		 (if* (eq (schar http i) #\.)
		    then ; found a period
			 (let ((type (subseq http (1+ i))))
			   (if* (member type '("gif" "jpg" "png")
					:test #'equalp)
			      then ; an image link
				   (return 
				     `((:img :src ,http)))
			      else (setq i 0) ; stop search
				   ))))
	       
	       ; value 3 - the rest of the line
	       (subseq line (cdr whole))))
       else line)))

	     
;; Chat archiver
;;
;; The chat archiver stores chat info to files

(let (last-master-controller)
(defun start-chat-archiver (master-controller)
  (and t (if* (not (eq master-controller last-master-controller))
     then ; we've already started the process
	  (setq last-master-controller master-controller)
	  (mp:process-run-function "chat archiver"
				   #'chat-archiver master-controller)))))

(defun chat-archiver (master-controller)
  (let ((sleep-time 30)
	(did-work))
    (loop
      (if* (not (eq *master-controller* master-controller))
	 then ; chat has been restarted, let this process die
	      (return))

      (format t "Chat archiver awoken~%")
      (setq did-work nil)
    
      ; write out the data
      (dolist (controller (controllers master-controller))
	(dolist (chat (chats controller))
	  (mp:with-process-lock ((chat-message-lock chat))
	    (format t " arch ~d   num ~d~%"
		    (chat-message-archive chat)
		    (chat-message-number  chat))
	    (if* (< (chat-message-archive chat)
		    (chat-message-number  chat))
	       then ; must do work
		    (archive-chat chat)
		    (setq did-work t)))))

      ; adjust archive time so that we sleep longer when 
      ; the chat is inactive.
      (if* did-work 
	 then (setq sleep-time 30)
	 else (setq sleep-time (min (+ sleep-time 30) 
				    (* 30 60) ; 30 minutes
				    )))
      
      (format t "Chat archiver going to sleep~%")
      (sleep sleep-time))))



(defun archive-chat (chat)
  ;; write out new messages for this chat
  ;; we are inside a process lock for this chat's message lock
  ;; so we can alter the fields at will
  (let ((messages (chat-messages chat))
	(message-next (chat-message-next chat))
	(message-archive (chat-message-archive chat)))
    
    ; we have to find the location of the 
    ; message-archive message
    (if* (> message-next 0)
       then ; it better be greater than 0 since to be zero
	    ; would be no messages stored
	    (let* ((last-message (svref messages (1- message-next)))
		   (last-mnum (message-number last-message))
		   (start-to-save
		    (+ (1- message-next) ; index of last message
		       (- message-archive last-mnum)))) ; amt to skip down
	      (with-open-file (archive-port (archive-filename chat)
			       :direction :output
			       :if-exists :append
			       :if-does-not-exist :create
			       )
		(do ((i start-to-save (1+ i)))
		    ((>= i message-next))
		  (pprint (svref messages i) archive-port)))
	      
	      (setf (chat-message-archive chat) (1+ last-mnum))))))
	      

(defun archive-filename (chat)
  (format nil "~a/~a" *chat-home* (chat-filename chat)))


	
(defmethod set-style ((style color-style))
  (setq *top-frame-bgcolor*     (color-style-bgcolor style)
	*top-frame-font-color*  (color-style-font-color style)
	*top-frame-vlink-color* (color-style-vlink-color style)
	*top-frame-link-color*  (color-style-link-color style)
	*top-frame-alink-color* (color-style-alink-color style)))

(if* (not (boundp '*top-frame-bgcolor*))
   then (set-style *normal-style*))




(defun chat-transcript (uc-string filename)
  ;; generate a transcript of the chat with the given uc-string
  ;; to the given filename
  ;
  ; find chat
  (let* ((query-alist (form-urlencoded-to-query uc-string))
	 (u (cdr (assoc "u" query-alist :test #'equalp)))
	 (c (cdr (assoc "c" query-alist :test #'equalp))))
    
    (let ((chat 
	   (dolist (controller (controllers *master-controller*))
	     (if* (equal u (ustring controller))
		then (return
		       (dolist (chat (chats controller))
			 (if* (equal c (ustring chat))
			    then (return chat))))))))
      (if* (null chat)
	 then (error "can't find chat with uc-string ~s" uc-string))
      
      (with-open-file (*html-stream* filename :direction :output
		       :if-exists :supersede)
	(html 
	 (:head
	  (:title "Transcript of "
		  (:princ-safe (chat-name chat))))
	 (:body
	  (:h1 "Transcript of "
	       (:princ-safe (chat-name chat)))
	  (show-chat-info chat (chat-message-next chat) nil nil nil)))))))
		     
		     
			 
;;  viewer tracking

(defun track-viewer (chat user req)
  ;; note that this user/req has read the postings for this chat
  (let* ((time (get-universal-time))
	 (viewers (chat-viewers chat))
	 (ipaddr (if* (null user)
		    then (socket:remote-host 
			  (request-socket req))))
	 (empty-ent))
    
    (mp::with-process-lock ((viewers-lock viewers))
      
      ;; scan list of viewers.
      ;; set emptyent to the first viewent with a null time, thus meaning
      ;;  it's a free entry
      ;; if an entry already exists for this user or ipaddr use it
      (dolist (viewent (viewers-list viewers)
		; not there yet
		(if* empty-ent
		   then ; replace old one
			(setf (viewent-time empty-ent) time
			      (viewent-user empty-ent) user
			      (viewent-ipaddr empty-ent) ipaddr)
		   else 
			(push (setq empty-ent 
				(make-viewent :time time
					      :user user
					      :ipaddr ipaddr))
			      (viewers-list viewers))
			))
	(if* user
	   then (if* (eq user (viewent-user viewent))
		   then ; update this one
			(setf (viewent-time viewent) time)
			(return))
	   else ; ipaddr test
		(if* (eql ipaddr (viewent-ipaddr viewent))
		   then (setf (viewent-time viewent) time)
			(return)))
	(if* (null (viewent-time viewent))
	   then (if* (null empty-ent)
		   then (setf empty-ent viewent))
	 elseif (> (- time (viewent-time viewent)) *max-active-time*)
	   then ; this one is too old
		(setf (viewent-time viewent) nil)
		(if* (null empty-ent)
		   then (setq empty-ent viewent)))))))

(defun chatviewers (req ent)
  ;; display page of chat viewers (except us)
  (let* ((chat (chat-from-req req))
	(user (user-from-req req))
	(time (get-universal-time))
	(is-owner
	  (equal (and chat (secret-key chat)) 
		 (request-query-value "s" req)))
	(qstring)
	(viewers))
    (if* (null chat)
       then (return-from chatviewers (ancient-link-error req ent)))
    
    (setq qstring
      (add-secret req
		  (add-user req
			    (chat-query-string chat))))
    (setq viewers (chat-viewers chat))
    
    (with-http-response (req ent)
      (with-http-body (req ent)
	(html
	 (:html
	  ((:meta :http-equiv "Refresh"
		  :content
		  (format nil "30;url=chatviewers?~a" qstring)))
	  (:body
	   
	   ((:font :size 1)
	    ((:a :href (concatenate 'string
			 "chatenter?pp=*&" qstring)
		 :target "chatenter"
		 )
	     "Send to All")
	    :hr
	    (mp::with-process-lock ((viewers-lock viewers))
	      (dolist (viewent (viewers-list viewers))
		(let* ((vtime (viewent-time viewent))
		       (vuser (viewent-user viewent))
		       (alive-time (if* vtime then (- time vtime)))
		       )
		  
		  (if* (and alive-time
			    (> alive-time *max-active-time*))
		     then (setq vtime nil)
			  (setf (viewent-time viewent) nil))
		  
		  (if* vtime
		     then (if* (not (eq vuser user))
			     then ; list this one
				  (if* vuser
				     then ; link to create a private message
					  (html
					   ((:a :href 
						(format nil
							"chatenter?pp=~a&~a"
							(user-ustring vuser)
							qstring)
						:target "chatenter"
						)
					    (:princ-safe
					     (user-handle vuser))))
				   elseif (and is-owner *do-dnscheck*)
				     then ; name then ip address
					  (let ((name (viewent-hostname
						       viewent)))
					    #+(version>= 6 0)
					    (if* (null name)
					       then (setq name
						      (setf (viewent-hostname
							     viewent)
							(socket::dns-query
							 (viewent-ipaddr
							  viewent)
							 :type :ptr
							 :repeat 1
							 :timeout 0))))
					    (if* (null name)
					       then (setf name
						      (socket:ipaddr-to-dotted
						       (viewent-ipaddr
							viewent))))
					    
					  (html
					   (:princ name)))
				     else ; ip address
						    
					  (html
					   (:princ
					    (socket:ipaddr-to-dotted
					     (viewent-ipaddr viewent)))))
				  (html 
				   " ("
				   (:princ (- time vtime))
				   "s)"
				   :br))))))))))))))
						
					  
	    
		   
    
  
      
			
    

    
    
    
    
    
    
;;;;; chat test code
;;
;;

(defun block-test (testers &rest args)
  (dotimes (i testers)
    (let ((name (format nil "tester-~d" i))
	  (delay (max 1 (random 10))))
      
      (mp:process-run-function name
			       #'(lambda ()
				   (apply #'test-chat
					  :name name
					  :delay delay
					  args))))))
					  


				 
  
  


(defun test-chat (&key uc-string
		       (count 100) 
		       (reads 5) 
		       (delay 2)
		       (name "dummy1")
		       (machine "localhost")
		       (port  8000)
		       (protocol :http/1.1))
  (let ((reader-url
	 (format nil "http://~a:~d/chattop?~a&~a"
		 machine
		 port
		 uc-string
		 (query-to-form-urlencoded
		  `(("count" . 10)
		    ("secs" . 5)))))
	(post-url 
	 (format nil "http://~a:~d/chatenter?~a"
		 machine
		 port
		 uc-string)))
		  
  (dotimes (i count)
    ; post first
    (let ((message (format nil "message ~d from ~a~%" i name)))
      (do-http-request post-url
	:protocol protocol
	:method :post
	:query `(("secs" . 5) ; not used
		 ("handle" . ,name)
		 ("body" . ,message)))
      (sleep delay)
      (dotimes (i reads)
	; read it now
	(do-http-request reader-url
	  :method :get
	  :protocol protocol)
	(sleep delay))))))
      
	  
