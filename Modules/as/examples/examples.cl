;; -*- mode: common-lisp; package: net.aserve.examples -*-
;;
;; examples.cl
;;
;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA 
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation; 
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
;; $Id: examples.cl,v 1.10 2000/06/11 15:24:19 jkf Exp $

;; Description:
;;   Allegro iServe examples

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-



;; examples of web pages
(defpackage :net.aserve.examples ;; aserve example
  (:use :common-lisp :excl :net.html.generator :net.aserve))

(in-package :net.aserve.examples)

;; flush all publishing done so far:
(unpublish :all t)

(defparameter *example-pathname* *load-truename*) ; where this file is
(defmacro example-file (name)
    ;; create an absolute address for this file we'll load
    `(merge-pathnames ,name *example-pathname*))



(publish :path "/" 
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (with-http-response (req ent)
	       (with-http-body (req ent)
		 (html
		  (:head (:title "Welcome to AllegroServe"))
		  (:body (:center ((:img :src "aservelogo.gif")))
			 (:h1 "Welcome to AllegroServe") 
			 (:p "These links show off some of AllegroServe's capabilities. ")
			 (:i "This server's host name is "
			     (:princ-safe (header-slot-value req "host")))
			 :p
			 (:b "Sample pages") :br
			 ((:a :href "gc") "Garbage Collector Stats") :br
			 ((:a :href "apropos") "Apropos") :br
			 ((:a :href "pic") "Sample jpeg") :br
			 ((:a :href "pic-redirect") "Redirect to previous picture") :br
			 ((:a :href "pic-gen") "generated jpeg") "- hit reload to switch images" :br
			 ((:a :href "cookietest") "test cookies") :br
			 ((:a :href "secret") "Test manual authorization")
			 " (name: " (:b "foo") ", password: " (:b "bar") ")"
			 :br
			 ((:a :href "secret-auth") "Test automatic authorization")
			 " (name: "
			 (:b "foo2")
			 " password: "
			 (:b "bar2") ")"
			 :br
			 ((:a :href "local-secret") "Test source based authorization") " This will only work if you can use "
			 "http:://localhost ... to reach this page" :
			 :br
			 ((:a :href "local-secret-auth") 
			  "Like the preceding but uses authorizer objects")
			 :br
			 ((:a :href "timeout") "Test timeout")
			 :br
			 ((:a :href "getfile") "Client to server file transfer")
			 :br
			 ((:a :href "missing-link") "Missing Link")
			 " should get an error when clicked"
			 )
		  
		  )))))
			     


;; a very simple page.  This is so simple it doesn't put out the required
;; tags (like <html>) yet I suspect that most browsers will display it
;; correctly regardless.
(publish :path "/hello"
	 :content-type  "text/html"
	 :function #'(lambda (req ent)
		       (with-http-response (req ent)
			 (with-http-body (req ent)
			   (html "Hello World!")))))

;; this is the "/hello" example above modified to put out the correct
;; html tags around the page.
(publish :path "/hello2"
	 :content-type  "text/html"
	 :function #'(lambda (req ent)
		       (with-http-response (req ent)
			 (with-http-body (req ent)
			   (html 
			    (:html
			     (:body "Hello World!")))))))

;; display the current gc statistics.
(publish :path "/gc"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (macrolet ((build-gsgc-table ()
			  `(html
			    ,@(mapcar 
			       #'(lambda (kind)
				   `(:tr (:td (:princ ,kind))
					 (:td (:princ-safe
					       (sys:gsgc-parameter ,kind)))))
			       '(:generation-spread
				 :current-generation
				 :tenure-limit
				 :free-bytes-new-other
				 :free-percent-new
				 :free-bytes-new-pages
				 :expansion-free-percent-new
				 :expansion-free-percent-old
				 :quantum
				 )))))
			     
				   
	       (with-http-response (req ent)
		 (with-http-body (req ent)
		     (html (:head (:title "Allegro gc parameters"))
			   (:body
			    ((:table :bgcolor "silver" :bordercolor "blue"
				     :border "3" :cellpadding "3"
				     :cellspacing "3")
			     (:tr (:td (:b "gsgc parameter")) (:td (:b "Value")))
			     (build-gsgc-table)))))))))
		       


;; display a picture from a file.
(publish-file :path "/pic" :file (example-file "prfile9.jpg")
	      :content-type "image/jpeg")



(publish-file :path "/aservelogo.gif" :file (example-file "aservelogo.gif")
	      :content-type "image/gif")

;; this is a demonstration of how you can return a jpeg 
;; image that was created on the fly (rather thsn read from
;; a file via publish-file). 
;; We don't want to actually create the image here, so we 
;; cheat and read it from a file, but it shows that you can
;; send any stream of bytes and they will be given the correct
;; mime type.
;; 
(publish :path "/pic-gen"
	 :content-type "image/jpeg"
	 :function
	 (let ((selector 0)) ; chose one of two pictures
	   #'(lambda (req ent)
	       (with-http-response (req ent)
		 (with-http-body (req ent :format :binary)
		   ; here is where you would generate the picture.
		   ; we're just reading it from a file in this example
		   (let ((stream (request-reply-stream req)))
		     (with-open-file (p (nth selector
					     `(,(example-file "prfile9.jpg")
					       ,(example-file "fresh.jpg")))
				      :element-type '(unsigned-byte 8))

		       (setq selector (mod (1+ selector) 2))
		     
		       (loop
			 (let ((val (read-byte p nil nil)))
			   (if* (null val) 
			      then ;eof 
				   (return))
			   (write-byte val stream)
			   )))))))))
	 


;; do a redirect to the picture

(publish :path "/pic-redirect"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (with-http-response (req ent
				      :response *response-moved-permanently*)
	       (setf (reply-header-slot-value req "location") "pic")
	       (with-http-body (req ent)
		 ;; this is optional and most likely unnecessary since most 
		 ;; browsers understand the redirect response
		 (html 
		  (:html
		   (:head (:title "Object Moved"))
		   (:body 
		    (:h1 "Object Moved")
		    "The picture you're looking for is now at "
		    ((:a :href "pic") "This location"))))))))
		    
		    
	 


;;
;; here's a form using the 'post' method
;;
(publish :path "/tform" 
	 :content-type "text/html"
	 :function
	 (let ((name "unknown"))
	   #'(lambda (req ent)
	       (let ((body (get-request-body req)))
		 (format t "got body ~s~%" body)
		 (let ((gotname (assoc "username"
				       (form-urlencoded-to-query body)
					:test #'equal)))
		   (if* gotname
		      then (setq name (cdr gotname)))))
		 
	       (with-http-response (req ent)
		 (with-http-body (req ent)
		   (html (:head (:title "test form"))
			 (:body "Hello " (:princ-safe name) ", "
				"Enter your name: "
				((:form :action "/tform"
					:method "post")
				 ((:input :type "text"
					  :maxlength 10
					  :size 10
					  :name "username"))))))))))

			      
				    

;; example of a form that uses that 'get' method
;;
(publish 
 :path "/apropos"
 :content-type "text/html"
 :function
 #'(lambda (req ent)
     
     (let ((lookup (assoc "symbol" (request-query req) :test #'equal)))
       (with-http-response (req ent)
	 (with-http-body (req ent)
	   (html (:head (:title "Allegro Apropos"))
		 ((:body :background "aserveweb/fresh.jpg")
		  "New Apropos of "
		  ((:form :action "apropos"
			  :method "get")
		   ((:input :type "text"
			    :maxlength 40
			    :size 20
			    :name "symbol")))
		  :p
			
		  (if* lookup
		     then (html :hr (:b "Apropos") " of " 
				(:princ-safe (cdr lookup))
				:br
				:br)
			  (let ((ans (apropos-list (cdr lookup))))
			    (if* (null ans)
			       then (html "No Match Found")
			       else (macrolet ((my-td (str)
						 `(html ((:td 
							  :bgcolor "blue")
							 ((:font :color "white"
								 :size "+1")
							  (:b ,str))))))
						       
				      (html ((:table
					      :bgcolor "silver"
					      :bordercolor "blue"
					      :border 3
					      :cellpadding 3
					      )
						   
					     (:tr
					      (my-td "Symbol")
					      (my-td "boundp")
					      (my-td "fboundp"))
						 
						   
					     (dolist (val ans)
					       (html (:tr 
						      (:td (:prin1-safe val))
						      (:td (:prin1 (and (boundp val) t)))
						      (:td (:prin1 (and (fboundp val) t))))
						     :newline)))))))
		     else (html "Enter name and type enter")))
		 :newline))))))


;; a preloaded picture file
(publish-file :path "/aserveweb/fresh.jpg"
	      :file (example-file "fresh.jpg")
	      :content-type "image/jpeg"
	      :preload t)

;; a preloaded text file
(publish-file :path "/foo"
	      :file (example-file "foo.txt")
	      :content-type "text/plain"
	      :preload t)

(publish-file :path "/foo.txt"
	      :file (example-file "foo.txt")
	      :content-type "text/plain"
	      :preload nil)


;; some entries for benchmarking
(publish-file :path "/file2000"
	      :file (example-file "file2000.txt")
	      :content-type "text/plain"
	      :preload nil)

(publish-file :path "/file2000-preload"
	      :file (example-file "file2000.txt")
	      :content-type "text/plain"
	      :preload t)

(publish :path "/dynamic-page"
	 :content-type "text/plain"
	 :function #'(lambda (req ent)
		       (with-http-response (req ent)
			 (with-http-body (req ent)
			   (html "This is a dynamic page")))))

;; an example which causes the web browser to put up the
;; name/password box and if you enter the name "foo" and password "bar"
;; then you get access to the secret info.
(publish :path "/secret"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (multiple-value-bind (name password) (get-basic-authorization req)
	       (if* (and (equal name "foo") (equal password "bar"))
		  then (with-http-response (req ent)
			 (with-http-body (req ent)
			   (html (:head (:title "Secret page"))
				 (:body "You made it to the secret page"))))
		  else
		       (with-http-response (req ent :response 
						*response-unauthorized*)
			 (set-basic-authorization req
						   "secretserver")
			 (with-http-body (req ent)))))))


(publish :path "/local-secret"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (let ((net-address (ash (socket:remote-host
				      (request-socket req))
				     -24)))
	       (if* (equal net-address 127)
		  then (with-http-response (req ent)
			 (with-http-body (req ent)
			   (html (:head (:title "Secret page"))
				 (:body (:b "Congratulations. ")
					"You are on the local network"))))
		  else
		       (with-http-response (req ent)
			 (with-http-body (req ent)
			   (html
			    (:html (:head (:title "Unauthorized"))
				   (:body 
				    "You cannot access this page "
				    "from your location")))))))))


(publish :path "/local-secret-auth"
	 :content-type "text/html"
	 :authorizer (make-instance 'location-authorizer
		       :patterns '((:accept "127.0" 8)
				   (:accept "tiger.franz.com")
				   :deny))
	 :function
	 #'(lambda (req ent)
	     (with-http-response (req ent)
	       (with-http-body (req ent)
		 (html (:head (:title "Secret page"))
		       (:body (:b "Congratulations. ")
			      "You made it to the secret page"))))))

;; these two urls show how to transfer a user-selected file from
;; the client browser to the server.
;; 
;; We use two urls (/getfile to put up the form and /getfile-post to
;; handle the post action of the form).   We could have done it all
;; with one url but since there's a lot of code it helps in the
;; presentation to separate the two.
;;
(publish :path "/getfile"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (with-http-response (req ent)
	       (with-http-body (req ent)
		 (html (:head "get file")
		       (:body
			((:form :enctype "multipart/form-data"
				:method "post"
				:action "getfile-got")
			 "Let me know what file to grab"
			 :br
			 ((:input :type "file" 
				  :name "thefile"
				  :value "*.txt"))
			 :br
			 ((:input :type "text" :name "textthing"))
			 :br
			 ((:input :type "checkbox" :name "checkone"))
			 "check box one"
			 :br
			 ((:input :type "checkbox" :name "checktwo"))
			 "check box two"
			 :br
			 ((:input :type "submit")))))))))


(publish :path "/secret-auth"
	 :content-type "text/html"
	 :authorizer (make-instance 'password-authorizer
		       :allowed '(("foo2" . "bar2")
				  ("foo3" . "bar3")
				  )
		       :realm  "SecretAuth")
	 :function
	 #'(lambda (req ent)
	     (with-http-response (req ent)
	       (with-http-body (req ent)
		 (html (:head (:title "Secret page"))
		       (:body "You made it to the secret page"))))))




;; this called with the file from 
(publish :path "/getfile-got"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     
	     (with-http-response (req ent)
	       (let ((h nil)
		     (counter 0)
		     (files-written)
		     )
		 (loop
		   ; get headers for the next item
		   (if* (null (setq h (get-multipart-header req)))
		      then ; no more items
			   (return))
		   ; we can get the filename from the header if 
		   ; it was an <input type="file"> item.  If there is
		   ; no filename, we just create one.
		   (let ((cd (assoc "content-disposition" h :test #'equalp))
			 (filename)
			 (sep))
		     (if* (and cd (consp (cadr cd)))
			then (setq filename (cdr (assoc "filename" 
							(cddr (cadr cd))
							:test #'equalp)))
			     (if* filename
				then ;; locate the part of the filename
				     ;; after the last directory separator.
				     ;; the common lisp pathname functions are
				     ;; no help since the filename syntax
				     ;; may be foreign to the OS on which
				     ;; the server is running.
				     (setq sep
				       (max (or (position #\/ filename
							  :from-end t) -1)
					    (or (position #\\ filename
							  :from-end t) -1)))
				     (setq filename
				       (subseq filename (1+ sep) 
					       (length filename)))))
		     (if* (null filename)
			then (setq filename (format nil "tempfile~d"
						    (incf counter))))
		     
		     (push filename files-written)
		     (with-open-file (pp filename :direction :output
				      :if-exists :supersede
				      :element-type '(unsigned-byte 8))
		       (format t "writing file ~s~%" filename)
		       (let ((buffer (make-array 1024
						 :element-type '(unsigned-byte 8))))
			 
			 (loop (let ((count (get-multipart-sequence 
					     req 
					     buffer
					     :raw t)))
				 (if* (null count) then (return))
				 (write-sequence buffer pp :end count)))))
		
		     ))
	       
	       
		 ;; now send back a response for the browser
	       
		 (with-http-body (req ent)
		   (html (:html (:head (:title "form example"))
				(:body "proceessed the form, files written"
				       (dolist (file (nreverse files-written))
					 (html :br "file: "
					       (:b (:prin1-safe file))))))))))))

	     

(publish :path "/cookietest"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (with-http-response (req ent)
	       (set-cookie-header req 
				  :name "froba" 
				  :value "vala"
				  :path "/"
				  :expires :never)
	       (set-cookie-header req
				  :name "the time"
				  :value (net.aserve::universal-time-to-date
					  (get-universal-time))
				  :path "/cookieverify"
				  :expires (+ (get-universal-time)
					      (* 20 60) ; 20 mins
					      )
				  )
				  
	       (with-http-body (req ent)
		 (html (:head (:title "Cookie Test"))
		       (:body "you should have a cookie now."
			      " Go "
			      ((:a :href "cookieverify") "here")
			      " to see if they were saved"))))))

(publish :path "/cookieverify"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (let ((cookie-info (get-cookie-values req)))
	       (with-http-response (req ent)
		 (with-http-body (req ent)
		   (html (:head (:title "Cookie results"))
			 (:body
			  "The following cookies were returned: " 
			  (:prin1-safe cookie-info))))))))
	 


(publish :path "/timeout"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     ;; do nothing interesting so that the timeout will
	     ;; occur
	     (with-http-response (req ent :timeout 15)
	       (loop (sleep 5)))))


;;;;;;  directory publishing.  These will only work on a particular
;; set of machines so you'll have to modify them to point to an
;; existing tree of pages on your machine if you want to see this work.

;; the franz home page
#+ignore (publish-directory :prefix "/"
		   :destination "/net/tanya/home/httpd/html/"
		   )


(publish-directory :prefix "/int"
		   :destination "/net/tanya/www/internal/htdocs/")




;; a separate world:

(defparameter *server2* (make-instance 'wserver))

(publish-directory :server *server2*
		   :prefix "/"
		   :destination "/home/httpd/html/")

