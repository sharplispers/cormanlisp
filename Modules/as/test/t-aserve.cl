;; -*- mode: common-lisp; package: net.aserve.test -*-
;;
;; t-iserve.cl
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
;; $Id: t-aserve.cl,v 1.6 2000/06/08 16:43:58 jkf Exp $

;; Description:
;;   test iserve

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-

(eval-when (compile load eval)
  (require :tester))

(defpackage :net.aserve.test
  (:use :common-lisp :excl :net.html.generator :net.aserve 
	:net.aserve.client
	:util.test)
  )

(in-package :net.aserve.test)

; set to nil before loading the test to prevent the test from auto-running
(defvar user::*do-aserve-test* t)

(defun test-aserve ()
  ;; tests are run on a variety of threads, so we have to 
  ;; account for those other thread errors separately.
  (setq util.test::*test-errors* 0
        util.test::*test-successes* 0
	util.test::*test-unexpected-failures* 0)
  (with-tests (:name "aserve")
    (let ((port (start-aserve-running)))
      (format t "server started on port ~d~%" port)
      (unwind-protect 
	  (progn
	    (test-publish-file port)
	    (test-publish-computed port)
	    (test-authorization port)
	    (test-encoding)
	    (test-forms port)
	    (test-client port)
	    )
	(stop-aserve-running))))
  (if* (or (> util.test::*test-errors* 0)
	   (> util.test::*test-successes* 0)
	   (> util.test::*test-unexpected-failures* 0))
     then (format t "~%Test information from other threads:~%")
	  (format t "Errors:    ~d~%" util.test::*test-errors*)
	  (format t "Successes: ~d~%~%" util.test::*test-successes*)
	  (format t "Unexpected failures: ~d~%" 
		  util.test::*test-unexpected-failures*)))
    


(defun start-aserve-running ()
  ;; start aserve, return the port on which we've started aserve
  (let ((wserver (start :port nil)))	; let the system pick a port
    (unpublish :all t) ; flush anything published
    (socket::local-port (net.aserve::wserver-socket wserver))
    ))

(defun stop-aserve-running ()
  (shutdown))




(defmacro values2 (form)
  ;; return the second value
  (let ((v1 (gensym))
	(v2 (gensym)))
    `(multiple-value-bind (,v1 ,v2) ,form
       (declare (ignore ,v1))
       ,v2)))

;-------- publish-file tests

(defvar *dummy-file-value* nil)
(defvar *dummy-file-name*  "aservetest.xx")

(defun build-dummy-file (length line-length name)
  ;; write a dummy file named  name  (if name isn't nil)
  ;; of a given   length   with spaces every line-length characters
  ;; Return the string holding the contents of the file.
  (let ((strp (make-string-output-stream))
	(result))
    (dotimes (i length)
      (write-char (code-char (+ #.(char-code #\a) (mod i 26))) strp)
      (if* (zerop (mod (1+ i) line-length))
	 then ; newlines cause a problem due to dos/unix differences.
	      ; so let's just use space
	      (write-char #\space strp)))
    (setq result (get-output-stream-string strp))
    (if* name
       then (with-open-file (p name :direction :output
			     :if-exists :supersede)
	      (write-sequence result p)))
    
    result))
  

(defun test-publish-file (port)
  (let (dummy-1-contents 
	(dummy-1-name "xxaservetest.txt")
	dummy-2-contents
	(dummy-2-name "xx2aservetest.txt")
	(prefix-local (format nil "http://localhost:~a" port))
	(prefix-dns   (format nil "http://~a:~a" 
			      (long-site-name)
			      port)))
    
    (setq dummy-1-contents (build-dummy-file 8055 70 dummy-1-name))

    
    ;; basic publish file test
    ;;
    ;; publish a file and retrieve it.
    ;; the result will be the same since given that we know the
    ;; length of the file, chunking won't be needed
    ;; 
    (publish-file :path "/frob" :file dummy-1-name
		  :content-type "text/plain")

    ;; 
    (dolist (cur-prefix (list prefix-local prefix-dns))
      (dolist (keep-alive '(nil t))
	(dolist (protocol '(:http/1.0 :http/1.1))
	  (format t "test 1 - ~s~%" (list keep-alive protocol))
	  (multiple-value-bind (body code headers)
	      (do-http-request (format nil "~a/frob" cur-prefix)
		:protocol protocol
		:keep-alive keep-alive)
	    (test 200 code)
	    (test (format nil "text/plain" port)
		  (cdr (assoc "content-type" headers :test #'equal))
		  :test #'equal)
	    #+ignore (if* (eq protocol :http/1.1)
			then (test "chunked"
				   (cdr (assoc "transfer-encoding" headers 
					       :test #'equal))
				   :test #'equalp))
	    (test dummy-1-contents body :test #'equal)))))


    (setq dummy-2-contents (build-dummy-file 8055 65 dummy-2-name))

    
    ;; preload publish file test
    ;;
    ;; publish a file and retrieve it.
    ;; ** Preload this time **
    ;;
    ;; the result will be the same since given that we know the
    ;; length of the file, chunking won't be needed
    ;; 
    (publish-file :path "/frob2" :file dummy-2-name
		  :content-type "text/plain"
		  :preload t)

    ;; 
    (dolist (cur-prefix (list prefix-local prefix-dns))
      (dolist (keep-alive '(nil t))
	(dolist (protocol '(:http/1.0 :http/1.1))
	  (format t "test 2 - ~s~%" (list keep-alive protocol))
	  (multiple-value-bind (body code headers)
	      (do-http-request (format nil "~a/frob2" cur-prefix)
		:protocol protocol
		:keep-alive keep-alive)
	    (test 200 code)
	    (test (format nil "text/plain" port)
		  (cdr (assoc "content-type" headers :test #'equal))
		  :test #'equal)
	    #+ignore (if* (eq protocol :http/1.1)
			then (test "chunked"
				   (cdr (assoc "transfer-encoding" headers 
					       :test #'equal))
				   :test #'equalp))
	    (test dummy-2-contents body :test #'equal)))))

    
    ;;;; remove published file test
    ;;
    ; verify it's still there
    (test 200 (values2 (do-http-request (format nil "~a/frob" prefix-local))))
    (test 200 (values2 (do-http-request (format nil "~a/frob" prefix-dns))))
    
    ; remove it
    (publish-file :path "/frob" :remove t)
    
    ; verify that it's not there:
    (test 404 (values2 (do-http-request (format nil "~a/frob" prefix-local))))
    (test 404 (values2 (do-http-request (format nil "~a/frob" prefix-dns))))
    
    ;; likewise for frob2
    
    ; verify it's still there
    (test 200 (values2 (do-http-request (format nil "~a/frob2" prefix-local))))
    (test 200 (values2 (do-http-request (format nil "~a/frob2" prefix-dns))))
    
    ; remove it
    (publish-file :path "/frob2" :remove t)
    
    ; verify that it's not there:
    (test 404 (values2 (do-http-request (format nil "~a/frob2" prefix-local))))
    (test 404 (values2 (do-http-request (format nil "~a/frob2" prefix-dns))))
    
    

    
    ;; now add different files for localhost and the dns names
    ;; and verify that we get served different files based on
    ;; the virtual host we choose
    (publish-file :path "/checkit" 
		  :host "localhost"
		  :file dummy-1-name
		  :content-type "text/plain")
    
    (publish-file :path "/checkit" 
		  :host (long-site-name)
		  :file dummy-2-name
		  :content-type "text/plain")
    
    (multiple-value-bind (body code headers)
	(do-http-request (format nil "~a/checkit" prefix-local))
      (declare (ignore headers))
      (test 200 (and :df-test code))
      (test dummy-1-contents body :test #'equal))
    
    (multiple-value-bind (body code headers)
	(do-http-request (format nil "~a/checkit" prefix-dns))
      (declare (ignore headers))
      (test 200 (and :df-test code))
      (test dummy-2-contents body :test #'equal))

    ;; remove the localhost one
    (publish-file :path "/checkit" 
		  :host "localhost"
		  :remove t)
    ; verify it's gone:
    (test 404 (values2 (do-http-request (format nil "~a/checkit" 
					       prefix-local))))
    ; but the the dns one is still there
    (test 200 (values2 (do-http-request (format nil "~a/checkit" prefix-dns))))
    
    ; remove the dns one
    (publish-file :path "/checkit" 
		  :host (long-site-name)
		  :remove t)
    
    ; verify it's gone too
    (test 404 (values2 (do-http-request (format nil "~a/checkit" 
					       prefix-dns))))

    
    
    
    
    
    
    ; cleanup
    (delete-file dummy-1-name)
    (delete-file dummy-2-name)
    ))
    



(defun test-publish-computed (port)
  ;; test publishing computed entities
  (let ((dummy-1-content (build-dummy-file 0 50 nil))
	(dummy-2-content (build-dummy-file 1 50 nil))
	(dummy-3-content (build-dummy-file 100 50 nil))
	(dummy-4-content (build-dummy-file 1000 50 nil))
	(dummy-5-content (build-dummy-file 10000 50 nil))
	(dummy-6-content (build-dummy-file 100000 50 nil))
	
	(prefix-local (format nil "http://localhost:~a" port))
	)

    ;;
    ;; publish strings of various sizes using various protocols
    ;; verify that chunking is turned on when we select http/1.1
    ;; 
    (dolist (pair `(("/dum1" ,dummy-1-content)
		    ("/dum2" ,dummy-2-content)
		    ("/dum3" ,dummy-3-content)
		    ("/dum4" ,dummy-4-content)
		    ("/dum5" ,dummy-5-content)
		    ("/dum6" ,dummy-6-content)))

      (let ((this (cadr pair)))
	;; to make a separate binding for each function
	(publish :path (car pair) 
		 :content-type "text/plain"
		 :function
		 #'(lambda (req ent)
		     (with-http-response (req ent)
		       (with-http-body (req ent)
			 (write-sequence this *html-stream*))))))
      (dolist (keep-alive '(nil t))
	(dolist (protocol '(:http/1.0 :http/1.1))
	  (multiple-value-bind (body code headers)
	      (do-http-request (format nil "~a~a" prefix-local (car pair))
		:protocol protocol
		:keep-alive keep-alive)
	    (test 200 code)
	    (test (format nil "text/plain" port)
		  (cdr (assoc "content-type" headers :test #'equal))
		  :test #'equal)
	    (if* (eq protocol :http/1.1)
	       then (test "chunked"
			  (cdr (assoc "transfer-encoding" headers 
				      :test #'equal))
			  :test #'equalp))
	    (test (cadr pair) body :test #'equal)))))))


(defun test-authorization (port)
  (let ((prefix-local (format nil "http://localhost:~a" port))
	(prefix-dns   (format nil "http://~a:~a" 
			      (long-site-name) port)))
    
    ;; manual authorization testing
    ;; basic authorization
    ;;
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
    
    ; no dice with no password
    (multiple-value-bind (body code headers)
	(do-http-request (format nil "~a/secret" prefix-local))
      (declare (ignore body))
      (test 401 code)
      ; verify that we are asking for the right realm
      (test "Basic realm=\"secretserver\""
	    (cdr (assoc "www-authenticate" headers :test #'equal))
	    :test #'equal))
  
    
    ; good password
    (test 200
	  (values2 (do-http-request (format nil "~a/secret" prefix-local)
		    :basic-authorization '("foo" . "bar"))))
    
    ; bad password
    (test 401
	  (values2 (do-http-request (format nil "~a/secret" prefix-local)
		    :basic-authorization '("xxfoo" . "bar"))))
    


    
    ;; manual authorization testing, testing via ip address
    
    (publish :path "/local-secret"
	     ;; this only "works" if we reference via localhost
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
		  else (failed-request req)))))
    
    (test 200
	  (values2 (do-http-request (format nil "~a/local-secret"
					   prefix-local))))
    
    (test 404
	  (values2 (do-http-request (format nil "~a/local-secret"
					   prefix-dns))))
    
    
    ;;
    ;; password authorizer class
    ;;
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
    
    (multiple-value-bind (body ccode headers)
	(do-http-request (format nil "~a/secret-auth" prefix-local))
      (declare (ignore body))
      (test 401 ccode)
      (test "Basic realm=\"SecretAuth\""
	    (cdr (assoc "www-authenticate" headers :test #'equal))
	    :test #'equal))
    
    (test 200
	  (values2 (do-http-request (format nil "~a/secret-auth" prefix-local)
		    :basic-authorization '("foo2" . "bar2"))))
    
    (test 200
	  (values2 (do-http-request (format nil "~a/secret-auth" prefix-local)
		    :basic-authorization '("foo3" . "bar3"))))
    
    (test 401
	  (values2 (do-http-request (format nil "~a/secret-auth" prefix-local)
		    :basic-authorization '("foo4" . "bar4"))))
    

    ;;
    ;; location authorizers
    ;; 
    (let ((loca (make-instance 'location-authorizer
			       :patterns nil)))
      (publish :path "/secret-loc-auth"
	 :content-type "text/html"
	 :authorizer loca
	 :function
	 #'(lambda (req ent)
	     (with-http-response (req ent)
	       (with-http-body (req ent)
		 (html (:head (:title "Secret page"))
		       (:body "You made it to the secret page"))))))
      
      ;; with a nil pattern list this should accept connections
      ;; from anywhere
      
      (test 200
	  (values2 (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-local))))
      (test 200
	  (values2 (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-dns))))
      
      ; now deny all
      (setf (location-authorizer-patterns loca) '(:deny)) 
      
      (test 404
	  (values2 (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-local))))
      
      (test 404
	  (values2 (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-dns))))
      
      
      ;; accept from localhost only
      (setf (location-authorizer-patterns loca) 
	'((:accept "127.0" 8)
	  :deny))
      (test 200
	  (values2 (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-local))))
      
      (test 404
	  (values2 (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-dns))))
      
      ;; accept from dns name only 
      
      (setf (location-authorizer-patterns loca) 
	`((:accept ,(long-site-name))
	  :deny))
      
      (test 404
	  (values2 (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-local))))
      
      (test 200
	  (values2 (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-dns))))
      
      
      ;; deny dns and accept all others
      (setf (location-authorizer-patterns loca) 
	`((:deny ,(long-site-name))
	  :accept))
      
      (test 200
	  (values2 (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-local))))
      
      (test 404
	  (values2 (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-dns))))
      
      
      ;; deny localhost and accept all others
      (setf (location-authorizer-patterns loca) 
	'((:deny "127.0" 8)
	  :accept))
      
      (test 404
	  (values2 (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-local))))
      
      (test 200
	  (values2 (do-http-request (format nil "~a/secret-loc-auth"
					   prefix-dns))))
      
    
    
    
    
    
    
      )))


(defun test-encoding ()
  ;; test the encoding and decoding
  (let ((str1 (make-string 256))
	(str2 (make-string 256)))
    (dotimes (i 256)
      (setf (schar str1 i) (code-char i))
      (setf (schar str1 i) (code-char (mod (+ i 10) 256))))
    
    (let ((query `(("foo bar" . "baz")
		   (,str1 . "a b c d")
		   ("efffg" . ,str2))))
      (test (form-urlencoded-to-query
	     (query-to-form-urlencoded query))
	    query
	    :test #'equal))))
		       

    
    
(defun test-forms (port)
  ;; test encoding and decoding info
  ;;
  ;; we can send the info as a uri query or as the body of a post
  ;;
  (let ((prefix-local (format nil "http://localhost:~a" port))
	(uri-var-vals '(("marketing" . "sammy c")
			("sales" . "masako o")
			("tech group" . "A Whole Big <Group> of Folks?")))
	(post-var-vals
	 '(("cessna" . "good#")
	   ("piper"  . "better###")
	   ("grumman" . "best<>###")))
	(req-query-res)
	)
    

    ;;-------------------------

    (publish :path "/form-tester-both"
	     :content-type "text/html"
	     :function
	     #'(lambda (req ent)
		 ;; get both uri and post
		 (if* (eql (request-method req) :post)
		    then (test "application/x-www-form-urlencoded"
			       (header-slot-value req "content-type")
			       :test #'equal))
		 (setq req-query-res (request-query req))
		 (with-http-response (req ent)
		   (with-http-body (req ent)
		     (html "hi")))))

    
    ;; send query only on uri
    (do-http-request (format nil "~a/form-tester-both?~a" 
			     prefix-local
			     (query-to-form-urlencoded uri-var-vals)))
    
    (test nil (set-difference uri-var-vals req-query-res
			      :test #'equal))
    
    
    ; - use query arg
    (do-http-request (format nil "~a/form-tester-both" prefix-local)
      :query uri-var-vals)
			     
    (test nil (set-difference uri-var-vals req-query-res
			      :test #'equal))  
    
    
    
    
    ;; send query only on post
    (do-http-request (format nil "~a/form-tester-both" 
			     prefix-local)
      :method :post
      :content (query-to-form-urlencoded post-var-vals)
      :content-type "application/x-www-form-urlencoded"
      )
    
    (test nil (set-difference post-var-vals req-query-res
			      :test #'equal))
    
    
    (do-http-request (format nil "~a/form-tester-both" 
			     prefix-local)
      :method :post
      :query post-var-vals)
    
    (test nil (set-difference post-var-vals req-query-res
			      :test #'equal))
    
    
    ;; send query on both uri and post
    (do-http-request (format nil "~a/form-tester-both?~a" 
			     prefix-local
			     (query-to-form-urlencoded uri-var-vals))
	
      :method :post
      :content (query-to-form-urlencoded post-var-vals)
      :content-type "application/x-www-form-urlencoded"
      )
    
    (test nil (set-difference (append post-var-vals 
				      uri-var-vals)
			      req-query-res
			      :test #'equal))
    
    
    ;;------------------------------------
    
    ;; only check uri
    
    (publish :path "/form-tester-uri"
	     :content-type "text/html"
	     :function
	     #'(lambda (req ent)
		 ;; get both uri and post
		 (setq req-query-res (request-query req :post nil))
		 (with-http-response (req ent)
		   (with-http-body (req ent)
		     (html "hi")))))
    
    
    (do-http-request (format nil "~a/form-tester-uri?~a" 
			     prefix-local
			     (query-to-form-urlencoded uri-var-vals)))
    
    (test nil (set-difference uri-var-vals req-query-res
			      :test #'equal))
    
    
    
    ;; send query only on post
    (do-http-request (format nil "~a/form-tester-uri" 
			     prefix-local)
      :method :post
      :content (query-to-form-urlencoded post-var-vals)
      :content-type "application/x-www-form-urlencoded"
      )
    
    (test nil req-query-res)
    
    
    ;; send query on both uri and post
    (do-http-request (format nil "~a/form-tester-uri?~a" 
			     prefix-local
			     (query-to-form-urlencoded uri-var-vals))
	
      :method :post
      :content (query-to-form-urlencoded post-var-vals)
      :content-type "application/x-www-form-urlencoded"
      )
    
    (test nil (set-difference uri-var-vals
			      req-query-res
			      :test #'equal))
    
    ;;-------------------------
    
    ; only check post
    
    (publish :path "/form-tester-post"
	     :content-type "text/html"
	     :function
	     #'(lambda (req ent)
		 ;; get both uri and post
		 (setq req-query-res (request-query req :uri nil))
		 (with-http-response (req ent)
		   (with-http-body (req ent)
		     (html "hi")))))
    

    (do-http-request (format nil "~a/form-tester-post?~a" 
			     prefix-local
			     (query-to-form-urlencoded uri-var-vals)))
    
    (test nil  req-query-res)
    
    
    
    ;; send query only on post
    (do-http-request (format nil "~a/form-tester-post" 
			     prefix-local)
      :method :post
      :content (query-to-form-urlencoded post-var-vals)
      :content-type "application/x-www-form-urlencoded"
      )
    
    (test nil (set-difference req-query-res post-var-vals :test #'equal))
    
    
    ;; send query on both uri and post
    (do-http-request (format nil "~a/form-tester-post?~a" 
			     prefix-local
			     (query-to-form-urlencoded uri-var-vals))
	
      :method :post
      :content (query-to-form-urlencoded post-var-vals)
      :content-type "application/x-www-form-urlencoded"
      )
    
    (test nil (set-difference post-var-vals req-query-res :test #'equal))

    ;
    ; test that we can do get-request-body more than once
    ;
    (publish :path "/get-request-body-tester"
	     :content-type "text/plain"
	     :function
	     #'(lambda (req ent)
		 
		 (with-http-response (req ent)
		   (test t 
			 (equal (get-request-body req)
				"foo and bar"))
		   (test t 
			 (equal (get-request-body req)
				"foo and bar"))
		   (with-http-body (req ent)))))
    (do-http-request (format nil "~a/get-request-body-tester" 
			     prefix-local)
      :method :post
      :content "foo and bar"
      :content-type "text/plain")
    
    ))
    

  
(defun test-client (port)
  (let ((prefix-local (format nil "http://localhost:~a" port)))
  
    ;; test redirection
    (publish :path "/redir-target"
	     :content-type "text/plain"
	     :function #'(lambda (req ent)
			   (with-http-response (req ent)
			     (with-http-body (req ent)
			       (html "foo")))))
  
    (publish :path "/redir-to"
	     :function #'(lambda (req ent)
			   (with-http-response (req ent
						    :response *response-found*)
			     (setf (reply-header-slot-value req "location") 
			       "redir-target")
			     (with-http-body (req ent)))))
    
    ; redirect to itself... danger danger!
    (publish :path "/redir-inf"
	     :function #'(lambda (req ent)
			   (with-http-response (req ent
						    :response *response-found*)
			     (setf (reply-header-slot-value req "location") 
			       "redir-inf")
			     (with-http-body (req ent)))))
    
  
    ; first test target
    (multiple-value-bind (body code headers)
	(do-http-request (format nil "~a/redir-target" prefix-local))
      (declare (ignore body headers))
      (test 200 code))
  
    ; now test through redirect
    (multiple-value-bind (body code headers)
	(do-http-request (format nil "~a/redir-to" prefix-local))
      (declare (ignore body headers))
      (test 200 (and :second code)))
  
    ; now turn off redirect and test
    (multiple-value-bind (body code headers)
	(do-http-request (format nil "~a/redir-to" prefix-local) :redirect nil)
      (declare (ignore body headers))
      (test 302 (and :third code)))

    ; turn off with a zero repeat count
    (multiple-value-bind (body code headers)
	(do-http-request (format nil "~a/redir-to" prefix-local) :redirect 0)
      (declare (ignore body headers))
      (test 302 (and :fourth code)))

    
    ; self redirect, we test that we eventually give up
    (multiple-value-bind (body code headers)
	(do-http-request (format nil "~a/redir-inf" prefix-local))
      (declare (ignore body headers))
      (test 302 (and :fifth code)))
    ))
  
  
  


    
    





      
    
    
    
    
			  
		   
    
    
    



    
(if* user::*do-aserve-test* then (test-aserve))

	
    
   
  
  

	
  

  

