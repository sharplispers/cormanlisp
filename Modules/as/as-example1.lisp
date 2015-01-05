;;;; AllegroServe Example for Corman Lisp - Version 1.0
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
;;;; More recent versions of this software may be available at:
;;;;   http://www.double.co.nz/cl
;;;;
;;;; Comments, suggestions and bug reports to the author, 
;;;; Christopher Double, at: chris@double.co.nz
;;;;
;;;; 03/03/2000 - 1.0 
;;;;              Initial release.
;;;;
(require 'allegroserve)

(defpackage "AS-EXAMPLE1"
  (:use 
		"COMMON-LISP" 
		"EXCL" 
		"NET.HTML.GENERATOR" 
		"NET.ASERVE"))

(in-package :as-example1)

(setq *wserver* 
	(make-instance 'wserver :enable-chunking nil :enable-keep-alive nil))

(publish 
	:path "/"
	:content-type "text/html"
	:function
	(let ((count 0))
		#'(lambda (req ent)
			(with-http-response (req ent)
				(with-http-body (req ent)
					(html
						(:head (:title "AllegroServe powered by Corman Lisp 1.42"))
						(:body
							(:h1 "Welcome to AllegroServe")
							(:p "This web server is powered by AllegroServe, a web server
								written in Common Lisp. The Lisp implementation running this
								server is Corman Lisp v1.42")
							(:i "This server's host name is "
								(:princ-safe (header-slot-value req "host")))
							:p
							(:b "Sample pages") :br
							((:a :href "gc") "Garbage Collector Stats") :br
							(:p "This page has been accessed "
								(:princ-safe (incf count))
								" times."))))))))
							
	
(publish 
	:path "/gc"
	:content-type "text/html"
	:function
	#'(lambda (req ent)
		(let ((room-output
					(with-output-to-string (ss)
						(let ((*standard-output* ss))
							(room)))))
			(with-http-response (req ent)
				(with-http-body (req ent)
					(html
						(:head (:title "Garbage Collection Statistics"))
						(:body
							(:h1 "Room")
							(:pre
								(:princ-safe room-output)))))))))
	
(defun start-server (&key (port 80))
	(mp:process-run-function "as-example1" 
		#'(lambda ()
			(start :server *wserver* :port port :chunking nil))))

(defun start-simple-server (&key (port 80))
	(start :server *wserver* :port port :chunking nil :listeners 0))

#|
(in-package :as-example1)
;;(net.aserve::debug-on :info :notrap)
(start-server)
(start-simple-server)
(mp:proc)
|#

#|
(in-package :as-example1)
(use-package :net.aserve.client)

(setq cookies (make-instance 'cookie-jar))
(do-http-request "http://www.double.co.nz/cl/index.htm" 
	:cookies cookies
	:protocol :http/1.0)
(net.aserve.client::cookie-jar-items cookies)
|#	
