;;;; XML-RPC library for Corman Lisp - Version 1.1
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
;;;; See the examples at the end of the file for useage. It has been tested
;;;; with at least version 1.41 of Corman Lisp available at 
;;;; http://www.corman.net
;;;;
;;;; More recent versions of this software may be available at:
;;;;   http://www.double.nz/cl
;;;;
;;;; Comments, suggestions and bug reports to the author, 
;;;; Christopher Double, at: chris@double.nz
;;;;
;;;; 09/10/2000 - 1.0 
;;;;              Initial release.
;;;;              Examples at end of file. Still need to implement date/time
;;;;              and base64.
;;;;
(require :sockets)

(defpackage "XMLRPC"
	(:use 
		:COMMON-LISP 
		:SOCKETS)
	(:export 
		"XML-RPC-SEND"
		"XMLRPC-FAULT"
		"XMLRPC-FAULT-CODE"
		"XMLRPC-FAULT-STRING"
		"DEFINE-XMLRPC-CLIENT-METHOD"
		))
(in-package :xmlrpc)

;; First define META routines to parse XML-RPC responses
(defmacro match (x)
	(etypecase x
		(character
			`(when (and (< index end) (eql (char string index) ',x))
				(incf index)))
		(string
			`(let ((old-index index)) ; 'old-index' is a lexical variable.
				(or (and ,@(map 'list #'(lambda (c) `(match ,c)) x))
					(progn (setq index old-index) nil))))))

(defmacro match-type (x v)
	`(when (and (< index end) (typep (char string index) ',x))
		(setq ,v (char string index)) (incf index)))

(defstruct (meta
		(:print-function
			(lambda (m s d &aux (char (meta-char m)) (form (meta-form m)))
                (declare (ignore d))
				(ecase char
					((#\@ #\! #\$) (format s "~A~A" char form))
					(#\[ (format s "[~{~A~^ ~}]" form))
					(#\{ (format s "{~{~A~^ ~}}" form))))))
	char
	form)

(defun compileit (x)
	(typecase x
		(meta
			(ecase (meta-char x)
				(#\! (meta-form x))
				(#\[ `(and ,@(mapcar #'compileit (meta-form x))))
				(#\{ `(or ,@(mapcar #'compileit (meta-form x))))
				(#\$ `(not (do ()((not ,(compileit (meta-form x)))))))
				(#\@ (let ((f (meta-form x))) `(match-type ,(car f) ,(cadr f))))))
		(t `(match ,x))))

(defmacro matchit (x)
	(compileit x))

(defparameter *saved-readtable* (copy-readtable))
(defparameter *meta-readtable* (copy-readtable))

(defun meta-reader (s c) (make-meta :char c :form (read s)))

(mapc #'(lambda (c) (set-macro-character c #'meta-reader nil *meta-readtable*)) '(#\@ #\$ #\!))

(set-macro-character #\{
	#'(lambda (s c) (make-meta :char c :form (read-delimited-list #\} s t))) nil *meta-readtable*)

(set-macro-character #\[
	#'(lambda (s c) (make-meta :char c :form (read-delimited-list #\] s t))) nil *meta-readtable*)

(mapc #'(lambda (c) (set-macro-character c (get-macro-character #\) nil)  nil *meta-readtable*))
	'(#\] #\}))

(defmacro with-meta (&body body)
	`(progn
		(copy-readtable *meta-readtable* *readtable*)
		(unwind-protect
			(progn
				,@body)
			(copy-readtable *saved-readtable* *readtable*))))

(defun enable-meta-syntax ()
	(copy-readtable *meta-readtable* *readtable*))

(defun disable-meta-syntax()
	(copy-readtable *saved-readtable* *readtable*))

;; Now onto the XML-RPC routines
(defun is-valid-string-char (ch)
	"Return T if the given character is valid for an XML-RPC string."
	(and (characterp ch)
		(not (eql ch #\<))))

(deftype valid-string-char () '(satisfies is-valid-string-char))

(defun is-valid-method-name-char (ch)
	"Return T if the given character is valid for an XML-RPC method name."
	(member ch
		'(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
			#\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
			#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
			#\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
			#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\_ #\. #\: #\/)))

(deftype valid-method-name-char () '(satisfies is-valid-method-name-char))

(defun is-valid-xml-header-char (ch)
	"Return T if the given character is valid for an XML header (<?xml...?>)"
	(and (characterp ch)
		(not (eql ch #\<))
		(not (eql ch #\>))))

(deftype valid-xml-header-char () '(satisfies is-valid-xml-header-char))

(deftype whitespace () '(member #\Tab #\Space #\Newline #\Return))

(deftype digit () '(member #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define-condition xmlrpc-fault (error)
	((fault-code :initarg :fault-code :initform nil :reader xmlrpc-fault-code)
		(fault-string :initarg :fault-string :initform nil :reader xmlrpc-fault-string))
	(:report (lambda (condition stream)
			(format stream "XML-RPC fault code ~A (~A)." 
				(xmlrpc-fault-code condition)
				(xmlrpc-fault-string condition)))))

(enable-meta-syntax)
(defun parse-response (string &aux (index 0) (end (length string)) last-call-result)
	"Parse the response from an XML-RPC call and return the value of that call."
	(labels ((ctoi (d) 
				(- (char-code d) #.(char-code #\0)))
			(make-temp-array ()
				(let ((array (make-array 10 :fill-pointer t)))
					(setf (fill-pointer array) 0)
					array))
			(array-to-string (array)
				(concatenate 'string array))
			(skip-ws (&aux (old-index index) ch)
                (declare (ignore ch))
				(or
					(matchit [$[@(whitespace ch)]])
					(progn (setq index old-index) nil)))
			(check-integer (&aux (old-index index) d (n 0))
				(or
					(matchit
						[!(skip-ws){"<i4>" "<int>"}
						@(digit d) !(setq n (ctoi d))
						$[@(digit d) !(setq n (+ (* n 10) (ctoi d)))]
						{"</i4>" "</int>"} !(skip-ws) !(setq last-call-result n)])
					(progn (setq index old-index) nil)))
			(check-string (&aux (old-index index) ch (string-result (make-temp-array)))
				(or
					(matchit
						[!(skip-ws) "<string>"
						$[@(valid-string-char ch) !(vector-push-extend ch string-result)]
						"</string>" !(skip-ws) !(setq last-call-result (array-to-string string-result))])
					(progn (setq index old-index) nil)))
			(check-date (&aux (old-index index) ch (string-result (make-temp-array)))
				(or
					(matchit
						[!(skip-ws) "<dateTime.iso8601>"
						$[@(valid-string-char ch) !(vector-push-extend ch string-result)]
						"</dateTime.iso8601>" !(skip-ws) !(setq last-call-result (array-to-string string-result))])
					(progn (setq index old-index) nil)))
			(check-double (&aux (old-index index) ch (string-result (make-temp-array)))
				(or
					(matchit
						[!(skip-ws) "<double>"
						$[@(valid-string-char ch) !(vector-push-extend ch string-result)]
						"</double>" !(skip-ws) !(setq last-call-result (array-to-string string-result))])
					(progn (setq index old-index) nil)))
			(check-base64 (&aux (old-index index) ch (string-result (make-temp-array)))
				(or
					(matchit
						[!(skip-ws) "<base64>"
						$[@(valid-string-char ch) !(vector-push-extend ch string-result)]
						"</base64>" !(skip-ws) !(setq last-call-result (array-to-string string-result))])
					(progn (setq index old-index) nil)))
			(check-name (&aux (old-index index) ch (result (make-temp-array)))
				(or
					(matchit
						[!(skip-ws) "<name>"
						$[@(valid-string-char ch) !(vector-push-extend ch result)]
						"</name>" !(skip-ws) !(setq last-call-result (array-to-string result))])
					(progn (setq index old-index) nil)))
			(check-member (&aux (old-index index) name value)
				(or
					(matchit
						[!(skip-ws) "<member>" !(skip-ws)
						[!(check-name) !(setq name last-call-result)]
						[!(check-value) !(setq value last-call-result)]
						!(skip-ws) "</member>" !(skip-ws) !(setq last-call-result (list name value))])
					(progn (setq index old-index) nil)))
			(check-array (&aux (old-index index) (result (make-temp-array)))
				(or
					(matchit
						[!(skip-ws) "<array>" !(skip-ws)
						"<data>" !(skip-ws)
						[!(check-value) !(vector-push-extend last-call-result result)]
						$[!(check-value) !(vector-push-extend last-call-result result)]
						!(skip-ws) "</data>"
						!(skip-ws) "</array>" !(skip-ws) !(setq last-call-result result)])
					(progn (setq index old-index) nil)))
			(check-struct (&aux (old-index index) (result (make-hash-table :test #'equal)))
				(or
					(matchit
						[!(skip-ws) "<struct>" !(skip-ws)
						[!(check-member) !(setf (gethash (first last-call-result) result) (second last-call-result))]
						$[!(check-member) !(setf (gethash (first last-call-result) result) (second last-call-result))]
						!(skip-ws) "</struct>" !(skip-ws) !(setq last-call-result result)])
					(progn (setq index old-index) nil)))
			(check-method-name (&aux (old-index index) ch (result (make-temp-array)))
				(or
					(matchit
						["<methodName>"
						$[@(valid-method-name-char ch) !(vector-push-extend ch result)]
						"</methodName>" !(setq last-call-result (array-to-string result))])
					(progn (setq index old-index) nil)))
			(check-value (&aux (old-index index) ch (string-result (make-temp-array)))
				(or
					(matchit
						[!(skip-ws) "<value>"
						 {!(check-integer)
						  !(check-string)
						  !(check-struct)
						  !(check-array)
						  !(check-date)
						  !(check-double)
						  !(check-base64)
						  [$[@(valid-string-char ch) 
						      !(vector-push-extend ch string-result)] 
						      !(setq last-call-result (array-to-string string-result))]}
						 "</value>" !(skip-ws)])
					(progn (setq index old-index) nil)))
			(check-param (&aux (old-index index))
				(or
					(matchit
						[!(skip-ws) "<param>" !(skip-ws)
						 !(check-value)
						 !(skip-ws) "</param>" !(skip-ws)])
					(progn (setq index old-index) nil)))
			(check-response-fault (&aux (old-index index))
				(or
					(matchit
						[!(skip-ws) "<fault>" !(skip-ws)
						 !(check-value)
						 !(skip-ws) "</fault>" !(skip-ws)
						 !(error 'xmlrpc-fault 
							:fault-code (gethash "faultCode" last-call-result)
							:fault-string (gethash "faultString" last-call-result))])
					(progn (setq index old-index) nil)))
			(check-response-params (&aux (old-index index))
				(or
					(matchit
						[!(skip-ws) "<params>" !(skip-ws)
						 !(check-param)
						 !(skip-ws) "</params>" !(skip-ws)])
					(progn (setq index old-index) nil)))
			(check-xml-header (&aux (old-index index) ch)
                (declare (ignore ch))
				(or
					(matchit
						[!(skip-ws)
						 "<?xml" !(skip-ws)
						 $[@(valid-xml-header-char ch)]
						 ">"
						 !(skip-ws)])
					(progn (setq index old-index) nil)))
			(check-payload (&aux (old-index index))
				(or
					(matchit
						[!(skip-ws) $[!(check-xml-header)] !(skip-ws)
						 !(skip-ws) "<methodResponse>" !(skip-ws)
						 {!(check-response-params) !(check-response-fault)}
						  !(skip-ws) "</methodResponse>" !(skip-ws) !(setq last-call-result last-call-result)])
					(progn (setq index old-index) nil))))
		(values (matchit {!(check-payload)}) index last-call-result)))
(disable-meta-syntax)
		

#|
(parse-response "123")
(parse-response "<methodResponse><params><param><value><string>test</string></value></param></params></methodResponse>")
|#

(defun hashtable-to-xmlrpc (table)
	"Convert a hash-table to its XML-RPC representation."
	(let ((members ""))
		(maphash #'(lambda (key value)
				(setq members
					(concatenate 'string
						members
						"<member><name>"
						key
						"</name><value>"
						(coerce-to-xmlrpc-type value)
						"</value></member>"))) table)
		(concatenate 'string "<struct>" members "</struct>")))

(defun array-to-xmlrpc (array)
	"Convert an array to its XML-RPC representation."
	(let ((members ""))
		(loop for value across array do
			(setq members
				(concatenate 'string
					members
					"<value>"
					(coerce-to-xmlrpc-type value)
					"</value>")))
		(concatenate 'string "<array><data>" members "</data></array>")))

(defun coerce-to-xmlrpc-type (value)
	"Convert a lisp type to the XML-RPC string."
	(cond ((eq value t) "<boolean>1</boolean>")
		((eq value nil) "<boolean>0</boolean>")
		(t
			(typecase value
				(string (format nil "<string>~A</string>" value))
				(fixnum (format nil "<i4>~A</i4>" value))
				(float (format nil "<double>~A</double>" value))
				(hash-table (hashtable-to-xmlrpc value))
				(array (array-to-xmlrpc value))
				(t "Unknown")))))

(defun xml-rpc-send (host url method &rest args)
	"Send an XML-RPC request to HOST at URL calling METHOD with ARGS."
		(let ((request (format nil
						"<?xml version='1.0'?><methodCall><methodName>~A</methodName>" method)))
			(unless (zerop (length args))
				(setq request (concatenate 'string request "<params>"))
				(dolist (arg args)
					(setq request 
						(concatenate 'string request
							(format nil
								"<param><value>~A</value></param>"
								(coerce-to-xmlrpc-type arg)))))
				(setq request (concatenate 'string request "</params>")))
			(setq request (concatenate 'string request "</methodCall>"))
			(with-client-socket (s :host host :port 80)
				(with-socket-stream (stream s)
					(write-line (format nil "POST ~A HTTP/1.0" url) stream)
					(write-line "User-Agent: Corman Lisp 1.41" stream)
					(write-line (format nil "Host: ~A" host) stream)
					(write-line "Pragma: no-cache" stream)
					(write-line "Content-Type: text/xml" stream)
					(write-line (format nil "Content-Length: ~A" (length request)) stream)
					(write-line "" stream)
					(write-sequence request stream)
					(force-output stream)
					(loop as line = (read-line stream nil :eof)
						until (or (eq line :eof) (zerop (length line))))
					(values
						(parse-response (apply #'concatenate 'string 
								(loop as line = (read-line stream nil :eof)
									until (or (eq line :eof) (zerop (length line)))
									collect line))))))))

(defmacro define-xmlrpc-client-method (name server url method-name (&rest args))
	"Macro to create a function wrapper for an XML-RPC call."
	(let ((doc (format nil "XMLRPC client method calling ~A on ~A~A taking arguments ~{~A ~}."
					method-name
					server
					url
					args)))
		`(defun ,name (,@args)
			,doc
			(xml-rpc-send ,server ,url ,method-name ,@args))))

(provide 'xmlrpc)

#|
(sockets::start-sockets)
(xml-rpc-send "betty.userland.com" "/RPC2" "examples.getStateName" 41)
(xml-rpc-send "www.wc.cc.va.us" "/dtod/xmlrpcb2/code/server.asp" "helloWorld" "Chris")
(setq x (make-hash-table :test #'equal))
(setf (gethash "state1" x) 10
	(gethash "state2" x) 20
	(gethash "state3" x) 41)
(xml-rpc-send "betty.userland.com" "/RPC2" "examples.getStateStruct" x)
(xml-rpc-send "betty.userland.com" "/RPC2" "examples.getStateList" (vector 12 28 33 39 46))
(xml-rpc-send "www.wc.cc.va.us" "/dtod/xmlrpcb2/code/server.asp" "superHello" (vector "Chris" "now"))
(xml-rpc-send "betty.userland.com" "/RPC2" "examples.getStateName" (vector 51 22))
(xml-rpc-send "aggregator.userland.com" "/RPC2" "aggregator.getServiceInfo" 747)

;; with macro
(define-xmlrpc-client-method get-state-name 
	"betty.userland.com" 
	"/RPC2" 
	"examples.getStateName" 
	(state-number))

(define-xmlrpc-client-method hello-world
	"www.wc.cc.va.us" 
	"/dtod/xmlrpcb2/code/server.asp" 
	"helloWorld" 
	(name))

(define-xmlrpc-client-method get-state-struct
	"betty.userland.com" 
	"/RPC2" 
	"examples.getStateStruct" 
	(state-structure))

(define-xmlrpc-client-method get-state-list
	"betty.userland.com" 
	"/RPC2" 
	"examples.getStateList" 
	(state-list))

(define-xmlrpc-client-method super-hello
	"www.wc.cc.va.us" 
	"/dtod/xmlrpcb2/code/server.asp" 
	"superHello" 
	(list))

(define-xmlrpc-client-method get-service-info
	"aggregator.userland.com" 
	"/RPC2" 
	"aggregator.getServiceInfo" 
	(channel))

(get-state-name 41)
(hello-world "Chris")
(get-state-struct x)
(get-state-list (vector 1 2 3 4))
(super-hello (vector "Chris" "now"))
(get-service-info 747)

;; More tests
(define-xmlrpc-client-method get-service-info
	"xmlrpc.usefulinc.com" 
	"/demo/server.php" 
	"system.listMethods" 
	(v))

|#
