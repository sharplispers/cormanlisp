;;;-*- Mode: Lisp; Package: (FFC) -*-

;; Foreign function compatibility module for MCL, LWW and ACL (LWW version)
;; Version 0.85
;; Copyright (C) Paul Meurer 1999. All rights reserved.
;; paul.meurer@hit.uib.no
;;
;; Documentation and the license agreement can be found in file 
;; "sql-odbc-documentation.lisp".
;; Bug reports and suggestions are highly welcome.

;; In this file the platform specific code is isolated.
;; The code in this file consists mostly of wrapper functions and macros 
;; around the platform-dependent foreign function interface.

;; This file contains LWW specific code.

(defpackage "FFC"
  (:use "COMMON-LISP")
  (:export "*FOREIGN-MODULE*" "DEFINE-FOREIGN-FUNCTION" 
    "MAKE-RECORD"
    "%WITH-TEMPORARY-ALLOCATION" "%WITH-SQL-POINTER" "%GET-CSTRING"
    "%CSTRING-INTO-STRING"
    "%CSTRING-INTO-VECTOR"
    "%GET-CSTRING-LENGTH" "WITH-CSTR" "%GET-PTR" "%NEW-PTR" "%DISPOSE-PTR"
    "%GET-SIGNED-WORD"
    "%GET-UNSIGNED-LONG"
    "%GET-SIGNED-LONG"
    "%GET-SINGLE-FLOAT"
    "%GET-DOUBLE-FLOAT"
    "%GET-WORD"
    "%GET-LONG"
    "%GET-SHORT"
    "%GET-SIGNED-LONG"
    "%GET-BIT"
    "%PUT-STR"
    "%PUT-WORD"
    "%PUT-SHORT"
    "%PUT-LONG"
    "%NEW-CSTRING" 
    "%NULL-PTR"
    "%PTR-EQL"
    "%GET-BINARY" "%PUT-BINARY" "%NEW-BINARY"
    "STRING-PTR" "SQL-HANDLE" "SQL-HANDLE-PTR"))

(in-package :ffc)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *foreign-module* nil))

;; LispWorks (Windows) specific code

(defun %get-cstring-length (ptr)
  (loop with size = 0
        until (char= (fli:dereference ptr :index size) #\Null)
        do (incf size)
        finally return size))

(defun %cstring-into-string (ptr string offset size-in-bytes)
    (dotimes (i size-in-bytes)
      (setf (char string offset)
            (fli:dereference ptr :index i))
      (incf offset))
    offset)

(defun %cstring-into-vector (ptr vector offset size-in-bytes)
    (dotimes (i size-in-bytes)
      (setf (aref vector offset)
            (fli:dereference ptr :index i))
      (incf offset))
    offset)

(defmacro with-cstr ((ptr str) &body body)
  `(fli:with-foreign-string (,ptr element-count byte-count
                                  :external-format 
                                  #+win32 win32:*multibyte-code-page-ef*
                                  #-win32 :latin-1) ; for Linux
       ,str
     (declare (ignore element-count byte-count))
     ,@body))

(defun %cstring-to-keyword (pointer)
  (let* ((len (%get-cstring-length pointer))
         (str (make-string len)))
    (declare (dynamic-extent str))
    (%cstring-into-string pointer str 0 len)
    (intern str (find-package 'keyword))))

(defun %new-ptr (type &optional bytecount)
  (fli:allocate-foreign-object :type 
                    (if bytecount
                      (list type bytecount)
                      type)))

(defun %dispose-ptr (p)
  (fli:free-foreign-object p))

(defmacro %with-sql-pointer ((pointer-var) &body body)
  `(let ((,pointer-var (fli:allocate-foreign-object :pointer-type 'sql-handle-ptr)))
     ,@body))

(defmacro %null-ptr ()
  '(fli:make-pointer :address 0 :type :void))

(defmacro %ptr-eql (ptr1 ptr2)
  `(= (fli:pointer-address ,ptr1) 
      (fli:pointer-address ,ptr2)))

(defmacro %address-to-pointer (address)
  `(fli:make-pointer :address ,address :type :void))

(defmacro %pointer-to-address (pointer)
  `(fli:pointer-address ,pointer))

;; all the same ...
(defmacro %get-ptr (ptr) `(fli:dereference ,ptr))

(defmacro %get-short (ptr) `(fli:dereference ,ptr))
(defmacro %put-short (ptr short) `(setf (%get-ptr ,ptr) ,short))

;; a cludge! better way?
(defmacro %get-bit (ptr) `(- (char-code (fli:dereference ,ptr :type :char))))

(defmacro %get-long (ptr) `(fli:dereference ,ptr))
(defmacro %put-long (ptr long) `(setf (%get-ptr ,ptr) ,long))

(defmacro %get-signed-word (ptr) `(fli:dereference ,ptr))

(defmacro %get-word (ptr) `(fli:dereference ,ptr))
(defmacro %put-word (ptr word) `(setf (%get-ptr ,ptr) ,word))

(defmacro %get-unsigned-long (ptr) `(fli:dereference ,ptr))

(defmacro %get-signed-long (ptr) `(fli:dereference ,ptr))

(defmacro %get-single-float (ptr) `(fli:dereference ,ptr))

(defmacro %get-double-float (ptr) `(fli:dereference ,ptr))

#+conses-too-much
(defun %get-cstring (ptr)
  (fli:convert-from-foreign-string 
   ptr :external-format win32:*multibyte-code-page-ef*))

(defun %get-cstring (ptr &optional (start 0))
  (let ((size 0))
    (fli:incf-pointer ptr start)
    (loop until (char= (fli:dereference ptr) #\Null)
          do
          (fli:incf-pointer ptr) ; better use offset??
          (incf size))
    (let ((str (make-string size)))
      (loop do 
            (fli:incf-pointer ptr -1)
            (decf size)
            (setf (char str size)
                  (fli:dereference ptr))
            until (zerop size))
      (fli:decf-pointer ptr start)
      str)))

(defmacro %put-str (ptr string &optional max-length)
  (let ((size (gensym)))
    `(let ((,size (length ,string)))
       (when (and ,max-length (> ,size ,max-length))
         (error "string \"~a\" of length ~d is longer than max-length: ~d"
                ,string ,size ,max-length))
       (dotimes (i ,size)
         (setf (fli:dereference ,ptr :index i) (char ,string i)))
       (setf (fli:dereference ,ptr :index ,size) 0))))

(defmacro %new-cstring (byte-count)
  `(fli:allocate-foreign-object :type :char :initial-element #\ø :nelems ,byte-count))

(defmacro make-record (type)
  `(fli:allocate-foreign-object :type ',type))

(fli:register-module "odbc32" :connection-style :automatic)

(fli:define-c-typedef sql-handle (:pointer :void))
(fli:define-c-typedef sql-handle-ptr (:pointer sql-handle))
(fli:define-c-typedef string-ptr :pointer)

(defmacro define-foreign-function (c-name args result-type &key documentation module)
  (let ((name-list (list (intern (string-upcase c-name)) c-name :source)))
    `(fli:define-foreign-function 
         ,name-list
         ,args
       :result-type ,result-type
       :language :ansi-c
       :documentation ,documentation
       :module (or ,module ,*foreign-module*))))

#+does-not-work
(defun allocate-dynamic-string (size)
  (fli:allocate-dynamic-foreign-object
   :type `(:ef-wc-string ,size)
   :initial-element (make-string size :initial-element #\Space)))

(defun allocate-dynamic-string (size)
  (fli:allocate-dynamic-foreign-object
   :type :char
   :initial-element #\Space
   :nelems size))

(defmacro %with-temporary-allocation (bindings &body body)
  (let ((simple-types ())
        (strings ())) 
    (dolist (binding bindings)
      (case (cadr binding)
        (:string (push (list (car binding)
                             (list 'allocate-dynamic-string (caddr binding))) strings))
        (otherwise (push (list (car binding) (cadr binding)) simple-types))))
    `(fli:with-dynamic-foreign-objects ,simple-types
       (let ,strings
         ,@body))))

;(fli:allocate-foreign-object :type 'sql-c-timestamp)

;; returns a byte array
(defun %get-binary (ptr len format)
  "FORMAT is one of :unsigned-byte-vector, :bit-vector (:string, :hex-string)"
  (ecase format
    (:unsigned-byte-vector
     (let ((vector (make-array len :element-type 'unsigned-byte)))
       (dotimes (i len)
         (setf (svref vector i) (fli:dereference ptr :index i :type :unsigned-byte)))
       vector))
    (:bit-vector
     (let ((vector (make-array (ash len 3) :element-type 'bit)))
       (dotimes (i len)
         (let ((byte (fli:dereference ptr :index i :type :unsigned-byte)))
           (dotimes (j 8)
             (setf (bit vector (+ (ash i 3) j)) (logand (ash byte (- j 7)) 1)))))
       vector))))

;; returns size in bytes
(defun %put-binary (ptr vector &optional max-length)
  (cond ((bit-vector-p vector)
         (let* ((bit-count (length vector))
                (byte-count (print (ceiling bit-count 8))))
           (when (and max-length (> byte-count max-length))
             (error "bit vector of length ~d is longer than max-length: ~d"
                    bit-count (* max-length 4)))
           (dotimes (i byte-count)
             (let ((byte 0))
               (dotimes (j 8)
                 (let ((index (+ (ash i 3) j)))
                   (if (< index bit-count)
                       (setf byte (logior byte (ash (bit vector (+ (ash i 3) j)) (- 7 j))))
                     (return))))
               (setf (fli:dereference ptr :index i :type :unsigned-byte) byte)))
           byte-count))
        (t (error "not yet implemented"))))

(defmacro %new-binary (byte-count)
  `(fli:allocate-foreign-object :type :unsigned-byte :nelems ,byte-count))
