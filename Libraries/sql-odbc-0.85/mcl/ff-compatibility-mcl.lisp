;;;-*- Mode: Lisp; Package: (FFC) -*-

;; Foreign function compatibility module for MCL, LWW and ACL (MCL version)
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

;; This file contains MCL specific code.

(defpackage "FFC"
  (:use "COMMON-LISP" "CCL")
  (:import-from "CCL" "WITH-CSTR")
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
    "%GET-SIGNED-LONG"
    "%GET-BIT"
    "%PUT-STR"
    "%PUT-WORD"
    "%PUT-SHORT"
    "%PUT-LONG"
    "%NEW-CSTRING" 
    "%NULL-PTR"
    "STRING-PTR" "SQL-HANDLE" "SQL-HANDLE-PTR"
    "%GET-BINARY" "%PUT-BINARY" "%NEW-BINARY"))

(in-package :ffc)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *foreign-module* nil))

;; MCL specific code

(eval-when (:load-toplevel :compile-toplevel :execute)
  (import 'ccl::with-cstr)
  
  (ccl::def-mactype :string-ptr (find-mactype :ptr))
  (ccl::def-mactype :sql-handle (find-mactype :ptr))
  (ccl::def-mactype :sql-handle-ptr (find-mactype :ptr))
  
  ;(setf *foreign-module* "vsi:ODBC$DriverMgr")
  
  (defun mac-to-lisp-type (mac-type)
    (ecase (ccl::make-keyword mac-type)
      ((:ptr :sql-handle :sql-handle-ptr) t)
      (:string-ptr 'string)
      ((:word :short :long) 'fixnum))))

;; Mask the MCL reader macro #_ (Kent Pitman's idea). This is not necessary
;; any more, but nice.
(defun %new-ptr (type &optional bytecount)
  (#.(read-from-string "#_NewPtr")
   (if bytecount bytecount (ccl::record-field-length type))))

(defun %dispose-ptr (p)
  #.(read-from-string "(#_DisposPtr p)"))

(defun %get-cstring-length (pointer)
  (with-pointers ((p pointer))
    (let ((len 0))
      (declare (fixnum len))
      (loop (if (ccl::%izerop (%get-byte p len))
              (return)
              (setq len (ccl::%i+ len 1))))
      len)))

(defmacro %put-str (ptr str &optional length)
 ; can't use name %put-string because of name clash
  (declare (ignore length))
  `(ccl::%cstr-pointer ,str ,ptr))

(defun %%str-pointer (string pointer)
  (multiple-value-bind (s o n) (ccl::dereference-base-string string)
    (declare (fixnum o n))
    (do* ((o o (1+ o))
          (i 0 (1+ i)))
         ((= i n))
      (declare (fixnum o i))
      (setf (%get-byte pointer i) (ccl::%scharcode s o)))))

(defun %cstring-into-string (pointer string start end)
  (let ((delta (- (min (%get-cstring-length pointer) end) start)))
    (with-pointers ((p pointer))
      (ccl::copy-ptr-to-string p string start delta))
    (+ start delta)))

(defun %cstring-into-vector (pointer vector offset size-in-bytes)
  (with-pointers ((p pointer))
    (let ((pos 0)
          (len offset))
      (declare (fixnum len))
      (loop (let ((code (%get-byte p pos))
                  (end (+ offset size-in-bytes)))
              (if (or (ccl::%izerop code) (= len end))
                (return)
                (setf (aref vector len) (code-char code)
                      len (ccl::%i+ len 1)
                      pos (ccl::%i+ pos 1)))))
      len)))

(defun %cstring-to-keyword (pointer)
  (with-pointers ((p pointer))
    (let* ((len (%get-cstring-length pointer))
           (str (make-string len :element-type 'base-character)))
      (declare (dynamic-extent str))
      (ccl::%copy-ptr-to-ivector p 0 str 0 len)
      (intern str (find-package 'keyword)))))

#+allegro
(defun %new-binary (bytecount)
   (allocate-fobject :unsigned-char :c bytecount))

(defmacro %new-binary (bytecount)
  `(%new-ptr :unsigned-char ,bytecount))

;; :string, :hex-string still missing!
(defun %get-binary (ptr len format)
  "FORMAT is one of :unsigned-byte-vector, :bit-vector (:string, :hex-string)"
  (ecase format
    (:unsigned-byte-vector
     (let ((vector (make-array len :element-type 'unsigned-byte)))
       (dotimes (i len)
         (setf (svref vector i)
               (%get-byte ptr i)
               #+ignore
               (sys:memref-int ptr 0 i :unsigned-byte)))
       vector))
    (:bit-vector
     (let ((vector (make-array (ash len 3) :element-type 'bit)))
       (dotimes (i len)
         (let ((byte (%get-byte ptr i)))
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
               (setf (%get-byte ptr i) byte)))
           byte-count))
        (t (error "not yet implemented"))))

(defmacro %with-sql-pointer ((ptr-var) &body body)
  `(%stack-block ((,ptr-var #.(ccl::record-field-length :ptr))) 
     ,@body))

;; bindings is a list of (var type &optional size)
(defmacro %with-temporary-allocation (bindings &body body)
  (let ((args ())) 
    (dolist (binding bindings) ; use destructuring-bind to make this clearer!
      (if (cddr binding)
        (push (list (car binding)
                    (caddr binding)) args)
        (push (list (car binding)
                    (ccl::record-field-length (cadr binding)))
              args)))
    `(%stack-block ,args ; need to reverse them here and in other macros?
       ,@body)))

; args is a list of (var type)'s
(defmacro define-foreign-function (c-name args result-type
                                              &key documentation module)
  (declare (ignore documentation))
  (let ((type-list
         (mapcar #'(lambda (var+type)
                     ; var is not used in MCL
                     (let ((type (ccl::make-keyword (cadr var+type))))
                       (list (mac-to-lisp-type type) type)))
                 args)))
    `(define-entry-point (,c-name (,(or module *foreign-module*)))
       ,type-list
       ,result-type)))
