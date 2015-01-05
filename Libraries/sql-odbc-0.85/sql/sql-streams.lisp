;;;-*- Mode: Lisp; Package: SQL -*-

;; SQL module for MCL, LWW and ACL
;; Version 0.85
;; Copyright (C) Paul Meurer 1999. All rights reserved.
;; paul.meurer@hit.uib.no
;;
;; Documentation and the license agreement can be found in file 
;; "sql-odbc-documentation.lisp" 

(in-package :sql)

;; Luckily, both MCL, LW and ACL use closified streams.
;; LW's and ACL's stream implementations are based on the Gray stream proposal.
;; For MCL, we have to define some classes to adhere to the Gray stream proposal.

#+mcl
(defclass fundamental-stream (stream) ())

#+mcl
(defclass fundamental-input-stream (input-stream fundamental-stream) ())

#+mcl
(defclass fundamental-output-stream (output-stream fundamental-stream) ())

#+mcl
(defclass fundamental-character-stream (fundamental-stream) ())

#+mcl
(defclass fundamental-binary-stream (fundamental-stream) ())

#+mcl
(defgeneric stream-read-char (stream))

;; there is a STREAM-PEEK in MCL
#+mcl
(defgeneric stream-peek-char (stream))

(defclass sql-stream (fundamental-stream)
  ((query :initarg :query :reader stream-query)
   (column :initarg :column :reader sql-stream-column)
   ;; data is read in in chunks from the database. 
   (buffer :initform nil)
   (buffer-size :initform 1024
                :initarg :buffer-size 
                :reader stream-buffer-size)
   ;; read- or write-position in the buffer
   (buffer-position :initform 0))
  (:documentation 
   "Abstract stream class (subclass of FUNDAMENTAL-STREAM) designed primarily for
input and output of long data."))

;; this one has to be subclassed in the odbc (etc.) module
(defclass sql-string-stream (sql-stream fundamental-character-stream) ()
  (:documentation "Subclass of SQL-STREAM for input and output of long string/character data."))

(defgeneric make-sql-stream (query &key column stream-class direction
                                      &allow-other-keys))

(defgeneric open-sql-stream (stream))

#+mcl
(defmethod stream-tyi ((stream sql-stream))
  (stream-read-char stream))

#+mcl
(defun read-sequence (sequence stream &key start end)
  (stream-read-sequence stream sequence start end))

#+mcl
(defgeneric stream-read-sequence (stream sequence start end))

#+mcl
(defgeneric stream-write-sequence (stream sequence start end))

#+mcl
(defun write-sequence (sequence stream &key start end)
  (stream-write-sequence stream sequence start end))

(defmacro bind-execute-with-stream ((query &rest parameters) &body body)
  "Binds values to the parameters of a prepared insert statement and executes the query.
The last parameter value should be a list of the form (:stream <stream-var> [size]),
where <var> is a variable bound to a SQL-STREAM (for output) in the body, and size is 
an optional value denoting the size of the stream. (This is necessary for drivers returning
\"Y\" for info type $SQL_NEED_LONG_DATA_LEN, e.g. the DataDirect Oracle drivers.)"
  (let* (;; list of :stream, stream-var and (optionally) size
         (last-parameter (car (last parameters))) 
         (simple-parameters 
          (progn (assert (and (consp last-parameter)
                              (eq :stream (car last-parameter))))
                 (butlast parameters)))
         (stream-var (cadr last-parameter))
         (new-parameters (append simple-parameters
                                 (list (list 'list :stream (caddr last-parameter))))))
    `(progn
       (db-execute-parameterized ,query (sql-expression ,query) ,@new-parameters)
       (let ((,stream-var (make-sql-stream ,query :column ,(1- (length parameters))
                                           :direction :output)))
         (open-sql-stream ,stream-var) 
         (prog1 ;; unwind-protect? 
           (progn ,@body)
           (close ,stream-var))))))

#| ;; old
(defclass sql-stream ()
  ((query :initarg :query :reader stream-query)
   (column :initarg :column :reader stream-column)
   (direction :initarg :direction :initform nil :reader stream-direction)))

(defclass sql-chunk-stream (sql-stream)
  ((next-chunk :initform nil :reader next-chunk)
   (chunk-length :initform 1024
                 :initarg :chunk-length 
                 :reader stream-chunk-length)))

(defmethod make-sql-stream ((query query) column
                               &key
                               (stream-class 'sql-chunk-stream)
                               (direction :output))
  (make-instance stream-class :query query 
                 :column column
                 :direction direction))

(defgeneric db-open-stream (query stream))
(defgeneric db-close-stream (query stream))
(defgeneric db-read-chunk (query stream eof))
(defgeneric db-write-chunk (query stream token))

(defun read-chunk (stream &optional (eof :eof))
  (db-read-chunk (stream-query stream) stream eof))

(defun write-chunk (chunk stream)
  (db-write-chunk (stream-query stream) stream chunk))

(defun peek-chunk (stream)
  (with-slots (next-chunk) stream
    (or next-chunk 
        (setf next-chunk (read-chunk stream)))))

(defmacro bind-execute-with-stream ((query &rest parameters) &body body)
  "Binds values to the parameters of a prepared statement and executes the query.
To be used in the body of the WITH-PREPARED-COMMAND macro."
  (let* (;; list of :stream, stream-var and (optionally) size
         (last-parameter (car (last parameters))) 
         (simple-parameters 
          (progn (assert (and (consp last-parameter)
                              (eq :stream (car last-parameter))))
                 (butlast parameters)))
         (stream-var (cadr last-parameter))
         (new-parameters (append simple-parameters
                                 (list (list 'list :stream (caddr last-parameter))))))
    `(progn
       (db-execute-parameterized 
        (or query *default-query* *default-database*) ,@new-parameters)
       (let ((,stream-var (make-sql-stream ,query ,(1- (length parameters))
                                           :direction :input)))
         (db-open-stream ,query ,stream-var) 
         (prog1 
           (progn ,@body)
           (db-close-stream ,query ,stream-var))))))

|#