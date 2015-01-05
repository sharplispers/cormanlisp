;;;-*- Mode: Lisp; Package: ODBC -*-

;; ODBC module for MCL, LWW and ACL
;; Version 0.85
;; Copyright (C) Paul Meurer 1999. All rights reserved.
;; paul.meurer@hit.uib.no
;;
;;; "This software is based on the design of the ODBC interface provided by 
;;;  Harlequin Group plc in the LispWorks and LispWorks for the Windows 
;;;  Operating System products. This implementation is not the property of
;;;  Harlequin and they have no responsibility for its content or accuracy."

;; Documentation and the license agreement can be found in file 
;; "sql-odbc-documentation.lisp".
;; Bug reports and suggestions are highly welcome.


(in-package :odbc)

(defclass odbc-query (query)
  ((hstmt :initform nil :initarg :hstmt :accessor hstmt) ; = cursor??
   (column-count :initform nil :accessor column-count)
   (column-names :initform (make-array 0 :element-type 'string :adjustable t :fill-pointer t)
                 :accessor column-names)
   (column-c-types :initform (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t)
                   :accessor column-c-types)
   (column-sql-types :initform (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t)
                     :accessor column-sql-types)
   (column-data-ptrs :initform (make-array 0 :adjustable t :fill-pointer t)
                     :accessor data-ptrs)
   (column-out-len-ptrs :initform (make-array 0 :adjustable t :fill-pointer t)
                        :accessor column-out-len-ptrs)
   (column-precisions :initform (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t)
                      :accessor column-precisions)
   (column-scales :initform (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t)
                  :accessor column-scales)
   (column-nullables-p :initform (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t)
                       :accessor column-nullables-p)
   ;;(parameter-count :initform 0 :accessor parameter-count)
   (parameter-data-ptrs :initform (make-array 0 :adjustable t :fill-pointer t)
                     :accessor parameter-ptrs)))

(defclass odbc-database (database)
  (;; any reason to have more than one henv?
   (henv :initform nil :allocation :class :initarg :henv :accessor henv)
   (hdbc :initform nil :initarg :hdbc :accessor hdbc)
   ;; info returned from SQLGetInfo
   (info :initform (make-hash-table) :reader db-info))
	;;#+cormanlisp (:metaclass cl::class-slot-class)
	)

(defclass oracle-database (sql::oracle-mixin odbc-database) ())
#-mcl
(defclass access-database (sql::access-mixin odbc-database) ())
#-mcl
(defclass solid-database (sql::solid-mixin odbc-database) ())
(defclass postgres-database (sql::postgres-mixin odbc-database) ())
#-mcl
(defclass mysql-database (sql::mysql-mixin odbc-database) ())

;; use this method if the db object does not exist yet
(defmethod db-connect ((db-type (eql :odbc)) db-name user-id password autocommit)
  (let ((db (make-instance 'odbc-database 
              :db-type :odbc
              :db-name db-name
              :user-id user-id
              :password password)))
    (unless (henv db) ;; has class allocation!
      (setf (henv db) (%new-environment-handle)))
    (setf (hdbc db) (%new-db-connection-handle (henv db)))
    ;; if connection cannot be established, we drop out here.
    (db-connect db db-name user-id password autocommit)))

(defmethod db-connect ((db odbc-database) db-name user-id password autocommit)
  (if user-id
    (%sql-connect (hdbc db) db-name user-id password)
    ;; does not work yet
    (%sql-driver-connect (hdbc db) db-name  ;$SQL_DRIVER_PROMPT
                         $SQL_DRIVER_NOPROMPT))
  ;; we have to connect before we can get the dbms/server names
  (let ((server-name (get-odbc-info db $SQL_SERVER_NAME))
        (dbms-name (get-odbc-info db $SQL_DBMS_NAME))
        (txn-capable-p (/= (get-odbc-info db $SQL_TXN_CAPABLE) $SQL_TC_NONE)))
    ;; need SERVER-NAME and DBMS-NAME because many drivers mix this up
    (cond ((or (search "oracle" server-name :test #'char-equal)
               (search "oracle" dbms-name :test #'char-equal))
           (change-class db 'oracle-database))
          ((or (search "access" server-name :test #'char-equal)
               (search "access" dbms-name :test #'char-equal))
           (change-class db 'access-database))
          ((or (search "mysql" server-name :test #'char-equal)
               (search "mysql" dbms-name :test #'char-equal))
           (change-class db 'mysql-database))
          (t nil))
    (when txn-capable-p ; has transaction support
      (if autocommit
        (enable-autocommit (hdbc db))
        (disable-autocommit (hdbc db)))))
  db)

(defmethod db-disconnect ((database odbc-database))
  (with-slots (hdbc queries sql::connected-p) database
    (when sql::connected-p
      (dolist (query queries)
        (if (query-active-p query)
          (with-slots (hstmt) query
            (when hstmt 
              (%free-statement hstmt :drop)
              (setf hstmt nil)))))
      (%disconnect hdbc)
      (setf *connected-databases* (delete database *connected-databases*)
            sql::connected-p nil
            *default-database* (car *connected-databases*)))))

(defmethod db-commit ((database odbc-database))
  (%commit (henv database) (hdbc database)))

(defmethod db-rollback ((database odbc-database))
  (%rollback (henv database) (hdbc database)))

(defmethod db-cancel-query ((query odbc-query))
  (with-slots (hstmt) query
    (%sql-cancel hstmt)
    (setf (query-active-p query) nil)))

(defmethod initialize-instance :after ((query odbc-query) 
                                           &key sql henv hdbc &allow-other-keys)
  (when sql
    (let ((hstmt (%new-statement-handle hdbc)))
      (%sql-exec-direct sql hstmt henv hdbc)
      (with-slots (column-count 
                   column-names column-c-types column-sql-types column-data-ptrs 
                   column-out-len-ptrs column-precisions column-scales
                   column-nullables-p active-p) query
        (setf (hstmt query) hstmt)
        (%initialize-query query)
        (setf active-p t)))))

;; one for odbc-database is missing
(defmethod terminate ((query odbc-query))
  (with-slots (column-data-ptrs column-out-len-ptrs hstmt) query
    (when hstmt 
      (%free-statement hstmt :drop)
      (%dispose-ptr hstmt)) ;; ??
    (loop for data-ptr across column-data-ptrs
          when data-ptr do (%dispose-ptr data-ptr))
    (loop for out-len-ptr across column-out-len-ptrs
          when out-len-ptr do (%dispose-ptr out-len-ptr))))

(defmethod db-open-query ((database odbc-database) query-expression
                             &key arglen col-positions
                             &allow-other-keys)
  (db-open-query (get-free-query database) query-expression
                 :arglen arglen :col-positions col-positions))

(defmethod db-open-query ((query odbc-query) query-expression
                             &key arglen col-positions &allow-other-keys)
  (%db-execute query query-expression)
  (%initialize-query query arglen col-positions))

(defmethod db-fetch-query-results ((database odbc-database) &optional count flatp)
  (db-fetch-query-results (sql::db-query-object database) count flatp))

(defmethod db-fetch-query-results ((query odbc-query) &optional count flatp)
  (when (query-active-p query)
    (with-slots (column-count column-data-ptrs column-c-types column-sql-types 
                              column-out-len-ptrs column-precisions hstmt)
                query
      (values
       (cond (flatp
              (when (> column-count 1)
                (error "If more than one column is to be fetched, flatp has to be nil."))
              (let ((data-ptr (aref column-data-ptrs 0))
                    (c-type (aref column-c-types 0))
                    (sql-type (aref column-sql-types 0))
                    (out-len-ptr (aref column-out-len-ptrs 0))
                    (precision (aref column-precisions 0)))
                (loop for i from 0 
                      until (or (and count (= i count))
                                ;;(setf no-data ;; not used???
                                (= (%sql-fetch hstmt) $SQL_NO_DATA_FOUND))
                      collect
                      (if (and precision (< precision +max-precision+))
                        (read-data data-ptr c-type sql-type out-len-ptr nil)
                        (read-data-in-chunks hstmt 0 data-ptr c-type sql-type
                                             out-len-ptr nil)))))
             (t
              (loop for i from 0 
                    until (or (and count (= i count))
                              ;;(setf no-data
                              (= (%sql-fetch hstmt) $SQL_NO_DATA_FOUND));)
                    collect
                    (loop for data-ptr across column-data-ptrs
                          for c-type across column-c-types
                          for sql-type across column-sql-types
                          for out-len-ptr across column-out-len-ptrs
                          for precision across column-precisions
                          for j from 0 ; column count is zero based in lisp
                          collect 
                          (if (and precision (< precision +max-precision+))
                            (read-data data-ptr c-type sql-type out-len-ptr nil)
                            (read-data-in-chunks hstmt j data-ptr c-type sql-type
                                                 out-len-ptr nil))))))
       query))))

#+lispworks
(defmacro without-interrupts (&body body)
  `(mp:without-preemption ,@body))

#+allegro
(defmacro without-interrupts (&body body)
  `(mp:without-scheduling ,@body))

#+cormanlisp
(defmacro without-interrupts (&body body)
	`(progn
		,@body))

(defmethod db-query ((database odbc-database) query-expression &optional flatp)
  (let ((free-query
         ;; make it thread safe 
         (get-free-query database)))
    (setf (sql-expression free-query) query-expression)
    (unwind-protect
      (progn
        (%db-execute free-query query-expression)
        (%initialize-query free-query)
        (db-fetch-query-results free-query nil flatp))
      (db-close-query free-query))))

(defmethod %db-execute ((database odbc-database) sql-expression &key &allow-other-keys)
  (%db-execute (get-free-query database) sql-expression))

(defmethod %db-execute ((query odbc-query) sql-expression &key &allow-other-keys)
  (with-slots (henv hdbc) (sql::query-database query)
    (with-slots (hstmt) query
      (unless hstmt (setf hstmt (%new-statement-handle hdbc))) 
      (%sql-exec-direct sql-expression hstmt henv hdbc)
      query)))

;; reuse inactive queries
(defmethod get-free-query ((database odbc-database))
  "get-free-query finds or makes a nonactive query object, and then sets it to active.
This makes the functions db-execute-command and db-query thread safe."
  (with-slots (queries) database
    (or (without-interrupts ;; not context switch allowed here 
         (let ((inactive-query (find-if (lambda (query)
                                          (not (query-active-p query)))
                                        queries)))
           (when inactive-query 
             (with-slots (column-count column-names column-c-types 
                                       column-sql-types column-data-ptrs
                                       column-out-len-ptrs column-precisions
                                       column-scales column-nullables-p)
                         inactive-query
               (setf column-count 0
                     (fill-pointer column-names) 0
                     (fill-pointer column-c-types) 0
                     (fill-pointer column-sql-types) 0
                     (fill-pointer column-data-ptrs) 0
                     (fill-pointer column-out-len-ptrs) 0
                     (fill-pointer column-precisions) 0
                     (fill-pointer column-scales) 0
                     (fill-pointer column-nullables-p) 0))
             (setf (query-active-p inactive-query) t))
           inactive-query))
        (let ((new-query (make-instance 'odbc-query :database database
                                        :active-p t)))
          (push new-query queries)
          new-query))))

(defmethod db-execute-command ((database odbc-database) sql-string)
  (db-execute-command (get-free-query database) sql-string))

(defmethod db-execute-command ((query odbc-query) sql-string)
  (with-slots (hstmt database) query
    (unless (eq *active-transactions* t)
      (pushnew database *active-transactions*))
    (with-slots (henv hdbc) database
      (unless hstmt (setf hstmt (%new-statement-handle hdbc))) 
      (unwind-protect 
        (%sql-exec-direct sql-string hstmt henv hdbc)
        (db-close-query query)))))

(defmethod %initialize-query ((database odbc-database) &optional arglen col-positions)
  (%initialize-query (db-query-object database) arglen col-positions))

(defmethod %initialize-query ((query odbc-query) &optional arglen col-positions)
  (with-slots (hstmt 
               column-count column-names column-c-types column-sql-types
               column-data-ptrs column-out-len-ptrs column-precisions
               column-scales column-nullables-p) 
              query 
    (setf column-count (if arglen
                         (min arglen (result-columns-count hstmt))
                         (result-columns-count hstmt)))
    (labels ((initialize-column (col-nr)
                (multiple-value-bind (name sql-type precision scale nullable-p)
                                     (%describe-column hstmt (1+ col-nr))
                  ;; allocate space to bind result rows to
                  (let* ((c-type (sql-to-c-type sql-type))
                         (size (if (zerop precision)
                                 +max-precision+ ;; if the precision cannot be determined
                                 (min precision +max-precision+)))
                         (data-ptr
                          (case c-type
                            (#.$SQL_C_SLONG (%new-ptr :long))
                            (#.$SQL_DOUBLE (%new-ptr :double))
                            (#.$SQL_C_DATE (make-record sql-c-date))
                            (#.$SQL_C_TIME (make-record sql-c-time))
                            (#.$SQL_C_TIMESTAMP (make-record sql-c-timestamp))
                            (#.$SQL_C_BINARY (%new-binary size)) ; new 0.83
                            #+:lispworks
                            (#.$SQL_CHAR (%new-cstring (1+ size)))
                            (t (%new-ptr :ptr (1+ size)))))
                         (long-p (= size +max-precision+))
                         (out-len-ptr (%new-ptr :long)))
                    (unless long-p ;; if long-p we fetch in chunks with %sql-get-data
                      (%bind-column hstmt col-nr c-type data-ptr (1+ size) out-len-ptr))
                    (vector-push-extend name column-names) 
                    (vector-push-extend sql-type column-sql-types)
                    (vector-push-extend (sql-to-c-type sql-type) column-c-types)
                    (vector-push-extend (if (zerop precision) nil precision)
                                        column-precisions)
                    (vector-push-extend scale column-scales)
                    (vector-push-extend nullable-p column-nullables-p)
                    (vector-push-extend data-ptr column-data-ptrs)
                    (vector-push-extend out-len-ptr column-out-len-ptrs)))))
      (if col-positions
        (dolist (col-nr col-positions)
          (initialize-column col-nr))
        (dotimes (col-nr column-count)
          ;; get column information
          (initialize-column col-nr)))))
  query)

(defmethod db-close-query ((query odbc-query))
  (with-slots (hstmt column-count column-names column-c-types column-sql-types
                     column-data-ptrs column-out-len-ptrs column-precisions
                     column-scales column-nullables-p) query
    (let ((count (fill-pointer column-data-ptrs)))
      (when (not (zerop count))
        (dotimes (col-nr count)
          (let ((data-ptr (aref column-data-ptrs col-nr))
                (out-len-ptr (aref column-out-len-ptrs col-nr)))
            (when data-ptr (%dispose-ptr data-ptr)) ; we *did* allocate them
            (when out-len-ptr (%dispose-ptr out-len-ptr)))))
      (%free-statement hstmt :unbind)
      (%free-statement hstmt :reset)
      (%free-statement hstmt :close)
      (setf (query-active-p query) nil)))
  query)

(defmethod %read-query-data ((database odbc-database) ignore-columns)
  (%read-query-data (db-query-object database) ignore-columns))

(defmethod %read-query-data ((query odbc-query) ignore-columns)
  (with-slots (hstmt column-count column-c-types column-sql-types
                     column-data-ptrs column-out-len-ptrs column-precisions)
              query
    (unless (= (SQLFetch hstmt) $SQL_NO_DATA_FOUND)
      (values
       (loop for col-nr from 0 to (- column-count 
                                     (if (eq ignore-columns :last) 2 1))
             collect
             (if (>= (aref column-precisions col-nr) +max-precision+)
               (read-data-in-chunks hstmt col-nr
                                    (aref column-data-ptrs col-nr) 
                                    (aref column-c-types col-nr)
                                    (aref column-sql-types col-nr)
                                    (aref column-out-len-ptrs col-nr)
                                    nil)
               (read-data (aref column-data-ptrs col-nr) 
                          (aref column-c-types col-nr)
                          (aref column-sql-types col-nr)
                          (aref column-out-len-ptrs col-nr)
                          nil)))
       t))))

(defmethod db-map-query ((database odbc-database) type function query-exp)
  (db-map-query (get-free-query database) type function query-exp))

(defmethod db-map-query ((query odbc-query) type function query-exp)
  (declare (ignore type)) ; preliminary. Do a type coersion here
  (%db-execute query (sql::sql-string query-exp))
  (unwind-protect
    (progn
      (%initialize-query query)
      ;; the main loop
      (loop with data
            while (setf data (%read-query-data query nil))
            do (apply function data)))
    ;; dispose of memory and set query inactive or get rid of it
    (db-close-query query)))

(defmethod db-map-bind-query ((query odbc-query) type function 
                                 &rest parameters)
  (declare (ignore type)) ; preliminary. Do a type coersion here
  (unwind-protect
    (progn
      (apply #'%db-bind-execute query parameters)
      ;; the main loop
      (loop with data
            while (setf data (%read-query-data query nil))
            do (apply function data)))
    ;; dispose of memory and set query inactive or get rid of it
    (%db-reset-query query)))

;; does not always return exactly a lisp type...
(defun sql-to-lisp-type (sql-type)
  (ecase sql-type
    ((#.$SQL_CHAR #.$SQL_VARCHAR #.$SQL_LONGVARCHAR) :string)
    ((#.$SQL_NUMERIC #.$SQL_DECIMAL #.$SQL_BIGINT) :string) ; ??
    (#.$SQL_INTEGER :long)
    (#.$SQL_SMALLINT :short)
    ((#.$SQL_FLOAT #.$SQL_DOUBLE) :long)
    (#.$SQL_REAL :long)
    (#.$SQL_DATE 'sql-c-date)
    (#.$SQL_TIME 'sql-c-time)
    (#.$SQL_TIMESTAMP 'sql-c-timestamp)
    ;((#.$SQL_BINARY #.$SQL_VARBINARY #.$SQL_LONGVARBINARY) $SQL_C_BINARY) ; ??
    (#.$SQL_TINYINT :short)
    ;(#.$SQL_BIT $SQL_C_BIT) ; ??
    ((#.$SQL_VARBINARY #.$SQL_LONGVARBINARY) :binary)
    ))

;; prepared queries

(defmethod db-prepare-statement ((database odbc-database) sql
                                     &key parameter-table parameter-columns)
  (with-slots (hdbc) database
    (let ((query (get-free-query database))) 
      (with-slots (hstmt) query
        (unless hstmt (setf hstmt (%new-statement-handle hdbc))))
      (db-prepare-statement query sql parameter-table parameter-columns))))

(defmethod db-prepare-statement ((query odbc-query) (sql string)
                                     &key parameter-table parameter-columns)
  ;; this is a workaround to get hold of the column types when the driver does not
  ;; support SQLDescribeParam. To do: put code in here for drivers that do
  ;; support it.
  (unless (string-equal sql "insert" :end1 6)
    (error "Only insert expressions are supported in literal SQL: '~a'." sql))
  (unless (eq *active-transactions* t)
    (pushnew (sql::query-database query) *active-transactions*))
  (%db-execute query (format nil "select ~{~a~^,~} from ~a where 0 = 1"
                             (or parameter-columns '("*")) parameter-table))
  (%initialize-query query)
  (with-slots (hstmt) query
    (%free-statement hstmt :unbind)
    (%free-statement hstmt :reset)
    (setf (sql-expression query) sql)
    (%sql-prepare hstmt sql))
  query)

(defmethod db-prepare-statement ((query odbc-query)
                                     (sql-expression sql::sql-insert-expression)
                                     &key &allow-other-keys)
  (unless (eq *active-transactions* t)
    (pushnew (sql::query-database query) *active-transactions*))
  (with-slots (sql::table sql::parameter-columns) sql-expression
    (when sql::parameter-columns 
      ;; this is a workaround to get hold of the column types when the driver does
      ;; not support SQLDescribeParam
      (let ((columns-as-positions (integerp (car sql::parameter-columns))))
        (%db-execute query 
                     (with-output-to-string (stream)
                       (write-string "select " stream)
                       (if columns-as-positions
                         (write-char #\* stream) ; we don't know the names
                         (sql::write-sql sql::parameter-columns stream))
                       (write-string " from " stream)
                       (sql::write-sql sql::table stream)
                       (write-string " where 0 = 1" stream)))
        (%initialize-query query nil (when columns-as-positions sql::parameter-columns)))))
  (with-slots (hstmt) query
    (%free-statement hstmt :close) ;; need all 3?
    (%free-statement hstmt :unbind)
    (%free-statement hstmt :reset)
    (setf (sql-expression query) sql-expression)
    (%sql-prepare hstmt (sql::sql-string sql-expression)))
  query)

(defmethod db-prepare-statement ((query odbc-query)
                                     (sql-expression sql::sql-select-expression)
                                     &key &allow-other-keys)
  (with-slots (hstmt) query
    (%sql-prepare hstmt (sql::sql-string sql-expression))
    (setf (sql-expression query) sql-expression)
    (%initialize-query query)
    query))

(defmethod %db-bind-execute ((query odbc-query) &rest parameters)
  (with-slots (hstmt parameter-data-ptrs) query
    (loop for parameter in parameters
          with data-ptr and size and parameter-string
          do
          (setf parameter-string
                (if (stringp parameter)
                  parameter
                  (write-to-string parameter))
           size (length parameter-string)
                data-ptr 
                #+:lispworks (%new-cstring (1+ size))
                #-:lispworks (%new-ptr :ptr (1+ size)))
          (vector-push-extend data-ptr parameter-data-ptrs)
          (%sql-bind-parameter 
           hstmt (1- (fill-pointer parameter-data-ptrs)) $SQL_PARAM_INPUT
           $SQL_C_CHAR ; (aref column-c-types parameter-count)
           $SQL_CHAR ; sql-type
           +max-precision+ ;precision ; this should be the actual precision!
           ;; scale
           0 ;; should be calculated for $SQL_DECIMAL,
           ;;$SQL_NUMERIC and $SQL_TIMESTAMP
           data-ptr ;; = rgbValue
           0
           ;; *pcbValue;
           ;; change this for output and binary input! (see 3-32)
           (%null-ptr))
          (%put-str data-ptr parameter-string size))
        (%sql-execute hstmt)))


;; is it possible to find out which sql-type the parameter is?
(defmethod db-execute-parameterized ((query odbc-query)
                                         (sql-expression sql::sql-select-expression)
                                         &rest parameters)
  (when (not (query-active-p query)) (error "Query ~s is inactive." query))
  (apply #'%db-bind-execute query parameters)
  (with-slots (hstmt parameter-data-ptrs) query
    (prog1
      (db-fetch-query-results query nil ; flatp
                              nil) 
      (%free-statement hstmt :reset) ;; but _not_ :unbind !
      (%free-statement hstmt :close)
      (dotimes (param-nr (fill-pointer parameter-data-ptrs))
        (let ((data-ptr (aref parameter-data-ptrs param-nr)))
          (when data-ptr (%dispose-ptr data-ptr))))
      (setf (fill-pointer parameter-data-ptrs) 0))))

(defmethod %db-reset-query ((query odbc-query))
  (with-slots (hstmt parameter-data-ptrs) query
    (prog1
      (db-fetch-query-results query nil ; flatp
                              nil) 
      (%free-statement hstmt :reset) ;; but _not_ :unbind !
      (%free-statement hstmt :close)
      (dotimes (param-nr (fill-pointer parameter-data-ptrs))
        (let ((data-ptr (aref parameter-data-ptrs param-nr)))
          (when data-ptr (%dispose-ptr data-ptr))))
      (setf (fill-pointer parameter-data-ptrs) 0))))

; only $SQL_PARAM_INPUT is supported
(defmethod db-execute-parameterized ((query odbc-query)
                                     (sql-expression sql::sql-insert-expression)
                                     &rest parameters)
  (when (not (query-active-p query)) (error "Query ~s is inactive." query))
  (with-slots (hstmt column-count 
                     column-names column-c-types column-sql-types column-data-ptrs 
                     column-out-len-ptrs column-precisions column-scales
                     column-nullables-p active-p database) query
    (unless (eq *active-transactions* t)
      (pushnew database *active-transactions*))
    (let ((sql-need-long-data-len ;; OpenLink gives wrong info
           ;; OpenLink driver (NT) gives wrong info here!
           ;; set to T of you are using OpenLink.
           (string= "Y" (get-odbc-info query $SQL_NEED_LONG_DATA_LEN))))
      (loop for parameter in parameters
            ; colums are here exactly the parameterized columns
            and parameter-nr from 0 to (1- column-count)
            do
            (let* ((sql-type (aref column-sql-types parameter-nr))
                   (lisp-type (sql-to-lisp-type sql-type))
                   (parameter (case sql-type
                                (#.$SQL_DECIMAL (write-to-string parameter :base 10))
                                (otherwise parameter)))
                   (precision (aref column-precisions parameter-nr))
                   (data-ptr (aref column-data-ptrs parameter-nr))
                   (out-len-ptr (aref column-out-len-ptrs parameter-nr))
                   (column-size (if (zerop precision)
                                  +max-precision+ ;; if the precision cannot be determined
                                  (min precision +max-precision+)))) 
              (cond ((and (< column-size +max-precision+)
                          (not (and (consp parameter) (eq (car parameter) :stream))))
                     (ecase lisp-type
                       (:short
                        (%put-word data-ptr parameter)
                        (setf out-len-ptr (%null-ptr)))
                       (:long 
                        (%put-long data-ptr parameter)
                        (setf out-len-ptr (%null-ptr)))
                       (:string
                        (%put-str data-ptr parameter precision)
                        (setf out-len-ptr (%null-ptr)))
                       (:binary
                        (%put-long out-len-ptr
                                   ;; returns size in bytes
                                   (%put-binary data-ptr parameter precision))))
                     (%sql-bind-parameter hstmt parameter-nr $SQL_PARAM_INPUT
                                          (aref column-c-types parameter-nr)
                                          sql-type
                                          precision
                                          ;; scale
                                          0 ;; should be calculated for $SQL_DECIMAL,
                                          ;;$SQL_NUMERIC and $SQL_TIMESTAMP
                                          data-ptr ;; = rgbValue
                                          0
                                          ;; change this for output (and binary input! done)
                                          ;;(see 3-32)
                                          out-len-ptr ;;(%null-ptr)
                                          ))
                    (t
                     (%sql-bind-parameter hstmt parameter-nr $SQL_PARAM_INPUT
                                          (aref column-c-types parameter-nr)
                                          sql-type
                                          +max-precision+
                                          0
                                          data-ptr
                                          0
                                          out-len-ptr)
                     (%put-long out-len-ptr
                                (%sql-len-data-at-exec 
                                 (if sql-need-long-data-len 
                                   (if (consp parameter)
                                     (cadr parameter) ;; size of stream
                                     (min (length parameter) +max-precision+))
                                   0)))))))
      (when (= $SQL_NEED_DATA (%sql-execute hstmt))
        ;; feed data for data-at-execution columns
        ;; should put this into an unwind-protect form and cancel the query if something happens,
        ;; but I did not get it to work.
        (loop with param-ptr
              while (setf param-ptr (data-parameter-ptr hstmt))
              do
              (let* ((param-nr (position-if (lambda (ptr)
                                              (%ptr-eql param-ptr ptr))
                                            column-data-ptrs))
                     (parameter (when param-nr (nth param-nr parameters)))
                     (size (length parameter)))
                (cond ((consp parameter)
                       (return-from db-execute-parameterized :stream))
                      (parameter
                       ;; feed the data in pieces
                       (do ((start 0 (+ start +max-precision+))
                            (end +max-precision+ (+ end +max-precision+)))
                           ((> start size))
                         (print start)
                         (let ((chunk (subseq parameter start (when (< end size) end)))) 
                           (%with-temporary-allocation
                             ((data-ptr :string (length chunk)))
                             ;;(ffc::%%str-pointer chunk data-ptr)
                             (%put-str data-ptr chunk)
                             (when
                               (= $SQL_SUCCESS_WITH_INFO
                                  (%sql-put-data hstmt data-ptr $SQL_NTS))
                               (error "something went wrong"))))))
                      (t nil))))))))

(defun data-parameter-ptr (hstmt)
  (%with-temporary-allocation ((param-ptr :ptr))
    (let ((return-code (%sql-param-data hstmt param-ptr)))
      ;;(format t "~%return-code from %sql-param-data: ~a~%" return-code)
      (when (= return-code $SQL_NEED_DATA)
        ;;(ffc::%pointer-to-address (%get-ptr param-ptr))
        (%get-ptr param-ptr)))))

;; database inquiery functions

(defmethod db-describe-columns ((database odbc-database) 
                                    table-qualifier table-owner table-name column-name)
  (with-slots (hdbc) database
    (%describe-columns hdbc table-qualifier table-owner table-name column-name)))

;; should translate info-type integers to keywords in order to make this 
;; more readable?
(defmethod get-odbc-info ((database odbc-database) info-type)
  (with-slots (hdbc info) database
    (or (gethash info-type info)
        (setf (gethash info-type info)
              (%sql-get-info hdbc info-type)))))

(defmethod get-odbc-info ((query odbc-query) info-type)
  (get-odbc-info (sql::query-database query) info-type))

;; driver inquiery
(defmethod db-data-sources ((db-type (eql :odbc)))
   "Returns a list of (data-source description) - pairs"
   (let ((henv (%new-environment-handle)))
    (unwind-protect
          (loop with direction = :first
               for data-source+description 
               = (multiple-value-list (%sql-data-sources henv :direction direction))
               while (car data-source+description)
               collect data-source+description
               do (setf direction :next))
      (%sql-free-environment henv))))
