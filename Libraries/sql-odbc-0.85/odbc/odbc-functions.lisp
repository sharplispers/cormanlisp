;;;-*- Mode: Lisp; Package: ODBC -*-

;; ODBC module for MCL, LWW and ACL/Windows
;; Version 0.85
;; Copyright (C) Paul Meurer 1999. All rights reserved.
;; paul.meurer@hit.uib.no
;;
;; Documentation and the license agreement can be found in file 
;; "sql-odbc-documentation.lisp".
;; Bug reports and suggestions are highly welcome.

(in-package :odbc)

(defun handle-error (henv hdbc hstmt)
  (%with-temporary-allocation ((sql-state :string 256)
                              (error-message :string $SQL_MAX_MESSAGE_LENGTH)
                              (error-code :long)
                              (msg-length :short))
    (SQLError henv hdbc hstmt sql-state
              error-code error-message $SQL_MAX_MESSAGE_LENGTH msg-length)
    (values
     (%get-cstring error-message)
     (%get-cstring sql-state)
     (%get-word msg-length) 
     (%get-long error-code))))

; test this: return a keyword for efficiency
(defun sql-state (henv hdbc hstmt)
  (%with-temporary-allocation ((sql-state :string 256)
                              (error-message :string $SQL_MAX_MESSAGE_LENGTH)
                              (error-code :long)
                              (msg-length :short))
    (SQLError henv hdbc hstmt sql-state
              error-code error-message $SQL_MAX_MESSAGE_LENGTH msg-length)
    (%get-cstring sql-state) ;(%cstring-to-keyword sql-state)
    ))

(defmacro with-error-handling ((&key henv hdbc hstmt (print-info t))
                                   odbc-call &body body)
  (let ((result-code (gensym)))
    `(let ((,result-code ,odbc-call))
       #+allegro (setf ,result-code (short-to-signed-short ,result-code))
       (case ,result-code
         (#.$SQL_SUCCESS
          (progn ,result-code ,@body))
         (#.$SQL_SUCCESS_WITH_INFO
          (when ,print-info
            (multiple-value-bind (error-message sql-state)
                                 (handle-error (or ,henv (%null-ptr))
                                               (or ,hdbc (%null-ptr))
                                               (or ,hstmt (%null-ptr)))
              (warn "[ODBC info] ~a state: ~a" ,result-code error-message sql-state)))
          (progn ,result-code ,@body))
         (#.$SQL_INVALID_HANDLE
          (error "[ODBC error] Invalid handle"))
         (#.$SQL_STILL_EXECUTING
          (error "[ODBC error] Still executing"))
         (#.$SQL_ERROR
          (multiple-value-bind (error-message sql-state)
                               (handle-error (or ,henv (%null-ptr))
                                             (or ,hdbc (%null-ptr))
                                             (or ,hstmt (%null-ptr)))
            (error "[ODBC error] ~a state: ~a" error-message sql-state)))
         (otherwise
          (progn ,result-code ,@body))))))

(defun %new-environment-handle ()
  (%with-sql-pointer (phenv) 
    (with-error-handling
      ()
      (SQLAllocEnv phenv)
      (%get-ptr phenv))))

(defun %sql-free-environment (henv)
  (with-error-handling 
    (:henv henv)
    (SQLFreeEnv henv)))

(defun %new-db-connection-handle (henv)
  (%with-sql-pointer (phdbc) 
    (with-error-handling
      (:henv henv)
      (SQLAllocConnect henv phdbc)
      (%get-ptr phdbc))))

(defun %free-statement (hstmt option)
  (with-error-handling 
    (:hstmt hstmt)
    (SQLFreeStmt 
     hstmt 
     (ecase option
       (:drop $SQL_DROP)
       (:close $SQL_CLOSE)
       (:unbind $SQL_UNBIND)
       (:reset $SQL_RESET_PARAMS)))))

(defmacro with-statement-handle ((hstmt hdbc) &body body)
  `(let ((,hstmt (%new-statement-handle ,hdbc)))
     (unwind-protect
       (progn ,@body)
       (%free-statement ,hstmt :drop))))

;; functional interface

(defun %sql-connect (hdbc server uid pwd)
  (with-cstr (server-ptr server)
    (with-cstr (uid-ptr uid)
      (with-cstr (pwd-ptr pwd)
        (with-error-handling 
          (:hdbc hdbc)
          (SQLConnect hdbc server-ptr $SQL_NTS uid-ptr 
                      $SQL_NTS pwd-ptr $SQL_NTS))))))

(defun %sql-driver-connect (hdbc connection-string completion-option)
  (with-cstr (connection-str-ptr connection-string)
    (%with-temporary-allocation
     ((complete-connection-str-ptr :string 512)
      (length-ptr :short))
     (with-error-handling 
       (:hdbc hdbc)
       (SQLDriverConnect hdbc (%null-ptr) ; no window
                         connection-str-ptr $SQL_NTS
                         complete-connection-str-ptr 512
                         length-ptr completion-option))
     (print (%get-cstring complete-connection-str-ptr)))))

(defun %disconnect (hdbc)
  (with-error-handling 
    (:hdbc hdbc)
    (SQLDisconnect hdbc)))

(defun %commit (henv hdbc)
  (with-error-handling 
    (:henv henv :hdbc hdbc)
    (SQLTransact 
     henv hdbc $SQL_COMMIT)))

(defun %rollback (henv hdbc)
  (with-error-handling 
    (:henv henv :hdbc hdbc)
    (SQLTransact 
     henv hdbc $SQL_ROLLBACK)))

; col-nr is zero-based in Lisp
; col-nr = :bookmark retrieves a bookmark.
(defun %bind-column (hstmt column-nr c-type data-ptr precision out-len-ptr)
  (with-error-handling
    (:hstmt hstmt)
    (SQLBindCol hstmt
                (if (eq column-nr :bookmark) 0 (1+ column-nr))
                c-type data-ptr precision out-len-ptr)))

; parameter-nr is zero-based in Lisp
(defun %sql-bind-parameter (hstmt parameter-nr parameter-type c-type
                                      sql-type precision scale data-ptr
                                      max-value out-len-ptr)
  (with-error-handling
    (:hstmt hstmt)
    (SQLBindParameter hstmt (1+ parameter-nr)
                      parameter-type ;$SQL_PARAM_INPUT 
                      c-type ;$SQL_C_CHAR
                      sql-type ;$SQL_VARCHAR
                      precision ;(1- (length str))
                      scale ;0
                      data-ptr
                      max-value
                      out-len-ptr ;#.(%null-ptr)
                      )))

(defun %sql-fetch (hstmt)
  (with-error-handling 
    (:hstmt hstmt)
    (SQLFetch hstmt)))

(defun %new-statement-handle (hdbc)
  (%with-sql-pointer (hstmt-ptr) 
    (with-error-handling 
      (:hdbc hdbc)
      (SQLAllocStmt hdbc hstmt-ptr) 
      (%get-ptr hstmt-ptr))))

(defun %sql-get-info (hdbc info-type)
  (ecase info-type
    ;; those return string
    ((#.$SQL_ACCESSIBLE_PROCEDURES
      #.$SQL_ACCESSIBLE_TABLES
      #.$SQL_COLUMN_ALIAS
      #.$SQL_DATA_SOURCE_NAME
      #.$SQL_DATA_SOURCE_READ_ONLY
      #.$SQL_DBMS_NAME
      #.$SQL_DBMS_VER
      #.$SQL_DRIVER_NAME
      #.$SQL_DRIVER_ODBC_VER
      #.$SQL_DRIVER_VER
      #.$SQL_EXPRESSIONS_IN_ORDERBY
      #.$SQL_IDENTIFIER_QUOTE_CHAR
      #.$SQL_KEYWORDS
      #.$SQL_LIKE_ESCAPE_CLAUSE
      #.$SQL_MAX_ROW_SIZE_INCLUDES_LONG
      #.$SQL_MULT_RESULT_SETS
      #.$SQL_MULTIPLE_ACTIVE_TXN
      #.$SQL_NEED_LONG_DATA_LEN
      #.$SQL_ODBC_SQL_OPT_IEF
      #.$SQL_ODBC_VER
      #.$SQL_ORDER_BY_COLUMNS_IN_SELECT
      #.$SQL_OUTER_JOINS
      #.$SQL_OWNER_TERM
      #.$SQL_PROCEDURE_TERM
      #.$SQL_PROCEDURES
      #.$SQL_QUALIFIER_NAME_SEPARATOR
      #.$SQL_QUALIFIER_TERM
      #.$SQL_ROW_UPDATES
      #.$SQL_SEARCH_PATTERN_ESCAPE
      #.$SQL_SERVER_NAME
      #.$SQL_SPECIAL_CHARACTERS
      #.$SQL_TABLE_TERM
      #.$SQL_USER_NAME)
     (%with-temporary-allocation ((info-ptr :string 1024)
                                  (info-length-ptr :short))
       (with-error-handling 
         (:hdbc hdbc)
         (SQLGetInfo hdbc info-type info-ptr 1023 info-length-ptr)
         (%get-cstring info-ptr))))
    ;; those returning a word
    ((#.$SQL_ACTIVE_CONNECTIONS
      #.$SQL_ACTIVE_STATEMENTS
      #.$SQL_CONCAT_NULL_BEHAVIOR
      #.$SQL_CORRELATION_NAME
      #.$SQL_CURSOR_COMMIT_BEHAVIOR
      #.$SQL_CURSOR_ROLLBACK_BEHAVIOR
      #.$SQL_MAX_COLUMN_NAME_LEN
      #.$SQL_MAX_COLUMNS_IN_GROUP_BY
      #.$SQL_MAX_COLUMNS_IN_INDEX
      #.$SQL_MAX_COLUMNS_IN_ORDER_BY
      #.$SQL_MAX_COLUMNS_IN_SELECT
      #.$SQL_MAX_COLUMNS_IN_TABLE
      #.$SQL_MAX_CURSOR_NAME_LEN
      #.$SQL_MAX_OWNER_NAME_LEN
      #.$SQL_MAX_PROCEDURE_NAME_LEN
      #.$SQL_MAX_QUALIFIER_NAME_LEN
      #.$SQL_MAX_TABLE_NAME_LEN
      #.$SQL_MAX_TABLES_IN_SELECT
      #.$SQL_MAX_USER_NAME_LEN
      #.$SQL_NON_NULLABLE_COLUMNS
      #.$SQL_NULL_COLLATION
      #.$SQL_ODBC_API_CONFORMANCE
      #.$SQL_ODBC_SAG_CLI_CONFORMANCE
      #.$SQL_ODBC_SQL_CONFORMANCE
      #.$SQL_QUALIFIER_LOCATION
      #.$SQL_QUOTED_IDENTIFIER_CASE
      #.$SQL_TXN_CAPABLE)
     (%with-temporary-allocation ((info-ptr :short)
                                  (info-length-ptr :short))
       (with-error-handling 
         (:hdbc hdbc)
         (SQLGetInfo hdbc info-type info-ptr 255 info-length-ptr)
         (%get-word info-ptr)))
     )
    ;; those returning a long bitmask
    ((#.$SQL_ALTER_TABLE 
      #.$SQL_BOOKMARK_PERSISTENCE
      #.$SQL_CONVERT_BIGINT
      #.$SQL_CONVERT_BINARY
      #.$SQL_CONVERT_BIT
      #.$SQL_CONVERT_CHAR
      #.$SQL_CONVERT_DATE
      #.$SQL_CONVERT_DECIMAL
      #.$SQL_CONVERT_DOUBLE
      #.$SQL_CONVERT_FLOAT
      #.$SQL_CONVERT_INTEGER
      #.$SQL_CONVERT_LONGVARCHAR
      #.$SQL_CONVERT_NUMERIC
      #.$SQL_CONVERT_REAL
      #.$SQL_CONVERT_SMALLINT
      #.$SQL_CONVERT_TIME
      #.$SQL_CONVERT_TIMESTAMP
      #.$SQL_CONVERT_TINYINT
      #.$SQL_CONVERT_VARBINARY
      #.$SQL_CONVERT_VARCHAR
      #.$SQL_CONVERT_LONGVARBINARY
      #.$SQL_CONVERT_FUNCTIONS
      #.$SQL_FETCH_DIRECTION
      #.$SQL_FILE_USAGE
      #.$SQL_GETDATA_EXTENSIONS
      #.$SQL_LOCK_TYPES
      #.$SQL_MAX_INDEX_SIZE
      #.$SQL_MAX_ROW_SIZE
      #.$SQL_MAX_STATEMENT_LEN
      #.$SQL_NUMERIC_FUNCTIONS
      #.$SQL_OWNER_USAGE
      #.$SQL_POS_OPERATIONS
      #.$SQL_POSITIONED_STATEMENTS
      #.$SQL_QUALIFIER_USAGE
      #.$SQL_SCROLL_CONCURRENCY
      #.$SQL_SCROLL_OPTIONS
      #.$SQL_STATIC_SENSITIVITY
      #.$SQL_STRING_FUNCTIONS
      #.$SQL_SUBQUERIES
      #.$SQL_SYSTEM_FUNCTIONS
      #.$SQL_TIMEDATE_ADD_INTERVALS
      #.$SQL_TIMEDATE_DIFF_INTERVALS
      #.$SQL_TIMEDATE_FUNCTIONS
      #.$SQL_TXN_ISOLATION_OPTION
      #.$SQL_UNION)
     (%with-temporary-allocation ((info-ptr :long)
                                  (info-length-ptr :short))
       (with-error-handling 
         (:hdbc hdbc)
         (SQLGetInfo hdbc info-type info-ptr 255 info-length-ptr)
         (%get-unsigned-long info-ptr)))
     )
    ;; those returning a long integer
    ((#.$SQL_DEFAULT_TXN_ISOLATION
      #.$SQL_DRIVER_HDBC
      #.$SQL_DRIVER_HENV
      #.$SQL_DRIVER_HLIB
      #.$SQL_DRIVER_HSTMT
      #.$SQL_GROUP_BY
      #.$SQL_IDENTIFIER_CASE
      #.$SQL_MAX_BINARY_LITERAL_LEN
      #.$SQL_MAX_CHAR_LITERAL_LEN
      )
     (%with-temporary-allocation ((info-ptr :long)
                                  (info-length-ptr :short))
       (with-error-handling 
         (:hdbc hdbc)
         (SQLGetInfo hdbc info-type info-ptr 255 info-length-ptr)
         (%get-unsigned-long info-ptr))))))
     
(defun %sql-exec-direct (sql hstmt henv hdbc)
  (with-cstr (sql-ptr sql)
    (with-error-handling
      (:hstmt hstmt :henv henv :hdbc hdbc)
      (SQLExecDirect hstmt sql-ptr $SQL_NTS))))

(defun %sql-cancel (hstmt)
  (with-error-handling
    (:hstmt hstmt)
    (SQLCancel hstmt)))

(defun %sql-execute (hstmt)
  (with-error-handling
    (:hstmt hstmt)
    (SQLExecute hstmt)))

(defun result-columns-count (hstmt)
  (%with-temporary-allocation ((columns-nr-ptr :short))
    (with-error-handling (:hstmt hstmt)
                         (SQLNumResultCols hstmt columns-nr-ptr)
      (%get-word columns-nr-ptr))))

(defun result-rows-count (hstmt)
  (%with-temporary-allocation ((row-count-ptr :long))
    (with-error-handling (:hstmt hstmt)
                         (SQLRowCount hstmt row-count-ptr)
      (%get-long row-count-ptr))))

;; column counting is 1-based
(defun %describe-column (hstmt column-nr)
  (%with-temporary-allocation ((column-name-ptr :string 256)
                               (column-name-length-ptr :short)
                               (column-sql-type-ptr :short)
                               (column-precision-ptr :long)
                               (column-scale-ptr :short)
                               (column-nullable-p-ptr :short))
    (with-error-handling (:hstmt hstmt)
                         (SQLDescribeCol hstmt column-nr column-name-ptr 256
                                         column-name-length-ptr column-sql-type-ptr
                                         column-precision-ptr column-scale-ptr
                                         column-nullable-p-ptr)
      (values
       (%get-cstring column-name-ptr)
       (%get-signed-word column-sql-type-ptr)
       (%get-unsigned-long column-precision-ptr)
       (%get-signed-word column-scale-ptr)
       (%get-signed-word column-nullable-p-ptr)))))

;; parameter counting is 1-based
(defun %describe-parameter (hstmt parameter-nr)
  (%with-temporary-allocation ((column-sql-type-ptr :short)
                               (column-precision-ptr :long)
                               (column-scale-ptr :short)
                               (column-nullable-p-ptr :short))
    (with-error-handling 
      (:hstmt hstmt)
      (SQLDescribeParam hstmt parameter-nr column-sql-type-ptr
                        column-precision-ptr column-scale-ptr
                        column-nullable-p-ptr)
      (values
       (%get-signed-word column-sql-type-ptr)
       (%get-unsigned-long column-precision-ptr)
       (%get-signed-word column-scale-ptr)
       (%get-signed-word column-nullable-p-ptr)))))

(defun %column-attributes (hstmt column-nr descriptor-type)
  (%with-temporary-allocation ((descriptor-info-ptr :string 256)
                               (descriptor-length-ptr :short)
                               (numeric-descriptor-ptr :long))
    (with-error-handling
      (:hstmt hstmt) 
      (SQLColAttributes hstmt column-nr descriptor-type descriptor-info-ptr 256
                        descriptor-length-ptr numeric-descriptor-ptr)
      (values
       (%get-cstring descriptor-info-ptr)
       (%get-signed-long numeric-descriptor-ptr)))))

(defun %prepare-describe-columns (hstmt table-qualifier table-owner 
                                   table-name column-name)
  (with-cstr (table-qualifier-ptr table-qualifier)
    (with-cstr (table-owner-ptr table-owner) 
      (with-cstr (table-name-ptr table-name)
        (with-cstr (column-name-ptr column-name)
          (with-error-handling
            (:hstmt hstmt) 
            (SQLColumns hstmt
                        table-qualifier-ptr (length table-qualifier)
                        table-owner-ptr (length table-owner)
                        table-name-ptr (length table-name)
                        column-name-ptr (length column-name))))))))

(defun %describe-columns (hdbc table-qualifier table-owner 
                                   table-name column-name)
  (with-statement-handle (hstmt hdbc)
    (%prepare-describe-columns hstmt table-qualifier table-owner 
                               table-name column-name)
    (fetch-all-rows hstmt)))

(defun %sql-data-sources (henv &key (direction :first))
  (%with-temporary-allocation 
    ((name-ptr :string #.(1+ $SQL_MAX_DSN_LENGTH))
     (name-length-ptr :short)
     (description-ptr :string 1024)
     (description-length-ptr :short))
    (let ((res (with-error-handling
                 (:henv henv)
                 (SQLDataSources henv
                                 (ecase direction (:first $SQL_FETCH_FIRST) (:next $SQL_FETCH_NEXT))
                                 name-ptr
                                 (1+ $SQL_MAX_DSN_LENGTH)
                                 name-length-ptr
                                 description-ptr
                                 1024
                                 description-length-ptr))))
      (unless (= res $SQL_NO_DATA_FOUND)
        (values (%get-cstring name-ptr)
                (%get-cstring description-ptr))))))

(defun sql-to-c-type (sql-type)
  (ecase sql-type
    ((#.$SQL_CHAR #.$SQL_VARCHAR #.$SQL_LONGVARCHAR 
      #.$SQL_NUMERIC #.$SQL_DECIMAL #.$SQL_BIGINT) $SQL_C_CHAR)
    (#.$SQL_INTEGER $SQL_C_SLONG)
    (#.$SQL_SMALLINT $SQL_C_SSHORT)
    ((#.$SQL_FLOAT #.$SQL_DOUBLE) $SQL_C_DOUBLE)
    (#.$SQL_REAL $SQL_C_FLOAT)
    (#.$SQL_DATE $SQL_C_DATE)
    (#.$SQL_TIME $SQL_C_TIME)
    (#.$SQL_TIMESTAMP $SQL_C_TIMESTAMP)
    ((#.$SQL_BINARY #.$SQL_VARBINARY #.$SQL_LONGVARBINARY) $SQL_C_BINARY)
    (#.$SQL_TINYINT $SQL_C_STINYINT)
    (#.$SQL_BIT $SQL_C_BIT)))

(defparameter *null* +null+)

(defun read-data (data-ptr c-type sql-type out-len-ptr convert-to-string-p)
  (let ((out-len (%get-long out-len-ptr)))
    (cond ((= out-len $SQL_NULL_DATA) *null*)
          ;((= out-len $SQL_NO_TOTAL) +null+) 
          ;; obsolete?
          (convert-to-string-p (%get-cstring data-ptr))
          (t (case c-type
               (#.$SQL_C_DATE
                (date-to-universal-time data-ptr))
               (#.$SQL_C_TIME
                (time-to-universal-time data-ptr))
               (#.$SQL_C_TIMESTAMP
                (timestamp-to-universal-time data-ptr))
               (:decimal
                (let ((*read-base* 10))
                  (read-from-string (%get-cstring data-ptr))))
               (#.$SQL_INTEGER
                (%get-word data-ptr))
               (#.$SQL_C_FLOAT
                (%get-single-float data-ptr))
               (#.$SQL_C_DOUBLE
                (%get-double-float data-ptr))
               (#.$SQL_C_SLONG
                (%get-long data-ptr))
               #+lispworks
               (#.$SQL_C_BIT ; encountered only in Acess
                (%get-bit data-ptr))
               (#.$SQL_C_BINARY
                (%get-binary data-ptr out-len *binary-format*))
               #+ignore
               (#.$SQL_C_CHAR
                (code-char (%get-word data-ptr)))
               (t
                (if (= sql-type $SQL_DECIMAL)
                  (let ((*read-base* 10))
                    (read-from-string (%get-cstring data-ptr)))
                  (%get-cstring data-ptr))))))))

;; which value is appropriate?
(defparameter +max-precision+ 
  ; 64 
  ; 256 ;; works in MCL
  ;512 ;; works in MCL
  #+mcl
  1024 ;; works in MCL
  #-mcl
  4001
  ;1999
  ;65536 ;; 2^16
  )

(defun fetch-all-rows (hstmt &key free-option flatp)
  (let ((column-count (result-columns-count hstmt)))
    (unless (zerop column-count)
      (let ((names (make-array column-count :element-type 'string))
            (sql-types (make-array column-count :element-type 'fixnum))
            (c-types (make-array column-count :element-type 'fixnum))
            (precisions (make-array column-count :element-type 'fixnum))
            (data-ptrs (make-array column-count :initial-element nil))
            (out-len-ptrs (make-array column-count :initial-element nil))
            (scales (make-array column-count :element-type 'fixnum))
            (nullables-p (make-array column-count :element-type 'fixnum)))
        (unwind-protect
          (values
           (progn
             (dotimes (col-nr column-count)
               ;; get column information
               (multiple-value-bind (name sql-type precision scale nullable-p)
                                    (%describe-column hstmt (1+ col-nr))
                 ;; allocate space to bind result rows to
                 (let* ((c-type (sql-to-c-type sql-type))
                        (data-ptr
                         (case c-type ;; add more?
                           (#.$SQL_C_SLONG (%new-ptr :long))
                           (#.$SQL_DOUBLE (%new-ptr :double))
                           (#.$SQL_C_DATE (make-record sql-c-date))
                           (#.$SQL_C_TIME (make-record sql-c-time))
                           (#.$SQL_C_TIMESTAMP (make-record sql-c-timestamp))
                           #+(or lispworks allegro)
                           (#.$SQL_CHAR (%new-cstring (1+ precision)))
                           (t (%new-ptr :ptr (1+ precision)))))
                        (out-len-ptr (%new-ptr :long)))
                   (%bind-column hstmt col-nr c-type data-ptr (1+ precision) out-len-ptr)
                   (setf (svref names col-nr) name
                         (aref sql-types col-nr) sql-type
                         (aref c-types col-nr) (sql-to-c-type sql-type)
                         (aref precisions col-nr) (if (zerop precision) nil precision)
                         (aref scales col-nr) scale
                         (aref nullables-p col-nr) nullable-p
                         (aref data-ptrs col-nr) data-ptr
                         (aref out-len-ptrs col-nr) out-len-ptr))))
             ;; the main loop
             (prog1
               (cond (flatp 
                      (when (> column-count 1)
                        (error "If more than one column is to be fetched, flatp has to be nil."))
                      (loop until (= (%sql-fetch hstmt) $SQL_NO_DATA_FOUND)
                            collect
                            (read-data (aref data-ptrs 0) 
                                       (aref c-types 0)
                                       (aref sql-types 0)
                                       (aref out-len-ptrs 0)
                                       nil)))
                     (t
                      (loop until (= (%sql-fetch hstmt) $SQL_NO_DATA_FOUND)
                            collect
                            (loop for col-nr from 0 to (1- column-count)
                                  collect
                                  (read-data (aref data-ptrs col-nr) 
                                             (aref c-types col-nr)
                                             (aref sql-types col-nr)
                                             (aref out-len-ptrs col-nr)
                                             nil)))))))
           names)
          ;; dispose of memory etc
          (when free-option (%free-statement hstmt free-option))
          (dotimes (col-nr column-count)
            (let ((data-ptr (aref data-ptrs col-nr))
                  (out-len-ptr (aref out-len-ptrs col-nr)))
              (when data-ptr (%dispose-ptr data-ptr)) ; we *did* allocate them
              (when out-len-ptr (%dispose-ptr out-len-ptr)))))))))

;; to do: factor out common parts, put the sceleton (the obligatory macro part)
;; of %do-fetch into sql package (has been done)

(defun %sql-prepare (hstmt sql)
  (with-cstr (sql-ptr sql)
    (with-error-handling (:hstmt hstmt)
      (SQLPrepare hstmt sql-ptr $SQL_NTS))))

;; depending on option, we return a long int or a string; string not implemented
(defun get-connection-option (hdbc option)
  (%with-temporary-allocation (;(param-ptr #.(1+ $SQL_MAX_OPTION_STRING_LENGTH))
                               (param-ptr :long))
    (with-error-handling (:hdbc hdbc)
                         (SQLGetConnectOption hdbc option param-ptr)
      (%get-long param-ptr))))

(defun set-connection-option (hdbc option param)
  (with-error-handling (:hdbc hdbc)
    (SQLSetConnectOption hdbc option param)))

(defun disable-autocommit (hdbc)
  (set-connection-option hdbc $SQL_AUTOCOMMIT $SQL_AUTOCOMMIT_OFF))

(defun enable-autocommit (hdbc)
  (set-connection-option hdbc $SQL_AUTOCOMMIT $SQL_AUTOCOMMIT_ON))

(defun %sql-set-pos (hstmt row option lock)
  (with-error-handling 
    (:hstmt hstmt)
    (SQLSetPos hstmt row option lock)))

(defun %sql-extended-fetch (hstmt fetch-type row)
  (%with-temporary-allocation ((row-count-ptr :unsigned-long)
                               (row-status-ptr :short))
    (with-error-handling (:hstmt hstmt)
      (SQLExtendedFetch hstmt fetch-type row row-count-ptr row-status-ptr)
      (values (%get-unsigned-long row-count-ptr)
              (%get-word row-status-ptr)))))

; column-nr is zero-based
(defun %sql-get-data (hstmt column-nr c-type data-ptr precision out-len-ptr)
  (with-error-handling
    (:hstmt hstmt :print-info nil)
    (SQLGetData hstmt (if (eq column-nr :bookmark) 0 (1+ column-nr))
                c-type data-ptr precision out-len-ptr)))

(defun %sql-param-data (hstmt param-ptr)
  (with-error-handling (:hstmt hstmt :print-info t) ;; nil
      (SQLParamData hstmt param-ptr)))

(defun %sql-put-data (hstmt data-ptr size)
  (with-error-handling
    (:hstmt hstmt :print-info t) ;; nil
    (SQLPutData hstmt data-ptr size)))

;(defconstant +null-ptr+ (%null-ptr))

(defun read-data-in-chunks (hstmt column-nr data-ptr c-type sql-type 
                                      out-len-ptr convert-to-string-p)
  (declare (ignore sql-type convert-to-string-p)) ; preliminary
  (let* ((res (%sql-get-data hstmt column-nr c-type data-ptr 
                             +max-precision+ out-len-ptr))
         (out-len (%get-long out-len-ptr))
         (offset 0))
    (case out-len
      (#.$SQL_NULL_DATA
       (return-from read-data-in-chunks *null*))
      (#.$SQL_NO_TOTAL ;; don't know how long it is going to be
       (let ((str (make-array 0 :element-type 'character :adjustable t)))
         (loop do (if (= c-type #.$SQL_CHAR)
                      (let ((data-length (%get-cstring-length data-ptr)))
                        (adjust-array str (+ offset data-length)
                                      :initial-element #\?)
                        (setf offset (%cstring-into-vector
                                      data-ptr str 
                                      offset 
                                      data-length)))
                    (error "wrong type. preliminary."))
               while (and (= res $SQL_SUCCESS_WITH_INFO)
                          (equal (sql-state (%null-ptr) (%null-ptr) hstmt)
                                 "01004"))
               do (setf res (%sql-get-data hstmt column-nr c-type data-ptr 
                                           +max-precision+ out-len-ptr)))
         (coerce str 'string)))
      (otherwise
       (let ((str (make-string out-len)))
         (loop do (if (= c-type #.$SQL_CHAR)
                      (setf offset (%cstring-into-vector ;string
                                    data-ptr str 
                                    offset 
                                    (min out-len (1- +max-precision+))))
                    (error "wrong type. preliminary."))
               while 
               (and (= res $SQL_SUCCESS_WITH_INFO)
                    (equal (sql-state (%null-ptr) (%null-ptr) hstmt)
                           "01004"))
               do (setf res (%sql-get-data hstmt column-nr c-type data-ptr 
                                           +max-precision+ out-len-ptr)
                        out-len (%get-long out-len-ptr)))
         str)))))

(defparameter *henv* nil)

(defparameter *hdbc* nil)

;; ??
(defun execute-sql-drop (henv hdbc sql)
  (%with-sql-pointer (hstmt-ptr) 
    (when (= (SQLAllocStmt hdbc hstmt-ptr) $SQL_SUCCESS)
      (let ((hstmt (%get-ptr hstmt-ptr)))
        (with-cstr (sql-ptr sql)
          (unwind-protect
            (let ((res (SQLExecDirect hstmt sql-ptr $SQL_NTS)))
              (or (= res $SQL_SUCCESS)
                  (error (handle-error henv hdbc hstmt))))
            (%free-statement hstmt :drop)))))))

#+mcl
(defun timestamp-to-universal-time (ptr)
  (values
   (encode-universal-time 
    (rref ptr sql-c-timestamp.second)
    (rref ptr sql-c-timestamp.minute)
    (rref ptr sql-c-timestamp.hour)
    (rref ptr sql-c-timestamp.day)
    (rref ptr sql-c-timestamp.month)
    (rref ptr sql-c-timestamp.year))
   (rref ptr sql-c-timestamp.fraction)))

#+mcl
(defun date-to-universal-time (ptr)
  (encode-universal-time
   0 0 0
   (rref ptr sql-c-date.day)
   (rref ptr sql-c-date.month)
   (rref ptr sql-c-date.year)))

#+mcl
(defun time-to-universal-time (ptr)
  (encode-universal-time 
   (rref ptr sql-c-time.second)
   (rref ptr sql-c-time.minute)
   (rref ptr sql-c-time.hour)
   0 0 0))

#+lispworks
(defun timestamp-to-universal-time (ptr)
  (values
   (encode-universal-time 
    (fli:foreign-slot-value ptr 'second)
    (fli:foreign-slot-value ptr 'minute)
    (fli:foreign-slot-value ptr 'hour)
    (fli:foreign-slot-value ptr 'day)
    (fli:foreign-slot-value ptr 'month)
    (fli:foreign-slot-value ptr 'year))
   (fli:foreign-slot-value ptr 'fraction)))

#+lispworks
(defun date-to-universal-time (ptr)
  (encode-universal-time
   0 0 0
   (fli:foreign-slot-value ptr 'day)
   (fli:foreign-slot-value ptr 'month)
   (fli:foreign-slot-value ptr 'year)))

#+lispworks
(defun time-to-universal-time (ptr)
  (encode-universal-time 
   (fli:foreign-slot-value ptr 'second)
   (fli:foreign-slot-value ptr 'minute)
   (fli:foreign-slot-value ptr 'hour)
   0 0 0))

#+allegro
(defun timestamp-to-universal-time (ptr)
  (values
   (encode-universal-time 
    (ff:fslot-value-typed 'sql-c-timestamp nil ptr 'second)
    (ff:fslot-value-typed 'sql-c-timestamp nil ptr 'minute)
    (ff:fslot-value-typed 'sql-c-timestamp nil ptr 'hour)
    (ff:fslot-value-typed 'sql-c-timestamp nil ptr 'day)
    (ff:fslot-value-typed 'sql-c-timestamp nil ptr 'month)
    (ff:fslot-value-typed 'sql-c-timestamp nil ptr 'year))
   (ff:fslot-value-typed 'sql-c-timestamp nil ptr 'fraction)))

#+allegro
(defun date-to-universal-time (ptr)
  (encode-universal-time
   0 0 0
   (ff:fslot-value-typed nil ptr 'day)
   (ff:fslot-value-typed nil ptr 'month)
   (ff:fslot-value-typed nil ptr 'year)))
   
#+allegro
(defun time-to-universal-time (ptr)
  (encode-universal-time 
    (ff:fslot-value-typed nil ptr 'second)
    (ff:fslot-value-typed nil ptr 'minute)
    (ff:fslot-value-typed nil ptr 'hour)
   0 0 0))

#+cormanlisp
(defun timestamp-to-universal-time (ptr)
  (values
   (encode-universal-time 
	(ct:cref sql-c-timestamp ptr second)
	(ct:cref sql-c-timestamp ptr minute)
	(ct:cref sql-c-timestamp ptr hour)
	(ct:cref sql-c-timestamp ptr day)
	(ct:cref sql-c-timestamp ptr month)
	(ct:cref sql-c-timestamp ptr year)
	(ct:cref sql-c-timestamp ptr fraction))))

#+cormanlisp
(defun date-to-universal-time (ptr)
  (encode-universal-time
   0 0 0
	(ct:cref sql-c-date ptr day)
	(ct:cref sql-c-date ptr month)
	(ct:cref sql-c-date ptr year)))

#+cormanlisp
(defun time-to-universal-time (ptr)
  (encode-universal-time 
	(ct:cref sql-c-time ptr second)
	(ct:cref sql-c-time ptr minute)
	(ct:cref sql-c-time ptr hour)
   0 0 0))
