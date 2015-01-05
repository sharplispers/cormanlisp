;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;; Portable SQL module
;; Version 0.85
;; Copyright (C) Paul Meurer 1999. All rights reserved.
;; paul.meurer@hit.uib.no

;;; "This software is based on the design of the ODBC interface provided by 
;;;  Harlequin Group plc in the LispWorks and LispWorks for the Windows 
;;;  Operating System products. This implementation is not the property of
;;;  Harlequin and they have no responsibility for its content or accuracy."

;; Documentation and the license agreement can be found in file 
;; "sql-odbc-documentation.lisp" 

(in-package :common-lisp-user)

(defpackage "SQL"
  (:use #+:mcl "CCL" "COMMON-LISP")
  #+lispworks
  (:import-from "LISPWORKS" "$")
  #-(or :mcl :cormanlisp)
  (:import-from #+:lispworks "STREAM" #+:allegro "EXCL"
                "FUNDAMENTAL-STREAM" "FUNDAMENTAL-CHARACTER-STREAM"
                "FUNDAMENTAL-CHARACTER-INPUT-STREAM"
                "FUNDAMENTAL-STREAM"
                "FUNDAMENTAL-INPUT-STREAM"
                "FUNDAMENTAL-CHARACTER-OUTPUT-STREAM"
                "FUNDAMENTAL-OUTPUT-STREAM"
                "FUNDAMENTAL-BINARY-STREAM"
                "FUNDAMENTAL-BINARY-INPUT-STREAM"
                "FUNDAMENTAL-BINARY-OUTPUT-STREAM"
                "STREAM-READ-CHAR"
                "STREAM-PEEK-CHAR"
                "STREAM-READ-SEQUENCE"
                "STREAM-WRITE-SEQUENCE")
  (:export "*DEFAULT-DATABASE-TYPE*" "*DEFAULT-DATABASE*" "*DEFAULT-QUERY*"
           "*CONNECT-IF-EXISTS*" "*CONNECTED-DATABASES*"
           "*ACTIVE-TRANSACTIONS*" "*COMMIT-NESTED-TRANSACTIONS-P*"
           "+NULL+" "*NULL*"
           "DATABASE" "QUERY" 
           "WITH-DATABASE"
           "INITIALIZE-DATABASE-TYPE"
           "DB-CONNECT" ;; Hmm. Why export this one?
           "CONNECT" "DISCONNECT"
           "COMMIT" "ROLLBACK" "WITH-TRANSACTION" "BEGIN-TRANSACTION"
           "EXECUTE-QUERY" "OPEN-QUERY" "CLOSE-QUERY" "CANCEL-QUERY"
           "DO-QUERY" "MAP-QUERY" "EXECUTE-COMMAND"
           "DO-BIND-QUERY" "MAP-BIND-QUERY"
           "PREPARE-STATEMENT" #-lispworks "$"
           ;;"EXECUTE-PARAMETERIZED"
           "WITH-PREPARED-STATEMENT" "BIND-EXECUTE"
           "BIND-EXECUTE-QUERY" "FETCH-QUERY-RESULTS"
           "SELECT" "SELECT-UNION" "INSERT"
           "INSERT-RECORDS" "UPDATE-RECORDS" "DELETE-RECORDS" 
           "CREATE-TABLE" "DROP-TABLE" "CREATE-INDEX" "DROP-INDEX"
           "CREATE-VIEW" "DROP-VIEW" "CREATE-USER" "EXECUTE"
           "LIKE" "NOT-LIKE" "IN" "NOT-IN" "NOT-NULL" "BETWEEN" "<>"
           "AVG" "SUM" "CONC" "MINUS"
           "ENABLE-SQL-READER-SYNTAX" "DISABLE-SQL-READER-SYNTAX"
           #+mcl "WITH-BLOB"
           "DESCRIBE-COLUMNS"
           ;; streams
           #+mcl "READ-SEQUENCE"
           #+mcl "WRITE-SEQUENCE"
           "BIND-EXECUTE-WITH-STREAM"
           "DATA-SOURCES"
           ))

