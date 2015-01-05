;;;-*- Mode: Lisp; Package: ODBC -*-

;; ODBC module for MCL, LWW and ACL
;; Version 0.85
;; Copyright (C) Paul Meurer 1999. All rights reserved.
;; paul.meurer@hit.uib.no
;;
;; Documentation and the license agreement can be found in file 
;; "sql-odbc-documentation.lisp".
;; Bug reports and suggestions are highly welcome.

(in-package :odbc)

;; to do: build a .h file parser!


(eval-when (:load-toplevel :compile-toplevel :execute)
  #+mcl (setf *foreign-module* "vsi:ODBC$DriverMgr")
  #+(and :allegro (not :unix)) (setf *foreign-module* "odbc32.dll")
  ;; adapt the library location to your needs
  #+(and :allegro :unix) (setf *foreign-module* "~/adabas/odbc/adabasodbc.so")
  
  (define-foreign-function "SQLAllocEnv"
    ((*phenv sql-handle-ptr)    ; HENV   FAR *phenv
     )
    :signed-short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLAllocConnect"
    ((henv sql-handle)          ; HENV        henv
     (*phdbc sql-handle-ptr)    ; HDBC   FAR *phdbc
     )
    :signed-short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLConnect"
    ((hdbc sql-handle)          ; HDBC        hdbc
     (*szDSN string-ptr)        ; UCHAR  FAR *szDSN
     (cbDSN :short)             ; SWORD       cbDSN
     (*szUID string-ptr)        ; UCHAR  FAR *szUID 
     (cbUID :short)             ; SWORD       cbUID
     (*szAuthStr string-ptr)    ; UCHAR  FAR *szAuthStr
     (cbAuthStr :short)         ; SWORD       cbAuthStr
     )
    :signed-short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLDriverConnect"
    ((hdbc sql-handle)          ; HDBC        hdbc
     (hwnd sql-handle)          ; SQLHWND     hwnd
     (*szConnStrIn string-ptr)  ; UCHAR  FAR *szConnStrIn
     (cbConnStrIn :short)       ; SWORD       cbConnStrIn
     (*szConnStrOut string-ptr) ; UCHAR  FAR *szConnStrOut
     (cbConnStrOutMax :short)   ; SWORD       cbConnStrOutMax
     (*pcbConnStrOut :ptr)      ; SWORD  FAR *pcbConnStrOut
     (fDriverCompletion :short) ; UWORD       fDriverCompletion
     )
    :signed-short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLDisconnect"
    ((hdbc sql-handle))         ; HDBC        hdbc
    :signed-short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLAllocStmt"
    ((hdbc sql-handle)          ; HDBC        hdbc
     (*phstmt sql-handle-ptr)   ; HSTMT  FAR *phstmt
     )
    :signed-short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLGetInfo"
    ((hdbc sql-handle)          ; HDBC        hdbc
     (fInfoType :short)         ; UWORD       fInfoType
     (rgbInfoValue :ptr)        ; PTR         rgbInfoValue
     (cbInfoValueMax :short)    ; SWORD       cbInfoValueMax
     (*pcbInfoValue :ptr)       ; SWORD  FAR *pcbInfoValue
     )
    :signed-short)              ; RETCODE_SQL_API

  (define-foreign-function "SQLPrepare"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (*szSqlStr string-ptr)     ; UCHAR  FAR *szSqlStr
     (cbSqlStr :long)           ; SDWORD      cbSqlStr
     )
    :signed-short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLExecute"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     )
    :signed-short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLExecDirect"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (*szSqlStr string-ptr)     ; UCHAR  FAR *szSqlStr
     (cbSqlStr :long)           ; SDWORD      cbSqlStr
     )
    :signed-short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLFreeStmt"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (fOption :short))          ; UWORD       fOption
    :signed-short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLCancel"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     )
    :signed-short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLError"
    ((henv sql-handle)          ; HENV        henv
     (hdbc sql-handle)          ; HDBC        hdbc
     (hstmt sql-handle)         ; HSTMT       hstmt
     (*szSqlState string-ptr)   ; UCHAR  FAR *szSqlState
     (*pfNativeError :ptr)      ; SDWORD FAR *pfNativeError
     (*szErrorMsg string-ptr)   ; UCHAR  FAR *szErrorMsg
     (cbErrorMsgMax :short)     ; SWORD       cbErrorMsgMax
     (*pcbErrorMsg :ptr)        ; SWORD  FAR *pcbErrorMsg
     )
    :signed-short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLNumResultCols"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (*pccol :ptr)              ; SWORD  FAR *pccol
     )
    :signed-short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLRowCount"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (*pcrow :ptr)              ; SDWORD FAR *pcrow
     )
    :signed-short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLDescribeCol"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (icol :short)              ; UWORD       icol
     (*szColName string-ptr)    ; UCHAR  FAR *szColName
     (cbColNameMax :short)      ; SWORD       cbColNameMax
     (*pcbColName :ptr)         ; SWORD  FAR *pcbColName
     (*pfSqlType :ptr)          ; SWORD  FAR *pfSqlType
     (*pcbColDef :ptr)          ; UDWORD FAR *pcbColDef
     (*pibScale :ptr)           ; SWORD  FAR *pibScale
     (*pfNullable :ptr)         ; SWORD  FAR *pfNullable
     )
    :signed-short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLColAttributes"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (icol :short)              ; UWORD       icol
     (fDescType :short)         ; UWORD       fDescType
     (rgbDesc :ptr)             ; PTR         rgbDesc
     (cbDescMax :short)         ; SWORD       cbDescMax
     (*pcbDesc :ptr)            ; SWORD  FAR *pcbDesc
     (*pfDesc :ptr)             ; SDWORD FAR *pfDesc
     )
    :signed-short)              ; RETCODE_SQL_API

  (define-foreign-function "SQLColumns"
    ((hstmt sql-handle)             ; HSTMT       hstmt
     (*szTableQualifier string-ptr) ; UCHAR  FAR *szTableQualifier
     (cbTableQualifier :short)      ; SWORD       cbTableQualifier
     (*szTableOwner string-ptr)     ; UCHAR  FAR *szTableOwner
     (cbTableOwner :short)          ; SWORD       cbTableOwner
     (*szTableName string-ptr)      ; UCHAR  FAR *szTableName
     (cbTableName :short)           ; SWORD       cbTableName
     (*szColumnName string-ptr)     ; UCHAR  FAR *szColumnName
     (cbColumnName :short)          ; SWORD       cbColumnName
     )
    :signed-short)              ; RETCODE_SQL_API

  (define-foreign-function "SQLBindCol"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (icol :short)              ; UWORD       icol
     (fCType :short)            ; SWORD       fCType
     (rgbValue :ptr)            ; PTR         rgbValue
     (cbValueMax :long)         ; SDWORD      cbValueMax
     (*pcbValue :ptr)           ; SDWORD FAR *pcbValue
     )
    :signed-short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLFetch"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     )
    :signed-short)              ; RETCODE_SQL_API
    
  (define-foreign-function "SQLTransact"
    ((henv sql-handle)          ; HENV        henv
     (hdbc sql-handle)          ; HDBC        hdbc
     (fType :short)             ; UWORD       fType ($SQL_COMMIT or $SQL_ROLLBACK)
     )
    :signed-short)              ; RETCODE_SQL_API

  ;; ODBC 2.0
  (define-foreign-function "SQLDescribeParam"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (ipar :short)              ; UWORD       ipar
     (*pfSqlType :ptr)          ; SWORD  FAR *pfSqlType
     (*pcbColDef :ptr)          ; UDWORD FAR *pcbColDef
     (*pibScale :ptr)           ; SWORD  FAR *pibScale
     (*pfNullable :ptr)         ; SWORD  FAR *pfNullable
     )
    :signed-short)              ; RETCODE_SQL_API
  
  ;; ODBC 2.0
  (define-foreign-function "SQLBindParameter"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (ipar :short)              ; UWORD       ipar
     (fParamType :short)        ; SWORD       fParamType
     (fCType :short)            ; SWORD       fCType
     (fSqlType :short)          ; SWORD       fSqlType
     (cbColDef :long)           ; UDWORD      cbColDef
     (ibScale :short)           ; SWORD       ibScale
     (rgbValue :ptr)            ; PTR         rgbValue
     (cbValueMax :long)         ; SDWORD      cbValueMax
     (*pcbValue :ptr)           ; SDWORD FAR *pcbValue
     )
    :signed-short)              ; RETCODE_SQL_API
  
  ;; level 1
  (define-foreign-function "SQLGetData"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (icol :short)              ; UWORD       icol
     (fCType :short)            ; SWORD       fCType
     (rgbValue :ptr)            ; PTR         rgbValue
     (cbValueMax :long)         ; SDWORD      cbValueMax
     (*pcbValue :ptr)           ; SDWORD FAR *pcbValue
     )
    :signed-short)              ; RETCODE_SQL_API

  (define-foreign-function "SQLParamData"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (*prgbValue :ptr)          ; PTR    FAR *prgbValue
     )
    :signed-short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLPutData"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (rgbValue :ptr)            ; PTR         rgbValue
     (cbValue :long)            ; SDWORD      cbValue
     )
    :signed-short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLGetConnectOption"
    ((hdbc sql-handle)          ; HDBC        hdbc
     (fOption :short)           ; UWORD       fOption
     (pvParam :ptr)             ; PTR         pvParam
     )
    :signed-short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLSetConnectOption"
    ((hdbc sql-handle)          ; HDBC        hdbc
     (fOption :short)           ; UWORD       fOption
     (vParam :long)             ; UDWORD      vParam
     )
    :signed-short)              ; RETCODE_SQL_API

  (define-foreign-function "SQLSetPos"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (irow :short)              ; UWORD       irow
     (fOption :short)           ; UWORD       fOption
     (fLock :short)             ; UWORD       fLock
     )
    :signed-short)              ; RETCODE_SQL_API

  ; level 2
  (define-foreign-function "SQLExtendedFetch"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (fFetchType :short)        ; UWORD       fFetchType
     (irow :long)               ; SDWORD      irow
     (*pcrow :ptr)              ; UDWORD FAR *pcrow
     (*rgfRowStatus :ptr)       ; UWORD  FAR *rgfRowStatus
     )
    :signed-short)              ; RETCODE_SQL_API
  
  (define-foreign-function "SQLDataSources"
    ((henv sql-handle)          ; HENV        henv
     (fDirection :short)
     (*szDSN string-ptr)        ; UCHAR  FAR *szDSN
     (cbDSNMax :short)          ; SWORD       cbDSNMax
     (*pcbDSN :ptr)             ; SWORD      *pcbDSN
     (*szDescription string-ptr) ; UCHAR     *szDescription
     (cbDescriptionMax :short)  ; SWORD       cbDescriptionMax
     (*pcbDescription :ptr)     ; SWORD      *pcbDescription
     )
    :signed-short)              ; RETCODE_SQL_API

  (define-foreign-function "SQLFreeEnv"
    ((henv sql-handle)          ; HSTMT       hstmt
     )
    :signed-short)              ; RETCODE_SQL_API
  )

(defmacro %sql-len-data-at-exec (length) 
  `(- $SQL_LEN_DATA_AT_EXEC_OFFSET ,length))

;; to do: make unifying interface for defrecord etc

#+mcl
(defrecord (sql-c-date :ptr)
  (year  :short)
  (month :short)
  (day   :short))

#+mcl
(defrecord (sql-c-time :ptr) 
  (hour   :short)
  (minute :short)
  (second :short))

#+mcl
(defrecord (sql-c-timestamp :ptr)
  (year     :short)
  (month    :short)
  (day      :short)
  (hour     :short)
  (minute   :short)
  (second   :short)
  (fraction :long))

#+lispworks
(eval-when (:execute :load-toplevel :compile-toplevel)
  (fli:define-c-struct sql-c-time 
    (hour   :short)
    (minute :short)
    (second :short))
  
  (fli:define-c-struct sql-c-date
    (year  :short)
    (month :short)
    (day   :short))
  
  (fli:define-c-struct sql-c-timestamp
    (year     :short)
    (month    :short)
    (day      :short)
    (hour     :short)
    (minute   :short)
    (second   :short)
    (fraction :long)))

#+allegro
(eval-when (:execute :load-toplevel :compile-toplevel)
  (ff:def-foreign-type sql-c-time 
    (:struct 
     (hour   :short)
     (minute :short)
     (second :short)))
  
  (ff:def-foreign-type sql-c-date
    (:struct 
     (year  :short)
     (month :short)
     (day   :short)))
  
  (ff:def-foreign-type sql-c-timestamp
    (:struct 
     (year     :short)
     (month    :short)
     (day      :short)
     (hour     :short)
     (minute   :short)
     (second   :short)
     (fraction :long))))

#+cormanlisp
(eval-when (:execute :load-toplevel :compile-toplevel)
	(defwinstruct sql-c-time
		((hour :short)
			(minute :short)
			(second :short)))

	(defwinstruct sql-c-date
		((year :short)
			(month :short)
			(day :short)))

	(defwinstruct sql-c-timestamp
		((year :short)
			(month :short)
			(day :short)
			(hour :short)
			(minute :short)
			(second :short)
			(fraction :long))))
