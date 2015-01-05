;;;-*- Mode: Lisp; Package: (ODBC) -*-

;; ODBC module for MCL, LWW and ACL/Windows
;; Version 0.85
;; Copyright (C) Paul Meurer 1999. All rights reserved.
;; paul.meurer@hit.uib.no
;;
;; Documentation and the license agreement can be found in file 
;; "sql-odbc-documentation.lisp".
;; Bug reports and suggestions are highly welcome.

(in-package :odbc)

(defconstant $ODBCVER	#x0210)

;; generally useful constants
;;#if (ODBCVER >= #x0200)
(defconstant $SQL_SPEC_MAJOR 2)		;; Major version of specification 
(defconstant $SQL_SPEC_MINOR 10) 	;; Minor version of specification 
(defconstant $SQL_SPEC_STRING 	"02.10") ;; String constant for version	  
;;;; #endif	;; ODBCVER >= #x0200
(defconstant $SQL_SQLSTATE_SIZE 5)		;; size of SQLSTATE 			  
(defconstant $SQL_MAX_MESSAGE_LENGTH 512)	;; message buffer size			  
(defconstant $SQL_MAX_DSN_LENGTH 32)		;; maximum data source name size  

;; RETCODEs
(defconstant $SQL_INVALID_HANDLE -2)
(defconstant $SQL_ERROR -1)
(defconstant $SQL_SUCCESS 0)
(defconstant $SQL_SUCCESS_WITH_INFO 1)
(defconstant $SQL_NO_DATA_FOUND 100)

;; Standard SQL datatypes, using ANSI type numbering
(defconstant $SQL_CHAR 1)
(defconstant $SQL_NUMERIC 2)
(defconstant $SQL_DECIMAL 3)
(defconstant $SQL_INTEGER 4)
(defconstant $SQL_SMALLINT 5)
(defconstant $SQL_FLOAT 6)
(defconstant $SQL_REAL 7)
(defconstant $SQL_DOUBLE 8)
(defconstant $SQL_VARCHAR 12)

(defconstant $SQL_TYPE_MIN $SQL_CHAR)
(defconstant $SQL_TYPE_NULL 0)
(defconstant $SQL_TYPE_MAX $SQL_VARCHAR)

;; C datatype to SQL datatype mapping	SQL types

(defconstant $SQL_C_CHAR $SQL_CHAR)		;; CHAR, VARCHAR, DECIMAL, NUMERIC
(defconstant $SQL_C_LONG $SQL_INTEGER)		;; INTEGER 
(defconstant $SQL_C_SHORT $SQL_SMALLINT)	;; SMALLINT 
(defconstant $SQL_C_FLOAT $SQL_REAL)		;; REAL 
(defconstant $SQL_C_DOUBLE $SQL_DOUBLE)		;; FLOAT, DOUBLE
(defconstant $SQL_C_DEFAULT 99)

;; NULL status constants.  These are used in SQLColumns, SQLColAttributes,
;;SQLDescribeCol, SQLDescribeParam, and SQLSpecialColumns to describe the
;;nullablity of a column in a table.

(defconstant $SQL_NO_NULLS 0)
(defconstant $SQL_NULLABLE 1)
(defconstant $SQL_NULLABLE_UNKNOWN 2)

;; Special length values
(defconstant $SQL_NULL_DATA -1)
(defconstant $SQL_DATA_AT_EXEC -2)
(defconstant $SQL_NTS -3)

;; SQLFreeStmt defines
(defconstant $SQL_CLOSE 0)
(defconstant $SQL_DROP 1)
(defconstant $SQL_UNBIND 2)
(defconstant $SQL_RESET_PARAMS 3)

;; SQLTransact defines
(defconstant $SQL_COMMIT 0)
(defconstant $SQL_ROLLBACK 1)

;; SQLColAttributes defines
(defconstant $SQL_COLUMN_COUNT 0)
(defconstant $SQL_COLUMN_NAME 1)
(defconstant $SQL_COLUMN_TYPE 2)
(defconstant $SQL_COLUMN_LENGTH 3)
(defconstant $SQL_COLUMN_PRECISION 4)
(defconstant $SQL_COLUMN_SCALE 5)
(defconstant $SQL_COLUMN_DISPLAY_SIZE 6)
(defconstant $SQL_COLUMN_NULLABLE 7)
(defconstant $SQL_COLUMN_UNSIGNED 8)
(defconstant $SQL_COLUMN_MONEY 9)
(defconstant $SQL_COLUMN_UPDATABLE 10)
(defconstant $SQL_COLUMN_AUTO_INCREMENT	11)
(defconstant $SQL_COLUMN_CASE_SENSITIVE	12)
(defconstant $SQL_COLUMN_SEARCHABLE 13)
(defconstant $SQL_COLUMN_TYPE_NAME 14)
;;#if (ODBCVER >= #x0200)
(defconstant $SQL_COLUMN_TABLE_NAME 15)
(defconstant $SQL_COLUMN_OWNER_NAME 16)
(defconstant $SQL_COLUMN_QUALIFIER_NAME	17)
(defconstant $SQL_COLUMN_LABEL 18)
(defconstant $SQL_COLATT_OPT_MAX $SQL_COLUMN_LABEL)
#|;; #else
(defconstant $SQL_COLATT_OPT_MAX $SQL_COLUMN_TYPE_NAME) |#

(defconstant $SQL_COLUMN_DRIVER_START 1000)

(defconstant $SQL_COLATT_OPT_MIN $SQL_COLUMN_COUNT)

;; SQLColAttributes subdefines for SQL_COLUMN_UPDATABLE
(defconstant $SQL_ATTR_READONLY 0)
(defconstant $SQL_ATTR_WRITE 1)
(defconstant $SQL_ATTR_READWRITE_UNKNOWN 2)

;; SQLColAttributes subdefines for SQL_COLUMN_SEARCHABLE
;; These are also used by SQLGetInfo                    
(defconstant $SQL_UNSEARCHABLE 0)
(defconstant $SQL_LIKE_ONLY 1)
(defconstant $SQL_ALL_EXCEPT_LIKE 2)
(defconstant $SQL_SEARCHABLE 3)

;; SQLError defines
(defconstant $SQL_NULL_HENV 0)
(defconstant $SQL_NULL_HDBC 0)
(defconstant $SQL_NULL_HSTMT 0)

;; Defines for SQLGetFunctions
;; Core Functions
;;
(defconstant $SQL_API_SQLALLOCCONNECT      1)
(defconstant $SQL_API_SQLALLOCENV          2)
(defconstant $SQL_API_SQLALLOCSTMT         3)
(defconstant $SQL_API_SQLBINDCOL           4)
(defconstant $SQL_API_SQLCANCEL            5)
(defconstant $SQL_API_SQLCOLATTRIBUTES     6)
(defconstant $SQL_API_SQLCONNECT           7)
(defconstant $SQL_API_SQLDESCRIBECOL       8)
(defconstant $SQL_API_SQLDISCONNECT        9)
(defconstant $SQL_API_SQLERROR            10)
(defconstant $SQL_API_SQLEXECDIRECT       11)
(defconstant $SQL_API_SQLEXECUTE          12)
(defconstant $SQL_API_SQLFETCH            13)
(defconstant $SQL_API_SQLFREECONNECT      14)
(defconstant $SQL_API_SQLFREEENV          15)
(defconstant $SQL_API_SQLFREESTMT         16)
(defconstant $SQL_API_SQLGETCURSORNAME    17)
(defconstant $SQL_API_SQLNUMRESULTCOLS    18)
(defconstant $SQL_API_SQLPREPARE          19)
(defconstant $SQL_API_SQLROWCOUNT         20)
(defconstant $SQL_API_SQLSETCURSORNAME    21)
(defconstant $SQL_API_SQLSETPARAM         22)
(defconstant $SQL_API_SQLTRANSACT         23)
(defconstant $SQL_NUM_FUNCTIONS           23)
(defconstant $SQL_EXT_API_START           40)

;; Level 1 Functions

(defconstant $SQL_API_SQLCOLUMNS          40)
(defconstant $SQL_API_SQLDRIVERCONNECT    41)
(defconstant $SQL_API_SQLGETCONNECTOPTION 42)
(defconstant $SQL_API_SQLGETDATA          43)
(defconstant $SQL_API_SQLGETFUNCTIONS     44)
(defconstant $SQL_API_SQLGETINFO          45)
(defconstant $SQL_API_SQLGETSTMTOPTION    46)
(defconstant $SQL_API_SQLGETTYPEINFO      47)
(defconstant $SQL_API_SQLPARAMDATA        48)
(defconstant $SQL_API_SQLPUTDATA          49)
(defconstant $SQL_API_SQLSETCONNECTOPTION 50)
(defconstant $SQL_API_SQLSETSTMTOPTION    51)
(defconstant $SQL_API_SQLSPECIALCOLUMNS   52)
(defconstant $SQL_API_SQLSTATISTICS       53)
(defconstant $SQL_API_SQLTABLES           54)

;; Level 2 Functions

(defconstant $SQL_API_SQLBROWSECONNECT    55)
(defconstant $SQL_API_SQLCOLUMNPRIVILEGES 56)
(defconstant $SQL_API_SQLDATASOURCES      57)
(defconstant $SQL_API_SQLDESCRIBEPARAM    58)
(defconstant $SQL_API_SQLEXTENDEDFETCH    59)
(defconstant $SQL_API_SQLFOREIGNKEYS      60)
(defconstant $SQL_API_SQLMORERESULTS      61)
(defconstant $SQL_API_SQLNATIVESQL        62)
(defconstant $SQL_API_SQLNUMPARAMS        63)
(defconstant $SQL_API_SQLPARAMOPTIONS     64)
(defconstant $SQL_API_SQLPRIMARYKEYS      65)
(defconstant $SQL_API_SQLPROCEDURECOLUMNS 66)
(defconstant $SQL_API_SQLPROCEDURES       67)
(defconstant $SQL_API_SQLSETPOS           68)
(defconstant $SQL_API_SQLSETSCROLLOPTIONS 69)
(defconstant $SQL_API_SQLTABLEPRIVILEGES  70)

;/*		SDK 2.0 Additions		*/
;#if (ODBCVER >= #x0200))
(defconstant $SQL_API_SQLDRIVERS 71)
(defconstant $SQL_API_SQLBINDPARAMETER	72)
(defconstant $SQL_EXT_API_LAST $SQL_API_SQLBINDPARAMETER)
;;; #else
;(defconstant $SQL_EXT_API_LAST $SQL_API_SQLTABLEPRIVILEGES)
;;; #endif	;; ODBCVER >= #x0200

(defconstant $SQL_API_ALL_FUNCTIONS 0)

(defconstant $SQL_NUM_EXTENSIONS (- $SQL_EXT_API_LAST $SQL_EXT_API_START -1))
;#if (ODBCVER >= #x0200))
(defconstant $SQL_API_LOADBYORDINAL 199)
;;;; #endif	;; ODBCVER >= #x0200

;;; Defines for SQLGetInfo
(defconstant $SQL_INFO_FIRST                       0)
(defconstant $SQL_ACTIVE_CONNECTIONS               0)
(defconstant $SQL_ACTIVE_STATEMENTS                1)
(defconstant $SQL_DATA_SOURCE_NAME                 2)
(defconstant $SQL_DRIVER_HDBC                      3)
(defconstant $SQL_DRIVER_HENV                      4)
(defconstant $SQL_DRIVER_HSTMT                     5)
(defconstant $SQL_DRIVER_NAME                      6)
(defconstant $SQL_DRIVER_VER                       7)
(defconstant $SQL_FETCH_DIRECTION                  8)
(defconstant $SQL_ODBC_API_CONFORMANCE             9)
(defconstant $SQL_ODBC_VER                        10)
(defconstant $SQL_ROW_UPDATES                     11)
(defconstant $SQL_ODBC_SAG_CLI_CONFORMANCE        12)
(defconstant $SQL_SERVER_NAME                     13)
(defconstant $SQL_SEARCH_PATTERN_ESCAPE           14)
(defconstant $SQL_ODBC_SQL_CONFORMANCE            15)

(defconstant $SQL_DBMS_NAME                       17)
(defconstant $SQL_DBMS_VER                        18)

(defconstant $SQL_ACCESSIBLE_TABLES               19)
(defconstant $SQL_ACCESSIBLE_PROCEDURES           20)
(defconstant $SQL_PROCEDURES                      21)
(defconstant $SQL_CONCAT_NULL_BEHAVIOR            22)
(defconstant $SQL_CURSOR_COMMIT_BEHAVIOR          23)
(defconstant $SQL_CURSOR_ROLLBACK_BEHAVIOR        24)
(defconstant $SQL_DATA_SOURCE_READ_ONLY           25)
(defconstant $SQL_DEFAULT_TXN_ISOLATION           26)
(defconstant $SQL_EXPRESSIONS_IN_ORDERBY          27)
(defconstant $SQL_IDENTIFIER_CASE                 28)
(defconstant $SQL_IDENTIFIER_QUOTE_CHAR           29)
(defconstant $SQL_MAX_COLUMN_NAME_LEN             30)
(defconstant $SQL_MAX_CURSOR_NAME_LEN             31)
(defconstant $SQL_MAX_OWNER_NAME_LEN              32)
(defconstant $SQL_MAX_PROCEDURE_NAME_LEN          33)
(defconstant $SQL_MAX_QUALIFIER_NAME_LEN          34)
(defconstant $SQL_MAX_TABLE_NAME_LEN              35)
(defconstant $SQL_MULT_RESULT_SETS                36)
(defconstant $SQL_MULTIPLE_ACTIVE_TXN             37)
(defconstant $SQL_OUTER_JOINS                     38)
(defconstant $SQL_OWNER_TERM                      39)
(defconstant $SQL_PROCEDURE_TERM                  40)
(defconstant $SQL_QUALIFIER_NAME_SEPARATOR        41)
(defconstant $SQL_QUALIFIER_TERM                  42)
(defconstant $SQL_SCROLL_CONCURRENCY              43)
(defconstant $SQL_SCROLL_OPTIONS                  44)
(defconstant $SQL_TABLE_TERM                      45)
(defconstant $SQL_TXN_CAPABLE                     46)
(defconstant $SQL_USER_NAME                       47)

(defconstant $SQL_CONVERT_FUNCTIONS               48)
(defconstant $SQL_NUMERIC_FUNCTIONS               49)
(defconstant $SQL_STRING_FUNCTIONS                50)
(defconstant $SQL_SYSTEM_FUNCTIONS                51)
(defconstant $SQL_TIMEDATE_FUNCTIONS              52)

(defconstant $SQL_CONVERT_BIGINT                  53)
(defconstant $SQL_CONVERT_BINARY                  54)
(defconstant $SQL_CONVERT_BIT                     55)
(defconstant $SQL_CONVERT_CHAR                    56)
(defconstant $SQL_CONVERT_DATE                    57)
(defconstant $SQL_CONVERT_DECIMAL                 58)
(defconstant $SQL_CONVERT_DOUBLE                  59)
(defconstant $SQL_CONVERT_FLOAT                   60)
(defconstant $SQL_CONVERT_INTEGER                 61)
(defconstant $SQL_CONVERT_LONGVARCHAR             62)
(defconstant $SQL_CONVERT_NUMERIC                 63)
(defconstant $SQL_CONVERT_REAL                    64)
(defconstant $SQL_CONVERT_SMALLINT                65)
(defconstant $SQL_CONVERT_TIME                    66)
(defconstant $SQL_CONVERT_TIMESTAMP               67)
(defconstant $SQL_CONVERT_TINYINT                 68)
(defconstant $SQL_CONVERT_VARBINARY               69)
(defconstant $SQL_CONVERT_VARCHAR                 70)
(defconstant $SQL_CONVERT_LONGVARBINARY           71)

(defconstant $SQL_TXN_ISOLATION_OPTION            72)
(defconstant $SQL_ODBC_SQL_OPT_IEF                73)

;;; ODBC SDK 1.0 Additions
(defconstant $SQL_CORRELATION_NAME 74)
(defconstant $SQL_NON_NULLABLE_COLUMNS 75)

;;; ODBC SDK 2.0 Additions
;;#if (ODBCVER >= #x0200)
(defconstant $SQL_DRIVER_HLIB 			76)
(defconstant $SQL_DRIVER_ODBC_VER		77)
(defconstant $SQL_LOCK_TYPES			78)
(defconstant $SQL_POS_OPERATIONS		79)
(defconstant $SQL_POSITIONED_STATEMENTS		80)
(defconstant $SQL_GETDATA_EXTENSIONS		81)
(defconstant $SQL_BOOKMARK_PERSISTENCE		82)
(defconstant $SQL_STATIC_SENSITIVITY		83)
(defconstant $SQL_FILE_USAGE			84)
(defconstant $SQL_NULL_COLLATION		85)
(defconstant $SQL_ALTER_TABLE 			86)
(defconstant $SQL_COLUMN_ALIAS			87)
(defconstant $SQL_GROUP_BY			88)
(defconstant $SQL_KEYWORDS			89)
(defconstant $SQL_ORDER_BY_COLUMNS_IN_SELECT	90)
(defconstant $SQL_OWNER_USAGE 			91)
(defconstant $SQL_QUALIFIER_USAGE		92)
(defconstant $SQL_QUOTED_IDENTIFIER_CASE	93)
(defconstant $SQL_SPECIAL_CHARACTERS		94)
(defconstant $SQL_SUBQUERIES			95)
(defconstant $SQL_UNION				96)
(defconstant $SQL_MAX_COLUMNS_IN_GROUP_BY	97)
(defconstant $SQL_MAX_COLUMNS_IN_INDEX		98)
(defconstant $SQL_MAX_COLUMNS_IN_ORDER_BY	99)
(defconstant $SQL_MAX_COLUMNS_IN_SELECT	       100)
(defconstant $SQL_MAX_COLUMNS_IN_TABLE		   101)
(defconstant $SQL_MAX_INDEX_SIZE				   102)
(defconstant $SQL_MAX_ROW_SIZE_INCLUDES_LONG	   103)
(defconstant $SQL_MAX_ROW_SIZE				   104)
(defconstant $SQL_MAX_STATEMENT_LEN			   105)
(defconstant $SQL_MAX_TABLES_IN_SELECT 106)
(defconstant $SQL_MAX_USER_NAME_LEN 107)
(defconstant $SQL_MAX_CHAR_LITERAL_LEN 108)
(defconstant $SQL_TIMEDATE_ADD_INTERVALS 109)
(defconstant $SQL_TIMEDATE_DIFF_INTERVALS 	   110)
(defconstant $SQL_NEED_LONG_DATA_LEN 111)
(defconstant $SQL_MAX_BINARY_LITERAL_LEN		   112)
(defconstant $SQL_LIKE_ESCAPE_CLAUSE			   113)
(defconstant $SQL_QUALIFIER_LOCATION			   114)

#|

;#if (ODBCVER >= #x0201)
/*** ODBC SDK 2.01 Additions ***/)
(defconstant $SQL_OJ_CAPABILITIES 			 65003	;; Temp value until ODBC 3.0
;;;; #endif	;; ODBCVER >= #x0201

(defconstant $SQL_INFO_LAST						SQL_QUALIFIER_LOCATION
;;;; #else
(defconstant $SQL_INFO_LAST						SQL_NON_NULLABLE_COLUMNS
;;;; #endif	;; ODBCVER >= #x0200
)
(defconstant $SQL_INFO_DRIVER_START             1000

;; SQL_CONVERT_*  return value bitmasks
)
(defconstant $SQL_CVT_CHAR				#x00000001L)
(defconstant $SQL_CVT_NUMERIC 			#x00000002L)
(defconstant $SQL_CVT_DECIMAL 			#x00000004L)
(defconstant $SQL_CVT_INTEGER 			#x00000008L)
(defconstant $SQL_CVT_SMALLINT			#x00000010L)
(defconstant $SQL_CVT_FLOAT				#x00000020L)
(defconstant $SQL_CVT_REAL				#x00000040L)
(defconstant $SQL_CVT_DOUBLE				#x00000080L)
(defconstant $SQL_CVT_VARCHAR 			#x00000100L)
(defconstant $SQL_CVT_LONGVARCHAR 		#x00000200L)
(defconstant $SQL_CVT_BINARY				#x00000400L)
(defconstant $SQL_CVT_VARBINARY			#x00000800L)
(defconstant $SQL_CVT_BIT 				#x00001000L)
(defconstant $SQL_CVT_TINYINT 			#x00002000L)
(defconstant $SQL_CVT_BIGINT				#x00004000L)
(defconstant $SQL_CVT_DATE				#x00008000L)
(defconstant $SQL_CVT_TIME				#x00010000L)
(defconstant $SQL_CVT_TIMESTAMP			#x00020000L)
(defconstant $SQL_CVT_LONGVARBINARY		#x00040000L)

;; SQL_CONVERT_FUNCTIONS functions)
(defconstant $SQL_FN_CVT_CONVERT			#x00000001L)

;; SQL_STRING_FUNCTIONS functions

(defconstant $SQL_FN_STR_CONCAT			#x00000001L)
(defconstant $SQL_FN_STR_INSERT			#x00000002L)
(defconstant $SQL_FN_STR_LEFT 			#x00000004L)
(defconstant $SQL_FN_STR_LTRIM			#x00000008L)
(defconstant $SQL_FN_STR_LENGTH			#x00000010L)
(defconstant $SQL_FN_STR_LOCATE			#x00000020L)
(defconstant $SQL_FN_STR_LCASE			#x00000040L)
(defconstant $SQL_FN_STR_REPEAT			#x00000080L)
(defconstant $SQL_FN_STR_REPLACE			#x00000100L)
(defconstant $SQL_FN_STR_RIGHT			#x00000200L)
(defconstant $SQL_FN_STR_RTRIM			#x00000400L)
(defconstant $SQL_FN_STR_SUBSTRING		#x00000800L)
(defconstant $SQL_FN_STR_UCASE			#x00001000L)
(defconstant $SQL_FN_STR_ASCII			#x00002000L)
(defconstant $SQL_FN_STR_CHAR 			#x00004000L
;#if (ODBCVER >= #x0200))
(defconstant $SQL_FN_STR_DIFFERENCE		#x00008000L)
(defconstant $SQL_FN_STR_LOCATE_2 		#x00010000L)
(defconstant $SQL_FN_STR_SOUNDEX			#x00020000L)
(defconstant $SQL_FN_STR_SPACE			#x00040000L
;; #endif	;; ODBCVER >= #x0200

;; SQL_NUMERIC_FUNCTIONS functions
)
(defconstant $SQL_FN_NUM_ABS				#x00000001L)
(defconstant $SQL_FN_NUM_ACOS 			#x00000002L)
(defconstant $SQL_FN_NUM_ASIN 			#x00000004L)
(defconstant $SQL_FN_NUM_ATAN 			#x00000008L)
(defconstant $SQL_FN_NUM_ATAN2			#x00000010L)
(defconstant $SQL_FN_NUM_CEILING			#x00000020L)
(defconstant $SQL_FN_NUM_COS				#x00000040L)
(defconstant $SQL_FN_NUM_COT				#x00000080L)
(defconstant $SQL_FN_NUM_EXP				#x00000100L)
(defconstant $SQL_FN_NUM_FLOOR			#x00000200L)
(defconstant $SQL_FN_NUM_LOG				#x00000400L)
(defconstant $SQL_FN_NUM_MOD				#x00000800L)
(defconstant $SQL_FN_NUM_SIGN 			#x00001000L)
(defconstant $SQL_FN_NUM_SIN				#x00002000L)
(defconstant $SQL_FN_NUM_SQRT 			#x00004000L)
(defconstant $SQL_FN_NUM_TAN				#x00008000L)
(defconstant $SQL_FN_NUM_PI				#x00010000L)
(defconstant $SQL_FN_NUM_RAND 			#x00020000L
;#if (ODBCVER >= #x0200))
(defconstant $SQL_FN_NUM_DEGREES			#x00040000L)
(defconstant $SQL_FN_NUM_LOG10			#x00080000L)
(defconstant $SQL_FN_NUM_POWER			#x00100000L)
(defconstant $SQL_FN_NUM_RADIANS			#x00200000L)
(defconstant $SQL_FN_NUM_ROUND			#x00400000L)
(defconstant $SQL_FN_NUM_TRUNCATE 		#x00800000L
;; #endif	;; ODBCVER >= #x0200

;; SQL_TIMEDATE_FUNCTIONS functions
)
(defconstant $SQL_FN_TD_NOW				#x00000001L)
(defconstant $SQL_FN_TD_CURDATE			#x00000002L)
(defconstant $SQL_FN_TD_DAYOFMONTH		#x00000004L)
(defconstant $SQL_FN_TD_DAYOFWEEK 		#x00000008L)
(defconstant $SQL_FN_TD_DAYOFYEAR 		#x00000010L)
(defconstant $SQL_FN_TD_MONTH 			#x00000020L)
(defconstant $SQL_FN_TD_QUARTER			#x00000040L)
(defconstant $SQL_FN_TD_WEEK				#x00000080L)
(defconstant $SQL_FN_TD_YEAR				#x00000100L)
(defconstant $SQL_FN_TD_CURTIME			#x00000200L)
(defconstant $SQL_FN_TD_HOUR				#x00000400L)
(defconstant $SQL_FN_TD_MINUTE			#x00000800L)
(defconstant $SQL_FN_TD_SECOND			#x00001000L
; #if (ODBCVER >= #x0200))
(defconstant $SQL_FN_TD_TIMESTAMPADD		#x00002000L)
(defconstant $SQL_FN_TD_TIMESTAMPDIFF 	#x00004000L)
(defconstant $SQL_FN_TD_DAYNAME			#x00008000L)
(defconstant $SQL_FN_TD_MONTHNAME 		#x00010000L
;; #endif	;; ODBCVER >= #x0200

;; SQL_SYSTEM_FUNCTIONS functions
)
(defconstant $SQL_FN_SYS_USERNAME 		#x00000001L)
(defconstant $SQL_FN_SYS_DBNAME			#x00000002L)
(defconstant $SQL_FN_SYS_IFNULL			#x00000004L

;; SQL_TIMEDATE_ADD_INTERVALS and SQL_TIMEDATE_DIFF_INTERVALS functions

; #if (ODBCVER >= #x0200))
(defconstant $SQL_FN_TSI_FRAC_SECOND		#x00000001L)
(defconstant $SQL_FN_TSI_SECOND			#x00000002L)
(defconstant $SQL_FN_TSI_MINUTE			#x00000004L)
(defconstant $SQL_FN_TSI_HOUR 			#x00000008L)
(defconstant $SQL_FN_TSI_DAY				#x00000010L)
(defconstant $SQL_FN_TSI_WEEK 			#x00000020L)
(defconstant $SQL_FN_TSI_MONTH			#x00000040L)
(defconstant $SQL_FN_TSI_QUARTER			#x00000080L)
(defconstant $SQL_FN_TSI_YEAR 			#x00000100L
;; #endif	;; ODBCVER >= #x0200

;; SQL_ODBC_API_CONFORMANCE values
)
(defconstant $SQL_OAC_NONE				#x0000)
(defconstant $SQL_OAC_LEVEL1				#x0001)
(defconstant $SQL_OAC_LEVEL2				#x0002

;; SQL_ODBC_SAG_CLI_CONFORMANCE values
)
(defconstant $SQL_OSCC_NOT_COMPLIANT		#x0000)
(defconstant $SQL_OSCC_COMPLIANT			#x0001

;; SQL_ODBC_SQL_CONFORMANCE values
)
(defconstant $SQL_OSC_MINIMUM 			#x0000)
(defconstant $SQL_OSC_CORE				#x0001)
(defconstant $SQL_OSC_EXTENDED			#x0002

;; SQL_CONCAT_NULL_BEHAVIOR values
)
(defconstant $SQL_CB_NULL 				#x0000)
(defconstant $SQL_CB_NON_NULL 			#x0001

;; SQL_CURSOR_COMMIT_BEHAVIOR and SQL_CURSOR_ROLLBACK_BEHAVIOR values
)
(defconstant $SQL_CB_DELETE				#x0000)
(defconstant $SQL_CB_CLOSE				#x0001)
(defconstant $SQL_CB_PRESERVE				#x0002

;; SQL_IDENTIFIER_CASE values
)
(defconstant $SQL_IC_UPPER				#x0001)
(defconstant $SQL_IC_LOWER				#x0002)
(defconstant $SQL_IC_SENSITIVE			#x0003)
(defconstant $SQL_IC_MIXED				#x0004

;; SQL_TXN_CAPABLE values
|#

(defconstant $SQL_TC_NONE 0)
(defconstant $SQL_TC_DML 1)
(defconstant $SQL_TC_ALL 2)

; #if (ODBCVER >= #x0200)
(defconstant $SQL_TC_DDL_COMMIT 3)
(defconstant $SQL_TC_DDL_IGNORE 4)
;; #endif	;; ODBCVER >= #x0200

;; SQL_SCROLL_OPTIONS masks


(defconstant $SQL_SO_FORWARD_ONLY #x00000001)
(defconstant $SQL_SO_KEYSET_DRIVEN #x00000002)
(defconstant $SQL_SO_DYNAMIC #x00000004)
(defconstant $SQL_SO_MIXED #x00000008)
; #if (ODBCVER >= #x0200))
(defconstant $SQL_SO_STATIC #x00000010)
;; #endif	;; ODBCVER >= #x0200

;; SQL_SCROLL_CONCURRENCY masks

(defconstant $SQL_SCCO_READ_ONLY #x00000001)
(defconstant $SQL_SCCO_LOCK #x00000002)
(defconstant $SQL_SCCO_OPT_ROWVER #x00000004)
(defconstant $SQL_SCCO_OPT_VALUES #x00000008)

;; SQL_FETCH_DIRECTION masks

(defconstant $SQL_FD_FETCH_NEXT #x00000001)
(defconstant $SQL_FD_FETCH_FIRST #x00000002)
(defconstant $SQL_FD_FETCH_LAST #x00000004)
(defconstant $SQL_FD_FETCH_PRIOR #x00000008)
(defconstant $SQL_FD_FETCH_ABSOLUTE #x00000010)
(defconstant $SQL_FD_FETCH_RELATIVE #x00000020)
(defconstant $SQL_FD_FETCH_RESUME #x00000040)
; #if (ODBCVER >= #x0200)
(defconstant $SQL_FD_FETCH_BOOKMARK #x00000080)
;; #endif	;; ODBCVER >= #x0200

#|
;; SQL_TXN_ISOLATION_OPTION masks
)
(defconstant $SQL_TXN_READ_UNCOMMITTED	#x00000001L)
(defconstant $SQL_TXN_READ_COMMITTED		#x00000002L)
(defconstant $SQL_TXN_REPEATABLE_READ 	#x00000004L)
(defconstant $SQL_TXN_SERIALIZABLE		#x00000008L)
(defconstant $SQL_TXN_VERSIONING			#x00000010L

;; SQL_CORRELATION_NAME values
)
(defconstant $SQL_CN_NONE 				#x0000)
(defconstant $SQL_CN_DIFFERENT			#x0001)
(defconstant $SQL_CN_ANY					#x0002

;; SQL_NON_NULLABLE_COLUMNS values
)
(defconstant $SQL_NNC_NULL			   	#x0000)
(defconstant $SQL_NNC_NON_NULL			#x0001

; #if (ODBCVER >= #x0200)
;; SQL_NULL_COLLATION values
									  )
(defconstant $SQL_NC_HIGH 				#x0000)
(defconstant $SQL_NC_LOW					#x0001)
(defconstant $SQL_NC_START				#x0002)
(defconstant $SQL_NC_END					#x0004

;; SQL_FILE_USAGE values
)
(defconstant $SQL_FILE_NOT_SUPPORTED		#x0000)
(defconstant $SQL_FILE_TABLE				#x0001)
(defconstant $SQL_FILE_QUALIFIER			#x0002

;; SQL_GETDATA_EXTENSIONS values
)
(defconstant $SQL_GD_ANY_COLUMN			#x00000001L)
(defconstant $SQL_GD_ANY_ORDER			#x00000002L)
(defconstant $SQL_GD_BLOCK				#x00000004L)
(defconstant $SQL_GD_BOUND				#x00000008L

;; SQL_ALTER_TABLE values
)
(defconstant $SQL_AT_ADD_COLUMN			#x00000001L)
(defconstant $SQL_AT_DROP_COLUMN			#x00000002L

;; SQL_POSITIONED_STATEMENTS masks
)
(defconstant $SQL_PS_POSITIONED_DELETE	#x00000001L)
(defconstant $SQL_PS_POSITIONED_UPDATE	#x00000002L)
(defconstant $SQL_PS_SELECT_FOR_UPDATE	#x00000004L

;; SQL_GROUP_BY values
)
(defconstant $SQL_GB_NOT_SUPPORTED			#x0000)
(defconstant $SQL_GB_GROUP_BY_EQUALS_SELECT	#x0001)
(defconstant $SQL_GB_GROUP_BY_CONTAINS_SELECT	#x0002)
(defconstant $SQL_GB_NO_RELATION				#x0003
													
;; SQL_OWNER_USAGE masks
)
(defconstant $SQL_OU_DML_STATEMENTS		#x00000001L)
(defconstant $SQL_OU_PROCEDURE_INVOCATION #x00000002L)
(defconstant $SQL_OU_TABLE_DEFINITION 	#x00000004L)
(defconstant $SQL_OU_INDEX_DEFINITION 	#x00000008L)
(defconstant $SQL_OU_PRIVILEGE_DEFINITION #x00000010L

;; SQL_QUALIFIER_USAGE masks
)
(defconstant $SQL_QU_DML_STATEMENTS		#x00000001L)
(defconstant $SQL_QU_PROCEDURE_INVOCATION #x00000002L)
(defconstant $SQL_QU_TABLE_DEFINITION 	#x00000004L)
(defconstant $SQL_QU_INDEX_DEFINITION 	#x00000008L)
(defconstant $SQL_QU_PRIVILEGE_DEFINITION #x00000010L

;; SQL_SUBQUERIES masks
)
(defconstant $SQL_SQ_COMPARISON				#x00000001L)
(defconstant $SQL_SQ_EXISTS					#x00000002L)
(defconstant $SQL_SQ_IN						#x00000004L)
(defconstant $SQL_SQ_QUANTIFIED				#x00000008L)
(defconstant $SQL_SQ_CORRELATED_SUBQUERIES	#x00000010L

;; SQL_UNION masks
)
(defconstant $SQL_U_UNION						#x00000001L)
(defconstant $SQL_U_UNION_ALL					#x00000002L

;; SQL_BOOKMARK_PERSISTENCE values
)
(defconstant $SQL_BP_CLOSE				#x00000001L)
(defconstant $SQL_BP_DELETE				#x00000002L)
(defconstant $SQL_BP_DROP 				#x00000004L)
(defconstant $SQL_BP_TRANSACTION			#x00000008L)
(defconstant $SQL_BP_UPDATE				#x00000010L)
(defconstant $SQL_BP_OTHER_HSTMT			#x00000020L)
(defconstant $SQL_BP_SCROLL				#x00000040L

;; SQL_STATIC_SENSITIVITY values
)
(defconstant $SQL_SS_ADDITIONS			#x00000001L)
(defconstant $SQL_SS_DELETIONS			#x00000002L)
(defconstant $SQL_SS_UPDATES				#x00000004L

;; SQL_LOCK_TYPESL masks
)
(defconstant $SQL_LCK_NO_CHANGE			#x00000001L)
(defconstant $SQL_LCK_EXCLUSIVE			#x00000002L)
(defconstant $SQL_LCK_UNLOCK				#x00000004L

;; SQL_POS_OPERATIONS masks
|#

(defconstant $SQL_POS_POSITION 1) ;; #x00000001L
(defconstant $SQL_POS_REFRESH 2)  ;; #x00000002L
(defconstant $SQL_POS_UPDATE 4)   ;; #x00000004L
(defconstant $SQL_POS_DELETE 8)   ;; #x00000008L
(defconstant $SQL_POS_ADD 16)     ;; #x00000010L

#|
;; SQL_QUALIFIER_LOCATION values
)
(defconstant $SQL_QL_START				#x0001L)
(defconstant $SQL_QL_END					#x0002L

;; SQL_OJ_CAPABILITIES values

; #if (ODBCVER >= #x0201))
(defconstant $SQL_OJ_LEFT					#x00000001L)
(defconstant $SQL_OJ_RIGHT				#x00000002L)
(defconstant $SQL_OJ_FULL					#x00000004L)
(defconstant $SQL_OJ_NESTED				#x00000008L)
(defconstant $SQL_OJ_NOT_ORDERED			#x00000010L)
(defconstant $SQL_OJ_INNER				#x00000020L)
(defconstant $SQL_OJ_ALL_COMPARISON_OPS	#x00000040L
;; #endif	;; ODBCVER >= #x0201
;; #endif	;; ODBCVER >= #x0200

;; options for SQLGetStmtOption/SQLSetStmtOption)
(defconstant $SQL_QUERY_TIMEOUT			0)
(defconstant $SQL_MAX_ROWS				1)
(defconstant $SQL_NOSCAN					2)
(defconstant $SQL_MAX_LENGTH				3)
(defconstant $SQL_ASYNC_ENABLE			4)
(defconstant $SQL_BIND_TYPE				5
; #if (ODBCVER >= #x0200))
(defconstant $SQL_CURSOR_TYPE 			6)
(defconstant $SQL_CONCURRENCY 			7)
(defconstant $SQL_KEYSET_SIZE 			8)
(defconstant $SQL_ROWSET_SIZE 			9)
(defconstant $SQL_SIMULATE_CURSOR 		10)
(defconstant $SQL_RETRIEVE_DATA			11)
(defconstant $SQL_USE_BOOKMARKS			12)
(defconstant $SQL_GET_BOOKMARK			13	/*	GetStmtOption Only)
(defconstant $SQL_ROW_NUMBER				14	/*	GetStmtOption Only)
(defconstant $SQL_STMT_OPT_MAX			SQL_ROW_NUMBER
;; #else)
(defconstant $SQL_STMT_OPT_MAX			SQL_BIND_TYPE
;; #endif	;; ODBCVER >= #x0200
)
(defconstant $SQL_STMT_OPT_MIN			SQL_QUERY_TIMEOUT


;; SQL_QUERY_TIMEOUT options)
(defconstant $SQL_QUERY_TIMEOUT_DEFAULT	0UL

;; SQL_MAX_ROWS options)
(defconstant $SQL_MAX_ROWS_DEFAULT		0UL

;; SQL_NOSCAN options)
(defconstant $SQL_NOSCAN_OFF				0UL	/*	1.0 FALSE)
(defconstant $SQL_NOSCAN_ON				1UL	/*	1.0 TRUE)
(defconstant $SQL_NOSCAN_DEFAULT			SQL_NOSCAN_OFF

;; SQL_MAX_LENGTH options)
(defconstant $SQL_MAX_LENGTH_DEFAULT		0UL

;; SQL_ASYNC_ENABLE options)
(defconstant $SQL_ASYNC_ENABLE_OFF		0UL)
(defconstant $SQL_ASYNC_ENABLE_ON			1UL)
(defconstant $SQL_ASYNC_ENABLE_DEFAULT	SQL_ASYNC_ENABLE_OFF

;; SQL_BIND_TYPE options)
(defconstant $SQL_BIND_BY_COLUMN			0UL)
(defconstant $SQL_BIND_TYPE_DEFAULT		SQL_BIND_BY_COLUMN		;; Default value

;; SQL_CONCURRENCY options)
(defconstant $SQL_CONCUR_READ_ONLY		1)
(defconstant $SQL_CONCUR_LOCK 			2)
(defconstant $SQL_CONCUR_ROWVER			3)
(defconstant $SQL_CONCUR_VALUES			4)
(defconstant $SQL_CONCUR_DEFAULT			SQL_CONCUR_READ_ONLY	;; Default value

; #if (ODBCVER >= #x0200)
;; SQL_CURSOR_TYPE options)
(defconstant $SQL_CURSOR_FORWARD_ONLY 	0UL)
(defconstant $SQL_CURSOR_KEYSET_DRIVEN	1UL)
(defconstant $SQL_CURSOR_DYNAMIC			2UL)
(defconstant $SQL_CURSOR_STATIC			3UL)
(defconstant $SQL_CURSOR_TYPE_DEFAULT		SQL_CURSOR_FORWARD_ONLY	;; Default value

;; SQL_ROWSET_SIZE options)
(defconstant $SQL_ROWSET_SIZE_DEFAULT 	1UL

;; SQL_KEYSET_SIZE options)
(defconstant $SQL_KEYSET_SIZE_DEFAULT		0UL

;; SQL_SIMULATE_CURSOR options)
(defconstant $SQL_SC_NON_UNIQUE			0UL)
(defconstant $SQL_SC_TRY_UNIQUE			1UL)
(defconstant $SQL_SC_UNIQUE				2UL

;; SQL_RETRIEVE_DATA options)
(defconstant $SQL_RD_OFF					0UL)
(defconstant $SQL_RD_ON					1UL)
(defconstant $SQL_RD_DEFAULT				SQL_RD_ON

;; SQL_USE_BOOKMARKS options)
(defconstant $SQL_UB_OFF					0UL)
(defconstant $SQL_UB_ON					1UL)
(defconstant $SQL_UB_DEFAULT				SQL_UB_OFF

;; #endif	;; ODBCVER >= #x0200
|#

;; options for SQLSetConnectOption/SQLGetConnectOption)
(defconstant $SQL_ACCESS_MODE 101)
(defconstant $SQL_AUTOCOMMIT 102)
(defconstant $SQL_LOGIN_TIMEOUT 103)
(defconstant $SQL_OPT_TRACE 104)
(defconstant $SQL_OPT_TRACEFILE 105)
(defconstant $SQL_TRANSLATE_DLL 106)
(defconstant $SQL_TRANSLATE_OPTION 107)
(defconstant $SQL_TXN_ISOLATION 108) 
(defconstant $SQL_CURRENT_QUALIFIER 109)
;;#if (ODBCVER >= #x0200))
(defconstant $SQL_ODBC_CURSORS 110)
(defconstant $SQL_QUIET_MODE 111)
(defconstant $SQL_PACKET_SIZE 112)
(defconstant $SQL_CONN_OPT_MAX $SQL_PACKET_SIZE)
;; #else
;(defconstant $SQL_CONN_OPT_MAX $SQL_CURRENT_QUALIFIER)
;; #endif	;; ODBCVER >= #x0200)
(defconstant $SQL_CONNECT_OPT_DRVR_START 1000)

;;#define	SQL_CONN_OPT_MIN			SQL_ACCESS_MODE

;; SQL_ACCESS_MODE options
(defconstant $SQL_MODE_READ_WRITE 0) ; 0UL
(defconstant $SQL_MODE_READ_ONLY 1)  ; 1UL
(defconstant $SQL_MODE_DEFAULT $SQL_MODE_READ_WRITE)

;; SQL_AUTOCOMMIT options)
(defconstant $SQL_AUTOCOMMIT_OFF 0) ;0UL
(defconstant $SQL_AUTOCOMMIT_ON 1) ;1UL
(defconstant $SQL_AUTOCOMMIT_DEFAULT $SQL_AUTOCOMMIT_ON)

;; SQL_LOGIN_TIMEOUT options)
(defconstant $SQL_LOGIN_TIMEOUT_DEFAULT	15) ; 15UL

;; SQL_OPT_TRACE options)
(defconstant $SQL_OPT_TRACE_OFF 0) ; 0UL
(defconstant $SQL_OPT_TRACE_ON 1) ; 1UL
(defconstant $SQL_OPT_TRACE_DEFAULT $SQL_OPT_TRACE_OFF)
; #ifndef SQL_OPT_TRACE_FILE_DEFAULT
; (defconstant $SQL_OPT_TRACE_FILE_DEFAULT	"\\SQL.LOG"
;; #endif

; #if (ODBCVER >= #x0200)
;; SQL_ODBC_CURSORS options)
(defconstant $SQL_CUR_USE_IF_NEEDED 0) ; 0UL
(defconstant $SQL_CUR_USE_ODBC 1) ; 1UL
(defconstant $SQL_CUR_USE_DRIVER 2) ; 2UL
(defconstant $SQL_CUR_DEFAULT $SQL_CUR_USE_DRIVER)
;; #endif	;; ODBCVER >= #x0200

#|
;; Column types and scopes in SQLSpecialColumns. )
(defconstant $SQL_BEST_ROWID 1)
(defconstant $SQL_ROWVER 2)
)
(defconstant $SQL_SCOPE_CURROW			0)
(defconstant $SQL_SCOPE_TRANSACTION		1)
(defconstant $SQL_SCOPE_SESSION			2

;; Defines for SQLSetPos)
(defconstant $SQL_ENTIRE_ROWSET			0
|#

;; Operations in SQLSetPos

(defconstant $SQL_POSITION 0) ;; 1.0 FALSE
(defconstant $SQL_REFRESH 1)  ;; 1.0 TRUE
; #if (ODBCVER >= #x0200))
(defconstant $SQL_UPDATE 2)
(defconstant $SQL_DELETE 3)
(defconstant $SQL_ADD 4)
;; #endif	;; ODBCVER >= #x0200

;; Lock options in SQLSetPos)
(defconstant $SQL_LOCK_NO_CHANGE 0) ;; 1.0 FALSE
(defconstant $SQL_LOCK_EXCLUSIVE 1) ;; 1.0 TRUE
; #if (ODBCVER >= #x0200)
(defconstant $SQL_LOCK_UNLOCK 2)

;; SQLBindParameter extensions
; #if (ODBCVER >= #x0200)
(defconstant $SQL_DEFAULT_PARAM	-5)
(defconstant $SQL_IGNORE -6)
(defconstant $SQL_LEN_DATA_AT_EXEC_OFFSET -100)
;(defconstant $SQL_LEN_DATA_AT_EXEC(length) (-length+SQL_LEN_DATA_AT_EXEC_OFFSET)
; #endif	/* ODBCVER >= #x0200 */

;; Special return values for SQLGetData
(defconstant $SQL_NO_TOTAL -4)

#|
;; Macros for SQLSetPos)
(defconstant $SQL_POSITION_TO(hstmt,irow) SQLSetPos(hstmt,irow,SQL_POSITION,SQL_LOCK_NO_CHANGE))
(defconstant $SQL_LOCK_RECORD(hstmt,irow,fLock) SQLSetPos(hstmt,irow,SQL_POSITION,fLock))
(defconstant $SQL_REFRESH_RECORD(hstmt,irow,fLock) SQLSetPos(hstmt,irow,SQL_REFRESH,fLock))
(defconstant $SQL_UPDATE_RECORD(hstmt,irow) SQLSetPos(hstmt,irow,SQL_UPDATE,SQL_LOCK_NO_CHANGE))
(defconstant $SQL_DELETE_RECORD(hstmt,irow) SQLSetPos(hstmt,irow,SQL_DELETE,SQL_LOCK_NO_CHANGE))
(defconstant $SQL_ADD_RECORD(hstmt,irow) SQLSetPos(hstmt,irow,SQL_ADD,SQL_LOCK_NO_CHANGE)
;; #endif	;; ODBCVER >= #x0200

; #ifndef RC_INVOKED

; #if (ODBCVER >= #x0200)
/*	This define is too large for RC)
(defconstant $SQL_ODBC_KEYWORDS \
"ABSOLUTE,ACTION,ADA,ADD,ALL,ALLOCATE,ALTER,AND,ANY,ARE,AS,"\
"ASC,ASSERTION,AT,AUTHORIZATION,AVG,"\
"BEGIN,BETWEEN,BIT,BIT_LENGTH,BOTH,BY,CASCADE,CASCADED,CASE,CAST,CATALOG,"\
"CHAR,CHAR_LENGTH,CHARACTER,CHARACTER_LENGTH,CHECK,CLOSE,COALESCE,"\
"COBOL,COLLATE,COLLATION,COLUMN,COMMIT,CONNECT,CONNECTION,CONSTRAINT,"\
"CONSTRAINTS,CONTINUE,CONVERT,CORRESPONDING,COUNT,CREATE,CROSS,CURRENT,"\
"CURRENT_DATE,CURRENT_TIME,CURRENT_TIMESTAMP,CURRENT_USER,CURSOR,"\
"DATE,DAY,DEALLOCATE,DEC,DECIMAL,DECLARE,DEFAULT,DEFERRABLE,"\
"DEFERRED,DELETE,DESC,DESCRIBE,DESCRIPTOR,DIAGNOSTICS,DISCONNECT,"\
"DISTINCT,DOMAIN,DOUBLE,DROP,"\
"ELSE,END,END-EXEC,ESCAPE,EXCEPT,EXCEPTION,EXEC,EXECUTE,"\
"EXISTS,EXTERNAL,EXTRACT,"\
"FALSE,FETCH,FIRST,FLOAT,FOR,FOREIGN,FORTRAN,FOUND,FROM,FULL,"\
"GET,GLOBAL,GO,GOTO,GRANT,GROUP,HAVING,HOUR,"\
"IDENTITY,IMMEDIATE,IN,INCLUDE,INDEX,INDICATOR,INITIALLY,INNER,"\
"INPUT,INSENSITIVE,INSERT,INTEGER,INTERSECT,INTERVAL,INTO,IS,ISOLATION,"\
"JOIN,KEY,LANGUAGE,LAST,LEADING,LEFT,LEVEL,LIKE,LOCAL,LOWER,"\
"MATCH,MAX,MIN,MINUTE,MODULE,MONTH,MUMPS,"\
"NAMES,NATIONAL,NATURAL,NCHAR,NEXT,NO,NONE,NOT,NULL,NULLIF,NUMERIC,"\
"OCTET_LENGTH,OF,ON,ONLY,OPEN,OPTION,OR,ORDER,OUTER,OUTPUT,OVERLAPS,"\
"PAD,PARTIAL,PASCAL,PLI,POSITION,PRECISION,PREPARE,PRESERVE,"\
"PRIMARY,PRIOR,PRIVILEGES,PROCEDURE,PUBLIC,"\
"REFERENCES,RELATIVE,RESTRICT,REVOKE,RIGHT,ROLLBACK,ROWS,"\
"SCHEMA,SCROLL,SECOND,SECTION,SELECT,SEQUENCE,SESSION,SESSION_USER,SET,SIZE,"\
"SMALLINT,SOME,SPACE,SQL,SQLCA,SQLCODE,SQLERROR,SQLSTATE,SQLWARNING,"\
"SUBSTRING,SUM,SYSTEM_USER,"\
"TABLE,TEMPORARY,THEN,TIME,TIMESTAMP,TIMEZONE_HOUR,TIMEZONE_MINUTE,"\
"TO,TRAILING,TRANSACTION,TRANSLATE,TRANSLATION,TRIM,TRUE,"\
"UNION,UNIQUE,UNKNOWN,UPDATE,UPPER,USAGE,USER,USING,"\
"VALUE,VALUES,VARCHAR,VARYING,VIEW,WHEN,WHENEVER,WHERE,WITH,WORK,YEAR")
;; #endif	;; ODBCVER >= #x0200
|#

(defconstant $SQL_PARAM_TYPE_UNKNOWN 0)
(defconstant $SQL_PARAM_INPUT 1)
(defconstant $SQL_PARAM_INPUT_OUTPUT 2)
(defconstant $SQL_RESULT_COL 3)
;;#if (ODBCVER >= #x0200)
(defconstant $SQL_PARAM_OUTPUT 4)
(defconstant $SQL_RETURN_VALUE 5)


;; Defines used by both Level 1 and Level 2 functions

;; generally useful constants
(defconstant $SQL_MAX_OPTION_STRING_LENGTH 256)

;; Additional return codes)
(defconstant $SQL_STILL_EXECUTING 2)
(defconstant $SQL_NEED_DATA 99)

;; SQL extended datatypes)
(defconstant $SQL_DATE 9)
(defconstant $SQL_TIME 10)
(defconstant $SQL_TIMESTAMP 11)
(defconstant $SQL_LONGVARCHAR -1)
(defconstant $SQL_BINARY -2)
(defconstant $SQL_VARBINARY -3)
(defconstant $SQL_LONGVARBINARY -4)
(defconstant $SQL_BIGINT -5)
(defconstant $SQL_TINYINT -6)
(defconstant $SQL_BIT -7)

(defconstant $SQL_INTERVAL_YEAR -80)
(defconstant $SQL_INTERVAL_MONTH -81)
(defconstant $SQL_INTERVAL_YEAR_TO_MONTH -82)
(defconstant $SQL_INTERVAL_DAY -83)
(defconstant $SQL_INTERVAL_HOUR -84)
(defconstant $SQL_INTERVAL_MINUTE -85)
(defconstant $SQL_INTERVAL_SECOND -86)
(defconstant $SQL_INTERVAL_DAY_TO_HOUR -87)
(defconstant $SQL_INTERVAL_DAY_TO_MINUTE -88)
(defconstant $SQL_INTERVAL_DAY_TO_SECOND -89)
(defconstant $SQL_INTERVAL_HOUR_TO_MINUTE -90)
(defconstant $SQL_INTERVAL_HOUR_TO_SECOND -91)
(defconstant $SQL_INTERVAL_MINUTE_TO_SECOND -92)
(defconstant $SQL_UNICODE -95)
(defconstant $SQL_TYPE_DRIVER_START $SQL_INTERVAL_YEAR)
(defconstant $SQL_TYPE_DRIVER_END $SQL_UNICODE)


;;#if (ODBCVER >= #x0200))
(defconstant $SQL_SIGNED_OFFSET	-20)
(defconstant $SQL_UNSIGNED_OFFSET -22)
;;; #endif	;; ODBCVER >= #x0200

;; C datatype to SQL datatype mapping
(defconstant $SQL_C_DATE $SQL_DATE)
(defconstant $SQL_C_TIME $SQL_TIME)
(defconstant $SQL_C_TIMESTAMP $SQL_TIMESTAMP)
(defconstant $SQL_C_BINARY $SQL_BINARY)
(defconstant $SQL_C_BIT $SQL_BIT)
(defconstant $SQL_C_TINYINT $SQL_TINYINT)
;;#if (ODBCVER >= #x0200))
(defconstant $SQL_C_SLONG (+ $SQL_C_LONG $SQL_SIGNED_OFFSET)) ;; SIGNED INTEGER
(defconstant $SQL_C_SSHORT (+ $SQL_C_SHORT $SQL_SIGNED_OFFSET)) ;; SIGNED SMALLINT
(defconstant $SQL_C_STINYINT (+ $SQL_TINYINT $SQL_SIGNED_OFFSET)) ;; SIGNED TINYINT
(defconstant $SQL_C_ULONG (+ $SQL_C_LONG $SQL_UNSIGNED_OFFSET)) ;; UNSIGNED INTEGER
(defconstant $SQL_C_USHORT (+ $SQL_C_SHORT $SQL_UNSIGNED_OFFSET)) ;; UNSIGNED SMALLINT
(defconstant $SQL_C_UTINYINT (+ $SQL_TINYINT $SQL_UNSIGNED_OFFSET)) ;;UNSIGNED TINYINT
(defconstant $SQL_C_BOOKMARK $SQL_C_ULONG) ;; BOOKMARK
;; #endif	;; ODBCVER >= #x0200

;; Options for SQLDriverConnect
(defconstant $SQL_DRIVER_NOPROMPT 0)
(defconstant $SQL_DRIVER_COMPLETE 1)
(defconstant $SQL_DRIVER_PROMPT 2)
(defconstant $SQL_DRIVER_COMPLETE_REQUIRED 3)

;; Level 2 Functions

;; SQLExtendedFetch "fFetchType" values
(defconstant $SQL_FETCH_NEXT 1)
(defconstant $SQL_FETCH_FIRST 2)
(defconstant $SQL_FETCH_LAST 3)
(defconstant $SQL_FETCH_PRIOR 4)
(defconstant $SQL_FETCH_ABSOLUTE 5)
(defconstant $SQL_FETCH_RELATIVE 6)
;#if (ODBCVER >= #x0200)
(defconstant $SQL_FETCH_BOOKMARK 8)
;#endif	/* ODBCVER >= #x0200 */

#|
/* SQLExtendedFetch "rgfRowStatus" element values */
(defconstant $SQL_ROW_SUCCESS 		0
(defconstant $SQL_ROW_DELETED 		1
(defconstant $SQL_ROW_UPDATED 		2
(defconstant $SQL_ROW_NOROW			3
#if (ODBCVER >= #x0200)
(defconstant $SQL_ROW_ADDED			4
(defconstant $SQL_ROW_ERROR			5
#endif	/* ODBCVER >= #x0200 */

/* Defines for SQLForeignKeys (returned in result set) */
(defconstant $SQL_CASCADE 			0
(defconstant $SQL_RESTRICT			1
(defconstant $SQL_SET_NULL			2

/* Defines for SQLBindParameter and
			   SQLProcedureColumns (returned in the result set) */
(defconstant $SQL_PARAM_TYPE_UNKNOWN  0
(defconstant $SQL_PARAM_INPUT         1
(defconstant $SQL_PARAM_INPUT_OUTPUT  2
(defconstant $SQL_RESULT_COL          3
#if (ODBCVER >= #x0200)
(defconstant $SQL_PARAM_OUTPUT		4
(defconstant $SQL_RETURN_VALUE		5
#endif	/* ODBCVER >= #x0200 */

/* Defines used by Driver Manager when mapping SQLSetParam to SQLBindParameter */
(defconstant $SQL_PARAM_TYPE_DEFAULT	$SQL_PARAM_INPUT_OUTPUT)
(defconstant $SQL_SETPARAM_VALUE_MAX	-1)

/* Defines for SQLStatistics */
(defconstant $SQL_INDEX_UNIQUE		0
(defconstant $SQL_INDEX_ALL			1

(defconstant $SQL_QUICK				0
(defconstant $SQL_ENSURE				1

/* Defines for SQLStatistics (returned in the result set) */
(defconstant $SQL_TABLE_STAT			0
(defconstant $SQL_INDEX_CLUSTERED 	1
(defconstant $SQL_INDEX_HASHED		2
(defconstant $SQL_INDEX_OTHER 		3

#if (ODBCVER >= #x0200)
/* Defines for SQLProcedures (returned in the result set) */
(defconstant $SQL_PT_UNKNOWN			0
(defconstant $SQL_PT_PROCEDURE		1
(defconstant $SQL_PT_FUNCTION 		2

/* Defines for SQLSpecialColumns (returned in the result set) */
(defconstant $SQL_PC_UNKNOWN			0
(defconstant $SQL_PC_NOT_PSEUDO		1
(defconstant $SQL_PC_PSEUDO			2
#endif	/* ODBCVER >= #x0200 */
|#