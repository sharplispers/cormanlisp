;;;-*- Mode: Lisp; Package: SQL -*-

;; SQL module for MCL, LWW and ACL/Windows
;; Version 0.8
;; Copyright (C) Paul Meurer 1999. All rights reserved.
;; paul.meurer@hit.uib.no
;;

;;; "This software is based on the design of the ODBC interface provided by 
;;;  Harlequin Group plc in the LispWorks and LispWorks for the Windows 
;;;  Operating System products. This implementation is not the property of
;;;  Harlequin and they have no responsibility for its content or accuracy."

;; Documentation and the license agreement can be found in file 
;; "sql-odbc-documentation.lisp" 

(in-package :sql)

(defclass database ()
  ((db-name :initform "" :initarg :db-name :accessor db-name)
   (user-id :initform "" :initarg :user-id :accessor db-user-id)
   (password :initform "" :initarg :password :accessor db-password)
   (type :initform nil :initarg :db-type :reader db-type)
   (connected-p :initform nil :accessor db-connected-p)
   ;; not used yet
   (count :initform 0 :initarg :count :accessor db-count)
   ;; not used yet
   (total-count :initform 0 :allocation :class :accessor db-total-count)
   ;; the use of this slot is deprecated; it will be removed when dtf works without it.
   (query :initform nil :accessor db-query-object)
   ;; resource of (active and inactive) query objects
   (queries :initform () :accessor db-queries))
  ;; #+cormanlisp (:metaclass cl::class-slot-class)	
  (:documentation
   "Class used to store connection information. 
Several database objects can coexist, and are listed in *connected-databases*.
The API packages (ODBC, DTF, or others) should subclass this class.
There is a *default-database*, so you don't have always to mention the
database you want to work with." ))

(defmethod print-object ((db database) stream)
  (print-unreadable-object (db stream :identity t)
    (format stream "~a ~s"
            (class-name (class-of db)) 
            (db-name db))))

;; mixin classes for specific databases.
;; those are not subclasses of odbc-database because they are not 
;; necessarily accessed via odbc.
(defclass db-mixin ()
  ;; plist stores SQL-specific anomalies
  ((plist :initform () :reader db-plist)))

(defmethod db-plist ((db database)) (declare (ignore db)) ())

(defclass oracle-mixin (db-mixin) ())
#-mcl
(defclass access-mixin (db-mixin) ())
#-mcl
(defclass solid-mixin (db-mixin) ())
(defclass postgres-mixin (db-mixin) ())
#-mcl
(defclass mysql-mixin (db-mixin)
  ((plist :initform '(:insert-string "replace into "))))

(defclass query ()
  (;; a query string or a query expression object
   (sql-expression :initform nil :initarg :sql-expression :accessor sql-expression)
   ;; database object the query is to be run against
   (database :initarg :database :reader query-database)
   (active-p :initform nil :initarg :active-p :accessor query-active-p))
  (:documentation
   "Stores query information, like SQL query string/expression and database to run
the query against." ))

(defvar *default-database* nil
  "Default database object.")
