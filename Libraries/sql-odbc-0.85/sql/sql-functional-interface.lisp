;;;-*- Mode: Lisp; Package: SQL -*-

;; Portable SQL module
;; Version 0.85
;; Copyright (C) Paul Meurer 1999. All rights reserved.
;; paul.meurer@hit.uib.no

;;; "This software is based on the design of the ODBC interface provided by 
;;;  Harlequin Group plc in the LispWorks and LispWorks for the Windows 
;;;  Operating System products. This implementation is not the property of
;;;  Harlequin and they have no responsibility for its content or accuracy."

;; Documentation and the license agreement can be found in file 
;; "sql-odbc-documentation.lisp".
;; Bug reports and suggestions are highly welcome.


(in-package :sql)

(defparameter *default-database-type* :odbc)

(defun initialize-database-type (&optional (database-type *default-database-type*)
                                               (set-default t))
  (when set-default (setf *default-database-type* database-type))
  (ecase database-type
    (:odbc
     #+mcl(um:use-module :odbc "sql:odbc;odbc-system") ;; without own defsystem
     #-mcl(require :odbc "sql:odbc;odbc-system") ;; with defsystem, don't need um
     (use-package :odbc))
    #+mcl
    (:dtf ;; only defined for mcl
     (um:use-module :dtf "sql:dtf;dtf")
     (use-package :dtf)))
  database-type)

;; new 0.85
(defmacro with-database ((database) &body body)
  "Binds *DEFAULT-DATABASE* to DATABASE in the scope of BODY"
  `(let ((*default-database* ,database))
     ,@body))

;; one of :new :warn-new :error :warn-old :old
(defparameter *connect-if-exists* :old)

;; should this be something different from a list?
(defvar *connected-databases* () "A list of connected databases.")

#+mcl
(defmethod initialize-instance ((query query) &key &allow-other-keys)
  (call-next-method)
  (terminate-when-unreachable query))

(defun disconnect-all-databases ()
  (dolist (database *connected-databases*)
    (disconnect :database database)))

#+mcl
(pushnew #'disconnect-all-databases *lisp-cleanup-functions*)

;; Support for database types is added by defining methods for the following
;; generic functions (there is no need to change the sql module, besides 
;; changing initialize-database-type):

(defgeneric db-connect (db-or-type db-name user-id password autocommit))
(defgeneric db-disconnect (database))
(defgeneric db-begin-transaction (database)) ;; not needed for odbc?
(defgeneric db-commit (database))
(defgeneric db-rollback (database))
(defgeneric db-query (database query-expression &optional flatp))
(defgeneric db-open-query (database query-expression &key &allow-other-keys))
(defgeneric db-close-query (database))
(defgeneric db-cancel-query (database))
(defgeneric db-fetch-query-results (database &optional count flatp))
(defgeneric db-execute-command (database sql-expression))
;; those two could well be one function
(defgeneric db-execute-parameterized (query sql-expression &rest parameters))
;(defgeneric db-query-parameterized (query sql-expression &rest parameters))
(defgeneric %db-execute (database sql-expression &key &allow-other-keys))
(defgeneric %read-query-data (database ignore-columns))
(defgeneric %initialize-query (database &optional arglen col-positions))
(defgeneric %db-bind-execute (query &rest parameters))
(defgeneric %db-reset-query (query))
(defgeneric db-map-query (database type function query-expression))
(defgeneric db-map-bind-query (query type function &rest parameters))
(defgeneric db-prepare-statement (database sql-expression &key &allow-other-keys))
(defgeneric db-describe-columns (database table-qualifier table-owner 
                                              table-name column-name))
(defgeneric db-data-sources (db-type))
(defgeneric column-names (query)) ;; returns a vector

;(defgeneric close-query (query))

;; dummy method? don't remember
(defmethod db-connect ((db-type (eql :rest)) db-name user-id password autocommit)
  (declare (ignore db-name password autocommit))
  (values user-id db-type))

;; dummy for odbc
(defmethod db-begin-transaction ((database database))
  nil)

(defun connect (db-name &key (user-id "") (password "")
                          (if-exists *connect-if-exists*) 
                          (db-type *default-database-type*)
                          (make-default t)
                          autocommit
                          &allow-other-keys)
  "Connects to a SQL database. Arguments:
 DB-NAME : Name of database to be used in connection string.
 USER-ID : Username
 PASSWORD : Password
 IF-EXISTS : Default: *connect-if-exists*.
  One of 
  :NEW : A new connection is created regardless of prior connections existing. 
  :WARN-NEW : Like :new, but a warning is issued. 
  :OLD : If an old connection exists, it is used, otherwise a new one is created. 
  :WARN-OLD : Like :OLD, but a warning is issued if an old connection is used. 
  :ERROR : If a connection already exists, an error is signalled.
 DB-TYPE : A database type (e.g., :odbc). Default: *default-database-type*.
 AUTOCOMMIT : Whether to open connection in autocommit mode. Default: nil.
 MAKE-DEFAULT : If non-nil, *default-database* is set to the database object.
Returns the database object."
  (let ((db ;; check if connection of requested type already exists
         (dolist (db *connected-databases*)
           (when (and (eq db-type (db-type db))
                      (string= db-name (db-name db))
                      (string= user-id (db-user-id db)))
             (return db)))))
    (cond ((or (not db) 
               (eq if-exists :new)
               (eq if-exists :warn-new))
           (when (and db (eq if-exists :warn-new))
             (warn "A connection to '~a/~a' already exists." db-name user-id))
           (setf db (db-connect db-type db-name user-id password autocommit))
           (setf (db-connected-p db) t)
           (when make-default (setf *default-database* db))
           (push db *connected-databases*))
          ((eq if-exists :error)
           (error "A connection to '~a/~a' already exists." db-name user-id))
          ((or (eq if-exists :old)
               (eq if-exists :warn-old)) 
           (when (eq if-exists :warn-old)
             (warn "Using existing connection to '~a/~a'." db-name user-id))
           (unless (db-connected-p db)
             (db-connect db db-name user-id password autocommit))
           (when make-default (setf *default-database* db))))
    db))

(defun disconnect (&key (database *default-database*) (rollback t))
  "Disconnects from a database.
If rollback is t (the default), rolls back uncommited transactions."
  (when rollback (db-rollback database))
  (db-disconnect database))

(defun commit (&key (database *default-database*))
  "Commits pending transactions."
  (db-commit database))

(defun rollback (&key (database *default-database*))
  "Rolls back pending transactions."
  (db-rollback database))

(defmethod execute-query ((query-string string) &key (database *default-database*) flatp)
  "Executes a SQL query string and fetches all rows."
  (db-query database query-string flatp))

(defmethod execute-query ((query-exp sql-query-expression) &key (database *default-database*) flatp)
  "Executes a SQL query expression and fetches all rows."
  (db-query database (sql-string query-exp) flatp))

(defgeneric execute-command (sql &key database)
  (:documentation "Executes a SQL command (string or expression)."))

(defmethod execute-command ((sql-string string)
                               &key (database *default-database*))
  (db-execute-command database sql-string))

(defmethod execute-command ((sql-expression sql-command-expression)  
                               &key (database *default-database*))
  (db-execute-command database (sql-string sql-expression)))

;; obsolete?
(defparameter *default-query* nil "Default query object.")

(defun open-query (query-expression &key (database *default-database*))
  "Opens a new query and executes query-expression, without closing open queries.
Returns the query object and makes it the default query"
  (when *default-query* ;; should be cerror? 
    (error "There is already a default query."))
  (setf *default-query*
        (db-open-query database (sql-string query-expression))))

(defun close-query (&optional (query *default-query*))
  "Closes a query."
  (db-close-query query)
  (when (eq query *default-query*)
    (setf *default-query* nil)))

;; I am not quite sure how useful this is. I have tried to cancel a query from a
;; different thread, but this does not seem to work. On the other hand, Command-.
;; (in MCL) does cancel a query (and obviously does not use cancel-query).
;; How does this work? Idea: look into SQLSetConnectOption.
(defun cancel-query (&optional (query-or-database
                                  (or *default-query* *default-database*)))
  "Cancels a query."
  (db-cancel-query query-or-database)
  (setf *default-query* nil))

;; has to be reworked?
(defun fetch-query-results (&key count (database-or-query 
                                            (or *default-query* *default-database*)))
  "Fetches the first <count> results (or all, if count is nil, the default).
The default for database-or-query is (or *default-query* *default-database*)."
  (db-fetch-query-results database-or-query count))

(defun begin-transaction (&key (database *default-database*))
  "Starts a transaction"
  (db-begin-transaction database))

;; a dynamic variable bound by with-transaction containing the transactions
;; to be committed inside a with-transaction form. It is only used inside 
;; a with-transaction form.
(defvar *active-transactions* t "A list of active transactions")
(defvar *inside-transaction-p* nil "True if we are inside a with-transaction form")

(defparameter *commit-nested-transactions-p* nil
  "If nil, only the outermost with-transaction form does commit on exit.")

;; The problem here is that one may start transactions with various connections,
;; and all of them should be committed/rolled back at exit of the form.
;; We solve it by using *active-transactions*. This might be a negligeable problem,
;; but difficult to tract when it occurs.
;; In addition, we have to catch errors in one db-commit/db-rollback and go on with
;; the next one, then report any error when everything is done. This is overkill.
;; Is this thread-safe?
;; In nested transactions with the same query, transactions  are (optionally)
;; only committed on exit from the outermost with-transaction form.
#+mcl
(defmacro with-transaction (&body body)
  "If the forms in body execute without error, all active transactions are 
committed on exit. If an error is occurs, the active transactions are rolled back.
If *commit-nested-transactions-p* is true, transaction are committed/rolled back
on exit from the outermost with-transaction form only."
  (let ((successp (gensym)))
    `(if *inside-transaction-p*
       (progn ,@body)
       (let ((,successp nil)
             (*active-transactions* ())
             (*inside-transaction-p* (not *commit-nested-transactions-p*)))
         (db-begin-transaction db)
         (unwind-protect
           (multiple-value-prog1 ; last form in body might return more/less than one value
             (progn ,@body)
             (setf ,successp t))
           (let ((errors ()))
             ;; to do: commit only transactions pertaining to the databases used
             ;; in the body??
             (dolist (db *active-transactions*)
               (multiple-value-bind (res error)
                                    (ignore-errors 
                                     (if ,successp 
                                       (db-commit db)             
                                       (db-rollback db)))
                 (declare (ignore res))
                 (when error (push error errors))))
             (when errors
               (apply 
                #'error
                (reduce #'(lambda (s error) 
                            (concatenate 
                             'string s 
                             (slot-value error 'ccl::format-string) "; "))
                        errors :initial-value "")
                (reduce #'(lambda (fa error) 
                            (append
                             fa (slot-value error 'ccl::format-arguments)))
                        errors :initial-value ())))))))))

#-mcl
(defmacro with-transaction (&body body) ;; simpler version
  "If the forms in body execute without error, all active transactions are 
committed on exit. If an error is occurs, the active transactions are rolled back.
If *commit-nested-transactions-p* is true, transaction are committed/rolled back
on exit from the outermost with-transaction form only."
  (let ((successp (gensym)))
   `(if *inside-transaction-p*
       (progn ,@body)
       (let ((,successp nil)
             (*active-transactions* ())
             (*inside-transaction-p* (not *commit-nested-transactions-p*)))
         (db-begin-transaction *default-database*)
         (unwind-protect
           (multiple-value-prog1 ; last form in body might return more/less than one value
             (progn ,@body)
             (setf ,successp t))
           (dolist (db *active-transactions*)
             ;(db-commit db)
             (if ,successp 
               (db-commit db)             
               (db-rollback db))))))))

;;; prepared statements

;; works for insert and select statements
(defun prepare-statement (sql-expression &key (database *default-database*) 
                                            parameter-columns parameter-table)
  "Prepares a statement for repeated execution and returns a query object. 
Use $ instead of SQL ? as parameter marker. 
Arguments:
 SQL-EXPRESSION : SQL statement to be prepared (Works for now for insert and select).
 PARAMETER-COLUMNS : A list of the parameter columns; only necessary if not all 
are parameterized and if sql-expression is a string. (Otherwise it is possible
to deduce their types and names from the sql-expression.)
 PARAMETER-TABLE : Name of the table; only necessary if sql-expression is a string."
  (db-prepare-statement database sql-expression
                        :parameter-table parameter-table
                        :parameter-columns parameter-columns))

(defmacro with-prepared-statement ((query sql-expression &key (database '*default-database*))
                                       &body body)
  "Prepares a (parameterized) statement (insert and select works by now) for repeated
 execution. A new query object is created and bound to the symbol QUERY. 
The function BIND-EXECUTE, to be used inside this macro, binds values to the
parameters and executes the query."
  `(let ((,query (prepare-statement ,sql-expression :database ,database)))
     (unwind-protect
       (progn ,@body)
       (close-query ,query))))

(defun bind-execute (query &rest parameters)
"Binds values to the parameters of a prepared statement and executes the command.
If the statement execution returns a result set, the result is fetched."
  (apply #'db-execute-parameterized query (sql-expression query) parameters))

;; should be about *null* ?
(defconstant +null+ nil 
  "Lisp representation of SQL Null value, default = nil.
May be bound locally to something else if a certain type is necessary.")

(defun select (&rest expressions)
  "Executes a select statement and returns a list of fetched result rows.
Signature: (&rest rows &key database from where group-by having order-by distinct
using set-operation all joins start-with connect-by flatp)
JOINS, USING and SET-OPERATION are not implemented yet" 
  (let* ((database (or (cadr (member :database expressions)) *default-database*))
         (query-exp (apply #'make-select-expression database expressions))
         (flatp (cadr (member :flatp expressions))))
    (execute-query query-exp :database database :flatp flatp)))

(defun select-union (&rest expressions)
  "Executes a union of select statement and returns a list of fetched result rows.
Signature: (&rest select-expressions &key database order-by flatp)"
  (let ((database (or (cadr (member :database expressions)) *default-database*))
        (query-exp (apply #'make-union-expression 'union expressions))
        (flatp (cadr (member :flatp expressions))))
    (execute-query (build-sql-string query-exp)
                   :database database :flatp flatp)))

(defun insert-records (&rest rest &key into attributes values av-pairs
                                query-expression (database *default-database*)
                                &allow-other-keys)
  "Inserts a record into a table."
  (let ((insert-exp 
         (make-insert-expression database 
          into attributes values av-pairs query-expression rest)))
    (execute-command
     (sql-string insert-exp)
     :database database)))

(defun update-records (table &rest rest &key attributes values av-pairs
                                where (database *default-database*)
                                &allow-other-keys)
  "Updates a record."
  (let ((update-exp 
         (make-update-expression database 
          table attributes values av-pairs where rest)))
    (execute-command
     (sql-string update-exp)
     :database database)))

(defun delete-records (&rest rest &key from where (database *default-database*)
                                &allow-other-keys)
  "Deletes a record."
  (let ((delete-exp 
         (make-delete-expression database 
          from where rest)))
    (execute-command
     (sql-string delete-exp)
     :database database)))

(defun create-table (table description &rest rest 
                             &key (database *default-database*)
                             &allow-other-keys)
  "Creates a database table.
Arguments:
 TABLE : table expression
 DESCRIPTION : A list of row descriptions and foreign key statements.
A row description is a list of the form (row-name row-type &optional key).
ROW-NAME is a symbol whose symbol-name is the row name, and 
ROW-TYPE is a symbol designating the type of the row or a list of row type and 
precision.
KEY is :NOT-NULL or :PRIMARY-KEY (which implies NOT NULL).
A foreign key statment is a list of the form 
(:foreign-key row-identifier-or-list
 :references foreign-table-identifier foreign-row-identifier-or-list)."
  (execute-command
   (make-create-table-expression database table description rest)
   :database database))

(defun drop-table (table &key (database *default-database*))
  "Drops a database table."
  (execute-command
   (make-drop-table-expression database table)
   :database database))

(defun create-index (index &rest rest &key on unique-p attributes
                             (database *default-database*))
  "Creates an index."
  (execute-command
   (apply #'make-create-index-expression database index on unique-p attributes rest)
   :database database))

(defun drop-index (index &key (database *default-database*))
  "Drops an index."
  (execute-command
   (make-drop-index-expression database index)
   :database database))

(defun create-view (view &rest rest 
                           &key alias as replace-p force-p check-option
                           (database *default-database*)
                           &allow-other-keys)
  "Creates a database view."
  (execute-command
   (apply #'make-create-view-expression database view alias replace-p 
          force-p as check-option rest)
   :database database))

(defun drop-view (view &key (database *default-database*))
  "Drops a database view."
  (execute-command
   (make-drop-view-expression database view)
   :database database))

; add password, tablespace etc
(defun create-user (username &rest rest
                           &key password (database *default-database*)
                           &allow-other-keys)
  "Creates a user (schema)."
  (execute-command
   (apply #'make-create-user-expression database username password rest)
   :database database))

(defun execute (&rest rest)
  "Executes a SQL (DDL) command in the generic syntax." 
  (let ((database (or (cadr (member :database rest)) *default-database*))) 
    (execute-command 
     (apply #'make-generic-execute-expression rest)
     :database database)))

(defun map-query (type function query-exp &key (database *default-database*))
  "Executes a query and maps a function over the result rows. Arguments:
 TYPE : Performs a type coersion. Ignored.
 FUNCTION : A function which as many arguments as there are return columns.
 QUERY-EXP : A select expression."
  (db-map-query database type function query-exp))

(defun map-bind-query (query type function &rest parameters)
  "Prepared version of MAP-QUERY. Executes a prepared query and maps a
function over the result rows. Arguments:
 QUERY : A prepared select expression.
 TYPE : Performs a type coersion. Ignored.
 FUNCTION : A function which as many arguments as there are return columns.
 PARAMETERS : Parameters to bind for execution."
  (apply #'db-map-bind-query query type function parameters))

;; put the version from sql-streams.lisp back here

(defmacro do-query (((&rest args) query-exp &key (database '*default-database*))
                      &rest body)
  "Executes a query; for each result row, the body is executed, while the column 
values are bound to symbols accessible in the body. Arguments:
 ARGS : A list of symbols to bind the result columns to. May contain a &rest 
argument. 
The last argument may be a stream argument of the form (:stream <var>); then, the
symbol <var> is bound to a SQL-STREAM for input which is opened before execution 
of the body and closed afterwards.
 QUERY-EXP : A select expression.
 BODY : body to execute for each row."
  (let* ((last-arg (car (last args)))
         (last-stream-p (and (consp last-arg) (eq :stream (car last-arg))))
         (simple-args (when last-stream-p (butlast args)))
         (stream-arg (when last-stream-p (cadr last-arg)))
         (arglen (let ((rest-pos (position '&rest args)))
                   (cond ((not rest-pos)
                          (length args))
                         ((= rest-pos (- (length args) 2))
                          (if last-stream-p
                            (error "stream and &rest arguments are not allowed together.")
                            nil))
                         (t
                          (error "Malformed argument list ~a." args)))))
         (query (gensym)))
    `(let ((,query (%db-execute ,database (sql-string ,query-exp)
                                :new-query-p t)))
       (unwind-protect
         (progn
           (%initialize-query ,query ,arglen)
           ,(if last-stream-p
              `(let ((,stream-arg (make-sql-stream ,query :column ,(1- arglen)))) 
                 (loop with data and data-found-p
                       do (multiple-value-setq (data data-found-p)
                            (%read-query-data ,query :last))
                       while data-found-p
                       do 
                       (open-sql-stream ,stream-arg)
                       (destructuring-bind ,simple-args data ,@body))
                 (close ,stream-arg))
              `(loop with data
                     while (setf data (%read-query-data ,query nil))
                     do (destructuring-bind ,args data ,@body))))
         ;; dispose of memory and set query inactive or get rid of it
         (db-close-query ,query)))))

(defmacro do-bind-query (((&rest args) query &rest parameters)
                            &rest body)
  "Prepared version of DO-QUERY. Executes a prepared query; for each result row, 
the body is executed, while the column values are bound to symbols accessible
in the body. Arguments:
 ARGS : A list of symbols to bind the result columns to. May contain a &rest 
argument or a stream argument as last argument.
 QUERY : A prepared query object
 PARAMETERS : Parameters to bind for execution.
 BODY : Body to execute for each row."
  (let* ((last-arg (car (last args)))
         (last-stream-p (and (consp last-arg) (eq :stream (car last-arg))))
         (simple-args (when last-stream-p (butlast args)))
         (stream-arg (when last-stream-p (cadr last-arg)))
         (arglen (let ((rest-pos (position '&rest args)))
                   (cond ((not rest-pos)
                          (length args))
                         ((= rest-pos (- (length args) 2))
                          (if last-stream-p
                            (error "stream and &rest arguments are not allowed together.")
                            nil))
                         (t
                          (error "Malformed argument list ~a." args))))))
    `(unwind-protect
       (progn
         (%db-bind-execute ,query ,@parameters)
         ,(if last-stream-p
            `(let ((,stream-arg (make-sql-stream ,query :column ,(1- arglen)))) 
               (loop with data and data-found-p
                     do (multiple-value-setq (data data-found-p)
                          (%read-query-data ,query :last))
                     while data-found-p
                     do 
                     (open-sql-stream ,stream-arg)
                     (destructuring-bind ,simple-args data ,@body))
               (close ,stream-arg))
            `(loop with data
                   while (setf data (%read-query-data ,query nil))
                   do (destructuring-bind ,args data ,@body))))
       ;; dispose of memory and set query inactive or get rid of it
       (%db-reset-query ,query))))

(defun data-sources (&optional (db-type *default-database-type*))
   "Returns a list of (data-source description) - pairs"
   (db-data-sources db-type))

(defmethod describe-columns (&key (database *default-database*)
                                     (table-qualifier "")
                                     (table-owner (db-user-id database))
                                     (table-name "%")
                                     (column-name "%"))
  (db-describe-columns database
                       (string-upcase table-qualifier)
                       (string-upcase table-owner)
                       (string-upcase table-name)
                       (string-upcase column-name)))

(defvar *binary-format* :bit-vector
  "The format binary data is output in. May be one of :bit-vector, :unsigned-byte-vector,
:string, :hex-string")
