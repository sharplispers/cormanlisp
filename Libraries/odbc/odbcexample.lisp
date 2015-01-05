;;;; odbcexample.lisp

;;;; Author: Vassili Bykov <vassili@magma.ca> <vassili@objectpeople.com>
;;;; Created: 11/24/1998
;;;; Last updated: 11/26/1998

;;;; An example of "raw" ODBC programming in Lisp.  A simple
;;;; application that lets you select a data source, connects to it,
;;;; creates a table, populates it, runs a simple query and prints the
;;;; results.  Nothing fancy, but contains a few reusable things that
;;;; could eventually become a part of a higher-level ODBC interface.
;;;; Would be nice to have working exceptions and a full macro support
;;;; before that.

(require "ODBCEXT-FFI")
(in-package :ODBC)

(defmacro phvalue (phandle)
  "Dereference a pointer to a handle."
  `(cref (:handle *) ,phandle 0))

(defmacro execute-checking-sql-rc (&body forms)
  "Evaluate all forms in the body in an implict PROGN, checking the return code
of each of them. (So they are all supposed to return an SQL error code).  If a
form returns a code indicating failure, throw ODBC-ERROR."
  `(progn
    ,@(mapcar
       #'(lambda (form)
	   `(unless (SQL_SUCCEEDED ,form) (throw 'ODBC-ERROR)))
       forms)))

(defmacro with-allocated-sql-handle ((var input-handle handle-type) &body forms)
  "Bind VAR to a pointer to a handle and allocate the handle to be an SQL handler
of the specified HANDLE-TYPE, allocated in the context of INPUT-HANDLE."
  `(let ((,var (malloc 4)))
    (execute-checking-sql-rc
     (SQLAllocHandle ,handle-type ,input-handle ,var))
    (unwind-protect (progn ,@forms)
      ;; Don't check errors on freeing, we have nothing to do while
      ;; unwinding even if there is an error anyway.
      (SQLFreeHandle ,handle-type (phvalue ,var)))))


(defmacro with-henv-and-hdbc ((henv-var hdbc-var) &body forms)
  "Bind HENV-VAR and HDBC-VAR to pointers to ODBC environment and connection
handles and evaluate FORMS as a PROGN."
  `(with-allocated-sql-handle (,henv-var NULL SQL_HANDLE_ENV)
    (execute-checking-sql-rc
     (SQLSetEnvAttr (phvalue ,henv-var) SQL_ATTR_ODBC_VERSION
                    (int-to-foreign-ptr SQL_OV_ODBC3) 0))
    (with-allocated-sql-handle (,hdbc-var (phvalue ,henv-var) SQL_HANDLE_DBC)
      ,@forms)))

(defun report-sql-errors (handle-type handle)
  "Retrieve and print on the standard error all the errors pending for the
given handle of the given type."
  (let ((pdata (malloc 1024))
	(pstate (malloc 6))
	(pnerr (malloc 4))
	(psize (malloc 4)))
    (flet ((get-error (rec-number)
	     (SQLGetDiagRec handle-type handle
			    rec-number
			    pstate
			    pnerr 
			    pdata 1024 psize)))
      (do* ((rn 1 (1+ rn))
	    (rc (get-error rn) (get-error rn)))
	   ((not (SQL_SUCCEEDED rc)))
	(format *error-output*
		"~&ODBC ERROR: state = ~S err = ~S message = ~S~%"
		(c-string-to-lisp-string pstate)
		(cref (:long *) pnerr 0)
		(c-string-to-lisp-string pdata))))))

(defun execute-sql (hstmt &rest strings)
  "Execute all the passed SQL STRINGS within a given statement handle.  Check success
after each execution and bail out by throwing ODBC-ERROR if there was an error, after
printing the error information.  If a SQL string is followed by a keyword
:if-error-ignore, continue execution even if there has been an error."
  ;; This is a good example of a horrible CATCH/THROW abuse.  When
  ;; exceptions are ready, this will be MUCH cleaner.
  (catch 'normal-exit
    (catch 'odbc-error
      (do* ((strs strings (if (eq (cadr strs) :if-error-ignore)
			      (cddr strs)
			    (cdr strs)))
	    (statement (car strs) (car strs)))
	   ((null strs))
	(format t "~&Executing: ~S~%" statement)
	(unless (or (SQL_SUCCEEDED (SQLExecDirect hstmt
						  (create-c-string statement)
						  SQL_NTS))
		    (eq (cadr strs) :if-error-ignore))
	  (throw 'odbc-error)))
      (throw 'normal-exit))
    ;; can get here if only 'odbc-error has been thrown
    (report-sql-errors SQL_HANDLE_STMT hstmt)
    (throw 'odbc-error)))


(defun test ()
  (with-henv-and-hdbc (phenv phdbc)
    (execute-checking-sql-rc
     (SQLDriverConnect (phvalue phdbc)
		       (cl::get-application-main-window)
		       NULL 0 NULL 0 NULL
		       SQL_DRIVER_COMPLETE))
    (with-allocated-sql-handle (phstmt (phvalue phdbc) SQL_HANDLE_STMT)
      (execute-sql (phvalue phstmt)
		   "DROP TABLE FOO" :if-error-ignore
		   "CREATE TABLE FOO (ID NUMERIC, NAME VARCHAR(20))"
		   "INSERT INTO FOO VALUES (1, 'First')"
		   "INSERT INTO FOO VALUES (2, 'Second')"
		   "INSERT INTO FOO VALUES (3, 'Third')"
		   "SELECT NAME FROM FOO WHERE ID >= 2")
      ;; Now get the values the last expression selected
      (let ((pdata (malloc 100))
	    (psize (malloc 4)))
	(do ((rc (SQLFetch (phvalue phstmt)) (SQLFetch (phvalue phstmt))))
	    ((not (SQL_SUCCEEDED rc)))
	  (SQLGetData (phvalue phstmt) 1 SQL_C_CHAR pdata 100 psize)
	  (format t "~&GOT: ~A~%" (c-string-to-lisp-string pdata)))))))
