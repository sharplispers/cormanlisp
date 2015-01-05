;;;; By adding the following lines in the Corman Lisp init.lisp
;;;; startup file you can use the following to automatically load
;;;; and compile the SQL-ODBC packages:
;;;;
;;;; (require 'ODBC)
;;;;
;;;; Note that you must make sure that you have applied all patches
;;;; to Corman Lisp 1.41 available at http://www.corman.net before
;;;; using this SQL-ODBC port.

;; This should be the directory pointing to the 
;; qsl-odbc installation.
(defconstant *sql-odbc-directory* 
	(concatenate 'string ccl:*cormanlisp-directory*
		"libraries\\sql-odbc-0.85\\"))

(defun sql-odbc-directory-name (relative-name)
	(concatenate 'string *sql-odbc-directory* relative-name))

(ccl:register-module-source "SQL" 
	(list 
		(sql-odbc-directory-name "sql\\sql-system.lisp")
		(sql-odbc-directory-name "sql\\sql-package.lisp")
		(sql-odbc-directory-name "sql\\sql-class.lisp")
		(sql-odbc-directory-name "sql\\sql-expressions.lisp")
		(sql-odbc-directory-name "sql\\sql-functional-interface.lisp")))

(ccl:register-module-source "ODBC" 
	(list 
		(sql-odbc-directory-name "corman\\ff-compatibility-corman.lisp")
		(sql-odbc-directory-name "odbc\\odbc-system.lisp")
		(sql-odbc-directory-name "odbc\\odbc-package.lisp")
		(sql-odbc-directory-name "odbc\\odbc-constants.lisp")
		(sql-odbc-directory-name "odbc\\odbc-ff-interface.lisp")
		(sql-odbc-directory-name "odbc\\odbc-functions.lisp")
		(sql-odbc-directory-name "odbc\\odbc-sql-interface.lisp")))

