;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;;; SQL/ODBC module for MCL, LWW and ACL
;;; Version 0.85
;;; Copyright (C) Paul Meurer 1999, All rights reserved.
;;; paul.meurer@hit.uib.no
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted, so long as the following
;;; conditions are met:
;;;      o This copyright notice is included intact.
;;;      o No fees or compensation are charged for use, copies, or
;;; 	   access to this software. You may charge a nominal
;;; 	   distribution fee for the physical act of transferring a
;;; 	   copy, but you may not charge for the program itself. 
;;;      o You are allowed to use this software as part of a commercial
;;;        software package, provided that its functionality significantly 
;;;        exceeds the functionality of this software, and that the use of 
;;;        this software is explicitly mentioned in your documentation.
;;;
;;; This software is made available AS IS, and no warranty is made about 
;;; the software or its performance. 

;;
;; Bug reports and suggestions are highly welcome.
;; Significant enhancements should be reported back to me.
;;


(in-package :cl-user)

(use-package :sql) ;; do this to test the examples

#|

   Documentation

0. Acknowledgements

The overall architecture of the SQL package (with the exception of streams, prepared statements, parameter and long data processing) is much inspired by Harlequin LispWorks´ SQL/ODBC module. I am very grateful to Harlequin for allowing me to use their design in this free software. (Otherwise, this work is not related to Harlequin; the implementation is totally independent from Harlequin´s code.)
I would like to thank Rainer Joswig and David Lamkins for valuable comments, suggestions and help.

1. Introduction

The SQL/ODBC module contains four different packages:

 SQL: A generic CL interface to SQL; with an extension of the CL syntax for writing SQL commands in a lispish way;
 FFC: Wrapper functions and macros for the foreign function interfaces of MCL, LispWorks (Windows) and ACL (Windows and Unix). More ports are planned;
 ODBC: A portable Common Lisp interface to the ODBC API;
 DTF: An interface to the API of the DtF database (www.snap.de) for MCL. (No longer developed.)

The code has been tested in MCL 4.2 with Oracle 8.0 (running on Windows NT) and PrimeBase (SNAP Innovation GmbH, www.snap.de) and DtF/SQL (www.slab.de) (on the same Mac, MacOS 7.6 and 8.5), in Allegro Common Lisp/Linux 5.0 and LispWorks/Linux 4.1 with Adabas D on Linux (SuSE 6.0) (MySQL/MyODBC is reported to work) and in LispWorks/Windows 4.1 and Allegro Common Lisp/Windows 5.0 with Oracle 8.0 and Solid Server (all on NT).

Availability of the ODBC drivers for the different platforms:

Macintosh: 

From Snap's site you can download the ³Visigenic ODBC for Macintosh Software Development Kit, Release 2.1.2², which contains the needed ODBC libraries and driver manager, plus the ODBC API spec. Snap's PrimeBase comes with an ODBC driver which works together with Visigenic's libraries.

InterSolve has the ODBC library set and drivers (³DataDirect²) for many commercial database products (Oracle, dBASE, Sybase etc.). There was an evaluation version on their web-site which seems to have disappeared.

OpenLink has drivers for many Unix (and otherwise) databases (Postgres, Informix, Sybase, Ingres, Oracle etc.). They are freely downloadable from their web-site (don't ask me what the exact licensing conditions are).

[This info is meant as a short overview and will be more precise in future versions.]

Unix:

(That is, Linux). Again, there are OpenLink drivers for many databases. (I haven't tested them.)
Adabas D comes with a static (.a) odbc library which has to be compiled into a shared library (.so) to be useful.
Solid Server has an ODBC-compatible API (not tested).
(All those products are on the SuSE 6.0 distribution CD.)
MySQL/MyODBC/unixODBC on Linux works for at least one person, although I were unable to install it.

Windows (NT):

Here, you should have no problems finding what you need.


2. The SQL package

This package implements a generic Common Lisp interface to SQL.
The architecture is as follows:

The top classes are database and query.

There are two levels of functions:

- Exported functions to query and manipulate the database that by default use the *default-database* or *default-query*, and 
- generic functions that interface with the database (ODBC etc.) API. 
The latter are used to implement the former and they begin with db-; methods for them are to be written in the API package(s).

The exported classes, functions and variables are:

database
[type]
Class used to store connection information. 
Several database objects can coexist, and are listed in *connected-databases*. The API packages (ODBC, DTF, or others) should subclass this class. There is a *default-database*, so you don't have always to mention the database you want to work with.

query
[type]
Stores query information, like SQL query string and database to run the query against. Each database has a default query (in the query slot), but you can create new ones to be able to run queries in parallel.

*default-query*
[variable]
Default query object.

*default-database*
[variable]
Default database object.

In future, I will drop the :database key on all toplevel functions. (The reason is that the behavior of the SQL string generator depends on the database type, and to the SQL syntax macros, the database can't be passed as an explicit argument.) Instead, if you want to access a database different from the default database, rebind *DEFAULT-DATABASE* locally. The following macro comes handy in here:

with-database (database)  &body body
[macro]
Binds *DEFAULT-DATABASE* to DATABASE in the scope of BODY

*connected-databases*
[variable]
A list of connected databases.

 Query and command execution:

execute-command sql  &key database
[generic-function]
Executes a SQL command (string or expression).

execute-query query-string  &key database flatp
[function]
Executes a SQL query string and fetches all rows.

open-query query-expression  &key database
[function]
Opens a new query and executes query-expression, without closing open queries. Returns the query object and makes it the default query

close-query query
[function]
Closes a query.

fetch-query-results &key count database-or-query
[function]
Fetches the first <count> results (or all, if count is nil, the default). The default for database-or-query is (or *default-query* *default-database*).

*null*
[variable]
Lisp representation of SQL Null value, default = nil. May be bound locally to something else if a certain type is necessary.

 Transaction processing:

*active-transactions*
[variable]
A list of active transactions

*commit-nested-transactions-p*
[variable]
If nil, only the outermost with-transaction form does commit on exit.

begin-transaction &key database
[function]
Starts a transaction

with-transaction &body body
[macro]
If the forms in body execute without error, all active transactions are committed on exit. If an error is occurs, the active transactions are rolled back. If *commit-nested-transactions-p* is true, transaction are committed/rolled back on exit from the outermost with-transaction form only.


 SQL Streams:

SQL streams are CLOS streams primarily designed for efficient input and output of long (character, binary) data. The implementation is based on the Gray streams proposal.
Luckily, both MCL, LW and ACL use closified streams.
LW's and ACL's stream implementations are already based on the Gray stream proposal (although there are small differences). For MCL, some extra classes and generic functions had to be defined.

The following classes are defined:

sql-stream
[type]
Abstract stream class (subclass of FUNDAMENTAL-STREAM) designed primarily for input and output of long data.

sql-string-stream
[type]
Subclass of SQL-STREAM for input and output of long string/character data.

Subclasses of sql-stream and sql-string-stream are defind in the odbc package.

Opening and closing of SQL streams happens behind the scenes in the macros that provide access to streams (BIND-EXECUTE-WITH-STREAM, DO-QUERY, DO-BIND-QUERY), therefore, no user functions are provided for opening and closing.

The following input and output functions have been implemented so far (only for string streams):

read-sequence
write-sequence
read-char
peek-char

 Mapping functions:

map-query type function query-exp  &key database
[function]
Executes a query and maps a function over the result rows. Arguments:
 TYPE : Performs a type coersion. Ignored.
 FUNCTION : A function which as many arguments as there are return columns.
 QUERY-EXP : A select expression.

do-query ((&rest args) query-exp &key (database '*default-database*))  &rest body
[macro]
Executes a query; for each result row, the body is executed, while the column values are bound to symbols accessible in the body. Arguments:
 ARGS : A list of symbols to bind the result columns to. May contain a &rest  argument. 
The last argument may be a stream argument of the form (:stream <var>); then, the symbol <var> is bound to a SQL-STREAM for input which is opened before execution  of the body and closed afterwards.
 QUERY-EXP : A select expression.
 BODY : body to execute for each row.

 Prepared statements and parameters:

prepare-statement sql-expression  &key database parameter-columns parameter-table
[function]
Prepares a statement for repeated execution and returns a query object. Use $ instead of SQL ? as parameter marker. 
Arguments:
 SQL-EXPRESSION : SQL statement to be prepared (Works for now for insert and select).
 PARAMETER-COLUMNS : A list of the parameter columns; only necessary if not all  are parameterized and if sql-expression is a string. (Otherwise it is possible to deduce their types and names from the sql-expression.)
 PARAMETER-TABLE : Name of the table; only necessary if sql-expression is a string.

with-prepared-statement (query sql-expression &key (database '*default-database*))  &body body
[macro]
Prepares a (parameterized) statement (insert and select works by now) for repeated  execution. A new query object is created and bound to the symbol QUERY. 
The function BIND-EXECUTE, to be used inside this macro, binds values to the parameters and executes the query.

bind-execute query  &rest parameters
[function]
Binds values to the parameters of a prepared statement and executes the command.
If the statement was a select expression, the result is fetched.
To be used in the body of the WITH-PREPARED-COMMAND macro.

bind-execute-with-stream (query &rest parameters)  &body body
[macro]
Binds values to the parameters of a prepared insert statement and executes the query.
The last parameter value should be a list of the form (:stream <stream-var> [size]), where <var> is a variable bound to a SQL-STREAM (for output) in the body, and size is  an optional value denoting the size of the stream. (This is necessary for drivers returning "Y" for info type $SQL_NEED_LONG_DATA_LEN, e.g. the DataDirect Oracle drivers.)

map-bind-query query type function  &rest parameters
[function]
Prepared version of MAP-QUERY. Executes a query and maps a function over the result rows. Arguments:
 TYPE : Performs a type coersion. Ignored.
 FUNCTION : A function which as many arguments as there are return columns.
 QUERY-EXP : A select expression.

do-bind-query ((&rest args) query &rest parameters)  &rest body
[macro]
Prepared version of DO-QUERY. Executes a prepared query; for each result row, the body is executed, while  the column values are bound to symbols accessible in the body. Arguments:
 ARGS : A list of symbols to bind the result columns to. May contain a &rest argument. 
The last argument may be a stream argument of the form (:stream <var>); then, the symbol <var> is bound to a SQL-STREAM for input which is opened before execution  of the body and closed afterwards.
 QUERY : A prepared query object
 PARAMETERS : Parameters to bind for execution.
 BODY : Body to execute for each row.

select &rest expressions
[function]
Executes a select statement and returns a list of fetched result rows.
Signature: (&rest rows &key database from where group-by having order-by distinct using set-operation all joins start-with connect-by flatp)
JOINS, USING and SET-OPERATION are not implemented yet

select-union &rest expressions
[function]
Executes a union of select statement and returns a list of fetched result rows.
Signature: (&rest select-expressions &key database order-by flatp)

insert-records &rest rest  &key into attributes values av-pairs query-expression database  &allow-other-keys
[function]
Inserts a record into a table.

update-records table  &rest rest  &key attributes values av-pairs where database  &allow-other-keys
[function]
Updates a record.

delete-records &rest rest  &key from where database  &allow-other-keys
[function]
Deletes a record.

create-table table description  &rest rest  &key database  &allow-other-keys
[function]
Creates a database table.
Arguments:
 TABLE : table expression
 DESCRIPTION : A list of row descriptions and foreign key statements.
A row description is a list with structure (row-name row-type &optional key).
ROW-NAME is a symbol whose symbol-name is the row name, and 
ROW-TYPE is a symbol designating the type of the row or a list of row type and precision.
KEY is :NOT-NULL or :PRIMARY-KEY (which implies NOT NULL).
A foreign key statment is a list of the form (:foreign-key row-identifier-or-list :references foreign-table-identifier foreign-row-identifier-or-list).

drop-table table  &key database
[function]
Drops a database table.

create-index index  &rest rest  &key on unique-p attributes database
[function]
Creates an index.

drop-index index  &key database
[function]
Drops an index.

create-view view  &rest rest  &key alias as replace-p force-p check-option database  &allow-other-keys
[function]
Creates a database view.

drop-view view  &key database
[function]
Drops a database view.

create-user username  &rest rest  &key password database  &allow-other-keys
[function]
Creates a user (schema).

execute &rest rest
[function]
Executes a SQL (DDL) command in the generic syntax (see below).

 Miscellaneous:

data-sources &optional db-type
[function]
Returns a list of (data-source description) - pairs

*binary-format*
[variable]
The format binary data is output in. May be one of :bit-vector, :unsigned-byte-vector
(in future also: :string, :hex-string)

3. The SQL syntax

The SQL expression syntax is an extension of the CL syntax for writing SQL commands. It is implemented through reader macros for [, ] and ?. 

To make editing expressions in the SQL syntax more convenient, I have tried to make [ and ] behave like ( and ) in the editors of the respective Lisp environments. This was very easy for LW, in MCL I had to change some functions of the Fred implementation. For the new integrated editor of ACL/Windows 5.0, this seems to be still more difficult, and I have no plans to look into this further. (I got no hints whatsoever from the support people, but luckily they provide the source code.)
The code for the Emacs lisp mode is easy to change: simply comment out the following two lines in <acl5>/eli/fi-modes.el:

(modify-syntax-entry ?\[ "_   " fi:lisp-mode-syntax-table)
(modify-syntax-entry ?\] "_   " fi:lisp-mode-syntax-table)

and recompile.

The SQL expression syntax has been tested mainly with Oracle, but since every database vendor implements his own extensions to and deviations from the SQL standard, not all SQL constructs for other RDBMSs are available, nor will the available ones work with every database.
To remedy this, the functions that implement the SQL string generation are methods that dispatch on the database type, this makes it possible to implement those syntax extensions.

Further on, there is a flexible generic command syntax (see below) which lets you write (almost) arbitrary SQL commands. 

As a last resort, you can always use raw SQL strings.

The SQL expression syntax is invoked with (enable-sql-reader-syntax). The syntax works as follows (explained in an informal way):

Bracketed expressions evaluate to an instance of (a subclass of) sql-expression which in its sql slot contains the sql string computed from the expression.

Database identifiers:
They are symbols in brackets. Database identifiers may contain (Lisp-style) hyphens which are translated to (SQL-style) underscores.
[foo] may be used as the table or column name "foo", whereas [foo bar] represents a qualified table or column "foo.bar". This is semantically ambiguous, but since the database's SQL parser only sees the generated string "FOO" or "FOO.BAR", this does not matter.
An extension for (Oracle?) remote tables:
[foo bar @ remote-database] => "FOO.BAR@REMOTE_DATABASE"

SQL operators, too, are coded in this way, f.ex. 

[count [*]] => "count(*)"
[and [= [a] 1] [= [b] 2] [= [c] 3]] => "((a = 1) and (b = 2) and (c = 3))"

Whether a bracket expression denotes a simple identifier or involves SQL operator application depends on the elements in the bracket: only if all of them are symbols, the expression is treated as a table or column identifier.
E.g. [count [*]] evaluates to "count(*)", whereas [count *] gives "count.*".

Optional elements (like "distinct" in the SQL "count" operator) are treated as optional Lisp arguments, and SQL 'keywords' are treated as Lisp keywords (but not always in a one-to-one way).
E.g.:

[count [length [key]]]           => "count(length(KEY))"
[count [length [key]] :distinct] => "count(distinct length(KEY))"

[select [concat [rpad "." [* 2 [level]] "."] [term]] [level]
        :from [foo bar]
        :start-with [= [term] "kjemisk forbindelse"]
        :connect-by [= [bt] [id] :prior]]
=>
"(select (rpad('.', (2 * LEVEL), '.') || TERM),level from FOO.BAR start with (TERM = 'kjemisk forbindelse') connect by (BT = prior ID))"

(This nice Oracle specific command prints a part of a chemistry thesaurus in a readable way:
..kjemisk forbindelse           
....uorganisk forbindelse             
......uorganisk salt          
......uorganisk nitrogenforbindelse                  
......svovelforbindelse            
........uorganisk sulfid            
........thionylklorid           
........svovelsyre         
........svoveloksid          
..........svoveldioksid            
........sulfat       
........klorsulfonsyre           
........hydrogensulfid           
......silisiumforbindelse             
........silisiumkarbid           
........silikat        
......oksygenforbindelse            
........peroksid        
........oksid       
..)

Quoted bracket expressions are treated in the same way (as long as they don't contain variables, of course). That is, you can say '([foo] [bar]) instead of (list [foo] [bar]) or `(,[foo] ,[bar]).

Variables in bracket expressions are symbols beginning with a question mark. Using this special syntax makes it unneccessary to quote symbols that are not to be evaluated. Variables can be used in any place of a bracket expression:

(let ((p '+) (makk [mokk]) (mikk 'mekk) (mukk 'makk) (op 'count) (nop 'max)) 
  [?p [?op ?makk] [?nop [?mikk ?mukk]]])
=> "(count(MOKK) + max(MEKK.MAKK))"

The bracket syntax can be interspersed freely with normal Lisp syntax:

(let ((x t) (op 'count))
  [select (if x [?op [*]] [max [mallery]]) :from [foo-foo (if x 'bar 'baz)]])

=> "(select count(*) from FOO_FOO.BAR)"

(let ((x t))
  [(if x 'foo 'bar) baz])
=> "FOO.BAZ"

The functions apply and funcall are defined for SQL operators:

(let ((x t))
  [funcall (if x 'min 'max) [id]])

=> "min(ID)"

(let ((x t))
  [apply (if x 'and 'or) '([a] [b])])

=> "(A and B)"

The generic command syntax invoked by the function EXECUTE which executes a DDL statement whose argument list may contain keywords, SQL bracket expressions, strings, numbers, symbols and lists. Keywords are translated to SQL keywords where a hyphen is translated by Space. Arguments of other types are treated in the usual way: bracket expressions are expanded, strings are translated to SQL-conformant strings. 
Lists are treated in two different ways: if the nesting degree is even (like a list on toplevel), the list is translated as comma-separated enumeration of its elements, if the nesting degree is odd, the elements are not separated by comma. [This needs further 