;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;; SQL module for MCL, LWW and ACL/Windows
;; Version 0.7
;; (C) Paul Meurer 1999
;; paul.meurer@hit.uib.no
;;
;; You may freely use this code as long as you don't remove the (C) notice.
;; Bug reports and suggestions are highly welcome.

;; Read the file "sql-odbc-documentation.lisp" for more info.

(in-package :cl-user)

;; customize this
(setf (logical-pathname-translations "sql")
      '(("**;*.*.*" "ccl:Lisp;sql;**;")))

(load "sql:use-module.lisp")

;; DBL: This code expects that the dtF shared library (or
;;  an alias) is available on the library search path, which
;;  includes the ccl: folder and the system folder's extensions
;;  folder.  dtF/SQL 1.73 includes the appropriate shared library.
;;  If you have MPW (available for free from Apple), you can use
;;  the MPW PPC static libraries from dtF 1.6 by converting them
;;  to a shared library; the converted library and the files used
;;  to do the conversion are in the sql:dtf-library folder.
;;
;;  If you can log in to sLab's web site, you should probably pick
;;  up version 1.73 instead of using the converted 1.6 library.

(um:use-module :sql "sql:sql;sql-system")

(sql:initialize-database-type :dtf)
