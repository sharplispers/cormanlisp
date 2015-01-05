;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;; ODBC module for MCL, LWW and ACL
;; Version 0.85
;; Copyright (C) Paul Meurer 1999. All rights reserved.
;; paul.meurer@hit.uib.no
;;

;;; "This software is based on the design of the ODBC interface provided by 
;;;  Harlequin Group plc in the LispWorks and LispWorks for the Windows 
;;;  Operating System products. This implementation is not the property of
;;;  Harlequin and they have no responsibility for its content or accuracy."

;; Documentation and the license agreement can be found in file 
;; "sql-odbc-documentation.lisp".
;; Bug reports and suggestions are highly welcome.

(in-package :common-lisp-user)

#+mcl
(eval-when (:execute :compile-toplevel :load-toplevel)
  (um:use-module :sql "sql:sql;sql-system")
  (um:use-module :ff-compatibility "sql:mcl;ff-compatibility-mcl")
  (um:use-module :odbc-package "sql:odbc;odbc-package")
  (um:use-module :odbc-constants "sql:odbc;odbc-constants")
  (um:use-module :odbc-ff-interface "sql:odbc;odbc-ff-interface")
  (um:use-module :odbc-functions "sql:odbc;odbc-functions")
  (um:use-module :odbc-sql-interface "sql:odbc;odbc-sql-interface")  
  (um:use-module :odbc-streams "sql:odbc;odbc-streams"))

#+lispworks
(eval-when (:execute :compile-toplevel :load-toplevel)
  (defsystem odbc
    (:package "COMMON-LISP-USER"
     :default-pathname "sql:odbc;")
    :members ("sql:lispworks;ff-compatibility-lw"
              "odbc-package" "odbc-constants" 
              "odbc-ff-interface" "odbc-functions" "odbc-sql-interface"
              "odbc-streams")
    :rules ((:in-order-to :compile :all
             (:requires (:load :previous)))))
  (compile-system 'odbc :force-p nil)
  (load-system 'odbc))

#+allegro
(eval-when (:execute :compile-toplevel :load-toplevel)
  (defsystem :odbc 
    (:default-pathname "sql:odbc;"
      :default-package :odbc)
    (:serial
     "sql:allegro;ff-compatibility-acl"
     "odbc-package" "odbc-constants" 
     "odbc-ff-interface" "odbc-functions" "odbc-sql-interface"
     "odbc-streams"
     ))
  (compile-system :odbc)
  (load-system :odbc))

;; See corman\system-init.lisp for details on loading the SQL 
;; and ODBC packages.
#+cormanlisp
(require 'SQL)

(pushnew :odbc *features*)
