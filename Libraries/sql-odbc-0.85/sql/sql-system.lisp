;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;; Portable SQL module
;; Version 0.85
;; Copyright (C) Paul Meurer 1999. All rights reserved.
;; paul.meurer@hit.uib.no
;;
;; Documentation and the license agreement can be found in file 
;; "sql-odbc-documentation.lisp" 

(in-package :common-lisp-user)

#+mcl
(eval-when (:execute :compile-toplevel :load-toplevel)
  (um:use-module :sql-package "sql:sql;sql-package")
  (um:use-module :sql-class "sql:sql;sql-class")
  (um:use-module :sql-expressions "sql:sql;sql-expressions")
  (um:use-module :sql-interface "sql:sql;sql-functional-interface")
  (um:use-module :sql-streams "sql:sql;sql-streams")
  (um:use-module :sql-fred "sql:mcl;sql-fred"))

#+lispworks
(eval-when (:execute :compile-toplevel :load-toplevel)
  (defsystem sql
    (:package "COMMON-LISP-USER"
     :default-pathname "sql:sql;")
    :members ("sql-package"
              "sql-class" 
              "sql-expressions"
              "sql-functional-interface"
              "sql-streams")
    :rules ((:in-order-to :compile :all
             (:requires (:load :previous)))))
  (compile-system 'sql :force-p nil)
  (load-system 'sql))

#+allegro
(eval-when (:execute :compile-toplevel :load-toplevel)
  (defsystem :sql 
    (:default-pathname "sql:sql;"
      :default-package :sql)
    (:serial
     "sql-package"
     "sql-class"
     "sql-expressions"
     "sql-functional-interface"
     "sql-streams"
     ))
  (compile-system :sql)
  (load-system :sql))

;;#+cormanlisp
;;See corman\system-init.lisp for details on loading the SQL package.

(pushnew :sql *features*)
