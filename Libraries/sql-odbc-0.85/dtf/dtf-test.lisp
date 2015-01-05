;;;-*- Mode: Lisp; Package: (COMMON-LISP-USER (SQL)) -*-

(in-package :cl-user)

(um:use-module :sql "sql:sql;sql-system")
(use-package :sql)

(sql:initialize-database-type :dtf)

(enable-sql-reader-syntax)

;; DBL: You must first create a database using
;;  the dtFAdmin application.

;; customize this
(connect "sql:dtf;database" :db-type :dtf
         :user-id "dtfadm" :password "dtfadm")

(disconnect)

#|
; create and insert

(with-transaction
  (execute-command "create table test (id integer, c shortstring)"))

(create-table 
 [test1]
 '((id longword)
   ;(c shortstring)
   ))

(with-transaction
  (insert-records :into [test1] :values '(23446)))

(with-transaction
  (insert-records :into [test] :values '(1234 "foo")))

(with-transaction
  (execute-command "insert into test values (1237, 'ackerschwaan')"))
|#


#|
; queries
(select [*] :from [test])

(let ((q (sql-exp::sql-string [select [*] :from [test]])))
  (print q)
  (query q))

(query "select * from test")

(dtf::retrieve-records "select * from test")
|#


#|
; shutdown

; ---------
;(execute-command "drop table test1")

(with-transaction
  (execute-command "delete from test"))

(disable-sql-reader-syntax)

(disconnect)
|#

(defun random-string (size)
  (let ((str (make-string size :element-type 'base-character)))
    (dotimes (i size)
      (setf (char str i) (code-char (+ 97 (random 26)))))
    str))

(execute-command "drop table benchmark1")


(drop-table [benchmark1])

(with-transaction
  (create-table
   [benchmark1]
   '((id integer)
     (a shortstring)
     (b shortstring))))

;; 168 sec; Wood: 80 sec
(time
 (dotimes (i 1000)
   (when (zerop (mod i 200)) (print i))
   (insert-records :into [benchmark1]
                   :values (list i (random-string 8) (random-string 8)))))

(insert-records :into [benchmark1]
                :values (list 0 (random-string 8) (random-string 8)))

(select [count [*]] :from [benchmark1])

(select [*] :from [benchmark1] :where [like [a] "%wz"])

(rollback)
(commit)


(drop-table [test1])

(with-transaction
  (drop-table [test])
  (create-table
   [test]
   '((id integer)
     (a shortstring))))

(insert-records :into [test]
                :values (list 9 "qweruiop"))

(select [id] :from [test])

(query "select a, count(id) from test group by a")

(select [count [*]] :from [benchmark1])

(select [id] [a] [b] :from [benchmark1]
        :where [like [a] "%wutz%"])

(do-query ((id)
           [select [count [*]] :from [benchmark1]])
          (print id))

(let ((str "%c"))
  (do-query ((id a b)
             [select [id] [a] [b] :from [benchmark1]
                     :where [or [like [a] ?str]
                                [like [b] ?str]]])
            (format t "~&~3d: ~a, ~a~%" id a b)))
