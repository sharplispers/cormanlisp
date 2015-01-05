;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-


;; this is a rather ad-hoc collection of example commands.
;; they have been tested in MCL 4.2 against Oracle 8.0 on NT

(in-package :cl-user)

;; customize this
(setf (logical-pathname-translations "sql")
      '(("**;*.*" "ccl:projects;sql-odbc 0.85;**;")))

#+mcl(load "sql:mcl;use-module.lisp")

;; sample session

(require :sql "sql:sql;sql-system")
(use-package :sql)

(initialize-database-type :odbc)

(enable-sql-reader-syntax)

;; customize this
(connect "Oracle8PC" :user-id "system" :password "=2)(/)11214((3/%&$/$1#22" :if-exists :warn-old)

(create-user [foo] :password "bar")
(execute :grant '(connect resource) :to [foo])

;(drop-table [foo bar])

(create-table [foo bar]
              '((id integer :primary-key)
                (text (varchar 32))))

(create-table [foo baz]
              '((id integer :primary-key)
                (text (varchar 32))
                (:foreign-key [id] :references [foo bar] [id])))

(create-view [foo bar-baz]
             :replace-p t
             :as [select '([baz id] [bar text] ([baz text] [text1]))
                         :from '([foo bar] [foo baz])
                         :where [= [bar id] [baz id]]])

(with-transaction
  (insert-records :into [foo bar] :values '(17 "jazz")))
(with-transaction
  (insert-records :into [foo baz] :values '(17 "Pacito d'Riveira")))

(select [id] :from [foo bar-baz])

(select [count [id]] :from [foo bar-baz])

(do-query ((a b)
           [select [*] :from [foo bar]])
          (print (list a b)))

(execute-query "select id from foo.bar union select id from foo.bar order by id desc")

(select-union [select id :from [foo bar]]
              [select id :from [foo baz]]
              :order-by '((id desc)))

(let ((schema 'foo))
  (select [id] [text] :from [?schema baz] :order-by '((id desc))))

;; lexicon is a rather huge dictionary database with two tables:

(create-user [lexicon] :password "bar")
(execute :grant '(connect resource) :to [lexicon])

(create-table [lexicon keys]
              '((id integer :primary-key)
                (key (varchar 256) :not-null)
                (category (varchar 32) :not-null)
                (translation (varchar 256))))

(create-table [lexicon examples]
              '((id integer :not-null)
                (example_number integer :not-null)
                (example (varchar 256) :not-null)
                (translation (varchar 256) :not-null)
                (:foreign-key id :references [lexicon keys] (id))))

(select [key] :from [lexicon keys] :where [< rownum 2])

(select [*] :from [lexicon keys] :where [< rownum 2])

(do-query ((a)
           [select [key] :from [lexicon keys] 
                   :where [and [> [length [key]] 20]
                               [< rownum 20]]])
          (print a))

(select '([category] 
          ([count [length [key]] :distinct] [sum]))
        :from [lexicon keys]
        :group-by [category]
        :order-by '(([sum] :desc)))

(select [count [key] :distinct]
        :from [lexicon keys]
        :where [< [rownum] 3])

(select [substr [key] 1 2]
        :from [lexicon keys]
        :where [< [rownum] 8])

(select [key]
        :from [lexicon keys] 
        :where [and [< rownum 200] [< [length [key]] 4]])

(select [category]
        :distinct t
        :from [elhuyar keys] 
        :where [< rownum 200])

(let ((cat 'category)
      (ref-exp [id])
      (operator 'avg))
  (select `(,[?cat] (,[/ [?operator ?ref-exp] 2] ,[?operator]))
          :from [lexicon keys] 
          :where [> [length [key]] 8]
          :group-by [category] 
          :having [> [length [category]] 4]))

(let ((fun 'lpad))
  (select (list [?fun [category] 20 "-+"]
                (list [* [sum [id]] 2] [total]))
          :from [lexicon keys] 
          :where [> [length [key]] 8]
          :group-by [category] 
          :having [> [length [category]] 4]))

(select '([conc [lpad [category] 20 "-+"] "asdf"]
          ([* [sum [id]] 2] [total]))
        :from [lexicon keys] 
        :where [> [length [key]] 8]
        :group-by [category] 
        :having [> [length [category]] 4])

(select-union [select [conc [key] ", Kategorie: " [category]]
                      :from [lexicon keys]
                      :where [< [rownum] 3]]
              [select [conc [category] ", Lemma: " [key]]
                      :from [lexicon keys]
                      :where [< [rownum] 3]]
              :flatp t)


(select [conc [key] ", Kategorie: " [category]]
        :from [lexicon keys]
        :where [<= 3 [id] 9]
        :flatp t)

(select [ceil 3.3] :from [sys dual])
(select [sysdate] :from [sys dual])

(select [keys key] [keys2 key]
        :from '([lexicon keys] ([lexicon keys] [keys2]))
        :where [and [like [keys key]
                          [conc "%" [keys2 key] "%"]]
                    [like [keys2 key] "ehun%"]])

(select [keys key] [example]
        :from '([lexicon keys]
                [select ([example] ([id] [ex-id]))
                        :from [lexicon examples]
                        :where [> [id] 30000]])
        :where [and [= [keys id] [ex-id]]
                    [like [keys key] "hautesk%"]])

(let ((example 'example) (id 'id) (ex-id 'ex-id))
  (declare (ignorable example id ex-id))
  (select [keys key] [example]
          :from `([lexicon keys]
                  ,[select `((,[lpad [?example] 50 "+"] [example])
                             (,[?id] [ex-id]))
                           ;(list [?example] (list [?id] [?ex-id]))
                           :from [lexicon examples]
                           :where [> [id] 30000]])
          :where [and [= [keys id] [ex-id]]
                      [like [keys key] "hautesk%"]]))

(update-records [foo bar]
                :av-pairs '(([id] [+ [id] +13]))
                :attributes '([text])
                :values '([select [text] :from [foo bar]
                                  :where [= [id] 7]])
                :where [> [id] 14])

(update-records [foo bar]
                :av-pairs '(([id] [+ [id] +13])
                            ([text] [select [text] 
                                            :from (([foo bar] [zup]))
                                            :where [= [zup id] [bar id]]
                                            ]))
                :where [> [id] 14])

(defun random-string (size)
  (let ((str (make-string size :element-type 'base-character)))
    (dotimes (i size)
      (setf (char str i) (code-char (+ 97 (random 26)))))
    str))

(values (describe-columns :table-owner "foo" :table-name "bar"))

;; SQL_BINARY

(create-table
 [foo raw-test]
 '((id integer)
   (bar (raw 10))
   (a (varchar 8))))

(insert-records :into [foo raw-test]
                :values (list 1 #*101110010101000011100000000 "raw!"))

;; prepared statements and parameters

(progn
  (drop-table [foo bazz])
  (create-table [foo bazz] '((text (varchar 200)))))

(delete-records :from [foo bazz])

(time
 (dotimes (i 1000)
   (with-transaction
     (insert-records :into [foo bazz]
                     :values (list (random-string (random 20)))))))

;; much faster:
(time
 (progn
  (with-prepared-statement (query [insert :into [foo bazz] :values '($)])
    (dotimes (i 1000)
      (with-transaction
        (bind-execute query (random-string (random 20))))))))

(progn
  (ignore-errors (drop-table [foo bizz]))
  (create-table [foo bizz] '((num integer)
                             (text1 (varchar 100))
                             (text2 (varchar 100)))))

(time 
 (with-prepared-command (query [insert :into [foo bizz] :values '($ $ $)])
   (dotimes (i 1000)
     (with-transaction
       (let ((id (random 100)))
         (bind-execute query id (random-string id) (random-string id)))))))

(do-query ((id str1 str2)
           [select [*] :from [foo bizz] :where [like [text1] "%gag%"]])
          (format t "~2d ~a ~a~%" id str1 str2))


(with-prepared-statement (query [select [*] :from [foo bar] 
                                        :where [and [< [id] $] [like [text] $]]])
  (list (bind-execute query 27 "%z%")
        (bind-execute query 24 "%f")))

(defparameter *query*
  (prepare-statement
   [select [*] :from [foo bar] :where [and [< [id] $] [like [text] $]]]))

(do-bind-query ((&rest args) *query* 100 "%s")
               (print args))

(map-bind-query *query* nil
                (lambda (&rest rest)
                  (print rest))
                100 "%s")

(close-query *query*)


;; streams

(do-query ((id (:stream text-stream))
           [select [id] [text] :from [foo bar] :where [< [id] 30]])
          (print id)
          (do ((char (read-char text-stream nil)
                     (read-char text-stream nil))
               (i 0 (1+ i)))
              ((null char))
            (print char)))

(do-query ((id (:stream stream))
           [select [id] [text] :from [foo bar] :where [< [id] 19]])
          (print id)
          (let ((str (make-string 3 :initial-element #\?)))
            (print (peek-char nil stream))
            (print (read-sequence str stream))
            (print str)
            (print (peek-char nil stream))
            (print (peek-char nil stream))
            (print (read-char stream nil))
            (print (read-sequence str stream))
            (print str)
            (print (read-char stream nil))
            ))

(create-table [foo long-test]
              '((id integer :primary-key)
                (text long)))

;; this one might crash in MCL with the DataDirect driver.

(with-prepared-statement (query [insert :into [foo long-test] :values '($ $)])
  (dotimes (i 10)
    (bind-execute-with-stream (query i (:stream stream))
      (dotimes (i 10)
        (write-sequence (random-string 2000) stream)))))
