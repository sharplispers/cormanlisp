;;;-*- Mode: Lisp; Package: SQL -*-

;; SQL module for MCL, LWW and ACL
;; Version 0.8
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


; The main functions dispatch on database type for example to add a keyword
; for Oracle optimizer hints etc which won't work for a simple-minded
; Access query

; To do: Possibly output of SQL to a stream give as a keyword argument
; or a broadcast stream

(in-package :sql)

;; abstract class
(defclass sql-expression ()
  ((sql :initform nil :initarg :sql-string :accessor sql-string)))

(defmethod sql-string ((sql-expression string))
  sql-expression)

(defmethod sql-expression-p ((exp t))
  (declare (ignore exp))
  nil)

(defmethod sql-expression-p ((exp sql-expression))
  (declare (ignore exp))
  t)

(defclass sql-reference-expression (sql-expression)
  ((exp1 :initform nil :initarg :exp1 :accessor sql-exp1)
   (exp2 :initform nil :initarg :exp2 :accessor sql-exp2)
   (exp3 :initform nil :initarg :exp3 :accessor sql-exp3)))

#-cormanlisp
(defmethod make-load-form ((exp sql-expression)
                              #-mcl &optional #-mcl environment)
  #-mcl(declare (ignore environment))
  (values
   `(make-instance ',(class-name (class-of exp)))
   `(progn
      ,@(mapcar
         (lambda (name)
           `(setf (slot-value ,exp ',name) ',(slot-value exp name)))
         (mapcar 
           #+mcl #'slot-definition-name
           #-mcl #'clos::slot-definition-name
           #+mcl(class-instance-slots (class-of exp))
           #-mcl(clos:class-slots (class-of exp)))))))

(defclass sql-operator-expression (sql-expression)
  ((operator :initform nil :initarg :op :accessor sql-operator)
   (exp-list :initform () :initarg :exp-list
             :accessor sql-exp-list)))

(defclass sql-infix-operator-expression (sql-operator-expression)
  ())

(defclass sql-in-predicate-expression (sql-expression)
  ((operator :initform nil :initarg :op :accessor sql-operator)
   (exp :initform nil :initarg :exp :accessor sql-exp)
   (list-or-subquery :initform nil :initarg :list-or-subquery
                     :accessor sql-list-or-subquery)))

;; defined elsewhere.
;(defvar *default-database* nil)

(defclass sql-query-expression (sql-expression)
  ())

(defclass sql-select-expression (sql-query-expression)
  ((flatp :initform nil :initarg :flatp :accessor flat-fetch-list-p)))

;; change name!
(defclass sql-union-expression (sql-select-expression)
  ((operator :initform nil :initarg :op :accessor sql-operator)
   (exp-list :initform () :initarg :exp-list :accessor sql-exp-list)
   (order-exp :initform nil :initarg :order-exp :accessor sql-order-exp)))

(defclass sql-command-expression (sql-query-expression)
  ())

(defclass sql-insert-expression (sql-command-expression)
  ((table :initform nil :initarg :table :accessor exp-table)
   (parameter-columns :initform () :initarg :parameter-columns 
                      :accessor exp-parameter-columns)))

;; are those necessary??
(defclass sql-update-expression (sql-command-expression)
  ())

(defclass sql-delete-expression (sql-command-expression)
  ())

(defmethod sql-string :before ((sql-exp sql-union-expression))
  (with-slots (sql) sql-exp
    (unless sql
      (setf sql (with-output-to-string (stream)
                  (write-sql sql-exp stream))))))

(defmethod print-object ((thing sql-expression) stream)
  (print-unreadable-object (thing stream)
    (format stream "~S \""
            (class-name (class-of thing)))
    (write-sql thing stream)
    (write-string "\"" stream)))

;; keep it private!
(defvar *in-sql* nil)

;; identity can be an sql identifier. So we define a name that can't.
(defun %aux-identity (thing)
  thing)

(defparameter *saved-readtable* (copy-readtable))

;(defparameter *saved-readtable* *readtable*)

(defparameter *sql-readtable* (copy-readtable))

;; ? is like comma in backquote.
(set-macro-character
 #\?
 (lambda (stream char)
   (declare (ignore char))
   (if *in-sql*
     ;; the effect of this is that the symbol is masked in (**) 
     ;; and won't get quoted
     (list '%aux-identity (read stream t nil t))
     ;; if ? is encountered outside sql brackets, make it behave in the
     ;; normal way.
     (let ((*readtable* *saved-readtable*))
       (unread-char #\? stream)
       (read stream t nil t))
     ; (error "? not inside sql brackets.")
     )) t *sql-readtable*)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defconstant +infix-operators+
    '(< > = <= >= <> /= / - + like not-like)))

;; still missing: between predicate, all-function, distinct-functon,
;; exists predicate, outer joints (sic!), USER, ALL, ANY, AS, AS column alias
;; etc.
(set-macro-character
 #\[
 (lambda (stream ignore)
   (declare (ignore ignore))
   (macrolet ((quote-list (exp-list)
                `(progn
                   (dotimes (i (length ,exp-list))
                     (let ((thing (nth i ,exp-list)))
                       (when (and (not (keywordp thing)) ; not necessary
                                  (symbolp thing)) ;; (**)
                         (setf (nth i ,exp-list) `',thing))))
                   ,exp-list))) 
     (let* ((*in-sql* t)
            (exp-list (read-delimited-list #\] stream t)))
       (cond ((find-if (lambda (exp) 
                         (and (consp exp)
                              (not (consp (car exp)))
                              (not (sql-expression-p (car exp)))))
                       exp-list)
              (quote-list exp-list)
              ; don't yet know what it is
              (cons 'make-sql-expression exp-list))
             ((and (cdr exp-list)
                   (eq :op (cadr exp-list)))
              (apply #'sql-operator-exp (car exp-list) (cddr exp-list))) 
             ((or (find '$ exp-list)
                  (find-if-not (lambda (exp)
                                 (or (symbolp exp) (stringp exp)))
                               exp-list))
              (apply #'make-sql-expression exp-list))
             (t
              (apply #'sql-ref-exp exp-list))))))
 nil *sql-readtable*)


(set-macro-character 
 #\]
 (get-macro-character #\))
 nil *sql-readtable*)

(defun enable-sql-reader-syntax ()
  #-:lispworks
  (copy-readtable *sql-readtable* *readtable*) ; some problems with LW
  #+:lispworks
  (setf *readtable* *sql-readtable*) ; does not work for ACL/Unix
  #+lispworks
  (progn ;; why can't it be that elegant in MCL and ACL?
    (editor::set-vector-value
     (slot-value editor::*default-syntax-table* 'editor::table) #\[ 2)
    (editor::set-vector-value 
     (slot-value editor::*default-syntax-table* 'editor::table) #\] 3)))

(defun disable-sql-reader-syntax ()
  #-:lispworks
  (copy-readtable *saved-readtable* *readtable*)
  #+:lispworks
  (setf *readtable* *saved-readtable*)
  #+:lispworks
 (progn
    (editor::set-vector-value
     (slot-value editor::*default-syntax-table* 'editor::table) #\[ 7)
    (editor::set-vector-value
     (slot-value editor::*default-syntax-table* 'editor::table) #\] 7)))

(defun sql-ref-exp (exp1 &optional exp2 exp3 exp4)
  (cond ((eq exp2 '@)
         (if (not exp4)
           (make-instance 'sql-reference-expression
             :exp1 exp1 :exp2 nil :exp3 exp3)
           (error "wrong syntax")))
        ((eq exp3 '@)
         (make-instance 'sql-reference-expression
           :exp1 exp1 :exp2 exp2 :exp3 exp4))
        ((not exp4)
         (make-instance 'sql-reference-expression
           :exp1 exp1 :exp2 exp2 :exp3 exp3))
        (t (error "wrong syntax"))))

(defun make-sql-expression (&rest exp-list)
  (case (car exp-list)
    (apply
     (apply #'make-sql-expression 
            (append (butlast (cdr exp-list))
                    (car (last exp-list)))))
    (funcall
     (apply #'make-sql-expression (cdr exp-list)))
    (#.+infix-operators+
     (apply #'sql-infix-operator-exp exp-list))
    ((in not-in)
     (apply #'sql-in-predicate-exp exp-list))
    ((and or not)
     (apply #'sql-boolean-exp exp-list))
    ((null not-null)
     (apply #'sql-operator-exp exp-list))
    (select
     (apply #'%make-select-expression (cdr exp-list)))
    ((union minus)
     (apply #'make-union-expression (car exp-list) (cdr exp-list)))
    (insert
     (destructuring-bind (&key into attributes values av-pairs query)
                         (cdr exp-list)
       (%make-insert-expression into attributes values av-pairs query))
     ;(apply #'make-insert-expression (cdr exp-list))
     )
    (* ;; * has double semantics
     (cond ((> (length exp-list) 2)
            (apply #'sql-infix-operator-exp exp-list))
           (t
            (apply #'sql-ref-exp exp-list))))
    (otherwise
     (if (find-if-not #'symbolp (cdr exp-list))
       (apply #'sql-operator-exp exp-list)
       (apply #'sql-ref-exp exp-list)))))

;; merge with sql-operator-exp?
(defun sql-boolean-exp (op &rest rest)
  (make-instance 'sql-infix-operator-expression 
    :op op 
    :exp-list 
    (let ((expressions (delete-if #'null rest)))
      (if (listp (car expressions)) ;; is this safe?
        (car expressions)
        expressions))))

(defun sql-in-predicate-exp (op exp list-or-subquery)
  (make-instance 'sql-in-predicate-expression 
    :op op 
    :exp exp
    :list-or-subquery list-or-subquery))

(defun sql-operator-exp (op &rest rest)
  (make-instance 'sql-operator-expression
    :op op
    :exp-list rest))

(defun sql-infix-operator-exp (op &rest rest)
  (make-instance 'sql-infix-operator-expression
    :op op
    :exp-list rest))

;; The sql code is written to stream
(defgeneric write-sql (sql-expression stream &key &allow-other-keys))

(defun sql (sql-expression)
  (with-output-to-string (stream)
    (write-sql sql-expression stream)))

(defmethod write-sql ((sql-expression string) stream
                                &key &allow-other-keys)
  (write-escaped-sql-string sql-expression stream))

(defmethod write-sql ((sql-expression number) stream 
                                &key &allow-other-keys)
  (format stream "~d" sql-expression))

(defun bit-vector-to-hex-string (vector)
  (let* ((bit-count (length vector))
         (byte-count (ceiling bit-count 8))
         (hex-string (make-string (* 2 byte-count))))
    (dotimes (i byte-count)
      (let ((byte1 0) (byte0 0))
        (loop for j from 0 to 3
              for index = (+ (ash i 3) j)
              while (< index bit-count)
              do (setf byte1 (logior byte1 (ash (bit vector (+ (ash i 3) j)) (- 3 j)))))
        (loop for j from 4 to 7
              for index = (+ (ash i 3) j)
              while (< index bit-count)
              do (setf byte0 (logior byte0 (ash (bit vector (+ (ash i 3) j)) (- 7 j)))))
        (setf (char hex-string (* i 2))
              (code-char (+ byte1 (if (< byte1 10)
                                      #.(char-code #\0)
                                    #.(- (char-code #\A) 10))))
              (char hex-string (+ (* i 2) 1))
              (code-char (+ byte0 (if (< byte0 10)
                                      #.(char-code #\0)
                                    #.(- (char-code #\A) 10)))))))
    hex-string))

(defmethod write-sql ((sql-expression bit-vector) stream 
                      &key &allow-other-keys)
  "The bit-vector is converted into a hex string."
  (format stream "'~d'" (bit-vector-to-hex-string sql-expression)))

(defun write-sql-string (exp stream)
  "replaces lisp-style hyphen with SQL-style underscore"
  (loop for c across exp
        do (write-char (if (char= c #\-) #\_ c) stream)))

(defmethod write-sql ((sql-expression symbol) stream 
                                &key &allow-other-keys)
  (let ((expression-string (symbol-name sql-expression)))
    (if (string= expression-string "$")
      (write-char #\? stream) ; translation of parameter marker
      (write-sql-string expression-string stream))))

(defmethod write-sql ((sql-expression null) stream 
                                &key &allow-other-keys)
  (declare (ignore sql-expression))
  (write-string "null" stream)
  nil)

(defmethod write-sql ((sql-expressions list) stream
                                &key (comma t) &allow-other-keys)
  (loop for sublist on sql-expressions
        do (write-sql (car sublist) stream :comma nil)
        when (cdr sublist)
        do (write-string (if comma "," " ") stream)))

(defmethod write-sql ((sql-expression sql-expression) stream
                                &key &allow-other-keys)
  (write-string (sql-string sql-expression) stream))

(defmethod write-sql ((sql-expression sql-select-expression) stream
                                &key toplevel-p &allow-other-keys)
  (unless toplevel-p (write-char #\( stream))
  (write-string (sql-string sql-expression) stream)
  (unless toplevel-p (write-char #\) stream)))

(defmethod write-sql ((sql-expression sql-reference-expression)
                        stream &key &allow-other-keys)
  (with-slots (sql exp1 exp2 exp3) sql-expression
    (if sql
        (write-string sql stream)
      (let ((exp1 (if (stringp exp1) 
                      (string-upcase exp1)
                    (symbol-name exp1)))
            (exp2 (cond
                   ((not exp2) nil)
                   ((stringp exp2) 
                    (string-upcase exp2))
                   (t
                    (symbol-name exp2))))
            (exp3 (cond
                   ((not exp3) nil)
                   ((stringp exp3) 
                    (string-upcase exp3))
                   (t
                    (symbol-name exp3)))))
        (write-sql-string exp1 stream)
        (when exp2
          (write-char #\. stream)
          (write-sql-string exp2 stream))
        (when exp3
          (write-char #\@ stream)
          (write-sql-string exp3 stream))))))

(defun write-escaped-sql-string (string &optional stream)
  (write-char #\' stream)
  (loop for c across string
        do (write-char c stream)
        when (char= c #\')
        do (write-char c stream))
  (write-char #\' stream))

;; better: split sql-operator-expression into unary, binary and n-ary?
(defmethod write-sql ((sql-expression sql-operator-expression)
                                stream &key &allow-other-keys)
  (with-slots (sql operator exp-list) sql-expression
    (cond (sql
           (write-string sql stream))
          (t
           (let ((exp1 (car exp-list)))
             (case operator 
               ((not null not-null) ; unary
                (write-char #\( stream)
                (when (cdr exp-list)
                  (error "~a has too many arguments: wanted 1, got ~d"
                         (length exp-list)))
                (case operator
                  (not
                   (write-string "not " stream)
                   (write-sql exp1 stream))
                  (null
                   (write-sql exp1 stream)
                   (write-string " is null" stream))
                  (not-null
                   (write-sql exp1 stream)
                   (write-string " is not null" stream))))
               ((count avg sum)
                (format stream "~a(" operator)
                (when (eq (cadr exp-list) :distinct)
                  (write-string "distinct " stream))
                (write-sql exp1 stream))
               (conc ; is this Oracle-specific?
                (write-char #\( stream)
                (make-conc-expression *default-database* exp-list stream))
               (otherwise
                (format stream "~a(" operator)
                (loop for (exp . rest) on exp-list
                      do (write-sql exp stream)
                      when rest
                      do (write-string ", " stream)))))
           (write-string ")" stream)))))

(defmethod make-conc-expression ((database t) exp-list stream)
  (loop for (exp . rest) on exp-list
        do (write-sql exp stream)
        when rest
        do (write-string " || " stream)))

#-mcl
(defmethod make-conc-expression ((database access-mixin) exp-list stream)
  (loop for (exp . rest) on exp-list
        do (write-sql exp stream)
        when rest
        do (write-string " & " stream)))

(defmethod write-sql ((sql-expression sql-infix-operator-expression)
                                stream &key &allow-other-keys)
  (with-slots (sql operator exp-list) sql-expression
    (cond (sql
           (write-string sql stream))
          (t
           (write-string "(" stream)
           (case operator
             (and ; n-ary
              (loop for sublist on exp-list
                    do (write-sql (car sublist) stream)
                    when (cdr sublist)
                    do (write-string " and " stream)))
             (or ; n-ary
              (loop for sublist on exp-list
                    do (write-sql (car sublist) stream)
                    when (cdr sublist)
                    do (write-string " or " stream))) 
             (not ; unary
              (write-string "not " stream)
              (write-sql (car exp-list) stream))
             (#.(cons '* +infix-operators+) ; binary or ternary
              (let ((ternary<= (and (eq operator '<=)
                                    (= (length exp-list) 3))))
                (let ((exp1 (car exp-list))
                      (exp2 (cadr exp-list)))
                  (when ternary<=
                    (write-sql exp2 stream)
                    (write-string " between " stream))
                  (write-sql exp1 stream)
                  (format stream " ~a " (case operator
                                          (not-like "not like")
                                          (<= (if ternary<= "and" "<="))
                                          (otherwise operator)))
                  (when (and (eq (nth 2 exp-list) :prior)
                             (eq operator '=))
                    (write-string "prior " stream))
                  (write-sql (if ternary<= (nth 2 exp-list) exp2) stream)))))
           (write-char #\) stream)))))

(defmethod write-sql ((sql-expression sql-in-predicate-expression)
                                stream &key &allow-other-keys)
  (with-slots (sql operator exp list-or-subquery) sql-expression
    (cond (sql
           (write-string sql stream))
          (t
           (write-char #\( stream)
           (write-sql exp stream)
           (write-string (if (eq operator 'in) " in " " not in ")
                         stream)
           (let ((is-list (listp list-or-subquery)))
             (when is-list (write-string "(" stream))
             (write-sql list-or-subquery stream)
             (when is-list (write-string ")" stream)))
           (write-char #\) stream)))))

(defmethod write-sql ((sql-expression sql-union-expression)
                                stream &key &allow-other-keys)
  (with-slots (sql operator exp-list order-exp) sql-expression
    (cond (sql
           (write-string sql stream))
          (t
           #+lispworks??
           (write-string "(select * from " stream)
           (write-char #\( stream)
           (loop for sublist on exp-list
                 do (write-sql (car sublist) stream)
                 when (cdr sublist)
                 do (write-string (ecase operator
                                    (union " union ")
                                    (minus " minus "))
                                  stream))
           (write-char #\) stream)
           #+lispworks??
           (write-char #\) stream)
           (when order-exp
             (write-string " order by " stream)
             (write-sql order-exp stream))))))

(defun build-sql-string (sql-expression)
  (with-output-to-string (stream)
    (write-sql sql-expression stream)))

;; to do: use destructuring-bind;
(defun %make-select-expression (&rest expressions)
  (let* ((from-exp nil)
         (where-exp nil)
         (group-by-exp nil)
         (order-exp nil)
         (having-exp nil)
         (distinct-p nil)
         (all-p nil)
         (using-exp nil)
         (set-op-exp nil)
         (joins-exp nil)
         (start-with-exp nil)
         (connect-by-exp nil))
    (make-instance 'sql-select-expression
      :sql-string
      (with-output-to-string (stream)
        ;; collect keyword expressions
        (loop for key-list on (loop for sublist on expressions
                                    until (keywordp (car sublist))
                                    finally (return sublist))
              by #'cddr
              do (let ((exp (cadr key-list)))
                   (case (car key-list)
                     (:from (setf from-exp exp))
                     (:where (setf where-exp exp))
                     (:group-by (setf group-by-exp exp))
                     (:having (setf having-exp exp))
                     (:order-by (setf order-exp exp))
                     (:distinct (setf distinct-p exp))
                     (:using (setf using-exp exp))
                     (:set-operation (setf set-op-exp exp))
                     (:all (setf all-p exp))
                     (:joins (setf joins-exp exp))
                     (:start-with (setf start-with-exp exp))
                     (:connect-by (setf connect-by-exp exp))
                     (:flatp nil)
                     (:database nil)
                     (otherwise (error "wrong keyword ~a" (car key-list))))))
        ;; select
        (write-string "select " stream)
        (cond (all-p (write-string "all " stream))
              (distinct-p (write-string "distinct " stream)))
        (loop for sublist on expressions
              until (keywordp (car sublist))
              unless (eq sublist expressions)
              do (write-string "," stream)
              do (write-sql (car sublist) stream)) 
        ;; from
        (write-string " from " stream)
        (write-sql from-exp stream)
        ;; where
        (when where-exp
          (write-string " where " stream)
          (write-sql where-exp stream))
        ;; start with + connect by (Oracle)
        (when start-with-exp
          (write-string " start with " stream)
          (write-sql start-with-exp stream))
        (when connect-by-exp
          (write-string " connect by " stream)
          (write-sql connect-by-exp stream))
        ;; group by
        (when group-by-exp
          (write-string " group by " stream)
          (write-sql group-by-exp stream))
        ;; having
        (when having-exp
          (write-string " having " stream)
          (write-sql having-exp stream))
        joins-exp
        set-op-exp
        using-exp
        ;; end of allowed subquery expressions
        ;; order by
        (when order-exp
          (write-string " order by " stream)
          (write-sql order-exp stream))))))

(defmethod make-select-expression ((database database) &rest expressions)
  (with-output-to-string (stream)
    (write-sql (apply #'%make-select-expression expressions) stream
               :toplevel-p t)))

(defun make-union-expression (operator &rest expressions)
  (make-instance 'sql-union-expression
    :op operator
    :exp-list (cond ((listp (car expressions))
                     (car expressions))
                    ((not (or (find :order-by expressions)
                              (find :flatp expressions)))
                     expressions)
                    (t
                     (loop for exp in expressions
                           until (member exp '(:order-by :flatp))
                           collect exp)))
    :order-exp (cadr (member :order-by expressions))))

(defmethod make-insert-expression ((database database) into attributes 
                                       values av-pairs query &optional keys)
  (with-output-to-string (stream)
    (write-sql (%make-insert-expression into attributes values av-pairs
                                        query keys)
               stream :toplevel-p t)))

(defun %make-insert-expression (into attributes values av-pairs query
                                         &optional keys)
  (declare (ignore keys))
  (let ((columns ()))
    (make-instance 'sql-insert-expression
      :sql-string
      (with-output-to-string (stream)
        (write-string (or (getf (db-plist *default-database*) :insert-string)
                          "insert into ") stream)
        (write-sql into stream)
        (when (and (or values av-pairs) query)
          (error "Values and a query expression may not occur in the same insert statement."))
        (when (not (or values av-pairs query))
          (error "You must specify values or a query."))
        (when attributes
          (when (and (not query) (/= (length attributes) (length values)))
            (error "You have to provide as many values as there are attributes."))
          (write-string " (" stream)
          (write-sql attributes stream)
          (write-string (if av-pairs "," ") ") stream))
        (when av-pairs
          (when (/= (length attributes) (length values))
            (error "You have to provide as many values as there are attributes."))
          (unless attributes (write-string " (" stream))
          (loop for ((att val) . rest) on av-pairs
                when (eq val '$)
                do (push att columns)
                do (write-sql att stream)
                while rest
                do (write-char #\, stream)
                finally (write-string ") " stream)))
        (cond (query
               (write-sql query stream))
              (t
               (write-string " values " stream)
               (when values
                 (write-char #\( stream)
                 (write-sql values stream)
                 (write-string (if av-pairs "," ") ") stream)
                 (if attributes ;; collect parameterized columns
                   (loop for val in values
                         and att in attributes
                         when (eq val '$)
                         do (push att columns))
                   (loop with i = 0 ; here we don't know the column names.
                         for val in values
                         when (eq val '$)
                         do (push i columns)
                         do (incf i))))
               (when av-pairs
                 (unless values (write-char #\( stream))
                 (loop for ((att val) . rest) on av-pairs
                       do (progn att ;; to avoid a warning about unused var 
                                 (write-sql val stream))
                       while rest
                       do (write-char #\, stream)
                       finally (write-char #\) stream))))))
      :table into
      :parameter-columns (nreverse columns)
      ;:database database 
      )))


(defmethod make-update-expression ((database database) table attributes 
                                       values av-pairs where &optional keys)
  (declare (ignore keys))
  (make-instance 'sql-update-expression
    :sql-string
    (with-output-to-string (stream)
      (write-string "update " stream)
      (write-sql table stream)
      (when (not (or values av-pairs where))
        (error "You must specify values or a query."))
      (write-string " set " stream)
      (when attributes
        (when (and (not where) (/= (length attributes) (length values)))
          (error "You have to provide as many values as there are attributes."))
        (loop for (att . rest) on attributes
              for val in values
              do
              (write-sql att stream)
              (write-string " = " stream)
              (write-sql val stream)
              while (or rest av-pairs)
              do (write-char #\, stream)))
      (when av-pairs
        (when (/= (length attributes) (length values))
          (error "You have to provide as many values as there are attributes."))
        (loop for ((att val) . rest) on av-pairs
              do
              (write-sql att stream)
              (write-string " = " stream)
              (write-sql val stream)
              while rest
              do (write-char #\, stream)))
      (when where 
        (write-string " where " stream)
        (write-sql where stream)))
    ;:database database
    ))

(defmethod make-delete-expression ((database database) table where
                                       &optional keys)
  (declare (ignore keys))
  (make-instance 'sql-delete-expression
    :sql-string
    (with-output-to-string (stream)
      (write-string "delete from " stream)
      (write-sql table stream)
      (when where 
        (write-string " where " stream)
        (write-sql where stream)))
    ;:database database
    ))

(defmethod make-create-table-expression ((database database)
                                              name description &optional keys)
  (declare (ignore keys))
  (with-output-to-string (stream)
    (write-string "create table " stream)
    (write-sql name stream)
    (write-string " (" stream)
    (loop for columns on description
          and column in description
          do
          (cond 
           ((eq (car column) :foreign-key)
            (destructuring-bind (cols ref-keyword foreign-table
                                      foreign-cols) (cdr column)
              (unless (eq ref-keyword :references)
                (error "Keyword :references expected."))
              (write-string "foreign key (" stream)
              (write-sql cols stream)
              (write-string ") references " stream)
              (write-sql foreign-table stream)
              (write-string " (" stream)
              (write-sql foreign-cols stream)
              (write-char #\) stream)))
           ((eq (car column) :primary-key)
            (write-string "primary key (" stream)
            (write-sql (cadr column) stream) ;; fixed 8.5
            (write-char #\) stream))
           ((eq (car column) :unique) ;; added 8.5
            (write-string "unique (" stream)
            (write-sql (cadr column) stream)
            (write-char #\) stream))
           (t
            ;; change this: a type may be composed of more than one symbol
            (destructuring-bind (name type &optional null-or-primary?)
                                column
              (write-sql name stream)
              (write-char #\Space stream)
              (cond ((consp type)
                     (write-sql (car type) stream)
                     (write-char #\( stream)
                     (write-sql (cadr type) stream)
                     (write-char #\) stream))
                    (t
                     (write-sql type stream)))
              (when (and null-or-primary?
                         (not (member null-or-primary? 
                                      '(:primary-key :not-null))))
                (error "Keyword ~s not allowed here." null-or-primary?))
              (when null-or-primary?
                (write-string " not null" stream))
              (when (eq null-or-primary? :primary-key)
                (write-string " primary key" stream)))))
          while (cdr columns)
          do (write-string ", " stream))
    (write-char #\) stream)))

(defmethod make-drop-table-expression ((database database) name)
  (with-output-to-string (stream)
    (write-string "drop table " stream)
    (write-sql name stream)))  

(defmethod make-create-index-expression ((database database)
                                              index on unique attributes 
                                              &key &allow-other-keys)
  (declare (ignore rest))
  (with-output-to-string (stream)
    (write-string "create " stream)
    (when unique (write-string "unique " stream))
    (write-string "index " stream)
    (write-sql index stream)
    (write-string " on " stream)
    (write-sql on stream)
    (write-string " (" stream)
    (write-sql attributes stream)
    (write-char #\) stream)))

(defmethod make-drop-index-expression ((database database) index)
  (with-output-to-string (stream)
    (write-string "drop index " stream)
    (write-sql index stream)))

(defmethod make-create-user-expression ((database database) username
                                             password &key &allow-other-keys)
  (with-output-to-string (stream)
    (write-string "create user " stream)
    (write-sql username stream)
    (write-string " identified by " stream)
    (write-string password stream)))

(defmethod make-create-view-expression ((database database) name
                                             alias replace-p force-p as
                                             check-option &key &allow-other-keys)
  (with-output-to-string (stream)
    (write-string "create " stream)
    (when replace-p (write-string "or replace " stream))
    (when force-p (write-string "force " stream)) ; noforce is the default, I suppose
    (write-string "view " stream)
    (write-sql name stream)
    (when alias 
      (write-string " (" stream)
      (write-sql alias stream)
      (write-char #\) stream))
    (write-string " as " stream)
    (write-sql as stream)
    (when check-option 
      (write-string " with check option" stream))))

(defmethod make-drop-view-expression ((database database) name)
  (with-output-to-string (stream)
    (write-string "drop view " stream)
    (write-sql name stream)))  

;; we have to take out :database which is a real keyword
(defun make-generic-execute-expression (&rest rest)
  (with-output-to-string (stream)
    (dolist (exp rest)
      (typecase exp
        (keyword 
         (loop for c across (string exp)
               do (write-char (if (char= c #\-) 
                                #\Space
                                (char-downcase c))
                              stream)))
        (t (write-sql exp stream)))
      (write-char #\Space stream))))

#|

(make-generic-execute-expression
 :create-user "paul" :identified-by "gvprckvnis"
 :default-tablespace [usr]
 :temporary-tablespace [tmp])

(make-generic-execute-expression
 :create-public-synonym [rigus-ackerschwaan]
 :for [paul rigus @ remote-base])

|#