;;;-*- Mode: Lisp; Package: DTF -*-

;; SQL module for MCL; DTF interface
;; Version 0.7
;; (C) Paul Meurer 1999
;; paul.meurer@hit.uib.no
;;
;; Thanks to David Lamkins for valuable help!

;; there remains a lot to do.

(defpackage "DTF"
  (:use "SQL" "COMMON-LISP" "CCL")
  (:import-from "SQL"
                "DB-DISCONNECT" "DB-COMMIT" "DB-ROLLBACK" "DB-QUERY"
                "DB-EXECUTE-COMMAND" "DB-QUERY-OBJECT"
                "%DB-EXECUTE" "%INITIALIZE-QUERY" 
                "%DISPOSE-QUERY-VARS" "%READ-QUERY-DATA"
                "WITH-BLOB"))

(in-package :dtf)

;; constants

;; from dtflvl1.h

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defconstant $dtFcolInfoSize 64)  ; SCresRecEnum: max # of res.col
  (defconstant $dtFnameSize 25)     ; SCresRecEnum: name string size
  ; includes trailing null
  (defconstant $dtFpwSize 17)       ; Length of password; includes trailing null
  
  (defconstant $dtFcolTypeDecimal     #x0000)  ; Decimal type
  (defconstant $dtFcolTypeByte        #x0f00)  ; Byte type
  (defconstant $dtFcolTypeWord        #x0f01)  ; Word type
  (defconstant $dtFcolTypeLongWord    #x0f02)  ; LongWord type
  (defconstant $dtFcolTypeChar        #x0f03)  ; Character type
  (defconstant $dtFcolTypeShort       #x0f04)  ; ShortInteger type
  (defconstant $dtFcolTypeLong        #x0f05)  ; LongInteger type
  (defconstant $dtFcolTypeReal        #x0f06)  ; Real Type
  (defconstant $dtFcolTypeShortString #x0f07)  ; ShortString type
  (defconstant $dtFcolTypeHugeString  #x0f08)  ; HugeString type
  (defconstant $dtFcolTypeDate        #x0f09)  ; Date type
  (defconstant $dtFcolTypeTime        #x0f0a)  ; Time type
  (defconstant $dtFcolTypeBit         #x0f80)  ; Raw blob type
  (defconstant $dtFcolTypeUndefined   #x0fff)  ; Undefined type
  )

(defun describe-type (type)
  (ecase type
    (#.$dtFcolTypeDecimal      "Decimal type")
    (#.$dtFcolTypeByte         "Byte type")
    (#.$dtFcolTypeWord         "Word type")
    (#.$dtFcolTypeLongWord     "LongWord type")
    (#.$dtFcolTypeChar         "Character type")
    (#.$dtFcolTypeShort        "ShortInteger type")
    (#.$dtFcolTypeLong         "LongInteger type")
    (#.$dtFcolTypeReal         "Real Type")
    (#.$dtFcolTypeShortString  "ShortString type")
    (#.$dtFcolTypeHugeString   "HugeString type")
    (#.$dtFcolTypeDate         "Date type")
    (#.$dtFcolTypeTime         "Time type")
    (#.$dtFcolTypeBit          "Raw blob type")
    (#.$dtFcolTypeUndefined    "Undefined type")))

#+ignore
(defun map-type (type)
  (ecase type
    (#.$dtFcolTypeDecimal      "Decimal type")
    (#.$dtFcolTypeByte         :byte)
    (#.$dtFcolTypeWord         :word)
    (#.$dtFcolTypeLongWord     :long)
    (#.$dtFcolTypeChar         :byte)
    (#.$dtFcolTypeShort        "ShortInteger type")
    (#.$dtFcolTypeLong         "LongInteger type")
    (#.$dtFcolTypeReal         "Real Type")
    (#.$dtFcolTypeShortString  "ShortString type")
    (#.$dtFcolTypeHugeString   "HugeString type")
    (#.$dtFcolTypeDate         "Date type")
    (#.$dtFcolTypeTime         "Time type")
    (#.$dtFcolTypeBit          "Raw blob type")
    (#.$dtFcolTypeUndefined    "Undefined type")))

;; from dtflvl2.h

(eval-when (load compile eval)
  (defconstant $dtF2moveStart   0)
  (defconstant $dtF2moveEnd     1)
  (defconstant $dtF2moveCurrent 2)
  
  (define-entry-point ("dtFcreateDatabase" ("dtFPPCSV1"))
    ((string :ptr))
    :word)
  
  (define-entry-point ("dtFMacSetDatabasePath" ("dtFPPCSV1"))
    ((fixnum :short)
     (fixnum :long)
     (string :ptr)
     (fixnum :short)
     (fixnum :long)
     (string :ptr))
    :word)
  
  (define-entry-point ("dtF2ident" ("dtFPPCSV1")) 
    ((string :ptr) (string :ptr))
    :word)
  
  (define-entry-point ("dtF2start" ("dtFPPCSV1")) 
    ()
    :word)
  
  (define-entry-point ("dtF2stop" ("dtFPPCSV1")) 
    ()
    :word)
  
  (define-entry-point ("dtF2exec" ("dtFPPCSV1")) 
    ((string :ptr))
    :word)
  
  (define-entry-point ("dtF2execWS" ("dtFPPCSV1")) 
    ((fixnum :word)
     (string :ptr))
    :word)
  
  (define-entry-point ("dtF2rows" ("dtFPPCSV1"))
    ()
    :long)
  
  (define-entry-point ("dtF2rowsWS" ("dtFPPCSV1"))
    ((fixnum :word))
    :long)
  
  (define-entry-point ("dtF2cols" ("dtFPPCSV1"))
    ()
    :long)
  
  (define-entry-point ("dtF2colsWS" ("dtFPPCSV1"))
    ((fixnum :word))
    :long)
  
  #+not-used
  (define-entry-point ("dtF2cinfo" ("dtFPPCSV1"))
    ((fixnum :byte))
    :ptr)
  
  (define-entry-point ("dtF2cinfoP" ("dtFPPCSV1"))
    ((fixnum :byte)
     (t :ptr))
    :word)
  
  (define-entry-point ("dtF2cinfoPWS" ("dtFPPCSV1"))
    ((fixnum :word)
     (fixnum :byte)
     (t :ptr))
    :word)
  
  (define-entry-point ("dtF2resultAvailable" ("dtFPPCSV1")) 
    ()
    :boolean)
  
  (define-entry-point ("dtF2resultAvailableWS" ("dtFPPCSV1")) 
    ((fixnum :word))
    :boolean)
  
  (define-entry-point ("dtF2move" ("dtFPPCSV1"))
    ((fixnum :word)
     (fixnum :signed-long))
    :word)
  
  (define-entry-point ("dtF2moveWS" ("dtFPPCSV1"))
    ((fixnum :word)
     (fixnum :word)
     (fixnum :signed-long))
    :word)
  
  (define-entry-point ("dtF2setBlob" ("dtFPPCSV1"))
    ((t :ptr)
     (fixnum :long))
    :ptr)
  
  (define-entry-point ("dtF2setBlobWS" ("dtFPPCSV1"))
    ((fixnum :word)
     (t :ptr)
     (fixnum :long))
    :ptr)
  
  (define-entry-point ("dtF2getFieldInfo" ("dtFPPCSV1"))
    ((fixnum :byte)
     (t :ptr)
     (t :ptr))
    :word)
  
  (define-entry-point ("dtF2getFieldInfoWS" ("dtFPPCSV1"))
    ((fixnum :word)
     (fixnum :byte)
     (t :ptr)
     (t :ptr))
    :word)
  
  (define-entry-point ("dtF2getField" ("dtFPPCSV1"))
    ((fixnum :byte)
     (t :ptr)
     (fixnum :long)
     (t :ptr)
     (t :ptr))
    :word)
  
  (define-entry-point ("dtF2getFieldWS" ("dtFPPCSV1"))
    ((fixnum :word)
     (fixnum :byte)
     (t :ptr)
     (fixnum :long)
     (t :ptr)
     (t :ptr))
    :word)
  
  (define-entry-point ("dtFgetError" ("dtFPPCSV1")) 
    ((fixnum :long))
    :ptr)
  
  (define-entry-point ("dtF2getWS" ("dtFPPCSV1")) 
    ()
    :word)
  
  (define-entry-point ("dtF2releaseWS" ("dtFPPCSV1"))
    ((fixnum :word))
    :word)
  
  (define-entry-point ("dtF2getDefaultWS" ("dtFPPCSV1")) 
    ()
    :word)
  
  (define-entry-point ("dtF2setDefaultWS" ("dtFPPCSV1")) 
    ((fixnum :word))
    :word)

  (define-entry-point ("dtFconvtFromDate" ("dtFPPCSV1"))
    ((fixnum :long)
     (t :ptr)
     (t :ptr)
     (t :ptr)))

  (define-entry-point ("dtFconvtFromTime" ("dtFPPCSV1"))
    ((fixnum :long)
     (t :ptr)
     (t :ptr)
     (t :ptr)))
  )

(defun %column-count (&optional workspace)
  (if workspace
    (dtf2colsWS workspace)
    (dtf2cols)))

(defun %row-count (&optional workspace)
  (if workspace
    (dtf2rowsWS workspace)
    (dtf2rows)))

;; thanks to Bruno Haible!
(defmacro collecting (&body forms)
  (let ((a (gensym))
        (b (gensym))
        (c (gensym)))
    `(let ((,a nil) (,b nil))
       (macrolet ((collect (form)
                    `((lambda (,',c)
                        (if ,',a
                          (setf (cdr ,',b) (setf ,',b ,',c))
                          (setf ,',a (setf ,',b ,',c))))
                      (list ,form))))
         (progn ,@forms)
         ,a ))))

(defmacro collect (form)
  (declare (ignore form))
  (error "collect used outside of collecting"))

(defclass dtf-database (database)
  ((info :initform (make-hash-table) :reader db-info) ; not used yet
   (workspace :initform nil :accessor workspace)))

(defclass dtf-query (query)
  ((active-p :initform nil :initarg :active-p :accessor query-active-p)
   (workspace :initform nil :accessor workspace) ; twice??
   (cursor-position :initform nil :accessor cursor-position)
   (column-count :initform nil :accessor column-count)
   (row-count :initform nil :accessor row-count)
   (table-names :initform (make-array 0 :element-type 'string :adjustable t :fill-pointer t)
                 :accessor table-names)
   (column-names :initform (make-array 0 :element-type 'string :adjustable t :fill-pointer t)
                 :accessor column-names)
   (column-types :initform (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t)
                   :accessor column-types)))

(defmethod initialize-instance :after ((database dtf-database) 
                                           &key &allow-other-keys)
  (with-slots (query) database
    (setf query (make-instance 'dtf-query))))

;; (defun create-database ...)

(defun get-error (error-code)
  (ccl::%get-cstring (dtfgeterror error-code)))

(defun catch-error (errno &optional desc)
  (unless (zerop errno)
    (format *standard-output* "~%~a (~a)" 
            (get-error errno)
            (or desc "?")))
  errno)

(defun logon (username password)
  (catch-error
   (ccl::with-cstr (username-ptr username)
     (ccl::with-cstr (password-ptr password)
       (dtF2ident username-ptr password-ptr)))))

(defmethod db-connect ((db-type (eql :dtf)) db-file user-id password autocommit)
  (let ((db (make-instance 'dtf-database 
              :db-type :dtf
              :db-name db-file
              :user-id user-id
              :password password)))
    (db-connect db db-file user-id password autocommit)))

(defun start-database ()
  (catch-error (dtf2start)))

(defun stop-database ()
  (catch-error (dtf2stop)))

(defmethod db-connect ((db dtf-database) db-file user-id password autocommit)
  (declare (ignore autocommit))
  (let ((db (make-instance 'dtf-database 
              :db-type :dtf
              :db-name db-file
              :user-id user-id
              :password password)))
    (choose-database (concatenate 'string db-file ".dtf")
                     (concatenate 'string db-file ".bit"))
    (logon user-id password)
    (start-database)
    db))

(defmethod db-disconnect ((db dtf-database))
  (stop-database))

(defun get-path-refnum+dirid (path &optional no-error)
  (unless (and no-error
               (not (ccl::directory-exists-p path)))
    (ccl::%stack-iopb (pb np)
      (ccl::%path-GetDirInfo path pb :errchk)
      (values (%get-signed-word pb $ioVRefNum)
              (%get-signed-long pb $ioDirID)
              (mac-file-namestring path)))))

(defun choose-database (&optional db-path blob-path)
  (let ((db-path (or db-path (choose-file-dialog)))
        (blob-path (or blob-path (choose-file-dialog))))
    (multiple-value-bind (db-refnum db-dirid db-name)
                         (get-path-refnum+dirid db-path)
      (multiple-value-bind (blob-refnum blob-dirid blob-name)
                           (get-path-refnum+dirid blob-path)
      (ccl::with-cstr (db-name-ptr db-name)
        (ccl::with-cstr (blob-name-ptr blob-name)
          (catch-error
           (dtfmacsetdatabasepath db-refnum db-dirid db-name-ptr 
                                  blob-refnum blob-dirid blob-name-ptr)
           "choose-database")))))))

(defmacro with-blob ((blob-ptr-or-hdl &optional workspace) &body body)
  (let ((blob-ptr (gensym))
        (blob-length (gensym))
        (ws (gensym))
        (ptr-or-hdl (gensym)))
    `(let ((,ws ,workspace)
           (,ptr-or-hdl ,blob-ptr-or-hdl))
       (assert (macptrp ,ptr-or-hdl))
       (let ((,blob-length (if (handlep ,ptr-or-hdl)
                             (require-trap #_GetHandleSize ,ptr-or-hdl)
                             (require-trap #_GetPtrSize ,ptr-or-hdl))))
         (with-pointers ((,blob-ptr ,ptr-or-hdl))
           (if ,ws
             (dtf2setblobws ,ws ,blob-ptr ,blob-length)
             (dtf2setblob ,blob-ptr ,blob-length))
           (unwind-protect
             (progn ,@body)
             (if ,ws
               (dtf2setblobws ,ws (%null-ptr) 0)
               (dtf2setblob (%null-ptr) 0))))))))

(defmethod db-execute-command ((database dtf-database) sql-expression)
  (unless (eq *active-transactions* t)
    (pushnew database *active-transactions*))
  (with-slots (workspace) database
    (ccl::with-cstr (sqlstr sql-expression)
      (catch-error
       (if workspace 
         (dtF2execws workspace sqlstr)
         (dtf2exec sqlstr))
       "query"))))

(defmethod db-commit ((database dtf-database))
  (db-execute-command database "commit"))

(defmethod db-query ((database dtf-database) query-expression &optional flatp)
  (retrieve-records query-expression (workspace database) flatp))

(defmacro r-cstr-ref (record accessor)
  (multiple-value-bind (offset field-type)
                       (ccl::parse-accessor accessor)
    (let ((end (gensym))
          (max-size (caddr field-type)))
      (assert (and (eq (car field-type) :array)
                   (eq (cadr field-type) :byte)
                   (fixnump max-size)))
      `(let* ((,end ,offset))
         (with-pointers ((p ,record))
           (loop (if (or (ccl::%izerop (%get-byte p ,end))
                         (= ,end (ccl::%i+ ,max-size ,offset))) ; to be sure
                   (return)
                   (setq ,end (ccl::%i+ ,end 1))))
           (%str-from-ptr-in-script (%incf-ptr p ,offset) 
                                    (ccl::%i- ,end ,offset)))))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defrecord (info-entry :ptr)
    (table  (:array :byte #.(+ $dtFnameSize 3)))
    (column (:array :byte #.(+ $dtFnameSize 3)))
    (type :word)
    (full-type :word)))

(defun get-field-info (column &optional workspace)
  (rlet ((info :info-entry))
    (if workspace 
      (dtf2cinfopws workspace column info)
      (dtf2cinfop column info))
    (values (r-cstr-ref info info-entry.table)
            (r-cstr-ref info info-entry.column)
            (rref info info-entry.type)
            (rref info info-entry.full-type))))

(defun result-available-p (&optional workspace)
  (if workspace 
    (dtf2resultavailablews workspace)
    (dtf2resultavailable)))

;; should be macro?
(defun move-cursor (move-type where &optional workspace)
  (let ((type (ecase move-type
                (:start #.$dtF2moveStart)
                (:current #.$dtF2moveCurrent)
                (:end #.$dtF2moveEnd))))
    (if workspace
      (dtf2movews workspace type where)
      (dtf2move type where))))

(defun print-dtf-time (time stream depth)
  (declare (ignore depth))
  (format stream "~2,'0D:~2,'0D:~2,'0D"
          (dtf-time-hour time)
          (dtf-time-minute time)
          (dtf-time-second time)))

(defstruct (dtf-time (:print-function print-dtf-time))
  (hour 0)
  (minute 0)
  (second 0))

(defun print-dtf-date (date stream depth)
  (declare (ignore depth))
  (format stream "~4,'0D/~2,'0D/~2,'0D"
          (dtf-date-year date)
          (dtf-date-month date)
          (dtf-date-day date)))

(defstruct (dtf-date (:print-function print-dtf-date))
  (year 1904)
  (month 1)
  (day 1))

;; to do: assign (%null-ptr) to a constant (in an eval-when :load-toplevel etc.
;; form)

;; RETRIEVE-FIELD does not yet handle:
;;   $dtFcolTypeDecimal

#-:ccl-4 (require "MACPTR-TERMINATION")

(defun retrieve-field (col type &optional workspace)
  (ecase type
    (#.$dtFcolTypeShortString
     ;; The DB allows embedded zeroes in the string.  We construct a Pascal
     ;; string from the DB contents, and convert that to a Lisp string.
     (%stack-block ((short-string 256)
                    (field-len #.(ccl::record-field-length :long)))
       (catch-error
        (if workspace
          (dtf2getfieldws workspace col (%inc-ptr short-string 1) 255 field-len (%null-ptr))
          (dtf2getfield col (%inc-ptr short-string 1) 255 field-len (%null-ptr)))
        "retrieve-field: short string")
       (ccl::%set-byte short-string (%get-long field-len))
       (if (zerop (%get-long field-len)) nil (%get-string short-string))))
    (#.$dtFcolTypeHugeString
     ;; We ignore the problem of embedded zeroes for this case.
     (%stack-block ((field-len #.(ccl::record-field-length :long)))
       (if workspace
         (dtf2getfieldinfows workspace col field-len (%null-ptr))
         (dtf2getfieldinfo col field-len (%null-ptr)))
       (let ((field-length (%get-long field-len)))
         (%stack-block ((huge-string field-length)
                        (field-len #.(ccl::record-field-length :long)))
           (catch-error 
            (if workspace
              (dtf2getfieldws workspace col huge-string field-length field-len (%null-ptr))
              (dtf2getfield col huge-string field-length field-len (%null-ptr)))
            "retrieve-field: huge string")
           (if (zerop (%get-long field-len)) nil (%get-cstring huge-string))))))
    (#.$dtFcolTypeBit
     (%stack-block ((field-len #.(ccl::record-field-length :long)))
       (catch-error
        (if workspace
          (dtf2getfieldinfows workspace col field-len (%null-ptr)) 
          (dtf2getfieldinfo col field-len (%null-ptr)))
        "retrieve-field/info: bit")
       (let* ((field-length (%get-long field-len))
              (buffer-hdl (#_NewHandle field-length)))
         (unless (%null-ptr-p buffer-hdl)
           #+:ccl-4 (terminate-when-unreachable buffer-hdl)
           #-:ccl-4 (make-terminable-macptr buffer-hdl #'terminable-macptr-p)
           (%stack-block ((field-len #.(ccl::record-field-length :long)))
             (with-dereferenced-handles ((buffer-ptr buffer-hdl))
               (catch-error
                (if workspace
                  (dtf2getfieldws workspace col buffer-ptr field-length field-len (%null-ptr))
                  (dtf2getfield col buffer-ptr field-length field-len (%null-ptr)))
                "retrieve-field: bit"))))
         (if (zerop (%get-long field-len)) nil buffer-hdl))))
    (#.$dtFcolTypeReal
     (%stack-block ((real #.(ccl::record-field-length :double-float))
                    (field-len #.(ccl::record-field-length :long))
                    (col-type #.(ccl::record-field-length :word)))
       (catch-error 
        (if workspace 
          (dtf2getfieldws workspace col real #.(ccl::record-field-length :long) field-len col-type)
          (dtf2getfield col real #.(ccl::record-field-length :double-float) field-len col-type))
        "retrieve-field: real")
       (if (zerop (%get-long field-len)) nil (%get-double-float real))))
    (#.$dtFcolTypeDate
     (%stack-block ((date #.(ccl::record-field-length :long))
                    (field-len #.(ccl::record-field-length :long))
                    (col-type #.(ccl::record-field-length :word)))
       (catch-error 
        (if workspace
          (dtf2getfieldws workspace col date #.(ccl::record-field-length :long) field-len col-type)
          (dtf2getfield col date #.(ccl::record-field-length :long) field-len col-type))
        "retrieve-field: date")
       (%stack-block ((day #.(ccl::record-field-length :word))
                      (month #.(ccl::record-field-length :word))
                      (year #.(ccl::record-field-length :word)))
         (dtfconvtfromdate (%get-long date) day month year)
         (if (zerop (%get-long field-len)) nil (make-dtf-date :year (%get-word year)
                                                              :month (%get-word month)
                                                              :day (%get-word day))))))
    (#.$dtFcolTypeTime
     (%stack-block ((time #.(ccl::record-field-length :long))
                    (field-len #.(ccl::record-field-length :long))
                    (col-type #.(ccl::record-field-length :word)))
       (catch-error
        (if workspace
          (dtf2getfieldws workspace col time #.(ccl::record-field-length :long) field-len col-type)
          (dtf2getfield col time #.(ccl::record-field-length :long) field-len col-type))
        "retrieve-field: time")
       (%stack-block ((second #.(ccl::record-field-length :word))
                      (minute #.(ccl::record-field-length :word))
                      (hour #.(ccl::record-field-length :word)))
         (dtfconvtfromtime (%get-long time) second minute hour)
         (if (zerop (%get-long field-len)) nil (make-dtf-time :hour (%get-word hour)
                                                              :minute (%get-word minute)
                                                              :second (%get-word second))))))
    (#.$dtFcolTypeChar
     (%stack-block ((char #.(ccl::record-field-length :byte))
                    (field-len #.(ccl::record-field-length :long))
                    (col-type #.(ccl::record-field-length :word)))
       (catch-error
        (if workspace
          (dtf2getfieldws workspace col char #.(ccl::record-field-length :long) field-len col-type)
          (dtf2getfield col char #.(ccl::record-field-length :long) field-len col-type))
        "retrieve-field: char")
       (if (zerop (%get-long field-len)) nil (code-char (%get-unsigned-byte char)))))
    (#.$dtFcolTypeByte
     (%stack-block ((byte #.(ccl::record-field-length :byte))
                    (field-len #.(ccl::record-field-length :long))
                    (col-type #.(ccl::record-field-length :word)))
       (catch-error 
        (if workspace
          (dtf2getfieldws workspace col byte #.(ccl::record-field-length :long) field-len col-type)
          (dtf2getfield col byte #.(ccl::record-field-length :long) field-len col-type))
        "retrieve-field: byte")
       (if (zerop (%get-long field-len)) nil (%get-byte byte))))
    (#.$dtFcolTypeShort
     (%stack-block ((short #.(ccl::record-field-length :word))
                    (field-len #.(ccl::record-field-length :long))
                    (col-type #.(ccl::record-field-length :word)))
       (catch-error 
        (if workspace
          (dtf2getfieldws workspace col short #.(ccl::record-field-length :long) field-len col-type)
          (dtf2getfield col short #.(ccl::record-field-length :long) field-len col-type))
        "retrieve-field: short")
       (if (zerop (%get-long field-len)) nil (%get-signed-word short))))
    (#.$dtFcolTypeWord
     (%stack-block ((word #.(ccl::record-field-length :word))
                    (field-len #.(ccl::record-field-length :long))
                    (col-type #.(ccl::record-field-length :word)))
       (catch-error
        (if workspace
          (dtf2getfieldws workspace col word #.(ccl::record-field-length :long) field-len col-type)
          (dtf2getfield col word #.(ccl::record-field-length :long) field-len col-type))
        "retrieve-field: word")
       (if (zerop (%get-long field-len)) nil (%get-word word))))
    (#.$dtFcolTypeLong
     (%stack-block ((long #.(ccl::record-field-length :long))
                    (field-len #.(ccl::record-field-length :long))
                    (col-type #.(ccl::record-field-length :word)))
       (catch-error
        (if workspace
          (dtf2getfieldws workspace col long #.(ccl::record-field-length :long) field-len col-type)
          (dtf2getfield col long #.(ccl::record-field-length :long) field-len col-type))
        "retrieve-field: long")
       (if (zerop (%get-long field-len)) nil (%get-long long))))
    (#.$dtFcolTypeLongWord
     (%stack-block ((long #.(ccl::record-field-length :long))
                    (field-len #.(ccl::record-field-length :long))
                    (col-type #.(ccl::record-field-length :word)))
       (catch-error
        (if workspace
          (dtf2getfieldws workspace col long #.(ccl::record-field-length :long) field-len col-type)
          (dtf2getfield col long #.(ccl::record-field-length :long) field-len col-type))
        "retrieve-field: long word")
       (if (zerop (%get-long field-len)) nil (%get-unsigned-long long))))))

#| ;; David Lambkins:
;; This section explains why Paul's original code didn't work in MCL 4.2.
;; A compiler bug tripped by this code is avoided by the above code.
;; The bug itself is fixed by the ffcall-xlong-patch.

(defun retrieve-field (col type)
  (ecase type
    (#.$dtFcolTypeShortString
     (%stack-block ((short-string 256)
                    (field-len #.(ccl::record-field-length :long)))
       (dtf2getfield col short-string 256 field-len (%null-ptr))
       (ccl::%get-cstring short-string)))
    #+broken
    (#.$dtFcolTypeHugeString ;; does not work
     (%stack-block ((field-len #.(ccl::record-field-length :long)))
       (dtf2getfieldinfo col field-len (%null-ptr))
       (let ((field-length (ccl::%get-long field-len)))
         (%stack-block ((huge-string field-length)
                        (field-len #.(ccl::record-field-length :long)))
           (dtf2getfield col huge-string field-length field-len (%null-ptr))
           (ccl::%get-long field-len)
           (ccl::%get-cstring huge-string)))))
    #-broken
    (#.$dtFcolTypeHugeString ;; this works
     (%stack-block ((field-len #.(ccl::record-field-length :long)))
       (dtf2getfieldinfo col field-len (%null-ptr))
       (let ((field-length (ccl::%get-long field-len)))
         (%stack-block ((huge-string field-length)
                        (field-len #.(ccl::record-field-length :long)))
           (dtf2getfield col huge-string field-length field-len (%null-ptr))
           ;(ccl::%get-long field-len)  ;; DBL: why does removing this help?
           (ccl::%get-cstring huge-string)))))
    (#.$dtFcolTypeBit
     (%stack-block ((field-len #.(ccl::record-field-length :long)))
       (catch-error
        (dtf2getfieldinfo col field-len (%null-ptr)) "retrieve-field/info: bit")
       (let ((field-length (ccl::%get-long field-len)))
         (%stack-block ((huge-string field-length)
                        (field-len #.(ccl::record-field-length :long)))
           (catch-error
            (dtf2getfield col huge-string field-length field-len (%null-ptr))
            "retrieve-field: bit")
           (ccl::%get-cstring huge-string)))))
    ; here something does not work
    ;; DBL: Looks like there's a compiler bug.  The col argument doesn't
    ;; get passed correctly.  Substituting a constant 0 makes the test
    ;; case work.  Look above at the #+broken and #-broken code; this affects
    ;; behavior in the following case.  It looks like the compiler gets the 
    ;; value of col just fine, but then tries to move it to the stack from
    ;; the wrong result register.  Bug submitted to Digitool on Halloween 98.
    ((#.$dtFcolTypeLong #.$dtFcolTypeLongWord)
     (%stack-block ((long #.(ccl::record-field-length :long))
                    (field-len #.(ccl::record-field-length :long))
                    (col-type #.(ccl::record-field-length :word)))
       (catch-error (dtf2getfield col long #.(ccl::record-field-length :long)
                                  field-len col-type)
                    "retrieve-field: long")
       (ccl::%get-long long)))))
|#

(defun retrieve-records (query workspace flatp &key (which :all) (start 0))
  (when (or (not (integerp start))
            (< start 0))
    (error "start has to be a positive integer, was ~a." start))
  (ccl::with-cstr (sqlstr query)
    (if workspace
      (dtf2execws workspace sqlstr)
      (dtf2exec sqlstr)))
  (when (result-available-p workspace)
    (let ((cols (%column-count workspace))
          (rows (%row-count workspace))
          (table-names ())
          (col-names ())
          (col-types ()))
      (when (>= start rows)
        (return-from retrieve-records ()))
      (dotimes (col cols)
        (multiple-value-bind (table col-name type)
                             (get-field-info (- cols col 1))
          (push table table-names)
          (push col-name col-names)
          (push type col-types)))
      (collecting
        (cond ((not flatp)
               (loop while (result-available-p workspace)
                     do
                     (let ((col 0))
                       (collect
                         (mapcar #'(lambda (type)
                                     (prog1
                                       (retrieve-field col type workspace)
                                       (incf col))) 
                                 col-types)))
                     while (and (neq which :first)
                                (zerop (move-cursor :current 1 workspace)))))
              ((> cols 1)
               (error "flatp must be nil when retrieving more than one column"))
              (t
               (loop while (result-available-p workspace)
                     do (collect (retrieve-field 0 (car col-types) workspace))
                     while (and (neq which :first)
                                (zerop (move-cursor :current 1 workspace))))))))))

;; new

(defmethod %db-execute ((database dtf-database) sql-expression &key new-query &allow-other-keys)
  (declare (ignore new-query)) ;; preliminary
  (with-slots (query) database
    (if t ;;new-query
      ;(%db-execute (get-free-query database) sql-expression)
      (%db-execute query sql-expression))))

(defmethod %db-execute ((query dtf-query) sql-expression &key &allow-other-keys)
  (with-slots (workspace) query
    (ccl::with-cstr (sqlstr sql-expression)
      (if workspace
        (dtf2execws workspace sqlstr)
        (dtf2exec sqlstr))))
  query)

#+obsolete
(defmethod %db-execute ((database dtf-database) sql-expression &key #+useful? new-query
                          &allow-other-keys)
  (with-slots (workspace) (db-query-object database)
    (ccl::with-cstr (sqlstr sql-expression)
      (if workspace
        (dtf2execws workspace sqlstr)
        (dtf2exec sqlstr)))))

(defmethod %initialize-query ((database dtf-database) &optional arglen col-positions)
  (%initialize-query (db-query-object database) arglen col-positions))

(defmethod %initialize-query ((query dtf-query) &optional arglen col-positions)
  (with-slots (workspace column-count row-count table-names column-names column-types 
                         cursor-position) 
              query
    (when (result-available-p workspace)
      (setf column-count (if arglen
                           (min arglen (%column-count workspace))
                           (%column-count workspace))
            row-count (%row-count workspace) 
            (fill-pointer table-names) 0
            (fill-pointer column-names) 0
            (fill-pointer column-types) 0
            cursor-position nil)
      (if col-positions 
        (dotimes (col column-count)
          (multiple-value-bind (table-name col-name col-type)
                               (get-field-info col)
            (vector-push-extend table-name table-names)
            (vector-push-extend col-name column-names) 
            (vector-push-extend col-type column-types)))
        (dotimes (col column-count)
          (multiple-value-bind (table-name col-name col-type)
                               (get-field-info col)
            (vector-push-extend table-name table-names)
            (vector-push-extend col-name column-names) 
            (vector-push-extend col-type column-types)))))))

(defmethod %read-query-data ((database dtf-database) ignore-columns)
  (%read-query-data (db-query-object database) ignore-columns))

(defmethod %read-query-data ((query dtf-query) ignore-columns)
  (declare (ignore ignore-columns))
  (with-slots (workspace column-count row-count column-names column-types cursor-position)
              query
    ;(print (list column-count row-count))
    (when (and (result-available-p workspace)
               (not (eq cursor-position :end))
               (not (zerop row-count)))
      (prog1
        (loop for col-nr from 0 to (1- column-count)
              for col-type across column-types
              collect (retrieve-field col-nr col-type workspace))
        (unless (zerop (move-cursor :current 1 workspace))
          (setf cursor-position :end))))))

(defmethod %dispose-query-vars ((database dtf-database))
  (with-slots (query) database
    (%dispose-query-vars query)))

(defmethod %dispose-query-vars ((query dtf-query))
  ()) ; really nothing to dispose of?

#+ignore
(defmethod db-rollback ((database dtf-database))
  (with-slots (workspace) database
    (ccl::with-cstr (sqlstr "rollback")
      (if workspace
        (dtf2execWS sqlstr workspace)
        (dtf2exec sqlstr)))))

(defmethod db-rollback ((database dtf-database))
  (db-execute-command database "rollback"))

(defmethod db-commit ((database dtf-database))
  (db-execute-command database "commit"))
