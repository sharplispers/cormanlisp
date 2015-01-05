;;;-*- Mode: Lisp; Package: ODBC -*-

;; ODBC module for MCL, LW and ACL
;; Version 0.85
;; Copyright (C) Paul Meurer 1999. All rights reserved.
;; paul.meurer@hit.uib.no

;; Please read the file "sql-odbc-documentation.lisp" for more info
;; and a license notice.

(in-package :odbc)

(defclass odbc-stream (sql-stream)
  ((data-ptr :initform nil)
   (c-type :initform nil)
   (sql-type :initform nil)
   (out-len-ptr :initform nil)
   (out-len :initform nil)
   (precision :initform nil)
   ;; the actual length of the buffer. BUFFER-SIZE is the upper bound.
   (buffer-length :initform nil)))

(defclass odbc-string-stream (odbc-stream sql-string-stream) ())

(defclass odbc-string-input-stream (odbc-string-stream fundamental-input-stream) ())

(defclass odbc-string-output-stream (odbc-string-stream fundamental-output-stream) ())

(defmethod make-sql-stream ((query odbc-query) &key column stream-class direction)
  (with-slots (column-count column-c-types) query
    (let* ((column (or column (1- (column-count query))))
           (stream-class (or stream-class
                            (case (aref column-c-types column)
                              (#.$SQL_CHAR 
                               (if (eq direction :output)
                                 'odbc-string-output-stream
                                 'odbc-string-input-stream))
                              (#.$SQL_INTEGER
                               (if (eq direction :output)
                                 'odbc-binary-output-stream
                                 'odbc-binary-input-stream))))))
      (make-instance stream-class :query query :column column))))

(defmethod initialize-instance :after ((stream odbc-stream) &key column &allow-other-keys)
  (with-slots (query data-ptr c-type sql-type out-len-ptr out-len chunk-length precision)
              stream 
    (with-slots (hstmt column-count column-c-types column-sql-types
                       column-data-ptrs column-out-len-ptrs column-precisions)
                query
      (unless (sql::sql-stream-column stream)
        (setf (slot-value stream 'sql::sql-stream-column) (1- column-count)))
      (setf data-ptr (aref column-data-ptrs column)
            c-type (aref column-c-types column)
            sql-type (aref column-sql-types column)
            out-len-ptr (aref column-out-len-ptrs column)
            precision (aref column-precisions column)))))

(defmethod open-sql-stream ((stream odbc-string-stream))
  (with-slots (buffer buffer-position) stream 
    (setf buffer nil buffer-position 0)))

(defun odbc-get-next-chunk (query column c-type data-ptr buffer-size out-len-ptr precision)
  (with-slots (hstmt) query
    (if (< precision +max-precision+)
      (let ((chunk (%get-cstring data-ptr)))
        (values chunk (length chunk)))
      (let ((res (%sql-get-data hstmt column c-type data-ptr (1+ buffer-size) out-len-ptr))
            (out-len (%get-long out-len-ptr)))  
        (if (= res $SQL_NO_DATA_FOUND)
            :eof
          (case out-len 
            (#.$SQL_NULL_DATA :eof)
            (#.$SQL_NO_TOTAL (error "$SQL_NO_TOTAL not yet implemented."))
            (otherwise (let ((chunk (%get-cstring data-ptr)))
                       (values chunk (length chunk))))))))))

(defmethod stream-read-char ((stream odbc-string-input-stream))
  (with-slots (buffer buffer-size buffer-length buffer-position query column data-ptr
                      c-type sql-type out-len-ptr out-len precision)
              stream
    (when (null buffer)
      (multiple-value-setq (buffer buffer-length)
        (odbc-get-next-chunk query column c-type data-ptr
                             buffer-size out-len-ptr precision)))
    (if (eq buffer :eof)
      #+:mcl nil #-:mcl :eof
      (prog1
        (aref buffer buffer-position)
        (incf buffer-position)
        (cond ((= buffer-position buffer-size)
               (setf buffer-position 1)
               (multiple-value-setq (buffer buffer-length)
                 (odbc-get-next-chunk query column c-type data-ptr
                                      buffer-size out-len-ptr precision)))
              ((= buffer-position buffer-length)
               (setf buffer :eof)))))))

(defmethod stream-peek-char ((stream odbc-string-input-stream))
  (with-slots (buffer buffer-size buffer-length buffer-position 
                      query column data-ptr c-type sql-type precision out-len-ptr out-len)
              stream
    (when (null buffer)
      (multiple-value-setq (buffer buffer-length)
        (odbc-get-next-chunk query column c-type data-ptr buffer-size
                                        out-len-ptr precision))) 
    (if (eq buffer :eof)
      :eof
      (aref buffer buffer-position))))

#+mcl
(defmethod stream-peek ((stream odbc-string-input-stream))
  (stream-peek-char stream))

;; here is room for improvement!
(defun odbc-read-into-string (query string start end column c-type
                                        data-ptr out-len-ptr precision)
  "Returns position of first unchanged char in string and in the case that 
precision < +max-precision+ the rest string, else nil (if there is more to fetch) or :eof.
It is thus safe to assume that everything is fetched if buffer-length < buffer-size."
  (with-slots (hstmt) query
    (if (< precision +max-precision+)
        (let ((offset (ffc::%cstring-into-string data-ptr string start end)))
          (values
           offset
           (let ((rest-string (%get-cstring data-ptr offset)))
             (if (zerop (length rest-string))
                 :eof rest-string))))
      (let ((eof nil))
        (loop with eof = nil
              do
              (let* ((fetch-size (min +max-precision+ (- end start)))
                     (res (%sql-get-data hstmt column c-type data-ptr (1+ fetch-size) out-len-ptr))
                     (out-len (%get-long out-len-ptr))) 
                (if (= res $SQL_NO_DATA_FOUND)
                    (setf eof :eof)
                  (case out-len
                    (#.$SQL_NULL_DATA (setf eof :eof))
                    (#.$SQL_NO_TOTAL (error "$SQL_NO_TOTAL not yet implemented."))
                    (otherwise 
                     (format t "~%res: ~d" res)
                     (setf start (ffc::%cstring-into-string 
                                  data-ptr string start (min +max-precision+ out-len (- end start))))))))
              until (or eof (= end start)))
        (values start eof)))))

(defmethod stream-read-sequence ((stream odbc-string-input-stream) sequence 
                                     #+:allegro &optional start end)
  (unless start (setf start 0))
  (let ((len (length sequence)))
    (setf end (if end (min end len) len)))
  (with-slots (buffer buffer-size buffer-length buffer-position query column data-ptr
                      c-type sql-type out-len-ptr out-len precision)
              stream
    (cond ((null buffer)
           ;; we can read directly into the string
           (multiple-value-bind (offset rest-string)
                                (odbc-read-into-string 
                                 query sequence start end column
                                 c-type data-ptr out-len-ptr precision)
             (setf buffer rest-string
                   buffer-length (length rest-string)
                   buffer-position 0)
             offset))
          ((eq buffer :eof)
           :eof)
          (t
           ;; use up rest of buffer ...
           (let ((end1 (min end (+ start (- buffer-length buffer-position))))
                 (end2 (min buffer-length (+ buffer-position (- end start)))))
             (replace sequence buffer
                      :start1 start :end1 end1
                      :start2 buffer-position :end2 end2)
             (cond ((= end1 end) ; read enough
                    (cond ((< end2 buffer-length)
                           (setf buffer-position end2))
                          ((< buffer-length buffer-size)
                           (setf buffer :eof))
                          (t (setf buffer nil)))
                    end1)
                   ((< (print buffer-length) buffer-size)
                    (setf buffer :eof)
                    end1)
                   (t 
                    ;; ... then fetch and read into the string
                    (multiple-value-bind 
                      (offset rest-string)
                      (odbc-read-into-string query sequence start end column
                                             c-type data-ptr out-len-ptr precision)
                      (setf buffer rest-string
                            buffer-length (length rest-string)
                            buffer-position 0) 
                      offset))))))))

;; This is prelimiary. We should buffer if sequence is too short.
(defmethod stream-write-sequence ((stream odbc-string-output-stream) sequence
                                      #+:allegro &optional start end)
  (let ((size (length sequence)))
    (unless start (setf start 0))
    (unless end (setf end size))
    (let ((length (- end start)))
      (when (or (/= start 0) (/= end size)) ;; preliminary, we should copy directly from the seq!
        (setf sequence (subseq sequence start end)))
      (with-slots (query) stream
        (with-slots (hstmt) query
          (%with-temporary-allocation
            ((data-ptr :string length))
            (%put-str data-ptr sequence)
            (%sql-put-data hstmt data-ptr length)))))))

(defmethod #-:mcl close
           #+:mcl close-stream
           :before ((stream odbc-string-output-stream) &key abort)
  (declare (ignore abort))
  (with-slots (query) stream
    (with-slots (hstmt) query
      (when (data-parameter-ptr hstmt)
        (error "Column data missing??")))))
