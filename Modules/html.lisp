;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	All rights reserved.
;;;;	-------------------------------
;;;;
;;;;	File:		htmll.lisp
;;;;	Contents:	Functions which can be used to generate HTML pages.
;;;;	History:	9/1/06  RGC  Created.
;;;;
;;;;    These functions were inspired by Paul Graham.
;;;;
(defpackage :html
	(:export
        "AS"
        "WITH"
        "WITH-TAGS"
        "TAG"
        "TEXT" 
        "DOCTYPE-HEADER"
        "PAGE"
        "<BR>"
		))
(in-package :html)

(defparameter *indent* 0)           ;; starting indent level
(defparameter *indent-length* 2)    ;; number spaces to indent HTML at each nesting level

;;;
;;; The AS macro is useful where the body is a single string or several strings.
;;; Optional keyword/value pairs may proceed the body strings, and these will
;;; be inserted as parameters to the opening tag. The value may be a number or 
;;; a string--if a number, it is converted to a string.
;;; It concatenates the body strings together. It will generate opening and 
;;; closing tags. The output will be on separate lines, properly indented, unless
;;; the :embedded keyword is passed. If :embedded it true, then the output
;;; will be inserted into the current line, which is useful for short expressions
;;; and to keep the HTML succinct.
;;;
(defmacro as (tag &rest body)
    (let ((args '())
          (embedded nil))
        (do ()
            ((not (keywordp (car body))))
            (if (eq (car body) ':embedded)
                (setf embedded (cadr body))
                (progn            
                    (push (car body) args)
                    (let ((value (cadr body)))
                        (if (numberp value)
                            (setq value (format nil "~A" value)))
                        (push value args))))
            (setf body (cddr body)))
        (setq args (nreverse args))
        (if embedded
            `(format t "<~A~{ ~A=\"~A\"~}>~A</~A>" ',tag (list ,@args) (concatenate 'string ,@body) ',tag)
           `(let ((*indent* (+ *indent* *indent-length*)))
               (format t "~v,0t<~A~{ ~A=\"~A\"~}>~A</~A>~%" 
                 *indent* ',tag (list ,@args) (concatenate 'string ,@body) ',tag)))))

;;;
;;; The WITH macro is useful where the body contains html which may have embedded tags.
;;; Optional keyword/value pairs may proceed the body expressions, and these will
;;; be inserted as parameters to the opening tag. The value may be a number or 
;;; a string--if a number, it is converted to a string.
;;; The body expressions which follow any optional key/value pairs are expanded to
;;; generate retulting HTML, which is inserted between opening and closing tags.
;;; The output will be on separate lines, properly indented.
;;; The :embedded keyword (see AS) is not supported in WITH.
;;;
(defmacro with (tag &rest body)
    (let ((args '())
          (single-line nil))
        (do ()
            ((not (keywordp (car body))))
            (if (eq (car body) ':single-line)
                (setf single-line (cadr body))
                (progn
                    (push (car body) args)
                    (let ((value (cadr body)))
                        (if (numberp value)
                            (setq value (format nil "~A" value)))
                        (push value args))))
            (setf body (cddr body)))
        (setq args (nreverse args))
        (if (find-if 'stringp body)
            (setf body (mapcar (lambda (x) (if (stringp x) `(text ,x) x)) body)))
        (if single-line
            `(let ((*indent* (+ *indent* *indent-length*)))         
                 (format t "~v,0t<~A~{ ~A=\"~A\"~}>" *indent* ',tag (list ,@args))
                 ,@body
                 (format t "</~A>~%" ',tag))
            
           `(let ((*indent* (+ *indent* *indent-length*)))         
                 (format t "~v,0t<~A~{ ~A=\"~A\"~}>~%" *indent* ',tag (list ,@args))
                 ,@body
                 (fresh-line)
                 (format t "~v,0t</~A>~%" *indent* ',tag)))))

;;;
;;; The TAG macro is for tags (such as BR) which are complete (do not have closing tags)
;;; Args are optional, and would be in the form of keyword/value pairs which are
;;; used to add parameters to the generated tag.
;;;
(defmacro tag (tag &rest args)
    (let ((outargs '())
          (embedded nil))
        (do ()
            ((not (keywordp (car args))))
            (if (eq (car args) ':embedded)
                (setf embedded (cadr args))
                (progn
                    (push (car args) outargs)
                    (let ((value (cadr args)))
                        (if (numberp value)
                            (setq value (format nil "~A" value)))
                        (push value outargs))))
            (setf args (cddr args)))
        (setq outargs (nreverse outargs))
        (if embedded
            `(format t "<~A~{ ~A=\"~A\"~}/>" ',tag (list ,@outargs))                  
            `(let ((*indent* (+ *indent* *indent-length*)))
                (format t "~v,0t<~A~{ ~A=\"~A\"~}/>~%" *indent* ',tag (list ,@outargs))))))

;;;
;;; Use the TEXT macro as a simple way to insert a series of one or more strings in a 
;;; WITH macro. These are concatenated together.
;;;
(defmacro text (&rest body)
    `(format t "~v,0t~A~%" *indent* (apply 'concatenate 'string (list ,@body))))

;;;
;;; Use WITH-TAGS to combine multiple tags into one form.
;;; Example use:
;;;     (with-tags ((table :cols 1) tr td) (text "roger"))
;;;     -generates-
;;;     (with table :cols 1 (with tr (with td (text "roger"))))
;;;     -which generates-
;;;      <TABLE COLS="1">
;;;         <TR>
;;;             <TD>
;;;             roger
;;;             </TD>
;;;         </TR>
;;;      </TABLE>
;;;
(defmacro with-tags ((&rest tags-and-params) &body body)
    (let ((last-tag (car (last tags-and-params))))
        (if (and last-tag (symbolp last-tag))
            (setf last-tag (list last-tag)))      
        (if (null last-tag)
            (car body)
            `(with-tags ,(butlast tags-and-params)
                (with ,@last-tag
                    ,@body)))))

;;;
;;; Standard HTML DOCTYPE header.
;;;
(defmacro doctype-header () 
    `(format t "~A~%" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2//EN\">"))

;;;
;;; The PAGE macro will cause the page to be expanded and output to a file.
;;;
(defmacro page (name &rest body)
    `(with-open-file (*standard-output*
                      ,name
                      :direction :output
                      :if-exists :supersede)
         ,@body))

(define-symbol-macro <BR> (tag br))
   
(provide :html)