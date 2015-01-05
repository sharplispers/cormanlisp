;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		load-sys.lisp
;;;;	Contents:	Corman Lisp code to build the system.
;;;;	History:	6/24/96  RGC  Created.
;;;;

(setq *compiler-save-lambdas* nil)
(setq *compiler-save-table-references* nil)
(setq *append-refs-to-code* t)
(editor-set-message "Loading BOOTSTRAP.LISP")	(load "sys/bootstrap.lisp")
;(setq *COMPILE-VERBOSE* t)
(editor-set-message "Loading EXPAND.LISP")		(load "sys/expand.lisp")
(editor-set-message "Loading UVECTOR.LISP")		(load "sys/uvector.lisp")
(editor-set-message "Loading READ.LISP")		(load "sys/read.lisp")
(editor-set-message "Loading MISC.LISP")		(load "sys/misc.lisp")
(editor-set-message "Loading READTABLE.LISP")	(load "sys/readtable.lisp")
(editor-set-message "Loading MASSAGE.LISP")		(load "sys/massage.lisp")
(editor-set-message "Loading WRITE.LISP")		(load "sys/write.lisp")

(editor-set-message "Loading DESTRUCTURE.LISP")	(load "sys/destructure.lisp")
(editor-set-message "Loading UTIL.LISP")		(load "sys/util.lisp")
(editor-set-message "Loading SEQUENCE.LISP")	(load "sys/sequence.lisp")
(editor-set-message "Loading FORMAT.LISP")		(load "sys/format.lisp")

(defun load-file (filename)
    (if (eq (cormanlisp-client-type) 2)
        (editor-set-message (format nil "Compiling ~a" filename))
        (progn (format t "Compiling ~a~%" filename)(force-output)))
    (load filename))

(setf *compiler-warn-on-unused-variable* t)
(load-file "sys/types.lisp")
(load-file "sys/io.lisp")
(load-file "sys/clmacros.lisp")
(load-file "sys/backquote.lisp")
(load-file "sys/package.lisp")
(load-file "sys/pl-imports.lisp")
(load-file "sys/setf.lisp")
(load-file "sys/structures.lisp")
(load-file "sys/array.lisp")
(load-file "sys/arrays.lisp")
(load-file "sys/assembler.lisp")
(load-file "sys/hash-table.lisp")
(load-file "sys/toplevel.lisp")
(editor-set-default-message)
(setq cl::*compiler-save-lambdas* t)
(setq cl::*compiler-save-table-references* t)
