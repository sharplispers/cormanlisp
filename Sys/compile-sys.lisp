;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
(progn (terpri)(write "Creating the file CormanLisp.img")(terpri) (values))
(progn (load "sys/load-sys.lisp")(values))
(top-level)
(progn (load "sys/load-sys2.lisp")(values))

(setf ccl::*save-relative-source-file-names* nil)    ;; after this, only store absolute paths
(progn (in-package :user)(values))
(progn (save-image "CormanLisp.img")(values))
(progn (format *terminal-io* "~%The file CormanLisp.img was created successfully.~%~%")
	(values))
