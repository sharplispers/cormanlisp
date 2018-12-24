;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		load-sys2.lisp
;;;;	Contents:	Corman Lisp code to build the system.
;;;;	History:	6/24/96  RGC  Created.
;;;;

(defun load-file (filename)
    (if (eq (cl::cormanlisp-client-type) 2)
        (editor-set-message (format nil "Compiling ~a" filename))
        (progn (format t "Compiling ~a~%" filename)(force-output)))
    (load filename))
    
(setq cl::*compiler-save-lambdas* nil)
(setq cl::*compiler-save-table-references* nil)

(load-file "sys/declarations.lisp")
(load-file "sys/kernel-asm.lisp")
(load-file "sys/kernel-funcs.lisp")
(load-file "sys/trees.lisp")
(load-file "sys/compiler.lisp")
(load-file "sys/lists.lisp")
(load-file "sys/characters.lisp")
(load-file "sys/strings.lisp")
(load-file "sys/math.lisp")
(load-file "sys/random.lisp")
(load-file "sys/symbols.lisp")
(load-file "sys/control-structures.lisp")
(load-file "sys/sequences.lisp")
(load-file "sys/subtypep.lisp")
(load-file "sys/coerce.lisp")
(load-file "sys/input-output.lisp")
(load-file "sys/errors.lisp")
(load-file "sys/defpackage.lisp")
(load-file "sys/misc-features.lisp")
(load-file "sys/clos.lisp")
(load-file "sys/fast-class-of.lisp")
(load-file "sys/conditions.lisp")
(load-file "sys/tail-calls.lisp")
(load-file "sys/profiler.lisp")
(load-file "sys/ffi.lisp")
(load-file "sys/trace.lisp")

(defun open ())			; avoid warnings
(defun ct::read-token ()) ; avoid warnings

(load-file "sys/parse-c-decls.lisp")
(load-file "sys/win32.lisp")
(load-file "sys/win-conditions.lisp")
(load-file "sys/com.lisp")
(load-file "sys/winsock.lisp")
(load-file "sys/time.lisp")
(load-file "sys/math2.lisp")
(load-file "sys/filenames.lisp")
(load-file "sys/streams.lisp")
(load-file "sys/autoload.lisp")

(load-file "sys/loop.lisp")
(load-file "sys/describe.lisp")
(load-file "sys/pretty.lisp")
(load-file "sys/directory.lisp")
(load-file "sys/open-file.lisp")
(load-file "sys/menus.lisp")
(load-file "sys/registry.lisp")
(load-file "sys/edit-window.lisp")
(load-file "sys/imagehlp.lisp")
(load-file "sys/map-file.lisp")
(load-file "sys/compile-file.lisp")
(load-file "sys/debug.lisp")
(load-file "sys/save-application.lisp")
(load-file "sys/dribble.lisp")
(load-file "sys/require.lisp")
(load-file "sys/documentation.lisp")
(load-file "sys/print-float.lisp")
(load-file "sys/bits.lisp")
(load-file "sys/boole.lisp")
(load-file "sys/bignums.lisp")
(load-file "sys/math-ops.lisp")
(load-file "sys/places.lisp")
(load-file "sys/misc-utility.lisp")
;; load code formatting engine
(let ((*package* (find-package :ide)))
  (with-input-from-string (in "") ;; to not let it hang, as it calls INDENT-LINES
    (let ((*standard-input* in))
      (load-file (concatenate 'string ccl::*cormanlisp-directory* "Sys\\scmindent\\lispindent.lisp")))))     
(load-file "sys/code-indenter.lisp")
(load-file "sys/context-menu.lisp")
(load-file "sys/setf-expander.lisp")
(load-file "sys/sockets.lisp")
(load-file "sys/xp.lisp")
(load-file "sys/threads.lisp")
(load-file "sys/version.lisp")
(load-file "sys/auto-update.lisp")
(load-file "sys/jumpmenu.lisp")
(load-file "sys/ide-menus.lisp")

;; Load patches
(in-package :cl)
(let ((patch-files
        (sort (directory 
                    (merge-pathnames "CormanLisp_3_0_patch_??.lisp" (ccl::local-patches-directory)))
            #'string< :key 'namestring)))
    (dolist (f (mapcar 'namestring patch-files))
        (let ((ccl::*patch* (ccl::make-patch f)))
            (load-file f))))

(export 'ccl::find-in-files :ccl)
(ccl:define-autoloaded-module "modules\\find-in-files.lisp" (:functions ccl::find-in-files))

;; QUASILOAD will replace all the functions and macros with thunks,
;; making the originals garbage-collectable.
;(show-message "Making PARSE-C-DECLS.LISP autoloadable")
;(ccl:quasiload "sys\\parse-c-decls.lisp")
;; Demote the ct::+c-keywords+ constant defined by the parser to a
;; mutable variable to avoid a warning when the autoload above fires.
(%symbol-set-flags  
	(logandc2 (%symbol-get-flags 'ct::+c-keywords+) *symbol-constant-flag*)
	'ct::+c-keywords+)

(do-symbols (sym (find-package :keyword))
	(cl::symbol-set-constant-flag sym)
	(cl::symbol-set-special-flag sym))		;; keywords defined in the kernel need to be made constant

(editor-set-default-message)

(in-package :cl-user)
(setq cl::*compiler-save-lambdas* t)
(setq cl::*compiler-save-table-references* t)
(setq cl::*loading-kernel* nil)
(setq cl::*compress-img* t)


