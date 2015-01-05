;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		pl-symbols.lisp
;;;;	Contents:	Exported Corman Lisp package symbols.
;;;;

(in-package :cormanlisp)

(defvar *HYPERSPEC-LOCAL-PATH* nil)

(export '(
	;;uref-set
	;;uref
	*compiler-code-buffer*
	*compiler-optimize-speed*
	*compiler-optimize-safety*
	*compiler-optimize-space*
	*compiler-optimize-debug*
	*compiler-optimize-compilation-speed*
	kernel-function-p
	*cormanlisp-directory*
	*cormanlisp-server-directory*
	*trace-exceptions*
	*source-file*
	*source-line*
	*save-debug-info*
	hyperspec
	*HYPERSPEC-LOCAL-PATH*
	uvector-length
	uvector-type-tag
	peek-byte
	peek-dword
	peek-lisp-object
	get-qv-reg
	lisp-object-bits
	*current-thread-id*
	*current-thread-handle*
	*current-process-id*
	*current-process-handle*
))

(defvar *current-thread-id*       *current-thread-id*)
(defvar *current-thread-handle*   *current-thread-handle*)
(defvar *current-process-id*      *current-process-id*)
(defvar *current-process-handle*  *current-process-handle*)
        

