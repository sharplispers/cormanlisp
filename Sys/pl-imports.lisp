;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		pl-imports.lisp
;;;;	Contents:	Corman Lisp package symbols imported 
;;;;				from other packages.
;;;;	History:	3/13/97  RGC  Created.
;;;;

(in-package :cormanlisp)

(let ((syms '(
	cl::uref 
	cl::uref-set
	cl::save-image
	cl::load-image
	cl::*compiler-save-lambdas*
	cl::editor-set-default-message
	cl::editor-set-message
	cl::uvectorp
	cl::alloc-uvector
	cl::lisp-object-id
	cl::execution-address 
	cl::load-dll
	cl::unload-dll
	cl::get-dll-proc-address
	cl::symbol-var-table-offset
	cl::symbol-jump-table-offset
	cl::symbol-value-offset
	cl::symbol-function-offset
	cl::symbol-function-type-offset
	cl::symbol-constant-offset
	cl::function-environment-offset
	cl::function-code-buffer-offset
	cl::array-dimensions-offset
	cl::foreign-heap-ptr-offset
	cl::array-type-offset
	cl::sequencep
	cl::structurep 
	cl::%symbol-get-flags 
	cl::%symbol-set-flags
	cl::function-environment
	cl::int-char 
	cl::array-dim1-offset
	cl::stack-trace
	cl::address-find-function
	cl::foreignp
	cl::allocate-c-heap
	cl::deallocate-c-heap
	cl::%uvector-address
	cl::compile-form
	cl::gc
	cl::bignump
	cl::fixnump
	cl::weak-pointer-p
	cl::compiled-code-p
	cl::uvector-function-tag
	cl::uvector-kfunction-tag
	cl::uvector-structure-tag
	cl::uvector-array-tag
	cl::uvector-symbol-tag
	cl::uvector-stream-tag
	cl::uvector-double-float-tag
	cl::uvector-package-tag
	cl::uvector-hashtable-tag
	cl::uvector-foreign-tag
	cl::uvector-compiled-code-tag
	cl::uvector-readtable-tag
	cl::uvector-complex-tag
	cl::uvector-ratio-tag
	cl::uvector-bignum-tag
	cl::uvector-foreign-heap-tag
	cl::uvector-weak-ptr-tag
	cl::uvector-simple-vector-tag
	cl::uvector-simple-char-vector-tag
	cl::uvector-simple-byte-vector-tag
	cl::uvector-simple-short-vector-tag
	cl::uvector-simple-double-float-vector-tag
	cl::uvector-simple-bit-vector-tag
	cl::uvector-simple-single-float-vector-tag
	cl::uvector-single-float-tag
	cl::uvector-clos-instance-tag
	cl::uvector-foreign-stack-tag
	cl::function-info-list
	cl::*error-trace*
	cl::make-weak-pointer
	cl::weak-pointer-obj
	cl::register-finalization
	cl::*top-level*
	cl::macroexpand-all
	cl::uninitialized-object-p
	cl::top-level
	cl::display-url
	cl::short-integer
	cl::compress-file	
	cl::uncompress-file
    cl::memory-report
    cl::lisp-shutdown
	)))

	(import syms)
	(export syms))
(export 'compile-form)

(defparameter ccl::*cormanlisp-patch-level* 0)
(export 'ccl::*cormanlisp-patch-level*)

(in-package :common-lisp)
