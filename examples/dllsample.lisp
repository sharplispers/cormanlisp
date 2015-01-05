;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		dllsample.lisp
;;;;	Contents:	Example of DLL generation
;;;;	History:	4/25/02  RGC  Created.
;;;;

(ct:defun-dll-export-c-function (lisp_add "lisp_add") ((x :long)(y :long))
	"long lisp_add(long a, long b)"
	(+ x y))

(ct:defun-dll-export-c-function (lisp_multiply "lisp_multiply") ((x :long)(y :long))
	"long lisp_multiply(long a, long b)"
	(* x y))

(ct:defun-dll-export-c-function (lisp_subtract "lisp_subtract") ((x :long)(y :long))
	"long lisp_subtract(long a, long b)"
	(- x y))

(ct:defun-dll-export-c-function (corman "CORMAN") ()
	"char* CORMAN()"
	(ct:lisp-string-to-c-string "This is Corman Lisp 2.0"))

(ct:defun-dll-export-c-function (lisp-double-add "lisp_double_add") ((x :double-float)(y :double-float))
	"double lisp_double_add(double a, double b)"
	(+ x y))

(ct:defun-dll-export-c-function (lisp-single-add "lisp_single_add") ((x :single-float)(y :single-float))
	"float lisp_single_add(float a, float b)"
	(+ x y))

(ct:defun-dll-export-c-function (lisp_apropos "lisp_apropos") ((str (:char *)))
    "char* lisp_apropos(char* str)"
    (let ((output-string 
                (make-array 2048 :element-type 'character :fill-pointer 0 :adjustable t)))
       (with-output-to-string (*standard-output* output-string)
            (apropos (ct:c-string-to-lisp-string str)))
        (ct:lisp-string-to-c-string output-string)))

#|
An example of how to declare a __stdcall function to be linked at load time,
and called as "lisp_add":

(ct:defun-dll-export-function (lisp_add "lisp_add@8") ((x :long)(y :long))
	"long __stdcall lisp_add(long a, long b)"
	(+ x y))

The number following the @ in the exported name must match the number
of bytes pushed on the stack by the function, which is 4 bytes per
argument, or 8 bytes per double-float argument. (0 if no arguments).
|#

#|
(ccl::compile-dll "examples/dllsample.lisp" :output-file "dllsample.dll" :verbose t
	:print t :def t :h t)

(win::system "lib /def:dllsample.def /out:dllsample.lib /machine:x86")
|#
