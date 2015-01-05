;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;
;;;	This is an example of both calling a C function (in a DLL)
;;; and being called back by it. The functions are called in the
;;; example DLL "TESTDLL.DLL".
;;;
(win:defctype CALLBACKTYPE (:void *))
	
#!  (:library "d:/roger/cormanlisp/examples/testdll.dll")
long testfunc1(long x, long y, long z);
long testfunc2(long x, CALLBACKTYPE func);
!#

(ct:defun-c-callback test-func ((x :long))
	(format t "x = ~A~%" x))

;(testfunc1 10 20 30)
;(testfunc2 5 (ct:get-callback-procinst 'test-func))

