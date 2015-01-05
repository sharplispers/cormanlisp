Hi, all, again:
 
How do I get the #c working for complex numbers in Corman LISP?  When I evaluate:
 
#c(1.4 4.4) I get back:
;;; An error occurred in function 0:
;;; Cannot compile form: (1.4 4.4)
 
although
 
(complex 1.4 4.4) works fine, but
(coerce 5.2 'complex) does not (it's not implemented).
 
How do I implement these?
 
-----
 
Broader question/personal preference:
 
What I'd really like to do is to express complex numbers with the i syntax, e.g.:
 
(+ 5 4i) => #C(5 4), 3.14i => #C(0 3.14), etc.
 
I know I can evaluate 5.7d0. Is there a way to do this for i?
 
------------------------------------------------------
Hi, all, the transcendental functions, specifically asin, acos, atan, do not compute their results to:
 
(/ (atan 1 (sqrt 3)) 6)  ; example lifted from hyperspec
 
but, instead, quit, complaining:
 
;;; An error occurred in function C-ATAN2:
;;; Not a :double-float: 1.73205
 
The regular functions: sin, cos, tan work fine, e.g.:
 
(cos pi) returns
-1.0d0
 
How do I, for the time being, coerce a number to a :double-float, AND is there a way to change Corman LISP so that it accepts NUMBERs instead of :double-floats?
 
Sincerely,
Douglas M. Auclair
-------------------------------------------
> ;;; An error occurred in function STD-COMPUTE-EFFECTIVE-METHOD-
FUNCTION:
> ;;; No primary methods for the
> generic function #<Standard-generic-function TEST #x11E8FB0>.
 
This appears to be because the CLOS implementation included with Corman Lisp doesn't now about Ratio's. This can be fixed by doing the
following:
 
  (in-package :common-lisp)
  (defclass ratio (number) ())
 
Now look in the file sys/fast-class-of.lisp. See the variable %*magic- class-table*? In the table you'll see:
 
  (find-class t) ;01101 - ratio
 
Change this to:
 
  (find-class 'ratio) ;01101 - ratio
 
Pop back to common-lisp-user:
 
  (in-package :cl-user)
 
Now your test method example will work. Hope this helps.
 
Chris.
 
-----------------------------------------------
In article <81c7ch$fg2$1@nnrp1.deja.com>,
  dauclair@hotmail.com wrote:
> Hi, I'm using CormanLisp 1.3 IDE and when I evaluate (describe 1/2) I
> get:
>
> ;;; An error occurred in function DESCRIBE-RATIO:
> ;;; Unbound variable: ~?
 
There is a bug in the describe-ratio function in the file
sys/describe.lisp. Look in this file at the function 'describe-ratio'. See the line:
  (format s "RATIO:~%~?~?~?"~?
 
The " character should be after the last ~?. The line should read:   (format s "RATIO:~%~?~?~?~?"
 
Also, the line containing (%uvector-address pathname) should be changed so it is (%uvector-address x). ie. Change the 'pathname' to an 'x'. To patch your existing system you can:
 
  (in-package :common-lisp)
 
  (defun describe-ratio (x s)
    (format s "RATIO:~%~?~?~?~?"
      "~4T~A:~20T~A~%" (list "value" x)
      "~4T~A:~20T~A~%" (list "numerator" (numerator x))       "~4T~A:~20T~A~%" (list "denominator" (denominator x))       "~4T~A:~20T#x~X~%" (list "heap address"         (%uvector-address x))))
 
  (in-package :common-lisp-user)
 
You might want to save an image after this. When this is done your example works.
 
Chris.
--------------------------------------------------------
 
 
Dear Chris, thanks for your two posts suggesting changes/corrections. Unfortunately, after I make the changes, CL still sees 1/2 as an instance of T, not RATIO, e.g.:
 
(class-of 1/2) returns:
#<Standard-class T #x16E0280>
 
So my
 
(defmethod test ((x number) (y integer)) '(num int))
 
still prints the failure message.
 
How does one coerce LISP into making ratios instances of RATIO, not T? 
 
 
-------------------------------------------------------------
(in-package :win32)
#! (:export t :library "KERNEL32" :pascal "WINAPI")
LPSTR WINAPI GetCommandLine();
!#
(getcommandline)
crashes system.
---------------------------------------------------------
