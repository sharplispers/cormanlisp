;;; Acad CLARX: CLIDE code goes here

;;; temp fix until ccl exports it, only needed without arx
(unless pl::*basemodule-directory* 
  (defconstant pl::*basemodule-directory* 
               (concatenate 'string pl::*cormanlisp-directory* "Libraries\\acad\\")))
;;; we need this for the registry-utils
(push-module-directory (concatenate 'string pl::*cormanlisp-directory* "examples"))

(princ "...loading clide-init.lisp")
(load (concatenate 'string 
	pl::*basemodule-directory*
        "acad.lisp"))

; (acad:clax-reg-app "SOFTWARE\\Corman Tools\\Corman Lisp 1.5" '("CLIDE") 12 "CormanLisp ARX")
