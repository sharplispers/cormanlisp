;;;; S-XML loader for Corman Lisp
(defconstant *sxml-source-directory* 
    (concatenate 'string ccl:*cormanlisp-directory* "\\libraries\\s-xml\\"))

(require :asdf)

(defconstant *sxml-files* 
	(list 
		"src\\package.lisp"
		"src\\xml.lisp"
		"src\\dom.lisp"
		"src\\lxml-dom.lisp"
		"src\\sxml-dom.lisp"
		"src\\xml-struct-dom.lisp"))

(loop for file in *sxml-files* do (load (concatenate 'string *sxml-source-directory* file)))

(provide :s-xml)