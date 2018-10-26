(defparameter ccl::*rdnzl-version* "RDNZL-0.13.3")
(defparameter ccl::*rdnzl-path* 
    (concatenate 'string 
        ccl:*cormanlisp-directory*
        "libraries" "/"
        "rdnzl" "/"
        ccl::*rdnzl-version* "/"))
(defparameter ccl::*rdnzl-load-path* 
    (concatenate 'string 
        ccl::*rdnzl-path*
        "load.lisp"))
(export '(ccl::*rdnzl-version* ccl::*rdnzl-path* ccl::*rdnzl-load-path*) :ccl)
(win:create-menu-item 
		(list :command "&RDNZL .Net Integration..." 
			(lambda (x)
				(declare (ignore x)) 
				(ccl:display-url (concatenate 'string ccl::*rdnzl-path* "doc/index.html"))))
			"&Help" 
		5)
(load ccl::*rdnzl-load-path*)
(provide :rdnzl)
(rdnzl:init-rdnzl)
