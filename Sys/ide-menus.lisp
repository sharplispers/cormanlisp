;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		ide-menus.lisp
;;;;	Contents:	Corman Lisp IDE Menu additions.
;;;;	History:	RGC  08/29/06  Created.

(in-package :ide)

(defun setup-ide-menus ()
    ;; create dynamic Package submenu (under Lisp menu)
	(win:create-dynamic-menu-item 
		(list :menu "Package"
            (lambda (hmenu)
                (let ((index 3)
                      (packages 
        				(sort (list-all-packages)
        					(lambda (x y) 
        						(string< (package-name x)(package-name y))))))
                    (dolist (x packages)
			             (let ((package x))
				            (win:create-dynamic-menu-item 
                                (list :command (package-name x) 
						          (lambda (y) (declare (ignore y))(setf *package* package)))
                                hmenu
                                index
                                :checked (eq x *package*)))
                        (incf index)))))
		"&Lisp"
		3)
    	
	(win:create-menu-item 
		(list :command "&Top Level" 
			(lambda (x)
				(declare (ignore x)) 
				(invoke-restart (first (last (compute-restarts))))))
			"&Lisp" 
		4)
        
    (win:create-menu-item 
		(list :command "&Zoom 1 to 1" 
			(lambda (x)
				(declare (ignore x)) 
				(ide:set-current-window-zoom 1 1)))
			"&Lisp"
		    5
            :message "Set edit window zoom to 1 to 1")
    
    (ide:add-jumpmenu-to-menubar)
    (ide:add-symbol-menu-to-menubar)
    (ide:add-command-history-menu-to-menubar)
    
    (win:create-menu-item 
		(list :command "Corman Lisp &Forums..." 
			(lambda (x)
				(declare (ignore x)) 
				(cl::display-url "www.artofprogramming.com")))
			"&Help" 
		4))

