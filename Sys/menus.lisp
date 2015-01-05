;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		menus.lisp
;;;;	Contents:	Functions to deal with menus.
;;;;	History:	2/1/98  RGC  Created.
;;;;

(in-package :win32)
(export '(
        create-menu-item 
        remove-menu-item 
        create-dynamic-menu-item
        get-main-menu
        remove-all-menu-items
        set-menu-item-checked))

(defwinconstant MF_INSERT 			#x00000000)
(defwinconstant MF_CHANGE 			#x00000080)
(defwinconstant MF_APPEND 			#x00000100)
(defwinconstant MF_DELETE 			#x00000200)
(defwinconstant MF_REMOVE 			#x00001000)
(defwinconstant MF_BYCOMMAND		#x00000000)
(defwinconstant MF_BYPOSITION		#x00000400)
(defwinconstant MF_SEPARATOR		#x00000800)
(defwinconstant MF_ENABLED			#x00000000)
(defwinconstant MF_GRAYED			#x00000001)
(defwinconstant MF_DISABLED			#x00000002)
(defwinconstant MF_UNCHECKED		#x00000000)
(defwinconstant MF_CHECKED			#x00000008)
(defwinconstant MF_USECHECKBITMAPS	#x00000200)
(defwinconstant MF_STRING			#x00000000)
(defwinconstant MF_BITMAP			#x00000004)
(defwinconstant MF_OWNERDRAW		#x00000100)
(defwinconstant MF_POPUP			#x00000010)
(defwinconstant MF_MENUBARBREAK		#x00000020)
(defwinconstant MF_MENUBREAK		#x00000040)
(defwinconstant MF_UNHILITE			#x00000000)
(defwinconstant MF_HILITE			#x00000080)
(defwinconstant MF_DEFAULT			#x00001000)
(defwinconstant MF_SYSMENU			#x00002000)
(defwinconstant MF_HELP				#x00004000)
(defwinconstant MF_RIGHTJUSTIFY		#x00004000)
(defwinconstant MF_MOUSESELECT		#x00008000)
(defwinconstant MF_END				#x00000080) ;; Obsolete -- only used by old RES files


(defwinconstant MFT_STRING         MF_STRING)
(defwinconstant MFT_BITMAP         MF_BITMAP)
(defwinconstant MFT_MENUBARBREAK   MF_MENUBARBREAK)
(defwinconstant MFT_MENUBREAK      MF_MENUBREAK)
(defwinconstant MFT_OWNERDRAW      MF_OWNERDRAW)
(defwinconstant MFT_RADIOCHECK     #x00000200)
(defwinconstant MFT_SEPARATOR      MF_SEPARATOR)
(defwinconstant MFT_RIGHTORDER     #x00002000)
(defwinconstant MFT_RIGHTJUSTIFY   MF_RIGHTJUSTIFY)

;; Menu flags for Add/Check/EnableMenuItem()
(defwinconstant MFS_GRAYED         #x00000003)
(defwinconstant MFS_DISABLED       MFS_GRAYED)
(defwinconstant MFS_CHECKED        MF_CHECKED)
(defwinconstant MFS_HILITE         MF_HILITE)
(defwinconstant MFS_ENABLED        MF_ENABLED)
(defwinconstant MFS_UNCHECKED      MF_UNCHECKED)
(defwinconstant MFS_UNHILITE       MF_UNHILITE)
(defwinconstant MFS_DEFAULT        MF_DEFAULT)

(defwinconstant MIIM_STATE       	#x00000001)
(defwinconstant MIIM_ID          	#x00000002)
(defwinconstant MIIM_SUBMENU     	#x00000004)
(defwinconstant MIIM_CHECKMARKS  	#x00000008)
(defwinconstant MIIM_TYPE        	#x00000010)
(defwinconstant MIIM_DATA        	#x00000020)

(defparameter *id-lisp-menu-item-start*             2000)
(defparameter *id-lisp-menu-item-end*			    2999)
(defparameter *id-lisp-menu-item*			        *id-lisp-menu-item-start*)
(defparameter *id-lisp-dynamic-menu-item-start*     3000)
(defparameter *id-lisp-dynamic-menu-item-end*       5999)
(defparameter *id-lisp-dynamic-menu-item*           *id-lisp-dynamic-menu-item-start*)

(defwinstruct MENUITEMINFO
	((cbSize 		UINT)
	 (fMask			UINT)
	 (fType			UINT)
	 (fState		UINT)
	 (wID			UINT)
	 (hSubMenu		HMENU)
	 (hbmpChecked	HBITMAP)
	 (hbmpUnchecked	HBITMAP)
	 (dwItemData	DWORD)
	 (dwTypeData	LPTSTR)
	 (cch			UINT)
	))
(defwintype LPMENUITEMINFO (MENUITEMINFO *))

(defwinstruct MENUINFO
    ((cbSize        DWORD)
     (fMask         DWORD)
     (dwStyle       DWORD)
     (cyMax         UINT)
     (hbrBack       HBRUSH)
     (dwContextHelpID  DWORD)
     (dwMenuData    (ULONG *))
    ))      
(defwintype LPMENUINFO (MENUINFO *))
(defwintype LPCMENUINFO (MENUINFO *))

;; MENUINFO dwStyle flags           
(defwinconstant MNS_NOCHECK         #x80000000)
(defwinconstant MNS_MODELESS        #x40000000)
(defwinconstant MNS_DRAGDROP        #x20000000)
(defwinconstant MNS_AUTODISMISS     #x10000000)
(defwinconstant MNS_NOTIFYBYPOS     #x08000000)
(defwinconstant MNS_CHECKORBMP      #x04000000)

;; MENUINFO fMask flags
(defwinconstant MIM_MAXHEIGHT       #x00000001)
(defwinconstant MIM_BACKGROUND      #x00000002)
(defwinconstant MIM_HELPID          #x00000004)
(defwinconstant MIM_MENUDATA        #x00000008)
(defwinconstant MIM_STYLE           #x00000010)
(defwinconstant MIM_APPLYTOSUBMENUS #x80000000)

(defwinapi GetMenu ((hWnd HWND))
   :return-type HMENU
   :library-name "user32.dll"
   :entry-name "GetMenu"
   :linkage-type :pascal)

(defwinapi CreateMenu ()
   :return-type HMENU
   :library-name "user32.dll"
   :entry-name "CreateMenu"
   :linkage-type :pascal)

(defwinapi CreatePopupMenu ()
   :return-type HMENU
   :library-name "user32.dll"
   :entry-name "CreatePopupMenu"
   :linkage-type :pascal)

;; BOOL GetMenuItemInfo(HMENU hMenu, UINT uItem, BOOL fByPosition, LPMENUITEMINFO lpmii); 
(defwinapi GetMenuItemInfo (
					(hMenu HMENU)
					(uItem UINT)
					(fByPosition BOOL) 
					(lpmii LPMENUITEMINFO))
   :return-type BOOL
   :library-name "user32.dll"
   :entry-name "GetMenuItemInfoA"
   :linkage-type :pascal)
 
;; BOOL AppendMenu(HMENU hMenu, UINT uFlags, INT uIDNewItem, LPCTSTR lpNewItem);
(defwinapi AppendMenu ((hMenu HMENU)
					      (uFlags UINT)
						  (uIDNewItem INT) 
						  (lpNewItem LPCTSTR))
   :return-type BOOL
   :library-name "user32.dll"
   :entry-name "AppendMenuA"
   :linkage-type :pascal)

;; BOOL InsertMenu(HMENU hMenu, UINT uPosition, UINT uFlags, UINT uIDNewItem, LPCTSTR lpNewItem);
(defwinapi InsertMenu ((hMenu HMENU)
							(uPosition UINT)
							(uFlags UINT)
						  	(uIDNewItem INT) 
						  	(lpNewItem LPCTSTR))
   :return-type BOOL
   :library-name "user32.dll"
   :entry-name "InsertMenuA"
   :linkage-type :pascal)


;; BOOL DrawMenuBar(HWND hWnd);
(defwinapi DrawMenuBar ((hWnd HWND))
   :return-type BOOL
   :library-name "user32.dll"
   :entry-name "DrawMenuBar"
   :linkage-type :pascal)
 
;; int GetMenuString(HMENU hMenu, UINT uIDItem, LPTSTR lpString, int nMaxCount, UINT uFlag); 
(defwinapi GetMenuString ((hMenu HMENU)
					      (uIDItem UINT)
						  (lpString LPTSTR) 
						  (nMaxCount int)
						  (uFlag UINT))
   :return-type int
   :library-name "user32.dll"
   :entry-name "GetMenuStringA"
   :linkage-type :pascal)

;; int GetMenuItemCount(HMENU hMenu);
(defwinapi GetMenuItemCount ((hMenu HMENU))
   :return-type int
   :library-name "user32.dll"
   :entry-name "GetMenuItemCount"
   :linkage-type :pascal)

;; HMENU GetSubMenu(HMENU hMenu, int nPos);
(defwinapi GetSubMenu ((hMenu HMENU)(nPos int))
   :return-type HMENU
   :library-name "user32.dll"
   :entry-name "GetSubMenu"
   :linkage-type :pascal)

;; UINT GetMenuItemID(HMENU hMenu, int nPos);
(defwinapi GetMenuItemID ((hMenu HMENU)(nPos int))
   :return-type UINT
   :library-name "user32.dll"
   :entry-name "GetMenuItemID"
   :linkage-type :pascal)

;; UINT GetMenuInfo(HMENU hmenu, LPCMENUINFO lpcmi);
(defwinapi GetMenuInfo ((hMenu HMENU)(lpcmi LPCMENUINFO))
   :return-type BOOL
   :library-name "user32.dll"
   :entry-name "GetMenuInfo"
   :linkage-type :pascal)

;; UINT SetMenuInfo(HMENU hmenu, LPCMENUINFO lpcmi);
(defwinapi SetMenuInfo ((hMenu HMENU)(lpcmi LPCMENUINFO))
   :return-type BOOL
   :library-name "user32.dll"
   :entry-name "SetMenuInfo"
   :linkage-type :pascal)

;; BOOL DestroyMenu(HMENU hMenu);
(defwinapi DestroyMenu ((hMenu HMENU))
   :return-type BOOL
   :library-name "user32.dll"
   :entry-name "DestroyMenu"
   :linkage-type :pascal)

;; DWORD CheckMenuItem(HMENU hmenu, UINT uIDCheckItem, UINT uCheck);
(defwinapi CheckMenuItem ((hMenu HMENU)(uIDCheckItem UINT)(uCheck UINT))
   :return-type DWORD
   :library-name "user32.dll"
   :entry-name "CheckMenuItem"
   :linkage-type :pascal)

;;;
;;;		CREATE-USER-MENU
;;;		Given a name, creates a new menu by that name and appends
;;;		it to the end of the main frame menu bar. The Newly created
;;;		menu handle is returned.
;;;
(defun create-user-menu (name)
	(let* ((main-wnd (cl::get-application-main-window))
		   (main-menu (GetMenu main-wnd))
		   (new-menu (CreateMenu)))
		(AppendMenu main-menu 
			(logior MF_ENABLED MF_STRING MF_POPUP) 
			(cl::foreign-ptr-to-int new-menu) 
			(create-c-string name))
		(DrawMenuBar main-wnd)
		new-menu))

(defun create-notify-by-command-menu ()
    (CreatePopupMenu))

(defun create-notify-by-position-menu ()
    (let ((hmenu (CreatePopupMenu)))
        (ct:with-fresh-foreign-block (menuinfo 'win:MENUINFO)
            (setf (ct:cref win:MENUINFO menuinfo cbSize) (ct:sizeof 'win:MENUINFO)
                   (ct:cref win:MENUINFO menuinfo fMask) win:MIM_STYLE)
            (GetMenuInfo hmenu menuinfo)
            (setf (ct:cref win:MENUINFO menuInfo dwStyle)
                    (logior (ct:cref win:MENUINFO menuInfo dwStyle) win:MNS_NOTIFYBYPOS))
            (SetMenuInfo hmenu menuinfo))
        hmenu))

(defun menu-notify-by-position-p (hmenu)
    (ct:with-fresh-foreign-block (menuinfo 'win:MENUINFO)
        (setf (ct:cref win:MENUINFO menuinfo cbSize) (ct:sizeof 'win:MENUINFO)
               (ct:cref win:MENUINFO menuinfo fMask) win:MIM_STYLE)
        (GetMenuInfo hmenu menuinfo)
        (not (zerop (logand (ct:cref win:MENUINFO menuInfo dwStyle) win:MNS_NOTIFYBYPOS)))))

(defun set-menu-notify-by-position (hmenu)
    (ct:with-fresh-foreign-block (menuinfo 'win:MENUINFO)
        (setf (ct:cref win:MENUINFO menuinfo cbSize) (ct:sizeof 'win:MENUINFO)
               (ct:cref win:MENUINFO menuinfo fMask) win:MIM_STYLE)
        (GetMenuInfo hmenu menuinfo)
        (setf (ct:cref win:MENUINFO menuInfo dwStyle)
                    (logior (ct:cref win:MENUINFO menuInfo dwStyle) win:MNS_NOTIFYBYPOS))
        (SetMenuInfo hmenu menuinfo)))
    
(defvar menu-string-list nil)

(defun find-named-menu (menu name)
    (declare (ignore menu name))
    ;; defined below
    )

(defun get-main-menu ()
	(let* ((main-wnd (cl::get-application-main-window))
		   (main-menu (GetMenu main-wnd)))
		main-menu))

(defparameter *menu-mappings* (make-hash-table :test 'equalp))
(defparameter *last-dynamic-menu* nil)

(defun append-menu-item-impl (menu id item-name command-func message checked enabled break separator)
	(AppendMenu menu 
		(logior 
            (if enabled MF_ENABLED MF_DISABLED)
            (if separator MF_SEPARATOR MF_STRING) ;; must be either string xor separator
            (if checked MF_CHECKED MF_UNCHECKED)
            (if break MF_MENUBARBREAK 0)) 
		id 
		(create-c-string item-name))
	(setf (gethash id *menu-mappings*) (list command-func message))
	t)
    
(defun append-menu-item (menu item-name command-func 
        &key message 
             checked 
            (enabled t)
            break
            separator)
    (append-menu-item-impl menu 
        (incf *id-lisp-menu-item*) 
        item-name command-func message checked enabled break separator))

(defun append-dynamic-menu-item (menu item-name command-func 
        &key message 
            checked 
            (enabled t)
            break
            separator)
    (append-menu-item-impl menu 
        (incf *id-lisp-dynamic-menu-item*) 
        item-name command-func message checked enabled break separator))

(defun insert-menu-item-impl (menu position id item-name command-func message checked enabled break separator)
	(InsertMenu menu 
		position
		(logior 
            (if enabled MF_ENABLED MF_DISABLED) 
            (if separator MF_SEPARATOR MF_STRING) ;; must be either string xor separator
            (if checked MF_CHECKED MF_UNCHECKED)
            (if break MF_MENUBREAK 0)) 
		id 
		(create-c-string item-name))
	(setf (gethash id *menu-mappings*) (list command-func message))
	t)

(defun insert-menu-item (menu position item-name command-func 
        &key message 
        checked 
        (enabled t)
        break
        separator)
    (insert-menu-item-impl menu position 
        (incf *id-lisp-menu-item*) 
        item-name command-func message checked enabled break separator))

(defun insert-dynamic-menu-item (menu position item-name command-func 
        &key message 
            checked 
            (enabled t) 
            break
            separator)
    (insert-menu-item-impl menu position 
        (incf *id-lisp-dynamic-menu-item*) 
        item-name command-func message checked enabled break separator))
        
;; returns the new menu
(defun insert-sub-menu-impl (menu position item-name init-func uninit-func)
    (let ((new-menu (create-notify-by-command-menu)))
        (InsertMenu 
			menu 
			position 
			(logior MF_ENABLED MF_STRING MF_BYPOSITION MF_POPUP)
			(cl::foreign-ptr-to-int new-menu)
			(create-c-string item-name))
        (if init-func
            (setf (gethash new-menu *menu-mappings*) (list init-func uninit-func)))
        new-menu))

;; returns the new menu
(defun insert-sub-menu (menu position item-name &optional init-func uninit-func)
    (insert-sub-menu-impl menu position item-name init-func uninit-func))

;; returns the new menu
(defun insert-dynamic-sub-menu (menu position item-name &optional init-func uninit-func)
    (insert-sub-menu-impl menu position item-name init-func uninit-func))

;; returns the new menu
(defun append-sub-menu-impl (menu item-name init-func uninit-func)
    (let ((new-menu (create-notify-by-command-menu)))
        (AppendMenu 
			menu 
			(logior MF_ENABLED MF_STRING MF_BYPOSITION MF_POPUP)
			(cl::foreign-ptr-to-int new-menu)
			(create-c-string item-name))
        (if init-func
            (setf (gethash new-menu *menu-mappings*) (list init-func uninit-func)))
        new-menu))

;; returns the new menu
(defun append-sub-menu (menu item-name &optional init-func uninit-func)
    (append-sub-menu-impl menu item-name init-func uninit-func))

;; returns the new menu
(defun append-dynamic-sub-menu (menu item-name &optional init-func uninit-func)
    (append-sub-menu-impl menu item-name init-func uninit-func))
        
;;;
;;; Remove an item from a menu, given the menu handle and position
;;;
(defun remove-menu-item (parent position)
    (let* (id)
        (if (null parent)
            (setf parent (get-main-menu))
            (if (stringp parent)
                (setf parent (find-named-menu (win:get-main-menu) parent))))
        (setf id (GetMenuItemID parent position))
        
        ;; if the returned id = -1 then it is a submenu
        (if (= id -1)
            (setf id (GetSubMenu parent position)))
        
        (win:RemoveMenu parent position win:MF_BYPOSITION)
        (remhash id *menu-mappings*)
        (if (foreignp id)   ;; if it was a submenu
            (win:DestroyMenu id))))

;;;
;;; Remove an item from a menu, given the menu handle and position
;;;
(defun remove-all-menu-items (hmenu)
    ;; remove all dynamic menu ids from menu mappings, and clear the menu
    (let* ((count (win:GetMenuItemCount hmenu)))
        (loop for i from (- count 1) downto 0 do
            (win:remove-menu-item hmenu i))))
      
(defun ccl::execute-user-command (id)
    (let ((func (gethash id *menu-mappings*)))
        (if (consp func)
            (setf func (first func)))
        (if (or (functionp func) (and func (symbolp func)))
            (ignore-errors (funcall func id)))))

(defun execute-menu-popup-init (hmenu)
    (let ((func (gethash hmenu *menu-mappings*)))
        (if (consp func)
            (setf func (first func)))
        (if (or (functionp func) (and func (symbolp func)))
            (ignore-errors (funcall func hmenu))))) 
                
(defun execute-menu-popup-uninit (hmenu)
    (let ((func (gethash hmenu *menu-mappings*)))
        (when (consp func)
            (setf func (second func))
            (if (or (functionp func) (and func (symbolp func)))
                (ignore-errors (funcall func hmenu))))))

(defun user-command-message (id)
    (let ((func (gethash id *menu-mappings*)))
        (when (consp func)
            (setf func (second func))
            (if (or (functionp func) (and func (symbolp func)))
                (ignore-errors (funcall func id))
                (if (stringp func)
                    func)))))
                    
(defun get-menu-item-name (menu index)
	(let* ((size-info (ct:sizeof 'MENUITEMINFO))
		   (info (ct:malloc size-info))
		   (buf (ct:malloc 256))
			ret)
		(setf (cref MENUITEMINFO info cbSize) size-info)
		(setf (cref MENUITEMINFO info fMask) MIIM_TYPE)
		(setf (cref MENUITEMINFO info dwTypeData) buf)
		
		(setq ret (GetMenuItemInfo menu index 1 info))
		(if (eq ret nil) nil (ct::c-string-to-lisp-string buf))))

(defconstant *max-menu-name-length* 255)

(defun get-menu-hierarchy (menu)
	(let* ((count (GetMenuItemCount menu))
			(buf (ct:malloc (+ *max-menu-name-length* 1)))
			(result nil))
		(dotimes (i count)
			(let (name submenu)
				(if (> (GetMenuString menu i buf 
						*max-menu-name-length* MF_BYPOSITION) 0)
					(setq name (ct:c-string-to-lisp-string buf))
					(setq name nil))
				(setq submenu (GetSubMenu menu i))
				(if (zerop (cl::foreign-ptr-to-int submenu))
					(push name result)
					(push (cons name (get-menu-hierarchy submenu)) result))))
		(nreverse result)))

(defun get-main-menu-hierarchy ()
	(let* ((main-wnd (cl::get-application-main-window))
		   (main-menu (GetMenu main-wnd)))
		(get-menu-hierarchy main-menu)))

#|
(setq my-menu (create-user-menu "Roger"))
(append-menu-item my-menu "Corman" 
	#'(lambda (x) (format t "This is the Corman command!~%")))
|#

(defun item-is-submenu (menu)
	(/= (cl::foreign-ptr-to-int menu) 0))

(defun foreign-ptr-equal (x1 x2)
	(and (cl::foreign-ptr-p x1)
		 (cl::foreign-ptr-p x2)
		(= (cl::foreign-ptr-to-int x1) (cl::foreign-ptr-to-int x2))))

(defun find-named-menu-impl (menu name)
	(let* ((count (GetMenuItemCount menu))
			(buf (ct:malloc (+ *max-menu-name-length* 1)))
			(result nil))
		(dotimes (i count)
			(let* (menu-name 
					(submenu (GetSubMenu menu i)))
				(when (and (item-is-submenu submenu)
							(> (GetMenuString menu i buf *max-menu-name-length* MF_BYPOSITION) 0))
					(setq menu-name (ct:c-string-to-lisp-string buf))
					(if (string= name menu-name)
						(return (setq result (GetSubMenu menu i)))
						(progn
							(setq result (find-named-menu-impl submenu name))
							(if result (return result)))))))
		result))

(defvar *menu-last-lookup* nil)
(defvar *menu-last-lookup-name* nil)
(defvar *menu-last-lookup-result* nil)

(defun find-named-menu (menu name)
	(if (and *menu-last-lookup-result*
		(foreign-ptr-equal menu *menu-last-lookup*)
		(equal name *menu-last-lookup-name*))
		(return-from find-named-menu *menu-last-lookup-result*))
	(setq *menu-last-lookup* menu)
	(setq *menu-last-lookup-name* name)
	(let ((result (find-named-menu-impl menu name)))
		(setq *menu-last-lookup-result* result)
		result))

(defun set-menu-item-checked (hmenu position flag)
    (CheckMenuItem hmenu position (logior MF_BYPOSITION (if flag MF_CHECKED MF_UNCHECKED))))

(defun uninit-dynamic-menu (hmenu)
    (remove-all-menu-items hmenu)
    (setf *id-lisp-dynamic-menu-item* *id-lisp-dynamic-menu-item-start*))

(defun remember-last-dynamic-menu (hmenu)
    (setf *last-dynamic-menu* hmenu))

	
;;	Ex:		(create-menu-item '(:menu "foo") nil 3)
;;			Inserts a new menu foo into the main menu bar at position 3.
;;
;;			(create-menu-item '(:menu "bar") "foo" 1)
;;			Inserts a new submenu bar into the menu foo at position 1.
;;
;;			(create-menu-item '(:command "my-item" my-func) "bar" 2)
;;			Inserts a new item my-item into the bar submenu at position 2.

(defun create-menu-item-impl (option parent position message checked enabled dynamic break separator)
	(let* ((key (first option))
		    (main-menu (get-main-menu)))
		(if (eq key :menu)
			(let* ((name (second option))
                  (func (third option))
                  (need-redraw nil)
                  (new-menu)
                  (add-func (if dynamic 'insert-dynamic-sub-menu 'insert-sub-menu))
                  (popup-init-func
                        (if dynamic
                            (lambda (hmenu)
                                (when *last-dynamic-menu*
                                    (uninit-dynamic-menu *last-dynamic-menu*) ;; clear previous dynamic menu
                                    (setf *last-dynamic-menu* nil))
                                (funcall func hmenu))
                            func))
                  (popup-uninit-func
                        (if dynamic 'remember-last-dynamic-menu nil)))
                
                (if (null parent)
                    (setf parent (get-main-menu) need-redraw t)
                    (if (stringp parent)
                        (setf parent (find-named-menu (get-main-menu) parent))))
                (setf new-menu
                    (funcall add-func 
    					parent 
    					position 
    					(create-c-string name)
                        popup-init-func
                        popup-uninit-func))
                (if need-redraw
                    (win:DrawMenuBar (cl::get-application-main-window)))
                new-menu)
                
			(if (eq key :command)
				(let* ((name (second option))
				       (func (third option))
                       (add-func (if dynamic 'insert-dynamic-menu-item 'insert-menu-item)))
					(if (null parent)
						(error "No menu was specified"))
					(funcall add-func
						(if (stringp parent)
                            (find-named-menu main-menu parent)
                            parent) 
						position 
						name
						func
                        :message message
                        :checked checked
                        :enabled enabled
                        :break break
                        :separator separator))
				(error "Invalid option in CREATE-MENU-ITEM: ~A" key)))))

(defun create-menu-item (option parent position &key message checked (enabled t) break separator)
    (create-menu-item-impl option parent position message checked enabled nil break separator))

(defun create-dynamic-menu-item (option parent position &key message checked (enabled t) break separator)
    (create-menu-item-impl option parent position message checked enabled t break separator))

    
       
        
    
