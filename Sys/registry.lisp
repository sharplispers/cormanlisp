;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;
;;;		File:		Registry-Utils.lisp
;;;		Contents:	Utility functions for reading/writing registry entries
;;;     History:    6/23/02  RGC  Created.
;;;

(in-package :win)

(export '(registry-set registry-lookup))

(defun open-subkey (hkey subkey-name)
	(let* ((hkptr (ct:malloc (ct:sizeof '(win:HKEY *))))
		   (ret (win:RegOpenKeyEx 
					(ct:cref (win:HKEY *) hkey 0) 
					(ct:create-c-string subkey-name) 
					0 (logior win:KEY_READ win:KEY_WRITE) hkptr)))
		(if (= ret win:ERROR_SUCCESS)
			hkptr
			(progn
				(ct:free hkptr)
				nil))))

(defun close-registry-entry (hk)
	(win:RegCloseKey (ct:cref (win:HKEY *) hk 0)))

(defun registry-query-value (hkey name)
	(let* ((buf nil)
		   (bufsize (ct:malloc (ct:sizeof 'win:LPDWORD)))
		   (cname (ct:create-c-string name))
		   (ret (win:RegQueryValueEx 
					(ct:cref (win:HKEY *) hkey 0) 
					cname
					null
					null
					null
					bufsize)))
		(if (/= ret win:ERROR_SUCCESS)
			(return-from registry-query-value nil))
		(setq buf (ct:malloc (ct:cref win:LPDWORD bufsize 0)))
		(setq ret (win:RegQueryValueEx 
						(ct:cref (win:HKEY *) hkey 0) 
						cname
						null
						null
						buf
						bufsize))
		(if (/= ret win:ERROR_SUCCESS)
			(return-from registry-query-value nil))
		(ct:c-string-to-lisp-string buf)))

(defun registry-set-value (hkey name data)
	(let* ((ret (win:RegSetValueEx 
					(ct:cref (win:HKEY *) hkey 0) 
					(ct:create-c-string name)
					0
					win:REG_SZ
					(ct:lisp-string-to-c-string data)
					(+ 1 (length data)))))
		(if (/= ret win:ERROR_SUCCESS)
			(return-from registry-set-value nil))
		data))

(defun registry-set (root key1 &rest args)
	(let* ((ret nil)
		   (root-sym (find-symbol root (find-package :win32)))
		   (keyptr (ct:malloc (ct:sizeof '(win:HKEY *))))
		   root-val
		   k1
		   k2
		   (num-args (length args)))
		(if (null args)
			(return-from registry-set nil))
		(unless root-sym (return-from registry-set nil))
		(setq root-val (symbol-value root-sym))
		(unless (integerp root-val) (return-from registry-set nil))
		(setf (ct:cref (win:HKEY *) keyptr 0)(cl::int-to-foreign-ptr root-val))
		(setq k1 
			(open-subkey keyptr key1))
		(if (null k1)
			(return-from registry-set nil))		
		(dotimes (i (- num-args 2))
			(setq k2 (open-subkey k1 (car args)))
			(close-registry-entry k1)
			(if (null k2)
				(return-from registry-set nil))
			(setq k1 k2)
			(setq args (cdr args)))
		(setq ret (registry-set-value k1 (car args) (cadr args)))
		(close-registry-entry k1)
		ret))

(defun registry-lookup (root key1 &rest args)
	(let* ((ret nil)
		   (root-sym (find-symbol root (find-package :win32)))
		   (keyptr (ct:malloc (ct:sizeof '(win:HKEY *))))
		   k1
		   k2
		   (num-args (length args)))
        (if (and args root-sym)
            (let ((root-val (symbol-value root-sym)))
        		(if (integerp root-val)
                    (progn
                		(setf (ct:cref (win:HKEY *) keyptr 0)(cl::int-to-foreign-ptr root-val))
                		(setq k1 
                			(open-subkey keyptr key1))
                		(if k1
                            (progn
                        		(dotimes (i (- num-args 1))
                        			(setq k2 (open-subkey k1 (car args)))
                        			(close-registry-entry k1)
                        			(if (null k2)
                        				(return-from registry-lookup nil))
                        			(setq k1 k2)
                        			(setq args (cdr args)))
                        		(setq ret (registry-query-value k1 (car args)))
                        		(close-registry-entry k1)
                        		ret))))))))

#|
;; example:

(reg-lookup "HKEY_CLASSES_ROOT"
	"CLSID" 	
	"{00000010-0000-0010-8000-00AA006D2EA4}"
	"ProgID"
	"")
|#


