;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;
;;;		File:		Registry-Utils.lisp
;;;		Contents:	Utility functions for reading/writing registry entries
;;;     History:    6/23/02  RGC  Created.
;;;
(defun reg-lookup (root key1 &rest args)
	(let* ((ret nil)
		   (root-sym (find-symbol root (find-package :win32)))
		   (keyptr (ct:malloc (ct:sizeof '(win:HKEY *))))
		   root-val
		   k1
		   k2
		   (num-args (length args)))
		(if (null args)
			(return-from reg-lookup nil))
		(unless root-sym (return-from reg-lookup nil))
		(setq root-val (symbol-value root-sym))
		(unless (integerp root-val) (return-from reg-lookup nil))
		(setf (ct:cref (win:HKEY *) keyptr 0)(cl::int-to-foreign-ptr root-val))
		(setq k1 
			(open-subkey keyptr key1))
		(if (null k1)
			(return-from reg-lookup nil))		
		(dotimes (i (- num-args 1))
			(setq k2 (open-subkey k1 (car args)))
			(close-reg-entry k1)
			(if (null k2)
				(return-from reg-lookup nil))
			(setq k1 k2)
			(setq args (cdr args)))
		(setq ret (reg-query-value k1 (car args)))
		(close-reg-entry k1)
		ret))

(defun reg-set (root key1 &rest args)
	(let* ((ret nil)
		   (root-sym (find-symbol root (find-package :win32)))
		   (keyptr (ct:malloc (ct:sizeof '(win:HKEY *))))
		   root-val
		   k1
		   k2
		   (num-args (length args)))
		(if (null args)
			(return-from reg-set nil))
		(unless root-sym (return-from reg-set nil))
		(setq root-val (symbol-value root-sym))
		(unless (integerp root-val) (return-from reg-set nil))
		(setf (ct:cref (win:HKEY *) keyptr 0)(cl::int-to-foreign-ptr root-val))
		(setq k1 
			(open-subkey keyptr key1))
		(if (null k1)
			(return-from reg-set nil))		
		(dotimes (i (- num-args 2))
			(setq k2 (open-subkey k1 (car args)))
			(close-reg-entry k1)
			(if (null k2)
				(return-from reg-set nil))
			(setq k1 k2)
			(setq args (cdr args)))
		(setq ret (reg-set-value k1 (car args) (cadr args)))
		(close-reg-entry k1)
		ret))

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

(defun close-reg-entry (hk)
	(win:RegCloseKey (ct:cref (win:HKEY *) hk 0)))

(defun reg-query-value (hkey name)
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
			(return-from reg-query-value nil))
		(setq buf (ct:malloc (ct:cref win:LPDWORD bufsize 0)))
		(setq ret (win:RegQueryValueEx 
						(ct:cref (win:HKEY *) hkey 0) 
						cname
						null
						null
						buf
						bufsize))
		(if (/= ret win:ERROR_SUCCESS)
			(return-from reg-query-value nil))
		(ct:c-string-to-lisp-string buf)))

(defun reg-set-value (hkey name data)
	(let* ((ret (win:RegSetValueEx 
					(ct:cref (win:HKEY *) hkey 0) 
					(ct:create-c-string name)
					0
					win:REG_SZ
					(ct:lisp-string-to-c-string data)
					(+ 1 (length data)))))
		(if (/= ret win:ERROR_SUCCESS)
			(return-from reg-set-value nil))
		data))

#|
;; example:

(reg-lookup "HKEY_CLASSES_ROOT"
	"CLSID" 	
	"{00000010-0000-0010-8000-00AA006D2EA4}"
	"ProgID"
	"")
|#


