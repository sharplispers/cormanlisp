;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		vm.lisp
;;;;	Contents:	Functions to deal with virtual memory blocks.
;;;;
(in-package :win32)

(defconstant PAGE_NOACCESS          #x01)     
(defconstant PAGE_READONLY          #x02)     
(defconstant PAGE_READWRITE         #x04)     
(defconstant PAGE_WRITECOPY         #x08)     
(defconstant PAGE_EXECUTE           #x10)     
(defconstant PAGE_EXECUTE_READ      #x20)     
(defconstant PAGE_EXECUTE_READWRITE #x40)     
(defconstant PAGE_EXECUTE_WRITECOPY #x80)     
(defconstant PAGE_GUARD            #x100)     
(defconstant PAGE_NOCACHE          #x200)     
(defconstant MEM_COMMIT           #x1000)     
(defconstant MEM_RESERVE          #x2000)     
(defconstant MEM_DECOMMIT         #x4000)     
(defconstant MEM_RELEASE          #x8000)     
(defconstant MEM_FREE            #x10000)     
(defconstant MEM_PRIVATE         #x20000)     
(defconstant MEM_MAPPED          #x40000)     
(defconstant MEM_RESET           #x80000)     
(defconstant MEM_TOP_DOWN       #x100000)     
(defconstant SEC_FILE           #x800000)     
(defconstant SEC_IMAGE         #x1000000)     
(defconstant SEC_RESERVE       #x4000000)     
(defconstant SEC_COMMIT        #x8000000)     
(defconstant SEC_NOCACHE      #x10000000)     
(defconstant MEM_IMAGE         SEC_IMAGE)     

(defconstant mem-flag-strings
(list
	PAGE_NOACCESS "PAGE_NOACCESS"
	PAGE_READONLY "PAGE_READONLY"
	PAGE_READWRITE "PAGE_READWRITE"
	PAGE_WRITECOPY "PAGE_WRITECOPY"
	PAGE_EXECUTE "PAGE_EXECUTE"
	PAGE_EXECUTE_READ "PAGE_EXECUTE_READ"
	PAGE_EXECUTE_READWRITE "PAGE_EXECUTE_READWRITE"
	PAGE_EXECUTE_WRITECOPY "PAGE_EXECUTE_WRITECOPY"
	PAGE_GUARD "PAGE_GUARD"
	PAGE_NOCACHE "PAGE_NOCACHE"
	MEM_COMMIT "MEM_COMMIT"
	MEM_RESERVE "MEM_RESERVE"
	MEM_DECOMMIT "MEM_DECOMMIT"
	MEM_RELEASE "MEM_RELEASE"
	MEM_FREE "MEM_FREE"
	MEM_PRIVATE "MEM_PRIVATE"
	MEM_MAPPED "MEM_MAPPED"
	MEM_RESET "MEM_RESET"
	MEM_TOP_DOWN "MEM_TOP_DOWN"
	SEC_FILE "SEC_FILE"
	SEC_IMAGE "SEC_IMAGE"
	SEC_RESERVE "SEC_RESERVE"
	SEC_COMMIT "SEC_COMMIT"
	SEC_NOCACHE "SEC_NOCACHE"
	MEM_IMAGE "MEM_IMAGE"))

(defun format-mem-flags (flags)
	(with-output-to-string (out-string)
		(do* ((p mem-flag-strings (cddr p))
		  	(flag (car p)(car p))
		  	(string (cadr p)(cadr p)))
			((null p))
			(unless (zerop (logand flags flag))
				(format out-string "~A " string)))))
 

(ct:defcstruct MEMORY_BASIC_INFORMATION
	((BaseAddress (:void *))
	 (AllocationBase (:void *))
	 (AllocationProtect :unsigned-long)
	 (RegionSize :unsigned-long)
	 (State :unsigned-long)
	 (Protect :unsigned-long)
	 (Type :unsigned-long)))

(ct:defun-dll VirtualQuery 
		((lpAddress (:void *))
		 (lpBuffer (:void *))
		 (dwLength :unsigned-long))
	:return-type :unsigned-long
	:library-name "kernel32.dll"
	:entry-name "VirtualQuery"
	:linkage-type :pascal)

;;; Get the current stack pointer
(pl:defasm get-esp ()
{
	push	ebp
	mov		ebp, esp
	push 	ebp
	callp	cl::%create-unsigned-lisp-integer
	add		esp, 4
	mov		ecx, 1
	pop		ebp
	ret
})

(defun get-mem-block-info (address)
	(let* ((mem-info-size (ct:sizeof 'MEMORY_BASIC_INFORMATION))
		   (mem-info (ct:malloc mem-info-size))
		   (ret (virtualquery address mem-info mem-info-size)))
		(if (/= ret mem-info-size)
			(error "The call to VirtualQuery() returned ~A" ret))
		mem-info))

(defun format-mem-block-info (s address)
	(let* ((info (get-mem-block-info address)))
		(format s "~A#x~x:~%" "Memory block at address " (ct:foreign-ptr-to-int address))
		(format s "~30A #x~x~%" "Base Address:"
			(cl::foreign-ptr-to-int
				(ct:cref MEMORY_BASIC_INFORMATION info BaseAddress)))
		(format s "~30A #x~x~%" "Allocation Base:"
			(cl::foreign-ptr-to-int
				(ct:cref MEMORY_BASIC_INFORMATION info AllocationBase)))
		(format s "~30A ~A~%" "Allocation Protect:"
			(format-mem-flags
				(ct:cref MEMORY_BASIC_INFORMATION info AllocationProtect)))
		(format s "~30A #x~x bytes~%" "Region Size:"
			(ct:cref MEMORY_BASIC_INFORMATION info RegionSize))
		(format s "~30A ~A~%" "State:"
			(format-mem-flags
				(ct:cref MEMORY_BASIC_INFORMATION info State)))
		(format s "~30A ~A~%" "Protect:"
			(format-mem-flags
				(ct:cref MEMORY_BASIC_INFORMATION info Protect)))
		(format s "~30A ~A~%" "Type:"
			(format-mem-flags
				(ct:cref MEMORY_BASIC_INFORMATION info Type)))))

(defun show-stack-mem-info (&optional (stream t))
	(format-mem-block-info stream (ct:int-to-foreign-ptr (get-esp))))
