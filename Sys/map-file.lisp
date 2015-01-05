;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		map-file.lisp
;;;;	Contents:	MAP-FILE, UNMAP-FILE implementation.
;;;;	History:	2/2/99  RGC  Created.
;;;;

(in-package :win)

(defwinconstant PAGE_NOACCESS          #x01)     
(defwinconstant PAGE_READONLY          #x02)     
(defwinconstant PAGE_READWRITE         #x04)     
(defwinconstant PAGE_WRITECOPY         #x08)     
(defwinconstant PAGE_EXECUTE           #x10)     
(defwinconstant PAGE_EXECUTE_READ      #x20)     
(defwinconstant PAGE_EXECUTE_READWRITE #x40)     
(defwinconstant PAGE_EXECUTE_WRITECOPY #x80)     
(defwinconstant PAGE_GUARD            #x100)     
(defwinconstant PAGE_NOCACHE          #x200)     
(defwinconstant MEM_COMMIT           #x1000)     
(defwinconstant MEM_RESERVE          #x2000)     
(defwinconstant MEM_DECOMMIT         #x4000)     
(defwinconstant MEM_RELEASE          #x8000)     
(defwinconstant MEM_FREE            #x10000)     
(defwinconstant MEM_PRIVATE         #x20000)     
(defwinconstant MEM_MAPPED          #x40000)     
(defwinconstant MEM_RESET           #x80000)     
(defwinconstant MEM_TOP_DOWN       #x100000)     
(defwinconstant SEC_FILE           #x800000)     
(defwinconstant SEC_IMAGE         #x1000000)     
(defwinconstant SEC_RESERVE       #x4000000)     
(defwinconstant SEC_COMMIT        #x8000000)     
(defwinconstant SEC_NOCACHE      #x10000000)     
(defwinconstant MEM_IMAGE         SEC_IMAGE)     

(defwinconstant STANDARD_RIGHTS_REQUIRED	#x000F0000)
(defwinconstant SECTION_QUERY	 	 		#x0001)
(defwinconstant SECTION_MAP_WRITE	 		#x0002)
(defwinconstant SECTION_MAP_READ	 		#x0004)
(defwinconstant SECTION_MAP_EXECUTE	 		#x0008)
(defwinconstant SECTION_EXTEND_SIZE	 		#x0010)
(defwinconstant SECTION_ALL_ACCESS	 		(logior STANDARD_RIGHTS_REQUIRED SECTION_QUERY
												SECTION_MAP_WRITE SECTION_MAP_READ 
												SECTION_MAP_EXECUTE SECTION_EXTEND_SIZE))


(defwinconstant FILE_MAP_COPY       		SECTION_QUERY)
(defwinconstant FILE_MAP_WRITE      		SECTION_MAP_WRITE)
(defwinconstant FILE_MAP_READ       		SECTION_MAP_READ)
(defwinconstant FILE_MAP_ALL_ACCESS 		SECTION_ALL_ACCESS)

(defwinapi CreateFileMapping
	((hFile HANDLE)
	 (lpFileMappingAttributes LPSECURITY_ATTRIBUTES)
	 (flProtect DWORD)
	 (dwMaximumSizeHigh DWORD)
	 (dwMaximumSizeLow DWORD)
	 (lpName LPCSTR))
	:return-type HANDLE
	:library-name "kernel32.dll"
	:entry-name "CreateFileMappingA"
	:linkage-type :pascal)

#! (:library "kernel32" :export t :pascal "WINAPI")
LPVOID WINAPI MapViewOfFile(HANDLE hFileMappingObject, DWORD dwDesiredAccess, DWORD dwFileOffsetHigh, DWORD dwFileOffsetLow, DWORD dwNumberOfBytesToMap);
BOOL WINAPI UnmapViewOfFile(LPCVOID lpBaseAddress);
!#

(in-package :ccl)
(export '(map-file unmap-file))

(defun map-file (path)
	"Usage:  (MAP-FILE pathname)
	     Returns (1) address of the memory mapped file (or null if an error occurred)
	             (2) the length (in bytes)"
	(let ((hfile (win:CreateFile
					(ct:create-c-string (namestring (pathname path)))
					win:GENERIC_READ
					win:FILE_SHARE_READ
					NULL
					win:OPEN_EXISTING
					win:FILE_ATTRIBUTE_NORMAL
					NULL))
			(length nil))
		(if (= (ct:foreign-ptr-to-int hfile) win:INVALID_HANDLE_VALUE)
			(return-from map-file nil))
		(setf length (win:GetFileSize hfile null))
		(when (= length #xffffffff)
			(win:CloseHandle hfile)
			(return-from map-file nil))
		(let ((hfilemap
					(win:CreateFileMapping
						hfile
						null
						win:PAGE_READONLY
						0
						0
						null)))
			(win:CloseHandle hfile)
			(if (ct:cpointer= hfilemap null)
				(return-from map-file nil))
			(let ((mapped-address
						(win:MapViewOfFile
							hfilemap
							win:FILE_MAP_READ
							0
							0
							0)))
				(win:CloseHandle hfilemap)
				(values mapped-address length)))))

(defun unmap-file (mapped-address)
	(win:UnmapViewOfFile mapped-address))	

(defpackage "ZLIB"
    (:export 
		"COMPRESS" 
		"UNCOMPRESS"
        "COMPRESS-BOUND"
    ))

(in-package :zlib)

;; redefine kernel compression functions
(ct:defun-kernel compress ((dest (:unsigned-char *))(destlen (:unsigned-long *))
        (src (:unsigned-char *)) (srclen :unsigned-long))
    :return-type :long
    :kernel-name cl::%compress-foreign-bytes
    :linkage-type :c)

(ct:defun-kernel uncompress ((dest (:unsigned-char *))(destlen (:unsigned-long *))
        (src (:unsigned-char *)) (srclen :unsigned-long))
    :return-type :long
    :kernel-name cl::%uncompress-foreign-bytes
    :linkage-type :c)

(ct:defun-kernel compress-bound ((srclen :unsigned-long))
    :return-type :unsigned-long
    :kernel-name cl::%compress-bound
    :linkage-type :c)

(win:defcstruct compressed-buffer 
    ((compressed-length :unsigned-long)
     (uncompressed-length :unsigned-long)
     (bytes (:unsigned-char 4))))  ;; this is actually variable in length

;;
;;	Corman Lisp function UNCOMPRESS-FOREIGN-BYTES.
;;	Like UNCOMPRESS-BYTES, except using foreign byte buffers.
;;	Returns a buffer allocated with CT:MALLOC.
;;
(defun cl::uncompress-foreign-bytes (src-buf)
    (let* ((src-len (ct:cref compressed-buffer src-buf compressed-length))
           (dest-len (ct:cref compressed-buffer src-buf uncompressed-length))
           (dest-buf (ct:malloc dest-len))
           (dest-len-ptr (ct:malloc (ct:sizeof ':unsigned-long)))
           (comp-ret))
        (setf (ct:cref (:unsigned-long *) dest-len-ptr 0) dest-len)
        (setf comp-ret 
            (uncompress dest-buf dest-len-ptr 
                    (ct:cref compressed-buffer src-buf bytes)
                    src-len))
        (unless (zerop comp-ret)
            (ct:free dest-buf)
            (error "A decompression error occurred"))
        dest-buf))
        
(defun cl::compress-foreign-bytes (src-buf length)
    (let* ((src-len length)
           (dest-len (compress-bound src-len))  	
           (dest-buf (ct:malloc dest-len))
           (dest-len-ptr (ct:malloc (ct:sizeof ':unsigned-long)))
           (comp-ret)
           (header-size 8))
        (setf (ct:cref (:unsigned-long *) dest-len-ptr 0) dest-len)
        (setf comp-ret
            (compress 
                (ct:cref compressed-buffer dest-buf bytes) dest-len-ptr 
                    src-buf
                    src-len))
        (unless (zerop comp-ret)
            (ct:free dest-buf)
            (error "A compression error occurred"))
        (setf (ct:cref compressed-buffer dest-buf uncompressed-length) src-len)
        (setf (ct:cref compressed-buffer dest-buf compressed-length) 
            (ct:cref (:unsigned-long *) dest-len-ptr 0))
        
        ;; now that we know the compressed length, allocate a new buffer and copy the
        ;; significant bytes to it
        (let* ((num-result-bytes (+ header-size (ct:cref compressed-buffer dest-buf compressed-length)))
               (ret (ct:malloc num-result-bytes)))
            (ct:memcpy ret dest-buf num-result-bytes)
            (ct:free dest-buf)
            ret)))
      
(defun cl::compress-bytes (byte-vector)
    (let ((compressed-bytes
              (cl::compress-foreign-bytes 
                    (ct:lisp-bytes-to-c-bytes byte-vector) (length byte-vector))))
        (ct:c-bytes-to-lisp-bytes compressed-bytes (ct:foreign-heap-length compressed-bytes))))

(defun cl::uncompress-bytes (byte-vector)
    (let ((uncompressed-bytes
              (cl::uncompress-foreign-bytes 
                    (ct:lisp-bytes-to-c-bytes byte-vector))))
        (ct:c-bytes-to-lisp-bytes uncompressed-bytes (ct:foreign-heap-length uncompressed-bytes))))

(in-package :cl)
;;;
;;; Use FFI to redefine DISASSEMBLY-STATEMENT
;;;
(ct:defun-kernel cl::unassemble ((address :unsigned-long)(offset :unsigned-long))
    :return-type :unsigned-long
    :kernel-name cl::%unassemble
    :linkage-type :c)

(defun cl::disassembly-statement (address offset)
    (declare (special cl::*disassembly-output-buf*))
    (let* ((num-bytes (cl::unassemble address offset))
           (s (ct:c-string-to-lisp-string cl::*disassembly-output-buf*)))
        (values s num-bytes)))
