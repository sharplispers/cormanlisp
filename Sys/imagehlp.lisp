;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		imagehlp.lisp
;;;;	Contents:	Interfaces to Microsoft IMAGEHLP facility 
;;;;				for Corman Lisp.
;;;;	History:	3/26/97  RGC  Created.
;;;;

;; requires win32.lisp to be loaded first
(in-package :win32)

(defwintype PUCHAR (:unsigned-char *))

;; DOS .EXE header
(defwinstruct IMAGE_DOS_HEADER			;; DOS .EXE header
   ((e_magic WORD) 		                ;; Magic number
	(e_cblp WORD)                      	;; Bytes on last page of file
    (e_cp WORD)                       	;; Pages in file
    (e_crlc WORD)                      	;; Relocations
    (e_cparhdr WORD)                   	;; Size of header in paragraphs
    (e_minalloc WORD)                   ;; Minimum extra paragraphs needed
    (e_maxalloc WORD)                   ;; Maximum extra paragraphs needed
    (e_ss WORD)                         ;; Initial (relative) SS value
    (e_sp WORD)                         ;; Initial SP value
    (e_csum WORD)                       ;; Checksum
    (e_ip WORD)                         ;; Initial IP value
    (e_cs WORD)                         ;; Initial (relative) CS value
    (e_lfarlc WORD)                     ;; File address of relocation table
    (e_ovno WORD)                       ;; Overlay number
    (e_res (WORD 4))                    ;; Reserved words
    (e_oemid WORD)                      ;; OEM identifier (for e_oeminfo)
    (e_oeminfo WORD)                    ;; OEM information; e_oemid specific
    (e_res2 (WORD 10))                  ;; Reserved words
    (e_lfanew LONG)))                   ;; File address of new exe header

(defwintype PIMAGE_DOS_HEADER (IMAGE_DOS_HEADER *))
		
(defwinstruct IMAGE_FILE_HEADER
	((Machine WORD)
	 (NumberOfSections WORD)
	 (TimeDateStamp DWORD)
	 (PointerToSymbolTable DWORD)
	 (NumberOfSymbols DWORD)
	 (SizeOfOptionalHeader WORD)
	 (Characteristics WORD)
	))
(defwintype PIMAGE_FILE_HEADER (IMAGE_FILE_HEADER *))

(defwinstruct IMAGE_DATA_DIRECTORY
	((VirtualAddress DWORD)
	 (Size DWORD)
	))
  
(defwintype PIMAGE_DATA_DIRECTORY (IMAGE_DATA_DIRECTORY *))
(defwinconstant IMAGE_NUMBEROF_DIRECTORY_ENTRIES     16)

(defwinstruct IMAGE_OPTIONAL_HEADER
	((Magic WORD)
	 (MajorLinkerVersion BYTE)
	 (MinorLinkerVersion BYTE)
	 (SizeOfCode DWORD)
	 (SizeOfInitializedData DWORD)
	 (SizeOfUninitializedData DWORD)
	 (AddressOfEntryPoint DWORD)
	 (BaseOfCode DWORD)
	 (BaseOfData DWORD)
    ;;
    ;; NT additional fields.
    ;;
	 (ImageBase DWORD)
	 (SectionAlignment DWORD)
	 (FileAlignment DWORD)
	 (MajorOperatingSystemVersion WORD)
	 (MinorOperatingSystemVersion WORD)
	 (MajorImageVersion WORD)
	 (MinorImageVersion WORD)
	 (MajorSubsystemVersion WORD)
	 (MinorSubsystemVersion WORD)
	 (Win32VersionValue DWORD)
	 (SizeOfImage DWORD)
	 (SizeOfHeaders DWORD)
	 (CheckSum DWORD)
	 (Subsystem WORD)
	 (DllCharacteristics WORD)
	 (SizeOfStackReserve DWORD)
	 (SizeOfStackCommit DWORD)
	 (SizeOfHeapReserve DWORD)
	 (SizeOfHeapCommit DWORD)
	 (LoaderFlags DWORD)
	 (NumberOfRvaAndSizes DWORD)
	 (DataDirectory (IMAGE_DATA_DIRECTORY IMAGE_NUMBEROF_DIRECTORY_ENTRIES))
	))
(defwintype PIMAGE_OPTIONAL_HEADER (IMAGE_OPTIONAL_HEADER *))

(defwinstruct IMAGE_NT_HEADERS
	((Signature DWORD)
	 (FileHeader IMAGE_FILE_HEADER)
	 (OptionalHeader IMAGE_OPTIONAL_HEADER)
	))
(defwintype PIMAGE_NT_HEADERS (IMAGE_NT_HEADERS *))

(defwinconstant IMAGE_SIZEOF_SHORT_NAME 	8)
(defwinstruct IMAGE_SECTION_HEADER
	((Name (BYTE IMAGE_SIZEOF_SHORT_NAME))
	 (Misc DWORD)		;; union { DWORD   PhysicalAddress; DWORD   VirtualSize; };
	 (VirtualAddress DWORD)
	 (SizeOfRawData DWORD)
	 (PointerToRawData DWORD)
	 (PointerToRelocations DWORD)
	 (PointerToLinenumbers DWORD)
	 (NumberOfRelocations WORD)
	 (NumberOfLinenumbers WORD)
	 (Characteristics DWORD)
	))
(defwintype PIMAGE_SECTION_HEADER (IMAGE_SECTION_HEADER *))
(defwinconstant IMAGE_SIZEOF_SECTION_HEADER          40) 

(defwintype LIST_ENTRY :long)	;; temp type
(defwinstruct LIST_ENTRY
	((Flink (LIST_ENTRY *))
	 (Blink (LIST_ENTRY *))
	))
(defwintype PLIST_ENTRY (LIST_ENTRY *)) 
(defwintype BOOLEAN :unsigned-char)

(defwinstruct LOADED_IMAGE
	((ModuleName 		LPSTR)
	 (hFile 			HANDLE)
	 (MappedAddress 	PUCHAR)
	 (FileHeader 		PIMAGE_NT_HEADERS)
	 (LastRvaSection 	PIMAGE_SECTION_HEADER)
	 (NumberOfSections ULONG)
	 (Sections 		PIMAGE_SECTION_HEADER)
	 (Characteristics ULONG)
	 (fSystemImage 	BOOLEAN)
	 (fDOSImage 		BOOLEAN)
	 (Links 			LIST_ENTRY)
	 (SizeOfImage 		ULONG)
	))
(defwintype PLOADED_IMAGE (LOADED_IMAGE *))

(defwintype PDWORD (DWORD *))
(defwintype PWORD (WORD *))

(defwinstruct IMAGE_EXPORT_DIRECTORY
	((Characteristics DWORD)
	 (TimeDateStamp DWORD)
	 (MajorVersion WORD)
	 (MinorVersion WORD)
	 (Name DWORD)
	 (Base DWORD)
	 (NumberOfFunctions DWORD)
	 (NumberOfNames DWORD)
	 (AddressOfFunctions PDWORD)
	 (AddressOfNames PDWORD)
	 (AddressOfNameOrdinals PWORD)
	))
(defwintype PIMAGE_EXPORT_DIRECTORY (IMAGE_EXPORT_DIRECTORY *))

(defwinstruct IMAGEHLP_SYMBOL
	((SizeOfStruct  DWORD)
	 (Address DWORD)
	 (Size DWORD)
	 (Flags DWORD)
	 (MaxNameLength DWORD)
	 (Name (:char 64))
	))
(defwintype PIMAGEHLP_SYMBOL (IMAGEHLP_SYMBOL *)) 	

;; Directory Entries

(defwinconstant IMAGE_DIRECTORY_ENTRY_EXPORT         0)  ;; Export Directory
(defwinconstant IMAGE_DIRECTORY_ENTRY_IMPORT         1)  ;; Import Directory
(defwinconstant IMAGE_DIRECTORY_ENTRY_RESOURCE       2)  ;; Resource Directory
(defwinconstant IMAGE_DIRECTORY_ENTRY_EXCEPTION      3)  ;; Exception Directory
(defwinconstant IMAGE_DIRECTORY_ENTRY_SECURITY       4)  ;; Security Directory
(defwinconstant IMAGE_DIRECTORY_ENTRY_BASERELOC      5)  ;; Base Relocation Table
(defwinconstant IMAGE_DIRECTORY_ENTRY_DEBUG          6)  ;; Debug Directory
(defwinconstant IMAGE_DIRECTORY_ENTRY_COPYRIGHT      7)  ;; Description String
(defwinconstant IMAGE_DIRECTORY_ENTRY_GLOBALPTR      8)  ;; Machine Value (MIPS GP)
(defwinconstant IMAGE_DIRECTORY_ENTRY_TLS            9)  ;; TLS Directory
(defwinconstant IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG   10)  ;; Load Configuration Directory
(defwinconstant IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT  11)  ;; Bound Import Directory in headers
(defwinconstant IMAGE_DIRECTORY_ENTRY_IAT           12)  ;; Import Address Table


;; BOOL MapAndLoad(LPSTR ImageName, LPSTR DllPath, PLOADED_IMAGE LoadedImage, 
;;					BOOL DotDll, BOOL ReadOnly);

(defwinapi MapAndLoad
	((ImageName LPSTR)
	 (DllPath LPSTR)
	 (LoadedImage PLOADED_IMAGE)
	 (DotDll BOOL)
	 (ReadOnly BOOL))
   :return-type BOOL
   :library-name "imagehlp.dll"
   :entry-name "MapAndLoad"
   :linkage-type :pascal)

;; BOOL UnMapAndLoad(PLOADED_IMAGE);
(defwinapi UnMapAndLoad
	((LoadedImage PLOADED_IMAGE))
   :return-type BOOL
   :library-name "imagehlp.dll"
   :entry-name "UnMapAndLoad"
   :linkage-type :pascal)

;; PVOID ImageRvaToVa(PIMAGE_NT_HEADERS NtHeaders, PVOID Base, 
;;						ULONG Rva, PIMAGE_SECTION_HEADER *LastRvaSection);
(defwinapi ImageRvaToVa
	((NtHeaders PIMAGE_NT_HEADERS)
	 (Base PVOID)
	 (Rva ULONG)
	 (LastRvaSection (PIMAGE_SECTION_HEADER *)))
   :return-type PVOID
   :library-name "imagehlp.dll"
   :entry-name "ImageRvaToVa"
   :linkage-type :pascal)

(defwinapi FindDebugInfoFile 
	((FileName LPSTR) 
	 (SymbolPath LPSTR)
	 (DebugFilePath LPSTR))
   :return-type HANDLE
   :library-name "imagehlp.dll"
   :entry-name "FindDebugInfoFile"
   :linkage-type :pascal)

(defwinapi FindExecutableImage 
	((FileName LPSTR)
	 (SymbolPath LPSTR)
	 (ImageFilePath LPSTR))
   :return-type HANDLE
   :library-name "imagehlp.dll"
   :entry-name "FindExecutableImage"
   :linkage-type :pascal)

;;;
;;;	HANDLE GetCurrentProcess()
;;;
(defwinapi GetCurrentProcess ()
   :return-type HANDLE
   :library-name "kernel32.dll"
   :entry-name "GetCurrentProcess"
   :linkage-type :pascal)

;;;
;;;	DWORD GetCurrentProcessId()
;;;
(defwinapi GetCurrentProcessId ()
   :return-type DWORD
   :library-name "kernel32.dll"
   :entry-name "GetCurrentProcessId"
   :linkage-type :pascal)

;;;
;;;	HANDLE GetCurrentThread()
;;;
(defwinapi GetCurrentThread ()
   :return-type HANDLE
   :library-name "kernel32.dll"
   :entry-name "GetCurrentThread"
   :linkage-type :pascal)

;;;
;;;	DWORD GetCurrentThreadId()
;;;
(defwinapi GetCurrentThreadId ()
   :return-type DWORD
   :library-name "kernel32.dll"
   :entry-name "GetCurrentThreadId"
   :linkage-type :pascal)

#! (:library "imagehlp" :export t :pascal "WINAPI")
WINAPI BOOL
SymGetSymFromAddr(
    HANDLE              hProcess,
    DWORD               dwAddr,
    PDWORD              pdwDisplacement,
    PIMAGEHLP_SYMBOL    Symbol
    );
!#

#|
(setq s (ct:malloc (ct:sizeof 'IMAGEHLP_SYMBOL)))
(setf (cref IMAGEHLP_SYMBOL s SizeOfStruct) 84)
(setf (cref IMAGEHLP_SYMBOL s MaxNameLength) 64)
(setf (cref (:char 64) (cref IMAGEHLP_SYMBOL s Name) 0) 0)
(setq disp (ct:malloc (ct:sizeof '(:long *))))

(setq a (SymGetSymFromAddr
	(getcurrentprocess)
	#x100239DE
	disp
	s))



(getcurrentprocess)
;;;	BOOL GetProcessTimes(
;;;		HANDLE hProcess,			// specifies the process of interest 
;;;		LPFILETIME lpCreationTime,	// when the process was created
;;;		LPFILETIME lpExitTime,		// when the process exited 
;;;		LPFILETIME lpKernelTime,	// time the process has spent in kernel mode 
;;;		LPFILETIME lpUserTime 		// time the process has spent in user mode 
;;;		);
(defwinapi GetProcessTimes (
		(hProcess :long)
		(lpCreationTime (:long *))
 		(lpExitTime (:long *))
		(lpKernelTime (:long *))
		(lpUserTime (:long *)))
	:return-type :long
	:library-name "kernel32.dll"
	:entry-name "GetProcessTimes"
	:linkage-type :pascal
	:modifies (lpCreationTime lpExitTime lpKernelTime lpUserTime))
|#