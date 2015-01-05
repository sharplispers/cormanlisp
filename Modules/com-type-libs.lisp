;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	All rights reserved.
;;;;	-------------------------------
;;;;
;;;;	File:		com-type-libs.lisp
;;;;	Contents:	IPP protocol definitions for Corman Lisp.
;;;;	History:	8/16/99  RGC  Created.
;;;;

(in-package :win32)


;; TYPEKIND enum definition
(defwintype TYPEKIND :long)
(defwinconstant TKIND_ENUM 		0)
(defwinconstant TKIND_RECORD 	1)
(defwinconstant TKIND_MODULE 	2)
(defwinconstant TKIND_INTERFACE 3)
(defwinconstant TKIND_DISPATCH 	4)
(defwinconstant TKIND_COCLASS 	5)
(defwinconstant TKIND_ALIAS 	6)
(defwinconstant TKIND_UNION 	7)
(defwinconstant TKIND_MAX 		8)
(defwinconstant TKIND-NAME 			;; for backward lookups
	#("ENUM" "RECORD" "MODULE" "INTERFACE" "DISPATCH" "COCLASS" "ALIAS" "UNION"))

;; SYSKIND enum
(defwintype SYSKIND :long)
(defwinconstant SYS_WIN16 		0)
(defwinconstant SYS_WIN32		1)
(defwinconstant SYS_MAC			2)

#! (:export t)
typedef struct  tagTLIBATTR
    {
    GUID guid;
    LCID lcid;
    SYSKIND syskind;
    WORD wMajorVerNum;
    WORD wMinorVerNum;
    WORD wLibFlags;
    }	TLIBATTR;

typedef struct TLIBATTR* LPTLIBATTR;

typedef unsigned short VARTYPE;

typedef struct  tagTYPEDESC
{
	void* *lptdesc;	// could be TYPEDESC*, ARRAYDESC*, HREFTYPE or unused
    VARTYPE vt;
	unsigned short pad;
}	TYPEDESC;

typedef struct  tagIDLDESC
{
    ULONG dwReserved;
    USHORT wIDLFlags;
    USHORT wPad;
}	IDLDESC;

typedef struct  tagTYPEATTR
    {
    GUID guid;
    LCID lcid;
    DWORD dwReserved;
    MEMBERID memidConstructor;
    MEMBERID memidDestructor;
    LPOLESTR lpstrSchema;
    ULONG cbSizeInstance;
    TYPEKIND typekind;
    WORD cFuncs;
    WORD cVars;
    WORD cImplTypes;
    WORD cbSizeVft;
    WORD cbAlignment;
    WORD wTypeFlags;
    WORD wMajorVerNum;
    WORD wMinorVerNum;
    TYPEDESC tdescAlias;
    IDLDESC idldescType;
    }	TYPEATTR;

typedef TYPEATTR* LPTYPEATTR;
!#

#! (:export t)

typedef struct tagDEC 
{
    USHORT wReserved;
    BYTE scale;
    BYTE sign;
    ULONG Hi32;
    ULONG Lo32;
    ULONG Mid32;
} DECIMAL;

struct VARIANT
{
	VARTYPE vt;
	WORD wReserved1;
	WORD wReserved2;
	WORD wReserved3;
	DWORD data_lo;
	DWORD data_high;
};
typedef VARIANT *LPVARIANT;

typedef VARIANT VARIANTARG;

typedef VARIANT *LPVARIANTARG;

typedef struct  tagDISPPARAMS
{
    VARIANTARG *rgvarg;
    DISPID *rgdispidNamedArgs;
    UINT cArgs;
    UINT cNamedArgs;
}	DISPPARAMS;

!#

;; CALLCONV enum definition
(defwintype CALLCONV :long)
(defwinconstant CC_FASTCALL 	0)
(defwinconstant CC_CDECL 		1)
(defwinconstant CC_MSCPASCAL 	2)
(defwinconstant CC_PASCAL 		2)
(defwinconstant CC_MACPASCAL 	3)
(defwinconstant CC_STDCALL 		4)
(defwinconstant CC_FPFASTCALL 	5)
(defwinconstant CC_SYSCALL 		6)
(defwinconstant CC_MPWCDECL 	7)
(defwinconstant CC_MPWPASCAL 	8)

;; FUNCKIND enum definition
(defwintype FUNCKIND :long)
(defwinconstant FUNC_VIRTUAL 	0)
(defwinconstant FUNC_PUREVIRTUAL 1)
(defwinconstant FUNC_NONVIRTUAL 2)
(defwinconstant FUNC_STATIC 	3)
(defwinconstant FUNC_DISPATCH 	4)

;; INVOKEKIND; enum definition
(defwintype INVOKEKIND :long)
(defwinconstant INVOKE_FUNC 			1)
(defwinconstant INVOKE_PROPERTYGET 		2)
(defwinconstant INVOKE_PROPERTYPUT 		4)
(defwinconstant INVOKE_PROPERTYPUTREF 	8)

(defwintype SCODE :long)

#! (:export t)
typedef struct  tagPARAMDESCEX
{
    ULONG cBytes;
    VARIANTARG varDefaultValue;
}	PARAMDESCEX;

typedef PARAMDESCEX *LPPARAMDESCEX;

typedef struct  tagPARAMDESC
{
    LPPARAMDESCEX pparamdescex;
    USHORT wParamFlags;
}	PARAMDESC;

typedef PARAMDESC *LPPARAMDESC;

typedef struct  tagELEMDESC
{
    TYPEDESC tdesc;
    PARAMDESC paramdesc;
}	ELEMDESC;

typedef struct  tagFUNCDESC
{
    MEMBERID memid;
    SCODE *lprgscode;
    ELEMDESC *lprgelemdescParam;
    FUNCKIND funckind;
    INVOKEKIND invkind;
    CALLCONV callconv;
    SHORT cParams;
    SHORT cParamsOpt;
    SHORT oVft;
    SHORT cScodes;
    ELEMDESC elemdescFunc;
    WORD wFuncFlags;
}	FUNCDESC;

typedef FUNCDESC *LPFUNCDESC;
!#

#! (:export t)
interface ITypeComp : IUnknown
{
	HRESULT Bind(LPOLESTR szName, ULONG lHashVal, WORD wFlags, ITypeInfo** ppTInfo,
			DESCKIND* pDescKind, BINDPTR* pBindPtr);
	HRESULT BindType(LPOLESTR szName, ULONG lHashVal, ITypeInfo** ppTInfo, ITypeComp** ppTComp);
};
!#

#! (:export t)
interface ITypeInfo : IUnknown
{
	HRESULT GetTypeAttr(TYPEATTR** ppTypeAttr);
    HRESULT GetTypeComp(ITypeComp** ppTComp);
    HRESULT GetFuncDesc(UINT index, FUNCDESC** ppFuncDesc);
    HRESULT GetVarDesc(UINT index, VARDESC** ppVarDesc);
    HRESULT GetNames(MEMBERID memid, BSTR* rgBstrNames, UINT cMaxNames, UINT* pcNames);
	HRESULT GetRefTypeOfImplType(UINT index, HREFTYPE* pRefType);
	HRESULT GetImplTypeFlags(UINT index, INT* pImplTypeFlags);
	HRESULT GetIDsOfNames(LPOLESTR* rgszNames, UINT cNames, MEMBERID* pMemId);
	HRESULT Invoke(PVOID pvInstance, MEMBERID memid, WORD wFlags, DISPPARAMS* pDispParams,
				VARIANT* pVarResult, EXCEPINFO* pExcepInfo, UINT* puArgErr);
	HRESULT GetDocumentation(MEMBERID memid, BSTR* pBstrName, BSTR* pBstrDocString,
				DWORD* pdwHelpContext, BSTR* pBstrHelpFile);
	HRESULT GetDllEntry(MEMBERID memid, INVOKEKIND invKind, BSTR* pBstrDllName,
				BSTR* pBstrName, WORD* pwOrdinal);
	HRESULT GetRefTypeInfo(HREFTYPE hRefType, ITypeInfo** ppTInfo);
	HRESULT AddressOfMember(MEMBERID memid, INVOKEKIND invKind, PVOID* ppv);
	HRESULT CreateInstance(IUnknown* pUnkOuter, REFIID riid, PVOID* ppvObj);
	HRESULT GetMops(MEMBERID memid, BSTR* pBstrMops);
	HRESULT GetContainingTypeLib(ITypeLib** ppTLib, UINT* pIndex);
	void 	ReleaseTypeAttr(TYPEATTR* pTypeAttr);
	void 	ReleaseFuncDesc(FUNCDESC* pFuncDesc);
	void 	ReleaseVarDesc(VARDESC* pVarDesc);   
};
!#

#! (:export t)
interface ITypeLib : IUnknown
{
	UINT GetTypeInfoCount();     
    HRESULT GetTypeInfo(UINT index, ITypeInfo** ppTInfo);
    HRESULT GetTypeInfoType(UINT index, TYPEKIND* pTKind);
    HRESULT GetTypeInfoOfGuid(REFGUID guid, ITypeInfo** ppTinfo);
	HRESULT GetLibAttr(TLIBATTR** ppTLibAttr);
	HRESULT GetTypeComp(ITypeComp** ppTComp);
	HRESULT GetDocumentation(INT index, BSTR* pBstrName, BSTR* pBstrDocString,
				DWORD* pdwHelpContext, BSTR* pBstrHelpFile);   
	HRESULT IsName(LPOLESTR szNameBuf, ULONG lHashVal, BOOL* pfName);      
	HRESULT FindName(LPOLESTR szNameBuf, ULONG lHashVal, ITypeInfo** ppTInfo,
				MEMBERID* rgMemId, USHORT* pcFound);   
	void 	ReleaseLibAttr(TLIBATTR* pTLibAttr);
};
!#

;;;
;;;	Takes a path of a .DLL or .EXE which should contain a type
;;; library as a resource.
;;; Returns the interface to the loaded type library, or NIL.
;;;
(defun load-type-lib (path)
	(ct:with-fresh-foreign-block (b '((ITypeLib *) *))
		(let ((ret (LoadTypeLib (ct:lisp-string-to-unicode path) b)))
			(if (zerop ret)
				(ct:cref ((ITypeLib *) *) b 0)))))

(defmacro with-com-interface ((var init-form) &rest forms)
	`(let ((,var ,init-form))
		(unwind-protect
			(progn ,@forms)
			(IUnknown-Release ,var))))

(defstruct type-lib 
	name 
	interface 
	element-count
	help-string
	elements)

(defstruct type-lib-element-info 
	name
	interface
	kind
	help-string
	kind-name)

(defstruct type-attributes
	guid
    lcid
    constructor
    destructor
    schema
    instance-size
    typekind
    funcs
    vars
    impl-types
    vtable-size
    alignment
    type-flags
    major-version
    minor-version
    alias
    idl-desc-type
	kind-name)

(defun com-type-lib (path)
	(let ((interface (load-type-lib path))
		  name
		  help-string
		  element-count
		  (elements nil))
		(unless interface 
			(error "Could not load type library from file ~A" path))
		(with-fresh-foreign-block (a '(BSTR *))
			(with-fresh-foreign-block (b '(BSTR *))
				(let ((ret (ITypeLib-GetDocumentation interface -1 a b null null)))
					(if (/= ret s_ok)
						(error "Error getting type-lib information. Error code = ~D" ret))
					(unless (ct:cpointer-null (ct:cref (BSTR *) a 0))
						(setf name 
							(bstr-to-lisp-string (ct:cref (BSTR *) a 0)))
						(SysFreeString a))
					(unless (ct:cpointer-null (ct:cref (BSTR *) b 0))
						(setf help-string 
							(bstr-to-lisp-string (ct:cref (BSTR *) b 0)))
						(SysFreeString b))
					(setf element-count (ITypeLib-GetTypeInfoCount interface))
					(dotimes (i element-count)
						(let (element-name 
							  element-helpstring 
							  element-kind 
							  element-interface)
							(setf ret (ITypeLib-GetDocumentation interface i a b null null))
							(if (/= ret s_ok)
								(error "Error getting type-lib element information.~
									 Error code = ~D" ret))
							(unless (ct:cpointer-null (ct:cref (BSTR *) a 0))
								(setf element-name 
									(bstr-to-lisp-string (ct:cref (BSTR *) a 0)))
								(SysFreeString a))
							(unless (ct:cpointer-null (ct:cref (BSTR *) b 0))
								(setf element-helpstring 
									(bstr-to-lisp-string (ct:cref (BSTR *) b 0)))
								(SysFreeString b))
							(setf ret (ITypeLib-GetTypeInfo interface i a))
							(if (/= ret s_ok)
								(error "Error getting type-lib element information.~
									 Error code = ~D" ret))
							(setf element-interface (ct:cref ((ITypeInfo *) *) a 0))
							(setf ret (ITypeLib-GetTypeInfoType interface i a))
							(if (/= ret s_ok)
								(error "Error getting type-lib element information.~
									 Error code = ~D" ret))
							(setf element-kind (ct:cref (TYPEKIND *) a 0))
							(push (make-type-lib-element-info
									:name element-name
									:interface element-interface
									:kind element-kind
									:help-string element-helpstring
									:kind-name (elt tkind-name element-kind))
								elements)))
															
					(make-type-lib :interface interface
						:name name
						:element-count element-count
						:help-string help-string
						:elements (nreverse elements)))))))	
 
(defun com-type-info (interface)
	(let* ((type-attr)
		   ret)
		(with-fresh-foreign-block (a '(BSTR *))
			(setf ret (ITypeInfo-GetTypeAttr interface a))
			(if (/= ret s_ok)
				(error "Error getting type-attr information.~
						Error code = ~D" ret))
			(unless (ct:cpointer-null (ct:cref (BSTR *) a 0))
				(ct:with-c-struct (s (ct:cref ((TYPEATTR *) *) a 0) TYPEATTR)
					(setf type-attr
						(make-type-attributes
							:guid guid
						    :lcid lcid
						    :constructor memidConstructor
						    :destructor memidDestructor
						    :schema lpstrSchema
						    :instance-size cbSizeInstance
						    :typekind typekind
						    :funcs cFuncs
						    :vars cVars
						    :impl-types cImplTypes
						    :vtable-size cbSizeVft
						    :alignment cbAlignment
						    :type-flags wTypeFlags
						    :major-version wMajorVerNum
						    :minor-version wMinorVerNum
						    :alias tdescAlias
						    :idl-desc-type idldescType
							:kind-name (elt tkind-name typekind))))
				(ITypeInfo-ReleaseTypeAttr interface a)))
		type-attr))
							
#|
;;; test stuff
(setf type-lib
	(com-type-lib "C:\\Program Files\\Microsoft Office\\Office\\REFEDIT.DLL"))
(setf info 
	(com-type-info 
		(type-lib-element-info-interface 
			(first (type-lib-elements type-lib)))))
(IUnknown-Release (type-lib-interface type-lib))

(with-com-interface (type-lib 
		(load-type-lib "C:\\Program Files\\Microsoft Office\\Office\\REFEDIT.DLL"))
	(format t "Type library: ~S, count = ~D~%" type-lib
		(GetTypeInfoCount-ITypeLib type-lib)))
|#


(provide "COM-TYPE-LIBS")