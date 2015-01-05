;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		com-interfaces.lisp
;;;;	Contents:	Standard COM interface definitions.
;;;;	History:	9/8/98  RGC  Created.
;;;;

(in-package :win32)

#! (:export t)
interface IUnknown
{
HRESULT QueryInterface(REFIID riid, LPVOID* ppvObject);
ULONG AddRef();
ULONG Release();
};
!#

#! (:export t)
interface IDispatch : IUnknown
{
    HRESULT GetTypeInfoCount(UINT *pctinfo);     
    HRESULT GetTypeInfo(UINT iTInfo, LCID lcid, ITypeInfo **ppTInfo);
    HRESULT GetIDsOfNames(REFIID riid, LPOLESTR *rgszNames, UINT cNames, 
        LCID lcid, DISPID *rgDispId);
    HRESULT Invoke(DISPID dispIdMember, REFIID riid, LCID lcid, WORD wFlags, 
        DISPPARAMS *pDispParams,
        VARIANT *pVarResult, EXCEPINFO *pExcepInfo, UINT *puArgErr);
};
!#

#! (:export t)
interface IMalloc : IUnknown
{
	LPVOID Alloc(ULONG cb);
	LPVOID Realloc(LPVOID pv, ULONG cb);
	VOID Free(LPVOID pv);
	ULONG GetSize(LPVOID pv);
	int DidAlloc(LPVOID pv);
	VOID HeapMinimize();
};
!#

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
			(iunknown-release ,var))))
#|
;;; test stuff
(setf type-lib
	(load-type-lib "C:\\Program Files\\Microsoft Office\\Office\\REFEDIT.DLL"))
(iunknown-release type-lib)

(with-com-interface (type-lib 
		(load-type-lib "C:\\Program Files\\Microsoft Office\\Office\\REFEDIT.DLL"))
	(format t "Type library: ~S, count = ~D~%" type-lib
		(ITypeLib-GetTypeInfoCount type-lib)))
|#
