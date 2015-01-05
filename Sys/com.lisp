;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		com.lisp
;;;;	Contents:	COM interface functions.
;;;;	History:	5/29/97  RGC  Created.
;;;;

(in-package :win32)
(export '(
	STRING-GUID
	GUID-STRING 
	ALLOCATE-GUID 
	CREATE-GUID
	PROGID-CLSID
	CLSID-PROGID
	ISEQUALGUID
	ISEQUALCLSID
	ISEQUALIID
    WITH-COM-INTERFACE
    BSTR-TO-LISP-STRING
	))

(defwintype HRESULT :unsigned-long)

(defwinstruct GUID
	((Data1 :unsigned-long)
	 (Data2 :unsigned-short)
	 (Data3 :unsigned-short)
	 (Data4 (:unsigned-char 8))
	))		
(defwintype LPGUID (GUID *))
(defwintype REFGUID (GUID *))
(defwintype LPOLESTR LPSTR)
(defwintype LPOLESTRPTR (LPSTR *))
(defwintype interface :void)
(defwintype IUnknown interface)
(defwintype LPUNKNOWN (IUnknown *))
(defwintype CLSID GUID)
(defwintype IID GUID)
(defwintype REFCLSID (CLSID *))
(defwintype LPCLSID (CLSID *))
(defwintype REFIID (IID *))
(defwintype LPIID (IID *))
(defwintype LPVOIDPTR (LPVOID *))

(defwinconstant guid-size (ct:sizeof 'GUID))

;; COM Success codes
(defwinconstant  S_OK						#x00000000)
(defwinconstant  S_FALSE     				#x00000001)

;; COM error codes
(defwinconstant OLE_E_FIRST 				#x80040000)
(defwinconstant OLE_E_LAST  				#x800400FF)
(defwinconstant OLE_S_FIRST 				#x00040000)
(defwinconstant OLE_S_LAST  				#x000400FF)
(defwinconstant OLE_E_OLEVERB 				#x80040000)
(defwinconstant OLE_E_ADVF                  #x80040001)
(defwinconstant OLE_E_ENUM_NOMORE           #x80040002)
(defwinconstant OLE_E_ADVISENOTSUPPORTED    #x80040003)
(defwinconstant OLE_E_NOCONNECTION         	#x80040004)
(defwinconstant OLE_E_NOTRUNNING           	#x80040005)
(defwinconstant OLE_E_NOCACHE              	#x80040006)
(defwinconstant OLE_E_BLANK                	#x80040007)
(defwinconstant OLE_E_CLASSDIFF            	#x80040008)
(defwinconstant OLE_E_CANT_GETMONIKER      	#x80040009)
(defwinconstant OLE_E_CANT_BINDTOSOURCE    	#x8004000A)
(defwinconstant OLE_E_STATIC               	#x8004000B)
(defwinconstant OLE_E_PROMPTSAVECANCELLED  	#x8004000C)
(defwinconstant OLE_E_INVALIDRECT          	#x8004000D)

(defwinconstant CLSCTX_INPROC_SERVER		#x1)
(defwinconstant CLSCTX_INPROC_HANDLER		#x2)
(defwinconstant CLSCTX_LOCAL_SERVER			#x4)
(defwinconstant CLSCTX_REMOTE_SERVER		#x10)
		
#! (:library "ole32" :export t :pascal "WINAPI")
HRESULT WINAPI CoCreateGuid (LPGUID pguid);
HRESULT WINAPI StringFromCLSID(REFCLSID rclsid, LPOLESTRPTR lplpsz);
HRESULT WINAPI CLSIDFromString(LPOLESTR lpsz, LPCLSID pclsid);
HRESULT WINAPI StringFromIID(REFIID rclsid, LPOLESTRPTR lplpsz);
HRESULT WINAPI IIDFromString(LPOLESTR lpsz, LPIID lpiid);
BOOL 	WINAPI CoIsOle1Class(REFCLSID rclsid);
HRESULT WINAPI ProgIDFromCLSID (REFCLSID clsid, LPOLESTRPTR lplpszProgID);
HRESULT WINAPI CLSIDFromProgID (LPCOLESTR lpszProgID, LPCLSID lpclsid);
int 	WINAPI StringFromGUID2(REFGUID rguid, LPOLESTR lpsz, int cbMax);
HRESULT WINAPI CoCreateInstance(REFCLSID rclsid, LPUNKNOWN pUnkOuter,
                    DWORD dwClsContext, REFIID riid, LPVOIDPTR ppv);

!#

#! (:export t)
typedef WCHAR OLECHAR;
typedef DWORD LCID;
typedef OLECHAR* BSTR;
typedef LONG DISPID;
typedef DISPID MEMBERID;
typedef DWORD HREFTYPE;
!#

#! (:library "oleaut32" :export t :pascal "WINOLEAUTAPI")
WINOLEAUTAPI HRESULT LoadTypeLib(const OLECHAR  *szFile, ITypeLib ** pptlib);

WINOLEAUTAPI BSTR SysAllocString(const OLECHAR *);
WINOLEAUTAPI INT  SysReAllocString(BSTR *, const OLECHAR *);
WINOLEAUTAPI BSTR SysAllocStringLen(const OLECHAR *, UINT);
WINOLEAUTAPI INT  SysReAllocStringLen(BSTR *, const OLECHAR *, UINT);
WINOLEAUTAPI void SysFreeString(BSTR);
WINOLEAUTAPI UINT SysStringLen(BSTR);
!#

(defun IsEqualGUID (rguid1 rguid2)
	(= (ct:memcmp rguid1 rguid2 guid-size) 0))
(defun IsEqualCLSID (g1 g2) (IsEqualGUID g1 g2))
(defun IsEqualIID (g1 g2) (IsEqualIID g1 g2))

(defun allocate-guid () (ct:malloc guid-size))
(defun create-guid ()
	(let ((guid (allocate-guid)))
		(CoCreateGuid guid)
		guid))

(defconstant guid-string-char-length 39)
(defun guid-string (guid)
	(let* ((size (* guid-string-char-length 2))
		   (buf (ct:malloc size)))
		(StringFromGUID2 guid buf size)
		(prog1 
			(ct:unicode-to-lisp-string buf) 
			(ct:free buf))))

;;; convert a string to a GUID
(defun string-guid (string guid)
	(let* ((size (* (+ 1 (length string)) 2))
		   (buf (ct:malloc size)))
		(ct:lisp-string-to-unicode string buf)
		(CLSIDFromString buf guid)
		(ct:free buf)
		guid))

(defun display-guid (guid &optional (stream *standard-output*))
	(format stream "#< GUID: ~A >" (guid-string guid))) 

(defun output-guid (guid &optional (stream *standard-output*))
	(format stream (guid-string guid))) 

(defun ProgID-CLSID (progID clsid)
	(let ((buf (ct:malloc (* (+ (length progID) 1) 2))))
		(ct:lisp-string-to-unicode progID buf)
		(CLSIDFromProgID buf clsid)))

(defun CLSID-ProgID (clsid)
	(let ((ptr (ct:malloc (ct:sizeof 'LPOLESTRPTR))))
		(unless (= (ProgIDFromCLSID clsid ptr) S_OK)
			(return-from CLSID-ProgID nil))
		(ct:unicode-to-lisp-string (ct:cref LPOLESTRPTR ptr 0))))

#|
(defun display-guid (g &optional (stream *standard-output*))
	(let ((bytes (ct:cref guid g data4)))
		(format stream 
			"#< GUID: ~8,'0x-~4,'0x-~4,'0x-~2,'0x-~2,'0x-~2,'0x-~2,'0x-~2,'0x-~2,'0x-~2,'0x-~2,'0x >"
			(ct:cref guid g data1)
			(ct:cref guid g data2)
			(ct:cref guid g data3)
			(ct:cref (:unsigned-char 8) bytes 0)
			(ct:cref (:unsigned-char 8) bytes 1)
			(ct:cref (:unsigned-char 8) bytes 2)
			(ct:cref (:unsigned-char 8) bytes 3)
			(ct:cref (:unsigned-char 8) bytes 4)
			(ct:cref (:unsigned-char 8) bytes 5)
			(ct:cref (:unsigned-char 8) bytes 6)
			(ct:cref (:unsigned-char 8) bytes 7))))
|#


(defvar *com-objects* (make-array 100 :fill-pointer 0))

(setq cl::*compiler-warn-on-undefined-function* nil)
(defun print-com-object (obj stream level)
	(declare (ignore level))
	(format stream "#< COM Object: ")
	(output-guid (com-object-guid obj) stream)
	(format stream " data: ~A  refcount: ~A id: ~A >"
		(com-object-data obj)
		(com-object-refcount obj)
		(com-object-id obj)))

(defstruct (com-object 
	(:print-function print-com-object))
	guid
	data
	refcount
	id)
(setq cl::*compiler-warn-on-undefined-function* t)

(defwinstruct COM-Interface
  ((vtable      (:void *))
   (id			:long)
  ))

(defun create-com-object (guid data)
	(let ((com-obj (make-com-object))
		  (id (fill-pointer *com-objects*)))
		(setf (com-object-guid com-obj) guid)
		(setf (com-object-data com-obj) data)
		(setf (com-object-refcount com-obj) 0)
		(setf (com-object-id com-obj) id)
		(setf (aref *com-objects* id) com-obj)
		(setf (fill-pointer *com-objects*) (+ id 1))
		id))

(defconstant func-ptr-size (ct:sizeof '(:void *)))

(defun create-com-interface (id callback-func-names)
	(let* ((num-funcs (length callback-func-names))
		   (vtable-size (* func-ptr-size num-funcs))
		   (com-obj (ct:malloc (+ vtable-size (ct:sizeof 'COM-Interface))))
		   (count 2)) 	;; start past interface 
		(setf (ct:cref COM-Interface com-obj vtable)
			(ct:int-to-foreign-ptr 
				(+ (ct:foreign-ptr-to-int com-obj) 
					(ct:sizeof 'COM-Interface))))  
		(setf (ct:cref COM-Interface com-obj id) id) 
		(dolist (func callback-func-names)
			(setf (ct:cref ((:void *) *) com-obj count)
				(ct:get-callback-procinst func))
			(incf count))
		com-obj))

(defun bstr-to-lisp-string (bstr)
	(let* ((length (truncate (ct:cref (DWORD *) bstr -1) 2))
		   (str (make-array length :element-type 'character)))
		(dotimes (i length str)
			(setf (char str i) (code-char (ct:cref (:unsigned-short *) bstr i))))))

(defwinconstant IID_IUnknown
	(string-guid "{00000000-0000-0000-C000-000000000046}" (allocate-guid)))

#! (:export t)
interface IUnknown
{
HRESULT QueryInterface(REFIID riid, LPVOID* ppvObject);
ULONG AddRef();
ULONG Release();
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

;;;
;;; Corman Lisp WITH-COM-INTERFACE macro.
;;;
(defmacro with-com-interface ((interface &optional pointer) expr &rest body)
    (if pointer
        `(let ((,interface
                (symbol-macrolet ((,interface ,pointer))
                        ,expr
                        (ct:cref ((win:interface *) *) ,interface 0))))
            (unwind-protect
                (progn ,@body)
                (win:iunknown-Release ,interface)))
        
        (let ((p (gensym)))
            `(let* ((,p (ct:malloc (ct:sizeof '(win:interface *))))
                    (,interface 
                        (symbol-macrolet ((,interface ,p))
                            ,expr
                            (ct:cref ((win:interface *) *) ,interface 0))))
                (unwind-protect
                    (progn ,@body)
                    (win:iunknown-Release ,interface)
                    (ct:free ,p))))))

#|
(setq my-data #( "roger" 10 40))
(ct:defun-callback func1 ((this (:void *))(x :long))
	(let ((obj (aref *com-objects* (ct:cref (COM-Interface *) this id)))) 
		(+ (second (com-object-data obj)) 1)))

(ct:defun-callback func2 ((this (:void *))(x :long))
	(let ((obj (aref *com-objects* (ct:cref (COM-Interface *) this id)))) 
		(+ (second (com-object-data obj)) 2)))

(setq callback-funcs '(func1 func2))
(setq guid (allocate-guid))
(CoCreateGuid guid)
(setq id (create-com-object guid my-data))
(setq iface (create-com-interface id callback-funcs))

(defconstant word-clsid 
	(string-guid "{000209FF-0000-0000-C000-000000000046}" (allocate-guid)))
(setf ppv (ct:malloc (ct:sizeof 'LPVOID)))
(setf (ct:cref LPVOIDPTR ppv 0) null)
(setq result 
	(CoCreateInstance 
		word-clsid 
		null 
		CLSCTX_LOCAL_SERVER
		IID_IUnknown
		ppv))
(setq i (ct:cref (LPUNKNOWN *) ppv 0))

(ct:defun-com-method IUnknown_QueryInterface
	((this LPUNKNOWN)
	 (riid REFIID)
	 (ppvObject LPVOIDPTR))
	0)

(ct:defun-com-method IUnknown_AddRef
	((this LPUNKNOWN))
	1
	:return-type ULONG)

(ct:defun-com-method IUnknown_Release
	((this LPUNKNOWN))
	2
	:return-type ULONG)

(iunknown_release i)

(setf a (ct:malloc (ct:sizeof '(:void *))))
(LoadTypeLib 
	(ct:lisp-string-to-unicode "C:\\Program Files\\Microsoft Office\\Office\\REFEDIT.DLL")
	 a) 

|#
