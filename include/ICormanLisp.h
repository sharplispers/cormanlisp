//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		ICormanLisp.h
//		Contents:	COM Class definitions for Corman Lisp COM server.
//		History:	8/5/97  RGC  Created.
//

#ifndef ICORMANLISP_H
#define ICORMANLISP_H

#include <Unknwn.h>
#include <stddef.h> //size_t

// {3A6AC641-0EAF-11d1-ACB9-00A024803258}
DEFINE_GUID(IID_ICormanLisp, 
0x3a6ac641, 0xeaf, 0x11d1, 0xac, 0xb9, 0x0, 0xa0, 0x24, 0x80, 0x32, 0x58);

// {F056CAF1-128E-11d1-ACB9-00A024803258}
DEFINE_GUID(IID_ICormanLispTextOutput, 
0xf056caf1, 0x128e, 0x11d1, 0xac, 0xb9, 0x0, 0xa0, 0x24, 0x80, 0x32, 0x58);

// {F056CAF2-128E-11d1-ACB9-00A024803258}
DEFINE_GUID(IID_ICormanLispStatusMessage, 
0xf056caf2, 0x128e, 0x11d1, 0xac, 0xb9, 0x0, 0xa0, 0x24, 0x80, 0x32, 0x58);

// {3EB3AE13-F3B9-4e19-BA6C-0354913612A6}
DEFINE_GUID(IID_ICormanLispDirectCall, 
0x3eb3ae13, 0xf3b9, 0x4e19, 0xba, 0x6c, 0x3, 0x54, 0x91, 0x36, 0x12, 0xa6);

// {3C89B650-1878-4743-8690-CE77B1EB3493}
DEFINE_GUID(IID_ICormanLispShutdown, 
0x3c89b650, 0x1878, 0x4743, 0x86, 0x90, 0xce, 0x77, 0xb1, 0xeb, 0x34, 0x93);

#define WIN_APP_CLIENT		0
#define CONSOLE_CLIENT		1
#define IDE_CLIENT			2

interface ICormanLisp : public IUnknown
{
    STDMETHOD(Initialize)(THIS_ IUnknown* clientInterface, 
		THIS_ const char* imageName, int clientType) PURE;
    STDMETHOD(Run)(THIS_ HANDLE*) PURE;
    STDMETHOD(ProcessSource)(THIS_ char* text, long numChars) PURE;
    STDMETHOD(GetNumThreads)(THIS_ long* numThreads) PURE;
    STDMETHOD(UserException)(THIS_) PURE;
    STDMETHOD(AbortThread)(THIS_) PURE;
    STDMETHOD(InitializeEx)(THIS_ IUnknown* clientInterface, 
		THIS_ const char* imageName, int clientType, int heapReserve, int heapInitialSize,
        int ephemeralHeap1Size, int ephemeralHeap2Size) PURE;
	// supply NULL as UserName to get real length in len
	STDMETHOD(GetCurrentUserName)(THIS_ char *UserName, size_t *len) PURE;
	// supply NULL as profileDirectory to get real length in len
	STDMETHOD(GetCurrentUserProfileDirectory)(THIS_ char *profileDirectory, size_t *len) PURE;
};

//
//	Outgoing interfaces.
//	The client should implement these for use by the component.
//
interface ICormanLispTextOutput : public IUnknown
{
    STDMETHOD(OutputText)(THIS_ char* text, long numChars) PURE;
    STDMETHOD(GetAppInstance)(THIS_ HINSTANCE* appInstance) PURE;
    STDMETHOD (GetAppMainWindow)(THIS_ HWND* appMainWindow) PURE;
};

//
// Optional interface. Typically used by an editor or IDE,
//
interface ICormanLispStatusMessage : public ICormanLispTextOutput
{
    STDMETHOD(SetMessage)(THIS_ char* text) PURE;
	STDMETHOD(GetMessage)(THIS_ char* text, long maxMessageLength) PURE;
    STDMETHOD(SetDefaultMessage)(THIS_) PURE;
    STDMETHOD(OpenEditWindow)(THIS_ char* file, HWND* wnd) PURE;
    STDMETHOD(AddMenu)(THIS_ char* menuName) PURE;
    STDMETHOD(AddMenuItem)(THIS_ char* menuName, char* menuItem) PURE;
    STDMETHOD(OpenURL)(THIS_ char* file, HWND* wnd) PURE;

	// ReplaceSelection is like OutputText, except that the sent
	// text is used to replace the current selection.
	//
    STDMETHOD(ReplaceSelection)(THIS_ char* text, long numChars) PURE;
};

//
// Optional interface. Used to call directly into lisp code from
// threads not created by the lisp runtime.
//
interface ICormanLispDirectCall : public IUnknown
{
    STDMETHOD(BlessThread)(THIS_) PURE;
    STDMETHOD(UnblessThread)(THIS_) PURE;
    STDMETHOD(GetFunctionAddress)(THIS_ wchar_t* functionName, wchar_t* packageName, void** funcptr) PURE;
    STDMETHOD(HandleStructuredException)(THIS_ long exception, LPEXCEPTION_POINTERS info, long* result) PURE;
};

//
//	Outgoing interface.
//	The client should implement this to be notified when Lisp has to shut down.
//
interface ICormanLispShutdown : public IUnknown
{
    STDMETHOD(LispShutdown)(THIS_ char* text, long numChars) PURE;
};

#endif	// ICORMANLISP_H