//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		CoCormanLisp.h
//		Contents:	COM Class definitions for Corman Lisp COM server.
//		History:	8/5/97  RGC  Created.
//					10/01/16  Artem Boldarev GetCurrentUserName method.
//

#ifndef COLISP_H
#define COLISP_H

#include "ICormanLisp.h"
#include "CoEnumConnectionPoints.h"
class CoConnectionPoint;

class CoCormanLisp :  
	public ICormanLisp,
	public IConnectionPointContainer,
	public ICormanLispDirectCall
{
public:
	CoCormanLisp();
	~CoCormanLisp();

// IUnknown methods
    STDMETHODIMP QueryInterface(REFIID riid, void** ppv);
    STDMETHODIMP_(ULONG) AddRef(void);
    STDMETHODIMP_(ULONG) Release(void);

// ICormanLisp methods
    STDMETHODIMP Initialize(IUnknown* clientInterface, const char* imageName, int clientType);
    STDMETHODIMP Run(HANDLE*);
    STDMETHODIMP ProcessSource(char* text, long numChars);
    STDMETHODIMP GetNumThreads(long* numThreads);
    STDMETHODIMP UserException();
    STDMETHODIMP AbortThread();
    STDMETHODIMP InitializeEx(IUnknown* clientInterface, const char* imageName, int clientType,
         int heapReserve, int heapInitialSize, int ephemeralHeap1Size, int ephemeralHeap2Size);
	STDMETHODIMP GetCurrentUserName(char *UserName, size_t *len);

// IConnectionPointContainer
	STDMETHODIMP EnumConnectionPoints(IEnumConnectionPoints** pEnumConnectionPoints);
	STDMETHODIMP FindConnectionPoint(REFIID riid, IConnectionPoint** ppConnectionPoint);

    STDMETHODIMP SetMessage(char* text);
	STDMETHODIMP GetMessage(char* text, long maxMessageLength);
    STDMETHODIMP SetDefaultMessage();
    STDMETHODIMP OutputText(char* text, long numChars);
    STDMETHODIMP GetAppInstance(HINSTANCE* appInstance);
    STDMETHODIMP GetAppMainWindow(HWND* appMainWindow);
    STDMETHODIMP OpenEditWindow(char* file, HWND* wnd);
    STDMETHODIMP OpenURL(char* file, HWND* wnd);
    STDMETHODIMP AddMenu(char* menuName);
    STDMETHODIMP AddMenuItem(char* menuName, char* menuItem);
    STDMETHODIMP ReplaceSelection(char* text, long numChars);

	// ICormanLispDirectCall 
    STDMETHODIMP BlessThread();
    STDMETHODIMP UnblessThread();
    STDMETHODIMP GetFunctionAddress(wchar_t* functionName, wchar_t* packageName, void** funcptr);
    STDMETHODIMP HandleStructuredException(long exception, LPEXCEPTION_POINTERS info, long* result);

	// ICormanLispShutdown
    STDMETHODIMP LispShutdown(char* text, long numChars);

private:
	long m_cRef;
	CoConnectionPoint* m_pcpPointSink;
	CoConnectionPoint* m_pcpShutdownPointSink;
	void *pUserInfo;
};

#endif // COLISP_H
