//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		CoCormanLispClient.h
//		Contents:	CoCormanLispClient class definition for Corman Lisp.
//		History:	8/11/97  RGC Created.
//

#ifndef COCORMANLISPCLIENT_H
#define	COCORMANLISPCLIENT_H

#include "ICormanLisp.h"

class CoCormanLispClient 
	: public ICormanLispStatusMessage
{
public:
	CoCormanLispClient();
	~CoCormanLispClient();

// IUnknown methods
    STDMETHODIMP QueryInterface(REFIID riid, void** ppv);
    STDMETHODIMP_(ULONG) AddRef();
    STDMETHODIMP_(ULONG) Release();

// ICormanLisp methods
    STDMETHODIMP OutputText(char* text, long numChars);
    STDMETHODIMP SetMessage(char* text);
	STDMETHODIMP GetMessage(char* text, long maxMessageLength);
    STDMETHODIMP SetDefaultMessage();
    STDMETHODIMP GetAppInstance(HINSTANCE* appInstance);
    STDMETHODIMP GetAppMainWindow(HWND* appMainWindow);
    STDMETHODIMP OpenEditWindow(char* file, HWND* wnd);
    STDMETHODIMP OpenURL(char* file, HWND* wnd);
    STDMETHODIMP AddMenu(char* menuName);
    STDMETHODIMP AddMenuItem(char* menuName, char* menuItem);
    STDMETHODIMP ReplaceSelection(char* text, long numChars);

// Helper functions
	STDMETHODIMP Connect(IConnectionPoint* pConnectionPoint);
	STDMETHODIMP Disconnect(IConnectionPoint* pConnectionPoint);

private:
	long m_cRef;
	DWORD m_dwCookie;
	DWORD m_eventLoopThreadID;
};

class CoCormanLispShutdownClient 
	: public ICormanLispShutdown
{
public:
	CoCormanLispShutdownClient();
	~CoCormanLispShutdownClient();

// IUnknown methods
    STDMETHODIMP QueryInterface(REFIID riid, void** ppv);
    STDMETHODIMP_(ULONG) AddRef();
    STDMETHODIMP_(ULONG) Release();

// ICormanLispShutdown methods
    STDMETHODIMP LispShutdown(char* text, long numChars);

// Helper functions
	STDMETHODIMP Connect(IConnectionPoint* pConnectionPoint);
	STDMETHODIMP Disconnect(IConnectionPoint* pConnectionPoint);

private:
	long m_cRef;
	DWORD m_dwCookie;
	DWORD m_eventLoopThreadID;
};

#endif // COCORMANLISPCLIENT_H