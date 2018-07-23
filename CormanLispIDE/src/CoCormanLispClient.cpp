//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		CoCormanLispClient.cpp
//		Contents:	CoCormanLispClient class implementation for Corman Lisp.
//		History:	8/11/97  RGC Created.
//

#include "stdafx.h"

#include "CoCormanLispClient.h"
#include "CormanLisp.h"

//////////////////////////////////////////////////////////////////////
// ctor and dtor
CoCormanLispClient::CoCormanLispClient()
{
	m_cRef = 0;
	m_dwCookie = -1;
	m_eventLoopThreadID = GetCurrentThreadId();
}

CoCormanLispClient::~CoCormanLispClient()
{
}


//////////////////////////////////////////////////////////////////////
// IUnknown interfaces

STDMETHODIMP CoCormanLispClient::QueryInterface(REFIID riid, void** ppv)
{
    if (riid == IID_IUnknown)
        *ppv = (ICormanLisp*)this;
    else if (riid == IID_ICormanLispTextOutput)
        *ppv = (ICormanLispTextOutput*)this;
    else if (riid == IID_ICormanLispStatusMessage)
        *ppv = (ICormanLispStatusMessage*)this;
    else if (riid == IID_ICormanLispShutdown)
        *ppv = (ICormanLispShutdown*)this;
    else
        *ppv = 0;
    if (*ppv)
        ((IUnknown*)*ppv)->AddRef();
    return *ppv ? S_OK : E_NOINTERFACE;
}

STDMETHODIMP_(ULONG) CoCormanLispClient::AddRef()
{
    return InterlockedIncrement(&m_cRef);
}

STDMETHODIMP_(ULONG) CoCormanLispClient::Release()
{
    if (InterlockedDecrement(&m_cRef) != 0)
        return m_cRef;
    delete this;
    return 0;
}

STDMETHODIMP CoCormanLispClient::OutputText(char* text, long numBytes)
{
	long offset = 0;
	long ret = 0;
	while (numBytes > 0)
	{
		ret = TerminalOutputBuf.addChars(text + offset, numBytes);
		numBytes -= ret;
		offset += ret;
	}
	return S_OK;
}

STDMETHODIMP CoCormanLispClient::SetMessage(char* message)
{
	theApp.SetMessage(message);
	return S_OK;
}

STDMETHODIMP CoCormanLispClient::GetMessage(char* text, long maxMessageLength)
{
	CString message(theApp.GetMessage());
	strncpy_s(text, maxMessageLength, (const char*)message, maxMessageLength);
	return S_OK;
}

STDMETHODIMP CoCormanLispClient::SetDefaultMessage()
{
	theApp.SetDefaultMessage();
	return S_OK;
}

STDMETHODIMP CoCormanLispClient::GetAppInstance(HINSTANCE* appInstance)
{
	*appInstance = AfxGetApp()->m_hInstance;
	return S_OK;
}

STDMETHODIMP CoCormanLispClient::GetAppMainWindow(HWND* appMainWindow)
{
	*appMainWindow = AfxGetApp()->GetMainWnd()->m_hWnd;
	return S_OK;
}

STDMETHODIMP CoCormanLispClient::Connect(IConnectionPoint* pConnectionPoint)
{
	return pConnectionPoint->Advise((ICormanLispStatusMessage*)this, &m_dwCookie);
}

STDMETHODIMP CoCormanLispClient::Disconnect(IConnectionPoint* pConnectionPoint)
{
	return pConnectionPoint->Unadvise(m_dwCookie);
}

STDMETHODIMP CoCormanLispClient::OpenEditWindow(char* file, HWND* wnd)
{
	bool ret = true;
	CDocument* doc = 0;
	if (m_eventLoopThreadID != GetCurrentThreadId())
	{
		*wnd = 0;
		while (theApp.waitingForDocumentToOpen());
		bool ret = theApp.SetDocumentToOpen(file);
		while (theApp.waitingForDocumentToOpen());
		doc = theApp.m_lastDocOpened;
	}
	else
		doc = theApp.OpenDocumentFile(file);
	if (doc)
	{
		POSITION pos = doc->GetFirstViewPosition();
		CView* pView = doc->GetNextView(pos);
		if (pView)
			*wnd = pView->m_hWnd;
	}
	return ret ? S_OK : E_FAIL;
}

STDMETHODIMP CoCormanLispClient::OpenURL(char* file, HWND* wnd)
{
	bool ret = true;
	*wnd = 0;
	if (m_eventLoopThreadID != GetCurrentThreadId())
	{
		while (theApp.waitingForDocumentToOpen());
		ret = theApp.SetURLToOpen(file);
		while (theApp.waitingForDocumentToOpen());
	}
	else
	{
		//theApp.NavigateURL(file);
		// We cannot call NavigateURL() directly from Lisp as it seems to leave stack
		// in inconsistent state in the case of an error (well, at least from the Lisp's POV).
		// To avoid the problem we use this ugly hack.
		// The problem is probably related to the (SEH) exception handling:
		// Internet Explorer internally uses (or causes) some exception on which we rely too (just a theory).

		while (theApp.waitingForDocumentToOpen());
		ret = theApp.SetURLToOpen(file);
		theApp.OnBrowse(); // create browser window (to return its handle)
		// Later the documented will be opened in a browser window.
	}
	if (theApp.m_browser)
		*wnd = theApp.m_browser->m_hWnd;
	return ret ? S_OK : E_FAIL;
}

STDMETHODIMP CoCormanLispClient::AddMenu(char* /*menuName*/)
{
	return S_OK;
}

STDMETHODIMP CoCormanLispClient::AddMenuItem(char* /*menuName*/, char* /*menuItem*/)
{
	return S_OK;
}

STDMETHODIMP CoCormanLispClient::ReplaceSelection(char* text, long numBytes)
{
	theApp.SetReplaceSelection(text, numBytes);
	return S_OK;
}

//
// CoCormanLispShutdownClient
//
//////////////////////////////////////////////////////////////////////
// ctor and dtor
CoCormanLispShutdownClient::CoCormanLispShutdownClient()
{
	m_cRef = 0;
	m_dwCookie = -1;
	m_eventLoopThreadID = GetCurrentThreadId();
}

CoCormanLispShutdownClient::~CoCormanLispShutdownClient()
{
}


//////////////////////////////////////////////////////////////////////
// IUnknown interfaces

STDMETHODIMP CoCormanLispShutdownClient::QueryInterface(REFIID riid, void** ppv)
{
    if (riid == IID_IUnknown)
        *ppv = this;
    else if (riid == IID_ICormanLispShutdown)
        *ppv = (ICormanLispShutdown*)this;
    else
        *ppv = 0;
    if (*ppv)
        ((IUnknown*)*ppv)->AddRef();
    return *ppv ? S_OK : E_NOINTERFACE;
}

STDMETHODIMP_(ULONG) CoCormanLispShutdownClient::AddRef()
{
    return InterlockedIncrement(&m_cRef);
}

STDMETHODIMP_(ULONG) CoCormanLispShutdownClient::Release()
{
    if (InterlockedDecrement(&m_cRef) != 0)
        return m_cRef;
    delete this;
    return 0;
}

extern void DisableLispSystem();

STDMETHODIMP CoCormanLispShutdownClient::LispShutdown(char* text, long numBytes)
{
	text[numBytes] = 0;
	::MessageBox(AfxGetApp()->GetMainWnd()->m_hWnd, text, "Lisp Shutdown", MB_OK);
	DisableLispSystem();
	long offset = 0;
	return S_OK;
}

STDMETHODIMP CoCormanLispShutdownClient::Connect(IConnectionPoint* pConnectionPoint)
{
	return pConnectionPoint->Advise((ICormanLispShutdown*)this, &m_dwCookie);
}

STDMETHODIMP CoCormanLispShutdownClient::Disconnect(IConnectionPoint* pConnectionPoint)
{
	return pConnectionPoint->Unadvise(m_dwCookie);
}


