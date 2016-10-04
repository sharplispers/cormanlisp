//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		CoCormanLisp.cpp
//		Contents:	COM Class definitions for Corman Lisp	COM server.
//		History:	8/5/97  RGC  Created.
//					10/01/16  Artem Boldarev GetCurrentUserName method.
//						 	  global UserInfo object.
//
#include "stdafx.h"

#include <windows.h>
#include <initguid.h>
#include <wchar.h>
#include <ole2.h>
#include <olectl.h>
#include <string.h>

#include "CoCormanLisp.H"
#include "clsids.h"
#include "CormanLispServer.h"
#include "CoEnumConnectionPoints.h"
#include "CoConnectionPoint.h"


#include "Lisp.h"
#include "UserInfo.h"

extern void SvcLock();
extern void SvcUnlock();

CoCormanLisp* CormanLispServer = 0;

//////////////////////////////////////////////////////////////////////
// ctor and dtor
CoCormanLisp::CoCormanLisp():
pUserInfo(NULL)
{
	m_cRef = 0;
	m_pcpPointSink = 0;
	m_pcpShutdownPointSink = 0;

 	SvcLock();
	CormanLispServer = this;
}

CoCormanLisp::~CoCormanLisp()
{
	UserInfo *info = (UserInfo *)pUserInfo;
	SvcUnlock();
	delete info;
}


//////////////////////////////////////////////////////////////////////
// IUnknown interfaces

STDMETHODIMP CoCormanLisp::QueryInterface(REFIID riid, void** ppv)
{
    if (riid == IID_IUnknown)
        *ppv = (ICormanLisp*)this;
    else if (riid == IID_ICormanLisp)
        *ppv = (ICormanLisp*)this;
    else if (riid == IID_IConnectionPointContainer)
        *ppv = (IConnectionPointContainer*)this;
    else if (riid == IID_ICormanLispDirectCall)
        *ppv = (ICormanLispDirectCall*)this;
    else
        *ppv = 0;
    if (*ppv)
        ((IUnknown*)*ppv)->AddRef();
    return *ppv ? S_OK : E_NOINTERFACE;
}

STDMETHODIMP_(ULONG) CoCormanLisp::AddRef()
{
    return InterlockedIncrement(&m_cRef);
}

STDMETHODIMP_(ULONG) CoCormanLisp::Release()
{
    if (InterlockedDecrement(&m_cRef) != 0)
        return m_cRef;
    delete this;
    return 0;
}

//////////////////////////////////////////////////////////////////////
// ICormanLisp interface
extern const int LispImageNameMax = 256;
char LispImageName[LispImageNameMax] = "cormanlisp.img";
int CormanLispClientType = 0;

// client type should be WIN_APP_CLIENT, CONSOLE_CLIENT or IDE_CLIENT
STDMETHODIMP CoCormanLisp::Initialize(IUnknown* clientInterface, 
			const char* lispImage, int clientType)
{
	if (lispImage)
		strncpy_s(LispImageName, sizeof(LispImageName), lispImage, LispImageNameMax);
	CormanLispClientType = clientType;
	if (pUserInfo == NULL)
	{
		UserInfo *info = new UserInfo;
		if (!UserInfo::FillUserInfo(*info))
		{
			delete info;
			info = NULL;
			return S_FALSE;
		}
		pUserInfo = info;
	}
	InitializeCormanLisp(clientInterface, (UserInfo *)pUserInfo);
	return S_OK;
}

// client type should be WIN_APP_CLIENT, CONSOLE_CLIENT or IDE_CLIENT
STDMETHODIMP CoCormanLisp::InitializeEx(IUnknown* clientInterface, 
			const char* lispImage, int clientType,
            int heapReserve, int heapInitialSize, 
            int ephemeralHeap1Size, int ephemeralHeap2Size)
{
	if (lispImage)
		strncpy_s(LispImageName, sizeof(LispImageName), lispImage, LispImageNameMax);
	CormanLispClientType = clientType;
    if (heapReserve != 0)
    {
        if (heapReserve < LispHeapReserveMin)
            heapReserve = LispHeapReserveMin;
        if (heapReserve > LispHeapReserveMax)
            heapReserve = LispHeapReserveMax;
         LispHeapReserveDefault = heapReserve;
    }

    if (heapInitialSize != 0)
    {
        if (heapInitialSize < LispHeapSizeMin)
            heapInitialSize = LispHeapSizeMin;
        if (heapInitialSize > LispHeapSizeMax)
            heapInitialSize = LispHeapSizeMax;
         LispHeapSize = heapInitialSize;
    }

    if (ephemeralHeap1Size != 0)
    {
        if (ephemeralHeap1Size < EphemeralHeap1SizeMin)
            ephemeralHeap1Size = EphemeralHeap1SizeMin;
        if (ephemeralHeap1Size > EphemeralHeap1SizeMax)
            ephemeralHeap1Size = EphemeralHeap1SizeMax;
        EphemeralHeap1Size = ephemeralHeap1Size;
    }

    if (ephemeralHeap2Size != 0)
    {
        if (ephemeralHeap2Size < EphemeralHeap2SizeMin)
            ephemeralHeap2Size = EphemeralHeap2SizeMin;
        if (ephemeralHeap2Size > EphemeralHeap2SizeMax)
            ephemeralHeap2Size = EphemeralHeap2SizeMax;
        EphemeralHeap2Size = ephemeralHeap2Size;
    }

	if (pUserInfo == NULL)
	{
		UserInfo *info = new UserInfo;
		if (!UserInfo::FillUserInfo(*info))
		{
			delete info;
			info = NULL;
			return S_FALSE;
		}
		pUserInfo = info;
	}

	InitializeCormanLisp(clientInterface, (UserInfo *)pUserInfo);
	return S_OK;
}

STDMETHODIMP CoCormanLisp::Run(HANDLE* thread)
{
	RunCormanLisp(thread);
	return S_OK;
}

STDMETHODIMP CoCormanLisp::ProcessSource(char* text, long numChars)
{
	ProcessLispSource(text, numChars);
	return S_OK;
}

STDMETHODIMP CoCormanLisp::GetNumThreads(long* numThreads)
{
	*numThreads	= GetNumLispThreads();
	return S_OK;
}

STDMETHODIMP CoCormanLisp::UserException()
{
	::ThrowUserException();
	return S_OK;
}

STDMETHODIMP CoCormanLisp::AbortThread()
{
	::AbortLispThread();
	return S_OK;
}

//////////////////////////////////////////////////////////////////////
// IConnectionPointContainer methods
STDMETHODIMP CoCormanLisp::EnumConnectionPoints(IEnumConnectionPoints** ppEnumConnectionPoints)
{
	*ppEnumConnectionPoints = NULL;
	IConnectionPoint* rgConnectionPoint = (IConnectionPoint*)m_pcpPointSink;

	// Create the enumerator, we have only one connection point
	CoEnumConnectionPoints* pEnum = 
		new CoEnumConnectionPoints((ICormanLisp*)this, 1, 
			&rgConnectionPoint);

	if (!pEnum)
		return E_OUTOFMEMORY;

	pEnum->AddRef();
	*ppEnumConnectionPoints = pEnum;
	
	return S_OK;	
}

STDMETHODIMP 
CoCormanLisp::FindConnectionPoint(REFIID riid, IConnectionPoint** ppConnectionPoint)
{
	*ppConnectionPoint = 0;

	if (riid == IID_ICormanLispTextOutput)
	{
		if (!m_pcpPointSink)
		{
			m_pcpPointSink = new CoConnectionPoint(IID_ICormanLispTextOutput,
			 (IConnectionPointContainer*)this);
			m_pcpPointSink->AddRef();
		}

		m_pcpPointSink->QueryInterface(IID_IConnectionPoint, (void**)ppConnectionPoint);
	}

	else if (riid == IID_ICormanLispShutdown)
	{
		if (!m_pcpShutdownPointSink)
		{
			m_pcpShutdownPointSink = new CoConnectionPoint(IID_ICormanLispShutdown,
			 (IConnectionPointContainer*)this);
			m_pcpShutdownPointSink->AddRef();
		}

		m_pcpShutdownPointSink->QueryInterface(IID_IConnectionPoint, (void**)ppConnectionPoint);
	}

	return *ppConnectionPoint ? S_OK : CONNECT_E_NOCONNECTION;
}

STDMETHODIMP 
CoCormanLisp::SetMessage(char* text)
{
	HRESULT hr = S_OK;
	IEnumConnections* pEnum = 0;
	CONNECTDATA cd = { 0 };
	ICormanLispStatusMessage* messager = 0;

	if (!m_pcpPointSink)
		return hr;

	if (FAILED(m_pcpPointSink->EnumConnections(&pEnum)))
		return hr;

	while (pEnum->Next(1, &cd, NULL) == NOERROR)
	{
		if (SUCCEEDED(cd.pUnk->QueryInterface(IID_ICormanLispStatusMessage, 
				(void**)&messager)))
		{
			hr = messager->SetMessage(text);
			messager->Release();
		}
		cd.pUnk->Release();
	}
	pEnum->Release();
	return hr;
}

STDMETHODIMP 
CoCormanLisp::GetMessage(char* text, long maxMessageLength)
{
	HRESULT hr = S_OK;
	IEnumConnections* pEnum = 0;
	CONNECTDATA cd = { 0 };
	ICormanLispStatusMessage* messager = 0;

	if (!m_pcpPointSink)
		return hr;

	if (FAILED(m_pcpPointSink->EnumConnections(&pEnum)))
		return hr;

	while (pEnum->Next(1, &cd, NULL) == NOERROR)
	{
		if (SUCCEEDED(cd.pUnk->QueryInterface(IID_ICormanLispStatusMessage, 
				(void**)&messager)))
		{
			hr = messager->GetMessage(text, maxMessageLength);
			messager->Release();
		}
		cd.pUnk->Release();
	}
	pEnum->Release();
	return hr;
}

STDMETHODIMP 
CoCormanLisp::SetDefaultMessage()
{
	HRESULT hr = S_OK;
	IEnumConnections* pEnum = 0;
	CONNECTDATA cd = { 0 };
	ICormanLispStatusMessage* messager = 0;

	if (!m_pcpPointSink)
		return hr;

	if (FAILED(m_pcpPointSink->EnumConnections(&pEnum)))
		return hr;

	while (pEnum->Next(1, &cd, NULL) == NOERROR)
	{
		if (SUCCEEDED(cd.pUnk->QueryInterface(IID_ICormanLispStatusMessage, 
				(void**)&messager)))
		{
			hr = messager->SetDefaultMessage();
			messager->Release();
		}
		cd.pUnk->Release();
	}
	pEnum->Release();
	return hr;
}

STDMETHODIMP 
CoCormanLisp::OutputText(char* text, long numChars)
{
	HRESULT hr = S_OK;
	IEnumConnections* pEnum = 0;
	CONNECTDATA cd = { 0 };
	ICormanLispTextOutput* output = 0;

	if (!m_pcpPointSink)
		return hr;

	if (FAILED(m_pcpPointSink->EnumConnections(&pEnum)))
		return hr;

	while (pEnum->Next(1, &cd, NULL) == NOERROR)
	{
		if (SUCCEEDED(cd.pUnk->QueryInterface(IID_ICormanLispTextOutput, 
				(void**)&output)))
		{
			hr = output->OutputText(text, numChars);
			output->Release();
		}
		cd.pUnk->Release();
	}
	pEnum->Release();
	return hr;
}

STDMETHODIMP 
CoCormanLisp::LispShutdown(char* text, long numChars)
{
	HRESULT hr = S_OK;
	IEnumConnections* pEnum = 0;
	CONNECTDATA cd = { 0 };
	ICormanLispShutdown* output = 0;

	if (!m_pcpShutdownPointSink)
		return hr;

	if (FAILED(m_pcpShutdownPointSink->EnumConnections(&pEnum)))
		return hr;

	while (pEnum->Next(1, &cd, NULL) == NOERROR)
	{
		if (SUCCEEDED(cd.pUnk->QueryInterface(IID_ICormanLispShutdown, 
				(void**)&output)))
		{
			hr = output->LispShutdown(text, numChars);
			output->Release();
		}
		cd.pUnk->Release();
	}
	pEnum->Release();
	return hr;
}

STDMETHODIMP CoCormanLisp::GetAppInstance(HINSTANCE* appInstance)
{
	HRESULT hr = S_OK;
	IEnumConnections* pEnum = 0;
	CONNECTDATA cd = { 0 };
	ICormanLispTextOutput* output = 0;

	*appInstance = 0;

	if (!m_pcpPointSink)
		return hr;

	if (FAILED(m_pcpPointSink->EnumConnections(&pEnum)))
		return hr;

	while (pEnum->Next(1, &cd, NULL) == NOERROR)
	{
		if (SUCCEEDED(cd.pUnk->QueryInterface(IID_ICormanLispTextOutput, 
				(void**)&output)))
		{
			hr = output->GetAppInstance(appInstance);
			output->Release();
		}
		cd.pUnk->Release();
	}
	pEnum->Release();
	return hr;
}

STDMETHODIMP CoCormanLisp::GetAppMainWindow(HWND* appMainWindow)
{
	HRESULT hr = S_OK;
	IEnumConnections* pEnum = 0;
	CONNECTDATA cd = { 0 };
	ICormanLispTextOutput* output = 0;

	*appMainWindow = 0;

	if (!m_pcpPointSink)
		return hr;

	if (FAILED(m_pcpPointSink->EnumConnections(&pEnum)))
		return hr;

	while (pEnum->Next(1, &cd, NULL) == NOERROR)
	{
		if (SUCCEEDED(cd.pUnk->QueryInterface(IID_ICormanLispTextOutput, 
				(void**)&output)))
		{
			hr = output->GetAppMainWindow(appMainWindow);
			output->Release();
		}
		cd.pUnk->Release();
	}
	pEnum->Release();
	return hr;
}

STDMETHODIMP CoCormanLisp::OpenEditWindow(char* file, HWND* wnd)
{
	HRESULT hr = S_OK;
	IEnumConnections* pEnum = 0;
	CONNECTDATA cd = { 0 };
	ICormanLispStatusMessage* messager = 0;

	if (!m_pcpPointSink)
		return hr;

	if (FAILED(m_pcpPointSink->EnumConnections(&pEnum)))
		return hr;

	while (pEnum->Next(1, &cd, NULL) == NOERROR)
	{
		if (SUCCEEDED(cd.pUnk->QueryInterface(IID_ICormanLispStatusMessage, 
				(void**)&messager)))
		{
			hr = messager->OpenEditWindow(file, wnd);
			messager->Release();
		}
		cd.pUnk->Release();
	}
	pEnum->Release();
	return hr;
}

STDMETHODIMP CoCormanLisp::OpenURL(char* file, HWND* wnd)
{
	HRESULT hr = S_OK;
	IEnumConnections* pEnum = 0;
	CONNECTDATA cd = { 0 };
	ICormanLispStatusMessage* messager = 0;

	if (!m_pcpPointSink)
		return hr;

	if (FAILED(m_pcpPointSink->EnumConnections(&pEnum)))
		return hr;

	while (pEnum->Next(1, &cd, NULL) == NOERROR)
	{
		if (SUCCEEDED(cd.pUnk->QueryInterface(IID_ICormanLispStatusMessage, 
				(void**)&messager)))
		{
			hr = messager->OpenURL(file, wnd);
			messager->Release();
		}
		cd.pUnk->Release();
	}
	pEnum->Release();
	return hr;
}

STDMETHODIMP CoCormanLisp::AddMenu(char* menuName)
{
	HRESULT hr = S_OK;
	IEnumConnections* pEnum = 0;
	CONNECTDATA cd = { 0 };
	ICormanLispStatusMessage* messager = 0;

	if (!m_pcpPointSink)
		return hr;

	if (FAILED(m_pcpPointSink->EnumConnections(&pEnum)))
		return hr;

	while (pEnum->Next(1, &cd, NULL) == NOERROR)
	{
		if (SUCCEEDED(cd.pUnk->QueryInterface(IID_ICormanLispStatusMessage, 
				(void**)&messager)))
		{
			hr = messager->AddMenu(menuName);
			messager->Release();
		}
		cd.pUnk->Release();
	}
	pEnum->Release();
	return hr;
}

STDMETHODIMP CoCormanLisp::AddMenuItem(char* menuName, char* menuItem)
{
	HRESULT hr = S_OK;
	IEnumConnections* pEnum = 0;
	CONNECTDATA cd = { 0 };
	ICormanLispStatusMessage* messager = 0;

	if (!m_pcpPointSink)
		return hr;

	if (FAILED(m_pcpPointSink->EnumConnections(&pEnum)))
		return hr;

	while (pEnum->Next(1, &cd, NULL) == NOERROR)
	{
		if (SUCCEEDED(cd.pUnk->QueryInterface(IID_ICormanLispStatusMessage, 
				(void**)&messager)))
		{
			hr = messager->AddMenuItem(menuName, menuItem);
			messager->Release();
		}
		cd.pUnk->Release();
	}
	pEnum->Release();
	return hr;
}

STDMETHODIMP 
CoCormanLisp::ReplaceSelection(char* text, long numChars)
{
	HRESULT hr = S_OK;
	IEnumConnections* pEnum = 0;
	CONNECTDATA cd = { 0 };
	ICormanLispStatusMessage* messager = 0;

	if (!m_pcpPointSink)
		return hr;

	if (FAILED(m_pcpPointSink->EnumConnections(&pEnum)))
		return hr;

	while (pEnum->Next(1, &cd, NULL) == NOERROR)
	{
		if (SUCCEEDED(cd.pUnk->QueryInterface(IID_ICormanLispStatusMessage, 
				(void**)&messager)))
		{
			hr = messager->ReplaceSelection(text, numChars);
			messager->Release();
		}
		cd.pUnk->Release();
	}
	pEnum->Release();
	return hr;
}

STDMETHODIMP CoCormanLisp::BlessThread()
{
	::BlessThread();
	return S_OK;
}

STDMETHODIMP CoCormanLisp::UnblessThread()
{
	::UnblessThread();
	return S_OK;
}

STDMETHODIMP CoCormanLisp::GetFunctionAddress(wchar_t* functionName, wchar_t* packageName, void** funcptr)
{
	*funcptr = GetCallbackFunctionPointer(functionName, packageName);
	return S_OK;
}

STDMETHODIMP CoCormanLisp::HandleStructuredException(long exception, LPEXCEPTION_POINTERS info, long* result)
{
	*result = handleStructuredException(exception, info);
	return S_OK;
}

STDMETHODIMP CoCormanLisp::GetCurrentUserName(char *UserName, size_t *len)
{
	UserInfo *info = (UserInfo *)pUserInfo;
	size_t old_len;

	if (UserName == NULL && len == NULL)
	{
		return S_FALSE;
	}

	old_len = *len;
	*len = strlen(info->GetName());
	if (UserName != NULL)
	{
		if (old_len < *len)
			return S_FALSE;

		strcpy_s(UserName, (*len) + 1, info->GetName());
	}

	return S_OK;
}

//////////////////////////////////////////////////////////////////////
// Factory method

extern "C" IUnknown* CreateCormanLisp()
{
    ICormanLisp* p = new CoCormanLisp;
    if (p)
        p->AddRef();
    return p;
}
