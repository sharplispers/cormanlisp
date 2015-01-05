//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		clbootapp.cpp
//		Contents:	Corman Lisp skeleton client.
//		Author:		Roger Corman
//		Created:	3/6/98
//

#include <wtypes.h>
#include <ocidl.h>
//#include <initguid.h>
#include <string.h>
#include <conio.h>

#include "clsids.h"
#include "ErrorMessage.h"
#include "ICormanLisp.h"

static long FAR PASCAL WndProc (HWND hwnd, UINT message, UINT wParam, LONG lParam);
typedef HRESULT (WINAPI *GETCLASSOBJECTFUNC)(REFCLSID rclsid, REFIID riid, void**ppv);
static HINSTANCE getLocalCormanLispServer();
static IClassFactory* getCormanLispClassFactory();
static IClassFactory* getCormanLispRegisteredClassFactory();
typedef void (WINAPI *LOADLIBRARYFUNC)();

static char gModuleName[256];
static char gImageName[256];

class ConsoleCormanLispClient 
	: public ICormanLispStatusMessage
{
public:
	ConsoleCormanLispClient();
	~ConsoleCormanLispClient();

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
    STDMETHODIMP AddMenu(char* menuName);
    STDMETHODIMP AddMenuItem(char* menuName, char* menuItem);
    STDMETHODIMP OpenURL(char* file, HWND* wnd);
    STDMETHODIMP ReplaceSelection(char* text, long numChars);

// Helper functions
	STDMETHODIMP Connect(IConnectionPoint* pConnectionPoint);
	STDMETHODIMP Disconnect(IConnectionPoint* pConnectionPoint);

private:
	long m_cRef;
	DWORD m_dwCookie;
};

class ConsoleCormanLispShutdownClient 
	: public ICormanLispShutdown
{
public:
	ConsoleCormanLispShutdownClient();
	~ConsoleCormanLispShutdownClient();

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
};

int mainx(int argc, char* argv[])
{
	ICormanLisp* pCormanLisp = 0;
	IConnectionPoint* pConnectionPoint = 0;
	IConnectionPoint* pShutdownConnectionPoint = 0;

    HRESULT hr = E_FAIL;
	CoInitialize(0);

	//  Get the CormanLisp class factory
	IClassFactory* pcf = getCormanLispClassFactory();
	IUnknown* pUnk = 0;
    hr = pcf->CreateInstance(0, IID_IUnknown, (void**)&pUnk);
	if (FAILED(hr))
	{
		ErrorMessage(__TEXT("QueryInterface() did not return IID_IUnknown"), hr);
		return FALSE;
	}
	pcf->Release();
	pcf = 0;

	hr = pUnk->QueryInterface(IID_ICormanLisp, (void**)&pCormanLisp);
	if (FAILED(hr))
	{
		ErrorMessage(__TEXT("QueryInterface() did not return IID_ICormanLisp"), hr);
		return FALSE;
	}

	// Connect the ICormanLispClient Sink
	IConnectionPointContainer* pConnectionPointContainer = 0;
	hr = pCormanLisp->QueryInterface(IID_IConnectionPointContainer,
		(void**)&pConnectionPointContainer);

	if (FAILED(hr))
	{
		ErrorMessage(__TEXT("QueryInterface() did not return IID_IConnectionPointContainer"), hr);
		return FALSE;
	}

	hr = pConnectionPointContainer->FindConnectionPoint(IID_ICormanLispTextOutput, 
		&pConnectionPoint);
	if (FAILED(hr))
	{
		pConnectionPointContainer->Release();
		ErrorMessage(__TEXT("FindConnectionPoint() did not return IID_ICormanLispStatusMessage"), 
			hr);
		return FALSE;
	}

	// make an instance of our outgoing class interface
	ConsoleCormanLispClient* pCormanLispClient = new ConsoleCormanLispClient;
	pCormanLispClient->AddRef();

	hr = pCormanLispClient->Connect(pConnectionPoint);
	if (FAILED(hr))
	{
		pCormanLispClient->Release();
		pConnectionPoint->Release();
		MessageBox(0, "Connection failed", 0, 0);
	}

	hr = pConnectionPointContainer->FindConnectionPoint(IID_ICormanLispShutdown, 
		&pShutdownConnectionPoint);
	pConnectionPointContainer->Release();
	if (FAILED(hr))
	{
		ErrorMessage(__TEXT("FindConnectionPoint() did not return IID_ICormanLispShutdowne"), 
			hr);
		return FALSE;
	}

	// make an instance of our outgoing shutdown class interface
	ConsoleCormanLispShutdownClient* pCormanLispShutdownClient = new ConsoleCormanLispShutdownClient;
	pCormanLispShutdownClient->AddRef();

	hr = pCormanLispShutdownClient->Connect(pShutdownConnectionPoint);
	if (FAILED(hr))
	{
		pCormanLispShutdownClient->Release();
		pShutdownConnectionPoint->Release();
		MessageBox(0, "Connection failed", 0, 0);
	}

	pCormanLisp->Initialize(pCormanLispClient, gImageName, WIN_APP_CLIENT);
	HANDLE thread = 0;
	pCormanLisp->Run(&thread);
//	WaitForSingleObject(thread, INFINITE);
	MsgWaitForMultipleObjects(1, &thread, FALSE, INFINITE, QS_ALLEVENTS); 
	if (pCormanLisp)
		pCormanLisp->Release();
	pCormanLisp = 0;
	CoUninitialize();
	return 0;
}

__declspec(dllexport) 
HRESULT StaticGetClassObject(REFCLSID rclsid, REFIID riid, void**ppv);

static IClassFactory* getCormanLispClassFactory()
{
	IClassFactory* pcf = 0;
	HRESULT hr = StaticGetClassObject(CLSID_CormanLisp,
					  IID_IClassFactory,
					  (void**)&pcf);

	if (FAILED(hr))
	{
		ErrorMessage(__TEXT("CoGetClassObject"), hr);
		return 0;
	}
	return pcf;
}

HINSTANCE gInstance = 0;
HWND gMainWnd = 0;

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE /*hPrevInstance*/,
					PSTR szCmdLine, int /*iCmdShow*/)
{
	gInstance = hInstance;
	GetModuleFileName(0, gModuleName, sizeof(gModuleName));
	strcpy_s(gImageName, sizeof(gImageName), gModuleName);
	mainx(0, 0);
	return 0;
}

//////////////////////////////////////////////////////////////////////
// ctor and dtor
ConsoleCormanLispClient::ConsoleCormanLispClient()
{
	m_cRef = 0;
	m_dwCookie = (unsigned long)-1;
}

ConsoleCormanLispClient::~ConsoleCormanLispClient()
{
}


//////////////////////////////////////////////////////////////////////
// IUnknown interfaces

STDMETHODIMP ConsoleCormanLispClient::QueryInterface(REFIID riid, void** ppv)
{
    if (riid == IID_IUnknown)
        *ppv = (ICormanLisp*)this;
    else if (riid == IID_ICormanLispTextOutput)
        *ppv = (ICormanLispTextOutput*)this;
    else if (riid == IID_ICormanLispStatusMessage)
        *ppv = (ICormanLispStatusMessage*)this;
    else
        *ppv = 0;
    if (*ppv)
        ((IUnknown*)*ppv)->AddRef();
    return *ppv ? S_OK : E_NOINTERFACE;
}

STDMETHODIMP_(ULONG) ConsoleCormanLispClient::AddRef()
{
    return InterlockedIncrement(&m_cRef);
}

STDMETHODIMP_(ULONG) ConsoleCormanLispClient::Release()
{
    if (InterlockedDecrement(&m_cRef) != 0)
        return m_cRef;
    delete this;
    return 0;
}

STDMETHODIMP ConsoleCormanLispClient::OutputText(char* text, long numBytes)
{
	text[numBytes] = 0;
	OutputDebugString(text);
	return S_OK;
}

STDMETHODIMP ConsoleCormanLispClient::SetMessage(char* /*message*/)
{
	return S_OK;
}

STDMETHODIMP ConsoleCormanLispClient::GetMessage(char* /*text*/, long /*maxMessageLength*/)
{
	return S_OK;
}

STDMETHODIMP ConsoleCormanLispClient::SetDefaultMessage()
{
	return S_OK;
}

STDMETHODIMP ConsoleCormanLispClient::GetAppInstance(HINSTANCE* appInstance)
{
//	*appInstance = AfxGetApp()->m_hInstance;
	*appInstance = gInstance;
	return S_OK;
}

STDMETHODIMP ConsoleCormanLispClient::GetAppMainWindow(HWND* appMainWindow)
{
//	*appMainWindow = AfxGetApp()->GetMainWnd()->m_hWnd;
	*appMainWindow = gMainWnd;
	return S_OK;
}

STDMETHODIMP ConsoleCormanLispClient::Connect(IConnectionPoint* pConnectionPoint)
{
	return pConnectionPoint->Advise((ICormanLispStatusMessage*)this, &m_dwCookie);
}

STDMETHODIMP ConsoleCormanLispClient::Disconnect(IConnectionPoint* pConnectionPoint)
{
	return pConnectionPoint->Unadvise(m_dwCookie);
}

STDMETHODIMP ConsoleCormanLispClient::OpenEditWindow(char* /*file*/, HWND* /*wnd*/)
{
	return E_FAIL;
}

STDMETHODIMP ConsoleCormanLispClient::OpenURL(char* /*file*/, HWND* /*wnd*/)
{
	return E_FAIL;
}

STDMETHODIMP ConsoleCormanLispClient::AddMenu(char* /*menuName*/)
{
	return E_FAIL;
}

STDMETHODIMP ConsoleCormanLispClient::AddMenuItem(char* /*menuName*/, char* /*menuItem*/)
{
	return E_FAIL;
}

STDMETHODIMP ConsoleCormanLispClient::ReplaceSelection(char* /*text*/, long /*numChars*/)
{
	return E_FAIL;
}

//////////////////////////////////////////////////////////////////////
// ctor and dtor
ConsoleCormanLispShutdownClient::ConsoleCormanLispShutdownClient()
{
	m_cRef = 0;
	m_dwCookie = (unsigned long)-1;
}

ConsoleCormanLispShutdownClient::~ConsoleCormanLispShutdownClient()
{
}


//////////////////////////////////////////////////////////////////////
// IUnknown interfaces

STDMETHODIMP ConsoleCormanLispShutdownClient::QueryInterface(REFIID riid, void** ppv)
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

STDMETHODIMP_(ULONG) ConsoleCormanLispShutdownClient::AddRef()
{
    return InterlockedIncrement(&m_cRef);
}

STDMETHODIMP_(ULONG) ConsoleCormanLispShutdownClient::Release()
{
    if (InterlockedDecrement(&m_cRef) != 0)
        return m_cRef;
    delete this;
    return 0;
}

STDMETHODIMP ConsoleCormanLispShutdownClient::LispShutdown(char* text, long numBytes)
{
	text[numBytes] = 0;
	OutputDebugString(text);
	return S_OK;
}


STDMETHODIMP ConsoleCormanLispShutdownClient::Connect(IConnectionPoint* pConnectionPoint)
{
	return pConnectionPoint->Advise((ICormanLispStatusMessage*)this, &m_dwCookie);
}

STDMETHODIMP ConsoleCormanLispShutdownClient::Disconnect(IConnectionPoint* pConnectionPoint)
{
	return pConnectionPoint->Unadvise(m_dwCookie);
}
