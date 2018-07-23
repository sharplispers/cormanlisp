//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		clconsole.cpp
//		Contents:	Corman Lisp console application client.
//		Author:		Roger Corman
//		Created:	3/6/98
//

#include <wtypes.h>
#include <string.h>
#include <stdio.h>
#include <ocidl.h>
#include <initguid.h>
#include <conio.h>
#include <fcntl.h>
#include <io.h>

#include "clsids.h"
#include "ErrorMessage.h"
#include "ICormanLisp.h"

static char gModuleName[MAX_PATH];
static char gImageName [MAX_PATH];
bool isTemplateApp = false;

const char SERVER_TITLE[] = "CormanLispServer.dll";
char LispServerPath[MAX_PATH + 1 + sizeof(SERVER_TITLE)];
typedef HRESULT (WINAPI *GETCLASSOBJECTFUNC)(REFCLSID rclsid, REFIID riid, void**ppv);

static void outputConsoleText(char* text);
static char* getConsoleText();
static HINSTANCE getLocalCormanLispServer();
static IClassFactory* getCormanLispClassFactory();
static IClassFactory* getCormanLispRegisteredClassFactory();
static void LoadFile(char* filename);

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

typedef void (WINAPI *LOADLIBRARYFUNC)();

ICormanLisp* pCormanLisp = 0;
char* execFile = 0;

int mainx(int argc, char* argv[])
{
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
		fprintf(stderr, "Connection failed");
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
		fprintf(stderr, "Connection failed");
	}

	pCormanLisp->Initialize(pCormanLispClient, gImageName, CONSOLE_CLIENT);
	HANDLE thread = 0;
	pCormanLisp->Run(&thread);

	long numThreads = 0;
	char* input = 0;
	bool quitting = false;
	if (execFile)
	{
		LoadFile(execFile);
		char exitCommand[] = "(win:ExitProcess 0)\n";
		pCormanLisp->ProcessSource(exitCommand, sizeof(exitCommand));
	}
	/*
	else
	{	
		if (!isTemplateApp)	// if the plain console app, put out this message
			outputConsoleText("Type :quit to exit.\r\n");
	}
	*/
	while (1)
	{
		if (!quitting)
			input = getConsoleText();
		if (strlen(input) > 0)
		{
			if (!_stricmp(input, ":quit\r\n"))
				break;
			if (!quitting)
				pCormanLisp->ProcessSource(input, strlen(input));
		}
		else
		if (!quitting)
		{
			// construct this so normally the first expression will be
			// read and executed. However, if the reader was in the middle
			// of reading an expression, we don't want to hang with both
			// threads waiting for the other, so we follow if immediately
			// with an expression which will generate a read error (in most
			// situations--could still be defeated if it comes inside a string
			// or comment or something).
			char exitCommand[] = "(win:ExitProcess 0) \n #< expression not finished > \n (win:ExitProcess 0)\n";
			quitting = true;
			pCormanLisp->ProcessSource(exitCommand, sizeof(exitCommand));
			Sleep(1000);
		}
	}
	if (pCormanLisp)
		pCormanLisp->Release();
	pCormanLisp = 0;
	CoUninitialize();
	return 0;
}

//
//	If a CormanLispServer.dll exists in the same directory as
//	the calling application, load it and return a handle to it.
//
static HINSTANCE getLocalCormanLispServer()
{
 	DWORD chars = GetModuleFileName(0, LispServerPath, sizeof(LispServerPath));
	int index = chars - 1;
	while (index >= 0 && LispServerPath[index] != '\\')
		index--;
	LispServerPath[index] = 0;	// get rid of file name, just leave the path
	if (chars > 0)
		strcat_s(LispServerPath, sizeof(LispServerPath), "\\");
	strcat_s(LispServerPath, sizeof(LispServerPath), SERVER_TITLE);

	return LoadLibrary(LispServerPath);
}

static IClassFactory* getCormanLispClassFactory()
{
	IClassFactory* pcf = 0;
	HINSTANCE module = getLocalCormanLispServer();
	if (module)
	{
		FARPROC proc = GetProcAddress(module, "DllGetClassObject");
		if (!proc)
		{
			ErrorMessage("Could not find DllGetClassObject() in CormanLispServer.dll");
			return 0;
		}
		GETCLASSOBJECTFUNC GetClassObjectFunc = (GETCLASSOBJECTFUNC)proc;

		HRESULT hr = GetClassObjectFunc(CLSID_CormanLisp,
					  IID_IClassFactory,
					  (void**)&pcf);

		if (FAILED(hr))
		{
			ErrorMessage(__TEXT("CoGetClassObject"), hr);
			return 0;
		}
	}
	if (!pcf)
		pcf = getCormanLispRegisteredClassFactory();
	return pcf;
}

static IClassFactory* getCormanLispRegisteredClassFactory()
{
	IClassFactory* pcf = 0;

	// see if the class is registered
	HRESULT hr = CoGetClassObject(CLSID_CormanLisp,
						  CLSCTX_INPROC_SERVER,
						  0,
						  IID_IClassFactory,
						  (void**)&pcf);
	if (FAILED(hr))
	{
		if (hr == REGDB_E_CLASSNOTREG)
		{
			// the server was not registered, so see if we can
			// find it and register it now
			HINSTANCE plserver = LoadLibrary(SERVER_TITLE);
			if (!plserver)
			{
				ErrorMessage("Could not load CormanLispServer.dll");
				return 0;
			}
			FARPROC proc = GetProcAddress(plserver, "DllRegisterServer");
			if (!proc)
			{
				ErrorMessage("Could not find DllRegisterServer() in CormanLispServer.dll");
				return 0;
			}
			LOADLIBRARYFUNC func = (LOADLIBRARYFUNC)proc;
			func();				// calls DllRegisterServer()

			// now try again
			hr = CoGetClassObject(CLSID_CormanLisp,
						  CLSCTX_INPROC_SERVER,
						  0,
						  IID_IClassFactory,
						  (void**)&pcf);

			if (FAILED(hr))
			{
				ErrorMessage(__TEXT("CoGetClassObject"), hr);
				return FALSE;
			}
		}
		else
		{
			ErrorMessage(__TEXT("CoGetClassObject"), hr);
			return FALSE;
		}
	}
	return pcf;
}

long numThreads = 0;

BOOL __stdcall HandlerRoutine(DWORD dwCtrlType)
{
	BOOL handled = FALSE;
	switch (dwCtrlType)
	{
	case CTRL_C_EVENT:
		if (pCormanLisp)
		{
			pCormanLisp->GetNumThreads(&numThreads);
			if (numThreads > 0)
				pCormanLisp->AbortThread();
		}
		handled = TRUE;
		break;
	case CTRL_BREAK_EVENT:
		if (pCormanLisp)
		{
			pCormanLisp->GetNumThreads(&numThreads);
			if (numThreads > 0)
				pCormanLisp->AbortThread();
		}
		handled = TRUE;
		break;
	case CTRL_CLOSE_EVENT:
		break;
	case CTRL_LOGOFF_EVENT:
		break;
	case CTRL_SHUTDOWN_EVENT:
		break;
	}
	return handled;
}

const char* consoleAppName = "CLCONSOLE.EXE";

int main(int argc, char* argv[])
{
	// use binary mode
	int result = _setmode(_fileno(stdout), _O_BINARY);
	result = _setmode(_fileno(stdin), _O_BINARY);

	BOOL ret = SetConsoleCtrlHandler(HandlerRoutine, TRUE);
	SetConsoleTitle("Corman Lisp Console");
#ifdef SET_CONSOLE_SCREEN_BUFFER_SIZE
	HANDLE handle = GetStdHandle(STD_OUTPUT_HANDLE);
	COORD coord;
	coord.X = 100;
	coord.Y = 100;		// default console size if 100 x 100
	SetConsoleScreenBufferSize(handle, coord);
#endif // SET_CONSOLE_SCREEN_BUFFER_SIZE
	GetModuleFileName(0, gModuleName, sizeof(gModuleName));

	isTemplateApp = _stricmp(argv[0], "clconsole") != 0;	// see if this app is being used
															// as a template

	char* imageName = 0;
	for (int i = 1; i < argc; i++)
	{
		if (!_stricmp(argv[i], "-image") && (i + 1) < argc)
		{
			imageName = argv[i+1];
		}
		if (!_stricmp(argv[i], "-execute") && (i + 1) < argc)
		{
			execFile = argv[i+1];
		}
	}
	if (imageName)
		strcpy_s(gImageName, sizeof(gImageName), imageName);
	else
			// default to the image has the same name as the executable,
			// but with a .img extension.
	{
		// as a special case, if this is the clconsole application 
		// default to CormanLisp.img
		unsigned int moduleNameLength = strlen(gModuleName);
		if (moduleNameLength >= strlen(consoleAppName)
			&& !_stricmp(gModuleName + (moduleNameLength - strlen(consoleAppName)), 
					consoleAppName))
        {
            // copy entire exe path and name
            strcpy_s(gImageName, sizeof(gImageName), gModuleName);
            // replace clconsole.exe with cormanlisp.img
			strcpy_s(gImageName + (moduleNameLength - strlen(consoleAppName)), sizeof(gImageName), "CormanLisp.img");
        }
		else
		{
			strcpy_s(gImageName, sizeof(gImageName), gModuleName);
		}
	}
	static char szAppName[] = "CormanLispConsole" ;
	mainx(0, 0);
	return 0;
}

//	If an entire line has been entered, return it.
//	Otherwise return an empty string.
//
char ConsoleTextBuf[4096];
static char* s = ConsoleTextBuf;

static void outputConsoleText(char* text)
{
	char* p = text;
	while (*p)
	{
		putchar(*p++);
	}
}

static char* getConsoleText()
{
	while (1)
	{
		int c = getchar();
	
		if (c == EOF)
		{
		//	*s++ = 0;
		//	s = ConsoleTextBuf; 
		//	return s;
		}
		else
		if (c == 10)
		{
			*s++ = (char)c;
			*s++ = 0;
			s = ConsoleTextBuf; 
			return s;
		}
		else
			*s++ = (char)c;
	}
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
	for (int i = 0; i < numBytes; i++)
	{
		putchar(text[i]);
	}
	fflush(stdout);
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
	*appInstance = 0;
	return S_OK;
}

STDMETHODIMP ConsoleCormanLispClient::GetAppMainWindow(HWND* appMainWindow)
{
	*appMainWindow = 0;
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

STDMETHODIMP ConsoleCormanLispClient::OpenURL(char* file, HWND* /*wnd*/)
{
	return (ShellExecuteA(NULL, "open", file, NULL, NULL, SW_SHOWNORMAL) > (HINSTANCE)32 ? S_OK : E_FAIL);
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

BYTE* MapFile(const char* path, DWORD* length)
{
	HANDLE hfile = CreateFile(path,
						GENERIC_READ,
						FILE_SHARE_READ,
						NULL,
						OPEN_EXISTING,
						FILE_ATTRIBUTE_NORMAL,
						NULL);
	if (hfile == INVALID_HANDLE_VALUE)
		return 0;
	*length = GetFileSize(hfile, 0);
	if (*length == 0xffffffff)
	{
		CloseHandle(hfile);
		return 0;
	}

	HANDLE hfilemap = CreateFileMapping(hfile,
						0,
						PAGE_READONLY,
						0, 0,
						0);

	CloseHandle(hfile);
	if (!hfilemap)
		return 0;

	BYTE* pbFile = (BYTE*)MapViewOfFile(hfilemap, FILE_MAP_READ, 0, 0, 0);
	CloseHandle(hfilemap);
	return pbFile;
}

void UnmapFile(BYTE* mapping)
{
	UnmapViewOfFile(mapping);
}

static void LoadFile(char* filename)
{
	char* ext = filename + strlen(filename);
	while (ext > filename && *ext != '.')
		ext--;
	if (*ext == '.' && !_stricmp(ext, ".fasl"))
	{
		// load a compiled file
		char command[512];
		strcpy_s(command, sizeof(command), "(common-lisp:load #P\"");
		strcat_s(command, sizeof(command), filename);
		strcat_s(command, sizeof(command), "\")");
		pCormanLisp->ProcessSource(command, strlen(command));
		return;
	}

	// load the file into memory
	DWORD fileSize;
	BYTE* data = MapFile(filename, &fileSize);

	if (!data)
	{
		fprintf(stderr, "Error: Could not open file %s.\n", filename);
		return;
	}

	// process code in the file
	pCormanLisp->ProcessSource((char*)data, fileSize);

	UnmapFile(data);
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
	for (int i = 0; i < numBytes; i++)
	{
		putchar(text[i]);
	}
	fflush(stdout);
	exit(0);
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
