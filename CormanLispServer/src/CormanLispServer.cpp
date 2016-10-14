//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		CormanLispServer.cpp
//		Contents:	Server for Corman Lisp
//		History:	8/5/97  RGC  Created.
//					4/10/16 Artem Boldarev
//							global UserInfo object.
//

#include "stdafx.h"
#include <process.h>
#include <windows.h>
#include <ole2.h>
#include <string.h>

#include "CormanLispServer.h"
#include "lisp.h"

#include "clsids.h"
#include "CoCormanLisp.h"

IUnknown*				 ClientUnknown		= 0;
ICormanLispTextOutput*	 ClientTextOutput	= 0;
ICormanLispStatusMessage* ClientMessage		= 0;
ICormanLispShutdown*     ClientShutdown		= 0;
const UserInfo*          CurrentUserInfo    = 0;


TextOutputFuncType TextOutputFuncPtr = 0;
HINSTANCE gAppInstance = 0;
HWND      gAppMainWindow = 0;

int Windows_NT = 0;
OSVERSIONINFO gOsVersionInfo;
TCHAR g_szFileName[MAX_PATH];
char CormanLispServerDirectory[MAX_PATH+1];
char CormanLispOutputDirectory[MAX_PATH + 1] = {0};
const size_t CormanLispOutputDirectoryBufferSize = sizeof(CormanLispOutputDirectory);
size_t CormanLispOutputDirectoryLen = 0;

CharBuf TerminalInputBuf(0x8000);

static void RunLispThread(HANDLE* thread);

const TCHAR *const g_szRegKeyValues[][2] =
{
  { __TEXT("CormanLisp.Lisp"), __TEXT("Lisp") },
  { __TEXT("CormanLisp.Lisp\\CLSID"), __TEXT("{99AA27B3-0EAE-11d1-ACB9-00A024803258}") },
  { __TEXT("CLSID\\{99AA27B3-0EAE-11d1-ACB9-00A024803258}"), __TEXT("Lisp") },
  { __TEXT("CLSID\\{99AA27B3-0EAE-11d1-ACB9-00A024803258}\\InprocServer32"), g_szFileName },
  { __TEXT("CLSID\\{99AA27B3-0EAE-11d1-ACB9-00A024803258}\\ProgID"), __TEXT("CormanLisp.Lisp") },
};

const int g_cRegKeyValues = sizeof(g_szRegKeyValues)
                            /sizeof(*g_szRegKeyValues);
int DLL_Loaded = false;		// we may get run when statically linked

void processAttach(HINSTANCE hInstance)
{
	BOOL ret = 0;
	char* result = 0;
	QV_Index = TlsAlloc();
	Thread_Index = TlsAlloc();
	TlsSetValue(QV_Index, 0);
	TlsSetValue(Thread_Index, 0);
	TlsGetValue(QV_Index);
	TlsGetValue(Thread_Index);
	gOsVersionInfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	ret = GetVersionEx(&gOsVersionInfo);
	Windows_NT = (gOsVersionInfo.dwPlatformId == VER_PLATFORM_WIN32_NT);
	GetModuleFileName(hInstance, g_szFileName, MAX_PATH);

	// create a named mutex that is used to determine if the
	// Corman Lisp kernel is loaded
	CreateMutex(NULL, FALSE, "CormanLispServer");

	// setup the DLL directory
	strcpy_s(CormanLispServerDirectory, sizeof(CormanLispServerDirectory), g_szFileName);
	result = strrchr(CormanLispServerDirectory, '\\');
	if (result)
		*result = 0;
	else
		CormanLispServerDirectory[0] = 0;
}

extern "C" int __declspec( dllexport ) __stdcall
DllMain(HINSTANCE hInstance, DWORD dwReason, LPVOID /*lpReserved*/)
{
	char* result = 0;
	BOOL ret = 0;
	switch (dwReason)
	{
		case DLL_PROCESS_ATTACH:
			DLL_Loaded = true;		
			processAttach(hInstance);
			break;
		case DLL_PROCESS_DETACH:
			TlsFree(QV_Index);
			TlsFree(Thread_Index);
			break;
		case DLL_THREAD_ATTACH:
			TlsSetValue(QV_Index, 0);
			TlsSetValue(Thread_Index, 0);
			TlsGetValue(QV_Index);
			TlsGetValue(Thread_Index);
			break;
		case DLL_THREAD_DETACH:
			// need to free QV, ThreadRecord
			TlsSetValue(QV_Index, 0);
			TlsSetValue(Thread_Index, 0);
			break;
	}
	return 1;   // ok
}

LONG g_cLocks = 0;
void SvcLock()	{ InterlockedIncrement(&g_cLocks); }
void SvcUnlock(){ InterlockedDecrement(&g_cLocks); }

class CoCormanLispClassFactory : public IClassFactory
{
public:
    STDMETHODIMP QueryInterface(REFIID riid, void**ppv)
    {
        if (riid == IID_IUnknown)
            *ppv = (IClassFactory*)this;
        else if (riid == IID_IClassFactory)
            *ppv = (IClassFactory*)this;
        else 
            *ppv = 0;
        if (*ppv)
            ((IUnknown *)*ppv)->AddRef();
        return *ppv ? S_OK : E_NOINTERFACE;
    }

    STDMETHODIMP_(ULONG) AddRef(void)
    {
        SvcLock();
        return 2;
    }

    STDMETHODIMP_(ULONG) Release(void)
    {
        SvcUnlock();
        return 1;
    }

    STDMETHODIMP CreateInstance(IUnknown *pUnkOuter, REFIID riid, void **ppv)
    {
        *ppv = 0;
        if (pUnkOuter)
            return CLASS_E_NOAGGREGATION;

        CoCormanLisp *p = new CoCormanLisp;

        if (!p)
            return E_OUTOFMEMORY;

        p->AddRef();
        HRESULT hr = p->QueryInterface(riid, ppv);
        p->Release();
        return hr;
    }

    STDMETHODIMP LockServer(BOOL bLock)
    {
        if (bLock)
            SvcLock();
        else
            SvcUnlock();
        return S_OK;
    }
};

// TODO-Declare an instance of your class factory at global scope.
CoCormanLispClassFactory g_classFactory;

STDAPI DllGetClassObject(REFCLSID rclsid, REFIID riid, void**ppv)
{
    *ppv = 0;
    if (rclsid == CLSID_CormanLisp)
        return g_classFactory.QueryInterface(riid, ppv);
    else
        return CLASS_E_CLASSNOTAVAILABLE;
}

// for static linking
__declspec(dllexport) HRESULT StaticGetClassObject(REFCLSID rclsid, REFIID riid, void**ppv)
{
    *ppv = 0;
    if (rclsid == CLSID_CormanLisp)
        return g_classFactory.QueryInterface(riid, ppv);
    else
        return CLASS_E_CLASSNOTAVAILABLE;
}

STDAPI DllCanUnloadNow()
{
    return g_cLocks ? S_FALSE : S_OK;
}

STDAPI DllUnregisterServer();

STDAPI DllRegisterServer()
{
	if (*g_szFileName == 0)
		strcpy_s(g_szFileName, sizeof(g_szFileName), "CormanLispServer.dll");
    LONG r = ERROR_SUCCESS;
    for (int i = 0; 
         r == ERROR_SUCCESS && i < g_cRegKeyValues; 
         i++)
        r = RegSetValue(HKEY_CLASSES_ROOT,
                        g_szRegKeyValues[i][0], 
                        REG_SZ,
                        g_szRegKeyValues[i][1],
                        lstrlen(g_szRegKeyValues[i][1]));
    if (r != ERROR_SUCCESS)
        DllUnregisterServer();
    return (r == ERROR_SUCCESS) ? S_OK : E_FAIL;
}

STDAPI DllUnregisterServer()
{
    HRESULT result = S_OK;
    for (int i = g_cRegKeyValues - 1; i >= 0; i--)
    {
        LONG r = RegDeleteKey(HKEY_CLASSES_ROOT,
                              g_szRegKeyValues[i][0]);
        if (r != ERROR_SUCCESS)
            result = S_FALSE;
    }
    return result;
}

void setWindowsNT()
{
	BOOL ret = 0;
	gOsVersionInfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	ret = GetVersionEx(&gOsVersionInfo);
	Windows_NT = (gOsVersionInfo.dwPlatformId == VER_PLATFORM_WIN32_NT);
}

extern "C" long Initialize(const wchar_t* imageName, int clientType, HINSTANCE appInstance,
			   HANDLE* thread, HWND mainWindow, TextOutputFuncType TextOutputFunc)
{
    static int initFlag = 0;
    static CriticalSection initSect;

    initSect.Enter();
    if (initFlag == 0)      // only want to initialize once
    {
        initFlag++;
	    const wchar_t* ch = 0;
	    int i = 0;
	    InitializationEvent.ResetEvent();

	    if (!DLL_Loaded)	
		    processAttach(appInstance);
	    setWindowsNT();
	    TextOutputFuncPtr = TextOutputFunc;
	    gAppInstance = appInstance;
	    gAppMainWindow = mainWindow;
	    if (imageName)
	    {
		    for (ch = imageName; *ch && i < LispImageNameMax - 1; ch++, i++)
			    LispImageName[i] = (unsigned char)(*ch & 0xff);		// just copy low byte
	    }
	    LispImageName[i] = 0;

	    // initialize lisp system
	    TlsSetValue(QV_Index, QV);	// store global QV as the QV for this thread
	    TlsGetValue(QV_Index);

	    initLisp();
	    TlsSetValue(QV_Index, 0);
	    CormanLispClientType = clientType;
	    RunLispThread(thread);		// start 'er up
    }
    initSect.Leave();

	return 1;		// success
}

void InitializeCormanLisp(IUnknown* client, const UserInfo *user_info)
{
	HRESULT hr = 0;
	ClientUnknown = client;
	BOOL ret = 0;
	HINSTANCE hInstance = 0;
	CurrentUserInfo = user_info;

	setWindowsNT();

	if (ClientUnknown)
	{
		hr = ClientUnknown->QueryInterface(IID_ICormanLispTextOutput, (void**)&ClientTextOutput);
		hr = ClientUnknown->QueryInterface(IID_ICormanLispStatusMessage, (void**)&ClientMessage);
		hr = ClientUnknown->QueryInterface(IID_ICormanLispShutdown, (void**)&ClientShutdown);
	}
	if (!DLL_Loaded)
	{
		ClientTextOutput->GetAppInstance(&hInstance);
		processAttach(hInstance);
	}

	// initialize lisp system
	TlsSetValue(QV_Index, QV);	// store global QV as the QV for this thread
	TlsGetValue(QV_Index);

	initLisp();
	TlsSetValue(QV_Index, 0);
}

void RunCormanLisp(HANDLE* thread)
{
	RunLispThread(thread);
}

void
ProcessLispSource(char* text, long numChars)
{
	long i = 0;
	long added = 0;
	while (numChars > 0)
	{
		added = TerminalInputBuf.addChars(text + i, numChars);
		i += added;
		numChars -= added;
	}
}

//#define NTONLY 1
//#define WIN98ONLY 1

#ifdef NTONLY
__declspec(naked)
LispObj*
ThreadQV()
{
	__asm
	{
		push	ebp
		mov		ebp, esp
		mov		eax, fs:0018h			;; eax = TIB
		mov		edx, dword ptr QV_Index
		mov		eax, dword ptr [eax + edx*4 + 0e10h]
		pop		ebp
		ret
	}
}
#elif WIN98ONLY
__declspec(naked)
LispObj*
ThreadQV()
{
	__asm
	{
		push	ebp
		mov		ebp, esp
		mov		eax, fs:0018h			;; eax = TIB
		mov		eax, [eax + 2Ch]		;; eax = TLS array
		mov		edx, dword ptr QV_Index
		mov		eax, [eax + edx*4]
		pop		ebp
		ret
	}
}
#else
LispObj*
ThreadQV()
{
	return (LispObj*)TlsGetValue(QV_Index);
}
#endif

extern int lispmain();

// This is the thread procedure for the primary lisp thread
UINT LispMainProc(LPVOID /*pParam*/)
{
	LispObj dummy = 0;
	LispObj fp = 0;
	ThreadRecord* th = 0;
	DWORD currThreadID = 0;
	currThreadID = GetCurrentThreadId();

	__asm	mov  dummy, ebp

	// find our thread record
	th = ThreadList.getList();
	while (th)
	{
		if (th->threadID == currThreadID)
			break;
		th = th->next;
	}

	th->stackStart = (unsigned long*)dummy;
	TlsSetValue(Thread_Index, th);
	TlsSetValue(QV_Index, th->QV_rec);
	TlsGetValue(QV_Index);	// force NT to allocate the block
	CoInitialize(0);

	pushDynamicBinding(COMPILER_RUNTIME, NIL);

	// bind ccl:*current-thread-id*, ccl:*current-thread-handle*
	pushDynamicBinding(CURRENT_THREAD_ID, createUnsignedLispInteger(th->threadID));
	fp = foreignNode();
	UVECTOR(fp)[FOREIGN_PTR] = (LispObj)th->thread;
	pushDynamicBinding(CURRENT_THREAD_HANDLE, fp);
	fp = 0;

	lispmain();

	CoUninitialize();
	_endthreadex(0);
	return 0;
}

//	RunLispThread()
//	Starts up a new lisp thread.
static void
RunLispThread(HANDLE* thread)
{
	ThreadRecord* th = new ThreadRecord;
	auto sz = sizeof(*th);
	th->QV_rec = createNewQV();
	NumLispThreads++;
	ThreadList.insert(th);
	th->type = ThreadRecord::PrimaryThread;
	th->started = 1;

	// create the thread
	th->thread = (HANDLE)_beginthreadex(
			0,		// security
			0,		// stack size (0 = use default)
			(unsigned(__stdcall*)(void*))LispMainProc, // thread proc
			0,		// arglist
			CREATE_SUSPENDED,		// init flags
			(UINT*)&th->threadID);
	if (thread)
		*thread = th->thread;

	if (!th->thread)
	{
	//	AfxMessageBox("Could not start main lisp thread.");
		return;
	}
	SetThreadPriority(th->thread, THREAD_PRIORITY_NORMAL);
	ResumeThread(th->thread);
}

extern int lispSecondary(LispObj func);

//
// Returns the address of the top of the current thread's stack--
// since the stack grows down, this is the *highest* word of the stack
// memory region.
//
unsigned long* getStackStart()
{
	long dummy = 0;
	long* stackEnd = &dummy;
	MEMORY_BASIC_INFORMATION info;
	long ret = VirtualQuery(stackEnd, &info, sizeof(MEMORY_BASIC_INFORMATION));
	return (unsigned long*)(((char*)info.BaseAddress) + info.RegionSize - 4);
}

// This is the thread procedure for all non-primary lisp threads
// i.e. any lisp thread which is explicitly created from lisp code.
//
UINT
SecondaryThreadProc(LPVOID func)
{
	ThreadRecord* thisThread = 0;
	DWORD currThreadID = GetCurrentThreadId();
	HANDLE currThreadHandle = 0;
	LispObj fp = 0;
	LispObj dummy = 0;
	LispObj f = (LispObj)func;

	// find our thread record
	thisThread = ThreadList.getList();
	while (thisThread)
	{
		if (thisThread->threadID == currThreadID)
			break;
		thisThread = thisThread->next;
	}
	if (thisThread->threadID != currThreadID)
	{
//		AfxMessageBox("Can't find our thread ID in the active thread list");
		return 0;
	}

	__asm	mov  dummy, ebp

	thisThread->stackStart = (unsigned long*)dummy;
	TlsSetValue(Thread_Index, thisThread);
	TlsSetValue(QV_Index, thisThread->QV_rec);
	TlsGetValue(QV_Index);	// force NT to allocate the block
	thisThread->event.SetEvent();

	pushDynamicBinding(COMPILER_RUNTIME, NIL);

	// bind ccl:*current-thread-id*, ccl:*current-thread-handle*
	pushDynamicBinding(CURRENT_THREAD_ID, createUnsignedLispInteger(currThreadID));
	fp = foreignNode();
	UVECTOR(fp)[FOREIGN_PTR] = (LispObj)thisThread->thread;
	pushDynamicBinding(CURRENT_THREAD_HANDLE, fp);

	lispSecondary(f);

	popDynamicBinding(CURRENT_THREAD_ID);
	popDynamicBinding(CURRENT_THREAD_HANDLE);

	// remove the thread record
	ThreadList.remove(thisThread);
 	NumLispThreads--;

	_endthreadex(0);
	CloseHandle(currThreadHandle);

	return 0;
}

//	RunSecondaryThread()
//	Starts up a new lisp thread.
LispObj
RunSecondaryThread(LispObj func)
{
	ThreadRecord* th = new ThreadRecord;
	th->QV_rec = createNewQV();
	NumLispThreads++;
	ThreadList.insert(th);

	// make sure garbage collection cannot happen while
	// the thread is being started. This is because the passed
	// function argument may not get updated properly.
	//
	GCCriticalSection.Enter();

    th->type = ThreadRecord::SecondaryThread;
    th->started = 1;

    // create the thread
    th->thread = (HANDLE)_beginthreadex(
		    0,		// security
		    0,		// stack size (0 = use default)
		    (unsigned(__stdcall*)(void*))SecondaryThreadProc, // thread proc
		    (void*)func,	// arglist
		    CREATE_SUSPENDED,		// init flags
		    (UINT*)&th->threadID);

    if (!th->thread)
    {
	    return 0;
    }

    SetThreadPriority(th->thread, THREAD_PRIORITY_NORMAL);
    ResumeThread(th->thread);
    WaitForSingleObject(th->event.m_hObject, INFINITE); // timeout after 5 seconds
    th->event.ResetEvent();

	GCCriticalSection.Leave();	// don't enable GC until the thread
	return createUnsignedLispInteger(th->threadID);
}

//
// Call this function to allow a non-lisp thread to become a lisp
// thread. That means it may call lisp functions (defined via defun-callback)
// directly, and the garbage collector will check its stack for references
// into the lisp heap.
//
// After being blessed, it will exist in the LispThreadQueue, and it
// will be considered to be executing foreign code.
//
// This thread will remain in the set of lisp threads until the process
// terminates, unless it explicitly removes itself by calling UnblessThread().
//
extern "C" __declspec(dllexport) void BlessThread()
{
	// If Lisp is initializing, wait for initialization to finish.
	// It will timeout after 10 seconds.
	WaitForSingleObject(InitializationEvent, INFINITE);

	GCCriticalSection.Enter();	// make sure garbage collection is not running
//    __try
//    {
	    if (TlsGetValue(QV_Index) == 0)
	    {
		    ThreadRecord* th = new ThreadRecord;
		    LispObj fp = 0;
		    HANDLE threadHandle = 0;
		    BOOL ret = FALSE;

		    th->QV_rec = createNewQV();
		    th->stackStart = getStackStart();
		    th->threadID = GetCurrentThreadId();
		    ret = DuplicateHandle(
			    GetCurrentProcess(),
			    GetCurrentThread(),
			    GetCurrentProcess(),
			    &threadHandle,
			    0,
			    TRUE,
			    DUPLICATE_SAME_ACCESS);

		    th->thread = threadHandle;

		    //th->thread = OpenThread(THREAD_ALL_ACCESS, FALSE, th->threadID);
		    th->type = ThreadRecord::BlessedThread;
		    th->started = true;
    		
		    NumLispThreads++;
		    ThreadList.insert(th);
		    TlsSetValue(Thread_Index, th);
		    TlsSetValue(QV_Index, th->QV_rec);
		    TlsGetValue(QV_Index);	// force NT to allocate the block
		    pushDynamicBinding(COMPILER_RUNTIME, NIL);

		    // bind ccl:*current-thread-id*, ccl:*current-thread-handle*
		    pushDynamicBinding(CURRENT_THREAD_ID, createUnsignedLispInteger(GetCurrentThreadId()));
		    fp = foreignNode();
		    UVECTOR(fp)[FOREIGN_PTR] = (LispObj)th->thread;
		    pushDynamicBinding(CURRENT_THREAD_HANDLE, fp);

			// set marker for foreign code transition
			th->QV_rec[STACK_MARKER_INDEX_Index] = 4;	// start at 1st transition (foreign code)
		    th->QV_rec[STACK_MARKERS_Index] = (LispObj)(th->stackStart);
		    th->QV_rec[STACK_MARKERS_Index + 1] = 0;

	    }
//    }
//    __finally
//    {
	    GCCriticalSection.Leave();
//    }
}

// 
// The reverse operation of BlessThread.
// Removes the thread from the lisp thread queue, and it may no
// longer be used to call directly into lisp function.
// It will know longer be checked by the garbage collector for 
// references. It generally shouldn't be necessary to call this function
// unless an application is creating and destroying threads. It should
// be called prior to any "blessed" thread being terminated by the
// application. If it is not called, the open thread handle will prevent
// the thread from being recycled, and cause the garbage collector to
// continue to scan it for references.
//
extern "C" __declspec(dllexport) void UnblessThread()
{
	GCCriticalSection.Enter();	// make sure garbage collection is not running
	ThreadRecord* th = (ThreadRecord*)TlsGetValue(Thread_Index);
	TlsSetValue(Thread_Index, 0);
	TlsSetValue(QV_Index, 0);
    __try
    {
	    if (th)
	    {
		    popDynamicBinding(CURRENT_THREAD_ID);
		    popDynamicBinding(CURRENT_THREAD_HANDLE);
		    CloseHandle(th->thread);

		    // remove the thread record--this also deletes the ThreadRecord
		    ThreadList.remove(th);
		    th = 0;
 		    NumLispThreads--;
	    }
    }
    __finally
    {
	    GCCriticalSection.Leave();	// make sure garbage collection is not running
    }
}

//
// Returns a direct callback function pointer to be called
// by foreign code (from a blessed thread).
//
extern "C" __declspec(dllexport) 
void* GetCallbackFunctionPointer(wchar_t* functionName, wchar_t* package)
{
	LispObj s = 0;
	LispObj sym = 0;
	LispObj ret = 0;

	// If Lisp is initializing, wait for initialization to finish.
	// It will timeout after 10 seconds.
	WaitForSingleObject(InitializationEvent, INFINITE);

	// need to wrap this so page faults get handled during memory
	// allocation
	__try
	{
		FOREIGN_TO_LISP()

		s = wstringNode(functionName);
		if (!package)
			sym = s;	// just use string
		else
		{
			// if INTERN is not defined, probably because the lisp image did not
			// load correctly, just return 0 now
			if (!isFunction(symbolFunction(INTERN)))
				return 0;
			sym = LispCall4(Funcall, FUNCALL, INTERN, s, wstringNode(package));
		}
		ret = LispCall3(Funcall, FUNCALL, GET_CALLBACK, sym);

		LISP_RETURN_TO_FOREIGN()
	}
	__except (handleStructuredException(GetExceptionCode(), GetExceptionInformation()))
	{
	}

	if (isForeignHeapPtr(ret) || isForeign(ret))
		return (void*)foreignPtr(ret);
	else
		return 0;
}

long GetNumLispThreads()
{
	return NumLispThreads;
}

void ThrowUserException()
{
	LispCall2(Funcall, THROW_SYSTEM_EXCEPTION, EX_USER_ABORT);
}

long user_abort = 0;
static CONTEXT lispContext;

// This function is used by AbortLispThread() to force
// a call to the ThrowUserException() function.
// This simulates a call from the original function,
void __declspec(naked) CallThrowUserExceptionStub()
{
	__asm push eax  ;; push return address
	__asm jmp ThrowUserException;
}

// Aborts the primary thread
void AbortLispThread()
{
	unsigned long addr = 0;
	ThreadRecord* tr = 0;
	HANDLE th = 0;

	// make sure we don't abort a thread while it is performing a garbage collection!
	GCCriticalSection.Enter();
    __try
    {
	    tr = ThreadList.getPrimaryThread();
	    if (!tr)
		    return;
	    th = tr->thread;
	    user_abort = 1;
	    SuspendThread(th);
	    ThreadList.ensureSafeState(tr);
	    lispContext.ContextFlags = CONTEXT_FULL;
	    GetThreadContext(th, &lispContext);
	    if (!((tr->QV_rec[STACK_MARKER_INDEX_Index] >> 2) & 1))
	    {
		    lispContext.Eax = lispContext.Eip;
		    lispContext.Eip = (long)CallThrowUserExceptionStub;
		    lispContext.ContextFlags = CONTEXT_FULL;
		    SetThreadContext(th, &lispContext);
	    }
	    else
	    {
		    // cannot abort--executing in foreign code
	    }
	    ResumeThread(th);
    }
    __finally
    {
	    GCCriticalSection.Leave();
    }
}

static CONTEXT terminateContext;

void __declspec(naked) TerminateLispThreadException()
{
	LispObj result;
	__asm mov result, edi

	LispCall4(Funcall, THROW_EXCEPTION, EXIT_THREAD_TAG, result, wrapInteger(1));
}

void TerminateLispThread(LispObj threadID, LispObj ret)
{
	HANDLE h = ThreadList.GetLispThreadHandle(lispIntegerToUnsignedLong(threadID));
	if (h)
	{
		SuspendThread(h);
		terminateContext.ContextFlags = CONTEXT_FULL;
		GetThreadContext(h, &terminateContext);
		terminateContext.Eip = (long)TerminateLispThreadException;
		terminateContext.Edi = (long)ret;
		terminateContext.ContextFlags = CONTEXT_FULL;
		SetThreadContext(h, &terminateContext);
		ResumeThread(h);
	}
	else
		Error("Not a valid lisp thread ID: ~A", threadID);
}