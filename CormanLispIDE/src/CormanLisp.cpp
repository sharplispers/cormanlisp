//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		CormanLisp.cpp
//		Contents:	Corman Lisp application source file
//		History:	7/25/96  RGC  Created.
//					7/01/01  RGC  Force mouse cue off when scrolling.
//

#include "stdafx.h"
#include "afxmt.h"
#include <process.h>
#include <objbase.h>
#include <initguid.h>
#include <math.h>
#include <winbase.h>

#include "Dialogs.h"
#include "CormanLisp.h"
#include "LispObjDisplay.h"
#include "ErrorMessage.h"
#include "CoCormanLispClient.h"
#include "PageSetupDlg.h"
#include "browser.h"
#include "UtilFuncs.h"
#include "CommonLispFuncs.h"
#include "PreferencesDialog.h"

#ifndef WM_MENUCOMMAND
#define WM_MENUCOMMAND 0x0126
#endif
#ifndef WM_UNINITMENUPOPUP
#define WM_UNINITMENUPOPUP 0x0125
#endif

CCormanLispApp theApp;
CLispView* CurrentView = 0;
int Windows__NT = 0;
ICormanLisp* pCormanLisp = 0;
ICormanLispDirectCall* pCormanLispDirectCall = 0;
static int readingFromConsole = false;

const char WORKSHEET_TITLE[] = "Lisp Worksheet";
const char SERVER_TITLE[] = "CormanLispServer.dll";
const int CCormanLispApp::m_nPrimaryNumUnits = 4;
const int CCormanLispApp::m_nNumUnits = 7;
char LispWorksheetPath[_MAX_PATH + 1 + sizeof(WORKSHEET_TITLE)];
char LispServerPath[_MAX_PATH + 1 + sizeof(SERVER_TITLE)];

const int REGDB_E_MISSINGLIB = 0x80070485;

static CPoint gLastPoint(0, 0);
static unsigned long gStartTimeAtLastPoint = 0;
static unsigned long gLastVariableUpdateTime = 0;
static unsigned long gVariableUpdateInterval = 1000;
static long gLastKeyPressTime = 0;
static CLispView* gView = 0;

const int WaitTimeBeforeMouseCue = 300;		// 300 ms wait time
const int WaitTimeBeforeColorize = 500;		// 500 ms wait time

HFONT EzCreateFont (HDC hdc, char * szFaceName, int iDeciPtHeight,
                    int iDeciPtWidth, int iAttributes, BOOL fLogRes);

typedef LONG (__stdcall * EDITWORDBREAKPROCEX__)(char *pchText, LONG cchText, BYTE bCharSet, INT action);
static  LONG __stdcall wordBreakProcEx(LPTSTR pchText, LONG cchText, BYTE bCharSet, int code);
static void delay(int ms);

static EDITWORDBREAKPROCEX__ defaultWordBreakProcEx = 0;

CUnit CCormanLispApp::m_units[7] =
{
//	TPU, 	SmallDiv,	MedDiv,	LargeDiv,	MinMove,	szAbbrev,			bSpace
CUnit(1440,	180,		720,	1440,		90,			IDS_INCH1_ABBREV,	FALSE),//inches
CUnit(568,	142,		284,	568,		142,		IDS_CM_ABBREV,		TRUE),//centimeters
CUnit(20,	120,		720,	720,		100,		IDS_POINT_ABBREV,	TRUE),//points
CUnit(240,	240,		1440,	1440,		120,		IDS_PICA_ABBREV,	TRUE),//picas
CUnit(1440,	180,		720,	1440,		90,			IDS_INCH2_ABBREV,	FALSE),//in
CUnit(1440,	180,		720,	1440,		90,			IDS_INCH3_ABBREV,	FALSE),//inch
CUnit(1440,	180,		720,	1440,		90,			IDS_INCH4_ABBREV,	FALSE)//inches
};

int CCormanLispApp::m_nPrinterChangedMsg = RegisterWindowMessage(_T("CormanLispPrinterChanged"));

CharBuf TerminalOutputBuf(0x8000);
CMultiDocTemplate* textDocTemplate = 0;
CMultiDocTemplate* lispSourceTemplate1 = 0;
CMultiDocTemplate* lispSourceTemplate2 = 0;

long getTextRange(CRichEditCtrl& ed, char* buf, long start, long end);
VOID CALLBACK TimerProc( HWND hwnd, UINT uMsg, UINT idEvent, DWORD dwTime);
static HINSTANCE getLocalCormanLispServer();
static IClassFactory* getCormanLispClassFactory();
static IClassFactory* getCormanLispRegisteredClassFactory();
static void AppendFilterSuffix(CString&, OPENFILENAME&, CDocTemplate*);
static void SetupDirectCallPointers();

static int maxFontSize = 73;

CFont** CCormanLispApp::defaultFont = 0;
CFont** CCormanLispApp::defaultUnderlineFont = 0;
CFont** CCormanLispApp::courierFont = 0;

CDialog* CCormanLispApp::splashScreen = 0;
DWORD CCormanLispApp::startupTime = 0;
DWORD CCormanLispApp::splashDisplayTimeMillis = 2 * 1000;	// 2 seconds

UINT gLastMenuItem;
HMENU gLastMenuHandle;

// Direct Lisp Call pointers
typedef long (*OnContextMenuFuncType)(long, long, void*);
OnContextMenuFuncType OnContextMenuFuncPtr = 0;
typedef long (*OnHeapSizeType)(long, long*, long*);
OnHeapSizeType OnHeapSizePtr = 0;
typedef int (*LookupLambdaListType)(char* sym, char* buf, int bufLength);
LookupLambdaListType LookupLambdaListPtr = 0;
typedef long (*OnColorizeType)(HWND hwnd, long start, long end);
OnColorizeType OnColorizeFuncPtr = 0;
typedef int (*DisplayLispVarsType)(char* buf, int bufLength);
DisplayLispVarsType DisplayLispVarsPtr = 0;
typedef int (*InitMenuType)(HMENU hmenu);
InitMenuType InitMenuPtr = 0;
typedef int (*InitMenuPopupType)(HMENU hmenu, UINT uint, BOOL b);
InitMenuPopupType InitMenuPopupPtr = 0;
typedef void (*UninitMenuPopupType)(HMENU hmenu);
UninitMenuPopupType UninitMenuPopupPtr = 0;
typedef void (*MenuSelectType)(HMENU hMenu, UINT nItemID, UINT nFlags);
MenuSelectType MenuSelectPtr = 0;
typedef void (*VersionCaptionType)(char* buf);
VersionCaptionType VersionCaptionPtr = 0;

static char lispVarsBuf[4096] = {0};
static char saveLispVarsBuf[4096] = {0};

bool OpeningWorksheet = false;

BEGIN_MESSAGE_MAP(CCormanLispApp, CWinApp)
	//{{AFX_MSG_MAP(CCormanLispApp)
	ON_COMMAND(ID_APP_ABOUT, OnAppAbout)
	//}}AFX_MSG_MAP
	ON_COMMAND(ID_FILE_NEW, OnFileNew)     // file commands...
	ON_COMMAND(ID_FILE_OPEN, OnFileOpen)
	ON_COMMAND(ID_FILE_PRINT_SETUP, CWinApp::OnFilePrintSetup)
	ON_COMMAND(ID_EXECUTE_FILE, OnExecuteFile)
	ON_COMMAND(ID_HELP_CORMANLISPCOM, OnCormanLispCom)
	ON_COMMAND(ID_HELP_CCL_DOC, OnBrowseCCLDoc)
	ON_COMMAND(ID_HELP_LICENSEAGREEMENT, OnLicenseAgreement)
	ON_COMMAND(ID_HELP_CREDITS, OnCredits)
	ON_COMMAND(ID_PREFERENCES, OnEditPreferences)
	ON_COMMAND(ID_REGISTRATION, checkRegistration)
	ON_COMMAND(ID_WINDOW_CLOSEALLDOCUMENTS, OnCloseAll)
END_MESSAGE_MAP()

CCormanLispApp::CCormanLispApp()
	: m_pConnectionPoint(0), m_CormanLispClient(0), m_appIsClosing(false),
	  m_worksheet(0), m_nUnits(0), m_lastDocOpened(0), m_explorer(0), m_browser(0),
	  m_browserDoc(0), m_event(0), m_closeSplash(FALSE)
{
	m_fileToOpen[0] = 0;
	m_urlToOpen[0] = 0;
	m_timer = 0;
	sortLambdaLists();
	m_defaultDirectory = "";
	m_defaultFilterIndex = 1;
	m_defaultExecDirectory = "";
	m_defaultExecFilterIndex = 1;
	m_lastExpiredScreenTime = 0;
	m_inRegistrationDialog = FALSE;
	m_replaceSelection = "";
	m_isActive = TRUE;

	defaultFont = new CFont*[maxFontSize];
	defaultUnderlineFont = new CFont*[maxFontSize];
	courierFont = new CFont*[maxFontSize];
	for (int i = 0; i < maxFontSize; i++)
		defaultFont[i] = defaultUnderlineFont[i] = courierFont[i] = 0;
}

CCormanLispApp::~CCormanLispApp()
{
	if (m_explorer)
		m_explorer->Release();
	// release fonts?
}

static char lispImageName[MAX_PATH];
typedef void (WINAPI *LOADLIBRARYFUNC)();
typedef HRESULT (WINAPI *GETCLASSOBJECTFUNC)(REFCLSID rclsid, REFIID riid, void**ppv);

static bool isWindowsNTRunning()
{
	OSVERSIONINFO OsVersionInfo;
	OsVersionInfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	BOOL ret = GetVersionEx(&OsVersionInfo);
	return (OsVersionInfo.dwPlatformId == VER_PLATFORM_WIN32_NT);
}

CDocument* CCormanLispApp::openWorksheet()
{
	DWORD chars = GetModuleFileName(0, LispWorksheetPath, sizeof(LispWorksheetPath));
	int index = chars - 1;
	while (index >= 0 && LispWorksheetPath[index] != '\\')
		index--;
	LispWorksheetPath[index] = 0;	// get rid of file name, just leave the path
	if (chars > 0)
		strcat_s(LispWorksheetPath, sizeof(LispWorksheetPath), "\\");
	strcat_s(LispWorksheetPath, sizeof(LispWorksheetPath), WORKSHEET_TITLE);
	HANDLE file = CreateFile(LispWorksheetPath, GENERIC_READ,
							0, 0, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
	CloseHandle(file);	// we just wanted to make sure it exists

	OpeningWorksheet = true;
	OpenDocumentFile(LispWorksheetPath);
	OpeningWorksheet = false;
	CDocument* doc = FindDocumentByTitle(WORKSHEET_TITLE);

	// disable worksheet close box
	POSITION pos = doc->GetFirstViewPosition();
	CView* view = doc->GetNextView(pos);
	CWnd* frame = view->GetParentFrame();
	if (frame)
	{
		CMenu* menu = frame->GetSystemMenu(FALSE);
		CString menuString;
		menu->GetMenuString(SC_CLOSE, menuString, MF_BYCOMMAND);
		menu->ModifyMenu(SC_CLOSE, MF_BYCOMMAND | MF_GRAYED, SC_CLOSE, menuString);
		DrawMenuBar(frame->m_hWnd);
	}

	return doc;
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

BOOL CCormanLispApp::InitInstance()
{
	BOOL ret = FALSE;
	CString lispImage;
	defaultMessageText = "Ready  (Use <Shift>-Enter to Execute)";
	//Enable3dControls();
	AfxEnableControlContainer();
	LoadStdProfileSettings();
	CoInitialize(0);

	Windows__NT = isWindowsNTRunning();

	AddDocTemplate(textDocTemplate=new CMultiDocTemplate(IDR_TEXTTYPE,
		RUNTIME_CLASS(CLispDoc), RUNTIME_CLASS(CLispDocumentFrame),
		RUNTIME_CLASS(CLispView)));

	AddDocTemplate(lispSourceTemplate1=new CMultiDocTemplate(IDR_LISPSOURCETYPE1,
		RUNTIME_CLASS(CLispDoc), RUNTIME_CLASS(CLispDocumentFrame),
		RUNTIME_CLASS(CLispView)));

	AddDocTemplate(lispSourceTemplate2=new CMultiDocTemplate(IDR_LISPSOURCETYPE2,
		RUNTIME_CLASS(CLispDoc), RUNTIME_CLASS(CLispDocumentFrame),
		RUNTIME_CLASS(CLispView)));

	AddDocTemplate(new CMultiDocTemplate(IDR_HTMLTYPE,
		RUNTIME_CLASS(CBrowserDoc), RUNTIME_CLASS(CBrowserFrame),
		RUNTIME_CLASS(CBrowserView)));

	AddDocTemplate(new CMultiDocTemplate(IDR_HTMLTYPE2,
		RUNTIME_CLASS(CBrowserDoc), RUNTIME_CLASS(CBrowserFrame),
		RUNTIME_CLASS(CBrowserView)));

	m_pMainWnd = new CMainFrame;
	((CFrameWnd*)m_pMainWnd)->LoadFrame(IDR_MAINFRAME);
	m_pMainWnd->ShowWindow(m_nCmdShow);

	checkRegistration();

	theApp.splashScreen = new CDialog();
	theApp.splashScreen->Create(IDD_SPLASH);
	theApp.startupTime = GetTickCount();

	// enable file manager drag/drop and DDE Execute open
	m_pMainWnd->DragAcceptFiles();
	EnableShellOpen();

	//RegisterShellFileTypes(FALSE);

	// Parse command line for standard shell commands, DDE, file open
	CCommandLineInfo cmdInfo;
	cmdInfo.m_nShellCommand	= CCommandLineInfo::FileNothing;
	ParseCommandLine(cmdInfo);
	PreferencesDialog::GetPreferencesInfo(&preferences);
	if (!cmdInfo.m_strFileName.IsEmpty() &&
			cmdInfo.m_strFileName.Right(4).CompareNoCase(".IMG") == 0)
	{
		lispImage = cmdInfo.m_strFileName;
	}
	else
	if (!cmdInfo.m_strFileName.IsEmpty())
	{
		char buf[_MAX_PATH + 1];
		char* p;
		DWORD ret = GetFullPathName(cmdInfo.m_strFileName, sizeof(buf), buf, &p);
		OpenDocumentFile(buf);
	}

	if (theApp.preferences.rememberOpenDocuments)
	{
		// open any docs which were open before
		CString openDocs = AfxGetApp()->GetProfileString(_TEXT("CormanLisp"), _TEXT("OpenDocs"));
		if (openDocs.GetLength() > 0)
		{
			const char* s = openDocs.GetBuffer();
			CString path;
			while (*s)
			{
				path = "";
				while (*s == ' ')
					s++;
				if (*s && *s == '"')
					s++;
				while (*s && *s != '"')
					path += *s++;
				if (*s && *s == '"')
					s++;

				if (path.GetLength() > 0)
					OpenDocumentFile(path);
			}
		}
	}

	// Dispatch commands specified on the command line
	//	if (!ProcessShellCommand(cmdInfo))
	//		return FALSE;

	m_worksheet = openWorksheet();
	ASSERT(m_worksheet);

	//  Get the CormanLisp class factory
	IClassFactory* pcf = getCormanLispClassFactory();
	IUnknown* pUnk = 0;
    HRESULT hr = pcf->CreateInstance(0, IID_IUnknown, (void**)&pUnk);
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

	// get the Direct Call interface
	hr = pUnk->QueryInterface(IID_ICormanLispDirectCall, (void**)&pCormanLispDirectCall);
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
		&m_pConnectionPoint);
	if (FAILED(hr))
	{
		pConnectionPointContainer->Release();
		ErrorMessage(__TEXT("FindConnectionPoint() did not return IID_ICormanLispStatusMessage"),
			hr);
		return FALSE;
	}

	hr = pConnectionPointContainer->FindConnectionPoint(IID_ICormanLispShutdown,
		&m_pShutdownConnectionPoint);
	pConnectionPointContainer->Release();
	if (FAILED(hr))
	{
		ErrorMessage(__TEXT("FindConnectionPoint() did not return IID_ICormanLispShutdown"),
			hr);
		return FALSE;
	}

	// make an instance of our outgoing class interface
	m_CormanLispClient = new CoCormanLispClient;
	m_CormanLispClient->AddRef();

	// make an instance of our outgoing shutdown class interface
	m_CormanLispShutdownClient = new CoCormanLispShutdownClient;
	m_CormanLispShutdownClient->AddRef();

	hr = m_CormanLispClient->Connect(m_pConnectionPoint);
	if (FAILED(hr))
	{
		m_CormanLispClient->Release();
		m_pConnectionPoint->Release();
		AfxMessageBox("Connection failed");
	}

	hr = m_CormanLispShutdownClient->Connect(m_pShutdownConnectionPoint);
	if (FAILED(hr))
	{
		m_CormanLispShutdownClient->Release();
		m_pShutdownConnectionPoint->Release();
		AfxMessageBox("Connection failed");
	}

	if (lispImage.GetLength() == 0)
	{
		ret = GetModuleFileName(0, lispImageName, sizeof lispImageName);
		int len = strlen(lispImageName);
		lispImageName[len - 3] = 0;
		strcat_s(lispImageName, sizeof(lispImageName), "IMG");
		lispImage = lispImageName;
	}
	m_event = CreateEvent(NULL, TRUE, FALSE, "CormanLisp Timer 1");
	m_timer = SetTimer(0, 1, WaitTimeBeforeMouseCue, TimerProc);
	m_blackBrush = (HBRUSH)GetStockObject(BLACK_BRUSH);

	pCormanLisp->Initialize(m_CormanLispClient, lispImage, IDE_CLIENT);
	HANDLE thread = 0;
	pCormanLisp->Run(&thread);

/*
	CString s;
	s.Format("(progn (setq ccl::*controller-thread-id* %u) (values))\r\n", GetCurrentThreadId());
	if (pCormanLisp)
		pCormanLisp->ProcessSource((char*)(const char*)s, s.GetLength());
*/
	SetDefaultMessage();
	//delay(5000);		// delay 3 second
	return TRUE;
}

BOOL CCormanLispApp::ExitInstance()
{
	KillTimer(0, m_timer);
	if (pCormanLisp)
		pCormanLisp->Release();
//	if (pCormanLispDirectCall)
//		pCormanLispDirectCall->Release();
	pCormanLisp = 0;
	pCormanLispDirectCall = 0;
	CoUninitialize();
	return 0;
}

void CCormanLispApp::OnFileNew()
{
	POSITION p = GetFirstDocTemplatePosition();
	CDocTemplate* pTemplate = GetNextDocTemplate(p);
	ASSERT(pTemplate != NULL);
	ASSERT_KINDOF(CDocTemplate, pTemplate);
	pTemplate->OpenDocumentFile(NULL);
}

void CCormanLispApp::checkRegistration()
{
	// avoid recursion
	if (m_inRegistrationDialog)
		return;

	return;
}

void CCormanLispApp::OnTimer(UINT)
{
	long time = GetTickCount();
	if (CurrentView && (gView == CurrentView)
			&& theApp.preferences.autoPrototypeOnMouseMove &&
		((time - gStartTimeAtLastPoint) > WaitTimeBeforeMouseCue))
	{
		if (!gView->usingKeyboard() && !gView->mouseCueDisabled())
		{
			CPoint p;
			p.x = max(gLastPoint.x - 8, 0);
			p.y = gLastPoint.y;
			gView->mouseCueOn(p);
			gView->displayMouseCue();
		}
	}
	if (CurrentView && (gView == CurrentView)
			&& theApp.preferences.autoColorize &&
		((time - gLastKeyPressTime) > WaitTimeBeforeColorize) && !gView->m_colorizeDisabled)
		gView->Colorize();
}

void invalidateHeapDisplay(long generation, CDialogBar* dialogBar, int index)
{
	RECT rect;
	CWnd* control = dialogBar->GetDlgItem(index);
	control->GetClientRect(&rect);
	rect.left += 1; rect.top += 1; rect.right -= 1; rect.bottom -= 1;
	control->InvalidateRect(&rect);
}

VOID CALLBACK TimerProc( HWND hwnd, UINT uMsg, UINT idEvent, DWORD dwTime)
{
	unsigned long time = GetTickCount();
	if (theApp.m_expired &&
		(time - theApp.m_lastExpiredScreenTime) > (30 * 60 * 1000))	// thirty minutes
	{
		if (theApp.m_lastExpiredScreenTime == 0)	// first time
		{
			theApp.m_lastExpiredScreenTime = time;
		}
		else
		{
			theApp.m_lastExpiredScreenTime = time;
			theApp.m_pMainWnd->PostMessage(WM_COMMAND, ID_REGISTRATION);
		}
	}

	if (!theApp.m_isActive)
		return;

	if (CurrentView && (gView == CurrentView)
			&& theApp.preferences.autoPrototypeOnMouseMove &&
		((time - gStartTimeAtLastPoint) > WaitTimeBeforeMouseCue))
	{
		if (!gView->usingKeyboard() && !gView->mouseCueDisabled())
		{
			CPoint p;
			p.x = max(gLastPoint.x - 5, 0);
			p.y = gLastPoint.y;
			gView->mouseCueOn(p);
			gView->displayMouseCue();
		}
	}
	if (CurrentView && (gView == CurrentView)
			&& theApp.preferences.autoColorize &&
		((time - gLastKeyPressTime) > WaitTimeBeforeColorize) && !gView->m_colorizeDisabled)
		gView->Colorize();

	// repaint lisp status dialog bar
	if (!OnHeapSizePtr)
		SetupDirectCallPointers();
	if (OnHeapSizePtr)
	{
		// don't invalidate unless they have changed
		CMainFrame* mainWnd = (CMainFrame*)theApp.m_pMainWnd;
		CDialogBar* lispStatus = &mainWnd->m_lispStatusDialogBar;
		static long lastPercent_0 = -1;
		static long lastPercent_1 = -1;
		static long lastPercent_2 = -1;
		long capacity, used, percent;
		percent = OnHeapSizePtr(0, &capacity, &used);
		if (percent != lastPercent_0)
		{
			invalidateHeapDisplay(0, lispStatus, IDC_LISPHEAP_0);
			lastPercent_0 = percent;
		}
		percent = OnHeapSizePtr(1, &capacity, &used);
		if (percent != lastPercent_1)
		{
			invalidateHeapDisplay(1, lispStatus, IDC_LISPHEAP_1);
			lastPercent_1 = percent;
		}
		percent = OnHeapSizePtr(2, &capacity, &used);
		if (percent != lastPercent_2)
		{
			invalidateHeapDisplay(2, lispStatus, IDC_LISPHEAP_2);
			lastPercent_2 = percent;
		}

        // do lisp variable display
        if (DisplayLispVarsPtr &&
            mainWnd->m_lispVarsDialogBar.IsVisible() &&
            mainWnd->m_lispVarsDialogBar.IsWindowEnabled() &&
            DisplayLispVarsPtr && (time - gLastVariableUpdateTime) > gVariableUpdateInterval)
        {
            gLastVariableUpdateTime = time;

		    // don't invalidate unless they have changed
		    CMainFrame* mainWnd = (CMainFrame*)theApp.m_pMainWnd;
		    CDialogBar* lispVars = &mainWnd->m_lispVarsDialogBar;
            int ret = DisplayLispVarsPtr(lispVarsBuf, sizeof(lispVarsBuf));
            if (strcmp(saveLispVarsBuf, lispVarsBuf))
            {
                strcpy_s(saveLispVarsBuf, sizeof(saveLispVarsBuf), lispVarsBuf);
                RECT rect;
                lispVars->GetClientRect(&rect);
                lispVars->Invalidate();
            }
        }
	}
}

BOOL CCormanLispApp::OnIdle(LONG lCount)
{
	if (theApp.m_worksheet && TerminalOutputBuf.numchars() > 0)
	{
		char* str = TerminalOutputBuf.getChars();
		CString s(str);
		delete [] str;

		POSITION pos = theApp.m_worksheet->GetFirstViewPosition();
		if (pos)
		{
			CLispView* view =
				(CLispView*)theApp.m_worksheet->GetNextView(pos);
 			view->outputText(s);
		}
	}

	if (m_fileToOpen[0])
	{
		m_lastDocOpened = OpenDocumentFile(m_fileToOpen);
		m_fileToOpen[0] = 0;
	}
	if (m_urlToOpen[0])
	{
		NavigateURL(m_urlToOpen);
		m_urlToOpen[0] = 0;
	}
	if (!m_replaceSelection.IsEmpty())
	{
		if (CurrentView)
 			CurrentView->replaceSelection(m_replaceSelection);
		m_replaceSelection = "";
	}

	// close splash screen if it is still up and startup time has
	// elapsed
	if (splashScreen &&
		(m_closeSplash ||
			((GetTickCount() - startupTime) > splashDisplayTimeMillis)))
	{
		delete splashScreen;
		splashScreen = 0;
	}

	return CWinApp::OnIdle(lCount);
}

bool CCormanLispApp::SetDocumentToOpen(const char* file)
{
	strcpy_s(m_fileToOpen, sizeof(m_fileToOpen), file);

	// trigger the idle processing
	BOOL ret = PostMessage(GetMainWnd()->m_hWnd, WM_ENTERIDLE, 0, 0);
	return true;
}

bool CCormanLispApp::SetURLToOpen(const char* file)
{
	strcpy_s(m_urlToOpen, sizeof(m_urlToOpen), file);

	// trigger the idle processing
	BOOL ret = PostMessage(GetMainWnd()->m_hWnd, WM_ENTERIDLE, 0, 0);
	return true;
}

bool CCormanLispApp::SetReplaceSelection(const char* text, int numChars)
{
	m_replaceSelection = CString(text, numChars);

	// trigger the idle processing
	BOOL ret = PostMessage(GetMainWnd()->m_hWnd, WM_ENTERIDLE, 0, 0);
	return true;
}

bool CCormanLispApp::waitingForDocumentToOpen()
{
	if (!(m_fileToOpen[0] | m_urlToOpen[0]))
		return false;
	delay(100);		// delay 100 ms
	return (m_fileToOpen[0] | m_urlToOpen[0]) ? true : false;
}

void CCormanLispApp::CloseAllDocuments(BOOL bEndSession)
{
	m_appIsClosing = bEndSession ? true : false;
//	CWinApp::CloseAllDocuments(bEndSession);

	POSITION pos = m_pDocManager->GetFirstDocTemplatePosition();
	while (pos != NULL)
	{
		CDocTemplate* pTemplate = m_pDocManager->GetNextDocTemplate(pos);
		POSITION p = pTemplate->GetFirstDocPosition();
		while (p != NULL)
		{
			CDocument* doc = pTemplate->GetNextDoc(p);
			// don't close lisp worksheet here--it will be loaded automatically
			POSITION vpos = doc->GetFirstViewPosition();
			if (vpos != NULL)
			{
				if (doc->GetTitle().CompareNoCase(WORKSHEET_TITLE))
				    doc->OnCmdMsg(ID_FILE_CLOSE, 0, 0, 0);
			}
		}
	}
}

bool CCormanLispApp::AppIsClosing()
{
	return m_appIsClosing;
}

void CCormanLispApp::AppIsClosing(bool f)
{
	m_appIsClosing = f;
}

BOOL CCormanLispApp::OnDDECommand(LPTSTR lpszCommand)
{
	if (CWinApp::OnDDECommand(lpszCommand))
      return TRUE;

   // Handle any DDE commands recognized by your application
   // and return TRUE.  See implementation of CWinApp::OnDDEComand
   // for example of parsing the DDE command string.

   // Return FALSE for any DDE commands you do not handle.
   return FALSE;
 }

static CFont* createFixedFont(long size, HDC hDC, bool underlined)
{
	CFont* f = new CFont;
	if (size == 0)
		size = 10;
	int height = -MulDiv(size, GetDeviceCaps(hDC, LOGPIXELSY), 72);
 	int width = 0;

	f->CreateFont(
			height,				// height
			width,				// width
			0,					// rotation (escapement)
			0,					// orientation
			theApp.preferences.charBold ? FW_BOLD : FW_NORMAL, // weight
			theApp.preferences.charItalic,	// italic
			underlined ? 1 : 0,	// underline?
			0,					// strikeout = NO
			ANSI_CHARSET,		// charset = ANSI
			OUT_DEFAULT_PRECIS,	// precision
			CLIP_DEFAULT_PRECIS,// clip precision
			DEFAULT_QUALITY,	// quality
			FIXED_PITCH,		// pitch
			theApp.preferences.fontName);			// family
	return f;
}

static CFont* createFixedCourierFont(long size, HDC hDC, bool underlined)
{
	CFont* f = new CFont;
	if (size == 0)
		size = 10;
	int height = -MulDiv(size, GetDeviceCaps(hDC, LOGPIXELSY), 72);
 	int width = 0;

	f->CreateFont(
			height,				// height
			width,				// width
			0,					// rotation (escapement)
			0,					// orientation
			FW_NORMAL,			// weight
			0,					// italic = NO
			underlined ? 1 : 0,	// underline?
			0,					// strikeout = NO
			ANSI_CHARSET,		// charset = ANSI
			OUT_DEFAULT_PRECIS,	// precision
			CLIP_DEFAULT_PRECIS,// clip precision
			DEFAULT_QUALITY,	// quality
			FIXED_PITCH,		// pitch
			"Courier New");			// family
	return f;
}

CFont* CCormanLispApp::getDefaultFont(HDC hDC, long pointSize)
{
	if (pointSize > maxFontSize)
		pointSize = 10;
	if (!defaultFont[pointSize])
		defaultFont[pointSize] = createFixedFont(pointSize, hDC, false);
	return defaultFont[pointSize];
}

CFont* CCormanLispApp::getCourierFont(HDC hDC, long pointSize)
{
	if (pointSize > maxFontSize)
		pointSize = 10;
	if (!courierFont[pointSize])
		courierFont[pointSize] = createFixedCourierFont(pointSize, hDC, false);
	return courierFont[pointSize];
}

CFont* CCormanLispApp::getDefaultUnderlineFont(HDC hDC, long pointSize)
{
	if (pointSize > maxFontSize)
		pointSize = 10;
	if (!defaultUnderlineFont[pointSize])
		defaultUnderlineFont[pointSize] = createFixedFont(pointSize, hDC, true);
	return defaultUnderlineFont[pointSize];
}

//
//	Convert the line feeds in a C string from '\n' to ascii 13/ascii 10 pairs.
//	This function leaves the original alone, but returns a new string
//	with the line feeds converted.
//
CString expandLineFeeds(const CString& s)
{
	int count = 0;
	const char* savep = 0;
	const char* p = 0;

	p = savep = (const char*)s;
	while (*p)
	{
		if (*p == 10)
		{
			if (p == savep || *(p - 1) != 13)
				count++;
		}
		p++;
	}

	int newLength = s.GetLength() + count;
	CString result(' ', newLength);

	p = (const char*)s;
	for (int i = 0; i < newLength; i++)
	{
		if (*p == '\n')
		{
			if (i == 0 || *(p - 1) != 13)
			{
				result.SetAt(i, 13);
				i++;
			}
			result.SetAt(i, 10);
		}
		else
			result.SetAt(i, *p);
		p++;
	}
	return result;
}

//
//	Convert the carriage returns (CRs) in a C string from '\r' to ascii 13/ascii 10 pairs.
//	This function leaves the original alone, but returns a new string
//	with the CRs converted.
//
CString expandCRs(const CString& s)
{
	int count = 0;
	const char* savep = 0;
	const char* p = 0;

	p = savep = (const char*)s;
	while (*p)
	{
		if (*p == 13 && *(p + 1) != 10)
			count++;
		p++;
	}

	int newLength = s.GetLength() + count;
	CString result(' ', newLength);

	p = (const char*)s;
	for (int i = 0; i < newLength; i++)
	{
		if (*p == 13 && *(p + 1) != 10)
		{
			result.SetAt(i, 13);
			i++;
			result.SetAt(i, 10);
		}
		else
			result.SetAt(i, *p);
		p++;
	}
	return result;
}

void CCormanLispApp::OnAppAbout()
{
	AboutDialog aboutDialog(IDD_ABOUTBOX);
	aboutDialog.DoModal();
}

void
CCormanLispApp::OnBrowse()
{
	if (!m_browserDoc)
	{
		POSITION p = GetFirstDocTemplatePosition();
		CDocTemplate* pTemplate = GetNextDocTemplate(p);
		pTemplate = GetNextDocTemplate(p);
		pTemplate = GetNextDocTemplate(p);
		pTemplate = GetNextDocTemplate(p);	// get 4th doc template
		ASSERT(pTemplate != NULL);
		ASSERT_KINDOF(CDocTemplate, pTemplate);
		m_browserDoc = (CBrowserDoc*)pTemplate->OpenDocumentFile(NULL);
		ASSERT(m_browserDoc);
	}
	if (m_browserDoc)
	{
		POSITION p = m_browserDoc->GetFirstViewPosition();
		CBrowserView* view = (CBrowserView*)m_browserDoc->GetNextView(p);
		ASSERT(view);
		ASSERT_KINDOF(CBrowserView, view);
		m_browser = &view->m_WebBrowser;
		ASSERT(m_browser);
		CFrameWnd* frame = (CFrameWnd*)view->GetParent();
		ASSERT(frame);
		ASSERT_KINDOF(CFrameWnd, frame);
		frame->ActivateFrame();
		/*
		try
		{
			COleVariant noArg;
			m_browser->Navigate("http://www.cormanlisp.com", &noArg, &noArg, &noArg, &noArg);
		}
		catch (CException*)
		{
		}
		*/
	}
}

void CCormanLispApp::OnBrowseCCLDoc()
{
	char CormanLispDirectory[MAX_PATH];

 	DWORD chars = GetModuleFileName(0, CormanLispDirectory, sizeof(CormanLispDirectory));
	int index = chars - 1;
	while (index >= 0 && CormanLispDirectory[index] != '\\')
		index--;
	CormanLispDirectory[index] = 0;	// get rid of file name, just leave the path
	if (chars > 0)
		strcat_s(CormanLispDirectory, sizeof(CormanLispDirectory), "\\");
	strcat_s(CormanLispDirectory, sizeof(CormanLispDirectory), "documentation\\CormanLisp.pdf");
	NavigateURL(CormanLispDirectory);
}

void CCormanLispApp::NavigateURL(const char* url)
{
	OnBrowse();
	if (m_browser)
	{
		COleVariant noArg;
		try
		{
			m_browser->Navigate(url, &noArg, &noArg, &noArg, &noArg);
		}
		catch (CException*)
		{
		}
	}
}

void CCormanLispApp::OnLicenseAgreement()
{
	char path[MAX_PATH];

 	DWORD chars = GetModuleFileName(0, path, sizeof(path));
	int index = chars - 1;
	while (index >= 0 && path[index] != '\\')
		index--;
	path[index] = 0;	// get rid of file name, just leave the path
	if (chars > 0)
		strcat_s(path, sizeof(path), "\\");
	strcat_s(path, sizeof(path), "LICENSE.txt");
	NavigateURL(path);
}

void CCormanLispApp::OnCredits()
{
	char path[MAX_PATH];

 	DWORD chars = GetModuleFileName(0, path, sizeof(path));
	int index = chars - 1;
	while (index >= 0 && path[index] != '\\')
		index--;
	path[index] = 0;	// get rid of file name, just leave the path
	if (chars > 0)
		strcat_s(path, sizeof(path), "\\");
	strcat_s(path, sizeof(path), "documentation\\credits.txt");
	NavigateURL(path);
}

void CCormanLispApp::OnCormanLispCom()
{
	NavigateURL("http://www.cormanlisp.com");
}

void CCormanLispApp::OnEditPreferences()
{
	PreferencesDialog preferencesDialog(IDD_PREFERENCES);
	int ret = preferencesDialog.DoModal();
	if (ret == IDOK)
	{
		int saveTab = preferences.tab;
		int saveCharSize = preferences.charSize;
		CString saveFontName = preferences.fontName;
		bool saveBold = preferences.charBold;
		bool saveItalic = preferences.charItalic;
		COLORREF saveTextColor = preferences.textColor;

		preferences = preferencesDialog.prefsInfo();

		bool fontChanged = (saveFontName != preferences.fontName)
				|| (saveBold != preferences.charBold)
				|| (saveItalic != preferences.charItalic);

		if (fontChanged)
			for (int i = 0; i < maxFontSize; i++)
				defaultFont[i] = defaultUnderlineFont[i] = 0;

		// update all open document views (lisp windows)
		if (saveTab != preferences.tab || saveCharSize != preferences.charSize
				|| saveTextColor != preferences.textColor || fontChanged)
		{
			CDocument* doc = 0;

			POSITION pos = GetFirstDocTemplatePosition();
			while (pos)
			{
				CDocTemplate* docTemplate = GetNextDocTemplate(pos);
				if (docTemplate)
				{
					POSITION docpos = docTemplate->GetFirstDocPosition();
					while (docpos)
					{
						doc = docTemplate->GetNextDoc(docpos);
						if (doc && doc->IsKindOf(RUNTIME_CLASS(CLispDoc)))
						{
						    POSITION p = doc->GetFirstViewPosition();
							while (p != NULL)
							{
								CView* v = doc->GetNextView(p);
								if (v && v->IsKindOf(RUNTIME_CLASS(CLispView)))
								{
									CLispView* lv = (CLispView*)v;
									if (saveCharSize != preferences.charSize || fontChanged)
										lv->SetCharSize(preferences.charSize);
									if (saveTextColor != preferences.textColor)
										lv->SetTextColor(preferences.textColor);
									if (saveTab != preferences.tab  || fontChanged
											|| saveCharSize != preferences.charSize)
										lv->SetTabStops(preferences.tab);
									lv->Colorize();
								}
							}
						}
					}
				}
			}
		}
	}
}

void CCormanLispApp::OnCloseAll()
{
	CloseAllDocuments(false);
}

char MessageBuf[256];
#define WM_SETMESSAGESTRING 0x0362
// from afxpriv.h
void
CCormanLispApp::SetMessage(const CString& message)
{
	messageText = message;
	strncpy_s(MessageBuf, sizeof(MessageBuf), (const char*)message, 255);
	MessageBuf[255] = 0;
	PostMessage(m_pMainWnd->m_hWnd, WM_SETMESSAGESTRING, 0, (LPARAM)MessageBuf);
//	((CFrameWnd*)m_pMainWnd)->SetMessageText(message);
}

void CCormanLispApp::SetLineNumber(long line)
{
	((CMainFrame*)m_pMainWnd)->SetLineNumber(line);
}

void CCormanLispApp::SetColumnNumber(long column)
{
	((CMainFrame*)m_pMainWnd)->SetColumnNumber(column);
}

void
CCormanLispApp::SetDefaultMessage()
{
	SetMessage(defaultMessageText);
}

void
CCormanLispApp::DisplayLispObj(CStringArray* contents)
{
	PostMessage(GetMainWnd()->m_hWnd, WM_DISPLAY_SELECTION, 0, (LPARAM)contents);
}

CDocument* CCormanLispApp::FindDocumentByTitle(const char* title)
{
	CString docTitle;
	CDocument* doc = 0;

	POSITION pos = GetFirstDocTemplatePosition();
	while (pos)
	{
		CDocTemplate* docTemplate = GetNextDocTemplate(pos);
		if (docTemplate)
		{
			POSITION docpos = docTemplate->GetFirstDocPosition();
			while (docpos)
			{
				doc = docTemplate->GetNextDoc(docpos);
				if (doc)
				{
					docTitle = doc->GetTitle();
					if (!docTitle.CompareNoCase(title))
						return doc;
				}
			}
		}
	}
	return 0;
}

void CCormanLispApp::NotifyPrinterChanged(BOOL bUpdatePrinterSelection)
{
	if (bUpdatePrinterSelection)
		UpdatePrinterSelection(FALSE);
	POSITION pos = m_listPrinterNotify.GetHeadPosition();
	while (pos != NULL)
	{
		HWND hWnd = m_listPrinterNotify.GetNext(pos);
		::SendMessage(hWnd, m_nPrinterChangedMsg, 0, 0);
	}
}

BOOL CCormanLispApp::ParseMeasurement(LPTSTR buf, int& lVal)
{
	TCHAR* pch;
	if (buf[0] == NULL)
		return FALSE;
	float f = (float)_tcstod(buf,&pch);

	// eat white space, if any
	while (isspace(*pch))
		pch++;

	if (pch[0] == NULL) // default
	{
		lVal = (f < 0.f) ? (int)(f*GetTPU()-0.5f) : (int)(f*GetTPU()+0.5f);
		return TRUE;
	}
	for (int i=0;i<m_nNumUnits;i++)
	{
		if (lstrcmpi(pch, GetAbbrev(i)) == 0)
		{
			lVal = (f < 0.f) ? (int)(f*GetTPU(i)-0.5f) : (int)(f*GetTPU(i)+0.5f);
			return TRUE;
		}
	}
	return FALSE;
}

void CCormanLispApp::PrintTwips(TCHAR* buf, int nValue, int nDec)
{
	ASSERT(nDec == 2);
	int div = GetTPU();
	int lval = nValue;
	BOOL bNeg = FALSE;
	int i = 0;

	int* pVal = new int[nDec+1];

	if (lval < 0)
	{
		bNeg = TRUE;
		lval = -lval;
	}

	for (i=0;i<=nDec;i++)
	{
		pVal[i] = lval/div; //integer number
		lval -= pVal[i]*div;
		lval *= 10;
	}
	i--;
	if (lval >= div/2)
		pVal[i]++;

	while ((pVal[i] == 10) && (i != 0))
	{
		pVal[i] = 0;
		pVal[--i]++;
	}

	while (nDec && pVal[nDec] == 0)
		nDec--;

	_stprintf_s(buf, sizeof(buf), _T("%.*f"), nDec, (float)nValue/(float)div);

	if (m_units[m_nUnits].m_bSpaceAbbrev)
		lstrcat(buf, _T(" "));
	lstrcat(buf, GetAbbrev());
	delete []pVal;
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

static char customFilterBuf[40];

void CCormanLispApp::OnExecuteFile()
{
	char buf[MAX_PATH + 1];
	DWORD numChars = GetCurrentDirectory(MAX_PATH, buf); // save the current directory

	CFileDialog dlgFile(TRUE, NULL, NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT, NULL, NULL, 0);
	CString fileName;
	CString title;
	VERIFY(title.LoadString(AFX_IDS_OPENFILE));
	dlgFile.m_ofn.Flags |= (OFN_HIDEREADONLY | OFN_FILEMUSTEXIST);

	CString strFilter = CString("Lisp Source Files (*.lisp;*.lsp;*.cl)")	+ '\0' + "*.lisp;*.lsp;*.cl" + '\0' +
								"HTML Files (*.html;*.htm)"	+ '\0' + "*.html;*.htm" + '\0' +
								"All files (*.*)"	+ '\0' + "*.*" + '\0' + '\0';

	dlgFile.m_ofn.lpstrFilter = strFilter;
	dlgFile.m_ofn.lpstrTitle = title;
	dlgFile.m_ofn.lpstrFile = fileName.GetBuffer(_MAX_PATH);
	int nResult = dlgFile.DoModal();
	fileName.ReleaseBuffer();
	SetCurrentDirectory(buf);							// restore current directory

	if (nResult != IDOK)
		return;	// user cancelled

	// successful
	m_defaultExecFilterIndex = dlgFile.m_ofn.nFilterIndex;
	m_defaultExecDirectory = CString(dlgFile.m_ofn.lpstrFile, dlgFile.m_ofn.nFileOffset);

	CString ext = CString(dlgFile.m_ofn.lpstrFile + dlgFile.m_ofn.nFileExtension,
			strlen(dlgFile.m_ofn.lpstrFile) - dlgFile.m_ofn.nFileExtension);
	if (ext.CompareNoCase("fasl") == 0)
	{
		// load a compiled file
		CString execCmd = CString("(common-lisp:load #P\"") + fileName + "\")";
		pCormanLisp->ProcessSource((char*)(const char*)execCmd, execCmd.GetLength());
		return;
	}

	// load the file into memory
	DWORD fileSize;
	BYTE* data = MapFile(fileName, &fileSize);

	if (!data)
	{
		AfxMessageBox("Could not open file");
		return;
	}

	// process code in the file
	pCormanLisp->ProcessSource((char*)data, fileSize);

	UnmapFile(data);
}


void CCormanLispApp::OnFileOpen()
{
	char buf[MAX_PATH + 1];
	DWORD numChars = GetCurrentDirectory(MAX_PATH, buf); // save the current directory

	CFileDialog dlgFile(TRUE, NULL, NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT | OFN_ALLOWMULTISELECT, NULL, NULL, 0);
	CString fileName;
	CString title;
	VERIFY(title.LoadString(AFX_IDS_OPENFILE));
	dlgFile.m_ofn.Flags |= (OFN_HIDEREADONLY | OFN_FILEMUSTEXIST);

	CString strFilter = CString("Lisp Source Files (*.lisp;*.lsp;*.cl)")	+ '\0' + "*.lisp;*.lsp;*.cl" + '\0' +
								"HTML Files (*.html;*.htm)"	+ '\0' + "*.html;*.htm" + '\0' +
								"All files (*.*)"	+ '\0' + "*.*" + '\0' + '\0';

	dlgFile.m_ofn.lpstrFilter = "";
	dlgFile.m_ofn.lpstrTitle = title;
	dlgFile.m_ofn.lpstrFile = fileName.GetBuffer(_MAX_PATH);
	int nResult = dlgFile.DoModal();
	fileName.ReleaseBuffer();
	SetCurrentDirectory(buf);							// restore current directory

	if (nResult != IDOK)
		return;	// user cancelled

	POSITION pos (dlgFile.GetStartPosition());
	while (pos)
	{
		CString csFileName(dlgFile.GetNextPathName(pos));
		if (AfxGetApp()->OpenDocumentFile(csFileName))
		{
			// successful
			m_defaultFilterIndex = dlgFile.m_ofn.nFilterIndex;
			m_defaultDirectory = CString(dlgFile.m_ofn.lpstrFile, dlgFile.m_ofn.nFileOffset);
		}
	}
}

static void AppendFilterSuffix(CString& filter, OPENFILENAME& ofn, CDocTemplate* pTemplate)
{
	ASSERT_VALID(pTemplate);
	ASSERT_KINDOF(CDocTemplate, pTemplate);

	CString strFilterExt, strFilterName;
	if (pTemplate->GetDocString(strFilterExt, CDocTemplate::filterExt) &&
	 !strFilterExt.IsEmpty() &&
	 pTemplate->GetDocString(strFilterName, CDocTemplate::filterName) &&
	 !strFilterName.IsEmpty())
	{
		// a file based document template - add to filter list
		ASSERT(strFilterExt[0] == '.');
		// add to filter
		filter += strFilterName;
		ASSERT(!filter.IsEmpty());  // must have a file type name
		filter += (TCHAR)'\0';  // next string please
		filter += (TCHAR)'*';
		filter += strFilterExt;
		filter += (TCHAR)'\0';  // next string please
	}
}

void CCormanLispApp::SaveOpenDocumentPaths()
{
	// save document paths
	CString savedDocPaths = "";
	CString beginquote("\"");
	CString endquote("\" ");

	POSITION pos = textDocTemplate->GetFirstDocPosition();
	while (pos != NULL)
	{
		CDocument* doc = textDocTemplate->GetNextDoc(pos);
		CString path = doc->GetPathName();
		CString title = doc->GetTitle();
		if (title.CompareNoCase(WORKSHEET_TITLE))		// don't save lisp worksheet here--it will be loaded automatically
			savedDocPaths += (beginquote + path + endquote);
	}

	pos = lispSourceTemplate1->GetFirstDocPosition();
	while (pos != NULL)
	{
		CDocument* doc = lispSourceTemplate1->GetNextDoc(pos);
		CString path = doc->GetPathName();
		if (path.CompareNoCase(WORKSHEET_TITLE))		// don't save lisp worksheet here--it will be loaded automatically
			savedDocPaths += (beginquote + path + endquote);
	}

	pos = lispSourceTemplate2->GetFirstDocPosition();
	while (pos != NULL)
	{
		CDocument* doc = lispSourceTemplate2->GetNextDoc(pos);
		CString path = doc->GetPathName();
		savedDocPaths += (beginquote + path + endquote);
	}
    AfxGetApp()->WriteProfileString(_TEXT("CormanLisp"), _TEXT("OpenDocs"), _TEXT(savedDocPaths));
}

BOOL CCormanLispApp::SaveAllModified()
{
	if (theApp.preferences.rememberOpenDocuments)
		SaveOpenDocumentPaths();
	theApp.AppIsClosing(true);
	return CWinApp::SaveAllModified();
}

LRESULT
CMainFrame::OnDisplayLispObj(WPARAM /*message*/, LPARAM n)
{
	CStringArray* contents = (CStringArray*)n;
	LispObjDisplay* d = new LispObjDisplay(0, contents);
	return 0;
}

void CMainFrame::OnLispMenuItem(UINT nID)
{
    if (nID == ID_LISP_MENU_ITEM_BY_POSITION)
    {
        HMENU hMenu = gLastMenuHandle;
        int position = gLastMenuItem;   
        if (hMenu && position)
        {
	        CString s;
            s.Format("(ccl::execute-user-command-by-position (ct:int-to-foreign-ptr %d) %d\r\n)", 
                (unsigned long)hMenu, position);
	        if (pCormanLisp)
		        pCormanLisp->ProcessSource((char*)(const char*)s, s.GetLength());
        }
    }
    else
    {
	    CString s;
	    s.Format("(ccl::execute-user-command %d\r\n)", nID);
	    if (pCormanLisp)
		    pCormanLisp->ProcessSource((char*)(const char*)s, s.GetLength());
    }
}

LRESULT CMainFrame::OnLispMenuItemByPosition(WPARAM wParam, LPARAM lParam)
{
    HMENU hMenu = (HMENU)lParam;
    int position = isWindowsNTRunning() ? LOWORD(wParam) : HIWORD(wParam);   
    // in Windows 98/ME, the position is in the hiword, and on Win 2k/XP it is in the loword... 
	CString s;
    s.Format("(ccl::execute-user-command-by-position (ct:int-to-foreign-ptr %d) %d\r\n)", 
        (unsigned long)hMenu, position);
	if (pCormanLisp)
		pCormanLisp->ProcessSource((char*)(const char*)s, s.GetLength());
    return 0;
}

void CMainFrame::SetLineNumber(long line)
{
	if (line != m_currentLineNo)
	{
		m_currentLineNo = line;
		char buf[16];
		sprintf_s(buf, sizeof(buf), "Ln %d", line);
		BOOL ret = m_StatusBar.SetPaneText(1, buf, TRUE);
	}
}

void CMainFrame::SetColumnNumber(long column)
{
	if (column != m_currentColumnNo)
	{
		m_currentColumnNo = column;
		char buf[16];
		sprintf_s(buf, sizeof(buf), "Col %d", column);
		BOOL ret = m_StatusBar.SetPaneText(2, buf, TRUE);
	}
}

void CMainFrame::OnActivateApp(BOOL bActive, DWORD dwThreadID)
{
	theApp.m_isActive = bActive;
}

CString
CCormanLispApp::GetMessage()
{
	return messageText;
}

IMPLEMENT_DYNCREATE(CMainFrame, CSMDIFrameWnd)
BEGIN_MESSAGE_MAP(CMainFrame, CSMDIFrameWnd)
	ON_COMMAND_RANGE(ID_LISP_MENU_ITEM_START,
		ID_LISP_MENU_ITEM_END, CMainFrame::OnLispMenuItem)
	ON_COMMAND_RANGE(ID_LISP_MENU_ITEM_BY_POSITION,
		ID_LISP_MENU_ITEM_BY_POSITION, CMainFrame::OnLispMenuItem)
	ON_COMMAND_RANGE(ID_LISP_DYNAMIC_MENU_ITEM_START,
		ID_LISP_DYNAMIC_MENU_ITEM_END, CMainFrame::OnLispMenuItem)

    ON_MESSAGE(WM_MENUCOMMAND, CMainFrame::OnLispMenuItemByPosition)
	//{{AFX_MSG_MAP(CMainFrame)
	ON_WM_CREATE()
	ON_WM_ACTIVATEAPP()
	ON_MESSAGE(WM_DISPLAY_SELECTION, CMainFrame::OnDisplayLispObj)
	ON_UPDATE_COMMAND_UI(ID_VIEW_LISP_HEAP, CMainFrame::OnUpdateControlBarMenu)
	ON_COMMAND_EX(ID_VIEW_LISP_HEAP, CMainFrame::OnBarCheck)
	ON_UPDATE_COMMAND_UI(ID_VIEW_LISPVARIABLES, CMainFrame::OnUpdateControlBarMenu)
	ON_COMMAND_EX(ID_VIEW_LISPVARIABLES, CMainFrame::OnBarCheck)
	ON_WM_INITMENU()
	ON_WM_INITMENUPOPUP()
    ON_MESSAGE(WM_UNINITMENUPOPUP, CMainFrame::OnUninitMenuPopup)
    ON_WM_MENUSELECT()

	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

static UINT buttons[] =
{
	ID_FILE_NEW, ID_FILE_OPEN, ID_FILE_SAVE, ID_SEPARATOR,
	ID_EDIT_CUT, ID_EDIT_COPY, ID_EDIT_PASTE, ID_SEPARATOR,
	ID_FILE_PRINT, ID_APP_ABOUT
};

static UINT indicators[] =
{
	ID_SEPARATOR,
	IDS_INDICATOR_LINE_NUMBER,
	IDS_INDICATOR_COLUMN_NUMBER,
	ID_INDICATOR_CAPS,
	ID_INDICATOR_NUM,
	ID_INDICATOR_SCRL
};

int CMainFrame::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	((m_ToolBar.Create(this) &&
		m_ToolBar.LoadBitmap(IDR_MAINFRAME) &&
		m_ToolBar.SetButtons(buttons, sizeof(buttons)/sizeof(UINT)) &&
		m_StatusBar.Create(this) &&
		m_StatusBar.SetIndicators(indicators, sizeof(indicators)/sizeof(UINT)))
	  ? 0 : -1);
	SetLineNumber(1);
	SetColumnNumber(1);
	/*
	CStatusBarCtrl& statusBar = m_StatusBar.GetStatusBarCtrl();
	CRect rect;
	statusBar.GetClientRect(&rect);
	const int numparts = 4;
	int widths[numparts] = { rect.right-300, rect.right-200, rect.right-100, -1 };
	statusBar.SetParts(numparts, widths);
	*/

	if (!m_lispStatusDialogBar.Create(this, IDD_VIEW_LISP_HEAP, WS_CHILD | WS_VISIBLE | CBRS_TOP,
		ID_VIEW_LISP_HEAP))
	{
		TRACE0("Failed to create toolbar\n");
		return -1;      // fail to create
	}

	if (!m_registrationDialogBar.Create(this, IDD_VIEW_REG_TOOLBAR,
		CBRS_LEFT | CBRS_TOOLTIPS | CBRS_FLYBY, IDD_VIEW_REG_TOOLBAR))
	{
		TRACE0("Failed to create registration dialog bar\n");
		return -1;      // Fail to create.
	}

	{
		char username[256] = { '\0' };
		DWORD username_size = sizeof(username);
		CWnd* item = m_registrationDialogBar.GetDlgItem(IDC_REG_NAME);
		CString msg("User: ");
		GetUserName(username, &username_size);
		msg += username;
		item->SetWindowText(msg);
  		CDC* cdc = GetDC();
  		CSize length = cdc->GetTextExtent(msg);
  		ReleaseDC(cdc);
  		RECT infoRect;
  		item->GetClientRect(&infoRect);
  		RECT dialogRect;
  		m_registrationDialogBar.GetClientRect(&dialogRect);
  		if (length.cx > infoRect.right)
  		{
  			int diff = length.cx - infoRect.right;
 		}
	}

	if (!m_lispVarsDialogBar.Create(this, 
            IDD_VIEW_LISPVARS, WS_CHILD | WS_VISIBLE | CBRS_TOP | CBRS_SIZE_DYNAMIC,
		ID_VIEW_LISPVARIABLES))
	{
		TRACE0("Failed to create LispVars toolbar\n");
		return -1;      // fail to create
	}

	//Make the toolbar dockable
	m_ToolBar.EnableDocking(CBRS_ALIGN_ANY);
	m_lispStatusDialogBar.EnableDocking(CBRS_ALIGN_ANY);
	m_registrationDialogBar.EnableDocking(CBRS_ALIGN_ANY);
	m_lispVarsDialogBar.EnableDocking(CBRS_ALIGN_ANY);
	EnableDocking(CBRS_ALIGN_ANY);
	DockControlBar(&m_ToolBar);
	DockControlBarLeftOf(&m_lispStatusDialogBar, &m_ToolBar);
	DockControlBarLeftOf(&m_registrationDialogBar, &m_lispStatusDialogBar);
    DockControlBarLeftOf(&m_lispVarsDialogBar, &m_registrationDialogBar);

	int res = CSMDIFrameWnd::OnCreate(lpCreateStruct);

	// set proper size for main toolbar
	{
		RECT r;
		int height = 0;

		memset(&r, 0, sizeof(r));
		m_lispStatusDialogBar.GetClientRect(&r);

		height = r.bottom - r.top;

		if (height > 0)
		{
			m_ToolBar.SetHeight(height);
			m_ToolBar.RedrawWindow();
		}
	}

	return res;
}

IMPLEMENT_DYNCREATE(CLispDoc, CRichEditDoc)
BEGIN_MESSAGE_MAP(CLispDoc, CRichEditDoc)
	ON_UPDATE_COMMAND_UI(ID_FILE_CLOSE, OnUpdateFileClose)
	ON_UPDATE_COMMAND_UI(ID_FILE_PRINT, OnUpdateIfEmbedded)
	ON_UPDATE_COMMAND_UI(ID_FILE_PRINT_DIRECT, OnUpdateIfEmbedded)
	ON_UPDATE_COMMAND_UI(ID_FILE_PRINT_PREVIEW, OnUpdateIfEmbedded)
	ON_COMMAND(ID_FILE_SAVE_AS, OnFileSaveAs)
	//ON_WM_CLOSE()
	//{{AFX_MSG_MAP(CLispDoc)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

CLispDoc::CLispDoc()
{
	m_bRTF = FALSE;
	lastWriteTime.dwHighDateTime = 0;
	lastWriteTime.dwLowDateTime = 0;
}

CLispDoc::~CLispDoc()
{
	if (this == theApp.m_worksheet)
		theApp.m_worksheet = 0;
}

void CLispDoc::OnFileSaveAs()
{
	char buf[MAX_PATH];
	DWORD numChars = GetCurrentDirectory(MAX_PATH, buf); // save the current directory
	if (!DoSave(NULL))
		TRACE0("Warning: File save-as failed.\n");
	SetCurrentDirectory(buf);							// restore current directory
}

void CLispDoc::OnFileSave()
{
	char buf[MAX_PATH];
	DWORD numChars = GetCurrentDirectory(MAX_PATH, buf); // save the current directory
	DoFileSave();
	SetCurrentDirectory(buf);							// restore current directory
}

BOOL CLispDoc::DoFileSave()
{
	DWORD dwAttrib = GetFileAttributes(m_strPathName);

	if (dwAttrib != INVALID_FILE_ATTRIBUTES && dwAttrib & FILE_ATTRIBUTE_READONLY)
	{
		int result = AfxMessageBox(CString("Do you wish to overwrite the read-only file ")+ m_strPathName + "?", MB_YESNOCANCEL);
		if (result == IDCANCEL)
			return FALSE;
		if (result == IDYES)
		{
			// turn off read-only bit
			BOOL result = SetFileAttributes(m_strPathName, dwAttrib & ~FILE_ATTRIBUTE_READONLY);
			if (result == 0)
				AfxMessageBox(CString("Could not set write access for file ") + m_strPathName + ".", MB_OK);
		}
		dwAttrib = GetFileAttributes(m_strPathName);
	}

	if (dwAttrib & FILE_ATTRIBUTE_READONLY)
	{
		// we do not have read-write access or the file does not (now) exist
		if (!DoSave(NULL))
		{
			TRACE0("Warning: File save with new name failed.\n");
			return FALSE;
		}
	}
	else
	{
		if (!DoSave(m_strPathName))
		{
			TRACE0("Warning: File save failed.\n");
			return FALSE;
		}
	}
	return TRUE;
}

// Returns TRUE if successful, FALSE otherwise.
static BOOL getLastWriteTime(LPCTSTR path, LPFILETIME lastWriteTime)
{
	CFile file;
	BOOL ret = file.Open(path, CFile::modeRead);
	if (!ret)
		return FALSE;
	HANDLE handle = file.m_hFile;
	if (!handle)
		return FALSE;
	FILETIME CreationTime;
	FILETIME LastAccessTime;
	ret = ::GetFileTime(handle, &CreationTime, &LastAccessTime, lastWriteTime);
	file.Close();
	return ret ? TRUE : FALSE;
}

//
// Override this to set the document title back to the full path name after a save
// or save as operation.
//
BOOL CLispDoc::OnSaveDocument(LPCTSTR path)
{
	DWORD dwAttrib = GetFileAttributes(m_strPathName);
	if (dwAttrib != INVALID_FILE_ATTRIBUTES && dwAttrib & FILE_ATTRIBUTE_READONLY)
	{
		int result = AfxMessageBox(CString("Do you wish to overwrite the read-only file ")+ path + "?", MB_OKCANCEL);
		if (result == IDCANCEL)
			return FALSE;
		if (result == IDOK)
		{
			// turn off read-only bit
			BOOL result = SetFileAttributes(path, dwAttrib & ~FILE_ATTRIBUTE_READONLY);
			if (result == 0)
				AfxMessageBox(CString("Could not set write access for file ") + path + ".", MB_OK);
		}
	}

	BOOL ret = CRichEditDoc::OnSaveDocument(path);

	if (theApp.preferences.fullPathInTitle && _stricmp(GetTitle(), WORKSHEET_TITLE))
	{
		CString pathName = GetPathName();
		if (pathName.GetLength() > 0)
			SetTitle(pathName);
	}
	if (ret)
	{
		// update the last write time
		BOOL result = getLastWriteTime(path, &lastWriteTime);
	}	
	return ret;
}

static BOOL skipStatusCheck = FALSE;

//
// We override this to track the file update time.
//
BOOL CLispDoc::OnOpenDocument(LPCTSTR path)
{
	BOOL ret = CRichEditDoc::OnOpenDocument(path);

	if (ret && !skipStatusCheck)
	{
		// update the last write time
		BOOL result = getLastWriteTime(path, &lastWriteTime);
	}
	return ret;
}

static DWORD CALLBACK MyStreamInCallback(DWORD dwCookie, LPBYTE pbBuff, LONG cb, LONG *pcb)
{
   CFile* pFile = (CFile*) dwCookie;
   *pcb = pFile->Read(pbBuff, cb);
   return 0;
}

void CLispDoc::CheckFileUpdateStatus()
{
	FILETIME writeTime;
	if (skipStatusCheck)
		return;
	CString path = GetPathName();
	BOOL result = getLastWriteTime(path, &writeTime);
	
	if (result && (writeTime.dwHighDateTime != lastWriteTime.dwHighDateTime
		|| writeTime.dwLowDateTime != lastWriteTime.dwLowDateTime))
	{
		CString message = "The contents of the file ";
		message += path;
		message += " has changed. Do you wish to reload the file?";
		if (IsModified())
		{
			message += " \nChoosing Yes will cause you to lose the changes you have made to the file.";
		}
		skipStatusCheck = TRUE;
		int result = AfxMessageBox(message, MB_YESNO);
		skipStatusCheck = FALSE;
		
		if (result == IDYES)
		{
			CLispView* view = (CLispView*)GetView();
			if (view)
			{
				CRichEditCtrl& ed = view->GetRichEditCtrl();
				CFile file;
				BOOL ret = file.Open(path, CFile::modeRead);
				
				if (!ret)
				{
					CString msg2 = "The file ";
					msg2 += path;
					msg2 += " could not be updated. An error occurred when trying to open the file for reading.";
					skipStatusCheck = TRUE;
					AfxMessageBox(msg2, MB_OK);
					skipStatusCheck = FALSE;
					return;
				}
				EDITSTREAM es;
				es.dwCookie = (DWORD)&file;
				es.pfnCallback = MyStreamInCallback; 
				skipStatusCheck = TRUE;
				ed.StreamIn(SF_TEXT, es);
				view->SetTabStops(theApp.preferences.tab);		// default tab = 4
				SetModifiedFlag(FALSE);

				file.Close();
				BOOL result = getLastWriteTime(path, &lastWriteTime);
				skipStatusCheck = FALSE;
			}
		}
		else
		{
			// they chose not to update, so update the time anyway to avoid
			// warning them again
			BOOL result = getLastWriteTime(path, &lastWriteTime);
		}
	}
}

void CLispDoc::OnFrameWindowActivate(BOOL bActivate)
{
	CRichEditDoc::OnFrameWindowActivate(bActivate);
	if (!bActivate)
		return;
	CheckFileUpdateStatus();
}

BOOL CLispDoc::DoSave(LPCTSTR lpszPathName, BOOL bReplace)
	// Save the document data to a file
	// lpszPathName = path name where to save document file
	// if lpszPathName is NULL then the user will be prompted (SaveAs)
	// note: lpszPathName can be different than 'm_strPathName'
	// if 'bReplace' is TRUE will change file name if successful (SaveAs)
	// if 'bReplace' is FALSE will not change path name (SaveCopyAs)
{
	CString newName = lpszPathName;
	if (newName.IsEmpty())
	{
//		CDocTemplate* pTemplate = GetDocTemplate();
		CDocTemplate* pTemplate = lispSourceTemplate1;

		ASSERT(pTemplate != NULL);

		newName = m_strPathName;
		if (bReplace && newName.IsEmpty())
		{
			newName = m_strTitle;
			// check for dubious filename
			int iBad = newName.FindOneOf(_T(" #%;/\\"));
			if (iBad != -1)
				newName.ReleaseBuffer(iBad);

			// append the default suffix if there is one
			CString strExt;
			if (pTemplate->GetDocString(strExt, CDocTemplate::filterExt) &&
			  !strExt.IsEmpty())
			{
				ASSERT(strExt[0] == '.');
				newName += strExt;
			}
		}

		if (!AfxGetApp()->DoPromptFileName(newName,
		  bReplace ? AFX_IDS_SAVEFILE : AFX_IDS_SAVEFILECOPY,
		  OFN_HIDEREADONLY | OFN_PATHMUSTEXIST, FALSE, pTemplate))
			return FALSE;       // don't even attempt to save
	}

	CWaitCursor wait;

	if (!OnSaveDocument(newName))
	{
		if (lpszPathName == NULL)
		{
			// be sure to delete the file
			TRY
			{
				CFile::Remove(newName);
			}
			CATCH_ALL(e)
			{
				TRACE0("Warning: failed to delete file after failed SaveAs.\n");
			//	DELETE_EXCEPTION(e);
			}
			END_CATCH_ALL
		}
		return FALSE;
	}

	// reset the title and change the document name
	if (bReplace)
		SetPathName(newName, TRUE);
	if (theApp.preferences.fullPathInTitle && _stricmp(GetTitle(), WORKSHEET_TITLE))
	{
		CString pathName = GetPathName();
		if (pathName.GetLength() > 0)
			SetTitle(pathName);
	}
	return TRUE;        // success
}

CRichEditCntrItem*
CLispDoc::CreateClientItem(REOBJECT* preo) const
{
	// cast away constness of this
	return new CRichEditCntrItem(preo, (CLispDoc*)this);
}

afx_msg void CLispDoc::OnUpdateFileClose(CCmdUI* pCmdUI)
{
	CString title = GetTitle();
	if (!title.CompareNoCase(WORKSHEET_TITLE))
		pCmdUI->Enable(FALSE);
	else
		pCmdUI->Enable(TRUE);
}

void CLispDoc::OnCloseDocument()
{
	CString title = GetTitle();
	if (title.CompareNoCase(WORKSHEET_TITLE) || theApp.AppIsClosing())
		CRichEditDoc::OnCloseDocument();
}

BOOL CLispDoc::CanCloseFrame(CFrameWnd* frame)
{
	CString title = GetTitle();
	if (!title.CompareNoCase(WORKSHEET_TITLE) && !theApp.AppIsClosing())
		return FALSE;
	else
		return CDocument::CanCloseFrame(frame);
}

void CLispDoc::OnUpdateIfEmbedded(CCmdUI* pCmdUI)
{
	pCmdUI->Enable(!IsEmbedded());
}

IMPLEMENT_DYNCREATE(CLispView, CRichEditView)
BEGIN_MESSAGE_MAP(CLispView, CRichEditView)
	//{{AFX_MSG_MAP(CLispView)
	ON_COMMAND(ID_EDIT_CUT, OnEditCut)
	ON_COMMAND(ID_EDIT_COPY, OnEditCopy)
	ON_COMMAND(ID_EDIT_PASTE, OnEditPaste)
	ON_COMMAND(ID_EDIT_UNDO, OnEditUndo)
	ON_COMMAND(ID_EDIT_REDO, OnEditRedo)
	ON_WM_KEYDOWN()
	ON_WM_CHAR()
	ON_WM_LBUTTONDOWN()
	ON_WM_RBUTTONDOWN()
	ON_WM_LBUTTONDBLCLK()
	ON_WM_PAINT()
	ON_WM_CREATE()
	ON_WM_SIZE()
	//}}AFX_MSG_MAP
	ON_WM_LBUTTONUP()
	ON_WM_RBUTTONUP()
	ON_WM_MOUSEMOVE()
	ON_WM_HSCROLL()
	ON_WM_VSCROLL()
	ON_WM_CONTEXTMENU()
	ON_WM_ACTIVATE()

//	ON_WM_NOTIFY()
	ON_COMMAND(ID_FILE_PRINT, OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_DIRECT, OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_PREVIEW, CRichEditView::OnFilePrintPreview)
	ON_REGISTERED_MESSAGE(CCormanLispApp::m_nPrinterChangedMsg, OnPrinterChangedMsg)
	ON_COMMAND(ID_PAGE_SETUP, OnPageSetup)
	ON_COMMAND(ID_EXECUTESELECTION, OnExecuteSelection)
	ON_COMMAND(ID_GOTO_LINE,		OnGotoLine)
END_MESSAGE_MAP()

const AFX_DATADEF DWORD worksheetStyleDefault =
	AFX_WS_DEFAULT_VIEW |
	/* WS_HSCROLL | */ WS_VSCROLL |
	/* ES_AUTOHSCROLL | */ ES_AUTOVSCROLL |
	ES_MULTILINE | ES_NOHIDESEL;

const AFX_DATADEF DWORD editStyleDefault =
	AFX_WS_DEFAULT_VIEW |
	WS_HSCROLL | WS_VSCROLL |
	ES_AUTOHSCROLL | ES_AUTOVSCROLL |
	ES_MULTILINE | ES_NOHIDESEL;

CLispView::CLispView() : m_bInPrint(FALSE), m_font(0),
m_lispHighlightStart(-1), m_lispHighlightEnd(-1),
m_highlightOn(false)
{
	RECT margins;		// set default margins
	margins.left = 1440 / 2;		//.5 inch
	margins.top = 1440 / 2;			//.5 inch
	margins.right = 1440 / 2;		//.5 inch
	margins.bottom = 1440 / 2;		//.5 inch
	SetMargins(&margins);
	m_mouseCueDisplayed = false;
	m_mouseCuePosition.x = 0;
	m_mouseCuePosition.y = 0;
	m_usingKeyboard = false;
	m_mouseCueDisabled = 0;
	m_colorizeDisabled = 0;
	m_firstModified = 0;
}

CLispView::~CLispView()
{
	DeleteObject(m_font);
}

void CLispView::Colorize()
{
	if (OnColorizeFuncPtr && theApp.preferences.autoColorize)
	{
		CDocument* doc = GetDocument();
		if (doc)
		{
			CString name = doc->GetPathName();
			bool isLispFile = false;
			if (name.GetLength() >= 5 && !name.Right(5).CompareNoCase(".lisp"))
				isLispFile = true;
			else if (name.GetLength() >= 4 && !name.Right(4).CompareNoCase(".lsp"))
				isLispFile = true;
			else if (name.GetLength() >= 3 && !name.Right(3).CompareNoCase(".cl"))
				isLispFile = true;
			else if (!doc->GetTitle().CompareNoCase(WORKSHEET_TITLE))
				isLispFile = true;

			if (isLispFile)
			{
				BOOL saveModified = doc->IsModified();

				int pos = GetScrollPos(SB_VERT);
				HideCaret();
				OnColorizeFuncPtr(m_hWnd, 0 /*max(0, m_firstModified - 1000)*/, GetTextLength());
				ShowCaret();
				SetScrollPos(SB_VERT, pos);
				doc->SetModifiedFlag(saveModified);
				m_colorizeDisabled = true;
				m_firstModified = 1000000;
			}
		}
	}
}

void CLispView::activateFont(HDC hDC)
{
	CFont* font = getDefaultFont(hDC, theApp.preferences.charSize);
	SetFont(font, TRUE);
	SelectObject(hDC, *font);

    // Extract font dimensions from the text metrics.
	TEXTMETRIC tm;
    GetTextMetrics (hDC, &tm);
	m_lineHeight = tm.tmHeight + tm.tmExternalLeading;
}

void
CLispView::OnInitialUpdate()
{
	CRichEditView::OnInitialUpdate();

	CDC* cdc = GetDC();
	HDC hDC = cdc->m_hDC;
	activateFont(hDC);
	ReleaseDC(cdc);

	SetTabStops(theApp.preferences.tab);		// default tab = 4
	SetCharSize(theApp.preferences.charSize);	// default character size = 10
	SetTextColor(theApp.preferences.textColor);

	CRichEditCtrl& ed = GetRichEditCtrl();
	ed.SetOptions(ECOOP_XOR, ECO_AUTOWORDSELECTION);	// turn off auto word selection
	ed.SetOptions(ECOOP_OR, ECO_WANTRETURN);			// turn on hard returns
	ed.SetModify(FALSE);
	BOOL ret1 = ed.GetModify();

	// get the vertical scrollbar
	ShowScrollBar(SB_VERT, TRUE);
	/*
	SCROLLINFO scrollInfo;
	memset(&scrollInfo, 0, sizeof(scrollInfo));
	scrollInfo.cbSize = sizeof(scrollInfo);
	scrollInfo.fMask = SIF_ALL;
	BOOL success = GetScrollInfo(SB_VERT, &scrollInfo);
	if (success)
	{
		scrollInfo.fMask = SIF_ALL | SIF_DISABLENOSCROLL;
		SetScrollInfo(SB_VERT, &scrollInfo, TRUE);
	}
	*/
	m_colorizeDisabled = false;
}

int CLispView::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	if (CRichEditView::OnCreate(lpCreateStruct) == -1)
		return -1;
	theApp.m_listPrinterNotify.AddTail(m_hWnd);
	GetRichEditCtrl().SetEventMask(GetRichEditCtrl().GetEventMask() | EN_MSGFILTER);
	return 0;
}

afx_msg void CLispView::OnPaint()
{
	CRichEditCtrl& ed = GetRichEditCtrl();
	CDC* cdc = GetDC();
	if (!cdc)
		return;	// for some reason we get a null pointer here occasionally
	HDC hDC = cdc->m_hDC;
	HGDIOBJ orig = SelectObject(hDC, m_font);
	CRichEditView::OnPaint();
	if (m_highlightOn)
	{
		highlightChar(ed, m_lispHighlightStart);
		highlightChar(ed, m_lispHighlightEnd);
	}
	if (m_mouseCueDisplayed && !mouseCueDisabled())
		displayMouseCue();
	SelectObject(hDC, orig);
	ReleaseDC(cdc);
}

void CLispView::OnSize(UINT nType, int cx, int cy)
{
	CRichEditView::OnSize(nType, cx, cy);
	if (nType == SIZE_RESTORED)
		ShowScrollBar(SB_VERT, TRUE);
}

BOOL CLispView::OnScroll(UINT nScrollCode, UINT nPos, BOOL bDoScroll)
{
	BOOL ret = CRichEditView::OnScroll(nScrollCode, nPos, bDoScroll);
	return ret;
}

class CharClassifier
{
public:
	enum Classes
	{
		Normal = 0,
		Period,
		Comma,
		LeftParen,
		RightParen,
		Quote,
		Backquote,
		Whitespace
	};

	CharClassifier();
	int GetClassification(char code);
private:
	unsigned char codes[256];
};

CharClassifier::CharClassifier()
{
	for (int i = 0; i < 256; i++)
	{
		if (isspace(i))
			codes[i] = (Whitespace | WBF_ISWHITE);
		else
			codes[i] = Normal;
	}
	codes['"'] = (Period | WBF_BREAKAFTER);
	codes[','] = (Comma | WBF_BREAKAFTER);
	codes['('] = (LeftParen | WBF_BREAKAFTER);
	codes[')'] = (RightParen | WBF_BREAKAFTER);
	codes['\''] = (Quote | WBF_BREAKAFTER);
	codes['`'] = (Backquote | WBF_BREAKAFTER);
}

int CharClassifier::GetClassification(char code)
{
	return (int)codes[code];
}

CharClassifier classifier;

// get the character classification
inline int cclass(char c)
{
	return classifier.GetClassification(c) & WBF_CLASS;
}

inline bool isSpace(char c)
{
	return cclass(c) == CharClassifier::Whitespace;
}

static void findNextNonWordChar(char* s, int& index, int charclass)
{
	if (!s[index])
		return;
	index++;
	while (s[index] && cclass(s[index]) == charclass)
		index++;
}

static void findPrevNonWordChar(char* s, int& index, int charclass)
{
	if (!s[index])
		return;
	index--;
	while (s[index] && cclass(s[index]) == charclass)
		index--;
}

static void findSExprBounds(char* s, int index, int& left, int& right)
{
	int parenCount = 0;

	if (cclass(s[index]) == CharClassifier::LeftParen)
	{
		left = index;
		parenCount = 1;
		while (s[index])
		{
			index++;
			if (cclass(s[index]) == CharClassifier::LeftParen)
				parenCount++;
			else
			if (cclass(s[index]) == CharClassifier::RightParen)
			{
				parenCount--;
				if (parenCount == 0)
				{
					right = index;
					return;
				}
			}
		}
		right = index;
	}
	else
	if (cclass(s[index - 1]) == CharClassifier::RightParen)
	{
		right = index;
		index--;
		parenCount = 1;
		while (s[index])
		{
			index--;
			if (cclass(s[index]) == CharClassifier::RightParen)
				parenCount++;
			else
			if (cclass(s[index]) == CharClassifier::LeftParen)
			{
				parenCount--;
				if (parenCount == 0)
				{
					left = index;
					return;
				}
			}
		}
		left = index;
	}
	else
	if (isSpace(s[index]))
	{
		left = right = index;
		return;
	}
	else
	{
		int charclass = cclass(s[index]);
		while (s[index])
		{
			index--;
			if (cclass(s[index]) != charclass)
			{
				index++;
				break;
			}
		}
		left = index;
		while (s[index])
		{
			index++;
			if (cclass(s[index]) != charclass)
			{
				index--;
				break;
			}
		}
		right = index;
		return;
	}
}

static void findPrevSExpression(char* s, int& index, int /*max*/)
{
	int left, right;
	findSExprBounds(s, index, left, right);
	assert(left <= index && index <= right);
	if (left < index)
	{
		index = left;
		return;
	}
	index = left - 1;
	while (index > 0 && isSpace(s[index]))
		index--;
	findSExprBounds(s, index, left, right);
	assert(left <= index && index <= right);
	index = left;
	return;
}

// Finds which s-expression contains the current character,
// and returns the first non-whitespace character following
// that s-expression
static void findNextSExpression(char* s, int& index, int max)
{
	int left, right;
	findSExprBounds(s, index, left, right);
	assert(left <= index && index <= right);
	index = right + 1;
	while (index < max && isSpace(s[index]))
		index++;
	return;
}


static LONG __stdcall wordBreakProcEx(LPTSTR s, LONG cchText, BYTE /*bCharSet*/,
										 int code)
{
	int index = 0;
	if (!s)
		return 0;

	switch (code)
	{
	case WB_ISDELIMITER:
		{
			return 0;		// no delimiters defined
		}

	case WB_MOVEWORDLEFT:
	case WB_LEFT:
		{
			findPrevSExpression(s, index, -cchText);
			return index;
		}

	case WB_RIGHT:
	case WB_MOVEWORDRIGHT:
		{
			int left, right;
			findSExprBounds(s, index, left, right);
			assert(left <= index && index <= right);
			index = right + 1;
			return index;
		}

	case WB_CLASSIFY:
		return classifier.GetClassification(s[index]);

	case WB_LEFTBREAK:
		{
			if (index == 0)
				return index;
			if (cclass(s[index]) == CharClassifier::RightParen)
				return index;
			if (cclass(s[index - 1]) == CharClassifier::RightParen)
				return index;
			index--;
			if (isSpace(s[index]))
				return index + 1;
			findPrevNonWordChar(s, index, cclass(s[index]));
			return index + 1;
		}

	case WB_RIGHTBREAK:
		{
			if (!s[index])
				return index;
			index++;
			if (isSpace(s[index]))
				return index + 1;
			findNextNonWordChar(s, index, cclass(s[index]));
			return index + 1;
		}

	default:
		return 0;
	}
}

void
CLispView::OnActivateView( BOOL bActivate, CView* pActivateView, CView* pDeactiveView )
{
	CurrentView = (CLispView*)pActivateView;
	m_colorizeDisabled = false;
	CRichEditView::OnActivateView(bActivate, pActivateView, pDeactiveView);
	if (!bActivate)
		return;
	CLispDoc* doc = (CLispDoc*)GetDocument();
	if (doc)
	{
		doc->CheckFileUpdateStatus();
	}
//	Colorize();		// why does this cause "Lookup Source" to crash the IDE?  -RGC TO DO
}

BOOL CLispView::PreCreateWindow(CREATESTRUCT& cs)
{
	// we only want to enable word wrap on the worksheet
	if (OpeningWorksheet)
	{
		m_dwDefaultStyle = worksheetStyleDefault;
		m_nWordWrap = WrapToWindow;
		WrapChanged();
	}
	else
	{
		m_dwDefaultStyle = editStyleDefault;
		m_nWordWrap = WrapNone;
	}
	return CRichEditView::PreCreateWindow(cs);
}

const int LineBufSize = 0x4000;
static char LineBuf[LineBufSize];

static void
collectInputText(CRichEditCtrl& ed, CView& view)
{
	long startChar = 0;
	long endChar = 0;
	long added = 0;
	long startLine = 0;
	long num = 0;
	CString str;

	readingFromConsole = 1;

	ed.GetSel(startChar, endChar);

	if (endChar == startChar)
	{
		// send the current line
		startLine = ed.LineFromChar(startChar /* - 1 */);
		num = ed.GetLine(startLine, LineBuf, LineBufSize - 1);
		if (pCormanLisp)
		{
			pCormanLisp->ProcessSource(LineBuf, num);
			pCormanLisp->ProcessSource("\r\n", 2);
		}
		return;
	}

	str = ed.GetSelText();
	if (pCormanLisp)
	{
		// expand the CRs into CR/LF pairs
		CString cs(str);
		CString expanded = expandCRs(cs);
		const char* chars = (const char*)expanded;
		pCormanLisp->ProcessSource((char*)chars, expanded.GetLength());
	}
}

void CLispView::OnEditCut()
{
	LispHighlightOff();
	CRichEditView::OnEditCut();
	m_colorizeDisabled = false;
	adjustLispHighlight();
}

void
CLispView::OnEditCopy()
{
	LispHighlightOff();
	CRichEditView::OnEditCopy();
	adjustLispHighlight();
}

void
CLispView::OnEditPaste()
{
//	CRichEditView::OnEditPaste();
	ASSERT(::IsWindow(m_hWnd));
	LispHighlightOff();
	CRichEditCtrl& ed = GetRichEditCtrl();
	CHARFORMAT format = {sizeof(CHARFORMAT)};
	format.dwMask = CFM_COLOR | CFM_UNDERLINE | CFM_BOLD | CFM_SIZE | CFM_CHARSET;
	ed.GetSelectionCharFormat(format);
	m_nPasteType = 0;
	format.crTextColor = theApp.preferences.textColor;
	format.dwEffects = 0;
	BOOL ret = ed.SetSelectionCharFormat(format);
	ed.PasteSpecial(CF_TEXT);
//	CRichEditView::OnEditPaste();
//	BOOL ret = ed.SetSelectionCharFormat(format);
	m_colorizeDisabled = false;
	long start, end;
	ed.GetSel(start, end);
	m_firstModified = min(start, m_firstModified);
	adjustLispHighlight();
}

void CLispView::OnEditUndo()
{
	LispHighlightOff();
	CRichEditView::OnEditUndo();
	m_colorizeDisabled = false;
	adjustLispHighlight();
}

void CLispView::OnEditRedo()
{
	LispHighlightOff();
	CRichEditView::OnEditRedo();
	m_colorizeDisabled = false;
	adjustLispHighlight();
}

void CLispView::OnLButtonDown(UINT nFlags, CPoint point)
{
	m_usingKeyboard = false;
	LispHighlightOff();
	mouseCueOff();
	m_mouseCueDisabled = 1;
	CRichEditView::OnLButtonDown(nFlags, point);
	adjustLispHighlight();
//	SetCapture();
}

void CLispView::OnRButtonDown(UINT nFlags, CPoint point)
{
	m_usingKeyboard = false;
	m_mouseCueDisabled = 1;
	LispHighlightOff();
	mouseCueOff();
	ClientToScreen(&point);
	OnContextMenu(this, point);
	adjustLispHighlight();
}

void
CLispView::OnLButtonUp(UINT nFlags, CPoint point)
{
	CRichEditView::OnLButtonUp(nFlags, point);
	m_mouseCueDisabled = 0;
}

void
CLispView::OnRButtonUp(UINT nFlags, CPoint point)
{
	CRichEditView::OnRButtonUp(nFlags, point);
	m_mouseCueDisabled = 0;
}

#define MaxWordScan 0x1000

void CLispView::OnLButtonDblClk(UINT /*nFlags*/, CPoint /*point*/)
{
	m_usingKeyboard = false;
	mouseCueOff();
	CRichEditCtrl& ed = GetRichEditCtrl();
	long start, end;
	ed.GetSel(start, end);

	if (m_highlightOn)
	{
		CRichEditCtrl& ed = GetRichEditCtrl();
		ed.SetSel(m_lispHighlightStart, m_lispHighlightEnd + 1);
	}
	else if (start == end)
	{
		// handle double-click on word
		// find the preceding character
		char buf[MaxWordScan * 2 + 1];
		int textStart = max(start - MaxWordScan, 0);
		int textEnd = start + MaxWordScan;
		int current = start - textStart;
//		ed.HideSelection(TRUE, FALSE);
//		ed.SetSel(textStart, textEnd);
//		long numChars = ed.GetSelText(buf);
		long numChars = getTextRange(ed, buf, textStart, textEnd);
		int wordStart, wordEnd;
		wordStart = wordEnd = current;
		findPrevNonWordChar(buf, wordStart, cclass(buf[current]));
		findNextNonWordChar(buf, wordEnd, cclass(buf[current]));
		ed.SetSel(textStart + wordStart + 1, textStart + wordEnd);
//		ed.HideSelection(FALSE, FALSE);
	}
//		CRichEditView::OnLButtonDblClk(nFlags, point);
}

// calculate column, taking tabs into account
int CLispView::calculateColumnNumber(int line, int pos)
{
	CRichEditCtrl& ed = GetRichEditCtrl();
	int lineStart = ed.LineIndex(line);
	int lineLength = max(ed.LineLength(lineStart), 4);
	ed.GetLine(line, LineBuf, min(lineLength, LineBufSize));
	int tab = theApp.preferences.tab;
	int offset = pos - lineStart;
	int column = 0;
	for (int n = 0; n < offset; n++)
	{
		if (n < LineBufSize && LineBuf[n] == '\t')
			column += (tab - (column % tab));
		else
			column++;
	}
	return column;
}

void CLispView::OnMouseMove(UINT nFlags, CPoint point)
{
	if (nFlags == 0 &&
		(gLastPoint.x != point.x ||
		 gLastPoint.y != point.y ||
		 gView != this))
	{
		m_usingKeyboard = false;
		mouseCueOff();
		gLastPoint = point;
		gStartTimeAtLastPoint = GetTickCount();
		gView = this;
	}
	else
	if (nFlags/* & MK_LBUTTON*/)
	{
		CRichEditCtrl& ed = GetRichEditCtrl();
		int pos = CharFromPos(point);
		int line = ed.LineFromChar(pos);

		theApp.SetLineNumber(line + 1);
		theApp.SetColumnNumber(calculateColumnNumber(line, pos) + 1);
	}
	CRichEditView::OnMouseMove(nFlags, point);
}

void
CLispView::OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags)
{
	long numThreads = 0;
	m_usingKeyboard = true;
	gLastKeyPressTime = GetTickCount();
	long start, end;
	CRichEditCtrl& ed = GetRichEditCtrl();
	ed.GetSel(start, end);
	m_firstModified = min(start, m_firstModified);
	m_colorizeDisabled = false;
	mouseCueOff();
	if (nChar == 3)
	{
		if (pCormanLisp)
		{
			pCormanLisp->GetNumThreads(&numThreads);
			if (numThreads > 0)
				pCormanLisp->AbortThread();
		}
	}
	else
	if (nChar == 13 && ((nFlags & 0x100) || GetKeyState(VK_SHIFT) < 0))
	{
		// Numeric Enter or Shift-Enter was pressed
	}
	else
	{
		CRichEditView::OnKeyDown(nChar, nRepCnt, nFlags);
		adjustLispHighlight();
	}
}

#define ASCII_TAB		 9
#define ASCII_NEWLINE	10
#define ASCII_CR		13
#define ASCII_SPACE		32

#define MaxScanLength 0x10000		// 64k

long getOpenLeftParenCount(CRichEditCtrl& ed, long pos)
{
	int firstPos = max(pos - MaxScanLength, 0);
	char str[MaxScanLength + 1];
	long numChars = getTextRange(ed, str, firstPos, pos);

	// go back looking for start of expression
	int leftParenCount = 0;
	int index = pos - 1;
	while (index >= firstPos)
	{
		if (str[index - firstPos] == ')')
			leftParenCount++;
		else
		if (str[index - firstPos] == '(')
		{
			leftParenCount--;
			if (index > firstPos &&
					(str[index - firstPos - 1] == ASCII_NEWLINE
						|| str[index - firstPos - 1] == ASCII_CR))
				break;
		}
		index--;
	}
	if (leftParenCount > 0)
		leftParenCount = 0;
	return -leftParenCount;
}

#define MaxTab 80

void delay(int ms)
{
	// delay ms milliseconds
	Sleep(ms);
}

void CLispView::OnChar(UINT nChar, UINT nRepCnt, UINT nFlags)
{
	// if the splash screen is up and escape is pressed, close the screen
	if (CCormanLispApp::splashScreen && nChar == 27)
		theApp.m_closeSplash = TRUE;
	m_usingKeyboard = true;
	mouseCueOff();
	CRichEditCtrl& ed = GetRichEditCtrl();
 	long numThreads = 0;
	long start, end;
	ed.GetSel(start, end);
	m_firstModified = min(start, m_firstModified);

	// watch for control-break
	if (nChar == 3 && (GetKeyState(VK_CONTROL) < 0))
	{
		if (pCormanLisp)
		{
			pCormanLisp->GetNumThreads(&numThreads);
			if (numThreads > 0)
				pCormanLisp->AbortThread();
		}
	}
	else
	// watch for control-G
	if (nChar == 7)
	{
		OnGotoLine();
	}
	else
	if (nChar == 13 && ((nFlags & 0x100) || GetKeyState(VK_SHIFT) < 0))
	{
		// Numeric Enter or Shift-Enter was pressed
		bool lispHighlightWasOn = false;
		long saveSelStart, saveSelEnd;

		if (m_highlightOn)		// if there is a highlighted lisp expression
		{
			// select it for 100 ms or so, then execute it
			lispHighlightWasOn = true;
			ed.GetSel(saveSelStart, saveSelEnd);
			long lispHighlightStart = m_lispHighlightStart;
			long lispHighlightEnd = m_lispHighlightEnd;
			LispHighlightOff();
			ed.SetSel(lispHighlightStart, lispHighlightEnd + 1);
			ed.HideSelection(FALSE, FALSE);
			ed.UpdateWindow();
			delay(100);
		}
		collectInputText(ed, *this);
		if (lispHighlightWasOn)
			ed.SetSel(saveSelStart, saveSelEnd);
	}
	else
	if (nChar == 13 && start == end)
	{
		// normal Enter was pressed
		long leftParenCount = getOpenLeftParenCount(ed, start);
		LispHighlightOff();
		CHARFORMAT format = {sizeof(CHARFORMAT)};
		format.dwMask = CFM_COLOR | CFM_UNDERLINE;
		format.crTextColor = theApp.preferences.textColor;
		format.dwEffects = CFE_AUTOCOLOR & ~CFE_UNDERLINE;
		BOOL ret = ed.SetSelectionCharFormat(format);

		CRichEditView::OnChar(nChar, nRepCnt, nFlags);

		if (theApp.preferences.autoIndent)
		{
			char* buf = new char[(leftParenCount * theApp.preferences.tab) + 1];
			int pos = 0;
			for (int i = 0; i < leftParenCount; i++)
			{
				if (theApp.preferences.replaceTabsWithSpaces)
				{
					for (int j = 0; j < theApp.preferences.tab; j++)
						buf[pos++] = ASCII_SPACE;
				}
				else
					buf[pos++] = ASCII_TAB;
			}
			buf[pos] = 0;
			ed.ReplaceSel(buf, TRUE);
			delete [] buf;
		}
		adjustLispHighlight();
	}
	else
	if (nChar == ASCII_TAB && start != end)		// TAB
	{
		long firstLine = ed.LineFromChar(start);
		long lastLine = ed.LineFromChar(end);
		long saveStart = start;
		long saveEnd = end;
		if (end > 0)
		{
			char ch[4];
			getTextRange(ed, ch, end - 1, end);
			if (ch[0] == ASCII_CR)
				lastLine--;
		}
		start = ed.LineIndex(firstLine);
		end = ed.LineIndex(lastLine + 1) - 1;
		if (end < 0)
			return;
		if (start != saveStart || end != saveEnd)
		{
			ed.SetSel(start, end);
		}
		char* buf = 0;
		if (GetKeyState(VK_SHIFT) < 0)
		{
			// shift-tab
			int bufsize = end - start + 1;
			buf = new char[bufsize];
			char* p = buf;
			p += getTextRange(ed, buf, start, end);
			*p = 0;
			p = buf;
			for (int i = lastLine; i >= firstLine; i--)
			{
				int startOfLine = ed.LineIndex(i);
				p = buf + startOfLine - start;

				if (*p == ASCII_TAB)
				{
					// remove the tab
					for (int j = p - buf; j < bufsize - 1; j++)
					{
						*p = *(p + 1);
						p++;
					}
				}
				else if (*p == ASCII_SPACE)
				{
					// remove spaces
					int numSpaces = 0;
					for (int k = 0; k < theApp.preferences.tab; k++)
						if (*(p + k) == ASCII_SPACE)
							numSpaces++;
					for (int j = p - buf; j < (bufsize - numSpaces); j++)
					{
						*p = *(p + numSpaces);
						p++;
					}
				}
			}
			ed.ReplaceSel(buf, TRUE);
			ed.SetSel(start, start + strlen(buf));
			delete [] buf;
		}
		else
		{
			// tab
			buf = new char[end - start +
					((lastLine - firstLine + 1) * theApp.preferences.tab)
					+ 1];
			char* p = buf;
			for (int i = firstLine, column = 0; i <= lastLine; i++, column = 0)
			{
				int startOfLine = ed.LineIndex(i);
				int endOfLine = ed.LineIndex(i + 1) - 1;
				if (theApp.preferences.replaceTabsWithSpaces)
				{
					*p++ = ASCII_SPACE;
					column++;
					while ((column % theApp.preferences.tab) != 0)
					{
						*p++ = ASCII_SPACE;
						column++;
					}
				}
				else
				{
					*p++ = ASCII_TAB;
				}
				p += getTextRange(ed, p, startOfLine, endOfLine);
				if (i < lastLine)
				{
					*p++ = ASCII_CR;
				}
			}
			*p = 0;
			ed.ReplaceSel(buf, TRUE);
			ed.SetSel(start, start + (p - buf));
			delete [] buf;
		}
	}
	else
	{
		// make sure entered text is not bold
		LispHighlightOff();
		CHARFORMAT format = {sizeof(CHARFORMAT)};
		format.dwMask = CFM_COLOR | CFM_UNDERLINE;
		BOOL ret = ed.GetSelectionCharFormat(format);
		if (format.crTextColor != theApp.preferences.textColor)
		{
			format.dwMask = CFM_COLOR | CFM_UNDERLINE;
			format.crTextColor = theApp.preferences.textColor;
			format.dwEffects = CFE_AUTOCOLOR & ~CFE_UNDERLINE;
			ret = ed.SetSelectionCharFormat(format);
		}

		if (nChar == ASCII_TAB && start == end &&
			theApp.preferences.replaceTabsWithSpaces)
		{
			long line = ed.LineFromChar(start);
			long startOfLine = ed.LineIndex(line);
			int column = start - startOfLine;
			int spaces = 1;
			column++;		// insert at least one space
			while ((column % theApp.preferences.tab) != 0)
			{
				column++;
				spaces++;
			}
			char* buf = new char[spaces + 1];
			for (int i = 0; i < spaces; i++)
				buf[i] = ASCII_SPACE;
			buf[spaces] = 0;
			ed.ReplaceSel(buf, TRUE);
			ed.SetSel(start + spaces, start + spaces);
			delete [] buf;
		}
		else
		{
			CRichEditView::OnChar(nChar, nRepCnt, nFlags);
		}
		adjustLispHighlight();

		if (nChar == ' ' && theApp.preferences.autoPrototypeOnKeyDown)
		{
			// find the current position
			ed.GetSel(start, end);
			CPoint p = PosFromChar(start - 2);
			mouseCueOn(p);
			displayMouseCue();
		}
	}
	m_colorizeDisabled = false;
}

long getTextRange(CRichEditCtrl& ed, char* buf, long start, long end)
{
	TEXTRANGE textRange;
	textRange.chrg.cpMin = start;
	textRange.chrg.cpMax = end;
	textRange.lpstrText = buf;
	return ed.SendMessage(EM_GETTEXTRANGE, 0,
			(LPARAM)&textRange);
}

void CLispView::adjustLispHighlight()
{
	CRichEditCtrl& ed = GetRichEditCtrl();
	long start, end;
	ed.GetSel(start, end);
	long line = ed.LineFromChar(end);
	theApp.SetLineNumber(line + 1);
	theApp.SetColumnNumber(calculateColumnNumber(line, end) + 1);

	if (m_lispHighlightStart == start && m_lispHighlightEnd == end
		&& m_highlightOn)
		return;

	if (!theApp.preferences.parenthesesMatching)
	{
		LispHighlightOff();
		return;
	}

	if (start != end)
	{
		LispHighlightOff();
		return;		// skip unless it is a vertical bar cursor
	}

	LockWindowUpdate();
	ed.LockWindowUpdate();
//	ed.SetOptions(ECOOP_XOR, ECO_AUTOHSCROLL|ECO_AUTOVSCROLL);	// turn off auto scrolling

	// find the preceding character

	char buf[4];
	char str[MaxScanLength + 1];
	long numChars = getTextRange(ed, buf, end - 1, end);

	if (buf[0] == ')')
	{
		int firstPos = max((end - 1) - MaxScanLength, 0);
		numChars = getTextRange(ed, str, firstPos, end - 1);

		// go back looking for start of expression
		int parenCount = 1;
		int index = end - 2;
		while (index >= firstPos)
		{
			if (str[index - firstPos] == ')')
				parenCount++;
			else
			if (str[index - firstPos] == '(')
			{
				parenCount--;
				if (!parenCount)
				{
					SetLispHighlight(index, start - 1);
					break;
				}
			}
			index--;
		}
		if (index < firstPos)
			SetLispHighlight(-1, -1);
	}
	else
	{
		// look for an expression to the right
		// find the next character
		numChars = getTextRange(ed, buf, end, end + 1);
		if (numChars && buf[0] == '(')
		{
			int lastPos = end + 1 + MaxScanLength;
			numChars = getTextRange(ed, str, end + 1, lastPos);

			// go back looking for start of expression
			int parenCount = 1;
			int index = 0;
			while (index < numChars)
			{
				if (str[index] == '(')
					parenCount++;
				else
				if (str[index] == ')')
				{
					parenCount--;
					if (!parenCount)
					{
						SetLispHighlight(end, index + end + 1);
						break;
					}
				}
				index++;
			}
			if (index == numChars)
				SetLispHighlight(-1, -1);
		}
		else
			SetLispHighlight(-1, -1);
	}

//	ed.ValidateRect(NULL);
//	ValidateRect(NULL);
//	ed.SetOptions(ECOOP_OR, ECO_AUTOHSCROLL|ECO_AUTOVSCROLL);	// turn on auto scrolling
	ed.UnlockWindowUpdate();
	UnlockWindowUpdate();
}

void CLispView::SetLispHighlight(long start, long end)
{
	if (start == -1 && end == -1)
	{
		LispHighlightOff();
		m_lispHighlightStart = start;
		m_lispHighlightEnd = end;
		return;
	}
	else
	if (m_lispHighlightStart != start ||
		m_lispHighlightEnd != end)
	{
		LispHighlightOff();
		m_lispHighlightStart = start;
		m_lispHighlightEnd = end;
	}
	LispHighlightOn();
}

void CLispView::highlightChar(CRichEditCtrl& ed, long position)
{
	CDC* dc = ed.GetDC();
	long start, end;
	ed.GetSel(start, end);
	if (start != end)
		return;		// don't do anything if a block of text is highlighted (inverse)

	HDC hDC = dc->m_hDC;
	CFont* font = theApp.getDefaultUnderlineFont(hDC, theApp.preferences.charSize);
	SelectObject(hDC, *font);

	CPoint p = ed.GetCharPos(position);
	CRect rect(p.x, p.y, p.x + 80, p.y + 100);
	char buf[4];
	long numChars = getTextRange(ed, buf, position, position + 1);
	if (numChars > 0)
	{
		COLORREF saveColor = dc->SetTextColor(theApp.preferences.highlightTextColor);
		buf[1] = 0;
		dc->DrawText(buf, -1, rect, 0);
		dc->SetTextColor(saveColor);
	}

	hDC = dc->m_hDC;
	font = theApp.getDefaultFont(hDC, theApp.preferences.charSize);
	SelectObject(hDC, *font);
	ReleaseDC(dc);
}

void CLispView::LispHighlightOn()
{
	if (m_highlightOn)
		return;

	CRichEditCtrl& ed = GetRichEditCtrl();
	RECT rect;
	CPoint p = ed.GetCharPos(m_lispHighlightStart);
	rect.left = p.x;
	rect.top = p.y;
	rect.bottom = rect.top + 20;
	rect.right = rect.left + 20;
	InvalidateRect(&rect, false);
	p = ed.GetCharPos(m_lispHighlightEnd);
	rect.left = p.x;
	rect.top = p.y;
	rect.bottom = rect.top + 20;
	rect.right = rect.left + 20;
	InvalidateRect(&rect, false);

	m_highlightOn = true;
}

void CLispView::LispHighlightOff()
{
	if (!m_highlightOn)
		return;

	CRichEditCtrl& ed = GetRichEditCtrl();
	RECT rect;
	CPoint p = ed.GetCharPos(m_lispHighlightStart);
	rect.left = p.x;
	rect.top = p.y;
	rect.bottom = rect.top + 20;
	rect.right = rect.left + 20;
	InvalidateRect(&rect, false);
	p = ed.GetCharPos(m_lispHighlightEnd);
	rect.left = p.x;
	rect.top = p.y;
	rect.bottom = rect.top + 20;
	rect.right = rect.left + 20;
	InvalidateRect(&rect, false);

	m_highlightOn = false;
}

void
CLispView::outputText(const CString& s)
{
	mouseCueOff();
	CRichEditCtrl& ed = GetRichEditCtrl();
	LispHighlightOff();
	int lastCallWasRead = readingFromConsole;
	readingFromConsole = 0;
	long startChar, endChar;
	long saveStart, saveEnd;
	BOOL ret;
	CHARFORMAT format = {sizeof(CHARFORMAT)};
	ed.GetSel(startChar, endChar);
	m_firstModified = min(startChar, m_firstModified);

	long outputLine;
	CString lineEnd("\r\n");
	long outputLineLength;
	long outputLinePos;

	if (theApp.preferences.appendLispOutputToEnd)
	{
		outputLine = ed.GetLineCount() - 1;
		outputLinePos = ed.LineIndex(outputLine);
		outputLineLength = ed.LineLength(outputLinePos);
	}
	else
	{
		outputLine = ed.LineFromChar(endChar);
		outputLinePos = ed.LineIndex(outputLine);
		outputLineLength = ed.LineLength(endChar);
	}
	startChar = outputLinePos + outputLineLength;
	ed.SetSel(outputLinePos + outputLineLength, outputLinePos + outputLineLength);
	if (outputLineLength > 0)
	{
		if (lastCallWasRead)
			ed.ReplaceSel(lineEnd, TRUE);
		format.dwMask = CFM_COLOR | CFM_UNDERLINE;
		format.crTextColor = theApp.preferences.outputTextColor;
		format.dwEffects = 0;
		ret = ed.SetSelectionCharFormat(format);
		ed.GetSel(saveStart, saveEnd);
		ed.SetSel(saveEnd, saveEnd);
		ed.ReplaceSel(s, TRUE);
	}
	else
	{
		ed.ReplaceSel(s, TRUE);
		endChar = startChar + s.GetLength();

		ed.GetSel(saveStart, saveEnd);
		format.dwMask = CFM_COLOR | CFM_UNDERLINE;
		format.crTextColor = theApp.preferences.outputTextColor;
		format.dwEffects = 0;
		ed.SetSel(startChar, endChar);
		ret = ed.SetSelectionCharFormat(format);
		ed.SetSel(saveStart, saveEnd);
	}
	adjustLispHighlight();
	m_colorizeDisabled = false;
}

void CLispView::replaceSelection(const CString& s)
{
	mouseCueOff();
	CRichEditCtrl& ed = GetRichEditCtrl();
	LispHighlightOff();
	ed.ReplaceSel(s, TRUE);
	adjustLispHighlight();
}

void
CLispView::SetTabStops(long numChars)
{
	// save modified setting--this function should not affect that
	CLispDoc* doc = (CLispDoc*)GetDocument();
	BOOL modified = doc->IsModified();

	theApp.preferences.tab = numChars;
	long saveSelStart, saveSelEnd;
	CRichEditCtrl& edit = GetRichEditCtrl();
	edit.GetSel(saveSelStart, saveSelEnd);
	edit.SetSel(0, -1);
	PARAFORMAT2 para;
	memset(&para, 0, sizeof(PARAFORMAT2));
	para.cbSize = sizeof(PARAFORMAT2);
	para.cTabCount = MAX_TAB_STOPS;
	para.dxStartIndent = 100;

	long tab = para.dxStartIndent;
	CDC* cdc = GetDC();
	HDC hDC = cdc->m_hDC;

	activateFont(hDC);

	int logx = ::GetDeviceCaps(hDC, LOGPIXELSX);
	CSize size;
	CString spaces(' ', numChars);
	BOOL ret1 = GetTextExtentPoint32(hDC, (const char*)spaces, numChars, &size);
	ReleaseDC(cdc);

	int advance = (1440 / logx) * size.cx;
	for (long i = 0; i < para.cTabCount; i++)
	{
		tab += advance;
		para.rgxTabs[i]	= tab;
	}
	para.dwMask = PFM_TABSTOPS|PFM_STARTINDENT;
	edit.SetParaFormat(para);

	edit.SetSel(saveSelStart, saveSelEnd);

	// restore modified setting
	doc->SetModifiedFlag(modified);
}

void
CLispView::SetCharSize(long pointSize)
{
	// save modified setting--this function should not affect that
	CLispDoc* doc = (CLispDoc*)GetDocument();
	BOOL modified = doc->IsModified();

	LispHighlightOff();

	theApp.preferences.charSize = pointSize;

	long saveSelStart, saveSelEnd;
	CRichEditCtrl& ed = GetRichEditCtrl();
	ed.GetSel(saveSelStart, saveSelEnd);
	ed.SetSel(0, -1);

	CDC* cdc = GetDC();
	HDC hDC = cdc->m_hDC;
	int height = -MulDiv(pointSize, GetDeviceCaps(hDC, LOGPIXELSY), 72);
	CHARFORMAT cf = {sizeof(CHARFORMAT)};

	ed.GetSelectionCharFormat(cf);
	cf.yHeight = height;
	cf.dwMask = CFM_SIZE;
	::SendMessage(m_hWnd, EM_SETCHARFORMAT, SCF_ALL, (LPARAM)&cf);

	activateFont(hDC);
	ReleaseDC(cdc);
	ed.SetSel(saveSelStart, saveSelEnd);
	ed.EmptyUndoBuffer();
	adjustLispHighlight();

	// restore modified setting
	doc->SetModifiedFlag(modified);
}

void CLispView::SetTextColor(COLORREF color)
{
	// save modified setting--this function should not affect that
	CLispDoc* doc = (CLispDoc*)GetDocument();
	BOOL modified = doc->IsModified();

	LispHighlightOff();

	theApp.preferences.textColor = color;

	long saveSelStart, saveSelEnd;
	CRichEditCtrl& ed = GetRichEditCtrl();
	ed.GetSel(saveSelStart, saveSelEnd);
	ed.SetSel(0, -1);

	CHARFORMAT cf = {sizeof(CHARFORMAT)};

	ed.GetSelectionCharFormat(cf);
	cf.crTextColor = color;
	cf.dwMask = CFM_COLOR;
	cf.dwEffects &= ~CFE_AUTOCOLOR;		// turn off autocolor
	::SendMessage(m_hWnd, EM_SETCHARFORMAT, SCF_ALL, (LPARAM)&cf);

	ed.SetSel(saveSelStart, saveSelEnd);
	ed.EmptyUndoBuffer();
	adjustLispHighlight();

	// restore modified setting
	doc->SetModifiedFlag(modified);
}

long CLispView::CharFromPos(CPoint pt)
{
	CRichEditCtrl& edit = GetRichEditCtrl();
	POINTL p;
	p.x = pt.x;
	p.y = pt.y;
	return (long)::SendMessage(edit.m_hWnd, EM_CHARFROMPOS, 0, (long)&p);
}

CPoint CLispView::PosFromChar(long ch)
{
	CRichEditCtrl& edit = GetRichEditCtrl();
	CPoint p;
	POINTL p1;
	long ret = (long)::SendMessage(edit.m_hWnd, EM_POSFROMCHAR, (long)&p1, ch);
	p.x = p1.x;
	p.y = p1.y;
	return p;
}

bool isLispTokenChar(int ch)
{
	if (ch < 0)
		ch += 255;		// should be in range 0-255
	if (isalnum(ch))
		return true;
	if (ch == '-' || ch == '.' || ch == '%' || ch == '&' || ch == '*' || ch == '+'
		|| ch == ':' || ch == '/' || ch == '=' || ch == '<' || ch == '>')
		return true;
	return false;
}

void CLispView::displayMouseCue()
{
	CRichEditCtrl& edit = GetRichEditCtrl();
	// find the text under the mouse
	int pos;
	pos = CharFromPos(m_mouseCuePosition);
	int line = edit.LineFromChar(pos);
	int lineStart = edit.LineIndex(line);
	int column = pos - lineStart;
	int lineLength = max(edit.LineLength(lineStart), 4);
	edit.GetLine(line, LineBuf, LineBufSize);
	LineBuf[LineBufSize - 1] = 0;
	int startChar = column;
	while (isLispTokenChar(LineBuf[startChar]) && startChar > 0)
		startChar--;
	if (!isLispTokenChar(LineBuf[startChar]))
		startChar++;
	int endChar = column;
	while (isLispTokenChar(LineBuf[endChar]))
		endChar++;

	if (startChar > endChar || ((startChar == endChar) && !isLispTokenChar(LineBuf[startChar])))
	{
		m_mouseCueRect = CRect(0, 0, 0, 0);
		return;
	}
	LineBuf[endChar] = 0;
	const char* lambdaList = findLambdaList(LineBuf + startChar);
	static char lambdaListBuf[1024];
	int lambdaListBufSize = 1024;
	if (!lambdaList)
	{
		// see if it is a lisp user-defined function
		if (!LookupLambdaListPtr)
			SetupDirectCallPointers();
		if (LookupLambdaListPtr)
		{
			int chars = LookupLambdaListPtr(LineBuf + startChar, lambdaListBuf, lambdaListBufSize);
			if (!chars)
			{
				return;
			}
			else
				lambdaList = lambdaListBuf;
		}
	}
	if (!lambdaList)
	{
		return;
	}

	CDC* dc = GetDC();
	if (!dc)
	{
		return;
	}
	CFont* font = theApp.getCourierFont(dc->m_hDC, 10);
	HGDIOBJ orig = SelectObject(dc->m_hDC, *font);
	CSize size;
	BOOL ret = GetTextExtentPoint32(dc->m_hDC, lambdaList,
		strlen(lambdaList), &size);
	CRect rect(m_mouseCuePosition.x + 10 - 2,
			  m_mouseCuePosition.y + 20 - 2,
			  m_mouseCuePosition.x + 10 + size.cx + 2,
			  m_mouseCuePosition.y + 20 + size.cy + 2);
	RECT bounds;
	GetClientRect(&bounds);
	if (rect.bottom > bounds.bottom)
	{
		rect.top -= 50;
		rect.bottom -= 50;	// display above instead of below to avoid getting cut off
	}

	dc->FillSolidRect(rect, theApp.preferences.hintBackgroundColor);
	FrameRect(dc->m_hDC, rect, theApp.m_blackBrush);
	dc->TextOut(rect.left + 2, rect.top + 2, lambdaList, strlen(lambdaList));
	m_mouseCueRect = rect;
	SelectObject(dc->m_hDC, orig);
	ReleaseDC(dc);
}

void CLispView::undisplayMouseCue()
{
	InvalidateRect(m_mouseCueRect, TRUE);
}

void CLispView::mouseCueOn(const CPoint& point)
{
	if (!m_mouseCueDisplayed && !mouseCueDisabled())
	{
		m_mouseCuePosition = point;
		m_mouseCueDisplayed = true;
	}
}

void CLispView::mouseCueOff()
{
	if (m_mouseCueDisplayed)
	{
		m_mouseCueDisplayed = false;
		undisplayMouseCue();
	}
}

class _afxRichEditCookie
{
public:
	CArchive& m_ar;
	DWORD m_dwError;
	_afxRichEditCookie(CArchive& ar) : m_ar(ar) {m_dwError=0;}
};

void
CLispView::Serialize(CArchive& ar)
{
	ASSERT_VALID(this);
	ASSERT(m_hWnd != NULL);
	StreamText(ar, FALSE);
	ASSERT_VALID(this);
}

void
CLispView::StreamText(CArchive& ar, BOOL bSelection)
{
	EDITSTREAM es = {0, 0, EditStreamCallBack};
	_afxRichEditCookie cookie(ar);
	es.dwCookie = (DWORD)&cookie;
	int nFormat = SF_TEXT;
	CFile* file = ar.GetFile();
	CString path;
	if (file)
		path = file->GetFileName();
	if (!path.Right(3).CompareNoCase("RTF"))
		nFormat = SF_RTF;		// use RTF format if RTF extension
	if (bSelection)
		nFormat |= SFF_SELECTION;
	if (ar.IsStoring())
		GetRichEditCtrl().StreamOut(nFormat, es);
	else
	{
		GetRichEditCtrl().StreamIn(nFormat, es);
		Invalidate();
	}
	if (cookie.m_dwError != 0)
		AfxThrowFileException(cookie.m_dwError);
}

/////////////////////////////////////////////////////////////////////////////
// CLispView printing

void CLispView::OnPrint(CDC* pDC, CPrintInfo* pInfo)
{
	CRichEditView::OnPrint(pDC, pInfo);
	if (pInfo != NULL && pInfo->m_bPreview)
		DrawMargins(pDC);
}

void CLispView::DrawMargins(CDC* pDC)
{
	if (pDC->m_hAttribDC != NULL)
	{
		CRect rect;
		rect.left = m_rectMargin.left;
		rect.right = m_sizePaper.cx - m_rectMargin.right;
		rect.top = m_rectMargin.top;
		rect.bottom = m_sizePaper.cy - m_rectMargin.bottom;
		//rect in twips
		int logx = ::GetDeviceCaps(pDC->m_hDC, LOGPIXELSX);
		int logy = ::GetDeviceCaps(pDC->m_hDC, LOGPIXELSY);
		rect.left = MulDiv(rect.left, logx, 1440);
		rect.right = MulDiv(rect.right, logx, 1440);
		rect.top = MulDiv(rect.top, logy, 1440);
		rect.bottom = MulDiv(rect.bottom, logy, 1440);
		CPen pen(PS_DOT, 0, pDC->GetTextColor());
		CPen* ppen = pDC->SelectObject(&pen);
		pDC->MoveTo(0, rect.top);
		pDC->LineTo(10000, rect.top);
		pDC->MoveTo(rect.left, 0);
		pDC->LineTo(rect.left, 10000);
		pDC->MoveTo(0, rect.bottom);
		pDC->LineTo(10000, rect.bottom);
		pDC->MoveTo(rect.right, 0);
		pDC->LineTo(rect.right, 10000);
		pDC->SelectObject(ppen);
	}
}

void CLispView::OnFilePrint()
{
	// don't allow winini changes to occur while printing
	m_bInPrint = TRUE;
	CRichEditView::OnFilePrint();
	// printer may have changed
	theApp.NotifyPrinterChanged(); // this will cause a GetDocument()->PrinterChanged();
	m_bInPrint = FALSE;
}

LONG CLispView::OnPrinterChangedMsg(UINT, LONG)
{
	CDC dc;
	AfxGetApp()->CreatePrinterDC(dc);
	OnPrinterChanged(dc);
	return 0;
}

BOOL CLispView::OnPreparePrinting(CPrintInfo* pInfo)
{
	return DoPreparePrinting(pInfo);
}

inline int roundleast(int n)
{
	int mod = n%10;
	n -= mod;
	if (mod >= 5)
		n += 10;
	else if (mod <= -5)
		n -= 10;
	return n;
}

static void RoundRect(LPRECT r1)
{
	r1->left = roundleast(r1->left);
	r1->right = roundleast(r1->right);
	r1->top = roundleast(r1->top);
	r1->bottom = roundleast(r1->bottom);
}

static void MulDivRect(LPRECT r1, LPRECT r2, int num, int div)
{
	r1->left = MulDiv(r2->left, num, div);
	r1->top = MulDiv(r2->top, num, div);
	r1->right = MulDiv(r2->right, num, div);
	r1->bottom = MulDiv(r2->bottom, num, div);
}

void CLispView::OnPageSetup()
{
	CPageSetupDialog dlg;
	PAGESETUPDLG& psd = dlg.m_psd;
	BOOL bMetric = theApp.GetUnits() == 1; //centimeters
	psd.Flags |= PSD_MARGINS | (bMetric ? PSD_INHUNDREDTHSOFMILLIMETERS :
		PSD_INTHOUSANDTHSOFINCHES);
	int nUnitsPerInch = bMetric ? 2540 : 1000;
	MulDivRect(&psd.rtMargin, m_rectMargin, nUnitsPerInch, 1440);
	RoundRect(&psd.rtMargin);
	// get the current device from the app
	PRINTDLG pd;
	pd.hDevNames = NULL;
	pd.hDevMode = NULL;
	theApp.GetPrinterDeviceDefaults(&pd);
	psd.hDevNames = pd.hDevNames;
	psd.hDevMode = pd.hDevMode;
	if (dlg.DoModal() == IDOK)
	{
		RoundRect(&psd.rtMargin);
		MulDivRect(m_rectMargin, &psd.rtMargin, 1440, nUnitsPerInch);
		theApp.m_rectPageMargin = m_rectMargin;
		theApp.SelectPrinter(psd.hDevNames, psd.hDevMode);
		theApp.NotifyPrinterChanged();
	}
	// PageSetupDlg failed
	if (CommDlgExtendedError() != 0)
	{
		CPageSetupDlg dlg;
		dlg.m_nBottomMargin = m_rectMargin.bottom;
		dlg.m_nLeftMargin = m_rectMargin.left;
		dlg.m_nRightMargin = m_rectMargin.right;
		dlg.m_nTopMargin = m_rectMargin.top;
		if (dlg.DoModal() == IDOK)
		{
			m_rectMargin.SetRect(dlg.m_nLeftMargin, dlg.m_nTopMargin,
				dlg.m_nRightMargin, dlg.m_nBottomMargin);
			// m_page will be changed at this point
			theApp.m_rectPageMargin = m_rectMargin;
			theApp.NotifyPrinterChanged();
		}
	}
}

void CLispView::OnExecuteSelection()
{
	CRichEditCtrl& ed = GetRichEditCtrl();

	// Numeric Enter or Shift-Enter was pressed
	bool lispHighlightWasOn = false;
	long saveSelStart, saveSelEnd;

	if (m_highlightOn)		// if there is a highlighted lisp expression
	{
		// select it for 100 ms or so, then execute it
		lispHighlightWasOn = true;
		ed.GetSel(saveSelStart, saveSelEnd);
		long lispHighlightStart = m_lispHighlightStart;
		long lispHighlightEnd = m_lispHighlightEnd;
		LispHighlightOff();
		ed.SetSel(lispHighlightStart, lispHighlightEnd + 1);
		ed.HideSelection(FALSE, FALSE);
		ed.UpdateWindow();
		delay(100);
	}
	collectInputText(ed, *this);
	if (lispHighlightWasOn)
		ed.SetSel(saveSelStart, saveSelEnd);
}

void setCheck(CCmdUI* cmd, bool b) { cmd->Enable(TRUE); cmd->SetCheck(b); }

void CLispView::OnGotoLine()
{
	CRichEditCtrl& ed = GetRichEditCtrl();
	GotoLineDialog gotoLineDialog(IDD_GOTO_LINE);
	int ret = gotoLineDialog.DoModal();
	if (ret == IDOK)
	{
		long num = gotoLineDialog.lineNumber();
		int linepos;
		if (num >= 0)
		{
			linepos = ed.LineIndex((int)num - 1);
			if (linepos != -1)
			{
				ed.SetSel(linepos, linepos);
			}
			else
			{
				linepos = ed.LineIndex(ed.GetLineCount());
				ed.SetSel(linepos, linepos);
			}
			adjustLispHighlight();
		}
	}
}

// Returns:
//	true if last input from keyoard,
//	false if last input from mouse
bool CLispView::usingKeyboard()
{
	return m_usingKeyboard;
}

int CLispView::mouseCueDisabled()
{
	return m_mouseCueDisabled;
}

void CLispView::OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar)
{
	mouseCueOff();
	CWnd::OnHScroll(nSBCode, nPos, pScrollBar);
}

void CLispView::OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar)
{
	mouseCueOff();
/*
	int offset = nPos % m_lineHeight;
	if (offset != 0)
	{
		nPos = nPos / m_lineHeight * m_lineHeight;
		SetScrollPos(SB_VERT, nPos, FALSE);
//		ScrollWindow(0, -offset, NULL, NULL);
	}
*/
	CWnd::OnVScroll(nSBCode, nPos, pScrollBar);
}

void CLispView::UpdateScrollPosition(UINT nPos)
{
	CScrollBar* pScrollBar = GetScrollBarCtrl(SB_VERT);
	SCROLLINFO si;
    si.cbSize = sizeof (si);
    si.fMask  = SIF_ALL;
    GetScrollInfo(SB_VERT, &si);
	int offset = nPos % m_lineHeight;
	if (offset != 0)
	{
	//	ScrollWindow(0, -offset, NULL, NULL);
		SetScrollPos(SB_VERT, -offset);
	}
}
static int LispSystemDisabled = 0;

void DisableLispSystem()
{
	LispSystemDisabled = 1;
	OnContextMenuFuncPtr = 0;
	OnHeapSizePtr = 0;
	LookupLambdaListPtr = 0;
	OnColorizeFuncPtr = 0;
    DisplayLispVarsPtr = 0;
    InitMenuPtr = 0;
    InitMenuPopupPtr = 0;
    UninitMenuPopupPtr = 0;
    MenuSelectPtr = 0;
    VersionCaptionPtr = 0;

	pCormanLisp = 0;
	pCormanLispDirectCall = 0;
}

void SetupDirectCallPointers()
{
	if (OnContextMenuFuncPtr == 0 && !LispSystemDisabled)
	{
		pCormanLispDirectCall->BlessThread();
		void* funcptr = 0;
		pCormanLispDirectCall->GetFunctionAddress(L"ON-CONTEXT-MENU", L"CCL", &funcptr);
		if (funcptr)
			OnContextMenuFuncPtr = (OnContextMenuFuncType)funcptr;
		else
			LispSystemDisabled = 1;		// the image is probably not valid or loaded
		pCormanLispDirectCall->GetFunctionAddress(L"ON-HEAP-SIZE", L"CCL", &funcptr);
		if (funcptr)
			OnHeapSizePtr = (OnHeapSizeType)funcptr;
		pCormanLispDirectCall->GetFunctionAddress(L"LOOKUP-LAMBDA-LIST", L"CCL", &funcptr);
		if (funcptr)
			LookupLambdaListPtr = (LookupLambdaListType)funcptr;
		pCormanLispDirectCall->GetFunctionAddress(L"ON-COLORIZE", L"IDE", &funcptr);
		if (funcptr)
			OnColorizeFuncPtr = (OnColorizeType)funcptr;
		pCormanLispDirectCall->GetFunctionAddress(L"DISPLAY-LISP-VARS", L"IDE", &funcptr);
		if (funcptr)
			DisplayLispVarsPtr = (DisplayLispVarsType)funcptr;
		pCormanLispDirectCall->GetFunctionAddress(L"ON-INIT-MENU", L"IDE", &funcptr);
		if (funcptr)
			InitMenuPtr = (InitMenuType)funcptr;
		pCormanLispDirectCall->GetFunctionAddress(L"ON-INIT-MENU-POPUP", L"IDE", &funcptr);
		if (funcptr)
			InitMenuPopupPtr = (InitMenuPopupType)funcptr;
		pCormanLispDirectCall->GetFunctionAddress(L"ON-UNINIT-MENU-POPUP", L"IDE", &funcptr);
		if (funcptr)
			UninitMenuPopupPtr = (UninitMenuPopupType)funcptr;
		pCormanLispDirectCall->GetFunctionAddress(L"ON-MENU-SELECT", L"IDE", &funcptr);
		if (funcptr)
			MenuSelectPtr = (MenuSelectType)funcptr;
		pCormanLispDirectCall->GetFunctionAddress(L"VERSION_CAPTION", L"CCL", &funcptr);
		if (funcptr)
			VersionCaptionPtr = (VersionCaptionType)funcptr;
	}
}

void CLispView::OnContextMenu(CWnd* wnd, CPoint point)
{
	CRichEditCtrl& ed = GetRichEditCtrl();
	SetupDirectCallPointers();
	if (OnContextMenuFuncPtr)
	{
		long start, end;
		ed.GetSel(start, end);

		// if there is no text selected, select the symbol under the caret
		if (start == end)
		{
			if (m_highlightOn)
				ed.SetSel(m_lispHighlightStart, m_lispHighlightEnd + 1);
			else
			{
				int line = ed.LineFromChar(start);
				int lineStart = ed.LineIndex(line);
				int column = start - lineStart;
				int lineLength = ed.LineLength(lineStart);
				if (lineLength == 0)
					return;
				ed.GetLine(line, LineBuf, sizeof(LineBuf));
				LineBuf[lineLength] = 0;
				int startChar = column;
				while (isLispTokenChar(LineBuf[startChar]) && startChar > 0)
					startChar--;
				if (!isLispTokenChar(LineBuf[startChar]))
					startChar++;
				int endChar = column;
				while (isLispTokenChar(LineBuf[endChar]))
					endChar++;

				if (startChar <= endChar &&
					((startChar < endChar) || isLispTokenChar(LineBuf[startChar])))
					ed.SetSel(lineStart + startChar, lineStart + endChar);
			}
		}

		long result = OnContextMenuFuncPtr(point.x, point.y, (void*)wnd->m_hWnd);
	}
}

 
void CLispView::OnActivate(UINT nState, CWnd* pWndOther, BOOL bMinimized)
{
	CRichEditView::OnActivate(nState, pWndOther, bMinimized);
}
 

IMPLEMENT_DYNCREATE(CLispDocumentFrame, CSMDIChildWnd)
BEGIN_MESSAGE_MAP(CLispDocumentFrame, CSMDIChildWnd)
	ON_WM_SHOWWINDOW()
END_MESSAGE_MAP()

BOOL CLispDocumentFrame::PreCreateWindow(CREATESTRUCT& cs)
{
	if (!AfxInitRichEdit2())
		return FALSE;
	return CSMDIChildWnd::PreCreateWindow(cs);
}

void CLispDocumentFrame::OnShowWindow(BOOL bShow, UINT nStatus)
{
	// set the title to be the full path
	CDocument* doc = GetActiveDocument();
	if (theApp.preferences.fullPathInTitle && _stricmp(doc->GetTitle(), WORKSHEET_TITLE))
	{
		CString pathName = doc->GetPathName();
		if (pathName.GetLength() > 0)
			doc->SetTitle(pathName);
	}
	CSMDIChildWnd::OnShowWindow(bShow, nStatus);
}

void CLispDocumentFrame::OnClose()
{
	CString title;
	GetWindowText(title);
	if (!title.CompareNoCase(WORKSHEET_TITLE) || theApp.AppIsClosing())
		CSMDIChildWnd::OnClose();
}

CFont* getDefaultFont(HDC hDC, long pointSize)
{
	return theApp.getDefaultFont(hDC, pointSize);
}

// LispDialogBar
BEGIN_MESSAGE_MAP(LispDialogBar, CDialogBar)
	ON_WM_PAINT()
	ON_WM_DRAWITEM()
END_MESSAGE_MAP()

afx_msg void LispDialogBar::OnPaint()
{
	CDialogBar::OnPaint();
}

void LispDialogBar::OnDrawItem(int nIDCtl, LPDRAWITEMSTRUCT lpDrawItemStruct)
{
	if (!OnHeapSizePtr)
		return;
	RECT rect = lpDrawItemStruct->rcItem;
	rect.left += 2; rect.top += 2; rect.right -= 2; rect.bottom -= 2;
	RECT totalRect = rect;
	long width = rect.right - rect.left;
	long capacity = 0;
	long used = 0;
	int generation;
	if (nIDCtl == IDC_LISPHEAP_0)
		generation = 0;
	else if (nIDCtl == IDC_LISPHEAP_1)
		generation = 1;
	else
		generation = 2;
	long percent = 0;
	if (OnHeapSizePtr)
		percent = OnHeapSizePtr(generation, &capacity, &used);
	rect.right = rect.left + width * percent / 100;
    CDC dc;
    dc.Attach(lpDrawItemStruct->hDC);
	CBrush brush;
	brush.CreateSolidBrush(RGB(0,0,0));
	RECT rect2 = lpDrawItemStruct->rcItem;
	rect2.left += 1; rect2.top += 1; rect2.right -= 1; rect2.bottom -= 1;
	dc.FrameRect(&rect2, &brush);
	dc.FillSolidRect(&rect, RGB(255, 0, 0));
}

// RegistrationDialogBar
BEGIN_MESSAGE_MAP(RegistrationDialogBar, CDialogBar)
	ON_WM_PAINT()
	ON_COMMAND(IDOK, OnOK)
	ON_UPDATE_COMMAND_UI(IDOK, OnUpdateOK)
END_MESSAGE_MAP()

void RegistrationDialogBar::OnPaint()
{
	CDialogBar::OnPaint();
}

void RegistrationDialogBar::OnOK()
{
	theApp.NavigateURL("http://www.cormanlisp.com/license_2_0.html");
}

void RegistrationDialogBar::OnUpdateOK(CCmdUI* pCmdUI)
{
	pCmdUI->Enable(TRUE);
}

void CMainFrame::DockControlBarLeftOf(CControlBar* Bar, CControlBar* LeftOf)
{
	// get MFC to adjust the dimensions of all docked ToolBars
	// so that GetWindowRect will be accurate
	RecalcLayout(TRUE);
	CRect rect;
	LeftOf->GetWindowRect(&rect);
	rect.OffsetRect(1,0);
	DWORD dw=LeftOf->GetBarStyle();
	UINT n = 0;
	n = (dw&CBRS_ALIGN_TOP) ? AFX_IDW_DOCKBAR_TOP : n;
	n = (dw&CBRS_ALIGN_BOTTOM && n==0) ? AFX_IDW_DOCKBAR_BOTTOM : n;
	n = (dw&CBRS_ALIGN_LEFT && n==0) ? AFX_IDW_DOCKBAR_LEFT : n;
	n = (dw&CBRS_ALIGN_RIGHT && n==0) ? AFX_IDW_DOCKBAR_RIGHT : n;

	// When we take the default parameters on rect, DockControlBar will dock
	// each Toolbar on a seperate line. By calculating a rectangle, we
	// are simulating a Toolbar being dragged to that location and docked.
	DockControlBar(Bar,n,&rect);
}

void CMainFrame::OnInitMenu(CMenu* pMenu) {
	this->CSMDIFrameWnd::OnInitMenu(pMenu);
	if (!InitMenuPtr)
		SetupDirectCallPointers();
	if (InitMenuPtr)
		InitMenuPtr(pMenu->m_hMenu);
}

void CMainFrame::OnInitMenuPopup(CMenu* pPopupMenu, UINT nIndex, BOOL bSysMenu)
{
    gLastMenuHandle = pPopupMenu->m_hMenu;
	this->CSMDIFrameWnd::OnInitMenuPopup(pPopupMenu, nIndex, bSysMenu);
	if (!InitMenuPopupPtr)
		SetupDirectCallPointers();
	if (InitMenuPopupPtr)
		InitMenuPopupPtr(pPopupMenu->m_hMenu, nIndex, bSysMenu);
}

LRESULT CMainFrame::OnUninitMenuPopup(WPARAM mHandle, LPARAM /*lParam*/)
{
	if (!UninitMenuPopupPtr)
		SetupDirectCallPointers();
	if (UninitMenuPopupPtr)
		UninitMenuPopupPtr((HMENU)mHandle);
    return 0;
}

void CMainFrame::OnMenuSelect(UINT nItemID, UINT nFlags, HMENU hSysMenu)
{
    gLastMenuItem = nItemID;
	this->CSMDIFrameWnd::OnMenuSelect(nItemID, nFlags, hSysMenu);
	if (!MenuSelectPtr)
		SetupDirectCallPointers();
	if (MenuSelectPtr)
		MenuSelectPtr(gLastMenuHandle, nItemID, nFlags);
}

// LispVarsDialogBar
BEGIN_MESSAGE_MAP(LispVarsDialogBar, CDialogBar)
	ON_WM_PAINT()
    ON_WM_SIZE()
END_MESSAGE_MAP()

BOOL LispVarsDialogBar::Create( CWnd* pParentWnd, UINT nIDTemplate,
                              UINT nStyle, UINT nID, BOOL bChange)
{
    if(!CDialogBar::Create(pParentWnd,nIDTemplate,nStyle,nID))
         return FALSE;

    m_bChangeDockedSize = bChange;
    m_sizeFloating = m_sizeDocked = m_sizeDefault;
    return TRUE;
}

void LispVarsDialogBar::OnPaint()
{
	CWnd* textBox = GetDlgItem(ID_VIEW_LISPVARS_TEXT);
    if (textBox != 0)
    {
        textBox->SetWindowText(lispVarsBuf);
    }
	CDialogBar::OnPaint();

#if 0
    RECT rect;
    this->GetClientRect(&rect);
    this->InvalidateRect(&rect, 0);

    PAINTSTRUCT ps;
    HDC hdc = ::BeginPaint(m_hWnd, &ps);
	CFont* font = getDefaultFont(hdc, theApp.preferences.charSize);
	SetFont(font, TRUE);
	SelectObject(hdc, *font);
    this->GetClientRect(&rect);
    DrawText(ps.hdc, lispVarsBuf, -1, &rect, 
        DT_LEFT|DT_END_ELLIPSIS|DT_VCENTER|DT_SINGLELINE);
    EndPaint(&ps);
#endif
}

CSize LispVarsDialogBar::CalcDynamicLayout(int nLength, DWORD dwMode)
{
    dwMode |= LM_STRETCH;
    // Return default if it is being docked or floated
    if ((dwMode & LM_VERTDOCK) || (dwMode & LM_HORZDOCK))
    {
        if (dwMode & LM_STRETCH) // if not docked stretch to fit
            return CSize((dwMode & LM_HORZ) ? 32767 : m_sizeDocked.cx,
                        (dwMode & LM_HORZ) ? m_sizeDocked.cy : 32767);
         else
            return m_sizeDocked;
    }
    if (dwMode & LM_MRUWIDTH)
        return m_sizeFloating;
    // In all other cases, accept the dynamic length
    if (dwMode & LM_LENGTHY)
        return CSize(m_sizeFloating.cx, (m_bChangeDockedSize) ?
                    m_sizeFloating.cy = m_sizeDocked.cy = nLength :
                    m_sizeFloating.cy = nLength);
    else
        return CSize((m_bChangeDockedSize) ?
                    m_sizeFloating.cx = m_sizeDocked.cx = nLength :
                    m_sizeFloating.cx = nLength, m_sizeFloating.cy);
}

void LispVarsDialogBar::OnSize(UINT nType, int cx, int cy)
{
	CWnd* textBox = GetDlgItem(ID_VIEW_LISPVARS_TEXT);
    if (textBox)
    {
        textBox->SetWindowPos(&CWnd::wndTop, 2, 2, (cx - 4), (cy - 4), 0);
    }
}

static char VersionCaptionBuffer[64];
char* getVersionCaption()
{
    if (VersionCaptionPtr)
    {
        VersionCaptionBuffer[0] = 0;
        VersionCaptionPtr(VersionCaptionBuffer);
        if (VersionCaptionBuffer[0] == 0)
            return 0;
        else
            return VersionCaptionBuffer;
    }
    else
        return 0;
}

