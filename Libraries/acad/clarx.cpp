//
//		File:		clarx.cpp
//		Contents:	Corman Lisp AutoCAD ARX Listener and 
//					basic bootstrapper code for ARX applications
//		Author:		Reini Urban and Roger Corman
//					Based on clconsole.cpp
//      Copyright:  © 2000-2003 Corman Technologies
//                  © 2000 Reini Urban
//		Created:	4/28/99
//		Last modification time: 5/20/99
//		Version:	0.2.009 (hopefully, but better see version.h)

/*
Notes:
- This is an example how to call the CormanLispServer.dll via COM 
  from AutoCAD or any conforming application.
  This specific example of the listener can only be compiled with 
  MSVC++, so you will only get an ARX for R13c4, R14 and higher.
  Simplier ANSI-C and ADS based lisp functions and windows apps 
  are also thinkable. If you have to use ANSI C you have to use 
  the correct number of COM pointer indirection to access the vtable 
  and the correct methods. But most apps do support MSVC++.
  I wouldn't do it IMHO but I would appreciate any C solution as well.
  Brockschmidt's Inside OLE 2 has the needed samples if you really 
  want to use AutoCAD R12 or IntelliCAD or any other.

- v0.1.004 was the very first hack in the first two hours I got it 
  running, so beware. I'm also a nono! windows programmer at all.
  It's just to show that is possible, not how it should be done.
  Other possibilities would be to just initialize the kernel, 
  load the image (forcing output of the top-level function to the 
  acad commandline), show no console, accept some AutoCAD commands, 
  like CLIDE, CLFUNC, CLQUIT and inside the IDE accept :acad and :quit.
  The console or IDE should also manage the correct focus and implement 
  at least one ACAD button. (I may need probably MFC for this, but i 
  still hope to avoid it)
  The typical CCL application will not use any console at run-time or 
  if so, then a LISP based and not MFC based window.
  But for testing and pasting it's great.

- CormanLisp can produce standalone EXE's, DLL's or ARX applications
  (registration required if you want to distribute them) with 
  SAVE-IMAGE or SAVE-APPLICATION and a basic ARX stub to connect to 
  the server, load the image and call the entry function. 
  A way to call arbitrary AutoLISP or Visual LISP functions is in 
  consideration.
  SAVE-APPLICATION is already fixed, use any non-nil key of :ACAD2000
  :ACAD14 or :ACAD13 (we'll see)
  The one-file solution also works. You have to pack it manually and 
  the system extracts the files to the filesystem at run-time. This 
  might be improved in the future.

- The current CormanLisp IDE is a MFC exe and no COM server, so you 
  cannot call it from within acad. However there are enough LISP 
  based methods available to enrich any listener with the basic IDE 
  functionalities if wanted. This is the way I might want to go, 
  similar to the VLISP IDE or emacs. Maybe I can talk to VLIDE as well.
  VLIDE has not case-preservant filenames but great for editing.

- Basic communication to and from Acad ADS for bootstrapping should be 
  "stringified" because the current ADS based communication buffer 
  is deficient. no symbols, no functions, dots are broken,...
  (I don't think that we'll need that anyway)
  However, once CCL is running you could easily define direct foreign 
  functions to call any ARX or Win32 function and also define callbacks.
  So this is true hardcore stuff and as fast as C++ or VB compiled to 
  native code in-proc.
  If you still don't know it: Corman Lisp compiles to native machine 
  code on-the-fly and has a superior garbage collector.

- The only problem right now is that there are no well-written examples at
  all. Nothing for acad yet, only my horrible listener.
  The ARX module (accessing AcDb et al) will arrive soon, when clarx is 
  stable enough.

- For a nice WinGUI abstraction model I thought of another two months or so.
  The petzold examples included with CCL are horrible for lispers, 
  but C folks will like it. At least it works.
  It's not as nice as the LWW CAPI or the Allegro Common Graphics or 
  let's say java, pythonwin, the perl WinGUI or TK for now. 
  BTW: We cannot compare it against VB or MSVC++ MFC. Lisp is dynamic 
  and not static.
  See also the major discussion about the future and object systems at
  news://adesknews.autocad.com/autodesk.autocad.customization 
  Subject: "CLOS(Common Lisp Object System)"

- Some obvious functions are not exported to the default package 
  as in most common lisps. The default package is :common-lisp-user
  So you have to import them: (use-package :ccl), (use-package :win32)
  Or use fully qualified names such as ccl::defclass

- You may have to register the CormanLispServer.DLL at first. This is done 
  automatically if you start the IDE or clconsole.exe once, it's a 
  self-registering COM DLL or manually by typing 
  > REGSVR32 CormanLispServer.DLL
  Otherwise the COM initializer doesn't know where to find it.

- Image loading:
  The <cormanlisp>.img image file, containing the saved state of all 
  needed modules and variables (the world), is searched in the following 
  order:
  1) the arx path, 2) the DLL path, 3) the current directory

  To support standalone applications created with save-application, 
  the default imagename is the ARX name without the last two numbers, 
  if they are two numbers.
  Rules for the imagename:
    cormanlisp14.arx => cormanlisp.img
    myapp15.arx      => myapp.img
    myapp.arx        => myapp.img

- The console window is easy but a mess. I might change it to use a 
  MFC based Windows app. Even a modeless dialog should be possible. 
  (easy with MFC)
  But so far it doesn't compile as MFC Autocad ARX, and I have better 
  things to do.

- CLIDE
  TODO: When invoking the CLIDE command a "clide-init.lisp" when available 
  in the ARX path is loaded. Note that also "init.lisp" is loaded, but from 
  the Server.dll path. This is to help in AutoCAD development, setting up the 
  default paths and the registry key for demand-loading on future invocations.
  Note: clide-init.lisp is not invoked automatically by stand-alone arx 
  applications. It might be useful to use functions from this file for your 
  own startup suite.

- TODO: IDropTarget: 
  The console is a Drop Traget which is useful for 
  drap & drop filenames from the explorer into a (load "") call e.g.
  But currently a backslash is a single backslash, so inside a string 
  this should be escaped or all backslahses should be converted to 
  forward slashes. NT understands these forward slashes.

Problems:
- CLIDE 
  :quit		(or using the close button)
  CLIDE will fail

- The actual path is the ACAD dir for now (the base module), so the 
  search path is under ACAD.EXE and not under the DLL location as it 
  should be. (or not)
  This can be fixed either 1) by a better init.lisp, which can use the 
  PL::*CORMANLISP-DIRECTORY* variable (when it will be fixed) or 
  2) by changing the core to define the base dir dependent of the DLL 
  and not of the base module (which is ACAD.EXE right now). 
  The current state is that there will appear a second variable that 
  points to the path of the base module.

- There's no default lisp extension for LOAD.
  So you you have to type (load "example.lisp") not (load "example")

- There's no default search path for LOAD. This will be changed in the 
  init.lisp to point to subdirectories under PL::*CORMANLISP-DIRECTORY* 
  "Modules", "Sys", "examples" and "test"
  but I guess this will work only for REQUIRE which is btw. better than 
  LOAD.

- PL::*CORMANLISP-DIRECTORY* points to the base module dir which is 
  the ACAD.EXE dir and not the server.dll dir. see above.

- CLFUNC and C:CLFUNC output is one line behind so far. i'll try to synch 
  that.

Release history:
0.2.010 21.May 99
    new directory layout: under Libraries\acad
    provided VC6 project files
    more ACAD.LISP functions and fixes.
    registry-utils.lisp is now a module and should be under Modules
    more AutoLISP macros (repeat, while, foreach)
0.2.009 20.May 99
    compiled msvcrtd.dll independent, linked to lib (people might not have 
      this debug dll, it is larger now)
    registry-utils.lisp: fixed REG-SET-STRING, added REG-SET-DWORD
    fixed acad.lisp: PRODUCT-KEY, added some alisp utils
    put win32.lisp changes to win32-1.lisp to load faster
0.2.008 20.May 99
    loads <arx-path>/clide-init.lisp on CLIDE
    implements corrects focus switching
    fixes pl::*cormanlisp-directory* variable to dll path (hmm, not possible yet)
    sets pl::*basemodule-name* variable to arx filename
    sets pl::*basemodule-directory* variable to arx path
    acad.lisp: added REG-APP, PRODUCT-KEY
    registry-utils.lisp: added REG-SET-STRING
    win32.lisp: added some registry stuff
    added DLL path to img search path: 1) ARX, 2) ACAD 3) DLL 4) actual dir
0.2.007 19.May 99
	first ALL_IN_ONE_FILE version, save-application support pending,
    packaged with CCL 1.3
0.2.006 15.May 99
	implemented CLFUNC, C:CLFUNC, CLQUIT
	started with ALL_IN_ONE_FILE and MFC stuff
0.2.005 14.May 99
	added SAVE-APPLICATION support, the rules for finding the correct
	image, CL-LOAD-IMAGE and fixed some console stuff.
    The window is not opened at startup, only on the command CLIDE.
0.1.004 12.May 99 
	first successful version with roger's fixed GC,
        also announced it on the web and in the newsgroups.
0.1.001 28.Apr 99
	started to write this and to fix GC.cpp
*/

#if ( defined(ACRXAPP) && defined(_DEBUG) )
  #define ACRXAPP_DEBUG
  #undef _DEBUG
#endif

#include <windows.h>
#include <wtypes.h>
//#include <iostream.h>
#include <string.h>
#include <stdio.h>
#include <ocidl.h>
#include <initguid.h>
#include <conio.h>

#undef _AFXDLL
// #define _AFXDLL	// MFC would be nicer
#ifdef _AFXDLL
#include "stdafx.h"
#endif

/* AutoCAD stuff */
#ifdef ACRXAPP_DEBUG
  #define _DEBUG
  #define VERBOSE
#endif

#include <stdlib.h>
#include <io.h>
#include <sys/stat.h>
#include "version.h"

#pragma comment( lib, "acad.lib" )
#pragma comment( lib, "rxapi.lib" )
#ifdef R15
#pragma comment( lib, "acrx15.lib" )
#pragma comment( lib, "acdb15.lib" )
#pragma comment( lib, "acutil15.lib" )
#endif

#include "clsids.h"
#include "ErrorMessage.h"
#include "ICormanLisp.h"
#include "charbuf.h"

/* CCL stuff */
CharBuf TerminalOutputBuf(0x8000);
HINSTANCE gInstance = 0;
HWND	  gMainWnd = 0;
HINSTANCE gArxInstance = 0;
static HWND hWndACAD = NULL;
const char* consoleCaption = "CormanLisp ARX Listener";
const char* gServerName    = "CommonLispServer.dll";
static char gServerPath[256]  = "";
static char gImageName[256]  = "";
static char gModuleName[256] = "";
ICormanLisp* pCormanLisp = 0;
long numThreads = 0;

typedef char tPath[_MAX_PATH];

int CormanLispInitialize (char *imagefile);
void CormanLispShutdown(void);
char *GetPath(char *fullpath, tPath outdir);
char *FindImageName (char *imagefile, char *gImageName);
int mainx(int argc, char* argv[]);
static char* getConsoleText();
void RedirectOutput (void);		// not implemented, we use the global buffer instead
#ifndef CONSOLE_APP
static long FAR PASCAL WndProc (HWND hwnd, UINT message, UINT wParam, LONG lParam);
#endif


#include "adslib.h"
#include "rxdefs.h"
#ifdef R15
	#include "rxregsvc.h"
#endif

#ifdef _AFXDLL
#include "rxmfcapi.h"	//R14, 15
#endif

extern "C" AcRx::AppRetCode acrxEntryPoint (AcRx::AppMsgCode msg, void* appId);

#ifdef _AFXDLL
extern HWND adsw_acadMainWnd();
// extern "C" HINSTANCE _hdllInstance;
// static HWND hWndACAD = NULL;
static HINSTANCE _hdllInstance = NULL;
extern "C" BOOL APIENTRY DllMain(HINSTANCE, DWORD, LPVOID);

void InitMFC()
{
	_hdllInstance = gArxInstance;
    DllMain(_hdllInstance, DLL_PROCESS_ATTACH, NULL);
    hWndACAD = /* GetActiveWindow() */ adsw_acadMainWnd();
}
void EndMFC()
{
    DllMain(_hdllInstance, DLL_PROCESS_DETACH, NULL);
}
#endif


/* private functions: */
int dofunc				(void);
int funcload			(void);
int funcunload			(void);

/* possible lisp functions, most of them not yet implemented */
int switch_to_console	(void);		/* (CLIDE) */
int close_console		(void);		/* (CLQUIT) */
int cl_func				(void);		/* (CLFUNC <string>) */
int cl_cfunc			(void);		/* (C:CLFUNC) */
int cl_load_image		(void);		/* (CL-LOAD-IMAGE [img-filename])=>T/nil*/
#if THREADS
int cl_newthread		(void);		/* (CL-NEW-THREAD <string>) => <string> */
int cl_endthread		(void);		/* (CL-END-THREAD <string>) */
#endif

struct func_entry { char *func_name; int (*func) (void); };

static struct func_entry func_table[] = {
{"C:CLIDE",		switch_to_console},	/* Open or switch to the console */
{"CLIDE",		switch_to_console},	/* The same */
{"C:CLQUIT",    close_console},		/* Destroy the console, but don't unload corman 
						lisp. You could also use Alt-F4 or [X] */
{"CLQUIT",		close_console},
{"CLFUNC",		cl_func},		/* Call any function or display a variable
						binding, output either to the console or to acad */
{"C:CLFUNC",	cl_cfunc},		/* Interactive from the ACAD text window */
{"CL-LOAD-IMAGE", cl_load_image},	/* Starts a new session with a new image 
								filename */
#if THREADS
{"CL-NEW-THREAD", cl_newthread},	/* Start a new thread in the same image */
{"CL-END-THREAD", cl_endthread},	/* End this new thread */
#endif
};

/*
get ACAD's memory size with
GetSystemInfo() 
=> SYSTEM_INFO.lpMinimumApplicationAddress
   SYSTEM_INFO.lpMaximumApplicationAddress
*/

/* 
// we don't need this here, acad is our server. maybe later when we'll use 
// MFC but then we'll use our own resource memory classes (acad's resourcehelper.h)
// MFC is quite hairy inside acad.
class AllocVirtualSpace
{
public:
	AllocVirtualSpace()
	{
		void* mem = VirtualAlloc((void*)0x1000000, 
				0x10000000 - 0x1000000, 
				MEM_RESERVE, PAGE_EXECUTE_READWRITE);
	}
} allocVirtualSpace;
*/


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

// Helper functions
	STDMETHODIMP Connect(IConnectionPoint* pConnectionPoint);
	STDMETHODIMP Disconnect(IConnectionPoint* pConnectionPoint);

private:
	long m_cRef;
	DWORD m_dwCookie;
};

// 0 if not existing
long FileDate (const char *fname)
{
    struct _stat buf;

    if (0 == _stat(fname,&buf))
        return buf.st_mtime;
    else 
        return 0;
}

bool FilesExists (const char *fname)
{
    return (0 == _access(fname, 0));
}

bool FileOlder (const char *fname1, const char *fname2)
{
    return (FileDate (fname1) < FileDate (fname2));
}

/*
    Only if the to be extracted resource filename is older than the 
    arx, extract it
	TODO: Don't know yet if UpdateResource() can really strip it from the 
	  actually loaded file. For now I only release its memory and reload it 
	  again from file later.
      CAN_UPD_RES doesn't work
*/
bool ExtractResource (const HMODULE hModule, const LPCTSTR lpName, const char *fname, const unsigned long magic)
{
	HRSRC reshdl = 0;
	HGLOBAL hdl  = 0;
    unsigned long lNum;

    if (!*gModuleName)
	    GetModuleFileName (hModule, gModuleName, sizeof(gModuleName));

	if ((reshdl = FindResource (hModule, lpName, RT_RCDATA)) &&
		(hdl = LoadResource(hModule, reshdl)))
    {
        memcpy (&lNum, hdl, 4);
        if (lNum == magic)
        {
            if ( !FilesExists(fname) ||	// file exists
			     FileOlder (gModuleName, fname))
    	    {
	    	int size = SizeofResource(hModule, reshdl);
		    FILE *os;
		    os = fopen (fname,"wb");
		    fwrite (hdl, size, 1, os);  // now it IS newer.
		    fclose (os);

#ifdef CAN_UPD_RES
		    HANDLE hUpd = BeginUpdateResource(gModuleName,FALSE);
		    UpdateResource(hUpd,RT_RCDATA,"DLLDATA",LANG_NEUTRAL,NULL,size);
		    EndUpdateResource(hUpd, TRUE);	// TRUE not to change the file
#endif
		    return TRUE;
	        }
        }
        else
        {
			ads_printf("\nWarning: Invalid lisp resource <%s>", lpName);
            return FALSE;
        }
    }
    return FALSE;
}

/*
	Check if any of the two resources RC_DATA IMAGEDATA and RC_DATA DLLDATA is available 
	and unpack them to the actual arx path. register the DLL as COM server
	and set gIMageName. 
	TODO: LOAD-IMAGE
        It would be nice to map the resources directly to the calling process space.
	IMG: The only problem is readHeapfromfile() in GC.cpp which uses fread() for now.
	It could be improved to load from the resource (or any buffered stream) instead.
	DLL: save as above, but it should be possible to register this DLL directly without
	saving it as interim file.
	TODO: SAVE-APPLICATION problem
	It would be nice to mimic the resource linker to link the resource into our
	our executable within LISP. The resource format is documented, but looks pretty 
	weird. For the beginning we can assume that LINK.EXE is available and call this 
	from SAVE-APPLICATION (not yet).
*/
void unpack_resources(void)
{
	HMODULE hModule;
	HRSRC reshdl;
	HGLOBAL hdl;

	char *pModuleName = ads_getappname();
	strcpy (gModuleName, pModuleName);
	FindImageName (NULL, gImageName);
	// check the resource in the base module for a onefile solution,
	// packed in a WinEXE "CLBOOT.EXE" or AutoCAD ARX "CLARX15.ARX"
	// SAVE-APPLCATION might support this in the future. (certainly not yet)
	hModule = GetModuleHandle(pModuleName);
	// search for the img filename in the module resource table
	// we better check for a named resource entry, and also check for the 
	// correct magic header in the image.
    // we might also want to check the versions, if the image and the dll match
    // (yes, this really it happened to me that i couldn't track down an exception
    //  caused by an old DLL being found by CoInitialize.)
	if (reshdl = FindResource(hModule, "IMAGENAME", RT_RCDATA))
	{
		hdl = LoadResource(hModule, reshdl);
		char *resimgname = (char *) hdl;
		ExtractResource(hModule,"IMAGEDATA",resimgname, 0xC0C0BABE);
	}
	if (ExtractResource(hModule,"DLLDATA",gServerName,0x00905A4D))
    {
        // register this DLL
        ;
    }
}

int acad_FlushOutput(void)
{
    int i = TerminalOutputBuf.numchars();
	if (i > 0)
	{
		char* output = TerminalOutputBuf.getChars();
		ads_printf("\n%s",output);
		delete [] output;
	}
    return i;
}
/* possible lisp functions */
/* call any function or display a variable binding, 
   I/O with inferior acad console only
   FIXME: TerminalOutputBuf.getChars() is out of sync
   better force unlocking the event?
*/
int cl_func ()
{
	struct resbuf *arg;
	arg = ads_getargs();
	if (!arg || (arg->restype != RTSTR))
	{
		ads_printf ("Bad argument type\n");
		return RTERROR;
	}
	else
	{
        char *str = arg->resval.rstring;

        acad_FlushOutput();
		pCormanLisp->ProcessSource(str, strlen(str));
        Sleep(100);
        acad_FlushOutput();
	}
	return RTNORM;
}

/*
   I/O with inferior acad console only
   FIXME: same problems as above.
*/
int cl_cfunc ()
{
	char buf[133];

    acad_FlushOutput();
	while (1)
	{
		ads_initget(128, NULL);
		if ((ads_getstring(1,"\nCL>", buf) == RTNORM) && *buf) 
		{
			pCormanLisp->ProcessSource(buf, strlen(buf));
            Sleep(100);
            acad_FlushOutput();
		}
		else
			break;
	}
	return RTNORM;
}

/* fname must be large enough to hold the result, path also to hold a temporary */
int ForcePath (char *path /* IN */, char *fname /* OUT */)
{
    char buf[_MAX_PATH];
    int len;

    GetPath (path, buf);
    GetPath (fname, path);
    if (*path)
        strcpy (fname, &fname[strlen(path)]);;
    len = strlen (buf);
    if (buf[len] == '.')
        strcpy (buf, "/");
    strcat (buf, fname);
    strcpy (fname, buf);
    return strlen (fname);
}

/*  only on the very first CLIDE, or 
    FIXME: after a new image and CLIDE then */
void load_clideinit(void)
{
    char buf[_MAX_PATH + 20];

    if (!*gModuleName)
	    strcpy (gModuleName, ads_getappname());

    strcpy (buf, "clide-init.lisp");
    ForcePath(gModuleName, buf);
    if (FilesExists (buf))
    {
        sprintf (buf, "(load \"%s\")", buf);
        pCormanLisp->ProcessSource (buf, strlen(buf));
    }
}

// temp. arx fix
int DefineBasemoduleVars ()
{
    char path[_MAX_PATH];
    char result[1024];

    if (!*gModuleName)
	    strcpy (gModuleName, ads_getappname());

    GetPath(gModuleName, path);
    // gServerPath
    sprintf (result, "(defconstant pl::*basemodule-directory* \"%s\")", path);
    sprintf (result, "%s (defconstant pl::*basemodule-name* \"%s\")", result,gModuleName);
    if (*gServerPath)   // not yet ready, if loaded via COM
    {
        GetPath (gServerPath, path);
        sprintf (result, "%s (defconstant pl::*cormanlisp-directory* \"%s\")", result,path);
    }
    return pCormanLisp->ProcessSource(result, strlen(result));
}

/* open or switch to the console */
int switch_to_console	(void)		/* (CLIDE) */
{
	if (!gInstance)
	{
		WinMain(gArxInstance,0,"",1);	// parent handle
        load_clideinit();
	}
	mainx(0, 0);
	return RTNORM;
}

/* destroy any console, but don't unload corman lisp 
   FIXME: sometimes destroys acad as well.
*/
int close_console		(void)		/* (CLQUIT) */
{
#if 1
	if (gInstance)
	{
		FreeConsole();
		gInstance = 0;
		// this is wrong! it closes acad and not the console
		//PostQuitMessage (0);
	}
	SetFocus(hWndACAD);
    // CormanLispShutdown()
	return RTNORM;
#else
	ads_printf ("Sorry! CLQUIT not yet defined\n");
	return RTERROR;
#endif
}

/* starts a new session with a new image filename */
int cl_load_image		(void)		/* (CL-LOAD-IMAGE [img-filename]) => T/nil */
{
	struct resbuf *arg;
	arg = ads_getargs();
	if (arg && (arg->restype != RTSTR))
		CormanLispInitialize (arg->resval.rstring);
	else
		CormanLispInitialize(NULL);
	ads_rett ();
	return RTNORM;
}


#if THREADS
/* start a new thread in the same image */
int cl_newthread		(void)		/* (CL-NEW-THREAD) => <num> */
{
	struct resbuf *arg;
	ads_printf ("Sorry! (CL-NEW-THREAD) not yet defined\n");
	// pCormanLisp->AddRef();
	return RTERROR;
}

/* end this new thread */
int cl_endthread		(void)		/* (CL-END-THREAD <num>) */
{
	struct resbuf *arg;
	ads_printf ("Sorry! (CL-END-THREAD <num>) not yet defined\n");
/*
	if (pCormanLisp)
	{
		pCormanLisp->GetNumThreads(&numThreads);
		if (numThreads > 0)
			pCormanLisp->AbortThread();
	}
*/
	return RTERROR;
}
#endif


// AutoCAD's DLL entry point
extern "C" AcRx::AppRetCode acrxEntryPoint (AcRx::AppMsgCode msg, void* appId)
{
    switch (msg) {
    case AcRx::kInitAppMsg:
        acrxUnlockApplication (appId);
#ifdef R15
		acrxRegisterAppMDIAware (appId);	
#endif
#ifdef _AFXDLL
		InitMFC();
#endif
        break;
    case AcRx::kLoadDwgMsg:
		gArxInstance = (HINSTANCE) GetModuleHandle(ads_getappname());
		funcload ();
#ifdef ALL_IN_ONE_FILE
		unpack_resources();
#endif
		CormanLispInitialize (NULL);
        hWndACAD = GetFocus();
		break;
    case AcRx::kInvkSubrMsg:
		 if (dofunc () != RTNORM)
            return AcRx::kRetError;
        break;
    case AcRx::kUnloadDwgMsg:
    case AcRx::kEndMsg:
		close_console();
		CormanLispShutdown();
#ifdef _AFXDLL
		EndMFC();
#endif
		funcunload ();
        break;
    }
    return AcRx::kRetOK;
}

#define ELEMENTS(array) (sizeof (array) / sizeof ((array)[0]))

int funcload ()
{
    int i;
    for (i = 0; i < ELEMENTS (func_table); i++) {
        if (!ads_defun (func_table[i].func_name, i))
          return RTERROR;
#ifdef DEF_REGFUNC	// This might be included to provide external ads_invoke() support
		else
          ads_regfunc (func_table[i].func, i); 
#endif
    }
    return RTNORM;
}

int funcunload ()
{
	int i;
	for (i = 0; i < ELEMENTS (func_table); i++) {
		ads_undef (func_table[i].func_name, i);
	}
	return RTNORM;
}

/* Return value comes from the function executed, RTNORM or RTERROR.  */
int dofunc ()
{
    int val;
    if ((val = ads_getfuncode ()) < 0 || val >= ELEMENTS (func_table)) {
        ads_fail ("Invalid function code.");
        return RTERROR;
    }
    return (*func_table[val].func) ();
}

/*===========================================================================*/

typedef void (WINAPI *LOADLIBRARYFUNC)();
HANDLE gEvent = 0;

void delay(int ms)
{
	// delay ms milliseconds
	Sleep(ms);
}


// returns the path portion of a filename in outdir
char *GetPath(char *fullpath, tPath outdir)
{
	char drive	[_MAX_DRIVE];   
	char dir	[_MAX_DIR];

	if (fullpath)
	{
		_splitpath (fullpath, drive, dir, NULL, NULL);
		_makepath  (outdir, drive, dir, NULL, NULL);
	}
	else 
		*outdir = '\0';
	return outdir;
}


// processes the filename only, paths are searched with GetPath()
// ..clarx.arx    => ..clarx.img
// ..clarx15.arx  => ..clarx.img	13,14,15,16,...
// ..clarx15d.arx => ..clarx.img
char *GetImageName (char* appname, char *result)
{
    char fname	[_MAX_FNAME];   
	char ext	[_MAX_EXT];
	int i;

	_splitpath (appname, NULL, NULL, fname, ext);
	i = strlen(fname) - 1;
	if (('d' == fname[i]) && ('1' == fname[i-2]))	// debug version
		fname[i-2] = '\0';
	if (('1' == fname[i-1]) && (fname[i] >= '3')  &&	(fname[i] <=  '6'))
		fname[i-1] = '\0';
	_makepath (result, NULL, NULL, fname, ".img");
	return result;
}

// searches the image in the given paths
int FindImage (char *imagename, tPath *Paths, int numPaths, char *result)
{
	char path_buffer[_MAX_PATH];
	char drive	[_MAX_DRIVE];   
	char dir	[_MAX_DIR];
    char fname	[_MAX_FNAME];   
	char ext	[_MAX_EXT];

	if (FilesExists(imagename))
	{
		strcpy (result, imagename);
		return TRUE;
	}
	_splitpath (imagename, NULL, NULL, fname, ext);
	for (int i=0; i<numPaths; i++)
	{
		_splitpath (&Paths[i][0], drive, dir, NULL, NULL);
		_makepath (path_buffer, drive, dir, fname, ext);
		if (FilesExists(path_buffer))
		{
			strcpy (result, path_buffer);
			return TRUE;
		}
	}
	return FALSE;
}

// Rules to find the image:
// The <cormanlisp>.img image file is searched in the following paths:
//  1) the arx path, 2) the DLL path, 3) the current directory
//
//  To support standalone applications created with save-application, 
//  the default imagename is the ARX name without the last two numbers, 
//  if they are two numbers.
//  Rules for the imagename:
//    cormanlisp14.arx => cormanlisp.img
//    myapp15.arx      => myapp.img
//    myapp.arx        => myapp.img
//
// TODO: check for the correct image version, the 0xC0C0BABE magic and 
// versionID.
// most often we might have loaded an old DLL, but old images 
// can also happen.
char *FindImageName (char *imagefile, char *gImageName)
{
	const char defaultImageName[] = "CormanLisp.img";
	char path_buffer[_MAX_PATH];
	const int numPaths = 4;
	tPath Paths[numPaths];

	// img search path: 1) ARX, 2) ACAD 3) DLL 4) actual dir
	GetPath (ads_getappname(), Paths[0]);
	GetModuleFileName(0, path_buffer, sizeof(path_buffer));
	GetPath (path_buffer, Paths[1]); 
	GetPath (gServerPath, Paths[2]);
	GetPath (NULL, Paths[3]);

	if (!imagefile)
	{
		GetImageName (ads_getappname(), path_buffer);
		if (FindImage (path_buffer, &Paths[0], numPaths, gImageName))
			return gImageName;
	}
	else
	{
		GetImageName (imagefile, path_buffer);
		if (FindImage (path_buffer, &Paths[0], numPaths, gImageName))
			return gImageName;
		GetImageName (ads_getappname(), path_buffer);
		if (FindImage (path_buffer, &Paths[0], numPaths, gImageName))
			return gImageName;
	}
	strcpy (gImageName, defaultImageName);
	return gImageName;
}	

void CormanLispShutdown(void)
{
	if (pCormanLisp)
		pCormanLisp->Release();
	pCormanLisp = 0;
	CoUninitialize();
#ifdef VERBOSE
	ads_printf ("-CormanLisp <%s> unloaded.", gImageName);
#endif
}

int CormanLispInitialize (char *imagefile)
{
	static IConnectionPoint* pConnectionPoint = 0;
    HRESULT hr; // = E_FAIL;
	HANDLE thread;
	ConsoleCormanLispClient* pCormanLispClient;
	
	if (pCormanLisp)
	{
		CormanLispShutdown();
	}
	
	CoInitialize(0);
	
	IClassFactory *pcf = 0;
	hr = CoGetClassObject(CLSID_CormanLisp,
		CLSCTX_INPROC_SERVER,
		0,
		IID_IClassFactory,
		(void**)&pcf);

    // check for the version
	if (FAILED(hr))
	{
		if (hr == REGDB_E_CLASSNOTREG)
		{
			// the server was not registered, so see if we can
			// register it now
			HINSTANCE plserver = LoadLibrary(gServerName);
			if (!plserver)
			{
				ErrorMessage("Could not load CommonLispServer.dll");
				return FALSE;
			}
            GetModuleFileName (plserver, gServerPath, sizeof(gServerPath));
			FARPROC proc = GetProcAddress(plserver, "DllRegisterServer");
			if (!proc)
			{
				ErrorMessage("Could not find DllRegisterServer() in CommonLispServer.dll");
				return FALSE;
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
	pConnectionPointContainer->Release();
	if (FAILED(hr))
	{
		ErrorMessage(__TEXT("FindConnectionPoint() did not return IID_ICormanLispStatusMessage"), 
			hr);
		return FALSE;
	}
	
	// make an instance of our outgoing class interface
	pCormanLispClient = new ConsoleCormanLispClient;
	pCormanLispClient->AddRef();
	
	hr = pCormanLispClient->Connect(pConnectionPoint);
	if (FAILED(hr))
	{
		pCormanLispClient->Release();
		pConnectionPoint->Release();
		fprintf(stderr, "Connection failed");
		return FALSE;
	}
	
	
	gEvent = CreateEvent(NULL, TRUE, FALSE, "CormanLisp Timer 1");

    if (!*gServerPath)
    {
        // how to get the Server.DLL module handle? via COM somehow? 
        // or do i really have to read the registry?
        ads_printf ("Warning: pl::*cormanlisp-directory* is probably wrong and couldn't be fixed.");
    }
	FindImageName (imagefile, gImageName);
	
#ifdef CONSOLE_APP
	pCormanLisp->Initialize(pCormanLispClient, gImageName, CONSOLE_CLIENT);
#else
	pCormanLisp->Initialize(pCormanLispClient, gImageName, WIN_APP_CLIENT);
#endif
	thread = 0;
	numThreads = 0;
	
	pCormanLisp->Run(&thread);
#ifdef VERBOSE
	ads_printf ("\n-CormanLisp <%s> loaded.", gImageName);
#endif

    DefineBasemoduleVars();
	return TRUE;
}

// redirects stdout to the acad text window
// IID_ICormanLispTextOutput => ACAD text window
// ICormanLispTextOutput*	 ClientTextOutput	= 0;
void RedirectOutput (void)
{
	;
}

// process input from the console window
// the main event-loop. lisp is already up and running.
int mainx(int argc, char* argv[])
{
//	CormanLispInitialize (argc ? argv[0] : NULL);

	char* input = 0;
	MSG msg;
	// Todo: provide an :acad image button
	_cputs("Type :quit to exit or :acad\r\n");
	while (1)
	{
		if (PeekMessage (&msg, NULL, 0, 0, TRUE))
		{
			if (msg.message == WM_QUIT)
				break;

			if (msg.message == WM_KILLFOCUS)
            {
#ifdef VERBOSE
				_cputs ("switching back to acad...\r\n");
				ads_printf ("-coming from Corman Lisp...");
#endif
				SetFocus(hWndACAD);
				return 1;
            }

			// watch for control-break
			if (msg.message == WM_CHAR && msg.wParam == 3  
				&& (GetKeyState(VK_CONTROL) < 0))
			{
				if (pCormanLisp)
				{
					pCormanLisp->GetNumThreads(&numThreads);
					if (numThreads > 0)
						pCormanLisp->AbortThread();
				}
			}
			else
			{
				TranslateMessage (&msg) ;
				DispatchMessage (&msg) ;
			}
		}
		if (TerminalOutputBuf.numchars() > 0)
		{
			char* output = TerminalOutputBuf.getChars();
			_cputs(output);
			_cputs("?");
			delete [] output;
		}

		input = getConsoleText();
		if (strlen(input) > 0)
		{
			if (!_stricmp(input, ":acad\r\n"))
			{	// switch to acad
#ifdef VERBOSE
				_cputs ("switching back to acad...\r\n");
				ads_printf ("-coming from Corman Lisp...");
#endif
				SetFocus(hWndACAD);
/*
#ifdef _AFXDLL
				acedGetAcadFrame()->SetActiveWindow();
#endif
*/
				return 1;
				// break;
			}
			if (!_stricmp(input, ":quit\r\n"))
			{
#ifdef VERBOSE
				_cputs ("bye...\r\n");
#endif
				close_console();
				break;
			}
			pCormanLisp->ProcessSource(input, strlen(input));
		}
		delay(50);	// 100 might be too much for pasting. we try to be gentle
	}
	return 0;
}


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
	case CTRL_CLOSE_EVENT:		// tja...
#ifdef VERBOSE
		ads_printf ("\n-Close event ");
#endif
		close_console();
		break;
	case CTRL_LOGOFF_EVENT:
		close_console();
		break;
	case CTRL_SHUTDOWN_EVENT:	// notify ACAD somehow.
#ifdef VERBOSE
		ads_printf ("\n-Shutdown event ");
#endif
		close_console();
		break;
	}
	return handled;
}

// this is also the same code for an external EXE listener

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
					PSTR szCmdLine, int iCmdShow)
{
#ifdef CONSOLE_APP
	BOOL ret = AllocConsole();
	ret = SetConsoleCtrlHandler(HandlerRoutine, TRUE);
	SetConsoleTitle(consoleCaption);
	HANDLE handle = GetStdHandle(STD_OUTPUT_HANDLE);
	COORD coord;
	coord.X = 100;
	coord.Y = 100;		// default console size if 100 x 100
	SetConsoleScreenBufferSize(handle, coord);
#else
// or a win app
	static char szAppName[] = "CormanLisp ARX";
	HWND        hwnd ;
	MSG         msg ;
	WNDCLASS    wndclass ;
	if (!hPrevInstance)	// always processed, only under win16 it failed
	{
		wndclass.style         = CS_HREDRAW | CS_VREDRAW ;
		wndclass.lpfnWndProc   = WndProc ;
		wndclass.cbClsExtra    = 0 ;
		wndclass.cbWndExtra    = 0 ;
		wndclass.hInstance     = hInstance ;
		wndclass.hIcon         = LoadIcon (NULL, IDI_APPLICATION) ;
		wndclass.hCursor       = LoadCursor (NULL, IDC_ARROW) ;
		wndclass.hbrBackground = (HBRUSH) GetStockObject (WHITE_BRUSH); // => HGDIOBJ 
		wndclass.lpszMenuName  = NULL ;
		wndclass.lpszClassName = szAppName ;

		RegisterClass (&wndclass) ;
	}

	hwnd = CreateWindow (szAppName,      // window class name
				consoleCaption,			 // window caption
				WS_OVERLAPPEDWINDOW,     // window style
				CW_USEDEFAULT,           // initial x position
				CW_USEDEFAULT,           // initial y position
				CW_USEDEFAULT,           // initial x size
				CW_USEDEFAULT,           // initial y size
				NULL,                    // parent window handle
				NULL,                    // window menu handle
				hInstance,               // program instance handle
				NULL) ;					 // creation parameters
	gMainWnd = hwnd;

    ShowWindow(hwnd, iCmdShow) ;
    UpdateWindow(hwnd) ;
#endif
	gInstance = hInstance;
	_cprintf("%s v%0d.%0d.%03d <%s>\r\n", 
        ads_getappname(),
        VERMAJOR,VERMINOR,BUILD,
        gImageName);
    // this is not needed anymore. we don't open the window anymore at startup
//#ifdef VERBOSE
//	_cputs("Type CLIDE inside ACAD to connect to this window\r\n");
//#endif
    // mainx(0,0); // decoupled the main event-loop from the window.
	return 0;
}

//	If an entire line has been entered, return it.
//	Otherwise return an empty string.
//
const int ASCII_BACKSPACE = 0x08;
const int ASCII_DELETE = 0xe0;
const int ASCII_SPACE = 0x20;

char ConsoleTextBuf[4096];
static char* s = ConsoleTextBuf;

static char* getConsoleText()
{
	char c;
	while (_kbhit())
	{
		c = (char)_getche();
		if (c == ASCII_BACKSPACE)
		{
			if (s > ConsoleTextBuf)
			{
				--s;
				// erase the position
				_putch(ASCII_SPACE);
				_putch(ASCII_BACKSPACE);
			}
		}
		// we might want to support <TAB> for completion.  
		// cursor keys? history buffer?
		else
			*s++ = c;

		if (c == 13)
		{
			*s++ = 10;
			_putch(10);
			*s++ = 0;
			s = ConsoleTextBuf; 
			return s;
		}
	}
	return "";
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

CharBuf::CharBuf(long length)
: fCharsAvailable() 
{ 
	fSize = length;
	fBuf = new char[fSize];
	fIndex = 0;
}

CharBuf::~CharBuf() 
{ 
	delete [] fBuf; 
}

long 
CharBuf::size() 
{ 
	return fSize; 
}

long 
CharBuf::numchars() 
{ 
	return fIndex; 
}

char* 
CharBuf::getChars() 
{ 
	PLSingleLock myLock(&fSemaphore, TRUE);
	fBuf[fIndex] = 0;
	char* s = new char[fIndex + 1];
	strcpy(s, fBuf);
	fIndex = 0;
	return s;
}

long 
CharBuf::getCharsInBuffer(char* buf, long num)
{
	PLSingleLock myLock(&fSemaphore, TRUE);
	if (fIndex > num)
	{
		memcpy(buf, fBuf, num);
		memcpy(fBuf, fBuf + num, fIndex - num);
		fIndex -= num;
		return num;
	}
	else
	{
		memcpy(buf, fBuf, fIndex);
		long temp = fIndex;
		fIndex = 0;
		return temp;
	}
}

long 
CharBuf::addChars(const char* buf, long num)
{
	long i;
	PLSingleLock myLock(&fSemaphore, TRUE);
	long spaceAvailable = (fSize - 1) - fIndex;
	if (spaceAvailable < num)
		num = spaceAvailable;
	for (i = 0; i < num; i++)
		fBuf[fIndex++] = buf[i];
	if (i > 0)
	{
		fCharsAvailable.SetEvent();
	}
	else
	if (i == 0)	// if buffer is full, wait until it is empty
	{
		fBufEmpty.ResetEvent();
		WaitForSingleObject(fBufEmpty, 100);
		fBufEmpty.ResetEvent();
	}
	return i;
}

PLEvent*
CharBuf::charsAvailable()
{
	return &fCharsAvailable;
}

PLEvent::PLEvent(BOOL bInitiallyOwn, BOOL bManualReset, LPCTSTR pstrName,
	LPSECURITY_ATTRIBUTES lpsaAttribute)
	: PLSyncObject(pstrName)
{
	m_hObject = ::CreateEvent(lpsaAttribute, bManualReset,
		bInitiallyOwn, pstrName);
}

PLEvent::~PLEvent()
{
}

BOOL PLEvent::Unlock()
{
	return TRUE;
}

PLSemaphore::PLSemaphore(LONG lInitialCount, LONG lMaxCount,
	LPCTSTR pstrName, LPSECURITY_ATTRIBUTES lpsaAttributes)
	:  PLSyncObject(pstrName)
{
	m_hObject = ::CreateSemaphore(lpsaAttributes, lInitialCount, lMaxCount,
		pstrName);
}

PLSemaphore::~PLSemaphore()
{
}

BOOL PLSemaphore::Unlock(LONG lCount, LPLONG lpPrevCount /* =NULL */)
{
	return ::ReleaseSemaphore(m_hObject, lCount, lpPrevCount);
}

PLSingleLock::PLSingleLock(PLSyncObject* pObject, BOOL bInitialLock)
{
	m_pObject = pObject;
	m_hObject = pObject->m_hObject;
	m_bAcquired = FALSE;

	if (bInitialLock)
		Lock();
}

BOOL PLSingleLock::Lock(DWORD dwTimeOut /* = INFINITE */)
{
	assert(!m_bAcquired);

	m_bAcquired = m_pObject->Lock(dwTimeOut);
	return m_bAcquired;
}

BOOL PLSingleLock::Unlock()
{
	if (m_bAcquired)
		m_bAcquired = !m_pObject->Unlock();

	// successfully unlocking means it isn't acquired
	return !m_bAcquired;
}

BOOL PLSingleLock::Unlock(LONG lCount, LPLONG lpPrevCount /* = NULL */)
{
	if (m_bAcquired)
		m_bAcquired = !m_pObject->Unlock(lCount, lpPrevCount);

	// successfully unlocking means it isn't acquired
	return !m_bAcquired;
}

PLSyncObject::PLSyncObject(LPCTSTR /*pstrName*/)
{
	m_hObject = NULL;
}

PLSyncObject::~PLSyncObject()
{
	if (m_hObject != NULL)
	{
		::CloseHandle(m_hObject);
		m_hObject = NULL;
	}
}

BOOL PLSyncObject::Lock(DWORD dwTimeout)
{
	if (::WaitForSingleObject(m_hObject, dwTimeout) == WAIT_OBJECT_0)
		return TRUE;
	else
		return FALSE;
}

#ifndef CONSOLE_APP
static long FAR PASCAL WndProc (HWND hwnd, UINT message, UINT wParam, LONG lParam)
     {
     HDC         hdc ;
     PAINTSTRUCT ps ;
     RECT	 rect ;

     switch (message)
     {
       case WM_PAINT:
		hdc = BeginPaint (hwnd, &ps) ;
        GetClientRect (hwnd, &rect) ;

		DrawText (hdc, consoleCaption, -1, &rect,
			 DT_SINGLELINE | DT_CENTER | DT_VCENTER) ;

	    EndPaint (hwnd, &ps) ;
        return 0 ;

	  case WM_DESTROY:
		PostQuitMessage (0) ;
        return 0 ;
	}
    return DefWindowProc (hwnd, message, wParam, lParam) ;
}
#endif

/*
// TODO: implement the "\\" => "/" converter inside string data (filenames)
// maybe only when holding the Ctrl key. hmm...
class CConsoleDropTarget : public CDropTarget 
{
	
	  
}
STDMETHODIMP 
CConsoleDropTarget::Drop(
    IDataObject * pDataObject,	//Points to the IDataObject interface for the source data
    DWORD grfKeyState,			//Current state of keyboard modifier keys
    POINTL pt,					//Current cursor coordinates
    DWORD * pdwEffect )			//Effect of the drag-and-drop operation
{
	BOOL fRet=TRUE;
	*pdwEffect = DROPEFFECT_NONE;

	if (NULL == m_pIDataObject)
		return ResultFromScode(E_FAIL);

	DragLeave();

    fRet = ::PasteFromData(pDataObject, rSize, objType);


    return NOERROR;
}

*/