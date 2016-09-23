//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		All rights reserved.
//		-------------------------------

#include <windows.h>
#include <stdio.h>

static HMODULE module = 0;
static HMODULE kernelModule = 0;

typedef void (*TextOutputFuncType)(wchar_t* text, long numChars);

typedef long (*InitializeType)(const wchar_t* imageName, int clientType, HINSTANCE appInstance,
			   HANDLE* thread, HWND mainWindow, TextOutputFuncType TextOutputFunc);

typedef int (*FuncPtr)();			// this is generic--used for all actual exported functions

static InitializeType InitializePtr = 0;
static void (*BlessThreadPtr)()		= 0;
static void (*UnblessThreadPtr)()	= 0;
typedef void* (*GetCallbackFunctionType)(wchar_t* functionName, wchar_t* package);
typedef void (*LoadFuncPtrType)(char*);

void OutputText(wchar_t* text, long numBytes);

static GetCallbackFunctionType GetCallbackFunctionPtr = 0;

static TCHAR moduleFileName[1024] = {0};
static wchar_t imageFileName [1024] = {0};

#define WIN_APP_CLIENT		0
#define CONSOLE_CLIENT		1
#define IDE_CLIENT			2

#define MAX_FUNCTION_NAME	256
#define MAX_PACKAGE_NAME	256

#define PAGE_SIZE			0x1000						// 4k pages

struct ExportNameEntry 
{
	DWORD nameRVA;
	DWORD addressRVA;
};

static IMAGE_EXPORT_DIRECTORY* ExportDirectory = 0;
static int ExportAddressTableSize = 0;
static DWORD* ExportAddressTable = 0;
static int NamePointerTableSize = 0;
static DWORD* NamePointerTable = 0;
static WORD* OrdinalTable = 0;
static DWORD* FunctionsTable = 0;
static HANDLE Console = 0;
static char* DllName = 0;

extern "C" int __stdcall DllMain(HINSTANCE hInstance, DWORD dwReason, LPVOID /*lpReserved*/)
{
	char* result = 0;
	BOOL ret = 0;
	module = hInstance;
	switch (dwReason)
	{
		case DLL_PROCESS_ATTACH:
			break;
		case DLL_PROCESS_DETACH:
			if (Console)
				CloseHandle(Console);
			break;
		case DLL_THREAD_ATTACH:
		//	if (BlessThreadPtr)
		//		BlessThreadPtr();
			break;
		case DLL_THREAD_DETACH:
		//	if (UnlessThreadPtr)
		//		UnlessThreadPtr();
			break;
	}
	return 1;   // ok
}

#define SizeOfNtSignature 4
char* KernelDllName = "CormanLispServer.dll";
char* ImageFileName = "CormanLisp.img";
char* Copyright = 0;
char* RegisteredName = 0;
char* RegisteredOrganization = 0;

static char buf[2048];

// Returns 0 if successful, -1 if an error occurs.
int initLib()
{
	if (kernelModule)
		return 0;

	IMAGE_DOS_HEADER* dosHeader = (IMAGE_DOS_HEADER*)module;
	IMAGE_FILE_HEADER* imageHeaderPos = (IMAGE_FILE_HEADER*)(((char*)module) + dosHeader->e_lfanew + SizeOfNtSignature);
	IMAGE_OPTIONAL_HEADER* optionalHeader = (IMAGE_OPTIONAL_HEADER*)(imageHeaderPos + 1);
	ExportDirectory = (IMAGE_EXPORT_DIRECTORY*)
			(((char*)module) + optionalHeader->DataDirectory[0].VirtualAddress);
	ExportAddressTableSize = ExportDirectory->NumberOfFunctions;
	ExportAddressTable = (DWORD*)(((char*)module) + ExportDirectory->AddressOfFunctions);
	DllName = ((char*)module) + ExportDirectory->Name;

	// find the .corman section--it should contain the kernel DLL name and image 
	// file name
	int numberOfSections = imageHeaderPos->NumberOfSections;
	IMAGE_SECTION_HEADER* section = (IMAGE_SECTION_HEADER*)(optionalHeader + 1);
	for (int i = 0; i < numberOfSections; i++)
	{
		if (!strcmp((char*)section[i].Name, ".corman"))
		{
			char* p = (char*)module + section[i].VirtualAddress;
			KernelDllName			= p;			while (*p) p++; p++;
			ImageFileName			= p;			while (*p) p++; p++;
			Copyright				= p;			while (*p) p++; p++;
			RegisteredName			= p;			while (*p) p++; p++;
			RegisteredOrganization	= p;
		}
	}

	kernelModule = LoadLibrary(KernelDllName);
	if (!kernelModule)
	{
		sprintf_s(buf, sizeof(buf), "Could not load the Corman Lisp kernel %s", KernelDllName);
		MessageBox(NULL, buf,
			NULL,
			MB_OK | MB_ICONERROR);
		return -1;
	}

	if (RegisteredName[0] == 0)		// if not created from a registered version
	{
		DWORD ticks = GetTickCount();
		if (((ticks / 1000) % 10) == 0)
		{
			// one out of ten times, throw up message
			sprintf_s(buf, sizeof(buf),
				"The library %s was created from an evaluation or "
				"personal version of Corman Lisp, and is not suitable for distribution. "
				"This message does not imply that any license violation has occurred, but "
				"is designed to be displayed occasionally as a reminder. "
				"DLLs created with licensed copies of Corman Lisp will never display this "
				"message.", 
				DllName);
			MessageBox(NULL, buf,
				"Info",
				MB_OK);
		}
	}

	// make sure this page is not write-protected
	DWORD oldProtect;
	BOOL result = VirtualProtect((void*)(((DWORD)ExportAddressTable) & ~0xfff), 
			PAGE_SIZE,	PAGE_EXECUTE_READWRITE, &oldProtect);
	if (!result)
	{
		MessageBox(NULL, "Could not write to function address table of module",
			NULL,
			MB_OK | MB_ICONERROR);
		return -1;
	}

	NamePointerTableSize = ExportDirectory->NumberOfNames;
	NamePointerTable = (DWORD*)(((char*)module) + ExportDirectory->AddressOfNames);
	OrdinalTable = (WORD*)(((char*)module) + ExportDirectory->AddressOfNameOrdinals);
	FunctionsTable = (DWORD*)(((char*)module) + ExportDirectory->AddressOfFunctions);

	// set up dynamic calls into the kernel
	InitializePtr = (InitializeType)GetProcAddress(kernelModule, "Initialize");
	BlessThreadPtr = (void (*)())GetProcAddress(kernelModule, "BlessThread");
	UnblessThreadPtr = (void (*)())GetProcAddress(kernelModule, "UnblessThread");
	GetCallbackFunctionPtr = (GetCallbackFunctionType)GetProcAddress(kernelModule, "GetCallbackFunctionPointer");

	// see if the Corman Lisp kernel is initialized
	HANDLE ret = OpenMutex(NULL, FALSE, "CormanLispServer");
	HANDLE thread = 0;
	if (!ret)
	{
		int chars = GetModuleFileName(module, moduleFileName, sizeof(moduleFileName));
		int lastSlash = 0;
		TCHAR* p = moduleFileName;
		while (*p)
		{
			if (*p == '\\' || *p == '/')
				lastSlash = p - moduleFileName;
			p++;
		}
		int i = 0;
		for (; i < lastSlash; i++)
			imageFileName[i] = moduleFileName[i];
		if (lastSlash > 0)
			imageFileName[i++] = '/';
		char* name = ImageFileName;
		char* c = name;
		while (*c)
		{
			imageFileName[i++] = *c;
			c++;
		}
		imageFileName[i++] = 0;

		Console = CreateConsoleScreenBuffer(GENERIC_READ|GENERIC_WRITE,
				FILE_SHARE_READ|FILE_SHARE_WRITE, NULL, CONSOLE_TEXTMODE_BUFFER, NULL);

		if (InitializePtr)
			InitializePtr(imageFileName, WIN_APP_CLIENT, module, &thread, 0, OutputText);
		
		BlessThreadPtr();

		LoadFuncPtrType LoadFuncPtr = 
			(LoadFuncPtrType)GetCallbackFunctionPtr(L"LOAD_FILE", L"CCL");

		if (!LoadFuncPtr)
		{
			MessageBox(NULL, "Could not find function CCL::LOAD_FILE in lisp image",
				NULL,
				MB_OK | MB_ICONERROR);
			return -1;
		}
		LoadFuncPtr(moduleFileName);	// load compiled lisp code from this DLL
	}
	return 0;
}

// returns the index into the ordinal/name tables of the requested ordinal
int findOrdinal(int ordinal)
{
	ordinal -= ExportDirectory->Base;	// values in the table are 0-based
	for (int i = 0; i < NamePointerTableSize; i++)
	{
		if (OrdinalTable[i] == ordinal)
			return i;
	}
	return -1;
}

// Receives the ordinal of the desired function
// Returns 0 if the function was not found.
FuncPtr getFunction(int ordinal)
{
	initLib();
	int offset = findOrdinal(ordinal);
	if (offset == -1)
		return 0;
	char* name = ((char*)module) + NamePointerTable[offset];
	FuncPtr func = 0;
	wchar_t functionName[MAX_FUNCTION_NAME];
	wchar_t packageName [MAX_PACKAGE_NAME];
	if (GetCallbackFunctionPtr)
	{
		int i = 0;
		char* p = name;
		while (*p)
		{
			if (*p == '_' && *(p + 1) == '_')	// package separator
			{
				p += 2;
				break;
			}
			packageName[i++] = *p;
			p++;
		}
		packageName[i] = 0;					// null terminate name
		int packageNameLength = i;
		i = 0;
		while (*p)
		{
			functionName[i++] = *p;
			p++;
		}
		functionName[i] = 0;				// null terminate name

		if (i == 0)
		{
			wcscpy_s(functionName, sizeof(functionName), packageName);	// no package was specified
			packageName[0] = 0;
		}
		if (wcslen(functionName) == 0)							
			return 0;			// Package can be empty, but not function name.
								// Empty package signifies lookup custom export name.
	
		func = (FuncPtr)GetCallbackFunctionPtr(functionName, 
				packageName[0] ? packageName : 0);
	}
	return func;
}

void OutputText(wchar_t* text, long numBytes)
{
	DWORD written = 0;

	byte* buf = (byte*)text;
	WriteConsoleW(Console, text, numBytes, &written, NULL);
}

#pragma warning(disable: 4731)	// EBP modified by function

#define ExportedFunc(name, index)		\
	extern "C" void CCL__##name()		\
	{									\
		static FuncPtr func = 0;		\
		if (!func)						\
			func = getFunction(index);	\
		__asm pop ebp					\
		__asm jmp dword ptr [func]		\
	}

ExportedFunc(F001, 1)
ExportedFunc(F002, 2)
ExportedFunc(F003, 3)
ExportedFunc(F004, 4)
ExportedFunc(F005, 5)
ExportedFunc(F006, 6)
ExportedFunc(F007, 7)
ExportedFunc(F008, 8)
ExportedFunc(F009, 9)
ExportedFunc(F010, 10)
ExportedFunc(F011, 11)
ExportedFunc(F012, 12)
ExportedFunc(F013, 13)
ExportedFunc(F014, 14)
ExportedFunc(F015, 15)
ExportedFunc(F016, 16)
ExportedFunc(F017, 17)
ExportedFunc(F018, 18)
ExportedFunc(F019, 19)
ExportedFunc(F020, 20)
ExportedFunc(F021, 21)
ExportedFunc(F022, 22)
ExportedFunc(F023, 23)
ExportedFunc(F024, 24)
ExportedFunc(F025, 25)
ExportedFunc(F026, 26)
ExportedFunc(F027, 27)
ExportedFunc(F028, 28)
ExportedFunc(F029, 29)
ExportedFunc(F030, 30)
ExportedFunc(F031, 31)
ExportedFunc(F032, 32)
ExportedFunc(F033, 33)
ExportedFunc(F034, 34)
ExportedFunc(F035, 35)
ExportedFunc(F036, 36)
ExportedFunc(F037, 37)
ExportedFunc(F038, 38)
ExportedFunc(F039, 39)
ExportedFunc(F040, 40)
ExportedFunc(F041, 41)
ExportedFunc(F042, 42)
ExportedFunc(F043, 43)
ExportedFunc(F044, 44)
ExportedFunc(F045, 45)
ExportedFunc(F046, 46)
ExportedFunc(F047, 47)
ExportedFunc(F048, 48)
ExportedFunc(F049, 49)
ExportedFunc(F050, 50)
ExportedFunc(F051, 51)
ExportedFunc(F052, 52)
ExportedFunc(F053, 53)
ExportedFunc(F054, 54)
ExportedFunc(F055, 55)
ExportedFunc(F056, 56)
ExportedFunc(F057, 57)
ExportedFunc(F058, 58)
ExportedFunc(F059, 59)
ExportedFunc(F060, 60)
ExportedFunc(F061, 61)
ExportedFunc(F062, 62)
ExportedFunc(F063, 63)
ExportedFunc(F064, 64)
ExportedFunc(F065, 65)
ExportedFunc(F066, 66)
ExportedFunc(F067, 67)
ExportedFunc(F068, 68)
ExportedFunc(F069, 69)
ExportedFunc(F070, 70)
ExportedFunc(F071, 71)
ExportedFunc(F072, 72)
ExportedFunc(F073, 73)
ExportedFunc(F074, 74)
ExportedFunc(F075, 75)
ExportedFunc(F076, 76)
ExportedFunc(F077, 77)
ExportedFunc(F078, 78)
ExportedFunc(F079, 79)
ExportedFunc(F080, 80)
ExportedFunc(F081, 81)
ExportedFunc(F082, 82)
ExportedFunc(F083, 83)
ExportedFunc(F084, 84)
ExportedFunc(F085, 85)
ExportedFunc(F086, 86)
ExportedFunc(F087, 87)
ExportedFunc(F088, 88)
ExportedFunc(F089, 89)
ExportedFunc(F090, 90)
ExportedFunc(F091, 91)
ExportedFunc(F092, 92)
ExportedFunc(F093, 93)
ExportedFunc(F094, 94)
ExportedFunc(F095, 95)
ExportedFunc(F096, 96)
ExportedFunc(F097, 97)
ExportedFunc(F098, 98)
ExportedFunc(F099, 99)
ExportedFunc(F100, 100)
