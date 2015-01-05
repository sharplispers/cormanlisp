//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//	File:			license.cpp
//	Contents:		Registration module
//	History:		5/28/02  RGC  Created.
//

#include <windows.h>
#include <time.h>
#include <stdio.h>

#include "EncryptDES.h"
#include "license.h"

#define EncryptionKey_1_4 "ArtHills"
#define EncryptionKey_1_5 "Fabian15"
#define EncryptionKey_2_0 "WoodyWoo"
#define EncryptionKey_2_5 "NezzyIdy"
#define EncryptionKey_3_0 "EmitIdy"

const char* RegKeyValues[][2] =
{
  { __TEXT("CormanLispIDE.Lisp"), __TEXT("Lisp") },
  { __TEXT("CormanLispIDE.Lisp\\CLSID"), __TEXT("{DB2C7123-62F8-11D2-9A73-004095076FF7}") },
  { __TEXT("CLSID\\{DB2C7123-62F8-11D2-9A73-004095076FF7}"), __TEXT("Lisp") },
  { __TEXT("CLSID\\{DB2C7123-62F8-11D2-9A73-004095076FF7}\\3.0\\RegistrationName"), "registered name" },
  { __TEXT("CLSID\\{DB2C7123-62F8-11D2-9A73-004095076FF7}\\3.0\\RegistrationOrganization"), __TEXT("organization") },
  { __TEXT("CLSID\\{DB2C7123-62F8-11D2-9A73-004095076FF7}\\3.0\\RegistrationCode"), __TEXT("code") },
};

const int RegKeyValuesNum = sizeof(RegKeyValues)
                            /sizeof(*RegKeyValues);

const char* TimeoutRegKeyValues[][2] =
{
  { __TEXT("CormanLispIDE.Lisp"), __TEXT("Lisp") },
  { __TEXT("CormanLispIDE.Lisp\\CLSID"), __TEXT("{DB2C7123-62F8-11D2-9A73-004095076FF7}") },
  { __TEXT("CLSID\\{DB2C7123-62F8-11D2-9A73-004095076FF7}"), __TEXT("Lisp") },
  { __TEXT("CLSID\\{DB2C7123-62F8-11D2-9A73-004095076FF7}\\2.6\\VersionInfo"), "to26" },
};

const int TimeoutRegKeyValuesNum = sizeof(TimeoutRegKeyValues)/sizeof(*TimeoutRegKeyValues);

static bool InstallTimeout(int days);

static EncryptDES des;

static long generateRegistrationCode(const char* name,
									 const char* organization, const char* key)
{
	static char buf[4096];
	static unsigned char outbuf[4096];

	char* p = 0;
	char* q = 0;
	int length = 0;
	long* longs = 0;
	long sum = 0;
	int numLongs = 0;
	int i = 0;

	p = (char*)name;
	q = buf;
	while (*p)
	{
		if (isalnum(*p))
		{
			if (islower(*p))
				*q = _toupper(*p);
			else
				*q = *p;
			q++;
		}
		p++;
	}
	p = (char*)organization;
	while (*p)
	{
		if (isalnum(*p))
		{
			if (islower(*p))
				*q = _toupper(*p);
			else
				*q = *p;
			q++;
		}
		p++;
	}
	*q = 0;

	length = (int)(q - buf);

	des.encrypt(key, (unsigned char*)buf, length);
	des.exportBinary(outbuf);
	longs = (long*)outbuf;

	numLongs = length / 4;
	for (i = 0; i < numLongs; i++)
		sum += longs[i];
	if (sum < 0)
		sum = -sum;
	return sum;
}

//
//	Returns:
//	0,  if the supplied code is not valid.
//	14, if the supplied code is for version 1.4 or prior
//  15, if the supplied code is for version 1.5
//  20, if the supplied code is for version 2.0
//  25, if the supplied code is for version 2.5
//  30, if the supplied code is for version 3.0
//
extern "C" int __declspec(dllexport)
CheckRegistration(const char* name, const char* organization, const char* registrationCode)
{
	long n = 0;
	long code = 0;

	n = atoi(registrationCode);
	if (n == 0)
		return 0;
	code = generateRegistrationCode(name, organization, EncryptionKey_3_0);
	if (code != 0 && n == code)
		return 30;
	code = generateRegistrationCode(name, organization, EncryptionKey_2_5);
	if (code != 0 && n == code)
		return 25;
	code = generateRegistrationCode(name, organization, EncryptionKey_2_0);
	if (code != 0 && n == code)
		return 20;
	code = generateRegistrationCode(name, organization, EncryptionKey_1_5);
	if (code != 0 && n == code)
		return 15;
	code = generateRegistrationCode(name, organization, EncryptionKey_1_4);
	if (code != 0 && n == code)
		return 14;
	return 0;
}

//
//	Returns true if successful, false otherwise.
//
extern "C" bool __declspec(dllexport)
Register(const char* name, const char* organization, const char* code)
{
    LONG r = 0;
	int i = 0;

	r = ERROR_SUCCESS;
	RegKeyValues[3][1] = name;
	RegKeyValues[4][1] = organization;
	RegKeyValues[5][1] = code;

    for (i = 0; r == ERROR_SUCCESS && i < RegKeyValuesNum; i++)
        r = RegSetValue(HKEY_CLASSES_ROOT,
                        RegKeyValues[i][0],
                        REG_SZ,
                        RegKeyValues[i][1],
                        lstrlen(RegKeyValues[i][1]));
    if (r != ERROR_SUCCESS)
        Unregister();
    return (r == ERROR_SUCCESS) ? true : false;
}


//
//	Returns true if successful, false otherwise.
//
extern "C" bool __declspec(dllexport)
Unregister()
{
    HRESULT result = 0;
	int i = 0;
	LONG r = 0;

	result = S_OK;
    for (i = RegKeyValuesNum - 1; i >= 0; i--)
    {
        r = RegDeleteKey(HKEY_CLASSES_ROOT,
                              RegKeyValues[i][0]);
        if (r != ERROR_SUCCESS)
            result = S_FALSE;
    }
    return result == S_OK ? true : false;
}

extern "C" void __declspec(dllexport) GetRegistrationInfo(RegistrationInfo* info)
{
	long size = 0;
	LONG ret = 0;
	time_t ltime = 0;
	time_t currTime = 0;

	memset(info, 0, sizeof(*info));

	// get name
	size = sizeof(info->name);
	info->name[0] = 0;
	ret = RegQueryValue(HKEY_CLASSES_ROOT,
							 RegKeyValues[3][0],
							 (char*)&(info->name),
							 &size);

	// get organization
	size = sizeof(info->organization);
	info->organization[0] = 0;
	ret = RegQueryValue(HKEY_CLASSES_ROOT,
							 RegKeyValues[4][0],
							 (char*)&(info->organization),
							 &size);

	// get registration code
	static char regbuf[16];
	size = sizeof(regbuf);
	regbuf[0] = 0;
	ret = RegQueryValue(HKEY_CLASSES_ROOT,
							 RegKeyValues[5][0],
							 (char*)&regbuf,
							 &size);
	if (regbuf[0])
		info->registrationCode = atoi(regbuf);

	// get timeout
	static char timeoutbuf[128];
	size = sizeof(timeoutbuf);
	timeoutbuf[0] = 0;
	ret = RegQueryValue(HKEY_CLASSES_ROOT,
							 TimeoutRegKeyValues[3][0],
							 timeoutbuf,
							 &size);
	if (ret != ERROR_SUCCESS)
	{
		// install 30 day evaluation time
		InstallTimeout(30);
		timeoutbuf[0] = 0;
		ret = RegQueryValue(HKEY_CLASSES_ROOT,
								TimeoutRegKeyValues[3][0],
								timeoutbuf,
								&size);
	}

	if (ret != ERROR_SUCCESS)
	{
		info->timeout = -1;
		info->daysRemaining = 0;
	}
	else
	if (timeoutbuf[0])
	{
		currTime = time(&ltime);
		info->timeout = atoi(timeoutbuf);
		info->daysRemaining =
			static_cast<int>(((time_t)(info->timeout) - currTime))
				/ (60 * 60 * 24);
	}
	info->version = CheckRegistration(info->name, info->organization, regbuf);
	info->isRegistered = (info->version >= 25);
}

//
//	Returns true if successful, false otherwise.
//
bool InstallTimeout(int days)
{
    LONG r = 0;
	int i = 0;

	r = ERROR_SUCCESS;
	static char buf[128];
	time_t ltime;
	time_t currentTime = time(&ltime);
	sprintf_s(buf, sizeof(buf), "%d", currentTime + (24 * 60 * 60 * days)
					+ ((24 * 60 * 60) - 1));

	TimeoutRegKeyValues[3][1] = buf;

    for (i = 0; r == ERROR_SUCCESS && i < TimeoutRegKeyValuesNum; i++)
        r = RegSetValue(HKEY_CLASSES_ROOT,
                        TimeoutRegKeyValues[i][0],
                        REG_SZ,
                        TimeoutRegKeyValues[i][1],
                        lstrlen(TimeoutRegKeyValues[i][1]));
	return (r == ERROR_SUCCESS) ? true : false;
}

#define DEBUG

#if 0
extern "C" LONG __declspec(dllexport) WINAPI
ValidateSN(HWND hwnd, LPSTR szSrcDir, LPSTR szSupport, LPSTR szSerialNum, LPSTR szDbase)
{
#ifdef DEBUG  //Display Debug information
	CHAR szTmp[1024];
	wsprintf(szTmp, "szSerialNum=%s \nszSrcDir=%s \nszSupport=%s \nszDbase=%s", szSerialNum, szSrcDir, szSupport, szDbase);
	MessageBox(GetFocus(), szTmp, "Serial Number Debug Window", MB_OK|MB_SYSTEMMODAL);
#endif

	return 1;	// success
}

#endif
