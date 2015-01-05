//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//	File:			license.h
//	Contents:		Registration module declarations
//	History:		5/28/02  RGC  Created.
//

struct RegistrationInfo
{
	char name[256];
	char organization[256];
	int registrationCode;
	int timeout;
	bool isRegistered;
	int daysRemaining;
	int version;
};

extern "C" int __declspec(dllexport)
CheckRegistration(const char* name, const char* organization, const char* registrationCode);
extern "C" bool __declspec(dllexport) Register(const char* name, const char* organization, const char* code);
extern "C" bool __declspec(dllexport) Unregister();
extern "C" void __declspec(dllexport) GetRegistrationInfo(RegistrationInfo* info);

