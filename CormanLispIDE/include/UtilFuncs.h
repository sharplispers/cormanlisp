//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		UtilFuncs.h
//		Contents:	Corman Lisp IDE Utility Function declarations.
//		History:	10/15/98  RGC  Created.
//
#ifndef UTILFUNCS_H
#define UTILFUNCS_H

extern BYTE* MapFile(const char* path, DWORD* length);
extern void UnmapFile(BYTE* mapping);
extern CFont* getDefaultFont(HDC, long size);

#endif // UTILFUNCS_H