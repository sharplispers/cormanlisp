//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		CommonLispFuncs.h
//		Contents:	Corman Lisp application source file
//		History:	1/27/99  RGC  Created.
//

#ifndef COMMONLISPFUNCS_H
#define COMMONLISPFUNCS_H

struct LambdaListString
{
	char* symbol;
	char* lambdaList;
};

void sortLambdaLists();
const char* findLambdaList(const char* symbol);

#endif // COMMONLISPFUNCS_H