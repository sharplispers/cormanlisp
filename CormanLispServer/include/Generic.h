//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		generic.h
//		Contents:	Definitions which all modules should have.
//		Author:		Roger Corman
//

#ifndef GENERIC_H
#define GENERIC_H

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#define X86		1
#define CORMANLISP 1

typedef unsigned char byte;
typedef long xbool;

#undef CATCH
#undef THROW

#endif // GENERIC_H
