//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		plserver_internal.h
//		Contents:	Server internal functions for Corman Lisp
//		History:	8/5/97  RGC  Created.
//

#include "CharBuf.h"

extern IUnknown*					ClientUnknown;
extern ICormanLispTextOutput*		ClientTextOutput;
extern ICormanLispStatusMessage*	ClientMessage;

extern CharBuf TerminalInputBuf;
