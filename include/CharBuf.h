//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		CharBuf.h
//		Contents:	Character buffer class for Corman Lisp.
//		History:	8/5/97  RGC Created.
//

#ifndef CHARBUF_H
#define CHARBUF_H

#include "threadclasses.h"

// The TerminalInputBuf and TerminalOutputBuf arrays (and the indexes)
// are accessed by both threads, so care must be taken when modifying them.
//
class CharBuf
{
public:
	CharBuf(long length); 
	~CharBuf();
	long size();
	long numchars();

	char* getChars(); 
	long getCharsInBuffer(unsigned char* buf, long num); 
	long addChars(const char* buf, long num);
	PLEvent* charsAvailable();
	PLEvent* bufEmpty();

private:
	long fSize;
	char* fBuf;
	long fIndex;
	PLSemaphore fSemaphore;
	PLEvent fCharsAvailable;
	PLEvent fBufEmpty;
};

#endif // CHARBUF_H
