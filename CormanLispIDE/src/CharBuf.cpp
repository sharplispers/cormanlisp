//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		CharBuf.cpp
//		Contents:	Character buffer class implementation for Corman Lisp.
//		History:	8/5/97  RGC Created.
//
#include "stdafx.h"

#include <assert.h>

#include "CharBuf.h"

CharBuf::CharBuf(long length)
: fCharsAvailable(), fBufEmpty() 
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
	strcpy_s(s, fIndex + 1, fBuf);
	fIndex = 0;
	fBufEmpty.SetEvent();
	return s;
}

long 
CharBuf::getCharsInBuffer(unsigned char* buf, long num)
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
		fBufEmpty.SetEvent();
		return temp;
	}
}

long 
CharBuf::addChars(const char* buf, long num)
{
	long i;
	if (num <= 0)
		return 0;

	PLSingleLock myLock(&fSemaphore, TRUE);
	long spaceAvailable = (fSize - 1) - fIndex;
	if (spaceAvailable < num)
		num = spaceAvailable;
	for (i = 0; i < num; i++)
		fBuf[fIndex++] = buf[i];
	if (i > 0)
	{
		fCharsAvailable.SetEvent();
		BOOL ret = PostMessage(AfxGetApp()->GetMainWnd()->m_hWnd, WM_ENTERIDLE, 0, 0);
	}
	else
	if (i == 0)	// if buffer is full, wait until it is empty
	{
		fBufEmpty.ResetEvent();
		myLock.Unlock();
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

PLEvent*
CharBuf::bufEmpty()
{
	return &fBufEmpty;
}

