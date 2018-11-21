//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		LispThreadQueue.h
//		Contents:	Thread data structure declarations for Corman Lisp.
//		History:	8/1/97  RGC Created.
//

#ifndef LISPTHREADQUEUE_H
#define LISPTHREADQUEUE_H

#include <windows.h>

#include "ThreadClasses.h"

struct ThreadRecord
{
	enum { PrimaryThread = 0, SecondaryThread = 1, BlessedThread = 2 };

	ThreadRecord();
	~ThreadRecord();

	ThreadRecord* next;
	LispObj* QV_rec;
	HANDLE thread;				// Win32 thread handle
	DWORD  threadID;			// Win32 thread ID
	unsigned long* stackStart;
	PLEvent event;
	int started;
	int type;					// 0 = primary lisp, 1 = secondary lisp, 2 = blessed foreign
	LONG image_loads_count;
};

class LispThreadQueue
{
public:
	LispThreadQueue();
	~LispThreadQueue();

	void insert(ThreadRecord*);
	HANDLE remove(DWORD threadID);
	HANDLE remove(ThreadRecord* rec);
	void suspendAllOtherThreads();
	void suspendAllOtherThreadsWithoutLocking();
	void resumeAllOtherThreads();
	void resumeAllOtherThreadsWithoutLocking();
	ThreadRecord* getList();
	void Lock();
	void Unlock();
	void delay(int ms);
	void ensureSafeStates();
	void ensureSafeState(ThreadRecord*);
	DWORD GetLispThreadIDs(DWORD* buf, int size);
	HANDLE GetLispThreadHandle(DWORD id);
	ThreadRecord* getPrimaryThread();

private:
	ThreadRecord* list;
	HANDLE waitEvent;
};

#endif	// LISPTHREADQUEUE_H
