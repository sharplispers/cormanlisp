//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		LispThreadQueue.cpp
//		Contents:	Thread data structure implementation for Corman Lisp.
//		History:	8/1/97  RGC Created.
//					2/8/99  RGC Modified LispThreadQueue::ensureSafeStates() to
//								increment the wait milliseconds each time
//								through the loop, in hope of avoiding 
//								having the other thread not get time to run.
//

#include "stdafx.h"
#include "Lisp.h"

CriticalSection TQCriticalSection;

ThreadRecord::ThreadRecord() 
:  event(FALSE, TRUE), next(0), QV_rec(0), 
   thread(0), threadID(0), stackStart(0), started(0), type(-1), image_loads_count(0)
{
}

ThreadRecord::~ThreadRecord() 
{
}

LispThreadQueue::LispThreadQueue() 
: list(0)
{
	// initialize wait timer
	waitEvent = CreateEvent(NULL, TRUE, FALSE, "Thread Wait Timer");
}

LispThreadQueue::~LispThreadQueue()
{
	ThreadRecord* p = 0;
	while (list)
	{
		p = list;
		list = list->next;
		delete p;
	}
}

void 
LispThreadQueue::insert(ThreadRecord* rec)
{
	TQCriticalSection.Enter();
	rec->next = list;
	list = rec;
	TQCriticalSection.Leave();
}

HANDLE 
LispThreadQueue::remove(DWORD threadID)
{
	TQCriticalSection.Enter();
	HANDLE threadHandle = 0;
	ThreadRecord* rec = 0;
	ThreadRecord** prec = &list;
	while (*prec)
	{
		if ((*prec)->threadID == threadID)
			break;
		prec = &((*prec)->next);
	}
	if ((*prec)->threadID == threadID)
	{
		rec = *prec;
		threadHandle = rec->thread;
		*prec = (*prec)->next;
		delete [] rec->QV_rec;
		delete rec;
	}
	// else error
	TQCriticalSection.Leave();
	return threadHandle;
}

HANDLE 
LispThreadQueue::remove(ThreadRecord* rec)
{
	TQCriticalSection.Enter();
	HANDLE threadHandle = 0;
	ThreadRecord** prec = &list;
	while (*prec)
	{
		if (*prec == rec)
			break;
		prec = &((*prec)->next);
	}
	if (*prec == rec)
	{
		threadHandle = rec->thread;
		*prec = (*prec)->next;
		delete [] rec->QV_rec;
		delete rec;
	}
	// else error
	TQCriticalSection.Leave();
	return threadHandle;
}

void 
LispThreadQueue::suspendAllOtherThreads()
{
    DWORD ret = 0;
    int wait = 1;
	TQCriticalSection.Enter();
	ThreadRecord* tr = list;
	ThreadRecord* currThread = (ThreadRecord*)TlsGetValue(Thread_Index);
	while (tr)
	{
		if (tr != currThread)
        {
        retry_suspend:
			ret = SuspendThread(tr->thread);
            if (ret == (DWORD)-1)
            {
                Sleep(wait++);
                goto retry_suspend;
            }
        }
		tr = tr->next;
	}
	TQCriticalSection.Leave();
}

// This does not require obtaining the lock.
// It is used by the Memory reporting (which suspends all threads,
// and is careful not to modify anything.
//
void LispThreadQueue::suspendAllOtherThreadsWithoutLocking()
{
	ThreadRecord* tr = list;
	ThreadRecord* currThread = (ThreadRecord*)TlsGetValue(Thread_Index);
	while (tr)
	{
		if (tr != currThread)
			SuspendThread(tr->thread);
		tr = tr->next;
	}
}

void LispThreadQueue::resumeAllOtherThreads()
{
	TQCriticalSection.Enter();
	ThreadRecord* tr = list;
	ThreadRecord* currThread = (ThreadRecord*)TlsGetValue(Thread_Index);
	while (tr)
	{
		if (tr != currThread)
			ResumeThread(tr->thread);
		tr = tr->next;
	}
	TQCriticalSection.Leave();
}

// This does not require obtaining the lock.
// It is used by the Memory reporting (which suspends all threads,
// and is careful not to modify anything.
//
void LispThreadQueue::resumeAllOtherThreadsWithoutLocking()
{
	ThreadRecord* tr = list;
	ThreadRecord* currThread = (ThreadRecord*)TlsGetValue(Thread_Index);
	while (tr)
	{
		if (tr != currThread)
			ResumeThread(tr->thread);
		tr = tr->next;
	}
}

ThreadRecord* LispThreadQueue::getList()
{
	return list;
}

ThreadRecord* LispThreadQueue::getPrimaryThread()
{
	ThreadRecord* tr = 0;
	TQCriticalSection.Enter();
	for (tr = list; tr != 0; tr = tr->next)
		if (tr->type == ThreadRecord::PrimaryThread)
			break;
	TQCriticalSection.Leave();
	return tr;
}

void LispThreadQueue::Lock()
{
	TQCriticalSection.Enter();
}

void LispThreadQueue::Unlock()
{
	TQCriticalSection.Leave();
}

void LispThreadQueue::delay(int ms)
{
	// delay ms milliseconds
	Sleep(ms);
}

#define DirectionFlagSet(flags) ((flags) & 0x400)	// check bit 10 of flags
extern bool inAnyLispHeap(DWORD addr);
extern int inSysGlobalsHeap(DWORD addr);

// Make sure none of the other threads are in lisp atomic units.
// To do this, check each thread. If the IP points into the Lisp heap,
// and the direction flag is set (flags bit 10), then it is in a
// lisp atomic unit. If this is the case, wake up that thread, 
// wait 1 millisecond and try again.
// Note that if the lisp thread tries to allocate memory, it will block
// on this (already occupied) critical section, which is good! At least it 
// will be in a safe spot when we try again.
//
// Also unsafe are the stack entry and exit EBP chaining instructions.
//	
void LispThreadQueue::ensureSafeStates()
{
	if (NumLispThreads > 1)
	{
		ThreadRecord* currThread = (ThreadRecord*)TlsGetValue(Thread_Index);
		ThreadRecord* tr = getList();
		static CONTEXT lispContext;
		while (tr)
		{
			if (tr != currThread)	// we know this thread is safe
				ensureSafeState(tr);
			tr = tr->next;
		}
	}
}

// Make sure the given thread is in a safe state (for GC or forced control transfer)
// It is in a safe state if it is not in a function prolog/epilog
// ie a PUSH EBP, MOV EBP, ESP pair or a POP EBP, RET pair
// or the direction flag is not set (which, if set, indicates an atomic
// sequence of instructions is being executed).
// This function assumes that the passed thread has already been suspended
// prior to this call.
//
void LispThreadQueue::ensureSafeState(ThreadRecord* tr)
{
	static CONTEXT lispContext;
    DWORD ret = 0;
	lispContext.ContextFlags = CONTEXT_CONTROL;
	GetThreadContext(tr->thread, &lispContext);
	DWORD ip = lispContext.Eip;
	DWORD flags = lispContext.EFlags;
	int wait = 1;
	while ((/*inAnyLispHeap(ip) &&*/ DirectionFlagSet(flags))
			|| inSysGlobalsHeap(ip)
			|| (ip &&
			   ((*(unsigned char*)ip == 0x55)	// push ebp
				|| (*(unsigned char*)ip == 0x8B && ((unsigned char*)ip)[1] == 0xEC) // mov ebp, esp
				|| (*(unsigned char*)ip == 0x5D)	// pop ebp
				|| (*(unsigned char*)ip == 0xC3 && ((unsigned char*)ip)[-1] == 0x5D/*&& inAnyLispHeap(ip)*/)	// ret
	//			|| (*(unsigned char*)ip == 0xFD)	// std  -- begin-atomic section
				|| (((unsigned char*)ip)[-1] == 0xFC)))) // cld -- end of atomic section
	{
		// unsafe--need to wake up the thread and wait
        // We lock the ThreadList during this time to prevent this thread
        // from attempting to modify the thread list
        //
        ThreadList.Lock();
		ResumeThread(tr->thread);
    retry_suspend:
		Sleep(wait++);	// wait 1 or more milliseconds
		ret = SuspendThread(tr->thread);	// try again
        if (ret == (DWORD)-1)
            goto retry_suspend;

        ThreadList.Unlock();

		ThreadRecord* tr2 = getList();
		// see if the thread is still alive
		while (tr2)
		{
			if (tr2 == tr)
				break;
			else
				tr2 = tr2->next;
		}
		if (tr2 == tr)
		{
			// still alive--update ip and flag information
			lispContext.ContextFlags = CONTEXT_CONTROL;
			GetThreadContext(tr->thread, &lispContext);
			ip = lispContext.Eip;
			flags = lispContext.EFlags;
			continue;
		}
	}
}


//
//	Given a buffer of size DWORDs, populates the buffer with
//	the thread IDs of all currently active lisp threads.
//	Returns the number of thread IDs actually stored in the buffer.
//
DWORD LispThreadQueue::GetLispThreadIDs(DWORD* buf, int size)
{
	TQCriticalSection.Enter();
	ThreadRecord* tr = list;
	int i = 0;
	while (tr && i < size)
	{
		buf[i++] = tr->threadID;
		tr = tr->next;
	}
	TQCriticalSection.Leave();
	return i;
}

HANDLE LispThreadQueue::GetLispThreadHandle(DWORD id)
{
	TQCriticalSection.Enter();
	ThreadRecord* tr = list;
	int i = 0;
	HANDLE ret = 0;
	while (tr)
	{
		if (id == tr->threadID)
		{
			ret = tr->thread;
			break;
		}
		tr = tr->next;
	}
	if (!tr)
		ret = 0;
	TQCriticalSection.Leave();
	return ret;
}
