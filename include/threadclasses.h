//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		ThreadClasses.h
//		Contents:	Thread synchronization classes for Corman Lisp.
//		History:	8/5/97  RGC Created.
//

#ifndef THREADCLASSES_H
#define THREADCLASSES_H

#include <windows.h>
#include <assert.h>

class PLSingleLock;
class PLSyncObject
{
public:
	PLSyncObject(LPCTSTR pstrName);
	virtual ~PLSyncObject();

	operator HANDLE() const;
	HANDLE  m_hObject;

	virtual BOOL Lock(DWORD dwTimeout = INFINITE);
	virtual BOOL Unlock() = 0;
	virtual BOOL Unlock(LONG, LPLONG) { return TRUE; }

	friend class PLSingleLock;
};

class PLEvent : public PLSyncObject
{
public:
	PLEvent(BOOL bInitiallyOwn = FALSE, BOOL bManualReset = FALSE,
		LPCTSTR lpszNAme = NULL, LPSECURITY_ATTRIBUTES lpsaAttribute = NULL);
	virtual ~PLEvent();

	BOOL SetEvent();
	BOOL PulseEvent();
	BOOL ResetEvent();
	BOOL Unlock();
};

class PLSemaphore : public PLSyncObject
{
public:
	PLSemaphore(LONG lInitialCount = 1, LONG lMaxCount = 1,
		LPCTSTR pstrName=NULL, LPSECURITY_ATTRIBUTES lpsaAttributes = NULL);
	virtual ~PLSemaphore();

	virtual BOOL Unlock();
	virtual BOOL Unlock(LONG lCount, LPLONG lprevCount = NULL);
};

class PLSingleLock
{
public:
	PLSingleLock(PLSyncObject* pObject, BOOL bInitialLock = FALSE);
	~PLSingleLock();

	BOOL Lock(DWORD dwTimeOut = INFINITE);
	BOOL Unlock();
	BOOL Unlock(LONG lCount, LPLONG lPrevCount = NULL);
	BOOL IsLocked();

protected:
	PLSyncObject* m_pObject;
	HANDLE  m_hObject;
	BOOL    m_bAcquired;
};

class CriticalSection
{
public:
	CriticalSection() { InitializeCriticalSection(&m_sect); }
	~CriticalSection(){	DeleteCriticalSection(&m_sect); }
	void Enter()	  { EnterCriticalSection(&m_sect); }
	void Leave()	  { LeaveCriticalSection(&m_sect); }
public:
	CRITICAL_SECTION m_sect;
};

inline PLSyncObject::operator HANDLE() const
	{ return m_hObject;}

inline BOOL PLSemaphore::Unlock()
	{ return Unlock(1, NULL); }

inline BOOL PLEvent::SetEvent()	  { return ::SetEvent(m_hObject); }
inline BOOL PLEvent::PulseEvent() { return ::PulseEvent(m_hObject); }
inline BOOL PLEvent::ResetEvent() { return ::ResetEvent(m_hObject); }

inline PLSingleLock::~PLSingleLock() { Unlock(); }
inline BOOL PLSingleLock::IsLocked() { return m_bAcquired; }


#endif // THREADCLASSES_H
