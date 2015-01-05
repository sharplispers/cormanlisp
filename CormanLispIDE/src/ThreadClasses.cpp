//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		ThreadClasses.cpp
//		Contents:	Thread synchronization classes for Corman Lisp.
//		History:	8/5/97  RGC Created.
//

#include <assert.h>

#include "ThreadClasses.h"

PLEvent::PLEvent(BOOL bInitiallyOwn, BOOL bManualReset, LPCTSTR pstrName,
	LPSECURITY_ATTRIBUTES lpsaAttribute)
	: PLSyncObject(pstrName)
{
	m_hObject = ::CreateEvent(lpsaAttribute, bManualReset,
		bInitiallyOwn, pstrName);
}

PLEvent::~PLEvent()
{
}

BOOL PLEvent::Unlock()
{
	return TRUE;
}

PLSemaphore::PLSemaphore(LONG lInitialCount, LONG lMaxCount,
	LPCTSTR pstrName, LPSECURITY_ATTRIBUTES lpsaAttributes)
	:  PLSyncObject(pstrName)
{
	m_hObject = ::CreateSemaphore(lpsaAttributes, lInitialCount, lMaxCount,
		pstrName);
}

PLSemaphore::~PLSemaphore()
{
}

BOOL PLSemaphore::Unlock(LONG lCount, LPLONG lpPrevCount /* =NULL */)
{
	return ::ReleaseSemaphore(m_hObject, lCount, lpPrevCount);
}

PLSingleLock::PLSingleLock(PLSyncObject* pObject, BOOL bInitialLock)
{
	m_pObject = pObject;
	m_hObject = pObject->m_hObject;
	m_bAcquired = FALSE;

	if (bInitialLock)
		Lock();
}

BOOL PLSingleLock::Lock(DWORD dwTimeOut /* = INFINITE */)
{
	assert(!m_bAcquired);

	m_bAcquired = m_pObject->Lock(dwTimeOut);
	return m_bAcquired;
}

BOOL PLSingleLock::Unlock()
{
	if (m_bAcquired)
		m_bAcquired = !m_pObject->Unlock();

	// successfully unlocking means it isn't acquired
	return !m_bAcquired;
}

BOOL PLSingleLock::Unlock(LONG lCount, LPLONG lpPrevCount /* = NULL */)
{
	if (m_bAcquired)
		m_bAcquired = !m_pObject->Unlock(lCount, lpPrevCount);

	// successfully unlocking means it isn't acquired
	return !m_bAcquired;
}

PLSyncObject::PLSyncObject(LPCTSTR /*pstrName*/)
{
	m_hObject = NULL;
}

PLSyncObject::~PLSyncObject()
{
	if (m_hObject != NULL)
	{
		::CloseHandle(m_hObject);
		m_hObject = NULL;
	}
}

BOOL PLSyncObject::Lock(DWORD dwTimeout)
{
	if (::WaitForSingleObject(m_hObject, dwTimeout) == WAIT_OBJECT_0)
		return TRUE;
	else
		return FALSE;
}

