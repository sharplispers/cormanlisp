//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		CoEnumConnections.cpp
//		Contents:	COM Class implementation for Corman Lisp COM server.
//		History:	8/11/97  RGC  Created.
//

#include "stdafx.h"
#include <windows.h>
#include <ole2.h>
#include <olectl.h>

#include "CoEnumConnections.h"

extern void SvrLock(void);
extern void SvrUnlock(void);

#ifdef _LOCALSERVER
const BOOL g_bIsDLL = FALSE;
#else
const BOOL g_bIsDLL = TRUE;
#endif

CoEnumConnections::CoEnumConnections(IUnknown* pUnk, ULONG cConnections, LPCONNECTDATA prgConnectData)
{
	UINT i;
	m_cRef = 0;
	m_pUnkRef=pUnk;

	m_iPosition=0;
	m_cConnections = cConnections;

	m_rgConnectData = new CONNECTDATA[(UINT)cConnections];

	if (NULL != m_rgConnectData)
	{
		for (i=0; i < cConnections; i++)
		{
			m_rgConnectData[i] = prgConnectData[i];
			m_rgConnectData[i].pUnk->AddRef();
		}
	}
}

CoEnumConnections::~CoEnumConnections()
{
	if (NULL != m_rgConnectData)
	{
		UINT i;

		for (i=0; i < m_cConnections; i++)
			m_rgConnectData[i].pUnk->Release();

		delete[] m_rgConnectData;
	}
}

// IUnknown methods
STDMETHODIMP 
CoEnumConnections::QueryInterface(REFIID riid, void** ppv)
{
	if (riid == IID_IUnknown)
		*ppv = (IEnumConnections*)this;
	else if (riid == IID_IEnumConnections)
		*ppv = (IEnumConnections*)this;
	else
		*ppv = 0;

	if (*ppv)
		((IUnknown*)*ppv)->AddRef();

	return *ppv ? S_OK : E_NOINTERFACE;
}

STDMETHODIMP_(ULONG) 
CoEnumConnections::AddRef()
{
    return InterlockedIncrement(&m_cRef);
}

STDMETHODIMP_(ULONG) 
CoEnumConnections::Release()
{
    if (InterlockedDecrement(&m_cRef) != 0)
        return m_cRef;
    delete this;
    return 0;
}

// IEnumConnections methods
STDMETHODIMP 
CoEnumConnections::Next(ULONG cConnections, CONNECTDATA* rgConnectData, ULONG* pcFetched)
{
    ULONG               cReturn=0L;

    if (NULL==m_rgConnectData)
        return S_FALSE;

    if (NULL==pcFetched)
        {
        if (1L!=cConnections)
            return E_POINTER;
        }
    else
        *pcFetched=0L;

    if (NULL==rgConnectData || m_iPosition >= m_cConnections)
        return S_FALSE;

    while (m_iPosition < m_cConnections && cConnections > 0)
        {
        *rgConnectData++=m_rgConnectData[m_iPosition];
        m_rgConnectData[m_iPosition++].pUnk->AddRef();
        cReturn++;
        cConnections--;
        }

    if (NULL!=pcFetched)
        *pcFetched=cReturn;

    return S_OK;
}

STDMETHODIMP 
CoEnumConnections::Skip(ULONG cConnections)
{
    if (((m_iPosition+cConnections) >= m_cConnections)
	 || NULL==m_rgConnectData)
        return S_FALSE;

    m_iPosition+=cConnections;
    return S_OK;
}

STDMETHODIMP 
CoEnumConnections::Reset()
{
	m_iPosition = 0;
	return S_OK;
}

STDMETHODIMP 
CoEnumConnections::Clone(IEnumConnections** ppEnumConnections)
{
    CoEnumConnections*   pClone;

    *ppEnumConnections=NULL;

    //Create the clone
    pClone=new CoEnumConnections(m_pUnkRef, m_cConnections, m_rgConnectData);

    if (NULL==pClone)
        return E_OUTOFMEMORY;

    pClone->AddRef();
    pClone->m_iPosition=m_iPosition;

    *ppEnumConnections=pClone;
    return S_OK;
}
