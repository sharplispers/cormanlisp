//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		CoEnumConnectionPoints.cpp
//		Contents:	COM Class implementation for Corman Lisp COM server.
//		History:	8/11/97  RGC  Created.
//

#include "stdafx.h"
#include <windows.h>
#include <ole2.h>

#include "CoEnumConnectionPoints.h"

extern void SvrLock(void);
extern void SvrUnlock(void);

#ifdef _LOCALSERVER
const BOOL g_bIsDLL = FALSE;
#else
const BOOL g_bIsDLL = TRUE;
#endif

CoEnumConnectionPoints::CoEnumConnectionPoints(IUnknown* pUnk, ULONG cPoints, 
					IConnectionPoint** rgpConnectionPoint)
{
	UINT i = 0;
	m_cRef = 0;
	m_pUnkRef = pUnk;

	m_iPosition = 0;
	m_cPoints = cPoints;

	m_rgpConnectionPoint = new IConnectionPoint* [(UINT)cPoints];

	if (m_rgpConnectionPoint)
	{
		for (i = 0; i < cPoints; i++)
		{
			m_rgpConnectionPoint[i] = rgpConnectionPoint[i];
			m_rgpConnectionPoint[i]->AddRef();
		}
	}
}

CoEnumConnectionPoints::~CoEnumConnectionPoints()
{
	UINT i = 0;
	if (m_rgpConnectionPoint)
	{
		for (i = 0; i < m_cPoints; i++)
			m_rgpConnectionPoint[i]->Release();

		delete[] m_rgpConnectionPoint;
	}
}

// IUnknown methods
STDMETHODIMP 
CoEnumConnectionPoints::QueryInterface(REFIID riid, void** ppv)
{
	if (riid == IID_IUnknown)
		*ppv = (IEnumConnectionPoints*)this;
	else if (riid == IID_IEnumConnectionPoints)
		*ppv = (IEnumConnectionPoints*)this;
	else
		*ppv = 0;

	if (*ppv)
		((IUnknown*)*ppv)->AddRef();

	return *ppv ? S_OK : E_NOINTERFACE;
}

STDMETHODIMP_(ULONG) 
CoEnumConnectionPoints::AddRef()
{
    return InterlockedIncrement(&m_cRef);
}

STDMETHODIMP_(ULONG) 
CoEnumConnectionPoints::Release()
{
    if (InterlockedDecrement(&m_cRef) != 0)
        return m_cRef;
    delete this;
    return 0;
}

// IEnumConnectionPoints methods
STDMETHODIMP 
CoEnumConnectionPoints::Next(ULONG cPoints, IConnectionPoint** rgpConnectionPoint, ULONG* pcFetched)
{
    ULONG cReturn = 0;

    if (!m_rgpConnectionPoint)
        return S_FALSE;

    if (!pcFetched)
	{
		if (cPoints != 1)
			return E_POINTER;
	}
    else
        *pcFetched = 0;

    if (!rgpConnectionPoint || m_iPosition >= m_cPoints)
        return S_FALSE;

	while (m_iPosition < m_cPoints && cPoints > 0)
	{
		*rgpConnectionPoint = m_rgpConnectionPoint[m_iPosition++];

		if (*rgpConnectionPoint)
			(*rgpConnectionPoint)->AddRef();

		rgpConnectionPoint++;
		cReturn++;
        cPoints--;
	}

    if (pcFetched)
        *pcFetched=cReturn;

    return S_OK;
}

STDMETHODIMP 
CoEnumConnectionPoints::Skip(ULONG cPoints)
{
    if ((m_iPosition + cPoints) >= m_cPoints || !m_rgpConnectionPoint)
        return S_FALSE;

    m_iPosition += cPoints;
    return S_OK;
}

STDMETHODIMP 
CoEnumConnectionPoints::Reset()
{
	m_iPosition = 0;
	return S_OK;
}

STDMETHODIMP 
CoEnumConnectionPoints::Clone(IEnumConnectionPoints** ppEnumConnectionPoints)
{
    CoEnumConnectionPoints* pClone = 0;

    *ppEnumConnectionPoints = 0;

    //Create the clone
    pClone = new CoEnumConnectionPoints(m_pUnkRef, m_cPoints, m_rgpConnectionPoint);

    if (!pClone)
        return E_OUTOFMEMORY;

    pClone->AddRef();
    pClone->m_iPosition = m_iPosition;

    *ppEnumConnectionPoints = pClone;
    return S_OK;
}

