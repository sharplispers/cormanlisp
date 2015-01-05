//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		CoConnectionPoint.cpp
//		Contents:	COM Class implementation for Corman Lisp COM server.
//		History:	8/11/97  RGC  Created.
//
#include "stdafx.h"

#include <windows.h>
#include <ole2.h>
#include <olectl.h>

#include "CoConnectionPoint.h"
#include "CoEnumConnections.h"

extern void SvrLock(void);
extern void SvrUnlock(void);

// Declare a const BOOL at global scope that indicates whether we are building
// a DLL or an EXE. Need to conditionally compile this based on preprocessor
// symbol _LOCALSERVER

#ifdef _LOCALSERVER
const BOOL g_bIsDLL = FALSE;
#else
const BOOL g_bIsDLL = TRUE;
#endif

// Set a block size for reallocating additional blocks
// for the ragged array of connected sinks.
const UINT nBlockSize = 8;

CoConnectionPoint::CoConnectionPoint(REFIID riid, IUnknown* pContainer)
{
	m_cRef = 0;
	m_iid = riid;
	m_pUnkContainer = pContainer;
	m_cConnections = 0;
	m_nArraySize = 0;
	m_rgpUnk = NULL;
}

CoConnectionPoint::~CoConnectionPoint()
{
	if (NULL != m_rgpUnk)
	{
		delete[] m_rgpUnk;
	}
}

// Helper functions
BOOL CoConnectionPoint::ExpandArray()
{
	return 0;
}

BOOL CoConnectionPoint::ContractArray()
{
	return 0;
}

UINT CoConnectionPoint::GetFirstEmptyPosition()
{
	return 0;
}

// IUnknown methods
STDMETHODIMP 
CoConnectionPoint::QueryInterface(REFIID riid, void** ppv)
{
	if (riid == IID_IUnknown)
		*ppv = (IConnectionPoint*)this;
	else if (riid == IID_IConnectionPoint)
		*ppv = (IConnectionPoint*)this;
	else
		*ppv = 0;

	if (*ppv)
		((IUnknown*)*ppv)->AddRef();

	return *ppv ? S_OK : E_NOINTERFACE;
}

STDMETHODIMP_(ULONG) 
CoConnectionPoint::AddRef()
{
    return InterlockedIncrement(&m_cRef);
}

STDMETHODIMP_(ULONG) 
CoConnectionPoint::Release()
{
    if (InterlockedDecrement(&m_cRef) != 0)
        return m_cRef;
    delete this;
    return 0;
}

// IConnectionPoint
STDMETHODIMP 
CoConnectionPoint::GetConnectionInterface(IID* pIID)
{
	if (NULL == pIID)
	{
		return E_POINTER;
	}

	*pIID = m_iid;
	return S_OK;
}

STDMETHODIMP 
CoConnectionPoint::GetConnectionPointContainer(IConnectionPointContainer** ppConnectionPointContainer)
{
	return m_pUnkContainer->QueryInterface(IID_IConnectionPointContainer,
	 (void**) ppConnectionPointContainer);
}

STDMETHODIMP 
CoConnectionPoint::Advise(IUnknown* pUnkSink, DWORD* pdwCookie)
{
	*pdwCookie=0;

	UINT cIndex = 0; // Find first blank position
	
	// if connection count is less than array size
	// check for empty array spot.
	if (m_cConnections < m_nArraySize)
	{
		// empty for loop; only want to set CIndex to blank spot
		for (; cIndex < m_nArraySize && m_rgpUnk[cIndex] != 0; cIndex++);
	}
	// No blank cell available, add new block to ragged array
	else
	{
		UINT nArraySize = m_nArraySize + nBlockSize;
		IUnknown** rgpUnk = new IUnknown*[nArraySize];
		
		// If the block can't get bigger, can't connect
		if (!rgpUnk)
			return CONNECT_E_ADVISELIMIT;

		// Copy existing connected sinks, and delete old ragged array
		for (cIndex= 0; cIndex < m_cConnections; cIndex++)
		{
			rgpUnk[cIndex] = m_rgpUnk[cIndex];
		}

		if (NULL != m_rgpUnk)
			delete[] m_rgpUnk;

		// Initialize new indexes to NULL
		for (UINT cNewElements = cIndex; cNewElements < nArraySize; cNewElements++)
		{
			rgpUnk[cNewElements] = NULL;
		}

		m_rgpUnk = rgpUnk;
		m_nArraySize = nArraySize;
	}

	// Verify that the sink supports the correct interface.
	// We don't have to know what it is because we have
	// m_iid to describe it. If this works, then we neatly 
	// have a pointer with an AddRef that we can stow away.

	IUnknown* pSink;
	if (FAILED(pUnkSink->QueryInterface(m_iid, (void**)&pSink)))
		return (CONNECT_E_CANNOTCONNECT);
	
	m_rgpUnk[cIndex]=pSink;
	*pdwCookie = cIndex + 1;
	m_cConnections++;
	return S_OK;
}


STDMETHODIMP 
CoConnectionPoint::Unadvise(DWORD dwCookie)
{
	if (dwCookie > 16 || m_rgpUnk[dwCookie - 1] == 0)
		return CONNECT_E_NOCONNECTION;
	(m_rgpUnk[dwCookie - 1])->Release();
	m_rgpUnk[dwCookie - 1] = 0;
	m_cConnections--;
	return S_OK;
}

STDMETHODIMP 
CoConnectionPoint::EnumConnections(IEnumConnections** ppEnumConnections)
{
	*ppEnumConnections = NULL;

	if (0 == m_cConnections)
		return E_FAIL;

	LPCONNECTDATA pCD = new CONNECTDATA[m_cConnections];

	if (NULL == pCD)
		return E_OUTOFMEMORY;

	UINT cIndex, cConnections;
	for (cIndex = 0, cConnections=0; cIndex < m_nArraySize; cIndex++)
	{
		if (NULL != m_rgpUnk[cIndex])
		{
			pCD[cConnections].pUnk = (LPUNKNOWN)m_rgpUnk[cIndex];
			pCD[cConnections].dwCookie = cIndex + 1;
			cConnections++;
		}
	}

	CoEnumConnections* pEnum = new CoEnumConnections(this, m_cConnections, pCD);

	delete [] pCD;

	if (NULL == pEnum)
		return E_OUTOFMEMORY;

	// This does an AddRef for us
	return pEnum->QueryInterface(IID_IEnumConnections, (void**) ppEnumConnections);
}
	

