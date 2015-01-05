//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		CoEnumConnections.h
//		Contents:	COM Class definitions for Corman Lisp COM server.
//		History:	8/11/97  RGC  Created.
//

#ifndef COENUMCONNECTIONS_H
#define COENUMCONNECTIONS_H

class CoEnumConnections : public IEnumConnections
{
	LONG m_cRef;
	IUnknown* m_pUnkRef;
	ULONG m_iPosition;
	ULONG m_cConnections;
	LPCONNECTDATA m_rgConnectData;

public:
	CoEnumConnections(IUnknown* pUnk, ULONG cConnections, LPCONNECTDATA prgConnectData);
	virtual ~CoEnumConnections();

// IUnknown methods
    STDMETHODIMP QueryInterface(REFIID riid, void** ppv);
    STDMETHODIMP_(ULONG) AddRef(void);
    STDMETHODIMP_(ULONG) Release(void);

// IEnumConnections methods
	STDMETHODIMP Next(ULONG cConnections, CONNECTDATA* rgpConnectData, ULONG* pcFetched);
	STDMETHODIMP Skip(ULONG cConnections);
	STDMETHODIMP Reset();
	STDMETHODIMP Clone(IEnumConnections** ppEnumConnections);
};

#endif // COENUMCONNECTIONS_H