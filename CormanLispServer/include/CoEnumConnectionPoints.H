//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		CoEnumConnectionPoints.h
//		Contents:	COM Class definitions for Corman Lisp COM server.
//		History:	8/11/97  RGC  Created.
//

#ifndef COENUMCONNECTIONPOINTS_H
#define COENUMCONNECTIONPOINTS_H

#include <ocidl.h>

class CoEnumConnectionPoints : public IEnumConnectionPoints
{
public:
	CoEnumConnectionPoints(IUnknown* pUnk, ULONG cConnections, IConnectionPoint** rgpConnectionPoints);
	virtual ~CoEnumConnectionPoints();

// IUnknown methods
    STDMETHODIMP QueryInterface(REFIID riid, void** ppv);
    STDMETHODIMP_(ULONG) AddRef(void);
    STDMETHODIMP_(ULONG) Release(void);

// IEnumConnectionPoints methods
	STDMETHODIMP Next(ULONG cConnections, IConnectionPoint** rgpConnectionPoints, ULONG* pcFetched);
	STDMETHODIMP Skip(ULONG cConnections);
	STDMETHODIMP Reset();
	STDMETHODIMP Clone(IEnumConnectionPoints** ppEnumConnectionPoints);

private:
	LONG m_cRef;
	IUnknown* m_pUnkRef;
	ULONG m_iPosition;
	ULONG m_cPoints;
	IConnectionPoint** m_rgpConnectionPoint;
};

#endif // COEnumConnectionPoints_H