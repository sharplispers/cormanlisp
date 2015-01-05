//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		CoConnectionPoint.h
//		Contents:	COM Class definitions for Corman Lisp COM server.
//		History:	8/11/97  RGC  Created.
//

#ifndef COCONNECTIONPOINT_H
#define COCONNECTIONPOINT_H

#include <ocidl.h>

class CoConnectionPoint : public IConnectionPoint
{
	IUnknown** m_rgpUnk;
	
	IUnknown* m_pUnkContainer;
	UINT m_cConnections;
	UINT m_nArraySize;
	IID m_iid;
	LONG m_cRef;
public:
	CoConnectionPoint(REFIID riid, IUnknown* pContainer);
	~CoConnectionPoint();

// IConnectionPoint
	STDMETHODIMP GetConnectionInterface(IID* pID);
	STDMETHODIMP GetConnectionPointContainer(IConnectionPointContainer** ppConnectionPointContainer);
	STDMETHODIMP Advise(IUnknown* pUnk, DWORD* pdwCookie);
	STDMETHODIMP Unadvise(DWORD dwCookie);
	STDMETHODIMP EnumConnections(IEnumConnections** ppEnumConnections);

// IUnknown methods
    STDMETHODIMP QueryInterface(REFIID riid, void** ppv);
    STDMETHODIMP_(ULONG) AddRef(void);
    STDMETHODIMP_(ULONG) Release(void);

protected:
// Helper functions
	BOOL CoConnectionPoint::ExpandArray();
	BOOL CoConnectionPoint::ContractArray();
	UINT CoConnectionPoint::GetFirstEmptyPosition();
};
#endif // COCONNECTIONPOINT_H