//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//	File:		DocOptions.h
//

#ifndef DOCOPTIONS_H
#define DOCOPTIONS_H

#include <afxadv.h>

class CUnit
{
public:
	int m_nTPU;
	int m_nSmallDiv;	// small divisions - small line displayed
	int m_nMediumDiv;	// medium divisions - large line displayed
	int m_nLargeDiv;	// large divisions - numbers displayed
	int m_nMinMove;		// minimum tracking movements
	UINT m_nAbbrevID;
	BOOL m_bSpaceAbbrev; // put space before abbreviation
	CString m_strAbbrev;// cm, pt, pi, ", in, inch, inches

	CUnit() {}
	CUnit(int nTPU, int nSmallDiv, int nMediumDiv, int nLargeDiv, 
		int nMinMove, UINT nAbbrevID, BOOL bSpaceAbbrev);
	const CUnit& operator=(const CUnit& unit);
};

class CDocOptions
{
public:
	CDocOptions(int nDefWrap) {m_nDefWrap = nDefWrap;}
	CDockState m_ds1;
	CDockState m_ds2;
	int m_nWordWrap;
	int m_nDefWrap;
	void SaveOptions(LPCTSTR lpsz);
	void LoadOptions(LPCTSTR lpsz);
	void SaveDockState(CDockState& ds, LPCTSTR lpszProfileName, 
		LPCTSTR lpszLayout);
	void LoadDockState(CDockState& ds, LPCTSTR lpszProfileName, 
		LPCTSTR lpszLayout);
	CDockState& GetDockState(BOOL bPrimary) {return (bPrimary) ? m_ds1 : m_ds2;}
};

#endif // DOCOPTIONS_H

