//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//	File:		PageSetupDlg.cpp
//

#include "stdafx.h"
#include "../resource.h"
#include "PageSetupDlg.h"
#include "helpids.h"
#include "CormanLisp.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CPageSetupDlg dialog

const DWORD CPageSetupDlg::m_nHelpIDs[] = 
{
	IDC_EDIT_TM, IDH_CORMANLISP_TOPMARGIN,
	IDC_EDIT_BM, IDH_CORMANLISP_BOTTOMMARGIN,
	IDC_EDIT_LM, IDH_CORMANLISP_LEFTMARGIN,
	IDC_EDIT_RM, IDH_CORMANLISP_RIGHTMARGIN,
	IDC_BOX, IDH_COMM_GROUPBOX,
	0, 0
};

CPageSetupDlg::CPageSetupDlg(CWnd* pParent /*=NULL*/)
	: CCSDialog(CPageSetupDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CPageSetupDlg)
	m_nTopMargin = 0;
	m_nRightMargin = 0;
	m_nLeftMargin = 0;
	m_nBottomMargin = 0;
	//}}AFX_DATA_INIT
}

void CPageSetupDlg::DoDataExchange(CDataExchange* pDX)
{
	CCSDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CPageSetupDlg)
	DDX_Twips(pDX, IDC_EDIT_TM, m_nTopMargin);
	DDV_MinMaxTwips(pDX, m_nTopMargin, -31680, 31680);
	DDX_Twips(pDX, IDC_EDIT_RM, m_nRightMargin);
	DDV_MinMaxTwips(pDX, m_nRightMargin, -31680, 31680);
	DDX_Twips(pDX, IDC_EDIT_LM, m_nLeftMargin);
	DDV_MinMaxTwips(pDX, m_nLeftMargin, -31680, 31680);
	DDX_Twips(pDX, IDC_EDIT_BM, m_nBottomMargin);
	DDV_MinMaxTwips(pDX, m_nBottomMargin, -31680, 31680);
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CPageSetupDlg, CCSDialog)
	//{{AFX_MSG_MAP(CPageSetupDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

// this routine prints a floatingpoint number with 2 digits after the decimal
void PASCAL DDX_Twips(CDataExchange* pDX, int nIDC, int& value)
{
	HWND hWndCtrl = pDX->PrepareEditCtrl(nIDC);
	TCHAR szT[64];

	if (pDX->m_bSaveAndValidate)
	{
		::GetWindowText(hWndCtrl, szT, sizeof(szT));
		if (szT[0] != NULL) // not empty
		{
			if (!theApp.ParseMeasurement(szT, value))
			{
				AfxMessageBox(IDS_INVALID_MEASUREMENT,MB_OK|MB_ICONINFORMATION);
				pDX->Fail();            // throws exception
			}
			theApp.PrintTwips(szT, value, 2);
			theApp.ParseMeasurement(szT, value);
		}
		else // empty
			value = INT_MAX;
	}
	else
	{
		// convert from twips to default units
		if (value != INT_MAX)
		{
			theApp.PrintTwips(szT, value, 2);
			SetWindowText(hWndCtrl, szT);
		}
	}
}

void PASCAL DDV_MinMaxTwips(CDataExchange* pDX, int value, int minVal, int maxVal)
{
	ASSERT(minVal <= maxVal);
	if (value < minVal || value > maxVal)
	{
		// "The measurement must be between %1 and %2."
		if (!pDX->m_bSaveAndValidate)
		{
			TRACE0("Warning: initial dialog data is out of range.\n");
			return;     // don't stop now
		}
		TCHAR szMin[32];
		TCHAR szMax[32];
		theApp.PrintTwips(szMin, minVal, 2);
		theApp.PrintTwips(szMax, maxVal, 2);
		CString prompt;
		AfxFormatString2(prompt, IDS_MEASUREMENT_RANGE, szMin, szMax);
		AfxMessageBox(prompt, MB_ICONEXCLAMATION, AFX_IDS_APP_TITLE);
		prompt.Empty(); // exception prep
		pDX->Fail();
	}
}
