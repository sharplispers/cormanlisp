//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
// 
//		File:	LispObjDisplay.cpp
//

#include "stdafx.h"
#include "CormanLisp.h"
#include "LispObjDisplay.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// LispObjDisplay dialog


LispObjDisplay::LispObjDisplay(CWnd* pParent, CStringArray* contents)
	: CDialog(LispObjDisplay::IDD, pParent)
{
	//{{AFX_DATA_INIT(LispObjDisplay)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
	if (!pParent)
		pParent = AfxGetApp()->GetMainWnd();
	ASSERT(m_pParent != NULL);
	m_pParent = pParent;
	m_nID = LispObjDisplay::IDD;

	CDialog::Create(m_nID, 0);
	CListBox* listbox = (CListBox*)GetDlgItem(IDC_LIST1);
	ASSERT(listbox != NULL);
	int count = contents->GetSize();
	if (count > 0)
		SetWindowText(contents->GetAt(0));
	for (int i = 1; i < count; i++)
	{
		listbox->AddString(contents->GetAt(i));
	}

	delete contents;
}


void LispObjDisplay::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(LispObjDisplay)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}

void 
LispObjDisplay::OnCancel()
{
	DestroyWindow();
}

void 
LispObjDisplay::PostNcDestroy()
{
	delete this;
}

BEGIN_MESSAGE_MAP(LispObjDisplay, CDialog)
	//{{AFX_MSG_MAP(LispObjDisplay)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// LispObjDisplay message handlers
