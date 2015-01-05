// SMDIChildWnd.cpp : implementation file
//

#include "stdafx.h"
#include "SMDIChildWnd.h"

#include "CormanLisp.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CSMDIChildWnd

IMPLEMENT_DYNCREATE(CSMDIChildWnd, CMDIChildWnd)

CSMDIChildWnd::CSMDIChildWnd() : m_bShowOnce(false)
{
}

CSMDIChildWnd::~CSMDIChildWnd()
{
}


BEGIN_MESSAGE_MAP(CSMDIChildWnd, CMDIChildWnd)
	//{{AFX_MSG_MAP(CSMDIChildWnd)
	ON_WM_SHOWWINDOW()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CSMDIChildWnd message handlers

void CSMDIChildWnd::OnShowWindow(BOOL bShow, UINT nStatus) 
{
	CMDIChildWnd::OnShowWindow(bShow, nStatus);
	
	if (theApp.preferences.rememberDocumentPositions)
	{
		if(!m_bShowOnce && bShow && !IsWindowVisible())
		{
			m_bShowOnce = true;

			WINDOWPLACEMENT *lwp;
			UINT nl;

			// Use window name
			CString WindowName; 
			GetWindowText(WindowName);
			CDocument* doc = GetActiveDocument();
			if (doc)
				WindowName = doc->GetPathName();
			if(AfxGetApp()->GetProfileBinary(WindowName, _TEXT("WP"), (LPBYTE*)&lwp, &nl))
			{
				SetWindowPlacement(lwp);
				delete [] lwp;
			}
		}
	}
}

BOOL CSMDIChildWnd::DestroyWindow() 
{
	if (theApp.preferences.rememberDocumentPositions)
	{
		// Save under window name
		CString WindowName; GetWindowText(WindowName);
		CDocument* doc = GetActiveDocument();
		if (doc)
			WindowName = doc->GetPathName();

		WINDOWPLACEMENT wp;
		GetWindowPlacement(&wp);
		AfxGetApp()->WriteProfileBinary(WindowName, _TEXT("WP"), (LPBYTE)&wp, sizeof(wp));
	}
	return CMDIChildWnd::DestroyWindow();
}
