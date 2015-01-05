// SMDIFrameWnd.cpp : implementation file
//

#include "stdafx.h"
#include "SMDIFrameWnd.h"
#include "CormanLisp.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CSMDIFrameWnd

IMPLEMENT_DYNCREATE(CSMDIFrameWnd, CMDIFrameWnd)

CSMDIFrameWnd::CSMDIFrameWnd() : m_bShowOnce(false)
{
}

CSMDIFrameWnd::~CSMDIFrameWnd()
{
}


BEGIN_MESSAGE_MAP(CSMDIFrameWnd, CMDIFrameWnd)
	//{{AFX_MSG_MAP(CSMDIFrameWnd)
	ON_WM_SHOWWINDOW()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CSMDIFrameWnd message handlers

BOOL CSMDIFrameWnd::DestroyWindow() 
{
	if (theApp.preferences.rememberDocumentPositions)
	{
		// Save under "MainFrame"
		WINDOWPLACEMENT wp;
		GetWindowPlacement(&wp);
		
		// if it is minimized, don't save it that way
		if (wp.showCmd == SW_SHOWMINIMIZED)
			wp.showCmd = SW_SHOWNORMAL;

		AfxGetApp()->WriteProfileBinary(_TEXT("CormanLisp"), _TEXT("WP"), (LPBYTE)&wp, sizeof(wp));
	}
	return CMDIFrameWnd::DestroyWindow();
}

void CSMDIFrameWnd::OnShowWindow(BOOL bShow, UINT nStatus) 
{
	CMDIFrameWnd::OnShowWindow(bShow, nStatus);

	PreferencesDialog::GetPreferencesInfo(&theApp.preferences);
	if (theApp.preferences.rememberDocumentPositions)
	{	
		if(!m_bShowOnce && bShow && !IsWindowVisible())
		{
			m_bShowOnce = true;

			WINDOWPLACEMENT *lwp;
			UINT nl;

			if(AfxGetApp()->GetProfileBinary(_TEXT("CormanLisp"), _TEXT("WP"), (LPBYTE*)&lwp, &nl))
			{
				SetWindowPlacement(lwp);
				delete [] lwp;
			}
		}
	}
}
