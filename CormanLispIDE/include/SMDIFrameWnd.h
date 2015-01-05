#if !defined(AFX_SMDIFRAMEWND_H__BA187822_FBE1_450E_9C75_58CAD43361FF__INCLUDED_)
#define AFX_SMDIFRAMEWND_H__BA187822_FBE1_450E_9C75_58CAD43361FF__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// SMDIFrameWnd.h : header file
//
//
// James Pullicino
// James@drinkinginthesun.com
//
/////////////////////////////////////////////////////////////////////////////
// CSMDIFrameWnd frame

class CSMDIFrameWnd : public CMDIFrameWnd
{
	DECLARE_DYNCREATE(CSMDIFrameWnd)
protected:
	CSMDIFrameWnd();           // protected constructor used by dynamic creation

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CSMDIFrameWnd)
	public:
	virtual BOOL DestroyWindow();
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual ~CSMDIFrameWnd();

	// Generated message map functions
	//{{AFX_MSG(CSMDIFrameWnd)
	afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

	bool	m_bShowOnce;
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_SMDIFRAMEWND_H__BA187822_FBE1_450E_9C75_58CAD43361FF__INCLUDED_)
