#if !defined(AFX_SMDICHILDWND_H__2FDD3D13_FE8F_4393_8225_9481CAB8CB62__INCLUDED_)
#define AFX_SMDICHILDWND_H__2FDD3D13_FE8F_4393_8225_9481CAB8CB62__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// SMDIChildWnd.h : header file
//
// James Pullicino
// James@drinkinginthesun.com
//
/////////////////////////////////////////////////////////////////////////////
// CSMDIChildWnd frame

class CSMDIChildWnd : public CMDIChildWnd
{
	DECLARE_DYNCREATE(CSMDIChildWnd)
protected:
	CSMDIChildWnd();           // protected constructor used by dynamic creation

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CSMDIChildWnd)
	public:
	virtual BOOL DestroyWindow();
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual ~CSMDIChildWnd();

	// Generated message map functions
	//{{AFX_MSG(CSMDIChildWnd)
	afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

	bool	m_bShowOnce;
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_SMDICHILDWND_H__2FDD3D13_FE8F_4393_8225_9481CAB8CB62__INCLUDED_)
