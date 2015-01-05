//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
/////////////////////////////////////////////////////////////////////////////
// CCSDialog dialog
#ifndef CSDIALOG_H
#define CSDIALOG_H

#include <afxdlgs.h>

class CCSDialog : public CDialog
{
public:
	CCSDialog();
	CCSDialog(LPCTSTR lpszTemplateName, CWnd* pParentWnd = NULL);
	CCSDialog(UINT nIDTemplate, CWnd* pParentWnd = NULL);

protected:
	virtual const DWORD* GetHelpIDs() = 0;

	// Generated message map functions
	//{{AFX_MSG(CCSDialog)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	afx_msg LONG OnHelp(UINT wParam, LONG lParam);
	afx_msg LONG OnHelpContextMenu(UINT wParam, LONG lParam);
	DECLARE_MESSAGE_MAP()
};

class CCSPropertyPage : public CPropertyPage
{
public:
	CCSPropertyPage(UINT nIDTemplate, UINT nIDCaption = 0);
	CCSPropertyPage(LPCTSTR lpszTemplateName, UINT nIDCaption = 0);

protected:
	virtual const DWORD* GetHelpIDs() = 0;

	// Generated message map functions
	//{{AFX_MSG(CCSPropertyPage)
	//}}AFX_MSG
	afx_msg LONG OnHelp(UINT wParam, LONG lParam);
	afx_msg LONG OnHelpContextMenu(UINT wParam, LONG lParam);
	DECLARE_MESSAGE_MAP()
};

class CCSPropertySheet : public CPropertySheet
{
public:
	CCSPropertySheet(UINT nIDCaption, CWnd *pParentWnd = NULL, 
		UINT iSelectPage = 0);
	CCSPropertySheet(LPCTSTR pszCaption, CWnd *pParentWnd = NULL, 
		UINT iSelectPage = 0);
protected:
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	// Generated message map functions
	//{{AFX_MSG(CCSPropertySheet)
	//}}AFX_MSG
	afx_msg LONG OnHelp(UINT wParam, LONG lParam);
	afx_msg LONG OnHelpContextMenu(UINT wParam, LONG lParam);
	DECLARE_MESSAGE_MAP()
};
#endif // CSDIALOG_H