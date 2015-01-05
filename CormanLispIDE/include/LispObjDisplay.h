//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File: LispObjDisplay.h
//

/////////////////////////////////////////////////////////////////////////////
// LispObjDisplay dialog

class LispObjDisplay : public CDialog
{
// Construction
public:
	LispObjDisplay(CWnd* pParent, CStringArray* contents);

// Dialog Data
	//{{AFX_DATA(LispObjDisplay)
	enum { IDD = IDD_DIALOG1 };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(LispObjDisplay)

	void OnCancel();
	void PostNcDestroy();

	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	CWnd* m_pParent;
	int m_nID;

	// Generated message map functions
	//{{AFX_MSG(LispObjDisplay)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};
