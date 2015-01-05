//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		CormanLisp.h
//		Contents:	Corman Lisp application main header
//

#ifndef CORMANLISP_H
#define	CORMANLISP_H
 
#include <afxext.h>
#include <afxmt.h>
#include <afxrich.h>
#include <afxtempl.h>

#include "../resource.h"
#include "CharBuf.h"
#include "CoCormanLispClient.h"
#include "clsids.h"
#include "ICormanLisp.h"
#include "DocOptions.h"
#include "WebBrowser.h"
#include "PreferencesDialog.h"

#include "SMDIFrameWnd.h"
#include "SMDIChildWnd.h"

class CBrowserDoc;

class LispDialogBar : public CDialogBar
{
public:
	DECLARE_MESSAGE_MAP()
	afx_msg void OnPaint();	
	afx_msg void OnDrawItem( int nIDCtl, LPDRAWITEMSTRUCT lpDrawItemStruct);
};

class RegistrationDialogBar : public CDialogBar
{
public:
	DECLARE_MESSAGE_MAP()
	afx_msg void OnPaint();	
	afx_msg void OnOK();
	afx_msg void OnUpdateOK(CCmdUI* pCmdUI);
	afx_msg void OnSize(UINT nType, int cx, int cy);
};

class LispVarsDialogBar : public CDialogBar
{
public:
	DECLARE_MESSAGE_MAP()
	afx_msg void OnPaint();	
	afx_msg void OnSize(UINT nType, int cx, int cy);
public:    
    BOOL Create( CWnd* pParentWnd, UINT nIDTemplate, UINT nStyle,
                     UINT nID, BOOL = TRUE);

    virtual CSize CalcDynamicLayout( int nLength, DWORD dwMode );

public:
        CSize m_sizeDocked;
        CSize m_sizeFloating;
        BOOL m_bChangeDockedSize;   // Indicates whether to keep
                                    // a default size for docking

};

#define WM_DISPLAY_SELECTION	(WM_USER + 0)
#define MAX_URL	512

class CCormanLispApp : public CWinApp
{
public:
	CCormanLispApp();
	~CCormanLispApp();

	// command handlers
	void OnExecuteFile();
	afx_msg void OnFileOpen();
	afx_msg void OnTimer(UINT);

	// overrides
	BOOL InitInstance();
	BOOL ExitInstance();
	BOOL OnIdle(LONG lCount);
	void CloseAllDocuments(BOOL bEndSession);
	BOOL SaveAllModified();
	void NotifyPrinterChanged(BOOL bUpdatePrinterSelection = FALSE);
	void SaveOpenDocumentPaths();

	void DisplayLispObj(CStringArray* contents);
	void SetMessage(const CString&);
	void SetDefaultMessage();
	CString GetMessage();
	CString messageText;
	CString defaultMessageText;
	void SetLineNumber(long line);
	void SetColumnNumber(long column);
	bool SetDocumentToOpen(const char*);
	bool SetURLToOpen(const char*);
	bool SetReplaceSelection(const char* text, int numChars);
	bool AppIsClosing();
	void AppIsClosing(bool);
	CDocument* FindDocumentByTitle(const char* title);
	int GetUnits() {return m_nUnits;}
	int GetTPU() { return GetTPU(m_nUnits);}
	int GetTPU(int n) { return m_units[n].m_nTPU;}
	LPCTSTR GetAbbrev() { return m_units[m_nUnits].m_strAbbrev;}
	LPCTSTR GetAbbrev(int n) { return m_units[n].m_strAbbrev;}
	const CUnit& GetUnit() {return m_units[m_nUnits];}
	void SetUnits(int n)
	{ ASSERT(n>=0 && n <m_nPrimaryNumUnits); m_nUnits = n; }
	BOOL ParseMeasurement(TCHAR* buf, int& lVal);
	void PrintTwips(TCHAR* buf, int nValue, int nDecimal);
	bool waitingForDocumentToOpen();
	void NavigateURL(const char* url);

	CFont* getDefaultFont(HDC, long size);
	CFont* getDefaultUnderlineFont(HDC, long size);
	CFont* getCourierFont(HDC, long size);
	void checkRegistration();
	CDocument* openWorksheet();
	BOOL OnDDECommand(LPTSTR lpszCommand);

	//{{AFX_MSG(CMultiPadApp)
	afx_msg void OnAppAbout();
	//}}AFX_MSG
	afx_msg void OnBrowse();
	afx_msg void OnBrowseCCLDoc();
	afx_msg void OnLicenseAgreement();
	afx_msg void OnCredits();
	afx_msg void OnCormanLispCom();
	afx_msg void OnFileNew();
	afx_msg void OnEditPreferences();
	afx_msg void OnCloseAll();

	DECLARE_MESSAGE_MAP()

private:
	IConnectionPoint* m_pConnectionPoint;
	IConnectionPoint* m_pShutdownConnectionPoint;
	CoCormanLispClient* m_CormanLispClient;
	CoCormanLispShutdownClient* m_CormanLispShutdownClient;
	char m_fileToOpen[_MAX_PATH + 1];		// pending document to open
	char m_urlToOpen[MAX_URL + 1];		// pending document to open
	bool m_appIsClosing;
	int m_nUnits;
	bool m_inRegistrationDialog;
	CString m_replaceSelection;
	static const int m_nPrimaryNumUnits;
	static const int m_nNumUnits;
	static CUnit m_units[7];
public:
	CDocument* m_worksheet;
	CDocument* m_lastDocOpened;
	CWebBrowser1* m_browser;
	CBrowserDoc* m_browserDoc;
	CList<HWND, HWND> m_listPrinterNotify;
	static int m_nPrinterChangedMsg;
	CRect m_rectPageMargin;
	IUnknown* m_explorer;
	HANDLE m_event;
	CString m_userName;
	CString m_company;
	bool m_expired;		// true if this program's registration has expired
	UINT m_timer;
	HBRUSH m_blackBrush;
	PreferencesInfo preferences;
	CString m_defaultDirectory;	// default directory for file open command
	int m_defaultFilterIndex;	// default filter index for file open command
	CString m_defaultExecDirectory;	// default directory for execute file command
	int m_defaultExecFilterIndex;	// default filter index for execute file command
	long m_lastExpiredScreenTime;	// last time an expired message was displayed
	BOOL m_isActive;
	BOOL m_closeSplash;

	static CFont** defaultFont;
	static CFont** defaultUnderlineFont;
	static CFont** courierFont;

	static CDialog* splashScreen;
	static DWORD startupTime;
	static DWORD splashDisplayTimeMillis;	// amount of time (milliseconds)
											// to display splash screen
};

extern CCormanLispApp theApp;

class CMainFrame : public CSMDIFrameWnd
{
	DECLARE_DYNCREATE(CMainFrame)
	CStatusBar  m_StatusBar;
	CToolBar	m_ToolBar;
	LispDialogBar m_lispStatusDialogBar;
	RegistrationDialogBar m_registrationDialogBar;
	LispVarsDialogBar m_lispVarsDialogBar;
	int			m_currentLineNo;
	int			m_currentColumnNo;

	//{{AFX_MSG(CMainFrame)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg LRESULT OnDisplayLispObj(WPARAM, LPARAM);
	afx_msg void OnActivateApp(BOOL bActive, DWORD dwThreadID);
	afx_msg void OnInitMenu(CMenu* pMenu);
	afx_msg void OnInitMenuPopup(CMenu* pPopupMenu, UINT nIndex, BOOL bSysMenu);
	afx_msg LRESULT OnUninitMenuPopup(WPARAM, LPARAM);
    afx_msg void OnMenuSelect(UINT nItemID, UINT nFlags, HMENU hSysMenu);

	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
public:
	void OnLispMenuItem(UINT nID);
    afx_msg LRESULT OnLispMenuItemByPosition(WPARAM, LPARAM);

	void SetLineNumber(long line);
	void SetColumnNumber(long column);
	void DockControlBarLeftOf(CControlBar* Bar, CControlBar* LeftOf);
};

class CLispDocumentFrame : public CSMDIChildWnd
{
public:
	DECLARE_DYNCREATE(CLispDocumentFrame)
	DECLARE_MESSAGE_MAP()
public:
	afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);

	afx_msg void OnClose();

	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
};

class CLispDoc : public CRichEditDoc
{
public:
	CLispDoc();
	~CLispDoc();

	afx_msg void OnUpdateFileClose(CCmdUI*);
	void OnUpdateIfEmbedded(CCmdUI* pCmdUI); 
	void OnCloseDocument();
	BOOL CanCloseFrame(CFrameWnd*);
	afx_msg void OnFileSaveAs();
	afx_msg void OnFileSave(); 
	void OnFrameWindowActivate(BOOL bActivate);
	
	BOOL OnSaveDocument(LPCTSTR lpszPathName);
	BOOL OnOpenDocument(LPCTSTR lpszPathName);

	BOOL DoFileSave();
	BOOL DoSave(LPCTSTR lpszPathName, BOOL bReplace = TRUE);
	void CheckFileUpdateStatus();

	DECLARE_DYNCREATE(CLispDoc)
//	void Serialize(CArchive& ar);
	//{{AFX_MSG(CLispDoc)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
public:
	CRichEditCntrItem* CreateClientItem(REOBJECT* preo) const;
private:
	FILETIME lastWriteTime;
};

class CLispView : public CRichEditView
{
	DECLARE_DYNCREATE(CLispView)
	CLispView();
	~CLispView();
	void OnInitialUpdate();
	BOOL OnPreparePrinting(CPrintInfo* pInfo);

	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnEditCut();
	afx_msg void OnEditCopy();
	afx_msg void OnEditPaste();
	afx_msg void OnEditUndo();
	afx_msg void OnEditRedo();

	//afx_msg void OnViewSelection();
	afx_msg void OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags);
	afx_msg void OnChar(UINT nChar, UINT nRepCnt, UINT nFlags);
	afx_msg void OnFilePrint();
	afx_msg LONG OnPrinterChangedMsg(UINT, LONG);
	afx_msg void OnPageSetup();
	afx_msg void OnPaint();
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnLButtonDblClk(UINT nFlags, CPoint point);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
//	afx_msg BOOL OnNotify(WPARAM wParam, LPARAM lParam, LRESULT* pResult);

	afx_msg void OnExecuteSelection();
	afx_msg void OnGotoLine();
	afx_msg void OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar);
	afx_msg void OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar);
	afx_msg void OnContextMenu(CWnd* wnd, CPoint point);
	afx_msg void OnRButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnRButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg BOOL OnScroll(UINT nScrollCode, UINT nPos, BOOL bDoScroll = TRUE);
	afx_msg void OnActivate(UINT nState, CWnd* pWndOther, BOOL bMinimized);

	void DrawMargins(CDC* pDC);
 	virtual void OnPrint(CDC* pDC, CPrintInfo* pInfo);

	virtual void Serialize(CArchive& ar);
	void StreamText(CArchive& ar, BOOL bSelection);
	virtual void UpdateScrollPosition(UINT nPos);

	void Colorize();

	void OnActivateView( BOOL bActivate, CView* pActivateView, CView* pDeactiveView );
	void outputText(const CString& s);
	void replaceSelection(const CString& s);
	void SetTabStops(long numChars);
	void SetCharSize(long pointSize);
	void SetTextColor(COLORREF color);
	void SetLispHighlight(long start, long end);
	void LispHighlightOff();
	void LispHighlightOn();
	void adjustLispHighlight();
	void displayMouseCue();
	void undisplayMouseCue();
	void mouseCueOn(const CPoint&);
	void mouseCueOff();
	long CharFromPos(CPoint pt);
	CPoint PosFromChar(long ch);
	bool usingKeyboard();
	int mouseCueDisabled();
	int calculateColumnNumber(int line, int offset);
	void activateFont(HDC);

	void highlightChar(CRichEditCtrl& ed, long position);

protected:
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	
	//{{AFX_MSG(CLispDoc)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

public:
	BOOL m_bInPrint;
	HFONT m_font;
	long m_lispHighlightStart;
	long m_lispHighlightEnd;
	COLORREF m_highlightStartSaveColor;
	COLORREF m_highlightEndSaveColor;
	bool m_highlightOn;
	CPoint m_mouseCuePosition;
	bool m_mouseCueDisplayed;
	CRect m_mouseCueRect;
	bool m_usingKeyboard;	// true if last input from keyoard, 
							// false if last input from mouse
	int m_mouseCueDisabled;// >0 if dragging mouse or something else where
							// we don't want tooltips coming on
	int m_colorizeDisabled; // >0 to disable auto colorize
	long m_firstModified;  // first char in file to be modified for colorization purposes
	int m_lineHeight;
};

// The TerminalInputBuf and TerminalOutputBuf arrays (and the indexes)
// are accessed by both threads, so care must be taken when modifying them.
//
extern CharBuf TerminalOutputBuf;

extern ICormanLisp* pCormanLisp;

CString expandLineFeeds(const CString&);
CString expandCRs(const CString&);
char* getVersionCaption();

#define ID_LISP_MENU_ITEM_BY_POSITION   1999
#define ID_LISP_MENU_ITEM_START			2000
#define ID_LISP_MENU_ITEM_END			2999
#define ID_LISP_DYNAMIC_MENU_ITEM_START	3000
#define ID_LISP_DYNAMIC_MENU_ITEM_END	5999

#endif // CORMANLISP_H
