//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
#ifndef BROWSER_H
#define BROWSER_H

#include <afxext.h>

#include "WebBrowser.h"
#include "SMDIChildWnd.h"

//
// Utility class for the WebBrowser Command Group
class CWBCmdGroup
{
public:
   // CommandTarget ids for menu driving
   enum 
   {
      HTMLID_FIND         = 1,
      HTMLID_VIEWSOURCE   = 2,
      HTMLID_OPTIONS      = 3,
   };
};

class CUrlEdit : public CEdit
{
public:
	afx_msg void OnActivate(UINT nState, CWnd* pWndOther, BOOL bMinimized);
	DECLARE_DYNCREATE(CUrlEdit)
	DECLARE_MESSAGE_MAP()
};

class CBrowserFrame : public CSMDIChildWnd
{
public:
   // Utility Methods
   void SetUrlCBText(const CString& strUrlText);
   void SetStatusBarText(const CString& strUrlText);

   void PrintDocument();
   void SaveAsFile();
   HWND m_hwndWebBrowser;
   void activateUrlView(bool);

protected: // create from serialization only
	CBrowserFrame();
	DECLARE_DYNCREATE(CBrowserFrame)

// Operations
public:
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	virtual BOOL PreTranslateMessage(MSG* pMsg);
protected:
	virtual BOOL OnCommand(WPARAM wParam, LPARAM lParam);

// Implementation
public:
	virtual ~CBrowserFrame();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:  // control bar embedded members
	CToolBar    m_wndToolBar;

protected:
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnUpdateFileSave(CCmdUI* pCmdUI);
	afx_msg void OnGoBack();
	afx_msg void OnFileNew();
	afx_msg void OnGoForward();
	afx_msg void OnGoHome();
	afx_msg void OnGoSearch();
	afx_msg void OnViewRefresh();
	afx_msg void OnViewStop();
	afx_msg void OnViewSource();
	afx_msg void OnEditCopy();
	afx_msg void OnEditCut();
	afx_msg void OnEditFind();
	afx_msg void OnEditPaste();
	afx_msg void OnEditSelectAll();
	afx_msg void OnFilePageSetup();
	afx_msg void OnViewOptions();
	afx_msg void OnUpdateViewStop(CCmdUI* pCmdUI);
	afx_msg void OnChar(UINT nChar, UINT nRepCnt, UINT nFlags);

	DECLARE_MESSAGE_MAP()
private:
	CStatusBar m_wndStatusBar;
	CUrlEdit  m_cbURL;              // The URL text box
	bool m_bUrlActive;
	HRESULT ExecCmdTarget(const GUID *pguidCmdGroup, DWORD nCmdID);
};

class CBrowserDoc : public CDocument
{
protected: // create from serialization only
	CBrowserDoc();
	DECLARE_DYNCREATE(CBrowserDoc)

public:
	virtual BOOL OnNewDocument();
	virtual void Serialize(CArchive& ar);

	virtual ~CBrowserDoc();
	virtual void OnCloseDocument();

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	DECLARE_MESSAGE_MAP()
public:
	CString m_urlToLoad;
};

class CBrowserView : public CView
{
public:
	DECLARE_DYNCREATE(CBrowserView)
	CBrowserView();
	~CBrowserView();

	virtual void OnDraw(CDC* pDC);  // overridden to draw this view
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	CBrowserDoc* GetDocument();
	virtual void OnInitialUpdate();
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	virtual BOOL OnPreparePrinting(CPrintInfo* pInfo);
	virtual void OnBeginPrinting(CDC* pDC, CPrintInfo* pInfo);
	virtual void OnEndPrinting(CDC* pDC, CPrintInfo* pInfo);

	CWebBrowser1 m_WebBrowser;
	int m_sizeX;
	int m_sizeY;

	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnSize( UINT nType, int cx, int cy );
	afx_msg void OnFilePrint();
	afx_msg void OnFileSaveAs();
	afx_msg void OnTitleChange(LPCTSTR lpszText);
	afx_msg void OnDownloadComplete();
	afx_msg void OnStatusTextChange(LPCTSTR lpszText);
	afx_msg void OnChar(UINT nChar, UINT nRepCnt, UINT nFlags);
 
	DECLARE_EVENTSINK_MAP()

	DECLARE_MESSAGE_MAP()

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
private:
	HRESULT ExecCmdTarget(const GUID *pguidCmdGroup, DWORD nCmdID);
   CBrowserFrame* m_pFrame;
};

#endif