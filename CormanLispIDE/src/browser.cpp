//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
#include "stdafx.h"

#include <docobj.h>

#include "browser.h"
#include "WebBrowser.h"
#include "afxdisp.h"
#include "../resource.h"
#include "clsids.h"
#include "CormanLisp.h"

/////////////////////////////////////////////////////////////////////////////
// CBrowserFrame

IMPLEMENT_DYNCREATE(CBrowserFrame, CSMDIChildWnd)

BEGIN_MESSAGE_MAP(CBrowserFrame, CSMDIChildWnd)
	ON_WM_CREATE()
	ON_UPDATE_COMMAND_UI(ID_FILE_SAVE, OnUpdateFileSave)
	ON_COMMAND(ID_GO_BACK, OnGoBack)
	ON_COMMAND(ID_GO_FORWARD, OnGoForward)
	ON_COMMAND(ID_GO_HOME, OnGoHome)
	ON_COMMAND(ID_GO_SEARCH, OnGoSearch)
	ON_COMMAND(ID_VIEW_REFRESH, OnViewRefresh)
	ON_COMMAND(ID_VIEW_STOP, OnViewStop)
	ON_COMMAND(ID_VIEW_SOURCE, OnViewSource)
	ON_COMMAND(ID_EDIT_COPY, OnEditCopy)
	ON_COMMAND(ID_EDIT_CUT, OnEditCut)
	ON_COMMAND(ID_EDIT_FIND, OnEditFind)
	ON_COMMAND(ID_EDIT_PASTE, OnEditPaste)
	ON_COMMAND(ID_EDIT_SELECT_ALL, OnEditSelectAll)
	ON_COMMAND(ID_FILE_PAGE_SETUP, OnFilePageSetup)
	ON_COMMAND(ID_VIEW_OPTIONS, OnViewOptions)
	ON_UPDATE_COMMAND_UI(ID_VIEW_STOP, OnUpdateViewStop)
	ON_WM_CHAR()
END_MESSAGE_MAP()

static UINT indicators[] =
{
	ID_SEPARATOR,           // status line indicator
	ID_INDICATOR_CAPS,
	ID_INDICATOR_NUM,
	ID_INDICATOR_SCRL,
};

/////////////////////////////////////////////////////////////////////////////
// CBrowserFrame construction/destruction

CBrowserFrame::CBrowserFrame() : m_bUrlActive(false)
{
}

CBrowserFrame::~CBrowserFrame()
{
}

//
// Utility Methods
//
void CBrowserFrame::SetUrlCBText(const CString& strUrlText)
{
   m_cbURL.SetWindowText(strUrlText);
}

void CBrowserFrame::SetStatusBarText(const CString& strUrlText)
{
   m_wndStatusBar.SetPaneText(0, strUrlText);
}

//
// Windows message handlers
//
int CBrowserFrame::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	if (CSMDIChildWnd::OnCreate(lpCreateStruct) == -1)
		return -1;
	
	if (!m_wndToolBar.Create(this) ||
		!m_wndToolBar.LoadToolBar(IDR_BROWSERFRAME))
	{
		TRACE0("Failed to create toolbar\n");
		return -1;      // fail to create
	}

   // Create a combo box in the toolbar
	m_wndToolBar.SetButtonInfo(0, IDC_URL_NAME, TBBS_SEPARATOR, 450);

	CRect rect;
	m_wndToolBar.GetItemRect(m_wndToolBar.CommandToIndex(IDC_URL_NAME), &rect);
	rect.top += 2;
	rect.bottom = rect.top + 25;

	if (!m_cbURL.CreateEx(WS_EX_CLIENTEDGE, _T("EDIT"), 0, 
			WS_VISIBLE|WS_CHILD|ES_AUTOHSCROLL|WS_BORDER, 
			rect, 
			&m_wndToolBar,
			IDC_URL_NAME))
	{
      TRACE0("Failed to create text edit box");
		return FALSE;
	}

	CDC* cdc = m_cbURL.GetDC();
	HDC hDC = cdc->m_hDC;

	// set the default font for the combo box
	CFont* font = theApp.getCourierFont(hDC, 10);
/*
	font = new CFont;
	int PointSize = 8;
	int nHeight = -8;
 	int nWidth = 0;

	font->CreateFont(nHeight,	// height
			nWidth,				// width 
			0,					// rotation (escapement)
			0,					// orientation					
			FW_NORMAL,			// weight
			0,					// italic = NO
			0,					// underline = NO
			0,					// strikeout = NO
			ANSI_CHARSET,		// charset = ANSI
			OUT_DEFAULT_PRECIS,	// precision
			CLIP_DEFAULT_PRECIS,// clip precision
			DEFAULT_QUALITY,	// quality
			FIXED_PITCH,		// pitch
			"Courier");		// family
*/
	m_cbURL.SetFont(font, TRUE);
	SelectObject(hDC, *font);

   // Create the status bar
   if (!m_wndStatusBar.Create(this) ||
		!m_wndStatusBar.SetIndicators(indicators,
		  sizeof(indicators)/sizeof(UINT)))
	{
		TRACE0("Failed to create status bar\n");
		return -1;      // fail to create
	}

	// TODO: Remove this if you don't want tool tips or a resizeable toolbar
	m_wndToolBar.SetBarStyle(m_wndToolBar.GetBarStyle() |
		CBRS_TOOLTIPS | CBRS_FLYBY | CBRS_SIZE_DYNAMIC);

	// TODO: Delete these three lines if you don't want the toolbar to
	//  be dockable
	//m_wndToolBar.EnableDocking(CBRS_ALIGN_ANY);
	//EnableDocking(CBRS_ALIGN_ANY);
	//DockControlBar(&m_wndToolBar);

	return 0;
}

BOOL CBrowserFrame::PreCreateWindow(CREATESTRUCT& cs)
{
	return CSMDIChildWnd::PreCreateWindow(cs);
}

/////////////////////////////////////////////////////////////////////////////
// CMainFrame diagnostics

#ifdef _DEBUG
void CBrowserFrame::AssertValid() const
{
	CSMDIChildWnd::AssertValid();
}

void CBrowserFrame::Dump(CDumpContext& dc) const
{
	CSMDIChildWnd::Dump(dc);
}

#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CMainFrame message handlers


void CBrowserFrame::OnUpdateFileSave(CCmdUI* pCmdUI) 
{
   // Disable the Save menu item
   pCmdUI->Enable(FALSE);
}

void CBrowserFrame::OnGoBack() 
{
	// Go back one page
	try
	{
   	// Navigate to the previous page in the history list
	   ((CBrowserView*)GetActiveView())->m_WebBrowser.GoBack();
	}
	catch(CException* excp)
	{
		excp->Delete();
	}
}

void CBrowserFrame::OnGoForward() 
{
	// Go back one page
	try
	{
   	// Navigate to the next page in the history list
	   ((CBrowserView*)GetActiveView())->m_WebBrowser.GoForward();
	}
	catch(CException* excp)
	{
		excp->Delete();
	}
}

void CBrowserFrame::OnGoHome() 
{
   // Navigate to the user's home page
	((CBrowserView*)GetActiveView())->m_WebBrowser.GoHome();
}

void CBrowserFrame::OnGoSearch() 
{
   // Navigate to the user's search page
	((CBrowserView*)GetActiveView())->m_WebBrowser.GoSearch();
}

void CBrowserFrame::OnUpdateViewStop(CCmdUI* pCmdUI) 
{
   // Only Enable the stop button and menu item
   // if the web browser object is busy.
	pCmdUI->Enable(((CBrowserView*)GetActiveView())->m_WebBrowser.GetBusy());
}

//////////////////////////////////////////////////////////////////////
// General IOleCommandTarget menu items
//
// Note: Saving and printing of the WebBrowser document must be
//       handled in the CView derived class in order to override
//       the default behavior.  The SaveAsFile() and PrintDocument()
//       methods of this class may be called to perform the 
//       actual saving or printing.
//////////////////////////////////////////////////////////////////////

void CBrowserFrame::SaveAsFile() 
{
   ExecCmdTarget(NULL, OLECMDID_SAVEAS);
}

void CBrowserFrame::OnFilePageSetup() 
{
   ExecCmdTarget(NULL, OLECMDID_PAGESETUP);
}

void CBrowserFrame::PrintDocument() 
{
   ExecCmdTarget(NULL, OLECMDID_PRINT);
}

void CBrowserFrame::OnEditCopy() 
{
	if (m_bUrlActive)
		m_cbURL.Copy();
	else
		ExecCmdTarget(NULL, OLECMDID_COPY);
}

void CBrowserFrame::OnEditCut() 
{
   ExecCmdTarget(NULL, OLECMDID_CUT);
}

void CBrowserFrame::OnEditPaste() 
{
   ExecCmdTarget(NULL, OLECMDID_PASTE);
}

//////////////////////////////////////////////////////////////////////
// WebBrowser specific menu items

void CBrowserFrame::OnEditFind() 
{
   // Invoke the find dialog box
   ExecCmdTarget(&CGID_IWebBrowser, CWBCmdGroup::HTMLID_FIND);
}

void CBrowserFrame::OnEditSelectAll() 
{
   // Select all items in the WebBrowser document
   ExecCmdTarget(NULL, OLECMDID_SELECTALL);
}

void CBrowserFrame::OnViewOptions() 
{
   // Invoke the view option sdialog box
	ExecCmdTarget(&CGID_IWebBrowser, CWBCmdGroup::HTMLID_OPTIONS);
}

void CBrowserFrame::OnViewRefresh() 
{
   // Refresh the current page
	((CBrowserView*)GetActiveView())->m_WebBrowser.Refresh();
}

void CBrowserFrame::OnViewStop() 
{
   // Stop loading the current page
	((CBrowserView*)GetActiveView())->m_WebBrowser.Stop();
}

void CBrowserFrame::OnViewSource() 
{
   // Invoke the view source dialog box
	ExecCmdTarget(&CGID_IWebBrowser, CWBCmdGroup::HTMLID_VIEWSOURCE);
}

void CBrowserFrame::activateUrlView(bool b)
{
	m_bUrlActive = b;
}

static void collectClipboardText(CView& view)
{
    if (!IsClipboardFormatAvailable(CF_TEXT))
		return; 
    if (!OpenClipboard(view.m_hWnd)) 
        return; 
	HGLOBAL hglb = GetClipboardData(CF_TEXT); 
    if (hglb != NULL && 6) 
    { 
        LPTSTR s = (LPTSTR)GlobalLock(hglb); 
        if (s != NULL) 
        { 
			// expand the CRs into CR/LF pairs
			CString cs(s);
			CString expanded = expandCRs(cs);
			const char* chars = (const char*)expanded;
			pCormanLisp->ProcessSource((char*)chars, expanded.GetLength());	
            GlobalUnlock(hglb); 
        } 
	}
    CloseClipboard(); 
}

void CBrowserFrame::OnChar(UINT nChar, UINT nRepCnt, UINT nFlags)
{
	if (nChar == 13 && ((nFlags & 0x100) || GetKeyState(VK_SHIFT) < 0))
	{
		// copy selected text to clipboard
		ExecCmdTarget(NULL, OLECMDID_COPY);
		CView* activeView = GetActiveView();
		if (activeView)
			collectClipboardText(*activeView);
	}
	else
		CSMDIChildWnd::OnChar(nChar, nRepCnt, nFlags);
}

////////////////////////////////////////////////////////////
// This method will call the IOleCommandTarget::Exec
// method to invoke the command target id specified
// for the command group specified.
////////////////////////////////////////////////////////////
HRESULT CBrowserFrame::ExecCmdTarget(const GUID *pguidCmdGroup, DWORD nCmdID)
{
   LPDISPATCH lpDispatch = NULL;
   LPOLECOMMANDTARGET lpOleCommandTarget = NULL;
   HRESULT hr = E_FAIL;

   lpDispatch = ((CBrowserView*)GetActiveView())->m_WebBrowser.GetDocument();
   ASSERT(lpDispatch);

   if (lpDispatch)
   {
      // Get an pointer for the IOleCommandTarget interface.
      hr = lpDispatch->QueryInterface(IID_IOleCommandTarget, (void**)&lpOleCommandTarget);
      ASSERT(lpOleCommandTarget);

      lpDispatch->Release();

      if (SUCCEEDED(hr))
      {
         // Invoke the given command id for the WebBrowser control
         hr = lpOleCommandTarget->Exec(pguidCmdGroup, nCmdID, 0, NULL, NULL);
         lpOleCommandTarget->Release();
      }
   }

   return hr;
}


BOOL CBrowserFrame::OnCommand(WPARAM wParam, LPARAM lParam) 
{
	// An item was entered into the URL combo box
   if (wParam == 1 && lParam == 0)
	{
	   CString strUrl;
      COleVariant	varEmpty;	// Default is VT_EMPTY

      // Navigate to the specified URL.
		m_cbURL.GetWindowText(strUrl);

	   try
	   {
         ((CBrowserView*)GetActiveView())->m_WebBrowser.Navigate(strUrl, &varEmpty,
                                                                 &varEmpty, &varEmpty,
                                                                 &varEmpty);
      }
      catch(CException* excp)
      {
         excp->Delete();
      }
	}
	
	return CSMDIChildWnd::OnCommand(wParam, lParam);
}

BOOL CBrowserFrame::PreTranslateMessage(MSG* pMsg) 
{
   // Pass all keystrokes to the browser unless the focus is set to 
   // the URL ComboBox.  Also, even if the focus is set to the URL
   // ComboBox, pass VK_TAB to the browser window.
   if (pMsg->message == WM_KEYDOWN)
   {
	  if (m_cbURL.m_hWnd != pMsg->hwnd
         || pMsg->wParam == VK_TAB)
      {
         LPDISPATCH lpDispatch = NULL;
         IOleInPlaceActiveObject* lpOleInPlaceActiveObj = NULL;

         lpDispatch = ((CBrowserView*)GetActiveView())->m_WebBrowser.GetDocument();
         ASSERT(lpDispatch);

         // Get an IDispatch pointer for the IOleInPlaceActiveObject interface.
         lpDispatch->QueryInterface(IID_IOleInPlaceActiveObject, (void**)&lpOleInPlaceActiveObj);
         ASSERT(lpOleInPlaceActiveObj);

         lpDispatch->Release();

         // Pass the keydown command to IOleInPlaceActiveObject::TranslateAccelerator 
         HRESULT hrTranslate = lpOleInPlaceActiveObj->TranslateAccelerator(pMsg);
         lpOleInPlaceActiveObj->Release();

         if (hrTranslate == S_OK)
            return TRUE;
      }
   }

   return CSMDIChildWnd::PreTranslateMessage(pMsg);
}

////////////////////////////////////////////////////////////////////

IMPLEMENT_DYNCREATE(CBrowserDoc, CDocument)

BEGIN_MESSAGE_MAP(CBrowserDoc, CDocument)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CWBSamplDoc construction/destruction

CBrowserDoc::CBrowserDoc()
{
}

CBrowserDoc::~CBrowserDoc()
{
}

BOOL CBrowserDoc::OnNewDocument()
{
	if (!CDocument::OnNewDocument())
		return FALSE;

	// TODO: add reinitialization code here
	// (SDI documents will reuse this document)

	return TRUE;
}

void CBrowserDoc::OnCloseDocument()
{
	((CCormanLispApp*)AfxGetApp())->m_browserDoc = 0;
	CDocument::OnCloseDocument();
}

void CBrowserDoc::Serialize(CArchive& ar)
{
	if (ar.IsStoring())
	{
		// TODO: add storing code here
	}
	else
	{
		CFile* file = ar.GetFile();
		if (file)
		{
			m_urlToLoad = file->GetFilePath();
		}
	}
}

#ifdef _DEBUG
void CBrowserDoc::AssertValid() const
{
	CDocument::AssertValid();
}

void CBrowserDoc::Dump(CDumpContext& dc) const
{
	CDocument::Dump(dc);
}
#endif //_DEBUG

////////////////////////////////////////////////////////////////

IMPLEMENT_DYNCREATE(CBrowserView, CView)
BEGIN_MESSAGE_MAP(CBrowserView, CView)
	ON_WM_CREATE()
	ON_WM_SIZE()
	ON_COMMAND(ID_FILE_PRINT, OnFilePrint)
	ON_COMMAND(ID_FILE_SAVE_AS, OnFileSaveAs)
	ON_COMMAND(ID_FILE_PRINT, CView::OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_DIRECT, CView::OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_PREVIEW, CView::OnFilePrintPreview)
	ON_WM_CHAR()
END_MESSAGE_MAP()

BEGIN_EVENTSINK_MAP(CBrowserView, CView)
	ON_EVENT(CBrowserView, ID_WEB_BROWSE, 102 /* StatusTextChange */, OnStatusTextChange, VTS_BSTR)
	ON_EVENT(CBrowserView, ID_WEB_BROWSE, 104 /* DownloadComplete */, OnDownloadComplete, VTS_NONE)
	ON_EVENT(CBrowserView, ID_WEB_BROWSE, 113 /* TitleChange */, OnTitleChange, VTS_BSTR)
END_EVENTSINK_MAP()

CBrowserView::CBrowserView() : CView(/*IDD_BROWSER*/)
{
}

CBrowserView::~CBrowserView()
{
}

BOOL CBrowserView::PreCreateWindow(CREATESTRUCT& cs)
{
	return CView::PreCreateWindow(cs);
}

void CBrowserView::OnDraw(CDC* /*pDC*/)
{
	CBrowserDoc* pDoc = GetDocument();
	ASSERT_VALID(pDoc);
}

BOOL CBrowserView::OnPreparePrinting(CPrintInfo* pInfo)
{
	return DoPreparePrinting(pInfo);
}

void CBrowserView::OnBeginPrinting(CDC* /*pDC*/, CPrintInfo* /*pInfo*/)
{
}

void CBrowserView::OnEndPrinting(CDC* /*pDC*/, CPrintInfo* /*pInfo*/)
{
}

#ifdef _DEBUG
void CBrowserView::AssertValid() const
{
	CView::AssertValid();
}

void CBrowserView::Dump(CDumpContext& dc) const
{
	CView::Dump(dc);
}
#endif //_DEBUG

CBrowserDoc* CBrowserView::GetDocument()
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CBrowserDoc)));
	return (CBrowserDoc*)m_pDocument;
}

/////////////////////////////////////////////////////////////////////////////
// CWBSamplView message handlers

int CBrowserView::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CView::OnCreate(lpCreateStruct) == -1)
		return -1;
	
	// Create WebBrowser control
	if (!m_WebBrowser.Create(NULL, WS_CHILD|WS_VISIBLE, CRect(), this, ID_WEB_BROWSE))
		return -1;

	// Navigate to the user's home page.
	// m_WebBrowser.GoHome();
	return 0;
}

void CBrowserView::OnSize(UINT nType, int cx, int cy) 
{
	CView::OnSize(nType, cx, cy);
	m_WebBrowser.MoveWindow(0, 0, cx, cy);
	m_WebBrowser.UpdateWindow();
}

////////////////////////////////////////////////////////////////////
// WebBrowser Events
void CBrowserView::OnTitleChange(LPCTSTR lpszText) 
{
	// The title has changed, so update our own document title
	GetDocument()->SetTitle(lpszText);
}

void CBrowserView::OnDownloadComplete() 
{
   ASSERT(m_pFrame);

   m_pFrame->SetUrlCBText(m_WebBrowser.GetLocationURL());
}

void CBrowserView::OnStatusTextChange(LPCTSTR lpszText) 
{
   ASSERT(m_pFrame);

	// The status text has changed, so update our own status bar
	if (m_pFrame)
		m_pFrame->SetStatusBarText(lpszText);
}

/////////////////////////////////////////////////////////////
// The saving and printing of the web document must be
// handled in the view in order to override the default
// behavior.  Call the save and print implementations
// available in the frame.  This method will enable you
// to have a central repository for menu handling code
// which will be most beneficial when using multiple views.
/////////////////////////////////////////////////////////////
void CBrowserView::OnFileSaveAs() 
{
   ASSERT(m_pFrame);
   m_pFrame->SaveAsFile();
}

void CBrowserView::OnFilePrint() 
{
   ASSERT(m_pFrame);
   m_pFrame->PrintDocument();
}

void
CBrowserView::OnInitialUpdate()
{
	CView::OnInitialUpdate();
	m_pFrame = (CBrowserFrame*)GetParent();
	ASSERT(m_pFrame);

	if (m_pFrame)
	{
		m_pFrame->m_hwndWebBrowser = m_WebBrowser.GetSafeHwnd();
		m_pFrame->CenterWindow();
	}
	CString path = GetDocument()->m_urlToLoad;
	if (!path.IsEmpty())
	{
		COleVariant noArg;
		m_WebBrowser.Navigate(path, &noArg, &noArg, &noArg, &noArg);
	}
}

BOOL CBrowserView::PreTranslateMessage(MSG* pMsg) 
{
   //
   // There is a problem on Windows 95 that prevents you
   // from typing in intrinsic controls (edit boxes, etc.)
   // This code fixes that problem.
   //
   if (IsDialogMessage(pMsg))
		return TRUE;
   else
		return CView::PreTranslateMessage(pMsg);
}

void CBrowserView::OnChar(UINT nChar, UINT nRepCnt, UINT nFlags)
{
	CView::OnChar(nChar, nRepCnt, nFlags);
}

IMPLEMENT_DYNCREATE(CUrlEdit, CEdit)

BEGIN_MESSAGE_MAP(CUrlEdit, CEdit)
	ON_WM_ACTIVATE()
END_MESSAGE_MAP()

void CUrlEdit::OnActivate(UINT nState, CWnd* pWndOther, BOOL bMinimized)
{
	CFrameWnd* frm = GetParentFrame();
	if (nState == WA_INACTIVE)
		((CBrowserFrame*)frm)->activateUrlView(false);
	else
		((CBrowserFrame*)frm)->activateUrlView(true);
	CEdit::OnActivate(nState, pWndOther, bMinimized);
}


