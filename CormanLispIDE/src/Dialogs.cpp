//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		Dialogs.cpp
//		Contents:	Corman Lisp application source file
//		History:	10/13/98  RGC  Created.
//					10/01/16  Artem Boldarev
//					           
//
#include "stdafx.h"
#include "../resource.h"
#include <stddef.h>
#include <string.h>

#include "Dialogs.h"
#include "UtilFuncs.h"
#include "CormanLisp.h"

CString getCormanLispDirectory();

BEGIN_MESSAGE_MAP(AboutDialog, CDialog)
	ON_COMMAND(ID_CREDITS,		OnCreditsInfo)
	ON_COMMAND(ID_CORMAN_NET,	OnCormanNet)
END_MESSAGE_MAP()

AboutDialog::AboutDialog(UINT nIDTemplate)
: CDialog(nIDTemplate)
{
}

BOOL AboutDialog::OnInitDialog()
{
	CStatic* name = (CStatic*)GetDlgItem(IDC_ABOUTNAME);
    CStatic* version = (CStatic*)GetDlgItem(IDC_REGVERSION);

	
    char* versionCaption = getVersionCaption();
    if (versionCaption)
		version->SetWindowText(versionCaption);

	name->SetWindowText("");
	try
	{
		size_t user_name_len = 0;
		if (pCormanLisp->GetCurrentUserName(NULL, &user_name_len) == S_OK)
		{
			char *username = new char[user_name_len + 1];
			pCormanLisp->GetCurrentUserName(username, &user_name_len);
			name->SetWindowText(username);
			delete[] username;
		}
	}
	catch (...)
	{

	}

	CWnd* item = GetDlgItem(IDOK);
	if (item)
		((CButton*)item)->SetButtonStyle(BS_DEFPUSHBUTTON);
	SetDefID(IDOK);
	return FALSE;
}

void AboutDialog::OnCreditsInfo()
{
	CreditsDialog creditsDlg(IDD_CREDITS);
	int result = creditsDlg.DoModal();
}

void AboutDialog::OnCormanNet()
{
	/*
	HINSTANCE result = ShellExecute(theApp.GetMainWnd()->m_hWnd, 
		"open", 
		"http://www.cormanlisp.com/index.html",
		"", "", SW_NORMAL);
	*/
	/*
	SHELLEXECUTEINFO ei;
	memset(&ei, 0, sizeof(ei));
	ei.cbSize = sizeof(ei);
	ei.lpVerb = "open";
	ei.lpFile = "http://www.cormanlisp.com";
	ei.fMask = SEE_MASK_NOCLOSEPROCESS;
	ei.nShow = SW_SHOWDEFAULT;
	BOOL success = ShellExecuteEx(&ei);
	*/
	theApp.ShellOpenURL("https://github.com/sharplispers/cormanlisp");
}

BEGIN_MESSAGE_MAP(LegalDialog, CDialog)
END_MESSAGE_MAP()

LegalDialog::LegalDialog(UINT nIDTemplate)
: CDialog(nIDTemplate), mappedFile(0)
{
}

BOOL LegalDialog::OnInitDialog()
{
	char legalFilePath[MAX_PATH];
	strcpy_s(legalFilePath, sizeof(legalFilePath), getCormanLispDirectory());
	strcat_s(legalFilePath, sizeof(legalFilePath), "\\documentation\\legal.txt");
	DWORD length;
	mappedFile = MapFile(legalFilePath, &length);
	char buf[0x6000];
	if (mappedFile)
		memcpy(buf, mappedFile, length);
	buf[length] = 0;
	CWnd* item = GetDlgItem(IDC_LEGALTEXT);
	if (item && mappedFile)
		item->SetWindowText(buf);

	CDC* cdc = item->GetDC();
	HDC hDC = cdc->m_hDC;
	CFont* font = theApp.getCourierFont(hDC, 10);
	item->SetFont(font, TRUE);
	SelectObject(hDC, *font);

	item = GetDlgItem(IDOK);
	if (item)
		((CButton*)item)->SetButtonStyle(BS_DEFPUSHBUTTON);
	SetDefID(IDOK);
	return FALSE;
}

LegalDialog::~LegalDialog()
{
	if (mappedFile)
	{
		UnmapFile(mappedFile);
	}
}

CString getCormanLispDirectory()
{
	char buf[MAX_PATH];
	int ret = GetModuleFileName(0, buf, sizeof buf);
	int len = strlen(buf);
	char* p = buf + len - 1;
	while (p > buf && (*p != '\\' && *p != '/'))
		p--;
	*p = 0;
	return CString(buf);
}

BEGIN_MESSAGE_MAP(CreditsDialog, CDialog)
END_MESSAGE_MAP()

CreditsDialog::CreditsDialog(UINT nIDTemplate)
: CDialog(nIDTemplate), mappedFile(0)
{
}

BOOL CreditsDialog::OnInitDialog()
{
	char legalFilePath[MAX_PATH];
	strcpy_s(legalFilePath, sizeof(legalFilePath), getCormanLispDirectory());
	strcat_s(legalFilePath, sizeof(legalFilePath), "\\documentation\\credits.txt");
	DWORD length;
	mappedFile = MapFile(legalFilePath, &length);
	char buf[0x6000];
	if (mappedFile)
		memcpy(buf, mappedFile, length);
	buf[length] = 0;
	CWnd* item = GetDlgItem(IDC_LEGALTEXT);
	if (item && mappedFile)
		item->SetWindowText(buf);

	CDC* cdc = item->GetDC();
	HDC hDC = cdc->m_hDC;
	CFont* font = theApp.getCourierFont(hDC, 10);
	item->SetFont(font, TRUE);
	SelectObject(hDC, *font);

	item = GetDlgItem(IDOK);
	if (item)
		((CButton*)item)->SetButtonStyle(BS_DEFPUSHBUTTON);
	SetDefID(IDOK);
	return FALSE;
}

CreditsDialog::~CreditsDialog()
{
	if (mappedFile)
	{
		UnmapFile(mappedFile);
	}
}

BEGIN_MESSAGE_MAP(GotoLineDialog, CDialog)
END_MESSAGE_MAP()

GotoLineDialog::GotoLineDialog(UINT nIDTemplate)
: CDialog(nIDTemplate), lineNumberEdit(0), lineNumber_(0)
{
}

BOOL GotoLineDialog::OnInitDialog()
{
	lineNumberEdit = (CEdit*)GetDlgItem(IDC_GOTO_LINE_NUMBER);
	GotoDlgCtrl(lineNumberEdit);	
	return FALSE;
}

int GotoLineDialog::lineNumber()
{
	return lineNumber_;
}

void GotoLineDialog::OnOK()
{
	lineNumber_ = (int)GetDlgItemInt(IDC_GOTO_LINE_NUMBER);
	CDialog::OnOK();
}

