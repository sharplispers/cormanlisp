//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		RegistrationDialog.cpp
//		Contents:	Corman Lisp application source file
//		History:	10/13/98  RGC  Created.
//
#include "stdafx.h"
#include "../resource.h"
#include <string.h>

#include "RegistrationDialog.h"
#include "UtilFuncs.h"
#include "CormanLisp.h"

CString getCormanLispDirectory();

BEGIN_MESSAGE_MAP(RegistrationDialog, CDialog)
	ON_WM_CHAR()
	ON_WM_KEYDOWN()
	ON_COMMAND(ID_LEGAL_INFO,	OnLegalInfo)
END_MESSAGE_MAP()

RegistrationDialog::RegistrationDialog(UINT nIDTemplate)
: CDialog(nIDTemplate)
{
}

BOOL RegistrationDialog::OnInitDialog()
{
	CWnd* item = GetDlgItem(IDOK);
	if (item)
	{
//		item->EnableWindow(FALSE); // disable this
	}
	m_name = (CEdit*)GetDlgItem(IDC_EDITNAME);
	m_organization = (CEdit*)GetDlgItem(IDC_EDITORG);
//	m_registrationCode = (CEdit*)GetDlgItem(IDC_EDITREGCODE);

	char namebuf[256];
	unsigned long length = 256;
	GetUserName(namebuf, &length);
	m_name->SetWindowText(namebuf);

	GotoDlgCtrl(m_name);
	SetDefID(IDOK);
	if (item)
		((CButton*)item)->SetButtonStyle(BS_DEFPUSHBUTTON);
	item = GetDlgItem(IDCANCEL);
	if (item)
		((CButton*)item)->SetButtonStyle(0);

	RegistrationInfo info;
	GetRegistrationInfo(&info);
/*
	if (!(info.isRegistered))
	{
		item = GetDlgItem(IDC_DAYSREMAINING);
		time_t ltime;
		time_t currTime = time(&ltime);
		int daysRemaining = info.daysRemaining;
		if (daysRemaining < 0)
		{
			char buf[256];
			sprintf_s(buf, sizeof(buf),
				"Sorry, your evaluation time has expired. "
				"You are %d days past your evaluation period. "
				"You may use this application, but you will be periodically "
				"reminded to register your copy.", -daysRemaining);
			AfxMessageBox(buf);
			theApp.m_expired = TRUE;
			theApp.m_lastExpiredScreenTime = GetTickCount();
			item->SetWindowText("Expired");
		}
		else
		{
			char buf[16];
			sprintf_s(buf, sizeof(buf), "%d", daysRemaining);
			item->SetWindowText(buf);
		}
	}
*/
	return FALSE;
}

void RegistrationDialog::OnChar(UINT nChar, UINT nRepCnt, UINT nFlags)
{
	CDialog::OnChar(nChar, nRepCnt, nFlags);
}

#define EncryptionKey_3_0 "EmitIdy"

void RegistrationDialog::OnOK()
{
	CString name;
	CString organization;
	CString registrationCode;
	m_name->GetWindowText(name);
	m_organization->GetWindowText(organization);
//	m_registrationCode->GetWindowText(registrationCode);

	HMODULE module = LoadLibrary("license.dll");
	long code;

	if (module)
	{
		FARPROC proc = GetProcAddress(module, "generateRegistrationCode");
		if (proc)
		{
			code = reinterpret_cast<long (*)(const char*, const char*, const char*)>(proc)(name, organization, EncryptionKey_3_0);
		}
	}

	registrationCode.Format("%ld", code);

	if (!registrationOK(name, organization, registrationCode))
	{
		AfxMessageBox("The registration code you entered was not valid.");
	}
	else
	{
		if (!Register(name, organization, registrationCode))
			AfxMessageBox("Registration failed.");
		CDialog::OnOK();
	}
}

void RegistrationDialog::OnCancel()
{
	CDialog::OnCancel();
}

void 
RegistrationDialog::OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags)
{
	CDialog::OnKeyDown(nChar, nRepCnt, nFlags);
}

bool RegistrationDialog::registrationOK(const CString& name,
										const CString& organization,
										const CString& registrationCode)
{
	HMODULE module = LoadLibrary("license.dll");

	if (module)
	{
		FARPROC proc = GetProcAddress(module, "CheckRegistration");
		if (proc)
		{
			int n = reinterpret_cast<int (*)(const char*, const char*, const char*)>(proc)(name, organization, registrationCode);
            return n != 0;
		} 			
	}

//	int n = ::CheckRegistration(name, organization, registrationCode);
	return false;
}

//
//	Returns true if successful, false otherwise.
//
bool RegistrationDialog::Register(const char* name, const char* organization, const char* code)
{
	HMODULE module = LoadLibrary("license.dll");
	if (module)
    {
		FARPROC proc = GetProcAddress(module, "Register");
		if (proc)
		{
			return reinterpret_cast<bool (*)(const char*, const char*, const char*)>(proc)(name, organization, code);
		} 			
    }
	return false;
}

//
//	Returns true if successful, false otherwise.
//
bool RegistrationDialog::Unregister()
{
	HMODULE module = LoadLibrary("license.dll");
	if (module)
    {
		FARPROC proc = GetProcAddress(module, "Unregister");
		if (proc)
		{
			return reinterpret_cast<bool (*)()>(proc)();
		} 			
    }
	return false;
}

void RegistrationDialog::GetRegistrationInfo(RegistrationInfo* info)
{
	HMODULE module = LoadLibrary("license.dll");
	if (module)
	{
		FARPROC proc = GetProcAddress(module, "GetRegistrationInfo");
		if (proc)
		{
			reinterpret_cast<void (*)(RegistrationInfo*)>(proc)(info);
		} 			
	}
}

void RegistrationDialog::OnLegalInfo()
{
	LegalDialog legalDlg(IDD_LEGAL);
	int result = legalDlg.DoModal();
}

BEGIN_MESSAGE_MAP(AboutDialog, CDialog)
	ON_COMMAND(ID_LEGAL_INFO,	OnLegalInfo)
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
	CStatic* org = (CStatic*)GetDlgItem(IDC_ABOUTORG);
	CStatic* evalMessage = (CStatic*)GetDlgItem(IDC_EVALMESSAGE);
	CStatic* daysRemaining = (CStatic*)GetDlgItem(IDC_DAYSREMAINING);
    CStatic* version = (CStatic*)GetDlgItem(IDC_REGVERSION);
	RegistrationInfo info;
	RegistrationDialog::GetRegistrationInfo(&info);
    char* versionCaption = getVersionCaption();
    if (versionCaption)
        version->SetWindowText(versionCaption);

	if (info.isRegistered)
	{
		name->SetWindowText(info.name);
		org->SetWindowText(info.organization);
		evalMessage->ShowWindow(SW_HIDE);
		daysRemaining->ShowWindow(SW_HIDE);
	}
	else
	{
		name->SetWindowText("Evaluation Copy");
		org->SetWindowText("");
		int remaining = info.daysRemaining;
		if (remaining < 0)
		{
			daysRemaining->SetWindowText("Expired");
			theApp.m_expired = TRUE;
		}
		else
		{
			char buf[16];
			sprintf_s(buf, sizeof(buf), "%d", remaining);
			daysRemaining->SetWindowText(buf);
		}
	}
	CWnd* item = GetDlgItem(IDOK);
	if (item)
		((CButton*)item)->SetButtonStyle(BS_DEFPUSHBUTTON);
	SetDefID(IDOK);
	return FALSE;
}

void AboutDialog::OnLegalInfo()
{
	//LegalDialog legalDlg(IDD_LEGAL);
	//int result = legalDlg.DoModal();
	char CormanLispDirectory[MAX_PATH];

 	DWORD chars = GetModuleFileName(0, CormanLispDirectory, sizeof(CormanLispDirectory));
	int index = chars - 1;
	while (index >= 0 && CormanLispDirectory[index] != '\\')
		index--;
	CormanLispDirectory[index] = 0;	// get rid of file name, just leave the path
	if (chars > 0)
		strcat_s(CormanLispDirectory, sizeof(CormanLispDirectory), "\\");
	strcat_s(CormanLispDirectory, sizeof(CormanLispDirectory), "documentation\\license-agreement.txt");
	theApp.NavigateURL(CormanLispDirectory);
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
	theApp.NavigateURL("http://www.cormanlisp.com");
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

