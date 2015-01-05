//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		PreferencesDialog.cpp
//		Contents:	Corman Lisp preferences dialog implementation
//		History:	3/24/99  RGC  Created.
//
#include "stdafx.h"
#include "../resource.h"
#include <string.h>
#include <afxdlgs.h>

#include "PreferencesDialog.h"
#include "CormanLisp.h"

// these go under HKEY_CURRENT_USER
char PrefsKeyValues[][2][128] =
{
	{ __TEXT("Software\\Corman Technologies\\Corman Lisp\\3.0\\Editor Preferences\\AutoIndent"), __TEXT("1") },
	{ __TEXT("Software\\Corman Technologies\\Corman Lisp\\3.0\\Editor Preferences\\ParenMatching"), __TEXT("1") },
	{ __TEXT("Software\\Corman Technologies\\Corman Lisp\\3.0\\Editor Preferences\\AutoPrototypeOnMouseMove"), __TEXT("1") },
	{ __TEXT("Software\\Corman Technologies\\Corman Lisp\\3.0\\Editor Preferences\\AutoPrototypeOnKeyDown"), __TEXT("1") },
	{ __TEXT("Software\\Corman Technologies\\Corman Lisp\\3.0\\Editor Preferences\\AppendLispOutputToEnd"), __TEXT("1") },
	{ __TEXT("Software\\Corman Technologies\\Corman Lisp\\3.0\\Editor Preferences\\Tab"), __TEXT("4") },
	{ __TEXT("Software\\Corman Technologies\\Corman Lisp\\3.0\\Editor Preferences\\CharSize"), __TEXT("10") },
	{ __TEXT("Software\\Corman Technologies\\Corman Lisp\\3.0\\Editor Preferences\\FontName"), __TEXT("Courier New") },
	{ __TEXT("Software\\Corman Technologies\\Corman Lisp\\3.0\\Editor Preferences\\CharBold"), __TEXT("0") },
	{ __TEXT("Software\\Corman Technologies\\Corman Lisp\\3.0\\Editor Preferences\\CharItalic"), __TEXT("0") },
	{ __TEXT("Software\\Corman Technologies\\Corman Lisp\\3.0\\Editor Preferences\\TextColor"), __TEXT("000000") },
	{ __TEXT("Software\\Corman Technologies\\Corman Lisp\\3.0\\Editor Preferences\\HighlightTextColor"), __TEXT("0000c8") },
	{ __TEXT("Software\\Corman Technologies\\Corman Lisp\\3.0\\Editor Preferences\\OutputTextColor"), __TEXT("800000") },
	{ __TEXT("Software\\Corman Technologies\\Corman Lisp\\3.0\\Editor Preferences\\HintBackgroundColor"), __TEXT("c0ffff") },
	{ __TEXT("Software\\Corman Technologies\\Corman Lisp\\3.0\\Editor Preferences\\ReplaceTabsWithSpaces"), __TEXT("1") },
	{ __TEXT("Software\\Corman Technologies\\Corman Lisp\\3.0\\Editor Preferences\\RememberOpenDocuments"), __TEXT("1") },
	{ __TEXT("Software\\Corman Technologies\\Corman Lisp\\3.0\\Editor Preferences\\RememberDocumentPositions"), __TEXT("1") },
	{ __TEXT("Software\\Corman Technologies\\Corman Lisp\\3.0\\Editor Preferences\\FullPathInTitle"), __TEXT("1") },
	{ __TEXT("Software\\Corman Technologies\\Corman Lisp\\3.0\\Editor Preferences\\AutoColorize"), __TEXT("1") },
};

const int PrefsKeyValuesNum = sizeof(PrefsKeyValues)
                            /sizeof(*PrefsKeyValues);

BEGIN_MESSAGE_MAP(PreferencesDialog, CDialog)
	ON_WM_CHAR()
	ON_WM_KEYDOWN()
	ON_WM_CTLCOLOR()
	ON_WM_DRAWITEM()
	ON_COMMAND(IDC_RESTORE, OnRestoreDefaults)
	ON_COMMAND(IDC_SELECT_FONT, OnSelectFont)
	ON_COMMAND(ID_TEXTCOLOR, OnChangeTextColor)
	ON_COMMAND(ID_HIGHLIGHTTEXTCOLOR, OnChangeHighlightTextColor)
	ON_COMMAND(ID_OUTPUTTEXTCOLOR, OnChangeOutputTextColor)
	ON_COMMAND(ID_HINTBACKGROUNDCOLOR, OnChangeHintBackgroundColor)
END_MESSAGE_MAP()

PreferencesDialog::PreferencesDialog(UINT nIDTemplate)
: CDialog(nIDTemplate)
{
}

void PreferencesDialog::updateDialog()
{
	int err = 0;
	char s[10];

	setCheckBox(IDC_PREFS_AUTO_INDENT,			info.autoIndent);
	setCheckBox(IDC_PREFS_PAREN_MATCHING,		info.parenthesesMatching);
	setCheckBox(IDC_PREFS_PROT_MOUSE_MOVE,		info.autoPrototypeOnMouseMove);
	setCheckBox(IDC_PREFS_PROT_KEY_DOWN,		info.autoPrototypeOnKeyDown);
	setCheckBox(IDC_PREFS_APPEND_LISP_OUTPUT,	info.appendLispOutputToEnd);
	setCheckBox(IDC_REPLACE_TABS_WITH_SPACES,	info.replaceTabsWithSpaces);
	setCheckBox(IDC_REMEMBER_OPEN_DOCUMENTS,	info.rememberOpenDocuments);
	setCheckBox(IDC_REMEMBER_DOCUMENT_POSITIONS,info.rememberDocumentPositions);
	setCheckBox(IDC_FULL_PATH_IN_TITLE,			info.fullPathInTitle);
	setCheckBox(IDC_AUTO_COLORIZE,				info.autoColorize);

	err = _itoa_s(info.tab, s, sizeof(s), 10);
	setText(IDC_TAB, s);
	err = _itoa_s(info.charSize, s, sizeof(s), 10);
	setText(IDC_CHAR_SIZE, s);
	setText(IDC_FONT_NAME,						info.fontName);
	setCheckBox(IDC_PREFS_BOLD,					info.charBold);
	setCheckBox(IDC_PREFS_ITALIC,				info.charItalic);

	textColorWnd->InvalidateRect(NULL, FALSE);
	highlightTextColorWnd->InvalidateRect(NULL, FALSE);
	outputTextColorWnd->InvalidateRect(NULL, FALSE);
	hintBackgroundColorWnd->InvalidateRect(NULL, FALSE);
}

BOOL PreferencesDialog::OnInitDialog()
{
	CDialog::OnInitDialog();

	CWnd* item = GetDlgItem(IDOK);

	SetDefID(IDOK);
	if (item)
		((CButton*)item)->SetButtonStyle(BS_DEFPUSHBUTTON);
	item = GetDlgItem(IDCANCEL);
	if (item)
		((CButton*)item)->SetButtonStyle(0);

	GetPreferencesInfo(&info);
	CRect rect;

	// initialize buttons
	textColorWnd = GetDlgItem(ID_TEXTCOLOR);
	highlightTextColorWnd = GetDlgItem(ID_HIGHLIGHTTEXTCOLOR);
	outputTextColorWnd = GetDlgItem(ID_OUTPUTTEXTCOLOR);
	hintBackgroundColorWnd = GetDlgItem(ID_HINTBACKGROUNDCOLOR);

	updateDialog();
	return TRUE;
}

HBRUSH PreferencesDialog::OnCtlColor(CDC* dc, CWnd* wnd, UINT color)
{
	return CDialog::OnCtlColor(dc, wnd, color);
}

void PreferencesDialog::OnDrawItem(int nIDCtl, LPDRAWITEMSTRUCT di)
{
	RECT rect;
	::GetClientRect(di->hwndItem, &rect);
	if (di->hwndItem == textColorWnd->m_hWnd)
	{
		HBRUSH b = CreateSolidBrush(info.textColor);
		FillRect(di->hDC, &di->rcItem, b);
		FrameRect(di->hDC, &di->rcItem, (HBRUSH)GetStockObject(BLACK_BRUSH));
	}
	if (di->hwndItem == highlightTextColorWnd->m_hWnd)
	{
		HBRUSH b = CreateSolidBrush(info.highlightTextColor);
		FillRect(di->hDC, &di->rcItem, b);
		FrameRect(di->hDC, &di->rcItem, (HBRUSH)GetStockObject(BLACK_BRUSH));
	}
	if (di->hwndItem == outputTextColorWnd->m_hWnd)
	{
		HBRUSH b = CreateSolidBrush(info.outputTextColor);
		FillRect(di->hDC, &di->rcItem, b);
		FrameRect(di->hDC, &di->rcItem, (HBRUSH)GetStockObject(BLACK_BRUSH));
	}
	if (di->hwndItem == hintBackgroundColorWnd->m_hWnd)
	{
		HBRUSH b = CreateSolidBrush(info.hintBackgroundColor);
		FillRect(di->hDC, &di->rcItem, b);
		FrameRect(di->hDC, &di->rcItem, (HBRUSH)GetStockObject(BLACK_BRUSH));
	}
}

void PreferencesDialog::OnChar(UINT nChar, UINT nRepCnt, UINT nFlags)
{
	CDialog::OnChar(nChar, nRepCnt, nFlags);
}

void PreferencesDialog::OnOK()
{
	info.autoIndent =				getCheckBox(IDC_PREFS_AUTO_INDENT);
	info.parenthesesMatching =		getCheckBox(IDC_PREFS_PAREN_MATCHING);
	info.autoPrototypeOnMouseMove = getCheckBox(IDC_PREFS_PROT_MOUSE_MOVE);
	info.autoPrototypeOnKeyDown =	getCheckBox(IDC_PREFS_PROT_KEY_DOWN);
	info.appendLispOutputToEnd =	getCheckBox(IDC_PREFS_APPEND_LISP_OUTPUT);
	info.replaceTabsWithSpaces =	getCheckBox(IDC_REPLACE_TABS_WITH_SPACES);
	info.rememberOpenDocuments =	getCheckBox(IDC_REMEMBER_OPEN_DOCUMENTS);
	info.rememberDocumentPositions =getCheckBox(IDC_REMEMBER_DOCUMENT_POSITIONS);
	info.fullPathInTitle =			getCheckBox(IDC_FULL_PATH_IN_TITLE);
	info.autoColorize	 =			getCheckBox(IDC_AUTO_COLORIZE);

	int n;
	CString s =						getText(IDC_TAB);
	if (!s.IsEmpty())
	{
		n = atoi((const char*)s);
		if (n < 1 || n > 16) 
			n = 4;
	}
	info.tab = n;

	s =								getText(IDC_CHAR_SIZE);
	if (!s.IsEmpty())
	{
		n = atoi((const char*)s);
		if (n < 4 || n > 72) 
			n = 10;
	}
	info.charSize = n;

	strcpy_s(info.fontName, sizeof(info.fontName), (const char*)getText(IDC_FONT_NAME));
	info.charBold =	getCheckBox(IDC_PREFS_BOLD);
	info.charItalic =	getCheckBox(IDC_PREFS_ITALIC);

	SavePreferencesInfo(&info);

	CDialog::OnOK();
}

void PreferencesDialog::OnCancel()
{
	CDialog::OnCancel();
}

void PreferencesDialog::restoreDefaults()
{
	info.autoIndent = true;
	info.parenthesesMatching = true;
	info.autoPrototypeOnMouseMove = true;
	info.autoPrototypeOnKeyDown = true;
	info.appendLispOutputToEnd = true;
	info.replaceTabsWithSpaces = true;
	info.rememberOpenDocuments = true;
	info.rememberDocumentPositions = true;
	info.fullPathInTitle = true;
	info.autoColorize = true;
	info.tab = 4;
	info.charSize = 10;
	strncpy_s(info.fontName, sizeof(info.fontName), "Courier New", 127);
	info.charBold = false;
	info.charItalic = false;
	info.textColor = RGB(0, 0, 0);
	info.highlightTextColor = RGB(200, 0, 0);
	info.outputTextColor = RGB(0, 0, 128);
	info.hintBackgroundColor = RGB(255, 255, 192);
}

void PreferencesDialog::OnRestoreDefaults()
{
	restoreDefaults();
	updateDialog();
}

void PreferencesDialog::OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags)
{
	CDialog::OnKeyDown(nChar, nRepCnt, nFlags);
}

void PreferencesDialog::OnSelectFont()
{
	CDC* cdc = GetDC();
	HDC hDC = cdc->m_hDC;
	CHARFORMAT cf = { sizeof(cf) };
//	int height = -MulDiv(theApp.preferences.charSize, GetDeviceCaps(hDC, LOGPIXELSY), 72);
	int height = -info.charSize * 20;
	strcpy_s(cf.szFaceName, sizeof(cf.szFaceName), info.fontName);
	cf.yHeight = height;
	cf.dwEffects |= (info.charBold ? CFE_BOLD : 0);
	cf.dwEffects |= (info.charItalic ? CFE_ITALIC : 0);

	cf.dwMask = CFM_FACE|CFM_BOLD|CFM_ITALIC|CFM_SIZE;
	CFontDialog dlg(cf, CF_FIXEDPITCHONLY|CF_BOTH|CF_NOOEMFONTS, NULL, this);
	if (dlg.DoModal() == IDOK)
	{
		dlg.GetCharFormat(cf);
		strcpy_s(info.fontName, sizeof(info.fontName), cf.szFaceName);
		info.charSize = cf.yHeight / 20;
		info.charBold = cf.dwEffects & CFE_BOLD;
		info.charItalic = (cf.dwEffects & CFE_ITALIC) ? true : false;
		updateDialog();
	}
}

void PreferencesDialog::OnChangeTextColor()
{
	CColorDialog dlg(info.textColor, 0, this);
	if (dlg.DoModal() == IDOK)
	{
		info.textColor = dlg.GetColor();
		updateDialog();
		textColorWnd->InvalidateRect(NULL, FALSE);
	}
}

void PreferencesDialog::OnChangeHighlightTextColor()
{
	CColorDialog dlg(info.highlightTextColor, 0, this);
	if (dlg.DoModal() == IDOK)
	{
		info.highlightTextColor = dlg.GetColor();
		updateDialog();
		highlightTextColorWnd->InvalidateRect(NULL, FALSE);
	}
}

void PreferencesDialog::OnChangeOutputTextColor()
{
	CColorDialog dlg(info.outputTextColor, 0, this);
	if (dlg.DoModal() == IDOK)
	{
		info.outputTextColor = dlg.GetColor();
		updateDialog();
		outputTextColorWnd->InvalidateRect(NULL, FALSE);
	}
}

void PreferencesDialog::OnChangeHintBackgroundColor()
{
	CColorDialog dlg(info.hintBackgroundColor, 0, this);
	if (dlg.DoModal() == IDOK)
	{
		info.hintBackgroundColor = dlg.GetColor();
		updateDialog();
		hintBackgroundColorWnd->InvalidateRect(NULL, FALSE);
	}
}

static int hexValue(int c) 
{
	if (c >= '0' && c <= '9')
		return c - '0';
	if (c >= 'a' && c <= 'f')
		return c - 'a' + 10;
	if (c >= 'A' && c <= 'F')
		return c - 'A' + 10;
	return 0;	// return -1 if erroneous char found
}

static unsigned long hextoi(char* s)
{
	unsigned long result = 0;
	while (*s)
	{
		int val = hexValue(*s);
		if (val != -1)		// skip over non hex digits
		{
			result *= 16;
			result += val;
		}
		s++;
	}
	return result;
}

void PreferencesDialog::GetPreferencesInfo(PreferencesInfo* info)
{
	LONG r = ERROR_SUCCESS;
	LONG d;
    for (int i = 0; r == ERROR_SUCCESS && i < PrefsKeyValuesNum; i++)
	{
		d = sizeof(PrefsKeyValues[i][1]);
        r = RegQueryValue(HKEY_CURRENT_USER,
                        (const char*)PrefsKeyValues[i][0], 
                        PrefsKeyValues[i][1],
                        &d);
	}
	
	info->autoIndent =				PrefsKeyValues[0][1][0] == '1' ? true : false;
	info->parenthesesMatching =		PrefsKeyValues[1][1][0] == '1' ? true : false;
	info->autoPrototypeOnMouseMove= PrefsKeyValues[2][1][0] == '1' ? true : false;
	info->autoPrototypeOnKeyDown =	PrefsKeyValues[3][1][0] == '1' ? true : false;
	info->appendLispOutputToEnd =	PrefsKeyValues[4][1][0] == '1' ? true : false;
	int n;
	char* s =						PrefsKeyValues[5][1];
	if (s != 0 && s[0] != 0)
	{
		n = atoi(s);
		if (n < 1 || n > 16) 
			n = 4;
	}
	info->tab = n;
	s =								PrefsKeyValues[6][1];
	if (s != 0 && s[0] != 0)
	{
		n = atoi(s);
		if (n < 4 || n > 72) 
			n = 10;
	}
	info->charSize = n;
	strncpy_s(info->fontName, sizeof(info->fontName), PrefsKeyValues[7][1], 127);
	info->charBold =				PrefsKeyValues[8][1][0] == '1' ? true : false;
	info->charItalic =				PrefsKeyValues[9][1][0] == '1' ? true : false;
	s =								PrefsKeyValues[10][1];
	if (s != 0 && s[0] != 0)
		info->textColor = hextoi(s);
	s =								PrefsKeyValues[11][1];
	if (s != 0 && s[0] != 0)
		info->highlightTextColor = hextoi(s);
	s =								PrefsKeyValues[12][1];
	if (s != 0 && s[0] != 0)
		info->outputTextColor = hextoi(s);
	s =								PrefsKeyValues[13][1];
	if (s != 0 && s[0] != 0)
		info->hintBackgroundColor = hextoi(s);

	info->replaceTabsWithSpaces =	PrefsKeyValues[14][1][0] == '1' ? true : false;
	info->rememberOpenDocuments =	PrefsKeyValues[15][1][0] == '1' ? true : false;
	info->rememberDocumentPositions=PrefsKeyValues[16][1][0] == '1' ? true : false;
	info->fullPathInTitle =			PrefsKeyValues[17][1][0] == '1' ? true : false;
	info->autoColorize =			PrefsKeyValues[18][1][0] == '1' ? true : false;
}

void PreferencesDialog::SavePreferencesInfo(PreferencesInfo* info)
{
	PrefsKeyValues[0][1][0] = info->autoIndent					? '1' : '0';
	PrefsKeyValues[1][1][0] = info->parenthesesMatching			? '1' : '0';
	PrefsKeyValues[2][1][0] = info->autoPrototypeOnMouseMove	? '1' : '0';
	PrefsKeyValues[3][1][0] = info->autoPrototypeOnKeyDown		? '1' : '0';
	PrefsKeyValues[4][1][0] = info->appendLispOutputToEnd		? '1' : '0';

	char s[10];
	int err = 0;
	err = _itoa_s(info->tab, s, sizeof(s), 10);
	strcpy_s(PrefsKeyValues[5][1], sizeof(PrefsKeyValues[5][1]), s);
	err = _itoa_s(info->charSize, s, sizeof(s), 10);
	strcpy_s(PrefsKeyValues[6][1], sizeof(PrefsKeyValues[6][1]), s);
	strncpy_s(PrefsKeyValues[7][1], sizeof(PrefsKeyValues[7][1]), info->fontName, 127);
	PrefsKeyValues[8][1][0] = info->charBold					? '1' : '0';
	PrefsKeyValues[9][1][0] = info->charItalic					? '1' : '0';
	err = _itoa_s(info->textColor, s, sizeof(s), 16);
	strcpy_s(PrefsKeyValues[10][1], sizeof(PrefsKeyValues[10][1]), s);
	err = _itoa_s(info->highlightTextColor, s, sizeof(s), 16);
	strcpy_s(PrefsKeyValues[11][1], sizeof(PrefsKeyValues[11][1]), s);
	err = _itoa_s(info->outputTextColor, s, sizeof(s), 16);
	strcpy_s(PrefsKeyValues[12][1], sizeof(PrefsKeyValues[12][1]), s);
	err = _itoa_s(info->hintBackgroundColor, s, sizeof(s), 16);
	strcpy_s(PrefsKeyValues[13][1], sizeof(PrefsKeyValues[13][1]), s);

	PrefsKeyValues[14][1][0] = info->replaceTabsWithSpaces		? '1' : '0';
	PrefsKeyValues[15][1][0] = info->rememberOpenDocuments		? '1' : '0';
	PrefsKeyValues[16][1][0] = info->rememberDocumentPositions	? '1' : '0';
	PrefsKeyValues[17][1][0] = info->fullPathInTitle			? '1' : '0';
	PrefsKeyValues[18][1][0] = info->autoColorize				? '1' : '0';

	LONG r = ERROR_SUCCESS;
    for (int i = 0; r == ERROR_SUCCESS && i < PrefsKeyValuesNum; i++)
        r = RegSetValue(HKEY_CURRENT_USER,
                        PrefsKeyValues[i][0], 
                        REG_SZ,
                        PrefsKeyValues[i][1],
                        lstrlen(PrefsKeyValues[i][1]));
}

PreferencesInfo PreferencesDialog::prefsInfo()
{
	return info;
}

void PreferencesDialog::setCheckBox(int id, bool value)
{
	CButton* checkBox = (CButton*)GetDlgItem(id);
	if (checkBox)
		checkBox->SetCheck(value ? 1 : 0);
}

bool PreferencesDialog::getCheckBox(int id)
{
	CButton* checkBox = (CButton*)GetDlgItem(id);
	if (checkBox)
		return checkBox->GetCheck() == 1;
	else
		return 0;
}

void PreferencesDialog::setText(int id, char* value)
{
	CEdit* edit = (CEdit*)GetDlgItem(id);
	if (edit)
		edit->SetWindowText(value);
}

CString PreferencesDialog::getText(int id)
{
	CEdit* edit = (CEdit*)GetDlgItem(id);
	CString s;
	if (edit)
		edit->GetWindowText(s);
	return s;
}

