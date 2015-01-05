//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		PreferencesDialog.h
//		Contents:	Corman Lisp preferences dialog definitions
//		History:	3/24/99  RGC  Created.
//
#ifndef PREFERENCESDIALOG_H
#define PREFERENCESDIALOG_H

struct PreferencesInfo
{
	bool autoIndent;
	bool parenthesesMatching;
	bool autoPrototypeOnMouseMove;
	bool autoPrototypeOnKeyDown;
	bool appendLispOutputToEnd;
	int  tab;
	int  charSize;
	char fontName[128];
	bool charBold;
	bool charItalic;
	COLORREF textColor;
	COLORREF highlightTextColor;
	COLORREF outputTextColor;
	COLORREF hintBackgroundColor;
	bool replaceTabsWithSpaces;
	bool rememberOpenDocuments;
	bool rememberDocumentPositions;
	bool fullPathInTitle;
	bool autoColorize;
};

class PreferencesDialog : public CDialog
{
public:
	PreferencesDialog(UINT nIDTemplate);
	
	BOOL OnInitDialog();
	void OnChar(UINT nChar, UINT nRepCnt, UINT nFlags);
	void OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags);
	void OnOK();
	void OnCancel();
	void OnRestoreDefaults();
	void OnSelectFont();
	void OnChangeTextColor();
	void OnChangeHighlightTextColor();
	void OnChangeOutputTextColor();
	void OnChangeHintBackgroundColor();

	afx_msg HBRUSH OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor);
	afx_msg void OnDrawItem(int nIDCtl, LPDRAWITEMSTRUCT lpDrawItemStruct);

	PreferencesInfo prefsInfo();

	DECLARE_MESSAGE_MAP()
public:
	static void GetPreferencesInfo(PreferencesInfo*);
	static void SavePreferencesInfo(PreferencesInfo*);

private:
	void restoreDefaults();
	void setCheckBox(int id, bool value);
	bool getCheckBox(int id);
	void setText(int id, char* value);
	CString getText(int id);
	void updateDialog();

	PreferencesInfo info;

	CWnd* textColorWnd;
	CWnd* highlightTextColorWnd;
	CWnd* outputTextColorWnd;
	CWnd* hintBackgroundColorWnd;
};

#endif // PREFERENCESDIALOG_H