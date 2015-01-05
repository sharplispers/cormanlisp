//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		ErrorMessage.h
//		Contents:	ErrorMessage() inline definition.
//		History:	8/6/97  RGC  Created.
//
inline void ErrorMessage(LPCTSTR szFn, DWORD dwError = GetLastError())
{
    TCHAR szTitle[1024];
    TCHAR szPrompt[1024];
    BOOL bRet = FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM,
                              0, dwError,
                              0,
                              szPrompt,
                              sizeof(szPrompt),
                              0);
    if (!bRet)
        lstrcpy(szPrompt, __TEXT("Unknown Error"));
    wsprintf(szTitle, __TEXT("%s failed with error code (0x%x)! Do you want to exit?"), szFn, dwError);
    int id = MessageBox(HWND_DESKTOP, szPrompt, szTitle, MB_YESNO|MB_SETFOREGROUND);
    if (id != IDNO)
        ExitProcess(0);
}
