//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
#pragma once

typedef LRESULT (_stdcall *pfnAccessibleObjectFromWindow)(HWND hwnd, DWORD dwId, 
                                                    REFIID riid, void **ppvObject);
typedef LRESULT (_stdcall *pfnCreateStdAccessibleObject)(HWND hwnd, LONG idObject, 
                                                    REFIID riid, void** ppvObject);
typedef LRESULT (_stdcall *pfnLresultFromObject)(REFIID riid, WPARAM wParam, 
                                                    LPUNKNOWN punk);

class COleaccProxy
{
public:
    COleaccProxy(void);
    virtual ~COleaccProxy(void);

private:
    static HMODULE m_hModule;
    static BOOL m_bFailed;
    
public:
    static void Init(void);
    static pfnAccessibleObjectFromWindow m_pfnAccessibleObjectFromWindow;
    static pfnCreateStdAccessibleObject m_pfnCreateStdAccessibleObject;
    static pfnLresultFromObject m_pfnLresultFromObject;
};
