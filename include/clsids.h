//		-------------------------------
//		Copyright (c) Corman Technologies Inc.
//		See LICENSE.txt for license information.
//		-------------------------------
//
//		File:		clsids.h
//		Contents:	COM Class IDs for Corman Lisp
//		History:	8/5/97  RGC  Created.
//

#ifndef CLSIDS_H
#define CLSIDS_H

// {99AA27B3-0EAE-11d1-ACB9-00A024803258}
DEFINE_GUID(CLSID_CormanLisp, 
0x99aa27b3, 0xeae, 0x11d1, 0xac, 0xb9, 0x0, 0xa0, 0x24, 0x80, 0x32, 0x58);

// {0002DF01-0000-0000-C000-000000000046}
DEFINE_GUID(CLSID_Internet_Explorer, 
0x0002DF01, 0x0000, 0x0000, 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46);

//#ifndef CGID_IWebBrowser
// CGID_WebBrowser: {ED016940-BD5B-11cf-BA4E-00C04FD70816}
DEFINE_GUID(CGID_IWebBrowser,0xED016940L,0xBD5B,0x11cf,0xBA,0x4E,0x00,0xC0,0x4F,0xD7,0x08,0x16);
//#endif

#endif // CLSIDS_H
