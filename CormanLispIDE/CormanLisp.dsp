# Microsoft Developer Studio Project File - Name="CormanLisp" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=CormanLisp - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "CormanLisp.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "CormanLisp.mak" CFG="CormanLisp - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "CormanLisp - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "CormanLisp - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName "CormanLisp"
# PROP Scc_LocalPath ".."
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "CormanLisp - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\Release"
# PROP BASE Intermediate_Dir ".\Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 2
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ".\bin"
# PROP Intermediate_Dir ".\bin"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MD /W3 /GX /Zi /Od /I ".\include" /I "../include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" /YX"stdafx.h" /FD /c
# SUBTRACT CPP /Fr
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 ..\CormanLispServer\bin\CormanLispServer.lib ole32.lib /nologo /subsystem:windows /pdb:"..\CormanLisp.pdb" /debug /machine:I386 /out:"..\CormanLisp.exe" /pdbtype:sept
# SUBTRACT LINK32 /pdb:none

!ELSEIF  "$(CFG)" == "CormanLisp - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ".\Debug"
# PROP BASE Intermediate_Dir ".\Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 2
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\bin\"
# PROP Intermediate_Dir ".\bin\"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MD /W3 /GX /Zi /I ".\include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" /YX"stdafx.h" /FD /I../include /c
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /i ".\include" /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386
# ADD LINK32 ..\CormanLispServer\bin\CormanLispServer.lib ole32.lib /nologo /subsystem:windows /incremental:no /pdb:"..\CormanLisp.pdb" /debug /machine:I386 /out:"..\CormanLisp.exe"
# SUBTRACT LINK32 /pdb:none /nodefaultlib

!ENDIF 

# Begin Target

# Name "CormanLisp - Win32 Release"
# Name "CormanLisp - Win32 Debug"
# Begin Group "src"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;hpj;bat;for;f90"
# Begin Source File

SOURCE=.\src\browser.cpp
# End Source File
# Begin Source File

SOURCE=.\src\CharBuf.cpp
# End Source File
# Begin Source File

SOURCE=.\src\CoCormanLispClient.cpp
# End Source File
# Begin Source File

SOURCE=.\src\CommonLispFuncs.cpp
# End Source File
# Begin Source File

SOURCE=.\src\CormanLisp.cpp
# End Source File
# Begin Source File

SOURCE=.\CormanLisp.rc
# End Source File
# Begin Source File

SOURCE=.\src\CSDialog.cpp
# End Source File
# Begin Source File

SOURCE=.\src\DocOptions.cpp
# End Source File
# Begin Source File

SOURCE=.\src\EncryptDes.cpp
# End Source File
# Begin Source File

SOURCE=.\src\LispObjDisplay.cpp
# End Source File
# Begin Source File

SOURCE=.\src\PageSetupDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\src\PreferencesDialog.cpp
# End Source File
# Begin Source File

SOURCE=.\src\RegistrationDialog.cpp
# End Source File
# Begin Source File

SOURCE=.\src\Stdafx.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ThreadClasses.cpp
# End Source File
# Begin Source File

SOURCE=.\src\webbrowser.cpp
# End Source File
# End Group
# Begin Group "include"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=.\include\browser.h
# End Source File
# Begin Source File

SOURCE=..\include\CoCormanLispClient.h
# End Source File
# Begin Source File

SOURCE=.\include\CommonLispFuncs.h
# End Source File
# Begin Source File

SOURCE=.\include\CormanLisp.h
# End Source File
# Begin Source File

SOURCE=.\include\CSDialog.h
# End Source File
# Begin Source File

SOURCE=.\include\DocOptions.h
# End Source File
# Begin Source File

SOURCE=.\include\EncryptDES.h
# End Source File
# Begin Source File

SOURCE=..\include\ErrorMessage.h
# End Source File
# Begin Source File

SOURCE=.\include\helpids.h
# End Source File
# Begin Source File

SOURCE=..\include\ICormanLisp.h
# End Source File
# Begin Source File

SOURCE=.\include\LispObjDisplay.h
# End Source File
# Begin Source File

SOURCE=.\include\PageSetupDlg.h
# End Source File
# Begin Source File

SOURCE=.\include\PreferencesDialog.h
# End Source File
# Begin Source File

SOURCE=.\include\RegistrationDialog.h
# End Source File
# Begin Source File

SOURCE=.\include\STDAFX.H
# End Source File
# Begin Source File

SOURCE=..\include\threadclasses.h
# End Source File
# Begin Source File

SOURCE=.\include\UtilFuncs.h
# End Source File
# Begin Source File

SOURCE=.\include\webbrowser.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\RES\background1.bmp
# End Source File
# Begin Source File

SOURCE=.\RES\Browserfram.bmp
# End Source File
# Begin Source File

SOURCE=.\RES\BrowserToolbar.bmp
# End Source File
# Begin Source File

SOURCE=.\RES\CormanLisp.bmp
# End Source File
# Begin Source File

SOURCE=.\RES\CormanLisp.ico
# End Source File
# Begin Source File

SOURCE=.\RES\CormanLisp_small.BMP
# End Source File
# Begin Source File

SOURCE=.\RES\CormanLisp_smaller.bmp
# End Source File
# Begin Source File

SOURCE=.\Res\icon1.ico
# End Source File
# Begin Source File

SOURCE=.\RES\LispDoc.ico
# End Source File
# Begin Source File

SOURCE=.\RES\TOOLBAR.BMP
# End Source File
# End Group
# End Target
# End Project
