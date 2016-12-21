#
#	File:		Makefile
#	Contents:	Makefile for Corman Lisp CLConsoleApp component.
#	History:	4/21/00  RGC  Created.
#
default: ../clconsoleapp.exe

../clconsoleapp.exe:
	echo Building clconsoleapp.exe...
	if not exist .\bin md .\bin 
	nmake /s /c clean
	cl.exe /nologo /MT /W3 /EHsc /O2 /c /Zi		\
		/D "WIN32"								\
		/D "NDEBUG"								\
		/D "_CONSOLE"							\
		/D "_MBCS"								\
		/Fo"./bin/"								\
		/Fd"./bin/"								\
		/I./include /I../include				\
		clconsoleapp.cpp
	rc.exe /l 0x409 /fo"bin\clconsoleapp.res" /i "res" /d "NDEBUG" /d "_AFXDLL" .\clconsoleapp.rc
	link.exe /nologo /nodefaultlib		\
		/subsystem:console						\
		/incremental:no							\
		/pdb:"../clconsoleapp.pdb"				\
		/machine:I386							\
		/out:"../clconsoleapp.exe"				\
		/manifest:no							\
		/nxcompat:no							\
		bin\clconsoleapp.obj					\
		..\CormanLispServer\bin\CormanLispStatic.lib\
		../zlib/zlib.lib						\
		kernel32.lib							\
		user32.lib								\
		gdi32.lib								\
		msvcrt.lib								\
		winspool.lib							\
		comdlg32.lib							\
		advapi32.lib							\
		shell32.lib								\
		ole32.lib								\
		oleaut32.lib							\
		uuid.lib							\
		vcruntime.lib							\
		ucrt.lib							\
		userenv.lib

	mt /outputresource:"../clconsoleapp.exe;#1" /manifest clconsoleapp.exe.manifest

clean:
	if exist bin\*.obj del bin\*.obj
	if exist bin\*.pdb del bin\*.pdb
	if exist clconsoleapp.opt del clconsoleapp.opt
	if exist clconsoleapp.plg del clconsoleapp.plg
	if exist clconsoleapp.ncb del clconsoleapp.ncb
	if exist ..\clconsoleapp.exe del ..\clconsoleapp.exe
 

