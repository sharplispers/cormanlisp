#
#	File:		Makefile
#	Contents:	Makefile for Corman Lisp CLConsole component.
#	History:	4/21/00  RGC  Created.
#
default:  ../clconsole.exe

../clconsole.exe:
	echo Building clconsole.exe...
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
		clconsole.cpp
 
	link.exe /nologo /nodefaultlib		        \
		/subsystem:console						\
		/incremental:no							\
		/machine:I386							\
		/out:"../clconsole.exe"					\
		/manifest:no							\
		/nxcompat:no							\
		bin\clconsole.obj						\
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
		uuid.lib

	mt /outputresource:"../clconsole.exe;#1" /manifest clconsole.exe.manifest

clean:
	if exist bin\*.obj del bin\*.obj
	if exist bin\*.pdb del bin\*.pdb
	if exist clconsole.opt del clconsole.opt
	if exist clconsole.plg del clconsole.plg
	if exist clconsole.ncb del clconsole.ncb
	if exist ..\clconsole.exe del ..\clconsole.exe
 

