#
#	File:		Makefile
#	Contents:	Makefile for Corman Lisp CLBoot component.
#	History:	4/21/00  RGC  Created.
#
default: ../clboot.exe
 

../clboot.exe:
	echo Building clboot.exe...
	if not exist .\bin md .\bin 
	nmake /s /c clean
	cl.exe /nologo /MT /W3 /EHsc /O2 /c /Zi		\
		/D "WIN32"								\
		/D "NDEBUG"								\
		/D "_WINDOWS"							\
		/Fo"./bin/"								\
		/Fd"./bin/"								\
		/I./include /I../include				\
		clboot.cpp
	rc.exe /l 0x409 /fo"bin\clboot.res" /i "res" /d "NDEBUG" /d "_AFXDLL" .\clboot.rc
	link.exe /nologo /debug /nodefaultlib 		\
		/subsystem:windows						\
		/incremental:no							\
		/pdb:"../clboot.pdb"					\
		/machine:I386							\
		/out:"../clboot.exe"					\
		/manifest:no							\
		/nxcompat:no							\
		bin\clboot.obj							\
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
		ucrt.lib

#	mt /outputresource:"../clboot.exe;#1" /manifest clboot.exe.manifest

clean:
	if exist bin\*.obj del bin\*.obj
	if exist bin\*.pdb del bin\*.pdb
	if exist clboot.opt del clboot.opt
	if exist ..\clboot.exe del ..\clboot.exe
