#
#	File:		Makefile
#	Contents:	Makefile for Corman Lisp CLBootApp component.
#	History:	4/21/00  RGC  Created.
#
default:  ../clbootapp.exe

../clbootapp.exe:
	echo Building clbootapp.exe...
	if not exist .\bin md .\bin 
	nmake /s /c clean
	cl.exe /nologo /MT /W3 /EHsc /O2 /c /Zi		\
		/D "WIN32"								\
		/D "NDEBUG"								\
		/D "_WINDOWS"							\
		/Fo"./bin/"								\
		/Fd"./bin/"								\
		/I./include /I../include				\
		clbootapp.cpp
	rc.exe /l 0x409 /fo"bin\clbootapp.res" /i "res" /d "NDEBUG" /d "_AFXDLL" .\clbootapp.rc
	link.exe /nologo /debug /nodefaultlib 		\
		/subsystem:windows						\
		/incremental:no							\
		/pdb:"../clbootapp.pdb"					\
		/machine:I386							\
		/out:"../clbootapp.exe"					\
		/manifest:no							\
		/nxcompat:no							\
		bin\clbootapp.obj						\
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

#	mt /outputresource:"../clbootapp.exe;#1" /manifest clbootapp.exe.manifest

clean:
	if exist bin\*.obj del bin\*.obj
	if exist bin\*.pdb del bin\*.pdb
	if exist ..\clbootapp.exe del ..\clbootapp.exe
