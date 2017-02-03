#
#	File:		Makefile
#	Contents:	Makefile for Corman Lisp DLLTemplate component.
#	History:	5/13/02  RGC  Created.
#
default: ../dlltemplate.dll

../dlltemplate.dll:
	echo Building dlltemplate.dll...
	if not exist .\bin md .\bin 
	nmake /s /c clean
	cl.exe /nologo /MT /W3 /EHsc /O2 /c /Zi /GS-\
		/D "WIN32"								\
		/D "NDEBUG"								\
		/D "_CONSOLE"							\
		/D "_MBCS"								\
		/Fo"bin\\"								\
		/Fd"bin\\"								\
		/I./include /I../include				\
		dlltemplate.cpp
 
	link.exe /nologo /dll /debug /nodefaultlib	\
		/subsystem:windows						\
		/incremental:no							\
		/pdb:"../dlltemplate.pdb"				\
		/machine:I386							\
		/def:".\dlltemplate.def"				\
		/out:"../dlltemplate.dll"				\
		/implib:"bin\dlltemplate.lib"			\
		/manifest:no							\
		/nxcompat:no							\
		bin\dlltemplate.obj						\
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

#	mt /outputresource:"../dlltemplate.dll;#1" /manifest ..\dlltemplate.dll.manifest

clean:
	if exist bin\*.obj del bin\*.obj
	if exist bin\*.pdb del bin\*.pdb
	if exist bin\*.idb del bin\*.idb
	if exist dlltemplate.ncb del dlltemplate.ncb
	if exist ..\dlltemplate.dll del ..\dlltemplate.dll
 

