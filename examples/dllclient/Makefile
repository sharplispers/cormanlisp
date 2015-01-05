#
#	File:		Makefile
#	Contents:	Makefile for DLLClient test application
#	History:	5/13/02  RGC  Created.
#
default: ../../dllclient.exe
 

../../dllclient.exe:	dllclient.cpp
	echo Building dllclient.exe...
	if not exist .\bin md .\bin 
	nmake /s /c clean
	cl.exe /nologo /MD /W1 /GX /O2 /c /Zi		\
		/D "WIN32"								\
		/D "NDEBUG"								\
		/D "_WINDOWS"							\
		/Fo"bin\\"								\
		/Fd"bin\\"								\
		/I./include /I../include				\
		dllclient.cpp
 
	link.exe /nologo /debug /nodefaultlib 		\
		/subsystem:console						\
		/incremental:no							\
		/pdb:"bin/dllclient.pdb"				\
		/machine:I386							\
		/out:"../../dllclient.exe"				\
		bin\dllclient.obj						\
		..\..\dllsample.lib						\
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

clean:
	if exist bin\*.obj del bin\*.obj
	if exist bin\*.pdb del bin\*.pdb
	if exist dllclient.opt del dllclient.opt
	if exist dllclient.ncb del dllclient.ncb
	if exist ..\..\dllclient.exe del ..\..\dllclient.exe

