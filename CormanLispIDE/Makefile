#
#	File:		Makefile
#	Contents:	Makefile for Corman Lisp IDE component.
#	History:	4/21/00  RGC  Created.
#
default: ../CormanLisp.exe


../CormanLisp.exe:
	echo Building CormanLisp.exe...
	if not exist .\bin md .\bin 
	nmake /s /c clean

	cl.exe /nologo /MD /W3 /EHsc /Od /c /Zi	/FD	\
		/D "WIN32"								\
		/D "NDEBUG"								\
		/D "_WINDOWS"							\
		/D "_AFXDLL"							\
		/D "_MBCS"								\
		/Fo"bin\\"								\
		/Fd"bin\\"								\
		/Fp"$(INTDIR)\CormanLisp.pch"			\
		/I./include /I../include				\
		./src/*.cpp
 
 	rc.exe /l 0x409 /fo"bin\CormanLisp.res" /i "res" /d "NDEBUG" /d "_AFXDLL" .\CormanLisp.rc

	link.exe /nologo /debug					 	\
		/subsystem:windows						\
		/incremental:no							\
		/pdb:"../CormanLisp.pdb"				\
		/machine:I386							\
		/out:"../CormanLisp.exe"				\
		/manifest:no							\
		bin/*.obj								\
		bin/CormanLisp.res						\
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

	mt /outputresource:"..\CormanLisp.exe;#1" /manifest ..\CormanLisp.exe.manifest
#	del ..\CormanLisp.exe.manifest

clean:
	if exist bin\*.obj del bin\*.obj
	if exist bin\*.pdb del bin\*.pdb
	if exist bin\*.res del bin\*.res
	if exist CormanLisp.opt del CormanLisp.opt
	if exist CormanLisp.plg del CormanLisp.plg
	if exist CormanLisp.ncb del CormanLisp.ncb
 	if exist *.aps del *.aps
 	if exist ..\CormanLisp.exe del ..\CormanLisp.exe
# 	if exist ..\CormanLisp.exe.manifest del ..\CormanLisp.exe.manifest


