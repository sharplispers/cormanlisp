#
#	File:		Makefile
#	Contents:	Makefile for CormanLispServer component.
#	History:	4/21/00  RGC  Created.
#
default:	../CormanLispServer.dll   

../CormanLispServer.dll:
	echo Building CormanLispServer.dll...
	if not exist .\bin md .\bin 
	nmake /s /c clean
	cl.exe /nologo /MT /W3 /EHsc /c /Zi /Od	/FD /Zi	/GS-\
		/D "WIN32"								\
		/D "NDEBUG"                             \
		/D "_WINDOWS"							\
		/D "_CormanLispServer"					\
		/D "_WINDLL"							\
		/Fo"./bin/"								\
		/Fp"./bin/CormanLispServer.pch"			\
		/I./include /I../include				\
		src\*.cpp
 
	rc.exe /l 0x409 /fo"bin\CormanLispServer.res" /i "res" .\res\CormanLispServer.rc

	link.exe /nologo /dll /nodefaultlib 	\
		/subsystem:windows						\
		/incremental:no							\
    		/machine:I386							\
		/def:".\CormanLispServer.def"			\
		/out:"../CormanLispServer.dll"			\
		/implib:"bin\CormanLispServer.lib"		\
		/safeseh:no					            \
		/manifest:no                            \
		bin\*.obj								\
		bin\CormanLispServer.res				\
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
		uuid.lib								\
		odbc32.lib								\
		odbccp32.lib
	
	lib /nologo /out:.\bin\CormanLispStatic.lib .\bin\*.obj
#	mt /outputresource:"../CormanLispServer.dll;#1" /manifest ..\CormanLispServer.dll.manifest

clean:
	if exist bin\*.obj del bin\*.obj
	if exist bin\*.pdb del bin\*.pdb
	if exist bin\*.res del bin\*.res
	if exist bin\*.lib del bin\*.lib
	if exist bin\*.exp del bin\*.exp
	if exist bin\*.pch del bin\*.pch
	if exist CormanLispServer.ncb del CormanLispServer.ncb
	if exist CormanLispServer.opt del CormanLispServer.opt
	if exist CormanLispServer.dep del CormanLispServer.dep
	if exist ..\CormanLispServer.dll del ..\CormanLispServer.dll

 

