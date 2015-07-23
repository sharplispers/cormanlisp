#
#	File:		Makefile
#	Contents:	Makefile for License component.
#	History:	5/29/02  RGC  Created.
#
default:  ../license.dll

../license.dll:
	echo Building license.dll...
	if not exist .\bin md .\bin 
	nmake /s /c clean
	cl.exe /nologo /MT /W3 /EHsc /O2 /c /Zi		\
		/D "_WINDOWS"							\
		/D "_WINDLL"							\
		/Fo"./bin/"								\
		/Fd"./bin/"								\
		license.cpp EncryptDES.cpp
 
	link.exe /dll /nologo /nodefaultlib	\
		/subsystem:windows						\
		/incremental:no							\
		/machine:I386							\
		/out:"../license.dll"					\
		/IMPLIB:"../license.lib"				\
		/DEF:"license.def"						\
		/manifest:no                            \
		/nxcompat:no							\
		bin\license.obj							\
		bin\EncryptDES.obj						\
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

#	mt /outputresource:"../license.dll;#1" /manifest ..\license.dll.manifest

clean:
	if exist bin\*.obj del bin\*.obj
	if exist bin\*.pdb del bin\*.pdb
	if exist bin\*.lib del bin\*.lib
	if exist license.opt del license.opt
	if exist license.plg del license.plg
	if exist license.ncb del license.ncb
	if exist ..\license.dll del ..\license.dll
 

