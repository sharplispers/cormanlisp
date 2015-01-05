#
#	File:		Makefile
#	Contents:	Root Makefile for Corman Lisp.
#	History:	4/22/00  RGC  Created.
#

SOURCEDIR=c:\roger\cormanlisp\Sources

default: all

all:						\
	zlib/zlib.lib			\
	CormanLispServer.dll	\
	license.dll				\
	CormanLisp.exe			\
	clboot.exe				\
	clbootapp.exe			\
	clconsole.exe			\
	clconsoleapp.exe		\
	dlltemplate.dll			\
	CormanLisp.img

zlib/zlib.lib:
	cd zlib
	nmake /s /c
	cd ..

license.dll:
	cd license
	nmake /s /c
	cd ..

CormanLispServer.dll:
	cd CormanLispServer
	nmake /s /c
	cd ..

CormanLisp.exe:	license.dll
	cd CormanLispIDE
	nmake /s /c
	cd ..

clboot.exe:
	cd CLBoot
	nmake /s /c
	cd ..

clbootapp.exe:
	cd CLBootApp
	nmake /s /c
	cd ..

clconsole.exe:
	cd CLConsole
	nmake /s /c
	cd ..

clconsoleapp.exe:
	cd CLConsoleApp
	nmake /s /c
	cd ..

dlltemplate.dll:
	cd dlltemplate
	nmake /s /c
	cd ..

CormanLisp.img:
	echo Building CormanLisp.img...
	clconsole -image "" -execute sys/compile-sys.lisp

clean:
	echo Removing all built files...
	if exist *.pdb del *.pdb
	if exist CormanLisp.exe del CormanLisp.exe
	if exist clboot.exe del clboot.exe
	if exist clbootapp.exe del clbootapp.exe
	if exist clconsole.exe del clconsole.exe
	if exist clconsoleapp.exe del clconsoleapp.exe
	if exist *.lib del *.lib
	if exist *.exp del *.exp
	if exist *.img del *.img
	if exist *.dll del *.dll
	if exist "Lisp Worksheet" del "Lisp Worksheet"
	if exist Release rmdir /s /q Release
	if exist Sources rmdir /s /q Sources

sources: sourcetree

sourcetree:
	copysources

release: cleanbuild copyrelease

cleanbuild:
	clean
	make all

copyrelease:
	copyrelease c:\roger\cormanlisp\Release
