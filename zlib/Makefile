#
#	File:		Makefile
#	Contents:	Makefile for zlib component of CormanLispServer.
#	History:	4/21/00  RGC  Created.
#				02/22/06  RGC  Updated for VS 2005.
#
default: zlib.lib

zlib.lib:
	echo Building zlib compression library...
	if not exist .\bin md .\bin 
	nmake /s /c clean
	cl.exe /nologo /MT /W3 /EHsc /O2 /c	/Ob1 /GF /FD /Gy /D_CRT_SECURE_NO_DEPRECATE\
		/D "WIN32"						\
		/D "NDEBUG"						\
		/D "_LIB"						\
		/Fo"./bin/"						\
		/Fd"./bin/"						\
		compress.c						\
		deflate.c						\
		gzio.c							\
		infback.c						\
		inffast.c						\
		inflate.c						\
		inftrees.c						\
		minigzip.c						\
		trees.c							\
		uncompr.c						\
		adler32.c						\
		crc32.c						    \
		zutil.c

	link.exe -lib /nologo				\
		/out:".\zlib.lib"				\
		bin\*.obj

clean:
	if exist bin\*.obj del bin\*.obj
	if exist zlib.plg del zlib.plg
	if exist zlib.ncb del zlib.ncb
	if exist zlib.opt del zlib.opt
	if exist zlib.lib del zlib.lib
	if exist zlib.dep del zlib.dep

 

