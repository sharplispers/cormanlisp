#
#	File:		Makefile
#	Contents:	Makefile for CormanLispStatic.lib
#	History:	11.06.2018  Artem Boldarev Created.
#

default: ./bin/CormanLispStatic.lib

./bin/CormanLispStatic.lib: ../CormanLispServer.dll
	makestaticlib.bat

clean:
	delstaticlib.bat

rebuild: clean default

