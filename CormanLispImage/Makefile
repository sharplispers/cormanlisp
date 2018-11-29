#
#	File:		Makefile
#	Contents:	Makefile for CormanLisp.img
#	History:	11.06.2018  Artem Boldarev Created.
#

default: ..\CormanLisp.img

# we depend on CormanLispServer.dll because it is good idea to rebuild image after changes in the Lisp kernel
..\CormanLisp.img: ..\CormanLispServer.dll ../Sys/*.lisp ../Sys/scmindent/*.lisp
	..\makeimg.bat

clean:
	if exist ..\CormanLisp.img del ..\CormanLisp.img

rebuild: clean default

