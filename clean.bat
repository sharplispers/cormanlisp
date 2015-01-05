echo off
echo Cleaning out all built files...
cd zlib
nmake /s /c clean
cd ..\CormanLispServer
nmake /s /c clean
cd ..\CormanLispIDE
nmake /s /c clean
cd ..\clboot
nmake /s /c clean
cd ..\clbootapp
nmake /s /c clean
cd ..\clconsole
nmake /s /c clean
cd ..\clconsoleapp
nmake /s /c clean
cd ..\dlltemplate
nmake /s /c clean
cd ..\license
nmake /s /c clean
cd ..
if exist *.pdb del *.pdb
if exist CormanLisp.exe del CormanLisp.exe
if exist clboot.exe del clboot.exe
if exist clbootapp.exe del clbootapp.exe
if exist clconsole.exe del clconsole.exe
if exist clconsoleapp.exe del clconsoleapp.exe
if exist CormanLispSources.zip del CormanLispSources.zip
if exist *.lib del *.lib
if exist *.exp del *.exp
if exist *.img del *.img
if exist CormanLispServer.dll del CormanLispServer.dll
if exist license.dll del license.dll
if exist dlltemplate.dll del dlltemplate.dll
if exist "Lisp Worksheet" del "Lisp Worksheet"

