cd zlib
nmake /s /c clean
nmake /s /c
cd ..\CormanLispServer
nmake /s /c clean
nmake /s /c
cd ..\clboot
nmake /s /c clean
nmake /s /c
cd ..\clbootapp
nmake /s /c clean
nmake /s /c
cd ..\clconsole
nmake /s /c clean
nmake /s /c
cd ..\clconsoleapp
nmake /s /c clean
nmake /s /c
cd ..
del cormanlisp.img
clconsole -execute sys/compile-sys.lisp -image ""
