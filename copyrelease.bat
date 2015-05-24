rem Create Release directory with all files which go into the
rem public release.
rem
echo off
echo Copying deployment files to Release
rmdir /s /q %1
mkdir %1
copy CormanLisp.exe %1
copy CormanLisp.exe.manifest %1
copy CormanLisp.img %1
copy clboot.exe %1
copy clbootapp.exe %1
copy clconsole.exe %1
copy clconsoleapp.exe %1
copy CormanLispServer.dll %1
copy dlltemplate.dll %1
copy license.dll %1
copy init.lisp %1
copy makeall.bat %1
rem copy *.ism %1
copy HyperSpec-6-0.tar.gz %1
mkdir %1\clboot
copy clboot\clboot.cpp %1\clboot
copy clboot\clboot.vcproj %1\clboot
copy clboot\Makefile %1\clboot
mkdir %1\clbootapp
copy clbootapp\clbootapp.cpp %1\clbootapp
copy clbootapp\clbootapp.vcproj %1\clbootapp
copy clbootapp\Makefile %1\clbootapp
mkdir %1\clconsole
copy clconsole\clconsole.cpp %1\clconsole
copy clconsole\clconsole.vcproj %1\clconsole
copy clconsole\Makefile %1\clconsole
mkdir %1\clconsoleapp
copy clconsoleapp\clconsoleapp.cpp %1\clconsoleapp
copy clconsoleapp\clconsoleapp.vcproj %1\clconsoleapp
copy clconsoleapp\Makefile %1\clconsoleapp
mkdir %1\CormanLispServer
copy CormanLispServer\CormanLispServer.def %1\CormanLispServer
copy CormanLispServer\CormanLispServer.vcproj %1\CormanLispServer
copy CormanLispServer\Makefile %1\CormanLispServer
copy CormanLispServer\makestaticlib.bat %1\CormanLispServer
mkdir %1\CormanLispServer\include
copy CormanLispServer\include\*.* %1\CormanLispServer\include
mkdir %1\CormanLispServer\res
copy CormanLispServer\res\*.* %1\CormanLispServer\res
mkdir %1\CormanLispServer\src
copy CormanLispServer\src\*.* %1\CormanLispServer\src
mkdir %1\documentation
copy documentation\*.txt %1\documentation
copy documentation\CormanLisp.pdf %1\documentation
copy documentation\parse.html %1\documentation
mkdir %1\utilities
xcopy /s utilities\*.* %1\utilities
mkdir %1\examples
xcopy /s examples\*.* %1\examples
mkdir %1\examples\gui
copy examples\gui\*.* %1\examples\gui
mkdir %1\include
copy include\*.h %1\include
mkdir %1\Libraries
xcopy /s Libraries\*.* %1\Libraries
mkdir %1\Modules
copy Modules\*.lisp %1\Modules
copy Modules\*.dll %1\Modules
copy Modules\*.txt %1\Modules
mkdir %1\sys
copy sys\*.lisp %1\sys
mkdir %1\test
copy test\*.lisp %1\test
mkdir %1\zlib
copy zlib\*.c %1\zlib
copy zlib\*.h %1\zlib
copy zlib\ChangeLog %1\zlib
copy zlib\Makefile %1\zlib
copy zlib\README %1\zlib
copy zlib\zlib.vcproj %1\zlib
rem mkdir %1\zlib\doc
rem copy zlib\doc\*.doc %1\zlib\doc
rem touch -t 200203151151 -recurse %1/*.*