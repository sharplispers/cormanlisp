@echo off
rem Repack the installer contents into a ZIP archive.

mkdir .\unpacked
msiexec /a CormanLispInstaller.msi /qn TARGETDIR="%~dp0unpacked"
move ".\unpacked\System\*" ".\unpacked\Corman Lisp\"
del CormanLispBinaries.zip

rem rem Visual C++ Runtime DLL files and Universal CRT files Local Deployment
rem copy "%VS140COMNTOOLS%..\..\VC\redist\x86\Microsoft.VC140.CRT\*.dll" ".\unpacked\Corman Lisp\"
rem copy "%VS140COMNTOOLS%..\..\VC\redist\x86\Microsoft.VC140.MFC\*.dll" ".\unpacked\Corman Lisp\"
rem copy "%VS140COMNTOOLS%..\..\VC\redist\x86\Microsoft.VC140.MFCLOC\*.dll" ".\unpacked\Corman Lisp\"
rem rem Visual Studio 2015 only
rem if exist "%SystemRoot%\SysWOW64\" (
rem     copy "%SystemRoot%\SysWOW64\mfc140.dll" ".\unpacked\Corman Lisp\"
rem     copy "%SystemRoot%\SysWOW64\mfcm140.dll" ".\unpacked\Corman Lisp\"
rem ) else (
rem     copy "%SystemRoot%\System32\mfc140.dll" ".\unpacked\Corman Lisp\"
rem     copy "%SystemRoot%\System32\mfcm140.dll" ".\unpacked\Corman Lisp\"
rem )
rem rem Universal C Runtime
rem if exist "%ProgramFiles(x86)%\" (
rem     copy "%ProgramFiles(x86)%\Windows Kits\10\Redist\ucrt\DLLs\x86\*.dll" ".\unpacked\Corman Lisp\"
rem ) else (
rem     copy "%ProgramFiles%\Windows Kits\10\Redist\ucrt\DLLs\x86\*.dll" ".\unpacked\Corman Lisp\"
rem )

powershell Compress-Archive -Path \"%~dp0unpacked\Corman Lisp\" -DestinationPath \"%~dp0CormanLispBinaries.zip\"
rd /s /q .\unpacked
