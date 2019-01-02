@echo off
rem Repack the installer contents into a ZIP archive.

mkdir .\unpacked
msiexec /a CormanLispInstaller.msi /qn TARGETDIR="%~dp0unpacked"
del CormanLispBinaries.zip
powershell Compress-Archive -Path \"%~dp0unpacked\Corman Lisp\" -DestinationPath \"%~dp0CormanLispBinaries.zip\"
rd /s /q .\unpacked
