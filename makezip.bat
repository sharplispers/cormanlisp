@echo off
rem 

mkdir .\unpacked
msiexec /a CormanLispInstaller.msi /qb TARGETDIR="%~dp0unpacked"
del CormanLispBinaries.zip
powershell Compress-Archive -Path \"%~dp0unpacked\Corman Lisp\" -DestinationPath \"%~dp0CormanLispBinaries.zip\"
rd /s /q .\unpacked
