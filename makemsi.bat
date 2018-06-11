@echo off
rem configuration Variables
set InstallerName=CormanLispInstaller.msi

set GeneratedWXSFiles=.\installer\Sys.wxs .\installer\Libraries.wxs .\installer\Modules.wxs .\installer\Documentation.wxs .\installer\Headers.wxs .\installer\Examples.wxs .\installer\HyperSpec.wxs
set InstallerFiles= %GeneratedWXSFiles% .\installer\CormanLispServerLib.wxs .\installer\HyperSpecLegalese.wxs .\installer\Features.wxs .\installer\DebugData.wxs .\installer\AddToPath.wxs .\installer\Shortcuts.wxs .\installer\Core.wxs .\installer\Main.wxs
set InstallerObjectFiles= .\installer\Sys.wixobj .\installer\Libraries.wixobj .\installer\Modules.wixobj .\installer\Documentation.wixobj .\installer\Headers.wixobj .\installer\CormanLispServerLib.wixobj .\installer\Examples.wixobj .\installer\HyperSpec.wixobj .\installer\HyperSpecLegalese.wixobj .\installer\Features.wixobj  .\installer\DebugData.wixobj .\installer\AddToPath.wixobj .\installer\Shortcuts.wixobj .\installer\Core.wixobj .\installer\Main.wixobj

set HeatOptions=-gg -scom -sreg -sfrag -ke -dr INSTALLDIR  -t .\installer\include.xsl
set WixExtensions=-ext WixUIExtension -ext WixUtilExtension

rem set old current directory
set OldDir=%cd%
cd "%~dp0\"

rem Cleanup
del %InstallerName%
del %GeneratedWXSFiles%
del %InstallerObjectFiles%

rem convert TXT license to RTF
clconsole.exe -execute Utilities\license-to-rtf.lisp

rem Unpack HyperSpec
rd /s /q HyperSpec
del HyperSpec-Legalese.text
del HyperSpec-README.text
clconsole.exe -execute Utilities\install-hyperspec.lisp

rem Generate file with build version information
del installer\Version.wxi
clconsole.exe -execute Utilities\generate-version-wxi.lisp

rem Generate include fragments.
rem These should be synchronised with values in 'Config.wxi'

rem Corman Lisp core
"%WIX%\bin\heat" dir .\Sys -cg SysDir -var var.SysDirectoryPath -out .\installer\Sys.wxs %HeatOptions%

"%WIX%\bin\heat" dir .\Libraries -cg LibrariesDir -var var.LibrariesDirectoryPath -out .\installer\Libraries.wxs %HeatOptions%

"%WIX%\bin\heat" dir .\Modules -cg ModulesDir -var var.ModulesDirectoryPath -out .\installer\Modules.wxs %HeatOptions%

"%WIX%\bin\heat" dir .\documentation -cg DocDir -var var.DocumentationDirectoryPath -out .\installer\Documentation.wxs %HeatOptions%

rem Corman Lisp extra
"%WIX%\bin\heat" dir .\include -cg HeadersDir -var var.HeadersDirectoryPath -out .\installer\Headers.wxs %HeatOptions%

"%WIX%\bin\heat" dir .\examples -cg ExamplesDir -var var.ExamplesDirectoryPath -out .\installer\Examples.wxs %HeatOptions%

"%WIX%\bin\heat" dir .\HyperSpec -cg HyperSpecDir -var var.HyperSpecDirectoryPath -out .\installer\HyperSpec.wxs %HeatOptions%

rem build the installer
"%WIX%\bin\candle" %WiXExtensions% -out .\installer\ %InstallerFiles%
"%WIX%\bin\light" %WiXExtensions% -sw1076 -dcl:high -cultures:en-US %InstallerObjectFiles% -out %InstallerName%

rem change current directory
cd "%OldDir%"
