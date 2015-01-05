@SET VSINSTALLDIR=C:\msdev8
@SET VCINSTALLDIR=C:\msdev8\VC
@SET FrameworkDir=C:\WINDOWS\Microsoft.NET\Framework
@SET FrameworkVersion=v2.0.50727
@SET FrameworkSDKDir=C:\msdev8\SDK\v2.0
@if "%VSINSTALLDIR%"=="" goto error_no_VSINSTALLDIR
@if "%VCINSTALLDIR%"=="" goto error_no_VCINSTALLDIR

@echo Setting environment for using Microsoft Visual Studio 2005 x86 tools.

@rem
@rem Root of Visual Studio IDE installed files.
@rem
@set DevEnvDir=C:\msdev8\Common7\IDE

@set PATH=C:\msdev8\Common7\IDE;C:\msdev8\VC\BIN;C:\msdev8\Common7\Tools;C:\msdev8\Common7\Tools\bin;C:\msdev8\VC\PlatformSDK\bin;C:\msdev8\SDK\v2.0\bin;C:\WINDOWS\Microsoft.NET\Framework\v2.0.50727;C:\msdev8\VC\VCPackages;%PATH%
@set INCLUDE=C:\msdev8\VC\ATLMFC\INCLUDE;C:\msdev8\VC\INCLUDE;C:\msdev8\VC\PlatformSDK\include;C:\msdev8\SDK\v2.0\include;%INCLUDE%
@set LIB=C:\msdev8\VC\ATLMFC\LIB;C:\msdev8\VC\LIB;C:\msdev8\VC\PlatformSDK\lib;C:\msdev8\SDK\v2.0\lib;%LIB%
@set LIBPATH=C:\WINDOWS\Microsoft.NET\Framework\v2.0.50727;C:\msdev8\VC\ATLMFC\LIB

@goto end

:error_no_VSINSTALLDIR
@echo ERROR: VSINSTALLDIR variable is not set. 
@goto end

:error_no_VCINSTALLDIR
@echo ERROR: VCINSTALLDIR variable is not set. 
@goto end

:end
