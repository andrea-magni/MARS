@echo off

REM Set this variable to the Delphi version you use. Examples:
REM - 17.0: Delphi 10.0 Seattle
REM - 18.0: Delphi 10.1 Berlin
REM - 19.0: Delphi 10.2 Tokyo
set DELPHI_VERSION=19.0

REM Name of generated static library
set LIB=obj\local\armeabi-v7a\libyaml_android.a

REM Retrieve the default Android NDK from registry.
REM The REG QUERY command returns 3 lines of text like this (first line is empty):
REM
REM REG QUERY "HKEY_CURRENT_USER\SOFTWARE\Embarcadero\BDS\18.0\PlatformSDKs
REM     Default_Android    REG_SZ    AndroidSDKAndTools-XE9.sdk

REM The FOR statement skips the first 2 lines and returns the 3rd token from the next (3rd) line.
REM (the 2^>nul redirects any errors (2=STDERR) to nul to ignore these)
set DEFAULT_ANDROID=
for /F "skip=2 tokens=3" %%A in ('reg query "HKEY_CURRENT_USER\SOFTWARE\Embarcadero\BDS\%DELPHI_VERSION%\PlatformSDKs" /v "Default_Android" 2^>nul') do (
  set DEFAULT_ANDROID=%%A
)
if not defined DEFAULT_ANDROID (
  echo Cannot find location of Default Android NDK in registry
  exit /b
)

REM Now use this value to find the location of ndk-build (again using the registry)
set NDK_BUILD=
for /F "skip=2 tokens=3" %%A in ('reg query "HKEY_CURRENT_USER\SOFTWARE\Embarcadero\BDS\%DELPHI_VERSION%\PlatformSDKs\%DEFAULT_ANDROID%" /v "NDKBasePath" 2^>nul') do (
  set NDK_BUILD=%%A
)

set NDK_BUILD=%NDK_BUILD%\ndk-build

if not exist %NDK_BUILD% (
  echo Cannot find ndk-build. Should be installed in: %NDK_BUILD%
  exit /b
)

REM Do the same to locate strip tool
set NDK_STRIP=
for /F "skip=2 tokens=3" %%A in ('reg query "HKEY_CURRENT_USER\SOFTWARE\Embarcadero\BDS\%DELPHI_VERSION%\PlatformSDKs\%DEFAULT_ANDROID%" /v "NDKArmLinuxAndroidStripFile" 2^>nul') do (
  set NDK_STRIP=%%A
)

if not exist %NDK_STRIP% (
  echo Cannot find ndk-strip. Should be installed in: %NDK_STRIP%
  exit /b
)

REM Run ndk-build to build static library
call %NDK_BUILD%

if not exist %LIB% (
  echo Cannot find static library %LIB%
  exit /b
)

REM Remove debug symbols
%NDK_STRIP% -g -X %LIB%

REM Copy static library to directory with Delphi source code
copy %LIB% ..\
if %ERRORLEVEL% NEQ 0 (
  echo Cannot copy static library. Make sure it is not write protected
)

REM Remove temprary files
rd obj /s /q