@echo off
rem Builds MARSTemplateServerDCSDaemon for Linux64 (Release) into bin, ready for the Dockerfile of this folder.
rem It needs RAD Studio with the Linux64 platform and a Linux SDK; no PAServer, no Linux machine.
rem Then, where Docker runs: docker compose up -d --build
setlocal
if defined BDS goto bds_ok
for %%V in (37.0 23.0 22.0 21.0) do if not defined BDS if exist "%ProgramFiles(x86)%\Embarcadero\Studio\%%V\bin\rsvars.bat" call "%ProgramFiles(x86)%\Embarcadero\Studio\%%V\bin\rsvars.bat"
if defined BDS goto bds_ok
echo RAD Studio not found: run this script from the RAD Studio Command Prompt.
exit /b 1
:bds_ok
rem MARSDIR: the MARS folder the project refers to (the IDE knows it, the command line may not)
if defined MARSDIR goto build
for %%I in ("%BDS%") do set "BDSVERSION=%%~nxI"
for /f "tokens=2,*" %%A in ('reg query "HKCU\Software\Embarcadero\BDS\%BDSVERSION%\Environment Variables" /v MARSDIR 2^>nul ^| find "MARSDIR"') do set "MARSDIR=%%B"
:build
msbuild "%~dp0MARSTemplateServerDCSDaemon.dproj" /t:Build /p:Config=Release /p:Platform=Linux64 /v:minimal /nologo
if errorlevel 1 exit /b 1
echo.
echo Built bin\MARSTemplateServerDCSDaemon: now "docker compose up -d --build" where Docker runs.
