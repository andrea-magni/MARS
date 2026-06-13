@echo off
REM Build the static VitePress site into .vitepress\dist
REM (also validates internal links, since ignoreDeadLinks is false).

REM Make sure Node.js is on PATH for this session:
set "PATH=C:\Sviluppo\nodejs;%PATH%"

REM Always work from the folder this script lives in (the docs root):
cd /d "%~dp0"

call npm run docs:build
echo.
echo Build finished. Output is in .vitepress\dist
echo Run "npm run docs:preview" (or preview the dist folder) to serve it locally.
