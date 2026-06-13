@echo off
REM Launch the VitePress dev server (live preview at http://localhost:5173)
REM Run this file from anywhere; it switches to the docs folder automatically.

REM Make sure Node.js is on PATH for this session (node is installed here):
set "PATH=C:\Sviluppo\nodejs;%PATH%"

REM Always work from the folder this script lives in (the docs root):
cd /d "%~dp0"

call npm run docs:dev
