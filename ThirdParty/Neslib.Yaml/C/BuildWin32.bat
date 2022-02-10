@echo off

REM Run this from the "VS x86 Native Tools Command Prompt"

cl /c /GS- /TC /analyze- /W3 /Zc:wchar_t /I"src" /Gm- /O2 /Ob2 /Zc:inline /fp:precise /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "YAML_DECLARE_STATIC" /D "_CRT_SECURE_NO_WARNINGS" /D "_NO_CRT_STDIO_INLINE" /D "HAVE_CONFIG_H" /D "_MBCS" /WX- /Zc:forScope /Gd /Oy- /MT "src/api.c" "src/dumper.c" "src/emitter.c" "src/loader.c" "src/parser.c" "src/reader.c" "src/scanner.c" "src/writer.c"

copy *.obj ..\Obj\Win32
del *.obj