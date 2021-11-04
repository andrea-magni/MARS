@echo off

REM Run this from the "VS x64 Native Tools Command Prompt"

cl /c /GS- /TC /W3 /Zc:wchar_t /I"src" /Gm- /O2 /Ob2 /Zc:inline /fp:precise /D "WIN32" /D "_WINDOWS" /D "NDEBUG" /D "HAVE_CONFIG_H" /D "YAML_DECLARE_STATIC" /D "_CRT_SECURE_NO_WARNINGS" /D "_NO_CRT_STDIO_INLINE" /D "_MBCS" /WX- /Zc:forScope /Gd /MT "src/api.c" "src/dumper.c" "src/emitter.c" "src/loader.c" "src/parser.c" "src/reader.c" "src/scanner.c" "src/writer.c"

copy *.obj ..\Obj\Win64
del *.obj