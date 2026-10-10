{$define MARS_FIREDAC} // To enable MARS FireDAC support
{$define MARS_FASTREPORT} // To enable MARS FastReport support
{.$define MARS_UNIDAC} // To enable MARS Devart UniDAC support
{.$define MARS_MYDAC} // To enable MARS Devart MyDAC support
{.$define MARS_IBDAC} // To enable MARS Devart IBDAC support
{.$define MARS_JSON_LEGACY} // To revert to Rest.JSON serializer/deserializer for JSON and TObject descendants
{$define MARS_WEBSTENCILS}

{$define MARS_ZLIB}
{$define MARS_ZIP}

// YAML support (MARS.YAML.ReadersAndWriters, YAMLName) needs Neslib.Yaml, which binds libyaml
// only on Windows (32/64 bit), Android, iOS and 32-bit macOS: no Linux, no 64-bit macOS
// (OSX64, OSXARM64). The list mirrors the platforms of ThirdParty\Neslib.Yaml\Neslib.LibYaml.pas.
// Without MARS_YAML, OpenAPI and the other resources are served as JSON only.
{$if Defined(WIN32) or Defined(WIN64) or Defined(ANDROID) or Defined(IOS) or Defined(MACOS32)}
  {$define MARS_YAML}
{$endif}

// MARS-Curiosity supports Delphi 10.4 Sydney and later
{$if CompilerVersion < 34}
  {$message fatal 'MARS-Curiosity requires Delphi 10.4 Sydney or later'}
{$endif}

// Delphi version symbols.
// MARS tests the ones from Delphi 11 on; the older *_UP symbols are always defined (10.4 is the
// minimum) and kept for application code that still tests them.
{$define DelphiXE_UP}
{$define DelphiXE2_UP}
{$define DelphiXE3_UP}
{$define DelphiXE4_UP}
{$define DelphiXE5_UP}
{$define DelphiXE6_UP}
{$define DelphiXE7_UP}
{$define DelphiXE8_UP}
{$define Delphi10Seattle_UP}
{$define Delphi10Berlin_UP}
{$define Delphi10Tokyo_UP}
{$define Delphi10Rio_UP}
{$define Delphi10Sydney_UP}

{$if CompilerVersion = 34}
  {$define Delphi10Sydney}
{$endif}

{$if CompilerVersion = 35}
  {$define Delphi11Alexandria}
{$endif}

{$if CompilerVersion = 36}
  {$define Delphi12Athens}
{$endif}

{$if CompilerVersion = 37}
  {$define Delphi13Florence}
{$endif}

{$if CompilerVersion >= 35}
  {$define Delphi11Alexandria_UP}
{$endif}

{$if CompilerVersion >= 36}
  {$define Delphi12Athens_UP}
{$endif}

{$if CompilerVersion >= 37}
  {$define Delphi13Florence_UP}
{$endif}

// WebBroker TCookie.HttpOnly and TCookie.SameSite, Indy TIdCookie.SameSite (added to Indy in
// April 2023): used where verified, Delphi 13 on. With older versions MARS writes those cookie
// attributes after the cookie path, which WebBroker and Indy copy as they are into Set-Cookie.
{$if CompilerVersion >= 37}
  {$define MARS_NATIVE_COOKIE_ATTRIBUTES}
{$endif}
