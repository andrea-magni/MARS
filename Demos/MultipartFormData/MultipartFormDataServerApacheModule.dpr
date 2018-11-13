(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
library MultipartFormDataServerApacheModule;

uses
  {$IFDEF MSWINDOWS}
  Winapi.ActiveX, System.Win.ComObj,
  {$ENDIF }
  Web.WebBroker,
  Web.ApacheApp,
  Web.HTTPD24Impl,
  Server.WebModule in 'Server.WebModule.pas' {ServerWebModule: TWebModule},
  Server.Ignition in 'Server.Ignition.pas',
  Server.Resources in 'Server.Resources.pas';

{$R *.res}

// httpd.conf entries:
//
(*
 LoadModule marstemplate_module modules/mod_marstemplate.dll

 <Location /xyz>
    SetHandler mod_marstemplate-handler
 </Location>
*)
//
// These entries assume that the output directory for this project is the apache/modules directory.
//
// httpd.conf entries should be different if the project is changed in these ways:
//   1. The TApacheModuleData variable name is changed.
//   2. The project is renamed.
//   3. The output directory is not the apache/modules directory.
//   4. The dynamic library extension depends on a platform. Use .dll on Windows and .so on Linux.
//

// Declare exported variable so that Apache can access this module.
var
  GModuleData: TApacheModuleData;
exports
  GModuleData name 'marstemplate_module';

begin
{$IFDEF MSWINDOWS}
  CoInitFlags := COINIT_MULTITHREADED;
{$ENDIF}
  Web.ApacheApp.InitApplication(@GModuleData);
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  Application.Run;
end.
