program MARSTemplateServerDCSDaemon;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Classes,
  SysUtils,
  {$IFDEF LINUX}
  MARS.Linux.Daemon,
  {$ENDIF }
  Server.Ignition in 'Server.Ignition.pas',
  Server.WebModule in 'Server.WebModule.pas' {ServerWebModule: TWebModule},
  Server.Resources.HelloWorld in 'Server.Resources.HelloWorld.pas',
  Server.Resources.OpenAPI in 'Server.Resources.OpenAPI.pas',
  Server.Resources.Token in 'Server.Resources.Token.pas';

begin
  {$IFDEF LINUX}
  TMARSDaemon.Current.Name := 'MARSTemplateServerDaemon';
  TMARSDaemon.Current.Start;
  {$ELSE}
  WriteLn('Warning: This is for LINUX platform only.');
  WriteLn('Current platform: '+ TOSVersion.ToString);
  ReadLn;
  {$ENDIF}
end.
