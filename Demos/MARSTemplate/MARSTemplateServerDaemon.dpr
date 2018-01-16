program MARSTemplateServerDaemon;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Classes,
  SysUtils,
  MARS.Linux.Daemon in '..\..\Source\MARS.Linux.Daemon.pas',
  Server.Ignition in 'Server.Ignition.pas',
  Server.WebModule in 'Server.WebModule.pas' {ServerWebModule: TWebModule};

begin
  TMARSDaemon.Current.Name := 'MARSTemplateServerDaemon';
  TMARSDaemon.Current.Start;
end.
