program AuthDemoServerDaemon;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Classes,
  SysUtils,
  {$IFDEF LINUX}
  MARS.Linux.Daemon in '..\..\Source\MARS.Linux.Daemon.pas',
  {$ENDIF }
  Server.Ignition in 'Server.Ignition.pas',
  Model.UserData in 'Model.UserData.pas';

begin
  {$IFDEF LINUX}
  TMARSDaemon.Current.Name := 'AuthDemoServerDaemon';
  TMARSDaemon.Current.Start;
  {$ENDIF}
end.
