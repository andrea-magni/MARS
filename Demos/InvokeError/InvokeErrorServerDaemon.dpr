program InvokeErrorServerDaemon;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Classes,
  SysUtils,
  {$IFDEF LINUX}
  MARS.Linux.Daemon in '..\..\Source\MARS.Linux.Daemon.pas',
  {$ENDIF }
  Server.Ignition in 'Server.Ignition.pas';

begin
  {$IFDEF LINUX}
  TMARSDaemon.Current.Name := 'InvokeErrorServerDaemon';
  TMARSDaemon.Current.Start;
  {$ENDIF}
end.
