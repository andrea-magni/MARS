program RoutesDemoServerDaemon;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Classes,
  SysUtils,
  {$IFDEF LINUX}
  MARS.Linux.Daemon in '..\..\Source\MARS.Linux.Daemon.pas',
  {$ENDIF }
  Server.Ignition in 'Server.Ignition.pas',
  Server.Resources.OpenAPI in 'Server.Resources.OpenAPI.pas',
  Server.Resources.Token in 'Server.Resources.Token.pas',
  Server.Routes in 'Server.Routes.pas',
  Server.Resources.HelloWorld in 'Server.Resources.HelloWorld.pas';

begin
  {$IFDEF LINUX}
  TMARSDaemon.Current.Name := 'RoutesDemoServerDaemon';
  // detaches from the terminal (fork) and logs to <executable>.log; with --foreground (or -f)
  // it stays in the current process and logs to stdout: systemd Type=simple, Docker
  TMARSDaemon.Current.Start;
  {$ELSE}
  WriteLn('Warning: This is for LINUX platform only.');
  WriteLn('Current platform: '+ TOSVersion.ToString);
  ReadLn;
  {$ENDIF}
end.
