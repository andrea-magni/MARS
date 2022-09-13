program LibraryStoreServerDaemon;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Classes,
  SysUtils,
  {$IFDEF LINUX}
  MARS.Linux.Daemon in '..\..\Source\MARS.Linux.Daemon.pas',
  {$ENDIF }
  Server.Ignition in 'Server.Ignition.pas',
  MBW.PlainText in 'MBW.PlainText.pas',
  Model in 'Model.pas',
  Storage in 'Storage.pas';

begin
  {$IFDEF LINUX}
  TMARSDaemon.Current.Name := 'LibraryStoreServerDaemon';
  TMARSDaemon.Current.Start;
  {$ENDIF}
end.
