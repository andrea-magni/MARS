(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit FMXClient.DataModules.Main;

interface

uses
  System.SysUtils, System.Classes
, MARS.Client.Application
, MARS.Client.Client, MARS.Client.Client.Net, MARS.Client.Log
;

type
  TMainDataModule = class(TDataModule)
    MARSApplication: TMARSClientApplication;
    MARSClient: TMARSNetClient;
    procedure MARSClientLog(Sender: TObject; const AEntry: TMARSClientLogEntry);
  private
  public
  end;

var
  MainDataModule: TMainDataModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

procedure TMainDataModule.MARSClientLog(Sender: TObject; const AEntry: TMARSClientLogEntry);
begin
  // Called after each request of MARSClient (also when it fails), in the thread of the call.
  // MARSClient.LogOptions decides what is logged (bodies up to 64 KB by default) and what is
  // masked (credentials by default). See https://andrea-magni.github.io/MARS/client/logging
{$IFDEF DEBUG}
  TMARSClientLog.ToDebugOutput(AEntry); // Delphi IDE: View > Debug Windows > Event Log
{$ENDIF}
end;

end.
