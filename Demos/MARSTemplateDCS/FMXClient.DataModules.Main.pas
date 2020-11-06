(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit FMXClient.DataModules.Main;

interface

uses
  System.SysUtils, System.Classes, MARS.Client.Application,
  MARS.Client.Client, MARS.Client.Client.Net
;

type
  TMainDataModule = class(TDataModule)
    MARSApplication: TMARSClientApplication;
    MARSClient: TMARSNetClient;
  private
  public
  end;

var
  MainDataModule: TMainDataModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
