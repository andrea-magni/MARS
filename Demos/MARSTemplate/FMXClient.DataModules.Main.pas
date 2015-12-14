(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit FMXClient.DataModules.Main;

interface

uses
  System.SysUtils, System.Classes, MARS.Client.Application,
  MARS.Client.Client
  ;

type
  TMainDataModule = class(TDataModule)
    MARSClient: TMARSClient;
    MARSApplication: TMARSClientApplication;
  private
  public
  end;

var
  MainDataModule: TMainDataModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
