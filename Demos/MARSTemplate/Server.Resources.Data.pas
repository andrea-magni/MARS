(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources.Data;

interface

uses
  System.SysUtils, System.Classes
  , Data.DB

  , FireDAC.Stan.Intf, FireDAC.Stan.Option
  , FireDAC.Stan.Error, FireDAC.UI.Intf
  , FireDAC.Phys.Intf, FireDAC.Stan.Def
  , FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys
  , FireDAC.Comp.Client
  , FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt
  , FireDAC.Comp.DataSet

  , MARS.Data.FireDAC.DataModule
  , MARS.Core.Attributes
  , MARS.Core.URL
  ;

type
  [Path('data')]
  TDataResource = class(TMARSFDDataModuleResource)
    DBConnection: TFDConnection;
  private
  public
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
    MARS.Core.Registry
  ;


initialization
  TMARSResourceRegistry.Instance.RegisterResource<TDataResource>(
    function:TObject
    begin
      Result := TDataResource.Create(nil);
    end
  );

end.
