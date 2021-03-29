(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.MainData;

interface

uses
  System.SysUtils, System.Classes,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, Data.DB, FireDAC.Comp.Client, FireDAC.Phys.FB,
  FireDAC.Phys.FBDef, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.VCLUI.Wait, FireDAC.Comp.UI
  , MARS.Data.FireDAC.DataModule
  , MARS.Core.Attributes
  , MARS.Core.URL
  , MARS.Core.Token
  ;

type
  [Path('/maindata')]
  TMainDataResource = class(TMARSFDDataModuleResource)
    employee: TFDQuery;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
  private
  public
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  MARS.Core.Registry;

{ TMainDataResource }

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TMainDataResource>;

end.
