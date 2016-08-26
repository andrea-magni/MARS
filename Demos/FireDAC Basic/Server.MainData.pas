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
    FDConnection1: TFDConnection;
    employee: TFDQuery;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
  private
  public
//    [GET, Path('/standard')]
//    function StandardDataSet: TArray<TDataset>;

//    [GET, Path('/employee')]
//    function EmployeeDataSet: TDataSet;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  MARS.Core.Registry;

{ TMainDataResource }

//function TMainDataResource.EmployeeDataSet: TDataSet;
//begin
//  Result := Employee;
//end;
//
//function TMainDataResource.StandardDataSet: TArray<TDataset>;
//begin
//  Result := [employee];
//end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TMainDataResource>(
    function: TObject
    begin
      Result := TMainDataResource.Create(nil);
    end
  );

end.
