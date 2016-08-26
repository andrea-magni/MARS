(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources.Datamodule;

interface

uses
  System.SysUtils, System.Classes, MARS.Data.FireDAC.DataModule
  , MARS.Core.Attributes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB,
  FireDAC.Phys.FBDef, Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet,
  FireDAC.VCLUI.Wait
  ;

type
  [Path('/datamodule')]
  TDataResource = class(TMARSFDDataModuleResource)
    FDConnection1: TFDConnection;
    QueryItems: TFDQuery;
    QueryAccounts: TFDQuery;
    procedure QueryItemsBeforeOpen(DataSet: TDataSet);
  private
  public
    [GET, Path('/standard')]
    function Standard: TArray<TDataSet>;
  end;

var
  DataResource: TDataResource;

implementation

uses
    MARS.Core.Registry
    , MARS.Data.FireDAC.MessageBodyWriters
  ;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

{ TDataResource }

procedure TDataResource.QueryItemsBeforeOpen(DataSet: TDataSet);
var
  LOwner: string;
begin
  inherited;
  LOwner := '';
  URL.QueryTokens.TryGetValue('username', LOwner);
  QueryItems.ParamByName('OWNER').AsString := LOwner;
end;

function TDataResource.Standard: TArray<TDataSet>;
begin
  Result := [QueryItems];
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TDataResource>(
    function: TObject
    begin
      Result := TDataResource.Create(nil);
    end
  );

end.
