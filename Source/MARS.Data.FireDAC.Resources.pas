(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Data.FireDAC.Resources;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, Generics.Collections
  , FireDAC.Comp.Client, FireDAC.Comp.DataSet
  , MARS.Core.MediaType, MARS.Core.Attributes, MARS.Data.FireDAC, MARS.Data.FireDAC.Utils
;

type

  [
    Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON_FireDAC)
  , Consumes(TMediaType.APPLICATION_JSON), Consumes(TMediaType.APPLICATION_JSON_FireDAC)
  ]
  TMARSFDDatasetResource = class
  private
    FStatements: TDictionary<string, string>;
  protected
    [Context] FConnection: TFDConnection;
    [Context] FD: TMARSFireDAC;
    procedure SetupStatements; virtual;

    procedure AfterOpenDataSet(ADataSet: TFDCustomQuery); virtual;
    procedure BeforeOpenDataSet(ADataSet: TFDCustomQuery); virtual;
    function ReadDataSet(const ADataSetName, ASQLStatement: string; const AAutoOpen: Boolean = True): TFDCustomQuery; virtual;

    property Connection: TFDConnection read FConnection write FConnection;
    property Statements: TDictionary<string, string> read FStatements;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    [GET]
    function Retrieve: TArray<TFDDataSet>; virtual;

    [POST]
    function Update([BodyParam] const ADeltas: TArray<TFDMemTable>): TArray<TMARSFDApplyUpdatesRes>; virtual;
  end;


implementation

uses
    MARS.Core.JSON
  , MARS.Core.Exceptions
  , MARS.Rtti.Utils
;

{ TMARSFDDatasetResource }

procedure TMARSFDDatasetResource.AfterConstruction;
begin
  inherited;
  FStatements := TDictionary<string, string>.Create;
end;

procedure TMARSFDDatasetResource.AfterOpenDataSet(ADataSet: TFDCustomQuery);
begin

end;

procedure TMARSFDDatasetResource.BeforeOpenDataSet(ADataSet: TFDCustomQuery);
begin

end;

destructor TMARSFDDatasetResource.Destroy;
begin
  FStatements.Free;
  inherited;
end;

function TMARSFDDatasetResource.ReadDataSet(const ADataSetName, ASQLStatement: string; const AAutoOpen: Boolean = True): TFDCustomQuery;
begin
  Result := TFDQuery.Create(nil);
  try
    Result.Connection := Connection;
    Result.SQL.Text := ASQLStatement;
    Result.Name := ADataSetName;
    BeforeOpenDataSet(Result);
    if AAutoOpen then
    begin
      Result.Open;
      AfterOpenDataSet(Result);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TMARSFDDatasetResource.Retrieve: TArray<TFDDataSet>;
var
  LStatement: TPair<string, string>;
  LData: TArray<TFDDataSet>;
  LCurrent: TFDDataSet;
begin
  LData := [];
  try
    // load dataset(s)
    SetupStatements;
    for LStatement in Statements do
      LData := LData + [ReadDataSet(LStatement.Key, LStatement.Value)];

    Result := LData;
  except
    // clean up
    for LCurrent in LData do
      LCurrent.Free;
    LData := [];
    raise;
  end;
end;

procedure TMARSFDDatasetResource.SetupStatements;
begin
  TRTTIHelper.ForEachAttribute<SQLStatementAttribute>(
    Self,
    procedure (AAttrib: SQLStatementAttribute)
    begin
      Statements.Add(AAttrib.Name, AAttrib.SQLStatement);
    end);
end;

function TMARSFDDatasetResource.Update([BodyParam] const ADeltas: TArray<TFDMemTable>): TArray<TMARSFDApplyUpdatesRes>;
begin
  // setup
  SetupStatements;

  // apply updates
  Result := FD.ApplyUpdates(Retrieve, ADeltas);
end;


end.
