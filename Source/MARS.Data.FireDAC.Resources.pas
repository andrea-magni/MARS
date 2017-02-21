(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Data.FireDAC.Resources;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, Generics.Collections
  , FireDAC.Comp.Client, FireDACJSONReflect
  , MARS.Core.JSON , MARS.Core.MediaType, MARS.Core.Attributes, MARS.Data.FireDAC
;

type

  [Produces(TMediaType.APPLICATION_JSON), Consumes(TMediaType.APPLICATION_JSON)]
  TMARSFDDatasetResource = class
  private
    FStatements: TDictionary<string, string>;
  protected
    [Context] FConnection: TFDConnection;
    procedure SetupStatements; virtual;

    procedure AfterOpenDataSet(ADataSet: TFDCustomQuery); virtual;
    procedure BeforeOpenDataSet(ADataSet: TFDCustomQuery); virtual;
    function ReadDataSet(const ADataSetName, ASQLStatement: string; const AAutoOpen: Boolean = True): TFDCustomQuery; virtual;

    procedure ApplyUpdates(ADeltas: TFDJSONDeltas;
      AOnApplyUpdates: TProc<string, Integer, IFDJSONDeltasApplyUpdates> = nil); virtual;

    property Connection: TFDConnection read FConnection write FConnection;
    property Statements: TDictionary<string, string> read FStatements;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    [GET]
    function Retrieve: TArray<TFDCustomQuery>; virtual;

    [POST]
    function Update([BodyParam] const AJSONDeltas: TJSONObject): string; virtual;
  end;


implementation

uses
    MARS.Core.Exceptions
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

procedure TMARSFDDatasetResource.ApplyUpdates(
  ADeltas: TFDJSONDeltas;
  AOnApplyUpdates: TProc<string, Integer, IFDJSONDeltasApplyUpdates>);
var
  LApplyUpdates: IFDJSONDeltasApplyUpdates;
  LIndex: Integer;
  LDelta: TPair<string, TFDMemTable>;
  LStatement: string;
  LDataSet: TFDCustomQuery;
  LApplyResult: Integer;
begin
  LApplyUpdates := TFDJSONDeltasApplyUpdates.Create(ADeltas);
  try
    for LIndex := 0 to TFDJSONDeltasReader.GetListCount(ADeltas) - 1 do
    begin
      LDelta := TFDJSONDeltasReader.GetListItem(ADeltas, LIndex);

      if Statements.TryGetValue(LDelta.Key, LStatement) then
      begin
        LDataSet := ReadDataSet(LDelta.Key, LStatement, False);
        try
          LApplyResult := LApplyUpdates.ApplyUpdates(LDelta.Key, LDataSet.Command);
          if Assigned(AOnApplyUpdates) then
            AOnApplyUpdates(LDelta.Key, LApplyResult, LApplyUpdates);
        finally
          LDataSet.Free;
        end;
      end
      else
        raise EMARSException.CreateFmt('Unable to build update command for delta: %s', [LDelta.Key]);
    end;
  finally
    LApplyUpdates := nil; // it's an interface
  end;
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

function TMARSFDDatasetResource.Retrieve: TArray<TFDCustomQuery>;
var
  LStatement: TPair<string, string>;
  LData: TArray<TFDCustomQuery>;
  LCurrent: TFDCustomQuery;
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

function TMARSFDDatasetResource.Update([BodyParam] const AJSONDeltas: TJSONObject): string;
var
  LDeltas: TFDJSONDeltas;
  LResult: TJSONArray;
begin
  // setup
  SetupStatements;

  LDeltas := TFDJSONDeltas.Create;
  try
    // build FireDAC delta objects
    if not TFDJSONInterceptor.JSONObjectToDataSets(AJSONDeltas, LDeltas) then
      raise EMARSException.Create('Error de-serializing deltas');

    // apply updates
    LResult := TJSONArray.Create;
    try
      ApplyUpdates(LDeltas,
        procedure(ADatasetName: string; AApplyResult: Integer; AApplyUpdates: IFDJSONDeltasApplyUpdates)
        var
          LResultObj: TJSONObject;
        begin
          LResultObj := TJSONObject.Create;
          try
            LResultObj.AddPair('dataset', ADatasetName);
            LResultObj.AddPair('result', TJSONNumber.Create(AApplyResult));
            LResultObj.AddPair('errors', TJSONNumber.Create(AApplyUpdates.Errors.Count));
            LResultObj.AddPair('errorText', AApplyUpdates.Errors.Strings.Text);
            LResult.AddElement(LResultObj);
          except
            LResultObj.Free;
            raise;
          end;
        end
      );

      Result := LResult.ToJSON;
    finally
      LResult.Free;
    end;
  finally
    LDeltas.Free;
  end;
end;


end.
