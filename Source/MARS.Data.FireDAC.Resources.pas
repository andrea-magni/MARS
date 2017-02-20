(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Data.FireDAC.Resources;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, Generics.Collections
  , Web.HttpApp
  , FireDAC.Comp.Client, FireDACJSONReflect
  , MARS.Core.MediaType, MARS.Core.Attributes, MARS.Data.FireDAC
;

type

  TMARSFDDatasetResource = class
  private
    FConnection: TFDConnection;
    FStatements: TDictionary<string, string>;
    FOwnsConnection: Boolean;
  protected
    [Context] Request: TWebRequest;

    procedure SetupConnection; virtual;
    procedure TeardownConnection; virtual;
    procedure CheckConnection; virtual;

    procedure SetupStatements; virtual;

    procedure AfterOpenDataSet(ADataSet: TFDCustomQuery); virtual;
    procedure BeforeOpenDataSet(ADataSet: TFDCustomQuery); virtual;
    function ReadDataSet(const ADataSetName, ASQLStatement: string; const AAutoOpen: Boolean = True): TFDCustomQuery; virtual;

    procedure ApplyUpdates(ADeltas: TFDJSONDeltas;
      AOnApplyUpdates: TProc<string, Integer, IFDJSONDeltasApplyUpdates> = nil); virtual;

    property Connection: TFDConnection read FConnection write FConnection;
    property OwnsConnection: Boolean read FOwnsConnection write FOwnsConnection;
    property Statements: TDictionary<string, string> read FStatements;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    [GET][Produces(TMediaType.APPLICATION_JSON)]
    function Retrieve: TArray<TFDCustomQuery>; virtual;

    [POST][Produces(TMediaType.APPLICATION_JSON)]
    [Consumes(TMediaType.APPLICATION_JSON)]
    function Update: string; virtual;
  end;


implementation

uses
    MARS.Core.Exceptions
  , MARS.Core.JSON
  , MARS.Rtti.Utils
;

{ TMARSFDDatasetResource }

procedure TMARSFDDatasetResource.AfterConstruction;
begin
  inherited;
  FStatements := TDictionary<string, string>.Create;
  FOwnsConnection := False;

  SetupConnection;
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

procedure TMARSFDDatasetResource.CheckConnection;
begin
  if not Assigned(Connection) then
    raise EMARSException.Create('No data connection available');
end;

destructor TMARSFDDatasetResource.Destroy;
begin
  TeardownConnection;
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
  CheckConnection;

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

procedure TMARSFDDatasetResource.SetupConnection;
begin
  TRTTIHelper.IfHasAttribute<ConnectionAttribute>(
    Self,
    procedure (AAttrib: ConnectionAttribute)
    begin
      Connection := CreateConnectionByDefName(AAttrib.ConnectionDefName);
      OwnsConnection := True;
    end
  );
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

procedure TMARSFDDatasetResource.TeardownConnection;
begin
  if OwnsConnection and Assigned(Connection) then
  begin
    Connection.Free;
    Connection := nil;
  end;
end;

function TMARSFDDatasetResource.Update: string;
var
  LJSONDeltas: TJSONObject;
  LDeltas: TFDJSONDeltas;
  LResult: TJSONArray;
begin
  SetupConnection;
  try
    CheckConnection;

    // setup
    SetupStatements;

    // parse JSON content
    LJSONDeltas := TJSONObject.ParseJSONValue(Request.Content) as TJSONObject;

    LDeltas := TFDJSONDeltas.Create;
    try
      // build FireDAC delta objects
      if not TFDJSONInterceptor.JSONObjectToDataSets(LJSONDeltas, LDeltas) then
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

  finally
    TeardownConnection;
  end;
end;


end.
