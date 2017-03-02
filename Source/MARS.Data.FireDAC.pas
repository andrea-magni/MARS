(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Data.FireDAC;

{$I MARS.inc}

interface

uses
    System.Classes, System.SysUtils, Generics.Collections

  , Data.DB
  , FireDAC.DApt
  , FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.Stan.Def
  , FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Stan.StorageXML, FireDAC.Stan.Param
  , FireDAC.Stan.StorageJSON, FireDAC.Stan.StorageBin
  , FireDAC.UI.Intf
  , FireDAC.Phys.Intf, FireDAC.Phys
  , FireDAC.Comp.Client, FireDAC.Comp.UI, FireDAC.Comp.DataSet

  , FireDACJSONReflect

  , MARS.Core.Application
  , MARS.Core.Attributes
  , MARS.Core.Classes
  , MARS.Core.Declarations
  , MARS.Core.JSON
  , MARS.Core.MediaType
  , MARS.Core.Registry
  , MARS.Core.Token
  , MARS.Core.URL
  , MARS.Utils.Parameters
  , MARS.Core.Invocation
;

type
  ConnectionAttribute = class(TCustomAttribute)
  private
    FConnectionDefName: string;
  public
    constructor Create(AConnectionDefName: string);
    property ConnectionDefName: string read FConnectionDefName;
  end;

  SQLStatementAttribute = class(TCustomAttribute)
  private
    FName: string;
    FSQLStatement: string;
  public
    constructor Create(AName, ASQLStatement: string);
    property Name: string read FName;
    property SQLStatement: string read FSQLStatement;
  end;

  TMARSFireDAC = class
  private
    FConnectionDefName: string;
    FConnection: TFDConnection;
    FActivationRecord: TMARSActivationRecord;
  protected
    procedure SetConnectionDefName(const Value: string);
    function GetConnection: TFDConnection;
    procedure InjectParamAndMacroValues(const ACommand: TFDCustomCommand);
  public
    constructor Create(const AConnectionDefName: string;
      const AActivationRecord: TMARSActivationRecord = nil); virtual;
    destructor Destroy; override;

    function CreateCommand(const ASQL: string = ''; const AExecuteImmediate: Boolean = False; const AOwned: Boolean = True): TFDCommand;
    function CreateQuery(const ASQL: string = ''; const AOpen: Boolean = False; const AOwned: Boolean = True): TFDQuery;

    procedure ExecuteSQL(const ASQL: string; const ABeforeExecute: TProc<TFDCommand> = nil;
      const AAfterExecute: TProc<TFDCommand> = nil); overload;
    procedure Query(const ASQL: string; const AOnBeforeOpen: TProc<TFDQuery>;
      const AOnDataSetReady: TProc<TFDQuery>);

    property Connection: TFDConnection read GetConnection;
    property ConnectionDefName: string read FConnectionDefName write SetConnectionDefName;
    property ActivationRecord: TMARSActivationRecord read FActivationRecord;

    class function LoadConnectionDefs(const AParameters: TMARSParameters;
      const ASliceName: string = ''): TArray<string>;
    class procedure CloseConnectionDefs(const AConnectionDefNames: TArray<string>);
  end;

  function CreateConnectionByDefName(const AConnectionDefName: string): TFDConnection;


implementation

uses
  StrUtils, System.Rtti
  , MARS.Core.Utils
  , MARS.Core.Exceptions
  , MARS.Data.Utils
  , MARS.Rtti.Utils
  , MARS.Data.FireDAC.InjectionService
  , MARS.Data.FireDAC.ReadersAndWriters
;

function GetAsTStrings(const AParameters: TMARSParameters): TStrings;
var
  LParam: TPair<string, TValue>;
begin
  Result := TStringList.Create;
  try
    for LParam in AParameters do
      Result.Values[LParam.Key] := LParam.Value.ToString;
  except
    Result.Free;
    raise;
  end;
end;

class function TMARSFireDAC.LoadConnectionDefs(const AParameters: TMARSParameters;
  const ASliceName: string = ''): TArray<string>;
var
  LData, LConnectionParams: TMARSParameters;
  LConnectionDefNames: TArray<string>;
  LConnectionDefName: string;
  LParams: TStrings;
begin
  Result := [];
  LData := TMARSParameters.Create('');
  try
    LData.CopyFrom(AParameters, ASliceName);
    LConnectionDefNames := LData.SliceNames;

    for LConnectionDefName in LConnectionDefNames do
    begin
      LConnectionParams := TMARSParameters.Create(LConnectionDefName);
      try
        LConnectionParams.CopyFrom(LData, LConnectionDefName);
        LParams := GetAsTStrings(LConnectionParams);
        try
          FDManager.AddConnectionDef(LConnectionDefName, LParams.Values['DriverID'], LParams);
          Result := Result + [LConnectionDefName];
        finally
          LParams.Free;
        end;
      finally
        LConnectionParams.Free;
      end;
    end;
  finally
    LData.Free;
  end;
end;

procedure TMARSFireDAC.Query(const ASQL: string; const AOnBeforeOpen,
  AOnDataSetReady: TProc<TFDQuery>);
var
  LQuery: TFDQuery;
begin
  LQuery := CreateQuery(ASQL, False, False);
  try
    if Assigned(AOnBeforeOpen) then
      AOnBeforeOpen(LQuery);
    LQuery.Active := True;
    if Assigned(AOnDataSetReady) then
      AOnDataSetReady(LQuery);
  finally
    LQuery.Free;
  end;
end;

function CreateConnectionByDefName(const AConnectionDefName: string): TFDConnection;
begin
  Result := TFDConnection.Create(nil);
  try
    Result.ConnectionDefName := AConnectionDefName;
  except
    Result.Free;
    raise;
  end;
end;

{ ConnectionAttribute }

constructor ConnectionAttribute.Create(AConnectionDefName: string);
begin
  inherited Create;
  FConnectionDefName := AConnectionDefName;
end;

{ SQLStatementAttribute }

constructor SQLStatementAttribute.Create(AName, ASQLStatement: string);
begin
  inherited Create;
  FName := AName;
  FSQLStatement := ASQLStatement;
end;

{ TMARSFireDAC }

class procedure TMARSFireDAC.CloseConnectionDefs(
  const AConnectionDefNames: TArray<string>);
var
  LConnectionDefName: string;
begin
  for LConnectionDefName in AConnectionDefNames do
    FDManager.CloseConnectionDef(LConnectionDefName);
end;

constructor TMARSFireDAC.Create(const AConnectionDefName: string;
  const AActivationRecord: TMARSActivationRecord);
begin
  inherited Create();
  ConnectionDefName := AConnectionDefName;
  FActivationRecord := AActivationRecord;
end;

function TMARSFireDAC.CreateCommand(const ASQL: string;
  const AExecuteImmediate: Boolean; const AOwned: Boolean): TFDCommand;
begin
  Result := TFDCommand.Create(nil);
  try
    Result.Connection := Connection;
    Result.CommandText.Text := ASQL;
    InjectParamAndMacroValues(Result);
    if AExecuteImmediate then
      Result.Execute();
    ActivationRecord.AddToContext(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TMARSFireDAC.CreateQuery(const ASQL: string; const AOpen: Boolean;
  const AOwned: Boolean): TFDQuery;
begin
  Result := TFDQuery.Create(nil);
  try
    Result.Connection := Connection;
    Result.SQL.Text := ASQL;
    InjectParamAndMacroValues(Result.Command);
    if AOpen then
      Result.Open;
    ActivationRecord.AddToContext(Result);
  except
    Result.Free;
    raise;
  end;
end;

destructor TMARSFireDAC.Destroy;
begin
  FreeAndNil(FConnection);
  inherited;
end;

procedure TMARSFireDAC.ExecuteSQL(const ASQL: string; const ABeforeExecute,
  AAfterExecute: TProc<TFDCommand>);
var
  LCommand: TFDCommand;
begin
  LCommand := CreateCommand(ASQL, False, False);
  try
    if Assigned(ABeforeExecute) then
      ABeforeExecute(LCommand);
    LCommand.Execute();
    if Assigned(AAfterExecute) then
      AAfterExecute(LCommand);
  finally
    LCommand.Free;
  end;
end;

function TMARSFireDAC.GetConnection: TFDConnection;
begin
  if not Assigned(FConnection) then
    FConnection := CreateConnectionByDefName(ConnectionDefName);
  Result := FConnection;
end;

procedure TMARSFireDAC.InjectParamAndMacroValues(const ACommand: TFDCustomCommand);
var
  LIndex: Integer;
  LParam: TFDParam;
  LMacro: TFDMacro;
begin
  for LIndex := 0 to ACommand.Params.Count-1 do
  begin
    LParam := ACommand.Params[LIndex];
    if SameText(LParam.Name, 'Token_UserName') then
      LParam.AsString := ActivationRecord.Token.UserName;
  end;

  for LIndex := 0 to ACommand.Macros.Count-1 do
  begin
    LMacro := ACommand.Macros[LIndex];
    if SameText(LMacro.Name, 'Token_UserName') then
      LMacro.AsString := ActivationRecord.Token.UserName;
  end;

end;

procedure TMARSFireDAC.SetConnectionDefName(const Value: string);
begin
  if FConnectionDefName <> Value then
  begin
    FreeAndNil(FConnection);
    FConnectionDefName := Value;
  end;
end;

end.
