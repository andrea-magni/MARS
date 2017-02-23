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
  , FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Stan.StorageXML
  , FireDAC.Stan.StorageJSON, FireDAC.Stan.StorageBin
  , FireDAC.UI.Intf
  , FireDAC.Phys.Intf, FireDAC.Phys
  , FireDAC.Comp.Client, FireDAC.Comp.UI

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
  protected
    procedure SetConnectionDefName(const Value: string);
    function GetConnection: TFDConnection;
  public
    constructor Create(const AConnectionDefName: string); virtual;
    destructor Destroy; override;

    function CreateCommand(const ASQL: string = ''; const AExecuteImmediate: Boolean = False): TFDCommand;
    function CreateQuery(const ASQL: string = ''; const AOpen: Boolean = False): TFDQuery;

    procedure ExecuteSQL(const ASQL: string; const ABeforeExecute: TProc<TFDCommand> = nil;
      const AAfterExecute: TProc<TFDCommand> = nil); overload;
    procedure ExecuteSQL(const ASQL: String; const AParams: array of Variant); overload;
    procedure ExecuteSQL(const ASQL: String; const AParams: array of Variant;
      const ATypes: array of TFieldType); overload;

    property Connection: TFDConnection read GetConnection;
    property ConnectionDefName: string read FConnectionDefName write SetConnectionDefName;

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

constructor TMARSFireDAC.Create(const AConnectionDefName: string);
begin
  inherited Create();
  ConnectionDefName := AConnectionDefName;
end;

function TMARSFireDAC.CreateCommand(const ASQL: string; const AExecuteImmediate: Boolean): TFDCommand;
begin
  Result := TFDCommand.Create(nil);
  try
    Result.Connection := Connection;
    Result.CommandText.Text := ASQL;
    if AExecuteImmediate then
      Result.Execute();
  except
    Result.Free;
    raise;
  end;
end;

function TMARSFireDAC.CreateQuery(const ASQL: string; const AOpen: Boolean): TFDQuery;
begin
  Result := TFDQuery.Create(nil);
  try
    Result.Connection := Connection;
    Result.SQL.Text := ASQL;
    if AOpen then
      Result.Open;
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

procedure TMARSFireDAC.ExecuteSQL(const ASQL: String;
  const AParams: array of Variant);
begin
  ExecuteSQL(ASQL, AParams, []);
end;

procedure TMARSFireDAC.ExecuteSQL(const ASQL: String;
  const AParams: array of Variant; const ATypes: array of TFieldType);
var
  LCommand: TFDCommand;
begin
  LCommand := CreateCommand();
  try
    LCommand.Execute(ASQL, AParams, ATypes);
  finally
    LCommand.Free;
  end;
end;

procedure TMARSFireDAC.ExecuteSQL(const ASQL: string; const ABeforeExecute,
  AAfterExecute: TProc<TFDCommand>);
var
  LCommand: TFDCommand;
begin
  LCommand := CreateCommand(ASQL, False);
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

procedure TMARSFireDAC.SetConnectionDefName(const Value: string);
begin
  if FConnectionDefName <> Value then
  begin
    FreeAndNil(FConnection);
    FConnectionDefName := Value;
  end;
end;

end.
