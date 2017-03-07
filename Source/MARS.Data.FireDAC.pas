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
    function GetContextValue(const AName: TArray<string>): Variant;
  public
    const PARAM_AND_MACRO_DELIMITER = '_';

    constructor Create(const AConnectionDefName: string;
      const AActivationRecord: TMARSActivationRecord = nil); virtual;
    destructor Destroy; override;

    function CreateCommand(const ASQL: string = ''; const ATransaction: TFDTransaction = nil;
      const AContextOwned: Boolean = True): TFDCommand; virtual;
    function CreateQuery(const ASQL: string = ''; const ATransaction: TFDTransaction = nil;
      const AContextOwned: Boolean = True): TFDQuery; virtual;
    function CreateTransaction(const AContextOwned: Boolean = True): TFDTransaction; virtual;

    procedure ExecuteSQL(const ASQL: string; const ATransaction: TFDTransaction = nil;
      const ABeforeExecute: TProc<TFDCommand> = nil;
      const AAfterExecute: TProc<TFDCommand> = nil); virtual;
    procedure Query(const ASQL: string; const ATransaction: TFDTransaction = nil;
      const AOnBeforeOpen: TProc<TFDQuery> = nil;
      const AOnDataSetReady: TProc<TFDQuery> = nil); virtual;
    procedure InTransaction(const ADoSomething: TProc<TFDTransaction>);


    property Connection: TFDConnection read GetConnection;
    property ConnectionDefName: string read FConnectionDefName write SetConnectionDefName;
    property ActivationRecord: TMARSActivationRecord read FActivationRecord;

    class function LoadConnectionDefs(const AParameters: TMARSParameters;
      const ASliceName: string = ''): TArray<string>;
    class procedure CloseConnectionDefs(const AConnectionDefNames: TArray<string>);
    class function CreateConnectionByDefName(const AConnectionDefName: string): TFDConnection;
  end;



implementation

uses
  StrUtils, System.Rtti, Variants
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

procedure TMARSFireDAC.Query(const ASQL: string; const ATransaction: TFDTransaction;
  const AOnBeforeOpen, AOnDataSetReady: TProc<TFDQuery>);
var
  LQuery: TFDQuery;
begin
  LQuery := CreateQuery(ASQL, ATransaction, False);
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

class function TMARSFireDAC.CreateConnectionByDefName(const AConnectionDefName: string): TFDConnection;
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
  const ATransaction: TFDTransaction; const AContextOwned: Boolean): TFDCommand;
begin
  Result := TFDCommand.Create(nil);
  try
    Result.Connection := Connection;
    Result.Transaction := ATransaction;
    Result.CommandText.Text := ASQL;
    InjectParamAndMacroValues(Result);
    if AContextOwned then
      ActivationRecord.AddToContext(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TMARSFireDAC.CreateQuery(const ASQL: string; const ATransaction: TFDTransaction;
  const AContextOwned: Boolean): TFDQuery;
begin
  Result := TFDQuery.Create(nil);
  try
    Result.Connection := Connection;
    Result.Transaction := ATransaction;
    Result.SQL.Text := ASQL;
    InjectParamAndMacroValues(Result.Command);
    if AContextOwned then
      ActivationRecord.AddToContext(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TMARSFireDAC.CreateTransaction(const AContextOwned: Boolean): TFDTransaction;
begin
  Result := TFDTransaction.Create(nil);
  try
    Result.Connection := Connection;
    if AContextOwned then
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

procedure TMARSFireDAC.ExecuteSQL(const ASQL: string; const ATransaction: TFDTransaction;
  const ABeforeExecute, AAfterExecute: TProc<TFDCommand>);
var
  LCommand: TFDCommand;
begin
  LCommand := CreateCommand(ASQL, ATransaction, False);
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

function TMARSFireDAC.GetContextValue(const AName: TArray<string>): Variant;
var
  LSubject: string;
  LIdentifier: string;
  LHasArgument: Boolean;
  LArgument: string;
begin
  Result := Null;
  if Length(AName) < 2 then
    Exit;

  LSubject := AName[0];
  LIdentifier := AName[1];
  LHasArgument := Length(AName) > 2;
  if LHasArgument then
    LArgument := AName[2];

  if SameText(LSubject, 'Token') then
  begin
    if SameText(LIdentifier, 'Token') then
      Result := ActivationRecord.Token.Token
    else if SameText(LIdentifier, 'UserName') then
      Result := ActivationRecord.Token.UserName
    else if SameText(LIdentifier, 'IsVerified') then
      Result := ActivationRecord.Token.IsVerified
    else if SameText(LIdentifier, 'IsExpired') then
      Result := ActivationRecord.Token.IsExpired
    else if SameText(LIdentifier, 'Expiration') then
      Result := ActivationRecord.Token.Expiration
    else if SameText(LIdentifier, 'Issuer') then
      Result := ActivationRecord.Token.Issuer
    else if SameText(LIdentifier, 'IssuedAt') then
      Result := ActivationRecord.Token.IssuedAt

    else if SameText(LIdentifier, 'HasRole') and LHasArgument then
      Result := ActivationRecord.Token.HasRole(LArgument)
    else if SameText(LIdentifier, 'Claim') and LHasArgument then
      Result := ActivationRecord.Token.Claims.ByNameText(LArgument).AsVariant;
  end;

  { TODO -oAndrea : URL, URLPrototype, Request, Application, Engine, ...}
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
    LParam.Value := GetContextValue(LParam.Name.Split([PARAM_AND_MACRO_DELIMITER]));
  end;

  for LIndex := 0 to ACommand.Macros.Count-1 do
  begin
    LMacro := ACommand.Macros[LIndex];
    LMacro.Value := GetContextValue(LMacro.Name.Split([PARAM_AND_MACRO_DELIMITER]));
  end;

end;

procedure TMARSFireDAC.InTransaction(const ADoSomething: TProc<TFDTransaction>);
var
  LTransaction: TFDTransaction;
begin
  if Assigned(ADoSomething) then
  begin
    LTransaction := CreateTransaction(False);
    try
      LTransaction.StartTransaction;
      try
        ADoSomething(LTransaction);
        LTransaction.Commit;
      except
        LTransaction.Rollback;
        raise;
      end;
    finally
      LTransaction.Free;
    end;
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
