(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Data.MyDAC;

{$I MARS.inc}

{$IFDEF MARS_MYDAC}

interface

uses
  System.Classes, System.SysUtils, Generics.Collections, Rtti, Data.DB
// Devart MyDAC
, DBAccess, MyAccess
// MARS
, MARS.Core.Activation.Interfaces
, MARS.Core.Exceptions
, MARS.Utils.Parameters
, MARS.Data.MyDAC.Utils
;

type
  EMARSMyDACException = class(EMARSApplicationException);

  MARSMyDACAttribute = class(TCustomAttribute);

  ConnectionAttribute = class(MARSMyDACAttribute)
  private
    FConnectionDefName: string;
    FExpandMacros: Boolean;
  public
    constructor Create(AConnectionDefName: string; const AExpandMacros: Boolean = False);
    property ConnectionDefName: string read FConnectionDefName;
    property ExpandMacros: Boolean read FExpandMacros;
  end;

  SQLStatementAttribute = class(MARSMyDACAttribute)
  private
    FName: string;
    FSQLStatement: string;
  public
    constructor Create(AName, ASQLStatement: string);
    property Name: string read FName;
    property SQLStatement: string read FSQLStatement;
  end;

  TContextValueProviderProc = reference to procedure (const AActivation: IMARSActivation;
    const AName: string; const ADesiredType: TFieldType; out AValue: TValue);

  TAfterCreateConnectionProc = reference to procedure(const AConnection: TMyConnection; const AActivation: IMARSActivation);

  TMARSMyDAC = class
  private
    FConnectionDefName: string;
    FConnection: TMyConnection;
    FActivation: IMARSActivation;
    class var FConnectionDefs: TDictionary<string, string>;
  protected
    procedure CheckTransaction(const ATransaction: TMyTransaction); virtual;
    procedure SetConnectionDefName(const Value: string); virtual;
    function GetConnection: TMyConnection; virtual;
    class var FContextValueProviders: TArray<TContextValueProviderProc>;
    class var FAfterCreateConnection: TAfterCreateConnectionProc;
  public
    const PARAM_AND_MACRO_DELIMITER = '_';

    class function GetContextValue(const AName: string; const AActivation: IMARSActivation;
      const ADesiredType: TFieldType = ftUnknown): TValue; virtual;

    procedure InjectParamValues(const AParams: TDAParams;
      const AOnlyIfEmpty: Boolean = True); virtual;
    procedure InjectMacroValues(const AMacros: TMacros;
      const AOnlyIfEmpty: Boolean = True); virtual;

    procedure InjectMacroAndParamValues(const ACommand: TCustomDASQL; const AOnlyIfEmpty: Boolean = True); overload;
    procedure InjectMacroAndParamValues(const ADataSet: TCustomDADataSet; const AOnlyIfEmpty: Boolean = True); overload;

    constructor Create(const AConnectionDefName: string;
      const AActivation: IMARSActivation = nil); virtual;
    destructor Destroy; override;

    function CreateCommand(const ASQL: string = ''; const ATransaction: TMyTransaction = nil;
      const AContextOwned: Boolean = True): TMyCommand; virtual;
    function CreateQuery(const ASQL: string = ''; const ATransaction: TMyTransaction = nil;
      const AContextOwned: Boolean = True; const AName: string = 'DataSet'): TMyQuery; virtual;
    function CreateTransaction(const AContextOwned: Boolean = True): TMyTransaction; virtual;

    function ExecuteSQL(const ASQL: string; const ATransaction: TMyTransaction = nil;
      const ABeforeExecute: TProc<TMyCommand> = nil;
      const AAfterExecute: TProc<TMyCommand> = nil): Integer; virtual;

    function Query(const ASQL: string): TMyQuery; overload; virtual;

    function Query(const ASQL: string;
      const ATransaction: TMyTransaction): TMyQuery; overload; virtual;

    function Query(const ASQL: string; const ATransaction: TMyTransaction;
      const AContextOwned: Boolean): TMyQuery; overload; virtual;

    function Query(const ASQL: string; const ATransaction: TMyTransaction;
      const AContextOwned: Boolean;
      const AOnBeforeOpen: TProc<TMyQuery>): TMyQuery; overload; virtual;

    procedure Query(const ASQL: string; const ATransaction: TMyTransaction;
      const AOnBeforeOpen: TProc<TMyQuery>;
      const AOnDataSetReady: TProc<TMyQuery>); overload; virtual;

    function SetName<T: TComponent>(const AComponent: T; const AName: string): T; overload;

    procedure InTransaction(const ADoSomething: TProc<TMyTransaction>);

    property Connection: TMyConnection read GetConnection;
    property ConnectionDefName: string read FConnectionDefName write SetConnectionDefName;
    property Activation: IMARSActivation read FActivation;

    class function LoadConnectionDefs(const AParameters: TMARSParameters;
      const ASliceName: string = ''): TArray<string>;
    class procedure CloseConnectionDefs(const AConnectionDefNames: TArray<string>);
    class function CreateConnectionByDefName(const AConnectionDefName: string;
      const AActivation: IMARSActivation = nil): TMyConnection;
    class function CreateConnectionByConnectString(const AConnectString: string): TMyConnection;

    class constructor CreateClass;
    class destructor DestroyClass;
    class procedure AddContextValueProvider(const AContextValueProviderProc: TContextValueProviderProc);
    class property AfterCreateConnection: TAfterCreateConnectionProc read FAfterCreateConnection write FAfterCreateConnection;
  end;

implementation

uses
  StrUtils, Variants
, MARS.Core.Activation
, MARS.Data.MyDAC.InjectionService
, MARS.Data.MyDAC.ReadersAndWriters
;

// Name=Value pairs, separated by ';' (MyDAC connect string format)
function GetAsConnectString(const AParameters: TMARSParameters): string;
var
  LParam: TPair<string, TValue>;
begin
  Result := '';
  for LParam in AParameters do
  begin
    if Result <> '' then
      Result := Result + ';';
    Result := Result + LParam.Key + '=' + LParam.Value.ToString;
  end;
end;

class function TMARSMyDAC.LoadConnectionDefs(const AParameters: TMARSParameters;
  const ASliceName: string = ''): TArray<string>;
var
  LData, LConnectionParams: TMARSParameters;
  LConnectionDefNames: TArray<string>;
  LConnectionDefName: string;
  LConnectString: string;
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

        // either a full ConnectString parameter or one parameter per connect string item
        LConnectString := LConnectionParams.ByNameText('ConnectString', '').AsString;
        if LConnectString = '' then
          LConnectString := GetAsConnectString(LConnectionParams);

        TMonitor.Enter(FConnectionDefs);
        try
          FConnectionDefs.AddOrSetValue(LConnectionDefName, LConnectString);
        finally
          TMonitor.Exit(FConnectionDefs);
        end;

        Result := Result + [LConnectionDefName];
      finally
        LConnectionParams.Free;
      end;
    end;
  finally
    LData.Free;
  end;
end;

function TMARSMyDAC.Query(const ASQL: string;
  const ATransaction: TMyTransaction): TMyQuery;
begin
  Result := Query(ASQL, ATransaction, True);
end;

function TMARSMyDAC.Query(const ASQL: string;
  const ATransaction: TMyTransaction; const AContextOwned: Boolean;
  const AOnBeforeOpen: TProc<TMyQuery>): TMyQuery;
begin
  Result := CreateQuery(ASQL, ATransaction, AContextOwned);
  try
    if Assigned(AOnBeforeOpen) then
      AOnBeforeOpen(Result);
    Result.Open;
  except
    if not AContextOwned then
      Result.Free;
    raise;
  end;
end;

function TMARSMyDAC.Query(const ASQL: string): TMyQuery;
begin
  Result := Query(ASQL, nil, True);
end;

procedure TMARSMyDAC.Query(const ASQL: string; const ATransaction: TMyTransaction;
  const AOnBeforeOpen, AOnDataSetReady: TProc<TMyQuery>);
var
  LQuery: TMyQuery;
begin
  LQuery := Query(ASQL, ATransaction, False, AOnBeforeOpen);
  try
    if Assigned(AOnDataSetReady) then
      AOnDataSetReady(LQuery);
  finally
    LQuery.Free;
  end;
end;

function TMARSMyDAC.Query(const ASQL: string;
  const ATransaction: TMyTransaction; const AContextOwned: Boolean): TMyQuery;
begin
  Result := Query(ASQL, ATransaction, AContextOwned, nil);
end;

class function TMARSMyDAC.CreateConnectionByConnectString(const AConnectString: string): TMyConnection;
begin
  Result := TMyConnection.Create(nil);
  try
    if AConnectString <> '' then
      Result.ConnectString := AConnectString;
    Result.LoginPrompt := False; // after ConnectString, that resets it
  except
    Result.Free;
    raise;
  end;
end;

class function TMARSMyDAC.CreateConnectionByDefName(
  const AConnectionDefName: string; const AActivation: IMARSActivation): TMyConnection;
var
  LConnectString: string;
  LFound: Boolean;
begin
  LConnectString := '';
  if AConnectionDefName <> '' then
  begin
    TMonitor.Enter(FConnectionDefs);
    try
      LFound := FConnectionDefs.TryGetValue(AConnectionDefName, LConnectString);
    finally
      TMonitor.Exit(FConnectionDefs);
    end;
    if not LFound then
      raise EMARSMyDACException.CreateFmt('MyDAC connection definition not found: %s', [AConnectionDefName]);
  end;

  Result := CreateConnectionByConnectString(LConnectString);
  try
    if Assigned(FAfterCreateConnection) then
      FAfterCreateConnection(Result, AActivation);
  except
    Result.Free;
    raise;
  end;
end;

{ ConnectionAttribute }

constructor ConnectionAttribute.Create(AConnectionDefName: string; const AExpandMacros: Boolean = False);
begin
  inherited Create;
  FConnectionDefName := AConnectionDefName;
  FExpandMacros := AExpandMacros;
end;

{ SQLStatementAttribute }

constructor SQLStatementAttribute.Create(AName, ASQLStatement: string);
begin
  inherited Create;
  FName := AName;
  FSQLStatement := ASQLStatement;
end;

{ TMARSMyDAC }

// MySQL has one transaction per connection: MyDAC commands and queries have no
// Transaction property, they take part in the active transaction of their connection.
procedure TMARSMyDAC.CheckTransaction(const ATransaction: TMyTransaction);
begin
  if Assigned(ATransaction) and (ATransaction.DefaultConnection <> Connection) then
    raise EMARSMyDACException.Create('The transaction does not belong to the connection of this TMARSMyDAC instance');
end;

class procedure TMARSMyDAC.AddContextValueProvider(
  const AContextValueProviderProc: TContextValueProviderProc);
begin
  FContextValueProviders := FContextValueProviders + [TContextValueProviderProc(AContextValueProviderProc)];
end;

class procedure TMARSMyDAC.CloseConnectionDefs(
  const AConnectionDefNames: TArray<string>);
var
  LConnectionDefName: string;
begin
  TMonitor.Enter(FConnectionDefs);
  try
    for LConnectionDefName in AConnectionDefNames do
      FConnectionDefs.Remove(LConnectionDefName);
  finally
    TMonitor.Exit(FConnectionDefs);
  end;
end;

constructor TMARSMyDAC.Create(const AConnectionDefName: string;
  const AActivation: IMARSActivation);
begin
  inherited Create();
  ConnectionDefName := AConnectionDefName;
  FActivation := AActivation;
end;

class constructor TMARSMyDAC.CreateClass;
begin
  FContextValueProviders := [];
  FAfterCreateConnection := nil;
  FConnectionDefs := TDictionary<string, string>.Create();
end;

function TMARSMyDAC.CreateCommand(const ASQL: string;
  const ATransaction: TMyTransaction; const AContextOwned: Boolean): TMyCommand;
begin
  Result := TMyCommand.Create(nil);
  try
    Result.Connection := Connection;
    CheckTransaction(ATransaction);
    Result.SQL.Text := ASQL;
    InjectMacroAndParamValues(Result);
    if AContextOwned and Assigned(Activation) then
      Activation.AddToContext(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TMARSMyDAC.CreateQuery(const ASQL: string; const ATransaction: TMyTransaction;
  const AContextOwned: Boolean; const AName: string): TMyQuery;
begin
  Result := TMyQuery.Create(nil);
  try
    Result.Name := AName;
    Result.Connection := Connection;
    CheckTransaction(ATransaction);
    Result.SQL.Text := ASQL;
    InjectMacroAndParamValues(Result);
    if AContextOwned and Assigned(Activation) then
      Activation.AddToContext(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TMARSMyDAC.CreateTransaction(const AContextOwned: Boolean): TMyTransaction;
begin
  Result := TMyTransaction.Create(nil);
  try
    // MyDAC starts a transaction only on an active connection
    if not Connection.Connected then
      Connection.Connect;
    Result.DefaultConnection := Connection;
    if AContextOwned and Assigned(Activation) then
      Activation.AddToContext(Result);
  except
    Result.Free;
    raise;
  end;
end;

destructor TMARSMyDAC.Destroy;
begin
  FreeAndNil(FConnection);
  inherited;
end;

class destructor TMARSMyDAC.DestroyClass;
begin
  FreeAndNil(FConnectionDefs);
end;

function TMARSMyDAC.ExecuteSQL(const ASQL: string; const ATransaction: TMyTransaction;
  const ABeforeExecute, AAfterExecute: TProc<TMyCommand>): Integer;
var
  LCommand: TMyCommand;
begin
  LCommand := CreateCommand(ASQL, ATransaction, False);
  try
    if Assigned(ABeforeExecute) then
      ABeforeExecute(LCommand);
    LCommand.Execute;
    Result := LCommand.RowsAffected;
    if Assigned(AAfterExecute) then
      AAfterExecute(LCommand);
  finally
    LCommand.Free;
  end;
end;

function TMARSMyDAC.GetConnection: TMyConnection;
begin
  if not Assigned(FConnection) then
    FConnection := CreateConnectionByDefName(ConnectionDefName, FActivation);
  Result := FConnection;
end;

class function TMARSMyDAC.GetContextValue(const AName: string; const AActivation: IMARSActivation;
  const ADesiredType: TFieldType): TValue;
var
  LCustomProvider: TContextValueProviderProc;
begin
  Result := TMARSActivation.GetValueByName(AName, AActivation);

  if Result.IsEmpty then   // last chance, custom injection
    for LCustomProvider in FContextValueProviders do
      LCustomProvider(AActivation, AName, ADesiredType, Result);
end;

procedure TMARSMyDAC.InjectMacroAndParamValues(
  const ACommand: TCustomDASQL; const AOnlyIfEmpty: Boolean);
begin
  if not Assigned(ACommand) then
    Exit;
  InjectMacroValues(ACommand.Macros, AOnlyIfEmpty);
  InjectParamValues(ACommand.Params, AOnlyIfEmpty);
end;

procedure TMARSMyDAC.InjectMacroAndParamValues(
  const ADataSet: TCustomDADataSet; const AOnlyIfEmpty: Boolean);
begin
  if not Assigned(ADataSet) then
    Exit;
  InjectMacroValues(ADataSet.Macros, AOnlyIfEmpty);
  InjectParamValues(ADataSet.Params, AOnlyIfEmpty);
end;

procedure TMARSMyDAC.InjectMacroValues(const AMacros: TMacros; const AOnlyIfEmpty: Boolean);
var
  LIndex: Integer;
  LMacro: TMacro;
  LValue: TValue;
begin
  if not Assigned(AMacros) then
    Exit;

  for LIndex := 0 to AMacros.Count-1 do
  begin
    LMacro := AMacros[LIndex];
    if (not AOnlyIfEmpty) or (LMacro.Value = '') then
    begin
      LValue := GetContextValue(LMacro.Name, Activation, ftString);
      if not LValue.IsEmpty then
        LMacro.Value := LValue.ToString;
    end;
  end;
end;

procedure TMARSMyDAC.InjectParamValues(const AParams: TDAParams; const AOnlyIfEmpty: Boolean);
var
  LIndex: Integer;
  LParam: TDAParam;
begin
  if not Assigned(AParams) then
    Exit;

  for LIndex := 0 to AParams.Count-1 do
  begin
    LParam := AParams[LIndex];
    if ((not AOnlyIfEmpty) or LParam.IsNull) then
      LParam.Value := GetContextValue(LParam.Name, Activation, LParam.DataType).AsVariant;
  end;
end;

procedure TMARSMyDAC.InTransaction(const ADoSomething: TProc<TMyTransaction>);
var
  LTransaction: TMyTransaction;
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

procedure TMARSMyDAC.SetConnectionDefName(const Value: string);
begin
  if FConnectionDefName <> Value then
  begin
    FreeAndNil(FConnection);
    FConnectionDefName := Value;
  end;
end;

function TMARSMyDAC.SetName<T>(const AComponent: T; const AName: string): T;
begin
  AComponent.Name := AName;
  Result := AComponent;
end;

{$ELSE}
interface
implementation
{$ENDIF}

end.
