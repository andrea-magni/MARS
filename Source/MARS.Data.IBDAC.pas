(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Data.IBDAC;

{$I MARS.inc}

{$IFDEF MARS_IBDAC}

interface

uses
  System.Classes, System.SysUtils, Generics.Collections, Rtti, Data.DB
// Devart IBDAC
, DBAccess, IBC
// MARS
, MARS.Core.Activation.Interfaces
, MARS.Core.Exceptions
, MARS.Utils.Parameters
, MARS.Data.IBDAC.Utils
;

type
  EMARSIBDACException = class(EMARSApplicationException);

  MARSIBDACAttribute = class(TCustomAttribute);

  ConnectionAttribute = class(MARSIBDACAttribute)
  private
    FConnectionDefName: string;
    FExpandMacros: Boolean;
  public
    constructor Create(AConnectionDefName: string; const AExpandMacros: Boolean = False);
    property ConnectionDefName: string read FConnectionDefName;
    property ExpandMacros: Boolean read FExpandMacros;
  end;

  // unambiguous name of ConnectionAttribute, i.e. [IBDACConnection('DEFNAME')], for units that
  // use more than one MARS data access integration (all of them declare ConnectionAttribute)
  IBDACConnectionAttribute = ConnectionAttribute;

  SQLStatementAttribute = class(MARSIBDACAttribute)
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

  TAfterCreateConnectionProc = reference to procedure(const AConnection: TIBCConnection; const AActivation: IMARSActivation);

  TMARSIBDAC = class
  private
    FConnectionDefName: string;
    FConnection: TIBCConnection;
    FActivation: IMARSActivation;
    class var FConnectionDefs: TDictionary<string, string>;
  protected
    procedure SetConnectionDefName(const Value: string); virtual;
    function GetConnection: TIBCConnection; virtual;
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

    function CreateCommand(const ASQL: string = ''; const ATransaction: TIBCTransaction = nil;
      const AContextOwned: Boolean = True): TIBCSQL; virtual;
    function CreateQuery(const ASQL: string = ''; const ATransaction: TIBCTransaction = nil;
      const AContextOwned: Boolean = True; const AName: string = 'DataSet'): TIBCQuery; virtual;
    function CreateTransaction(const AContextOwned: Boolean = True): TIBCTransaction; virtual;

    function ExecuteSQL(const ASQL: string; const ATransaction: TIBCTransaction = nil;
      const ABeforeExecute: TProc<TIBCSQL> = nil;
      const AAfterExecute: TProc<TIBCSQL> = nil): Integer; virtual;

    function Query(const ASQL: string): TIBCQuery; overload; virtual;

    function Query(const ASQL: string;
      const ATransaction: TIBCTransaction): TIBCQuery; overload; virtual;

    function Query(const ASQL: string; const ATransaction: TIBCTransaction;
      const AContextOwned: Boolean): TIBCQuery; overload; virtual;

    function Query(const ASQL: string; const ATransaction: TIBCTransaction;
      const AContextOwned: Boolean;
      const AOnBeforeOpen: TProc<TIBCQuery>): TIBCQuery; overload; virtual;

    procedure Query(const ASQL: string; const ATransaction: TIBCTransaction;
      const AOnBeforeOpen: TProc<TIBCQuery>;
      const AOnDataSetReady: TProc<TIBCQuery>); overload; virtual;

    function SetName<T: TComponent>(const AComponent: T; const AName: string): T; overload;

    procedure InTransaction(const ADoSomething: TProc<TIBCTransaction>);

    property Connection: TIBCConnection read GetConnection;
    property ConnectionDefName: string read FConnectionDefName write SetConnectionDefName;
    property Activation: IMARSActivation read FActivation;

    class function LoadConnectionDefs(const AParameters: TMARSParameters;
      const ASliceName: string = ''): TArray<string>;
    class procedure CloseConnectionDefs(const AConnectionDefNames: TArray<string>);
    class function CreateConnectionByDefName(const AConnectionDefName: string;
      const AActivation: IMARSActivation = nil): TIBCConnection;
    class function CreateConnectionByConnectString(const AConnectString: string): TIBCConnection;

    class constructor CreateClass;
    class destructor DestroyClass;
    class procedure AddContextValueProvider(const AContextValueProviderProc: TContextValueProviderProc);
    class property AfterCreateConnection: TAfterCreateConnectionProc read FAfterCreateConnection write FAfterCreateConnection;
  end;

implementation

uses
  StrUtils, Variants
, MARS.Core.Activation
, MARS.Data.IBDAC.InjectionService
, MARS.Data.IBDAC.ReadersAndWriters
;

// Name=Value pairs, separated by ';' (IBDAC connect string format)
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

class function TMARSIBDAC.LoadConnectionDefs(const AParameters: TMARSParameters;
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

function TMARSIBDAC.Query(const ASQL: string;
  const ATransaction: TIBCTransaction): TIBCQuery;
begin
  Result := Query(ASQL, ATransaction, True);
end;

function TMARSIBDAC.Query(const ASQL: string;
  const ATransaction: TIBCTransaction; const AContextOwned: Boolean;
  const AOnBeforeOpen: TProc<TIBCQuery>): TIBCQuery;
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

function TMARSIBDAC.Query(const ASQL: string): TIBCQuery;
begin
  Result := Query(ASQL, nil, True);
end;

procedure TMARSIBDAC.Query(const ASQL: string; const ATransaction: TIBCTransaction;
  const AOnBeforeOpen, AOnDataSetReady: TProc<TIBCQuery>);
var
  LQuery: TIBCQuery;
begin
  LQuery := Query(ASQL, ATransaction, False, AOnBeforeOpen);
  try
    if Assigned(AOnDataSetReady) then
      AOnDataSetReady(LQuery);
  finally
    LQuery.Free;
  end;
end;

function TMARSIBDAC.Query(const ASQL: string;
  const ATransaction: TIBCTransaction; const AContextOwned: Boolean): TIBCQuery;
begin
  Result := Query(ASQL, ATransaction, AContextOwned, nil);
end;

class function TMARSIBDAC.CreateConnectionByConnectString(const AConnectString: string): TIBCConnection;
begin
  Result := TIBCConnection.Create(nil);
  try
    if AConnectString <> '' then
      Result.ConnectString := AConnectString;
    Result.LoginPrompt := False; // after ConnectString, that resets it
  except
    Result.Free;
    raise;
  end;
end;

class function TMARSIBDAC.CreateConnectionByDefName(
  const AConnectionDefName: string; const AActivation: IMARSActivation): TIBCConnection;
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
      raise EMARSIBDACException.CreateFmt('IBDAC connection definition not found: %s', [AConnectionDefName]);
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

{ TMARSIBDAC }

class procedure TMARSIBDAC.AddContextValueProvider(
  const AContextValueProviderProc: TContextValueProviderProc);
begin
  FContextValueProviders := FContextValueProviders + [TContextValueProviderProc(AContextValueProviderProc)];
end;

class procedure TMARSIBDAC.CloseConnectionDefs(
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

constructor TMARSIBDAC.Create(const AConnectionDefName: string;
  const AActivation: IMARSActivation);
begin
  inherited Create();
  ConnectionDefName := AConnectionDefName;
  FActivation := AActivation;
end;

class constructor TMARSIBDAC.CreateClass;
begin
  FContextValueProviders := [];
  FAfterCreateConnection := nil;
  FConnectionDefs := TDictionary<string, string>.Create();
end;

function TMARSIBDAC.CreateCommand(const ASQL: string;
  const ATransaction: TIBCTransaction; const AContextOwned: Boolean): TIBCSQL;
begin
  Result := TIBCSQL.Create(nil);
  try
    Result.Connection := Connection;
    Result.Transaction := ATransaction;
    Result.SQL.Text := ASQL;
    InjectMacroAndParamValues(Result);
    if AContextOwned and Assigned(Activation) then
      Activation.AddToContext(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TMARSIBDAC.CreateQuery(const ASQL: string; const ATransaction: TIBCTransaction;
  const AContextOwned: Boolean; const AName: string): TIBCQuery;
begin
  Result := TIBCQuery.Create(nil);
  try
    Result.Name := AName;
    Result.Connection := Connection;
    Result.Transaction := ATransaction;
    Result.SQL.Text := ASQL;
    InjectMacroAndParamValues(Result);
    if AContextOwned and Assigned(Activation) then
      Activation.AddToContext(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TMARSIBDAC.CreateTransaction(const AContextOwned: Boolean): TIBCTransaction;
begin
  Result := TIBCTransaction.Create(nil);
  try
    // IBDAC starts a transaction only on an active connection
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

destructor TMARSIBDAC.Destroy;
begin
  FreeAndNil(FConnection);
  inherited;
end;

class destructor TMARSIBDAC.DestroyClass;
begin
  FreeAndNil(FConnectionDefs);
end;

function TMARSIBDAC.ExecuteSQL(const ASQL: string; const ATransaction: TIBCTransaction;
  const ABeforeExecute, AAfterExecute: TProc<TIBCSQL>): Integer;
var
  LCommand: TIBCSQL;
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

function TMARSIBDAC.GetConnection: TIBCConnection;
begin
  if not Assigned(FConnection) then
    FConnection := CreateConnectionByDefName(ConnectionDefName, FActivation);
  Result := FConnection;
end;

class function TMARSIBDAC.GetContextValue(const AName: string; const AActivation: IMARSActivation;
  const ADesiredType: TFieldType): TValue;
var
  LCustomProvider: TContextValueProviderProc;
begin
  Result := TMARSActivation.GetValueByName(AName, AActivation);

  if Result.IsEmpty then   // last chance, custom injection
    for LCustomProvider in FContextValueProviders do
      LCustomProvider(AActivation, AName, ADesiredType, Result);
end;

procedure TMARSIBDAC.InjectMacroAndParamValues(
  const ACommand: TCustomDASQL; const AOnlyIfEmpty: Boolean);
begin
  if not Assigned(ACommand) then
    Exit;
  InjectMacroValues(ACommand.Macros, AOnlyIfEmpty);
  InjectParamValues(ACommand.Params, AOnlyIfEmpty);
end;

procedure TMARSIBDAC.InjectMacroAndParamValues(
  const ADataSet: TCustomDADataSet; const AOnlyIfEmpty: Boolean);
begin
  if not Assigned(ADataSet) then
    Exit;
  InjectMacroValues(ADataSet.Macros, AOnlyIfEmpty);
  InjectParamValues(ADataSet.Params, AOnlyIfEmpty);
end;

procedure TMARSIBDAC.InjectMacroValues(const AMacros: TMacros; const AOnlyIfEmpty: Boolean);
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

procedure TMARSIBDAC.InjectParamValues(const AParams: TDAParams; const AOnlyIfEmpty: Boolean);
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

procedure TMARSIBDAC.InTransaction(const ADoSomething: TProc<TIBCTransaction>);
var
  LTransaction: TIBCTransaction;
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

procedure TMARSIBDAC.SetConnectionDefName(const Value: string);
begin
  if FConnectionDefName <> Value then
  begin
    FreeAndNil(FConnection);
    FConnectionDefName := Value;
  end;
end;

function TMARSIBDAC.SetName<T>(const AComponent: T; const AName: string): T;
begin
  AComponent.Name := AName;
  Result := AComponent;
end;

{$ELSE}
interface
implementation
{$ENDIF}

end.
