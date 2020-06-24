(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Data.UniDAC;

{$I MARS.inc}

interface

uses
    System.Classes, System.SysUtils, Generics.Collections, Rtti, Data.DB
  // Devart UniDAC
  , Uni, UniProvider, VirtualTable
  // MARS
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
  , MARS.Core.Activation.Interfaces
  , MARS.Data.UniDAC.Utils
;

type
  EMARSUniDACException = class(EMARSApplicationException);

  MARSUniDACAttribute = class(TCustomAttribute);

  ConnectionAttribute = class(MARSUniDACAttribute)
  private
    FConnectionDefName: string;
    FExpandMacros: Boolean;
  public
    constructor Create(AConnectionDefName: string; const AExpandMacros: Boolean = False);
    property ConnectionDefName: string read FConnectionDefName;
    property ExpandMacros: Boolean read FExpandMacros;
  end;

  SQLStatementAttribute = class(MARSUniDACAttribute)
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

  TMARSUniMemTable = class(TVirtualTable)
  private
    FApplyUpdatesRes: TMARSUniApplyUpdatesRes;
  protected
//    function DoApplyUpdates(ATable: TVirtualTable; AMaxErrors: Integer): Integer; override;
//    procedure DoBeforeApplyUpdate; override;
//    procedure DoUpdateError(DataSet: TDataSet; E: EDatabaseError;
//  UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction); override;
  public
    property ApplyUpdatesRes: TMARSUniApplyUpdatesRes read FApplyUpdatesRes;
  end;

  TMARSUniDAC = class
  private
    FConnectionDefName: string;
    FConnection: TUniConnection;
    FActivation: IMARSActivation;
    class var FConnectionDefs: TDictionary<string, string>;
  protected
    procedure DoUpdateError(DataSet: TDataSet; E: EDatabaseError;
  UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
    procedure SetConnectionDefName(const Value: string); virtual;
    function GetConnection: TUniConnection; virtual;
    class var FContextValueProviders: TArray<TContextValueProviderProc>;
  public
    const PARAM_AND_MACRO_DELIMITER = '_';

    class function GetContextValue(const AName: string; const AActivation: IMARSActivation;
      const ADesiredType: TFieldType = ftUnknown): TValue; virtual;

    procedure InjectParamValues(const ACommand: TUniSQL;
      const AOnlyIfEmpty: Boolean = True); overload; virtual;
    procedure InjectParamValues(const ACommand: TUniQuery;
      const AOnlyIfEmpty: Boolean = True); overload; virtual;
    procedure InjectMacroValues(const ACommand: TUniSQL;
      const AOnlyIfEmpty: Boolean = True); overload; virtual;
    procedure InjectMacroValues(const ACommand: TUniQuery;
      const AOnlyIfEmpty: Boolean = True); overload; virtual;

    procedure InjectMacroAndParamValues(const ACommand: TUniSQL; const AOnlyIfEmpty: Boolean = True); overload;
    procedure InjectMacroAndParamValues(const ACommand: TUniQuery; const AOnlyIfEmpty: Boolean = True); overload;

    constructor Create(const AConnectionDefName: string;
      const AActivation: IMARSActivation = nil); virtual;
    destructor Destroy; override;

    // Need to understand better before I do coding about below -Ertan
//    function ApplyUpdates(const ADelta: TVirtualTable; const AUniQuery: TUniQuery;
//      const AMaxErrors: Integer = -1): TMARSUniApplyUpdatesRes; overload; virtual;

    // Need to understand better before I do coding about below -Ertan
//    function ApplyUpdates(ADataSets: TArray<TVirtualTable>; ADeltas: TArray<TVirtualTable>;
//      AOnApplyUpdates: TProc<TFDCustomQuery, Integer, IFDJSONDeltasApplyUpdates> = nil;
//      ): TArray<TMARSUniApplyUpdatesRes>; overload; virtual;

    function CreateCommand(const ASQL: string = ''; const ATransaction: TUniTransaction = nil;
      const AContextOwned: Boolean = True): TUniSQL; virtual;
    function CreateQuery(const ASQL: string = ''; const ATransaction: TUniTransaction = nil;
      const AContextOwned: Boolean = True; const AName: string = 'DataSet'): TUniQuery; virtual;
    function CreateTransaction(const AContextOwned: Boolean = True): TUniTransaction; virtual;

    procedure ExecuteSQL(const ASQL: string; const ATransaction: TUniTransaction = nil;
      const ABeforeExecute: TProc<TUniSQL> = nil;
      const AAfterExecute: TProc<TUniSQL> = nil); virtual;

    function Query(const ASQL: string): TUniQuery; overload; virtual;

    function Query(const ASQL: string;
      const ATransaction: TUniTransaction): TUniQuery; overload; virtual;

    function Query(const ASQL: string; const ATransaction: TUniTransaction;
      const AContextOwned: Boolean): TUniQuery; overload; virtual;

    function Query(const ASQL: string; const ATransaction: TUniTransaction;
      const AContextOwned: Boolean;
      const AOnBeforeOpen: TProc<TUniQuery>): TUniQuery; overload; virtual;

    procedure Query(const ASQL: string; const ATransaction: TUniTransaction;
      const AOnBeforeOpen: TProc<TUniQuery>;
      const AOnDataSetReady: TProc<TUniQuery>); overload; virtual;

    function SetName<T: TComponent>(const AComponent: T; const AName: string): T; overload;

    procedure InTransaction(const ADoSomething: TProc<TUniTransaction>);

    property Connection: TUniConnection read GetConnection;
    property ConnectionDefName: string read FConnectionDefName write SetConnectionDefName;
    property Activation: IMARSActivation read FActivation;

    class function LoadConnectionDefs(const AParameters: TMARSParameters;
      const ASliceName: string = ''): TArray<string>;
    class procedure CloseConnectionDefs(const AConnectionDefNames: TArray<string>);
    class function CreateConnectionByDefName(const AConnectionDefName: string;
      const AActivation: IMARSActivation): TUniConnection;
    class function CreateConnectionByConnectString(const AConnectString: string): TUniConnection;

    class constructor CreateClass;
    class destructor DestroyClass;
    class procedure AddContextValueProvider(const AContextValueProviderProc: TContextValueProviderProc);
  end;

implementation

uses
  StrUtils, Variants, DBAccess
  , MARS.Core.Utils
  , MARS.Core.Exceptions
  , MARS.Data.Utils
  , MARS.Rtti.Utils
  , MARS.Data.UniDAC.InjectionService
  , MARS.Data.UniDAC.ReadersAndWriters
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

class function TMARSUniDAC.LoadConnectionDefs(const AParameters: TMARSParameters;
  const ASliceName: string = ''): TArray<string>;
var
  LData, LConnectionParams: TMARSParameters;
  LConnectionDefNames: TArray<string>;
  LConnectionDefName: string;
  LParams: TStrings;
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
        LParams := GetAsTStrings(LConnectionParams);
        try
          LConnectString := LConnectionParams.ByNameText('ConnectString', '').AsString;
          if LConnectString = '' then
          begin
            LParams.Delimiter := ';';
            LParams.QuoteChar := #0;
            LConnectString := LParams.DelimitedText;
          end;

          FConnectionDefs.AddOrSetValue(LConnectionDefName, LConnectString);

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

function TMARSUniDAC.Query(const ASQL: string;
  const ATransaction: TUniTransaction): TUniQuery;
begin
  Result := Query(ASQL, ATransaction, True);
end;

function TMARSUniDAC.Query(const ASQL: string;
  const ATransaction: TUniTransaction; const AContextOwned: Boolean;
  const AOnBeforeOpen: TProc<TUniQuery>): TUniQuery;
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

function TMARSUniDAC.Query(const ASQL: string): TUniQuery;
begin
  Result := Query(ASQL, nil, True);
end;

procedure TMARSUniDAC.Query(const ASQL: string; const ATransaction: TUniTransaction;
  const AOnBeforeOpen, AOnDataSetReady: TProc<TUniQuery>);
var
  LQuery: TUniQuery;
begin
  LQuery := Query(ASQL, ATransaction, False, AOnBeforeOpen);
  try
    if Assigned(AOnDataSetReady) then
      AOnDataSetReady(LQuery);
  finally
    LQuery.Free;
  end;
end;

function TMARSUniDAC.Query(const ASQL: string;
  const ATransaction: TUniTransaction; const AContextOwned: Boolean): TUniQuery;
begin
  Result := CreateQuery(ASQL, ATransaction, AContextOwned);
  Result.Open;
end;

class function TMARSUniDAC.CreateConnectionByConnectString(const AConnectString: string): TUniConnection;
begin
  Result := TUniConnection.Create(nil);
  try
    Result.ConnectString := AConnectString;
  except
    Result.Free();
    raise;
  end;
end;

class function TMARSUniDAC.CreateConnectionByDefName(
  const AConnectionDefName: string; const AActivation: IMARSActivation): TUniConnection;
var
  LConnectString: string;
begin
  //AM TDictionary is not thread-safe but connection definitions are not supposed
  // to change during server execution. A monitor object would be a safer choice, if
  // errors should arise.
  Result := nil;
  if FConnectionDefs.TryGetValue(AConnectionDefName, LConnectString) then
    Result := CreateConnectionByConnectString(LConnectString)
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

{ TMARSUniDAC }

class procedure TMARSUniDAC.AddContextValueProvider(
  const AContextValueProviderProc: TContextValueProviderProc);
begin
  FContextValueProviders := FContextValueProviders + [TContextValueProviderProc(AContextValueProviderProc)];
end;

function DataSetByName(const ADataSets: TArray<TVirtualTable>; const AName: string): TVirtualTable;
var
  LCurrent: TVirtualTable;
begin
  Result := nil;
  for LCurrent in ADataSets do
    if SameText(LCurrent.Name, AName) then
    begin
      Result := LCurrent;
      Break;
    end;
  if Result = nil then
    raise EMARSUniDACException.CreateFmt('DataSet %s not found', [AName]);
end;

// Need to understand ApplyUpdates in MARS better before trying to convert below code lines -Ertan
(*
function TMARSUniDAC.ApplyUpdates(const ADelta: TMARSUniMemTable;
  const AUniQuery: TUniQuery;
  const AMaxErrors: Integer): TMARSUniApplyUpdatesRes;
var
  LUniMemTable: TMARSUniMemTable;
  LUniQuery: TUniQuery;
begin
  Assert(AUniQuery <> nil);

  Result.Clear;

  LUniMemTable := TMARSUniMemTable.Create(nil);
  try
    LUniMemTable.Name := AUniQuery.Name;
    LUniQuery := TUniQuery.Create(nil);
    try
      if Assigned(AUniQuery.SQL) then
        LUniQuery.SQL := AUniQuery.SQL;
      if Assigned(AUniQuery.SQLInsert) then
        LUniQuery.SQLInsert := AUniQuery.SQLInsert;
      if Assigned(AUniQuery.SQLUpdate) then
        LUniQuery.SQLUpdate := AUniQuery.SQLUpdate;
      if Assigned(AUniQuery.SQLDelete) then
        LUniQuery.SQLDelete := AUniQuery.SQLDelete;

      LUniQuery.UpdatingTable := AUniQuery.UpdatingTable;
      if LUniQuery.UpdatingTable = '' then
        LUniQuery.UpdatingTable := LUniQuery.SQL.UpdateOptions.UpdateTableName;

      LUniMemTable.ResourceOptions.StoreItems := [siMeta, siDelta];
      LUniMemTable.CachedUpdates := True;
      LUniMemTable.Adapter := LUniQuery;
      LUniMemTable.Data := ADelta.Data;
      LUniMemTable.ApplyUpdates(AMaxErrors);
      Result := LUniMemTable.ApplyUpdatesRes;
    finally
      LUniQuery.Free;
    end;
  finally
    LUniMemTable.Free;
  end;
end;

function TMARSUniDAC.ApplyUpdates(ADataSets: TArray<TVirtualTable>;
  ADeltas: TArray<TVirtualTable>;
  AOnBeforeApplyUpdates: TProc<TVirtualTable, TVirtualTable>): TArray<TMARSUniApplyUpdatesRes>;
var
  LDelta: TVirtualTable;
  LDataSet: TFDAdaptedDataSet;
  LFDAdapter: TFDTableAdapter;

begin
  Result := [];
  for LDelta in ADeltas do
  begin
    LDataSet := DataSetByName(ADataSets, LDelta.Name) as TFDAdaptedDataSet;
    Assert(LDataSet <> nil);
    Assert(LDataSet.Command <> nil);

    InjectMacroAndParamValues(LDataSet.Command);

    if Assigned(AOnBeforeApplyUpdates) then
      AOnBeforeApplyUpdates(LDataSet, LDelta);

    LFDAdapter := TFDTableAdapter.Create(nil);
    try
      LFDAdapter.Name := LDataSet.Name;
      LFDAdapter.SelectCommand := LDataSet.Command;
      Result := Result + [ApplyUpdates(LDelta, LFDAdapter, -1)];
    finally
      LFDAdapter.Free;
    end;
  end;
end;
*)

class procedure TMARSUniDAC.CloseConnectionDefs(
  const AConnectionDefNames: TArray<string>);
begin
  FConnectionDefs.Clear;
end;

constructor TMARSUniDAC.Create(const AConnectionDefName: string;
  const AActivation: IMARSActivation);
begin
  inherited Create();
  ConnectionDefName := AConnectionDefName;
  FActivation := AActivation;
end;

class constructor TMARSUniDAC.CreateClass;
begin
  FContextValueProviders := [];
  FConnectionDefs := TDictionary<string, string>.Create();
end;

function TMARSUniDAC.CreateCommand(const ASQL: string;
  const ATransaction: TUniTransaction; const AContextOwned: Boolean): TUniSQL;
begin
  Result := TUniSQL.Create(nil);
  try
    Result.Connection := Connection;
    Result.Transaction := ATransaction;
    Result.SQL.Text := ASQL;
    InjectMacroAndParamValues(Result);
    if AContextOwned then
      Activation.AddToContext(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TMARSUniDAC.CreateQuery(const ASQL: string; const ATransaction: TUniTransaction;
  const AContextOwned: Boolean; const AName: string): TUniQuery;
begin
  Result := TUniQuery.Create(nil);
  try
    Result.Name := AName;
    Result.Connection := Connection;
    Result.Transaction := ATransaction;
    Result.SQL.Text := ASQL;
    InjectMacroAndParamValues(Result);
    if AContextOwned then
      Activation.AddToContext(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TMARSUniDAC.CreateTransaction(const AContextOwned: Boolean): TUniTransaction;
begin
  Result := TUniTransaction.Create(nil);
  try
    Result.DefaultConnection := Connection;
    if AContextOwned then
      Activation.AddToContext(Result);
  except
    Result.Free;
    raise;
  end;
end;

destructor TMARSUniDAC.Destroy;
begin
  FreeAndNil(FConnection);
  inherited;
end;

class destructor TMARSUniDAC.DestroyClass;
begin
  FreeAndNil(FConnectionDefs);
end;

procedure TMARSUniDAC.DoUpdateError(DataSet: TDataSet; E: EDatabaseError;
  UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
begin

end;

procedure TMARSUniDAC.ExecuteSQL(const ASQL: string; const ATransaction: TUniTransaction;
  const ABeforeExecute, AAfterExecute: TProc<TUniSQL>);
var
  LCommand: TUniSQL;
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

function TMARSUniDAC.GetConnection: TUniConnection;
begin
  if not Assigned(FConnection) then
    FConnection := CreateConnectionByDefName(ConnectionDefName, Activation);
  Result := FConnection;
end;

class function TMARSUniDAC.GetContextValue(const AName: string; const AActivation: IMARSActivation;
  const ADesiredType: TFieldType): TValue;
var
  LFirstToken, LSecondToken: string;
  LHasThirdToken: Boolean;
  LSecondTokenAndAll, LThirdTokenAndAll: string;
  LNameTokens: TArray<string>;
  LFirstDelim, LSecondDelim: Integer;

  LIndex: Integer;
  LCustomProvider: TContextValueProviderProc;
begin
  Result := TValue.Empty;
  LNameTokens := AName.Split([PARAM_AND_MACRO_DELIMITER]);
  if Length(LNameTokens) < 2 then
    Exit;

  LFirstToken := LNameTokens[0];
  LSecondToken := LNameTokens[1];
  LFirstDelim := AName.IndexOf(PARAM_AND_MACRO_DELIMITER);
  LSecondTokenAndAll := AName.Substring(LFirstDelim + 1);
  LHasThirdToken := Length(LNameTokens) > 2;
  if LHasThirdToken then
  begin
    LSecondDelim := AName.IndexOf(PARAM_AND_MACRO_DELIMITER, LFirstDelim + Length(PARAM_AND_MACRO_DELIMITER));
    LThirdTokenAndAll := AName.Substring(LSecondDelim + 1);
  end;

  if SameText(LFirstToken, 'Token') then
  begin
    Result := ReadPropertyValue(AActivation.Token, LSecondToken);

    if SameText(LSecondToken, 'HasRole') and LHasThirdToken then
      Result := AActivation.Token.HasRole(LThirdTokenAndAll)
    else if SameText(LSecondToken, 'Claim') and LHasThirdToken then
      Result := AActivation.Token.Claims.ByNameText(LThirdTokenAndAll);
  end
  else if SameText(LFirstToken, 'PathParam') then
  begin
    LIndex := AActivation.URLPrototype.GetPathParamIndex(LSecondTokenAndAll);
    if (LIndex > -1) and (LIndex < Length(AActivation.URL.PathTokens)) then
      Result := AActivation.URL.PathTokens[LIndex] { TODO -oAndrea : Try to convert according to ADesiredType }
    else
      raise EMARSUniDACException.CreateFmt('PathParam not found: %s', [LSecondTokenAndAll]);
  end
  else if SameText(LFirstToken, 'QueryParam') then
    Result := AActivation.URL.QueryTokenByName(LSecondTokenAndAll)
  else if SameText(LFirstToken, 'FormParam') then
    Result := AActivation.Request.GetFormParamValue(LSecondTokenAndAll)
  else if SameText(LFirstToken, 'Request') then
    Result := ReadPropertyValue(AActivation.Request.AsObject, LSecondTokenAndAll)
//  else if SameText(LFirstToken, 'Response') then
//    Result := ReadPropertyValue(AActivation.Response, LSecondToken)
  else if SameText(LFirstToken, 'URL') then
    Result := ReadPropertyValue(AActivation.URL, LSecondTokenAndAll)
  else if SameText(LFirstToken, 'URLPrototype') then
    Result := ReadPropertyValue(AActivation.URLPrototype, LSecondTokenAndAll)
  else // last chance, custom injection
    for LCustomProvider in FContextValueProviders do
      LCustomProvider(AActivation, AName, ADesiredType, Result);
end;

procedure TMARSUniDAC.InjectMacroAndParamValues(
  const ACommand: TUniSQL; const AOnlyIfEmpty: Boolean);
begin
  InjectMacroValues(ACommand, AOnlyIfEmpty);
  InjectParamValues(ACommand, AOnlyIfEmpty);
end;

procedure TMARSUniDAC.InjectMacroAndParamValues(
  const ACommand: TUniQuery; const AOnlyIfEmpty: Boolean);
begin
  InjectMacroValues(ACommand, AOnlyIfEmpty);
  InjectParamValues(ACommand, AOnlyIfEmpty);
end;

procedure TMARSUniDAC.InjectMacroValues(const ACommand: TUniSQL; const AOnlyIfEmpty: Boolean);
var
  LIndex: Integer;
  LMacro: TMacro;
begin
  for LIndex := 0 to ACommand.Macros.Count-1 do
  begin
    LMacro := ACommand.Macros[LIndex];
    if (not AOnlyIfEmpty) or (LMacro.Value = '') then
      LMacro.Value := GetContextValue(LMacro.Name, Activation).AsVariant;
  end;
end;

procedure TMARSUniDAC.InjectMacroValues(const ACommand: TUniQuery; const AOnlyIfEmpty: Boolean);
var
  LIndex: Integer;
  LMacro: TMacro;
begin
  for LIndex := 0 to ACommand.Macros.Count-1 do
  begin
    LMacro := ACommand.Macros[LIndex];
    if (not AOnlyIfEmpty) or (LMacro.Value = '') then
      LMacro.Value := GetContextValue(LMacro.Name, Activation).AsVariant;
  end;
end;

procedure TMARSUniDAC.InjectParamValues(const ACommand: TUniSQL; const AOnlyIfEmpty: Boolean);
var
  LIndex: Integer;
  LParam: TUniParam;
begin
  for LIndex := 0 to ACommand.Params.Count-1 do
  begin
    LParam := ACommand.Params[LIndex];
    if ((not AOnlyIfEmpty) or LParam.IsNull) then
      LParam.Value := GetContextValue(LParam.Name, Activation, LParam.DataType).AsVariant;
  end;
end;

procedure TMARSUniDAC.InjectParamValues(const ACommand: TUniQuery; const AOnlyIfEmpty: Boolean);
var
  LIndex: Integer;
  LParam: TUniParam;
begin
  for LIndex := 0 to ACommand.Params.Count-1 do
  begin
    LParam := ACommand.Params[LIndex];
    if ((not AOnlyIfEmpty) or LParam.IsNull) then
      LParam.Value := GetContextValue(LParam.Name, Activation, LParam.DataType).AsVariant;
  end;
end;

procedure TMARSUniDAC.InTransaction(const ADoSomething: TProc<TUniTransaction>);
var
  LTransaction: TUniTransaction;
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

procedure TMARSUniDAC.SetConnectionDefName(const Value: string);
begin
  if FConnectionDefName <> Value then
  begin
    FreeAndNil(FConnection);
    FConnectionDefName := Value;
  end;
end;

function TMARSUniDAC.SetName<T>(const AComponent: T; const AName: string): T;
begin
  AComponent.Name := AName;
  Result := AComponent;
end;

{ TMARSFDMemTable }

// Need to understand better before doing anything below -Ertan
(*
function TMARSFDMemTable.DoApplyUpdates(ATable: TFDDatSTable;
  AMaxErrors: Integer): Integer;
begin
  Result := inherited DoApplyUpdates(ATable, AMaxErrors);
  FApplyUpdatesRes.result := Result;
end;

procedure TMARSFDMemTable.DoBeforeApplyUpdate;
begin
  inherited;
  FApplyUpdatesRes := TMARSUniApplyUpdatesRes.Create(Self.Name);
end;

procedure TMARSUniMemTable.DoUpdateErrorHandler(DataSet: TDataSet; E: EDatabaseError;
  UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
begin
  inherited;
  FApplyUpdatesRes.AddError(ARow, AException, ARequest);
end;
*)

end.
