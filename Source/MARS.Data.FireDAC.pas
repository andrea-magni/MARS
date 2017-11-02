(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Data.FireDAC;

{$I MARS.inc}

interface

uses
    System.Classes, System.SysUtils, Generics.Collections, Rtti

  , Data.DB
  , FireDAC.DApt, FireDAC.DApt.Intf, FireDAC.DatS
  , FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.Stan.Def
  , FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Stan.StorageXML, FireDAC.Stan.Param
  , FireDAC.Stan.StorageJSON, FireDAC.Stan.StorageBin
  , FireDAC.UI.Intf
  , FireDAC.Phys.Intf, FireDAC.Phys
  , FireDAC.Comp.Client, FireDAC.Comp.UI, FireDAC.Comp.DataSet

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
  , MARS.Data.FireDAC.Utils
;

type
  EMARSFireDACException = class(EMARSApplicationException);

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

  TContextValueProviderProc = reference to procedure (const AActivation: IMARSActivation;
    const AName: string; const ADesiredType: TFieldType; out AValue: TValue);


  TMARSFDMemTable = class(TFDMemTable)
  private
    FApplyUpdatesRes: TMARSFDApplyUpdatesRes;
  protected
    function DoApplyUpdates(ATable: TFDDatSTable; AMaxErrors: Integer): Integer; override;
    procedure DoBeforeApplyUpdate; override;
    procedure DoUpdateErrorHandler(ARow: TFDDatSRow; AException: Exception;
      ARequest: TFDUpdateRequest; var AAction: TFDErrorAction); override;
  public
    property ApplyUpdatesRes: TMARSFDApplyUpdatesRes read FApplyUpdatesRes;
  end;

  TMARSFireDAC = class
  private
    FConnectionDefName: string;
    FConnection: TFDConnection;
    FActivation: IMARSActivation;
  protected
    procedure DoUpdateError(ASender: TDataSet; AException: EFDException;
      ARow: TFDDatSRow; ARequest: TFDUpdateRequest; var AAction: TFDErrorAction);
    procedure SetConnectionDefName(const Value: string); virtual;
    function GetConnection: TFDConnection; virtual;
    function GetContextValue(const AName: string; const ADesiredType: TFieldType = ftUnknown): TValue; virtual;
    class var FContextValueProviders: TArray<TContextValueProviderProc>;
  public
    const PARAM_AND_MACRO_DELIMITER = '_';

    procedure InjectParamValues(const ACommand: TFDCustomCommand); virtual;
    procedure InjectMacroValues(const ACommand: TFDCustomCommand); virtual;

    constructor Create(const AConnectionDefName: string;
      const AActivation: IMARSActivation = nil); virtual;
    destructor Destroy; override;

    function ApplyUpdates(const ADelta: TFDMemTable; const ATableAdapter: TFDTableAdapter;
      const AMaxErrors: Integer = -1): TMARSFDApplyUpdatesRes; overload; virtual;

    function ApplyUpdates(ADataSets: TArray<TFDDataSet>; ADeltas: TArray<TFDMemTable>;
//      AOnApplyUpdates: TProc<TFDCustomQuery, Integer, IFDJSONDeltasApplyUpdates> = nil;
      AOnBeforeApplyUpdates: TProc<TFDDataSet, TFDMemTable> = nil
      ): TArray<TMARSFDApplyUpdatesRes>; overload; virtual;

    function CreateCommand(const ASQL: string = ''; const ATransaction: TFDTransaction = nil;
      const AContextOwned: Boolean = True): TFDCommand; virtual;
    function CreateQuery(const ASQL: string = ''; const ATransaction: TFDTransaction = nil;
      const AContextOwned: Boolean = True; const AName: string = 'DataSet'): TFDQuery; virtual;
    function CreateTransaction(const AContextOwned: Boolean = True): TFDTransaction; virtual;

    procedure ExecuteSQL(const ASQL: string; const ATransaction: TFDTransaction = nil;
      const ABeforeExecute: TProc<TFDCommand> = nil;
      const AAfterExecute: TProc<TFDCommand> = nil); virtual;

    function Query(const ASQL: string): TFDQuery; overload; virtual;

    function Query(const ASQL: string;
      const ATransaction: TFDTransaction): TFDQuery; overload; virtual;

    function Query(const ASQL: string; const ATransaction: TFDTransaction;
      const AContextOwned: Boolean): TFDQuery; overload; virtual;

    function Query(const ASQL: string; const ATransaction: TFDTransaction;
      const AContextOwned: Boolean;
      const AOnBeforeOpen: TProc<TFDQuery>): TFDQuery; overload; virtual;

    procedure Query(const ASQL: string; const ATransaction: TFDTransaction;
      const AOnBeforeOpen: TProc<TFDQuery>;
      const AOnDataSetReady: TProc<TFDQuery>); overload; virtual;

    procedure InTransaction(const ADoSomething: TProc<TFDTransaction>);

    property Connection: TFDConnection read GetConnection;
    property ConnectionDefName: string read FConnectionDefName write SetConnectionDefName;
    property Activation: IMARSActivation read FActivation;

    class function LoadConnectionDefs(const AParameters: TMARSParameters;
      const ASliceName: string = ''): TArray<string>;
    class procedure CloseConnectionDefs(const AConnectionDefNames: TArray<string>);
    class function CreateConnectionByDefName(const AConnectionDefName: string): TFDConnection;

    class constructor CreateClass;
    class procedure AddContextValueProvider(const AContextValueProviderProc: TContextValueProviderProc);
  end;

  function MacroDataTypeToFieldType(const AMacroDataType: TFDMacroDataType): TFieldType;

implementation

uses
  StrUtils, Variants
  , MARS.Core.Utils
  , MARS.Core.Exceptions
  , MARS.Data.Utils
  , MARS.Rtti.Utils
  , MARS.Data.FireDAC.InjectionService
  , MARS.Data.FireDAC.ReadersAndWriters
;


function MacroDataTypeToFieldType(const AMacroDataType: TFDMacroDataType): TFieldType;
begin
  case AMacroDataType of
    mdUnknown: Result := ftUnknown;
    mdString: Result := ftString;
    mdIdentifier: Result := ftString; // !!!
    mdInteger: Result := ftInteger;
    mdBoolean: Result := ftBoolean;
    mdFloat: Result := ftFloat;
    mdDate: Result := ftDate;
    mdTime: Result := ftTime;
    mdDateTime: Result := ftDateTime;
    mdRaw: Result := ftUnknown;   // ???
    else
      Result := ftUnknown;
  end;
end;


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

function TMARSFireDAC.Query(const ASQL: string;
  const ATransaction: TFDTransaction): TFDQuery;
begin
  Result := Query(ASQL, ATransaction, True);
end;

function TMARSFireDAC.Query(const ASQL: string;
  const ATransaction: TFDTransaction; const AContextOwned: Boolean;
  const AOnBeforeOpen: TProc<TFDQuery>): TFDQuery;
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

function TMARSFireDAC.Query(const ASQL: string): TFDQuery;
begin
  Result := Query(ASQL, nil, True);
end;

procedure TMARSFireDAC.Query(const ASQL: string; const ATransaction: TFDTransaction;
  const AOnBeforeOpen, AOnDataSetReady: TProc<TFDQuery>);
var
  LQuery: TFDQuery;
begin
  LQuery := Query(ASQL, ATransaction, False, AOnBeforeOpen);
  try
    if Assigned(AOnDataSetReady) then
      AOnDataSetReady(LQuery);
  finally
    LQuery.Free;
  end;
end;

function TMARSFireDAC.Query(const ASQL: string;
  const ATransaction: TFDTransaction; const AContextOwned: Boolean): TFDQuery;
begin
  Result := CreateQuery(ASQL, ATransaction, AContextOwned);
  Result.Open;
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

class procedure TMARSFireDAC.AddContextValueProvider(
  const AContextValueProviderProc: TContextValueProviderProc);
begin
  FContextValueProviders := FContextValueProviders + [TContextValueProviderProc(AContextValueProviderProc)];
end;

function DataSetByName(const ADataSets: TArray<TFDDataSet>; const AName: string): TFDDataSet;
var
  LCurrent: TFDDataSet;
begin
  Result := nil;
  for LCurrent in ADataSets do
    if SameText(LCurrent.Name, AName) then
    begin
      Result := LCurrent;
      Break;
    end;
  if Result = nil then
    raise EMARSFireDACException.CreateFmt('DataSet %s not found', [AName]);
end;

function TMARSFireDAC.ApplyUpdates(const ADelta: TFDMemTable;
  const ATableAdapter: TFDTableAdapter;
  const AMaxErrors: Integer): TMARSFDApplyUpdatesRes;
var
  LFDMemTable: TMARSFDMemTable;
  LFDAdapter: TFDTableAdapter;
begin
  Assert(ATableAdapter <> nil);

  Result.Clear;

  LFDMemTable := TMARSFDMemTable.Create(nil);
  try
    LFDMemTable.Name := ATableAdapter.Name;
    LFDAdapter := TFDTableAdapter.Create(nil);
    try
      if Assigned(ATableAdapter.SelectCommand) then
        LFDAdapter.SelectCommand := ATableAdapter.SelectCommand;
      if Assigned(ATableAdapter.InsertCommand) then
        LFDAdapter.InsertCommand := ATableAdapter.InsertCommand;
      if Assigned(ATableAdapter.UpdateCommand) then
        LFDAdapter.UpdateCommand := ATableAdapter.UpdateCommand;
      if Assigned(ATableAdapter.DeleteCommand) then
        LFDAdapter.DeleteCommand := ATableAdapter.DeleteCommand;

      LFDAdapter.UpdateTableName := ATableAdapter.UpdateTableName;
      if LFDAdapter.UpdateTableName = '' then
        LFDAdapter.UpdateTableName := LFDAdapter.SelectCommand.UpdateOptions.UpdateTableName;

      LFDMemTable.ResourceOptions.StoreItems := [siMeta, siDelta];
      LFDMemTable.CachedUpdates := True;
      LFDMemTable.Adapter := LFDAdapter;
      LFDMemTable.Data := ADelta.Data;
      LFDMemTable.ApplyUpdates(AMaxErrors);
      Result := LFDMemTable.ApplyUpdatesRes;
    finally
      LFDAdapter.Free;
    end;
  finally
    LFDMemTable.Free;
  end;
end;

function TMARSFireDAC.ApplyUpdates(ADataSets: TArray<TFDDataSet>;
  ADeltas: TArray<TFDMemTable>;
  AOnBeforeApplyUpdates: TProc<TFDDataSet, TFDMemTable>): TArray<TMARSFDApplyUpdatesRes>;
var
  LDelta: TFDMemTable;
  LDataSet: TFDAdaptedDataSet;
  LFDAdapter: TFDTableAdapter;

begin
  Result := [];
  for LDelta in ADeltas do
  begin
    LDataSet := DataSetByName(ADataSets, LDelta.Name) as TFDAdaptedDataSet;
    Assert(LDataSet <> nil);
    Assert(LDataSet.Command <> nil);

    InjectMacroValues(LDataSet.Command);
    InjectParamValues(LDataSet.Command);

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

class procedure TMARSFireDAC.CloseConnectionDefs(
  const AConnectionDefNames: TArray<string>);
var
  LConnectionDefName: string;
begin
  for LConnectionDefName in AConnectionDefNames do
    FDManager.CloseConnectionDef(LConnectionDefName);
end;

constructor TMARSFireDAC.Create(const AConnectionDefName: string;
  const AActivation: IMARSActivation);
begin
  inherited Create();
  ConnectionDefName := AConnectionDefName;
  FActivation := AActivation;
end;

class constructor TMARSFireDAC.CreateClass;
begin
  FContextValueProviders := [];
end;

function TMARSFireDAC.CreateCommand(const ASQL: string;
  const ATransaction: TFDTransaction; const AContextOwned: Boolean): TFDCommand;
begin
  Result := TFDCommand.Create(nil);
  try
    Result.Connection := Connection;
    Result.Transaction := ATransaction;
    Result.CommandText.Text := ASQL;
    InjectMacroValues(Result);
    InjectParamValues(Result);
    if AContextOwned then
      Activation.AddToContext(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TMARSFireDAC.CreateQuery(const ASQL: string; const ATransaction: TFDTransaction;
  const AContextOwned: Boolean; const AName: string): TFDQuery;
begin
  Result := TFDQuery.Create(nil);
  try
    Result.Name := AName;
    Result.Connection := Connection;
    Result.Transaction := ATransaction;
    Result.SQL.Text := ASQL;
    InjectMacroValues(Result.Command);
    InjectParamValues(Result.Command);
    if AContextOwned then
      Activation.AddToContext(Result);
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
      Activation.AddToContext(Result);
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

procedure TMARSFireDAC.DoUpdateError(ASender: TDataSet;
  AException: EFDException; ARow: TFDDatSRow; ARequest: TFDUpdateRequest;
  var AAction: TFDErrorAction);
begin

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

function TMARSFireDAC.GetContextValue(const AName: string; const ADesiredType: TFieldType): TValue;
var
  LSubject: string;
  LIdentifier: string;
  LHasArgument: Boolean;
  LArgument: string;
  LNameTokens: TArray<string>;
  LFirstDelim, LSecondDelim: Integer;

  LIndex: Integer;
  LCustomProvider: TContextValueProviderProc;
begin
  Result := TValue.Empty;
  LNameTokens := AName.Split([PARAM_AND_MACRO_DELIMITER]);
  if Length(LNameTokens) < 2 then
    Exit;

  LSubject := LNameTokens[0];
  LIdentifier := LNameTokens[1];
  LHasArgument := Length(LNameTokens) > 2;
  if LHasArgument then
  begin
    LFirstDelim := AName.IndexOf(PARAM_AND_MACRO_DELIMITER);
    LSecondDelim := AName.IndexOf(PARAM_AND_MACRO_DELIMITER, LFirstDelim + Length(PARAM_AND_MACRO_DELIMITER));
    LArgument := AName.Substring(LSecondDelim + 1);
  end;

  if SameText(LSubject, 'Token') then
  begin
    Result := ReadPropertyValue(Activation.Token, LIdentifier);

    if SameText(LIdentifier, 'HasRole') and LHasArgument then
      Result := Activation.Token.HasRole(LArgument)
    else if SameText(LIdentifier, 'Claim') and LHasArgument then
      Result := Activation.Token.Claims.ByNameText(LArgument);
  end
  else if SameText(LSubject, 'PathParam') then
  begin
    LIndex := Activation.URLPrototype.GetPathParamIndex(LIdentifier);
    if (LIndex > -1) and (LIndex < Length(Activation.URL.PathTokens)) then
      Result := Activation.URL.PathTokens[LIndex] { TODO -oAndrea : Try to convert according to ADesiredType }
    else
      raise EMARSFireDACException.CreateFmt('PathParam not found: %s', [LIdentifier]);
  end
  else if SameText(LSubject, 'QueryParam') then
    Result := Activation.URL.QueryTokenByName(LIdentifier)
  else if SameText(LSubject, 'FormParam') then
    Result := Activation.Request.ContentFields.Values[LIdentifier]
  else if SameText(LSubject, 'Request') then
    Result := ReadPropertyValue(Activation.Request, LIdentifier)
//  else if SameText(LSubject, 'Response') then
//    Result := ReadPropertyValue(Activation.Response, LIdentifier)
  else if SameText(LSubject, 'URL') then
    Result := ReadPropertyValue(Activation.URL, LIdentifier)
  else if SameText(LSubject, 'URLPrototype') then
    Result := ReadPropertyValue(Activation.URLPrototype, LIdentifier)
  else // last chance, custom injection
    for LCustomProvider in FContextValueProviders do
      LCustomProvider(Activation, AName, ADesiredType, Result);
end;

procedure TMARSFireDAC.InjectMacroValues(const ACommand: TFDCustomCommand);
var
  LIndex: Integer;
  LMacro: TFDMacro;
begin
  for LIndex := 0 to ACommand.Macros.Count-1 do
  begin
    LMacro := ACommand.Macros[LIndex];
    LMacro.Value := GetContextValue(LMacro.Name, MacroDataTypeToFieldType(LMacro.DataType)).AsVariant;
  end;
end;

procedure TMARSFireDAC.InjectParamValues(const ACommand: TFDCustomCommand);
var
  LIndex: Integer;
  LParam: TFDParam;
begin
  for LIndex := 0 to ACommand.Params.Count-1 do
  begin
    LParam := ACommand.Params[LIndex];
    LParam.Value := GetContextValue(LParam.Name, LParam.DataType).AsVariant;
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


{ TMARSFDMemTable }

function TMARSFDMemTable.DoApplyUpdates(ATable: TFDDatSTable;
  AMaxErrors: Integer): Integer;
begin
  Result := inherited DoApplyUpdates(ATable, AMaxErrors);
  FApplyUpdatesRes.result := Result;
end;

procedure TMARSFDMemTable.DoBeforeApplyUpdate;
begin
  inherited;
  FApplyUpdatesRes := TMARSFDApplyUpdatesRes.Create(Self.Name);
end;

procedure TMARSFDMemTable.DoUpdateErrorHandler(ARow: TFDDatSRow;
  AException: Exception; ARequest: TFDUpdateRequest;
  var AAction: TFDErrorAction);
begin
  inherited;
  FApplyUpdatesRes.AddError(ARow, AException, ARequest);
end;

end.
