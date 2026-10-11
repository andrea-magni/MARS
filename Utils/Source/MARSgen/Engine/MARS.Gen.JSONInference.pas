(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Gen.JSONInference;

// Infers the schemas of JSON samples (objects or arrays) into the model of MARSgen, so that the
// same mapping and emitter of OpenAPI documents (MARS.Gen.Types) generate their Delphi types.
//
// More samples are merged: a key missing in some of them is optional, integers and decimals give
// numbers, objects seen with different keys get their union. Strings that are always ISO 8601
// dates (or date-times, or GUIDs) get that format. Nested objects are named after their key
// (address -> Address, items -> Item), the root after the given name.

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  MARS.Gen.Model;

type
  TGenInferenceOptions = record
    // the name of the root type (without T): Root by default
    RootName: string;
    // ISO 8601 strings as dates (TDateTime, TDate)
    DetectDates: Boolean;
    // nested objects named after their key (Address) instead of after their parent (OrderAddress)
    NameAfterKeys: Boolean;
    class function Default: TGenInferenceOptions; static;
  end;

  TGenSampleKind = (Null, Boolean, Integer, Number, &String, &Array, &Object);
  TGenSampleKinds = set of TGenSampleKind;

  // what is known about one position of the samples ($.items[].price), merged
  TGenSampleNode = class
  private
    FKinds: TGenSampleKinds;
    FSeen: Integer;
    FObjects: Integer;
    FArrays: Integer;
    FArrayElements: Integer;
    FStrings, FDateTimes, FDates, FGuids: Integer;
    FBigIntegers, FHugeIntegers: Boolean;
    FKeys: TStringList;
    FKeySeen: TDictionary<string, Integer>;
    FProperties: TObjectDictionary<string, TGenSampleNode>;
    FItems: TGenSampleNode;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AValue: TJSONValue);
  end;

  TGenJSONInference = class
  private
    FOptions: TGenInferenceOptions;
    FRoot: TGenSampleNode;
    FSamples: Integer;
    FApi: TGenApi;
    function ToSchema(const ANode: TGenSampleNode; const APointer, AName: string): TGenSchema;
  public
    constructor Create(const AOptions: TGenInferenceOptions);
    destructor Destroy; override;

    procedure AddSample(const AValue: TJSONValue);
    procedure AddSampleText(const AText: string);
    procedure AddSampleFile(const AFileName: string);

    // the model of the samples (the caller owns it): one named schema per object type
    function Execute: TGenApi;

    // one or more files, a model ready for TGenTypeMapper
    class function FromFiles(const AFileNames: TArray<string>; const AOptions: TGenInferenceOptions): TGenApi; static;
  end;

implementation

uses
  System.RegularExpressions,
  MARS.Gen.JSONSource, MARS.Gen.Naming;

const
  RE_DATE_TIME = '^\d{4}-\d{2}-\d{2}[Tt ]\d{2}:\d{2}(:\d{2}(\.\d+)?)?([Zz]|[+-]\d{2}:?\d{2})?$';
  RE_DATE = '^\d{4}-\d{2}-\d{2}$';
  RE_GUID = '^\{?[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}\}?$';

var
  // compiled once
  GDateTime, GDate, GGuid: TRegEx;

{ TGenInferenceOptions }

class function TGenInferenceOptions.Default: TGenInferenceOptions;
begin
  Result.RootName := 'Root';
  Result.DetectDates := True;
  Result.NameAfterKeys := True;
end;

{ TGenSampleNode }

constructor TGenSampleNode.Create;
begin
  inherited Create;
  FKeys := TStringList.Create;
  FKeySeen := TDictionary<string, Integer>.Create;
  FProperties := TObjectDictionary<string, TGenSampleNode>.Create([doOwnsValues]);
end;

destructor TGenSampleNode.Destroy;
begin
  FItems.Free;
  FProperties.Free;
  FKeySeen.Free;
  FKeys.Free;
  inherited;
end;

procedure TGenSampleNode.Add(const AValue: TJSONValue);
var
  LText: string;
  LInt64: Int64;
  LChild: TGenSampleNode;
  LCount: Integer;
begin
  Inc(FSeen);
  if (AValue = nil) or (AValue is TJSONNull) then
    Include(FKinds, TGenSampleKind.Null)

  else if AValue is TJSONBool then
    Include(FKinds, TGenSampleKind.Boolean)

  else if AValue is TJSONNumber then
  begin
    LText := TJSONNumber(AValue).Value;
    if LText.IndexOfAny(['.', 'e', 'E']) >= 0 then
      Include(FKinds, TGenSampleKind.Number)
    else if TryStrToInt64(LText, LInt64) then
    begin
      Include(FKinds, TGenSampleKind.Integer);
      if (LInt64 > High(Integer)) or (LInt64 < Low(Integer)) then
        FBigIntegers := True;
    end
    else
    begin
      // beyond Int64: a number
      Include(FKinds, TGenSampleKind.Number);
      FHugeIntegers := True;
    end;
  end

  else if AValue is TJSONString then
  begin
    Include(FKinds, TGenSampleKind.String);
    LText := TJSONString(AValue).Value;
    Inc(FStrings);
    if GDateTime.IsMatch(LText) then
      Inc(FDateTimes)
    else if GDate.IsMatch(LText) then
      Inc(FDates);
    if GGuid.IsMatch(LText) then
      Inc(FGuids);
  end

  else if AValue is TJSONArray then
  begin
    Include(FKinds, TGenSampleKind.Array);
    Inc(FArrays);
    for var LElement in TJSONArray(AValue) do
    begin
      if not Assigned(FItems) then
        FItems := TGenSampleNode.Create;
      FItems.Add(LElement);
      Inc(FArrayElements);
    end;
  end

  else if AValue is TJSONObject then
  begin
    Include(FKinds, TGenSampleKind.Object);
    Inc(FObjects);
    for var LPair in TJSONObject(AValue) do
    begin
      if not FProperties.TryGetValue(LPair.JsonString.Value, LChild) then
      begin
        LChild := TGenSampleNode.Create;
        FProperties.Add(LPair.JsonString.Value, LChild);
        // the order of first appearance
        FKeys.Add(LPair.JsonString.Value);
      end;
      LChild.Add(LPair.JsonValue);
      if not FKeySeen.TryGetValue(LPair.JsonString.Value, LCount) then
        LCount := 0;
      FKeySeen.AddOrSetValue(LPair.JsonString.Value, LCount + 1);
    end;
  end;
end;

{ TGenJSONInference }

constructor TGenJSONInference.Create(const AOptions: TGenInferenceOptions);
begin
  inherited Create;
  FOptions := AOptions;
  if FOptions.RootName = '' then
    FOptions.RootName := 'Root';
  FRoot := TGenSampleNode.Create;
end;

destructor TGenJSONInference.Destroy;
begin
  FRoot.Free;
  inherited;
end;

procedure TGenJSONInference.AddSample(const AValue: TJSONValue);
begin
  FRoot.Add(AValue);
  Inc(FSamples);
end;

procedure TGenJSONInference.AddSampleText(const AText: string);
var
  LValue: TJSONValue;
begin
  LValue := TGenJSONSource.FromText(AText);
  try
    AddSample(LValue);
  finally
    LValue.Free;
  end;
end;

procedure TGenJSONInference.AddSampleFile(const AFileName: string);
var
  LValue: TJSONValue;
begin
  LValue := TGenJSONSource.Load(AFileName);
  try
    AddSample(LValue);
  finally
    LValue.Free;
  end;
end;

function TGenJSONInference.ToSchema(const ANode: TGenSampleNode; const APointer, AName: string): TGenSchema;
var
  LKinds: TGenSampleKinds;
  LSeen: Integer;
  LChildName: string;
begin
  Result := FApi.NewSchema(APointer);
  LKinds := ANode.FKinds - [TGenSampleKind.Null];
  if TGenSampleKind.Null in ANode.FKinds then
    Result.Types := [TGenSchemaType.Null];

  // integers and decimals: numbers
  if LKinds = [TGenSampleKind.Integer, TGenSampleKind.Number] then
    LKinds := [TGenSampleKind.Number];

  if LKinds = [] then
  begin
    // only null: nothing to tell the type
    Result.IsAny := True;
    Result.Description := 'Always null in the samples.';
    FApi.AddDiagnostic(TGenSeverity.Warning, APointer, 'always null in the samples: the type is unknown');
  end
  else if (LKinds <> [TGenSampleKind.Boolean]) and (LKinds <> [TGenSampleKind.Integer])
    and (LKinds <> [TGenSampleKind.Number]) and (LKinds <> [TGenSampleKind.String])
    and (LKinds <> [TGenSampleKind.Array]) and (LKinds <> [TGenSampleKind.Object])
  then
  begin
    // i.e. a string here, a number there
    Result.IsAny := True;
    Result.Description := 'Values of different kinds in the samples.';
    FApi.AddDiagnostic(TGenSeverity.Warning, APointer, 'values of different kinds in the samples');
  end
  else if LKinds = [TGenSampleKind.Boolean] then
    Result.Types := Result.Types + [TGenSchemaType.Boolean]
  else if LKinds = [TGenSampleKind.Integer] then
  begin
    Result.Types := Result.Types + [TGenSchemaType.Integer];
    if ANode.FBigIntegers then
      Result.Format := 'int64';
  end
  else if LKinds = [TGenSampleKind.Number] then
    Result.Types := Result.Types + [TGenSchemaType.Number]
  else if LKinds = [TGenSampleKind.String] then
  begin
    Result.Types := Result.Types + [TGenSchemaType.String];
    // a format only when every string has it
    if FOptions.DetectDates and (ANode.FDateTimes = ANode.FStrings) then
      Result.Format := 'date-time'
    else if FOptions.DetectDates and (ANode.FDates = ANode.FStrings) then
      Result.Format := 'date'
    else if ANode.FGuids = ANode.FStrings then
      Result.Format := 'uuid';
  end
  else if LKinds = [TGenSampleKind.Array] then
  begin
    Result.Types := Result.Types + [TGenSchemaType.Array];
    if Assigned(ANode.FItems) then
    begin
      // the root array: its items are the root type; elsewhere items -> Item
      LChildName := TGenNaming.Singular(AName);
      if APointer = '#' then
        LChildName := AName
      else if SameText(LChildName, AName) then
        LChildName := AName + 'Item';
      Result.Items := ToSchema(ANode.FItems, APointer + '/[]', LChildName);
    end
    else
    begin
      Result.Description := 'Always an empty array in the samples.';
      FApi.AddDiagnostic(TGenSeverity.Warning, APointer, 'always an empty array in the samples: the type of the items is unknown');
    end;
  end
  else if LKinds = [TGenSampleKind.Object] then
  begin
    Result.Types := Result.Types + [TGenSchemaType.Object];
    // a type of its own, named after its key (or after the root)
    Result.Name := AName;
    FApi.Schemas.Add(Result);
    if ANode.FKeys.Count = 0 then
      FApi.AddDiagnostic(TGenSeverity.Warning, APointer, 'always an empty object in the samples');
    for var LKey in ANode.FKeys do
    begin
      if FOptions.NameAfterKeys then
        LChildName := TGenNaming.PascalCase(LKey)
      else
        LChildName := AName + TGenNaming.PascalCase(LKey);
      if LChildName = '' then
        LChildName := AName + 'Value';
      LSeen := ANode.FKeySeen[LKey];
      Result.AddProperty(LKey
      , ToSchema(ANode.FProperties[LKey], APointer + '/' + LKey.Replace('~', '~0').Replace('/', '~1'), LChildName)
      // in every object of this position
      , LSeen = ANode.FObjects);
    end;
  end;

  if Result.IsAny and (TGenSampleKind.Null in ANode.FKinds) then
    Result.Types := [TGenSchemaType.Null];
  if ANode.FHugeIntegers then
    FApi.AddDiagnostic(TGenSeverity.Info, APointer, 'integers beyond Int64: a number');
end;

function TGenJSONInference.Execute: TGenApi;
begin
  if FSamples = 0 then
    raise EGenError.Create('No JSON samples');

  FApi := TGenApi.Create;
  try
    FApi.Title := FOptions.RootName;
    ToSchema(FRoot, '#', TGenNaming.PascalCase(FOptions.RootName));
    if not (FRoot.FKinds * [TGenSampleKind.Object, TGenSampleKind.Array] <> []) then
      FApi.AddDiagnostic(TGenSeverity.Warning, '#', 'the samples are not objects or arrays: no types');
    Result := FApi;
    FApi := nil;
  except
    FreeAndNil(FApi);
    raise;
  end;
end;

class function TGenJSONInference.FromFiles(const AFileNames: TArray<string>;
  const AOptions: TGenInferenceOptions): TGenApi;
var
  LInference: TGenJSONInference;
begin
  LInference := TGenJSONInference.Create(AOptions);
  try
    for var LFileName in AFileNames do
      LInference.AddSampleFile(LFileName);
    Result := LInference.Execute;
  finally
    LInference.Free;
  end;
end;

initialization
  GDateTime := TRegEx.Create(RE_DATE_TIME);
  GDate := TRegEx.Create(RE_DATE);
  GGuid := TRegEx.Create(RE_GUID);

end.
