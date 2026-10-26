unit MARS.YAML.ReadersAndWriters;

interface

{$I MARS.INC}

// Without MARS_YAML (Linux, 64-bit macOS: see MARS.inc) the unit is empty, so listing it in a
// uses clause does not break the build there; no YAML writer is registered.
{$IFDEF MARS_YAML}
uses
  Classes, SysUtils, Rtti
, MARS.Core.Attributes, MARS.Core.Declarations, MARS.Core.MediaType
, MARS.Core.MessageBodyWriter, MARS.Core.Activation.Interfaces, MARS.Core.JSON
, Neslib.Yaml, Neslib.SysUtils, Neslib.Utf8
;

type
  [Produces(TMediaType.APPLICATION_YAML)]
  TYAMLObjectWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(TMediaType.APPLICATION_YAML)]
  TYAMLArrayOfObjectWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(TMediaType.APPLICATION_YAML)]
  TYAMLRecordWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(TMediaType.APPLICATION_YAML)]
  TYAMLArrayOfRecordWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(TMediaType.APPLICATION_YAML)]
  TYAMLValueWriter = class(TInterfacedObject, IMessageBodyWriter)
  protected
  public
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);

    class procedure WriteYAMLValue(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation); inline;
  end;

// -------------------------------------------------------------------------------------
  TYAMLRawString = type string;

  TToRecordFilterProc = reference to procedure (const AMember: TRttiMember;
    const AValue: TValue; const AYAML: TYamlNode; var AAccept: Boolean);

  TToYAMLFilterProc = reference to procedure (const AMember: TRttiMember;
    const AValue: TValue; const AYAML: TYamlNode; var AAccept: Boolean);

  YAMLNameAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
  end;

  // AOptions: the JSON serialization options (dates, empty values; the writers use the ones of
  // the activation); without AOptions: DefaultMARSJSONSerializationOptions
  TMARSYAML = class
  private
  public
    class function DictionaryToYAML(const ARoot: TYamlNode; const AName: string; const ADictionary: TObject): Boolean; overload;
    class function DictionaryToYAML(const ARoot: TYamlNode; const AName: string; const ADictionary: TObject; const AOptions: TMARSJSONSerializationOptions): Boolean; overload;
    class function ListOfPairsToYAML(const ARoot: TYamlNode; const AName: string; const AList: TObject): Boolean; overload;
    class function ListOfPairsToYAML(const ARoot: TYamlNode; const AName: string; const AList: TObject; const AOptions: TMARSJSONSerializationOptions): Boolean; overload;
    class function ObjectListToYAML(const ARoot: TYamlNode; const AName: string; const AObjectList: TObject): Boolean; overload;
    class function ObjectListToYAML(const ARoot: TYamlNode; const AName: string; const AObjectList: TObject; const AOptions: TMARSJSONSerializationOptions): Boolean; overload;
    class function ObjectToYAML(const AObject: TObject; const AFilterProc: TToYAMLFilterProc = nil): IYamlDocument; overload;
    class function ObjectToYAML(const AObject: TObject; const AOptions: TMARSJSONSerializationOptions; const AFilterProc: TToYAMLFilterProc = nil): IYamlDocument; overload;
    class function ObjectToYAML(const ARoot: TYamlNode; const AName: string; const AObject: TObject; const AFilterProc: TToYAMLFilterProc = nil): Boolean; overload;
    class function ObjectToYAML(const ARoot: TYamlNode; const AName: string; const AObject: TObject; const AOptions: TMARSJSONSerializationOptions; const AFilterProc: TToYAMLFilterProc = nil): Boolean; overload;
    class function RecordToYAML(const ARecord: TValue; const AFilterProc: TToYAMLFilterProc = nil): IYamlDocument; overload;
    class function RecordToYAML(const ARecord: TValue; const AOptions: TMARSJSONSerializationOptions; const AFilterProc: TToYAMLFilterProc = nil): IYamlDocument; overload;
    class function RecordToYAML(const ARoot: TYamlNode; const AName: string; const ARecord: TValue; const AFilterProc: TToYAMLFilterProc = nil): Boolean; overload;
    class function RecordToYAML(const ARoot: TYamlNode; const AName: string; const ARecord: TValue; const AOptions: TMARSJSONSerializationOptions; const AFilterProc: TToYAMLFilterProc = nil): Boolean; overload;

//    class function YAMLToRecord<T: record>(const AYAML: TYamlNode; const AFilterProc: TToRecordFilterProc = nil): T; overload;
//    class function YAMLToRecord(const AYAML: TYamlNode; const ARecordType: TRttiType; const AFilterProc: TToRecordFilterProc = nil): TValue; overload;
//    class function YAMLToObject<T: class>(const AYAML: TYamlNode; const AFilterProc: TToRecordFilterProc = nil): T; overload;
//    class function YAMLToObject(const AYAML: TYamlNode; const AObjectType: TRttiType; const AFilterProc: TToRecordFilterProc = nil): TValue; overload;

    class function TValueToYAML(const AValue: TValue): IYamlDocument; overload;
    class function TValueToYAML(const AValue: TValue; const AOptions: TMARSJSONSerializationOptions): IYamlDocument; overload;
    class function TValueToYaml(const ARoot: TYamlNode; const AKeyName: string; const AValue: TValue): Boolean; overload;
    class function TValueToYaml(const ARoot: TYamlNode; const AKeyName: string; const AValue: TValue; const AOptions: TMARSJSONSerializationOptions): Boolean; overload;
  end;
{$ENDIF}

implementation

{$IFDEF MARS_YAML}
uses
  System.TypInfo, DateUtils, StrUtils, Generics.Collections
, MARS.Core.Utils, MARS.Rtti.Utils
;

type
  TYAMLEmptyValue = (EmptyString, EmptyNumber, EmptyBoolean, EmptyArray, EmptyObject);

// the SkipEmpty* option of the JSON for a member with an empty value
function SkipEmpty(const AOptions: TMARSJSONSerializationOptions; const AEmpty: TYAMLEmptyValue): Boolean;
begin
{$IFDEF MARS_JSON_LEGACY}
  // no SkipEmpty* options: only 0 is written
  Result := AEmpty <> EmptyNumber;
{$ELSE}
  case AEmpty of
    EmptyString: Result := AOptions.SkipEmptyStrings;
    EmptyNumber: Result := AOptions.SkipEmptyNumbers;
    EmptyBoolean: Result := AOptions.SkipEmptyBooleans;
    EmptyArray: Result := AOptions.SkipEmptyArrays;
  else
    Result := AOptions.SkipEmptyObjects;
  end;
{$ENDIF}
end;

// dates as the JSON of MARS (DateIsUTC, ZeroDateAsEmpty, TDateAsDate); False: not a date
function DateToYAML(const AValue: TValue; const AOptions: TMARSJSONSerializationOptions; out ADate: string): Boolean;
begin
  Result := True;
  if AValue.TypeInfo = TypeInfo(TDateTime) then
    ADate := DateToJSON(AValue.AsType<TDateTime>, AOptions)
  else if AValue.TypeInfo = TypeInfo(TDate) then
    ADate := DateOnlyToJSON(AValue.AsType<TDate>, AOptions)
  else if AValue.TypeInfo = TypeInfo(TTime) then
    ADate := DateToJSON(AValue.AsType<TTime>, AOptions)
  else
    Result := False;
end;

// an empty mapping or sequence as a member: {} or [], unless SkipEmptyObjects or SkipEmptyArrays
function AddEmpty(const ARoot: TYamlNode; const AName: string; const AIsArray: Boolean;
  const AOptions: TMARSJSONSerializationOptions): Boolean;
begin
  Result := False;
  if (not ARoot.IsMapping) or (AName = '') then
    Exit;

  if AIsArray then
  begin
    Result := not SkipEmpty(AOptions, EmptyArray);
    if Result then
      ARoot.AddOrSetSequence(AName).SequenceStyle := TYamlSequenceStyle.Flow;
  end
  else
  begin
    Result := not SkipEmpty(AOptions, EmptyObject);
    if Result then
      ARoot.AddOrSetMapping(AName).MappingStyle := TYamlMappingStyle.Flow;
  end;
end;

// a string that YAML would read as another type (a number, a boolean, null)
function IsAmbiguousString(const AString: string): Boolean;
begin
  if AString = '' then
    Exit(True);
  case AString[1] of
    '0'..'9': Exit(True);
    '-', '+', '.':
      if (Length(AString) > 1) and CharInSet(AString[2], ['0'..'9', '.']) then
        Exit(True);
  end;
  Result := MatchText(AString, ['true', 'false', 'yes', 'no', 'on', 'off', 'y', 'n', 'null', '~'
    , '.inf', '-.inf', '+.inf', '.nan']);
end;

// strings stay strings: quoted when plain they would be read as another type ("200", "8080")
function StringStyle(const AString: string): TYamlScalarStyle;
begin
  if IsAmbiguousString(AString) then
    Result := TYamlScalarStyle.SingleQuoted
  else
    Result := TYamlScalarStyle.Plain;
end;

// the keys of dictionaries are data, e.g. the status codes of the responses
procedure QuoteKey(const AMapping: TYamlNode; const AKey: string);
var
  LIndex: Integer;
  LKey: TYamlNode;
begin
  if not IsAmbiguousString(AKey) then
    Exit;
  for LIndex := AMapping.Count - 1 downto 0 do
  begin
    LKey := AMapping.Elements[LIndex].Key;
    if LKey.ToString = AKey then
    begin
      LKey.ScalarStyle := TYamlScalarStyle.SingleQuoted;
      Break;
    end;
  end;
end;

{ TYAMLObjectWriter }

procedure TYAMLObjectWriter.WriteTo(const AValue: TValue;
  const AMediaType: TMediaType; AOutputStream: TStream;
  const AActivation: IMARSActivation);
var
  LYAML: IYamlDocument;
begin
  LYAML := TMARSYAML.ObjectToYAML(AValue.AsObject, JSONSerializationOptionsFor(AActivation));
  try
    TYAMLValueWriter.WriteYAMLValue(LYAML.ToYaml, AMediaType, AOutputStream, AActivation);
  finally
    LYAML := nil;
  end;
end;

{ TYAMLValueWriter }

class procedure TYAMLValueWriter.WriteYAMLValue(const AValue: TValue;
  const AMediaType: TMediaType; AOutputStream: TStream;
  const AActivation: IMARSActivation);
begin
  TMARSMessageBodyWriter.WriteWith<TYAMLValueWriter>(AValue, AMediaType, AOutputStream, AActivation);
end;

procedure TYAMLValueWriter.WriteTo(const AValue: TValue;
  const AMediaType: TMediaType; AOutputStream: TStream;
  const AActivation: IMARSActivation);
var
  LYAMLString: string;
  LContentBytes: TBytes;
  LEncoding: TEncoding;
  LYAML: IYamlDocument;
begin
  if not TMARSMessageBodyWriter.GetDesiredEncoding(AActivation, LEncoding) then
    LEncoding := TEncoding.UTF8; // UTF8 by default

  LYAMLString := '';
  if AValue.IsType<string> then
    LYAMLString := AValue.AsType<string>
  else
  begin
    LYAML := TMARSYAML.TValueToYAML(AValue, JSONSerializationOptionsFor(AActivation));
    LYAMLString := LYAML.ToYaml;
  end;
  if LYAMLString = '' then
    Exit;

  LContentBytes := LEncoding.GetBytes(LYAMLString);
  AOutputStream.Write(LContentBytes, Length(LContentBytes));
end;

{ TYAMLArrayOfObjectWriter }

procedure TYAMLArrayOfObjectWriter.WriteTo(const AValue: TValue;
  const AMediaType: TMediaType; AOutputStream: TStream;
  const AActivation: IMARSActivation);
var
  LYAML: IYamlDocument;
begin
  if not AValue.IsArray then
    Exit;

  LYAML := TMARSYAML.TValueToYAML(AValue, JSONSerializationOptionsFor(AActivation));
  try
    TYAMLValueWriter.WriteYAMLValue(LYAML.ToYaml, AMediaType, AOutputStream, AActivation);
  finally
    LYAML := nil;
  end;
end;

{ TYAMLRecordWriter }

procedure TYAMLRecordWriter.WriteTo(const AValue: TValue;
  const AMediaType: TMediaType; AOutputStream: TStream;
  const AActivation: IMARSActivation);
var
  LYAML: IYamlDocument;
begin
  LYAML := TMARSYAML.RecordToYAML(AValue, JSONSerializationOptionsFor(AActivation));
  try
    TYAMLValueWriter.WriteYAMLValue(LYAML.ToYaml, AMediaType, AOutputStream, AActivation);
  finally
    LYAML := nil;
  end;
end;

{ TYAMLArrayOfRecordWriter }

procedure TYAMLArrayOfRecordWriter.WriteTo(const AValue: TValue;
  const AMediaType: TMediaType; AOutputStream: TStream;
  const AActivation: IMARSActivation);
var
  LYAML: IYamlDocument;
begin
  if not AValue.IsArray then
    Exit;

  LYAML := TMARSYAML.TValueToYAML(AValue, JSONSerializationOptionsFor(AActivation));
  try
    TYAMLValueWriter.WriteYAMLValue(LYAML.ToYaml, AMediaType, AOutputStream, AActivation);
  finally
    LYAML := nil;
  end;
end;

{ TMARSYAML }

class function TMARSYAML.ObjectToYAML(const AObject: TObject;
  const AFilterProc: TToYAMLFilterProc): IYamlDocument;
begin
  Result := ObjectToYAML(AObject, DefaultMARSJSONSerializationOptions, AFilterProc);
end;

class function TMARSYAML.ObjectToYAML(const AObject: TObject;
  const AOptions: TMARSJSONSerializationOptions; const AFilterProc: TToYAMLFilterProc): IYamlDocument;
var
  LStream: IYamlStream;
  LDocument: IYamlDocument;
begin
  LStream := TYamlStream.Create;
  try
    LDocument := LStream.AddMapping;
    try
      LDocument.Flags := [TYamlDocumentFlag.ImplicitStart, TYamlDocumentFlag.ImplicitEnd];
      LDocument.Root.MappingStyle := TYamlMappingStyle.Block;

      ObjectToYAML(LDocument.Root, '', AObject, AOptions, AFilterProc);

      Result := LDocument;
    except
      LDocument := nil;
      raise;
    end;
  except
    LStream := nil;
    raise;
  end;
end;

class function TMARSYAML.DictionaryToYAML(const ARoot: TYamlNode; const AName: string;
  const ADictionary: TObject): Boolean;
begin
  Result := DictionaryToYAML(ARoot, AName, ADictionary, DefaultMARSJSONSerializationOptions);
end;

class function TMARSYAML.DictionaryToYAML(const ARoot: TYamlNode; const AName: string;
  const ADictionary: TObject; const AOptions: TMARSJSONSerializationOptions): Boolean;
var
  LResult: Boolean;
  LNode: TYamlNode;
  LOptions: TMARSJSONSerializationOptions;
begin
  LResult := False;
  LOptions := AOptions;

  TRttiHelper.EnumerateDictionary(ADictionary
  , procedure (AKey, AValue: TValue)
    begin
      if not LResult then
      begin
        // as for objects: an element of a sequence (e.g. the security requirements), a named member or the root
        if ARoot.IsSequence then
          LNode := ARoot.AddMapping
        else if AName <> '' then
          LNode := ARoot.AddOrSetMapping(AName)
        else
          LNode := ARoot;
        LResult := True;
      end;

      // the key of a dictionary is data: an empty array is written too (a security requirement without scopes)
      if AValue.IsArray and (AValue.GetArrayLength = 0) then
        LNode.AddOrSetSequence(AKey.ToString).SequenceStyle := TYamlSequenceStyle.Flow
      else
        TValueToYaml(LNode, AKey.ToString, AValue, LOptions);
      QuoteKey(LNode, AKey.ToString);
    end
  );

  Result := LResult;
end;

class function TMARSYAML.ListOfPairsToYAML(const ARoot: TYamlNode; const AName: string;
  const AList: TObject): Boolean;
begin
  Result := ListOfPairsToYAML(ARoot, AName, AList, DefaultMARSJSONSerializationOptions);
end;

class function TMARSYAML.ListOfPairsToYAML(const ARoot: TYamlNode; const AName: string;
  const AList: TObject; const AOptions: TMARSJSONSerializationOptions): Boolean;
var
  LResult: Boolean;
  LNode: TYamlNode;
  LOptions: TMARSJSONSerializationOptions;
begin
  LResult := False;
  LOptions := AOptions;

  // TList<TPair<string, T>>: a mapping, in the order of the list (e.g. the properties of an OpenAPI schema)
  TRttiHelper.EnumerateListOfPairOf(AList
  , procedure (APair: TValue)
    begin
      if not LResult then
      begin
        if ARoot.IsSequence then
          LNode := ARoot.AddMapping
        else if AName <> '' then
          LNode := ARoot.AddOrSetMapping(AName)
        else
          LNode := ARoot;
        LResult := True;
      end;

      var LKey := ReadFieldValue(APair, 'Key').AsString;
      var LValue := ReadFieldValue(APair, 'Value');
      if LValue.IsArray and (LValue.GetArrayLength = 0) then
        LNode.AddOrSetSequence(LKey).SequenceStyle := TYamlSequenceStyle.Flow
      else
        TValueToYaml(LNode, LKey, LValue, LOptions);
      QuoteKey(LNode, LKey);
    end
  );

  Result := LResult;
end;

class function TMARSYAML.ObjectListToYAML(const ARoot: TYamlNode; const AName: string;
  const AObjectList: TObject): Boolean;
begin
  Result := ObjectListToYAML(ARoot, AName, AObjectList, DefaultMARSJSONSerializationOptions);
end;

class function TMARSYAML.ObjectListToYAML(const ARoot: TYamlNode; const AName: string;
  const AObjectList: TObject; const AOptions: TMARSJSONSerializationOptions): Boolean;
var
  LNode: TYamlNode;
  LResult: Boolean;
  LOptions: TMARSJSONSerializationOptions;
begin
  LResult := False;
  LOptions := AOptions;

  TRttiHelper.EnumerateObjectList(AObjectList
  , procedure (AValue: TValue)
    begin
      if not LResult then
      begin
        if ARoot.IsSequence then
          LNode := ARoot.AddSequence
        else
          LNode := ARoot.AddOrSetSequence(AName);
        LResult := True;
      end;

      TValueToYaml(LNode, '', AValue, LOptions);
    end
  );

  Result := LResult;
end;

class function TMARSYAML.ObjectToYAML(const ARoot: TYamlNode; const AName: string; const AObject: TObject;
  const AFilterProc: TToYAMLFilterProc = nil): Boolean;
begin
  Result := ObjectToYAML(ARoot, AName, AObject, DefaultMARSJSONSerializationOptions, AFilterProc);
end;

class function TMARSYAML.ObjectToYAML(const ARoot: TYamlNode; const AName: string; const AObject: TObject;
  const AOptions: TMARSJSONSerializationOptions; const AFilterProc: TToYAMLFilterProc = nil): Boolean;

    function GetObjectFilterProc(const AObjectType: TRttiType): TToYAMLFilterProc;
    var
      LMethod: TRttiMethod;
    begin
      Result := nil;
      // looking for TMyClass.ToYAMLFilter(const AMember: TRttiMember; const AYAML: TYamlNode): Boolean;
      LMethod := AObjectType.FindMethodFunc<TRttiMember, TYamlNode, Boolean>('ToYAMLFilter');
      if Assigned(LMethod) then
        Result :=
          procedure (const AMember: TRttiMember; const AValue: TValue; const AYAML: TYamlNode; var AAccept: Boolean)
          begin
            AAccept := LMethod.Invoke(AObject, [AMember, TValue.From<TYamlNode>(AYAML)]).AsBoolean;
          end;
    end;

var
  LType: TRttiType;
  LMember: TRttiMember;
  LValue: TValue;
  LAccept: Boolean;
  LFilterProc: TToYAMLFilterProc;
  LNode: TYamlNode;
  LWritten: Boolean;
  LYAMLName: string;
begin
  Result := False;

  if not Assigned(AObject) then
    Exit;

  LType := TRttiContext.Create.GetType(AObject.ClassType);

  LFilterProc := AFilterProc;
  if not Assigned(LFilterProc) then
    LFilterProc := GetObjectFilterProc(LType);

  if ARoot.IsSequence then
    LNode := ARoot.AddMapping
  else if ARoot.IsMapping and (AName <> '') then
    LNode := ARoot.AddOrSetMapping(AName)
  else
    LNode := ARoot;

  for LMember in LType.GetPropertiesAndFields do
  begin
    if (LMember.Visibility < TMemberVisibility.mvPublic) or (not LMember.IsReadable) then
      Continue;

    LAccept := True;
    if Assigned(LFilterProc) then
      LFilterProc(LMember, AObject, LNode, LAccept);
    if not LAccept then
      Continue;

    LYAMLName := LMember.Name;
    LMember.HasAttribute<YAMLNameAttribute>(
      procedure (AAttr: YAMLNameAttribute)
      begin
        LYAMLName := AAttr.Name;
      end
    );
    if LYAMLName = '' then
      Continue;

    LValue := LMember.GetValue(AObject);
    LWritten := TValueToYaml(LNode, LYAMLName, LValue, AOptions);
    if LWritten then
      Result := True
    else if LNode.IsMapping then
      LNode.Remove(LYAMLName);
  end;
end;

class function TMARSYAML.RecordToYAML(const ARecord: TValue;
  const AFilterProc: TToYAMLFilterProc): IYamlDocument;
begin
  Result := RecordToYAML(ARecord, DefaultMARSJSONSerializationOptions, AFilterProc);
end;

class function TMARSYAML.RecordToYAML(const ARecord: TValue;
  const AOptions: TMARSJSONSerializationOptions; const AFilterProc: TToYAMLFilterProc): IYamlDocument;
var
  LStream: IYamlStream;
  LDocument: IYamlDocument;
begin
  LStream := TYamlStream.Create;
  try
    LDocument := LStream.AddMapping;
    try
      LDocument.Flags := [TYamlDocumentFlag.ImplicitStart, TYamlDocumentFlag.ImplicitEnd];
      LDocument.Root.MappingStyle := TYamlMappingStyle.Block;

      RecordToYAML(LDocument.Root, '', ARecord, AOptions, AFilterProc);

      Result := LDocument;
    except
      LDocument := nil;
      raise;
    end;
  except
    LStream := nil;
    raise;
  end;
end;

class function TMARSYAML.RecordToYAML(const ARoot: TYamlNode; const AName: string; const ARecord: TValue;
  const AFilterProc: TToYAMLFilterProc = nil): Boolean;
begin
  Result := RecordToYAML(ARoot, AName, ARecord, DefaultMARSJSONSerializationOptions, AFilterProc);
end;

class function TMARSYAML.RecordToYAML(const ARoot: TYamlNode; const AName: string; const ARecord: TValue;
  const AOptions: TMARSJSONSerializationOptions; const AFilterProc: TToYAMLFilterProc = nil): Boolean;

    function GetRecordFilterProc(const ARecordType: TRttiType): TToYAMLFilterProc;
    var
      LMethod: TRttiMethod;
      LRecord: TValue;
    begin
      Result := nil;
      LRecord := ARecord;
      // looking for TMyRecord.ToYAMLFilter(const AMember: TRttiMember; const AYAML: TYamlNode): Boolean;
      LMethod := ARecordType.FindMethodFunc<TRttiMember, TYamlNode, Boolean>('ToYAMLFilter');
      if Assigned(LMethod) then
        Result :=
          procedure (const AMember: TRttiMember; const AValue: TValue; const AYAML: TYamlNode; var AAccept: Boolean)
          begin
            AAccept := LMethod.Invoke(LRecord, [AMember, TValue.From<TYamlNode>(AYAML)]).AsBoolean;
          end;
    end;

var
  LType: TRttiType;
  LMember: TRttiMember;
  LValue: TValue;
  LFilterProc: TToYAMLFilterProc;
  LAccept: Boolean;
  LNode: TYamlNode;
  LWritten: Boolean;
  LYAMLName: string;
begin
  Result := False;

  LType := TRttiContext.Create.GetType(ARecord.TypeInfo);

  LFilterProc := AFilterProc;
  if not Assigned(LFilterProc) then
    LFilterProc := GetRecordFilterProc(LType);

  if ARoot.IsSequence then
    LNode := ARoot.AddMapping
  else if ARoot.IsMapping and (AName <> '') then
    LNode := ARoot.AddOrSetMapping(AName)
  else
    LNode := ARoot;

  for LMember in LType.GetPropertiesAndFields do
  begin
    if (LMember.Visibility < TMemberVisibility.mvPublic) or (not LMember.IsReadable) then
      Continue;

    LYAMLName := LMember.Name;
    LMember.HasAttribute<YAMLNameAttribute>(
      procedure (AAttr: YAMLNameAttribute)
      begin
        LYAMLName := AAttr.Name;
      end
    );
    if LYAMLName = '' then
      Continue;

    LAccept := True;
    if Assigned(LFilterProc) then
      LFilterProc(LMember, ARecord, LNode, LAccept);
    if not LAccept then
      Continue;

    LValue := LMember.GetValue(ARecord.GetReferenceToRawData);
    LWritten := TValueToYaml(LNode, LYAMLName, LValue, AOptions);
    if LWritten then
      Result := True
    else if LNode.IsMapping then
      LNode.Remove(LYAMLName);
  end;
end;


class function TMARSYAML.TValueToYAML(const AValue: TValue): IYamlDocument;
begin
  Result := TValueToYAML(AValue, DefaultMARSJSONSerializationOptions);
end;

class function TMARSYAML.TValueToYAML(const AValue: TValue;
  const AOptions: TMARSJSONSerializationOptions): IYamlDocument;
var
  LStream: IYamlStream;
  LDocument: IYamlDocument;
  LDate: string;
begin
  LStream := TYamlStream.Create;
  try
    if AValue.IsArray then
    begin
      LDocument := LStream.AddSequence;
      LDocument.Root.SequenceStyle := TYamlSequenceStyle.Block;
      if AValue.GetArrayLength > 0 then
        TValueToYAML(LDocument.Root, '', AValue, AOptions);
    end
    else if AValue.IsObject or TRttiContext.Create.GetType(AValue.TypeInfo).IsRecord then
    begin
      LDocument := LStream.AddMapping;
      LDocument.Root.MappingStyle := TYamlMappingStyle.Flow;
      TValueToYAML(LDocument.Root, '', AValue, AOptions);
    end
    else if DateToYAML(AValue, AOptions, LDate) then
      LDocument := LStream.AddScalar(LDate)
    else
    begin
      LDocument := LStream.AddScalar(AValue.ToString);
      if AValue.Kind in [tkString, tkUString, tkChar, tkWChar, tkLString, tkWString] then
        LDocument.Root.ScalarStyle := StringStyle(AValue.ToString);
    end;

    LDocument.Flags := [TYamlDocumentFlag.ImplicitStart, TYamlDocumentFlag.ImplicitEnd];
    Result := LDocument;
  except
    LStream := nil;
    raise;
  end;
end;

class function TMARSYAML.TValueToYAML(const ARoot: TYamlNode;
  const AKeyName: string; const AValue: TValue): Boolean;
begin
  Result := TValueToYAML(ARoot, AKeyName, AValue, DefaultMARSJSONSerializationOptions);
end;

class function TMARSYAML.TValueToYAML(const ARoot: TYamlNode;
  const AKeyName: string; const AValue: TValue; const AOptions: TMARSJSONSerializationOptions): Boolean;
var
  LTypeName: string;
  LBool: Boolean;
  LString: string;
  LIndex: Integer;
  LSequence, LNested: TYamlNode;
  LElement: TValue;
begin
  Result := False;
  LTypeName := string(AValue.TypeInfo^.Name);

  // the start of the name: TObjectList<TDictionary<string, ...>> contains the name of a dictionary
  if IsDictionaryOfStringAndT(LTypeName) then
    Result := DictionaryToYaml(ARoot, AKeyName, AValue.AsObject, AOptions)
      or ((AValue.AsObject <> nil) and AddEmpty(ARoot, AKeyName, False, AOptions))

  else if IsObjectListOfT(LTypeName) then
    Result := ObjectListToYaml(ARoot, AKeyName, AValue.AsObject, AOptions)
      or ((AValue.AsObject <> nil) and AddEmpty(ARoot, AKeyName, True, AOptions))

  else if IsListOfPairOfStringAndT(LTypeName) then
    Result := ListOfPairsToYAML(ARoot, AKeyName, AValue.AsObject, AOptions)
      or ((AValue.AsObject <> nil) and AddEmpty(ARoot, AKeyName, False, AOptions))

  {$IFDEF Delphi11Alexandria_UP}
  else if AValue.IsObjectInstance then
  {$ELSE}
  else if AValue.IsObject then
  {$ENDIF}
    // nil: omitted (no null in YAML)
    Result := ObjectToYaml(ARoot, AKeyName, AValue.AsObject, AOptions)
      or ((AValue.AsObject <> nil) and AddEmpty(ARoot, AKeyName, False, AOptions))

  else if AValue.IsArray and (AValue.GetArrayLength > 0) then
  begin
    Result := True;

    if (ARoot.IsSequence) and (AKeyName = '') then
      LSequence := ARoot
    else begin
      LSequence := ARoot.AddOrSetSequence(AKeyName);
      LSequence.SequenceStyle := TYamlSequenceStyle.Block;
    end;

    Assert(LSequence.IsSequence);

    for LIndex := 0 to AValue.GetArrayLength-1 do
    begin
      LElement := AValue.GetArrayElement(LIndex);
      if LElement.IsObject then
        ObjectToYAML(LSequence, '', LElement.AsObject, AOptions)
      else if LElement.Kind in [tkRecord{$ifdef Delphi11Alexandria_UP}, tkMRecord{$endif}] then
        RecordToYAML(LSequence, '', LElement, AOptions)
      else if LElement.Kind in [tkString, tkShortString, tkWString, tkUString, tkChar, tkWChar, tkLString] then
        LSequence.Add(LElement.AsString).ScalarStyle := StringStyle(LElement.AsString)
      // elements keep their position: False, 0 and empty values are written too
      else if LElement.IsType<Boolean> then
        LSequence.Add(LElement.AsType<Boolean>)
      else if DateToYAML(LElement, AOptions, LString) then
      begin
        if LString = '' then
          LSequence.Add(LString).ScalarStyle := TYamlScalarStyle.SingleQuoted
        else
          LSequence.Add(LString);
      end
      else if LElement.Kind = tkInt64 then
        LSequence.Add(LElement.AsType<Int64>)
      else if LElement.Kind = tkInteger then
        LSequence.Add(LElement.AsType<Integer>)
      else if LElement.Kind = tkFloat then
        LSequence.Add(LElement.AsType<Double>)
      else if LElement.IsArray then
      begin
        LNested := LSequence.AddSequence;
        if LElement.GetArrayLength > 0 then
          TValueToYaml(LNested, '', LElement, AOptions);
      end
      else
        LSequence.Add(LElement.ToString);
    end;
  end

  else if AValue.IsArray then
    Result := AddEmpty(ARoot, AKeyName, True, AOptions)

  else if (AValue.Kind in [tkRecord{$ifdef Delphi11Alexandria_UP}, tkMRecord{$endif}]) then
    Result := RecordToYaml(ARoot, AKeyName, AValue, AOptions)
      or AddEmpty(ARoot, AKeyName, False, AOptions)

  else if (AValue.Kind in [tkString, tkUString, tkChar, tkWideChar, tkLString, tkWString]) then
  begin
    LString := AValue.AsString;
    if (LString <> '') or not SkipEmpty(AOptions, EmptyString) then
    begin
      ARoot.AddOrSetValue(AKeyName, LString).ScalarStyle := StringStyle(LString);
      Result := True;
    end;
  end

  else if (AValue.IsType<Boolean>) then
  begin
    LBool := AValue.AsType<Boolean>;
    if LBool or not SkipEmpty(AOptions, EmptyBoolean) then
    begin
      ARoot.AddOrSetValue(AKeyName, LBool);
      Result := True;
    end;
  end

  // a zero date can be "" (ZeroDateAsEmpty): an empty string
  else if DateToYAML(AValue, AOptions, LString) then
  begin
    if LString <> '' then
    begin
      ARoot.AddOrSetValue(AKeyName, LString);
      Result := True;
    end
    else if not SkipEmpty(AOptions, EmptyString) then
    begin
      ARoot.AddOrSetValue(AKeyName, LString).ScalarStyle := TYamlScalarStyle.SingleQuoted;
      Result := True;
    end;
  end

  else if (AValue.Kind in [tkInt64]) then
  begin
    if (AValue.AsType<Int64> <> 0) or not SkipEmpty(AOptions, EmptyNumber) then
    begin
      ARoot.AddOrSetValue(AKeyName, AValue.AsType<Int64>);
      Result := True;
    end;
  end
  else if (AValue.Kind in [tkInteger]) then
  begin
    if (AValue.AsType<Integer> <> 0) or not SkipEmpty(AOptions, EmptyNumber) then
    begin
      ARoot.AddOrSetValue(AKeyName, AValue.AsType<Integer>);
      Result := True;
    end;
  end

  else if (AValue.Kind in [tkFloat]) then
  begin
    if (AValue.AsType<Double> <> 0) or not SkipEmpty(AOptions, EmptyNumber) then
    begin
      ARoot.AddOrSetValue(AKeyName, AValue.AsType<Double>);
      Result := True;
    end;
  end

  else
  begin
    LString := AValue.ToString;
    if (LString <> '') or not SkipEmpty(AOptions, EmptyString) then
    begin
      ARoot.AddOrSetValue(AKeyName,  LString);
      Result := True;
    end;
  end;
end;

procedure RegisterWriters;
begin
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TYamlNode>(TYAMLValueWriter);
  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TYAMLValueWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := (AType.Handle = TypeInfo(TYAMLRawString)) and AMediaType.Contains('yaml');
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyRegistry.AFFINITY_MEDIUM;
      end
  );

  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TYAMLValueWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := (AType.TypeKind in [tkInteger, tkInt64, tkChar, tkEnumeration, tkFloat,
          tkString, tkSet, tkWChar, tkLString, tkWString,
          tkVariant, tkArray, tkRecord{$ifdef Delphi11Alexandria_UP}, tkMRecord{$endif}, tkInt64, tkDynArray, tkUString])
          // YAML only: without this check the writer took */* requests (no Accept) from the JSON writers
          and AMediaType.Contains('yaml');
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyRegistry.AFFINITY_VERY_LOW;
      end
  );



  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TYAMLObjectWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := AType.IsObjectOfType<TObject> and AMediaType.Contains('yaml');
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyRegistry.AFFINITY_MEDIUM;
      end
  );

  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TYAMLArrayOfObjectWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := AType.IsDynamicArrayOf<TObject> and AMediaType.Contains('yaml');
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyRegistry.AFFINITY_MEDIUM;
      end
  );

  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TYAMLRecordWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := AType.IsRecord and AMediaType.Contains('yaml');
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyRegistry.AFFINITY_MEDIUM;
      end
  );

  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TYAMLArrayOfRecordWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := AType.IsDynamicArrayOfRecord and AMediaType.Contains('yaml');
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyRegistry.AFFINITY_MEDIUM;
      end
  );
end;


{ YAMLNameAttribute }

constructor YAMLNameAttribute.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

initialization
  RegisterWriters;
{$ENDIF}

end.
