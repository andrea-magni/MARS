unit MARS.YAML.ReadersAndWriters;

interface

uses
  Classes, SysUtils, Rtti
  , MARS.Core.Attributes, MARS.Core.Declarations, MARS.Core.MediaType
  , MARS.Core.MessageBodyWriter, MARS.Core.Activation.Interfaces

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


  TMARSYAML = class
  private
  public
    class function DictionaryToYAML(const ARoot: TYamlNode; const AName: string; const ADictionary: TObject): Boolean;
    class function ObjectListToYAML(const ARoot: TYamlNode; const AName: string; const AObjectList: TObject): Boolean;
    class function ObjectToYAML(const AObject: TObject; const AFilterProc: TToYAMLFilterProc = nil): IYamlDocument; overload;
    class function ObjectToYAML(const ARoot: TYamlNode; const AName: string; const AObject: TObject; const AFilterProc: TToYAMLFilterProc = nil): Boolean; overload;
    class function RecordToYAML(const ARecord: TValue; const AFilterProc: TToYAMLFilterProc = nil): IYamlDocument; overload;
    class function RecordToYAML(const ARoot: TYamlNode; const AName: string; const ARecord: TValue; const AFilterProc: TToYAMLFilterProc = nil): Boolean; overload;

//    class function YAMLToRecord<T: record>(const AYAML: TYamlNode; const AFilterProc: TToRecordFilterProc = nil): T; overload;
//    class function YAMLToRecord(const AYAML: TYamlNode; const ARecordType: TRttiType; const AFilterProc: TToRecordFilterProc = nil): TValue; overload;
//    class function YAMLToObject<T: class>(const AYAML: TYamlNode; const AFilterProc: TToRecordFilterProc = nil): T; overload;
//    class function YAMLToObject(const AYAML: TYamlNode; const AObjectType: TRttiType; const AFilterProc: TToRecordFilterProc = nil): TValue; overload;

    class function TValueToYAML(const AValue: TValue): IYamlDocument; overload;
    class function TValueToYaml(const ARoot: TYamlNode; const AKeyName: string; const AValue: TValue): Boolean; overload;
  end;

implementation

uses
  System.TypInfo, DateUtils, Generics.Collections
, MARS.Core.Utils, MARS.Rtti.Utils
;

{ TYAMLObjectWriter }

procedure TYAMLObjectWriter.WriteTo(const AValue: TValue;
  const AMediaType: TMediaType; AOutputStream: TStream;
  const AActivation: IMARSActivation);
var
  LYAML: IYamlDocument;
begin
  LYAML := TMARSYAML.ObjectToYAML(AValue.AsObject);
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
begin
  if not TMARSMessageBodyWriter.GetDesiredEncoding(AActivation, LEncoding) then
    LEncoding := TEncoding.UTF8; // UTF8 by default

  LYAMLString := '';
  if AValue.IsType<string> then
    LYAMLString := AValue.AsType<string>;

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

  LYAML := TMARSYAML.TValueToYAML(AValue);
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
  LYAML := TMARSYAML.RecordToYAML(AValue);
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

  LYAML := TMARSYAML.TValueToYAML(AValue);
  try
    TYAMLValueWriter.WriteYAMLValue(LYAML.ToYaml, AMediaType, AOutputStream, AActivation);
  finally
    LYAML := nil;
  end;
end;

{ TMARSYAML }

class function TMARSYAML.ObjectToYAML(const AObject: TObject;
  const AFilterProc: TToYAMLFilterProc): IYamlDocument;
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

      ObjectToYAML(LDocument.Root, '', AObject, AFilterProc);

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
var
  LDictionaryType, LEnumeratorType, LPairType: TRttiType;
  LGetEnumeratorMethod, LEnumeratorMoveNextMethod: TRttiMethod;
  LEnumeratorCurrentProperty: TRttiProperty;
  LKeyField, LValueField: TRttiField;
  LEnumeratorValue, LEnumeratorCurrentValue, LCurrentValue, LKeyValue, LValueValue: TValue;
  LEnumeratorObj: TObject;
  LPair: Pointer;
  LElement: TYamlNode;
  LNode: TYamlNode;
begin
  // highly inspired by https://en.delphipraxis.net/topic/2546-how-to-iterate-a-tdictionary-using-rtti-and-tvalue/
  // Thanks to Remy Lebeau

  LDictionaryType := TRttiContext.Create.GetType(ADictionary.ClassType);
  LGetEnumeratorMethod := LDictionaryType.GetMethod('GetEnumerator');
  LEnumeratorValue := LGetEnumeratorMethod.Invoke(ADictionary, []);
  LEnumeratorObj := LEnumeratorValue.AsObject;
  try
    LEnumeratorType := LGetEnumeratorMethod.ReturnType;
    LEnumeratorMoveNextMethod := LEnumeratorType.GetMethod('MoveNext');
    LEnumeratorCurrentProperty := LEnumeratorType.GetProperty('Current');

    LPairType := LEnumeratorCurrentProperty.PropertyType;
    LKeyField := LPairType.GetField('Key');
    LValueField := LPairType.GetField('Value');
    Result := False;
    LEnumeratorCurrentValue := LEnumeratorMoveNextMethod.Invoke(LEnumeratorObj, []);
    while LEnumeratorCurrentValue.AsBoolean do
    begin
      LCurrentValue := LEnumeratorCurrentProperty.GetValue(LEnumeratorObj);
      LPair := LCurrentValue.GetReferenceToRawData;
      LKeyValue := LKeyField.GetValue(LPair);
      LValueValue := LValueField.GetValue(LPair);

      if not Result then
      begin
        LNode := ARoot.AddOrSetMapping(AName);
        Result := True;
      end;

      LElement := LNode.AddOrSetMapping(LKeyValue.ToString);

      if LValueValue.IsObject then
        ObjectToYAML(LElement, '', LValueValue.AsObject)
      else if LValueValue.Kind in [tkRecord, tkMRecord] then
        RecordToYAML(LElement, '', LValueValue);

      LEnumeratorCurrentValue := LEnumeratorMoveNextMethod.Invoke(LEnumeratorObj, []);
    end;
  finally
    LEnumeratorObj.Free;
  end;
end;

class function TMARSYAML.ObjectListToYAML(const ARoot: TYamlNode; const AName: string;
  const AObjectList: TObject): Boolean;
var
  LObjectListType, LEnumeratorType{, LElementType}: TRttiType;
  LGetEnumeratorMethod, LEnumeratorMoveNextMethod: TRttiMethod;
  LEnumeratorCurrentProperty: TRttiProperty;
  LEnumeratorValue, LEnumeratorCurrentValue, LCurrentValue: TValue;
  LEnumeratorObj: TObject;
  LNode: TYamlNode;
begin
  LObjectListType := TRttiContext.Create.GetType(AObjectList.ClassType);
  LGetEnumeratorMethod := LObjectListType.GetMethod('GetEnumerator');
  LEnumeratorValue := LGetEnumeratorMethod.Invoke(AObjectList, []);
  LEnumeratorObj := LEnumeratorValue.AsObject;
  try
    LEnumeratorType := LGetEnumeratorMethod.ReturnType;
    LEnumeratorMoveNextMethod := LEnumeratorType.GetMethod('MoveNext');
    LEnumeratorCurrentProperty := LEnumeratorType.GetProperty('Current');

//    LElementType := LEnumeratorCurrentProperty.PropertyType;

    Result := False;
    LEnumeratorCurrentValue := LEnumeratorMoveNextMethod.Invoke(LEnumeratorObj, []);
    while LEnumeratorCurrentValue.AsBoolean do
    begin
      LCurrentValue := LEnumeratorCurrentProperty.GetValue(LEnumeratorObj);

      if not Result then
      begin
        LNode := ARoot.AddOrSetSequence(AName);
        Result := True;
      end;

      if LCurrentValue.IsObject then
        ObjectToYAML(LNode, '', LCurrentValue.AsObject)
      else if LCurrentValue.Kind in [tkRecord, tkMRecord] then
        RecordToYAML(LNode, '', LCurrentValue);

      LEnumeratorCurrentValue := LEnumeratorMoveNextMethod.Invoke(LEnumeratorObj, []);
    end;
  finally
    LEnumeratorObj.Free;
  end;
end;

class function TMARSYAML.ObjectToYAML(const ARoot: TYamlNode; const AName: string; const AObject: TObject;
  const AFilterProc: TToYAMLFilterProc = nil): Boolean;

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
begin
  Result := False;

  if not Assigned(AObject) then
    Exit;

  LType := TRttiContext.Create.GetType(AObject.ClassType);

  LFilterProc := AFilterProc;
  if not Assigned(LFilterProc) then
    LFilterProc := GetObjectFilterProc(LType);

  for LMember in LType.GetPropertiesAndFields do
  begin
    if (LMember.Visibility < TMemberVisibility.mvPublic) or (not LMember.IsReadable) then
      Continue;

    LAccept := True;
    if Assigned(LFilterProc) then
      LFilterProc(LMember, AObject, ARoot, LAccept);
    if not LAccept then
      Continue;

    if not Result then
    begin
      if ARoot.IsSequence then
        LNode := ARoot.AddMapping
      else
        LNode := ARoot;
      Result := True;
    end;

    LValue := LMember.GetValue(AObject);
    TValueToYaml(LNode, LMember.Name, LValue);
  end;
end;

class function TMARSYAML.RecordToYAML(const ARecord: TValue;
  const AFilterProc: TToYAMLFilterProc): IYamlDocument;
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

      RecordToYAML(LDocument.Root, '', ARecord, AFilterProc);

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

    function GetRecordFilterProc(const ARecordType: TRttiType): TToYAMLFilterProc;
    var
      LMethod: TRttiMethod;
    begin
      Result := nil;
      // looking for TMyRecord.ToYAMLFilter(const AMember: TRttiMember; const AYAML: TYamlNode): Boolean;
      LMethod := ARecordType.FindMethodFunc<TRttiMember, TYamlNode, Boolean>('ToYAMLFilter');
      if Assigned(LMethod) then
        Result :=
          procedure (const AMember: TRttiMember; const AValue: TValue; const AYAML: TYamlNode; var AAccept: Boolean)
          begin
            AAccept := LMethod.Invoke(ARecord, [AMember, TValue.From<TYamlNode>(AYAML)]).AsBoolean;
          end;
    end;

var
  LType: TRttiType;
  LMember: TRttiMember;
  LValue: TValue;
  LFilterProc: TToYAMLFilterProc;
  LAccept: Boolean;
  LNode: TYamlNode;
begin
  Result := False;

  LType := TRttiContext.Create.GetType(ARecord.TypeInfo);

  LFilterProc := AFilterProc;
  if not Assigned(LFilterProc) then
    LFilterProc := GetRecordFilterProc(LType);

  for LMember in LType.GetPropertiesAndFields do
  begin
    if (LMember.Visibility < TMemberVisibility.mvPublic) or (not LMember.IsReadable) then
      Continue;

    LAccept := True;
    if Assigned(LFilterProc) then
      LFilterProc(LMember, ARecord, ARoot, LAccept);
    if not LAccept then
      Continue;

    if not Result then
    begin
      if ARoot.IsSequence then
        LNode := ARoot.AddMapping
      else
        LNode := ARoot;
      Result := True;
    end;

    LValue := LMember.GetValue(ARecord.GetReferenceToRawData);
    TValueToYaml(LNode, LMember.Name, LValue);
  end;
end;


class function TMARSYAML.TValueToYAML(const AValue: TValue): IYamlDocument;
var
  LStream: IYamlStream;
  LDocument: IYamlDocument;
begin
  LStream := TYamlStream.Create;
  try
    if AValue.IsArray then
    begin
      LDocument := LStream.AddSequence;
      LDocument.Root.SequenceStyle := TYamlSequenceStyle.Block;
    end
    else
    begin
      LDocument := LStream.AddMapping;
      LDocument.Root.MappingStyle := TYamlMappingStyle.Flow;
    end;
    try
      LDocument.Flags := [TYamlDocumentFlag.ImplicitStart, TYamlDocumentFlag.ImplicitEnd];

      TValueToYAML(LDocument.Root, '', AValue);

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

class function TMARSYAML.TValueToYAML(const ARoot: TYamlNode;
  const AKeyName: string; const AValue: TValue): Boolean;
var
  LTypeName: string;
  LBool: Boolean;
  LString: string;
  LIndex: Integer;
  LSequence: TYamlNode;
begin
  Result := False;
  LTypeName := string(AValue.TypeInfo^.Name);

  if LTypeName.Contains('TDictionary<System.string,') or LTypeName.Contains('TObjectDictionary<System.string,')  then
    Result := DictionaryToYaml(ARoot, AKeyName, AValue.AsObject)

  else if LTypeName.Contains('TObjectList<') then
    Result := ObjectListToYaml(ARoot, AKeyName, AValue.AsObject)

  else if AValue.IsObjectInstance then
    Result := ObjectToYaml(ARoot, AKeyName, AValue.AsObject)

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
      var LElement := AValue.GetArrayElement(LIndex);
      if LElement.IsObject then
        ObjectToYAML(LSequence, '', LElement.AsObject)
      else if LElement.Kind in [tkRecord, tkMRecord] then
        RecordToYAML(LSequence, '', LElement)
      else if LElement.Kind in [tkString, tkShortString, tkWString, tkUString, tkChar, tkWChar] then
        LSequence.Add(LElement.AsString); //AM TODO Numbers, Boolean, ...
    end;
  end

  else if (AValue.Kind in [tkRecord, tkMRecord]) then
    Result := RecordToYaml(ARoot, AKeyName, AValue)

  else if (AValue.Kind in [tkString, tkUString, tkChar, {$ifdef DelphiXE6_UP} tkWideChar, {$endif} tkLString, tkWString])  then
  begin
    LString := AValue.AsString;
    if LString <> '' then
    begin
      ARoot.AddOrSetValue(AKeyName, LString).ScalarStyle := TYamlScalarStyle.Plain;
      Result := True;
    end;
  end

  else if (AValue.IsType<Boolean>) then
  begin
    LBool := AValue.AsType<Boolean>;
    if LBool <> false then
    begin
      ARoot.AddOrSetValue(AKeyName, LBool);
      Result := True;
    end;
  end

  else if AValue.TypeInfo = TypeInfo(TDateTime) then
  begin
    ARoot.AddOrSetValue(AKeyName, DateToISO8601(AValue.AsType<TDateTime>, False));
    Result := True;
  end
  else if AValue.TypeInfo = TypeInfo(TDate) then
  begin
    ARoot.AddOrSetValue(AKeyName, DateToISO8601(AValue.AsType<TDate>, False));
    Result := True;
  end
  else if AValue.TypeInfo = TypeInfo(TTime) then
  begin
    ARoot.AddOrSetValue(AKeyName, DateToISO8601(AValue.AsType<TTime>, False));
    Result := True;
  end

  else if (AValue.Kind in [tkInt64]) then
  begin
    ARoot.AddOrSetValue(AKeyName, AValue.AsType<Int64>);
    Result := True;
  end
  else if (AValue.Kind in [tkInteger]) then
  begin
    ARoot.AddOrSetValue(AKeyName, AValue.AsType<Integer>);
    Result := True;
  end

  else if (AValue.Kind in [tkFloat]) then
  begin
    ARoot.AddOrSetValue(AKeyName, AValue.AsType<Double>);
    Result := True;
  end

  else
  begin
    LString := AValue.ToString;
    if LString <> '' then
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


initialization
  RegisterWriters;

end.
