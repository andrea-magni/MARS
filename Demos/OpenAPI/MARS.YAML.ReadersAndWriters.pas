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
    class function ObjectToYAML(const AObject: TObject; const AFilterProc: TToYAMLFilterProc = nil): IYamlDocument; overload;
    class procedure ObjectToYAML(const ARoot: TYamlNode; const AObject: TObject; const AFilterProc: TToYAMLFilterProc = nil); overload;
    class function RecordToYAML(const ARecord: TValue; const AFilterProc: TToYAMLFilterProc = nil): IYamlDocument; overload;
    class procedure RecordToYAML(const ARoot: TYamlNode; const ARecord: TValue; const AFilterProc: TToYAMLFilterProc = nil); overload;

//    class function YAMLToRecord<T: record>(const AYAML: TYamlNode; const AFilterProc: TToRecordFilterProc = nil): T; overload;
//    class function YAMLToRecord(const AYAML: TYamlNode; const ARecordType: TRttiType; const AFilterProc: TToRecordFilterProc = nil): TValue; overload;
//    class function YAMLToObject<T: class>(const AYAML: TYamlNode; const AFilterProc: TToRecordFilterProc = nil): T; overload;
//    class function YAMLToObject(const AYAML: TYamlNode; const AObjectType: TRttiType; const AFilterProc: TToRecordFilterProc = nil): TValue; overload;

    class function TValueToYAML(const AValue: TValue): IYamlDocument; overload;
    class procedure TValueToYaml(const ARoot: TYamlNode; const AKeyName: string; const AValue: TValue); overload;
  end;

implementation

uses
  System.TypInfo, DateUtils
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

      ObjectToYAML(LDocument.Root, AObject, AFilterProc);

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


class procedure TMARSYAML.ObjectToYAML(const ARoot: TYamlNode; const AObject: TObject;
  const AFilterProc: TToYAMLFilterProc = nil);

    function GetObjectFilterProc(const AObjectType: TRttiType): TToYAMLFilterProc;
    var
      LMethod: TRttiMethod;
    begin
      Result := nil;
      // looking for TMyClass.ToYAMLFilter(const AField: TRttiField; const AYAML: TYamlNode): Boolean;
      LMethod := AObjectType.FindMethodFunc<TRttiField, TYamlNode, Boolean>('ToYAMLFilter');
      if Assigned(LMethod) then
        Result :=
          procedure (const AMember: TRttiMember; const AValue: TValue; const AYAML: TYamlNode; var AAccept: Boolean)
          begin
            AAccept := LMethod.Invoke(AObject, [AMember, TValue.From<TYamlNode>(AYAML)]).AsBoolean;
          end;
    end;

var
  LType: TRttiType;
  LProperty: TRttiProperty;
  LValue: TValue;
  LAccept: Boolean;
  LFilterProc: TToYAMLFilterProc;
begin
  if not Assigned(AObject) then
    Exit;

  LType := TRttiContext.Create.GetType(AObject.ClassType);

  LFilterProc := AFilterProc;
  if not Assigned(LFilterProc) then
    LFilterProc := GetObjectFilterProc(LType);

  for LProperty in LType.GetProperties do
  begin
    if (LProperty.Visibility < TMemberVisibility.mvPublic) or (not LProperty.IsReadable) then
      Continue;

    LAccept := True;
    if Assigned(LFilterProc) then
      LFilterProc(LProperty, AObject, ARoot, LAccept);
    if not LAccept then
      Continue;

    LValue := LProperty.GetValue(AObject);
    TValueToYaml(ARoot, LProperty.Name, LValue);
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

      RecordToYAML(LDocument.Root, ARecord, AFilterProc);

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

class procedure TMARSYAML.RecordToYAML(const ARoot: TYamlNode; const ARecord: TValue;
  const AFilterProc: TToYAMLFilterProc = nil);

    function GetRecordFilterProc(const ARecordType: TRttiType): TToYAMLFilterProc;
    var
      LMethod: TRttiMethod;
    begin
      Result := nil;
      // looking for TMyRecord.ToYAMLFilter(const AField: TRttiField; const AYAML: TYamlNode): Boolean;
      LMethod := ARecordType.FindMethodFunc<TRttiField, TYamlNode, Boolean>('ToYAMLFilter');
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
  LMembers: TArray<TRttiMember>;
begin
//  if not Assigned(AObject) then
//    Exit;

  LType := TRttiContext.Create.GetType(ARecord.TypeInfo);

  LFilterProc := AFilterProc;
  if not Assigned(LFilterProc) then
    LFilterProc := GetRecordFilterProc(LType);

  LMembers := TArray<TRttiMember>(LType.GetProperties) + TArray<TRttiMember>(LType.GetFields);

  for LMember in LMembers do
  begin
    if (LMember.Visibility < TMemberVisibility.mvPublic) or (not LMember.IsReadable) then
      Continue;

    LAccept := True;
    if Assigned(LFilterProc) then
      LFilterProc(LMember, ARecord, ARoot, LAccept);
    if not LAccept then
      Continue;

    LValue := LMember.GetValue(ARecord.GetReferenceToRawData);
    TValueToYaml(ARoot, LMember.Name, LValue);
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

class procedure TMARSYAML.TValueToYAML(const ARoot: TYamlNode;
  const AKeyName: string; const AValue: TValue);
begin
  if AValue.IsObjectInstance then
    ObjectToYaml(ARoot.AddOrSetMapping(AKeyName), AValue.AsObject)

  else if AValue.IsArray then
  begin
    var LSequence: TYamlNode;
    if (ARoot.IsSequence) and (AKeyName = '') then
      LSequence := ARoot
    else begin
      LSequence := ARoot.AddOrSetSequence(AKeyName);
      LSequence.SequenceStyle := TYamlSequenceStyle.Block;
    end;

    Assert(LSequence.IsSequence);

    for var LIndex := 0 to AValue.GetArrayLength-1 do
    begin
      var LElement := AValue.GetArrayElement(LIndex);
      if LElement.IsObject then
        ObjectToYAML(LSequence.AddMapping, LElement.AsObject)
      else if LElement.Kind in [tkRecord] then
        RecordToYAML(LSequence.AddMapping, LElement);
    end;
  end

  else if (AValue.Kind in [tkRecord]) then
    RecordToYaml(ARoot.AddOrSetMapping(AKeyName), AValue)

  else if (AValue.Kind in [tkString, tkUString, tkChar, {$ifdef DelphiXE6_UP} tkWideChar, {$endif} tkLString, tkWString])  then
    ARoot.AddOrSetValue(AKeyName, AValue.AsString).ScalarStyle := TYamlScalarStyle.Plain

  else if (AValue.IsType<Boolean>) then
    ARoot.AddOrSetValue(AKeyName, AValue.AsType<Boolean>)

  else if AValue.TypeInfo = TypeInfo(TDateTime) then
    ARoot.AddOrSetValue(AKeyName, DateToISO8601(AValue.AsType<TDateTime>, False))
  else if AValue.TypeInfo = TypeInfo(TDate) then
    ARoot.AddOrSetValue(AKeyName, DateToISO8601(AValue.AsType<TDate>, False))
  else if AValue.TypeInfo = TypeInfo(TTime) then
    ARoot.AddOrSetValue(AKeyName, DateToISO8601(AValue.AsType<TTime>, False))

  else if (AValue.Kind in [tkInt64]) then
    ARoot.AddOrSetValue(AKeyName, AValue.AsType<Int64>)
  else if (AValue.Kind in [tkInteger]) then
    ARoot.AddOrSetValue(AKeyName, AValue.AsType<Integer>)

  else if (AValue.Kind in [tkFloat]) then
    ARoot.AddOrSetValue(AKeyName, AValue.AsType<Double>)

  else
    ARoot.AddOrSetValue(AKeyName,  AValue.ToString);
end;

procedure RegisterWriters;
begin
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TYamlNode>(TYAMLValueWriter);
  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TYAMLValueWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := (AType.Handle = TypeInfo(TYAMLRawString)) and (AMediaType = TMediaType.APPLICATION_YAML);
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
        Result := AType.IsObjectOfType<TObject>;
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
        Result := AType.IsDynamicArrayOf<TObject>;
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
        Result := AType.IsRecord;
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
        Result := AType.IsDynamicArrayOfRecord;
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
