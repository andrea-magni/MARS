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
  public
    procedure FromRecord<T: record>(ARecord: T; const AFilterProc: TToYAMLFilterProc = nil); overload; experimental;
    procedure FromRecord(const ARecord: TValue; const AFilterProc: TToYAMLFilterProc = nil); overload; experimental;
    function ToRecord<T: record>(const AFilterProc: TToRecordFilterProc = nil): T; overload; experimental;
    function ToRecord(const ARecordType: TRttiType; const AFilterProc: TToRecordFilterProc = nil): TValue; overload; experimental;

    class function ObjectToYAML(const AObject: TObject; const AFilterProc: TToYAMLFilterProc = nil): IYamlDocument; overload;
    class procedure ObjectToYAML(const ARoot: TYamlNode; const AObject: TObject; const AFilterProc: TToYAMLFilterProc = nil); overload;
    class procedure RecordToYAML(const ARoot: TYamlNode; const ARecord: TValue; const AFilterProc: TToYAMLFilterProc = nil); overload;

    function ToObject<T: class>(const AFilterProc: TToRecordFilterProc = nil): T; overload;
    function ToObject(const AObjectType: TRttiType; const AFilterProc: TToRecordFilterProc = nil): TValue; overload;

    class procedure WriteTValueToYaml(const ARoot: TYamlNode; const AKeyName: string; const AValue: TValue);
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
begin

end;

{ TYAMLRecordWriter }

procedure TYAMLRecordWriter.WriteTo(const AValue: TValue;
  const AMediaType: TMediaType; AOutputStream: TStream;
  const AActivation: IMARSActivation);
begin

end;

{ TYAMLArrayOfRecordWriter }

procedure TYAMLArrayOfRecordWriter.WriteTo(const AValue: TValue;
  const AMediaType: TMediaType; AOutputStream: TStream;
  const AActivation: IMARSActivation);
begin

end;

{ TMARSYAML }

class function TMARSYAML.ObjectToYAML(const AObject: TObject;
  const AFilterProc: TToYAMLFilterProc): IYamlDocument;
var
  LStream: IYamlStream;
  LDocument: IYamlDocument;
  LRoot: TYamlNode;

begin
  LStream := TYamlStream.Create;
  LDocument := LStream.AddMapping;
  LDocument.Flags := [TYamlDocumentFlag.ImplicitStart, TYamlDocumentFlag.ImplicitEnd];

  LRoot := LDocument.Root;
  LRoot.MappingStyle := TYamlMappingStyle.Block;

  ObjectToYAML(LRoot, AObject, AFilterProc);

  Result := LDocument;
end;

procedure TMARSYAML.FromRecord(const ARecord: TValue;
  const AFilterProc: TToYAMLFilterProc);
begin

end;

procedure TMARSYAML.FromRecord<T>(ARecord: T;
  const AFilterProc: TToYAMLFilterProc);
begin

end;

class procedure TMARSYAML.ObjectToYAML(const ARoot: TYamlNode; const AObject: TObject;
  const AFilterProc: TToYAMLFilterProc = nil);
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
//  if not Assigned(LFilterProc) then
//    LFilterProc := TMARSYAML.GetObjectFilterProc(LType);

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

    if LProperty.PropertyType.IsDynamicArrayOf<TObject> then
    begin
      var LSequence := ARoot.AddOrSetSequence(LProperty.Name);
      LSequence.SequenceStyle := TYamlSequenceStyle.Block;
      for var LIndex := 0 to LValue.GetArrayLength-1 do
        ObjectToYAML(LSequence.AddMapping, LValue.GetArrayElement(LIndex).AsObject);
    end
    else
      WriteTValueToYaml(ARoot, LProperty.Name, LValue);
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

//    if LMember.GetRttiType.IsDynamicArrayOf<TObject> then
//    begin
//      var LSequence := ARoot.AddOrSetSequence(LMember.Name);
//      LSequence.SequenceStyle := TYamlSequenceStyle.Block;
//      for var LIndex := 0 to LValue.GetArrayLength-1 do
//        ObjectToYAML(LSequence.AddMapping, LValue.GetArrayElement(LIndex).AsObject);
//    end
//    else
    WriteTValueToYaml(ARoot, LMember.Name, LValue);
  end;
end;

function TMARSYAML.ToObject(const AObjectType: TRttiType;
  const AFilterProc: TToRecordFilterProc): TValue;
begin

end;

function TMARSYAML.ToObject<T>(const AFilterProc: TToRecordFilterProc): T;
begin

end;

function TMARSYAML.ToRecord(const ARecordType: TRttiType;
  const AFilterProc: TToRecordFilterProc): TValue;
begin

end;

function TMARSYAML.ToRecord<T>(const AFilterProc: TToRecordFilterProc): T;
begin

end;

class procedure TMARSYAML.WriteTValueToYaml(const ARoot: TYamlNode;
  const AKeyName: string; const AValue: TValue);
begin
  if AValue.IsObjectInstance then
    ObjectToYaml(ARoot.AddOrSetMapping(AKeyName), AValue.AsObject)

  else if AValue.IsArray then
  begin
    var LSequence := ARoot.AddOrSetSequence(AKeyName);
    LSequence.SequenceStyle := TYamlSequenceStyle.Block;

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
        Result := TMARSMessageBodyRegistry.AFFINITY_VERY_LOW;
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
        Result := TMARSMessageBodyRegistry.AFFINITY_VERY_LOW;
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
