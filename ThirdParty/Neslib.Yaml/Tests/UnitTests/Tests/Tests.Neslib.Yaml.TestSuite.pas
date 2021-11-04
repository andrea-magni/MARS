unit Tests.Neslib.Yaml.TestSuite;

interface

uses
  System.Classes,
  System.Generics.Collections,
  DUnitX.TestFramework,
  Neslib.Yaml;

type
  TTestData = record
  public
    InYaml: String;
    OutYaml: String;
    TestEvent: TStringList;
  public
    procedure Init;
    procedure Free;

    function Load(const ADirectory: String): Boolean;
  end;

type
  TestSuiteCApi = class
  private
    procedure TestParser(const ADirectory: String; const ATestData: TTestData);
    procedure TestEmitter(const ADirectory: String; const ATestData: TTestData);
  public
    [Test] procedure TestParse;
    [Test] procedure TestEmit;
  end;

type
  TestSuiteParser = class
  private
    FDirectory: String;
    FEvents: TStringList;
    FEventIndex: Integer;
  private
    procedure TestParser(const ATestData: TTestData);
    procedure TestDocument(const ADocument: IYamlDocument);
    procedure TestNode(const ANode: TYamlNode);
    procedure TestScalar(const ANode: TYamlNode);
    procedure TestSequence(const ANode: TYamlNode);
    procedure TestMapping(const ANode: TYamlNode);
    procedure TestAlias(const ANode: TYamlNode);
    function GetAnchorAndTag(const ANode: TYamlNode): String;
    procedure CheckEvent(const AExpectedKey, AExpectedValue: String);
  public
    [Test] procedure TestParse;
  end;

type
  TestSuiteGenerator = class
  private type
    TEvent = record
      Key: String;
      Anchor: String;
      Tag: String;
      Value: String;
    end;
  private type
    TScalar = record
      Value: String;
      Style: TYamlScalarStyle;
    end;
  private
    FDirectory: String;
    FEvents: TStringList;
    FEventIndex: Integer;
    FAnchors: TDictionary<String, TYamlNode>;
  private
    procedure TestGenerator(const ATestData: TTestData);
    function GetEvent: TEvent;
    procedure SetupNode(const ANode: TYamlNode; const AEvent: TEvent);
    procedure SetupSequence(const ANode: TYamlNode);
    procedure SetupMapping(const ANode: TYamlNode);
    function GetAnchor(const AValue: String): TYamlNode;
    function GetScalar(const AValue: String): TScalar;
  public
    [Test] procedure TestGenerate;
  end;

implementation

{$R '..\YamlTestData.res'}
{$WARN SYMBOL_PLATFORM OFF}

uses
  System.SysUtils,
  System.Types,
  System.Zip,
  System.IOUtils,
  Neslib.LibYaml,
  Neslib.Utf8;

var
  GTestDataStream: TStream = nil;
  GTestDataZipFile: TZipFile = nil;
  GTestDataDirectories: TArray<String> = nil;
  GStringBuilder: TStringBuilder = nil;

const
  { These tests fail with LibYaml. }
  KNOWN_FAILED_TESTS: array [0..36] of String = ('2JQS/', '2LFX/', '2SXE/',
    '4ABK/', '4MUZ/', '5MUD/', '6BCT/', '6LVF/', '6M2F/', '7MNF/', '7Z25/',
    '8XYN/', '9CWY/', '9MMW/', '9SA2/', 'A2M4/', 'BEC7/', 'DBG4/', 'DK3J/',
    'FP8R/', 'FRK4/', 'H7J7/', 'HWV9/', 'K3WX/', 'KZN9/', 'M7A3/', 'NHX8/',
    'NJ66/', 'P2EQ/', 'Q5MG/', 'QT73/', 'R4YG/', 'S3PD/', 'UT92/', 'W5VH/',
    'WZ62/', 'Y2GN/');

function OpenTestData: TArray<String>;
var
  Infos: TArray<TZipHeader>;
  Attrs: TFileAttributes;
  KnownFailedTests: TStringList;
  I, Count: Integer;
  S: String;
begin
  if (GTestDataStream = nil) then
  begin
    System.Assert(GTestDataZipFile = nil);
    GTestDataStream := TResourceStream.Create(HInstance, 'YAML_TEST_DATA', RT_RCDATA);
    GTestDataZipFile := TZipFile.Create;
    GTestDataZipFile.Open(GTestDataStream, TZipMode.zmRead);
    RegisterExpectedMemoryLeak(GTestDataZipFile.Encoding); { Memory leak in System.Zip! }
    GStringBuilder := TStringBuilder.Create;

    KnownFailedTests := TStringList.Create;
    try
      KnownFailedTests.Sorted := True;
      KnownFailedTests.CaseSensitive := False;
      KnownFailedTests.Duplicates := dupError;
      for S in KNOWN_FAILED_TESTS do
        KnownFailedTests.Add(S);

      Infos := GTestDataZipFile.FileInfos;
      SetLength(GTestDataDirectories, Length(Infos));
      Count := 0;
      for I := 0 to Length(Infos) - 1 do
      begin
        Attrs := TFile.IntegerToFileAttributes(Infos[I].ExternalAttributes);
        if (TFileAttribute.faDirectory in Attrs) then
        begin
          S := GTestDataZipFile.FileName[I];
          if (KnownFailedTests.IndexOf(S) < 0) then
          begin
            GTestDataDirectories[Count] := S;
            Inc(Count);
          end;
        end;
      end;
    finally
      KnownFailedTests.Free;
    end;
    SetLength(GTestDataDirectories, Count);
  end;

  Result := GTestDataDirectories;
end;

procedure CloseTestData;
begin
  GStringBuilder.Free;
  GTestDataZipFile.Free;
  GTestDataStream.Free;
end;

function Unzip(const APath: String): String;
var
  Index: Integer;
  Bytes: TBytes;
begin
  Index := GTestDataZipFile.IndexOf(APath);
  if (Index < 0) then
    Exit('');

  GTestDataZipFile.Read(Index, Bytes);
  Result := TEncoding.UTF8.GetString(Bytes);
end;

function Escaped(const AStr: String): String; overload;
var
  C: Char;
begin
  System.Assert(GStringBuilder <> nil);
  GStringBuilder.Clear;
  for C in AStr do
  begin
    case C of
      '\': GStringBuilder.Append('\\');
       #0: GStringBuilder.Append('\0');
       #8: GStringBuilder.Append('\b');
       #9: GStringBuilder.Append('\t');
      #10: GStringBuilder.Append('\n');
      #13: GStringBuilder.Append('\r');
    else
      GStringBuilder.Append(C);
    end;
  end;
  Result := GStringBuilder.ToString;
end;

function Escaped(const AStr: Pyaml_char_t; const ALength: NativeInt): String; overload;
var
  S: String;
begin
  S := Utf8ToUtf16(AStr, ALength);
  Result := Escaped(S);
end;

function Unescaped(const AStr: String): String;
var
  I: Integer;
  C: Char;
begin
  System.Assert(GStringBuilder <> nil);
  GStringBuilder.Clear;
  I := 0;
  while (I < AStr.Length) do
  begin
    C := AStr.Chars[I];
    if (C = '\') then
    begin
      Inc(I);
      Assert.AreNotEqual(I, AStr.Length);
      case AStr.Chars[I] of
        '\': GStringBuilder.Append('\');
        '0': GStringBuilder.Append(#0);
        'b': GStringBuilder.Append(#8);
        't': GStringBuilder.Append(#9);
        'n': GStringBuilder.Append(#10);
        'r': GStringBuilder.Append(#13);
      else
        Assert.Fail('Invalid escape code');
      end;
    end
    else
      GStringBuilder.Append(C);

    Inc(I);
  end;
  Result := GStringBuilder.ToString;
end;

function WriteHandler(AData, ABuffer: Pointer; ASize: NativeInt): Integer; cdecl;
var
  Stream: TMemoryStream absolute AData;
begin
  System.Assert(AData <> nil);
  System.Assert(TObject(AData) is TMemoryStream);
  Result := Ord(Stream.Write(ABuffer^, ASize) = ASize);
end;

{ TTestData }

procedure TTestData.Free;
begin
  TestEvent.Free;
end;

procedure TTestData.Init;
begin
  TestEvent := TStringList.Create;
end;

function TTestData.Load(const ADirectory: String): Boolean;
begin
  InYaml := Unzip(ADirectory + 'in.yaml');
  OutYaml := Unzip(ADirectory + 'out.yaml');
  TestEvent.Text := Unzip(ADirectory + 'test.event');

  if (InYaml = '') or (InYaml.Contains('%YAML 1.2')) then
    { LibYaml does not support YAML 1.2 }
    Exit(False);

  Assert.AreNotEqual(0, TestEvent.Count, ADirectory);
  Result := True;
end;

{ TestSuiteCApi }

procedure TestSuiteCApi.TestEmit;
var
  Directory: String;
  TestData: TTestData;
begin
  TestData.Init;
  try
    for Directory in OpenTestData do
    begin
      if (TestData.Load(Directory)) and (TestData.OutYaml <> '') then
        TestEmitter(Directory, TestData);
    end;
  finally
    TestData.Free;
  end;
end;

procedure TestSuiteCApi.TestEmitter(const ADirectory: String;
  const ATestData: TTestData);
var
  Emitter: yaml_emitter_t;
  Event: yaml_event_t;
  Stream: TMemoryStream;
  S, Key, Value: String;
  Anchor, Tag, UtfValue: UTF8String;
  AnchorPtr, TagPtr, ValuePtr: PUTF8Char;
  Style: yaml_scalar_style_t;
  ActualStream, ExpectedStream: IYamlStream;
  ActualDocument, ExpectedDocument: IYamlDocument;
  I: Integer;
begin
  Assert.AreNotEqual('', ATestData.OutYaml, ADirectory);
  Assert.AreNotEqual(0, ATestData.TestEvent.Count, ADirectory);

  Stream := nil;
  Assert.AreNotEqual(0, yaml_emitter_initialize(@Emitter), ADirectory);
  try
    Stream := TMemoryStream.Create;
    yaml_emitter_set_output(@Emitter, WriteHandler, Stream);
    yaml_emitter_set_canonical(@Emitter, 0);
    yaml_emitter_set_indent(@Emitter, 2);
    yaml_emitter_set_width(@Emitter, -1);
    yaml_emitter_set_break(@Emitter, YAML_LN_BREAK);

    for S in ATestData.TestEvent do
    begin
      I := S.IndexOf(' ');
      if (I < 0) then
      begin
        Key := S.Trim;
        Value := '';
      end
      else
      begin
        Key := S.Substring(0, I).Trim;
        Value := S.Substring(I + 1);
      end;

      AnchorPtr := nil;
      TagPtr := nil;

      if (Value.StartsWith('&')) then
      begin
        I := Value.IndexOf(' ');
        if (I < 0) then
        begin
          Anchor := UTF8String(Value.Substring(1));
          Value := '';
        end
        else
        begin
          Anchor := UTF8String(Value.Substring(1, I - 1));
          Value := Value.Substring(I + 1);
        end;

        AnchorPtr := Pointer(Anchor);
      end;

      if (Value.StartsWith('<')) then
      begin
        I := Value.IndexOf(' ');
        if (I < 0) then
        begin
          Assert.AreEqual('>', Value.Chars[Value.Length - 1], ADirectory);
          Tag := UTF8String(Value.Substring(1, Value.Length - 2));
          Value := '';
        end
        else
        begin
          Assert.AreEqual('>', Value.Chars[I - 1], ADirectory);
          Tag := UTF8String(Value.Substring(1, I - 2));
          Value := Value.Substring(I + 1);
        end;

        TagPtr := Pointer(Tag);
      end;

      if (Key = '+STR') then
        yaml_stream_start_event_initialize(@Event, YAML_ANY_ENCODING)
      else if (Key = '-STR') then
        yaml_stream_end_event_initialize(@Event)
      else if (Key = '+DOC') then
        yaml_document_start_event_initialize(@Event, nil, nil, nil, Ord(Value <> '---'))
      else if (Key = '-DOC') then
        yaml_document_end_event_initialize(@Event, Ord(Value <> '...'))
      else if (Key = '+SEQ') then
        yaml_sequence_start_event_initialize(@Event, AnchorPtr, TagPtr, 0, YAML_BLOCK_SEQUENCE_STYLE)
      else if (Key = '-SEQ') then
        yaml_sequence_end_event_initialize(@Event)
      else if (Key = '+MAP') then
        yaml_mapping_start_event_initialize(@Event, AnchorPtr, TagPtr, 0, YAML_BLOCK_MAPPING_STYLE)
      else if (Key = '-MAP') then
        yaml_mapping_end_event_initialize(@Event)
      else if (Key = '=ALI') then
      begin
        Assert.AreNotEqual('', Value, ADirectory);
        Assert.AreEqual('*', Value.Chars[0], ADirectory);
        Anchor := UTF8String(Value.Substring(1));
        AnchorPtr := Pointer(Anchor);
        yaml_alias_event_initialize(@Event, AnchorPtr);
      end
      else if (Key = '=VAL') then
      begin
        Style := YAML_ANY_SCALAR_STYLE;
        Assert.AreNotEqual('', Value, ADirectory);
        case Value.Chars[0] of
          ':' : Style := YAML_PLAIN_SCALAR_STYLE;
          '''': Style := YAML_SINGLE_QUOTED_SCALAR_STYLE;
          '"' : Style := YAML_DOUBLE_QUOTED_SCALAR_STYLE;
          '|' : Style := YAML_LITERAL_SCALAR_STYLE;
          '>' : Style := YAML_FOLDED_SCALAR_STYLE;
        else
          Assert.Fail('Invalid scalar style "' + S + '" in ' + ADirectory);
        end;

        UtfValue := UTF8String(Unescaped(Value.Substring(1)));
        if (UtfValue = '') then
          ValuePtr := nil
        else
          ValuePtr := Pointer(UtfValue);

        yaml_scalar_event_initialize(@Event, AnchorPtr, TagPtr, ValuePtr,
          Length(UtfValue), Ord(TagPtr = nil), Ord(TagPtr = nil), Style);
      end
      else
        Assert.Fail('Invalid TestEvent "' + S + '" in ' + ADirectory);

      if (yaml_emitter_emit(@Emitter, @Event) = 0) then
        Assert.Fail(Format('(%s) %s', [ADirectory, String(UTF8String(Emitter.problem))]));
    end;

    S := Utf8ToUtf16(Stream.Memory, Stream.Size);
  finally
    Stream.Free;
    yaml_emitter_delete(@Emitter);
  end;

  if (ADirectory = 'DFF7/') then
    { LibYaml cannot parse ATestData.OutYaml for this test case.
      However, it can parse our emitter output, so we don't regard this a
      failure. }
    Exit;

  ExpectedStream := TYamlStream.Parse(ATestData.OutYaml);
  ActualStream := TYamlStream.Parse(S);
  Assert.AreEqual(ExpectedStream.DocumentCount, ActualStream.DocumentCount, ADirectory);
  for I := 0 to ExpectedStream.DocumentCount - 1 do
  begin
    ExpectedDocument := ExpectedStream.Documents[I];
    ActualDocument := ActualStream.Documents[I];
    Assert.IsTrue(ActualDocument.Root.StrictEquals(ExpectedDocument.Root), ADirectory);
  end;
end;

procedure TestSuiteCApi.TestParse;
var
  Directory: String;
  TestData: TTestData;
begin
  TestData.Init;
  try
    for Directory in OpenTestData do
    begin
      if (TestData.Load(Directory)) then
        TestParser(Directory, TestData);
    end;
  finally
    TestData.Free;
  end;
end;

procedure TestSuiteCApi.TestParser(const ADirectory: String;
  const ATestData: TTestData);
var
  Parser: yaml_parser_t;
  Event: yaml_event_t;
  Content: TBytes;
  S, Key, Value, Actual: String;
  I: Integer;

  function GetAnchorAndTag(const AAnchor, ATag: PUTF8Char): String;
  begin
    Result := '';
    if (AAnchor <> nil) then
      Result := Result + ' &' + String(UTF8String(AAnchor));
    if (ATag <> nil) then
      Result := Result + ' <' + String(UTF8String(ATag)) + '>';
  end;

begin
  Assert.AreNotEqual('', ATestData.InYaml, ADirectory);
  Assert.AreNotEqual(0, ATestData.TestEvent.Count, ADirectory);
  Assert.AreNotEqual(0, yaml_parser_initialize(@Parser), ADirectory);
  try
    Content := TEncoding.UTF8.GetBytes(ATestData.InYaml);
    yaml_parser_set_input_string(@Parser, Content, Length(Content));

    for S in ATestData.TestEvent do
    begin
      if (yaml_parser_parse(@Parser, @Event) = 0) then
        Assert.Fail(Format('(%s) %s', [ADirectory, String(UTF8String(Parser.problem))]));

      try
        I := S.IndexOf(' ');
        if (I < 0) then
        begin
          Key := S.Trim;
          Value := '';
        end
        else
        begin
          Key := S.Substring(0, I).Trim;
          Value := S.Substring(I).TrimRight;
        end;

        case Event._type of
          YAML_NO_EVENT:
            Assert.Fail('YAML_NO_EVENT in ' + ADirectory);

          YAML_STREAM_START_EVENT:
            begin
              Assert.AreEqual(Key, '+STR', ADirectory);
              Assert.AreEqual('', Value, ADirectory);
            end;

          YAML_STREAM_END_EVENT:
            begin
              Assert.AreEqual(Key, '-STR', ADirectory);
              Assert.AreEqual('', Value, ADirectory);
            end;

          YAML_DOCUMENT_START_EVENT:
            begin
              Assert.AreEqual(Key, '+DOC', ADirectory);
              if (Event.data.document_start.implicit = 0) then
                Assert.AreEqual(' ---', Value, ADirectory)
              else
                Assert.AreEqual('', Value, ADirectory)
            end;

          YAML_DOCUMENT_END_EVENT:
            begin
              Assert.AreEqual(Key, '-DOC', ADirectory);
              if (Event.data.document_end.implicit = 0) then
                Assert.AreEqual(' ...', Value, ADirectory)
              else
                Assert.AreEqual('', Value, ADirectory)
            end;

          YAML_ALIAS_EVENT:
            begin
              Assert.AreEqual(Key, '=ALI', ADirectory);
              Actual := ' *' + String(UTF8String(Event.data.alias.anchor));
              Assert.AreEqual(Value, Actual, ADirectory);
            end;

          YAML_SCALAR_EVENT:
            begin
              Assert.AreEqual(Key, '=VAL', ADirectory);
              Actual := GetAnchorAndTag(Event.data.scalar.anchor, Event.data.scalar.tag);

              case Event.data.scalar.style of
                YAML_PLAIN_SCALAR_STYLE:
                  Actual := Actual + ' :';

                YAML_SINGLE_QUOTED_SCALAR_STYLE:
                  Actual := Actual + ' ''';

                YAML_DOUBLE_QUOTED_SCALAR_STYLE:
                  Actual := Actual + ' "';

                YAML_LITERAL_SCALAR_STYLE:
                  Actual := Actual + ' |';

                YAML_FOLDED_SCALAR_STYLE:
                  Actual := Actual + ' >';

                YAML_ANY_SCALAR_STYLE:
                  Assert.Fail('Invalid scalar style in ' + ADirectory);
              end;

              Actual := Actual + Escaped(Event.data.scalar.value, Event.data.scalar.length).TrimRight;
              Assert.AreEqual(Value, Actual, ADirectory);
            end;

          YAML_SEQUENCE_START_EVENT:
            begin
              Assert.AreEqual(Key, '+SEQ', ADirectory);
              Actual := GetAnchorAndTag(Event.data.sequence_start.anchor, Event.data.sequence_start.tag);
              Assert.AreEqual(Actual, Value, ADirectory);
            end;

          YAML_SEQUENCE_END_EVENT:
            begin
              Assert.AreEqual(Key, '-SEQ', ADirectory);
              Assert.AreEqual('', Value, ADirectory);
            end;

          YAML_MAPPING_START_EVENT:
            begin
              Assert.AreEqual(Key, '+MAP', ADirectory);
              Actual := GetAnchorAndTag(Event.data.mapping_start.anchor, Event.data.mapping_start.tag);
              Assert.AreEqual(Actual, Value, ADirectory);
            end;

          YAML_MAPPING_END_EVENT:
            begin
              Assert.AreEqual(Key, '-MAP', ADirectory);
              Assert.AreEqual('', Value, ADirectory);
            end
        else
          Assert.Fail('Unknown event type in ' + ADirectory);
        end;
      finally
        yaml_event_delete(@Event);
      end;
    end;
  finally
    yaml_parser_delete(@Parser);
  end;
end;

{ TestSuiteParser }

procedure TestSuiteParser.CheckEvent(const AExpectedKey, AExpectedValue: String);
var
  S, Key, Value: String;
  I: Integer;
begin
  Assert.IsTrue(FEventIndex < FEvents.Count, FDirectory);

  S := FEvents[FEventIndex];
  Inc(FEventIndex);

  I := S.IndexOf(' ');
  if (I < 0) then
  begin
    Key := S.Trim;
    Value := '';
  end
  else
  begin
    Key := S.Substring(0, I).Trim;
    Value := S.Substring(I).TrimRight;
  end;

  Assert.AreEqual(AExpectedKey, Key, FDirectory);
  Assert.AreEqual(AExpectedValue, Value, FDirectory);
end;

function TestSuiteParser.GetAnchorAndTag(const ANode: TYamlNode): String;
begin
  Result := '';
  if (ANode.Anchor <> '') then
    Result := Result + ' &' + ANode.Anchor;
  if (ANode.Tag <> '') then
    Result := Result + ' <' + ANode.Tag + '>';
end;

procedure TestSuiteParser.TestAlias(const ANode: TYamlNode);
var
  Value: String;
begin
  Value := ' *' + ANode.Target.Anchor;
  CheckEvent('=ALI', Value);
end;

procedure TestSuiteParser.TestDocument(const ADocument: IYamlDocument);
begin
  if (TYamlDocumentFlag.ImplicitStart in ADocument.Flags) then
    CheckEvent('+DOC', '')
  else
    CheckEvent('+DOC', ' ---');

  TestNode(ADocument.Root);

  if (TYamlDocumentFlag.ImplicitEnd in ADocument.Flags) then
    CheckEvent('-DOC', '')
  else
    CheckEvent('-DOC', ' ...');
end;

procedure TestSuiteParser.TestMapping(const ANode: TYamlNode);
var
  I: Integer;
  E: PYamlElement;
begin
  CheckEvent('+MAP', GetAnchorAndTag(ANode));

  for I := 0 to ANode.Count - 1 do
  begin
    E := ANode.Elements[I];
    TestNode(E.Key);
    TestNode(E.Value);
  end;

  CheckEvent('-MAP', '')
end;

procedure TestSuiteParser.TestNode(const ANode: TYamlNode);
begin
  case ANode.NodeType of
    TYamlNodeType.Scalar:
      TestScalar(ANode);

    TYamlNodeType.Sequence:
      TestSequence(ANode);

    TYamlNodeType.Mapping:
      TestMapping(ANode);

    TYamlNodeType.Alias:
      TestAlias(ANode);
  else
    Assert.Fail('Invalid node type in ' + FDirectory);
  end;
end;

procedure TestSuiteParser.TestParse;
var
  Directory: String;
  TestData: TTestData;
begin
  TestData.Init;
  try
    for Directory in OpenTestData do
    begin
      if (TestData.Load(Directory)) then
      begin
        FDirectory := Directory;
        TestParser(TestData);
      end;
    end;
  finally
    TestData.Free;
  end;
end;

procedure TestSuiteParser.TestParser(const ATestData: TTestData);
var
  Stream: IYamlStream;
  I: Integer;
begin
  Assert.AreNotEqual('', ATestData.InYaml, FDirectory);
  Assert.AreNotEqual(0, ATestData.TestEvent.Count, FDirectory);

  if (ATestData.TestEvent[ATestData.TestEvent.Count - 1] <> '-STR') then
    { This is an incomplete stream. Ignore it. }
    Exit;

  FEventIndex := 0;
  FEvents := ATestData.TestEvent;
  CheckEvent('+STR', '');

  Stream := TYamlStream.Parse(ATestData.InYaml);

  if (Stream = nil) then
  begin
    { This can only happen for empty streams. }
    Assert.AreEqual(2, ATestData.TestEvent.Count, FDirectory);
    Assert.AreEqual('-STR', ATestData.TestEvent[1], FDirectory);
    Exit;
  end;

  for I := 0 to Stream.DocumentCount - 1 do
    TestDocument(Stream.Documents[I]);

  CheckEvent('-STR', '');
end;

procedure TestSuiteParser.TestScalar(const ANode: TYamlNode);
var
  Value: String;
begin
  Value := GetAnchorAndTag(ANode);
  case ANode.ScalarStyle of
    TYamlScalarStyle.Plain:
      Value := Value + ' :';

    TYamlScalarStyle.SingleQuoted:
      Value := Value + ' ''';

    TYamlScalarStyle.DoubleQuoted:
      Value := Value + ' "';

    TYamlScalarStyle.Literal:
      Value := Value + ' |';

    TYamlScalarStyle.Folded:
      Value := Value + ' >';
  else
    Assert.Fail('Invalid scalar style in ' + FDirectory);
  end;

  Value := Value + Escaped(ANode.ToString).TrimRight;
  CheckEvent('=VAL', Value);
end;

procedure TestSuiteParser.TestSequence(const ANode: TYamlNode);
var
  I: Integer;
begin
  CheckEvent('+SEQ', GetAnchorAndTag(ANode));

  for I := 0 to ANode.Count - 1 do
    TestNode(ANode.Nodes[I]);

  CheckEvent('-SEQ', '')
end;

{ TestSuiteGenerator }

function TestSuiteGenerator.GetAnchor(const AValue: String): TYamlNode;
var
  Anchor: String;
begin
  Assert.IsTrue(AValue.StartsWith('*'), FDirectory);

  Anchor := AValue.Substring(1);
  Assert.IsTrue(FAnchors.TryGetValue(Anchor, Result), FDirectory);
end;

function TestSuiteGenerator.GetEvent: TEvent;
var
  S: String;
  I: Integer;
begin
  Assert.IsTrue(FEventIndex < FEvents.Count, FDirectory);
  Result.Anchor := '';
  Result.Tag := '';

  S := FEvents[FEventIndex];
  Inc(FEventIndex);

  I := S.IndexOf(' ');
  if (I < 0) then
  begin
    Result.Key := S.Trim;
    Result.Value := '';
  end
  else
  begin
    Result.Key := S.Substring(0, I).Trim;
    Result.Value := S.Substring(I).TrimLeft;

    if (Result.Value.StartsWith('&')) then
    begin
      I := Result.Value.IndexOf(' ');
      if (I < 0) then
      begin
        Result.Anchor := Result.Value.Substring(1);
        Result.Value := '';
      end
      else
      begin
        Result.Anchor := Result.Value.Substring(1, I - 1);
        Result.Value := Result.Value.Substring(I + 1);
      end;
    end;

    if (Result.Value.StartsWith('<')) then
    begin
      I := Result.Value.IndexOf(' ');
      if (I < 0) then
      begin
        Assert.AreEqual('>', Result.Value.Chars[Result.Value.Length - 1], FDirectory);
        Result.Tag := Result.Value.Substring(1, Result.Value.Length - 2);
        Result.Value := '';
      end
      else
      begin
        Assert.AreEqual('>', Result.Value.Chars[I - 1], FDirectory);
        Result.Tag := Result.Value.Substring(1, I - 2);
        Result.Value := Result.Value.Substring(I + 1);
      end;
    end;
  end;

  Result.Value := Unescaped(Result.Value);
end;

function TestSuiteGenerator.GetScalar(const AValue: String): TScalar;
begin
  Assert.AreNotEqual('', AValue);
  case AValue.Chars[0] of
    ':': Result.Style := TYamlScalarStyle.Plain;
   '''': Result.Style := TYamlScalarStyle.SingleQuoted;
    '"': Result.Style := TYamlScalarStyle.DoubleQuoted;
    '|': Result.Style := TYamlScalarStyle.Literal;
    '>': Result.Style := TYamlScalarStyle.Folded;
  else
    Assert.Fail('Invalid scalar style in ' + FDirectory);
  end;
  Result.Value := AValue.Substring(1);
end;

procedure TestSuiteGenerator.SetupMapping(const ANode: TYamlNode);
var
  Event: TEvent;
  Key: IYamlKey;
  Scalar: TScalar;
  Child: TYamlNode;
begin
  while True do
  begin
    Event := GetEvent;
    if (Event.Key = '-MAP') then
      Break;

    if (Event.Key = '=VAL') then
      Key := TYamlNode.CreateScalarKey(GetScalar(Event.Value).Value)
    else if (Event.Key = '=ALI') then
      Key := TYamlNode.CreateAliasKey(GetAnchor(Event.Value))
    else if (Event.Key = '+MAP') then
      Key := TYamlNode.CreateMappingKey
    else if (Event.Key = '+SEQ') then
      Key := TYamlNode.CreateSequenceKey
    else
      Assert.Fail('Invalid mapping key type in ' + FDirectory);

    SetupNode(Key.Node^, Event);

    Event := GetEvent;
    if (Event.Key = '+MAP') then
      Child := ANode.AddOrSetMapping(Key)
    else if (Event.Key = '+SEQ') then
      Child := ANode.AddOrSetSequence(Key)
    else if (Event.Key = '=ALI') then
      Child := ANode.AddOrSetAlias(Key, GetAnchor(Event.Value))
    else if (Event.Key = '=VAL') then
    begin
      Scalar := GetScalar(Event.Value);
      Child := ANode.AddOrSetValue(Key, Scalar.Value);
      Child.ScalarStyle := Scalar.Style;
    end
    else
      Assert.Fail('Invalid mapping value type in ' + FDirectory);

    SetupNode(Child, Event);
  end;

  Assert.AreEqual('-MAP', Event.Key);
  Assert.AreEqual('', Event.Value);
end;

procedure TestSuiteGenerator.SetupNode(const ANode: TYamlNode;
  const AEvent: TEvent);
begin
  ANode.Anchor := AEvent.Anchor;
  ANode.Tag := AEvent.Tag;

  if (AEvent.Anchor <> '') then
    FAnchors.AddOrSetValue(AEvent.Anchor, ANode);

  case ANode.NodeType of
    TYamlNodeType.Sequence:
      SetupSequence(ANode);

    TYamlNodeType.Mapping:
      SetupMapping(ANode);

    TYamlNodeType.Scalar,
    TYamlNodeType.Alias:
      { Nothing to setup };
  else
    Assert.Fail('Invalid node type in ' + FDirectory);
  end;
end;

procedure TestSuiteGenerator.SetupSequence(const ANode: TYamlNode);
var
  Event: TEvent;
  Child: TYamlNode;
  Scalar: TScalar;
begin
  while True do
  begin
    Event := GetEvent;
    if (Event.Key = '-SEQ') then
      Break;

    if (Event.Key = '+MAP') then
      Child := ANode.AddMapping
    else if (Event.Key = '+SEQ') then
      Child := ANode.AddSequence
    else if (Event.Key = '=ALI') then
      Child := ANode.AddAlias(GetAnchor(Event.Value))
    else if (Event.Key = '=VAL') then
    begin
      Scalar := GetScalar(Event.Value);
      Child := ANode.Add(Scalar.Value);
      Child.ScalarStyle := Scalar.Style;
    end
    else
      Assert.Fail('Invalid event type in ' + FDirectory);

    SetupNode(Child, Event);
  end;

  Assert.AreEqual('-SEQ', Event.Key);
  Assert.AreEqual('', Event.Value);
end;

procedure TestSuiteGenerator.TestGenerate;
var
  Directory: String;
  TestData: TTestData;
begin
  TestData.Init;
  try
    FAnchors := TDictionary<String, TYamlNode>.Create;
    try
      for Directory in OpenTestData do
      begin
        if (TestData.Load(Directory)) and (TestData.OutYaml <> '') then
        begin
          FDirectory := Directory;
          TestGenerator(TestData);
        end;
      end;
    finally
      FAnchors.Free;
    end;
  finally
    TestData.Free;
  end;
end;

procedure TestSuiteGenerator.TestGenerator(const ATestData: TTestData);
var
  Event: TEvent;
  Stream, ExpectedStream: IYamlStream;
  Scalar: TScalar;
  Doc, ExpectedDoc: IYamlDocument;
  DocFlags: TYamlDocumentFlags;
  I: Integer;
begin
  Assert.AreNotEqual('', ATestData.OutYaml, FDirectory);
  Assert.AreNotEqual(0, ATestData.TestEvent.Count, FDirectory);

  if (ATestData.TestEvent[ATestData.TestEvent.Count - 1] <> '-STR') then
    { This is an incomplete stream. Ignore it. }
    Exit;

  FEventIndex := 0;
  FEvents := ATestData.TestEvent;

  Event := GetEvent;
  Assert.AreEqual('+STR', Event.Key, FDirectory);
  Assert.AreEqual('', Event.Value, FDirectory);

  Stream := TYamlStream.Create;
  while (True) do
  begin
    Event := GetEvent;
    if (Event.Key = '-STR') then
      Break;

    Assert.AreEqual('+DOC', Event.Key, FDirectory);
    FAnchors.Clear;

    DocFlags := [];
    if (Event.Value = '') then
      DocFlags := [TYamlDocumentFlag.ImplicitStart]
    else
      Assert.AreEqual('---', Event.Value, FDirectory);

    Event := GetEvent;
    if (Event.Key = '+SEQ') then
      Doc := Stream.AddSequence
    else if (Event.Key = '+MAP') then
      Doc := Stream.AddMapping
    else if (Event.Key = '=VAL') then
    begin
      Scalar := GetScalar(Event.Value);
      Doc := Stream.AddScalar(Scalar.Value);
      Doc.Root.ScalarStyle := Scalar.Style;
    end
    else
      Assert.Fail('Document must start with Sequence, Mapping or Scalar in ' + FDirectory);

    SetupNode(Doc.Root, Event);

    Event := GetEvent;
    Assert.AreEqual('-DOC', Event.Key, FDirectory);
    if (Event.Value = '') then
      Include(DocFlags, TYamlDocumentFlag.ImplicitEnd)
    else
      Assert.AreEqual('...', Event.Value, FDirectory);

    Doc.Flags := DocFlags;
  end;

  Assert.AreEqual('-STR', Event.Key, FDirectory);
  Assert.AreEqual('', Event.Value, FDirectory);

  if (FDirectory = 'DFF7/') then
    { LibYaml cannot parse ATestData.OutYaml for this test case.
      However, it can parse our emitter output, so we don't regard this a
      failure. }
    Exit;

  ExpectedStream := TYamlStream.Parse(ATestData.OutYaml);
  Assert.AreEqual(ExpectedStream.DocumentCount, Stream.DocumentCount, FDirectory);
  for I := 0 to ExpectedStream.DocumentCount - 1 do
  begin
    ExpectedDoc := ExpectedStream.Documents[I];
    Doc := Stream.Documents[I];
    Assert.IsTrue(Doc.Root.StrictEquals(ExpectedDoc.Root), FDirectory);
  end;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;
  TDUnitX.RegisterTestFixture(TestSuiteCApi);
  TDUnitX.RegisterTestFixture(TestSuiteParser);
  TDUnitX.RegisterTestFixture(TestSuiteGenerator);

finalization
  CloseTestData;

end.
