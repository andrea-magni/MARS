unit Tests.MARSgen.JSONSource;

interface

uses
  System.SysUtils, System.JSON,
  DUnitX.TestFramework,
  MARS.Gen.JSONSource;

type
  [TestFixture('JSONSource')]
  TGenJSONSourceTest = class
  private
    function YAML(const AText: string): TJSONObject;
  public
    [Test]
    procedure DetectsTheFormat;
    [Test]
    procedure YAMLScalarsFollowTheCoreSchema;
    [Test]
    procedure YAMLNumbersKeepTheirText;
    [Test]
    procedure YAMLSequencesAndMappings;
    [Test]
    procedure YAMLAnchorsAliasesAndMergeKeys;
    [Test]
    procedure InvalidInputRaises;
    [Test]
    procedure InvalidYAMLDoesNotLeak;
  end;

implementation

{ TGenJSONSourceTest }

function TGenJSONSourceTest.YAML(const AText: string): TJSONObject;
var
  LValue: TJSONValue;
begin
  LValue := TGenJSONSource.FromText(AText, gsfYAML);
  Assert.IsTrue(LValue is TJSONObject, 'a mapping');
  Result := TJSONObject(LValue);
end;

procedure TGenJSONSourceTest.DetectsTheFormat;
begin
  Assert.AreEqual(gsfJSON, TGenJSONSource.DetectFormat('  {"a": 1}'));
  Assert.AreEqual(gsfJSON, TGenJSONSource.DetectFormat(#$FEFF'[1]'));
  Assert.AreEqual(gsfYAML, TGenJSONSource.DetectFormat('openapi: 3.0.0'));
  Assert.AreEqual(gsfYAML, TGenJSONSource.DetectFormat('# a comment'#10'a: 1'));
end;

procedure TGenJSONSourceTest.YAMLScalarsFollowTheCoreSchema;
var
  LObject: TJSONObject;
begin
  LObject := YAML(
    'int: 42'#10 +
    'negative: -7'#10 +
    'hex: 0x1F'#10 +
    'octal: 0o17'#10 +
    'float: 2.5'#10 +
    'quoted: "42"'#10 +
    'single: ''true'''#10 +
    'yes: yes'#10 +
    'bool: true'#10 +
    'boolUpper: FALSE'#10 +
    'tilde: ~'#10 +
    'empty:'#10 +
    'nullWord: null'#10 +
    'tagged: !!str 12'#10 +
    'text: hello world'#10 +
    'inf: .inf'
  );
  try
    Assert.IsTrue(LObject.GetValue('int') is TJSONNumber, 'int');
    Assert.AreEqual(42, TJSONNumber(LObject.GetValue('int')).AsInt);
    Assert.AreEqual(-7, TJSONNumber(LObject.GetValue('negative')).AsInt);
    Assert.AreEqual(31, TJSONNumber(LObject.GetValue('hex')).AsInt);
    Assert.AreEqual(15, TJSONNumber(LObject.GetValue('octal')).AsInt);
    Assert.AreEqual(Double(2.5), TJSONNumber(LObject.GetValue('float')).AsDouble);

    // quoted and tagged: strings
    Assert.IsFalse(LObject.GetValue('quoted') is TJSONNumber, 'quoted');
    Assert.AreEqual('42', LObject.GetValue('quoted').Value);
    Assert.AreEqual('true', LObject.GetValue('single').Value);
    Assert.IsFalse(LObject.GetValue('tagged') is TJSONNumber, 'tagged');
    Assert.AreEqual('12', LObject.GetValue('tagged').Value);

    // YAML 1.2: yes is a string, not a boolean
    Assert.IsFalse(LObject.GetValue('yes') is TJSONBool, 'yes');
    Assert.IsTrue(LObject.GetValue('bool') is TJSONTrue, 'true');
    Assert.IsTrue(LObject.GetValue('boolUpper') is TJSONFalse, 'FALSE');

    Assert.IsTrue(LObject.GetValue('tilde') is TJSONNull, '~');
    Assert.IsTrue(LObject.GetValue('empty') is TJSONNull, 'empty: ' + LObject.ToJSON);
    Assert.IsTrue(LObject.GetValue('nullWord') is TJSONNull, 'null');

    Assert.AreEqual('hello world', LObject.GetValue('text').Value);
    Assert.AreEqual('.inf', LObject.GetValue('inf').Value);
  finally
    LObject.Free;
  end;
end;

procedure TGenJSONSourceTest.YAMLNumbersKeepTheirText;
var
  LObject: TJSONObject;
begin
  LObject := YAML('version: 1.0'#10'half: .5'#10'big: 123456789012345678901234567890'#10'exp: 1e3');
  try
    // info.version: 1.0 must not become 1
    Assert.AreEqual('1.0', LObject.GetValue('version').Value);
    Assert.AreEqual('{"version":1.0,"half":0.5,"big":123456789012345678901234567890,"exp":1e3}', LObject.ToJSON);
  finally
    LObject.Free;
  end;
end;

procedure TGenJSONSourceTest.YAMLSequencesAndMappings;
var
  LObject: TJSONObject;
begin
  LObject := YAML(
    'list:'#10 +
    '  - a'#10 +
    '  - 2'#10 +
    '  - {x: 1}'#10 +
    'flow: [1, two, null]'#10 +
    'nested:'#10 +
    '  inner:'#10 +
    '    deep: true'#10 +
    '''200'':'#10 +
    '  description: a quoted numeric key'
  );
  try
    Assert.AreEqual('["a",2,{"x":1}]', LObject.GetValue('list').ToJSON);
    Assert.AreEqual('[1,"two",null]', LObject.GetValue('flow').ToJSON);
    Assert.AreEqual('{"inner":{"deep":true}}', LObject.GetValue('nested').ToJSON);
    Assert.IsNotNull(LObject.GetValue('200'), 'numeric key');
  finally
    LObject.Free;
  end;
end;

procedure TGenJSONSourceTest.YAMLAnchorsAliasesAndMergeKeys;
var
  LObject: TJSONObject;
begin
  LObject := YAML(
    'base: &base'#10 +
    '  x: 1'#10 +
    '  y: 2'#10 +
    'copy: *base'#10 +
    'derived:'#10 +
    '  <<: *base'#10 +
    '  y: 3'
  );
  try
    Assert.AreEqual('{"x":1,"y":2}', LObject.GetValue('copy').ToJSON);
    // a key of the mapping wins over the merged one
    Assert.AreEqual(1, TJSONObject(LObject.GetValue('derived')).GetValue<Integer>('x'));
    Assert.AreEqual(3, TJSONObject(LObject.GetValue('derived')).GetValue<Integer>('y'));
    Assert.IsNull(TJSONObject(LObject.GetValue('derived')).GetValue('<<'), 'no << key');
  finally
    LObject.Free;
  end;
end;

procedure TGenJSONSourceTest.InvalidInputRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      TGenJSONSource.FromText('{"a": ', gsfJSON).Free;
    end
  , EGenSourceError);
  Assert.WillRaise(
    procedure
    begin
      TGenJSONSource.FromText('a: [1, 2', gsfYAML).Free;
    end
  , EGenSourceError);
  Assert.WillRaise(
    procedure
    begin
      TGenJSONSource.FromFile('does-not-exist.yaml').Free;
    end
  , EGenSourceError);
end;

// bytes allocated by the memory manager (Windows: the tests run there)
{$WARN SYMBOL_PLATFORM OFF}
function MemoryInUse: NativeUInt;
var
  LState: TMemoryManagerState;
begin
  GetMemoryManagerState(LState);
  Result := LState.TotalAllocatedMediumBlockSize + LState.TotalAllocatedLargeBlockSize;
  for var LIndex := Low(LState.SmallBlockTypeStates) to High(LState.SmallBlockTypeStates) do
    Inc(Result, LState.SmallBlockTypeStates[LIndex].AllocatedBlockCount
      * LState.SmallBlockTypeStates[LIndex].UseableBlockSize);
end;
{$WARN SYMBOL_PLATFORM ON}

procedure TGenJSONSourceTest.InvalidYAMLDoesNotLeak;
const
  // an error inside a sequence, inside a mapping (after a key), an unknown alias, nested
  INVALID: array[0..3] of string = (
    'a: [1, 2',
    'a:'#10'  b: 1'#10' c: 2',
    'a: 1'#10'b: *nope',
    'root:'#10'  list:'#10'    - x: 1'#10'      y: [1, {z: 2'
  );
  ITERATIONS = 50;
var
  LBefore, LAfter: NativeUInt;

  procedure ParseAll;
  begin
    for var LText in INVALID do
      try
        TGenJSONSource.FromText(LText, gsfYAML).Free;
        Assert.Fail('no error for: ' + LText);
      except
        on E: EGenSourceError do ; // expected
      end;
  end;

begin
  // warm-up: one-time allocations (exception classes, regular expressions...)
  ParseAll;
  LBefore := MemoryInUse;
  for var LIteration := 1 to ITERATIONS do
    ParseAll;
  LAfter := MemoryInUse;
  Assert.IsTrue(LAfter <= LBefore + 1024
  , Format('%d bytes more after %d parses of invalid documents', [LAfter - LBefore, ITERATIONS * Length(INVALID)]));
end;

initialization
  TDUnitX.RegisterTestFixture(TGenJSONSourceTest);

end.
