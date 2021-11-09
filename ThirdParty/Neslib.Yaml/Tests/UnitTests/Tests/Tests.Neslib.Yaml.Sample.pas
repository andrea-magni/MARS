unit Tests.Neslib.Yaml.Sample;

interface

uses
  DUnitX.TestFramework,
  Neslib.Yaml;

type
  TestYamlSample = class
  private
    procedure CheckStream(const AStream: IYamlStream);
    procedure CheckDocument0(const ADocument: IYamlDocument);
    procedure CheckDocument1(const ADocument: IYamlDocument);
    procedure CheckDocument2(const ADocument: IYamlDocument);
    procedure BuildDocument0(const ADocument: IYamlDocument);
    procedure BuildDocument1(const ADocument: IYamlDocument);
    procedure BuildDocument2(const ADocument: IYamlDocument);
  public
    [Test] procedure TestRoundTripStream;
    [Test] procedure TestRoundTripDocument;
    [Test] procedure TestCreateStream;
  end;

implementation

uses // For inlining
  System.Classes,
  System.SysUtils,
  Neslib.SysUtils,
  Neslib.Utf8;

const
  SAMPLE_DOCUMENT0 =
    'invoice: 34843' + sLineBreak +
    'date   : "2001-01-23"' + sLineBreak +
    'bill-to: &id001' + sLineBreak +
    '    given  : ''Chris''' + sLineBreak +
    '    family : Dumars' + sLineBreak +
    '    address:' + sLineBreak +
    '        lines: |' + sLineBreak +
    '            458 Walkman Dr.' + sLineBreak +
    '            Suite #292' + sLineBreak +
    '        city    : Royal Oak' + sLineBreak +
    '        state   : MI' + sLineBreak +
    '        postal  : 48046' + sLineBreak +
    'ship-to: *id001' + sLineBreak +
    'product:' + sLineBreak +
    '    - sku         : BL394D' + sLineBreak +
    '      quantity    : 4' + sLineBreak +
    '      description : Basketball' + sLineBreak +
    '      price       : 450.00' + sLineBreak +
    '    - sku         : BL4438H' + sLineBreak +
    '      quantity    : 1' + sLineBreak +
    '      description : Super Hoop' + sLineBreak +
    '      price       : 2392.00' + sLineBreak +
    'tax  : 251.42' + sLineBreak +
    'total: 4443.52' + sLineBreak +
    'comments: >' + sLineBreak +
    '    Late afternoon is best.' + sLineBreak +
    '    Backup contact is Nancy' + sLineBreak +
    '    Billsmer @ 338-4338.';

const
  SAMPLE_DOCUMENT1 =
    '[100, 12.5, -130, 1.3e+9]';

const
  SAMPLE_DOCUMENT2 =
    '%YAML 1.1' + sLineBreak +
    '%TAG ! tag:clarkevans.com,2002:' + sLineBreak +
    '%TAG !e! tag:e.com:' + sLineBreak +
    '%TAG !f! tag:f.com:' + sLineBreak +
    '---' + sLineBreak +
    'not-date: !!str 2002-04-28' + sLineBreak +
    'local-tag1: !foo bar' + sLineBreak +
    'local-tag2: !f!foo bar';

const
  SAMPLE_STREAM =
    '---' + sLineBreak +
    SAMPLE_DOCUMENT0 + sLineBreak +
    '---' + sLineBreak +
    SAMPLE_DOCUMENT1 + sLineBreak +
    SAMPLE_DOCUMENT2;

{ TestYamlSample }

procedure TestYamlSample.BuildDocument0(const ADocument: IYamlDocument);
var
  N1, N2, N3, BillTo: TYamlNode;
begin
  N1 := ADocument.Root;
  N1.MappingStyle := TYamlMappingStyle.Block;

  N1.AddOrSetValue('invoice', 34843).ScalarStyle := TYamlScalarStyle.Plain;
  N1.AddOrSetValue('date', '2001-01-23').ScalarStyle := TYamlScalarStyle.DoubleQuoted;

  BillTo := N1.AddOrSetMapping('bill-to');
  BillTo.MappingStyle := TYamlMappingStyle.Block;
  BillTo.Anchor := 'id001';
  BillTo.AddOrSetValue('given', 'Chris').ScalarStyle := TYamlScalarStyle.SingleQuoted;
  BillTo.AddOrSetValue('family', 'Dumars');

  N2 := BillTo.AddOrSetMapping('address');
  N2.MappingStyle := TYamlMappingStyle.Block;
  N2.AddOrSetValue('lines', '458 Walkman Dr.'#10'Suite #292'#10).ScalarStyle := TYamlScalarStyle.Literal;
  N2.AddOrSetValue('city', 'Royal Oak');
  N2.AddOrSetValue('state', 'MI');
  N2.AddOrSetValue('postal', 48046);

  N1.AddOrSetAlias('ship-to', BillTo);

  N2 := N1.AddOrSetSequence('product');
  N2.SequenceStyle := TYamlSequenceStyle.Block;

  N3 := N2.AddMapping;
  N3.AddOrSetValue('sku', 'BL394D');
  N3.AddOrSetValue('quantity', 4);
  N3.AddOrSetValue('description', 'Basketball');
  N3.AddOrSetValue('price', 450.00);

  N3 := N2.AddMapping;
  N3.AddOrSetValue('sku', 'BL4438H');
  N3.AddOrSetValue('quantity', 1);
  N3.AddOrSetValue('description', 'Super Hoop');
  N3.AddOrSetValue('price', 2392.00);

  N1.AddOrSetValue('tax', 251.42);
  N1.AddOrSetValue('total', 4443.52);
  N1.AddOrSetValue('comments', 'Late afternoon is best. '+
    'Backup contact is Nancy Billsmer @ 338-4338.'#10).ScalarStyle := TYamlScalarStyle.Folded;
end;

procedure TestYamlSample.BuildDocument1(const ADocument: IYamlDocument);
var
  N: TYamlNode;
begin
  N := ADocument.Root;
  N.SequenceStyle := TYamlSequenceStyle.Flow;
  N.Add(100);
  N.Add(12.5);
  N.Add(-130);
  N.Add(1.3e9);
end;

procedure TestYamlSample.BuildDocument2(const ADocument: IYamlDocument);
var
  N: TYamlNode;
begin
  ADocument.Version := TYamlVersion.Create(1, 1);
  ADocument.TagDirectives := TYamlTagDirectives.Create(
    TYamlTagDirective.Create('!', 'tag:clarkevans.com,2002:'),
    TYamlTagDirective.Create('!e!', 'tag:e.com:'),
    TYamlTagDirective.Create('!f!', 'tag:f.com:'));

  N := ADocument.Root;
  N.AddOrSetValue('not-date', '2002-04-28').Tag := YAML_TAG_STR;
  N.AddOrSetValue('local-tag1', 'bar').Tag := 'tag:clarkevans.com,2002:foo';
  N.AddOrSetValue('local-tag2', 'bar').Tag := 'tag:f.com:foo';
end;

procedure TestYamlSample.CheckDocument0(const ADocument: IYamlDocument);
var
  N1, N2, N3, N4: TYamlNode;
  Price: Double;
begin
  Assert.AreEqual(0, ADocument.Version.Major);
  Assert.AreEqual(0, ADocument.Version.Minor);

  N1 := ADocument.Root;
  Assert.IsTrue(N1.IsMapping);
  Assert.AreEqual<TYamlMappingStyle>(TYamlMappingStyle.Block, N1.MappingStyle);
  Assert.AreEqual(8, N1.Count);

  N2 := N1.Values['invoice'];
  Assert.AreEqual(34843, N2.ToInteger);
  Assert.AreEqual<TYamlScalarStyle>(TYamlScalarStyle.Plain, N2.ScalarStyle);

  N2 := N1.Values['date'];
  Assert.AreEqual('2001-01-23', N2.ToString);
  Assert.AreEqual<TYamlScalarStyle>(TYamlScalarStyle.DoubleQuoted, N2.ScalarStyle);

  N2 := N1.Values['bill-to'];
  Assert.IsTrue(N2.IsMapping);
  Assert.AreEqual('id001', N2.Anchor);

  N3 := N2.Values['given'];
  Assert.AreEqual('Chris', N3.ToString);
  Assert.AreEqual<TYamlScalarStyle>(TYamlScalarStyle.SingleQuoted, N3.ScalarStyle);

  Assert.AreEqual('Dumars', N2.Values['family'].ToString);

  N3 := N2.Values['address'];
  Assert.IsTrue(N3.IsMapping);

  N4 := N3.Values['lines'];
  Assert.AreEqual('458 Walkman Dr.'#10'Suite #292'#10, N4.ToString);
  Assert.AreEqual<TYamlScalarStyle>(TYamlScalarStyle.Literal, N4.ScalarStyle);

  Assert.AreEqual('Royal Oak', N3.Values['city'].ToString);
  Assert.AreEqual('MI', N3.Values['state'].ToString);
  Assert.AreEqual('48046', N3.Values['postal'].ToString);

  N2 := N1.Values['ship-to'];
  Assert.IsTrue(N2.IsAlias);
  Assert.IsTrue(N2.Target = N1.Values['bill-to']);

  N2 := N1.Values['product'];
  Assert.IsTrue(N2.IsSequence);
  Assert.AreEqual<TYamlSequenceStyle>(TYamlSequenceStyle.Block, N2.SequenceStyle);
  Assert.AreEqual(2, N2.Count);

  N3 := N2.Nodes[0];
  Assert.IsTrue(N3.IsMapping);
  Assert.AreEqual('BL394D', N3.Values['sku'].ToString);
  Assert.AreEqual(4, N3.Values['quantity'].ToInteger);
  Assert.AreEqual('BasketBall', N3.Values['description'].ToString);
  Assert.AreEqual<Double>(450.0, N3.Values['price'].ToDouble);

  N3 := N2.Nodes[1];
  Assert.IsTrue(N3.IsMapping);
  Assert.AreEqual('BL4438H', N3.Values['sku'].ToString);
  Assert.AreEqual(1, N3.Values['quantity'].ToInteger);
  Assert.AreEqual('Super Hoop', N3.Values['description'].ToString);
  Assert.AreEqual<Double>(2392.0, N3.Values['price'].ToDouble);

  N2 := N1.Values['tax'];
  Assert.IsTrue(N2.IsScalar);
  Assert.AreEqual<Double>(251.42, N2.ToDouble);

  N2 := N1.Values['total'];
  Assert.IsTrue(N2.IsScalar);
  Assert.AreEqual<Double>(4443.52, N2.ToDouble);

  N2 := N1.Values['comments'];
  Assert.IsTrue(N2.IsScalar);
  Assert.AreEqual<TYamlScalarStyle>(TYamlScalarStyle.Folded, N2.ScalarStyle);
  Assert.AreEqual('Late afternoon is best. Backup contact is Nancy Billsmer @ 338-4338.'#10, N2.ToString);

  Price := ADocument.Root.Values['product'].Nodes[0].Values['price'].ToDouble;
  Assert.AreEqual<Double>(450.00, Price);
end;

procedure TestYamlSample.CheckDocument1(const ADocument: IYamlDocument);
var
  N: TYamlNode;
begin
  N := ADocument.Root;
  Assert.IsTrue(N.IsSequence);
  Assert.AreEqual<TYamlSequenceStyle>(TYamlSequenceStyle.Flow, N.SequenceStyle);
  Assert.AreEqual(4, N.Count);

  Assert.AreEqual<Double>(100, N.Nodes[0].ToDouble);
  Assert.AreEqual<Double>(12.5, N.Nodes[1].ToDouble);
  Assert.AreEqual<Double>(-130, N.Nodes[2].ToDouble);
  Assert.AreEqual<Double>(1.3e9, N.Nodes[3].ToDouble);
end;

procedure TestYamlSample.CheckDocument2(const ADocument: IYamlDocument);
var
  N: TYamlNode;
begin
  Assert.AreEqual(1, ADocument.Version.Major);
  Assert.AreEqual(1, ADocument.Version.Minor);

  Assert.AreEqual<Integer>(3, Length(ADocument.TagDirectives));
  Assert.AreEqual<UTF8String>('!', ADocument.TagDirectives[0].Handle);
  Assert.AreEqual<UTF8String>('tag:clarkevans.com,2002:', ADocument.TagDirectives[0].Prefix);
  Assert.AreEqual<UTF8String>('!f!', ADocument.TagDirectives[2].Handle);
  Assert.AreEqual<UTF8String>('tag:f.com:', ADocument.TagDirectives[2].Prefix);

  N := ADocument.Root;
  Assert.IsTrue(N.IsMapping);
  Assert.AreEqual(3, N.Count);

  Assert.AreEqual('not-date', N.Elements[0].Key.ToString);
  Assert.AreEqual(YAML_TAG_STR, N.Elements[0].Value.Tag);
  Assert.AreEqual('2002-04-28', N.Elements[0].Value.ToString);

  Assert.AreEqual('local-tag1', N.Elements[1].Key.ToString);
  Assert.AreEqual('tag:clarkevans.com,2002:foo', N.Elements[1].Value.Tag);
  Assert.AreEqual('bar', N.Elements[1].Value.ToString);

  Assert.AreEqual('local-tag2', N.Elements[2].Key.ToString);
  Assert.AreEqual('tag:f.com:foo', N.Elements[2].Value.Tag);
  Assert.AreEqual('bar', N.Elements[2].Value.ToString);
end;

procedure TestYamlSample.CheckStream(const AStream: IYamlStream);
begin
  Assert.AreEqual(3, AStream.DocumentCount);
  CheckDocument0(AStream.Documents[0]);
  CheckDocument1(AStream.Documents[1]);
  CheckDocument2(AStream.Documents[2]);
end;

procedure TestYamlSample.TestCreateStream;
var
  Stream: IYamlStream;
  Document: IYamlDocument;
  Yaml: String;
begin
  Stream := TYamlStream.Create;

  Document := Stream.AddMapping;
  BuildDocument0(Document);
  CheckDocument0(Document);

  Document := Stream.AddSequence;
  BuildDocument1(Document);
  CheckDocument1(Document);

  Document := Stream.AddMapping;
  BuildDocument2(Document);
  CheckDocument2(Document);

  Yaml := Stream.ToYaml;

  Stream := nil;
  Stream := TYamlStream.Parse(Yaml);
  CheckStream(Stream);
end;

procedure TestYamlSample.TestRoundTripDocument;
var
  Document: IYamlDocument;
  Yaml: String;
begin
  Document := TYamlDocument.Parse(SAMPLE_DOCUMENT0 + sLineBreak);
  CheckDocument0(Document);

  Yaml := Document.ToYaml;
  Document := nil;

  Document := TYamlDocument.Parse(Yaml);
  CheckDocument0(Document);
end;

procedure TestYamlSample.TestRoundTripStream;
var
  Stream: IYamlStream;
  Yaml: String;
begin
  Stream := TYamlStream.Parse(SAMPLE_STREAM);
  CheckStream(Stream);

  Yaml := Stream.ToYaml;
  Stream := nil;

  Stream := TYamlStream.Parse(Yaml);
  CheckStream(Stream);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;
  TDUnitX.RegisterTestFixture(TestYamlSample);

end.
