unit FMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.TreeView,
  Neslib.Utf8,
  Neslib.Yaml;

type
  TFormMain = class(TForm)
    TreeView: TTreeView;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure DumpDocument(const ADoc: IYamlDocument;
      const AIndex: Integer);
    procedure DumpNode(const APrefix: String; const ANode: TYamlNode;
      const AParentItem: TTreeViewItem);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  System.TypInfo;

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

procedure TFormMain.DumpDocument(const ADoc: IYamlDocument;
  const AIndex: Integer);
var
  Item, Child: TTreeViewItem;
  S: String;
  I: Integer;
begin
  S := Format('Document %d (Flags=%s', [AIndex,
    SetToString(PTypeInfo(TypeInfo(TYamlDocumentFlags)), Byte(ADoc.Flags), True)]);

  if (ADoc.Version.Major <> 0) then
    S := S + Format(', version %d.%d', [ADoc.Version.Major, ADoc.Version.Minor]);

  Item := TTreeViewItem.Create(Self);
  Item.Text := S + ')';
  TreeView.AddObject(Item);

  for I := 0 to Length(ADoc.TagDirectives) - 1 do
  begin
    Child := TTreeViewItem.Create(Self);
    Child.Text := Format('TAG %s = %s', [ADoc.TagDirectives[I].Handle, ADoc.TagDirectives[I].Prefix]);
    Item.AddObject(Child);
  end;

  DumpNode('', ADoc.Root, Item);
end;

procedure TFormMain.DumpNode(const APrefix: String; const ANode: TYamlNode;
  const AParentItem: TTreeViewItem);
var
  Item: TTreeViewItem;
  S: String;
  I: Integer;
begin
  case ANode.NodeType of
    TYamlNodeType.Null:
      S := 'nil (';

    TYamlNodeType.Scalar:
      S := Format('"%s" (Scalar: Style=%s, Flags=%s', [ANode.ToString,
        GetEnumName(TypeInfo(TYamlScalarStyle), Ord(ANode.ScalarStyle)),
        SetToString(PTypeInfo(TypeInfo(TYamlScalarFlags)), Byte(ANode.ScalarFlags), True)]);

    TYamlNodeType.Sequence:
      S := Format('(Sequence: Style=%s', [
        GetEnumName(TypeInfo(TYamlSequenceStyle), Ord(ANode.SequenceStyle))]);

    TYamlNodeType.Mapping:
      S := Format('(Mapping: Style=%s', [
        GetEnumName(TypeInfo(TYamlMappingStyle), Ord(ANode.MappingStyle))]);

    TYamlNodeType.Alias:
      S := Format('(Alias: Target=%s', [ANode.Target.Anchor]);
  else
    Assert(False);
  end;

  if (ANode.Anchor <> '') then
    S := S + ', Anchor=' + ANode.Anchor;

  if (ANode.Tag <> '') then
    S := S + ', Tag=' + ANode.Tag;

  Item := TTreeViewItem.Create(Self);
  Item.Text := APrefix + S + ')';
  AParentItem.AddObject(Item);

  case ANode.NodeType of
    TYamlNodeType.Sequence:
      begin
        for I := 0 to ANode.Count - 1 do
          DumpNode(Format('ITEM %d: ', [I]), ANode.Nodes[I], Item);
      end;

    TYamlNodeType.Mapping:
      begin
        for I := 0 to ANode.Count - 1 do
        begin
          DumpNode(Format('KEY %d: ', [I]), ANode.Elements[I].Key, Item);
          DumpNode(Format('VAL %d: ', [I]), ANode.Elements[I].Value, Item);
        end;
      end;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  Stream: IYamlStream;
  I: Integer;
begin
  Stream := TYamlStream.Parse(SAMPLE_STREAM);
  TreeView.BeginUpdate;
  try
    for I := 0 to Stream.DocumentCount - 1 do
      DumpDocument(Stream.Documents[I], I);
    TreeView.ExpandAll;
  finally
    TreeView.EndUpdate;
  end;
end;

end.
