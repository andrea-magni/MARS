(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Gen.Emit.Datasets;

// Writes Data.<Api>: per object type of the model, a procedure adding its field definitions to a
// dataset (any TDataSet with FieldDefs: TFDMemTable, TVirtualTable, TClientDataSet), as the
// datasets of MARS read and write JSON (MARS.Data.Utils, JSONArrayToDataSet, DataSetToJSONArray):
// nested objects flattened (category.name), arrays and values without a type as JSON text.

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  MARS.Gen.Model, MARS.Gen.Types, MARS.Gen.Output;

type
  TGenDatasetField = record
    // the flattened JSON name: address.city
    Name: string;
    // ftWideString, ftInteger...
    DataType: string;
    // ftWideString: its size (0 otherwise)
    Size: Integer;
    Comment: string;
  end;

  TGenDatasetsEmitter = class
  private
    FTypes: TGenTypeMapper;
    FNames: TDictionary<TGenDelphiType, string>;
    procedure AddFields(const AType: TGenDelphiType; const APrefix: string;
      const AVisiting: TList<TGenDelphiType>; var AFields: TArray<TGenDatasetField>);
  public
    const
      STRING_SIZE = 255;
    constructor Create(const ATypes: TGenTypeMapper);
    destructor Destroy; override;
    // the object types with fields, in the order of the types unit
    function DatasetTypes: TArray<TGenDelphiType>;
    // the fields of a type, flattened
    function FieldsOf(const AType: TGenDelphiType): TArray<TGenDatasetField>;
    // Define<Name>: the procedure of a type
    function ProcedureName(const AType: TGenDelphiType): string;
    // Data.<Api>
    function UnitName: string;
    function DatasetsUnit: string;

    // Data.<Api> through AOutput; returns its name
    class function WriteTo(const ATypes: TGenTypeMapper; const AOutput: TGenOutput): string; static;
  end;

implementation

uses
  System.StrUtils,
  MARS.Gen.Naming;

const
  INDENT = '  ';

{ TGenDatasetsEmitter }

constructor TGenDatasetsEmitter.Create(const ATypes: TGenTypeMapper);
begin
  inherited Create;
  FTypes := ATypes;
  FNames := TDictionary<TGenDelphiType, string>.Create;
end;

destructor TGenDatasetsEmitter.Destroy;
begin
  FNames.Free;
  inherited;
end;

function TGenDatasetsEmitter.UnitName: string;
begin
  Result := TGenNaming.Sanitize(TGenNaming.PascalCase(FTypes.Api.Title));
  if Result = '_' then
    Result := 'Api';
  Result := 'Data.' + Result;
end;

function TGenDatasetsEmitter.DatasetTypes: TArray<TGenDelphiType>;
begin
  Result := [];
  for var LType in FTypes.Types do
    if (LType.Kind = TGenDelphiTypeKind.&Record) and (Length(FieldsOf(LType)) > 0) then
      Result := Result + [LType];
end;

function TGenDatasetsEmitter.ProcedureName(const AType: TGenDelphiType): string;
var
  LNames: TGenNameScope;
begin
  // TPet -> DefinePet, unique among the procedures of the unit
  if FNames.Count = 0 then
  begin
    LNames := TGenNameScope.Create;
    try
      for var LType in FTypes.Types do
        if LType.Kind = TGenDelphiTypeKind.&Record then
          FNames.Add(LType, LNames.Unique('Define' + LType.Name.TrimLeft(['&']).Substring(1)));
    finally
      LNames.Free;
    end;
  end;
  Result := FNames[AType];
end;

procedure TGenDatasetsEmitter.AddFields(const AType: TGenDelphiType; const APrefix: string;
  const AVisiting: TList<TGenDelphiType>; var AFields: TArray<TGenDatasetField>);
var
  LField: TGenDatasetField;
  LType: string;
  LSize: Integer;
begin
  AVisiting.Add(AType);
  try
    // classes: the fields of the base first, as in JSON
    if Assigned(AType.BaseType) and not AVisiting.Contains(AType.BaseType) then
      AddFields(AType.BaseType, APrefix, AVisiting, AFields);

    for var LItem in AType.Fields do
    begin
      LType := LItem.TypeName;
      LField.Name := APrefix + LItem.JSONName;
      LField.Size := 0;
      LField.Comment := '';

      // a nested object: its fields, flattened (one that contains itself: its JSON)
      if Assigned(LItem.Target) and not LItem.TargetViaArray and (LItem.Target.Kind = TGenDelphiTypeKind.&Record) then
      begin
        if AVisiting.Contains(LItem.Target) then
        begin
          LField.DataType := 'ftWideMemo';
          LField.Comment := 'JSON (' + LItem.Target.Name + ' contains itself)';
          AFields := AFields + [LField];
        end
        else
          AddFields(LItem.Target, LField.Name + '.', AVisiting, AFields);
        Continue;
      end;

      if Assigned(LItem.Target) and (LItem.Target.Kind = TGenDelphiTypeKind.Enumeration) and not LItem.TargetViaArray then
      begin
        // the values as text
        LSize := 1;
        for var LValue in LItem.Target.EnumValues do
          if Length(LValue.JSONValue) > LSize then
            LSize := Length(LValue.JSONValue);
        LField.DataType := 'ftWideString';
        LField.Size := LSize;
        LField.Comment := LItem.Target.Name;
      end
      else if LType = 'Boolean' then
        LField.DataType := 'ftBoolean'
      else if LType = 'Integer' then
        LField.DataType := 'ftInteger'
      else if LType = 'Int64' then
        LField.DataType := 'ftLargeint'
      else if LType = 'Double' then
        LField.DataType := 'ftFloat'
      else if LType = 'Currency' then
        LField.DataType := 'ftCurrency'
      else if LType = 'TDateTime' then
        LField.DataType := 'ftDateTime'
      else if LType = 'TDate' then
        LField.DataType := 'ftDate'
      else if LType = 'TTime' then
        LField.DataType := 'ftTime'
      else if LType = 'string' then
      begin
        LField.DataType := 'ftWideString';
        LField.Size := STRING_SIZE;
      end
      else
      begin
        // arrays, values without a Delphi type: their JSON text
        LField.DataType := 'ftWideMemo';
        LField.Comment := 'JSON';
        if LType.StartsWith('TArray<') or LType.StartsWith('TObjectList<') then
          LField.Comment := 'JSON array';
      end;
      AFields := AFields + [LField];
    end;
  finally
    AVisiting.Remove(AType);
  end;
end;

function TGenDatasetsEmitter.FieldsOf(const AType: TGenDelphiType): TArray<TGenDatasetField>;
var
  LVisiting: TList<TGenDelphiType>;
begin
  Result := [];
  LVisiting := TList<TGenDelphiType>.Create;
  try
    AddFields(AType, '', LVisiting, Result);
  finally
    LVisiting.Free;
  end;
end;

function TGenDatasetsEmitter.DatasetsUnit: string;
var
  LLines: TStringList;
  LApi: TGenApi;
  LSource, LLine: string;
  LTypes: TArray<TGenDelphiType>;
begin
  LApi := FTypes.Api;
  LSource := '"' + LApi.Title + '"';
  if LApi.Version <> '' then
    LSource := LSource + ' ' + LApi.Version;
  if LApi.OpenAPIVersion <> '' then
    LSource := LSource + ' (OpenAPI ' + LApi.OpenAPIVersion + ')';
  LTypes := DatasetTypes;

  LLines := TStringList.Create;
  try
    LLines.Add('(*');
    LLines.Add('  Generated by MARSgen from ' + LSource + '.');
    LLines.Add('  Do not edit: this unit is written again when the document is imported again.');
    LLines.Add('*)');
    LLines.Add('unit ' + UnitName + ';');
    LLines.Add('');
    LLines.Add('// The fields of the types of ' + FTypes.UnitName + ' for a dataset (TFDMemTable, TVirtualTable,');
    LLines.Add('// TClientDataSet...), as MARS reads and writes the JSON of datasets: nested objects flattened');
    LLines.Add('// (category.name), arrays and values without a type as JSON text.');
    LLines.Add('//');
    if Length(LTypes) > 0 then
      LLines.Add('//   ' + ProcedureName(LTypes[0]) + '(FDMemTable1.FieldDefs);')
    else
      LLines.Add('//   Define<Type>(FDMemTable1.FieldDefs);');
    LLines.Add('//   JSONArrayToDataSet(LArray, FDMemTable1); // MARS.Data.Utils: creates it, fills it');
    LLines.Add('');
    LLines.Add('interface');
    LLines.Add('');
    LLines.Add('uses');
    LLines.Add(INDENT + 'Data.DB;');
    LLines.Add('');
    for var LType in LTypes do
    begin
      LLines.Add('// ' + LType.Name + IfThen(Assigned(LType.Schema) and (LType.Schema.Name <> ''), ': ' + LType.Schema.Name));
      LLines.Add('procedure ' + ProcedureName(LType) + '(const AFieldDefs: TFieldDefs);');
    end;
    LLines.Add('');
    LLines.Add('implementation');
    for var LType in LTypes do
    begin
      LLines.Add('');
      LLines.Add('procedure ' + ProcedureName(LType) + '(const AFieldDefs: TFieldDefs);');
      LLines.Add('begin');
      for var LField in FieldsOf(LType) do
      begin
        LLine := INDENT + 'AFieldDefs.Add(''' + LField.Name.Replace('''', '''''') + ''', ' + LField.DataType;
        if LField.Size > 0 then
          LLine := LLine + ', ' + LField.Size.ToString;
        LLine := LLine + ');';
        if LField.Comment <> '' then
          LLine := LLine + ' // ' + LField.Comment;
        LLines.Add(LLine);
      end;
      LLines.Add('end;');
    end;
    LLines.Add('');
    LLines.Add('end.');
    Result := LLines.Text;
  finally
    LLines.Free;
  end;
end;

class function TGenDatasetsEmitter.WriteTo(const ATypes: TGenTypeMapper; const AOutput: TGenOutput): string;
var
  LEmitter: TGenDatasetsEmitter;
begin
  LEmitter := TGenDatasetsEmitter.Create(ATypes);
  try
    AOutput.WriteGenerated(LEmitter.UnitName + '.pas', LEmitter.DatasetsUnit);
    Result := LEmitter.UnitName;
  finally
    LEmitter.Free;
  end;
end;

end.
