(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Metadata.Attributes;

interface

uses
  Classes, SysUtils, System.Rtti, System.TypInfo
, MARS.Core.Attributes
;

type
  MetadataAttribute = class(MARSAttribute)

  end;

  MetadataTextAttribute = class(MetadataAttribute)
  private
    FText: string;
  public
    constructor Create(AText: string);
    property Text: string read FText;
    class function RetrieveText(const ARttiObject: TRttiObject; const ADefault: string = ''): string;
  end;

  MetaSummaryAttribute = class(MetadataTextAttribute);
  MetaDescriptionAttribute = class(MetadataTextAttribute);

  MetadataBooleanAttribute = class(MetadataAttribute)
  private
    FValue: Boolean;
  public
    constructor Create(AValue: Boolean);
    property Value: Boolean read FValue;
    class function RetrieveValue(const ARttiObject: TRttiObject; const ADefault: Boolean = false): Boolean;
  end;

  MetaVisibleAttribute = class(MetadataBooleanAttribute);

  // Shape of the request body of a method that reads it by itself (Request.Body,
  // Request.GetFormParamValue, ...) instead of through [BodyParam]/[FormParam] parameters:
  // ATypeName is the qualified name of a record or class (e.g.
  // 'MARS.Core.Token.Resource.TCredentials'), used by the documentation (OpenAPI). Parameters
  // describing the body, if any, take precedence.
  MetaRequestBodyAttribute = class(MetadataAttribute)
  private
    FTypeName: string;
    FDescription: string;
  public
    constructor Create(const ATypeName: string; const ADescription: string = '');
    // the type, nil if it cannot be found
    function FindType: TRttiType;
    property TypeName: string read FTypeName;
    property Description: string read FDescription;
  end;

  // Shape of the result of a method, for the documentation (OpenAPI), when the declared type
  // does not tell it: a dataset (TFDQuery...) whose rows are records, a TJSONValue, a TStream, a
  // method that writes the response by itself. It describes the JSON media types of the
  // response (application/json, */*, +json); the others keep the declared type.
  //   [MetaResult(TypeInfo(TCustomerRow), True)]  an array of TCustomerRow: the type stays in the
  //                                               program and is checked by the compiler
  //   [MetaResult('Server.Types.TCustomerRow')]   by qualified name, or 'TArray<...>' of it: the
  //                                               type must be used elsewhere too, or the linker
  //                                               leaves it out and the declared type is used
  MetaResultAttribute = class(MetadataAttribute)
  private
    FTypeInfo: PTypeInfo;
    FTypeName: string;
    FIsArray: Boolean;
    FDescription: string;
  public
    constructor Create(const ATypeInfo: PTypeInfo; const AIsArray: Boolean = False;
      const ADescription: string = ''); overload;
    constructor Create(const ATypeName: string; const ADescription: string = ''); overload;
    // the type of the result, or of its elements when IsArray; nil if it cannot be found
    function FindType: TRttiType;
    // a type name ('Unit.TType' or 'TArray<Unit.TType>'): the type, AIsArray for TArray<...>
    class function ResolveType(const ATypeName: string; out AIsArray: Boolean): TRttiType; static;
    property TypeName: string read FTypeName;
    property Description: string read FDescription;
    // an array of the type
    property IsArray: Boolean read FIsArray;
  end;

implementation

uses
  MARS.Core.URL, MARS.Rtti.Utils
;

{ MetaResultAttribute }

constructor MetaResultAttribute.Create(const ATypeInfo: PTypeInfo; const AIsArray: Boolean;
  const ADescription: string);
begin
  inherited Create;
  FTypeInfo := ATypeInfo;
  FIsArray := AIsArray;
  FDescription := ADescription;
end;

constructor MetaResultAttribute.Create(const ATypeName, ADescription: string);
begin
  inherited Create;
  FTypeName := ATypeName.Trim;
  ResolveType(FTypeName, FIsArray);
  FDescription := ADescription;
end;

function MetaResultAttribute.FindType: TRttiType;
var
  LContext: TRttiContext;
  LIsArray: Boolean;
begin
  if Assigned(FTypeInfo) then
  begin
    LContext := TRttiContext.Create;
    Result := LContext.GetType(FTypeInfo);
  end
  else
    Result := ResolveType(FTypeName, LIsArray);
end;

class function MetaResultAttribute.ResolveType(const ATypeName: string; out AIsArray: Boolean): TRttiType;
var
  LContext: TRttiContext;
  LName: string;
begin
  LName := ATypeName.Trim;
  AIsArray := LName.StartsWith('TArray<', True) and LName.EndsWith('>');
  if AIsArray then
    LName := LName.Substring(Length('TArray<'), LName.Length - Length('TArray<') - 1).Trim;
  Result := nil;
  if LName <> '' then
  begin
    LContext := TRttiContext.Create;
    Result := LContext.FindType(LName);
  end;
end;


{ MetadataBooleanAttribute }

{ MetaRequestBodyAttribute }

constructor MetaRequestBodyAttribute.Create(const ATypeName, ADescription: string);
begin
  inherited Create;
  FTypeName := ATypeName;
  FDescription := ADescription;
end;

function MetaRequestBodyAttribute.FindType: TRttiType;
var
  LContext: TRttiContext;
begin
  LContext := TRttiContext.Create;
  Result := LContext.FindType(FTypeName);
end;

constructor MetadataBooleanAttribute.Create(AValue: Boolean);
begin
  inherited Create;
  FValue := AValue;
end;


class function MetadataBooleanAttribute.RetrieveValue(
  const ARttiObject: TRttiObject; const ADefault: Boolean): Boolean;
var
  LResult: Boolean;
begin
  LResult := ADefault;
  ARttiObject.ForEachAttribute<MetadataBooleanAttribute>(
    procedure (Attribute: MetadataBooleanAttribute)
    begin
      if Attribute is Self then
        LResult := Attribute.Value;
    end);
  Result := LResult;
end;

{ MetadataTextAttribute }

constructor MetadataTextAttribute.Create(AText: string);
begin
  inherited Create;
  FText := AText;
end;

class function MetadataTextAttribute.RetrieveText(
  const ARttiObject: TRttiObject; const ADefault: string): string;
var
  LResult: string;
begin
  LResult := ADefault;
  ARttiObject.ForEachAttribute<MetadataTextAttribute>(
    procedure (Attribute: MetadataTextAttribute)
    begin
      if Attribute is Self then
        LResult := Attribute.Text;
    end);
  Result := LResult;
end;


end.
