(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Metadata.Attributes;

interface

uses
  Classes, SysUtils, System.Rtti
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

implementation

uses
  MARS.Core.URL, MARS.Rtti.Utils
;


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
