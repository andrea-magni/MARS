(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Metadata.Reader;

interface

uses
  Classes, SysUtils, Rtti, TypInfo
, MARS.Metadata, MARS.Core.Engine, MARS.Core.Application, MARS.Core.Registry.Utils
;

type
  TMARSMetadataReader=class
  private
    FEngine: TMARSEngine;
    FMetadata: TMARSEngineMetadata;
  protected
    procedure ReadApplication(const AApplication: TMARSApplication); virtual;
    procedure ReadResource(const AApplication: TMARSApplication;
      const AApplicationMetadata: TMARSApplicationMetadata;
      const AResourcePath: string; AResourceInfo: TMARSConstructorInfo); virtual;
    procedure ReadMethod(const AResourceMetadata: TMARSResourceMetadata;
      const AMethod: TRttiMethod); virtual;
    procedure ReadParameter(const AResourceMetadata: TMARSResourceMetadata;
      const AMethodMetadata: TMARSMethodMetadata;
      const AParameter: TRttiParameter; const AMethod: TRttiMethod); virtual;
  public
    constructor Create(const AEngine: TMARSEngine; const AReadImmediately: Boolean = True); virtual;
    destructor Destroy; override;

    procedure Read; virtual;

    property Engine: TMARSEngine read FEngine;
    property Metadata: TMARSEngineMetadata read FMetadata;
  end;

implementation

uses
  MARS.Core.Utils, MARS.Core.URL, MARS.Rtti.Utils, MARS.Core.Attributes
, MARS.Core.Exceptions, MARS.Metadata.Attributes
;


{ TMARSMetadataReader }

constructor TMARSMetadataReader.Create(const AEngine: TMARSEngine; const AReadImmediately: Boolean);
begin
  inherited Create;
  FEngine := AEngine;
  FMetadata := TMARSEngineMetadata.Create(nil);

  if AReadImmediately then
    Read;
end;

destructor TMARSMetadataReader.Destroy;
begin
  FMetadata.Free;
  inherited;
end;

procedure TMARSMetadataReader.Read;
begin
  Metadata.Name := Engine.Name;
  Metadata.Path := Engine.BasePath;

  Engine.EnumerateApplications(
    procedure (APath: string; AApplication: TMARSApplication)
    begin
      ReadApplication(AApplication);
    end
  );

end;

procedure TMARSMetadataReader.ReadApplication(
  const AApplication: TMARSApplication);
var
  LApplicationMetadata: TMARSApplicationMetadata;
begin
  LApplicationMetadata := TMARSApplicationMetadata.Create(FMetadata);
  try
    LApplicationMetadata.Name := AApplication.Name;
    LApplicationMetadata.Path := AApplication.BasePath;

    AApplication.EnumerateResources(
      procedure (APath: string; AConstructorInfo: TMARSConstructorInfo)
      begin
        ReadResource(AApplication, LApplicationMetadata, APath, AConstructorInfo);
      end
    );
  except
    LApplicationMetadata.Free;
    raise;
  end;
end;

procedure TMARSMetadataReader.ReadMethod(
  const AResourceMetadata: TMARSResourceMetadata; const AMethod: TRttiMethod);
var
  LMethodMetadata: TMARSMethodMetadata;
  LParameters: TArray<TRttiParameter>;
  LParameter: TRttiParameter;
  LPathFullPath: string;
  LResPathParams: TArray<string>;
  LResPathParam: string;
  LResPathParamMetadata: TMARSRequestParamMetadata;
begin
  LMethodMetadata := TMARSMethodMetadata.Create(AResourceMetadata);
  try
    LMethodMetadata.Name := AMethod.Name;
    LMethodMetadata.Path := PathAttribute.RetrieveValue(AMethod);
    LMethodMetadata.Summary := MetaSummaryAttribute.RetrieveText(AMethod);
    LMethodMetadata.Description := MetaDescriptionAttribute.RetrieveText(AMethod);
    LMethodMetadata.Visible := MetaVisibleAttribute.RetrieveValue(AMethod, True);

    LMethodMetadata.DataType := '';
    if (AMethod.MethodKind in [mkFunction, mkClassFunction]) then
    begin
      LMethodMetadata.DataType := AMethod.ReturnType.QualifiedName;
      LMethodMetadata.DataTypeRttiType := AMethod.ReturnType;
    end;

    AMethod.ForEachAttribute<HttpMethodAttribute>(
      procedure (Attribute: HttpMethodAttribute)
      begin
        LMethodMetadata.HttpMethod := SmartConcat([LMethodMetadata.HttpMethod, Attribute.HttpMethodName]);
      end);

    AMethod.ForEachAttribute<ProducesAttribute>(
      procedure (Attribute: ProducesAttribute)
      begin
        LMethodMetadata.Produces := SmartConcat([LMethodMetadata.Produces, Attribute.Value]);
      end);
    if LMethodMetadata.Produces.IsEmpty then
      LMethodMetadata.Produces := AResourceMetadata.Produces
    else if not AResourceMetadata.Produces.IsEmpty then
      LMethodMetadata.Produces := SmartConcat([LMethodMetadata.Produces, AResourceMetadata.Produces]);
    LMethodMetadata.Produces := SmartConcat(LMethodMetadata.Produces.Split([',']).RemoveDuplicates, ',');

    AMethod.ForEachAttribute<ConsumesAttribute>(
      procedure (Attribute: ConsumesAttribute)
      begin
        LMethodMetadata.Consumes := SmartConcat([LMethodMetadata.Consumes, Attribute.Value]);
      end);
    if LMethodMetadata.Consumes.IsEmpty then
      LMethodMetadata.Consumes := AResourceMetadata.Consumes
    else if not AResourceMetadata.Consumes.IsEmpty then
      LMethodMetadata.Consumes := SmartConcat([LMethodMetadata.Consumes, AResourceMetadata.Consumes]);
    LMethodMetadata.Consumes := SmartConcat(LMethodMetadata.Consumes.Split([',']).RemoveDuplicates, ',');

     AMethod.ForEachAttribute<AuthorizationAttribute>(
      procedure (Attribute: AuthorizationAttribute)
      begin
        LMethodMetadata.Authorization :=  SmartConcat([LMethodMetadata.Authorization, Attribute.ToString]);
      end);

    // Path params due to resource URL prototype
    LResPathParams := TMARSURL.ExtractPathParams(AResourceMetadata.Path);
    for LResPathParam in LResPathParams do
    begin
      LResPathParamMetadata := TMARSRequestParamMetadata.Create(LMethodMetadata);
      try
//        LResPathParamMetadata.Summary := '';
//        LResPathParamMetadata.Description := '';
        LResPathParamMetadata.Kind := 'PathParam';
        LResPathParamMetadata.SwaggerKind := 'path';
        LResPathParamMetadata.Name := LResPathParam;
        LResPathParamMetadata.DataType := 'string';
        LResPathParamMetadata.DataTypeRttiType := TRttiContext.Create.GetType(TypeInfo(string));
        LResPathParamMetadata.Required := true;
      except
        LResPathParamMetadata.Free;
        raise;
      end;
    end;

    // params due to method's parameters (annotated with attributes)
    LParameters := AMethod.GetParameters;
    for LParameter in LParameters do
      ReadParameter(AResourceMetadata, LMethodMetadata, LParameter, AMethod);

    LPathFullPath := TMARSURL.CombinePath([AResourceMetadata.Path, LMethodMetadata.Path]);
    AResourceMetadata.GetParent.AddPath(LPathFullPath, LMethodMetadata);
  except
    LMethodMetadata.Free;
    raise;
  end;
end;

procedure TMARSMetadataReader.ReadParameter(
  const AResourceMetadata: TMARSResourceMetadata;
  const AMethodMetadata: TMARSMethodMetadata;
  const AParameter: TRttiParameter; const AMethod: TRttiMethod);
begin
  AParameter.HasAttribute<RequestParamAttribute>(
    procedure (AAttribute: RequestParamAttribute)
    var
      LRequestParamMetadata: TMARSRequestParamMetadata;
    begin
      LRequestParamMetadata := TMARSRequestParamMetadata.Create(AMethodMetadata);
      try
        LRequestParamMetadata.Summary := '';
        AMethod.HasAttribute<MetaSummaryAttribute>(
          procedure (Attribute: MetaSummaryAttribute)
          begin
            LRequestParamMetadata.Summary := Attribute.Text;
          end
        );

        LRequestParamMetadata.Description := '';
        AParameter.HasAttribute<MetaDescriptionAttribute>(
          procedure (Attribute: MetaDescriptionAttribute)
          begin
            LRequestParamMetadata.Description := Attribute.Text;
          end
        );

        LRequestParamMetadata.Kind := AAttribute.Kind;
        LRequestParamMetadata.SwaggerKind := AAttribute.SwaggerKind;
        if AAttribute is NamedRequestParamAttribute then
          LRequestParamMetadata.Name := NamedRequestParamAttribute(AAttribute).Name;
        if LRequestParamMetadata.Name.IsEmpty then
          LRequestParamMetadata.Name := AParameter.Name;
        LRequestParamMetadata.DataType := AParameter.ParamType.QualifiedName;
        LRequestParamMetadata.DataTypeRttiType := AParameter.ParamType;
        LRequestParamMetadata.Required := AAttribute.IsRequired(AParameter);
      except
        LRequestParamMetadata.Free;
        raise;
      end;
    end
  );
end;

procedure TMARSMetadataReader.ReadResource(const AApplication: TMARSApplication;
  const AApplicationMetadata: TMARSApplicationMetadata;
  const AResourcePath: string; AResourceInfo: TMARSConstructorInfo);
var
  LRttiContext: TRttiContext;
  LResourceType: TRttiType;
  LResourceMetadata: TMARSResourceMetadata;
begin
  LResourceType := LRttiContext.GetType(AResourceInfo.TypeTClass);

  LResourceMetadata := TMARSResourceMetadata.Create(AApplicationMetadata);
  try
    LResourceMetadata.Path := AResourceInfo.Path;
    LResourceMetadata.Name := LResourceType.Name;

    LResourceMetadata.Description := '';
    LResourceType.HasAttribute<MetaDescriptionAttribute>(
      procedure (Attribute: MetaDescriptionAttribute)
      begin
        LResourceMetadata.Description := Attribute.Text;
      end);

    LResourceMetadata.Visible := True;
    LResourceType.HasAttribute<MetaVisibleAttribute>(
      procedure (Attribute: MetaVisibleAttribute)
      begin
        LResourceMetadata.Visible := Attribute.Value;
      end);

    LResourceType.ForEachAttribute<ProducesAttribute>(
      procedure (Attribute: ProducesAttribute)
      begin
        LResourceMetadata.Produces := SmartConcat([LResourceMetadata.Produces, Attribute.Value], ',');
      end
    , True);
    LResourceMetadata.Produces := SmartConcat(LResourceMetadata.Produces.Split([',']).RemoveDuplicates, ',');

    LResourceType.ForEachAttribute<ConsumesAttribute>(
      procedure (Attribute: ConsumesAttribute)
      begin
        LResourceMetadata.Consumes := SmartConcat([LResourceMetadata.Consumes, Attribute.Value]);
      end
    , True);
    LResourceMetadata.Consumes := SmartConcat(LResourceMetadata.Consumes.Split([',']).RemoveDuplicates, ',');

    LResourceType.ForEachAttribute<AuthorizationAttribute>(
      procedure (Attribute: AuthorizationAttribute)
      begin
        LResourceMetadata.Authorization := SmartConcat([LResourceMetadata.Authorization, Attribute.ToString]);
      end
    , True);

    LResourceType.ForEachMethodWithAttribute<HttpMethodAttribute>(
      function (AMethod: TRttiMethod; AHttpMethodAttribute: HttpMethodAttribute): Boolean
      begin
        Result := True;
        ReadMethod(LResourceMetadata, AMethod);
      end);
  except
    LResourceMetadata.Free;
    raise;
  end;
end;

end.
