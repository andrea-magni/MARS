(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Metadata.Reader;

interface

uses
    Classes, SysUtils, Rtti, TypInfo
  , MARS.Metadata
  , MARS.Core.Engine
  , MARS.Core.Application
  , MARS.Core.Registry
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
    MARS.Core.Utils
  , MARS.Rtti.Utils
  , MARS.Core.Attributes
  , MARS.Core.Exceptions
  , MARS.Metadata.Attributes
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
begin
  LMethodMetadata := TMARSMethodMetadata.Create(AResourceMetadata);
  try
    LMethodMetadata.Name := AMethod.Name;
    AMethod.HasAttribute<PathAttribute>(
      procedure (Attribute: PathAttribute)
      begin
        LMethodMetadata.Path := Attribute.Value;
      end
    );

    LMethodMetadata.Description := '';
    AMethod.HasAttribute<MetaDescriptionAttribute>(
      procedure (Attribute: MetaDescriptionAttribute)
      begin
        LMethodMetadata.Description := Attribute.Text;
      end
    );

    LMethodMetadata.Visible := True;
    AMethod.HasAttribute<MetaVisibleAttribute>(
      procedure (Attribute: MetaVisibleAttribute)
      begin
        LMethodMetadata.Visible := Attribute.Value;
      end
    );

    LMethodMetadata.DataType := '';
    if (AMethod.MethodKind in [mkFunction, mkClassFunction]) then
      LMethodMetadata.DataType := AMethod.ReturnType.QualifiedName;

    AMethod.ForEachAttribute<HttpMethodAttribute>(
      procedure (Attribute: HttpMethodAttribute)
      begin
        LMethodMetadata.HttpMethod := SmartConcat([LMethodMetadata.HttpMethod, Attribute.HttpMethodName]);
      end
    );

    AMethod.ForEachAttribute<ProducesAttribute>(
      procedure (Attribute: ProducesAttribute)
      begin
        LMethodMetadata.Produces := SmartConcat([LMethodMetadata.Produces, Attribute.Value]);
      end
    );
    if LMethodMetadata.Produces.IsEmpty then
      LMethodMetadata.Produces := AResourceMetadata.Produces;


    AMethod.ForEachAttribute<ConsumesAttribute>(
      procedure (Attribute: ConsumesAttribute)
      begin
        LMethodMetadata.Consumes := SmartConcat([LMethodMetadata.Consumes, Attribute.Value]);
      end
    );
    if LMethodMetadata.Consumes.IsEmpty then
      LMethodMetadata.Consumes := AResourceMetadata.Consumes;

     AMethod.ForEachAttribute<AuthorizationAttribute>(
      procedure (Attribute: AuthorizationAttribute)
      begin
        LMethodMetadata.Authorization :=  SmartConcat([LMethodMetadata.Authorization, Attribute.ToString]);
      end
    );
    if LMethodMetadata.Authorization.IsEmpty then
      LMethodMetadata.Authorization := AResourceMetadata.Authorization;

    LParameters := AMethod.GetParameters;
    for LParameter in LParameters do
      ReadParameter(AResourceMetadata, LMethodMetadata, LParameter, AMethod);
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
        LRequestParamMetadata.Description := '';
        AParameter.HasAttribute<MetaDescriptionAttribute>(
          procedure (Attribute: MetaDescriptionAttribute)
          begin
            LRequestParamMetadata.Description := Attribute.Text;
          end
        );

        LRequestParamMetadata.Kind := AAttribute.Kind;
        if AAttribute is NamedRequestParamAttribute then
          LRequestParamMetadata.Name := NamedRequestParamAttribute(AAttribute).Name;
        if LRequestParamMetadata.Name.IsEmpty then
          LRequestParamMetadata.Name := AParameter.Name;
        LRequestParamMetadata.DataType := AParameter.ParamType.QualifiedName;
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
    LResourceMetadata.Path := AResourcePath;
    LResourceMetadata.Name := LResourceType.Name;

    LResourceMetadata.Description := '';
    LResourceType.HasAttribute<MetaDescriptionAttribute>(
      procedure (Attribute: MetaDescriptionAttribute)
      begin
        LResourceMetadata.Description := Attribute.Text;
      end
    );

    LResourceMetadata.Visible := True;
    LResourceType.HasAttribute<MetaVisibleAttribute>(
      procedure (Attribute: MetaVisibleAttribute)
      begin
        LResourceMetadata.Visible := Attribute.Value;
      end
    );

    LResourceType.ForEachAttribute<ProducesAttribute>(
      procedure (Attribute: ProducesAttribute)
      begin
        LResourceMetadata.Produces := SmartConcat([LResourceMetadata.Produces, Attribute.Value]);
      end
    );

    LResourceType.ForEachAttribute<ConsumesAttribute>(
      procedure (Attribute: ConsumesAttribute)
      begin
        LResourceMetadata.Consumes := SmartConcat([LResourceMetadata.Consumes, Attribute.Value]);
      end
    );

    LResourceType.ForEachAttribute<AuthorizationAttribute>(
      procedure (Attribute: AuthorizationAttribute)
      begin
        LResourceMetadata.Authorization := SmartConcat([LResourceMetadata.Authorization, Attribute.ToString]);
      end
    );

    LResourceType.ForEachMethodWithAttribute<HttpMethodAttribute>(
      function (AMethod: TRttiMethod; AHttpMethodAttribute: HttpMethodAttribute): Boolean
      begin
        Result := True;

        ReadMethod(LResourceMetadata, AMethod);
      end
    );
  except
    LResourceMetadata.Free;
    raise;
  end;
end;

end.
