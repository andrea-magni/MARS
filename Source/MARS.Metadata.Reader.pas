(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Metadata.Reader;

interface

uses
  Classes, SysUtils, Rtti, TypInfo
, MARS.Metadata
, MARS.Utils.Parameters
, MARS.Core.Engine.Interfaces
, MARS.Core.Application.Interfaces
, MARS.Core.Registry.Utils
, MARS.Core.Routes
;

type
  TMARSMetadataReader=class
  private
    FEngine: IMARSEngine;
    FMetadata: TMARSEngineMetadata;
    FDefaultProduces: string;
    // OpenAPI.Header.<name> of the application being read: request headers of all its operations
    FApplicationHeaders: TArray<TMARSRouteMetaHeader>;
  protected
    // the request parameters of fields and properties of the resource ([HeaderParam],
    // [QueryParam], [CookieParam]): parameters of all its methods
    procedure ReadMemberParameters(const AResourceMetadata: TMARSResourceMetadata;
      const AMethodMetadata: TMARSMethodMetadata); virtual;
    // the headers for the documentation only, the nearest first: [MetaHeader] of the method and
    // of the resource, MetaHeader of a route and of its groups (AHeaders); OpenAPI.Header.<name>
    // of the application
    procedure ReadMetaHeaders(const AMethodMetadata: TMARSMethodMetadata;
      const AHeaders: TArray<TMARSRouteMetaHeader>); virtual;
    procedure ReadApplication(const AApplication: IMARSApplication); virtual;
    procedure ReadResource(const AApplication: IMARSApplication;
      const AApplicationMetadata: TMARSApplicationMetadata;
      const AResourcePath: string; AResourceInfo: TMARSConstructorInfo); virtual;
    procedure ReadMethod(const AResourceMetadata: TMARSResourceMetadata;
      const AMethod: TRttiMethod); virtual;
    procedure ReadParameter(const AResourceMetadata: TMARSResourceMetadata;
      const AMethodMetadata: TMARSMethodMetadata;
      const AParameter: TRttiParameter; const AMethod: TRttiMethod); virtual;
    procedure ReadRoutes(const AApplication: IMARSApplication;
      const AApplicationMetadata: TMARSApplicationMetadata); virtual;
    procedure ReadRoute(const AResourceMetadata: TMARSResourceMetadata;
      const ARoute: TMARSRoute); virtual;
  public
    constructor Create(const AEngine: IMARSEngine; const AReadImmediately: Boolean = True); virtual;
    destructor Destroy; override;

    procedure Read; virtual;

    property Engine: IMARSEngine read FEngine;
    property Metadata: TMARSEngineMetadata read FMetadata;
    // Produces.Default of the application being read: what its endpoints without [Produces] produce
    property DefaultProduces: string read FDefaultProduces;
  end;

implementation

uses
  Generics.Collections, StrUtils
, MARS.Core.Utils, MARS.Core.URL, MARS.Rtti.Utils, MARS.Core.Attributes
, MARS.Core.Exceptions, MARS.Metadata.Attributes, MARS.Core.MessageBodyWriter
;

// [MetaHeader] attributes as headers
function MetaHeadersOf(const AAttributes: TArray<TCustomAttribute>): TArray<TMARSRouteMetaHeader>;
var
  LAttribute: TCustomAttribute;
  LHeader: TMARSRouteMetaHeader;
begin
  Result := [];
  for LAttribute in AAttributes do
    if LAttribute is MetaHeaderAttribute then
    begin
      LHeader.Name := MetaHeaderAttribute(LAttribute).Name;
      LHeader.Description := MetaHeaderAttribute(LAttribute).Description;
      LHeader.Required := MetaHeaderAttribute(LAttribute).Required;
      Result := Result + [LHeader];
    end;
end;


{ TMARSMetadataReader }

constructor TMARSMetadataReader.Create(const AEngine: IMARSEngine; const AReadImmediately: Boolean);
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
    procedure (APath: string; AApplication: IMARSApplication)
    begin
      ReadApplication(AApplication);
    end
  );

end;

// OpenAPI.Header.<name>=[required|optional;]<description> of AParameters into AHeaders (the
// ones with the same name replaced)
procedure ReadParameterHeaders(const AParameters: TMARSParameters; var AHeaders: TArray<TMARSRouteMetaHeader>);
const
  HEADER_PREFIX = 'OpenAPI.Header.';
var
  LParamName, LValue, LFirst: string;
  LSeparator, LIndex: Integer;
  LHeader: TMARSRouteMetaHeader;
begin
  for LParamName in AParameters.ParamNames do
    if StartsText(HEADER_PREFIX, LParamName) and (Length(LParamName) > Length(HEADER_PREFIX)) then
    begin
      LValue := AParameters.ByNameAsText(LParamName).Trim;
      LHeader.Name := Copy(LParamName, Length(HEADER_PREFIX) + 1, MaxInt);
      LHeader.Required := False;
      LHeader.Description := LValue;
      LSeparator := Pos(';', LValue);
      if LSeparator > 0 then
        LFirst := Copy(LValue, 1, LSeparator - 1).Trim
      else
        LFirst := LValue;
      if SameText(LFirst, 'required') or SameText(LFirst, 'optional') then
      begin
        LHeader.Required := SameText(LFirst, 'required');
        if LSeparator > 0 then
          LHeader.Description := Copy(LValue, LSeparator + 1, MaxInt).Trim
        else
          LHeader.Description := '';
      end;

      LIndex := High(AHeaders);
      while (LIndex >= 0) and not SameText(AHeaders[LIndex].Name, LHeader.Name) do
        Dec(LIndex);
      if LIndex >= 0 then
        AHeaders[LIndex] := LHeader
      else
        AHeaders := AHeaders + [LHeader];
    end;
end;

procedure TMARSMetadataReader.ReadApplication(
  const AApplication: IMARSApplication);
var
  LApplicationMetadata: TMARSApplicationMetadata;
begin
  // OpenAPI.Header.<name>: a request header of every operation, for the documentation only
  // (i.e. an API key checked by a middleware); of the engine (as OpenAPI.info.*), of the
  // application (DefaultApp.OpenAPI.Header.<name>), which wins
  FApplicationHeaders := [];
  if Assigned(FEngine) then
    ReadParameterHeaders(FEngine.Parameters, FApplicationHeaders);
  ReadParameterHeaders(AApplication.Parameters, FApplicationHeaders);

  LApplicationMetadata := TMARSApplicationMetadata.Create(FMetadata);
  try
    LApplicationMetadata.Name := AApplication.Name;
    LApplicationMetadata.Path := AApplication.BasePath;
    FDefaultProduces := string.Join(',', TMARSMessageBodyRegistry.GetDefaultProduces(AApplication));

    AApplication.EnumerateResources(
      procedure (APath: string; AConstructorInfo: TMARSConstructorInfo)
      begin
        ReadResource(AApplication, LApplicationMetadata, APath, AConstructorInfo);
      end
    );

    ReadRoutes(AApplication, LApplicationMetadata);
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
    LMethodMetadata.RttiMethod := AMethod;
    LMethodMetadata.Name := AMethod.Name;
    LMethodMetadata.Path := PathAttribute.RetrieveValue(AMethod);
    LMethodMetadata.Summary := MetaSummaryAttribute.RetrieveText(AMethod);
    LMethodMetadata.Description := MetaDescriptionAttribute.RetrieveText(AMethod);
    LMethodMetadata.Visible := MetaVisibleAttribute.RetrieveValue(AMethod, True);
    var LMetaResult := AMethod.GetAttribute<MetaResultAttribute>;
    if Assigned(LMetaResult) then
    begin
      LMethodMetadata.ResultType := LMetaResult.FindType;
      LMethodMetadata.ResultIsArray := LMetaResult.IsArray;
      LMethodMetadata.ResultDescription := LMetaResult.Description;
    end;

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
    if LMethodMetadata.Produces.IsEmpty then
      LMethodMetadata.Produces := FDefaultProduces;

    AMethod.ForEachAttribute<ConsumesAttribute>(
      procedure (Attribute: ConsumesAttribute)
      begin
        LMethodMetadata.Consumes := SmartConcat([LMethodMetadata.Consumes, Attribute.Value]);
        LMethodMetadata.OwnConsumes := True;
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

    // params of the resource (fields and properties), headers for the documentation
    ReadMemberParameters(AResourceMetadata, LMethodMetadata);
    ReadMetaHeaders(LMethodMetadata,
      MetaHeadersOf(AMethod.GetAttributes + AResourceMetadata.RttiType.GetAttributes));

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
        LRequestParamMetadata.RttiParameter := AParameter;
        LRequestParamMetadata.RttiObject := AParameter;
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

// Produces, Consumes and Authorization values of a list of attributes (routes)
procedure ReadRouteAttributes(const AAttributes: TArray<TCustomAttribute>;
  out AProduces, AConsumes, AAuthorization: string);
var
  LAttribute: TCustomAttribute;
begin
  AProduces := '';
  AConsumes := '';
  AAuthorization := '';
  for LAttribute in AAttributes do
  begin
    if LAttribute is ProducesAttribute then
      AProduces := SmartConcat([AProduces, ProducesAttribute(LAttribute).Value])
    else if LAttribute is ConsumesAttribute then
      AConsumes := SmartConcat([AConsumes, ConsumesAttribute(LAttribute).Value])
    else if LAttribute is AuthorizationAttribute then
      AAuthorization := SmartConcat([AAuthorization, LAttribute.ToString]);
  end;
  AProduces := SmartConcat(AProduces.Split([',']).RemoveDuplicates, ',');
  AConsumes := SmartConcat(AConsumes.Split([',']).RemoveDuplicates, ',');
end;

procedure TMARSMetadataReader.ReadRoutes(const AApplication: IMARSApplication;
  const AApplicationMetadata: TMARSApplicationMetadata);
var
  LTable: TMARSRouteTable;
  LRoute: TMARSRoute;
  LRouter: TMARSRouter;
  LResourceMetadata: TMARSResourceMetadata;
  LMethodMetadata: TMARSMethodMetadata;
  LHidden: Boolean;
  LProduces, LConsumes, LAuthorization: string;
  LGroups: TDictionary<TMARSRouter, TMARSResourceMetadata>;
  LOperationIds: TDictionary<string, Integer>;
  LCount: Integer;
begin
  if not (AApplication.RouteTable is TMARSRouteTable) then
    Exit;
  LTable := TMARSRouteTable(AApplication.RouteTable);

  LGroups := TDictionary<TMARSRouter, TMARSResourceMetadata>.Create;
  LOperationIds := TDictionary<string, Integer>.Create;
  try
    for LRoute in LTable.Routes do
    begin
      // one resource for each group: its declarations and visibility apply to its routes only
      if not LGroups.TryGetValue(LRoute.Router, LResourceMetadata) then
      begin
        LHidden := False;
        LRouter := LRoute.Router;
        while Assigned(LRouter) do
        begin
          LHidden := LHidden or LRouter.IsHidden;
          LRouter := LRouter.Parent;
        end;
        LRouter := LRoute.Router;
        ReadRouteAttributes(LRoute.GroupAttributes, LProduces, LConsumes, LAuthorization);

        LResourceMetadata := TMARSResourceMetadata.Create(AApplicationMetadata);
        try
          LResourceMetadata.RttiType := nil;
          LResourceMetadata.Path := LRouter.PrototypePath;
          LResourceMetadata.Name := StringFallback([LRouter.GroupName, LRouter.PrototypePath], 'routes');
          LResourceMetadata.Summary := LRouter.SummaryText;
          LResourceMetadata.Description := LRouter.DescriptionText;
          LResourceMetadata.Visible := not LHidden;
          LResourceMetadata.Produces := LProduces;
          LResourceMetadata.Consumes := LConsumes;
          LResourceMetadata.Authorization := LAuthorization;
        except
          LResourceMetadata.Free;
          raise;
        end;
        LGroups.Add(LRoute.Router, LResourceMetadata);
      end;

      ReadRoute(LResourceMetadata, LRoute);

      // unique operation ids (i.e. get_people_id for people/{id} and people/id)
      LMethodMetadata := LResourceMetadata.Methods.Last as TMARSMethodMetadata;
      if LOperationIds.TryGetValue(LMethodMetadata.Name.ToLower, LCount) then
      begin
        Inc(LCount);
        LOperationIds[LMethodMetadata.Name.ToLower] := LCount;
        LMethodMetadata.Name := LMethodMetadata.Name + '_' + LCount.ToString;
      end;
      LOperationIds.AddOrSetValue(LMethodMetadata.Name.ToLower, 1);
    end;
  finally
    LOperationIds.Free;
    LGroups.Free;
  end;
end;

function HasParameter(const AMethodMetadata: TMARSMethodMetadata; const ASwaggerKind, AName: string): Boolean;
var
  LFound: Boolean;
begin
  LFound := False;
  AMethodMetadata.ForEachParameter(
    procedure (AParam: TMARSRequestParamMetadata)
    begin
      if SameText(AParam.SwaggerKind, ASwaggerKind) and SameText(AParam.Name, AName) then
        LFound := True;
    end);
  Result := LFound;
end;

procedure TMARSMetadataReader.ReadMemberParameters(const AResourceMetadata: TMARSResourceMetadata;
  const AMethodMetadata: TMARSMethodMetadata);

  procedure AddMember(const AMember: TRttiMember; const AType: TRttiType);
  var
    LAttribute: RequestParamAttribute;
    LParamMetadata: TMARSRequestParamMetadata;
    LName: string;
  begin
    LAttribute := AMember.GetAttribute<RequestParamAttribute>;
    if not Assigned(LAttribute) or not Assigned(AType)
      or (IndexText(LAttribute.SwaggerKind, ['header', 'query', 'cookie']) = -1) then
      Exit;
    LName := '';
    if LAttribute is NamedRequestParamAttribute then
      LName := NamedRequestParamAttribute(LAttribute).Name;
    if LName = '' then
      LName := AMember.Name;
    // a parameter of the method with the same name wins
    if HasParameter(AMethodMetadata, LAttribute.SwaggerKind, LName) then
      Exit;

    LParamMetadata := TMARSRequestParamMetadata.Create(AMethodMetadata);
    try
      LParamMetadata.Kind := LAttribute.Kind;
      LParamMetadata.SwaggerKind := LAttribute.SwaggerKind;
      LParamMetadata.Name := LName;
      LParamMetadata.Description := MetaDescriptionAttribute.RetrieveText(AMember);
      LParamMetadata.DataType := AType.QualifiedName;
      LParamMetadata.DataTypeRttiType := AType;
      LParamMetadata.Required := LAttribute.IsRequired(AMember);
      LParamMetadata.RttiObject := AMember;
    except
      LParamMetadata.Free;
      raise;
    end;
  end;

var
  LField: TRttiField;
  LProperty: TRttiProperty;
begin
  if not Assigned(AResourceMetadata.RttiType) then
    Exit;
  for LField in AResourceMetadata.RttiType.GetFields do
    AddMember(LField, LField.FieldType);
  for LProperty in AResourceMetadata.RttiType.GetProperties do
    AddMember(LProperty, LProperty.PropertyType);
end;

procedure TMARSMetadataReader.ReadMetaHeaders(const AMethodMetadata: TMARSMethodMetadata;
  const AHeaders: TArray<TMARSRouteMetaHeader>);

  procedure AddHeader(const AHeader: TMARSRouteMetaHeader);
  var
    LParamMetadata: TMARSRequestParamMetadata;
  begin
    // a parameter of the method, the route or a nearer [MetaHeader] wins
    if (AHeader.Name = '') or HasParameter(AMethodMetadata, 'header', AHeader.Name) then
      Exit;
    LParamMetadata := TMARSRequestParamMetadata.Create(AMethodMetadata);
    try
      LParamMetadata.Kind := 'HeaderParam';
      LParamMetadata.SwaggerKind := 'header';
      LParamMetadata.Name := AHeader.Name;
      LParamMetadata.Description := AHeader.Description;
      LParamMetadata.DataTypeRttiType := TRttiContext.Create.GetType(TypeInfo(string));
      LParamMetadata.DataType := LParamMetadata.DataTypeRttiType.QualifiedName;
      LParamMetadata.Required := AHeader.Required;
    except
      LParamMetadata.Free;
      raise;
    end;
  end;

var
  LHeader: TMARSRouteMetaHeader;
begin
  // the nearest first: the method (route), the resource (groups), the application
  for LHeader in AHeaders do
    AddHeader(LHeader);
  for LHeader in FApplicationHeaders do
    AddHeader(LHeader);
end;

procedure TMARSMetadataReader.ReadRoute(const AResourceMetadata: TMARSResourceMetadata;
  const ARoute: TMARSRoute);

  procedure AddParam(const AMethodMetadata: TMARSMethodMetadata; const AKind, ASwaggerKind, AName: string;
    const ADataType: PTypeInfo; const ADescription: string; const ARequired: Boolean);
  var
    LParamMetadata: TMARSRequestParamMetadata;
  begin
    LParamMetadata := TMARSRequestParamMetadata.Create(AMethodMetadata);
    try
      LParamMetadata.Kind := AKind;
      LParamMetadata.SwaggerKind := ASwaggerKind;
      LParamMetadata.Name := AName;
      LParamMetadata.Description := ADescription;
      LParamMetadata.DataTypeRttiType := TMARSRouteRtti.RttiType(ADataType);
      LParamMetadata.DataType := LParamMetadata.DataTypeRttiType.QualifiedName;
      LParamMetadata.Required := ARequired;
    except
      LParamMetadata.Free;
      raise;
    end;
  end;

var
  LMethodMetadata: TMARSMethodMetadata;
  LSegment: TMARSRouteSegment;
  LParam: TMARSRouteParam;
  LProduces, LConsumes, LAuthorization: string;
begin
  LMethodMetadata := TMARSMethodMetadata.Create(AResourceMetadata);
  try
    LMethodMetadata.RttiMethod := nil;
    LMethodMetadata.Name := ARoute.OperationId;
    LMethodMetadata.Path := ARoute.RelativePrototypePath;
    LMethodMetadata.Summary := ARoute.SummaryText;
    LMethodMetadata.Description := ARoute.DescriptionText;
    LMethodMetadata.Visible := not ARoute.IsHidden;
    LMethodMetadata.HttpMethod := ARoute.HttpMethod;
    if Assigned(ARoute.ResultMetaType) then
      LMethodMetadata.ResultType := TRttiContext.Create.GetType(ARoute.ResultMetaType)
    else if ARoute.ResultTypeName <> '' then
      LMethodMetadata.ResultType := MetaResultAttribute.ResolveType(ARoute.ResultTypeName, LMethodMetadata.ResultIsArray);
    LMethodMetadata.ResultDescription := ARoute.ResultDescription;

    LMethodMetadata.DataType := '';
    if Assigned(ARoute.ResultType) then
    begin
      LMethodMetadata.DataTypeRttiType := TMARSRouteRtti.RttiType(ARoute.ResultType);
      LMethodMetadata.DataType := LMethodMetadata.DataTypeRttiType.QualifiedName;
    end;

    // route values, then group values (as ReadMethod)
    ReadRouteAttributes(ARoute.Attributes, LProduces, LConsumes, LAuthorization);
    LMethodMetadata.Produces := StringFallback([LProduces, AResourceMetadata.Produces, FDefaultProduces]);
    LMethodMetadata.Consumes := StringFallback([LConsumes, AResourceMetadata.Consumes]);
    LMethodMetadata.OwnConsumes := LConsumes <> '';
    LMethodMetadata.Authorization := LAuthorization;

    // path parameters: from the template, typed by the constraint
    for LSegment in ARoute.Segments do
      case LSegment.Kind of
        rskParam:
          if LSegment.Constraint = 'int' then
            AddParam(LMethodMetadata, 'PathParam', 'path', LSegment.Text, TypeInfo(Integer), '', True)
          else
            AddParam(LMethodMetadata, 'PathParam', 'path', LSegment.Text, TypeInfo(string), '', True);
        rskWildcard:
          AddParam(LMethodMetadata, 'PathParam', 'path', LSegment.Text, TypeInfo(string), '', True);
      end;

    // declared parameters
    for LParam in ARoute.Params do
      AddParam(LMethodMetadata, LParam.Kind, LParam.SwaggerKind, LParam.Name, LParam.DataType
        , LParam.Description, LParam.Required);

    // typed body
    if Assigned(ARoute.BodyType) then
      AddParam(LMethodMetadata, 'BodyParam', 'body', 'body', ARoute.BodyType, '', ARoute.IsBodyRequired);

    // headers for the documentation: MetaHeader of the route, of its groups, of the application
    ReadMetaHeaders(LMethodMetadata, ARoute.MetaHeaders + ARoute.GroupMetaHeaders);

    AResourceMetadata.GetParent.AddPath(
      TMARSURL.CombinePath([AResourceMetadata.Path, LMethodMetadata.Path]), LMethodMetadata);
  except
    LMethodMetadata.Free;
    raise;
  end;
end;

procedure TMARSMetadataReader.ReadResource(const AApplication: IMARSApplication;
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
    LResourceMetadata.RttiType := LResourceType;
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
