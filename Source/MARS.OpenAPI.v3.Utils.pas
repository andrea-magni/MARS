unit MARS.OpenAPI.v3.Utils;

interface

uses
  Classes, SysUtils, System.Rtti, System.TypInfo, MARS.Rtti.Utils
, MARS.OpenAPI.v3
, MARS.Core.Engine, MARS.Core.Application, MARS.Core.Activation.Interfaces, MARS.Utils.Parameters
, MARS.Metadata
;


type
  TOpenAPIHelper = class helper for TOpenAPI
  private
    procedure ReadBearerSecurityScheme(const AName: string; const AParams: TMARSParameters);
    procedure ReadCookieSecurityScheme(const AName: string; const AParams: TMARSParameters);
    procedure ReadInfoFromParams(const AParams: TMARSParameters);
    function AddServerFromEngine(const AEngine: TMARSEngine): TServer;
    procedure ReadApplication(const AAppMD: TMARSApplicationMetadata);
    procedure ReadOperation(const AOperation: TOperation; const ARes: TMARSResourceMetadata;
      const AMet: TMARSMethodMetadata);
  public
    function EnsureTypeInComponentsSchemas(const AType: TRttiType): Boolean;
    function MARSKindToOpenAPIKind(const AString: string): string;
    function MARSDataTypeToOpenAPIType(const AType: TRttiType; const ARefPrefix: string = '#/components/schemas/'): string;
    class function BuildFrom(const AEngine: TMARSEngine; const AApplication: TMARSApplication): TOpenAPI; overload;
    class function BuildFrom(const AActivation: IMARSActivation): TOpenAPI; overload;
  end;


implementation

uses
  StrUtils
, MARS.Core.Registry.Utils, MARS.Core.URL, MARS.Utils.JWT
, MARS.Metadata.Reader
, MARS.Core.MediaType, MARS.Core.JSON, MARS.YAML.ReadersAndWriters
;

{ TOpenAPIHelper }

class function TOpenAPIHelper.BuildFrom(const AEngine: TMARSEngine;
  const AApplication: TMARSApplication): TOpenAPI;
var
  LOpenAPI: TOpenAPI;
  LReader: TMARSMetadataReader;
begin
  Assert(Assigned(AEngine));

  LOpenAPI := TOpenAPI.Create;
  try
    LOpenAPI.openapi := '3.0.2';
    LOpenAPI.ReadInfoFromParams(AEngine.Parameters);
    LOpenAPI.AddServerFromEngine(AEngine)
    .variables.Add('application'
    , TServerVariable.Create([AApplication.BasePath]
      , AApplication.BasePath, 'Application')
    );

    if AApplication.Parameters.ByNameText(JWT_SECRET_PARAM, JWT_SECRET_PARAM_DEFAULT).AsString <> '' then
      LOpenAPI.ReadBearerSecurityScheme('JWT_bearer', AApplication.Parameters);
    if AApplication.Parameters.ByNameText(JWT_COOKIEENABLED_PARAM, JWT_COOKIEENABLED_PARAM_DEFAULT).AsBoolean then
      LOpenAPI.ReadCookieSecurityScheme('JWT_cookie', AApplication.Parameters);

    LReader := TMARSMetadataReader.Create(AEngine);
    try
      LOpenAPI.ReadApplication(LReader.Metadata.ApplicationByName(AApplication.Name));
    finally
      LReader.Free;
    end;

  except
    FreeAndNil(LOpenAPI);
    raise;
  end;

  Result := LOpenAPI;
end;

class function TOpenAPIHelper.BuildFrom(
  const AActivation: IMARSActivation): TOpenAPI;
begin
  Result := BuildFrom(AActivation.Engine, AActivation.Application);
end;

function TOpenAPIHelper.EnsureTypeInComponentsSchemas(
  const AType: TRttiType): Boolean;
var
  LSchema: TSchema;
  LSchemaExists: Boolean;
  LMember: TRttiMember;
  LJSONName, LYAMLName: string;
  LProperty: TSchema;
begin
  Result := False;
  LSchemaExists := components.HasSchema(AType.Name);
  if not LSchemaExists then
  begin
    Result := True;
    LSchema := components.AddSchema(AType.Name);

    if (AType.IsInstance) or (AType.IsRecord) then
    begin
      LSchema.SetType('object');
      LSchema.description := 'Schema for type ' + AType.QualifiedName;

      for LMember in AType.GetPropertiesAndFields do
      begin
        if (LMember.Visibility < TMemberVisibility.mvPublic) or (not LMember.IsReadable) then
          Continue;

        LJSONName := LMember.Name;
        LMember.HasAttribute<JSONNameAttribute>(
          procedure (AAttr: JSONNameAttribute)
          begin
            LJSONName := AAttr.Name;
          end
        );
        LYAMLName := LMember.Name;
        LMember.HasAttribute<YAMLNameAttribute>(
          procedure (AAttr: YAMLNameAttribute)
          begin
            LYAMLName := AAttr.Name;
          end
        );
        if (LJSONName = '') and (LYAMLName = '') then
          Continue;

        LProperty := LSchema.AddProperty(LJSONName);
        LProperty.SetType(LMember.GetRttiType, Self);
      end;
    end
    else if AType is TRttiEnumerationType then
    begin
      LSchema.SetType('string');
      LSchema.description := 'Schema for type ' + AType.QualifiedName;
      LSchema.enum := TRttiEnumerationType(AType).GetNames;
    end;

  end;
end;

function TOpenAPIHelper.MARSDataTypeToOpenAPIType(
  const AType: TRttiType; const ARefPrefix: string): string;
var
  LPrimitiveType: Boolean;
  LElementType: TRttiType;
begin

{
  type    format
  ------------------------------------------
  integer	int32	signed 32 bits
  integer	int64	signed 64 bits (a.k.a long)
  number	float
  number	double
  string
  string	byte	base64 encoded characters
  string	binary	any sequence of octets
  boolean
  string	date	As defined by full-date - RFC3339
  string	date-time	As defined by date-time - RFC3339
  string	password	A hint to UIs to obscure input.
}

  Result := AType.Name;

  if IndexStr(AType.QualifiedName, ['System.TDate', 'System.TDateTime', 'System.TTime']) <> -1 then
    Result := 'string';
  if IndexStr(AType.QualifiedName, ['System.Int64', 'System.UInt64', 'System.Int32', 'System.UInt32']) <> -1 then
    Result := 'integer'; //AM TODO format !
  if IndexStr(AType.QualifiedName, ['System.Currency', 'System.Single', 'System.Double', 'System.Extended']) <> -1 then
    Result := 'number'; //AM TODO format

  LPrimitiveType := IndexText(Result.ToLower, ['string', 'integer', 'boolean', 'number']) <> -1;
  if LPrimitiveType then
    Result := Result.ToLower
  else if (ARefPrefix <> '') then
  begin
    Result := ARefPrefix + Result;
    if AType.IsArray(LElementType) then
      EnsureTypeInComponentsSchemas(LElementType)
    else if AType.IsDictionaryOfStringAndT(LElementType) then
      EnsureTypeInComponentsSchemas(LElementType)
    else if AType.IsObjectListOfT(LElementType) then
      EnsureTypeInComponentsSchemas(LElementType)
    else
      EnsureTypeInComponentsSchemas(AType);
  end;
end;

function TOpenAPIHelper.MARSKindToOpenAPIKind(
  const AString: string): string;
begin
  Result := AString.Replace('Param', '').ToLower;
end;

procedure TOpenAPIHelper.ReadApplication(
  const AAppMD: TMARSApplicationMetadata);
begin
  AAppMD.ForEachResource(
    procedure (ARes: TMARSResourceMetadata)
    begin
      if not ARes.Visible then
        Exit;

      AddTag(ARes.Path, ARes.Description);

      ARes.ForEachMethod(
        procedure (AMet: TMARSMethodMetadata)
        var
          LPath: TPathItem;
          LOperation: TOperation;
        begin
          if not AMet.Visible then
            Exit;

          LPath := GetPath(TMARSURL.CombinePath([ARes.Path, AMet.Path], True, False));

          LPath.summary := ARes.Name + ' resource';
          LPath.description := ARes.Description;

          LOperation := LPath.OperationByHttpMethod(AMet.HttpMethodLowerCase);
          if Assigned(LOperation) then
            ReadOperation(LOperation, ARes, AMet);
        end
      );

    end
  );
end;

procedure TOpenAPIHelper.ReadBearerSecurityScheme(const AName: string;
  const AParams: TMARSParameters);
var
  LSchema: TSecurityScheme;
begin
  LSchema := components.AddSecurityScheme(AName, 'http');
  LSchema.scheme := 'bearer';
  LSchema.bearerFormat := 'JWT';
  LSchema.name := ''; // name is not required for http bearer schema and would raise a warning
  LSchema.description := AParams.ByNameText(JWT_ISSUER_PARAM, JWT_ISSUER_PARAM_DEFAULT).AsString;
  FBearerSecurityConfigured := True;
end;

procedure TOpenAPIHelper.ReadCookieSecurityScheme(const AName: string;
  const AParams: TMARSParameters);
var
  LSchema: TSecurityScheme;
begin
  LSchema := components.AddSecurityScheme(AName, 'apiKey');
  LSchema.&in := 'cookie';
  LSchema.name := AParams.ByNameText(JWT_COOKIENAME_PARAM, JWT_COOKIENAME_PARAM_DEFAULT).AsString;
  LSchema.description := AParams.ByNameText(JWT_ISSUER_PARAM, JWT_ISSUER_PARAM_DEFAULT).AsString;
  FCookieSecurityConfigured := True;
end;

procedure TOpenAPIHelper.ReadInfoFromParams(const AParams: TMARSParameters);

  function FromParams(const AName: string; const ADefault: TValue): TValue;
  begin
    Result := AParams.ByNameText('OpenAPI.' + AName, ADefault);
  end;

begin
  info.title          := FromParams('info.title', 'MARS API').AsString;
  info.summary        := FromParams('info.summary', '').AsString;
  info.description    := FromParams('info.description', '').AsString;
  info.termsOfService := FromParams('info.termsOfService', '').AsString;

  info.contact.name   := FromParams('info.contact.name', 'MARS Developer').AsString;
  info.contact.url    := FromParams('info.contact.url', 'https://mars.space').AsString;
  info.contact.email  := FromParams('info.contact.email', 'me@mars.space').AsString;

  info.license.name       := FromParams('info.license.name', '').AsString;
  info.license.identifier := FromParams('info.license.identifier', '').AsString;
  info.license.url        := FromParams('info.license.url', '').AsString;

  info.x_logo.url := FromParams('info.x-logo.url', 'https://andreamagni.eu/images/MARS-Curiosity-d.png').AsString;
  info.x_logo.backgroundColor := FromParams('info.x-logo.backgroundColor', '#FFFFFF').AsString;
  info.x_logo.altText := FromParams('info.x-logo.altText', 'Powered by MARS-Curiosity REST library').AsString;
  info.x_logo.href := FromParams('info.x-logo.href', 'https://github.com/andrea-magni/MARS').AsString;

  info.version := FromParams('info.version', '0.1.0').AsString;
end;

procedure TOpenAPIHelper.ReadOperation(const AOperation: TOperation;
  const ARes: TMARSResourceMetadata; const AMet: TMARSMethodMetadata);
var
  LMethodSummary: string;
  LParamMD: TMARSRequestParamMetadata;
  LParam: TParameter;
  LRequestBody: TRequestBody;
  LContent: TMediaTypeObj;
  LProperty: TSchema;
  LMediaType: string;
  LMetAuthorization: string;
begin
  LMethodSummary := AMet.Summary;
  if LMethodSummary = '' then
    LMethodSummary := AMet.Description;
  if LMethodSummary = '' then
    LMethodSummary := AMet.Name;

  AOperation.operationId := AMet.Name;
  AOperation.summary := LMethodSummary;
  AOperation.description := AMet.Description;
  AOperation.tags := [ARes.Path];

  // PARAMETERS
  AMet.ForEachParameter(
    procedure (AParam: TMARSRequestParamMetadata)
    var
      LIn: string;
    begin
      LIn := MARSKindToOpenAPIKind(AParam.Kind);
      if IndexText(LIn, ['query', 'header', 'path', 'cookie']) <> -1 then
      begin
        LParam := AOperation.AddParameter(AParam.Name, LIn);
        LParam.description := AParam.Description;
        LParam.schema.SetType(AParam.DataTypeRttiType, Self);
        if LIn = 'path' then
          LParam.required := True;
      end;
    end
  );

  // REQUEST BODY
  if AMet.Consumes <> '' then
  begin
    LRequestBody := AOperation.requestBody;
    LRequestBody.description := 'Request body';
    for LMediaType in AMet.Consumes.Split([',']) do
    begin
      LContent := LRequestBody.AddContent(LMediaType);

      // x-www-form-urlencoded
      if LMediaType = TMediaType.APPLICATION_FORM_URLENCODED_TYPE then
      begin
        LContent.schema.SetType('object');
        for LParamMD in AMet.ParametersByKind('FormParam') do
        begin
          LProperty := LContent.schema.AddProperty(LParamMD.Name);
          LProperty.description := LParamMD.Description;
          LProperty.SetType(LParamMD.DataTypeRttiType, Self);
        end;
      end
      else // all other request body types
      begin
        LParamMD := AMet.ParameterByKind('BodyParam');
        if Assigned(LParamMD) then
        begin
          LContent.schema.SetType(LParamMD.DataTypeRttiType, Self);
          LRequestBody.description := LParamMD.Name + ': ' + LParamMD.DataTypeRttiType.Name;
        end;
      end;
    end;
  end;

  var response := AOperation.AddResponse('200');
  response.description := 'Successful response';
  for LMediaType in AMet.Produces.Split([',']) do
  begin
    response.AddContent(LMediaType)
      .schema.SetType(AMet.DataTypeRttiType, Self)
  end;

  AOperation.AddResponse('500').description := 'Internal server error';

  LMetAuthorization := AMet.FullAuthorization;
  if LMetAuthorization <> '' then //AM TODO Check what happens with Deny DenyAll etc
  begin
    if FBearerSecurityConfigured then
      AOperation.AddSecurityRequirement('JWT_bearer', LMetAuthorization.Split([',']));
    if FCookieSecurityConfigured then
      AOperation.AddSecurityRequirement('JWT_cookie', LMetAuthorization.Split([',']));

    if FBearerSecurityConfigured or FCookieSecurityConfigured then
      AOperation.AddResponse('403').description := 'Token missing, not valid or expired';
  end;
end;

function TOpenAPIHelper.AddServerFromEngine(const AEngine: TMARSEngine): TServer;
begin
  Result := AddServer;
             // '{protocol}://localhost:{port}/rest{application}'
  Result.url := '{protocol}://localhost:{port}' + AEngine.BasePath + '{application}';
  Result.description := AEngine.Name;

  Result.variables.Add('port'
  , TServerVariable.Create([AEngine.Port.ToString, AEngine.PortSSL.ToString]
    , AEngine.Port.ToString, 'Port number')
  );

  Result.variables.Add('protocol'
  , TServerVariable.Create(['http', 'https']
    , 'http', 'Protocol')
  );
end;

end.
