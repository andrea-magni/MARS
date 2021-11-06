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
    procedure ReadInfoFromParams(const AParams: TMARSParameters);
    function AddServerFromEngine(const AEngine: TMARSEngine): TServer;
    procedure ReadApplication(const AAppMD: TMARSApplicationMetadata);
    procedure ReadOperation(const AOperation: TOperation; const ARes: TMARSResourceMetadata;
      const AMet: TMARSMethodMetadata);
  public
    function EnsureTypeInComponentsSchemas(const AType: TRttiType): Boolean;
    function MARSKindToOpenAPIKind(const AString: string): string;
    function MARSDataTypeToOpenAPIType(const AType: TRttiType; const ARefPrefix: string = '#/components/schemas/'): string;
    class function BuildDemoAPI(): TOpenAPI;
    class function BuildFrom(const AEngine: TMARSEngine; const AApplication: TMARSApplication): TOpenAPI; overload;
    class function BuildFrom(const AActivation: IMARSActivation): TOpenAPI; overload;
  end;


implementation

uses
  StrUtils
, MARS.Core.Registry.Utils, MARS.Core.URL
, MARS.Metadata.Reader
, MARS.Core.MediaType, MARS.Core.JSON
;

{ TOpenAPIHelper }

class function TOpenAPIHelper.BuildDemoAPI: TOpenAPI;
var
  server1, server2, server3: TServer;
begin
  Result := TOpenAPI.Create;
  Result.openapi := '3.0.2';

  Result.info.title := 'Sample Pet Store App';
//  Result.info.summary := 'A pet store manager.';
  Result.info.description := 'This is a sample server for a pet store.';
  Result.info.termsOfService := 'https://example.com/terms/';

  Result.info.contact.name := 'API Support';
  Result.info.contact.url := ' https://www.example.com/support';
  Result.info.contact.email := 'support@example.com';

  Result.info.license.name := 'Apache 2.0';
  Result.info.license.url := 'https://www.apache.org/licenses/LICENSE-2.0.html';

  Result.info.version := '1.0.1';

  server1 := TServer.Create;
  server1.url := 'https://development.gigantic-server.com/';
  server1.description := 'Development server';
  server1.variables.Add('test', TServerVariable.Create(['One', 'Two'], 'def1', 'Andrea Magni'));

  server2 := TServer.Create;
  server2.url := 'https://{username}.gigantic-server.com/';
  server2.description := 'User specific server';
  server2.variables.Add('username', TServerVariable.Create([], 'demo', 'this value is assigned by the service provider'));

  server3 := TServer.Create;
  server3.url := 'https://development.gigantic-server.com:{port}/{basepath}';
  server3.description := 'Port specific server';
  server3.variables.Add('port', TServerVariable.Create(['8080', '8443'], '8080', 'port number'));
  server3.variables.Add('basepath', TServerVariable.Create([], 'v2', ''));

  Result.servers.Add(server1);
  Result.servers.Add(server2);
  Result.servers.Add(server3);
end;

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
  LJSONName: string;
begin
  Result := False;
  LSchemaExists := components.HasSchema(AType.Name);
  if not LSchemaExists then
  begin
    Result := True;
    LSchema := components.AddSchema(AType.Name);
    LSchema.SetType('object');
    LSchema.description := 'Schema for type ' + AType.QualifiedName;

    if (AType.IsInstance) or (AType.IsRecord) then
    begin
      for var LMember in AType.GetPropertiesAndFields do
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
        if LJSONName <> '' then
        begin
          var LProperty := LSchema.AddProperty(LMember.Name);
          LProperty.SetType(LMember.GetRttiType, Self);
        end;
      end;
    end
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
      AddTag(ARes.Path, ARes.Description);
    end
  );

  AAppMD.ForEachMethod(
    procedure (ARes: TMARSResourceMetadata; AMet: TMARSMethodMetadata)
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

  info.version := FromParams('info.version', '0.1.0').AsString;
end;

procedure TOpenAPIHelper.ReadOperation(const AOperation: TOperation;
  const ARes: TMARSResourceMetadata; const AMet: TMARSMethodMetadata);
var
  LMetDescription: string;
  LParamMD: TMARSRequestParamMetadata;
  LParam: TParameter;
  LRequestBody: TRequestBody;
  LContent: TMediaTypeObj;
  LProperty: TSchema;
  LMediaType: string;
begin
  LMetDescription := AMet.Description;
  if LMetDescription = '' then
    LMetDescription := AMet.Name;

  AOperation.operationId := AMet.Name;
  AOperation.description := LMetDescription;
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
    LRequestBody.description := 'Body for ' + LMetDescription;
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
      else // all other request LRequestBody types
      begin
        LParamMD := AMet.ParameterByKind('BodyParam');
        if Assigned(LParamMD) then
          LContent.schema.SetType(LParamMD.DataTypeRttiType, Self);
      end;
    end;
  end;

  var response := AOperation.AddResponse('200');
  response.description := 'Successful response for ' + LMetDescription;
  for LMediaType in AMet.Produces.Split([',']) do
  begin
    response.AddContent(LMediaType)
      .schema.SetType(AMet.DataTypeRttiType, Self)
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
