unit MARS.OpenAPI.v3.Utils;

interface

uses
  Classes, SysUtils, System.Rtti, System.TypInfo, MARS.Rtti.Utils
, MARS.OpenAPI.v3
, MARS.Core.Engine, MARS.Core.Application, MARS.Core.Activation.Interfaces
;


type
  TOpenAPIHelper = class helper for TOpenAPI
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
, MARS.Utils.Parameters, MARS.Core.Registry.Utils, MARS.Core.URL
, MARS.Metadata, MARS.Metadata.Reader
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
  LEngineParams, LAppParams: TMARSParameters;
  LReader: TMARSMetadataReader;

  function FromEngineParams(const AName: string; const ADefault: TValue): TValue;
  begin
    Result := LEngineParams.ByNameText('OpenAPI.' + AName, ADefault);
  end;

begin
  Assert(Assigned(AEngine));

  LEngineParams := AEngine.Parameters;
  LAppParams := AApplication.Parameters;

  LOpenAPI := TOpenAPI.Create;
  try
    LOpenAPI.openapi := '3.0.2';

    LOpenAPI.info.title          := FromEngineParams('info.title', 'MARS ' + AEngine.Name).AsString;
    LOpenAPI.info.summary        := FromEngineParams('info.summary', '').AsString;
    LOpenAPI.info.description    := FromEngineParams('info.description', '').AsString;
    LOpenAPI.info.termsOfService := FromEngineParams('info.termsOfService', '').AsString;

    LOpenAPI.info.contact.name   := FromEngineParams('info.contact.name', 'MARS Developer').AsString;
    LOpenAPI.info.contact.url    := FromEngineParams('info.contact.url', 'https://mars.space').AsString;
    LOpenAPI.info.contact.email  := FromEngineParams('info.contact.email', 'me@mars.space').AsString;

    LOpenAPI.info.license.name       := FromEngineParams('info.license.name', '').AsString;
    LOpenAPI.info.license.identifier := FromEngineParams('info.license.identifier', '').AsString;
    LOpenAPI.info.license.url        := FromEngineParams('info.license.url', '').AsString;

    LOpenAPI.info.version := FromEngineParams('info.version', '0.1.0').AsString;

    var server := LOpenAPI.AddServer;
               // '{protocol}://localhost:{port}/rest{application}'
    server.url := '{protocol}://localhost:{port}' + AEngine.BasePath + '{application}';
    server.description := AEngine.Name;

    server.variables.Add('port'
    , TServerVariable.Create([AEngine.Port.ToString, AEngine.PortSSL.ToString]
      , AEngine.Port.ToString
      , 'Port number'
      )
    );

    server.variables.Add('protocol'
    , TServerVariable.Create(['http', 'https']
      , 'http'
      , 'Protocol'
      )
    );

    server.variables.Add('application'
    , TServerVariable.Create([AApplication.BasePath]
      , AApplication.BasePath
      , 'Application'
      )
    );


    LReader := TMARSMetadataReader.Create(AEngine);
    try
      LReader.Metadata.ForEachApplication(
        procedure (AAppMD: TMARSApplicationMetadata)
        begin
          if AAppMD.Name = AApplication.Name then
          begin
            AAppMD.ForEachResource(
              procedure (ARes: TMARSResourceMetadata)
//              var
//                tag: TTag;
              begin
                {tag := }LOpenAPI.AddTag(ARes.Path, ARes.Description);
              end
            );



            AAppMD.ForEachMethod(
              procedure (ARes: TMARSResourceMetadata; AMet: TMARSMethodMetadata)
              var
                path: TPathItem;
                LMetDescription: string;
              begin
                if not AMet.Visible then
                  Exit;

                LMetDescription := AMet.Description;
                if LMetDescription = '' then
                  LMetDescription := AMet.Name;

                path := LOpenAPI.GetPath(TMARSURL.CombinePath([ARes.Path, AMet.Path], True, False));

                path.summary := ARes.Name + ' resource';
                path.description := ARes.Description;

                var LOperation: TOperation := nil;
                if AMet.HttpMethodLowerCase = 'get' then
                  LOperation := path.get
                else if AMet.HttpMethodLowerCase = 'post' then
                  LOperation := path.post
                else if AMet.HttpMethodLowerCase = 'put' then
                  LOperation := path.put
                else if AMet.HttpMethodLowerCase = 'delete' then
                  LOperation := path.delete
                else if AMet.HttpMethodLowerCase = 'options' then
                  LOperation := path.options
                else if AMet.HttpMethodLowerCase = 'head' then
                  LOperation := path.head
                else if AMet.HttpMethodLowerCase = 'patch' then
                  LOperation := path.patch
                else if AMet.HttpMethodLowerCase = 'trace' then
                  LOperation := path.trace;

                if Assigned(LOperation) then
                begin
                  LOperation.operationId := AMet.Name;
                  LOperation.description := LMetDescription;

                  LOperation.tags := [ARes.Path];

                  AMet.ForEachParameter(
                    procedure (AParam: TMARSRequestParamMetadata)
                    begin
                      var LIn := LOpenAPI.MARSKindToOpenAPIKind(AParam.Kind);
                      if IndexText(LIn, ['query', 'header', 'path', 'cookie']) <> -1 then
                      begin
                        var LParam := LOperation.AddParameter(AParam.Name, LIn);
                        LParam.description := AParam.Description;
                        LParam.schema.SetType(AParam.DataTypeRttiType, LOpenAPI);
                        if LIn = 'path' then
                          LParam.required := True;
                      end;
                    end
                  );

                  if AMet.Consumes <> '' then
                  begin
                    var body := LOperation.requestBody;
                    body.description := 'Body for ' + LMetDescription;
                    for var LConsumes in AMet.Consumes.Split([',']) do
                    begin
                      var LContent := body.AddContent(LConsumes);

                      if LConsumes = TMediaType.APPLICATION_FORM_URLENCODED_TYPE then
                      begin
                        LContent.schema.SetType('object');
                        AMet.ForEachParameter(
                          procedure (AParam: TMARSRequestParamMetadata)
                          begin
                            if LOpenAPI.MARSKindToOpenAPIKind(AParam.Kind) = 'form' then
                            begin
                              var LProperty := LContent.schema.AddProperty(AParam.Name);
                              LProperty.description := AParam.Description;
                              LProperty.SetType(AParam.DataTypeRttiType, LOpenAPI);
                            end;
                          end
                        );

                      end
                      else // everything else from x-www-form-urlencoded
                      begin
                        AMet.ForEachParameter(
                          procedure (AParam: TMARSRequestParamMetadata)
                          begin
                            if LOpenAPI.MARSKindToOpenAPIKind(AParam.Kind) = 'body' then
                              LContent.schema.SetType(AParam.DataTypeRttiType, LOpenAPI);
                          end
                        );
                      end;
                    end;
                  end;

                  var response := LOperation.AddResponse('200');
                  response.description := 'Successful response for ' + LMetDescription;
                  for var LProduces in AMet.Produces.Split([',']) do
                  begin
                    var LContent := response.AddContent(LProduces);
                    LContent.schema.SetType(AMet.DataTypeRttiType, LOpenAPI)
                  end;
                end;

              end
            );
          end;
        end
      );
    finally
      LReader.Free;
    end;


//    AApplication.EnumerateResources(
//      procedure(AName: string; AInfo: TMARSConstructorInfo)
//      begin
//
//        var path := LOpenAPI.AddPath('/' + AName);
//        path.summary := AName + ' resource';
//        path.description := AName + ' resource, implementor: ' + AInfo.TypeTClass.ClassName;
//
//        path.get.description := 'GET request';
//        var response := path.get.AddResponse('200');
//        response.description := 'A greeting';
//        response.AddContent('text/plain');
//      end
//    );

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

end.
