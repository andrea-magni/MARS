unit MARS.OpenAPI.v3.Utils;

interface

uses
  Classes, SysUtils
, MARS.OpenAPI.v3
, MARS.Core.Engine, MARS.Core.Application, MARS.Core.Activation.Interfaces
;


type
  TOpenAPIHelper = class helper for TOpenAPI
    class function BuildDemoAPI(): TOpenAPI;
    class function BuildFrom(const AEngine: TMARSEngine; const AApplication: TMARSApplication): TOpenAPI; overload;
    class function BuildFrom(const AActivation: IMARSActivation): TOpenAPI; overload;
  end;


implementation

uses
  System.Rtti
, MARS.Utils.Parameters, MARS.Core.Registry.Utils
, MARS.Metadata, MARS.Metadata.Reader
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
    LOpenAPI.info.summary        := FromEngineParams('info.summary', 'A brief summary here.').AsString;
    LOpenAPI.info.description    := FromEngineParams('info.description', 'A description here.').AsString;
    LOpenAPI.info.termsOfService := FromEngineParams('info.termsOfService', 'https://dummy.org/termsOfService/').AsString;

    LOpenAPI.info.contact.name   := FromEngineParams('info.contact.name', 'Martian Developer').AsString;
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
              var
                tag: TTag;
              begin
                tag := LOpenAPI.AddTag(ARes.Path, ARes.Description);
              end
            );



            AAppMD.ForEachMethod(
              procedure (ARes: TMARSResourceMetadata; AMet: TMARSMethodMetadata)
              var
                path: TPathItem;
              begin
                path := LOpenAPI.GetPath('/' + ARes.Path + AMet.Path);

                path.summary := ARes.Name + ' resource';
                path.description := ARes.Description;

                if AMet.HttpMethod = 'GET' then
                begin
                  path.get.operationId := AMet.Name;
                  path.get.description := AMet.Description;
                  path.get.tags := [ARes.Path];
                  var response := path.get.AddResponse('200');
                  response.description := 'Response for ' + AMet.Description;
                  for var LProduces in AMet.Produces.Split([',']) do
                    response.AddContent(LProduces);
                end
                else if AMet.HttpMethod = 'POST' then
                begin
                  path.post.operationId := AMet.Name;
                  path.post.description := AMet.Description;
                  path.post.tags := [ARes.Path];
                  var response := path.post.AddResponse('200');
                  response.description := 'Response for ' + AMet.Description;
                  for var LProduces in AMet.Produces.Split([',']) do
                    response.AddContent(LProduces);
                end
                else if AMet.HttpMethod = 'PUT' then
                begin
                  path.put.operationId := AMet.Name;
                  path.put.description := AMet.Description;
                  path.put.tags := [ARes.Path];
                  var response := path.put.AddResponse('200');
                  response.description := 'Response for ' + AMet.Description;
                  for var LProduces in AMet.Produces.Split([',']) do
                    response.AddContent(LProduces);
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

end.
