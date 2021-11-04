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
, MARS.Utils.Parameters
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
  LEngineParams, LAppParams: TMARSParameters;

  function FromEngineParams(const AName: string; const ADefault: TValue): TValue;
  begin
    Result := LEngineParams.ByNameText('OpenAPI.' + AName, ADefault);
  end;

begin
  Assert(Assigned(AEngine));

  LEngineParams := AEngine.Parameters;
  LAppParams := AApplication.Parameters;

  Result := TOpenAPI.Create;
  try
    Result.openapi := '3.0.2';

    Result.info.title          := FromEngineParams('info.title', 'MARS ' + AEngine.Name).AsString;
    Result.info.summary        := FromEngineParams('info.summary', 'A brief summary here.').AsString;
    Result.info.description    := FromEngineParams('info.description', 'A description here.').AsString;
    Result.info.termsOfService := FromEngineParams('info.termsOfService', 'https://dummy.org/termsOfService/').AsString;

    Result.info.contact.name   := FromEngineParams('info.contact.name', 'Martian Developer').AsString;
    Result.info.contact.url    := FromEngineParams('info.contact.url', 'https://mars.space').AsString;
    Result.info.contact.email  := FromEngineParams('info.contact.email', 'me@mars.space').AsString;

    Result.info.license.name       := FromEngineParams('info.license.name', '').AsString;
    Result.info.license.identifier := FromEngineParams('info.license.identifier', '').AsString;
    Result.info.license.url        := FromEngineParams('info.license.url', '').AsString;

    Result.info.version := FromEngineParams('info.version', '0.1.0').AsString;
    xx
    var server := Result.AddServer;
    server.url := '{protocol}://localhost:{port}/rest/default';
    server.description := 'Development server';
    server.variables.Add('port', TServerVariable.Create(['8080', '8443'], '8080', 'Port number'));
    server.variables.Add('protocol', TServerVariable.Create(['http', 'https'], 'http', 'Protocol'));

    var path := Result.AddPath('/helloworld');
    path.summary := 'HelloWorld resource';
    path.description := 'HelloWorld resource';
    path.get.description := 'GET request';
    var response := path.get.AddResponse('200');
    response.description := 'A greeting';
    response.AddContent('text/plain');


  except
    FreeAndNil(Result);
    raise;
  end;
end;

class function TOpenAPIHelper.BuildFrom(
  const AActivation: IMARSActivation): TOpenAPI;
begin
  Result := BuildFrom(AActivation.Engine, AActivation.Application);
end;

end.
