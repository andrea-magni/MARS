unit MARS.OpenAPI.v3.Utils;

interface

uses
//  Classes, SysUtils,
  MARS.OpenAPI.v3
;

function DemoAPI: TOpenAPI;

implementation

function DemoAPI: TOpenAPI;
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

end.
