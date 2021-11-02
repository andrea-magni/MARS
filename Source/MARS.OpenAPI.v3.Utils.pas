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
  demo: TOpenAPI;
  server1, server2, server3: TServer;
begin
  demo.openapi := '3.0.2';

  demo.info.title := 'Sample Pet Store App';
//  demo.info.summary := 'A pet store manager.';
  demo.info.description := 'This is a sample server for a pet store.';
  demo.info.termsOfService := 'https://example.com/terms/';

  demo.info.contact.name := 'API Support';
  demo.info.contact.url := ' https://www.example.com/support';
  demo.info.contact.email := 'support@example.com';

  demo.info.license.name := 'Apache 2.0';
  demo.info.license.url := 'https://www.apache.org/licenses/LICENSE-2.0.html';

  demo.info.version := '1.0.1';

  server1.url := 'https://development.gigantic-server.com/';
  server1.description := 'Development server';
  server1.variables.Add('test', TServerVariable.Create(['One', 'Two'], 'def1', 'Andrea Magni'));

  server2.url := 'https://{username}.gigantic-server.com/';
  server2.description := 'User specific server';
  server2.variables.Add('username', TServerVariable.Create([], 'demo', 'this value is assigned by the service provider'));

  server3.url := 'https://development.gigantic-server.com:{port}/';
  server3.description := 'Port specific server';
  server3.variables.Add('port', TServerVariable.Create(['8080', '8443'], 'port', ''));

  demo.servers := [server1, server2, server3];

  Result := demo;
end;

end.
