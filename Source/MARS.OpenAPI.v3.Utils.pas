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
  server: TServer;
begin
  demo.openapi := '3.0.2';

  demo.info.title := 'Sample Pet Store App';
  demo.info.summary := 'A pet store manager.';
  demo.info.description := 'This is a sample server for a pet store.';
  demo.info.termsOfService := 'https://example.com/terms/';

  demo.info.contact.name := 'API Support';
  demo.info.contact.url := ' https://www.example.com/support';
  demo.info.contact.email := 'support@example.com';

  demo.info.license.name := 'Apache 2.0';
  demo.info.license.url := 'https://www.apache.org/licenses/LICENSE-2.0.html';

  demo.info.version := '1.0.1';

  server.url := 'https://development.gigantic-server.com/v1';
  server.description := 'Development server';


  demo.servers := [server];

  Result := demo;
end;

end.
