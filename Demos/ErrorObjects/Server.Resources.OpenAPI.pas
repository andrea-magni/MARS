(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Resources.OpenAPI;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.URL
, MARS.WebServer.Resources
, MARS.OpenAPI.v3, MARS.Metadata.Attributes
;

type
  [Path('openapi'), MetaVisible(False)]
  TOpenAPIResource = class
  protected
  public
    [GET, Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_YAML)]
    function GetOpenAPI([Context] AOpenAPI: TOpenAPI): TOpenAPI;
  end;

  [Path('www/{*}'), RootFolder('{bin}\..\..\..\www\swagger-ui-3.52.5-dist', True), MetaVisible(False)]
  TStaticContentResource = class(TFileSystemResource)
  end;

implementation

uses
  MARS.Core.Registry
;

{ TOpenAPIResource }

function TOpenAPIResource.GetOpenAPI(AOpenAPI: TOpenAPI): TOpenAPI;
begin
  Result := AOpenAPI;
end;

initialization
  MARSRegister([
    TOpenAPIResource
  , TStaticContentResource
  ]);

end.
