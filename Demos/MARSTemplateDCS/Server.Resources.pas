(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.JSON, MARS.Core.Response
, MARS.Core.URL
//, MARS.Core.Token
, MARS.Core.Token.Resource
, MARS.OpenAPI.v3, MARS.Metadata.Attributes
;

type
  [Path('helloworld')]
  THelloWorldResource = class
  protected
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;

    [ GET
    , Path('/openapi')
    , Produces(TMediaType.APPLICATION_JSON)
    , Produces(TMediaType.APPLICATION_YAML)
    // prevents the method itself being part of the OpenAPI specification
    , MetaVisible(False)]
    function GetOpenAPI([Context] AOpenAPI: TOpenAPI): TOpenAPI;
  end;

  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  end;

implementation

uses
    MARS.Core.Registry
;

{ THelloWorldResource }

function THelloWorldResource.GetOpenAPI(AOpenAPI: TOpenAPI): TOpenAPI;
begin
  Result := AOpenAPI;
end;

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello World!';
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;
end.
