(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes

  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.JSON
  , MARS.Core.Response

  , MARS.Core.Token.Resource

  , MARS.dmustache, MARS.dmustache.InjectionService
  , MARS.Metadata, MARS.Metadata.JSON, MARS.Metadata.InjectionService
  ;

type
  [Path('helloworld')]
  THelloWorldResource = class
  protected
    [Context] mustache: TMARSdmustache;
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;

    [GET, Path('/to/{Someone}'), Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloTo([PathParam] Someone: string): string;

    [GET, Path('metadata'), Produces(TMediaType.TEXT_HTML)]
    function Metadata([Context] Metadata: TMARSApplicationMetadata): string;

    [GET, Path('metadata/json'), Produces(TMediaType.APPLICATION_JSON)]
    function MetadataJSON([Context] Metadata: TMARSApplicationMetadata): string;
  end;

  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  end;

implementation

uses
  MARS.Core.Registry
, SynCommons
;

{ THelloWorldResource }

function THelloWorldResource.Metadata(Metadata: TMARSApplicationMetadata): string;
begin
  Result := mustache.RenderTemplateWithJSON('metadata.html', Metadata.ToJSON, True);
end;

function THelloWorldResource.MetadataJSON(
  Metadata: TMARSApplicationMetadata): string;
var
  LJSON: TJSONObject;
begin
  LJSON := Metadata.ToJSON;
  try
    Result := LJSON.ToJSON;
  finally
    LJSON.Free;
  end;
end;

function THelloWorldResource.SayHelloTo(Someone: string): string;
var
  LContext: Variant;
begin
  TDocVariant.New(LContext);
  LContext.Someone := Someone;
  LContext.Now := Now;
  Result := mustache.Render('Hello, {{Someone}}. Current time is: {{Now}}.', LContext);
end;

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello World!';
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;
end.
