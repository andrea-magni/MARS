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
  , MARS.Metadata.Attributes
  ;

type
  [Path('helloworld'), MetaDescription('This is my helloworld resource!')]
  THelloWorldResource = class
  protected
    [Context] mustache: TMARSdmustache;
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;

    [GET, Path('/to/{Someone}'), Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloTo([PathParam] Someone: string): string;

    [GET, Path('metadata/simple'), Produces(TMediaType.TEXT_HTML)]
    function MetadataSimple([Context] Metadata: TMARSApplicationMetadata): string;

    [GET, Path('metadata/bootstrap'), Produces(TMediaType.TEXT_HTML)]
    function MetadataBootstrap([Context] Metadata: TMARSApplicationMetadata): string;

    [GET, Path('metadata/swagger'), Produces(TMediaType.APPLICATION_JSON), MetaVisible(False)]
    function MetadataSwagger([Context] Metadata: TMARSEngineMetadata): TMARSResponse;

    [GET, Path('metadata/json'), Produces(TMediaType.APPLICATION_JSON)]
    function MetadataJSON([Context] Metadata: TMARSEngineMetadata): TJSONObject;
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

function THelloWorldResource.MetadataSimple(Metadata: TMARSApplicationMetadata): string;
begin
  Result := mustache.RenderTemplateWithJSON('metadata_simple.html', Metadata.ToJSON, True);
end;

function THelloWorldResource.MetadataSwagger(
  Metadata: TMARSEngineMetadata): TMARSResponse;
begin
  Result := TMARSResponse.Create;
  Result.ContentType := 'application/json';
  Result.Content := mustache.RenderTemplateWithJSON('swagger.json', Metadata.ToJSON, True);
  Result.StatusCode := 200;
end;

function THelloWorldResource.MetadataBootstrap(
  Metadata: TMARSApplicationMetadata): string;
begin
  Result := mustache.RenderTemplateWithJSON('metadata_bootstrap.html', Metadata.ToJSON, True);
end;

function THelloWorldResource.MetadataJSON(
  Metadata: TMARSEngineMetadata): TJSONObject;
begin
  Result := Metadata.ToJSON;
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
