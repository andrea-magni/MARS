(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.OpenAPI.Elements;

// The documentation of an application with Stoplight Elements (https://stoplight.io/open-source/elements),
// an open source web component that renders its OpenAPI document: derive a resource with a path,
//
//   [Path('docs'), MetaVisible(False)]
//   TDocsResource = class(TOpenAPIElementsResource);
//
// and open <application>/docs. The page loads Elements from a CDN (unpkg) and the document from
// <application>/openapi. Application parameters:
//   OpenAPI.Elements.SpecPath     the path of the OpenAPI document in the application (openapi)
//   OpenAPI.Elements.Layout       sidebar (default) or stacked
//   OpenAPI.Elements.Script       the URL of web-components.min.js (i.e. a copy of your own)
//   OpenAPI.Elements.Stylesheet   the URL of styles.min.css

interface

uses
  System.SysUtils, System.Classes
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.URL
, MARS.Core.Application.Interfaces
;

const
  OPENAPI_ELEMENTS_VERSION = '9.0.28';
  OPENAPI_ELEMENTS_SCRIPT = 'https://unpkg.com/@stoplight/elements@' + OPENAPI_ELEMENTS_VERSION + '/web-components.min.js';
  OPENAPI_ELEMENTS_STYLESHEET = 'https://unpkg.com/@stoplight/elements@' + OPENAPI_ELEMENTS_VERSION + '/styles.min.css';

type
  TOpenAPIElementsResource = class
  protected
    [Context] App: IMARSApplication;
    [Context] URL: TMARSURL;
    function Parameter(const AName, ADefault: string): string; virtual;
    // the absolute path of the OpenAPI document (base path of the application + SpecPath)
    function SpecURL: string; virtual;
  public
    [GET, Produces(TMediaType.TEXT_HTML)]
    function GetPage: string; virtual;
  end;

implementation

uses
  System.NetEncoding;

{ TOpenAPIElementsResource }

function TOpenAPIElementsResource.Parameter(const AName, ADefault: string): string;
begin
  Result := App.Parameters.ByName('OpenAPI.Elements.' + AName, ADefault).ToString;
  if Result = '' then
    Result := ADefault;
end;

function TOpenAPIElementsResource.SpecURL: string;
begin
  Result := TMARSURL.CombinePath([URL.BasePath, Parameter('SpecPath', 'openapi')], True);
end;

function TOpenAPIElementsResource.GetPage: string;

  function Attr(const AValue: string): string;
  begin
    Result := TNetEncoding.HTML.Encode(AValue);
  end;

begin
  Result :=
      '<!doctype html>' + sLineBreak
    + '<html lang="en">' + sLineBreak
    + '<head>' + sLineBreak
    + '  <meta charset="utf-8">' + sLineBreak
    + '  <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">' + sLineBreak
    + '  <title>' + Attr(App.Name) + ' API</title>' + sLineBreak
    + '  <script src="' + Attr(Parameter('Script', OPENAPI_ELEMENTS_SCRIPT)) + '"></script>' + sLineBreak
    + '  <link rel="stylesheet" href="' + Attr(Parameter('Stylesheet', OPENAPI_ELEMENTS_STYLESHEET)) + '">' + sLineBreak
    + '  <style>html, body { height: 100%; margin: 0; }</style>' + sLineBreak
    + '</head>' + sLineBreak
    + '<body>' + sLineBreak
    // the hash router: the navigation of the page never reaches the server
    + '  <elements-api apiDescriptionUrl="' + Attr(SpecURL) + '" router="hash" layout="'
    + Attr(Parameter('Layout', 'sidebar')) + '"></elements-api>' + sLineBreak
    + '</body>' + sLineBreak
    + '</html>' + sLineBreak;
end;

end.
