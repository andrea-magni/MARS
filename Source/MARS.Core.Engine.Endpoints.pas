(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Engine.Endpoints;

interface

uses
  MARS.Core.Engine.Interfaces
;

// The endpoints of AEngine as text, i.e. for the "endpoints" command of the console servers:
// its applications, their resources and groups of routes, and a line for each endpoint with
// the HTTP method and the path (the base paths of the engine and of the application included).
//
//   DefaultEngine  /rest  (port 8080)
//     DefaultApp  /rest/default
//       THelloWorldResource
//         GET     /rest/default/helloworld
//   1 endpoint
function MARSEndpointsAsText(const AEngine: IMARSEngine): string;

// The URLs to show when a server starts, a line each (most terminals make them clickable):
// the base URL of each application of AEngine and, where the application has them, its OpenAPI
// document (a GET method returning TOpenAPI), Swagger UI (static content on "www", as in the
// templates), the documentation page (TOpenAPIElementsResource) and the other static contents
// (TFileSystemResource). Only what can work: a static content whose folder does not exist is
// left out, and Swagger UI needs its index.html there (and the OpenAPI document).
// With both Port and PortSSL, the http URLs and then the https ones.
//
//   - DefaultApp: http://localhost:8080/rest/default
//     - OpenAPI: http://localhost:8080/rest/default/openapi
//     - Swagger UI: http://localhost:8080/rest/default/www/index.html?openAPIURL=...
//     - API docs: http://localhost:8080/rest/default/docs
function MARSStartupURLsAsText(const AEngine: IMARSEngine; const AHost: string = 'localhost'): string;

implementation

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.Rtti, System.NetEncoding
, System.IOUtils
, MARS.Core.Application.Interfaces, MARS.Core.Registry.Utils, MARS.Core.Routes, MARS.Core.URL
, MARS.Core.Attributes, MARS.Rtti.Utils, MARS.WebServer.Resources
;

const
  // by name: no need to link the OpenAPI units into every server
  OPENAPI_TYPE_NAME = 'TOpenAPI';
  DOCS_CLASS_NAME = 'TOpenAPIElementsResource';
  SWAGGER_UI_PATH = 'www';
  SWAGGER_UI_PAGE = 'index.html';

type
  TFileSystemResourceClass = class of TFileSystemResource;

// The folder a static content resource serves, with the trailing delimiter: '' when AClass is
// not one, has no [RootFolder] or the folder does not exist
function StaticContentFolder(const AClass: TClass): string;
var
  LResource: TFileSystemResource;
begin
  Result := '';
  if not AClass.InheritsFrom(TFileSystemResource) then
    Exit;
  try
    LResource := TFileSystemResourceClass(AClass).Create; // applies [RootFolder]
    try
      if LResource.RootFolder <> '' then
        Result := IncludeTrailingPathDelimiter(TPath.GetFullPath(LResource.RootFolder));
    finally
      LResource.Free;
    end;
    if not TDirectory.Exists(Result) then
      Result := '';
  except
    Result := ''; // cannot tell: not shown
  end;
end;

// the parts joined with one slash, without the leading and trailing ones: 'rest/default/www'
// for '/rest', '/default/' and 'www' (a "http://host" first part stays as it is)
function JoinPath(const AParts: array of string): string;
var
  LPart, LTrimmed: string;
begin
  Result := '';
  for LPart in AParts do
  begin
    LTrimmed := LPart.Trim([TMARSURL.URL_PATH_SEPARATOR]);
    if LTrimmed = '' then
      Continue;
    if Result <> '' then
      Result := Result + TMARSURL.URL_PATH_SEPARATOR;
    Result := Result + LTrimmed;
  end;
end;

function InheritsFromClassNamed(AClass: TClass; const AClassName: string): Boolean;
begin
  Result := False;
  while Assigned(AClass) and not Result do
  begin
    Result := AClass.ClassNameIs(AClassName);
    AClass := AClass.ClassParent;
  end;
end;

function MARSStartupURLsAsText(const AEngine: IMARSEngine; const AHost: string): string;
var
  LLines: TStringList;
  LBaseURLs: TArray<string>;
  LBaseURL: string;
begin
  LBaseURLs := [];
  if AEngine.Port <> 0 then
    LBaseURLs := LBaseURLs + [Format('http://%s:%d', [AHost, AEngine.Port])];
  if AEngine.PortSSL <> 0 then
    LBaseURLs := LBaseURLs + [Format('https://%s:%d', [AHost, AEngine.PortSSL])];

  LLines := TStringList.Create;
  try
    for LBaseURL in LBaseURLs do
      AEngine.EnumerateApplications(
        procedure (AName: string; AApplication: IMARSApplication)
        var
          LAppURL, LOpenAPIURL, LDocsURL: string;
          LStaticURLs: TStringList;
          LSwaggerUI, LIndex: Integer;
        begin
          LAppURL := JoinPath([LBaseURL, AEngine.BasePath, AApplication.BasePath]);
          LOpenAPIURL := '';
          LDocsURL := '';
          LStaticURLs := TStringList.Create;
          try
            AApplication.EnumerateResources(
              procedure (AResourceName: string; AInfo: TMARSConstructorInfo)
              var
                LMethod: TRttiMethod;
                LHttpMethod: HttpMethodAttribute;
                LPath: PathAttribute;
                LResourcePath: string;
              begin
                // "www/{*}": the folder
                LResourcePath := AInfo.Path.Replace(TMARSURL.PATH_PARAM_WILDCARD, '')
                  .TrimRight([TMARSURL.URL_PATH_SEPARATOR]);

                if AInfo.TypeTClass.InheritsFrom(TFileSystemResource) then
                begin
                  // path and folder; nothing to show when the folder is not there
                  if StaticContentFolder(AInfo.TypeTClass) <> '' then
                    LStaticURLs.AddPair(LResourcePath, StaticContentFolder(AInfo.TypeTClass));
                end
                else if InheritsFromClassNamed(AInfo.TypeTClass, DOCS_CLASS_NAME) then
                begin
                  if LDocsURL = '' then
                    LDocsURL := JoinPath([LAppURL, LResourcePath]);
                end
                else if LOpenAPIURL = '' then
                  for LMethod in AInfo.Methods do
                  begin
                    LHttpMethod := LMethod.GetAttribute<HttpMethodAttribute>;
                    if Assigned(LMethod.ReturnType) and LMethod.ReturnType.Name.Equals(OPENAPI_TYPE_NAME)
                      and Assigned(LHttpMethod) and (LHttpMethod.HttpMethodName = 'GET') then
                    begin
                      LOpenAPIURL := JoinPath([LAppURL, AInfo.Path]);
                      LPath := LMethod.GetAttribute<PathAttribute>;
                      if Assigned(LPath) then
                        LOpenAPIURL := JoinPath([LOpenAPIURL, LPath.Value]);
                      Break;
                    end;
                  end;
              end
            );

            LLines.Add('- ' + AApplication.Name + ': ' + LAppURL);
            if LOpenAPIURL <> '' then
              LLines.Add('  - OpenAPI: ' + LOpenAPIURL);
            // Swagger UI only where it can work: its page in the folder, and a document to show
            LSwaggerUI := LStaticURLs.IndexOfName(SWAGGER_UI_PATH);
            if (LSwaggerUI <> -1) and ((LOpenAPIURL = '')
              or not TFile.Exists(LStaticURLs.ValueFromIndex[LSwaggerUI] + SWAGGER_UI_PAGE)) then
              LSwaggerUI := -1;
            if LSwaggerUI <> -1 then
              LLines.Add('  - Swagger UI: ' + JoinPath([LAppURL, SWAGGER_UI_PATH, SWAGGER_UI_PAGE])
                + '?openAPIURL=' + TURLEncoding.URL.Encode(LOpenAPIURL));
            if LDocsURL <> '' then
              LLines.Add('  - API docs: ' + LDocsURL);
            for LIndex := 0 to LStaticURLs.Count - 1 do
              if LIndex <> LSwaggerUI then
                LLines.Add('  - Static content: ' + JoinPath([LAppURL, LStaticURLs.Names[LIndex]])
                  + TMARSURL.URL_PATH_SEPARATOR);
          finally
            LStaticURLs.Free;
          end;
        end
      );
    Result := LLines.Text.TrimRight;
  finally
    LLines.Free;
  end;
end;

function MARSEndpointsAsText(const AEngine: IMARSEngine): string;
var
  LLines: TStringList;
  LCount: Integer;
  LHeader: string;
begin
  LLines := TStringList.Create;
  try
    LCount := 0;

    LHeader := AEngine.Name + '  ' + AEngine.BasePath;
    if AEngine.Port <> 0 then
      LHeader := LHeader + Format('  (port %d)', [AEngine.Port]);
    if AEngine.PortSSL <> 0 then
      LHeader := LHeader + Format('  (SSL port %d)', [AEngine.PortSSL]);
    LLines.Add(LHeader);

    AEngine.EnumerateApplications(
      procedure (AName: string; AApplication: IMARSApplication)
      var
        // resources and groups of routes in the order they come, the endpoints of each one sorted
        LGroups: TObjectList<TStringList>;
        LGroupNames: TStringList;
        LBasePath: string;
        LGroup: TStringList;
        LEndpoint: string;
        LIndex: Integer;
      begin
        LBasePath := TMARSURL.URL_PATH_SEPARATOR + JoinPath([AEngine.BasePath, AApplication.BasePath]);
        LLines.Add('  ' + AApplication.Name + '  ' + LBasePath);

        LGroupNames := TStringList.Create;
        LGroups := TObjectList<TStringList>.Create(True);
        try
          AApplication.EnumerateEndpoints(
            procedure (AResourceName: string; AInfo: TMARSConstructorInfo; APath: string; AHttpMethod: string)
            var
              LGroupIndex: Integer;
            begin
              LGroupIndex := LGroupNames.IndexOf(AInfo.TypeTClass.ClassName);
              if LGroupIndex = -1 then
              begin
                LGroupIndex := LGroupNames.Add(AInfo.TypeTClass.ClassName);
                LGroups.Add(TStringList.Create);
              end;
              LGroups[LGroupIndex].Add(TMARSURL.URL_PATH_SEPARATOR + JoinPath([LBasePath, APath]) + #9 + AHttpMethod.ToUpper);
            end
          );
          TMARSRouteTable.EnumerateRoutes(AApplication,
            procedure (AGroupName, ARoutePath, AHttpMethod: string)
            var
              LGroupIndex: Integer;
            begin
              LGroupIndex := LGroupNames.IndexOf(AGroupName);
              if LGroupIndex = -1 then
              begin
                LGroupIndex := LGroupNames.Add(AGroupName);
                LGroups.Add(TStringList.Create);
              end;
              LGroups[LGroupIndex].Add(TMARSURL.URL_PATH_SEPARATOR + JoinPath([LBasePath, ARoutePath]) + #9 + AHttpMethod.ToUpper);
            end
          );

          for LIndex := 0 to LGroupNames.Count - 1 do
          begin
            LLines.Add('    ' + LGroupNames[LIndex]);
            LGroup := LGroups[LIndex];
            LGroup.Sort; // by path, then by method
            for LEndpoint in LGroup do
            begin
              LLines.Add('      ' + LEndpoint.Split([#9])[1].PadRight(7) + ' ' + LEndpoint.Split([#9])[0]);
              Inc(LCount);
            end;
          end;
        finally
          LGroups.Free;
          LGroupNames.Free;
        end;
      end
    );

    if LCount = 1 then
      LLines.Add('1 endpoint')
    else
      LLines.Add(Format('%d endpoints', [LCount]));
    Result := LLines.Text.TrimRight;
  finally
    LLines.Free;
  end;
end;

end.
