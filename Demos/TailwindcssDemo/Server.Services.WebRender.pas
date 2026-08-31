(*
  Copyright 2026, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Services.WebRender;

interface

uses
  System.SysUtils, System.Generics.Collections, System.IOUtils,
  Web.Stencils,
  Server.Web.Models;

type
  TWebRenderService = class
  private
    class var FEngine: TWebStencilsEngine;
    class function TemplatesRoot: string; static;
    class function EnsureEngine: TWebStencilsEngine; static;
  public
    class function Render(
      const ARelativeTemplate: string;
      APage: TWebPageInfo;
      ASession: TWebSessionInfo = nil;
      AExtra: TProc<TWebStencilsProcessor> = nil
    ): string; static;
    class procedure ReleaseEngine; static;
  end;

implementation

class function TWebRenderService.TemplatesRoot: string;
begin
   Result := TPath.GetFullPath(
    TPath.Combine(
      [ExtractFilePath(ParamStr(0)), '..', 'templates']
    )
  );
end;

class function TWebRenderService.EnsureEngine: TWebStencilsEngine;
begin
  if FEngine = nil then
  begin
    FEngine := TWebStencilsEngine.Create(nil);
    FEngine.RootDirectory := TemplatesRoot;
    FEngine.DefaultFileExt := '.html';
  end;
  Result := FEngine;
end;

class function TWebRenderService.Render(
  const ARelativeTemplate: string;
  APage: TWebPageInfo;
  ASession: TWebSessionInfo;
  AExtra: TProc<TWebStencilsProcessor>
): string;
var
  LProc: TWebStencilsProcessor;
begin
  LProc := TWebStencilsProcessor.Create(nil);
  try
    LProc.Engine := EnsureEngine;
    LProc.InputFilename := TPath.Combine(TemplatesRoot, ARelativeTemplate);

    if APage <> nil then
    begin
      if APage.BasePath = '' then
        APage.BasePath := '/rest/default';
      LProc.AddVar('page', APage, False);
    end;

    if ASession <> nil then
      LProc.AddVar('session', ASession, False);

    if Assigned(AExtra) then
      AExtra(LProc);

    Result := LProc.Content;
  finally
    LProc.Free;
  end;
end;

class procedure TWebRenderService.ReleaseEngine;
begin
  FreeAndNil(FEngine);
end;

end.
