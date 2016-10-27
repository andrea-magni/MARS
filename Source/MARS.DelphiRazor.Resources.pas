(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.DelphiRazor.Resources;

interface

uses
  SysUtils, Classes, Generics.Collections

  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.URL
  , MARS.Core.Response
  , MARS.Core.Token
  , MARS.Core.Classes
  , MARS.Core.Engine
  , MARS.Core.Application


  , Web.HttpApp
  , RlxRazor
  ;
type
  TContextEntry=record
    Name: string;
    Instance: TObject;
    Owned: Boolean;
    constructor Create(AName: string; AInstance: TObject; AOwned: Boolean = True);
  end;

  TRazorResource = class
  private
    FRazorEngine: TRlxRazorEngine;
  protected
    [Context] URL: TMARSURL;
    [Context] Token: TMARSToken;
    [Context] ARequest: TWebRequest;

    function GetTemplatesFolder: string; virtual;
    function GetFilesFolder: string; virtual;
    procedure OnObjectForPathHandler(Sender: TObject; ExecData: TRazorExecData); virtual;
    function DoProvideContext(const APathInfo, APathParam: string): TArray<TContextEntry>; virtual;

    property RazorEngine: TRlxRazorEngine read FRazorEngine;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    [GET, Path('/{*}')]
    function GetDocument():string; virtual;
  end;



implementation

uses
    IOUtils, DateUtils
  , MARS.Core.Registry
  , MARS.Core.Exceptions
  , MARS.Core.Utils
;

{ TRazorResource }

constructor TRazorResource.Create;
begin
  inherited Create;
  FRazorEngine := TRlxRazorEngine.Create(nil);
  try
    FRazorEngine.TemplatesFolder := GetTemplatesFolder;
    FRazorEngine.FilesFolder := GetFilesFolder;
    FRazorEngine.HomePage := 'index';
    FRazorEngine.OnObjectForPath := OnObjectForPathHandler;

    FRazorEngine.AddToDictionary('resource', Self, False);
  except
    FRazorEngine.Free;
    raise;
  end;
end;

destructor TRazorResource.Destroy;
begin
  FRazorEngine.Free;
  inherited;
end;

function TRazorResource.DoProvideContext(const APathInfo,
  APathParam: string): TArray<TContextEntry>;
begin
  Result := [];
end;

function TRazorResource.GetDocument: string;
var
  LFound: Boolean;
begin
  RazorEngine.BasePath := URL.BasePath + URL.Resource;

  Result := RazorEngine.ProcessRequest(
      ARequest
    , LFound
    , Token.IsVerified
    , Token.Claims['LanguageID'].AsInteger
    , ''
    , StringArrayToString(Token.Roles)
  );
end;

function TRazorResource.GetFilesFolder: string;
begin
  Result := IncludeTrailingPathDelimiter(
    TPath.Combine(ExtractFilePath(ParamStr(0)), 'files')
  );
end;

function TRazorResource.GetTemplatesFolder: string;
begin
  Result := IncludeTrailingPathDelimiter(
    TPath.Combine(ExtractFilePath(ParamStr(0)), 'templates')
  );
end;

procedure TRazorResource.OnObjectForPathHandler(Sender: TObject;
  ExecData: TRazorExecData);
var
  LContext: TArray<TContextEntry>;
  LEntry: TContextEntry;
begin
  LContext := DoProvideContext(ExecData.PathInfo, ExecData.PathParam);

  for LEntry in LContext do
    ExecData.razorProc.AddToDictionary(LEntry.Name, LEntry.Instance, LEntry.Owned);
end;

{ TContextEntry }

constructor TContextEntry.Create(AName: string; AInstance: TObject;
  AOwned: Boolean);
begin
  Name := AName;
  Instance := AInstance;
  Owned := AOwned;
end;

end.
