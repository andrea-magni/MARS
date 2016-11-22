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
  RazorAttribute = class(MARSAttribute);


  RazorSingleValueAttribute = class(RazorAttribute)
  private
    FValue: string;
  public
    constructor Create(AValue: string); virtual;
    property Value: string read FValue;
  end;

  RazorHomePageAttribute = class(RazorSingleValueAttribute);
  RazorErrorPageAttribute = class(RazorSingleValueAttribute);
  RazorFilesFolderAttribute = class(RazorSingleValueAttribute);
  RazorTemplatesFolderAttribute = class(RazorSingleValueAttribute);


  TContextEntry=record
    Name: string;
    Instance: TObject;
    Owned: Boolean;
    InitFunction: TInitFunction;
    constructor Create(AName: string; AInstance: TObject; AOwned: Boolean = True); overload;
    constructor Create(AName: string; AInitFunction: TInitFunction); overload;
  end;

  TRazorResource = class
  private
    FRazorEngine: TRlxRazorEngine;
  protected
    [Context] URL: TMARSURL;
    [Context] Token: TMARSToken;
    [Context] Request: TWebRequest;
    [Context] Response: TWebResponse;

    function GetRazorAttributeValue<T: RazorSingleValueAttribute>(const AInstance: TObject; const ADefault: string = ''): string;
    function ContentTypeFromFileExt(const AFileExt: string): string; virtual;

    function GetHomePage: string; virtual;
    function GetErrorPage: string; virtual;
    function GetFilesFolder: string; virtual;
    function GetTemplatesFolder: string; virtual;

    procedure OnObjectForPathHandler(Sender: TObject; ExecData: TRazorExecData); virtual;
    procedure OnPageErrorHandler(Sender: TObject; pageInfo: TPageInfo); virtual;

    function DoProvideContext(const APathInfo, APathParam: string): TArray<TContextEntry>; virtual;
    procedure DoError(APageInfo: TPageInfo); virtual;
    procedure SetResponseContentType(const APathInfo, APathParam, AInputFileName: string); virtual;

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
  , MARS.Rtti.Utils
;

{ TRazorResource }

function TRazorResource.ContentTypeFromFileExt(const AFileExt: string): string;
begin
  if SameText(AFileExt, '.htm') or  SameText(AFileExt, '.html') then
    Result := 'text/html'
  else if SameText(AFileExt, '.js') then
    Result := 'application/javascript'
  else if SameText(AFileExt, '.json') then
    Result := 'application/json'
  else if SameText(AFileExt, '.xml') then
    Result := 'application/xml'
  else if SameText(AFileExt, '.csv') then
    Result := 'text/csv'
  else if SameText(AFileExt, '.css') then
    Result := 'text/css'
  else if SameText(AFileExt, '.jpg') or SameText(AFileExt, '.jpeg') then
    Result := 'image/jpeg'
  else if SameText(AFileExt, '.png') then
    Result := 'image/png'
  else if SameText(AFileExt, '.gif') then
    Result := 'image/gif'
  else
    Result := '';
end;

constructor TRazorResource.Create;
begin
  inherited Create;
  FRazorEngine := TRlxRazorEngine.Create(nil);
  try
    FRazorEngine.TemplatesFolder := GetTemplatesFolder;
    FRazorEngine.FilesFolder := GetFilesFolder;
    FRazorEngine.HomePage := GetHomePage;
    FRazorEngine.ErrorPage := GetErrorPage;

    FRazorEngine.OnObjectForPath := OnObjectForPathHandler;
    FRazorEngine.OnPageError := OnPageErrorHandler;
{ TODO -oandrea : Implementare OnLang e OnValue }
{
    FRazorEngine.OnValue
    FRazorEngine.OnLang
}
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

procedure TRazorResource.DoError(APageInfo: TPageInfo);
begin
end;

function TRazorResource.DoProvideContext(const APathInfo,
  APathParam: string): TArray<TContextEntry>;
begin
  Result := [TContextEntry.Create('resource', Self, False)];
end;

function TRazorResource.GetDocument: string;
var
  LFound: Boolean;
begin
  RazorEngine.BasePath := URL.BasePath + URL.Resource;

  Result := RazorEngine.ProcessRequest(
      Request
    , LFound
    , Token.IsVerified
    , Token.Claims['LanguageID'].AsInteger
    , ''
    , StringArrayToString(Token.Roles)
  );
end;

function TRazorResource.GetErrorPage: string;
begin
  Result := GetRazorAttributeValue<RazorErrorPageAttribute>(Self, 'error.html');
end;

function TRazorResource.GetFilesFolder: string;
begin
  Result := GetRazorAttributeValue<RazorFilesFolderAttribute>(Self,
    IncludeTrailingPathDelimiter(
      TPath.Combine(ExtractFilePath(ParamStr(0)), 'files')
    )
  );
end;

function TRazorResource.GetHomePage: string;
begin
  Result := GetRazorAttributeValue<RazorHomePageAttribute>(Self, 'index');
end;

function TRazorResource.GetRazorAttributeValue<T>(const AInstance: TObject;
  const ADefault: string): string;
var
  LValue: string;
begin
  LValue := ADefault;
  TRttiHelper.IfHasAttribute<T>(Self
    , procedure (AAttrib: T)
      begin
        LValue := AAttrib.Value;
      end
  );
  Result := LValue;
end;

function TRazorResource.GetTemplatesFolder: string;
begin
  Result := GetRazorAttributeValue<RazorTemplatesFolderAttribute>(Self,
    IncludeTrailingPathDelimiter(
      TPath.Combine(ExtractFilePath(ParamStr(0)), 'templates')
    )
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

  SetResponseContentType(ExecData.PathInfo, ExecData.PathParam, ExecData.razorProc.InputFilename);
end;

procedure TRazorResource.OnPageErrorHandler(Sender: TObject;
  pageInfo: TPageInfo);
begin
  DoError(pageInfo);
end;

procedure TRazorResource.SetResponseContentType(const APathInfo, APathParam,
  AInputFileName: string);
begin
  Response.ContentType := ContentTypeFromFileExt(ExtractFileExt(AInputFilename));
end;

{ TContextEntry }

constructor TContextEntry.Create(AName: string; AInstance: TObject;
  AOwned: Boolean);
begin
  Name := AName;
  Instance := AInstance;
  InitFunction := nil;
  Owned := AOwned;
end;

constructor TContextEntry.Create(AName: string; AInitFunction: TInitFunction);
begin
  Name := AName;
  Instance := nil;
  InitFunction := AInitFunction;
  Owned := true;
end;

{ RazorSingleValueAttribute }

constructor RazorSingleValueAttribute.Create(AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

end.
