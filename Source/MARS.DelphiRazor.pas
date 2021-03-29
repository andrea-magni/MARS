(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.DelphiRazor;

{$I MARS.inc}

interface

uses
    System.Classes, System.SysUtils, Rtti

  , MARS.Core.Application
  , MARS.Core.Attributes
  , MARS.Core.Classes
  , MARS.Core.Declarations
  , MARS.Core.JSON
  , MARS.Core.MediaType
  , MARS.Core.Registry
  , MARS.Core.Token
  , MARS.Core.URL
  , MARS.Utils.Parameters
  , MARS.Core.Activation.Interfaces

  , RlxRazorMARS
;

type
  RazorAttribute = class(MARSAttribute);

  RazorEngineAttribute = class(MARSAttribute)
  private
    FName: string;
  public
    constructor Create(AName: string);
    property Name: string read FName;
  end;

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

  RazorTranslateAttribute = class(RazorAttribute);

  TOnLangProc = reference to procedure (const AFieldName: string; var AReplaceText: string);
  TOnValueProc = reference to procedure (const ObjectName: string; const FieldName: string; var ReplaceText: string);
  TOnObjectForPathProc = reference to procedure (ExecData: TRazorExecData);
  TOnScaffoldingProc = reference to procedure (AQualifClassName: string; var AReplaceText: string);

  TMARSDelphiRazor = class
  private
    FActivation: IMARSActivation;
    FRazorEngine: TRlxRazorEngine;
    FRazorProc: TRlxRazorProcessor;
    FName: string;
    FApplication: TMARSApplication;
    FParameters: TMARSParameters;
    FURL: TMARSURL;
    FToken: TMARSToken;
    FOnLangProc: TOnLangProc;
    FOnValueProc: TOnValueProc;
    FOnObjectForPath: TOnObjectForPathProc;
    FOnScaffolding: TOnScaffoldingProc;
  protected
    function GetRazorAttributeValue<T: RazorSingleValueAttribute>(
      const AType: TRttiType; const ADefault: string = ''): string;
    function GetRazorEngine(const AName: string): TRlxRazorEngine; overload; virtual;
    function GetRazorEngine: TRlxRazorEngine; overload; virtual;

    function GetBasePath: string; virtual;
    function GetHomePage: string; virtual;
    function GetErrorPage: string; virtual;
    function GetFilesFolder: string; virtual;
    function GetTemplatesFolder: string; virtual;
    function UserLoggedIn: boolean; virtual;
    function UserRoles: string; virtual;
    function UserLanguageID: Integer; virtual;


    procedure OnObjectForPathHandler(Sender: TObject; ExecData: TRazorExecData); virtual;
    procedure OnScaffoldingHandler(Sender: TObject; const qualifClassName: string;
      var ReplaceText: string); virtual;
    procedure OnPageErrorHandler(Sender: TObject; pageInfo: TPageInfo); virtual;
    procedure OnLangHandler(Sender: TObject; const FieldName: string; var ReplaceText: string); virtual;
    procedure OnValueHandler(Sender: TObject; const ObjectName: string;
      const FieldName: string; var ReplaceText: string); virtual;
  public
    constructor Create(const AName: string; const AActivation: IMARSActivation = nil); virtual;
    destructor Destroy; override;

    function ProcessRequest(const AErrorIfNotFound: Boolean = True): string; virtual;
    function DoBlock(const AContent: string; const AEncoding: TEncoding = nil): string; overload; virtual;

    property Activation: IMARSActivation read FActivation;
    property Application: TMARSApplication read FApplication;
    property URL: TMARSURL read FURL;
    property Token: TMARSToken read FToken;
    property Parameters: TMARSParameters read FParameters;
    property RazorEngine: TRlxRazorEngine read GetRazorEngine;
    property Name: string read FName;

    property OnObjectForPath: TOnObjectForPathProc read FOnObjectForPath write FOnObjectForPath;
    property OnLang: TOnLangProc read FOnLangProc write FOnLangProc;
    property OnValue: TOnValueProc read FOnValueProc write FOnValueProc;
    property OnScaffolding: TOnScaffoldingProc read FOnScaffolding write FOnScaffolding;
  end;

implementation

uses
  IOUTils
, MARS.Core.Utils
, MARS.Core.Exceptions
, MARS.Rtti.Utils
, MARS.DelphiRazor.InjectionService
, MARS.Core.RequestAndResponse.Interfaces, MARS.http.Server.Indy
;

  { TMARSDelphiRazor }

constructor TMARSDelphiRazor.Create(const AName: string; const AActivation: IMARSActivation);
var
  LDelphiRazorSlice: TMARSParameters;
begin
  inherited Create;
  FName := AName;
  FActivation := AActivation;

  // shortcuts
  FApplication := FActivation.Application;
  FURL := FActivation.URL;
  FToken := FActivation.Token;

  // DelphiRazor parameters
  FParameters := TMARSParameters.Create(AName);
  try
    if FApplication.Parameters.ContainsSlice('DelphiRazor') then
    begin
      LDelphiRazorSlice := TMARSParameters.Create('DelphiRazor');
      try
        LDelphiRazorSlice.CopyFrom(FApplication.Parameters, 'DelphiRazor');
        FParameters.CopyFrom(LDelphiRazorSlice, FName);
      finally
        LDelphiRazorSlice.Free;
      end;
    end;
  except
    FParameters.Free;
    raise;
  end;
end;

destructor TMARSDelphiRazor.Destroy;
begin
  FreeAndNil(FRazorProc);
  FreeAndNil(FParameters);
  FreeAndNil(FRazorEngine);
  inherited;
end;

function TMARSDelphiRazor.DoBlock(const AContent: string; const AEncoding: TEncoding): string;
var
  LRazorProc: TRlxRazorProcessor;
begin
//  if not Assigned(FRazorProc) then
//    FRazorProc := TRlxRazorProcessor.Create(nil);
//  LRazorProc := FRazorProc;
  LRazorProc := TRlxRazorProcessor.Create(nil);
  try
    LRazorProc.RazorEngine := RazorEngine;
//    LRazorProc.InputFilename := FilePath;
    LRazorProc.UserLoggedIn := UserLoggedIn;
    LRazorProc.UserRoles := UserRoles;
    LRazorProc.LanguageID := UserLanguageID;
    LRazorProc.OnLang := Self.OnLangHandler;
//    LRazorProc.AddToDictionary ('page', pageInfo, False);
//    execData.PathInfo := pageInfo.Page;
//    execData.PathParam := pageInfo.Item;
//    execData.LRazorProc := LRazorProc;
//    ConnectObjectForPath (execData);
//    LRazorProc.Request := Request; // passed manually
//    Result := LRazorProc.Content;
    Result := LRazorProc.DoBlock(RawByteString(AContent), AEncoding)
  finally
    LRazorProc.Free;
  end;
end;

function TMARSDelphiRazor.GetBasePath: string;
var
  LBasePath: string;
begin
  LBasePath := URL.BasePath + URL.Resource;

  Activation.Method.HasAttribute<PathAttribute>(
    procedure (AAttr: PathAttribute)
    begin
      if not (LBasePath.EndsWith('/') or AAttr.Value.StartsWith('/')) then
        LBasePath := LBasePath + '/';
      LBasePath := LBasePath + AAttr.Value;
    end
  );

  Result := LBasePath;
end;

function TMARSDelphiRazor.GetErrorPage: string;
begin
  Result := GetRazorAttributeValue<RazorErrorPageAttribute>(Activation.Resource
    // default
    , Parameters.ByName('ErrorPage', 'error.html').AsString
  );
end;

function TMARSDelphiRazor.GetFilesFolder: string;
begin
  Result := GetRazorAttributeValue<RazorFilesFolderAttribute>(Activation.Resource
    // default
    , Parameters.ByName('FilesFolder'
      , IncludeTrailingPathDelimiter(TPath.Combine(ExtractFilePath(ParamStr(0)), 'files'))
      ).AsString
  );
end;

function TMARSDelphiRazor.GetHomePage: string;
begin
  Result := GetRazorAttributeValue<RazorHomePageAttribute>(Activation.Resource
    // default
    , Parameters.ByName('HomePage', 'index').AsString
  );
end;

function TMARSDelphiRazor.GetRazorAttributeValue<T>(const AType: TRttiType;
  const ADefault: string): string;
var
  LValue: string;
begin
  LValue := ADefault;
  AType.HasAttribute<T>(
    procedure (AAttrib: T)
    begin
      LValue := AAttrib.Value;
    end
  );
  Result := LValue;
end;

function TMARSDelphiRazor.GetRazorEngine: TRlxRazorEngine;
begin
  if not Assigned(FRazorEngine) then
  begin
    FRazorEngine := GetRazorEngine(Name);
    if Assigned(FRazorEngine) then
    begin
      FRazorEngine.OnObjectForPath := OnObjectForPathHandler;
      FRazorEngine.OnScaffolding := OnScaffoldingHandler;
      FRazorEngine.OnPageError := OnPageErrorHandler;

      FRazorEngine.AddToDictionary('token', Token, False);
      FRazorEngine.AddToDictionary('resource', Activation.ResourceInstance, False);

      FRazorEngine.OnLang := OnLangHandler;
      FRazorEngine.OnValue := OnValueHandler;
    end;
  end;
  Result := FRazorEngine;
end;

function TMARSDelphiRazor.GetRazorEngine(const AName: string): TRlxRazorEngine;
begin
  Result := TRlxRazorEngine.Create(nil);
  try
    Result.Name := AName;
    Result.TemplatesFolder := GetTemplatesFolder;
    Result.FilesFolder := GetFilesFolder;
    Result.HomePage := GetHomePage;
    Result.ErrorPage := GetErrorPage;
    Result.BasePath :=  GetBasePath;
  except
    Result.Free;
    raise;
  end;
end;

function TMARSDelphiRazor.GetTemplatesFolder: string;
begin
  Result := GetRazorAttributeValue<RazorTemplatesFolderAttribute>(Activation.Resource
    // default
    , Parameters.ByName('TemplatesFolder'
      , IncludeTrailingPathDelimiter(TPath.Combine(ExtractFilePath(ParamStr(0)), 'templates'))
      ).AsString
  );
end;

procedure TMARSDelphiRazor.OnLangHandler(Sender: TObject;
  const FieldName: string; var ReplaceText: string);
begin
  if Assigned(FOnLangProc) then
    FOnLangProc(FieldName, ReplaceText);
end;

procedure TMARSDelphiRazor.OnObjectForPathHandler(Sender: TObject;
  ExecData: TRazorExecData);
begin
  if Assigned(FOnObjectForPath) then
    FOnObjectForPath(ExecData);
end;

procedure TMARSDelphiRazor.OnPageErrorHandler(Sender: TObject;
  pageInfo: TPageInfo);
begin

end;

procedure TMARSDelphiRazor.OnScaffoldingHandler(Sender: TObject;
  const qualifClassName: string; var ReplaceText: string);
begin
  if Assigned(FOnScaffolding) then
    FOnScaffolding(qualifClassName, ReplaceText);
end;

procedure TMARSDelphiRazor.OnValueHandler(Sender: TObject; const ObjectName: string;
    const FieldName: string; var ReplaceText: string);
begin
  if Assigned(FOnValueProc) then
    FOnValueProc(ObjectName, FieldName, ReplaceText);
end;

function TMARSDelphiRazor.ProcessRequest(const AErrorIfNotFound: Boolean = True): string;
var
  LFound: Boolean;
  LMARSWebRequest: TMARSWebRequest;
  LRequest: IMARSRequest;
  LResponse: IMARSResponse;
begin
  LFound := False;

  LRequest := Activation.Request;
  LMARSWebRequest := LRequest.AsObject as TMARSWebRequest;

  LResponse := Activation.Response;

  Result := RazorEngine.ProcessRequest(LMARSWebRequest.WebRequest
    , LFound
    , UserLoggedIn
    , UserLanguageID
    , ''
    , UserRoles
  );

  if LRequest.RawPath.EndsWith('.html', True) or LRequest.RawPath.EndsWith('.htm', True) then
    LResponse.ContentType := TMediaType.TEXT_HTML
  else if LRequest.RawPath.EndsWith('.js', True) then
    LResponse.ContentType := 'application/javascript'
  else
    LResponse.ContentType := '*/*';

  LResponse.Content := Result;

  if (not LFound) and AErrorIfNotFound then
    raise EMARSHttpException.Create('File not found', 404);
end;

function TMARSDelphiRazor.UserLanguageID: Integer;
begin
  Result := 1;
  if Assigned(Token) then
    Result := Token.Claims.ByName('LANGUAGE_ID', 1).AsInteger;
end;

function TMARSDelphiRazor.UserLoggedIn: boolean;
begin
  Result := Assigned(Token) and (Token.IsVerified and not Token.IsExpired);
end;

function TMARSDelphiRazor.UserRoles: string;
begin
  Result := string.join(',', Token.Roles);
end;

{ RazorSingleValueAttribute }

constructor RazorSingleValueAttribute.Create(AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

{ RazorEngineAttribute }

constructor RazorEngineAttribute.Create(AName: string);
begin
  inherited Create;
  FName := AName;
end;

end.
