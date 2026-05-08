unit MARS.Client.Client.Http;
(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)

{$I MARS.inc}

interface

uses
  SysUtils, Classes
  , MARS.Core.JSON, MARS.Client.Utils, MARS.Core.Utils, MARS.Client.Client,  MARS.Utils.Parameters

  // Net
  , System.Net.URLClient, System.Net.HttpClient
  , System.Net.Mime
;

type
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TMARSHttpClient = class(TMARSCustomClient)
  private
    FHttpClient: THTTPClient;
    FLastResponse: IHTTPResponse;

    // HttpClient wrappers
    function GetProtocolVersion: THTTPProtocolVersion; inline;
    function GetHandleRedirects: Boolean; inline;
    function GetMaxRedirects: Integer; inline;
    function GetOnRedirect: THTTPRedirectEvent; inline;
    function GetRedirectsWithGET: THTTPRedirectsWithGET; inline;
    function GetAllowCookies: Boolean; inline;
    function GetAutomaticDecompression: THTTPCompressionMethods; inline;
    function GetCookieManager: TCookieManager; inline;
    function GetNeedClientCertificateCallback: TNeedClientCertificateCallback; inline;
    function GetNeedClientCertificateEvent: TNeedClientCertificateEvent; inline;
    function GetOnReceiveData: TReceiveDataEvent; inline;
    function GetOnReceiveDataEx: TReceiveDataExEvent; inline;
    function GetOnSendData: TSendDataEvent; inline;
    function GetOnUpdateCookie: THTTPUpdateCookieEvent; inline;
    function GetPreemptiveAuthentication: Boolean; inline;
    function GetReceiveDataCallback: TReceiveDataCallback; inline;
    function GetReceiveDataExCallback: TReceiveDataExCallback; inline;
    function GetSecureFailureReasons: THTTPSecureFailureReasons; inline;
    function GetSecureProtocols: THTTPSecureProtocols; inline;
    function GetSendDataCallback: TSendDataCallback; inline;
    function GetUseDefaultCredentials: Boolean; inline;
    function GetValidateServerCertificateCallback: TValidateCertificateCallback; inline;
    function GetValidateServerCertificateEvent: TValidateCertificateEvent; inline;
    procedure SetAllowCookies(const Value: Boolean); inline;
    procedure SetAutomaticDecompression(const Value: THTTPCompressionMethods); inline;
    procedure SetCookieManager(const Value: TCookieManager); inline;
    procedure SetHandleRedirects(const Value: Boolean); inline;
    procedure SetMaxRedirects(const Value: Integer); inline;
    procedure SetOnRedirect(const Value: THTTPRedirectEvent); inline;
    procedure SetNeedClientCertificateEvent(const Value: TNeedClientCertificateEvent); inline;
    procedure SetOnReceiveData(const Value: TReceiveDataEvent); inline;
    procedure SetOnReceiveDataEx(const Value: TReceiveDataExEvent); inline;
    procedure SetOnSendData(const Value: TSendDataEvent); inline;
    procedure SetOnUpdateCookie(const Value: THTTPUpdateCookieEvent); inline;
    procedure SetPreemptiveAuthentication(const Value: Boolean); inline;
    procedure SetProtocolVersion(const Value: THTTPProtocolVersion); inline;
    procedure SetReceiveDataCallback(const Value: TReceiveDataCallback); inline;
    procedure SetReceiveDataExCallback(const Value: TReceiveDataExCallback); inline;
    procedure SetRedirectsWithGET(const Value: THTTPRedirectsWithGET); inline;
    procedure SetSecureProtocols(const Value: THTTPSecureProtocols); inline;
    procedure SetSendDataCallback(const Value: TSendDataCallback); inline;
    procedure SetUseDefaultCredentials(const Value: Boolean); inline;
    procedure SetValidateServerCertificateCallback(const Value: TValidateCertificateCallback); inline;
    procedure SetValidateServerCertificateEvent(const Value: TValidateCertificateEvent); inline;
    procedure SetNeedClientCertificateCallback(const Value: TNeedClientCertificateCallback); inline;

  protected
    procedure AssignTo(Dest: TPersistent); override;

    procedure CloneCookies(const ADestination, ASource: THTTPClient);

    function GetConnectTimeout: Integer; override;
    function GetReadTimeout: Integer; override;
    procedure SetConnectTimeout(const Value: Integer); override;
    procedure SetReadTimeout(const Value: Integer); override;

    function CreateMultipartFormData(AFormData: TArray<TFormParam>): TMultipartFormData;

    procedure EndorseAuthorization; override;
    procedure CheckLastCmdSuccess; virtual;
    procedure ApplyProxyConfig; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ApplyCustomHeaders(const AHeaders: TStrings); override;

    procedure Delete(const AURL: string; AContent, AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); override;
    procedure Get(const AURL: string; AResponseContent: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); override;

    procedure Patch(const AURL: string; AContent, AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); override;

    procedure Post(const AURL: string; AContent, AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); override;
    procedure Post(const AURL: string; const AFormData: TArray<TFormParam>;
      const AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); override;
    procedure Post(const AURL: string; const AFormUrlEncoded: TMARSParameters;
      const AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); override;

    procedure Put(const AURL: string; AContent, AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); override;
    procedure Put(const AURL: string; const AFormData: System.TArray<TFormParam>;
      const AResponse: TStream; const AAuthToken: string;
      const AAccept: string; const AContentType: string); override;

    function LastCmdSuccess: Boolean; override;
    function ResponseStatusCode: Integer; override;
    function ResponseText: string; override;
    property HttpClient: THTTPClient read FHttpClient;

    // HttpClient wrappers
    property NeedClientCertificateCallback: TNeedClientCertificateCallback read GetNeedClientCertificateCallback write SetNeedClientCertificateCallback;
    property ValidateServerCertificateCallback: TValidateCertificateCallback read GetValidateServerCertificateCallback write SetValidateServerCertificateCallback;
    property SendDataCallBack: TSendDataCallback read GetSendDataCallback write SetSendDataCallback;
    property ReceiveDataCallBack: TReceiveDataCallback read GetReceiveDataCallback write SetReceiveDataCallback;
    property ReceiveDataExCallback: TReceiveDataExCallback read GetReceiveDataExCallback write SetReceiveDataExCallback;
  published

    // HttpClient wrappers
    property ProtocolVersion: THTTPProtocolVersion read GetProtocolVersion write SetProtocolVersion
      default THTTPProtocolVersion.UNKNOWN_HTTP;
    property HandleRedirects: Boolean read GetHandleRedirects write SetHandleRedirects
      default True;
    property MaxRedirects: Integer read GetMaxRedirects write SetMaxRedirects
      default CHTTPDefMaxRedirects;
    property RedirectsWithGET: THTTPRedirectsWithGET read GetRedirectsWithGET write SetRedirectsWithGET
     default CHTTPDefRedirectsWithGET;
    property SecureProtocols: THTTPSecureProtocols read GetSecureProtocols write SetSecureProtocols
      default CHTTPDefSecureProtocols;
    property SecureFailureReasons: THTTPSecureFailureReasons read GetSecureFailureReasons;
    property PreemptiveAuthentication: Boolean read GetPreemptiveAuthentication write SetPreemptiveAuthentication
      default False;
    property UseDefaultCredentials: Boolean read GetUseDefaultCredentials write SetUseDefaultCredentials
      default True;
    property AllowCookies: Boolean read GetAllowCookies write SetAllowCookies
      default True;
    property CookieManager: TCookieManager read GetCookieManager write SetCookieManager;
    property AutomaticDecompression: THTTPCompressionMethods read GetAutomaticDecompression write SetAutomaticDecompression
      default [];

//    property Accept: string read GetAccept write SetAccept;
//    property AcceptCharSet: string read GetAcceptCharSet write SetAcceptCharSet;
//    property AcceptEncoding: string read GetAcceptEncoding write SetAcceptEncoding;
//    property AcceptLanguage: string read GetAcceptLanguage write SetAcceptLanguage;
//    property ContentType: string read GetContentType write SetContentType;

    property OnRedirect: THTTPRedirectEvent read GetOnRedirect write SetOnRedirect;
    property OnSendData: TSendDataEvent read GetOnSendData write SetOnSendData;
    property OnReceiveData: TReceiveDataEvent read GetOnReceiveData write SetOnReceiveData;
    property OnReceiveDataEx: TReceiveDataExEvent read GetOnReceiveDataEx write SetOnReceiveDataEx;
    property OnNeedClientCertificate: TNeedClientCertificateEvent read GetNeedClientCertificateEvent write SetNeedClientCertificateEvent;
    property OnValidateServerCertificate: TValidateCertificateEvent read GetValidateServerCertificateEvent write SetValidateServerCertificateEvent;
    property OnUpdateCookie: THTTPUpdateCookieEvent read GetOnUpdateCookie write SetOnUpdateCookie;

//    // from TURLClient
//    property ConnectionTimeout: Integer read FConnectionTimeout write SetConnectionTimeout;
//    property SendTimeout: Integer read FSendTimeout write SetSendTimeout;
//    property ResponseTimeout: Integer read FResponseTimeout write SetResponseTimeout;
//    property UserAgent: string read GetUserAgent write SetUserAgent;
//    property AuthCallback: TCredentialsStorage.TCredentialAuthCallback read FAuthCallback write FAuthCallback;
//    property AuthEvent: TCredentialsStorage.TCredentialAuthEvent read FAuthEvent write FAuthEvent;
//    property CredentialsStorage: TCredentialsStorage read FCredentialsStorage write SetCredentialsStorage;
//    property ProxySettings: TProxySettings read FProxySettings write SetProxySettings;
//    property CustomHeaders[const AName: string]: string read GetCustomHeaderValue write SetCustomHeaderValue;
//    property CustHeaders: TURLHeaders read FCustomHeaders;

//    // from TMARSCustomClient
//    property ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout;
//    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
//    property OnError: TMARSClientErrorEvent read FOnError write FOnError;
//    property AuthEndorsement: TMARSAuthEndorsement read FAuthEndorsement write SetAuthEndorsement default TMARSAuthEndorsement.Cookie;
//    property AuthCookieName: string read FAuthCookieName write FAuthCookieName;
//    property ProxyConfig: TMARSProxyConfig read FProxyConfig write SetProxyConfig;


  end;

implementation

uses
    Rtti, TypInfo
  , MARS.Client.CustomResource
  , MARS.Client.Resource
  , MARS.Client.Resource.JSON
  , MARS.Client.Resource.Stream
  , MARS.Client.Application
;

{ TMARSHttpClient }

procedure TMARSHttpClient.ApplyCustomHeaders(const AHeaders: TStrings);
var
  LIndex: Integer;
begin
  inherited;
  for LIndex := 0 to AHeaders.Count-1 do
    HttpClient.CustomHeaders[AHeaders.Names[LIndex]] := AHeaders.ValueFromIndex[LIndex];
end;

procedure TMARSHttpClient.ApplyProxyConfig;
begin
  inherited;
  if not Assigned(FHttpClient) then
    Exit;

  if ProxyConfig.Enabled then
  FHttpClient.ProxySettings := TProxySettings.Create(
    ProxyConfig.Host, ProxyConfig.Port, ProxyConfig.UserName, ProxyConfig.Password
  )
  else
    FHttpClient.ProxySettings := TProxySettings.Create('', 0);
end;

procedure TMARSHttpClient.AssignTo(Dest: TPersistent);
//var
//  LDestClient: TMARSHttpClient;
begin
  inherited;
//  if Dest is TMARSHttpClient then
//  begin
//    LDestClient := Dest as TMARSHttpClient;
//
//    CloneCookies(LDestClient.HttpClient, HttpClient);
//  end;
end;

procedure TMARSHttpClient.CheckLastCmdSuccess;
begin
  if not Assigned(FLastResponse) then
    Exit;

  if not LastCmdSuccess then
    raise EMARSClientHttpException.Create(FLastResponse.StatusText, FLastResponse.StatusCode,
      FLastResponse.ContentStream, FLastResponse.MimeType);
end;

procedure TMARSHttpClient.CloneCookies(const ADestination,
  ASource: THttpClient);
var
  LIndex: Integer;
  LCookie: TCookie;
  LURI: TURI;
begin
  for LIndex := 0 to Length(ASource.CookieManager.Cookies)-1 do
  begin
    LCookie := ASource.CookieManager.Cookies[LIndex];
    LURI := Default(TURI);
    LURI.Host := LCookie.Domain;
    LURI.Path := LCookie.Path;
    ADestination.CookieManager.AddServerCookie(LCookie, LURI);
  end;
end;

constructor TMARSHttpClient.Create(AOwner: TComponent);
begin
  inherited;
  FHttpClient := THttpClient.Create;
end;

procedure TMARSHttpClient.Delete(const AURL: string; AContent, AResponse: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  inherited;
  FHttpClient.Accept := AAccept;
  FHttpClient.ContentType := AContentType;
  FLastResponse := FHttpClient.Delete(AURL, AResponse);
  CheckLastCmdSuccess;
end;

destructor TMARSHttpClient.Destroy;
begin
  FHttpClient.Free;
  inherited;
end;

procedure TMARSHttpClient.EndorseAuthorization;
var
  LCookie: TCookie;
  LFound: Boolean;
begin
  if AuthEndorsement = AuthorizationBearer then
  begin
    if not (AuthToken = '') then
      FHttpClient.CustomHeaders['Authorization'] := 'Bearer ' + AuthToken
    else
      FHttpClient.CustomHeaders['Authorization'] := '';
  end
  else if AuthEndorsement = Cookie then
  begin
    if not (AuthToken = '') then
    begin
      LFound := False;
      for LCookie in FHttpClient.CookieManager.Cookies do
      begin
        if SameText(LCookie.Name, AuthCookieName) then
        begin
          LFound := True;
          Break;
        end;
      end;

      if not LFound then
        FHttpClient.CookieManager.AddServerCookie(AuthCookieName + '=' + AuthToken, MARSEngineURL);
    end;
  end;
end;

function TMARSHttpClient.CreateMultipartFormData(
  AFormData: TArray<TFormParam>): TMultipartFormData;
var
  LFormParam: TFormParam;
begin
  Result := TMultipartFormData.Create();
  try
    for LFormParam in AFormData do
    begin
      if not LFormParam.IsFile then
        Result.AddField(LFormParam.FieldName, LFormParam.Value.ToString)
      else
      begin
        //TODO AM: save bytes to file and use TempFileName
        Result.AddFile(LFormParam.AsFile.FieldName, LFormParam.AsFile.FileName);
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TMARSHttpClient.Get(const AURL: string; AResponseContent: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  FHttpClient.Accept := AAccept;
  FHttpClient.ContentType := AContentType;
  inherited;
  FLastResponse := FHttpClient.Get(AURL, AResponseContent);
  CheckLastCmdSuccess;
end;

function TMARSHttpClient.GetAllowCookies: Boolean;
begin
  Result := FHttpClient.AllowCookies;
end;

function TMARSHttpClient.GetAutomaticDecompression: THTTPCompressionMethods;
begin
  Result := FHttpClient.AutomaticDecompression;
end;

function TMARSHttpClient.GetConnectTimeout: Integer;
begin
  Result := FHttpClient.ConnectionTimeout;
end;

function TMARSHttpClient.GetCookieManager: TCookieManager;
begin
  Result := FHttpClient.CookieManager;
end;

function TMARSHttpClient.GetHandleRedirects: Boolean;
begin
  Result := FHttpClient.HandleRedirects;
end;

function TMARSHttpClient.GetMaxRedirects: Integer;
begin
  Result := FHttpClient.MaxRedirects;
end;

function TMARSHttpClient.GetNeedClientCertificateCallback: TNeedClientCertificateCallback;
begin
  Result := FHttpClient.NeedClientCertificateCallback;
end;

function TMARSHttpClient.GetNeedClientCertificateEvent: TNeedClientCertificateEvent;
begin
  Result := FHttpClient.OnNeedClientCertificate;
end;

function TMARSHttpClient.GetOnReceiveData: TReceiveDataEvent;
begin
  Result := FHttpClient.OnReceiveData;
end;

function TMARSHttpClient.GetOnReceiveDataEx: TReceiveDataExEvent;
begin
  Result := FHttpClient.OnReceiveDataEx;
end;

function TMARSHttpClient.GetOnRedirect: THTTPRedirectEvent;
begin
  Result := FHttpClient.OnRedirect;
end;

function TMARSHttpClient.GetOnSendData: TSendDataEvent;
begin
  Result := FHttpClient.OnSendData;
end;

function TMARSHttpClient.GetOnUpdateCookie: THTTPUpdateCookieEvent;
begin
  Result := FHttpClient.OnUpdateCookie;
end;

function TMARSHttpClient.GetPreemptiveAuthentication: Boolean;
begin
  Result := FHttpClient.PreemptiveAuthentication;
end;

function TMARSHttpClient.GetProtocolVersion: THTTPProtocolVersion;
begin
  Result := FHttpClient.ProtocolVersion;
end;

function TMARSHttpClient.GetReadTimeout: Integer;
begin
  {$ifdef Delphi10Berlin_UP}
    Result := FHttpClient.ResponseTimeout;
  {$else}
    Result := -1;
  {$endif}
end;

function TMARSHttpClient.GetReceiveDataCallback: TReceiveDataCallback;
begin
  Result := FHttpClient.ReceiveDataCallBack;
end;

function TMARSHttpClient.GetReceiveDataExCallback: TReceiveDataExCallback;
begin
  Result := FHttpClient.ReceiveDataExCallback;
end;

function TMARSHttpClient.GetRedirectsWithGET: THTTPRedirectsWithGET;
begin
  Result := FHttpClient.RedirectsWithGET;
end;

function TMARSHttpClient.GetSecureFailureReasons: THTTPSecureFailureReasons;
begin
  Result := FHttpClient.SecureFailureReasons;
end;

function TMARSHttpClient.GetSecureProtocols: THTTPSecureProtocols;
begin
  Result := FHttpClient.SecureProtocols;
end;

function TMARSHttpClient.GetSendDataCallback: TSendDataCallback;
begin
  Result := FHttpClient.SendDataCallBack;
end;

function TMARSHttpClient.GetUseDefaultCredentials: Boolean;
begin
  Result := FHttpClient.UseDefaultCredentials;
end;

function TMARSHttpClient.GetValidateServerCertificateCallback: TValidateCertificateCallback;
begin
  Result := FHttpClient.ValidateServerCertificateCallback;
end;

function TMARSHttpClient.GetValidateServerCertificateEvent: TValidateCertificateEvent;
begin
  Result := FHttpClient.OnValidateServerCertificate;
end;

function TMARSHttpClient.LastCmdSuccess: Boolean;
begin
  Result := Assigned(FLastResponse)
    and ((FLastResponse.StatusCode >= 200) and (FLastResponse.StatusCode < 300));
end;

procedure TMARSHttpClient.Post(const AURL: string; AContent, AResponse: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  inherited;
  FHttpClient.Accept := AAccept;
  FHttpClient.ContentType := AContentType;
  if Assigned(AContent) then
    AContent.Position := 0;
  FLastResponse := FHttpClient.Post(AURL, AContent, AResponse);
  CheckLastCmdSuccess;
end;

procedure TMARSHttpClient.Put(const AURL: string; AContent, AResponse: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  inherited;
  FHttpClient.Accept := AAccept;
  FHttpClient.ContentType := AContentType;
  if Assigned(AContent) then
    AContent.Position := 0;
  FLastResponse := FHttpClient.Put(AURL, AContent, AResponse);
  CheckLastCmdSuccess;
end;

function TMARSHttpClient.ResponseStatusCode: Integer;
begin
  Result := -1;
  if Assigned(FLastResponse) then
    Result := FLastResponse.StatusCode;
end;

function TMARSHttpClient.ResponseText: string;
begin
  Result := '';
  if Assigned(FLastResponse) then
    Result := FLastResponse.StatusText;
end;

procedure TMARSHttpClient.SetAllowCookies(const Value: Boolean);
begin
  FHttpClient.AllowCookies := Value;
end;

procedure TMARSHttpClient.SetAutomaticDecompression(
  const Value: THTTPCompressionMethods);
begin
  FHttpClient.AutomaticDecompression := Value;
end;

procedure TMARSHttpClient.SetConnectTimeout(const Value: Integer);
begin
  FHttpClient.ConnectionTimeout := Value;
end;

procedure TMARSHttpClient.SetCookieManager(const Value: TCookieManager);
begin
  FHttpClient.CookieManager := Value;
end;

procedure TMARSHttpClient.SetHandleRedirects(const Value: Boolean);
begin
  FHttpClient.HandleRedirects := Value;
end;

procedure TMARSHttpClient.SetMaxRedirects(const Value: Integer);
begin
  FHttpClient.MaxRedirects := Value;
end;

procedure TMARSHttpClient.SetNeedClientCertificateCallback(
  const Value: TNeedClientCertificateCallback);
begin
  FHttpClient.NeedClientCertificateCallback := Value;
end;

procedure TMARSHttpClient.SetNeedClientCertificateEvent(
  const Value: TNeedClientCertificateEvent);
begin
  FHttpClient.OnNeedClientCertificate := Value;
end;

procedure TMARSHttpClient.SetOnReceiveData(const Value: TReceiveDataEvent);
begin
  FHttpClient.OnReceiveData := Value;
end;

procedure TMARSHttpClient.SetOnReceiveDataEx(const Value: TReceiveDataExEvent);
begin
  FHttpClient.OnReceiveDataEx := Value;
end;

procedure TMARSHttpClient.SetOnRedirect(const Value: THTTPRedirectEvent);
begin
  FHttpClient.OnRedirect := Value;
end;

procedure TMARSHttpClient.SetOnSendData(const Value: TSendDataEvent);
begin
  FHttpClient.OnSendData := Value;
end;

procedure TMARSHttpClient.SetOnUpdateCookie(
  const Value: THTTPUpdateCookieEvent);
begin
  FHttpClient.OnUpdateCookie := Value;
end;

procedure TMARSHttpClient.SetPreemptiveAuthentication(const Value: Boolean);
begin
  FHttpClient.PreemptiveAuthentication := Value;
end;

procedure TMARSHttpClient.SetProtocolVersion(const Value: THTTPProtocolVersion);
begin
  FHttpClient.ProtocolVersion := Value;
end;

procedure TMARSHttpClient.SetReadTimeout(const Value: Integer);
begin
  {$ifdef Delphi10Berlin_UP}
    FHttpClient.ResponseTimeout := Value;
  {$else}
    // not available!
  {$endif}
end;

procedure TMARSHttpClient.SetReceiveDataCallback(
  const Value: TReceiveDataCallback);
begin
  FHttpClient.ReceiveDataCallBack := Value;
end;

procedure TMARSHttpClient.SetReceiveDataExCallback(
  const Value: TReceiveDataExCallback);
begin
  FHttpClient.ReceiveDataExCallback := Value;
end;

procedure TMARSHttpClient.SetRedirectsWithGET(
  const Value: THTTPRedirectsWithGET);
begin
  FHttpClient.RedirectsWithGET := Value;
end;

procedure TMARSHttpClient.SetSecureProtocols(const Value: THTTPSecureProtocols);
begin
  FHttpClient.SecureProtocols := Value;
end;

procedure TMARSHttpClient.SetSendDataCallback(const Value: TSendDataCallback);
begin
  FHttpClient.SendDataCallBack := Value;
end;

procedure TMARSHttpClient.SetUseDefaultCredentials(const Value: Boolean);
begin
  FHttpClient.UseDefaultCredentials := Value;
end;

procedure TMARSHttpClient.SetValidateServerCertificateCallback(
  const Value: TValidateCertificateCallback);
begin
  FHttpClient.ValidateServerCertificateCallback := Value;
end;

procedure TMARSHttpClient.SetValidateServerCertificateEvent(
  const Value: TValidateCertificateEvent);
begin
  FHttpClient.OnValidateServerCertificate := Value;
end;

procedure TMARSHttpClient.Post(const AURL: string;
  const AFormData: TArray<TFormParam>; const AResponse: TStream;
  const AAuthToken, AAccept: string; const AContentType: string);
var
  LFormData: TMultipartFormData;
begin
  inherited;

  FHttpClient.Accept := AAccept;
  FHttpClient.ContentType := AContentType;
  LFormData := CreateMultipartFormData(AFormData);
  try
    FLastResponse := FHttpClient.Post(AURL, LFormData, AResponse);
    CheckLastCmdSuccess;
  finally
    LFormData.Free;
  end;
end;

procedure TMARSHttpClient.Patch(const AURL: string; AContent, AResponse: TStream;
  const AAuthToken, AAccept, AContentType: string);
begin
  inherited;
  FHttpClient.Accept := AAccept;
  FHttpClient.ContentType := AContentType;
  AContent.Position := 0;
  FLastResponse := FHttpClient.Patch(AURL, AContent, AResponse);
  CheckLastCmdSuccess;
end;

procedure TMARSHttpClient.Post(const AURL: string; const AFormUrlEncoded: TMARSParameters; const AResponse: TStream;
  const AAuthToken, AAccept, AContentType: string);
var
  LFormUrlEncoded: TStrings;
begin
  inherited;

  FHttpClient.Accept := AAccept;
  FHttpClient.ContentType := AContentType;
  LFormUrlEncoded := AFormUrlEncoded.AsStrings;
  try
    FLastResponse := FHttpClient.Post(AURL, LFormUrlEncoded, AResponse);
    CheckLastCmdSuccess;
  finally
    LFormUrlEncoded.Free;
  end;
end;

procedure TMARSHttpClient.Put(const AURL: string;
  const AFormData: System.TArray<TFormParam>; const AResponse: TStream;
  const AAuthToken, AAccept: string; const AContentType: string);
var
  LFormData: TMultipartFormData;
begin
  inherited;

  FHttpClient.Accept := AAccept;
  FHttpClient.ContentType := AContentType;
  LFormData := CreateMultipartFormData(AFormData);
  try
    LFormData.Stream.Position := 0;
    FHttpClient.ContentType := LFormData.MimeTypeHeader;
    FLastResponse := FHttpClient.Put(AURL, LFormData, AResponse);
    CheckLastCmdSuccess;
  finally
    LFormData.Free;
  end;
end;

end.
