(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.Client.Net;

{$I MARS.inc}

interface

uses
  SysUtils, Classes
  , MARS.Core.JSON, MARS.Client.Utils, MARS.Core.Utils, MARS.Client.Client,  MARS.Utils.Parameters

  // Net
  , System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent
, System.Net.Mime
  ;

type
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TMARSNetClient = class(TMARSCustomClient)
  private
    FHttpClient: TNetHTTPClient;
    FLastResponse: IHTTPResponse;
  protected
    procedure AssignTo(Dest: TPersistent); override;
//    function GetProtocolVersion: TIdHTTPProtocolVersion;
//    procedure SetProtocolVersion(const Value: TIdHTTPProtocolVersion);

    procedure CloneCookies(const ADestination, ASource: TNetHTTPClient);

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
  published
//    property ProtocolVersion: TIdHTTPProtocolVersion read GetProtocolVersion write SetProtocolVersion;
    property HttpClient: TNetHTTPClient read FHttpClient;
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

{ TMARSNetClient }

procedure TMARSNetClient.ApplyCustomHeaders(const AHeaders: TStrings);
var
  LIndex: Integer;
begin
  inherited;
  for LIndex := 0 to AHeaders.Count-1 do
    HttpClient.CustomHeaders[AHeaders.Names[LIndex]] := AHeaders.ValueFromIndex[LIndex];
end;

procedure TMARSNetClient.ApplyProxyConfig;
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

procedure TMARSNetClient.AssignTo(Dest: TPersistent);
var
  LDestClient: TMARSNetClient;
begin
  inherited;
  if Dest is TMARSNetClient then
  begin
    LDestClient := Dest as TMARSNetClient;
//    LDestClient.ProtocolVersion := ProtocolVersion;
    LDestClient.AuthEndorsement := AuthEndorsement;
//    LDestClient.HttpClient.IOHandler := HttpClient.IOHandler;
    LDestClient.HttpClient.AllowCookies := HttpClient.AllowCookies;
    CloneCookies(LDestClient.HttpClient, HttpClient);
//    LDestClient.HttpClient.ProxyParams.BasicAuthentication := HttpClient.ProxyParams.BasicAuthentication;
//    LDestClient.HttpClient.ProxyParams.ProxyPort := HttpClient.ProxyParams.ProxyPort;
//    LDestClient.HttpClient.ProxyParams.ProxyServer := HttpClient.ProxyParams.ProxyServer;
//    LDestClient.HttpClient.Request.BasicAuthentication := HttpClient.Request.BasicAuthentication;
//    LDestClient.HttpClient.Request.Host := HttpClient.Request.Host;
//    LDestClient.HttpClient.Request.Password := HttpClient.Request.Password;
//    LDestClient.HttpClient.Request.Username := HttpClient.Request.Username;
  end;
end;

procedure TMARSNetClient.CheckLastCmdSuccess;
begin
  if not Assigned(FLastResponse) then
    Exit;

  if not LastCmdSuccess then
    raise EMARSClientHttpException.Create(FLastResponse.StatusText, FLastResponse.StatusCode,
      FLastResponse.ContentStream, FLastResponse.MimeType);
end;

procedure TMARSNetClient.CloneCookies(const ADestination,
  ASource: TNetHTTPClient);
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

constructor TMARSNetClient.Create(AOwner: TComponent);
begin
  inherited;

  FHttpClient := TNetHTTPClient.Create(Self);
  try
    FHttpClient.SetSubComponent(True);
    FHttpClient.Name := 'HttpClient';
  except
    FHttpClient.Free;
    raise;
  end;
end;


procedure TMARSNetClient.Delete(const AURL: string; AContent, AResponse: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  inherited;
  FHttpClient.Accept := AAccept;
  FHttpClient.ContentType := AContentType;
  FLastResponse := FHttpClient.Delete(AURL, AResponse);
  CheckLastCmdSuccess;
end;

destructor TMARSNetClient.Destroy;
begin
  FHttpClient.Free;
  inherited;
end;

procedure TMARSNetClient.EndorseAuthorization;
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

function TMARSNetClient.CreateMultipartFormData(
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

procedure TMARSNetClient.Get(const AURL: string; AResponseContent: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  FHttpClient.Accept := AAccept;
  FHttpClient.ContentType := AContentType;
  inherited;
  FLastResponse := FHttpClient.Get(AURL, AResponseContent);
  CheckLastCmdSuccess;
end;

function TMARSNetClient.GetConnectTimeout: Integer;
begin
  {$ifdef Delphi10Berlin_UP}
    Result := FHttpClient.ConnectionTimeout;
  {$else}
    Result := -1;
  {$endif}
end;

function TMARSNetClient.GetReadTimeout: Integer;
begin
  {$ifdef Delphi10Berlin_UP}
    Result := FHttpClient.ResponseTimeout;
  {$else}
    Result := -1;
  {$endif}
end;

function TMARSNetClient.LastCmdSuccess: Boolean;
begin
  Result := Assigned(FLastResponse)
    and ((FLastResponse.StatusCode >= 200) and (FLastResponse.StatusCode < 300));
end;

procedure TMARSNetClient.Post(const AURL: string; AContent, AResponse: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  inherited;
  FHttpClient.Accept := AAccept;
  FHttpClient.ContentType := AContentType;
  AContent.Position := 0;
  FLastResponse := FHttpClient.Post(AURL, AContent, AResponse);
  CheckLastCmdSuccess;
end;

procedure TMARSNetClient.Put(const AURL: string; AContent, AResponse: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  inherited;
  FHttpClient.Accept := AAccept;
  FHttpClient.ContentType := AContentType;
  AContent.Position := 0;
  FLastResponse := FHttpClient.Put(AURL, AContent, AResponse);
  CheckLastCmdSuccess;
end;

function TMARSNetClient.ResponseStatusCode: Integer;
begin
  Result := -1;
  if Assigned(FLastResponse) then
    Result := FLastResponse.StatusCode;
end;

function TMARSNetClient.ResponseText: string;
begin
  Result := '';
  if Assigned(FLastResponse) then
    Result := FLastResponse.StatusText;
end;

procedure TMARSNetClient.SetConnectTimeout(const Value: Integer);
begin
  {$ifdef Delphi10Berlin_UP}
    FHttpClient.ConnectionTimeout := Value;
  {$else}
    // not available!
  {$endif}
end;

//procedure TMARSNetClient.SetProtocolVersion(const Value: TIdHTTPProtocolVersion);
//begin
//  FHttpClient.ProtocolVersion := Value;
//end;

procedure TMARSNetClient.SetReadTimeout(const Value: Integer);
begin
  {$ifdef Delphi10Berlin_UP}
    FHttpClient.ResponseTimeout := Value;
  {$else}
    // not available!
  {$endif}
end;

//function TMARSNetClient.GetProtocolVersion: TIdHTTPProtocolVersion;
//begin
//  Result := FHttpClient.ProtocolVersion;
//end;

procedure TMARSNetClient.Post(const AURL: string;
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

procedure TMARSNetClient.Post(const AURL: string; const AFormUrlEncoded: TMARSParameters; const AResponse: TStream;
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

procedure TMARSNetClient.Put(const AURL: string;
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
    //TODO AM: verify if calling PUT with LFormData.Stream is safe enough and actually working
    // (TNetHttpClient does not provide an overload of put for TMultipartFormData (10.2.2 Tokyo)
    LFormData.Stream.Position := 0;
    FHttpClient.ContentType := LFormData.MimeTypeHeader;
    FLastResponse := FHttpClient.Put(AURL, LFormData.Stream, AResponse);
    CheckLastCmdSuccess;
  finally
    LFormData.Free;
  end;
end;

end.