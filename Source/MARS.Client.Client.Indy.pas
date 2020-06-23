(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.Client.Indy;

{$I MARS.inc}

interface

uses
  SysUtils, Classes
  , MARS.Core.JSON, MARS.Client.Utils, MARS.Core.Utils, MARS.Client.Client

  // Indy
  , IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, IdMultipartFormData
  , IdCookie, IdCookieManager, IdURI
  ;

type
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TMARSIndyClient = class(TMARSCustomClient)
  private
    FHttpClient: TIdHTTP;
  protected
    procedure AssignTo(Dest: TPersistent); override;
//    function GetRequest: TIdHTTPRequest;
//    function GetResponse: TIdHTTPResponse;

    procedure CloneCookies(const ADestination, ASource: TIdHTTP);

    function GetProtocolVersion: TIdHTTPProtocolVersion;
    procedure SetProtocolVersion(const Value: TIdHTTPProtocolVersion);

    function GetConnectTimeout: Integer; override;
    function GetReadTimeout: Integer; override;

    procedure SetConnectTimeout(const Value: Integer); override;
    procedure SetReadTimeout(const Value: Integer); override;

    function CreateMultipartFormData(AFormData: TArray<TFormParam>): TIdMultiPartFormDataStream;


    procedure EndorseAuthorization; override;
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
    procedure Put(const AURL: string; AContent, AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); override;
    procedure Put(const AURL: string; const AFormData: TArray<TFormParam>;
      const AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); override;

    function LastCmdSuccess: Boolean; override;
    function ResponseStatusCode: Integer; override;
    function ResponseText: string; override;

  //  property Request: TIdHTTPRequest read GetRequest;
  //  property Response: TIdHTTPResponse read GetResponse;

  published
    property ProtocolVersion: TIdHTTPProtocolVersion read GetProtocolVersion write SetProtocolVersion;
    property HttpClient: TIdHTTP read FHttpClient;
  end;

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TMARSClient = class(TMARSIndyClient); // compatibility

implementation

uses
    Rtti, TypInfo
  , MARS.Client.CustomResource
  , MARS.Client.Resource
  , MARS.Client.Resource.JSON
  , MARS.Client.Resource.Stream
  , MARS.Client.Application
;

{ TMARSIndyClient }

procedure TMARSIndyClient.ApplyCustomHeaders(const AHeaders: TStrings);
begin
  inherited;
  FHttpClient.Request.CustomHeaders.AddStrings(AHeaders);
end;

procedure TMARSIndyClient.ApplyProxyConfig;
begin
  inherited;
  if not Assigned(FHttpClient) then
    Exit;

  if ProxyConfig.Enabled then
  begin
    FHttpClient.ProxyParams.ProxyServer := ProxyConfig.Host;
    FHttpClient.ProxyParams.ProxyPort := ProxyConfig.Port;
    FHttpClient.ProxyParams.ProxyUsername := ProxyConfig.UserName;
    FHttpClient.ProxyParams.ProxyPassword := ProxyConfig.Password;
  end
  else
    FHttpClient.ProxyParams.Clear;
end;

procedure TMARSIndyClient.AssignTo(Dest: TPersistent);
var
  LDestClient: TMARSIndyClient;
begin
  inherited;
  if Dest is TMARSIndyClient then
  begin
    LDestClient := Dest as TMARSIndyClient;
    LDestClient.ProtocolVersion := ProtocolVersion;
    LDestClient.AuthEndorsement := AuthEndorsement;
    LDestClient.HttpClient.IOHandler := HttpClient.IOHandler;
    LDestClient.HttpClient.AllowCookies := HttpClient.AllowCookies;
    CloneCookies(LDestClient.HttpClient, HttpClient);
    LDestClient.HttpClient.ProxyParams.BasicAuthentication := HttpClient.ProxyParams.BasicAuthentication;
    LDestClient.HttpClient.ProxyParams.ProxyPort := HttpClient.ProxyParams.ProxyPort;
    LDestClient.HttpClient.ProxyParams.ProxyServer := HttpClient.ProxyParams.ProxyServer;
    LDestClient.HttpClient.Request.BasicAuthentication := HttpClient.Request.BasicAuthentication;
    LDestClient.HttpClient.Request.Host := HttpClient.Request.Host;
    LDestClient.HttpClient.Request.Password := HttpClient.Request.Password;
    LDestClient.HttpClient.Request.Username := HttpClient.Request.Username;
  end;
end;

procedure TMARSIndyClient.CloneCookies(const ADestination, ASource: TIdHTTP);
var
  LCookieManager: TIdCookieManager;
begin
  if ASource.AllowCookies and Assigned(ASource.CookieManager) then
  begin
    LCookieManager := TIdCookieManager.Create(ADestination);
    try
      LCookieManager.CookieCollection.AddCookies(
        ASource.CookieManager.CookieCollection
      );
      ADestination.CookieManager := LCookieManager;
    except
      LCookieManager.Free;
      raise;
    end;
  end;
end;

constructor TMARSIndyClient.Create(AOwner: TComponent);
begin
  inherited;

  FHttpClient := TIdHTTP.Create(Self);
  try
    FHttpClient.SetSubComponent(True);
    FHttpClient.Name := 'HttpClient';
  except
    FHttpClient.Free;
    raise;
  end;

end;


function TMARSIndyClient.CreateMultipartFormData(
  AFormData: TArray<TFormParam>): TIdMultiPartFormDataStream;
var
  LFormParam: TFormParam;
begin
  Result := TIdMultiPartFormDataStream.Create;
  try
    for LFormParam in AFormData do
    begin
      if not LFormParam.IsFile then
        Result.AddFormField(LFormParam.FieldName, LFormParam.Value.ToString)
      else
        Result.AddFile(LFormParam.FieldName, LFormParam.AsFile.FileName, LFormParam.AsFile.ContentType);
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TMARSIndyClient.Delete(const AURL: string; AContent, AResponse: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  inherited;
  FHttpClient.Request.Accept := AAccept;
  FHttpClient.Request.ContentType := AContentType;
{$ifdef DelphiXE7_UP}
  FHttpClient.Delete(AURL, AResponse);
{$else}
  FHttpClient.Delete(AURL{, AResponse});
{$endif}
end;

destructor TMARSIndyClient.Destroy;
begin
  FHttpClient.Free;
  inherited;
end;

procedure TMARSIndyClient.EndorseAuthorization;
var
  LURI: TIdURI;
begin
  if AuthEndorsement = AuthorizationBearer then
  begin
    if not (AuthToken = '') then
    begin
      FHttpClient.Request.CustomHeaders.FoldLines := False;
      FHttpClient.Request.CustomHeaders.Values['Authorization'] := 'Bearer ' + AuthToken;
    end
    else
      FHttpClient.Request.CustomHeaders.Values['Authorization'] := '';
  end
  else if AuthEndorsement = Cookie then
  begin
    if not (AuthToken = '') then
    begin
      if FHttpClient.AllowCookies then
      begin
        if not Assigned(FHttpClient.CookieManager) then
          FHttpClient.CookieManager := TIdCookieManager.Create(FHttpClient);

        if (FHttpClient.CookieManager.CookieCollection.GetCookieIndex(AuthCookieName) = -1) then
        begin
          LURI := TIdURI.Create(MARSEngineURL);
          try
            FHttpClient.CookieManager.AddServerCookie(AuthCookieName + '=' + AuthToken, LURI);
          finally
            LURI.Free;
          end;
        end;
      end;
    end;
  end;

end;

procedure TMARSIndyClient.Get(const AURL: string; AResponseContent: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  FHttpClient.Request.Accept := AAccept;
  FHttpClient.Request.ContentType := AContentType;
  inherited;
  FHttpClient.Get(AURL, AResponseContent);
end;

function TMARSIndyClient.GetConnectTimeout: Integer;
begin
  Result := FHttpClient.ConnectTimeout;
end;

function TMARSIndyClient.GetReadTimeout: Integer;
begin
  Result := FHttpClient.ReadTimeout;
end;

//function TMARSIndyClient.GetRequest: TIdHTTPRequest;
//begin
//  Result := FHttpClient.Request;
//end;

//function TMARSIndyClient.GetResponse: TIdHTTPResponse;
//begin
//  Result := FHttpClient.Response;
//end;

function TMARSIndyClient.LastCmdSuccess: Boolean;
begin
  Result := (FHttpClient.ResponseCode >= 200) and (FHttpClient.ResponseCode < 300);
end;

procedure TMARSIndyClient.Post(const AURL: string; AContent, AResponse: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  inherited;
  FHttpClient.Request.Accept := AAccept;
  FHttpClient.Request.ContentType := AContentType;
  FHttpClient.Post(AURL, AContent, AResponse);
end;

procedure TMARSIndyClient.Put(const AURL: string; AContent, AResponse: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  inherited;
  FHttpClient.Request.Accept := AAccept;
  FHttpClient.Request.ContentType := AContentType;
  FHttpClient.Put(AURL, AContent, AResponse);
end;

function TMARSIndyClient.ResponseStatusCode: Integer;
begin
  Result := FHttpClient.ResponseCode;
end;

function TMARSIndyClient.ResponseText: string;
begin
  Result := FHttpClient.ResponseText;
end;

procedure TMARSIndyClient.SetConnectTimeout(const Value: Integer);
begin
  FHttpClient.ConnectTimeout := Value;
end;

procedure TMARSIndyClient.SetProtocolVersion(const Value: TIdHTTPProtocolVersion);
begin
  FHttpClient.ProtocolVersion := Value;
end;

procedure TMARSIndyClient.SetReadTimeout(const Value: Integer);
begin
  FHttpClient.ReadTimeout := Value;
end;

function TMARSIndyClient.GetProtocolVersion: TIdHTTPProtocolVersion;
begin
  Result := FHttpClient.ProtocolVersion;
end;

procedure TMARSIndyClient.Post(const AURL: string;
  const AFormData: TArray<TFormParam>; const AResponse: TStream;
  const AAuthToken, AAccept: string; const AContentType: string);
var
  LFormDataStream: TIdMultiPartFormDataStream;
begin
  inherited;

  FHttpClient.Request.Accept := AAccept;

  LFormDataStream := CreateMultipartFormData(AFormData);
  try

    FHttpClient.Request.ContentType :=  'multipart/form-data, ' + LFormDataStream.RequestContentType;
    FHttpClient.Post(AURL, LFormDataStream, AResponse);
  finally
    LFormDataStream.Free;
  end;
end;

procedure TMARSIndyClient.Put(const AURL: string;
  const AFormData: TArray<TFormParam>; const AResponse: TStream;
  const AAuthToken, AAccept: string; const AContentType: string);
var
  LFormDataStream: TIdMultiPartFormDataStream;
begin
  inherited;

  FHttpClient.Request.Accept := AAccept;
  LFormDataStream := CreateMultipartFormData(AFormData);
  try
    FHttpClient.Request.ContentType := LFormDataStream.RequestContentType;
    FHttpClient.Put(AURL, LFormDataStream, AResponse);
  finally
    LFormDataStream.Free;
  end;
end;

end.
