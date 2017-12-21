(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.Client.Indy;

{$I MARS.inc}

interface

uses
  SysUtils, Classes
  , MARS.Core.JSON, MARS.Client.Utils, MARS.Client.Client

  // Indy
  , IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP
  ;

type
  {$ifdef DelphiXE2_UP}
    [ComponentPlatformsAttribute(
        pidWin32 or pidWin64
     or pidOSX32
     or pidiOSSimulator
     or pidiOSDevice
    {$ifdef DelphiXE8_UP}
     or pidiOSDevice32 or pidiOSDevice64
    {$endif}
     or pidAndroid)]
  {$endif}
  TMARSIndyClient = class(TMARSCustomClient)
  private
    FHttpClient: TIdHTTP;
  protected
    procedure AssignTo(Dest: TPersistent); override;
//    function GetRequest: TIdHTTPRequest;
//    function GetResponse: TIdHTTPResponse;
    function GetProtocolVersion: TIdHTTPProtocolVersion;
    procedure SetProtocolVersion(const Value: TIdHTTPProtocolVersion);

    function GetConnectTimeout: Integer; override;
    function GetReadTimeout: Integer; override;
    procedure SetConnectTimeout(const Value: Integer); override;
    procedure SetReadTimeout(const Value: Integer); override;

    procedure EndorseAuthorization(const AAuthToken: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Delete(const AURL: string; AContent, AResponse: TStream; const AAuthToken: string); override;
    procedure Get(const AURL: string; AResponseContent: TStream; const AAccept: string; const AAuthToken: string); override;
    procedure Post(const AURL: string; AContent, AResponse: TStream; const AAuthToken: string); override;
    procedure Put(const AURL: string; AContent, AResponse: TStream; const AAuthToken: string); override;

    function LastCmdSuccess: Boolean; override;
    function ResponseText: string; override;

  //  property Request: TIdHTTPRequest read GetRequest;
  //  property Response: TIdHTTPResponse read GetResponse;

  published
    property ProtocolVersion: TIdHTTPProtocolVersion read GetProtocolVersion write SetProtocolVersion;
    property HttpClient: TIdHTTP read FHttpClient;
  end;

  TMARSClient = class(TMARSIndyClient); // compatibility

procedure Register;

implementation

uses
    Rtti, TypInfo
  , MARS.Client.CustomResource
  , MARS.Client.Resource
  , MARS.Client.Resource.JSON
  , MARS.Client.Resource.Stream
  , MARS.Client.Application
;

procedure Register;
begin
  RegisterComponents('MARS-Curiosity Client', [TMARSClient]);
end;

{ TMARSIndyClient }

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
    LDestClient.HttpClient.ProxyParams.BasicAuthentication := HttpClient.ProxyParams.BasicAuthentication;
    LDestClient.HttpClient.ProxyParams.ProxyPort := HttpClient.ProxyParams.ProxyPort;
    LDestClient.HttpClient.ProxyParams.ProxyServer := HttpClient.ProxyParams.ProxyServer;
    LDestClient.HttpClient.Request.BasicAuthentication := HttpClient.Request.BasicAuthentication;
    LDestClient.HttpClient.Request.Host := HttpClient.Request.Host;
    LDestClient.HttpClient.Request.Password := HttpClient.Request.Password;
    LDestClient.HttpClient.Request.Username := HttpClient.Request.Username;
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


procedure TMARSIndyClient.Delete(const AURL: string; AContent, AResponse: TStream; const AAuthToken: string);
begin
  inherited;

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

procedure TMARSIndyClient.EndorseAuthorization(const AAuthToken: string);
begin
  if AuthEndorsement = AuthorizationBearer then
  begin
    if not (AAuthToken = '') then
    begin
      FHttpClient.Request.CustomHeaders.FoldLines := False;
      FHttpClient.Request.CustomHeaders.Values['Authorization'] := 'Bearer ' + AAuthToken;
    end
    else
      FHttpClient.Request.CustomHeaders.Values['Authorization'] := '';
  end;
end;

procedure TMARSIndyClient.Get(const AURL: string; AResponseContent: TStream;
  const AAccept: string; const AAuthToken: string);
begin
  FHttpClient.Request.Accept := AAccept;
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
  Result := FHttpClient.ResponseCode = 200;
end;

procedure TMARSIndyClient.Post(const AURL: string; AContent, AResponse: TStream; const AAuthToken: string);
begin
  inherited;
  FHttpClient.Post(AURL, AContent, AResponse);
end;

procedure TMARSIndyClient.Put(const AURL: string; AContent, AResponse: TStream; const AAuthToken: string);
begin
  inherited;
  FHttpClient.Put(AURL, AContent, AResponse);
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

end.
