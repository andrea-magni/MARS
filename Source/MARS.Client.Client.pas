(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
  Home: https://github.com/andrea-magni/MARS

*)
unit MARS.Client.Client;

{$I MARS.inc}

interface

uses
  SysUtils, Classes

{$ifdef DelphiXE7_UP}
  , System.Threading
{$endif}

  // Indy
  , IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP
  ;

type
  {$ifdef DelphiXE2_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  {$endif}
  TMARSClient = class(TComponent)
  private
    FHttpClient: TIdHTTP;
    FMARSEngineURL: string;

{$ifdef DelphiXE7_UP}
    FWorkerTask: ITask;
{$endif}
    function GetRequest: TIdHTTPRequest;
    function GetResponse: TIdHTTPResponse;
    function GetConnectTimeout: Integer;
    function GetReadTimeout: Integer;
    procedure SetConnectTimeout(const Value: Integer);
    procedure SetReadTimeout(const Value: Integer);
  protected
{$ifdef DelphiXE7_UP}
    property WorkerTask: ITask read FWorkerTask;
{$endif}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Delete(const AURL: string; AResponseContent: TStream; const AAuthToken: string);
    procedure Get(const AURL: string; AResponseContent: TStream; const AAccept: string; const AAuthToken: string);
    procedure Post(const AURL: string; AContent, AResponse: TStream; const AAuthToken: string);
    procedure Put(const AURL: string; AContent, AResponse: TStream; const AAuthToken: string);
    function LastCmdSuccess: Boolean;
    function ResponseText: string;

    procedure ExecuteAsync(const AProc: TProc);
    function IsRunningAsync: Boolean;

    property Request: TIdHTTPRequest read GetRequest;
    property Response: TIdHTTPResponse read GetResponse;
  published
    property MARSEngineURL: string read FMARSEngineURL write FMARSEngineURL;
    property ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout;
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MARS Client', [TMARSClient]);
end;

{ TMARSClient }

constructor TMARSClient.Create(AOwner: TComponent);
begin
  inherited;
  FHttpClient := TIdHTTP.Create(nil);
  FMARSEngineURL := 'http://localhost:8080/rest';
end;

procedure TMARSClient.Delete(const AURL: string; AResponseContent: TStream; const AAuthToken: string);
begin
  FHttpClient.Request.CustomHeaders.Values['auth_token'] := AAuthToken;
{$ifdef DelphiXE7_UP}
  FHttpClient.Delete(AURL, AResponseContent);
{$else}
  FHttpClient.Delete(AURL{, AResponseContent});
{$endif}
end;

destructor TMARSClient.Destroy;
begin
  FHttpClient.Free;
  inherited;
end;

procedure TMARSClient.ExecuteAsync(const AProc: TProc);
begin
{$ifdef DelphiXE7_UP}
  if IsRunningAsync then
    raise Exception.Create('Multiple async execution not yet supported');
  FWorkerTask := TTask.Create(AProc).Start;
{$else}
  raise Exception.Create('Async execution not yet supported');
{$endif}
end;

procedure TMARSClient.Get(const AURL: string; AResponseContent: TStream;
  const AAccept: string; const AAuthToken: string);
begin
  FHttpClient.Request.Accept := AAccept;
  FHttpClient.Request.CustomHeaders.Values['auth_token'] := AAuthToken;
  FHttpClient.Get(AURL, AResponseContent);
end;

function TMARSClient.GetConnectTimeout: Integer;
begin
  Result := FHttpClient.ConnectTimeout;
end;

function TMARSClient.GetReadTimeout: Integer;
begin
  Result := FHttpClient.ReadTimeout;
end;

function TMARSClient.GetRequest: TIdHTTPRequest;
begin
  Result := FHttpClient.Request;
end;

function TMARSClient.GetResponse: TIdHTTPResponse;
begin
  Result := FHttpClient.Response;
end;

function TMARSClient.IsRunningAsync: Boolean;
begin
{$ifdef DelphiXE7_UP}
  Result := Assigned(FWorkerTask) and (FWorkerTask.Status < TTaskStatus.Completed);
{$else}
  Result := False;
{$endif}
end;

function TMARSClient.LastCmdSuccess: Boolean;
begin
  Result := FHttpClient.ResponseCode = 200;
end;

procedure TMARSClient.Post(const AURL: string; AContent, AResponse: TStream; const AAuthToken: string);
begin
  FHttpClient.Request.CustomHeaders.Values['auth_token'] := AAuthToken;
  FHttpClient.Post(AURL, AContent, AResponse);
end;

procedure TMARSClient.Put(const AURL: string; AContent, AResponse: TStream; const AAuthToken: string);
begin
  FHttpClient.Request.CustomHeaders.Values['auth_token'] := AAuthToken;
  FHttpClient.Put(AURL, AContent, AResponse);
end;

function TMARSClient.ResponseText: string;
begin
  Result := FHttpClient.ResponseText;
end;

procedure TMARSClient.SetConnectTimeout(const Value: Integer);
begin
  FHttpClient.ConnectTimeout := Value;
end;

procedure TMARSClient.SetReadTimeout(const Value: Integer);
begin
  FHttpClient.ReadTimeout := Value;
end;

end.
