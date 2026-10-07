(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.http.Server.DCS;

{$I MARS.inc}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.TimeSpan, DateUtils
, Net.CrossSocket.Base, Net.CrossSocket, Net.CrossHttpServer
, Net.CrossHttpMiddleware, Net.CrossHttpUtils
, MARS.Core.Engine.Interfaces
, MARS.Core.Token
, MARS.Core.RequestAndResponse.Interfaces
;

type
  TMARSDCSRequest = class(TInterfacedObject, IMARSRequest)
  private
    FDCSRequest: ICrossHttpRequest;
  public
    // IMARSRequest ------------------------------------------------------------
    function AsObject: TObject;
    function GetAccept: string;
    function GetAuthorization: string;
    function GetContent: string;

    function GetCookieParamIndex(const AName: string): Integer;
    function GetCookieParamValue(const AIndex: Integer): string; overload;
    function GetCookieParamValue(const AName: string): string; overload;
    function GetCookieParamCount: Integer;
    function GetCookies: TMARSCookies;

    function GetFilesCount: Integer;
    function GetFormParamCount: Integer;
    function GetFormParamIndex(const AName: string): Integer;
    function GetFormParamName(const AIndex: Integer): string;
    function GetFormParamValue(const AIndex: Integer): string; overload;
    function GetFormParamValue(const AName: string): string; overload;
    function GetFormFileParamIndex(const AName: string): Integer;
    function GetFormFileParam(const AIndex: Integer; out AFieldName: string;
      out AFileName: string; out ABytes: System.TArray<System.Byte>;
      out AContentType: string): Boolean;
    function GetFormParams: string;

    function GetHeaderParamCount: Integer; inline;
    function GetHeaderParamIndex(const AName: string): Integer; inline;
    function GetHeaderParamName(const AIndex: Integer): string; inline;
    function GetHeaderParamValue(const AHeaderName: string): string; overload; inline;
    function GetHeaderParamValue(const AIndex: Integer): string; overload; inline;
    function GetHeaders: TMARSHeaders; inline;

    function GetHostName: string;
    function GetMethod: string;
    function GetPort: Integer;
    function GetDate: TDateTime; inline;

    function GetQueryParamIndex(const AName: string): Integer; inline;
    function GetQueryParamValue(const AIndex: Integer): string; overload; inline;
    function GetQueryParamValue(const AName: string): string; overload; inline;
    function GetQueryParamName(const AIndex: Integer): string; inline;
    function GetQueryParamCount: Integer; inline;
    function GetQueryString: string; inline;
    function GetQueryParams: TMARSQueryParams;

    function GetRawContent: TBytes; inline;
    function GetRawPath: string;
    function GetContentFields: TArray<string>;
    function GetQueryFields: TArray<string>;
    function GetRemoteIP: string;
    function GetUserAgent: string;
    function GetIsSecure: Boolean;
    procedure CheckWorkaroundForISAPI;
    // -------------------------------------------------------------------------
    constructor Create(ADCSRequest: ICrossHttpRequest); virtual;
  end;

  // MARS sets content type, status code, headers and content in any order (TMARSResponse.CopyTo
  // sets the content first): the content is kept here and sent by Send, when the request has
  // been handled. DCS sends the response as soon as its Send is called.
  TMARSDCSResponse = class(TInterfacedObject, IMARSResponse)
  private
    FDCSResponse: ICrossHttpResponse;
    FContent: string;
    FContentStream: TStream;
    FContentEncoding: string;
    FSent: Boolean;
  public
    // IMARSResponse -----------------------------------------------------------
    function GetContent: string;
    function GetContentEncoding: string;
    function GetContentLength: Integer;
    function GetContentStream: TStream;
    function GetContentType: string;
    function GetStatusCode: Integer;
    function GetReasonString: string;
    procedure SetContent(const AContent: string);
    procedure SetContentEncoding(const AContentEncoding: string);
    procedure SetContentLength(const ALength: Integer);
    procedure SetContentStream(const AContentStream: TStream);
    procedure SetContentType(const AContentType: string);
    procedure SetHeader(const AName: string; const AValue: string);
    procedure SetStatusCode(const AStatusCode: Integer);
    procedure SetReasonString(const AReasonString: string);
    procedure SetCookie(const AName, AValue, ADomain, APath: string; const AExpiration: TDateTime; const ASecure: Boolean);
    procedure RedirectTo(const AURL: string);
    // -------------------------------------------------------------------------
    constructor Create(ADCSResponse: ICrossHttpResponse); virtual;
    destructor Destroy; override;
    // sends status code, headers and content (once)
    procedure Send;
  end;


  EMARSDCSServerException = class(Exception);

  // HTTP and/or HTTPS server based on Delphi Cross Socket.
  // HTTP listens on DefaultPort, HTTPS on SSLPort (0 disables either). HTTPS needs a certificate
  // and its private key in PEM format (the certificate file can hold the whole chain, e.g. the
  // fullchain.pem of Let's Encrypt) and OpenSSL at run time: libssl-3-x64.dll and
  // libcrypto-3-x64.dll (libssl-3.dll and libcrypto-3.dll for Win32; 1.1 works too) next to the
  // executable or in the PATH on Windows, libssl from the distribution on Linux.
  // Engine parameters (read when the server is created):
  //   PortSSL           HTTPS port (SSLPort), 0 = disabled
  //   DCS.SSL.CertFile  certificate (CertificateFile), default localhost.crt
  //   DCS.SSL.KeyFile   private key (PrivateKeyFile), default localhost.key
  // Relative file names are relative to the folder of the executable (or library).
  TMARShttpServerDCS = class
  public const
    SSL_CERTFILE_PARAM = 'DCS.SSL.CertFile';
    SSL_KEYFILE_PARAM = 'DCS.SSL.KeyFile';
    SSL_CERTFILE_DEFAULT = 'localhost.crt';
    SSL_KEYFILE_DEFAULT = 'localhost.key';
  private
    FStoppedAt: TDateTime;
    FEngine: IMARSEngine;
    FStartedAt: TDateTime;
    FActive: Boolean;
    FDefaultPort: Integer;
    FSSLPort: Integer;
    FCertificateFile: string;
    FPrivateKeyFile: string;
    FCertificate: string;
    FPrivateKey: string;
    FHttpServer: ICrossHttpServer;
    FHttpsServer: ICrossHttpServer;
    function GetUpTime: TTimeSpan;
    procedure SetActive(const Value: Boolean);
    procedure SetDefaultPort(const Value: Integer);
  protected
    function CreateServer(const APort: Integer; const ASsl: Boolean): ICrossHttpServer; virtual;
    procedure LoadCertificate(const AServer: ICrossHttpServer); virtual;
    function ExpandFileName(const AFileName: string): string; virtual;
    procedure Startup; virtual;
    procedure Shutdown; virtual;
  public
    constructor Create(AEngine: IMARSEngine); virtual;
    destructor Destroy; override;

    property Active: Boolean read FActive write SetActive;
    // HTTP port, 0 = no HTTP
    property DefaultPort: Integer read FDefaultPort write SetDefaultPort;
    // HTTPS port, 0 = no HTTPS (default: the PortSSL engine parameter)
    property SSLPort: Integer read FSSLPort write FSSLPort;
    // PEM files for HTTPS (defaults: DCS.SSL.CertFile and DCS.SSL.KeyFile engine parameters)
    property CertificateFile: string read FCertificateFile write FCertificateFile;
    property PrivateKeyFile: string read FPrivateKeyFile write FPrivateKeyFile;
    // PEM content for HTTPS, used instead of the files when not empty
    property Certificate: string read FCertificate write FCertificate;
    property PrivateKey: string read FPrivateKey write FPrivateKey;
    property Engine: IMARSEngine read FEngine;
    // the DCS servers while active (nil when disabled), for fine tuning
    property HttpServer: ICrossHttpServer read FHttpServer;
    property HttpsServer: ICrossHttpServer read FHttpsServer;
    property StartedAt: TDateTime read FStartedAt;
    property StoppedAt: TDateTime read FStoppedAt;
    property UpTime: TTimeSpan read GetUpTime;
  end;

implementation

uses
  System.IOUtils
, Net.CrossHttpParams;

{ TMARShttpServerDCS }

constructor TMARShttpServerDCS.Create(AEngine: IMARSEngine);
begin
  inherited Create;
  FEngine := AEngine;
  if Assigned(FEngine) then
  begin
    FSSLPort := FEngine.PortSSL;
    FCertificateFile := FEngine.Parameters.ByNameText(SSL_CERTFILE_PARAM, SSL_CERTFILE_DEFAULT).AsString;
    FPrivateKeyFile := FEngine.Parameters.ByNameText(SSL_KEYFILE_PARAM, SSL_KEYFILE_DEFAULT).AsString;
  end;
end;

destructor TMARShttpServerDCS.Destroy;
begin
  if Active then
    Shutdown;
  FHttpServer := nil;
  FHttpsServer := nil;
  inherited;
end;

function TMARShttpServerDCS.ExpandFileName(const AFileName: string): string;
begin
  Result := AFileName;
  if (Result <> '') and TPath.IsRelativePath(Result) then
    Result := TPath.Combine(ExtractFilePath(GetModuleName(HInstance)), Result);
end;

const
  OPENSSL_LIBRARIES =
{$IF defined(MSWINDOWS) and defined(CPU64BITS)}
    'libssl-3-x64.dll and libcrypto-3-x64.dll (or libssl-1_1-x64.dll and libcrypto-1_1-x64.dll)';
{$ELSEIF defined(MSWINDOWS)}
    'libssl-3.dll and libcrypto-3.dll (or libssl-1_1.dll and libcrypto-1_1.dll)';
{$ELSE}
    'libssl and libcrypto (i.e. package libssl3)';
{$ENDIF}

function TMARShttpServerDCS.CreateServer(const APort: Integer; const ASsl: Boolean): ICrossHttpServer;
var
  LEngine: IMARSEngine;
  LHandler: TCrossHttpRouterProc;
  LBasePath: string;
begin
  try
    Result := TCrossHttpServer.Create(0, ASsl);
  except
    on E: Exception do
      if ASsl then
        raise EMARSDCSServerException.CreateFmt('HTTPS (DCS) on port %d: OpenSSL cannot be loaded (%s).'
          + ' %s must be next to the executable or in the PATH', [APort, E.Message, OPENSSL_LIBRARIES])
      else
        raise;
  end;
  if ASsl then
    LoadCertificate(Result);

  Result.Addr := IPv4v6_ALL; // IPv4v6
  Result.Port := APort;
  Result.Compressible := True;

  LEngine := FEngine;
  LHandler :=
    procedure(const ARequest: ICrossHttpRequest; const AResponse: ICrossHttpResponse; var AHandled: Boolean)
    var
      LResponse: TMARSDCSResponse;
      LResponseIntf: IMARSResponse;
    begin
      LResponse := TMARSDCSResponse.Create(AResponse);
      LResponseIntf := LResponse; // keeps it alive
      AHandled := LEngine.HandleRequest(TMARSDCSRequest.Create(ARequest), LResponseIntf);
      if AHandled then
        LResponse.Send;
    end;

  // the DCS router matches by path segment and '*' is a wildcard only as a whole last segment
  // ('/rest/*'): '/rest*' would match a single segment
  LBasePath := FEngine.BasePath;
  while LBasePath.EndsWith('/') do
    LBasePath := LBasePath.Substring(0, LBasePath.Length - 1);
  if LBasePath = '' then
    Result.All('*', LHandler)
  else
  begin
    Result.All(LBasePath, LHandler);
    Result.All(LBasePath + '/*', LHandler);
  end;
end;

procedure TMARShttpServerDCS.LoadCertificate(const AServer: ICrossHttpServer);
var
  LCertFile, LKeyFile: string;
begin
  try
    if FCertificate <> '' then
      AServer.SetCertificate(FCertificate)
    else
    begin
      LCertFile := ExpandFileName(FCertificateFile);
      if not FileExists(LCertFile) then
        raise EMARSDCSServerException.CreateFmt('certificate file not found: %s', [LCertFile]);
      AServer.SetCertificateFile(LCertFile);
    end;

    if FPrivateKey <> '' then
      AServer.SetPrivateKey(FPrivateKey)
    else
    begin
      LKeyFile := ExpandFileName(FPrivateKeyFile);
      if not FileExists(LKeyFile) then
        raise EMARSDCSServerException.CreateFmt('private key file not found: %s', [LKeyFile]);
      AServer.SetPrivateKeyFile(LKeyFile);
    end;
  except
    on E: Exception do
      raise EMARSDCSServerException.CreateFmt('HTTPS (DCS) on port %d: %s (%s=%s, %s=%s)'
        , [SSLPort, E.Message, SSL_CERTFILE_PARAM, FCertificateFile, SSL_KEYFILE_PARAM, FPrivateKeyFile]);
  end;
end;

function TMARShttpServerDCS.GetUpTime: TTimeSpan;
begin
  if Active then
    Result := TTimeSpan.FromSeconds(SecondsBetween(FStartedAt, Now))
  else if StoppedAt > 0 then
    Result := TTimeSpan.FromSeconds(SecondsBetween(FStartedAt, FStoppedAt))
  else
    Result := TTimeSpan.Zero;
end;

procedure TMARShttpServerDCS.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
      Startup
    else
      Shutdown;
    FActive := Value;
  end;
end;

procedure TMARShttpServerDCS.SetDefaultPort(const Value: Integer);
begin
  FDefaultPort := Value;
end;

procedure TMARShttpServerDCS.Shutdown;
begin
  if Assigned(FHttpServer) then
    FHttpServer.Stop;
  if Assigned(FHttpsServer) then
    FHttpsServer.Stop;
  FHttpServer := nil;
  FHttpsServer := nil;

  FStoppedAt := Now;
end;

procedure TMARShttpServerDCS.Startup;
begin
  if (DefaultPort <= 0) and (SSLPort <= 0) then
    raise EMARSDCSServerException.Create('DCS server: no port to listen on (DefaultPort and SSLPort are both 0)');

  // fresh servers at each start: routes are registered once per server
  FHttpServer := nil;
  FHttpsServer := nil;
  try
    if DefaultPort > 0 then
    begin
      FHttpServer := CreateServer(DefaultPort, False);
      FHttpServer.Start;
    end;

    if SSLPort > 0 then
    begin
      FHttpsServer := CreateServer(SSLPort, True);
      FHttpsServer.Start;
    end;
  except
    if Assigned(FHttpServer) then
      FHttpServer.Stop;
    if Assigned(FHttpsServer) then
      FHttpsServer.Stop;
    FHttpServer := nil;
    FHttpsServer := nil;
    raise;
  end;

  FStartedAt := Now;
  FStoppedAt := 0;
end;

{ TMARSDCSRequest }

function TMARSDCSRequest.AsObject: TObject;
begin
  Result := Self;
end;

procedure TMARSDCSRequest.CheckWorkaroundForISAPI;
begin
  // nothing to do
end;

constructor TMARSDCSRequest.Create(ADCSRequest: ICrossHttpRequest);
begin
  inherited Create;
  FDCSRequest := ADCSRequest;
end;

function TMARSDCSRequest.GetAccept: string;
begin
  Result := FDCSRequest.Accept;
end;

function TMARSDCSRequest.GetAuthorization: string;
begin
  Result := FDCSRequest.Authorization;
end;

function TMARSDCSRequest.GetContent: string;
begin
  Result := TEncoding.UTF8.GetString(GetRawContent);
end;

function TMARSDCSRequest.GetContentFields: TArray<string>;
var
  LMultiPartBody: THttpMultiPartFormData;
  LIndex: Integer;
  LFormField: TFormField;
  LURLParamsBody: THttpUrlParams;
  LParam: TNameValue;
begin
  Result := [];

  if FDCSRequest.BodyType = btMultiPart then
  begin
    LMultiPartBody := FDCSRequest.Body as THttpMultiPartFormData;

    for LIndex := 0 to LMultiPartBody.Count - 1 do
    begin
      LFormField := LMultiPartBody.Items[LIndex];

      Result := Result + [LFormField.AsString];
    end;
  end
  else if FDCSRequest.BodyType = btUrlEncoded then
  begin
    LURLParamsBody := FDCSRequest.Body as THttpUrlParams;

    for LIndex := 0 to LURLParamsBody.Count-1 do
    begin
      LParam := LURLParamsBody.Items[LIndex];
      Result := Result + [LParam.Name + '=' + LParam.Value];
    end;
  end;
end;

function TMARSDCSRequest.GetCookieParamCount: Integer;
begin
  Result := FDCSRequest.Cookies.Count;
end;

function TMARSDCSRequest.GetCookieParamIndex(const AName: string): Integer;
var
  LIndex: Integer;
begin
  Result := -1;
  for LIndex := 0 to FDCSRequest.Cookies.Count -1 do
  begin
    if SameText(FDCSRequest.Cookies.Items[LIndex].Name, AName) then
    begin
      Result := LIndex;
      Break;
    end;
  end;
end;

function TMARSDCSRequest.GetCookieParamValue(const AName: string): string;
var
  LIndex: Integer;
  LCookie: TNameValue;
begin
  Result := '';
  for LIndex := 0 to FDCSRequest.Cookies.Count -1 do
  begin
    LCookie := FDCSRequest.Cookies.Items[LIndex];
    if SameText(LCookie.Name, AName) then
    begin
      Result := LCookie.Value;
      Break;
    end;
  end;
end;

function TMARSDCSRequest.GetCookies: TMARSCookies;
var
  LIndex: Integer;
  LCookie: TNameValue;
begin
  SetLength(Result, FDCSRequest.Cookies.Count);

  for LIndex := 0 to FDCSRequest.Cookies.Count -1 do
  begin
    LCookie := FDCSRequest.Cookies.Items[LIndex];
    Result[LIndex].Name := LCookie.Name;
    Result[LIndex].Value := LCookie.Value;
  end;
end;

function TMARSDCSRequest.GetDate: TDateTime;
begin
  Result := TCrossHttpUtils.RFC1123_StrToDate(GetHeaderParamValue('Date'));
end;

function TMARSDCSRequest.GetCookieParamValue(const AIndex: Integer): string;
begin
  Result := FDCSRequest.Cookies.Items[AIndex].Value;
end;

function TMARSDCSRequest.GetFilesCount: Integer;
begin
//AM TODO
  Result := 0;
end;

function TMARSDCSRequest.GetFormFileParam(const AIndex: Integer; out AFieldName,
  AFileName: string; out ABytes: System.TArray<System.Byte>;
  out AContentType: string): Boolean;
var
  LMultiPartBody: THttpMultiPartFormData;
  LFile: TFormField;

begin
  Result := False;
  if FDCSRequest.BodyType = btMultiPart then
  begin
    LMultiPartBody := FDCSRequest.Body as THttpMultiPartFormData;
    Result := (AIndex >= 0) and (AIndex < LMultiPartBody.Count);
    if Result then
    begin
      LFile := LMultiPartBody.Items[AIndex];
      AFieldName := LFile.Name;
      AFileName := LFile.FileName;
      ABytes := LFile.AsBytes;
      AContentType := LFile.ContentType;
    end;
  end;
end;

function TMARSDCSRequest.GetFormFileParamIndex(const AName: string): Integer;
var
  LMultiPartBody: THttpMultiPartFormData;
  LIndex: Integer;
  LItem: TFormField;
  LURLParamsBody: THttpUrlParams;
  LURLParamItem: TNameValue;
begin
  Result := -1;
  if FDCSRequest.BodyType = btMultiPart then
  begin
    LMultiPartBody := FDCSRequest.Body as THttpMultiPartFormData;

    for LIndex := 0 to LMultiPartBody.Count - 1 do
    begin
      LItem := LMultiPartBody.Items[LIndex];

      if SameText(LItem.Name, AName) then
      begin
        Result := LIndex;
        Break;
      end;
    end;
  end
  else if FDCSRequest.BodyType = btUrlEncoded then
  begin
    LURLParamsBody := FDCSRequest.Body as THttpUrlParams;

    for LIndex := 0 to LURLParamsBody.Count-1 do
    begin
      LURLParamItem := LURLParamsBody.Items[LIndex];
      if SameText(LURLParamItem.Name, AName) then
      begin
        Result := LIndex;
        Break;
      end;
    end;
  end;
end;

function TMARSDCSRequest.GetFormParamCount: Integer;
begin
//AM TODO
  Result := 0;
end;

function TMARSDCSRequest.GetFormParamIndex(const AName: string): Integer;
var
  LMultiPartBody: THttpMultiPartFormData;
  LIndex: Integer;
  LItem: TFormField;
  LURLParamsBody: THttpUrlParams;
  LURLParamItem: TNameValue;
begin
  Result := -1;
  if FDCSRequest.BodyType = btMultiPart then
  begin
    LMultiPartBody := FDCSRequest.Body as THttpMultiPartFormData;

    for LIndex := 0 to LMultiPartBody.Count - 1 do
    begin
      LItem := LMultiPartBody.Items[LIndex];

      if SameText(LItem.Name, AName) then
      begin
        Result := LIndex;
        Break;
      end;
    end;
  end
  else if FDCSRequest.BodyType = btUrlEncoded then
  begin
    LURLParamsBody := FDCSRequest.Body as THttpUrlParams;

    for LIndex := 0 to LURLParamsBody.Count-1 do
    begin
      LURLParamItem := LURLParamsBody.Items[LIndex];
      if SameText(LURLParamItem.Name, AName) then
      begin
        Result := LIndex;
        Break;
      end;
    end;
  end;
end;

function TMARSDCSRequest.GetFormParamName(const AIndex: Integer): string;
var
  LMultiPartBody: THttpMultiPartFormData;
  LURLParamsBody: THttpUrlParams;
begin
  Result := '';
  if FDCSRequest.BodyType = btMultiPart then
  begin
    LMultiPartBody := FDCSRequest.Body as THttpMultiPartFormData;
    Result := LMultiPartBody.Items[AIndex].Name;
  end
  else if FDCSRequest.BodyType = btUrlEncoded then
  begin
    LURLParamsBody := FDCSRequest.Body as THttpUrlParams;
    Result := LURLParamsBody.Items[AIndex].Name;
  end;
end;


function TMARSDCSRequest.GetFormParams: string;
begin
//AM TODO
  Result := '';
end;

function TMARSDCSRequest.GetFormParamValue(const AName: string): string;
begin
  Result := GetFormParamValue(GetFormParamIndex(AName));
end;

function TMARSDCSRequest.GetFormParamValue(const AIndex: Integer): string;
var
  LMultiPartBody: THttpMultiPartFormData;
  LURLParamsBody: THttpUrlParams;
begin
  Result := '';
  if FDCSRequest.BodyType = btMultiPart then
  begin
    LMultiPartBody := FDCSRequest.Body as THttpMultiPartFormData;
    Result := LMultiPartBody.Items[AIndex].AsString;
  end
  else if FDCSRequest.BodyType = btUrlEncoded then
  begin
    LURLParamsBody := FDCSRequest.Body as THttpUrlParams;
    Result := LURLParamsBody.Items[AIndex].Value;
  end;
end;

function TMARSDCSRequest.GetHeaderParamValue(const AHeaderName: string): string;
begin
  Result := '';
  FDCSRequest.Header.GetParamValue(AHeaderName, Result);
end;

function TMARSDCSRequest.GetHeaderParamValue(const AIndex: Integer): string;
begin
  Result := FDCSRequest.Header.Items[AIndex].Value;
end;

function TMARSDCSRequest.GetHeaders: TMARSHeaders;
var
  LIndex: Integer;
  LParam: TNameValue;
begin
  SetLength(Result, FDCSRequest.Header.Count);
  for LIndex := Low(Result) to High(Result) do
  begin
    LParam := FDCSRequest.Header.Items[LIndex];
    Result[LIndex].Name := LParam.Name;
    Result[LIndex].Value := LParam.Value;
  end;
end;

function TMARSDCSRequest.GetHostName: string;
begin
  Result := FDCSRequest.HostName;
end;

function TMARSDCSRequest.GetMethod: string;
begin
  Result := FDCSRequest.Method;
end;

function TMARSDCSRequest.GetPort: Integer;
begin
  Result := FDCSRequest.HostPort;
end;

function TMARSDCSRequest.GetQueryFields: TArray<string>;
var
  LQuery: TNameValue;
begin
  Result := [];
  for LQuery in FDCSRequest.Query do
    Result := Result + [LQuery.Name + '=' + LQuery.Value];
end;

function TMARSDCSRequest.GetQueryParamCount: Integer;
begin
  Result := FDCSRequest.Query.Count;
end;

function TMARSDCSRequest.GetQueryParamIndex(const AName: string): Integer;
var
  LIndex: Integer;
begin
  Result := -1;
  for LIndex := 0 to FDCSRequest.Query.Count -1 do
  begin
    if SameText(FDCSRequest.Query.Items[LIndex].Name, AName) then
    begin
      Result := LIndex;
      Break;
    end;
  end;
end;

function TMARSDCSRequest.GetQueryParamName(const AIndex: Integer): string;
begin
  Result := FDCSRequest.Query.Items[AIndex].Name;
end;

function TMARSDCSRequest.GetQueryParams: TMARSQueryParams;
var
  LIndex: Integer;
begin
  SetLength(Result, FDCSRequest.Query.Count);

  for LIndex := 0 to FDCSRequest.Query.Count -1 do
  begin
    Result[LIndex].Name := FDCSRequest.Query.Items[LIndex].Name;
    Result[LIndex].Value := FDCSRequest.Query.Items[LIndex].Value;
  end;
end;

function TMARSDCSRequest.GetQueryParamValue(const AName: string): string;
var
  LValue: string;
begin
  if FDCSRequest.Query.GetParamValue(AName, LValue) then
    Result := LValue
  else
    Result := '';
end;

function TMARSDCSRequest.GetQueryParamValue(const AIndex: Integer): string;
begin
  Result := FDCSRequest.Query.Items[AIndex].Value;
end;

function TMARSDCSRequest.GetQueryString: string;
var
  LRaw: string;
  LPos: Integer;
begin
  // the query string as sent by the client (Query.ToString is the class name)
  LRaw := FDCSRequest.RawPathAndParams;
  LPos := Pos('?', LRaw);
  if LPos > 0 then
    Result := Copy(LRaw, LPos + 1, MaxInt)
  else
    Result := '';
end;

function TMARSDCSRequest.GetRawContent: TBytes;
var
  LStream: TStream;
  LPosition: Int64;
begin
  Result := [];
  // DCS keeps the raw body of url-encoded and binary (i.e. JSON) requests in a TMemoryStream;
  // nil for multipart/form-data
  LStream := FDCSRequest.RawBody;
  if Assigned(LStream) and (LStream.Size > 0) then
  begin
    SetLength(Result, LStream.Size);
    LPosition := LStream.Position;
    LStream.Position := 0;
    LStream.ReadBuffer(Result[0], LStream.Size);
    LStream.Position := LPosition;
  end;
end;

function TMARSDCSRequest.GetRawPath: string;
begin
//AM TODO controllare RawPathAndParams?
  Result := FDCSRequest.Path;
end;

function TMARSDCSRequest.GetRemoteIP: string;
begin
  Result := FDCSRequest.Connection.PeerAddr;
end;

function TMARSDCSRequest.GetIsSecure: Boolean;
begin
  Result := Assigned(FDCSRequest.Connection) and FDCSRequest.Connection.Ssl;
end;

function TMARSDCSRequest.GetUserAgent: string;
begin
  Result := FDCSRequest.UserAgent;
end;

function TMARSDCSRequest.GetHeaderParamCount: Integer;
begin
  Result := FDCSRequest.Header.Count;
end;

function TMARSDCSRequest.GetHeaderParamIndex(const AName: string): Integer;
var
  LIndex: Integer;
  LParam: TNameValue;
begin
  Result := -1;
  for LIndex := 0 to FDCSRequest.Header.Count-1 do
  begin
    LParam := FDCSRequest.Header.Items[LIndex];
    if SameText(LParam.Name, AName) then
    begin
      Result := LIndex;
      Break;
    end;
  end;
end;

function TMARSDCSRequest.GetHeaderParamName(const AIndex: Integer): string;
begin
  Result := FDCSRequest.Header.Items[AIndex].Name;
end;

{ TMARSDCSResponse }

constructor TMARSDCSResponse.Create(ADCSResponse: ICrossHttpResponse);
begin
  inherited Create;
  FDCSResponse := ADCSResponse;
end;

destructor TMARSDCSResponse.Destroy;
begin
  // not sent (request not handled): the response owns the content stream
  FreeAndNil(FContentStream);
  inherited;
end;

procedure TMARSDCSResponse.Send;
var
  LStream: TStream;
begin
  if FSent then
    Exit;
  FSent := True;

  if Assigned(FContentStream) then
  begin
    // DCS sends the stream asynchronously, it is freed when the send is complete
    LStream := FContentStream;
    FContentStream := nil;
    FDCSResponse.Send(LStream,
      procedure(const AConnection: ICrossConnection; const ASuccess: Boolean)
      begin
        LStream.Free;
      end
    );
  end
  else
    FDCSResponse.Send(FContent);
end;

function TMARSDCSResponse.GetContent: string;
begin
  Result := FContent;
end;

function TMARSDCSResponse.GetContentEncoding: string;
begin
  Result := FContentEncoding;
end;

function TMARSDCSResponse.GetContentLength: Integer;
begin
  Result := -1;
end;

function TMARSDCSResponse.GetContentStream: TStream;
begin
  Result := FContentStream;
end;

function TMARSDCSResponse.GetContentType: string;
begin
  Result := FDCSResponse.ContentType;
end;

function TMARSDCSResponse.GetReasonString: string;
begin
  Result := ''; // unsupported
end;

function TMARSDCSResponse.GetStatusCode: Integer;
begin
  Result := FDCSResponse.StatusCode;
end;

procedure TMARSDCSResponse.RedirectTo(const AURL: string);
begin
  FSent := True;
  FDCSResponse.Redirect(AURL);
end;

procedure TMARSDCSResponse.SetContent(const AContent: string);
begin
  FContent := AContent;
end;

procedure TMARSDCSResponse.SetContentEncoding(const AContentEncoding: string);
begin
  FContentEncoding := AContentEncoding;
  if AContentEncoding <> '' then
    FDCSResponse.Header['Content-Encoding'] := AContentEncoding
  else
    FDCSResponse.Header.Remove('Content-Encoding');
end;

procedure TMARSDCSResponse.SetContentLength(const ALength: Integer);
begin
  // unsupported
end;

procedure TMARSDCSResponse.SetContentStream(const AContentStream: TStream);
begin
  // the response owns the content stream (as TWebResponse does with Indy); like TWebResponse,
  // the previous one is not freed (i.e. the compression hook frees it)
  FContentStream := AContentStream;
end;

procedure TMARSDCSResponse.SetContentType(const AContentType: string);
begin
  FDCSResponse.ContentType := AContentType;
end;

procedure TMARSDCSResponse.SetCookie(const AName, AValue, ADomain,
  APath: string; const AExpiration: TDateTime; const ASecure: Boolean);
begin
  // HttpOnly, as with Indy: the token cookie must not be readable by scripts
  FDCSResponse.Cookies.AddOrSet(AName, AValue, SecondsBetween(Now, AExpiration), APath, ADomain, True {AHttpOnly}, ASecure);
end;

procedure TMARSDCSResponse.SetHeader(const AName, AValue: string);
begin
  FDCSResponse.Header.Add(AName, AValue);
end;

procedure TMARSDCSResponse.SetReasonString(const AReasonString: string);
begin
  // unsupported
end;

procedure TMARSDCSResponse.SetStatusCode(const AStatusCode: Integer);
begin
  FDCSResponse.StatusCode := AStatusCode;
end;

end.
