(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.http.Server.DCS;

{$I MARS.inc}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.TimeSpan, DateUtils
, Net.CrossSocket.Base, Net.CrossSocket, Net.CrossHttpServer
, Net.CrossHttpMiddleware, Net.CrossHttpUtils
, MARS.Core.Engine, MARS.Core.Token
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
    function GetHeaderParamValue(const AHeaderName: string): string; overload; inline;
    function GetHeaderParamValue(const AIndex: Integer): string; overload; inline;
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
    function GetRawContent: TBytes; inline;
    function GetRawPath: string;
    function GetContentFields: TArray<string>;
    function GetQueryFields: TArray<string>;
    function GetRemoteIP: string;
    function GetUserAgent: string;
    procedure CheckWorkaroundForISAPI;
    // -------------------------------------------------------------------------
    constructor Create(ADCSRequest: ICrossHttpRequest); virtual;
  end;

  TMARSDCSResponse = class(TInterfacedObject, IMARSResponse)
  private
    FDCSResponse: ICrossHttpResponse;
  public
    // IMARSResponse -----------------------------------------------------------
    function GetContent: string;
    function GetContentEncoding: string;
    function GetContentLength: Integer;
    function GetContentStream: TStream;
    function GetContentType: string;
    function GetStatusCode: Integer;
    procedure SetContent(const AContent: string);
    procedure SetContentEncoding(const AContentEncoding: string);
    procedure SetContentLength(const ALength: Integer);
    procedure SetContentStream(const AContentStream: TStream);
    procedure SetContentType(const AContentType: string);
    procedure SetHeader(const AName: string; const AValue: string);
    procedure SetStatusCode(const AStatusCode: Integer);
    procedure SetCookie(const AName, AValue, ADomain, APath: string; const AExpiration: TDateTime; const ASecure: Boolean);
    procedure RedirectTo(const AURL: string);
    // -------------------------------------------------------------------------
    constructor Create(ADCSResponse: ICrossHttpResponse); virtual;
  end;


  TMARShttpServerDCS = class
  private
    FStoppedAt: TDateTime;
    FEngine: TMARSEngine;
    FStartedAt: TDateTime;
    FActive: Boolean;
    FDefaultPort: Integer;
    FHttpServer: ICrossHttpServer;
    function GetUpTime: TTimeSpan;
    procedure SetActive(const Value: Boolean);
    procedure SetDefaultPort(const Value: Integer);
  protected
    procedure Startup; virtual;
    procedure Shutdown; virtual;
  public
    constructor Create(AEngine: TMARSEngine); virtual;
    destructor Destroy; override;

    property Active: Boolean read FActive write SetActive;
    property DefaultPort: Integer read FDefaultPort write SetDefaultPort;
    property Engine: TMARSEngine read FEngine;
    property StartedAt: TDateTime read FStartedAt;
    property StoppedAt: TDateTime read FStoppedAt;
    property UpTime: TTimeSpan read GetUpTime;
  end;

implementation

uses
  Net.CrossHttpParams;

{ TMARShttpServerDCS }

constructor TMARShttpServerDCS.Create(AEngine: TMARSEngine);
begin
  inherited Create;
  FEngine := AEngine;
  FHttpServer := TCrossHttpServer.Create(0);
end;

destructor TMARShttpServerDCS.Destroy;
begin
  FHttpServer := nil;
  inherited;
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
    FActive := Value;
    if FActive then
      Startup
    else
      Shutdown;
  end;
end;

procedure TMARShttpServerDCS.SetDefaultPort(const Value: Integer);
begin
  FDefaultPort := Value;
end;

procedure TMARShttpServerDCS.Shutdown;
begin
  FHttpServer.Stop;

  FStoppedAt := Now;
end;

procedure TMARShttpServerDCS.Startup;
begin
  FHttpServer.Addr := IPv4v6_ALL; // IPv4v6
  FHttpServer.Port := DefaultPort;
  FHttpServer.Compressible := True;

  FHttpServer
//  .Get('/hello',
//    procedure(ARequest: ICrossHttpRequest; AResponse: ICrossHttpResponse)
//    begin
//      AResponse.Send('Hello World');
//    end)
  .All(FEngine.BasePath + '*',
    procedure(ARequest: ICrossHttpRequest; AResponse: ICrossHttpResponse; var AHandled: Boolean)
    begin
      AHandled := FEngine.HandleRequest(TMARSDCSRequest.Create(ARequest), TMARSDCSResponse.Create(AResponse))
    end
  );

  FHttpServer.Start;

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
//AM TODO
  Result := '';
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
begin
//AM TODO controllare
  Result := FDCSRequest.Query.ToString;
end;

function TMARSDCSRequest.GetRawContent: TBytes;
begin
  Result := [];
  case FDCSRequest.BodyType of
//    btNone: Result := [];
//    btUrlEncoded: ;
//    btMultiPart: Result := THttpMultiPartFormData(FDCSRequest.Body).Bytes;
    btBinary: Result := TBytesStream(FDCSRequest.Body).Bytes;
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

{ TMARSDCSResponse }

constructor TMARSDCSResponse.Create(ADCSResponse: ICrossHttpResponse);
begin
  inherited Create;
  FDCSResponse := ADCSResponse;
end;

function TMARSDCSResponse.GetContent: string;
begin
//AM TODO
  Result := '';
end;

function TMARSDCSResponse.GetContentEncoding: string;
begin
//AM TODO
  Result := '';
end;

function TMARSDCSResponse.GetContentLength: Integer;
begin
  Result := -1;
end;

function TMARSDCSResponse.GetContentStream: TStream;
begin
//AM TODO
  Result := nil;
end;

function TMARSDCSResponse.GetContentType: string;
begin
  Result := FDCSResponse.ContentType;
end;

function TMARSDCSResponse.GetStatusCode: Integer;
begin
  Result := FDCSResponse.StatusCode;
end;

procedure TMARSDCSResponse.RedirectTo(const AURL: string);
begin
  FDCSResponse.Redirect(AURL);
end;

procedure TMARSDCSResponse.SetContent(const AContent: string);
begin
  FDCSResponse.Send(AContent);
end;

procedure TMARSDCSResponse.SetContentEncoding(const AContentEncoding: string);
begin
//AM TODO
end;

procedure TMARSDCSResponse.SetContentLength(const ALength: Integer);
begin
  // unsupported
end;

procedure TMARSDCSResponse.SetContentStream(const AContentStream: TStream);
begin
  FDCSResponse.Send(AContentStream);
end;

procedure TMARSDCSResponse.SetContentType(const AContentType: string);
begin
  FDCSResponse.ContentType := AContentType;
end;

procedure TMARSDCSResponse.SetCookie(const AName, AValue, ADomain,
  APath: string; const AExpiration: TDateTime; const ASecure: Boolean);
begin
  FDCSResponse.Cookies.AddOrSet(AName, AValue, SecondsBetween(Now, AExpiration), APath, ADomain, False {AHttpOnly}, ASecure);
end;

procedure TMARSDCSResponse.SetHeader(const AName, AValue: string);
begin
  FDCSResponse.Header.Add(AName, AValue);
end;

procedure TMARSDCSResponse.SetStatusCode(const AStatusCode: Integer);
begin
  FDCSResponse.StatusCode := AStatusCode;
end;

end.
