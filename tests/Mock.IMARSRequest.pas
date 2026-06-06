unit Mock.IMARSRequest;

interface

uses
  Classes, SysUtils, System.Generics.Collections
, MARS.Core.RequestAndResponse.Interfaces
, MARS.Core.URL
;

type
  TMARSRequestMock = class(TInterfacedObject, IMARSRequest)
  private
    FActualURL: string;
    FURL: TMARSURL;
    FHeaders: TMARSHeaders;
    FQueryParams: TMARSQueryParams;
    FRawContent: TBytes;
    FContent: string;
    FMethod: string;
  protected
  public
    // IMARSRequest ------------------------------------------------- BEGIN ----
    function GetRawContent: TBytes;
    function GetContent: string;
    // Query params
    function GetQueryParamIndex(const AName: string): Integer;
    function GetQueryParamName(const AIndex: integer): string;
    function GetQueryParamValue(const AIndex: Integer): string; overload;
    function GetQueryParamValue(const AName: string): string; overload;
    function GetQueryParamCount: Integer;
    function GetQueryParams: TMARSQueryParams;

    // Form params
    function GetFormParamIndex(const AName: string): Integer;
    function GetFormParamName(const AIndex: Integer): string;
    function GetFormParamValue(const AIndex: Integer): string; overload;
    function GetFormParamValue(const AName: string): string; overload;
    function GetFormFileParamIndex(const AName: string): Integer;
    function GetFormFileParam(const AIndex: Integer; out AFieldName, AFileName: string;
      out ABytes: TBytes; out AContentType: string): Boolean;
    function GetFormParamCount: Integer;
    function GetFilesCount: Integer;
    function GetFormParams: string;

    // Header params
    function GetHeaderParamCount: Integer;
    function GetHeaderParamIndex(const AName: string): Integer;
    function GetHeaderParamName(const AIndex: Integer): string;
    function GetHeaderParamValue(const AHeaderName: string): string; overload;
    function GetHeaderParamValue(const AIndex: Integer): string; overload;
    function GetHeaders: TMARSHeaders;

    // Cookie params
    function GetCookieParamIndex(const AName: string): Integer;
    function GetCookieParamValue(const AIndex: Integer): string; overload;
    function GetCookieParamValue(const AName: string): string; overload;
    function GetCookieParamCount: Integer;
    function GetCookies: TMARSCookies;

    function GetAccept: string;
    function GetAuthorization: string;
    function GetMethod: string;
    function GetQueryString: string;
    function GetHostName: string;
    function GetPort: Integer;
    function GetRawPath: string;
    function GetDate: TDateTime;
    function GetContentFields: TArray<string>;
    function GetQueryFields: TArray<string>;
    function GetRemoteIP: string;
    function GetUserAgent: string;

    function AsObject: TObject;
    procedure CheckWorkaroundForISAPI;
    // IMARSRequest --------------------------------------------------- END ----
    constructor Create; overload;
    constructor Create(const AMethod, AActualURL: string; const AHeaders: TMARSHeaders; const ABody: TBytes); overload;
    constructor Create(const AMethod, AActualURL: string; const AHeaders: TMARSHeaders; const ABody: string); overload;
    destructor Destroy; override;

  end;

implementation

{ TMARSRequestMock }

function TMARSRequestMock.AsObject: TObject;
begin
  Result := Self;
end;

procedure TMARSRequestMock.CheckWorkaroundForISAPI;
begin
  // do nothing
end;

constructor TMARSRequestMock.Create;
begin
  Create('GET', '', [], '');
end;

constructor TMARSRequestMock.Create(const AMethod, AActualURL: string;
  const AHeaders: TMARSHeaders; const ABody: TBytes);
begin
  Create(AMethod, AActualURL, AHeaders, TEncoding.UTF8.GetString(ABody));
end;


constructor TMARSRequestMock.Create(const AMethod, AActualURL: string;
  const AHeaders: TMARSHeaders; const ABody: string);
begin
  inherited Create;

  FMethod := AMethod;
  FActualURL := AActualURL;
  FHeaders := AHeaders;
  FRawContent := TEncoding.UTF8.GetBytes(ABody);
  FContent := ABody;
  FQueryParams := [];

  FURL := TMARSURL.Create(AActualURL);
  const LQueryTokens = FURL.QueryTokens.ToArray;
  for var LIndex := Low(LQueryTokens) to High(LQueryTokens) do
    FQueryParams := FQueryParams + [TMARSQueryParam.Create(LQueryTokens[LIndex].Key, LQueryTokens[LIndex].Value)];
end;

destructor TMARSRequestMock.Destroy;
begin
  FreeAndNil(FURL);
  inherited;
end;

function TMARSRequestMock.GetAccept: string;
begin
  Result := GetHeaderParamValue('Accept');
end;

function TMARSRequestMock.GetAuthorization: string;
begin
  Result := GetHeaderParamValue('Authorization');
end;

function TMARSRequestMock.GetContent: string;
begin
  Result := FContent;
end;

function TMARSRequestMock.GetContentFields: TArray<string>;
begin
  Result := [];
end;

function TMARSRequestMock.GetCookieParamCount: Integer;
begin
  Result := 0;
end;

function TMARSRequestMock.GetCookieParamIndex(const AName: string): Integer;
begin
  Result := -1;
end;

function TMARSRequestMock.GetCookieParamValue(const AIndex: Integer): string;
begin
  Result := '';
end;

function TMARSRequestMock.GetCookieParamValue(const AName: string): string;
begin
  Result := '';
end;

function TMARSRequestMock.GetCookies: TMARSCookies;
begin
  Result := [];
end;

function TMARSRequestMock.GetDate: TDateTime;
begin
  Result := StrToDateTime(GetHeaderParamValue('Date'));
end;

function TMARSRequestMock.GetFilesCount: Integer;
begin
  Result := 0;
end;

function TMARSRequestMock.GetFormFileParam(const AIndex: Integer;
  out AFieldName, AFileName: string; out ABytes: TBytes;
  out AContentType: string): Boolean;
begin
  Result := False;
end;

function TMARSRequestMock.GetFormFileParamIndex(const AName: string): Integer;
begin
  Result := -1;
end;

function TMARSRequestMock.GetFormParamCount: Integer;
begin
  Result := 0;
end;

function TMARSRequestMock.GetFormParamIndex(const AName: string): Integer;
begin
  Result := -1;
end;

function TMARSRequestMock.GetFormParamName(const AIndex: Integer): string;
begin
  Result := '';
end;

function TMARSRequestMock.GetFormParams: string;
begin
  Result := '';
end;

function TMARSRequestMock.GetFormParamValue(const AIndex: Integer): string;
begin
  Result := '';
end;

function TMARSRequestMock.GetFormParamValue(const AName: string): string;
begin
  Result := '';
end;

function TMARSRequestMock.GetHeaderParamCount: Integer;
begin
  Result := Length(FHeaders);
end;

function TMARSRequestMock.GetHeaderParamIndex(const AName: string): Integer;
begin
  Result := -1;
  for var LIndex := Low(FHeaders) to High(FHeaders) do
    if SameText(FHeaders[LIndex].Name, AName) then
    begin
      Result := LIndex;
      Exit;
    end;
end;

function TMARSRequestMock.GetHeaderParamName(const AIndex: Integer): string;
begin
  Result := '';
  if (AIndex >= Low(FHeaders)) and (AIndex <= High(FHeaders)) then
    Result := FHeaders[AIndex].Name;
end;

function TMARSRequestMock.GetHeaderParamValue(
  const AHeaderName: string): string;
begin
  var LIndex := GetHeaderParamIndex(AHeaderName);
  if LIndex <> -1 then
    Result := FHeaders[LIndex].Value
  else
    Result := '';
end;

function TMARSRequestMock.GetHeaderParamValue(const AIndex: Integer): string;
begin
  Result := '';
  if (AIndex >= Low(FHeaders)) and (AIndex <= High(FHeaders)) then
    Result := FHeaders[AIndex].Value;
end;

function TMARSRequestMock.GetHeaders: TMARSHeaders;
begin
  Result := FHeaders;
end;

function TMARSRequestMock.GetHostName: string;
begin
  Result := GetHeaderParamValue('Host');
end;

function TMARSRequestMock.GetMethod: string;
begin
  Result := FMethod;
end;

function TMARSRequestMock.GetPort: Integer;
begin
  Result := FURL.PortNumber;
end;

function TMARSRequestMock.GetQueryFields: TArray<string>;
begin
  Result := [];
  for var LIndex := Low(FQueryParams) to High(FQueryParams) do
    Result := Result + [FQueryParams[LIndex].Name];
end;

function TMARSRequestMock.GetQueryParamCount: Integer;
begin
  Result := Length(FQueryParams);
end;

function TMARSRequestMock.GetQueryParamIndex(const AName: string): Integer;
begin
  Result := -1;
  for var LIndex := Low(FQueryParams) to High(FQueryParams) do
    if SameText(FQueryParams[LIndex].Name, AName) then
    begin
      Result := LIndex;
      Break;
    end;
end;

function TMARSRequestMock.GetQueryParamName(const AIndex: integer): string;
begin
  if (AIndex >= Low(FQueryParams)) and (AIndex <= High(FQueryParams)) then
    Result := FQueryParams[AIndex].Name
  else
    Result := '';
end;

function TMARSRequestMock.GetQueryParams: TMARSQueryParams;
begin
  Result := FQueryParams;
end;

function TMARSRequestMock.GetQueryParamValue(const AName: string): string;
begin
  Result := '';
  for var LIndex := Low(FQueryParams) to High(FQueryParams) do
    if SameText(FQueryParams[LIndex].Name, AName) then
    begin
      Result := FQueryParams[LIndex].Value;
      Break;
    end;
end;

function TMARSRequestMock.GetQueryParamValue(const AIndex: Integer): string;
begin
  if (AIndex >= Low(FQueryParams)) and (AIndex <= High(FQueryParams)) then
    Result := FQueryParams[AIndex].Value
  else
    Result := '';
end;

function TMARSRequestMock.GetQueryString: string;
begin
  Result := FURL.Query;
end;

function TMARSRequestMock.GetRawContent: TBytes;
begin
  Result := FRawContent;
end;

function TMARSRequestMock.GetRawPath: string;
begin
  Result := FURL.Path;
end;

function TMARSRequestMock.GetRemoteIP: string;
begin
  Result := '127.0.0.1';
end;

function TMARSRequestMock.GetUserAgent: string;
begin
  Result := GetHeaderParamValue('User-Agent');
end;

end.
