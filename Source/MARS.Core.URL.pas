(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS

  Contributors:
    Nando Dessena <nando(at)dessena(dot)it>
*)
unit MARS.Core.URL;

{$I MARS.inc}

interface

uses
    Classes, SysUtils
  , MARS.Core.JSON
  , HTTPApp
  , Generics.Collections
;

type

  TMARSURL = class
  private
    FURL: string;
    FPath: string;
    FDocument: string;
    FPortNumber: Integer;
    FProtocol: string;
    FQuery: string;
    FPassword: string;
    FHostName: string;
    FUserName: string;
    FPathTokens: TArray<string>;
    FQueryTokens: TDictionary<string, string>;

    FResource: string;
    FSubResources: TDictionary<Integer, string>;
    FPathParams: TDictionary<Integer, string>;
    FBasePath: string;

    procedure SetURL(const Value: string);

    function GetHasSubResources: Boolean;
    function GetHasPathParams: Boolean;
    procedure SetBasePath(const Value: string);
  protected
    procedure Parse; virtual;
    function ParsePathTokens(const APath: string): TArray<string>; virtual;
    procedure ParseQueryTokens; virtual;
    procedure URLChanged; virtual;
    procedure BasePathChanged; virtual;
  public
    const URL_PATH_SEPARATOR = '/';
    const URL_QUERY_SEPARATOR = '&';
    const DUMMY_URL = 'http://localhost:1234/';
    const PATH_PARAM_WILDCARD = '{*}';

    constructor Create(const AURL: string); overload; virtual;
    constructor CreateDummy(const APath: string; const ABaseURL: string = DUMMY_URL); overload; virtual;
    constructor CreateDummy(const APaths: array of string; const ABaseURL: string = DUMMY_URL); overload; virtual;
    constructor Create(AWebRequest: TWebRequest); overload; virtual;
    destructor Destroy; override;

    function MatchPath(AOtherURL: TMARSURL; const ACaseSensitive: Boolean = False): Boolean; overload; virtual;
    function MatchPath(APath: string; const ACaseSensitive: Boolean = False): Boolean; overload; virtual;

    function HasPathTokens(const AtLeast: Integer = 1): Boolean;

    function SubResourcesToArray: TArray<string>;
    function ToString: string; override;
    function ToJSON: string; virtual;
    function ToJSONObject: TJSONObject; virtual;
    function GetPathParamIndex(const AParam: string): Integer; virtual;
    property URL: string read FURL write SetURL;

    // protocollo://<username:password@>nomehost<:porta></percorso><?querystring>
    property Protocol: string read FProtocol;
    property UserName: string read FUserName;
    property Password: string read FPassword;
    property HostName: string read FHostName;
    property PortNumber: Integer read FPortNumber;
    property Path: string read FPath;
    property Document: string read FDocument;
    property PathTokens: TArray<string> read FPathTokens;
    property Query: string read FQuery;
    property QueryTokens: TDictionary<string, string> read FQueryTokens;
    function QueryTokenByName(const AName: string; const ACaseInsensitive: Boolean = True;
      const AExceptionIfNotFound: Boolean = True): string; virtual;

    property BasePath: string read FBasePath write SetBasePath;
    property Resource: string read FResource write FResource;
    property SubResources: TDictionary<Integer, string> read FSubResources;
    property PathParams: TDictionary<Integer, string> read FPathParams;
    property HasSubResources: Boolean read GetHasSubResources;
    property HasPathParams: Boolean read GetHasPathParams;

    class function CombinePath(const APathTokens: array of string;
      const AEnsureFirst: Boolean = False; const AEnsureLast: Boolean = False): string;
    class function EnsureLastPathDelimiter(const APath: string): string;
    class function EnsureFirstPathDelimiter(const APath: string): string;

    class function URLEncode(const AString: string): string; overload;
    class function URLEncode(const AStrings: TArray<string>): TArray<string>; overload;
    class function URLDecode(const AString: string): string; overload;
    class function URLDecode(const AStrings: TArray<string>): TArray<string>; overload;
  end;

implementation

uses
    StrUtils

  , MARS.Core.Utils
  , MARS.Core.Exceptions
  , IdURI
  ;

{ TMARSURL }

constructor TMARSURL.Create(const AURL: string);
begin
  inherited Create;

  // init
  FURL := '';
  FPath := '';

  {$ifndef DelphiXE7_UP}
  SetLength(FPathTokens, 0);
  {$else}
  FPathTokens := [];
  {$endif}

  FSubResources := TDictionary<Integer, string>.Create;
  FPathParams := TDictionary<Integer, string>.Create;

  FPortNumber := 0;
  FProtocol := '';
  FQuery := '';
  FQueryTokens := TDictionary<string, string>.Create;
  FPassword := '';
  FHostName := '';
  FUserName := '';
  FResource := '';

  // set value
  URL := AURL;
end;

procedure TMARSURL.BasePathChanged;
var
  LToken: string;
  LIndex: Integer;
  LRemainingPath: string;
  LTokens: TArray<string>;
begin
  FResource := '';
  FSubResources.Clear;
  FPathParams.Clear;

  if StartsText(FBasePath, FPath) then
  begin
    LRemainingPath := FPath;
    Delete(LRemainingPath, 1, Length(FBasePath));
    LTokens := ParsePathTokens(LRemainingPath);

    if Length(LTokens) > 0 then
    begin
      FResource := LTokens[0];
      for LIndex := 0 to Length(LTokens) -1 do
      begin
        LToken := LTokens[LIndex];
        if StartsStr('{', LToken) then
        begin
          LToken := Copy(LToken, 2);
          if EndsStr('}', LToken) then
            Delete(LToken, Length(LToken), 1);
          FPathParams.Add(LIndex, LToken);
        end
        else
        begin
          if LIndex > 0 then
            FSubResources.Add(LIndex, LToken);
        end;
      end;
    end;
  end;
end;

class function TMARSURL.CombinePath(const APathTokens: array of string;
  const AEnsureFirst: Boolean; const AEnsureLast: Boolean): string;
begin
  Result := SmartConcat(APathTokens, URL_PATH_SEPARATOR);

  if AEnsureFirst then
    Result := EnsureFirstPathDelimiter(Result);

  if AEnsureLast then
    Result := EnsureLastPathDelimiter(Result);
end;

constructor TMARSURL.Create(AWebRequest: TWebRequest);
var
  LQuery: string;
begin
  LQuery := string(AWebRequest.Query);
  if LQuery <> '' then
    LQuery := '?' + LQuery;

  // Add the protocol in order to make Parse work.
  Create('http://' + string(AWebRequest.Host) + ':' + IntToStr(AWebRequest.ServerPort) + string(AWebRequest.PathInfo) + LQuery);
end;

constructor TMARSURL.CreateDummy(const APaths: array of string; const ABaseURL: string);
begin
  Create(CombinePath([ABaseURL, CombinePath(APaths)]));
end;

constructor TMARSURL.CreateDummy(const APath: string; const ABaseURL: string);
begin
  Create(CombinePath([ABaseURL, APath]));
end;

destructor TMARSURL.Destroy;
begin
  FPathParams.Free;
  FSubResources.Free;
  FQueryTokens.Free;
  inherited;
end;

class function TMARSURL.EnsureFirstPathDelimiter(const APath: string): string;
begin
  Result := EnsurePrefix(APath, URL_PATH_SEPARATOR);
end;

class function TMARSURL.EnsureLastPathDelimiter(const APath: string): string;
begin
  Result := EnsureSuffix(APath, URL_PATH_SEPARATOR);
end;

function TMARSURL.GetHasPathParams: Boolean;
begin
  Result := FPathParams.Count > 0;
end;

function TMARSURL.GetHasSubResources: Boolean;
begin
  Result := FSubResources.Count > 0;
end;

function TMARSURL.GetPathParamIndex(const AParam: string): Integer;
var
  LPair: TPair<Integer, string>;
begin
  Result := -1;
  for LPair in PathParams do
  begin
    if SameText(AParam, LPair.Value) then
    begin
      Result := LPair.Key;
      Break;
    end;
  end;
end;

function TMARSURL.HasPathTokens(const AtLeast: Integer): Boolean;
begin
  Result := Length(FPathTokens) >= AtLeast ;
end;

function TMARSURL.MatchPath(APath: string; const ACaseSensitive: Boolean): Boolean;
begin
  if ACaseSensitive then
    Result := StartsStr(APath, Path)
  else
    Result := StartsText(APath, Path);
end;

function TMARSURL.MatchPath(AOtherURL: TMARSURL; const ACaseSensitive: Boolean): Boolean;
var
  LIndex: Integer;
  LToken, LOtherToken: string;
  LMatches: Boolean;
begin
  Result := (Length(PathTokens) = Length(AOtherURL.PathTokens))
    or (PathTokens[Length(PathTokens)-1] = PATH_PARAM_WILDCARD);

  if Result then
  begin
    for LIndex := 0 to Length(PathTokens)-1 do
    begin
      LToken := PathTokens[LIndex];
      LOtherToken := AOtherURL.PathTokens[LIndex];

      if ACaseSensitive then
        LMatches := LToken = LOtherToken
      else
        LMatches := SameText(LToken, LOtherToken);

      if not (
        LMatches
        or (StartsStr('{', LToken) and EndsStr('}', LToken)) // LToken is a param
        or (StartsStr('{', LOtherToken) and EndsStr('}', LOtherToken)) // LOtherToken is a param
      ) then
        Result := False;
    end;
  end;
end;

procedure TMARSURL.Parse;
var
  LDefaultPortNumber: Integer;
  LURI: TIdURI;
begin
  LURI := TIdURI.Create(FURL);
  try
    FProtocol := LURI.Protocol;
    { TODO -oAndrea : Generalizzare le default port per i vari protocolli }
    if SameText(FProtocol, '') or SameText(FProtocol, 'http') then
      LDefaultPortNumber := 80
    else if SameText(FProtocol, 'https') then
      LDefaultPortNumber := 443
    else
      LDefaultPortNumber := 0;
    FUserName := LURI.Username;
    FPassword := LURI.Password;
    FHostName := LURI.Host;
    FPortNumber := StrToIntDef(LURI.Port, LDefaultPortNumber);
    FDocument := LURI.Document;
    FPath := LURI.Path + LURI.Document;
    FPathTokens := ParsePathTokens(FPath);
    FQuery := LURI.Params;
    ParseQueryTokens;
    BasePathChanged;
  finally
    FreeAndNil(LURI);
  end;
end;

function TMARSURL.ParsePathTokens(const APath: string): TArray<string>;
var
  LPath: string;
begin
  LPath := EnsureFirstPathDelimiter(EnsureLastPathDelimiter(APath));
  Result := TArray<string>(SplitString(LPath, URL_PATH_SEPARATOR));

  while (Length(Result) > 0) and (Result[0] = '') do
    Result := Copy(Result, 1);
  while (Length(Result) > 0) and (Result[High(Result)] = '') do
    SetLength(Result, High(Result));
end;

procedure TMARSURL.ParseQueryTokens;
var
  LQuery: string;
  LStrings: TStringList;
  LIndex: Integer;
begin
  FQueryTokens.Clear;

  if FQuery <> '' then
  begin
    LQuery := URLDecode(FQuery);
    while StartsStr(LQuery, '?') do
      LQuery := RightStr(LQuery, Length(LQuery) - 1);

    LStrings := TStringList.Create;
    try
      LStrings.Delimiter := URL_QUERY_SEPARATOR;
      LStrings.StrictDelimiter := True;
      LStrings.DelimitedText := LQuery;
      for LIndex := 0 to LStrings.Count - 1 do
        FQueryTokens.Add(LStrings.Names[LIndex], LStrings.ValueFromIndex[LIndex]);
    finally
      LStrings.Free;
    end;
  end;
end;

function TMARSURL.QueryTokenByName(const AName: string;
  const ACaseInsensitive: Boolean; const AExceptionIfNotFound: Boolean): string;
var
  LCurrentKey: string;
  LKeys: TArray<string>;
  LIndex: Integer;
  LFound: Boolean;
begin
  LFound := False;
  Result := '';

  LKeys := QueryTokens.Keys.ToArray;
  for LIndex := Low(LKeys) to High(LKeys) do
  begin
    LCurrentKey := LKeys[LIndex];

    if (AName = LCurrentKey) or (ACaseInsensitive and SameText(AName, LCurrentKey)) then
    begin
      QueryTokens.TryGetValue(LCurrentKey, Result);
      LFound := True;
      Break;
    end;
  end;

  if (not LFound) and AExceptionIfNotFound then
    raise EMARSException.CreateFmt('QueryParam not found: %s', [AName]);
end;

procedure TMARSURL.SetBasePath(const Value: string);
begin
  if FBasePath <> Value then
  begin
    FBasePath := EnsureFirstPathDelimiter(EnsureLastPathDelimiter(Value));
    BasePathChanged;
  end;
end;

procedure TMARSURL.SetURL(const Value: string);
begin
  if FURL <> Value then
  begin
    FURL := Value;
    URLChanged;
  end;
end;

function TMARSURL.SubResourcesToArray: TArray<string>;
var
  LKeys: TArray<Integer>;
  LIndex: Integer;
begin
  LKeys := FSubResources.Keys.ToArray;
  TArray.Sort<Integer>(LKeys);
  SetLength(Result, Length(LKeys));
  for LIndex := Low(LKeys) to High(LKeys) do
    Result[LIndex] := FSubResources.Items[LKeys[LIndex]];
end;

function TMARSURL.ToJSON: string;
var
  LObj: TJSONObject;
begin
  LObj := ToJSONObject;
  try
    Result := LObj.ToJSON;
  finally
    LObj.Free;
  end;
end;

function TMARSURL.ToJSONObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('URL', FURL);
  Result.AddPair('Protocol', FProtocol);
  Result.AddPair('UserName', FUserName);
  Result.AddPair('Password', FPassword);
  Result.AddPair('HostName', FHostName);
  Result.AddPair('PortNumber', TJSONNumber.Create(FPortNumber));
  Result.AddPair('Path', FPath);
  Result.AddPair('Query', FQuery);
end;

function TMARSURL.ToString: string;
begin
  Result := Format(
      'URL: %s' + sLineBreak
    + 'Protocol: %s' + sLineBreak
    + 'UserName: %s' + sLineBreak
    + 'Password: %s' + sLineBreak
    + 'HostName: %s' + sLineBreak
    + 'PortNumber: %d' + sLineBreak
    + 'Path: %s' + sLineBreak
    + 'PathTokens count: %d' + sLineBreak
    + 'Query: %s' + sLineBreak
    + 'QueryTokens count: %d' + sLineBreak
    + 'Resource: %s' + sLineBreak
    + 'SubResources count: %d'
    + 'PathParams count: %d'
  , [
    FURL
    , FProtocol
    , FUserName
    , FPassword
    , FHostName
    , FPortNumber
    , FPath
    , Length(FPathTokens)
    , FQuery
    , FQueryTokens.Count
    , FResource
    , FSubResources.Count
    , FPathParams.Count
  ]
  );
end;

procedure TMARSURL.URLChanged;
begin
  Parse;
end;

class function TMARSURL.URLDecode(const AString: string): string;
begin
//  Result := TNetEncoding.URL.Decode(AString);
  Result := TIdURI.URLDecode(AString);
end;

class function TMARSURL.URLDecode(const AStrings: TArray<string>): TArray<string>;
var
  LIndex: Integer;
begin
  Result := AStrings; // copy on write

  // encode each result item
  for LIndex := 0 to Length(Result)-1 do
    Result[LIndex] := URLDecode(Result[LIndex]);
end;

class function TMARSURL.URLEncode(const AString: string): string;
begin
//  Result := TNetEncoding.URL.Encode(AString);
  Result := TIdURI.PathEncode(AString);
end;

class function TMARSURL.URLEncode(const AStrings: TArray<string>): TArray<string>;
var
  LIndex: Integer;
begin
  Result := AStrings; // copy on write

  // encode each result item
  for LIndex := 0 to Length(Result)-1 do
    Result[LIndex] := URLEncode(Result[LIndex]);
end;

end.
