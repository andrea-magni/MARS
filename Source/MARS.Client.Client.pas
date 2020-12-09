(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.Client;

{$I MARS.inc}

interface

uses
  SysUtils, Classes
, MARS.Core.JSON
, MARS.Core.Utils
, MARS.Core.MediaType
, MARS.Client.Utils
, MARS.Utils.Parameters
;

type
  TMARSAuthEndorsement = (Cookie, AuthorizationBearer);
  TMARSHttpVerb = (Get, Put, Post, Head, Delete, Patch);
  TMARSClientErrorEvent = procedure (
    AResource: TObject; AException: Exception; AVerb: TMARSHttpVerb;
    const AAfterExecute: TMARSClientResponseProc; var AHandled: Boolean) of object;

  TMARSCustomClient = class; // fwd
  TMARSCustomClientClass = class of TMARSCustomClient;

  TMARSClientBeforeExecuteProc = reference to procedure (const AURL: string;
    const AClient: TMARSCustomClient);

  TMARSProxyConfig = class(TPersistent)
  private
    FPort: Integer;
    FPassword: string;
    FHost: string;
    FUserName: string;
    FEnabled: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
  end;

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TMARSCustomClient = class(TComponent)
  private
    FMARSEngineURL: string;
    FOnError: TMARSClientErrorEvent;
    FAuthEndorsement: TMARSAuthEndorsement;
    FProxyConfig: TMARSProxyConfig;
    FAuthToken: string;
    FAuthCookieName: string;
    procedure SetProxyConfig(const Value: TMARSProxyConfig);
  protected
    class var FBeforeExecuteProcs: TArray<TMARSClientBeforeExecuteProc>;
    class procedure FireBeforeExecute(const AURL: string; const AClient: TMARSCustomClient);
  protected
    procedure AssignTo(Dest: TPersistent); override;

    function GetConnectTimeout: Integer; virtual;
    function GetReadTimeout: Integer; virtual;
    procedure SetConnectTimeout(const Value: Integer); virtual;
    procedure SetReadTimeout(const Value: Integer); virtual;
    procedure SetAuthEndorsement(const Value: TMARSAuthEndorsement);

    procedure ApplyProxyConfig; virtual;
    procedure EndorseAuthorization; virtual;
    procedure AuthEndorsementChanged; virtual;
    procedure BeforeExecute; virtual;

    property AuthToken: string read FAuthToken;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CloneSetup(const ASource: TMARSCustomClient); virtual;
//    procedure CloneStatus(const ASource: TMARSClientCustomClient); virtual;

    procedure ApplyCustomHeaders(const AHeaders: TStrings); virtual;
    procedure DoError(const AResource: TObject; const AException: Exception;
      const AVerb: TMARSHttpVerb; const AAfterExecute: TMARSClientResponseProc); virtual;

    procedure Delete(const AURL: string; AContent, AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); virtual;
    procedure Get(const AURL: string; AResponseContent: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); virtual;
    procedure Post(const AURL: string; AContent, AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); overload; virtual;
    procedure Post(const AURL: string; const AFormData: TArray<TFormParam>;
      const AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); overload; virtual;
    procedure Post(const AURL: string; const AFormUrlEncoded: TMARSParameters;
      const AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); overload; virtual;
    procedure Put(const AURL: string; AContent, AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); overload; virtual;
    procedure Put(const AURL: string; const AFormData: TArray<TFormParam>;
      const AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); overload; virtual;
    procedure Put(const AURL: string; const AFormUrlEncoded: TMARSParameters;
      const AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); overload; virtual;

    function LastCmdSuccess: Boolean; virtual;
    function ResponseStatusCode: Integer; virtual;
    function ResponseText: string; virtual;

    // shortcuts
    class function GetJSON<T: TJSONValue>(const AEngineURL, AAppName, AResourceName: string;
      const AToken: string = ''): T; overload;

    class function GetJSON<T: TJSONValue>(const AEngineURL, AAppName, AResourceName: string;
      const APathParams: TArray<string>; const AQueryParams: TStrings;
      const AToken: string = '';
      const AIgnoreResult: Boolean = False): T; overload;

{$ifdef DelphiXE7_UP}
    class procedure GetJSONAsync<T: TJSONValue>(const AEngineURL, AAppName, AResourceName: string;
      const APathParams: TArray<string>; const AQueryParams: TStrings;
      const ACompletionHandler: TProc<T>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AToken: string = '';
      const ASynchronize: Boolean = True); overload;
{$endif}

    class function GetAsString(const AURL: string;
      const AToken: string = ''; const AAccept: string = TMediaType.WILDCARD): string; overload;

    class function GetAsString(const AEngineURL, AAppName, AResourceName: string;
      const APathParams: TArray<string>; const AQueryParams: TStrings = nil;
      const AToken: string = ''; const AAccept: string = TMediaType.WILDCARD): string; overload;

    class function PostJSON(const AEngineURL, AAppName, AResourceName: string;
      const APathParams: TArray<string>; const AQueryParams: TStrings;
      const AContent: TJSONValue;
      const ACompletionHandler: TProc<TJSONValue>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AToken: string = ''
    ): Boolean;

{$ifdef DelphiXE7_UP}
    class procedure PostJSONAsync(const AEngineURL, AAppName, AResourceName: string;
      const APathParams: TArray<string>; const AQueryParams: TStrings;
      const AContent: TJSONValue;
      const ACompletionHandler: TProc<TJSONValue>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AToken: string = '';
      const ASynchronize: Boolean = True);
{$endif}

    class function GetStream(const AEngineURL, AAppName, AResourceName: string;
      const AToken: string = ''): TStream; overload;

    class function GetStream(const AEngineURL, AAppName, AResourceName: string;
      const APathParams: TArray<string>; const AQueryParams: TStrings;
      const AToken: string = ''): TStream; overload;

    class function PostStream(const AEngineURL, AAppName, AResourceName: string;
      const APathParams: TArray<string>; const AQueryParams: TStrings;
      const AContent: TStream; const AToken: string = ''): Boolean;

    class function RegisterBeforeExecute(const ABeforeExecute: TMARSClientBeforeExecuteProc): Integer;
    class procedure UnregisterBeforeExecute(const AIndex: Integer);
    class procedure ClearBeforeExecute;
  published
    property MARSEngineURL: string read FMARSEngineURL write FMARSEngineURL;
    property ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout;
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
    property OnError: TMARSClientErrorEvent read FOnError write FOnError;
    property AuthEndorsement: TMARSAuthEndorsement read FAuthEndorsement write SetAuthEndorsement default TMARSAuthEndorsement.Cookie;
    property AuthCookieName: string read FAuthCookieName write FAuthCookieName;
    property ProxyConfig: TMARSProxyConfig read FProxyConfig write SetProxyConfig;
  end;

function TMARSHttpVerbToString(const AVerb: TMARSHttpVerb): string;

implementation

uses
  Rtti, TypInfo
, MARS.Core.URL
, MARS.Client.CustomResource
, MARS.Client.Resource
, MARS.Client.Resource.JSON
, MARS.Client.Resource.Stream
, MARS.Client.Application
;

function TMARSHttpVerbToString(const AVerb: TMARSHttpVerb): string;
begin
{$ifdef DelphiXE7_UP}
  Result := TRttiEnumerationType.GetName<TMARSHttpVerb>(AVerb);
{$else}
  Result := GetEnumName(TypeInfo(TMARSHttpVerb), Integer(AVerb));
{$endif}
end;

{ TMARSCustomClient }

procedure TMARSCustomClient.ApplyCustomHeaders(const AHeaders: TStrings);
begin
  // to be implemented in inherited classes
end;

procedure TMARSCustomClient.ApplyProxyConfig;
begin
  // to be implemented in inherited classes
end;

procedure TMARSCustomClient.AssignTo(Dest: TPersistent);
var
  LDestClient: TMARSCustomClient;
begin
//  inherited;
  LDestClient := Dest as TMARSCustomClient;

  if Assigned(LDestClient) then
  begin
    LDestClient.AuthCookieName := AuthCookieName;
    LDestClient.AuthEndorsement := AuthEndorsement;
    LDestClient.MARSEngineURL := MARSEngineURL;
    LDestClient.ConnectTimeout := ConnectTimeout;
    LDestClient.ReadTimeout := ReadTimeout;
    LDestClient.OnError := OnError;
    LDestClient.ProxyConfig.Assign(ProxyConfig);
  end;
end;

procedure TMARSCustomClient.AuthEndorsementChanged;
begin

end;

procedure TMARSCustomClient.BeforeExecute;
begin
  EndorseAuthorization;
  if ProxyConfig.Enabled then
    ApplyProxyConfig;
end;

class procedure TMARSCustomClient.ClearBeforeExecute;
begin
  FBeforeExecuteProcs := [];
end;

procedure TMARSCustomClient.CloneSetup(const ASource: TMARSCustomClient);
begin
  if not Assigned(ASource) then
    Exit;

  Assign(ASource);
end;

constructor TMARSCustomClient.Create(AOwner: TComponent);
begin
  inherited;
  FProxyConfig := TMARSProxyConfig.Create;
  FAuthEndorsement := Cookie;
  FAuthCookieName := 'access_token';
  FMARSEngineURL := 'http://localhost:8080/rest';
end;


procedure TMARSCustomClient.Delete(const AURL: string; AContent, AResponse: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  FAuthToken := AAuthToken;
  BeforeExecute;
  FireBeforeExecute(AURL, Self);
end;

destructor TMARSCustomClient.Destroy;
begin
  FreeAndNil(FProxyConfig);
  inherited;
end;

procedure TMARSCustomClient.DoError(const AResource: TObject;
  const AException: Exception; const AVerb: TMARSHttpVerb;
  const AAfterExecute: TMARSClientResponseProc);
var
  LHandled: Boolean;
begin
  LHandled := False;

  if Assigned(FOnError) then
    FOnError(AResource, AException, AVerb, AAfterExecute, LHandled);

  if not LHandled then
    raise EMARSClientException.Create(AException.Message)
end;

procedure TMARSCustomClient.EndorseAuthorization;
begin
  // to be implemented in inherited classes
end;

class procedure TMARSCustomClient.FireBeforeExecute(const AURL: string;
  const AClient: TMARSCustomClient);
var
  LProc: TMARSClientBeforeExecuteProc;
begin
  for LProc in FBeforeExecuteProcs do
    LProc(AURL, AClient);
end;

procedure TMARSCustomClient.Get(const AURL: string; AResponseContent: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  FAuthToken := AAuthToken;
  BeforeExecute;
  FireBeforeExecute(AURL, Self);
end;

function TMARSCustomClient.GetConnectTimeout: Integer;
begin
  Result := -1;
end;

function TMARSCustomClient.GetReadTimeout: Integer;
begin
  Result := -1;
end;

function TMARSCustomClient.LastCmdSuccess: Boolean;
begin
  Result := False;
end;

procedure TMARSCustomClient.Post(const AURL: string; AContent, AResponse: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  FAuthToken := AAuthToken;
  BeforeExecute;
  FireBeforeExecute(AURL, Self);
end;

procedure TMARSCustomClient.Put(const AURL: string; AContent, AResponse: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  FAuthToken := AAuthToken;
  BeforeExecute;
  FireBeforeExecute(AURL, Self);
end;

class function TMARSCustomClient.RegisterBeforeExecute(
  const ABeforeExecute: TMARSClientBeforeExecuteProc): Integer;
begin
  FBeforeExecuteProcs := FBeforeExecuteProcs + [TMARSClientBeforeExecuteProc(ABeforeExecute)];
  Result := Length(FBeforeExecuteProcs) - 1;
end;

function TMARSCustomClient.ResponseStatusCode: Integer;
begin
  Result := -1;
end;

function TMARSCustomClient.ResponseText: string;
begin
  Result := '';
end;

procedure TMARSCustomClient.SetAuthEndorsement(
  const Value: TMARSAuthEndorsement);
begin
  if FAuthEndorsement <> Value then
  begin
    FAuthEndorsement := Value;
    AuthEndorsementChanged;
  end;
end;

procedure TMARSCustomClient.SetConnectTimeout(const Value: Integer);
begin
  // to be implemented in inherited classes
end;

procedure TMARSCustomClient.SetProxyConfig(const Value: TMARSProxyConfig);
begin
  FProxyConfig := Value;
  ApplyProxyConfig;
end;

procedure TMARSCustomClient.SetReadTimeout(const Value: Integer);
begin
  // to be implemented in inherited classes
end;

class procedure TMARSCustomClient.UnregisterBeforeExecute(
  const AIndex: Integer);
begin
  System.Delete(FBeforeExecuteProcs, AIndex, 1);
end;

class function TMARSCustomClient.GetAsString(const AEngineURL, AAppName,
  AResourceName: string; const APathParams: TArray<string>;
  const AQueryParams: TStrings; const AToken: string;
  const AAccept: string): string;
var
  LURL: TMARSURL;
  LQuery: string;
begin
  LURL := TMARSURL.CreateDummy(APathParams + [AAppName, AResourceName], AEngineURL);
  try
    LQuery := '';
    if Assigned(AQueryParams) and (AQueryParams.Count > 0) then
      LQuery := '?' + SmartConcat(TMARSURL.URLEncode(AQueryParams.ToStringArray), TMARSURL.URL_QUERY_SEPARATOR);

    Result := GetAsString(LURL.URL + LQuery, AToken, AAccept);
  finally
    LURL.Free;
  end;
end;

class function TMARSCustomClient.GetAsString(const AURL: string; const AToken,
  AAccept: string): string;
var
  LClient: TMARSCustomClient;
  LResource: TMARSClientResource;
begin
  LClient := Create(nil);
  try
    LClient.AuthEndorsement := AuthorizationBearer;
    LClient.MARSEngineURL := '';

    LResource := TMARSClientResource.Create(nil);
    try
      LResource.SpecificClient := LClient;
      LResource.Resource := '';

      LResource.SpecificURL := AURL;
      LResource.SpecificToken := AToken;
      LResource.SpecificAccept := AAccept;
      FireBeforeExecute(LResource.URL, LClient);
      Result := LResource.GETAsString();
    finally
      LResource.Free;
    end;
  finally
    LClient.Free;
  end;
end;

class function TMARSCustomClient.GetJSON<T>(const AEngineURL, AAppName,
  AResourceName: string; const APathParams: TArray<string>;
  const AQueryParams: TStrings; const AToken: string; const AIgnoreResult: Boolean): T;
var
  LClient: TMARSCustomClient;
  LResource: TMARSClientResourceJSON;
  LApp: TMARSClientApplication;
  LIndex: Integer;
  LFinalURL: string;
begin
  Result := nil;
  LClient := Create(nil);
  try
    LClient.AuthEndorsement := AuthorizationBearer;
    LClient.MARSEngineURL := AEngineURL;
    LApp := TMARSClientApplication.Create(nil);
    try
      LApp.Client := LClient;
      LApp.AppName := AAppName;
      LResource := TMARSClientResourceJSON.Create(nil);
      try
        LResource.Application := LApp;
        LResource.Resource := AResourceName;

        LResource.PathParamsValues.Clear;
        for LIndex := 0 to Length(APathParams)-1 do
          LResource.PathParamsValues.Add(APathParams[LIndex]);

        if Assigned(AQueryParams) then
          LResource.QueryParams.Assign(AQueryParams);

        LResource.SpecificToken := AToken;
        FireBeforeExecute(LResource.URL, LClient);
        LResource.GET(nil, nil, nil);

        Result := nil;
        if not AIgnoreResult then
          Result := LResource.Response.Clone as T;
      finally
        LResource.Free;
      end;
    finally
      LApp.Free;
    end;
  finally
    LClient.Free;
  end;
end;

{$ifdef DelphiXE7_UP}
class procedure TMARSCustomClient.GetJSONAsync<T>(const AEngineURL, AAppName,
  AResourceName: string; const APathParams: TArray<string>;
  const AQueryParams: TStrings; const ACompletionHandler: TProc<T>;
  const AOnException: TMARSClientExecptionProc; const AToken: string;
  const ASynchronize: Boolean);
var
  LClient: TMARSCustomClient;
  LResource: TMARSClientResourceJSON;
  LApp: TMARSClientApplication;
  LIndex: Integer;
  LFinalURL: string;
begin
  LClient := Create(nil);
  try
    LClient.AuthEndorsement := AuthorizationBearer;
    LClient.MARSEngineURL := AEngineURL;
    LApp := TMARSClientApplication.Create(nil);
    try
      LApp.Client := LClient;
      LApp.AppName := AAppName;
      LResource := TMARSClientResourceJSON.Create(nil);
      try
        LResource.Application := LApp;
        LResource.Resource := AResourceName;

        LResource.PathParamsValues.Clear;
        for LIndex := 0 to Length(APathParams)-1 do
          LResource.PathParamsValues.Add(APathParams[LIndex]);

        if Assigned(AQueryParams) then
          LResource.QueryParams.Assign(AQueryParams);

        LFinalURL := LResource.URL;
        LResource.SpecificToken := AToken;
        LResource.GETAsync(
          procedure (AResource: TMARSClientCustomResource)
          begin
            try
              if Assigned(ACompletionHandler) then
                ACompletionHandler((AResource as TMARSClientResourceJSON).Response as T);
            finally
              LResource.Free;
              LApp.Free;
              LClient.Free;
            end;
          end
        , AOnException
        , ASynchronize
        );
        except
          LResource.Free;
          raise;
        end;
      except
        LApp.Free;
        raise;
      end;
    except
      LClient.Free;
      raise;
    end;
end;
{$endif}

class function TMARSCustomClient.GetStream(const AEngineURL, AAppName,
  AResourceName: string; const AToken: string): TStream;
begin
  Result := GetStream(AEngineURL, AAppName, AResourceName, nil, nil, AToken);
end;

class function TMARSCustomClient.GetJSON<T>(const AEngineURL, AAppName,
  AResourceName: string; const AToken: string): T;
begin
  Result := GetJSON<T>(AEngineURL, AAppName, AResourceName, nil, nil, AToken);
end;

class function TMARSCustomClient.GetStream(const AEngineURL, AAppName,
  AResourceName: string; const APathParams: TArray<string>;
  const AQueryParams: TStrings; const AToken: string): TStream;
var
  LClient: TMARSCustomClient;
  LResource: TMARSClientResourceStream;
  LApp: TMARSClientApplication;
  LIndex: Integer;
begin
  LClient := Create(nil);
  try
    LClient.AuthEndorsement := AuthorizationBearer;
    LClient.MARSEngineURL := AEngineURL;
    LApp := TMARSClientApplication.Create(nil);
    try
      LApp.Client := LClient;
      LApp.AppName := AAppName;
      LResource := TMARSClientResourceStream.Create(nil);
      try
        LResource.Application := LApp;
        LResource.Resource := AResourceName;

        LResource.PathParamsValues.Clear;
        for LIndex := 0 to Length(APathParams)-1 do
          LResource.PathParamsValues.Add(APathParams[LIndex]);

        if Assigned(AQueryParams) then
          LResource.QueryParams.Assign(AQueryParams);

        LResource.SpecificToken := AToken;
        FireBeforeExecute(LResource.URL, LClient);
        LResource.GET(nil, nil, nil);

        Result := TMemoryStream.Create;
        try
          Result.CopyFrom(LResource.Response, LResource.Response.Size);
        except
          Result.Free;
          raise;
        end;
      finally
        LResource.Free;
      end;
    finally
      LApp.Free;
    end;
  finally
    LClient.Free;
  end;
end;

procedure TMARSCustomClient.Post(const AURL: string;
  const AFormData: TArray<TFormParam>; const AResponse: TStream;
  const AAuthToken, AAccept: string; const AContentType: string);
begin
  FAuthToken := AAuthToken;
  BeforeExecute;
  FireBeforeExecute(AURL, Self);
end;

procedure TMARSCustomClient.Post(const AURL: string; const AFormUrlEncoded: TMARSParameters; const AResponse: TStream;
  const AAuthToken, AAccept, AContentType: string);
begin
  FAuthToken := AAuthToken;
  BeforeExecute;
end;

class function TMARSCustomClient.PostJSON(const AEngineURL, AAppName,
  AResourceName: string; const APathParams: TArray<string>; const AQueryParams: TStrings;
  const AContent: TJSONValue; const ACompletionHandler: TProc<TJSONValue>; const AToken: string
): Boolean;
var
  LClient: TMARSCustomClient;
  LResource: TMARSClientResourceJSON;
  LApp: TMARSClientApplication;
  LIndex: Integer;
begin
  LClient := Create(nil);
  try
    LClient.AuthEndorsement := AuthorizationBearer;
    LClient.MARSEngineURL := AEngineURL;
    LApp := TMARSClientApplication.Create(nil);
    try
      LApp.Client := LClient;
      LApp.AppName := AAppName;
      LResource := TMARSClientResourceJSON.Create(nil);
      try
        LResource.Application := LApp;
        LResource.Resource := AResourceName;

        LResource.PathParamsValues.Clear;
        for LIndex := 0 to Length(APathParams)-1 do
          LResource.PathParamsValues.Add(APathParams[LIndex]);

        if Assigned(AQueryParams) then
          LResource.QueryParams.Assign(AQueryParams);

        LResource.SpecificToken := AToken;
        FireBeforeExecute(LResource.URL, LClient);
        LResource.POST(
          procedure (AStream: TMemoryStream)
          begin
            JSONValueToStream(AContent, AStream);
          end
        , procedure (AStream: TStream)
          begin
            if Assigned(ACompletionHandler) then
              ACompletionHandler(LResource.Response);
          end
        , nil
        );
        Result := LClient.LastCmdSuccess;
      finally
        LResource.Free;
      end;
    finally
      LApp.Free;
    end;
  finally
    LClient.Free;
  end;
end;


{$ifdef DelphiXE7_UP}
class procedure TMARSCustomClient.PostJSONAsync(const AEngineURL, AAppName,
  AResourceName: string; const APathParams: TArray<string>;
  const AQueryParams: TStrings; const AContent: TJSONValue;
  const ACompletionHandler: TProc<TJSONValue>;
  const AOnException: TMARSClientExecptionProc;
  const AToken: string;
  const ASynchronize: Boolean);
var
  LClient: TMARSCustomClient;
  LResource: TMARSClientResourceJSON;
  LApp: TMARSClientApplication;
  LIndex: Integer;
begin
  LClient := Create(nil);
  try
    LClient.AuthEndorsement := AuthorizationBearer;
    LClient.MARSEngineURL := AEngineURL;
    LApp := TMARSClientApplication.Create(nil);
    try
      LApp.Client := LClient;
      LApp.AppName := AAppName;
      LResource := TMARSClientResourceJSON.Create(nil);
      try
        LResource.Application := LApp;
        LResource.Resource := AResourceName;

        LResource.PathParamsValues.Clear;
        for LIndex := 0 to Length(APathParams)-1 do
          LResource.PathParamsValues.Add(APathParams[LIndex]);

        if Assigned(AQueryParams) then
          LResource.QueryParams.Assign(AQueryParams);

        LResource.SpecificToken := AToken;
        LResource.POSTAsync(
          procedure (AStream: TMemoryStream)
          begin
            JSONValueToStream(AContent, AStream);
          end
        , procedure (AResource: TMARSClientCustomResource)
          begin
            try
              if Assigned(ACompletionHandler) then
                ACompletionHandler((AResource as TMARSClientResourceJSON).Response);
            finally
              LResource.Free;
              LApp.Free;
              LClient.Free;
            end;
          end
        , AOnException
        , ASynchronize
        );
      except
        LResource.Free;
        raise;
      end;
    except
      LApp.Free;
      raise;
    end;
  except
    LClient.Free;
    raise;
  end;
end;
{$endif}

class function TMARSCustomClient.PostStream(const AEngineURL, AAppName,
  AResourceName: string; const APathParams: TArray<string>;
  const AQueryParams: TStrings; const AContent: TStream; const AToken: string
): Boolean;
var
  LClient: TMARSCustomClient;
  LResource: TMARSClientResourceStream;
  LApp: TMARSClientApplication;
  LIndex: Integer;
begin
  LClient := Create(nil);
  try
    LClient.AuthEndorsement := AuthorizationBearer;
    LClient.MARSEngineURL := AEngineURL;
    LApp := TMARSClientApplication.Create(nil);
    try
      LApp.Client := LClient;
      LApp.AppName := AAppName;
      LResource := TMARSClientResourceStream.Create(nil);
      try
        LResource.Application := LApp;
        LResource.Resource := AResourceName;

        LResource.PathParamsValues.Clear;
        for LIndex := 0 to Length(APathParams)-1 do
          LResource.PathParamsValues.Add(APathParams[LIndex]);

        if Assigned(AQueryParams) then
          LResource.QueryParams.Assign(AQueryParams);

        LResource.SpecificToken := AToken;
        FireBeforeExecute(LResource.URL, LClient);
        LResource.POST(
          procedure (AStream: TMemoryStream)
          begin
            if Assigned(AContent) then
            begin
              AStream.Size := 0; // reset
              AContent.Position := 0;
              AStream.CopyFrom(AContent, AContent.Size);
            end;
          end
        , nil, nil
        );
        Result := LClient.LastCmdSuccess;
      finally
        LResource.Free;
      end;
    finally
      LApp.Free;
    end;
  finally
    LClient.Free;
  end;
end;

procedure TMARSCustomClient.Put(const AURL: string; const AFormUrlEncoded: TMARSParameters; const AResponse: TStream;
  const AAuthToken, AAccept, AContentType: string);
begin
  FAuthToken := AAuthToken;
  BeforeExecute;
end;

procedure TMARSCustomClient.Put(const AURL: string;
  const AFormData: TArray<TFormParam>; const AResponse: TStream;
  const AAuthToken, AAccept: string; const AContentType: string);
begin
  FAuthToken := AAuthToken;
  BeforeExecute;
  FireBeforeExecute(AURL, Self);
end;

{ TMARSProxyConfig }

procedure TMARSProxyConfig.AssignTo(Dest: TPersistent);
var
  LDest: TMARSProxyConfig;
begin
//  inherited;
  if Dest is TMARSProxyConfig then
  begin
    LDest := TMARSProxyConfig(Dest);

    LDest.Enabled := Enabled;
    LDest.Host := Host;
    LDest.Port := Port;
    LDest.UserName := UserName;
    LDest.Password := Password;
  end;

end;

end.
