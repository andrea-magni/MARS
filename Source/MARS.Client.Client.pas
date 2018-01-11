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
  , MARS.Client.Utils, MARS.Core.Utils
  ;

type
  TMARSAuthEndorsement = (Cookie, AuthorizationBearer);
  TMARSHttpVerb = (Get, Put, Post, Head, Delete, Patch);
  TMARSClientErrorEvent = procedure (
    AResource: TObject; AException: Exception; AVerb: TMARSHttpVerb;
    const AAfterExecute: TMARSClientResponseProc; var AHandled: Boolean) of object;

  TMARSCustomClient = class; // fwd
  TMARSCustomClientClass = class of TMARSCustomClient;

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
  TMARSCustomClient = class(TComponent)
  private
    FMARSEngineURL: string;
    FOnError: TMARSClientErrorEvent;
    FAuthEndorsement: TMARSAuthEndorsement;
  protected
    procedure AssignTo(Dest: TPersistent); override;

    function GetConnectTimeout: Integer; virtual;
    function GetReadTimeout: Integer; virtual;
    procedure SetConnectTimeout(const Value: Integer); virtual;
    procedure SetReadTimeout(const Value: Integer); virtual;
    procedure SetAuthEndorsement(const Value: TMARSAuthEndorsement);

    procedure EndorseAuthorization(const AAuthToken: string); virtual;
    procedure AuthEndorsementChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;

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
    procedure Put(const AURL: string; AContent, AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); overload; virtual;
    procedure Put(const AURL: string; const AFormData: TArray<TFormParam>;
      const AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); overload; virtual;

    function LastCmdSuccess: Boolean; virtual;
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

    class function GetAsString(const AEngineURL, AAppName, AResourceName: string;
      const APathParams: TArray<string>; const AQueryParams: TStrings;
      const AToken: string = ''): string; overload;

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
  published
    property MARSEngineURL: string read FMARSEngineURL write FMARSEngineURL;
    property ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout;
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
    property OnError: TMARSClientErrorEvent read FOnError write FOnError;
    property AuthEndorsement: TMARSAuthEndorsement read FAuthEndorsement write SetAuthEndorsement default TMARSAuthEndorsement.Cookie;
  end;

function TMARSHttpVerbToString(const AVerb: TMARSHttpVerb): string;

implementation

uses
    Rtti, TypInfo
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

procedure TMARSCustomClient.AssignTo(Dest: TPersistent);
var
  LDestClient: TMARSCustomClient;
begin
//  inherited;
  LDestClient := Dest as TMARSCustomClient;

  if Assigned(LDestClient) then
  begin
    LDestClient.MARSEngineURL := MARSEngineURL;
    LDestClient.ConnectTimeout := ConnectTimeout;
    LDestClient.ReadTimeout := ReadTimeout;
    LDestClient.OnError := OnError;
  end;
end;

procedure TMARSCustomClient.AuthEndorsementChanged;
begin

end;

constructor TMARSCustomClient.Create(AOwner: TComponent);
begin
  inherited;
  FAuthEndorsement := Cookie;
  FMARSEngineURL := 'http://localhost:8080/rest';
end;


procedure TMARSCustomClient.Delete(const AURL: string; AContent, AResponse: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  EndorseAuthorization(AAuthToken);
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

procedure TMARSCustomClient.EndorseAuthorization(const AAuthToken: string);
begin

end;

procedure TMARSCustomClient.Get(const AURL: string; AResponseContent: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  EndorseAuthorization(AAuthToken);
end;

function TMARSCustomClient.GetConnectTimeout: Integer;
begin
  Result := -1;
end;

function TMARSCustomClient.GetReadTimeout: Integer;
begin
  Result := -1;
end;

//function TMARSCustomClient.GetRequest: TIdHTTPRequest;
//begin
//  Result := FHttpClient.Request;
//end;

//function TMARSCustomClient.GetResponse: TIdHTTPResponse;
//begin
//  Result := FHttpClient.Response;
//end;

function TMARSCustomClient.LastCmdSuccess: Boolean;
begin
  Result := False;
end;

procedure TMARSCustomClient.Post(const AURL: string; AContent, AResponse: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  EndorseAuthorization(AAuthToken);
end;

procedure TMARSCustomClient.Put(const AURL: string; AContent, AResponse: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  EndorseAuthorization(AAuthToken);
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

end;

procedure TMARSCustomClient.SetReadTimeout(const Value: Integer);
begin

end;

class function TMARSCustomClient.GetAsString(const AEngineURL, AAppName,
  AResourceName: string; const APathParams: TArray<string>;
  const AQueryParams: TStrings; const AToken: string): string;
var
  LClient: TMARSCustomClient;
  LResource: TMARSClientResource;
  LApp: TMARSClientApplication;
  LIndex: Integer;
  LFinalURL: string;
begin
  LClient := Create(nil);
  try
    LClient.MARSEngineURL := AEngineURL;
    LApp := TMARSClientApplication.Create(nil);
    try
      LApp.Client := LClient;
      LApp.AppName := AAppName;
      LResource := TMARSClientResource.Create(nil);
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
        Result := LResource.GETAsString();
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
  EndorseAuthorization(AAuthToken);
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
        LResource.POST(
          procedure (AStream: TMemoryStream)
          var
            LWriter: TStreamWriter;
          begin
            if Assigned(AContent) then
            begin
              LWriter := TStreamWriter.Create(AStream);
              try
                LWriter.Write(AContent.ToJSON);
              finally
                LWriter.Free;
              end;
            end;
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
          var
            LWriter: TStreamWriter;
          begin
            if Assigned(AContent) then
            begin
              LWriter := TStreamWriter.Create(AStream);
              try
                LWriter.Write(AContent.ToJSON);
              finally
                LWriter.Free;
              end;
            end;
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


procedure TMARSCustomClient.Put(const AURL: string;
  const AFormData: TArray<TFormParam>; const AResponse: TStream;
  const AAuthToken, AAccept: string; const AContentType: string);
begin
  EndorseAuthorization(AAuthToken);
end;

end.
