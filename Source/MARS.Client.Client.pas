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
  , MARS.Core.JSON

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

    // shortcuts
    class function GetJSON<T: TJSONValue>(const AEngineURL, AAppName, AResourceName: string;
      const ASilentMode: Boolean = False): T; overload;

    class function GetJSON<T: TJSONValue>(const AEngineURL, AAppName, AResourceName: string;
      const APathParams: TArray<string>; const AQueryParams: TStrings;
      const ASilentMode: Boolean = False; const AIgnoreResult: Boolean = False): T; overload;

    class function GetAsString(const AEngineURL, AAppName, AResourceName: string;
      const APathParams: TArray<string>; const AQueryParams: TStrings;
      const ASilentMode: Boolean = False): string; overload;

    class function PostJSON(const AEngineURL, AAppName, AResourceName: string;
      const APathParams: TArray<string>; const AQueryParams: TStrings;
      const AContent: TJSONValue;
      const ASilentMode: Boolean = False): Boolean;

    class function GetStream(const AEngineURL, AAppName, AResourceName: string;
      const ASilentMode: Boolean = False): TStream; overload;

    class function GetStream(const AEngineURL, AAppName, AResourceName: string;
      const APathParams: TArray<string>; const AQueryParams: TStrings;
      const ASilentMode: Boolean = False): TStream; overload;

    class function PostStream(const AEngineURL, AAppName, AResourceName: string;
      const APathParams: TArray<string>; const AQueryParams: TStrings;
      const AContent: TStream;
      const ASilentMode: Boolean = False): Boolean;

  published
    property MARSEngineURL: string read FMARSEngineURL write FMARSEngineURL;
    property ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout;
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
  end;

procedure Register;

implementation

uses
    MARS.Client.Resource
  , MARS.Client.Resource.JSON
  , MARS.Client.Resource.Stream
  , MARS.Client.Application
;

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

class function TMARSClient.GetAsString(const AEngineURL, AAppName,
  AResourceName: string; const APathParams: TArray<string>;
  const AQueryParams: TStrings; const ASilentMode: Boolean): string;
var
  LClient: TMARSClient;
  LResource: TMARSClientResource;
  LApp: TMARSClientApplication;
  LIndex: Integer;
  LFinalURL: string;
begin
  try
    LClient := TMARSClient.Create(nil);
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
  except on E:Exception do
    begin
//      Logger.LogWarning('GetAsString exception: [%s] %s, Engine: [%s], App: [%s], Resource: [%s], URL: [%s]'
//        , [E.ClassName, E.Message, AEngineURL, AAppName, AResourceName, LFinalURL]);
      if not ASilentMode then
        raise E;
    end;
  end;
end;

class function TMARSClient.GetJSON<T>(const AEngineURL, AAppName,
  AResourceName: string; const APathParams: TArray<string>;
  const AQueryParams: TStrings; const ASilentMode: Boolean;
  const AIgnoreResult: Boolean): T;
var
  LClient: TMARSClient;
  LResource: TMARSClientResourceJSON;
  LApp: TMARSClientApplication;
  LIndex: Integer;
  LFinalURL: string;
begin
  Result := nil;
  try
    LClient := TMARSClient.Create(nil);
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
          LResource.GET();

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
  except on E:Exception do
    begin
//      Logger.LogWarning('GetJSON exception: [%s] %s, Engine: [%s], App: [%s], Resource: [%s], URL: [%s]'
//        , [E.ClassName, E.Message, AEngineURL, AAppName, AResourceName, LFinalURL]);
      if not ASilentMode then
        raise E;
    end;
  end;
end;

class function TMARSClient.GetStream(const AEngineURL, AAppName,
  AResourceName: string; const ASilentMode: Boolean): TStream;
begin
  Result := GetStream(AEngineURL, AAppName, AResourceName, nil, nil, ASilentMode);
end;

class function TMARSClient.GetJSON<T>(const AEngineURL, AAppName,
  AResourceName: string; const ASilentMode: Boolean): T;
begin
  Result := GetJSON<T>(AEngineURL, AAppName, AResourceName, nil, nil, ASilentMode);
end;

class function TMARSClient.GetStream(const AEngineURL, AAppName,
  AResourceName: string; const APathParams: TArray<string>;
  const AQueryParams: TStrings; const ASilentMode: Boolean): TStream;
var
  LClient: TMARSClient;
  LResource: TMARSClientResourceStream;
  LApp: TMARSClientApplication;
  LIndex: Integer;
begin
  Result := nil;
  try
    LClient := TMARSClient.Create(nil);
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

          LResource.GET();

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
  except on E:Exception do
    begin
//      Logger.LogWarning('GetStream exception: [%s] %s, Engine: [%s], App: [%s], Resource: [%s]'
//        , [E.ClassName, E.Message, AEngineURL, AAppName, AResourceName]);
      if not ASilentMode then
        raise E;
    end;
  end;
end;

class function TMARSClient.PostJSON(const AEngineURL, AAppName,
  AResourceName: string; const APathParams: TArray<string>; const AQueryParams: TStrings;
  const AContent: TJSONValue; const ASilentMode: Boolean): Boolean;
var
  LClient: TMARSClient;
  LResource: TMARSClientResourceJSON;
  LApp: TMARSClientApplication;
  LIndex: Integer;
begin
  Result := False;
  try
    LClient := TMARSClient.Create(nil);
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

          LResource.POST(
            procedure (AStream: TMemoryStream)
            var
              LWriter: TStreamWriter;
            begin
              LWriter := TStreamWriter.Create(AStream);
              try
                LWriter.Write(AContent.ToJSON);
              finally
                LWriter.Free;
              end;
            end
          );
          Result := LClient.Response.ResponseCode = 200;
        finally
          LResource.Free;
        end;
      finally
        LApp.Free;
      end;
    finally
      LClient.Free;
    end;
  except on E:Exception do
    begin
//      Logger.LogWarning('PostJSON exception: [%s] %s, Engine: [%s], App: [%s], Resource: [%s]'
//        , [E.ClassName, E.Message, AEngineURL, AAppName, AResourceName]);
      if not ASilentMode then
        raise E;
    end;
  end;
end;

class function TMARSClient.PostStream(const AEngineURL, AAppName,
  AResourceName: string; const APathParams: TArray<string>;
  const AQueryParams: TStrings; const AContent: TStream;
  const ASilentMode: Boolean): Boolean;
var
  LClient: TMARSClient;
  LResource: TMARSClientResourceStream;
  LApp: TMARSClientApplication;
  LIndex: Integer;
begin
  Result := False;
  try
    LClient := TMARSClient.Create(nil);
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

          LResource.POST(
            procedure (AStream: TMemoryStream)
            begin
              AStream.Size := 0; // reset
              AContent.Position := 0;
              AStream.CopyFrom(AContent, AContent.Size);
            end
          );
          Result := LClient.Response.ResponseCode = 200;
        finally
          LResource.Free;
        end;
      finally
        LApp.Free;
      end;
    finally
      LClient.Free;
    end;
  except on E:Exception do
    begin
//      Logger.LogWarning('PostStream exception: [%s] %s, Engine: [%s], App: [%s], Resource: [%s]'
//        , [E.ClassName, E.Message, AEngineURL, AAppName, AResourceName]);
      if not ASilentMode then
        raise E;
    end;
  end;
end;


end.
