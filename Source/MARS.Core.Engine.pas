(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)

unit MARS.Core.Engine;

{$I MARS.inc}

interface

uses
  SysUtils, Classes, Generics.Collections, SyncObjs
, MARS.Core.Classes, MARS.Core.Registry
, MARS.Core.Exceptions
, MARS.Core.Engine.Interfaces
, MARS.Core.Application.Interfaces
, MARS.Core.URL
, MARS.Utils.Parameters, MARS.Core.RequestAndResponse.Interfaces
;

{$M+}

const
  DEFAULT_ENGINE_NAME = 'DefaultEngine';

type
  TMARSEngine = class(TInterfacedObject, IMARSEngine)
  private
    FApplications: TMARSApplicationDictionary;
    FCriticalSection: TCriticalSection;
    FParameters: TMARSParameters;
    FName: string;
    FBeforeHandleRequest: TBeforeHandleRequestProc;
    FAfterHandleRequest: TAfterHandleRequestProc;
    FOnGetApplication: TGetApplicationProc;
  protected
    procedure PatchCORS(const ARequest: IMARSRequest; const AResponse: IMARSResponse); virtual;
  public
    constructor Create(const AName: string = DEFAULT_ENGINE_NAME); virtual;
    destructor Destroy; override;

    // IMARSEngine -------------------------------------------------------------
    function HandleRequest(ARequest: IMARSRequest; AResponse: IMARSResponse): Boolean; virtual;

    function AddApplication(const AName, ABasePath: string;
      const AResources: array of string; const AParametersSliceName: string = '';
      const ADefaultResourcePath: string = ''): IMARSApplication; virtual;
    function ApplicationByName(const AName: string): IMARSApplication; virtual;
    function ApplicationByBasePath(const ABasePath: string): IMARSApplication; virtual;

    procedure EnumerateApplications(const ADoSomething: TProc<string, IMARSApplication>); virtual;
    function IsCORSEnabled: Boolean; virtual;

    function GetApplications: TMARSApplicationDictionary;
    function GetParameters: TMARSParameters;
    function GetBasePath: string;
    procedure SetBasePath(const AValue: string);
    function GetName: string;
    function GetPort: Integer;
    procedure SetPort(const AValue: Integer);
    function GetPortSSL: Integer;
    procedure SetPortSSL(const AValue: Integer);
    function GetThreadPoolSize: Integer;
    procedure SetThreadPoolSize(const AValue: Integer);
    function GetBeforeHandleRequest: TBeforeHandleRequestProc;
    procedure SetBeforeHandleRequest(const AValue: TBeforeHandleRequestProc);
    function GetAfterHandleRequest: TAfterHandleRequestProc;
    procedure SetAfterHandleRequest(const AValue: TAfterHandleRequestProc);
    function GetOnGetApplication: TGetApplicationProc;
    procedure SetOnGetApplication(const AValue: TGetApplicationProc);


    // IMARSEngine -------------------------------------------------------------
    property Parameters: TMARSParameters read FParameters;
    property Name: string read FName;
(*
    property Applications: TMARSApplicationDictionary read FApplications;

    property BasePath: string read GetBasePath write SetBasePath;

    property Port: Integer read GetPort write SetPort;
    property PortSSL: Integer read GetPortSSL write SetPortSSL;
    property ThreadPoolSize: Integer read GetThreadPoolSize write SetThreadPoolSize;

    property BeforeHandleRequest: TBeforeHandleRequestProc read FBeforeHandleRequest write FBeforeHandleRequest;
    property AfterHandleRequest: TAfterHandleRequestProc read FAfterHandleRequest write FAfterHandleRequest;
    property OnGetApplication: TGetApplicationProc read FOnGetApplication write FOnGetApplication;
*)
  end;

  TMARSEngineRegistry=class
  private
    FItems: TDictionary<string, IMARSEngine>;
    FCriticalSection: TCriticalSection;
  protected
    class var _Instance: TMARSEngineRegistry;
    class function GetInstance: TMARSEngineRegistry; static;
    function GetCount: Integer; virtual;
    function GetEngine(const AName: string): IMARSEngine; virtual;
    class function GetDefaultEngine: IMARSEngine; static;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure RegisterEngine(AEngine: IMARSEngine); virtual;
    procedure UnregisterEngine(AEngine: IMARSEngine); overload; virtual;
    procedure UnregisterEngine(const AEngineName: string); overload; virtual;

    procedure EnumerateEngines(const ADoSomething: TProc<string, IMARSEngine>); virtual;

    property Engine[const AName: string]: IMARSEngine read GetEngine; default;

    property Count: Integer read GetCount;

    class property Instance: TMARSEngineRegistry read GetInstance;
    class function HasInstance: Boolean;
    class procedure Cleanup;

    class property DefaultEngine: IMARSEngine read GetDefaultEngine;
  end;

implementation

uses
  MARS.Core.Utils
, MARS.Core.Activation, MARS.Core.Activation.Interfaces
, MARS.Core.Application
, MARS.Core.MediaType
;

function TMARSEngine.AddApplication(const AName, ABasePath: string;
  const AResources: array of string; const AParametersSliceName: string;
  const ADefaultResourcePath: string): IMARSApplication;
var
  LResource: string;
  LParametersSliceName: string;
begin
  Result := TMARSApplication.Create(AName);

  Result.BasePath := ABasePath;
  Result.DefaultResourcePath := ADefaultResourcePath;
  for LResource in AResources do
    Result.AddResource(LResource);

  LParametersSliceName := AParametersSliceName;
  if LParametersSliceName = '' then
    LParametersSliceName := AName;
  Result.Parameters.CopyFrom(Parameters, LParametersSliceName);

  FCriticalSection.Enter;
  try
    GetApplications.Add(
      TMARSURL.CombinePath([GetBasePath, ABasePath]).ToLower
    , Result
    );
  finally
    FCriticalSection.Leave;
  end;
end;

function TMARSEngine.ApplicationByBasePath(
  const ABasePath: string): IMARSApplication;
begin
  if not FApplications.TryGetValue(ABasePath, Result) then
    Result := nil;
end;

function TMARSEngine.ApplicationByName(const AName: string): IMARSApplication;
var
  LApplication: IMARSApplication;
begin
  Result := nil;
  for LApplication in FApplications.Values.ToArray do
    if LApplication.Name = AName then
      Exit(LApplication);
end;

constructor TMARSEngine.Create(const AName: string);
begin
  inherited Create;

  FName := AName;

  FApplications := TMARSApplicationDictionary.Create();
  FCriticalSection := TCriticalSection.Create;
  FParameters := TMARSParameters.Create(FName);

  FBeforeHandleRequest := nil;
  FAfterHandleRequest := nil;
  FOnGetApplication := nil;

  // default parameters
  with Parameters do
  begin
    Values['Port'] := 8080;
    Values['PortSSL'] := 0;
    Values['ThreadPoolSize'] := 75;
    Values['BasePath'] := '/rest';
  end;

  TMARSEngineRegistry.Instance.RegisterEngine(Self);
end;

destructor TMARSEngine.Destroy;
begin
  if TMARSEngineRegistry.HasInstance then
    TMARSEngineRegistry.Instance.UnregisterEngine(Self);

  FParameters.Free;
  FCriticalSection.Free;
  FApplications.Free;
  inherited;
end;

procedure TMARSEngine.EnumerateApplications(
  const ADoSomething: TProc<string, IMARSApplication>);
var
  LPair: TPair<string, IMARSApplication>;
begin
  if Assigned(ADoSomething) then
  begin
    FCriticalSection.Enter;
    try
      for LPair in FApplications do
        ADoSomething(LPair.Key, LPair.Value);
    finally
      FCriticalSection.Leave;
    end;
  end;
end;

function TMARSEngine.HandleRequest(ARequest: IMARSRequest; AResponse: IMARSResponse): Boolean;
var
  LApplication: IMARSApplication;
  LURL: TMARSURL;
  LEnginePath: string;
  LApplicationPath: string;
  LActivation: IMARSActivation;
begin
  Result := False;

  PatchCORS(ARequest, AResponse);

  LEnginePath := GetBasePath;

  LURL := TMARSURL.Create(ARequest);
  try
    if Assigned(FBeforeHandleRequest) then
      if not FBeforeHandleRequest(Self, LURL, ARequest, AResponse, Result) then
        Exit;

    LApplicationPath := '';
    if LURL.HasPathTokens then
      LApplicationPath := TMARSURL.CombinePath([LURL.PathTokens[0]]);

    if (LEnginePath <> '') and (LEnginePath <> TMARSURL.URL_PATH_SEPARATOR) then
    begin
      if not LURL.MatchPath(LEnginePath) then
        raise EMARSEngineException.Create(
            Format('Bad request [%s] does not match engine URL [%s]', [LURL.URL, LEnginePath])
            , 404
          );
      if LURL.HasPathTokens(2) then
        LApplicationPath := TMARSURL.CombinePath([LURL.PathTokens[0], LURL.PathTokens[1]]);
    end;

    if not FApplications.TryGetValue(LApplicationPath.ToLower, LApplication) then
      LApplication := nil;

    if Assigned(FOnGetApplication) then
      FOnGetApplication(Self, LURL, ARequest, AResponse, LApplication);

    if not Assigned(LApplication) then
      raise EMARSEngineException.Create(Format('Bad request [%s]: unknown application [%s]', [LURL.URL, LApplicationPath]), 404);

    LApplicationPath := TMARSURL.CombinePath([LEnginePath, LApplication.BasePath]);
    LURL.BasePath := LApplicationPath;

    LActivation := TMARSActivation.CreateActivation(Self, LApplication, ARequest, AResponse, LURL);
    if Assigned(LActivation) then
    begin
      LActivation.Invoke;
      Result := True;
    end;

    if Assigned(FAfterHandleRequest) then
      FAfterHandleRequest(Self, LURL, ARequest, AResponse, Result); // TODO: switch to Activation
  finally
    LURL.Free;
  end;
end;

function TMARSEngine.IsCORSEnabled: Boolean;
begin
  Result := Parameters.ByName('CORS.Enabled').AsBoolean;
end;

procedure TMARSEngine.PatchCORS(const ARequest: IMARSRequest;
  const AResponse: IMARSResponse);

  procedure SetHeaderFromParameter(const AHeader, AParamName, ADefault: string);
  begin
    AResponse.SetHeader(AHeader, Parameters.ByName(AParamName, ADefault).AsString);
  end;

begin
  if not IsCORSEnabled then
    Exit;

  SetHeaderFromParameter('Access-Control-Allow-Origin', 'CORS.Origin', '*');
  SetHeaderFromParameter('Access-Control-Allow-Methods', 'CORS.Methods', 'HEAD,GET,PUT,POST,PATCH,DELETE,OPTIONS');
  SetHeaderFromParameter('Access-Control-Allow-Headers', 'CORS.Headers', 'X-Requested-With,Content-Type,Authorization');
  SetHeaderFromParameter('Access-Control-Allow-Private-Network', 'CORS.PrivateNetwork', 'true');
end;

function TMARSEngine.GetAfterHandleRequest: TAfterHandleRequestProc;
begin
  Result := FAfterHandleRequest;
end;

function TMARSEngine.GetApplications: TMARSApplicationDictionary;
begin
  Result := FApplications;
end;

function TMARSEngine.GetBasePath: string;
begin
  Result := Parameters['BasePath'].AsString;
end;

function TMARSEngine.GetBeforeHandleRequest: TBeforeHandleRequestProc;
begin
  Result := FBeforeHandleRequest;
end;

function TMARSEngine.GetName: string;
begin
  Result := FName;
end;

function TMARSEngine.GetOnGetApplication: TGetApplicationProc;
begin
  Result := FOnGetApplication;
end;

procedure TMARSEngine.SetAfterHandleRequest(
  const AValue: TAfterHandleRequestProc);
begin
  FAfterHandleRequest := AValue;
end;

procedure TMARSEngine.SetBasePath(const AValue: string);
begin
  Parameters['BasePath'] := AValue;
end;

procedure TMARSEngine.SetBeforeHandleRequest(
  const AValue: TBeforeHandleRequestProc);
begin
  FBeforeHandleRequest := AValue;
end;

procedure TMARSEngine.SetOnGetApplication(const AValue: TGetApplicationProc);
begin
  FOnGetApplication := AValue;
end;

procedure TMARSEngine.SetPort(const AValue: Integer);
begin
  Parameters['Port'] := AValue;
end;

procedure TMARSEngine.SetPortSSL(const AValue: Integer);
begin
  Parameters['PortSSL'] := AValue;
end;

procedure TMARSEngine.SetThreadPoolSize(const AValue: Integer);
begin
  Parameters['ThreadPoolSize'] := AValue;
end;

function TMARSEngine.GetParameters: TMARSParameters;
begin
  Result := FParameters;
end;

function TMARSEngine.GetPort: Integer;
begin
  Result := Parameters['Port'].AsInteger;
end;

function TMARSEngine.GetPortSSL: Integer;
begin
  Result := Parameters['PortSSL'].AsInteger;
end;

function TMARSEngine.GetThreadPoolSize: Integer;
begin
  Result := Parameters['ThreadPoolSize'].AsInteger;
end;

{ TMARSEngineRegistry }

constructor TMARSEngineRegistry.Create;
begin
  inherited Create;
  FItems := TDictionary<string, IMARSEngine>.Create;
  FCriticalSection := TCriticalSection.Create;
end;

destructor TMARSEngineRegistry.Destroy;
begin
  FCriticalSection.Free;
  FItems.Free;
  inherited;
end;

procedure TMARSEngineRegistry.EnumerateEngines(
  const ADoSomething: TProc<string, IMARSEngine>);
var
  LPair: TPair<string, IMARSEngine>;
begin
  if Assigned(ADoSomething) then
  begin
    FCriticalSection.Enter;
    try
      for LPair in FItems do
        ADoSomething(LPair.Key, LPair.Value);
    finally
      FCriticalSection.Leave;
    end;
  end;
end;

class procedure TMARSEngineRegistry.Cleanup;
begin
  FreeAndNil(_Instance);
end;

function TMARSEngineRegistry.GetCount: Integer;
begin
  Result := FItems.Count;
end;

class function TMARSEngineRegistry.GetDefaultEngine: IMARSEngine;
begin
  Result := Instance.Engine[DEFAULT_ENGINE_NAME];
end;

function TMARSEngineRegistry.GetEngine(const AName: string): IMARSEngine;
begin
  if not FItems.TryGetValue(AName, Result) then
    Result := nil;
end;

class function TMARSEngineRegistry.GetInstance: TMARSEngineRegistry;
begin
  if not Assigned(_Instance) then
    _Instance := TMARSEngineRegistry.Create;
  Result := _Instance;
end;

class function TMARSEngineRegistry.HasInstance: Boolean;
begin
  Result := Assigned(_Instance);
end;

procedure TMARSEngineRegistry.RegisterEngine(AEngine: IMARSEngine);
begin
  Assert(Assigned(AEngine));

  FCriticalSection.Enter;
  try
    FItems.AddOrSetValue(AEngine.Name, AEngine);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TMARSEngineRegistry.UnregisterEngine(AEngine: IMARSEngine);
begin
  Assert(Assigned(AEngine));

  UnregisterEngine(AEngine.Name);
end;

procedure TMARSEngineRegistry.UnregisterEngine(const AEngineName: string);
begin
  FCriticalSection.Enter;
  try
    FItems.Remove(AEngineName);
  finally
    FCriticalSection.Leave;
  end;
end;

initialization

finalization
  TMARSEngineRegistry.Cleanup;

end.
