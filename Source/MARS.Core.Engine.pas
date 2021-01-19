(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Engine;

{$I MARS.inc}

interface

uses
  SysUtils, Classes, Generics.Collections
  , SyncObjs

  , MARS.Core.Classes, MARS.Core.Registry, MARS.Core.Application, MARS.Core.URL
  , MARS.Core.Exceptions, MARS.Utils.Parameters, MARS.Core.RequestAndResponse.Interfaces
;

{$M+}

const
  DEFAULT_ENGINE_NAME = 'DefaultEngine';

type
  EMARSEngineException = class(EMARSHttpException);
  TMARSEngine = class;

  TBeforeHandleRequestProc = reference to function(const AEngine: TMARSEngine;
    const AURL: TMARSURL; const ARequest: IMARSRequest; const AResponse: IMARSResponse;
    var Handled: Boolean): Boolean;
  TAfterHandleRequestProc = reference to procedure(const AEngine: TMARSEngine;
    const AURL: TMARSURL; const ARequest: IMARSRequest; const AResponse: IMARSResponse;
    var Handled: Boolean);

  TMARSEngine = class
  private
    FApplications: TMARSApplicationDictionary;
    FCriticalSection: TCriticalSection;
    FParameters: TMARSParameters;
    FName: string;
    FBeforeHandleRequest: TBeforeHandleRequestProc;
    FAfterHandleRequest: TAfterHandleRequestProc;
  protected
    function GetBasePath: string; virtual;
    function GetPort: Integer; virtual;
    function GetPortSSL: Integer; virtual;
    function GetThreadPoolSize: Integer; virtual;
    procedure SetBasePath(const Value: string); virtual;
    procedure SetPort(const Value: Integer); virtual;
    procedure SetPortSSL(const Value: Integer); virtual;
    procedure SetThreadPoolSize(const Value: Integer); virtual;
    procedure PatchCORS(const ARequest: IMARSRequest; const AResponse: IMARSResponse); virtual;
  public
    constructor Create(const AName: string = DEFAULT_ENGINE_NAME); virtual;
    destructor Destroy; override;

    function HandleRequest(ARequest: IMARSRequest; AResponse: IMARSResponse): Boolean; virtual;

    function AddApplication(const AName, ABasePath: string;
      const AResources: array of string; const AParametersSliceName: string = ''): TMARSApplication; virtual;

    procedure EnumerateApplications(const ADoSomething: TProc<string, TMARSApplication>); virtual;

    property Applications: TMARSApplicationDictionary read FApplications;
    property Parameters: TMARSParameters read FParameters;

    property BasePath: string read GetBasePath write SetBasePath;
    property Name: string read FName;
    property Port: Integer read GetPort write SetPort;
    property PortSSL: Integer read GetPortSSL write SetPortSSL;
    property ThreadPoolSize: Integer read GetThreadPoolSize write SetThreadPoolSize;

    property BeforeHandleRequest: TBeforeHandleRequestProc read FBeforeHandleRequest write FBeforeHandleRequest;
    property AfterHandleRequest: TAfterHandleRequestProc read FAfterHandleRequest write FAfterHandleRequest;
  end;

  TMARSEngineRegistry=class
  private
    FItems: TDictionary<string, TMARSEngine>;
    FCriticalSection: TCriticalSection;
  protected
    class var _Instance: TMARSEngineRegistry;
    class function GetInstance: TMARSEngineRegistry; static;
    function GetCount: Integer; virtual;
    function GetEngine(const AName: string): TMARSEngine; virtual;
    class function GetDefaultEngine: TMARSEngine; static;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure RegisterEngine(const AEngine: TMARSEngine); virtual;
    procedure UnregisterEngine(const AEngine: TMARSEngine); overload; virtual;
    procedure UnregisterEngine(const AEngineName: string); overload; virtual;

    procedure EnumerateEngines(const ADoSomething: TProc<string, TMARSEngine>); virtual;

    property Engine[const AName: string]: TMARSEngine read GetEngine; default;

    property Count: Integer read GetCount;

    class property Instance: TMARSEngineRegistry read GetInstance;
    class property DefaultEngine: TMARSEngine read GetDefaultEngine;
    class destructor ClassDestructor;
  end;

implementation

uses
    MARS.Core.Utils
  , MARS.Core.Activation, MARS.Core.Activation.Interfaces
  , MARS.Core.MediaType
  ;

function TMARSEngine.AddApplication(const AName, ABasePath: string;
  const AResources: array of string; const AParametersSliceName: string): TMARSApplication;
var
  LResource: string;
  LParametersSliceName: string;
begin
  Result := TMARSApplication.Create(AName);
  try
    Result.BasePath := ABasePath;
    for LResource in AResources do
      Result.AddResource(LResource);

    LParametersSliceName := AParametersSliceName;
    if LParametersSliceName = '' then
      LParametersSliceName := AName;
    Result.Parameters.CopyFrom(Parameters, LParametersSliceName);

    FCriticalSection.Enter;
    try
      Applications.Add(
        TMARSURL.CombinePath([BasePath, ABasePath]).ToLower
        , Result
      );
    finally
      FCriticalSection.Leave;
    end;
  except
    Result.Free;
    raise
  end;
end;

constructor TMARSEngine.Create(const AName: string);
begin
  inherited Create;

  FName := AName;

  FApplications := TMARSApplicationDictionary.Create([doOwnsValues]);
  FCriticalSection := TCriticalSection.Create;
  FParameters := TMARSParameters.Create(FName);

  // default parameters
  Parameters.Values['Port'] := 8080;
  Parameters.Values['PortSSL'] := 0;
  Parameters.Values['ThreadPoolSize'] := 75;
  Parameters.Values['BasePath'] := '/rest';

  TMARSEngineRegistry.Instance.RegisterEngine(Self);
end;

destructor TMARSEngine.Destroy;
begin
  TMARSEngineRegistry.Instance.UnregisterEngine(Self);

  FParameters.Free;
  FCriticalSection.Free;
  FApplications.Free;
  inherited;
end;

procedure TMARSEngine.EnumerateApplications(
  const ADoSomething: TProc<string, TMARSApplication>);
var
  LPair: TPair<string, TMARSApplication>;
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
  LApplication: TMARSApplication;
  LURL: TMARSURL;
  LApplicationPath: string;
  LActivation: IMARSActivation;
begin
  Result := False;

  PatchCORS(ARequest, AResponse);

  LURL := TMARSURL.Create(ARequest);
  try
    if Assigned(FBeforeHandleRequest) then
      if not FBeforeHandleRequest(Self, LURL, ARequest, AResponse, Result) then
        Exit;

    LApplicationPath := '';
    if LURL.HasPathTokens then
      LApplicationPath := TMARSURL.CombinePath([LURL.PathTokens[0]]);

    if (BasePath <> '') and (BasePath <> TMARSURL.URL_PATH_SEPARATOR) then
    begin
      if not LURL.MatchPath(BasePath) then
        raise EMARSEngineException.Create(
            Format('Bad request [%s] does not match engine URL [%s]', [LURL.URL, BasePath])
            , 404
          );
      if LURL.HasPathTokens(2) then
        LApplicationPath := TMARSURL.CombinePath([LURL.PathTokens[0], LURL.PathTokens[1]]);
    end;

    if not FApplications.TryGetValue(LApplicationPath.ToLower, LApplication) then
      raise EMARSEngineException.Create(Format('Bad request [%s]: unknown application [%s]', [LURL.URL, LApplicationPath]), 404);

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

procedure TMARSEngine.PatchCORS(const ARequest: IMARSRequest;
  const AResponse: IMARSResponse);

  procedure SetHeaderFromParameter(const AHeader, AParamName, ADefault: string);
  begin
    AResponse.SetHeader(AHeader, Parameters.ByName(AParamName, ADefault).AsString);
  end;

begin
  if Parameters.ByName('CORS.Enabled').AsBoolean then
  begin
    SetHeaderFromParameter('Access-Control-Allow-Origin', 'CORS.Origin', '*');
    SetHeaderFromParameter('Access-Control-Allow-Methods', 'CORS.Methods', 'HEAD,GET,PUT,POST,DELETE,OPTIONS');
    SetHeaderFromParameter('Access-Control-Allow-Headers', 'CORS.Headers', 'X-Requested-With,Content-Type,Authorization');
  end;
end;

function TMARSEngine.GetBasePath: string;
begin
  Result := Parameters['BasePath'].AsString;
end;

procedure TMARSEngine.SetBasePath(const Value: string);
begin
  Parameters['BasePath'] := Value;
end;

procedure TMARSEngine.SetPort(const Value: Integer);
begin
  Parameters['Port'] := Value;
end;

procedure TMARSEngine.SetPortSSL(const Value: Integer);
begin
  Parameters['PortSSL'] := Value;
end;

procedure TMARSEngine.SetThreadPoolSize(const Value: Integer);
begin
  Parameters['ThreadPoolSize'] := Value;
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

class destructor TMARSEngineRegistry.ClassDestructor;
begin
  if Assigned(_Instance) then
    FreeAndNil(_Instance);
end;

constructor TMARSEngineRegistry.Create;
begin
  inherited Create;
  FItems := TDictionary<string, TMARSEngine>.Create;
  FCriticalSection := TCriticalSection.Create;
end;

destructor TMARSEngineRegistry.Destroy;
begin
  FCriticalSection.Free;
  FItems.Free;
  inherited;
end;

procedure TMARSEngineRegistry.EnumerateEngines(
  const ADoSomething: TProc<string, TMARSEngine>);
var
  LPair: TPair<string, TMARSEngine>;
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

function TMARSEngineRegistry.GetCount: Integer;
begin
  Result := FItems.Count;
end;

class function TMARSEngineRegistry.GetDefaultEngine: TMARSEngine;
begin
  Result := Instance.Engine[DEFAULT_ENGINE_NAME];
end;

function TMARSEngineRegistry.GetEngine(const AName: string): TMARSEngine;
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

procedure TMARSEngineRegistry.RegisterEngine(const AEngine: TMARSEngine);
begin
  Assert(Assigned(AEngine));

  FCriticalSection.Enter;
  try
    FItems.AddOrSetValue(AEngine.Name, AEngine);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TMARSEngineRegistry.UnregisterEngine(const AEngine: TMARSEngine);
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

end.
