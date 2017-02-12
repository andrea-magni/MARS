(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Engine;

interface

uses
  SysUtils, HTTPApp, Classes, Generics.Collections
  , SyncObjs
  , Diagnostics

  , MARS.Core.Classes
  , MARS.Core.Registry
  , MARS.Core.Application
  , MARS.Core.URL
  , MARS.Core.Exceptions
  , MARS.Utils.Parameters
;

{$M+}

const
  DEFAULT_ENGINE_NAME = 'DefaultEngine';

type
  EMARSEngineException = class(EMARSHttpException);
  TMARSEngine = class;

  IMARSHandleRequestEventListener = interface
    procedure BeforeHandleRequest(const ASender: TMARSEngine; const AApplication: TMARSApplication; var AIsAllowed: Boolean);
    procedure AfterHandleRequest(const ASender: TMARSEngine; const AApplication: TMARSApplication; const AStopWatch: TStopWatch);
  end;

  TMARSEngineBeforeHandleRequestEvent = TFunc<TMARSEngine, TMARSURL, Boolean>;

  TMARSEngine = class
  private
    class threadvar FWebRequest: TWebRequest;
    class threadvar FWebResponse: TWebResponse;
    class threadvar FURL: TMARSURL;
  private
    FApplications: TMARSApplicationDictionary;
    FSubscribers: TList<IMARSHandleRequestEventListener>;
    FCriticalSection: TCriticalSection;
    FParameters: TMARSParameters;
    FName: string;
    FOnBeforeHandleRequest: TMARSEngineBeforeHandleRequestEvent;

    function GetCurrentRequest: TWebRequest;
    function GetCurrentResponse: TWebResponse;
    function GetCurrentURL: TMARSURL;
    function GetBasePath: string;
    function GetPort: Integer;
    function GetThreadPoolSize: Integer;
    procedure SetBasePath(const Value: string);
    procedure SetPort(const Value: Integer);
    procedure SetThreadPoolSize(const Value: Integer);
  protected
    function DoBeforeHandleRequest(const AApplication: TMARSApplication): Boolean; virtual;
    procedure DoAfterHandleRequest(const AApplication: TMARSApplication; const AStopWatch: TStopWatch); virtual;
  public
    constructor Create(const AName: string = DEFAULT_ENGINE_NAME); virtual;
    destructor Destroy; override;

    function HandleRequest(ARequest: TWebRequest; AResponse: TWebResponse): Boolean;

    function AddApplication(const AName, ABasePath: string;
      const AResources: array of string; const AParametersSliceName: string = ''): TMARSApplication; virtual;
    procedure AddSubscriber(const ASubscriber: IMARSHandleRequestEventListener);
    procedure RemoveSubscriber(const ASubscriber: IMARSHandleRequestEventListener);

    procedure EnumerateApplications(const ADoSomething: TProc<string, TMARSApplication>);

    property Applications: TMARSApplicationDictionary read FApplications;
    property Parameters: TMARSParameters read FParameters;

    property BasePath: string read GetBasePath write SetBasePath;
    property Name: string read FName;
    property Port: Integer read GetPort write SetPort;
    property ThreadPoolSize: Integer read GetThreadPoolSize write SetThreadPoolSize;

    // Transient properties
    property CurrentURL: TMARSURL read GetCurrentURL;
    property CurrentRequest: TWebRequest read GetCurrentRequest;
    property CurrentResponse: TWebResponse read GetCurrentResponse;

    property OnBeforeHandleRequest: TMARSEngineBeforeHandleRequestEvent read FOnBeforeHandleRequest write FOnBeforeHandleRequest;
  end;

  TMARSEngineRegistry=class
  private
    FItems: TDictionary<string, TMARSEngine>;
    function GetCount: Integer;
  protected
    class var _Instance: TMARSEngineRegistry;
    class function GetInstance: TMARSEngineRegistry; static;
    function GetEngine(const AName: string): TMARSEngine;
    class function GetDefaultEngine: TMARSEngine; static;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure RegisterEngine(const AEngine: TMARSEngine);
    procedure UnregisterEngine(const AEngine: TMARSEngine); overload;
    procedure UnregisterEngine(const AEngineName: string); overload;

    property Engine[const AName: string]: TMARSEngine read GetEngine; default;

    property Count: Integer read GetCount;

    class property Instance: TMARSEngineRegistry read GetInstance;
    class property DefaultEngine: TMARSEngine read GetDefaultEngine;
    class destructor ClassDestructor;
  end;

implementation

uses
  MARS.Core.Utils
  ;

function TMARSEngine.AddApplication(const AName, ABasePath: string;
  const AResources: array of string; const AParametersSliceName: string): TMARSApplication;
var
  LResource: string;
  LParametersSliceName: string;
begin
  Result := TMARSApplication.Create(Self, AName);
  try
    Result.BasePath := ABasePath;
    for LResource in AResources do
      Result.AddResource(LResource);

    LParametersSliceName := AParametersSliceName;
    if LParametersSliceName = '' then
      LParametersSliceName := AName;
    Result.Parameters.CopyFrom(Parameters, LParametersSliceName);

    Applications.Add(
      TMARSURL.CombinePath([BasePath, ABasePath])
      , Result
    );
  except
    Result.Free;
    raise
  end;
end;

procedure TMARSEngine.AddSubscriber(
  const ASubscriber: IMARSHandleRequestEventListener);
begin
  FSubscribers.Add(ASubscriber);
end;

constructor TMARSEngine.Create(const AName: string);
begin
  inherited Create;

  FName := AName;

  FApplications := TMARSApplicationDictionary.Create([doOwnsValues]);
  FCriticalSection := TCriticalSection.Create;
  FSubscribers := TList<IMARSHandleRequestEventListener>.Create;
  FParameters := TMARSParameters.Create(FName);

  // default parameters
  Parameters.Values['Port'] := 8080;
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
  FSubscribers.Free;
  inherited;
end;

procedure TMARSEngine.DoAfterHandleRequest(const AApplication: TMARSApplication;
  const AStopWatch: TStopWatch);
var
  LSubscriber: IMARSHandleRequestEventListener;
begin
  for LSubscriber in FSubscribers do
    LSubscriber.AfterHandleRequest(Self, AApplication, AStopWatch);
end;

function TMARSEngine.DoBeforeHandleRequest(const AApplication: TMARSApplication): Boolean;
var
  LSubscriber: IMARSHandleRequestEventListener;
begin
  Result := True;
  for LSubscriber in FSubscribers do
    LSubscriber.BeforeHandleRequest(Self, AApplication, Result);
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

function TMARSEngine.HandleRequest(ARequest: TWebRequest; AResponse: TWebResponse): Boolean;
var
  LApplication: TMARSApplication;
  LURL: TMARSURL;
  LApplicationPath: string;
  LStopWatch: TStopWatch;
begin
  Result := False;

  LURL := TMARSURL.Create(ARequest);
  try
    if Assigned(FOnBeforeHandleRequest) and not FOnBeforeHandleRequest(Self, LURL) then
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

    if FApplications.TryGetValue(LApplicationPath, LApplication) then
    begin
      LURL.BasePath := LApplicationPath;
      FWebRequest := ARequest;
      FWebResponse := AResponse;
      FURL := LURL;
      if DoBeforeHandleRequest(LApplication) then begin
        LStopWatch := TStopwatch.StartNew;
        LApplication.HandleRequest(ARequest, AResponse, LURL);
        LStopWatch.Stop;
        DoAfterHandleRequest(LApplication, LStopWatch);
      end;
      Result := True;
    end
    else
      raise EMARSEngineException.Create(
        Format('Bad request [%s]: unknown application [%s]', [LURL.URL, LApplicationPath])
        , 404
      );
  finally
    LURL.Free;
  end;
end;

procedure TMARSEngine.RemoveSubscriber(
  const ASubscriber: IMARSHandleRequestEventListener);
begin
  FSubscribers.Remove(ASubscriber);
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

procedure TMARSEngine.SetThreadPoolSize(const Value: Integer);
begin
  Parameters['ThreadPoolSize'] := Value;
end;

function TMARSEngine.GetCurrentRequest: TWebRequest;
begin
  Result := FWebRequest;
end;

function TMARSEngine.GetCurrentResponse: TWebResponse;
begin
  Result := FWebResponse;
end;

function TMARSEngine.GetCurrentURL: TMARSURL;
begin
  Result := FURL;
end;

function TMARSEngine.GetPort: Integer;
begin
  Result := Parameters['Port'].AsInteger;
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
end;

destructor TMARSEngineRegistry.Destroy;
begin
  FItems.Free;
  inherited;
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
  FItems.AddOrSetValue(AEngine.Name, AEngine);
end;

procedure TMARSEngineRegistry.UnregisterEngine(const AEngine: TMARSEngine);
begin
  Assert(Assigned(AEngine));
  UnregisterEngine(AEngine.Name);
end;

procedure TMARSEngineRegistry.UnregisterEngine(const AEngineName: string);
begin
  FItems.Remove(AEngineName);
end;

end.
