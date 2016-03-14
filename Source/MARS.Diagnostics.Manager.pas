(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit MARS.Diagnostics.Manager;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, StrUtils
  , Generics.Collections

  , SyncObjs
  , MARS.Core.JSON
  , Diagnostics

  , MARS.Core.Classes
  , MARS.Core.URL
  , MARS.Core.Token
  , MARS.Core.Engine
  , MARS.Core.Application
  ;

type
  TMARSDiagnosticInfo = class
  private
    FRequestCount: Integer;
    FLastRequestTime: TDateTime;
    FCriticalSection: TCriticalSection;
    FBasePath: string;
    FTotalExecutionTime: Int64;
    function GetAverageTimePerRequest: Double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AcquireRequest(AURL: TMARSURL; AExecutionTimeInMilliseconds: Integer = 0); virtual;

    function ToJSON: TJSONObject; virtual;

    property BasePath: string read FBasePath write FBasePath;
    property RequestCount: Integer read FRequestCount;
    property LastRequestTime: TDateTime read FLastRequestTime;
    property TotalExecutionTime: Int64 read FTotalExecutionTime;
    property AverageTimePerRequest: Double read GetAverageTimePerRequest;
  end;

  TMARSDiagnosticAppInfo = class(TMARSDiagnosticInfo)
  end;

  TMARSDiagnosticEngineInfo = class(TMARSDiagnosticInfo)
  private
    FLastSessionEnd: TDateTime;
    FActiveSessionCount: Integer;
    FSessionCount: Integer;
    FLastSessionStart: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AcquireNewSession();
    procedure AcquireEndSession();

    function ToJSON: TJSONObject; override;

    property ActiveSessionCount: Integer read FActiveSessionCount;
    property SessionCount: Integer read FSessionCount;
    property LastSessionStart: TDateTime read FLastSessionStart;
    property LastSessionEnd: TDateTime read FLastSessionEnd;
  end;

  TMARSDiagnosticsManager = class(TNonInterfacedObject, IMARSHandleRequestEventListener)
  private
  private
    FEngineInfo: TMARSDiagnosticEngineInfo;
    FAppDictionary: TObjectDictionary<string, TMARSDiagnosticAppInfo>;
    FCriticalSection: TCriticalSection;
  protected
    class var _Instance: TMARSDiagnosticsManager;
    class function GetInstance: TMARSDiagnosticsManager; static;
    function GetAppInfo(const App: string; const ADoSomething: TProc<TMARSDiagnosticAppInfo>): Boolean; overload;
  public
    constructor Create;
    destructor Destroy; override;

    function ToJSON: TJSONObject; virtual;

    procedure RetrieveAppInfo(const App: string; const ADoSomething: TProc<TMARSDiagnosticAppInfo>);

    class property Instance: TMARSDiagnosticsManager read GetInstance;

    // IMARSTokenEventListener
    procedure OnTokenStart(const AToken: string);
    procedure OnTokenEnd(const AToken: string);

    // IMARSHandleRequestEventListener
    procedure BeforeHandleRequest(const ASender: TMARSEngine; const AApplication: TMARSApplication);
    procedure AfterHandleRequest(const ASender: TMARSEngine; const AApplication: TMARSApplication; const AStopWatch: TStopWatch);
    class var FEngine: TMARSEngine; //TODO: remove, as you remove the singleton from TMARSDiagnosticsManager
    class destructor ClassDestructor;
  end;


implementation

uses
    Math
  , DateUtils
  , MARS.Core.Utils
  ;

{ TMARSDiagnosticsManager }


procedure TMARSDiagnosticsManager.AfterHandleRequest(const ASender: TMARSEngine;
  const AApplication: TMARSApplication; const AStopWatch: TStopWatch);
var
  LStopWatch: TStopwatch;
begin
  LStopWatch := AStopWatch;

  GetAppInfo(AApplication.Name,
    procedure (AAppInfo: TMARSDiagnosticAppInfo)
    begin
      AAppInfo.AcquireRequest(FEngine.CurrentURL, LStopWatch.ElapsedMilliseconds);

      FCriticalSection.Enter;
      try
        FEngineInfo.AcquireRequest(FEngine.CurrentURL, LStopWatch.ElapsedMilliseconds);
      finally
        FCriticalSection.Leave;
      end
    end
  );
end;

procedure TMARSDiagnosticsManager.BeforeHandleRequest(const ASender: TMARSEngine;
  const AApplication: TMARSApplication);
begin

end;

class destructor TMARSDiagnosticsManager.ClassDestructor;
begin
  if Assigned(_Instance) then
    FreeAndNil(_Instance);
end;

constructor TMARSDiagnosticsManager.Create;
begin
  FAppDictionary := TObjectDictionary<string, TMARSDiagnosticAppInfo>.Create([doOwnsValues]);
  FCriticalSection := TCriticalSection.Create;

  FEngineInfo := TMARSDiagnosticEngineInfo.Create;

  inherited Create;

  FEngine.AddSubscriber(Self);
end;

destructor TMARSDiagnosticsManager.Destroy;
begin
  if Assigned(FEngine) then
    FEngine.RemoveSubscriber(Self);

  FEngineInfo.Free;
  FCriticalSection.Free;
  FAppDictionary.Free;
  inherited;
end;

function TMARSDiagnosticsManager.GetAppInfo(const App: string;
  const ADoSomething: TProc<TMARSDiagnosticAppInfo>): Boolean;
var
  LInfo: TMARSDiagnosticAppInfo;
  LMARSApp: TMARSApplication;
begin
  Result := False;

  FCriticalSection.Enter;
  try
    if FEngine.Applications.TryGetValue(App, LMARSApp) then // real application
    begin
      if not LMARSApp.System then // skip system app
      begin
        if not FAppDictionary.TryGetValue(App, LInfo) then // find or create
        begin
          LInfo := TMARSDiagnosticAppInfo.Create;
          LInfo.BasePath := LMARSApp.BasePath;
          FAppDictionary.Add(App, LInfo);
        end;

        if Assigned(ADoSomething) then
          ADoSomething(LInfo);
      end;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

class function TMARSDiagnosticsManager.GetInstance: TMARSDiagnosticsManager;
begin
  if not Assigned(_Instance) then
    _Instance := TMARSDiagnosticsManager.Create;
  Result := _Instance;
end;

procedure TMARSDiagnosticsManager.OnTokenEnd(const AToken: string);
begin
  inherited;
  FCriticalSection.Enter;
  try
    FEngineInfo.AcquireEndSession;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TMARSDiagnosticsManager.OnTokenStart(const AToken: string);
begin
  inherited;

  FCriticalSection.Enter;
  try
    FEngineInfo.AcquireNewSession;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TMARSDiagnosticsManager.RetrieveAppInfo(const App: string;
  const ADoSomething: TProc<TMARSDiagnosticAppInfo>);
begin
  GetAppInfo(App, ADoSomething);
end;

function TMARSDiagnosticsManager.ToJSON: TJSONObject;
var
  LObj: TJSONObject;
  LPair: TPair<string, TMARSDiagnosticAppInfo>;
  LAppArray: TJSONArray;
begin
  LObj := TJSONObject.Create;
  FCriticalSection.Enter;
  try
    LObj.AddPair('engine',
      TJSONObject.Create(
        TJSONPair.Create(
          FEngineInfo.BasePath, FEngineInfo.ToJSON
        )
      )
    );
    LAppArray := TJSONArray.Create;

    for LPair in FAppDictionary do
      LAppArray.Add(TJsonObject.Create(TJSONPair.Create(LPair.Key, LPair.Value.ToJSON)));

    LObj.AddPair('apps', LAppArray);
  finally
    FCriticalSection.Leave;
  end;

  Result := LObj;
end;

{ TAppInfo }

procedure TMARSDiagnosticInfo.AcquireRequest(AURL: TMARSURL; AExecutionTimeInMilliseconds: Integer);
begin
  FCriticalSection.Enter;
  try
    Inc(FRequestCount);
    FTotalExecutionTime := FTotalExecutionTime + AExecutionTimeInMilliseconds;
    FLastRequestTime := Now;
  finally
    FCriticalSection.Leave;
  end;
end;

constructor TMARSDiagnosticInfo.Create;
begin
  inherited Create;

  FRequestCount := 0;
  FLastRequestTime := 0;

  FCriticalSection := TCriticalSection.Create;
end;

destructor TMARSDiagnosticInfo.Destroy;
begin
  FCriticalSection.Free;
  inherited;
end;

function TMARSDiagnosticInfo.GetAverageTimePerRequest: Double;
begin
  if FRequestCount = 0 then
    Result := 0
  else
  Result := RoundTo(FTotalExecutionTime / FRequestCount, -2);
end;

function TMARSDiagnosticInfo.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;

  Result.AddPair('BasePath', BasePath);
  Result.AddPair('RequestCount', TJSONNumber.Create(FRequestCount));
  Result.AddPair('LastRequestTime', DateToISO8601(FLastRequestTime));
  Result.AddPair('TotalExecutionTime', TJSONNumber.Create(FTotalExecutionTime));
  Result.AddPair('AverageTimePerRequest', TJSONNumber.Create(AverageTimePerRequest));
end;

{ TEngineInfo }

procedure TMARSDiagnosticEngineInfo.AcquireEndSession;
begin
  FCriticalSection.Enter;
  try
    FLastSessionEnd := Now;
    Dec(FActiveSessionCount);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TMARSDiagnosticEngineInfo.AcquireNewSession;
begin
  FCriticalSection.Enter;
  try
    Inc(FSessionCount);
    Inc(FActiveSessionCount);
    FLastSessionStart := Now;
  finally
    FCriticalSection.Leave;
  end;
end;

constructor TMARSDiagnosticEngineInfo.Create;
begin
  inherited Create;
  FLastSessionEnd := 0;
  FSessionCount := 0;
  FLastSessionStart := 0;
  FActiveSessionCount := 0;

end;

destructor TMARSDiagnosticEngineInfo.Destroy;
begin

  inherited;
end;

function TMARSDiagnosticEngineInfo.ToJSON: TJSONObject;
begin
  Result := inherited ToJSON;

  Result.AddPair('SessionCount', TJSONNumber.Create(FSessionCount));
  Result.AddPair('ActiveSessionCount', TJSONNumber.Create(FActiveSessionCount));
  Result.AddPair('LastSessionStart',  DateToISO8601(FLastSessionStart));
  Result.AddPair('LastSessionEnd', DateToISO8601(FLastSessionEnd));
end;

end.
