(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

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
  , MARS.Core.Token
  , MARS.Utils.Parameters
;

{$M+}

type
  TMARSEngine = class;

  IMARSHandleRequestEventListener = interface
    procedure BeforeHandleRequest(const ASender: TMARSEngine; const AApplication: TMARSApplication);
    procedure AfterHandleRequest(const ASender: TMARSEngine; const AApplication: TMARSApplication; const AStopWatch: TStopWatch);
  end;

  TMARSEngine = class
  private
    FApplications: TMARSApplicationDictionary;
    FSubscribers: TList<IMARSHandleRequestEventListener>;
    FCriticalSection: TCriticalSection;
    FParameters: TMARSParameters;
    FName: string;
   private
    class threadvar FWebRequest: TWebRequest;
    class threadvar FWebResponse: TWebResponse;
    class threadvar FURL: TMARSURL;
    class threadvar FToken: TMARSToken;

    function GetCurrentRequest: TWebRequest;
    function GetCurrentResponse: TWebResponse;
    function GetCurrentURL: TMARSURL;
    function GetCurrentToken: TMARSToken;
    function GetBasePath: string;
    function GetPort: Integer;
    function GetThreadPoolSize: Integer;
    procedure SetBasePath(const Value: string);
    procedure SetPort(const Value: Integer);
    procedure SetThreadPoolSize(const Value: Integer);
  protected
    procedure DoBeforeHandleRequest(const AApplication: TMARSApplication); virtual;
    procedure DoAfterHandleRequest(const AApplication: TMARSApplication; const AStopWatch: TStopWatch); virtual;
  public
    constructor Create(const AName: string = 'DefaultEngine'); virtual;
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
    property CurrentToken: TMARSToken read GetCurrentToken;
    property CurrentRequest: TWebRequest read GetCurrentRequest;
    property CurrentResponse: TWebResponse read GetCurrentResponse;
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
end;

destructor TMARSEngine.Destroy;
begin
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

procedure TMARSEngine.DoBeforeHandleRequest(const AApplication: TMARSApplication);
var
  LSubscriber: IMARSHandleRequestEventListener;
begin
  for LSubscriber in FSubscribers do
    LSubscriber.BeforeHandleRequest(Self, AApplication);
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
    LApplicationPath := TMARSURL.CombinePath([LURL.PathTokens[0]]);
    if (BasePath <> '') and (BasePath <> TMARSURL.URL_PATH_SEPARATOR) then
    begin
      if not LURL.MatchPath(BasePath) then
        raise Exception.CreateFmt('[Engine] Requested URL [%s] does not match base engine URL [%s]', [LURL.URL, BasePath]);
      LApplicationPath := TMARSURL.CombinePath([LURL.PathTokens[0], LURL.PathTokens[1]]);
    end;

    if FApplications.TryGetValue(LApplicationPath, LApplication) then
    begin
      LURL.BasePath := LApplicationPath;
      FWebRequest := ARequest;
      FWebResponse := AResponse;
      FURL := LURL;
      FToken := TMARSToken.Create(FWebRequest
        , LApplication.Parameters.ByName(TMARSToken.JWT_SECRET_PARAM, TMARSToken.JWT_SECRET_PARAM_DEFAULT).AsString
      );
      try
        DoBeforeHandleRequest(LApplication);
        LStopWatch := TStopwatch.StartNew;
        LApplication.HandleRequest(ARequest, AResponse, LURL);
        LStopWatch.Stop;
        DoAfterHandleRequest(LApplication, LStopWatch);
      finally
        FToken.Free;
      end;

      Result := True;
    end
    else
      raise Exception.CreateFmt('[Engine] Requested URL [%s] belongs to an unknown application [%s]', [LURL.URL, LApplicationPath]);
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

function TMARSEngine.GetCurrentToken: TMARSToken;
begin
  Result := FToken;
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

end.
