unit Server;

interface

uses
  Classes, SysUtils
, MARS.Core.Engine, MARS.Core.Application
, MARS.Utils.Parameters
, MARS.http.Server.Indy
;

type
  TServer = class
  // class declarations
  private
    class var _Instance: TServer;
  protected
    class function GetInstance: TServer; static;
  // instance declarations
  private
    FEngine: TMARSEngine;
    FApplication: TMARSApplication;
    FServer: TMARShttpServerIndy;
    FEngineBasePath: string;
    FEnginePort: Integer;
    FThreadPoolSize: Integer;
    FOnStatusChange: TProc<TServer>;
    FApplicationPath: string;
    FApplicationName: string;
    FApplicationResources: TArray<string>;
    FBeforeStart: TProc<TServer, TMARSParameters>;
    procedure SetEngineBasePath(const Value: string);
    procedure SetEnginePort(const Value: Integer);
    procedure SetThreadPoolSize(const Value: Integer);
    procedure SetApplicationName(const Value: string);
    procedure SetApplicationPath(const Value: string);
    procedure SetApplicationResources(const Value: TArray<string>);
  protected
    procedure CheckNotRunning;
    procedure DoBeforeStart;
    procedure DoStatusChanged;
  public
    constructor Create; virtual;

    procedure Start;
    procedure Stop;
    function IsRunning: Boolean;
    procedure ForEachApplicationResource(const ADoSomething: TProc<string, string>);

    property EngineBasePath: string read FEngineBasePath write SetEngineBasePath;
    property EnginePort: Integer read FEnginePort write SetEnginePort;
    property ThreadPoolSize: Integer read FThreadPoolSize write SetThreadPoolSize;

    property ApplicationName: string read FApplicationName write SetApplicationName;
    property ApplicationPath: string read FApplicationPath write SetApplicationPath;
    property ApplicationResources: TArray<string> read FApplicationResources write SetApplicationResources;

    property BeforeStart: TProc<TServer, TMARSParameters> read FBeforeStart write FBeforeStart;
    property OnStatusChange: TProc<TServer> read FOnStatusChange write FOnStatusChange;
    property HttpServer: TMARShttpServerIndy read FServer;

    class property Instance: TServer read GetInstance;
  end;


implementation

uses
  Generics.Collections
, MARS.Core.Registry.Utils, MARS.mORMotJWT.Token, MARS.Core.MessageBodyWriters
, MARS.Core.Activation, MARS.Utils.Parameters.IniFile
, Server.Resources
;

procedure TServer.SetApplicationName(const Value: string);
begin
  if FApplicationName <> Value then
  begin
    CheckNotRunning;
    FApplicationName := Value;
    DoStatusChanged;
  end;
end;

procedure TServer.SetApplicationPath(const Value: string);
begin
  if FApplicationPath <> Value then
  begin
    CheckNotRunning;
    FApplicationPath := Value;
    DoStatusChanged;
  end;
end;

procedure TServer.SetApplicationResources(const Value: TArray<string>);
begin
  if FApplicationResources <> Value then
  begin
    CheckNotRunning;
    FApplicationResources := Value;
    DoStatusChanged;
  end;
end;

procedure TServer.SetEngineBasePath(const Value: string);
begin
  if FEngineBasePath <> Value then
  begin
    CheckNotRunning;
    FEngineBasePath := Value;
    DoStatusChanged;
  end;
end;

procedure TServer.SetEnginePort(const Value: Integer);
begin
  if FEnginePort <> Value then
  begin
    CheckNotRunning;
    FEnginePort := Value;
    DoStatusChanged;
  end;
end;

procedure TServer.SetThreadPoolSize(const Value: Integer);
begin
  if FThreadPoolSize <> Value then
  begin
    CheckNotRunning;
    FThreadPoolSize := Value;
    DoStatusChanged;
  end;
end;

procedure TServer.Start;
begin
  if IsRunning then
    Stop;

  FEngine := TMARSEngine.Create;
  FEngine.BasePath := FEngineBasePath;
  FEngine.Port := FEnginePort;
  FEngine.ThreadPoolSize := FThreadPoolSize;
  FEngine.Parameters.LoadFromIniFile;

  FApplication := FEngine.AddApplication(FApplicationName, FApplicationPath, FApplicationResources);

  DoBeforeStart;
  FServer := TMARShttpServerIndy.Create(FEngine);
  FServer.Active := True;
  DoStatusChanged;
end;

procedure TServer.Stop;
begin
  FServer.Active := False;
  FreeAndNil(FServer);
  FreeAndNil(FEngine);
  FApplication := nil;
  DoStatusChanged;
end;

function TServer.IsRunning: Boolean;
begin
  Result := Assigned(FEngine) and Assigned(FServer) and (FServer.Active);
end;

procedure TServer.CheckNotRunning;
begin
  if IsRunning then
    raise Exception.Create('Server is running');
end;

constructor TServer.Create;
begin
  inherited Create;

  // defaults
  FEngineBasePath := '/rest';
  FEnginePort := 8080;
  FThreadPoolSize := 25;
  FApplicationPath := '/default';
  FApplicationName := 'DefaultApp';
  FApplicationResources := ['Server.Resources.*'];
end;

procedure TServer.DoBeforeStart;
begin
  if Assigned(FBeforeStart) then
    FBeforeStart(Self, FEngine.Parameters);
end;

procedure TServer.DoStatusChanged;
begin
  if Assigned(FOnStatusChange) then
    FOnStatusChange(Self);
end;

procedure TServer.ForEachApplicationResource(const ADoSomething: TProc<string, string>);
var
  LResource: TPair<string, TMARSConstructorInfo>;
begin
  if Assigned(FApplication) and Assigned(ADoSomething) then
  begin
    for LResource in FApplication.Resources do
      ADoSomething(LResource.Key, LResource.Value.TypeTClass.ClassName);
  end;
end;

class function TServer.GetInstance: TServer;
begin
  if not Assigned(_Instance) then
    _Instance := TServer.Create;
  Result := _Instance
end;

end.
