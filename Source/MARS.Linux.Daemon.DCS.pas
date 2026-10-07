(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS

  Linux daemon hosting the MARS engine on Delphi Cross Socket: same classes of
  MARS.Linux.Daemon (Indy), use one unit or the other.
*)
unit MARS.Linux.Daemon.DCS;

{$I MARS.inc}

{$IFDEF LINUX}

interface

uses
  Classes, SysUtils
, Posix.Stdlib, Posix.SysStat, Posix.SysTypes, Posix.Unistd, Posix.Signal, Posix.Fcntl
, MARS.http.Server.DCS
;

const EXIT_CODE_SUCCESS = 0;
const EXIT_CODE_FAILURE = 1;

type
  TLinuxDaemon = class
  private
    FTerminated: Boolean;
  protected
    procedure DoLog(const AMsg: string); virtual;
    procedure DoError(const AMsg: string = ''; const AFatal: Boolean = True);
  protected
    procedure DoExecute; virtual;
  public
    constructor Create; virtual;
    procedure Start; virtual;
    procedure Terminate; virtual;

    property Terminated: Boolean read FTerminated;
  end;

  TMARSDaemon = class(TLinuxDaemon)
  private
    class var _Instance: TMARSDaemon;
  private
    FName: string;
  protected
    FServer: TMARShttpServerDCS;

    procedure DoExecute; override;
    procedure DoLog(const AMsg: string); override;

    procedure StartServer; virtual;
    procedure StopServer; virtual;
    procedure IdleCycle;
  public
    constructor Create; override;
    procedure Start; override;
    procedure Log(const AMsg: string);

    property Name: string read FName write FName;

    class function Current: TMARSDaemon;
    class constructor ClassCreate;
  end;

  procedure SignalsHandler(ASigNum: Integer); cdecl;

implementation

uses
  Types, StrUtils
, Server.Ignition
;

procedure SignalsHandler(ASigNum: Integer); cdecl;
begin
  case ASigNum of
    SIGTERM:
    begin
      TMARSDaemon.Current.Log('*** SIGTERM ***');
      TMARSDaemon.Current.Terminate;
    end;
    SIGHUP:
    begin
      TMARSDaemon.Current.Log('*** SIGHUP ***');
    end;
  end;
end;


{ TLinuxDaemon }

constructor TLinuxDaemon.Create;
begin
  inherited Create;
  FTerminated := False;
end;

procedure TLinuxDaemon.DoError(const AMsg: string; const AFatal: Boolean);
begin
  DoLog(AMsg);
  if AFatal then
    Halt(EXIT_CODE_FAILURE);
end;

procedure TLinuxDaemon.DoExecute;
begin

end;

procedure TLinuxDaemon.DoLog(const AMsg: string);
begin
  WriteLn(AMsg);
end;

procedure TLinuxDaemon.Start;
var
  Lpid, Lsid: pid_t;
  Lfid: Integer;
begin
  signal(SIGTERM, SignalsHandler);
  signal(SIGHUP, SignalsHandler);

  Lpid := fork();
  if Lpid < 0 then
    DoError('fork failed');
  if Lpid > 0  then
  begin
//      DoLog('fork done. Here is parent process. Will quit now.');
    Halt(0);
  end;
  if Lpid = 0 then
  begin
    DoLog('Forked process id ' + getpid.ToString);
    // INIT FILESYSTEM PERMISSION MODE
    umask(027);

    // ACQUIRE NEW SESSION ID
    Lsid := setsid;
    if Lsid < 0 then
      DoError('Unable to set sid');

    // CLOSE AND REROUTE STDIN/OUT/ERR
    __close(STDIN_FILENO);
    __close(STDOUT_FILENO);
    __close(STDERR_FILENO);

    Lfid := __open('/dev/null', O_RDWR);
    dup(Lfid); // STDOUT
    dup(Lfid); // STDERR

    // MOVE TO AN ALWAYS EXISTING DIRECTORY
    ChDir('/');

    // run actual daemon code
    DoExecute;

    halt(0);
  end;
end;

procedure TLinuxDaemon.Terminate;
begin
  FTerminated := True;
end;

{ TMARSDaemon }

class constructor TMARSDaemon.ClassCreate;
begin
  _Instance := nil;
end;

constructor TMARSDaemon.Create;
begin
  inherited Create;
  FName := 'MARSDaemon';
end;

class function TMARSDaemon.Current: TMARSDaemon;
begin
  if not Assigned(_Instance) then
    _Instance := TMARSDaemon.Create;
  Result := _Instance;
end;

procedure TMARSDaemon.DoExecute;
begin
  inherited;
  FServer := TMARShttpServerDCS.Create(TServerEngine.Default);
  try
    // http port (Port parameter, default 8080, 0 disables http)
    FServer.DefaultPort := TServerEngine.Default.Port;
    // https (0 = disabled): PortSSL, DCS.SSL.CertFile and DCS.SSL.KeyFile parameters, libssl
    // installed (i.e. package libssl3), see https://andrea-magni.github.io/MARS/server/engine#https
    FServer.SSLPort := TServerEngine.Default.PortSSL;

    StartServer;

    IdleCycle;
  finally
    FServer.Free;
  end;
end;

procedure TMARSDaemon.DoLog(const AMsg: string);
var
  LExeFileName, LLogFileName: string;
  LFileStream: TFileStream;
  LBytes: TBytes;
begin
//  inherited DoLog('[' + Name +'] ' + AMsg);

  LExeFileName := ParamStr(0);
  LLogFileName := ChangeFileExt(LExeFileName, '.log');

  // appended to the log file (fmCreate alone truncated it at every line)
  if FileExists(LLogFileName) then
    LFileStream := TFileStream.Create(LLogFileName, fmOpenWrite or fmShareDenyWrite)
  else
    LFileStream := TFileStream.Create(LLogFileName, fmCreate or fmShareDenyWrite);
  try
    LFileStream.Seek(0, soEnd);
    LBytes := TEncoding.UTF8.GetBytes(string.join('|', [DateTimeToStr(Now), Name, AMsg]) + sLineBreak);
    LFileStream.Write(LBytes, Length(LBytes));
  finally
    LFileStream.Free;
  end;
end;

procedure TMARSDaemon.IdleCycle;
begin
  Log('Started');
  while not Terminated do
    Sleep(500);
  Log('Stopping');
end;

procedure TMARSDaemon.Log(const AMsg: string);
begin
  DoLog(AMsg);
end;

procedure TMARSDaemon.Start;
begin
  Log('Starting...');
  inherited;
end;

procedure TMARSDaemon.StartServer;
begin
  try
    FServer.Active := True;
  except
    on E: Exception do
      DoError('The server could not start: ' + E.Message);
  end;
end;

procedure TMARSDaemon.StopServer;
begin
  FServer.Active := False;
end;

{$ENDIF}

end.
