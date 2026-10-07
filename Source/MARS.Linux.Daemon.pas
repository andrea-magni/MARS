(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Linux.Daemon;

{$I MARS.inc}

{$IFDEF LINUX}

interface

uses
  Classes, SysUtils
, Posix.Stdlib, Posix.SysStat, Posix.SysTypes, Posix.Unistd, Posix.Signal, Posix.Fcntl
, MARS.http.Server.Indy
;

const EXIT_CODE_SUCCESS = 0;
const EXIT_CODE_FAILURE = 1;

type
  // Start runs DoExecute in a daemon process (fork, new session, stdio to /dev/null) or, when
  // Foreground is True, in the current process: the way Docker (main process of the
  // container) and systemd (Type=simple) expect it. Foreground defaults to True when the
  // command line has --foreground (or -f).
  TLinuxDaemon = class
  private
    FTerminated: Boolean;
    FForeground: Boolean;
    FLastSignal: Integer;
  protected
    procedure DoLog(const AMsg: string); virtual;
    procedure DoError(const AMsg: string = ''; const AFatal: Boolean = True);
  protected
    procedure DoExecute; virtual;
  public
    constructor Create; virtual;
    procedure Start; virtual;
    procedure Terminate; virtual;
    // --foreground or -f on the command line
    class function ForegroundRequested: Boolean;

    property Terminated: Boolean read FTerminated;
    property Foreground: Boolean read FForeground write FForeground;
    // last SIGTERM/SIGINT/SIGHUP received (0 = none)
    property LastSignal: Integer read FLastSignal;
  end;

  TMARSDaemon = class(TLinuxDaemon)
  private
    class var _Instance: TMARSDaemon;
  private
    FName: string;
  protected
    FServer: TMARShttpServerIndy;

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

// only sets flags: the daemon logs and stops outside of the signal handler
procedure SignalsHandler(ASigNum: Integer); cdecl;
begin
  TMARSDaemon.Current.FLastSignal := ASigNum;
  case ASigNum of
    SIGTERM, SIGINT: TMARSDaemon.Current.Terminate;
  end;
end;


{ TLinuxDaemon }

constructor TLinuxDaemon.Create;
begin
  inherited Create;
  FTerminated := False;
  FForeground := ForegroundRequested;
end;

class function TLinuxDaemon.ForegroundRequested: Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := 1 to ParamCount do
    if SameText(ParamStr(LIndex), '--foreground') or SameText(ParamStr(LIndex), '-f') then
      Exit(True);
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
  signal(SIGINT, SignalsHandler);
  signal(SIGHUP, SignalsHandler);

  if Foreground then
  begin
    DoLog('Running in foreground, process id ' + getpid.ToString);
    DoExecute;
    Exit;
  end;

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
  FServer := TMARShttpServerIndy.Create(TServerEngine.Default);
  try
    // http port, default is 8080, set 0 to disable http
    // you can specify 'Port' parameter or hard-code value here
//    FServer.Engine.Port := 80;

// to enable Indy standalone SSL -----------------------------------------------
//------------------------------------------------------------------------------
//    default https port value is 0, use PortSSL parameter or hard-code value here
//    FServer.Engine.PortSSL := 443;
// Available parameters:
//     'PortSSL', default: 0 (disabled)
//     'Indy.SSL.RootCertFile', default: 'localhost.pem' (bin folder)
//     'Indy.SSL.CertFile', default: 'localhost.crt' (bin folder)
//     'Indy.SSL.KeyFile', default: 'localhost.key' (bin folder)
// if needed, setup additional event handlers or properties
//    FServer.SSLIOHandler.OnGetPassword := YourGetPasswordHandler;
//    FServer.SSLIOHandler.OnVerifyPeer := YourVerifyPeerHandler;
//    FServer.SSLIOHandler.SSLOptions.VerifyDepth := 1;
//------------------------------------------------------------------------------

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
  // foreground: standard output, collected by docker logs / journald
  if Foreground then
  begin
    WriteLn(string.join('|', [DateTimeToStr(Now), Name, AMsg]));
    Flush(Output);
    Exit;
  end;

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
  begin
    Sleep(500);
    if LastSignal = SIGHUP then
    begin
      FLastSignal := 0;
      Log('SIGHUP');
    end;
  end;
  case LastSignal of
    SIGTERM: Log('Stopping (SIGTERM)');
    SIGINT: Log('Stopping (SIGINT)');
  else
    Log('Stopping');
  end;
  StopServer;
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
  FServer.Active := True;
end;

procedure TMARSDaemon.StopServer;
begin
  FServer.Active := False;
end;

{$ENDIF}

end.
