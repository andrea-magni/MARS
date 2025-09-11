(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

program MARSTemplateServerFCGI;

{$APPTYPE CONSOLE}

{$I MARS.inc}

uses
  System.SysUtils,
  System.Types,
  IPPeerServer,
  IPPeerAPI,
  Web.FastCGIApp,
  Web.WebReq,
  Web.WebBroker,
  ServerConst in 'ServerConst.pas',
  Server.Ignition in 'Server.Ignition.pas',
  Server.Resources.HelloWorld in 'Server.Resources.HelloWorld.pas',
  Server.Resources.OpenAPI in 'Server.Resources.OpenAPI.pas',
  Server.Resources.Token in 'Server.Resources.Token.pas',
  Server.WebModule in 'Server.WebModule.pas' {ServerWebModule: TWebModule};

{$R *.res}

procedure StartServer(const AServer: TFastCGIApplication);
begin
  if not AServer.Active then
  begin
    AServer.DefaultPort := TServerEngine.Default.Port;
    Writeln(Format(sStartingServer, [AServer.DefaultPort]));
    AServer.Active := True;
  end
  else
    Writeln(sServerRunning);
  Write(cArrow);
end;

procedure StopServer(const AServer: TFastCGIApplication);
begin
  if AServer.Active then
  begin
    Writeln(sStoppingServer);
    AServer.Active := False;
    Writeln(sServerStopped);
  end
  else
    Writeln(sServerNotRunning);
  Write(cArrow);
end;

procedure SetPort(const AServer: TFastCGIApplication; APort: String);
var
  LPort: Integer;
  LWasActive: Boolean;
begin
  LPort := StrToIntDef(APort, -1);
  if LPort = -1 then
  begin
    Writeln('Port should be an integer number. Try again.');
    Exit;
  end;

  LWasActive := AServer.Active;
  if LWasActive  then
    StopServer(AServer);
  TServerEngine.Default.Port := LPort;
  if LWasActive then
    StartServer(AServer);
  Writeln(Format(sPortSet, [IntToStr(TServerEngine.Default.Port)]));
  Write(cArrow);
end;


function BindPort(APort: Integer): Boolean;
var
  LTestServer: IIPTestServer;
begin
  Result := True;
  try
    LTestServer := PeerFactory.CreatePeer('', IIPTestServer) as IIPTestServer;
    LTestServer.TestOpenPort(APort, nil);
  except
    Result := False;
  end;
end;

function CheckPort(APort: Integer): Integer;
begin
  if BindPort(APort) then
    Result := APort
  else
    Result := 0;
end;


procedure SetLog(const AServer: TFastCGIApplication; ALog: String);
begin
  ALog := ALog.Replace(cCommandSetLog, '').Trim;
  if sametext(ALog, CLogDebug) then
    AServer.MinSeverity := TLogSeverity.lsDebug
  else if sametext(ALog, CLogInfo) then
    AServer.MinSeverity := TLogSeverity.lsInfo
  else if sametext(ALog, CLogWarning) then
    AServer.MinSeverity := TLogSeverity.lsWarning
  else if sametext(ALog, CLogError) then
    AServer.MinSeverity := TLogSeverity.lsError
  else
    AServer.MinSeverity := TLogSeverity.lsInfo;
  Write(cArrow);
end;



procedure WriteCommands;
begin
  Writeln(sCommands);
  Write(cArrow);
end;

procedure WriteStatus(const AServer: TFastCGIApplication);
const
  CLogSeverity: array [TLogSeverity] of string =
    (cLogDebug, cLogInfo, cLogWarning, cLogError);
begin
  Writeln(sRTLVersion + (GetRTLVersion shr 8).ToString + '.' + (GetRTLVersion and $FF).ToString);
  Writeln(sActive + AServer.Active.ToString(TUseBoolStrs.True));
  Writeln(sPort + AServer.DefaultPort.ToString);
  Writeln(sLogging + CLogSeverity[AServer.MinSeverity]);
  Write(cArrow);
end;

procedure RunServer();
var
  LServer: TFastCGIApplication;
  LResponse: string;
begin
  WriteCommands;
  LServer := TFastCGIApplication(Application);
  while True do
  begin
    Readln(LResponse);
    LResponse := LowerCase(LResponse);
    if LResponse.StartsWith(cCommandSetLog) then
      SetLog(LServer, LResponse)
    {$ifdef DelphiXE3_UP}
    else if LResponse.StartsWith(cCommandSetPort, True) then
      SetPort(LServer, LResponse.Split([' '])[2])
    {$else}
    else if AnsiStartsText(cCommandSetPort, LResponse) then
      SetPort(LServer, Copy(LResponse, Length(cCommandSetPort)+1, MAXINT))
    {$endif}
    else if sametext(LResponse, cCommandStart) then
      StartServer(LServer)
    else if sametext(LResponse, cCommandStatus) then
      WriteStatus(LServer)
    else if sametext(LResponse, cCommandStop) then
      StopServer(LServer)
    else if sametext(LResponse, cCommandHelp) then
      WriteCommands
    else if sametext(LResponse, cCommandExit) then
      if LServer.Active then
      begin
        StopServer(LServer);
        break
      end
      else
        break
    else
    begin
      Writeln(sInvalidCommand);
      Write(cArrow);
    end;
  end;
end;

begin
  try
    Application.WebModuleClass := WebModuleClass;
    Application.Initialize;
    RunServer();
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end
end.
