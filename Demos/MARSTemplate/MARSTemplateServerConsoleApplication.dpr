(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
program MARSTemplateServerConsoleApplication;
{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Types,
  IPPeerServer,
  IPPeerAPI,
  IdHTTPWebBrokerBridge,
  Web.WebReq,
  Web.WebBroker,
  ServerConst in 'ServerConst.pas',
  Server.WebModule in 'Server.WebModule.pas' {ServerWebModule: TWebModule},
  Server.Ignition in 'Server.Ignition.pas';

{$R *.res}

function BindPort(Aport: Integer): Boolean;
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

function CheckPort(Aport: Integer): Integer;
begin
  if BindPort(Aport) then
    Result := Aport
  else
    Result := 0;
end;

procedure SetPort(const Aserver: TIdHTTPWebBrokerBridge; APort: String);
begin
  if not (Aserver.Active) then
  begin
    APort := APort.Replace(cCommandSetPort, '').Trim;
    if CheckPort(APort.ToInteger) > 0 then
    begin
      Aserver.DefaultPort := APort.ToInteger;
      Writeln(Format(sPortSet, [APort]));
    end
    else
      Writeln(Format(sPortInUse, [Aport]));
  end
  else
    Writeln(sServerRunning);
  Write(cArrow);
end;

procedure StartServer(const Aserver: TIdHTTPWebBrokerBridge);
begin
  if not (Aserver.Active) then
  begin
    if CheckPort(Aserver.DefaultPort) > 0 then
    begin
      Writeln(Format(sStartingServer, [Aserver.DefaultPort]));
      Aserver.Active := True;
    end
    else
      Writeln(Format(sPortInUse, [Aserver.DefaultPort]));
  end
  else
    Writeln(sServerRunning);
  Write(cArrow);
end;

procedure StopServer(const Aserver: TIdHTTPWebBrokerBridge);
begin
  if Aserver.Active  then
  begin
    Writeln(sStoppingServer);
    Aserver.Active := False;
    Writeln(sServerStopped);
  end
  else
    Writeln(sServerNotRunning);
  Write(cArrow);
end;

procedure  WriteCommands;
begin
  Writeln(sCommands);
  Write(cArrow);
end;

procedure  WriteStatus(const Aserver: TIdHTTPWebBrokerBridge);
begin
  Writeln(sIndyVersion + Aserver.SessionList.Version);
  Writeln(sActive + Aserver.Active.ToString(TUseBoolStrs.True));
  Writeln(sPort + Aserver.DefaultPort.ToString);
  Write(cArrow);
end;

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
  LResponse: string;
begin
  WriteCommands;
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    while True do
    begin
      Readln(LResponse);
      LResponse := LowerCase(LResponse);
      if LResponse.StartsWith(cCommandSetPort) then
        SetPort(LServer, LResponse)
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
  finally
    LServer.Free;
  end;
end;

begin
  try
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
    RunServer(8080);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end
end.
