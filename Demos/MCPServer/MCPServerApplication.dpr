(*
  Copyright 2026, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

program MCPServerApplication;

{$APPTYPE CONSOLE}

{$I MARS.inc}

uses
  SysUtils,
  StrUtils,
  Types,
  MARS.http.Server.Indy,
  Server.Ignition in 'Server.Ignition.pas',
  Server.Resources in 'Server.Resources.pas',
  Server.Resources.Token in 'Server.Resources.Token.pas',
  Server.Resources.DB in 'Server.Resources.DB.pas',
  ServerConst in 'ServerConst.pas';

procedure StartServer(const AServer: TMARShttpServerIndy);
begin
  if not (AServer.Active) then
  begin
    AServer.DefaultPort := TServerEngine.Default.Port;
    Writeln(Format(sStartingServer, [AServer.DefaultPort]));
    Writeln('- MCP endpoint (Streamable HTTP): http://localhost:'
      + AServer.DefaultPort.ToString + TServerEngine.Default.BasePath + '/default/mcp');
    AServer.Active := True;
  end
  else
    Writeln(sServerRunning);
  Write(cArrow);
end;

procedure StopServer(const AServer: TMARShttpServerIndy);
begin
  if AServer.Active  then
  begin
    Writeln(sStoppingServer);
    AServer.Active := False;
    Writeln(sServerStopped);
  end
  else
    Writeln(sServerNotRunning);
  Write(cArrow);
end;

procedure SetPort(const AServer: TMARShttpServerIndy; const APort: string);
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

procedure  WriteCommands;
begin
  Writeln(sCommands);
  Write(cArrow);
end;

procedure  WriteStatus(const AServer: TMARShttpServerIndy);
begin
  Writeln(sIndyVersion + AServer.SessionList.Version);
  Writeln(sActive + BoolToStr(AServer.Active, True));
  Writeln(sPort + IntToStr(TServerEngine.Default.Port));
  Write(cArrow);
end;

procedure RunServer();
var
  LServer: TMARShttpServerIndy;
  LResponse: string;
begin
  WriteCommands;

  LServer := TMARShttpServerIndy.Create(TServerEngine.Default);
  try
    StartServer(LServer);

    while True do
    begin
      Readln(LResponse);
      LResponse := LowerCase(LResponse);
      if sametext(LResponse, cCommandStart) then
        StartServer(LServer)
      else if sametext(LResponse, cCommandStatus) then
        WriteStatus(LServer)
      else if sametext(LResponse, cCommandStop) then
        StopServer(LServer)
      else if LResponse.StartsWith(cCommandSetPort, True) then
        SetPort(LServer, LResponse.Split([' '])[2])
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
    RunServer();
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end
end.
