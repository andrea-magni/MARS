program MARSMinimalConsole;

{$APPTYPE CONSOLE}

{$R *.res}

uses
//  Scalemm2,
  System.SysUtils, SyncObjs, Windows, DateUtils, StrUtils,
  MARS.Core.Engine,
  MARS.http.Server.Indy,
  MARS.mORMotJWT.Token,
  MARS.Core.Attributes,
  MARS.Core.MediaType,
  MARS.Core.Registry,
  MARS.Core.MessageBodyWriters,
  MARS.Core.Activation,
  MARS.Core.Activation.Interfaces,
  ConsoleUtils in 'ConsoleUtils.pas';

type
  [Path('helloworld')]
  THelloWorldResource = class
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;
  end;

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello World! Zażółć gęślą jaźń.';
end;

var
  FEngine: TMARSEngine;
  FServer: TMARShttpServerIndy;
  FRequestCount: Integer;
  FInvocationTime, FSetupTime: Int64;
  FTimePerReq: Double;

procedure PrintStats;
  function FixedLengthString(const AString: string; const ALength: Integer; const ALeft: Boolean = True): string;
  begin
    if ALeft then
      Result := AString + DupeString(' ', ALength - AString.Length)
    else
      Result := DupeString(' ', ALength - AString.Length) + AString;
  end;

  procedure WriteLine(const AName, AValue: string);
  begin
    Writeln(' |' + FixedLengthString(AName, 30, False) + FixedLengthString(AValue, 30) + ' |');
  end;

var
  LRequestTime: Int64;
  LInvocationTime: Int64;
  LSetupTime: Int64;
begin
  LInvocationTime := FInvocationTime;
  LSetupTime := FSetupTime;

  GotoXY(0, 3);
  Writeln('');
  Writeln(' --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---');
  WriteLine('Current time: ', DateTimeToStr(Now));
  WriteLine('Server started: ', DateTimeToStr(FServer.StartedAt));
  WriteLine('Server up time: ', FServer.UpTime.ToString);
  WriteLine('Served requests: ', FRequestCount.ToString);
  WriteLine('Setup time (ms): ', LSetupTime.ToString);
  WriteLine('Invocation time (ms): ', LInvocationTime.ToString);

  LRequestTime := LSetupTime + LInvocationTime;
  FTimePerReq := 0;
  if FRequestCount > 0 then
  begin
    FTimePerReq := LRequestTime / FRequestCount;
    WriteLine('Request time (ms/req): ', FormatFloat('#,#0.00', FTimePerReq));
    if FTimePerReq > 0 then
      WriteLine('Request per second: ', FormatFloat('#,#0.00', 1000 / FTimePerReq))
    else
      WriteLine('Request per second: ', 'INF');
  end
  else
  begin
    FTimePerReq := 0;
    WriteLine('Request time (ms/req): ', 'N/A');
    WriteLine('Request per second: ', 'N/A');
  end;

  if FServer.Active then
    WriteLine('Status: ', 'RUNNING, press Enter to stop')
  else
  begin
    WriteLine('Status: ', 'STOPPED');
    WriteLine('Server stopped: ', DateTimeToStr(FServer.StoppedAt));
  end;

  Writeln(' --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---');
  Writeln('');
end;


begin
  // ----------------------------------
  // Register resources
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  // ----------------------------------
  // Build and configure the engine and server
  FEngine := TMARSEngine.Create;
  try
    FEngine.BasePath := '/rest';
    FEngine. ThreadPoolSize := 20;
    FEngine.Port := 8080;
    FEngine.AddApplication('DefaultAPI', '/default', ['*.THelloWorldResource']);

    FServer := TMARShttpServerIndy.Create(FEngine);
    try
      // ----------------------------------
      // Logging
      FRequestCount := 0;
      FSetupTime := 0;
      FInvocationTime := 0;

      TMARSActivation.RegisterAfterInvoke(
        procedure(const AActivation: IMARSActivation)
        begin
          TInterlocked.Increment(FRequestCount);
          TInterlocked.Add(FSetupTime, AActivation.SetupTime.ElapsedMilliseconds);
          TInterlocked.Add(FInvocationTime, AActivation.InvocationTime.ElapsedMilliseconds);
        end
      );

      // ----------------------------------
      // Run REST server
      FServer.Active := True;
      Writeln('HelloWorld resource URL:');
      Writeln('  http://localhost:8080/rest/default/helloworld/');
      ExecuteUntilEnterKeyPressed(
        procedure
        begin
          PrintStats();
        end
      );
      Readln;
      // ----------------------------------
      // Close the server
      FServer.Active := False;
      PrintStats();
    finally
      FreeAndNil(FServer);
    end;
  finally
    FreeAndNil(FEngine);
  end;
  WriteLn('Press any key to quit');
  ReadLN;
end.
