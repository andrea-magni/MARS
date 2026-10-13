unit Tests.Engine.COM;

// The COM.Initialize parameter of the engine: COM initialized for each request, in its thread (#117).

interface

uses
  Classes, SysUtils
, DUnitX.TestFramework
;

type
  [TestFixture('Engine COM')]
  TMARSEngineCOMTest = class
  public
    [SetupFixture]
    procedure SetupFixture;

    [Test]
    procedure InitializedOnDemand;
  end;

implementation

uses
  MARS.Core.Engine.Interfaces
, Tests.Client.TestServer, Tests.RequestTarget
;

{ TMARSEngineCOMTest }

procedure TMARSEngineCOMTest.SetupFixture;
begin
  SharedTestServer;
end;

procedure TMARSEngineCOMTest.InitializedOnDemand;
begin
  {$IFNDEF MSWINDOWS}
  Assert.Pass('COM: Windows only');
  {$ENDIF}
  // by default, not initialized by the engine (nor left initialized after a request)
  for var LIndex := 1 to 3 do
    Assert.Contains(RawRequest(8080, 'GET /rest/default/test/com HTTP/1.1'), 'not initialized');

  SharedTestServer.Engine.Parameters.Values['COM.Initialize'] := True;
  try
    for var LIndex := 1 to 3 do
      Assert.Contains(RawRequest(8080, 'GET /rest/default/test/com HTTP/1.1'), #13#10#13#10'initialized');
  finally
    SharedTestServer.Engine.Parameters.Values['COM.Initialize'] := False;
  end;

  // uninitialized after each request: not initialized again
  for var LIndex := 1 to 3 do
    Assert.Contains(RawRequest(8080, 'GET /rest/default/test/com HTTP/1.1'), 'not initialized');
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSEngineCOMTest);

end.
