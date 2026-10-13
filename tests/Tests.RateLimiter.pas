unit Tests.RateLimiter;

// The rate limiter (token bucket) and the RateLimit.* parameters of the engine (#119)

interface

uses
  Classes, SysUtils
, DUnitX.TestFramework
, MARS.Utils.RateLimiter
;

type
  [TestFixture('RateLimiter')]
  TMARSRateLimiterTest = class
  private
    FNow: Int64;
    FLimiter: TMARSRateLimiter;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure BurstThenLimited;
    [Test]
    procedure Refill;
    [Test]
    procedure KeysAreIndependent;
    [Test]
    procedure IdleClientsPurged;
    [Test]
    procedure Disabled;
    [Test]
    procedure EngineParameters;
  end;

implementation

uses
  MARS.Core.Engine, MARS.Core.Engine.Interfaces
, Tests.Client.TestServer, Tests.RequestTarget
;

{ TMARSRateLimiterTest }

procedure TMARSRateLimiterTest.Setup;
begin
  FNow := 1000000;
  FLimiter := TMARSRateLimiter.Create(
    function: Int64
    begin
      Result := FNow;
    end);
end;

procedure TMARSRateLimiterTest.Teardown;
begin
  FreeAndNil(FLimiter);
end;

procedure TMARSRateLimiterTest.BurstThenLimited;
var
  LResult: TMARSRateLimitResult;
begin
  // 3 requests per 60 seconds: 3 at once, then a 429 for 20 seconds (one every 20)
  for var LIndex := 1 to 3 do
  begin
    LResult := FLimiter.Check('a', 3, 60);
    Assert.IsTrue(LResult.Allowed, 'request ' + LIndex.ToString);
    Assert.AreEqual(3 - LIndex, LResult.Remaining);
  end;
  LResult := FLimiter.Check('a', 3, 60);
  Assert.IsFalse(LResult.Allowed);
  Assert.AreEqual(0, LResult.Remaining);
  Assert.AreEqual(20, LResult.RetryAfterSecs);
  Assert.AreEqual(60, LResult.ResetSecs);
  Assert.AreEqual(3, LResult.Limit);
end;

procedure TMARSRateLimiterTest.Refill;
begin
  for var LIndex := 1 to 3 do
    FLimiter.Check('a', 3, 60);
  Assert.IsFalse(FLimiter.Check('a', 3, 60).Allowed);

  FNow := FNow + 19000;
  Assert.IsFalse(FLimiter.Check('a', 3, 60).Allowed, 'not yet');
  FNow := FNow + 1000;
  Assert.IsTrue(FLimiter.Check('a', 3, 60).Allowed, 'one more after 20 seconds');
  Assert.IsFalse(FLimiter.Check('a', 3, 60).Allowed);

  // never more than the limit, however long the pause
  FNow := FNow + 3600000;
  for var LIndex := 1 to 3 do
    Assert.IsTrue(FLimiter.Check('a', 3, 60).Allowed);
  Assert.IsFalse(FLimiter.Check('a', 3, 60).Allowed);
end;

procedure TMARSRateLimiterTest.KeysAreIndependent;
begin
  for var LIndex := 1 to 2 do
    FLimiter.Check('a', 2, 60);
  Assert.IsFalse(FLimiter.Check('a', 2, 60).Allowed);
  Assert.IsTrue(FLimiter.Check('b', 2, 60).Allowed);
  Assert.AreEqual(2, FLimiter.Count);
end;

procedure TMARSRateLimiterTest.IdleClientsPurged;
begin
  FLimiter.Check('a', 2, 60);
  FLimiter.Check('b', 2, 60);
  FNow := FNow + 50000;
  FLimiter.Check('b', 2, 60);
  Assert.AreEqual(2, FLimiter.Count);
  // a window later: a is full again (idle), b is not
  FNow := FNow + 11000;
  FLimiter.Check('c', 2, 60);
  Assert.AreEqual(2, FLimiter.Count, 'a purged');
end;

procedure TMARSRateLimiterTest.Disabled;
begin
  for var LIndex := 1 to 10 do
    Assert.IsTrue(FLimiter.Check('a', 0, 60).Allowed);
  Assert.AreEqual(0, FLimiter.Count);
end;

procedure TMARSRateLimiterTest.EngineParameters;
var
  LEngine: IMARSEngine;
  LResponse, LClient: string;
begin
  LEngine := SharedTestServer.Engine;
  // a client of its own (the header as the key), not to affect the other tests
  LClient := 'X-Test-Client: ratelimit-' + IntToStr(Random(MaxInt));
  LEngine.Parameters.Values['RateLimit.Requests'] := 2;
  LEngine.Parameters.Values['RateLimit.Window'] := 60;
  LEngine.Parameters.Values['RateLimit.ClientIPHeader'] := 'X-Test-Client';
  try
    for var LIndex := 1 to 2 do
    begin
      LResponse := RawRequest(8080, 'GET /rest/default/test/helloworld HTTP/1.1', [LClient]);
      Assert.Contains(LResponse, 'HTTP/1.1 200', LResponse);
      Assert.Contains(LResponse, 'RateLimit-Limit: 2');
      Assert.Contains(LResponse, 'RateLimit-Remaining: ' + (2 - LIndex).ToString);
    end;
    LResponse := RawRequest(8080, 'GET /rest/default/test/helloworld HTTP/1.1', [LClient]);
    Assert.Contains(LResponse, 'HTTP/1.1 429', LResponse);
    Assert.Contains(LResponse, 'Retry-After: 30');
    Assert.Contains(LResponse, 'Too Many Requests');
    // also for a path that does not exist: the limit comes first
    Assert.Contains(RawRequest(8080, 'GET /rest/nothing HTTP/1.1', [LClient]), 'HTTP/1.1 429');
    // a request without the header: the remote IP (it was an Access Violation)
    Assert.Contains(RawRequest(8080, 'GET /rest/default/test/helloworld HTTP/1.1'), 'HTTP/1.1 200');
    // another client
    Assert.Contains(RawRequest(8080, 'GET /rest/default/test/helloworld HTTP/1.1',
      ['X-Test-Client: another-' + IntToStr(Random(MaxInt))]), 'HTTP/1.1 200');
  finally
    LEngine.Parameters.Values['RateLimit.Requests'] := 0;
    LEngine.Parameters.Values['RateLimit.ClientIPHeader'] := '';
  end;
  // disabled: no headers
  LResponse := RawRequest(8080, 'GET /rest/default/test/helloworld HTTP/1.1', [LClient]);
  Assert.Contains(LResponse, 'HTTP/1.1 200');
  Assert.DoesNotContain(LResponse, 'RateLimit-Limit');
end;

initialization
  Randomize;
  TDUnitX.RegisterTestFixture(TMARSRateLimiterTest);

end.
