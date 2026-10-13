(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Utils.RateLimiter;

// A rate limiter (token bucket) per key, i.e. per client: ALimit requests per AWindowSecs on
// average, bursts up to ALimit. The engine uses one for the RateLimit.* parameters; create your
// own for a middleware with other limits (per route, per user...). Thread safe.

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.SyncObjs;

type
  TMARSRateLimitResult = record
    Allowed: Boolean;
    Limit: Integer;
    // the requests still allowed right now
    Remaining: Integer;
    // seconds until the next request is allowed (0 when Allowed)
    RetryAfterSecs: Integer;
    // seconds until the full limit is available again
    ResetSecs: Integer;
  end;

  TMARSRateLimiter = class
  private
    type
      TBucket = record
        Tokens: Double;
        Updated: Int64;
      end;
  private
    FBuckets: TDictionary<string, TBucket>;
    FLock: TCriticalSection;
    FClock: TFunc<Int64>;
    FLastPurge: Int64;
    function GetCount: Integer;
  protected
    // the buckets back to full: clients idle for a whole window
    procedure Purge(const ANow: Int64; const ALimit: Integer; const ARate: Double); virtual;
  public
    // AClock: milliseconds (default: TThread.GetTickCount64), for tests
    constructor Create(const AClock: TFunc<Int64> = nil);
    destructor Destroy; override;

    // a request of AKey: allowed or not, and the numbers for the RateLimit-* headers
    function Check(const AKey: string; const ALimit, AWindowSecs: Integer): TMARSRateLimitResult;
    procedure Clear;
    // the keys tracked
    property Count: Integer read GetCount;
  end;

implementation

uses
  System.Math;

{ TMARSRateLimiter }

constructor TMARSRateLimiter.Create(const AClock: TFunc<Int64>);
begin
  inherited Create;
  FBuckets := TDictionary<string, TBucket>.Create;
  FLock := TCriticalSection.Create;
  FClock := AClock;
  if not Assigned(FClock) then
    FClock :=
      function: Int64
      begin
        Result := TThread.GetTickCount64;
      end;
  FLastPurge := FClock();
end;

destructor TMARSRateLimiter.Destroy;
begin
  FLock.Free;
  FBuckets.Free;
  inherited;
end;

function TMARSRateLimiter.Check(const AKey: string; const ALimit,
  AWindowSecs: Integer): TMARSRateLimitResult;
var
  LNow: Int64;
  LRate: Double; // tokens per millisecond
  LBucket: TBucket;
begin
  Result := Default(TMARSRateLimitResult);
  Result.Limit := ALimit;
  if (ALimit <= 0) or (AWindowSecs <= 0) then
  begin
    Result.Allowed := True;
    Result.Remaining := Max(ALimit, 0);
    Exit;
  end;

  LRate := ALimit / (AWindowSecs * 1000.0);
  FLock.Enter;
  try
    LNow := FClock();
    if not FBuckets.TryGetValue(AKey, LBucket) then
    begin
      LBucket.Tokens := ALimit;
      LBucket.Updated := LNow;
    end
    else
    begin
      LBucket.Tokens := Min(ALimit, LBucket.Tokens + (LNow - LBucket.Updated) * LRate);
      LBucket.Updated := LNow;
    end;

    Result.Allowed := LBucket.Tokens >= 1;
    if Result.Allowed then
      LBucket.Tokens := LBucket.Tokens - 1
    else
      Result.RetryAfterSecs := Max(1, Ceil((1 - LBucket.Tokens) / LRate / 1000));
    Result.Remaining := Trunc(LBucket.Tokens);
    Result.ResetSecs := Ceil((ALimit - LBucket.Tokens) / LRate / 1000);
    FBuckets.AddOrSetValue(AKey, LBucket);

    // the idle clients, once per window
    if LNow - FLastPurge >= AWindowSecs * 1000 then
      Purge(LNow, ALimit, LRate);
  finally
    FLock.Leave;
  end;
end;

procedure TMARSRateLimiter.Purge(const ANow: Int64; const ALimit: Integer; const ARate: Double);
var
  LIdle: TArray<string>;
begin
  // called with the lock held
  LIdle := [];
  for var LPair in FBuckets do
    if LPair.Value.Tokens + (ANow - LPair.Value.Updated) * ARate >= ALimit then
      LIdle := LIdle + [LPair.Key];
  for var LKey in LIdle do
    FBuckets.Remove(LKey);
  FLastPurge := ANow;
end;

procedure TMARSRateLimiter.Clear;
begin
  FLock.Enter;
  try
    FBuckets.Clear;
  finally
    FLock.Leave;
  end;
end;

function TMARSRateLimiter.GetCount: Integer;
begin
  FLock.Enter;
  try
    Result := FBuckets.Count;
  finally
    FLock.Leave;
  end;
end;

end.
