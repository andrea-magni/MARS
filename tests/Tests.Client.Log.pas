unit Tests.Client.Log;

interface

uses
  Classes, SysUtils, StrUtils
, DUnitX.TestFramework

, MARS.Core.Utils, MARS.Core.MediaType
, MARS.Client.Client, MARS.Client.Client.Indy, MARS.Client.Client.Net, MARS.Client.Client.Http
, MARS.Client.Log
, MARS.Client.Resource

, Tests.Client
;

const
  LOGIN_BODY = '{"username":"andrea","password":"p4ssw0rd"}';
  TOKEN = 'my-client-token';

type
  // same tests against every client implementation
  TClientLogTestBase<C: TMARSCustomClient> = class(TMARSResourceClientTest<C, TMARSClientResource>)
  private
    FEntries: TArray<TMARSClientLogEntry>;
    procedure HandleLog(Sender: TObject; const AEntry: TMARSClientLogEntry);
    function LastEntry: TMARSClientLogEntry;
    procedure GetText(const AResource: string);
    procedure PostLogin;
  public
    [Setup]
    procedure Setup;

    [Test] procedure LogsSuccessfulGet;
    [Test] procedure LogsHttpError;
    [Test] procedure LogsConnectionError;
    [Test] procedure MasksHeadersAndFieldsByDefault;
    [Test] procedure MaskingNone;
    [Test] procedure MaskingHeadersOnly;
    [Test] procedure MaskingAll;
    [Test] procedure ContentHeadersOnly;
    [Test] procedure ContentTruncated;
    [Test] procedure ContentFull;
    [Test] procedure RegisteredLoggerWithoutComponent;
    [Test] procedure CloneSetupCopiesLogSettings;
  end;

  [TestFixture('Client Log')]
  TIndyClientLogTest = class(TClientLogTestBase<TMARSIndyClient>)
  end;

  [TestFixture('Client Log Net')]
  TNetClientLogTest = class(TClientLogTestBase<TMARSNetClient>)
  end;

  [TestFixture('Client Log Http')]
  THttpClientLogTest = class(TClientLogTestBase<TMARSHttpClient>)
  end;

  // masking and formatting helpers, no server involved
  [TestFixture('Client Log helpers')]
  TClientLogHelpersTest = class
  public
    [Test] procedure MaskJSONFields;
    [Test] procedure MaskUrlEncodedFields;
    [Test] procedure TextContentTypes;
    [Test] procedure RequestBodyFromParameters;
  end;

implementation

uses
  MARS.Utils.Parameters
;

{ TClientLogTestBase<C> }

procedure TClientLogTestBase<C>.HandleLog(Sender: TObject; const AEntry: TMARSClientLogEntry);
begin
  FEntries := FEntries + [AEntry];
end;

function TClientLogTestBase<C>.LastEntry: TMARSClientLogEntry;
begin
  Assert.IsTrue(Length(FEntries) > 0, 'nothing logged');
  Result := FEntries[High(FEntries)];
end;

procedure TClientLogTestBase<C>.Setup;
var
  LDefaults: TMARSClientLogOptions;
begin
  FEntries := [];
  LDefaults := TMARSClientLogOptions.Create;
  try
    Client.LogOptions := LDefaults;
  finally
    LDefaults.Free;
  end;
  Client.OnLog := HandleLog;
  Client.AuthEndorsement := TMARSAuthEndorsement.AuthorizationBearer;
  FRequest.SpecificURL := '';
  FRequest.SpecificToken := '';
  FRequest.SpecificAccept := '';
  FRequest.SpecificContentType := '';
end;

procedure TClientLogTestBase<C>.GetText(const AResource: string);
begin
  FRequest.Resource := AResource;
  FRequest.SpecificAccept := TMediaType.TEXT_PLAIN;
  FRequest.GET(nil, nil, nil);
end;

procedure TClientLogTestBase<C>.PostLogin;
begin
  FRequest.Resource := 'test/login';
  FRequest.SpecificToken := TOKEN;
  FRequest.SpecificAccept := TMediaType.APPLICATION_JSON;
  FRequest.SpecificContentType := TMediaType.APPLICATION_JSON;
  FRequest.POST(
    procedure (AContent: TMemoryStream)
    var
      LBytes: TBytes;
    begin
      LBytes := TEncoding.UTF8.GetBytes(LOGIN_BODY);
      AContent.WriteBuffer(LBytes, Length(LBytes));
    end
  , nil, nil);
end;

procedure TClientLogTestBase<C>.LogsSuccessfulGet;
var
  LEntry: TMARSClientLogEntry;
begin
  GetText('test/helloworld');

  Assert.AreEqual(1, Length(FEntries), 'one call, one entry');
  LEntry := LastEntry;
  Assert.AreSame(TObject(Client), LEntry.Client);
  Assert.AreEqual('GET', LEntry.Verb);
  Assert.IsTrue(LEntry.URL.EndsWith('/default/test/helloworld'), LEntry.URL);
  Assert.AreEqual(200, LEntry.StatusCode);
  Assert.IsTrue(LEntry.Succeeded);
  Assert.AreEqual('', LEntry.ExceptionClass);
  Assert.AreEqual(TMediaType.TEXT_PLAIN, LEntry.HeaderValue(LEntry.RequestHeaders, 'Accept'));
  Assert.AreEqual('Hello World!', LEntry.ResponseBody);
  Assert.AreEqual(Int64(12), LEntry.ResponseSize);
  Assert.IsTrue(StartsText(TMediaType.TEXT_PLAIN, LEntry.ResponseContentType), LEntry.ResponseContentType);
  Assert.IsTrue(Length(LEntry.ResponseHeaders) > 0, 'response headers');
  Assert.IsTrue(LEntry.DurationMs >= 0);
  Assert.IsTrue(LEntry.ToString.StartsWith('GET '), LEntry.ToString);
end;

procedure TClientLogTestBase<C>.LogsHttpError;
var
  LEntry: TMARSClientLogEntry;
  LCall: TTestLocalMethod;
begin
  LCall :=
    procedure
    begin
      GetText('test/notfound');
    end;
  Assert.WillRaiseAny(LCall);

  LEntry := LastEntry;
  Assert.AreEqual(404, LEntry.StatusCode);
  Assert.IsFalse(LEntry.Succeeded);
  Assert.AreNotEqual('', LEntry.ExceptionClass);
  Assert.Contains(LEntry.ResponseBody, 'nothing here');
end;

procedure TClientLogTestBase<C>.LogsConnectionError;
var
  LEntry: TMARSClientLogEntry;
  LCall: TTestLocalMethod;
begin
  FRequest.SpecificURL := 'http://127.0.0.1:1/nobody/listens/here';
  LCall :=
    procedure
    begin
      FRequest.GET(nil, nil, nil);
    end;
  Assert.WillRaiseAny(LCall);

  LEntry := LastEntry;
  Assert.AreEqual('http://127.0.0.1:1/nobody/listens/here', LEntry.URL);
  Assert.AreEqual(0, LEntry.StatusCode, 'no response');
  Assert.AreNotEqual('', LEntry.ExceptionClass);
  Assert.Contains(LEntry.ToString, 'no response');
end;

procedure TClientLogTestBase<C>.MasksHeadersAndFieldsByDefault;
var
  LEntry: TMARSClientLogEntry;
begin
  PostLogin;

  LEntry := LastEntry;
  Assert.AreEqual(200, LEntry.StatusCode, LEntry.ResponseBody);
  Assert.AreEqual(TMARSClientLog.MASK, LEntry.HeaderValue(LEntry.RequestHeaders, 'Authorization'));
  Assert.AreEqual(TMediaType.APPLICATION_JSON, LEntry.HeaderValue(LEntry.RequestHeaders, 'Content-Type'));
  Assert.AreEqual('{"username":"andrea","password":"***"}', LEntry.RequestBody);
  Assert.AreEqual(Int64(Length(LOGIN_BODY)), LEntry.RequestSize);
  Assert.Contains(LEntry.ResponseBody, '"name":"andrea"');
  Assert.Contains(LEntry.ResponseBody, '"Token":"***"');
  Assert.DoesNotContain(LEntry.ResponseBody, 'server-issued-token');
  Assert.DoesNotContain(LEntry.ToText, TOKEN);
end;

procedure TClientLogTestBase<C>.MaskingNone;
var
  LEntry: TMARSClientLogEntry;
begin
  Client.LogOptions.Masking := TMARSClientLogMasking.None;
  PostLogin;

  LEntry := LastEntry;
  Assert.AreEqual('Bearer ' + TOKEN, LEntry.HeaderValue(LEntry.RequestHeaders, 'Authorization'));
  Assert.AreEqual(LOGIN_BODY, LEntry.RequestBody);
  Assert.Contains(LEntry.ResponseBody, 'server-issued-token');
end;

procedure TClientLogTestBase<C>.MaskingHeadersOnly;
var
  LEntry: TMARSClientLogEntry;
begin
  Client.LogOptions.Masking := TMARSClientLogMasking.HeadersOnly;
  PostLogin;

  LEntry := LastEntry;
  Assert.AreEqual(TMARSClientLog.MASK, LEntry.HeaderValue(LEntry.RequestHeaders, 'Authorization'));
  Assert.AreEqual(LOGIN_BODY, LEntry.RequestBody);
end;

procedure TClientLogTestBase<C>.MaskingAll;
var
  LEntry: TMARSClientLogEntry;
begin
  Client.LogOptions.Masking := TMARSClientLogMasking.All;
  PostLogin;

  LEntry := LastEntry;
  Assert.AreEqual(TMARSClientLog.MASK, LEntry.HeaderValue(LEntry.RequestHeaders, 'Authorization'));
  Assert.AreEqual(TMediaType.APPLICATION_JSON, LEntry.HeaderValue(LEntry.RequestHeaders, 'Accept'));
  Assert.AreEqual(TMARSClientLog.MASK, LEntry.RequestBody);
  Assert.AreEqual(TMARSClientLog.MASK, LEntry.ResponseBody);
  Assert.AreEqual(Int64(Length(LOGIN_BODY)), LEntry.RequestSize);
  Assert.IsTrue(LEntry.ResponseSize > 0);
end;

procedure TClientLogTestBase<C>.ContentHeadersOnly;
var
  LEntry: TMARSClientLogEntry;
begin
  Client.LogOptions.Content := TMARSClientLogContent.HeadersOnly;
  PostLogin;

  LEntry := LastEntry;
  Assert.AreEqual('', LEntry.RequestBody);
  Assert.AreEqual('', LEntry.ResponseBody);
  Assert.AreEqual(Int64(Length(LOGIN_BODY)), LEntry.RequestSize);
  Assert.IsTrue(LEntry.ResponseSize > 0);
end;

procedure TClientLogTestBase<C>.ContentTruncated;
var
  LEntry: TMARSClientLogEntry;
begin
  GetText('test/big'); // 100000 bytes, default MaxBodySize 64 KB

  LEntry := LastEntry;
  Assert.AreEqual(Int64(100000), LEntry.ResponseSize);
  Assert.IsTrue(LEntry.ResponseBody.StartsWith(StringOfChar('x', TMARSClientLogOptions.DEFAULT_MAX_BODY_SIZE) + '...'));
  Assert.Contains(LEntry.ResponseBody, 'truncated');

  FEntries := [];
  Client.LogOptions.MaxBodySize := 10;
  GetText('test/big');
  Assert.IsTrue(LastEntry.ResponseBody.StartsWith('xxxxxxxxxx...'), LastEntry.ResponseBody);
end;

procedure TClientLogTestBase<C>.ContentFull;
begin
  Client.LogOptions.Content := TMARSClientLogContent.Full;
  GetText('test/big');

  Assert.AreEqual(StringOfChar('x', 100000), LastEntry.ResponseBody);
end;

procedure TClientLogTestBase<C>.RegisteredLoggerWithoutComponent;
var
  LCount: Integer;
  LIndex: Integer;
  LLastURL: string;
begin
  Client.OnLog := nil;
  LCount := 0;
  LIndex := TMARSCustomClient.RegisterLogger(
    procedure (const AEntry: TMARSClientLogEntry)
    begin
      Inc(LCount);
      LLastURL := AEntry.URL;
    end
  );
  try
    GetText('test/helloworld');
    Assert.AreEqual(1, LCount);
    Assert.IsTrue(LLastURL.EndsWith('test/helloworld'));

    // also clients created internally, i.e. by the class function shortcuts
    C.GetAsString(Client.MARSEngineURL + '/default/test/helloworld', '', TMediaType.TEXT_PLAIN);
    Assert.AreEqual(2, LCount);
  finally
    TMARSCustomClient.UnregisterLogger(LIndex);
  end;

  GetText('test/helloworld');
  Assert.AreEqual(2, LCount, 'unregistered');
  Assert.AreEqual(0, Length(FEntries), 'OnLog unassigned');
end;

procedure TClientLogTestBase<C>.CloneSetupCopiesLogSettings;
var
  LClone: TMARSCustomClient;
begin
  Client.LogOptions.Masking := TMARSClientLogMasking.None;
  Client.LogOptions.MaxBodySize := 123;
  Client.SynchronizeLog := True;
  try
    LClone := C.Create(nil);
    try
      LClone.CloneSetup(Client);
      Assert.IsTrue(Assigned(LClone.OnLog));
      Assert.IsTrue(LClone.SynchronizeLog);
      Assert.AreEqual(123, LClone.LogOptions.MaxBodySize);
      Assert.IsTrue(LClone.LogOptions.Masking = TMARSClientLogMasking.None);
    finally
      LClone.Free;
    end;
  finally
    Client.SynchronizeLog := False;
  end;
end;

{ TClientLogHelpersTest }

procedure TClientLogHelpersTest.MaskJSONFields;
var
  LOptions: TMARSClientLogOptions;
begin
  LOptions := TMARSClientLogOptions.Create;
  try
    // nested, case insensitive, escaped quotes, numbers and null
    Assert.AreEqual('{"user":{"name":"a","Password":"***"},"pin":1234,"secret":"***","token":"***"}',
      TMARSClientLog.MaskFields('{"user":{"name":"a","Password":"x\"y"},"pin":1234,"secret":42,"token":null}'
        , TMediaType.APPLICATION_JSON, LOptions));
    // truncated text
    Assert.AreEqual('[{"password":"***"},{"password":"***"',
      TMARSClientLog.MaskFields('[{"password":"a"},{"password":"bc', TMediaType.APPLICATION_JSON, LOptions));
    // other fields
    LOptions.MaskedFields := 'name';
    Assert.AreEqual('{"name":"***","password":"x"}',
      TMARSClientLog.MaskFields('{"name":"a","password":"x"}', TMediaType.APPLICATION_JSON, LOptions));
  finally
    LOptions.Free;
  end;
end;

procedure TClientLogHelpersTest.MaskUrlEncodedFields;
var
  LOptions: TMARSClientLogOptions;
begin
  LOptions := TMARSClientLogOptions.Create;
  try
    Assert.AreEqual('username=a&password=***&next=%2F',
      TMARSClientLog.MaskFields('username=a&password=secret&next=%2F', 'application/x-www-form-urlencoded', LOptions));

    LOptions.MaskedFields := 'username';
    Assert.AreEqual('username=***&password=secret',
      TMARSClientLog.MaskFields('username=a&password=secret', 'application/x-www-form-urlencoded', LOptions));
  finally
    LOptions.Free;
  end;
end;

procedure TClientLogHelpersTest.TextContentTypes;
begin
  Assert.IsTrue(TMARSClientLog.IsTextContentType('application/json; charset=utf-8'));
  Assert.IsTrue(TMARSClientLog.IsTextContentType('application/problem+json'));
  Assert.IsTrue(TMARSClientLog.IsTextContentType('text/html'));
  Assert.IsTrue(TMARSClientLog.IsTextContentType('application/x-www-form-urlencoded'));
  Assert.IsFalse(TMARSClientLog.IsTextContentType('application/octet-stream'));
  Assert.IsFalse(TMARSClientLog.IsTextContentType('image/png'));
  Assert.IsFalse(TMARSClientLog.IsTextContentType(''));
end;

procedure TClientLogHelpersTest.RequestBodyFromParameters;
var
  LOptions: TMARSClientLogOptions;
  LParams: TMARSParameters;
  LSize: Int64;
  LText: string;
begin
  LOptions := TMARSClientLogOptions.Create;
  try
    LParams := TMARSParameters.Create('');
    try
      LParams.Values['username'] := 'andrea';
      LParams.Values['password'] := 'secret';
      LText := TMARSClientLog.DescribeRequestBody(TMARSClientLogBody.FromParameters(LParams)
        , 'application/x-www-form-urlencoded', LOptions, LSize);
      Assert.Contains(LText, 'username=andrea');
      Assert.Contains(LText, 'password=***');
      Assert.DoesNotContain(LText, 'secret');
    finally
      LParams.Free;
    end;
  finally
    LOptions.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TIndyClientLogTest);
  TDUnitX.RegisterTestFixture(TNetClientLogTest);
  TDUnitX.RegisterTestFixture(THttpClientLogTest);
  TDUnitX.RegisterTestFixture(TClientLogHelpersTest);

end.
