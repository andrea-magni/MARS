unit Tests.Activation;

interface

uses
  Classes, SysUtils, Rtti, Types, TypInfo
, DUnitX.TestFramework, Delphi.Mocks
, MARS.Core.Activation, MARS.Core.Activation.Interfaces
;

type
  [TestFixture('Activation')]
  TMARSActivationFixture = class
  private
  protected
    function MockActivation: TMock<IMARSActivation>;

  public
    [ Test ]
    procedure GetValueByName;
  end;

implementation


uses
  IOUtils
, MARS.Core.Utils, MARS.Core.URL
, MARS.http.Server.Indy, Web.HTTPApp, IdCustomHTTPServer
, MARS.Core.RequestAndResponse.Interfaces
, MARS.Core.JSON
;

{ TMARSActivationFixture }

procedure TMARSActivationFixture.GetValueByName;
begin
  var LActivation := MockActivation;

  var LFileName := TMARSActivation.GetValueByName('PathParam_filename', LActivation).ToString();
  Assert.AreEqual('myfile.txt', LFileName);

  var LQuery1 := TMARSActivation.GetValueByName('QueryParam_query1', LActivation).ToString();
  Assert.AreEqual('value1', LQuery1);

  var LValue := TMARSActivation.GetValueByName('Request_QueryString', LActivation);
  var LQueryString := LValue.ToString();
  Assert.AreEqual('query1=value1&query2=value2', LQueryString);
end;

function TMARSActivationFixture.MockActivation: TMock<IMARSActivation>;
begin
  var LURLPrototype := TMARSURL.Create('http://localhost:8080/rest/default/helloworld/{filename}');
  var LURL := TMARSURL.Create('http://localhost:8080/rest/default/helloworld/myfile.txt?query1=value1&query2=value2');

  var LRequestInfo: TIdHttpRequestInfo := TIdHttpRequestInfo.Create(nil);
  LRequestInfo.QueryParams := 'query1=value1&query2=value2';
  var LResponseInfo: TIdHttpResponseInfo := nil;

  var LWebRequest := TMARSIdHTTPAppRequest.Create(nil, LRequestInfo, LResponseInfo);
  var LRequest := TMARSWebRequest.Create(LWebRequest) as IMARSRequest;

  Result := TMock<IMARSActivation>.Create;
  with Result.Setup do
  begin
    AllowRedefineBehaviorDefinitions := True;
    WillReturn(LURLPrototype).When.GetUrLPrototype;
    WillReturn(LURL).When.GetURL;
    WillReturn(TValue.From<IMARSRequest>(LRequest)).When.GetRequest;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSActivationFixture);

end.
