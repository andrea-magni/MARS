unit Tests.Activation;

interface

uses
  Classes, SysUtils, Rtti, Types, TypInfo, Contnrs
, DUnitX.TestFramework, Delphi.Mocks
, MARS.Core.Activation, MARS.Core.Activation.Interfaces
;

type
  [TestFixture('Activation')]
  TMARSActivationFixture = class
  private
    FTempObjs: TObjectList;
    procedure AddToTempObjs(const AObj: TObject);
    procedure FreeAll;
  protected
    function MockActivation: TMock<IMARSActivation>;

  public
    [Setup] procedure Setup;
    [Teardown] procedure Teardown;
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

procedure TMARSActivationFixture.AddToTempObjs(const AObj: TObject);
begin
   FTempObjs.Add(AObj);
end;

procedure TMARSActivationFixture.FreeAll;
begin
  FreeAndNil(FTempObjs);
end;

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
  AddToTempObjs(LURLPrototype);
  var LURL := TMARSURL.Create('http://localhost:8080/rest/default/helloworld/myfile.txt?query1=value1&query2=value2');
  AddToTempObjs(LURL);

  var LRequestInfo: TIdHttpRequestInfo := TIdHttpRequestInfo.Create(nil);
  AddToTempObjs(LRequestInfo);
  LRequestInfo.QueryParams := 'query1=value1&query2=value2';
  var LResponseInfo: TIdHttpResponseInfo := nil;

  var LWebRequest := TMARSIdHTTPAppRequest.Create(nil, LRequestInfo, LResponseInfo);
  AddToTempObjs(LWebRequest);
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

procedure TMARSActivationFixture.Setup;
begin
  FTempObjs := TObjectList.Create(True);
end;

procedure TMARSActivationFixture.Teardown;
begin
  FreeAll;
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSActivationFixture);

end.
