(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Tests.Default;

interface

uses
  Classes, SysUtils, System.Rtti, System.JSON
// Test Frameworks
, DUnitX.TestFramework
// MARS Core
, MARS.Core.Engine.Interfaces, MARS.Core.RequestAndResponse.Interfaces
// MARS Test Framework
, MARS.Tests, MARS.Tests.Types
// Application specific
, Server.Ignition
;

const
  // the media type of the data access library (TMARSFDResource on the client)
  NATIVE_MEDIA_TYPE = 'application/json-firedac';

type
  /// <summary> The customers resource, executed in process (no HTTP), on the database of
  /// MAIN_DB (see bin\Server.ini): the tests add the records they need and delete them </summary>
  [TestFixture('Customers')]
  TCustomersTest = class(TMARSTestFixture)
  protected
    function GetEngine: IMARSEngine; override;
    function Request(const AHttpMethod, APath: string; const AQueryString: string = '';
      const ABody: string = ''; const AAccept: string = 'application/json'): IMARSResponse;
    function AddCustomer(const AName, ACity: string; const ACredit: Currency): Integer;
    function GetCredit(const AId: Integer): Currency;
  public
    [Test] procedure List;
    [Test] procedure ListByCity;
    [Test] procedure ListAsXML;
    [Test] procedure Summary;
    [Test] procedure CustomersData;
    [Test] procedure CreateReadUpdateDelete;
    [Test] procedure Transfer;
    [Test] procedure NotFound;
  end;

implementation

// the field names depend on the database (i.e. upper case with Firebird)
function ValueOf(const AObject: TJSONValue; const AName: string): TJSONValue;
var
  LPair: TJSONPair;
begin
  Result := nil;
  for LPair in AObject as TJSONObject do
    if SameText(LPair.JsonString.Value, AName) then
      Exit(LPair.JsonValue);
end;

function ParseArray(const AContent: string): TJSONArray;
begin
  Result := TJSONObject.ParseJSONValue(AContent) as TJSONArray;
  Assert.IsNotNull(Result, 'JSON array expected: ' + AContent);
end;

{ TCustomersTest }

function TCustomersTest.GetEngine: IMARSEngine;
begin
  Result := TServerEngine.Default;
end;

function TCustomersTest.Request(const AHttpMethod, APath, AQueryString, ABody,
  AAccept: string): IMARSResponse;
begin
  Result := ExecuteMockRequest(TRequestData.Create(TRequestData.DEFAULT_HOSTNAME, Engine.Port
    , '{basePath}/' + APath, AHttpMethod, AQueryString, ABody, AAccept));
end;

function TCustomersTest.AddCustomer(const AName, ACity: string; const ACredit: Currency): Integer;
var
  LResponse: IMARSResponse;
  LRecords: TJSONArray;
begin
  LResponse := Request('POST', 'customers', ''
    , Format('{"name": "%s", "city": "%s", "credit": %d}', [AName, ACity, Trunc(ACredit)]));
  Assert.AreEqual(200, LResponse.StatusCode, 'POST customers: ' + LResponse.Content);
  LRecords := ParseArray(LResponse.Content);
  try
    Assert.AreEqual(1, LRecords.Count, 'The new record');
    Result := ValueOf(LRecords.Items[0], 'id').AsType<Integer>;
  finally
    LRecords.Free;
  end;
end;

function TCustomersTest.GetCredit(const AId: Integer): Currency;
var
  LResponse: IMARSResponse;
  LRecords: TJSONArray;
begin
  LResponse := Request('GET', 'customers/' + AId.ToString);
  Assert.AreEqual(200, LResponse.StatusCode, LResponse.Content);
  LRecords := ParseArray(LResponse.Content);
  try
    Result := ValueOf(LRecords.Items[0], 'credit').AsType<Currency>;
  finally
    LRecords.Free;
  end;
end;

procedure TCustomersTest.List;
var
  LResponse: IMARSResponse;
  LRecords: TJSONArray;
begin
  LResponse := Request('GET', 'customers');
  Assert.AreEqual(200, LResponse.StatusCode, LResponse.Content);
  LRecords := ParseArray(LResponse.Content);
  try
    Assert.IsTrue(LRecords.Count > 0, 'The demo database has a few customers');
  finally
    LRecords.Free;
  end;
end;

procedure TCustomersTest.ListByCity;
var
  LResponse: IMARSResponse;
  LRecords: TJSONArray;
  LRecord: TJSONValue;
begin
  LResponse := Request('GET', 'customers', 'city=London');
  Assert.AreEqual(200, LResponse.StatusCode, LResponse.Content);
  LRecords := ParseArray(LResponse.Content);
  try
    Assert.IsTrue(LRecords.Count > 0, 'Customers in London');
    for LRecord in LRecords do
      Assert.AreEqual('London', ValueOf(LRecord, 'city').Value);
  finally
    LRecords.Free;
  end;
end;

procedure TCustomersTest.ListAsXML;
var
  LResponse: IMARSResponse;
begin
  LResponse := Request('GET', 'customers', '', '', 'application/xml');
  Assert.AreEqual(200, LResponse.StatusCode, LResponse.Content);
  Assert.StartsWith('<', LResponse.Content.Trim, 'XML expected');
end;

procedure TCustomersTest.Summary;
var
  LResponse: IMARSResponse;
  LJSON: TJSONObject;
begin
  // the native format: one member per dataset (Base64 of the zipped XML of the dataset)
  LResponse := Request('GET', 'customers/summary', '', '', NATIVE_MEDIA_TYPE);
  Assert.AreEqual(200, LResponse.StatusCode, LResponse.Content);
  LJSON := TJSONObject.ParseJSONValue(LResponse.Content) as TJSONObject;
  try
    Assert.IsNotNull(LJSON, LResponse.Content);
    Assert.IsNotNull(LJSON.Values['Customers'], 'Customers dataset');
    Assert.IsNotNull(LJSON.Values['Cities'], 'Cities dataset');
  finally
    LJSON.Free;
  end;
end;

procedure TCustomersTest.CustomersData;
var
  LResponse: IMARSResponse;
  LJSON: TJSONObject;
begin
  // TMARSFDDatasetResource: the datasets of the SQLStatement attributes...
  LResponse := Request('GET', 'customersdata', '', '', NATIVE_MEDIA_TYPE);
  Assert.AreEqual(200, LResponse.StatusCode, LResponse.Content);
  LJSON := TJSONObject.ParseJSONValue(LResponse.Content) as TJSONObject;
  try
    Assert.IsNotNull(LJSON, LResponse.Content);
    Assert.IsNotNull(LJSON.Values['Customers'], 'Customers dataset');
    Assert.IsNotNull(LJSON.Values['Cities'], 'Cities dataset');
  finally
    LJSON.Free;
  end;

  // ...and POST of the deltas (none here): one result for each delta
  LResponse := Request('POST', 'customersdata', '', '{}', 'application/json');
  Assert.AreEqual(200, LResponse.StatusCode, LResponse.Content);
  Assert.AreEqual('[]', LResponse.Content.Trim);
end;

procedure TCustomersTest.CreateReadUpdateDelete;
var
  LId: Integer;
  LResponse: IMARSResponse;
begin
  LId := AddCustomer('Test Customer', 'Test City', 100);

  LResponse := Request('PUT', 'customers/' + LId.ToString, ''
    , '{"name": "Test Customer", "city": "Test City", "credit": 150}');
  Assert.AreEqual(200, LResponse.StatusCode, 'PUT: ' + LResponse.Content);
  Assert.AreEqual<Currency>(150, GetCredit(LId));

  LResponse := Request('DELETE', 'customers/' + LId.ToString);
  Assert.IsTrue(IsSuccessful(LResponse.StatusCode), 'DELETE: ' + LResponse.Content);

  LResponse := Request('GET', 'customers/' + LId.ToString);
  Assert.AreEqual(404, LResponse.StatusCode, 'Deleted');
end;

procedure TCustomersTest.Transfer;
var
  LFrom, LTo: Integer;
  LResponse: IMARSResponse;
begin
  LFrom := AddCustomer('Test From', 'Test City', 100);
  LTo := AddCustomer('Test To', 'Test City', 0);
  try
    LResponse := Request('POST', 'customers/transfer', Format('from=%d&to=%d&amount=30', [LFrom, LTo]));
    Assert.AreEqual(200, LResponse.StatusCode, LResponse.Content);
    Assert.AreEqual<Currency>(70, GetCredit(LFrom));
    Assert.AreEqual<Currency>(30, GetCredit(LTo));

    // insufficient credit: 409 and nothing changes (rollback)
    LResponse := Request('POST', 'customers/transfer', Format('from=%d&to=%d&amount=1000', [LFrom, LTo]));
    Assert.AreEqual(409, LResponse.StatusCode, LResponse.Content);
    Assert.AreEqual<Currency>(70, GetCredit(LFrom));

    // the second update fails: the first one is rolled back
    LResponse := Request('POST', 'customers/transfer', Format('from=%d&to=0&amount=10', [LFrom]));
    Assert.AreEqual(404, LResponse.StatusCode, LResponse.Content);
    Assert.AreEqual<Currency>(70, GetCredit(LFrom));
  finally
    Request('DELETE', 'customers/' + LFrom.ToString);
    Request('DELETE', 'customers/' + LTo.ToString);
  end;
end;

procedure TCustomersTest.NotFound;
begin
  Assert.AreEqual(404, Request('GET', 'customers/999999').StatusCode);
end;

initialization
  TDUnitX.RegisterTestFixture(TCustomersTest);

end.
