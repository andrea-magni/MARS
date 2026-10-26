unit Tests.OpenAPI3;

{$I MARS.inc}

interface
uses
  Classes, SysUtils, Generics.Collections
, DUnitX.TestFramework
, MARS.Core.URL, MARS.Core.Utils

, MARS.OpenAPI.v3, MARS.OpenAPI.v3.Utils
, MARS.Core.JSON
;

type
  // #149: members left out of the JSON
  TOpenAPIHiddenMembers = class
  private
    FSecret: string;
  public
    Name: string;
    [JSONName('')] Hidden: string;
    [JSONName('alias')] Renamed: string;
    [JSONName('')] property Secret: string read FSecret write FSecret;
  end;

{$IFDEF MARS_YAML}
  TYAMLOptionsNested = record
    Note: string;
  end;

  // #304: the YAML of MARS and the JSON.* options
  TYAMLOptionsCase = record
    Name: string;
    Count: Integer;
    Active: Boolean;
    Tags: TArray<string>;
    Nested: TYAMLOptionsNested;
    Day: TDate;
    At: TDateTime;
    Zero: TDateTime;
  end;
{$ENDIF}

  [TestFixture('OpenAPI3')]
  TMARSOpenAPI3Test = class(TObject)
  private
  public
    [Test] procedure Basic;
    [Test] procedure QueryOperationNeedsOpenAPI32;
    [Test] procedure RequestBodyWithoutConsumes;
    [Test] procedure MetaRequestBody;
    [Test] procedure RequiredPermitAllAndProcedures;
    [Test] procedure JSONValuesAndStreams;
    // #149: a member left out of the JSON (JSONName('')) is not a property named ""
    [Test] procedure MembersLeftOutOfTheJSON;
    // #276: the JSON on the wire, not the Delphi classes (datasets, token, framework types)
    [Test] procedure DataSetsAndFrameworkTypes;
    // #277: no request body for the methods without one, whatever the Consumes of the resource
    [Test] procedure NoBodyWithoutBody;
    // request parameters of fields and properties of the resource, [MetaHeader],
    // OpenAPI.Header.<name> of the application
    [Test] procedure HeadersOfResourcesAndApplication;
{$IFDEF MARS_YAML}
    [Test] procedure SecurityAsYAML;
    [Test] procedure StringsStayStringsInYAML;
    // #304: dates and empty values as the JSON.* options say
    [Test] procedure DatesInYAML;
    [Test] procedure EmptyValuesInYAML;
{$ENDIF}
  end;

implementation

uses
  System.Rtti, System.TypInfo, System.StrUtils, MARS.Core.MediaType
, System.JSON
, Tests.Objects.Types, Tests.DefaultEngine.Definition
{$IFDEF MARS_YAML}, Neslib.Yaml, Neslib.Utf8, MARS.YAML.ReadersAndWriters{$ENDIF}
;

function QueryOperationIds(const AOpenAPI: TOpenAPI): TArray<string>;
begin
  Result := [];
  for var LPath in AOpenAPI.paths.Values do
    if not LPath.query.operationId.IsEmpty then
      Result := Result + [LPath.query.operationId];
end;

{ TMARSOpenAPI3Test }

procedure TMARSOpenAPI3Test.Basic;
begin
  var LType := TRttiContext.Create.GetType(TObjectWithNames);

  var LOpenAPI := TOpenAPI.Create;
  try
    var LSchema := LOpenAPI.components.AddSchema(LType.Name);

    LOpenAPI.FillSchemaForObjectOrRecord(LSchema, LType, LOpenAPI);

    var LJSONSchema := TJSONObject.ObjectToJSON(LSchema);
    try
      var LJSONProperties := LJSONSchema.Pairs[0].JsonValue as TJSONObject;

      Assert.AreEqual('KPrelieviT', LJSONProperties.Pairs[0].JsonString.Value);
      Assert.AreEqual('Numero', LJSONProperties.Pairs[3].JsonString.Value);
      Assert.AreEqual('IdCausale', LJSONProperties.Pairs[5].JsonString.Value);
    finally
      LJSONSchema.Free;
    end;
  finally
    LOpenAPI.Free;
  end;
end;

procedure TMARSOpenAPI3Test.QueryOperationNeedsOpenAPI32;
begin
  // TItemResource (Tests.DefaultEngine.Resources) exposes a [QUERY] Search method
  var LEngine := TDefaultEngine.Create;
  try
    var LApp := LEngine.Engine.ApplicationByName('DefaultApp');

    // default document version (3.0.x): no query operation exists, the endpoint is left out
    var LOpenAPI := TOpenAPI.BuildFrom(LEngine.Engine, LApp);
    try
      Assert.AreEqual('3.0.2', LOpenAPI.openapi);
      Assert.AreEqual(0, Length(QueryOperationIds(LOpenAPI)), 'QUERY endpoints must not appear in a 3.0 document');
    finally
      LOpenAPI.Free;
    end;

    // OpenAPI 3.2 has the query operation
    LEngine.Engine.Parameters.Values['OpenAPI.openapi'] := '3.2.0';
    LOpenAPI := TOpenAPI.BuildFrom(LEngine.Engine, LApp);
    try
      Assert.AreEqual('3.2.0', LOpenAPI.openapi);
      var LIds := QueryOperationIds(LOpenAPI);
      Assert.AreEqual(1, Length(LIds), 'The QUERY endpoint should be documented');
      Assert.AreEqual('Search', LIds[0]);
    finally
      LOpenAPI.Free;
    end;
  finally
    LEngine.Free;
  end;
end;

function OperationById(const AOpenAPI: TOpenAPI; const AOperationId: string): TOperation;
begin
  Result := nil;
  for var LPath in AOpenAPI.paths.Values do
    for var LOperation in [LPath.get, LPath.post, LPath.put, LPath.delete, LPath.patch] do
      if Assigned(LOperation) and (LOperation.operationId = AOperationId) then
        Exit(LOperation);
end;

procedure TMARSOpenAPI3Test.RequestBodyWithoutConsumes;
begin
  // TOpenAPIBodyResource (Tests.DefaultEngine.Resources) has no [Consumes]
  var LEngine := TDefaultEngine.Create;
  try
    var LOpenAPI := TOpenAPI.BuildFrom(LEngine.Engine, LEngine.Engine.ApplicationByName('DefaultApp'));
    try
      // record: application/json, schema in components
      var LOperation := OperationById(LOpenAPI, 'Elabora');
      Assert.IsNotNull(LOperation, 'Elabora');
      Assert.IsTrue(LOperation.requestBody.content.ContainsKey('application/json'), 'record body');
      Assert.AreEqual('#/components/schemas/TOpenAPIPayload'
        , LOperation.requestBody.content['application/json'].schema.ref);
      Assert.IsTrue(LOpenAPI.components.schemas.ContainsKey('TOpenAPIPayload'), 'record schema');
      Assert.AreEqual(2, LOpenAPI.components.schemas['TOpenAPIPayload'].properties.Count);

      // string: text/plain
      LOperation := OperationById(LOpenAPI, 'Text');
      Assert.IsTrue(LOperation.requestBody.content.ContainsKey('text/plain'), 'string body');
      Assert.AreEqual('string', LOperation.requestBody.content['text/plain'].schema.&type);

      // form params: application/x-www-form-urlencoded
      LOperation := OperationById(LOpenAPI, 'Form');
      Assert.IsTrue(LOperation.requestBody.content.ContainsKey('application/x-www-form-urlencoded'), 'form');

      // no body, no requestBody
      LOperation := OperationById(LOpenAPI, 'NoBody');
      Assert.AreEqual(0, LOperation.requestBody.content.Count, 'no body');
    finally
      LOpenAPI.Free;
    end;
  finally
    LEngine.Free;
  end;
end;

procedure TMARSOpenAPI3Test.MetaRequestBody;
begin
  var LEngine := TDefaultEngine.Create;
  try
    var LOpenAPI := TOpenAPI.BuildFrom(LEngine.Engine, LEngine.Engine.ApplicationByName('DefaultApp'));
    try
      // TMARSTokenResource.DoLogin: username and password read from the form
      var LOperation := OperationById(LOpenAPI, 'DoLogin');
      Assert.IsNotNull(LOperation, 'DoLogin');
      var LContent := LOperation.requestBody.content['application/x-www-form-urlencoded'];
      Assert.AreEqual('#/components/schemas/TCredentials', LContent.schema.ref);
      var LCredentials := LOpenAPI.components.schemas['TCredentials'];
      Assert.AreEqual(2, LCredentials.properties.Count);
      Assert.AreEqual('username', LCredentials.properties[0].Key);
      Assert.AreEqual('password', LCredentials.properties[1].Key);
      Assert.AreEqual('Credentials: username and password', LOperation.requestBody.description);

      // with [Consumes]
      LOperation := OperationById(LOpenAPI, 'ManualForm');
      Assert.AreEqual('#/components/schemas/TOpenAPIPayload'
        , LOperation.requestBody.content['application/x-www-form-urlencoded'].schema.ref);
      Assert.AreEqual('The payload', LOperation.requestBody.description);

      // without [Consumes]: media type from the type
      LOperation := OperationById(LOpenAPI, 'ManualJSON');
      Assert.AreEqual('#/components/schemas/TOpenAPIPayload'
        , LOperation.requestBody.content['application/json'].schema.ref);

      // a type that does not exist: no body, no error
      LOperation := OperationById(LOpenAPI, 'ManualWrong');
      Assert.AreEqual(0, LOperation.requestBody.content.Count);
    finally
      LOpenAPI.Free;
    end;
  finally
    LEngine.Free;
  end;
end;

procedure TMARSOpenAPI3Test.RequiredPermitAllAndProcedures;
begin
  var LEngine := TDefaultEngine.Create;
  try
    // the security schemes need a secret
    var LApp := LEngine.Engine.ApplicationByName('DefaultApp');
    LApp.Parameters.Values['JWT.Secret'] := 'openapi-test-secret-0123456789-0123456789-0123456789';
    var LOpenAPI := TOpenAPI.BuildFrom(LEngine.Engine, LApp);
    try
      // [Required] parameters and bodies; path parameters always
      var LRequired := LOpenAPI.paths['/required'];
      Assert.IsTrue(LRequired.get.parameters[0].required, 'query [Required]');
      Assert.IsTrue(LRequired.post.requestBody.required, 'body [Required]');
      Assert.IsTrue(LOpenAPI.paths['/required/header'].get.parameters[0].required, 'header [Required]');
      Assert.IsTrue(LOpenAPI.paths['/item/{id}'].get.parameters[0].required, 'path');
      Assert.IsFalse(LOpenAPI.paths['/item/single'].post.requestBody.required, 'body without [Required]');

      // PermitAll alone: public
      var LCases := LOpenAPI.paths['/openapicases'];
      Assert.AreEqual(0, LCases.get.security.Count, 'PermitAll: no security requirement');
      Assert.IsFalse(LCases.get.responses.ContainsKey('403'));
      // roles: the scopes
      Assert.AreEqual(2, LCases.delete.security.Count, 'bearer and cookie');
      Assert.AreEqual('admin', string.Join(',', LCases.delete.security[0]['JWT_bearer']));
      // a token, any role: a requirement without scopes ([Authenticated], or PermitAll with roles)
      for var LPath in ['/openapicases/signed', '/openapicases/anytoken'] do
      begin
        var LOperation := LOpenAPI.paths[LPath].get;
        Assert.AreEqual(2, LOperation.security.Count, LPath);
        Assert.AreEqual(0, Length(LOperation.security[0]['JWT_bearer']), LPath);
        Assert.IsTrue(LOperation.responses.ContainsKey('403'), LPath);
      end;

      // a procedure: a response without content
      Assert.AreEqual(0, LCases.delete.responses['200'].content.Count, 'no content for a procedure');
      Assert.AreEqual(1, LCases.get.responses['200'].content.Count);
    finally
      LOpenAPI.Free;
    end;
  finally
    LEngine.Free;
  end;
end;

procedure TMARSOpenAPI3Test.JSONValuesAndStreams;
begin
  var LEngine := TDefaultEngine.Create;
  try
    var LOpenAPI := TOpenAPI.BuildFrom(LEngine.Engine, LEngine.Engine.ApplicationByName('DefaultApp'));
    try
      Assert.IsTrue(LOpenAPI.components.schemas.ContainsKey('TOpenAPIJSONHolder'));
      // JSON values are JSON: any value, an object, an array; not their Delphi properties
      var LValue := LOpenAPI.components.schemas['TJSONValue'];
      Assert.AreEqual('', LValue.&type, 'any JSON value');
      Assert.AreEqual(0, LValue.properties.Count);
      Assert.AreEqual('object', LOpenAPI.components.schemas['TJSONObject'].&type);
      Assert.AreEqual(0, LOpenAPI.components.schemas['TJSONObject'].properties.Count);
      Assert.AreEqual('array', LOpenAPI.components.schemas['TJSONArray'].&type);
      // a stream: its bytes
      Assert.AreEqual('string', LOpenAPI.components.schemas['TStream'].&type);
      Assert.AreEqual(0, LOpenAPI.components.schemas['TStream'].properties.Count);
    finally
      LOpenAPI.Free;
    end;
  finally
    LEngine.Free;
  end;
end;

procedure TMARSOpenAPI3Test.DataSetsAndFrameworkTypes;

  function Schema(const AOpenAPI: TOpenAPI; const APath: string; const AGet: Boolean; const AMediaType: string): TSchema;
  var
    LContent: TDictionary<string, TMediaTypeObj>;
  begin
    Assert.IsTrue(AOpenAPI.paths.ContainsKey(APath), APath);
    if AGet then
      LContent := AOpenAPI.paths[APath].get.responses['200'].content
    else
      LContent := AOpenAPI.paths[APath].post.requestBody.content;
    Assert.IsTrue(LContent.ContainsKey(AMediaType), APath + ' ' + AMediaType + ': ' + string.Join(',', LContent.Keys.ToArray));
    Result := LContent[AMediaType].schema;
  end;

begin
  var LEngine := TDefaultEngine.Create;
  try
    var LOpenAPI := TOpenAPI.BuildFrom(LEngine.Engine, LEngine.Engine.ApplicationByName('DefaultApp'));
    try
      // no types of the libraries: FireDAC, DB, events, interfaces, pointers, MARS internals
      for var LName in LOpenAPI.components.schemas.Keys do
      begin
        Assert.IsFalse(LName.StartsWith('TFD') or LName.StartsWith('IFD') or LName.StartsWith('EFD'), LName);
        Assert.IsFalse(MatchText(LName, ['TDataSet', 'TField', 'TFields', 'TNotifyEvent', 'Pointer',
          'IInterface', 'TClass', 'TMARSParameters', 'TCollection', 'TPersistent']), LName);
      end;

      // a dataset: its rows in JSON, opaque in the native format
      var LRows := Schema(LOpenAPI, '/openapidata/query', True, TMediaType.APPLICATION_JSON);
      Assert.AreEqual('array', LRows.&type);
      Assert.AreEqual('object', LRows.items.&type);
      Assert.AreEqual('object', Schema(LOpenAPI, '/openapidata/query', True, TMediaType.APPLICATION_JSON_FireDAC).&type);
      // arrays of datasets: a member per dataset; in input as their native format too
      Assert.AreEqual('object', Schema(LOpenAPI, '/openapidata/sets', True, TMediaType.APPLICATION_JSON).&type);
      Assert.AreEqual('object', Schema(LOpenAPI, '/openapidata/deltas', False, TMediaType.APPLICATION_JSON_FireDAC).&type);
      Assert.AreEqual('array', Schema(LOpenAPI, '/openapidata/rows', False, TMediaType.APPLICATION_JSON).&type);

      // [MetaResult]: the shape chosen by the developer
      var LTyped := Schema(LOpenAPI, '/openapidata/typed', True, TMediaType.APPLICATION_JSON);
      Assert.AreEqual('array', LTyped.&type);
      Assert.AreEqual('#/components/schemas/TDataRow', LTyped.items.ref);
      Assert.AreEqual('The rows', LTyped.description);
      Assert.AreEqual('#/components/schemas/TDataRow', Schema(LOpenAPI, '/openapidata/single', True, TMediaType.APPLICATION_JSON).ref);
      Assert.IsTrue(LOpenAPI.components.schemas.ContainsKey('TDataRow'));
      // a wrong name: the declared type
      Assert.AreEqual('array', Schema(LOpenAPI, '/openapidata/wrong', True, TMediaType.APPLICATION_JSON).&type);

      // a class: its data only; a dataset member as rows, a component opaque
      Assert.IsTrue(LOpenAPI.components.schemas.ContainsKey('TComponentHolder'), 'TComponentHolder');
      var LHolder := LOpenAPI.components.schemas['TComponentHolder'];
      var LNames: TArray<string> := [];
      for var LProperty in LHolder.properties do
        LNames := LNames + [LProperty.Key];
      Assert.AreEqual('Name,Rows,Owner', string.Join(',', LNames));
      Assert.AreEqual('array', LHolder.GetProperty('Rows').&type, 'a dataset member: its rows');
      Assert.IsTrue(LOpenAPI.components.schemas.ContainsKey('TComponent'), 'TComponent: ' + string.Join(',', LOpenAPI.components.schemas.Keys.ToArray));
      Assert.AreEqual('object', LOpenAPI.components.schemas['TComponent'].&type, 'a component: opaque');
      Assert.AreEqual(0, LOpenAPI.components.schemas['TComponent'].properties.Count);

      // the token as TMARSTokenWriterJSON writes it
      Assert.IsTrue(LOpenAPI.components.schemas.ContainsKey('TMARSToken'), 'TMARSToken');
      var LToken := LOpenAPI.components.schemas['TMARSToken'];
      LNames := [];
      for var LProperty in LToken.properties do
        LNames := LNames + [LProperty.Key];
      Assert.AreEqual('Token,IsVerified,UserName,Roles,Expiration,IssuedAt,Claims', string.Join(',', LNames));
      Assert.AreEqual('string', LToken.GetProperty('Roles').&type, 'comma separated');
    finally
      LOpenAPI.Free;
    end;
  finally
    LEngine.Free;
  end;
end;

function FindParameter(const AOperation: TOperation; const AIn, AName: string; out ACount: Integer): TParameter;
begin
  Result := nil;
  ACount := 0;
  for var LParameter in AOperation.parameters do
    if (LParameter.&in = AIn) and SameText(LParameter.name, AName) then
    begin
      Result := LParameter;
      Inc(ACount);
    end;
end;

procedure TMARSOpenAPI3Test.HeadersOfResourcesAndApplication;
var
  LCount: Integer;
begin
  var LEngine := TDefaultEngine.Create;
  try
    var LApplication := LEngine.Engine.ApplicationByName('DefaultApp');
    LApplication.Parameters.Values['OpenAPI.Header.X-Client'] := 'required; The client application';
    LApplication.Parameters.Values['OpenAPI.Header.X-Debug'] := 'Optional debugging';
    // of the engine, for every application; the application wins on the same name
    LEngine.Engine.Parameters.Values['OpenAPI.Header.X-Region'] := 'required';
    LEngine.Engine.Parameters.Values['OpenAPI.Header.X-Client'] := 'optional; Overridden';
    var LOpenAPI := TOpenAPI.BuildFrom(LEngine.Engine, LApplication);
    try
      var LGet := LOpenAPI.paths['/headers'].get;
      // a field
      var LParam := FindParameter(LGet, 'header', 'X-Tenant', LCount);
      Assert.IsNotNull(LParam, 'X-Tenant of the field');
      Assert.AreEqual('The tenant', LParam.description);
      Assert.IsTrue(LParam.required, '[Required] field');
      Assert.AreEqual('string', LParam.schema.&type);
      // a property
      LParam := FindParameter(LGet, 'query', 'lang', LCount);
      Assert.IsNotNull(LParam, 'lang of the property');
      Assert.AreEqual('The language', LParam.description);
      Assert.IsFalse(LParam.required);
      // [MetaHeader] of the method and of the resource
      LParam := FindParameter(LGet, 'header', 'X-Trace', LCount);
      Assert.IsNotNull(LParam, 'X-Trace');
      Assert.AreEqual('A trace id', LParam.description);
      Assert.IsFalse(LParam.required);
      LParam := FindParameter(LGet, 'header', 'X-Api-Key', LCount);
      Assert.IsNotNull(LParam, 'X-Api-Key');
      Assert.AreEqual('The key of the client', LParam.description);
      Assert.IsTrue(LParam.required);
      // OpenAPI.Header.<name>
      LParam := FindParameter(LGet, 'header', 'X-Client', LCount);
      Assert.IsNotNull(LParam, 'X-Client');
      Assert.AreEqual('The client application', LParam.description);
      Assert.IsTrue(LParam.required);
      LParam := FindParameter(LGet, 'header', 'X-Debug', LCount);
      Assert.AreEqual('Optional debugging', LParam.description);
      Assert.IsFalse(LParam.required);
      LParam := FindParameter(LGet, 'header', 'X-Region', LCount);
      Assert.IsNotNull(LParam, 'X-Region of the engine');
      Assert.IsTrue(LParam.required);
      Assert.AreEqual('', LParam.description);
      FindParameter(LGet, 'header', 'X-Client', LCount);
      Assert.AreEqual(1, LCount, 'X-Client once');

      // the parameter of the method wins, once
      LParam := FindParameter(LOpenAPI.paths['/headers/own'].get, 'header', 'X-Tenant', LCount);
      Assert.AreEqual(1, LCount);
      Assert.AreEqual('Own tenant', LParam.description);
      Assert.IsFalse(LParam.required);
      LParam := FindParameter(LOpenAPI.paths['/headers/key'].get, 'header', 'X-Api-Key', LCount);
      Assert.AreEqual(1, LCount);
      Assert.AreEqual('', LParam.description);

      // the application headers are on every operation
      Assert.IsNotNull(FindParameter(LOpenAPI.paths['/item'].get, 'header', 'X-Client', LCount));
      Assert.IsNull(FindParameter(LOpenAPI.paths['/item'].get, 'header', 'X-Tenant', LCount));
    finally
      LOpenAPI.Free;
    end;
  finally
    LEngine.Free;
  end;
end;

procedure TMARSOpenAPI3Test.NoBodyWithoutBody;
begin
  var LEngine := TDefaultEngine.Create;
  try
    var LOpenAPI := TOpenAPI.BuildFrom(LEngine.Engine, LEngine.Engine.ApplicationByName('DefaultApp'));
    try
      // TItemResource declares Consumes(application/json) on the class
      Assert.AreEqual(0, LOpenAPI.paths['/item/{id}'].get.requestBody.content.Count, 'GET without a body');
      Assert.AreEqual(0, LOpenAPI.paths['/item'].get.requestBody.content.Count, 'GET without a body');
      Assert.AreEqual(1, LOpenAPI.paths['/item'].post.requestBody.content.Count, 'a [BodyParam]');
      Assert.AreEqual(1, LOpenAPI.paths['/item/single'].post.requestBody.content.Count, 'a [BodyParam]');
      // a Consumes of the method itself (TOpenAPIDataResource.Deltas, with a body parameter too)
      Assert.AreEqual(1, LOpenAPI.paths['/openapidata/deltas'].post.requestBody.content.Count);
      Assert.AreEqual(1, LOpenAPI.paths['/openapidata/raw'].post.requestBody.content.Count, 'own Consumes, no parameter');
    finally
      LOpenAPI.Free;
    end;
  finally
    LEngine.Free;
  end;
end;

procedure TMARSOpenAPI3Test.MembersLeftOutOfTheJSON;
begin
  var LType := TRttiContext.Create.GetType(TOpenAPIHiddenMembers);
  var LOpenAPI := TOpenAPI.Create;
  try
    Assert.IsTrue(LOpenAPI.EnsureTypeInComponentsSchemas(LType));
    var LSchema := LOpenAPI.components.schemas[LType.Name];
    var LNames: TArray<string> := [];
    for var LProperty in LSchema.properties do
      LNames := LNames + [LProperty.Key];
    Assert.AreEqual('Name,alias', string.Join(',', LNames));

    var LJSON := TJSONObject.ObjectToJSON(LOpenAPI);
    try
      Assert.DoesNotContain(LJSON.ToJSON, '"":');
    finally
      LJSON.Free;
    end;
  finally
    LOpenAPI.Free;
  end;
end;

{$IFDEF MARS_YAML}
procedure TMARSOpenAPI3Test.SecurityAsYAML;
begin
  var LEngine := TDefaultEngine.Create;
  try
    var LApp := LEngine.Engine.ApplicationByName('DefaultApp');
    LApp.Parameters.Values['JWT.Secret'] := 'openapi-test-secret-0123456789-0123456789-0123456789';
    var LOpenAPI := TOpenAPI.BuildFrom(LEngine.Engine, LApp);
    try
      // security requirements: a list of objects, each a name with its scopes
      var LYAML := TMARSYAML.ObjectToYAML(LOpenAPI).ToYaml;
      var LDocument := TYamlDocument.Parse(LYAML);
      var LPaths := LDocument.Root.Values['paths'];

      var LSecurity := LPaths.Values['/openapicases'].Values['delete'].Values['security'];
      Assert.IsTrue(LSecurity.IsSequence, 'security: a sequence');
      Assert.AreEqual(2, LSecurity.Count, 'bearer and cookie');
      Assert.AreEqual('admin', LSecurity.Nodes[0].Values['JWT_bearer'].Nodes[0].ToString);
      Assert.AreEqual('admin', LSecurity.Nodes[1].Values['JWT_cookie'].Nodes[0].ToString);

      // a token, any role: the scopes are an empty sequence, not missing ({} would be public)
      LSecurity := LPaths.Values['/openapicases/anytoken'].Values['get'].Values['security'];
      Assert.AreEqual(2, LSecurity.Count);
      Assert.IsTrue(LSecurity.Nodes[0].Values['JWT_bearer'].IsSequence, 'JWT_bearer: []');
      Assert.AreEqual(0, LSecurity.Nodes[0].Values['JWT_bearer'].Count);

      // the other dictionaries as before
      Assert.IsTrue(LPaths.Values['/openapicases'].Values['get'].Values['responses'].Values['200'].IsMapping);

      // the properties of a schema (a list of pairs): a mapping of schemas, not the Delphi list
      var LProperties := LDocument.Root.Values['components'].Values['schemas'].Values['TOpenAPIJSONHolder'].Values['properties'];
      Assert.IsTrue(LProperties.IsMapping, 'properties: a mapping');
      Assert.AreEqual(LOpenAPI.components.schemas['TOpenAPIJSONHolder'].properties.Count, LProperties.Count);
      Assert.AreEqual('#/components/schemas/TJSONValue', LProperties.Values['Value'].Values['$ref'].ToString);
      Assert.IsFalse(LProperties.Contains('Capacity'), 'no properties of the list');
    finally
      LOpenAPI.Free;
    end;
  finally
    LEngine.Free;
  end;
end;

procedure TMARSOpenAPI3Test.StringsStayStringsInYAML;
begin
  // strings that plain YAML reads as numbers, booleans or null are quoted
  var LYAML := TMARSYAML.TValueToYAML(TValue.From<TArray<string>>(['8080', '0', '-1.5', 'true', 'null', 'abc', 'v1'])).ToYaml;
  for var LQuoted in ['''8080''', '''0''', '''-1.5''', '''true''', '''null'''] do
    Assert.IsTrue(LYAML.Contains('- ' + LQuoted), LQuoted + ' in ' + LYAML);
  Assert.IsFalse(LYAML.Contains('!'), 'no tags: ' + LYAML);
  Assert.IsTrue(LYAML.Contains('- abc'), 'abc plain');
  Assert.IsTrue(LYAML.Contains('- v1'), 'v1 plain');
  Assert.AreEqual('''8080''', TMARSYAML.TValueToYAML('8080').ToYaml.Trim);

  // the status codes, keys of the responses
  var LEngine := TDefaultEngine.Create;
  try
    var LOpenAPI := TOpenAPI.BuildFrom(LEngine.Engine, LEngine.Engine.ApplicationByName('DefaultApp'));
    try
      LYAML := TMARSYAML.ObjectToYAML(LOpenAPI).ToYaml;
      Assert.IsTrue(LYAML.Contains('''200'':'), 'quoted status code');
      Assert.IsFalse(LYAML.Contains(' 200:'), 'no plain status code');
    finally
      LOpenAPI.Free;
    end;
  finally
    LEngine.Free;
  end;
end;

function YAMLOptionsCase: TYAMLOptionsCase;
begin
  Result := Default(TYAMLOptionsCase);
  Result.Day := EncodeDate(2026, 10, 19);
  Result.At := EncodeDate(2026, 10, 19) + EncodeTime(9, 30, 0, 0);
end;

function YAMLOf(const ARecord: TYAMLOptionsCase; const AOptions: TMARSJSONSerializationOptions): string;
begin
  Result := TMARSYAML.RecordToYAML(TValue.From<TYAMLOptionsCase>(ARecord), AOptions).ToYaml.Replace(#13, '');
end;

procedure TMARSOpenAPI3Test.DatesInYAML;
var
  LOptions: TMARSJSONSerializationOptions;
  LYAML: string;
begin
  LOptions := DefaultMARSJSONSerializationOptions;

  LOptions.DateIsUTC := True;
  LYAML := YAMLOf(YAMLOptionsCase, LOptions);
  Assert.Contains(LYAML, 'At: 2026-10-19T09:30:00.000Z', LYAML);
  Assert.Contains(LYAML, 'Day: 2026-10-19'#10, LYAML);
  // a zero date is empty (ZeroDateAsEmpty), then omitted (SkipEmptyStrings)
  Assert.DoesNotContain(LYAML, 'Zero', LYAML);

  LOptions.DateIsUTC := False;
  LYAML := YAMLOf(YAMLOptionsCase, LOptions);
  Assert.Contains(LYAML, 'At: ' + DateToJSON(YAMLOptionsCase.At, LOptions), LYAML);

  LOptions.ZeroDateAsEmpty := False;
  LYAML := YAMLOf(YAMLOptionsCase, LOptions);
  Assert.Contains(LYAML, 'Zero: 1899-12-30T', LYAML);
  LOptions.ZeroDateAsEmpty := True;

  LOptions.TDateAsDate := False;
  LYAML := YAMLOf(YAMLOptionsCase, LOptions);
  Assert.Contains(LYAML, 'Day: ' + DateToJSON(YAMLOptionsCase.Day, LOptions), LYAML);

  // elements of sequences keep their position: an empty date is ''
  LYAML := TMARSYAML.TValueToYAML(TValue.From<TArray<TDateTime>>([0, YAMLOptionsCase.At]), LOptions).ToYaml;
  Assert.Contains(LYAML, '- '''''#10'- ' + DateToJSON(YAMLOptionsCase.At, LOptions), LYAML.Replace(#13, ''));
end;

procedure TMARSOpenAPI3Test.EmptyValuesInYAML;
var
  LOptions: TMARSJSONSerializationOptions;
  LYAML: string;
begin
  // the defaults: only 0 among the empty values
  LOptions := DefaultMARSJSONSerializationOptions;
  LOptions.DateIsUTC := True;
  Assert.AreEqual('Count: 0'#10'Day: 2026-10-19'#10'At: 2026-10-19T09:30:00.000Z',
    YAMLOf(YAMLOptionsCase, LOptions).Trim);
  // the overloads without options: the global default
  Assert.AreEqual(YAMLOf(YAMLOptionsCase, DefaultMARSJSONSerializationOptions),
    TMARSYAML.RecordToYAML(TValue.From<TYAMLOptionsCase>(YAMLOptionsCase)).ToYaml.Replace(#13, ''));

  LOptions.IncludeEmptyOrNullValues;
  LYAML := YAMLOf(YAMLOptionsCase, LOptions);
  for var LExpected in ['Name: ''''', 'Count: 0', 'Active: false', 'Tags: []', 'Nested:'#10'  Note: '''''] do
    Assert.Contains(LYAML, LExpected + #10, LYAML);
  // an object without members
  LOptions.SkipEmptyStrings := True;
  LYAML := YAMLOf(YAMLOptionsCase, LOptions);
  Assert.Contains(LYAML, 'Nested: {}'#10, LYAML);

  LOptions.SkipAllEmptyOrNullValues;
  LYAML := YAMLOf(YAMLOptionsCase, LOptions);
  for var LMissing in ['Name', 'Count', 'Active', 'Tags', 'Nested'] do
    Assert.DoesNotContain(LYAML, LMissing, LYAML);

  // elements of sequences keep their position
  LYAML := TMARSYAML.TValueToYAML(TValue.From<TArray<Integer>>([1, 0]), LOptions).ToYaml;
  Assert.AreEqual('- 1'#10'- 0', LYAML.Replace(#13, '').Trim);
end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TMARSOpenAPI3Test);


end.
