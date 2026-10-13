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
{$IFDEF MARS_YAML}
    [Test] procedure SecurityAsYAML;
    [Test] procedure StringsStayStringsInYAML;
{$ENDIF}
  end;

implementation

uses
  System.Rtti, System.TypInfo
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
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TMARSOpenAPI3Test);


end.
