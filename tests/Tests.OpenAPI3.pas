unit Tests.OpenAPI3;

interface
uses
  Classes, SysUtils, Generics.Collections
, DUnitX.TestFramework
, MARS.Core.URL, MARS.Core.Utils

, MARS.OpenAPI.v3, MARS.OpenAPI.v3.Utils
;

type
  [TestFixture('OpenAPI3')]
  TMARSOpenAPI3Test = class(TObject)
  private
  public
    [Test] procedure Basic;
    [Test] procedure QueryOperationNeedsOpenAPI32;
  end;

implementation

uses
  System.Rtti, System.TypInfo
, System.JSON, MARS.Core.JSON
, Tests.Objects.Types, Tests.DefaultEngine.Definition
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

initialization
  TDUnitX.RegisterTestFixture(TMARSOpenAPI3Test);


end.
