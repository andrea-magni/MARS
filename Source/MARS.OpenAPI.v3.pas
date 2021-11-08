unit MARS.OpenAPI.v3;

interface

uses
  Classes, SysUtils, Generics.Collections, System.Rtti, System.TypInfo
, MARS.Core.JSON, MARS.YAML.ReadersAndWriters, MARS.Rtti.Utils
;

type
  TOpenAPI = class; // fwd

  TReferenceableType = class
  public
    [JSONName('$ref'), YAMLName('$ref')] ref: string;
  end;

  TContact = class
  public
    name: string;
    url: string;
    email: string;
  end;

  TLicense = class
  public
    name: string; // required
    identifier: string;
    url: string;
  end;

  TXLogo = record
    url: string;
    backgroundColor: string;
    altText: string;
    href: string;
  end;

  TInfo = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    title: string; // required
    summary: string;
    description: string;
    termsOfService: string;
    contact: TContact;
    license: TLicense;
    version: string; // required
    [JSONName('x-logo'), YAMLName('x-logo')] x_logo: TXLogo;
  end;

  TServerVariable = class
  public
    constructor Create(const AEnum: TArray<string>; const ADefault: string; const ADescription: string); virtual;
  public
    enum: TArray<string>;
    default: string; // required
    description: string;
  end;

  TServer = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    url: string; // required
    description: string;
    variables: TObjectDictionary<string, TServerVariable>;
  end;

  TExternalDocumentation = class
  public
  end;

  TSchemaBase = class(TReferenceableType)
  public
    procedure SetType(const AType: string);
  public
    &type: string; // - Value MUST be a string. Multiple types via an array are not supported.
    description: string; // - CommonMark syntax MAY be used for rich text representation.
  end;

  TSchema = class(TSchemaBase)
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddProperty(const AName: string): TSchema;
    procedure SetType(const AType: TRttiType; const AOpenAPI: TOpenAPI); overload;
  public
    title: string;
    multipleOf: string;
    maximum: string;
    exclusiveMaximum: string;
    minimum: string;
    exclusiveMinimum: string;
    maxLength: string;
    minLength: string;
    pattern: string; // (This string SHOULD be a valid regular expression, according to the ECMA 262 regular expression dialect)
    maxItems: string;
    minItems: string;
    uniqueItems: string;
    maxProperties: string;
    minProperties: string;
    required: boolean;
    enum: TArray<string>;

    allOf: string; // - Inline or referenced schema MUST be of a Schema Object and not a standard JSON Schema.
    oneOf: string; // - Inline or referenced schema MUST be of a Schema Object and not a standard JSON Schema.
    anyOf: string; // - Inline or referenced schema MUST be of a Schema Object and not a standard JSON Schema.
    &not: string;  //  - Inline or referenced schema MUST be of a Schema Object and not a standard JSON Schema.
    items: TSchemaBase; // - Value MUST be an object and not an array. Inline or referenced schema MUST be of a Schema Object and not a standard JSON Schema. items MUST be present if the type is array.
    properties: TObjectDictionary<string,TSchema>; // - Property definitions MUST be a Schema Object and not a standard JSON Schema (inline or referenced).
    additionalProperties: string; // - Value can be boolean or object. Inline or referenced schema MUST be of a Schema Object and not a standard JSON Schema. Consistent with JSON Schema, additionalProperties defaults to true.
    format: string; // - See Data Type Formats for further details. While relying on JSON Schema's defined formats, the OAS offers a few additional predefined formats.
    default: string; // - The default value represents what would be assumed by the consumer of the input as the value of the schema if one is not provided. Unlike JSON Schema, the value MUST conform to the defined type for the Schema Object defined at the same level. For example, if type is string, then default can be "foo" but cannot be 1.
  end;


  TMediaTypeObj = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    schema: TSchema;
  end;


  TParameter = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    name: string; // required
    &in: string; // query, header, path, cookie // required
    description: string;
    required: boolean;
    &deprecated: boolean;
    allowEmptyValue: boolean;
    schema: TSchema;
  end;


  TRequestBody = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddContent(const AMediaType: string): TMediaTypeObj;
  public
    description: string;
    content: TObjectDictionary<string, TMediaTypeObj>;
    required: boolean;
  end;

  THeader = class
  public
//    constructor Create; virtual;
//    destructor Destroy; override;
  public
    description: string;
    required: boolean;
    &deprecated: boolean;
//    allowEmptyValue: boolean;
    style: string;
//    schema: TSchema;
//    example: TExample;
//    examples: TObjectDictionary<string, TExample>;
  end;

  TLink = class
  public
  end;

  TResponse = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddContent(const AMediaType: string): TMediaTypeObj;
  public
    description: string;
    headers: TObjectDictionary<string, THeader>;
    content: TObjectDictionary<string, TMediaTypeObj>;
    links: TObjectDictionary<string, TLink>;
  end;

  TCallback = class
  public
  end;

  TSecurityRequirement = TArray<string>;

  TOperation = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddResponse(const AStatusCode: string): TResponse;
    function AddParameter(const AName: string; const AIn: string): TParameter;
    procedure AddSecurityRequirement(const AName: string; const ARoles: TArray<string>);
  public
    tags: TArray<string>;
    summary: string;
    description: string;
    externalDocs: TExternalDocumentation;
    operationId: string;
    parameters: TObjectList<TParameter>;
    requestBody: TRequestBody;
    responses: TObjectDictionary<string, TResponse>;
    callbacks: TObjectDictionary<string, TCallback>;
    &deprecated: Boolean;
    security: TObjectList<TDictionary<string,TSecurityRequirement>>;
    servers: TObjectList<TServer>;
  end;

  TPathItem = class(TReferenceableType)
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function OperationByHttpMethod(const AHttpMethod: string): TOperation;
  public
    summary: string;
    description: string;
    get: TOperation;
    put: TOperation;
    post: TOperation;
    delete: TOperation;
    options: TOperation;
    head: TOperation;
    patch: TOperation;
    trace: TOperation;
    servers: TObjectList<TServer>;
    parameters: TObjectList<TParameter>;
  end;

  TSecurityScheme = class(TReferenceableType)
  public
    &type: string; // required
    description: string;
    name: string; // apiKey required
    &in: string; // apiKey required
    scheme: string; // http required
    bearerFormat: string; // http bearer
    flows: string; // oauth2
    openIdConnectUrl: string; //openIdConnect
  end;

  TComponents = class
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function AddSchema(const AName: string): TSchema;
    function GetSchema(const AName: string; const ACreateIfMissing: Boolean = True): TSchema;
    function HasSchema(const AName: string): Boolean;
    function AddSecurityScheme(const AName: string; const AType: string): TSecurityScheme;
  public
    schemas: TObjectDictionary<string,TSchema>;
    securitySchemes: TObjectDictionary<string,TSecurityScheme>;
  end;

  TTag = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    name: string;
    description: string;
    externalDocs: TExternalDocumentation;
  end;

  TOpenAPI = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddServer: TServer;
    function GetPath(const APath: string; const ACreateIfMissing: Boolean = True): TPathItem;
    function AddTag(const AName: string; const ADescription: string = ''): TTag;
  public
    openapi: string; // required
    info: TInfo; // required
    servers: TObjectList<TServer>;
    paths: TObjectDictionary<string, TPathItem>; // required
    components: TComponents;
    security: TObjectList<TDictionary<string, TSecurityRequirement>>;
    tags: TObjectList<TTag>;
    externalDocs: TExternalDocumentation;
    [JSONName(''), YAMLName('')] FBearerSecurityConfigured: Boolean;
    [JSONName(''), YAMLName('')] FCookieSecurityConfigured: Boolean;
  end;

implementation


uses
  MARS.OpenAPI.v3.Utils;

{ TInfo }

constructor TInfo.Create;
begin
  inherited Create;
  contact := TContact.Create;
  license := TLicense.Create;
end;

destructor TInfo.Destroy;
begin
  license.Free;
  contact.Free;
  inherited;
end;

{ TServerVariable }

constructor TServerVariable.Create(const AEnum: TArray<string>; const ADefault,
  ADescription: string);
begin
  inherited Create;
  enum := AEnum;
  default := ADefault;
  description := ADescription;
end;

{ TServer }

constructor TServer.Create;
begin
  inherited Create;
  variables := TObjectDictionary<string, TServerVariable>.Create([doOwnsValues]);
end;

destructor TServer.Destroy;
begin
  variables.Free;
  inherited;
end;

{ TOperation }

function TOperation.AddParameter(const AName: string; const AIn: string): TParameter;
begin
  Result := TParameter.Create;
  Result.name := AName;
  Result.&in := AIn;
  parameters.Add(Result);
end;

function TOperation.AddResponse(const AStatusCode: string): TResponse;
begin
  Result := TResponse.Create;
  responses.Add(AStatusCode, Result);
end;

procedure TOperation.AddSecurityRequirement(const AName: string; const ARoles: TArray<string>);
var
  LDict: TDictionary<string, TSecurityRequirement>;
begin
  LDict := TDictionary<string,TSecurityRequirement>.Create;
  LDict.Add(AName, ARoles);
  security.Add(LDict);
end;

constructor TOperation.Create;
begin
  inherited Create;
  externalDocs := TExternalDocumentation.Create;
  callbacks := TObjectDictionary<string, TCallback>.Create([doOwnsValues]);
  requestBody := TRequestBody.Create;
  responses := TObjectDictionary<string, TResponse>.Create([doOwnsValues]);
  parameters := TObjectList<TParameter>.Create(True);
  security := TObjectList<TDictionary<string,TSecurityRequirement>>.Create(True);
  servers := TObjectList<TServer>.Create(True);
end;

destructor TOperation.Destroy;
begin
  servers.Free;
  security.Free;
  parameters.Free;
  responses.Free;
  requestBody.Free;
  callbacks.Free;
  externalDocs.Free;
  inherited;
end;

{ TPathItem }

constructor TPathItem.Create;
begin
  inherited Create;
  get := TOperation.Create;
  put := TOperation.Create;
  post := TOperation.Create;
  delete := TOperation.Create;
  options := TOperation.Create;
  head := TOperation.Create;
  patch := TOperation.Create;
  trace := TOperation.Create;
  parameters := TObjectList<TParameter>.Create(True);
  servers := TObjectList<TServer>.Create(True);
end;

destructor TPathItem.Destroy;
begin
  servers.Free;
  parameters.Free;
  trace.Free;
  patch.Free;
  head.Free;
  options.Free;
  delete.Free;
  post.Free;
  put.Free;
  get.Free;
  inherited;
end;

function TPathItem.OperationByHttpMethod(const AHttpMethod: string): TOperation;
var
  LHttpMethod: string;
begin
  LHttpMethod := AHttpMethod.ToLower;

  Result := nil;
  if LHttpMethod = 'get' then
    Result := get
  else if LHttpMethod = 'post' then
    Result := post
  else if LHttpMethod = 'put' then
    Result := put
  else if LHttpMethod = 'delete' then
    Result := delete
  else if LHttpMethod = 'options' then
    Result := options
  else if LHttpMethod = 'head' then
    Result := head
  else if LHttpMethod = 'patch' then
    Result := patch
  else if LHttpMethod = 'trace' then
    Result := trace;
end;

{ TOpenAPI }

function TOpenAPI.GetPath(const APath: string; const ACreateIfMissing: Boolean): TPathItem;
begin
  if not paths.TryGetValue(APath, Result) then
  begin
    Result := TPathItem.Create;
    paths.Add(APath, Result);
  end;
end;

function TOpenAPI.AddServer: TServer;
begin
  Result := TServer.Create;
  servers.Add(Result);
end;

function TOpenAPI.AddTag(const AName: string; const ADescription: string): TTag;
begin
  Result := TTag.Create;
  Result.name := AName;
  Result.description := ADescription;
  tags.Add(Result);
end;

constructor TOpenAPI.Create;
begin
  inherited Create;
  info := TInfo.Create;
  paths := TObjectDictionary<string, TPathItem>.Create([doOwnsValues]);
  components := TComponents.Create;
  externalDocs := TExternalDocumentation.Create;
  servers := TObjectList<TServer>.Create(True);
  security := TObjectList<TDictionary<string,TSecurityRequirement>>.Create(True);
  tags := TObjectList<TTag>.Create(True);
end;

destructor TOpenAPI.Destroy;
begin
  tags.Free;
  security.Free;
  servers.Free;
  externalDocs.Free;
  components.Free;
  paths.Free;
  info.Free;
  inherited;
end;

{ TResponse }

function TResponse.AddContent(const AMediaType: string): TMediaTypeObj;
begin
  Result := TMediaTypeObj.Create;
  content.Add(AMediaType, Result);
end;

constructor TResponse.Create;
begin
  inherited Create;
  headers := TObjectDictionary<string, THeader>.Create([doOwnsValues]);
  content := TObjectDictionary<string, TMediaTypeObj>.Create([doOwnsValues]);
  links := TObjectDictionary<string, TLink>.Create([doOwnsValues]);
end;

destructor TResponse.Destroy;
begin
  links.Free;
  content.Free;
  headers.Free;
  inherited;
end;

{ TTag }

constructor TTag.Create;
begin
  inherited Create;
  externalDocs := TExternalDocumentation.Create;
end;

destructor TTag.Destroy;
begin
  externalDocs.Free;
  inherited;
end;

{ TParameter }

constructor TParameter.Create;
begin
  inherited Create;
  schema := TSchema.Create;
end;

destructor TParameter.Destroy;
begin
  schema.Free;
  inherited;
end;

{ TRequestBody }

function TRequestBody.AddContent(const AMediaType: string): TMediaTypeObj;
begin
  Result := TMediaTypeObj.Create;
  content.Add(AMediaType, Result);
end;

constructor TRequestBody.Create;
begin
  inherited Create;
  content := TObjectDictionary<string,TMediaTypeObj>.Create([doOwnsValues]);
end;

destructor TRequestBody.Destroy;
begin
  content.Free;
  inherited;
end;

{ TMediaTypeObj }

constructor TMediaTypeObj.Create;
begin
  inherited Create;
  schema := TSchema.Create;
end;

destructor TMediaTypeObj.Destroy;
begin
  schema.Free;
  inherited;
end;

{ TSchema }

function TSchema.AddProperty(const AName: string): TSchema;
begin
  Result := TSchema.Create;
  properties.Add(AName, Result);
end;

constructor TSchema.Create;
begin
  inherited Create;
  properties := TObjectDictionary<string, TSchema>.Create([doOwnsValues]);
  items := TSchemaBase.Create();
end;

destructor TSchema.Destroy;
begin
  items.Free;
  properties.Free;
  inherited;
end;

procedure TSchema.SetType(const AType: TRttiType; const AOpenAPI: TOpenAPI);
var
  LElementType: TRttiType;
begin
  if AType.IsArray(LElementType) then
  begin
    SetType('array');
    items.SetType(AOpenAPI.MARSDataTypeToOpenAPIType(LElementType));
  end
  else if AType.IsDictionaryOfStringAndT(LElementType) then
  begin
    SetType('array');
    items.SetType(AOpenAPI.MARSDataTypeToOpenAPIType(LElementType));
  end
  else if AType.IsObjectListOfT(LElementType) then
  begin
    SetType('array');
    items.SetType(AOpenAPI.MARSDataTypeToOpenAPIType(LElementType));
  end
  else
    SetType(AOpenAPI.MARSDataTypeToOpenAPIType(AType));
end;

{ TComponents }

function TComponents.AddSchema(const AName: string): TSchema;
begin
  Result := TSchema.Create;
  schemas.Add(AName, Result);
end;

function TComponents.AddSecurityScheme(const AName: string; const AType: string): TSecurityScheme;
begin
  Result := TSecurityScheme.Create;
  Result.&type := AType;
  Result.name := AName;
  securitySchemes.Add(AName, Result);
end;

constructor TComponents.Create;
begin
  inherited Create;
  schemas := TObjectDictionary<string, TSchema>.Create([doOwnsValues]);
  securitySchemes := TObjectDictionary<string,TSecurityScheme>.Create([doOwnsValues]);
end;

destructor TComponents.Destroy;
begin
  securitySchemes.Free;
  schemas.Free;
  inherited;
end;

function TComponents.GetSchema(const AName: string;
  const ACreateIfMissing: Boolean): TSchema;
begin
  if not schemas.TryGetValue(AName, Result) then
    Result := AddSchema(AName);
end;

function TComponents.HasSchema(const AName: string): Boolean;
begin
  Result := schemas.ContainsKey(AName);
end;

{ TSchemaBase }

procedure TSchemaBase.SetType(const AType: string);
begin
  if AType.StartsWith('#') then
  begin
    ref := AType;
    &type := '';
  end
  else
  begin
    ref := '';
    &type := AType;
  end;
end;

end.
