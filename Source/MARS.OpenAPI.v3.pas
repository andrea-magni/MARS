unit MARS.OpenAPI.v3;

interface

uses
  Classes, SysUtils, Generics.Collections;

type
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

  TParameter = class
  public
  end;


  TRequestBody = class
  public
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

  TMediaTypeObj = class
  public

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

  TSecurityRequirement = class
  public
  end;

  TOperation = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddResponse(const AStatusCode: string): TResponse;
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
    security: TObjectList<TSecurityRequirement>;
    servers: TObjectList<TServer>;
  end;

  TPathItem = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    ref: string; // $ref
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

  TComponents = class
  public
  end;

  TSecurity = class
  public
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
    security: TObjectList<TSecurity>;
    tags: TObjectList<TTag>;
    externalDocs: TExternalDocumentation;
  end;

implementation


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

function TOperation.AddResponse(const AStatusCode: string): TResponse;
begin
  Result := TResponse.Create;
  responses.Add(AStatusCode, Result);
end;

constructor TOperation.Create;
begin
  inherited Create;
  externalDocs := TExternalDocumentation.Create;
  callbacks := TObjectDictionary<string, TCallback>.Create([doOwnsValues]);
  requestBody := TRequestBody.Create;
  responses := TObjectDictionary<string, TResponse>.Create([doOwnsValues]);
  parameters := TObjectList<TParameter>.Create(True);
  security := TObjectList<TSecurityRequirement>.Create(True);
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
  security := TObjectList<TSecurity>.Create(True);
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

end.
