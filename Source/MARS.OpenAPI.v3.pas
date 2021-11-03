unit MARS.OpenAPI.v3;

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TContact = record
    name: string;
    url: string;
    email: string;
  end;

  TLicense = record
    name: string; // required
    identifier: string;
    url: string;
  end;

  TInfo = record
    title: string; // required
    summary: string;
    description: string;
    termsOfService: string;
    contact: TContact;
    license: TLicense;
    version: string; // required
  end;

  TServerVariable = record
    enum: TArray<string>;
    default: string; // required
    description: string;
    constructor Create(const AEnum: TArray<string>; const ADefault: string; const ADescription: string);
  end;

  TServer = record
    url: string; // required
    description: string;
    variables: TDictionary<string, TServerVariable>;

    class operator Initialize(out ARecord: TServer);
    class operator Finalize(var ARecord: TServer);
    class operator Assign(var ADest: TServer; const [ref] ASource: TServer);
  end;

  TExternalDocumentation = record
  end;

  TParameter = record
  end;

  TRequestBody = record
  end;

  TResponses = record
  end;

  TCallback = record
  end;

  TSecurityRequirement = record
  end;

  TOperation = record
    tags: TArray<string>;
    summary: string;
    description: string;
    externalDocs: TExternalDocumentation;
    operationId: string;
    parameters: TArray<TParameter>;
    requestBody: TRequestBody;
    responses: TResponses;
    callbacks: TDictionary<string, TCallback>;
    &deprecated: Boolean;
    security: TArray<TSecurityRequirement>;
    servers: TArray<TServer>;

    class operator Initialize(out ARecord: TOperation);
    class operator Finalize(var ARecord: TOperation);
    class operator Assign(var ADest: TOperation; const [ref] ASource: TOperation);
  end;

  TPathItem = record
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
    servers: TArray<TServer>;
    parameters: TArray<TParameter>;
  end;

  TComponents = record
  end;

  TSecurity = record
  end;

  TTag = record
  end;

  TOpenAPI = record
    openapi: string; // required
    info: TInfo; // required
    servers: TArray<TServer>;
    paths: TDictionary<string, TPathItem>; // required
    components: TComponents;
    security: TArray<TSecurity>;
    tags: TArray<TTag>;
    externalDocs: TExternalDocumentation;

    class operator Initialize(out ARecord: TOpenAPI);
    class operator Finalize(var ARecord: TOpenAPI);
    class operator Assign(var ADest: TOpenAPI; const [ref] ASource: TOpenAPI);
  end;

implementation

{ TServer }

class operator TServer.Assign(var ADest: TServer; const [ref] ASource: TServer);
begin
  ADest.url := ASource.url;
  ADest.description := ASource.description;

  ADest.variables.Clear;
  for var LVar in ASource.variables do
    ADest.variables.Add(LVar.Key, LVar.Value);
end;

class operator TServer.Finalize(var ARecord: TServer);
begin
  ARecord.variables.Free;
  ARecord.variables := nil;
end;

class operator TServer.Initialize(out ARecord: TServer);
begin
  ARecord.url := '';
  ARecord.description := '';
  ARecord.variables := TDictionary<string, TServerVariable>.Create();
end;

{ TServerVariable }

constructor TServerVariable.Create(const AEnum: TArray<string>; const ADefault,
  ADescription: string);
begin
  enum := AEnum;
  default := ADefault;
  description := ADescription;
end;

{ TOperation }

class operator TOperation.Assign(var ADest: TOperation; const [ref] ASource: TOperation);
begin
  ADest.tags := ASource.tags;
  ADest.summary := ASource.summary;
  ADest.description := ASource.description;
  ADest.externalDocs := ASource.externalDocs;
  ADest.operationId := ASource.operationId;
  ADest.parameters := ASource.parameters;
  ADest.requestBody := ASource.requestBody;
  ADest.responses := ASource.responses;

  ADest.callbacks.Clear;
  for var LVar in ASource.callbacks do
    ADest.callbacks.Add(LVar.Key, LVar.Value);

  ADest.&deprecated := ASource.deprecated;
  ADest.security := ASource.security;
  ADest.servers := ASource.servers;
end;

class operator TOperation.Initialize(out ARecord: TOperation);
begin
  ARecord.callbacks := TDictionary<string, TCallback>.Create();
end;

class operator TOperation.Finalize(var ARecord: TOperation);
begin
  ARecord.callbacks.Free;
  ARecord.callbacks := nil;
end;

{ TOpenAPI }

class operator TOpenAPI.Assign(var ADest: TOpenAPI; const [ref] ASource: TOpenAPI);
begin
  ADest.openapi := ASource.openapi;
  ADest.info := ASource.info;
  ADest.servers := ASource.servers;

  ADest.paths.Clear;
  for var LVar in ASource.paths do
    ADest.paths.Add(LVar.Key, LVar.Value);

  ADest.components := ASource.components;
  ADest.security := ASource.security;
  ADest.tags := ASource.tags;
  ADest.externalDocs := ASource.externalDocs;
end;

class operator TOpenAPI.Initialize(out ARecord: TOpenAPI);
begin
  ARecord.paths := TDictionary<string, TPathItem>.Create();
end;

class operator TOpenAPI.Finalize(var ARecord: TOpenAPI);
begin
  ARecord.paths.Free;
  ARecord.paths := nil;
end;

end.
