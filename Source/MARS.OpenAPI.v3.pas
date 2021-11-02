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

  TPaths = record
  end;

  TComponents = record
  end;

  TSecurity = record
  end;

  TTag = record
  end;

  TExternalDocumentation = record
  end;

  TOpenAPI = record
    openapi: string; // required
    info: TInfo; // required
    servers: TArray<TServer>;
    paths: TPaths; // required
    components: TComponents;
    security: TArray<TSecurity>;
    tags: TArray<TTag>;
    externalDocs: TExternalDocumentation;
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

end.
