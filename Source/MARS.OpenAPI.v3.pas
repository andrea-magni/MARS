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
  end;

  TStringMap<T> = record
  end;


  TServer = record
    url: string; // required
    description: string;
    variables: TStringMap<TServerVariable>;
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

end.
