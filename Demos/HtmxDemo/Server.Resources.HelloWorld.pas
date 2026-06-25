(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Resources.HelloWorld;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.URL
, MARS.Core.JSON, MARS.Core.Response
, MARS.WebServer.Resources
//, MARS.Core.Token
, MARS.OpenAPI.v3
;

type
  [ Path('web')
  , RootFolder('.\www', True)
  , OAPIDescription('Serves static content from the www folder.')
  ]
  TStaticContentResource = class(TFileSystemResource)
  end;

  TEndpoint = record
    path: string;
    methods: TArray<string>;
    summary: string;
    description: string;
    parameterCount: Integer;
    constructor Create(const APath: string; const AMethods: TArray<string>);
  end;

  TDataResponse = record
    endpoints: TArray<TEndpoint>;
  end;

  [Path('helloworld')]
  THelloworldResource = class
  public
    [GET
    , OAPIDescription('Retrieves the Hello world string with current timestamp')
    ]
    function Retrieve: string;

    [GET, Path('data')
    , OAPISummary('All paths available in this API will be listed here as they are in the OpenAPI documentation.')
    , OAPIDescription('Retrieves all endpoints avaible in my API')
    ]
    function RetrieveData([Context] AOpenAPI: TOpenAPI): TDataResponse;
  end;

implementation

uses
  MARS.Core.Registry
;

{ THelloworldResource }

function THelloworldResource.Retrieve: string;
begin
  Result := 'Hello, World! Current time is: ' + FormatDateTime('hh:nn:ss.zzz', Now);
end;

function THelloworldResource.RetrieveData(
  AOpenAPI: TOpenAPI): TDataResponse;
begin
  Result := Default(TDataResponse);

  for var LPath in AOpenAPI.paths do
  begin
    const LName = LPath.Key;
    const LValue = LPath.Value;
    var LEndpoint := TEndpoint.Create(LName, LValue.Methods);
    LEndpoint.summary := LValue.summary;
    LEndpoint.description := LValue.description;

    var LParameters := LValue.parameters.ToArray;
    LEndPoint.parameterCount := Length(LParameters);

    Result.endpoints := Result.endpoints + [LEndpoint];
  end;
end;

{ TEndpoint }

constructor TEndpoint.Create(const APath: string; const AMethods: TArray<string>);
begin
  path := APath;
  methods := AMethods;
  summary := '';
  description := '';
  parameterCount := 0;
end;

initialization
  MARSRegister([TStaticContentResource, THelloWorldResource]);

end.
