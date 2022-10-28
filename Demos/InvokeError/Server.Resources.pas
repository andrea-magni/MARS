(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes, System.JSON
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.JSON, MARS.Core.Response
, MARS.Core.URL
//, MARS.Core.Token
, MARS.Core.Token.Resource
, MARS.OpenAPI.v3, MARS.Metadata.Attributes
, MARS.Core.RequestAndResponse.Interfaces
;

type
  ENumberTooBig = class(Exception);


  [Path('helloworld')]
  THelloWorldResource = class
  protected
    [Context] Response: IMARSResponse;
  public
    [InvokeError]
    function ErrorHandler(const AException: Exception): Boolean;

    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;

    [GET
     , Path('sumSmall/{firstNum}/{secondNum}')
     , Produces(TMediaType.TEXT_PLAIN)
     , MetaDescription('Beware: I can only sum numbers smaller than 100.')
    ]
    function SumSmall([PathParam] firstNum: Double; [PathParam] secondNum: Double): Double;
  end;

  [Path('openapi'), MetaVisible(False)]
  TOpenAPIResource = class
  protected
  public
    [GET, Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_YAML)]
    function GetOpenAPI([Context] AOpenAPI: TOpenAPI): TOpenAPI;
  end;

  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  end;

implementation

uses
    MARS.Core.Registry
;

{ THelloWorldResource }

function THelloWorldResource.ErrorHandler(const AException: Exception): Boolean;
var
  LObj: TJSONObject;
begin
  Result := False;
  if AException is ENumberTooBig then
  begin
    Response.StatusCode := 400;
    Response.ContentType := TMediaType.APPLICATION_JSON;
    LObj := TJSONObject.Create();
    try
      LObj.WriteStringValue('message', AException.Message);
      LObj.WriteStringValue('error', AException.ClassName);

      Response.Content := LObj.ToJSON;
    finally
      LObj.Free;
    end;

    Result := True;
  end;
end;

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello World!';
end;

function THelloWorldResource.SumSmall(firstNum, secondNum: Double): Double;
begin
  if (firstNum > 100) or (secondNum > 100) then
    raise ENumberTooBig.Create('Sum of numbers greater than 100 is not supported');

  Result := firstNum + secondNum;
end;

{ TOpenAPIResource }

function TOpenAPIResource.GetOpenAPI(AOpenAPI: TOpenAPI): TOpenAPI;
begin
  Result := AOpenAPI;
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResources([THelloWorldResource, TTokenResource, TOpenAPIResource]);

end.
