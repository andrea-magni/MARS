unit Tests.DefaultEngine.Resources;

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
  [Path('helloworld')]
  THelloWorldResource = class
  private
  protected
  public
    [GET]
    function GetContent: string;
  end;

  [Path('wildcard/{*}')]
  TWildcardResource = class
  private
  protected
  public
    [GET, Produces(TMediaType.TEXT_HTML)]
    function GetContent: string;
  end;

  TItem = record
    Id: Integer;
    Description: string;
  end;

  [Path('item'), Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
  TItemResource = class
  private
  protected
  public
    [POST]
    function ConsumeAll([BodyParam] const AData: TArray<TItem>): Integer;

    [GET, Path('/{id}')]
    function Retrieve([PathParam] id: Integer): TItem;

    [GET]
    function RetrieveAll: TArray<TItem>;
  end;


implementation

uses
  MARS.Core.Registry
;

{ TWildcardResource }

function TWildcardResource.GetContent: string;
begin
  Result :=
    '''
    <html>
      <head></head>
      <body>
        <h1>It works!</h1>
      </body>
    </html>
    ''';
end;

{ THelloWorldResource }

function THelloWorldResource.GetContent: string;
begin
  Result := 'Hello, world!';
end;

{ TItemResource }

function TItemResource.ConsumeAll(const AData: TArray<TItem>): Integer;
begin
  Result := Length(AData);
end;

function TItemResource.Retrieve(id: Integer): TItem;
begin
  Result := Default(TItem);
  Result.Id := id;
  Result.Description := 'Item #' + id.ToString;
end;

function TItemResource.RetrieveAll: TArray<TItem>;
begin
  var LItem1: TItem := Default(TItem);
  LItem1.Id := 1;
  LItem1.Description := 'Item #1';
  Result := [
    LItem1
  ];
end;

initialization
  MARSRegister([THelloWorldResource, TWildcardResource, TItemResource]);

end.
