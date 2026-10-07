unit Tests.DefaultEngine.Resources;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.URL
, MARS.Core.JSON, MARS.Core.Response
, MARS.WebServer.Resources
//, MARS.Core.Token
, MARS.OpenAPI.v3
, MARS.Metadata.Attributes, MARS.Core.Token.Resource
;

type
  TJSONOptionsRecord = record
    name: string;
    note: string; // left empty: shows the effect of JSON.SkipEmptyStrings
  end;

  TDateRecord = record
    when: TDateTime;
  end;

  // JSON serialization options from the application parameters (JSON.*)
  TOpenAPIPayload = record
    Name: string;
    Quantity: Integer;
  end;

  // request bodies without [Consumes]: documented anyway in the OpenAPI document
  [Path('openapibody')]
  TOpenAPIBodyResource = class
  public
    [POST]
    function Elabora([BodyParam] APayload: TOpenAPIPayload): Integer;

    [POST, Path('text')]
    function Text([BodyParam] AText: string): string;

    [POST, Path('form')]
    function Form([FormParam('name')] AName: string): string;

    [GET, Path('nobody')]
    function NoBody: string;

    // body read by the method itself, documented with [MetaRequestBody]
    [POST, Path('manualform'), Consumes(TMediaType.APPLICATION_FORM_URLENCODED_TYPE)
    , MetaRequestBody('Tests.DefaultEngine.Resources.TOpenAPIPayload', 'The payload')]
    function ManualForm: string;

    [POST, Path('manualjson'), MetaRequestBody('Tests.DefaultEngine.Resources.TOpenAPIPayload')]
    function ManualJSON: string;

    [POST, Path('manualwrong'), MetaRequestBody('No.Such.Type')]
    function ManualWrong: string;
  end;

  [Path('token')]
  TTestTokenResource = class(TMARSTokenResource);

  [Path('jsonoptions')]
  TJSONOptionsResource = class
  public
    [GET, Produces(TMediaType.APPLICATION_JSON)]
    function GetRecord: TJSONOptionsRecord;

    [GET, Path('skip'), Produces(TMediaType.APPLICATION_JSON), JSONSkipEmptyValues]
    function GetRecordSkip: TJSONOptionsRecord;

    // the reader uses the same options: JSON.DateIsUTC decides how "...Z" dates are read
    [POST, Path('hour'), Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.TEXT_PLAIN)]
    function PostHour([BodyParam] const AData: TDateRecord): string;
  end;

  // a JSON response with non-ASCII text (issue #208, JSON.EscapeNonASCII)
  [Path('unicodejson')]
  TUnicodeJSONResource = class
  public
    [GET, Produces(TMediaType.APPLICATION_JSON)]
    function GetContent: TJSONObject;
  end;

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

  // catch-all resource (SPA scenario: serve index.html for client-side routes)
  [Path('{*}')]
  TCatchAllResource = class
  private
  protected
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function GetContent: string;
  end;

  // specific sibling of the catch-all: must win over '{*}' on /images/* URLs
  [Path('images/{*}')]
  TImagesResource = class
  private
  protected
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function GetContent: string;
  end;

  // static files rooted at the test executable folder (files are created by the tests)
  [Path('static/{*}'), RootFolder('{bin}', False)]
  TStaticResource = class(TFileSystemResource)
  end;

  // same root, subfolders allowed
  [Path('statictree/{*}'), RootFolder('{bin}', True)]
  TStaticTreeResource = class(TFileSystemResource)
  end;

  // same root, no directory listing
  [Path('staticnolist/{*}'), RootFolder('{bin}', True), DirectoryListing(False)]
  TStaticNoListResource = class(TFileSystemResource)
  end;

  // same root, subfolders and dot-segments allowed (still confined to the root)
  [Path('staticdots/{*}'), RootFolder('{bin}', True), DotSegments]
  TStaticDotsResource = class(TFileSystemResource)
  end;

  // dot-segments allowed, root folder only
  [Path('staticdotsflat/{*}'), RootFolder('{bin}', False), DotSegments]
  TStaticDotsFlatResource = class(TFileSystemResource)
  end;

  // same root, files matching the mask are never served
  [Path('staticexclude/{*}'), RootFolder('{bin}', False), Exclude('*.secret')]
  TStaticExcludeResource = class(TFileSystemResource)
  end;

  TItem = record
    Id: Integer;
    Description: string;
  end;

  [Path('required'), Produces(TMediaType.TEXT_PLAIN)]
  TRequiredResource = class
  public
    [GET]
    function GetQuery([QueryParam, Required] name: string): string;

    [GET, Path('header')]
    function GetHeader([HeaderParam('X-Name'), Required] name: string): string;

    [POST, Consumes(TMediaType.APPLICATION_JSON)]
    function PostBody([BodyParam, Required] const AItem: TItem): string;
  end;

  [Path('item'), Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
  TItemResource = class
  private
  protected
  public
    [POST]
    function ConsumeAll([BodyParam] const AData: TArray<TItem>): Integer;

    [POST, Path('/single')]
    function ConsumeOne([BodyParam] const AItem: TItem): Integer;

    [GET, Path('/{id}')]
    function Retrieve([PathParam] id: Integer): TItem;

    [GET]
    function RetrieveAll: TArray<TItem>;

    // HTTP QUERY: safe request whose query is carried in the body
    [QUERY]
    function Search([BodyParam] const AFilter: TItem): TArray<TItem>;
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

{ TCatchAllResource }

function TCatchAllResource.GetContent: string;
begin
  Result := 'catch-all';
end;

{ TImagesResource }

function TImagesResource.GetContent: string;
begin
  Result := 'images';
end;

{ THelloWorldResource }

{ TOpenAPIBodyResource }

function TOpenAPIBodyResource.Elabora(APayload: TOpenAPIPayload): Integer;
begin
  Result := APayload.Quantity;
end;

function TOpenAPIBodyResource.Form(AName: string): string;
begin
  Result := AName;
end;

function TOpenAPIBodyResource.ManualForm: string;
begin
  Result := '';
end;

function TOpenAPIBodyResource.ManualJSON: string;
begin
  Result := '';
end;

function TOpenAPIBodyResource.ManualWrong: string;
begin
  Result := '';
end;

function TOpenAPIBodyResource.NoBody: string;
begin
  Result := '';
end;

function TOpenAPIBodyResource.Text(AText: string): string;
begin
  Result := AText;
end;

{ TJSONOptionsResource }

function TJSONOptionsResource.GetRecord: TJSONOptionsRecord;
begin
  Result.name := 'MARS';
  Result.note := '';
end;

function TJSONOptionsResource.GetRecordSkip: TJSONOptionsRecord;
begin
  Result := GetRecord;
end;

function TJSONOptionsResource.PostHour(const AData: TDateRecord): string;
begin
  Result := FormatDateTime('hh', AData.when);
end;

{ TUnicodeJSONResource }

function TUnicodeJSONResource.GetContent: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('name', #$0413#$0430#$0440#$0434#$0435#$0440#$043E); // Cyrillic
end;

function THelloWorldResource.GetContent: string;
begin
  Result := 'Hello, world!';
end;

{ TItemResource }

function TItemResource.ConsumeAll(const AData: TArray<TItem>): Integer;
begin
  Result := Length(AData);
end;

function TItemResource.ConsumeOne(const AItem: TItem): Integer;
begin
  Result := AItem.Id;
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

function TItemResource.Search(const AFilter: TItem): TArray<TItem>;
begin
  Result := [];
  for var LItem in RetrieveAll do
    if LItem.Description.Contains(AFilter.Description) then
      Result := Result + [LItem];
end;

{ TRequiredResource }

function TRequiredResource.GetQuery(name: string): string;
begin
  Result := 'query ' + name;
end;

function TRequiredResource.GetHeader(name: string): string;
begin
  Result := 'header ' + name;
end;

function TRequiredResource.PostBody(const AItem: TItem): string;
begin
  Result := 'body ' + AItem.Description;
end;

initialization
  MARSRegister([TRequiredResource]);
  MARSRegister([THelloWorldResource, TUnicodeJSONResource, TJSONOptionsResource, TOpenAPIBodyResource, TTestTokenResource, TWildcardResource, TItemResource
  , TCatchAllResource, TImagesResource, TStaticResource, TStaticTreeResource, TStaticNoListResource
  , TStaticDotsResource, TStaticDotsFlatResource, TStaticExcludeResource]);

end.
