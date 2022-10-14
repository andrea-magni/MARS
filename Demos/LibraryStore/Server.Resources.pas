(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes, Generics.Collections
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.JSON, MARS.Core.Response
, MARS.Core.URL
, MARS.Core.Token, MARS.Utils.JWT
, MARS.OpenAPI.v3, MARS.Metadata.Attributes

, Model, Storage
, MARS.Core.Exceptions
;

type
  [ Path('book')
  , Produces(TMediaType.APPLICATION_JSON), Consumes(TMediaType.APPLICATION_JSON)
//  , Produces(TMediaType.APPLICATION_YAML), Consumes(TMediaType.APPLICATION_YAML)
//  , Produces(TMediaType.TEXT_PLAIN)
  ]
  TBookResource = class
  protected
  public
    [GET, Path('all')]
    function RetrieveAll: TArray<TBook>;

    [GET, Path('recentlyAdded')]
    function RetrieveRecentlyAdded: TArray<TBook>;


    [GET, Path('byAuthor/{authorId}')]
    function RetrieveByAuthor([PathParam] authorId: Integer): TArray<TBook>;

    [GET, Path('{id}')]
    function Retrieve([PathParam] id: Integer): TBook;

    [POST, RolesAllowed('manager')]
    function Add([BodyParam] book: TBook): TBook;
  end;

  [ Path('author')
  , Produces(TMediaType.APPLICATION_JSON), Consumes(TMediaType.APPLICATION_JSON)
  ]
  TAuthorResource = class
  protected
  public
    [GET, Path('all')]
    function RetrieveAll: TArray<TAuthor>;

    [GET, Path('{id}')]
    function Retrieve([PathParam] id: Integer): TAuthor;

    [GET, Path('{id}/books')]
    function RetrieveBooks([PathParam] id: Integer): TArray<TBook>;
  end;

// -----------------------------------------------------------------------------

  [ Path('token')
  , Produces(TMediaType.APPLICATION_JSON), Consumes(TMediaType.APPLICATION_JSON)]
  TTokenResource = class
  public
    type TCredentialsData = record
      userName: string;
      password: string;
      other: string;
    end;
  private
  protected
    [Context] FToken: TMARSToken;
    [Context, ApplicationParam(JWT_SECRET_PARAM)] FSecret: string;
    [Context] FURL: TMARSURL;
  public
    [GET, IsReference]
    function Retrieve: TMARSToken;

    [POST, IsReference]
    function Authenticate([BodyParam] const ACredentials: TCredentialsData): TMARSToken;

    [DELETE, IsReference]
    function Logout: TMARSToken;
  end;

// -----------------------------------------------------------------------------

  [Path('openapi'), MetaVisible(False)]
  TOpenAPIResource = class
  protected
  public
    [GET, Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_YAML)]
    function GetOpenAPI([Context] AOpenAPI: TOpenAPI): TOpenAPI;
  end;

implementation

uses
    MARS.Core.Registry
;

{ TOpenAPIResource }

function TOpenAPIResource.GetOpenAPI(AOpenAPI: TOpenAPI): TOpenAPI;
begin
  Result := AOpenAPI;
end;

{ TBookResource }

function TBookResource.RetrieveAll: TArray<TBook>;
begin
  Result := S.BooksArray;
end;

function TBookResource.RetrieveByAuthor(authorId: Integer): TArray<TBook>;
begin
  Result := [];
  for var LBook in S.BooksArray do
    if LBook.Author.id = authorId then
      Result := Result + [LBook];
end;

function TBookResource.RetrieveRecentlyAdded: TArray<TBook>;
begin
  Result := S.BooksArray(
    function (b: TBook): Boolean
    begin
      Result := (Now - b.AddedAt) <= 1; // last 24 hours
    end
  );
end;

function TBookResource.Add(book: TBook): TBook;
begin
  if book.id = 0 then
    raise Exception.Create('id cannot be zero');

  Result := book;
  Result.AddedAt := Now;
  S.Books.Add(Result);
end;

function TBookResource.Retrieve(id: Integer): TBook;
begin
  for var LBook in S.BooksArray do
    if LBook.id = id then
      Exit(LBook);

  raise EMARSHttpException.CreateFmt('Book %d not found', [id], 404);
end;

{ TAuthorResource }

function TAuthorResource.Retrieve(id: Integer): TAuthor;
begin
  for var LAuthor in S.AuthorsArray do
    if LAuthor.id = id then
      Exit(LAuthor);

  raise EMARSHttpException.CreateFmt('Author %d not found', [id], 404);
end;

function TAuthorResource.RetrieveAll: TArray<TAuthor>;
begin
  Result := S.AuthorsArray;
end;

function TAuthorResource.RetrieveBooks(id: Integer): TArray<TBook>;
begin
  var LBookResource := TBookResource.Create;
  try
    Result := LBookResource.RetrieveByAuthor(id);
  finally
    LBookResource.Free;
  end;
end;

{ TTokenResource }

function TTokenResource.Authenticate(
  const ACredentials: TCredentialsData): TMARSToken;
const
  ADMIN_USER = 'admin';
begin
  if ACredentials.userName = '' then
    raise EMARSHttpException.Create('Invalid userName', 501);


  if SameText(ACredentials.userName, ADMIN_USER) then
  begin
    FToken.UserName := ADMIN_USER;
    FToken.Roles := ['manager'];
    FToken.Build(FSecret);
    Result := FToken;
  end
  else
  begin
    FToken.Clear; // empty FToken
    Result := FToken;
  end;
end;

function TTokenResource.Logout: TMARSToken;
begin
  FToken.Clear;
  Result := FToken;
end;

function TTokenResource.Retrieve: TMARSToken;
begin
  Result := FToken;
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResources(
    [TBookResource, TAuthorResource, TTokenResource]
  + [TOpenAPIResource]
  );

end.
