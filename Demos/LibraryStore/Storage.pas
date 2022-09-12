unit Storage;

interface

uses
  Classes, SysUtils, Generics.Collections
, Model;

type
  TStorage = class
  private
    class var _Instance: TStorage;
  protected
  public
    Books: TThreadList<TBook>;
    function BooksArray(const AFilter: TPredicate<TBook> = nil): TArray<TBook>;
    function AuthorsArray: TArray<TAuthor>;

    procedure PrefillBooks;

    constructor Create; virtual;
    destructor Destroy; override;
  public
    class function Instance: TStorage;
    class destructor ClassDestroy;
  end;

  function S: TStorage; // shortcut

implementation


function S: TStorage;
begin
  Result := TStorage.Instance;
end;

{ TStorage }

function TStorage.AuthorsArray: TArray<TAuthor>;
var
  LBooks: TArray<TBook>;
begin
  Result := [];

  LBooks := BooksArray;
  for var LBook in LBooks do
  begin
    var LAuthor := LBook.Author;

    var LFound := False;
    for var LTempAuthor in Result do
      if LAuthor.id = LTempAuthor.id then
      begin
        LFound := True;
        Break;
      end;

    if not LFound then
      Result := Result + [LAuthor];
  end;
end;

function TStorage.BooksArray(const AFilter: TPredicate<TBook> = nil): TArray<TBook>;
var
  LBooks: TList<TBook>;
begin
  LBooks := Books.LockList;
  try
    if Assigned(AFilter) then
    begin
      Result := [];
      for var LBook in LBooks do
      begin
        if AFilter(LBook) then
          Result := Result + [LBook];
      end;
    end
    else
      Result := LBooks.ToArray;
  finally
    Books.UnlockList;
  end;
end;

class destructor TStorage.ClassDestroy;
begin
  if Assigned(_Instance) then
    _Instance.Free;
end;

constructor TStorage.Create;
begin
  inherited Create;
  Books := TThreadList<TBook>.Create;
end;

destructor TStorage.Destroy;
begin
  Books.Free;
  inherited;
end;

class function TStorage.Instance: TStorage;
begin
  if not Assigned(_Instance) then
    _Instance := TStorage.Create;
  Result := _Instance;
end;

procedure TStorage.PrefillBooks;
var LBook: TBook;
begin
  // 1 Andrea Magni, Delphi GUI programming with Firemonkey
  LBook.Author.id := 1;
  LBook.Author.Name := 'Andrea';
  LBook.Author.SurName := 'Magni';
  LBook.Author.Nationality := 'IT';
  LBook.Author.Email := 'andrea.magni@gmail.com';
  LBook.id := 1;
  LBook.Title := 'Delphi GUI programming with Firemonkey';
  LBook.Description := 'Unleash the full potential of the FMX framework to build exciting cross-platform apps with Embarcadero Delphi';
  LBook.Year := 2020;
  LBook.PublicationDate := EncodeDate(2020, 10, 29);
  LBook.AddedAt := IncMonth(LBook.PublicationDate, 6);
  Books.Add(LBook);

  // 2 Cantù, OPH
  LBook.Author.id := 2;
  LBook.Author.Name := 'Marco';
  LBook.Author.SurName := 'Cantù';
  LBook.Author.Nationality := 'IT';
  LBook.Author.Email := 'marco.cantu@embarcadero.com';
  LBook.id := 2;
  LBook.Title := 'Object Pascal Handbook';
  LBook.Description := 'The Object Pascal Handbook is the complete guide to the programming language of Delphi and AppMethod, by Embarcadero Technologies.';
  LBook.Year := 2015;
  LBook.PublicationDate := EncodeDate(2015, 8, 5);
  LBook.AddedAt := IncMonth(LBook.PublicationDate, 6);
  Books.Add(LBook);

  // 3 Cantù, Mastering Delphi 2
  LBook.Author.id := 2;
  LBook.Author.Name := 'Marco';
  LBook.Author.SurName := 'Cantù';
  LBook.Author.Nationality := 'IT';
  LBook.Author.Email := 'marco.cantu@embarcadero.com';
  LBook.id := 3;
  LBook.Title := 'Mastering Delphi 2 for Windows 95';
  LBook.Description := 'Mastering Delphi 2 is a comprehensive guide to Delphi programming that focuses entirely on the new 32-bit version.';
  LBook.Year := 1996;
  LBook.PublicationDate := EncodeDate(1996, 2, 1);
  LBook.AddedAt := IncMonth(LBook.PublicationDate, 6);
  Books.Add(LBook);
end;

end.
