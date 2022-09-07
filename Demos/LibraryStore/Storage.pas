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

end.
