unit Model;

interface

uses
  Classes, SysUtils, Data.DB
;

type
  TToDoItem = record
    Id: Integer;
    Owner: string;
    Text: string;
    Creation_Date: TDateTime;
    Done_Date: TDateTime;
    Due_Date: TDateTime;
    Last_Update: TDateTime;
  end;

  TToDoItems = TArray<TToDoItem>;

  TToDoItemHelper = class helper for TDataSet
    function ToToDoItems: TTodoItems;
  end;

  TAccount = record
    Id: Integer;
    Username: string;
    First_Name: string;
    Last_Name: string;
    Activated: Boolean;
    Roles: string;
    Pwd_Hash: string;
    Reset_Code: string;
    Last_Login: TDateTime;
    Creation_Date: TDateTime;
    Last_Update: TDateTime;
  end;

implementation

uses
    MARS.Rtti.Utils
  , Model.Utilities
;

{ TToDoItemHelper }

function TToDoItemHelper.ToToDoItems: TTodoItems;
var
  LItem: TToDoItem;
begin
  if not Self.Active then
    Self.Active := True
  else
    Self.First;
  Result := [];
  while not Self.Eof do
  begin
    TRecord<TToDoItem>.FromDataSet(LItem, Self);
    Result := Result + [LItem];
    Self.Next;
  end;
end;


end.
