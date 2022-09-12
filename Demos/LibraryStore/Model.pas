unit Model;

interface

uses
  Classes, SysUtils;

type
  TAuthor = record
    id: Integer;
    Name: string;
    Surname: string;
    Nationality: string;
    Email: string;
  end;

  TBook = record
    id: Integer;
    Author: TAuthor;
    Title: string;
    Description: string;
    Year: Integer;
    PublicationDate: TDateTime;
    AddedAt: TDateTime;
  end;


implementation

end.
