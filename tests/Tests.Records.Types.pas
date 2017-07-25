unit Tests.Records.Types;

interface

uses
  Classes, SysUtils, Generics.Collections
;

type
  TNamedIntegerRecord=record
    Name: string;
    Value: Integer;
    constructor Create(const AName: string; const AValue: Integer);
  end;

  TArrayNamedIntegerRecord=record
    Name: string;
    Data: TArray<TNamedIntegerRecord>;
  end;

  TOuterRecord=record
    Name: string;
    Inner: TNamedIntegerRecord;
    constructor Create(const AName: string; const AInnerName: string; const AInnerValue: Integer);
  end;

  TPersonRecord=record
    Name: string;
    Surname: string;
    DateOfBirth: TDate;
    function Age: Integer;
    constructor Create(const AName, ASurname: string; const ADateOfBirth: TDate);
  end;

  TPrimitiveTypesRecord=record
    AString: string;
    ABoolean: Boolean;
    AInteger: Integer;
    AFloat: Double;
    ACurrency: Currency;
    ADate: TDateTime;
    AChar: Char;
  end;


implementation

uses
  DateUtils
;

{ TNamedIntegerRecord }

constructor TNamedIntegerRecord.Create(const AName: string;
  const AValue: Integer);
begin
  Name := AName;
  Value := AValue;
end;

{ TOuterRecord }

constructor TOuterRecord.Create(const AName, AInnerName: string;
  const AInnerValue: Integer);
begin
  Name := AName;
  Inner.Name := AInnerName;
  Inner.Value := AInnerValue;
end;

{ TPersonRecord }

function TPersonRecord.Age: Integer;
begin
  Result := YearsBetween(DateOfBirth, Now);
end;

constructor TPersonRecord.Create(const AName, ASurname: string;
  const ADateOfBirth: TDate);
begin
  Name := AName;
  Surname := ASurname;
  DateOfBirth := ADateOfBirth;
end;


end.
