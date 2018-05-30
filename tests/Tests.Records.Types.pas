unit Tests.Records.Types;

interface

uses
  Classes, SysUtils, Generics.Collections
, MARS.Core.JSON
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

  TSwingNamesRecord = record
    [JSONName('Surname')]
    Name: string;
    [JSONName('Name')]
    Surname: string;
    constructor Create(const AName, ASurname: string);
  end;

  TKeepTrackOfValuesRecord = record
    _AssignedValues: TArray<string>;
    Name: string;
    Surname: string;
    Age: Integer;
    PlaceOfBirth: string;
    Mistery: TNamedIntegerRecord;
  end;

  TVariantsRecord = record
    Value1: Variant;
    Value2: Variant;
    Value3: Variant;
    Value4: Variant;
    Value5: Variant;
    Value6: Variant;
    Value7: Variant;
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


{ TSwingNamesRecord }

constructor TSwingNamesRecord.Create(const AName, ASurname: string);
begin
  Name := AName;
  Surname := ASurname;
end;

end.
