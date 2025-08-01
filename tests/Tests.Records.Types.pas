unit Tests.Records.Types;

interface

uses
  Classes, SysUtils, Generics.Collections
, System.Rtti
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

  TRecordWithObject = record
    Name: string;
    Instance: TObject;
    constructor Create(const AName: string; const AInstance: TObject);
  end;

  TRecordWithCustomDate = record
  public
    Date: TDateTime;

    function ToJSONFilter(AMember: TRttiMember; AJSON: TJSONObject): Boolean;
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

{ TRecordWithObject }

constructor TRecordWithObject.Create(const AName: string;
  const AInstance: TObject);
begin
  Name := AName;
  Instance := AInstance;
end;


{ TRecordWithCustomDate }

function TRecordWithCustomDate.ToJSONFilter(AMember: TRttiMember;
  AJSON: TJSONObject): Boolean;
const
  CUSTOM_DATETIME_FORMAT = 'yyyy-mm-dd hh:nn.ss';
begin
  Result := True;
  if (AMember is TRttiDataMember) then
  begin
    var LDataMember := AMember as TRttiDataMember;
    if (LDataMember.DataType.Name = 'TDateTime') or (LDataMember.DataType.Name = 'TDate') then
    begin
      var LDateValue := LDataMember.GetValue(@Self).AsType<TDateTime>;

      var LJSONValue := '';
      if LDateValue <> 0 then
        LJSONValue := FormatDateTime(CUSTOM_DATETIME_FORMAT, LDateValue);

      var LJSONName := AMember.Name;
      var LJSONNameAttribute := AMember.GetAttribute<JSONNameAttribute>;
      if Assigned(LJSONNameAttribute) then
        LJSONName := LJSONNameAttribute.Name;

      AJSON.WriteStringValue(AMember.Name, LJSONValue);
    end;
  end;
end;

end.
