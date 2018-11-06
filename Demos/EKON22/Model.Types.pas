unit Model.Types;

interface

uses
  Classes, SysUtils
, MARS.Rtti.Utils
;

type
  TMyRecord = record
    Firstname: string;
    Lastname: string;
    DateOfBirth: TDateTime;
    constructor Create(const AFirstname, ALastname: string; const ADateOfBirth: TDateTime);
    function ToString: string;
  end;

  TAge = record
    Name: string;
    Seconds: Integer;
    Minutes: Integer;
    Hours: Integer;
    Days: Integer;
    Months: Integer;
    Years: Integer;
    constructor Create(const AName: string; const ADateOfBirth: TDateTime);
    function ToString: string;
  end;

  TMyObject = class
  private
    FFirstname: string;
    FLastname: string;
    FDateOfBirth: TDateTime;
  public
    constructor Create(); overload;
    constructor Create(const AFirstname, ALastname: string; const ADateOfBirth: TDateTime); overload;
    function ToString: string; override;

    property Firstname: string read FFirstname write FFirstname;
    property Lastname: string read FLastname write FLastname;
    property DateOfBirth: TDateTime read FDateOfBirth write FDateOfBirth;
  end;

   TMyEmployeeRecord = record
    EMP_NO: Integer;
    FIRST_NAME: string;
    LAST_NAME: string;
    PHONE_EXT: string;
    HIRE_DATE: TDateTime;
   end;

implementation

uses DateUtils;

{ TMyRecord }

constructor TMyRecord.Create(const AFirstname, ALastname: string;
  const ADateOfBirth: TDateTime);
begin
  Firstname := AFirstname;
  Lastname := ALastname;
  DateOfBirth := ADateOfBirth;
end;

function TMyRecord.ToString: string;
begin
  Result := string.join(' ', [FirstName, LastName, FormatDateTime('ddddd', DateOfBirth)]);
end;

{ TMyObject }

constructor TMyObject.Create(const AFirstname, ALastname: string;
  const ADateOfBirth: TDateTime);
begin
  inherited Create;
  FFirstname := AFirstname;
  FLastname := ALastname;
  FDateOfBirth := ADateOfBirth;
end;

constructor TMyObject.Create;
begin
  inherited Create;
end;

function TMyObject.ToString: string;
begin
  Result := string.join(' ', [FirstName, LastName, FormatDateTime('ddddd', DateOfBirth)]);
end;

{ TAge }

constructor TAge.Create(const AName: string; const ADateOfBirth: TDateTime);
begin
  Name := AName;
  Seconds := SecondsBetween(ADateOfBirth, Now);
  Minutes := MinutesBetween(ADateOfBirth, Now);
  Hours := HoursBetween(ADateOfBirth, Now);
  Days := DaysBetween(ADateOfBirth, Now);
  Months := MonthsBetween(ADateOfBirth, Now);
  Years := YearsBetween(ADateOfBirth, Now);
end;

function TAge.ToString: string;
begin
  Result := Format('%s is %d years old.', [Name, Years]);
end;

end.
