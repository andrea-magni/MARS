unit Model;

interface

uses
   Classes, SysUtils, Data.DB, Rtti
 , MARS.Core.JSON
;

type
  TToDoItem = record
    Id: Integer;
    Owner_Id: Integer;
    Text: string;
    Creation_Date: TDateTime;
    Done_Date: TDateTime;
    Due_Date: TDateTime;
    Last_Update: TDateTime;
  end;

  TAccount = record
    Id: Integer;
    Username: string;
    First_Name: string;
    Last_Name: string;
    Active: Boolean;
    Roles: string;
    Pwd_Hash: string;
    Reset_Code: string;
    Last_Login: TDateTime;
    Creation_Date: TDateTime;
    Last_Update: TDateTime;
    function ToJSONFilter(const AField: TRttiField; const AObj: TJSONObject): Boolean;
  end;

implementation

uses
    MARS.Rtti.Utils
  , Model.Utilities
;

{ TAccount }

function TAccount.ToJSONFilter(const AField: TRttiField;
  const AObj: TJSONObject): Boolean;
begin
  Result := True;
  if SameText(AField.Name, 'Pwd_Hash') or SameText(AField.Name, 'Reset_Code') then
    Result := False;
end;

end.
