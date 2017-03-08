unit Model.Utilities;

interface

uses
  Classes, SysUtils, System.Hash, Data.DB
;

type
  TModelUtilities = class
    class function GetPasswordHash(const APassword: string): string;
    class function GenerateUniqueCode(): string;
  end;


implementation

uses
  Rtti, TypInfo
;

class function TModelUtilities.GetPasswordHash(const APassword: string): string;
begin
  //TODO: add salt
//  Result := THashMD5.GetHashString(APassword);
  Result := '_' + APassword + '_';
end;

class function TModelUtilities.GenerateUniqueCode(): string;
begin
  Result := TGUID.NewGuid.ToString;
end;

end.
