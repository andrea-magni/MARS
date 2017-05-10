unit Model.Utilities;

interface

uses
  Classes, SysUtils, Data.DB
;

type
  TModelUtilities = class
    class function GetPasswordHash(const APassword: string): string;
    class function GenerateUniqueCode(): string;
  end;


implementation

uses
  Rtti, TypInfo, System.Hash
;

class function TModelUtilities.GetPasswordHash(const APassword: string): string;
begin
  Result := THashMD5.GetHashString(APassword + '$'); // TODO: improve salting
end;

class function TModelUtilities.GenerateUniqueCode(): string;
begin
  Result := TGUID.NewGuid.ToString;
end;

end.
