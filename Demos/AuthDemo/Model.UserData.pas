unit Model.UserData;

interface

uses Classes, SysUtils, Generics.Collections, System.Rtti
, MARS.Core.JSON;

type
  TUserData = record
  public
    displayName: string;
    username: string;
    password: string;
    passwordHash: string;
    roles: TArray<string>;
    createdAt: TDateTime;
    lastUpdateAt: TDateTime;

    function IsEmpty(): Boolean;
    function passwordHashMatches(const APassword: string): Boolean;
    function ToJSONFilter(const AMember: TRttiMember; const AObj: TJSONObject): Boolean;
    constructor Create(Ausername: string; const Apassword: string; const Aroles: TArray<string>; const AdisplayName: string);
    class function Empty(): TUserData; static;
  end;

  TUserDataStorage = class
  private
    FData: TDictionary<string, TUserData>;
    class var _Instance: TUserDataStorage;
  protected
  public
    function Retrieve(const AUserName: string; out AUserData: TUserData): Boolean;
    function Store(const AUserData: TUserData): TUserData;
    function Delete(const AUserName: string): Boolean;
    function RetrieveAll(): TArray<TUserData>;

    constructor Create; virtual;
    destructor Destroy; override;
  public
    class function Instance: TUserDataStorage;
    class destructor ClassDestroy;
  end;


implementation

uses
  StrUtils, System.Hash
;

function SimpleSalt(const ASource: string): string;
const SALT_STRING = 'MARS-Curiosity';
begin
  if ASource.Length < 3 then
    Result := ASource
  else
    Result := ASource.Substring(0, 2) + SALT_STRING + ASource.Substring(2);

  Result := SALT_STRING + Result + SALT_STRING;
  Result := Result + Result + Result;
end;

function SaltAndHash(const AString: string): string;
begin
  Result := THashSHA2.GetHashString(SimpleSalt(AString));
end;

{ TUserData }

constructor TUserData.Create(Ausername: string; const Apassword: string; const Aroles: TArray<string>; const AdisplayName: string);
begin
  displayName := AdisplayName;
  username := Ausername;
  password := Apassword;
  passwordHash := SaltAndHash(password);
  roles := Aroles;
  createdAt := Now;
  lastUpdateAt := createdAt;
end;

class function TUserData.Empty: TUserData;
begin
  Result := TUserData.Create('', '', [], '');
end;

function TUserData.IsEmpty: Boolean;
begin
  Result := username.IsEmpty;
end;

function TUserData.passwordHashMatches(const APassword: string): Boolean;
begin
  Result := SaltAndHash(APassword) = passwordHash;
end;

function TUserData.ToJSONFilter(const AMember: TRttiMember;
  const AObj: TJSONObject): Boolean;
begin
  // do not serialize password and passwordHash fields to JSON
  if IndexStr(AMember.Name, ['password', 'passwordHash']) > -1 then
    Result := False
  else
    Result := True;
end;

{ TUserDataStorage }

class destructor TUserDataStorage.ClassDestroy;
begin
  if Assigned(_Instance) then
    FreeAndNil(_Instance);
end;

constructor TUserDataStorage.Create;
begin
  inherited Create;
  FData := TDictionary<string, TUserData>.Create;
end;

function TUserDataStorage.Delete(const AUserName: string): Boolean;
begin
  Result := FData.ContainsKey(AUserName);
  if Result then
    FData.Remove(AUserName);
end;

destructor TUserDataStorage.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

class function TUserDataStorage.Instance: TUserDataStorage;
begin
  if not Assigned(_Instance) then
    _Instance := TUserDataStorage.Create;
  Result := _Instance;
end;

function TUserDataStorage.Retrieve(const AUserName: string;
  out AUserData: TUserData): Boolean;
var
  LKeys: TArray<string>;
begin
  LKeys := FData.Keys.ToArray;

  Result := False;
  for var LKey in LKeys do
  begin
    if SameText(LKey, AUserName) then
    begin
      Result := True;
      AUserData := FData[LKey];
    end;
  end;
end;

function TUserDataStorage.RetrieveAll: TArray<TUserData>;
begin
  Result := FData.Values.ToArray;
end;

function TUserDataStorage.Store(const AUserData: TUserData): TUserData;
var
  LUserData: TUserData;
  LFound: Boolean;
begin
  LFound := FData.TryGetValue(AUserData.username, LUserData);

  Result := AUserData;

  Result.passwordHash := SaltAndHash(Result.password);
  Result.password := ''; // no need to store password here

  if LFound then
  begin
    Result.createdAt := LUserData.createdAt;
    Result.lastUpdateAt := Now;
  end
  else
  begin
    Result.createdAt := Now;
    Result.lastUpdateAt := Result.createdAt;
  end;

  FData.AddOrSetValue(AUserData.username, Result);
end;

end.
