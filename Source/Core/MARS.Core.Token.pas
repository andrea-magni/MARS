(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit MARS.Core.Token;

{$I MARS.inc}

interface

uses
    SysUtils
  , Classes
  , Generics.Collections
  , SyncObjs
  , MARS.Core.JSON
  , Rtti

  , HTTPApp
  , IdGlobal
  , MARS.Core.Singleton
  ;

type
  TMARSToken = class
  private
    FToken: string;
    FUserName: string;
    FAuthenticated: Boolean;
    FRequestCount: Integer;
    FUserRoles: TStringList;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FLastRequest: string;
  public
    constructor Create(const AToken: string); virtual;
    destructor Destroy; override;

    procedure IncRequestCount;
    procedure SetUserNameAndRoles(const AUserName: string; const ARoles: TArray<string>); virtual;
    function UserHasRole(const ARole: string): Boolean; virtual;
    function ToJSON: TJSONObject; virtual;
    function ToJSONString: string;

    property UserRoles: TStringList read FUserRoles;
    property UserName: string read FUserName write FUserName;
    property Authenticated: Boolean read FAuthenticated write FAuthenticated;
    property RequestCount: Integer read FRequestCount write FRequestCount;
    property StartTime: TDateTime read FStartTime;
    property EndTime: TDateTime read FEndTime;
    property LastRequest: string read FLastRequest write FLastRequest;

    property Token: string read FToken;
  end;

  IMARSTokenEventListener = interface
    procedure OnTokenStart(const AToken: string);
    procedure OnTokenEnd(const AToken: string);
  end;

  TMARSTokenList = class
  private
    type
      TMARSTokenListSingleton = TMARSSingleton<TMARSTokenList>;
  private
    FTokens: TObjectDictionary<string, TMARSToken>;
    FCriticalSection: TCriticalSection;
    FSubscribers: TList<IMARSTokenEventListener>;
    procedure KeyNotify(Sender: TObject; const Value: string; Action: TCollectionNotification);
    class function GetInstance: TMARSTokenList; static; inline;
  public
    constructor Create;
    destructor Destroy; override;

    function GetToken(const AToken: string): TMARSToken; overload;
    function GetToken(const ARequest: TWebRequest): TMARSToken; overload;

    procedure AddToken(const AToken: string);
    procedure RemoveToken(const AToken: string);

    procedure AddSubscriber(const ASubscriber: IMARSTokenEventListener);
    procedure RemoveSubscriber(const ASubscriber: IMARSTokenEventListener);

    procedure EnumerateTokens(const ADoSomething: TProc<string, TMARSToken>);

    class property Instance: TMARSTokenList read GetInstance;
  end;

  function GetTokenString(ARequest: TWebRequest): string;
  function DecodeAuthorization(const AAuthorization: string; var AUserName, APassword: string): Boolean;

threadvar
  _TOKEN: string;

implementation

uses
  DateUtils

  {$ifndef DelphiXE7_UP}
  , IdCoderMIME
  {$else}
  , System.NetEncoding
  {$endif}
  , MARS.Core.Utils
  //, MARS.Diagnostics.Manager
  ;

function DecodeAuthorization(const AAuthorization: string; var AUserName, APassword: string): Boolean;
var
  LEncodedData: string;
  LDecodedData: string;
begin
  AUserName := '';
  APassword := '';

  LEncodedData := StringReplace(AAuthorization, 'Basic ', '', [rfIgnoreCase]);
  {$ifndef DelphiXE7_UP}
  LDecodedData := TIdDecoderMIME.DecodeString(LEncodedData);
  {$else}
  LDecodedData := TNetEncoding.Base64.Decode(LEncodedData);  
  {$endif}
  

  Result := Pos(':', LDecodedData) > 0;
  if Result then
  begin
    AUserName := Copy(LDecodedData, 1, Pos(':', LDecodedData) - 1);
    APassword := Copy(LDecodedData, Pos(':', LDecodedData) + 1, MAXINT);
  end;
end;

function GetTokenString(ARequest: TWebRequest): string;
begin
  Result := ARequest.CookieFields.Values['IDHTTPSESSIONID'];
end;

procedure TMARSTokenList.KeyNotify(Sender: TObject; const Value: string;
  Action: TCollectionNotification);
var
  LIntf: IMARSTokenEventListener;
begin
  inherited;

  case Action of
    cnAdded:
        for LIntf in FSubscribers do
          LIntf.OnTokenStart(Value);
    cnRemoved:
        for LIntf in FSubscribers do
          LIntf.OnTokenEnd(Value);
    cnExtracted: ; //  notifications not supported
  end;
end;

procedure TMARSTokenList.RemoveSubscriber(
  const ASubscriber: IMARSTokenEventListener);
begin
  FSubscribers.Remove(ASubscriber);
end;

procedure TMARSTokenList.RemoveToken(const AToken: string);
begin
  FCriticalSection.Enter;
  try
    FTokens.Remove(AToken);
  finally
    FCriticalSection.Leave;
  end;
end;

function TMARSTokenList.GetToken(const AToken: string): TMARSToken;
begin
  FCriticalSection.Enter;
  try
    Result := nil;
    FTokens.TryGetValue(AToken, Result);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TMARSTokenList.AddSubscriber(
  const ASubscriber: IMARSTokenEventListener);
begin
  FSubscribers.Add(ASubscriber);
end;

procedure TMARSTokenList.AddToken(const AToken: string);
begin
  FCriticalSection.Enter;
  try
    if not FTokens.ContainsKey(AToken) then
      FTokens.Add(AToken, TMARSToken.Create(AToken));
    // raise exception when token already exists?
  finally
    FCriticalSection.Leave
  end;
end;

constructor TMARSTokenList.Create;
begin
  inherited Create;

  FCriticalSection := TCriticalSection.Create;
  FSubscribers := TList<IMARSTokenEventListener>.Create;
  FTokens := TObjectDictionary<string, TMARSToken>.Create([doOwnsValues]);
  FTokens.OnKeyNotify := KeyNotify;
end;

destructor TMARSTokenList.Destroy;
begin
  FTokens.Free;
  FSubscribers.Free;
  FCriticalSection.Free;
  inherited;
end;

procedure TMARSTokenList.EnumerateTokens(
  const ADoSomething: TProc<string, TMARSToken>);
var
  LPair: TPair<string, TMARSToken>;
begin
  if Assigned(ADoSomething) then
  begin
    FCriticalSection.Enter;
    try
      for LPair in FTokens do
        ADoSomething(LPair.Key, LPair.Value);
    finally
      FCriticalSection.Leave;
    end;
  end;
end;

class function TMARSTokenList.GetInstance: TMARSTokenList;
begin
  Result := TMARSTokenListSingleton.Instance;
end;

function TMARSTokenList.GetToken(const ARequest: TWebRequest): TMARSToken;
begin
  Result := GetToken(GetTokenString(ARequest));
  if not Assigned(Result) then
    Result := GetToken(_TOKEN);
end;

{ TMARSToken }

constructor TMARSToken.Create(const AToken: string);
begin
  inherited Create;

  FToken := AToken;
  FUserName := '';
  FAuthenticated := false;
  FRequestCount := 0;
  FUserRoles := TStringList.Create;
  FUserRoles.Sorted := True;
  FUserRoles.Duplicates := TDuplicates.dupIgnore;
  FStartTime := Now;
  FEndTime := 0;
  FLastRequest := '';
end;

destructor TMARSToken.Destroy;
begin
  FUserRoles.Free;
  inherited;
end;

procedure TMARSToken.IncRequestCount;
begin
  Inc(FRequestCount);
end;

procedure TMARSToken.SetUserNameAndRoles(const AUserName: string;
  const ARoles: TArray<string>);
var
  LRole: string;
begin
  FUserName := AUserName;
  FUserRoles.Clear;
  for LRole in ARoles do
    FUserRoles.Add(LRole);
end;

function TMARSToken.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Result.AddPair('Token', Token);
    Result.AddPair('UserName', UserName);
    Result.AddPair('Authenticated', BooleanToTJSON(Authenticated));
    Result.AddPair('UserRoles', UserRoles.CommaText);
    Result.AddPair('StartTime', DateToJSON(StartTime));
    Result.AddPair('EndTime', DateToJSON(EndTime));
    Result.AddPair('LastRequest', LastRequest);
  except
    Result.Free;
    raise;
  end;
end;

function TMARSToken.ToJSONString: string;
var
  LObj: TJSONObject;
begin
  LObj := ToJSON;
  try
    Result := LObj.ToJSON;
  finally
    LObj.Free;
  end;
end;

function TMARSToken.UserHasRole(const ARole: string): Boolean;
begin
  Assert(Assigned(UserRoles));
  Result := UserRoles.IndexOf(ARole) <> -1;
end;

end.
