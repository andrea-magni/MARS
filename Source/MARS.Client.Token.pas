(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.Token;

{$I MARS.inc}

interface

uses
  SysUtils, Classes
, MARS.Core.JSON, MARS.Client.Resource, MARS.Client.CustomResource
, MARS.Client.Utils
, MARS.Utils.Parameters, MARS.Utils.Parameters.JSON
;

type
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TMARSClientToken = class(TMARSClientResource)
  private
    FData: TJSONObject;
    FIsVerified: Boolean;
    FToken: string;
    FPassword: string;
    FUserName: string;
    FUserRoles: TStrings;
    FClaims: TMARSParameters;
    FExpiration: TDateTime;
    FIssuedAt: TDateTime;
    function GetIsExpired: Boolean;
  protected
    procedure AfterGET(const AContent: TStream); override;
    procedure BeforePOST(const AContent: TMemoryStream); override;
    procedure AfterPOST(const AContent: TStream); override;
    procedure AfterDELETE(const AContent: TStream); override;
    procedure ParseData; virtual;
    function GetAuthToken: string; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear; virtual;
    procedure SetData(const AJSON: TJSONObject); virtual;
    function SaveToBytes: TBytes; virtual;
    procedure LoadFromBytes(const ABytes: TBytes); virtual;
    procedure SaveToStream(const AStream: TStream); virtual;
    procedure LoadFromStream(const AStream: TStream); virtual;
    procedure SaveToFile(const AFilename: string); virtual;
    procedure LoadFromFile(const AFilename: string); virtual;
    procedure CloneStatus(const ASource: TMARSClientCustomResource); override;
  published
    property Data: TJSONObject read FData;

    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property Token: string read FToken;
    property Authenticated: Boolean read FIsVerified;
    property IsVerified: Boolean read FIsVerified;
    property UserRoles: TStrings read FUserRoles;
    property IssuedAt: TDateTime read FIssuedAt;
    property Expiration: TDateTime read FExpiration;
    property Claims: TMARSParameters read FClaims;
    property IsExpired: Boolean read GetIsExpired;
  end;

implementation

uses
  DateUtils
, MARS.Core.Utils, MARS.Rtti.Utils
, MARS.Core.MediaType
;

{ TMARSClientToken }

procedure TMARSClientToken.AfterDELETE(const AContent: TStream);
begin
  inherited;
  SetData(StreamToJSONValue(AContent) as TJSONObject);
end;

procedure TMARSClientToken.AfterGET(const AContent: TStream);
begin
  inherited;
  SetData(StreamToJSONValue(AContent) as TJSONObject);
end;

procedure TMARSClientToken.AfterPOST(const AContent: TStream);
begin
  inherited;
  SetData(StreamToJSONValue(AContent) as TJSONObject);
end;

procedure TMARSClientToken.AssignTo(Dest: TPersistent);
var
  LDest: TMARSClientToken;
begin
  inherited AssignTo(Dest);
  LDest := Dest as TMARSClientToken;
  if Assigned(LDest) then
  begin
    LDest.LoadFromBytes(SaveToBytes);
    LDest.UserName := UserName;
    LDest.Password := Password;
  end;
end;

procedure TMARSClientToken.BeforePOST(const AContent: TMemoryStream);
begin
  inherited;
  StringToStream(AContent, 'username=' + FUserName + '&password=' + FPassword);
end;

procedure TMARSClientToken.Clear;
begin
  FToken := '';
  FIsVerified := False;

  FClaims.Clear;

  FIssuedAt := 0.0;
  FExpiration := 0.0;
  FUserName := '';
  FUserRoles.Clear;
end;

procedure TMARSClientToken.CloneStatus(
  const ASource: TMARSClientCustomResource);
var
  LSource: TMARSClientToken;
begin
  inherited;
  LSource := ASource as TMARSClientToken;
  if Assigned(LSource) then
  begin
    if Assigned(LSource.Data) then
      SetData(LSource.Data.Clone as TJSONObject)
    else
      SetData(nil);
  end;
end;

constructor TMARSClientToken.Create(AOwner: TComponent);
begin
  inherited;
  Resource := 'token';
  FData := TJSONObject.Create;
  FUserRoles := TStringList.Create;
  FClaims := TMARSParameters.Create('');
  SpecificAccept := TMediaType.APPLICATION_JSON;
  SpecificContentType := TMediaType.APPLICATION_JSON;
end;

destructor TMARSClientToken.Destroy;
begin
  FreeAndNil(FClaims);
  FreeAndNil(FUserRoles);
  FreeAndNil(FData);

  inherited;
end;

function TMARSClientToken.GetAuthToken: string;
begin
  Result := FToken;
end;

function TMARSClientToken.GetIsExpired: Boolean;
begin
  Result := Expiration < Now;
end;

procedure TMARSClientToken.LoadFromBytes(const ABytes: TBytes);
var
  LStream: TBytesStream;
begin
  LStream := TBytesStream.Create(ABytes);
  try
    LStream.Position := 0;
    LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TMARSClientToken.LoadFromFile(const AFilename: string);
var
  LFileStream: TFileStream;
begin
  LFileStream := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(LFileStream);
  finally
    LFileStream.Free;
  end;
end;

procedure TMARSClientToken.LoadFromStream(const AStream: TStream);
begin
  SetData(StreamToJSONValue(AStream) as TJSONObject);
end;

procedure TMARSClientToken.ParseData;
var
  LClaims: TJSONObject;
begin
  Assert(Assigned(FData));

  FToken := FData.ReadStringValue('Token');
  FIsVerified := FData.ReadBoolValue('IsVerified');

  FClaims.Clear;
{$IFNDEF DelphiXE8_UP}
  if FData.TryGetValue<TJSONObject>('Claims', LClaims) then
{$ELSE}
  if FData.TryGetValue('Claims', LClaims) then
{$endif}
  begin
    FClaims.LoadFromJSON(LClaims);

    FIssuedAt := UnixToDateTime(FClaims['iat'].AsInt64{$IFDEF DelphiXE7_UP}, False {$ENDIF});
    FExpiration := UnixToDateTime(FClaims['exp'].AsInt64{$IFDEF DelphiXE7_UP}, False {$ENDIF});
    FUserName := FClaims['UserName'].AsString;
    FUserRoles.CommaText := FClaims['Roles'].AsString;
  end
  else
  begin
    FIssuedAt := 0.0;
    FExpiration := 0.0;
    FUserRoles.Clear;
  end;
end;

function TMARSClientToken.SaveToBytes: TBytes;
var
  LStream: TBytesStream;
begin
  LStream := TBytesStream.Create;
  try
    JSONValueToStream(FData, LStream);
    LStream.Position := 0;
    SetLength(Result, LStream.Size);
    LStream.Read(Result, Length(Result));
  finally
    LStream.Free;
  end;
end;

procedure TMARSClientToken.SaveToFile(const AFilename: string);
var
  LFileStream: TFileStream;
begin
  LFileStream := TFileStream.Create(AFilename, fmCreate or fmOpenReadWrite);
  try
    SaveToStream(LFileStream);
  finally
    LFileStream.Free;
  end;
end;

procedure TMARSClientToken.SaveToStream(const AStream: TStream);
begin
  JSONValueToStream(FData, AStream);
end;

procedure TMARSClientToken.SetData(const AJSON: TJSONObject);
begin
  FreeAndNil(FData);
  if Assigned(AJSON) then
  begin
    FData := AJSON;
    ParseData;
  end
  else
    Clear;
end;

end.
