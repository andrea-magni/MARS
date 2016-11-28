(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.Resource.JSON;

{$I MARS.inc}

interface

uses
  SysUtils, Classes
  , MARS.Core.JSON

  , MARS.Client.Resource
  , MARS.Client.Client
  ;

type
  {$ifdef DelphiXE2_UP}
    [ComponentPlatformsAttribute(
        pidWin32 or pidWin64
     or pidOSX32
     or pidiOSSimulator
     or pidiOSDevice
    {$ifdef DelphiXE8_UP}
     or pidiOSDevice32 or pidiOSDevice64
    {$endif}
     or pidAndroid)]
  {$endif}
  TMARSClientResourceJSON = class(TMARSClientResource)
  private
    FResponse: TJSONValue;
  protected
    procedure AfterGET(); override;
    procedure AfterPOST(); override;
    function GetResponseAsString: string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Response: TJSONValue read FResponse write FResponse;
    property ResponseAsString: string read GetResponseAsString;
  end;

procedure Register;

implementation

uses
  MARS.Core.Utils;

procedure Register;
begin
  RegisterComponents('MARS-Curiosity Client', [TMARSClientResourceJSON]);
end;

{ TMARSClientResourceJSON }

procedure TMARSClientResourceJSON.AfterGET();
begin
  inherited;
  if Assigned(FResponse) then
    FResponse.Free;
  FResponse := StreamToJSONValue(Client.Response.ContentStream);
end;

procedure TMARSClientResourceJSON.AfterPOST;
begin
  inherited;
  if Assigned(FResponse) then
    FResponse.Free;
  FResponse := StreamToJSONValue(Client.Response.ContentStream);
end;

constructor TMARSClientResourceJSON.Create(AOwner: TComponent);
begin
  inherited;
  FResponse := TJSONObject.Create;
end;

destructor TMARSClientResourceJSON.Destroy;
begin
  FResponse.Free;
  inherited;
end;

function TMARSClientResourceJSON.GetResponseAsString: string;
begin
  Result := '';
  if Assigned(FResponse) then
    Result := FResponse.ToJSON;
end;

end.
