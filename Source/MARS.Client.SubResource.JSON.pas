(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.SubResource.JSON;

{$I MARS.inc}

interface

uses
  SysUtils, Classes
  , MARS.Core.JSON

  , MARS.Client.SubResource, MARS.Client.Client
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
  TMARSClientSubResourceJSON = class(TMARSClientSubResource)
  private
    FResponse: TJSONValue;
  protected
    procedure AfterGET(const AContent: TStream); override;
    procedure AfterPOST(const AContent: TStream); override;
    function GetResponseAsString: string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ResponseAs<T: record>: T;
    function ResponseAsArray<T: record>: TArray<T>;
  published
    property Response: TJSONValue read FResponse write FResponse;
    property ResponseAsString: string read GetResponseAsString;
  end;

procedure Register;

implementation

uses
  MARS.Core.Utils, MARS.Core.MediaType
;

procedure Register;
begin
  RegisterComponents('MARS-Curiosity Client', [TMARSClientSubResourceJSON]);
end;

{ TMARSClientResourceJSON }

procedure TMARSClientSubResourceJSON.AfterGET(const AContent: TStream);
begin
  inherited;
  if Assigned(FResponse) then
    FResponse.Free;
  FResponse := StreamToJSONValue(AContent);
end;

procedure TMARSClientSubResourceJSON.AfterPOST(const AContent: TStream);
begin
  inherited;
  if Assigned(FResponse) then
    FResponse.Free;
  FResponse := StreamToJSONValue(AContent);
end;

constructor TMARSClientSubResourceJSON.Create(AOwner: TComponent);
begin
  inherited;
  FResponse := TJSONObject.Create;
  SpecificAccept := TMediaType.APPLICATION_JSON;
  SpecificContentType := TMediaType.APPLICATION_JSON;
end;

destructor TMARSClientSubResourceJSON.Destroy;
begin
  FResponse.Free;
  inherited;
end;

function TMARSClientSubResourceJSON.GetResponseAsString: string;
begin
  Result := '';
  if Assigned(FResponse) then
    Result := FResponse.ToJSON;
end;

function TMARSClientSubResourceJSON.ResponseAs<T>: T;
begin
  Result := (Response as TJSONObject).ToRecord<T>;
end;

function TMARSClientSubResourceJSON.ResponseAsArray<T>: TArray<T>;
begin
  Result := (Response as TJSONArray).ToArrayOfRecord<T>;
end;

end.
