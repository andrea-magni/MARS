(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.SubResource.Stream;

{$I MARS.inc}

interface

uses
  SysUtils, Classes

  , MARS.Client.SubResource
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
  TMARSClientSubResourceStream = class(TMARSClientSubResource)
  private
    FResponse: TStream;
  protected
    procedure AfterGET(const AContent: TStream); override;
    procedure AfterPOST(const AContent: TStream); override;
    function GetResponseSize: Int64; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Response: TStream read FResponse;
    property ResponseSize: Int64 read GetResponseSize;
  end;

implementation

uses
  MARS.Core.Utils,
  MARS.Core.MediaType;

{ TMARSClientResourceJSON }

procedure TMARSClientSubResourceStream.AfterGET(const AContent: TStream);
begin
  inherited;
  CopyStream(AContent, FResponse);
end;

procedure TMARSClientSubResourceStream.AfterPOST(const AContent: TStream);
begin
  inherited;
  CopyStream(AContent, FResponse);
end;

constructor TMARSClientSubResourceStream.Create(AOwner: TComponent);
begin
  inherited;
  FResponse := TMemoryStream.Create;
  SpecificAccept := TMediaType.WILDCARD;
  SpecificContentType := TMediaType.APPLICATION_OCTET_STREAM;
end;

destructor TMARSClientSubResourceStream.Destroy;
begin
  FResponse.Free;
  inherited;
end;


function TMARSClientSubResourceStream.GetResponseSize: Int64;
begin
  Result := FResponse.Size;
end;

end.
