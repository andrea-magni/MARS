(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.Resource.Stream;

{$I MARS.inc}

interface

uses
  SysUtils, Classes

  , MARS.Client.Resource
  , MARS.Client.Client, MARS.Client.Utils
  ;

type
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TMARSClientResourceStream = class(TMARSClientResource)
  private
    FResponse: TStream;
  protected
    procedure AfterGET(const AContent: TStream); override;
    procedure AfterPOST(const AContent: TStream); override;
    function GetResponseSize: Int64; virtual;
    function GetResponseAsString: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure POST(const AStream: TStream;
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}); overload;

  published
    property Response: TStream read FResponse;
    property ResponseAsString;
    property ResponseSize: Int64 read GetResponseSize;
  end;

implementation

uses
  MARS.Core.Utils,
  MARS.Core.MediaType;

{ TMARSClientResourceStream }

procedure TMARSClientResourceStream.AfterGET(const AContent: TStream);
begin
  inherited;
  CopyStream(AContent, FResponse);
end;

procedure TMARSClientResourceStream.AfterPOST(const AContent: TStream);
begin
  inherited;
  CopyStream(AContent, FResponse);
end;

constructor TMARSClientResourceStream.Create(AOwner: TComponent);
begin
  inherited;
  SpecificAccept := TMediaType.WILDCARD;
  SpecificContentType := TMediaType.APPLICATION_OCTET_STREAM;
  FResponse := TMemoryStream.Create;
end;

destructor TMARSClientResourceStream.Destroy;
begin
  FResponse.Free;
  inherited;
end;

function TMARSClientResourceStream.GetResponseAsString: string;
begin
  Result := Format('[Binary data, size: %d]', [GetResponseSize]);
end;

function TMARSClientResourceStream.GetResponseSize: Int64;
begin
  Result := FResponse.Size;
end;

procedure TMARSClientResourceStream.POST(const AStream: TStream;
  const ABeforeExecute: TProc<TMemoryStream>;
  const AAfterExecute: TMARSClientResponseProc;
  const AOnException: TMARSClientExecptionProc);
begin
  POST(
    procedure (AContent: TMemoryStream)
    begin
      AContent.CopyFrom(AStream, 0);
      AContent.Position := 0;
      if Assigned(ABeforeExecute) then
        ABeforeExecute(AContent);
    end
  , AAfterExecute
  , AOnException
  );
end;

end.
