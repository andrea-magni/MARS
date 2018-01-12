(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.Resource.FormData;

{$I MARS.inc}

interface

uses
  SysUtils, Classes, Generics.Collections

  , MARS.Client.Resource, MARS.Client.Client
  , MARS.Client.Utils, MARS.Core.Utils
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
  TMARSClientResourceFormData = class(TMARSClientResource)
  private
    FFormData: TArray<TFormParam>;
    FResponse: TMemoryStream;
  protected
    procedure AfterPOST(const AContent: TStream); override;
    function GetResponseAsString: string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure POST(const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}); overload; override;
    procedure POST(const AFormData: TArray<TFormParam>;
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}); overload; virtual;
  published
    property FormData: TArray<TFormParam> read FFormData;
    property Response: TMemoryStream read FResponse;
    property ResponseAsString: string read GetResponseAsString;
  end;

procedure Register;

implementation

uses
  MARS.Core.MediaType
;

procedure Register;
begin
  RegisterComponents('MARS-Curiosity Client', [TMARSClientResourceFormData]);
end;

{ TMARSClientResourceFormData }

procedure TMARSClientResourceFormData.AfterPOST(const AContent: TStream);
begin
  inherited;
  AContent.Position := 0;
  FResponse.Size := 0; // clear
  FResponse.CopyFrom(AContent, 0);
end;

constructor TMARSClientResourceFormData.Create(AOwner: TComponent);
begin
  inherited;
  FFormData := [];
  FResponse := TMemoryStream.Create;
  SpecificContentType := TMediaType.MULTIPART_FORM_DATA;
end;

destructor TMARSClientResourceFormData.Destroy;
begin
  FResponse.Free;
  FFormData := [];
  inherited;
end;

function TMARSClientResourceFormData.GetResponseAsString: string;
begin
  Result := StreamToString(FResponse);
end;

procedure TMARSClientResourceFormData.POST(const AFormData: TArray<TFormParam>;
  const ABeforeExecute: TProc<TMemoryStream>;
  const AAfterExecute: TMARSClientResponseProc;
  const AOnException: TMARSClientExecptionProc);
begin
  FFormData := AFormData;
  POST(ABeforeExecute, AAfterExecute, AOnException);
end;

procedure TMARSClientResourceFormData.POST(
  const ABeforeExecute: TProc<TMemoryStream>;
  const AAfterExecute: TMARSClientResponseProc;
  const AOnException: TMARSClientExecptionProc);
var
  LResponseStream: TMemoryStream;
begin
  // inherited (!)

  try
    BeforePOST(nil);

    if Assigned(ABeforeExecute) then
      ABeforeExecute(nil);

    LResponseStream := TMemoryStream.Create;
    try
      Client.Post(URL, FFormData, LResponseStream, AuthToken, Accept, ContentType);

      AfterPOST(LResponseStream);

      if Assigned(AAfterExecute) then
        AAfterExecute(LResponseStream);
    finally
      LResponseStream.Free;
    end;
  except
    on E:Exception do
    begin
      if Assigned(AOnException) then
        AOnException(E)
      else
        DoError(E, TMARSHttpVerb.Post, AAfterExecute);
    end;
  end;
end;

end.
