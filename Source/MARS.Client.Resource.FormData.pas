(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.Resource.FormData;

{$I MARS.inc}

interface

uses
  SysUtils, Classes, Generics.Collections

  , MARS.Client.Resource, MARS.Client.CustomResource, MARS.Client.Client
  , MARS.Client.Utils, MARS.Core.Utils, MARS.Core.JSON
;

type
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TMARSClientResourceFormData = class(TMARSClientResource)
  private
    FFormData: TArray<TFormParam>;
    FResponse: TMemoryStream;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure AfterPOST(const AContent: TStream); override;
    procedure AfterPUT(const AContent: TStream); override;
    function GetResponseAsString: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure POST(
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}); overload; override;
    procedure POST(
      const AFormData: TArray<TFormParam>;
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}); overload; virtual;

    procedure POSTAsync(
      const AFormData: TArray<TFormParam>;
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const ACompletionHandler: TProc<TMARSClientCustomResource>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const ASynchronize: Boolean = True); overload; virtual;

    procedure PUT(
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}); overload; override;
    procedure PUT(
      const AFormData: TArray<TFormParam>;
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}); overload; virtual;

    procedure PUTAsync(
      const AFormData: TArray<TFormParam>;
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const ACompletionHandler: TProc<TMARSClientCustomResource>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const ASynchronize: Boolean = True); overload; virtual;

    function ResponseAsJSON: TJSONValue;
    function ResponseAs<T: record>: T;
    function ResponseAsArray<T: record>: TArray<T>;
    procedure CloneStatus(const ASource: TMARSClientCustomResource); override;
  published
    property FormData: TArray<TFormParam> read FFormData write FFormData;
    property Response: TMemoryStream read FResponse;
    property ResponseAsString;
  end;

implementation

uses
  MARS.Core.MediaType
;

{ TMARSClientResourceFormData }

procedure TMARSClientResourceFormData.AfterPOST(const AContent: TStream);
begin
  inherited;
  AContent.Position := 0;
  FResponse.Size := 0; // clear
  FResponse.CopyFrom(AContent, 0);
end;

procedure TMARSClientResourceFormData.AfterPUT(const AContent: TStream);
begin
  inherited;
  AContent.Position := 0;
  FResponse.Size := 0; // clear
  FResponse.CopyFrom(AContent, 0);
end;

procedure TMARSClientResourceFormData.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TMARSClientResourceFormData then
    TMARSClientResourceFormData(Dest).FormData := FFormData;
end;

procedure TMARSClientResourceFormData.CloneStatus(
  const ASource: TMARSClientCustomResource);
var
  LSource: TMARSClientResourceFormData;
begin
  inherited;
  LSource := ASource as TMARSClientResourceFormData;
  if Assigned(LSource) then
  begin
    Response.Size := 0; // empty
    Response.CopyFrom(LSource.Response, 0);
  end;
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
  FreeAndNil(FResponse);
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

procedure TMARSClientResourceFormData.POSTAsync(
  const AFormData: TArray<TFormParam>;
  const ABeforeExecute: TProc<TMemoryStream>;
  const ACompletionHandler: TProc<TMARSClientCustomResource>;
  const AOnException: TMARSClientExecptionProc; const ASynchronize: Boolean);
begin
  FFormData := AFormData;
  inherited POSTAsync(ABeforeExecute, ACompletionHandler, AOnException, ASynchronize);
end;

procedure TMARSClientResourceFormData.PUT(const AFormData: TArray<TFormParam>;
  const ABeforeExecute: TProc<TMemoryStream>;
  const AAfterExecute: TMARSClientResponseProc;
  const AOnException: TMARSClientExecptionProc);
begin
  FFormData := AFormData;
  PUT(ABeforeExecute, AAfterExecute, AOnException);
end;

procedure TMARSClientResourceFormData.PUTAsync(
  const AFormData: TArray<TFormParam>;
  const ABeforeExecute: TProc<TMemoryStream>;
  const ACompletionHandler: TProc<TMARSClientCustomResource>;
  const AOnException: TMARSClientExecptionProc; const ASynchronize: Boolean);
begin
  FFormData := AFormData;
  inherited PUTAsync(ABeforeExecute, ACompletionHandler, AOnException, ASynchronize);
end;

function TMARSClientResourceFormData.ResponseAs<T>: T;
var
  LJSON: TJSONValue;
begin
  LJSON :=  ResponseAsJSON;
  try
    Result := (LJSON as TJSONObject).ToRecord<T>;
  finally
    LJSON.Free;
  end;
end;

function TMARSClientResourceFormData.ResponseAsArray<T>: TArray<T>;
var
  LJSON: TJSONValue;
begin
  LJSON := ResponseAsJSON;
  try
    Result := (LJSON as TJSONArray).ToArrayOfRecord<T>;
  finally
    LJSON.Free;
  end;
end;

function TMARSClientResourceFormData.ResponseAsJSON: TJSONValue;
begin
  Result := StreamToJSONValue(Response);
end;

procedure TMARSClientResourceFormData.PUT(
  const ABeforeExecute: TProc<TMemoryStream>;
  const AAfterExecute: TMARSClientResponseProc;
  const AOnException: TMARSClientExecptionProc);
var
  LResponseStream: TMemoryStream;
begin
  // inherited (!)

  try
    BeforePUT(nil);

    if Assigned(ABeforeExecute) then
      ABeforeExecute(nil);

    LResponseStream := TMemoryStream.Create;
    try
      Client.Put(URL, FFormData, LResponseStream, AuthToken, Accept, ContentType);

      AfterPUT(LResponseStream);

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
        DoError(E, TMARSHttpVerb.Put, AAfterExecute);
    end;
  end;
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
