(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)

unit MARS.Client.Resource.FormUrlEncoded;

{$I MARS.inc}

interface

uses
  SysUtils, Classes, Generics.Collections

  , MARS.Client.Resource, MARS.Client.CustomResource, MARS.Client.Client
  , MARS.Client.Utils, MARS.Core.Utils, MARS.Utils.Parameters, MARS.Core.JSON
;

type
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TMARSClientResourceFormUrlEncoded = class(TMARSClientResource)
  private
    FFormUrlEncoded: TMARSParameters;
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
      const AFormUrlEncoded: TMARSParameters;
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}); overload; virtual;

    procedure POSTAsync(
      const AFormUrlEncoded: TMARSParameters;
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const ACompletionHandler: TProc<TMARSClientCustomResource>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const ASynchronize: Boolean = True); overload; virtual;

    procedure PUT(
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}); overload; override;
    procedure PUT(
      const AFormUrlEncoded: TMARSParameters;
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}); overload; virtual;

    function ResponseAsJSON: TJSONValue;
    function ResponseAs<T: record>: T;
    function ResponseAsArray<T: record>: TArray<T>;
    procedure CloneStatus(const ASource: TMARSClientCustomResource); override;
  published
    property FormUrlEncoded: TMARSParameters read FFormUrlEncoded write FFormUrlEncoded;
    property Response: TMemoryStream read FResponse;
    property ResponseAsString;
  end;

const
  FORM_URL_ENCODED_SEPARATOR = '-';

implementation

uses
  MARS.Core.MediaType
;

{ TMARSClientResourceFormUrlEncoded }

procedure TMARSClientResourceFormUrlEncoded.AfterPOST(const AContent: TStream);
begin
  inherited;
  AContent.Position := 0;
  FResponse.Size := 0; // clear
  FResponse.CopyFrom(AContent, 0);
end;

procedure TMARSClientResourceFormUrlEncoded.AfterPUT(const AContent: TStream);
begin
  inherited;
  AContent.Position := 0;
  FResponse.Size := 0; // clear
  FResponse.CopyFrom(AContent, 0);
end;

procedure TMARSClientResourceFormUrlEncoded.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TMARSClientResourceFormUrlEncoded then
    TMARSClientResourceFormUrlEncoded(Dest).FFormUrlEncoded := FFormUrlEncoded;
end;

procedure TMARSClientResourceFormUrlEncoded.CloneStatus(
  const ASource: TMARSClientCustomResource);
var
  LSource: TMARSClientResourceFormUrlEncoded;
begin
  inherited;
  LSource := ASource as TMARSClientResourceFormUrlEncoded;
  if Assigned(LSource) then
  begin
    Response.Size := 0; // empty
    Response.CopyFrom(LSource.Response, 0);
  end;
end;

constructor TMARSClientResourceFormUrlEncoded.Create(AOwner: TComponent);
begin
  inherited;
  FFormUrlEncoded := TMARSParameters.Create('keys');
  FResponse := TMemoryStream.Create;
  SpecificContentType := TMediaType.APPLICATION_FORM_URLENCODED_TYPE;
end;

destructor TMARSClientResourceFormUrlEncoded.Destroy;
begin
  FResponse.Free;
  FFormUrlEncoded.Free;
  inherited;
end;

function TMARSClientResourceFormUrlEncoded.GetResponseAsString: string;
begin
  Result := StreamToString(FResponse);
end;

procedure TMARSClientResourceFormUrlEncoded.POST(const AFormUrlEncoded: TMARSParameters;
  const ABeforeExecute: TProc<TMemoryStream>; const AAfterExecute: TMARSClientResponseProc;
  const AOnException: TMARSClientExecptionProc);
begin
  FFormUrlEncoded := AFormUrlEncoded;
  POST(ABeforeExecute, AAfterExecute, AOnException);
end;

procedure TMARSClientResourceFormUrlEncoded.POSTAsync(const AFormUrlEncoded: TMARSParameters;
  const ABeforeExecute: TProc<TMemoryStream>; const ACompletionHandler: TProc<TMARSClientCustomResource>;
  const AOnException: TMARSClientExecptionProc; const ASynchronize: Boolean);
begin
  FFormUrlEncoded := AFormUrlEncoded;
  inherited POSTAsync(ABeforeExecute, ACompletionHandler, AOnException, ASynchronize);
end;

procedure TMARSClientResourceFormUrlEncoded.PUT(const AFormUrlEncoded: TMARSParameters;
  const ABeforeExecute: TProc<TMemoryStream>; const AAfterExecute: TMARSClientResponseProc;
  const AOnException: TMARSClientExecptionProc);
begin
  FFormUrlEncoded := AFormUrlEncoded;
  PUT(ABeforeExecute, AAfterExecute, AOnException);
end;

function TMARSClientResourceFormUrlEncoded.ResponseAs<T>: T;
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

function TMARSClientResourceFormUrlEncoded.ResponseAsArray<T>: TArray<T>;
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

function TMARSClientResourceFormUrlEncoded.ResponseAsJSON: TJSONValue;
begin
  Result := StreamToJSONValue(Response);
end;

procedure TMARSClientResourceFormUrlEncoded.PUT(const ABeforeExecute: TProc<TMemoryStream>;
  const AAfterExecute: TMARSClientResponseProc; const AOnException: TMARSClientExecptionProc);
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
      Client.Put(URL, FFormUrlEncoded, LResponseStream, AuthToken, Accept, ContentType);

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

procedure TMARSClientResourceFormUrlEncoded.POST(const ABeforeExecute: TProc<TMemoryStream>;
  const AAfterExecute: TMARSClientResponseProc; const AOnException: TMARSClientExecptionProc);
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
      Client.Post(URL, FFormUrlEncoded, LResponseStream, AuthToken, Accept, ContentType);

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
