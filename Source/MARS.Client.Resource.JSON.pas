(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.Resource.JSON;

{$I MARS.inc}

interface

uses
  SysUtils, Classes, REST.Client, Generics.Collections
  , MARS.Core.JSON, MARS.Client.Utils
  , MARS.Client.Resource, MARS.Client.CustomResource
  , MARS.Client.Client
  ;

type
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TMARSClientResourceJSON = class(TMARSClientResource, IRESTResponseJSON)
  private
    FResponse: TJSONValue;
    FNotifyList: TList<TNotifyEvent>;
  protected
    procedure AfterGET(const AContent: TStream); override;
    procedure AfterPOST(const AContent: TStream); override;
    procedure AfterPUT(const AContent: TStream); override;
    procedure AfterDELETE(const AContent: TStream); override;
    procedure AssignTo(Dest: TPersistent); override;
    function GetResponseAsString: string; override;
    function GetResponseAsJSON: string; virtual;
    procedure RefreshResponse(const AContent: TStream); overload; virtual;
    procedure RefreshResponse(const AContent: TJSONValue); overload; virtual;
  public
    // IRESTResponseJSON interface ---------------------------------------------
    procedure AddJSONChangedEvent(const ANotify: TNotifyEvent);
    procedure RemoveJSONChangedEvent(const ANotify: TNotifyEvent);
    procedure GetJSONResponse(out AJSONValue: TJSONValue;
      out AHasOwner: Boolean);
    function HasJSONResponse: Boolean;
    function HasResponseContent: Boolean;
    // -------------------------------------------------------------------------

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure POST(const AJSONValue: TJSONValue;
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}); overload;

    procedure POST<R: record>(const ARecord: R;
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}); overload;

    procedure POST<R: record>(const AArrayOfRecord: TArray<R>;
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}); overload;

    procedure POSTAsync(const AJSONValue: TJSONValue;
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const ACompletionHandler: TProc<TMARSClientCustomResource>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const ASynchronize: Boolean = True); overload;

    procedure POSTAsync<R: record>(const ARecord: R;
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const ACompletionHandler: TProc<TMARSClientCustomResource>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const ASynchronize: Boolean = True); overload;

    procedure POSTAsync<R: record>(const AArrayOfRecord: TArray<R>;
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const ACompletionHandler: TProc<TMARSClientCustomResource>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const ASynchronize: Boolean = True); overload;

    procedure PUT(const AJSONValue: TJSONValue;
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}); overload;

    procedure PUT<R: record>(const ARecord: R;
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}); overload;

    procedure PUT<R: record>(const AArrayOfRecord: TArray<R>;
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}); overload;

    procedure PUTAsync(const AJSONValue: TJSONValue;
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const ACompletionHandler: TProc<TMARSClientCustomResource>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const ASynchronize: Boolean = True); overload;

    function ResponseAs<T: record>: T;
    function ResponseAsArray<T: record>: TArray<T>;
  published
    property Response: TJSONValue read FResponse write FResponse;
    property ResponseAsString;
    property ResponseAsJSON: string read GetResponseAsJSON;
  end;

implementation

uses
  System.JSON
, MARS.Core.Utils, MARS.Core.MediaType
;

{ TMARSClientResourceJSON }

procedure TMARSClientResourceJSON.AddJSONChangedEvent(
  const ANotify: TNotifyEvent);
begin
  if not FNotifyList.Contains(ANotify) then
    FNotifyList.Add(ANotify);
end;

procedure TMARSClientResourceJSON.AfterDELETE(const AContent: TStream);
begin
  inherited;
  RefreshResponse(AContent);
end;

procedure TMARSClientResourceJSON.AfterGET(const AContent: TStream);
begin
  inherited;
  RefreshResponse(AContent);
end;

procedure TMARSClientResourceJSON.AfterPOST(const AContent: TStream);
begin
  inherited;
  RefreshResponse(AContent);
end;

procedure TMARSClientResourceJSON.AfterPUT(const AContent: TStream);
begin
  inherited;
  RefreshResponse(AContent);
end;

procedure TMARSClientResourceJSON.AssignTo(Dest: TPersistent);
var
  LDest: TMARSClientResourceJSON;
  LNotifyEvent: TNotifyEvent;
begin
  inherited AssignTo(Dest);
  LDest := Dest as TMARSClientResourceJSON;
  LDest.FNotifyList.Clear;
  for LNotifyEvent in FNotifyList do
    LDest.FNotifyList.Add(LNotifyEvent);

  LDest.RefreshResponse(FResponse);
end;

constructor TMARSClientResourceJSON.Create(AOwner: TComponent);
begin
  inherited;
  FResponse := nil;
  SpecificAccept := TMediaType.APPLICATION_JSON;
  SpecificContentType := TMediaType.APPLICATION_JSON;
  FNotifyList := TList<TNotifyEvent>.Create;
end;

destructor TMARSClientResourceJSON.Destroy;
begin
  FreeAndNil(FResponse);
  FreeAndNil(FNotifyList);
  inherited;
end;

procedure TMARSClientResourceJSON.GetJSONResponse(out AJSONValue: TJSONValue;
  out AHasOwner: Boolean);
begin
  AJSONValue := FResponse;
  AHasOwner := True;
end;

function TMARSClientResourceJSON.GetResponseAsJSON: string;
begin
  Result := '';
  if Assigned(FResponse) then
    Result := FResponse.ToJSON;
end;

function TMARSClientResourceJSON.GetResponseAsString: string;
begin
  Result := '';
  if Assigned(FResponse) then
    Result := FResponse.ToString;
end;

function TMARSClientResourceJSON.HasJSONResponse: Boolean;
begin
  Result := Assigned(FResponse);
end;

function TMARSClientResourceJSON.HasResponseContent: Boolean;
begin
  Result := Assigned(FResponse);
end;

procedure TMARSClientResourceJSON.POST(const AJSONValue: TJSONValue;
  const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
  const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
  const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif});
begin
  POST(
    procedure (AContent: TMemoryStream)
    begin
      JSONValueToStream(AJSONValue, AContent);
      AContent.Position := 0;
      if Assigned(ABeforeExecute) then
        ABeforeExecute(AContent);
    end
  , AAfterExecute
  , AOnException
  );
end;

procedure TMARSClientResourceJSON.POST<R>(const ARecord: R;
  const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
  const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
  const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif});
begin
  POST(
    procedure (AContent: TMemoryStream)
    var
      LJSONValue: TJSONValue;
    begin
      LJSONValue := TJSONObject.RecordToJSON<R>(ARecord);
      try
        JSONValueToStream(LJSONValue, AContent);
      finally
        LJSONValue.Free;
      end;
      AContent.Position := 0;
      if Assigned(ABeforeExecute) then
        ABeforeExecute(AContent);
    end
  , AAfterExecute
  , AOnException
  );
end;

procedure TMARSClientResourceJSON.POST<R>(const AArrayOfRecord: TArray<R>;
  const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
  const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
  const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif});
begin
  POST(
    procedure (AContent: TMemoryStream)
    var
      LJSONValue: TJSONValue;
    begin
      LJSONValue := TJSONArray.ArrayOfRecordToJSON<R>(AArrayOfRecord);
      try
        JSONValueToStream(LJSONValue, AContent);
      finally
        LJSONValue.Free;
      end;
      AContent.Position := 0;
      if Assigned(ABeforeExecute) then
        ABeforeExecute(AContent);
    end
  , AAfterExecute
  , AOnException
  );
end;


procedure TMARSClientResourceJSON.POSTAsync(const AJSONValue: TJSONValue;
  const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
  const ACompletionHandler: TProc<TMARSClientCustomResource>{$ifdef DelphiXE2_UP} = nil{$endif};
  const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif};
  const ASynchronize: Boolean = True);
begin
  POSTAsync(
    procedure (AContent: TMemoryStream)
    begin
      JSONValueToStream(AJSONValue, AContent);
      AContent.Position := 0;
      if Assigned(ABeforeExecute) then
        ABeforeExecute(AContent);
    end
  , ACompletionHandler
  , AOnException
  , ASynchronize
  );
end;

procedure TMARSClientResourceJSON.POSTAsync<R>(const AArrayOfRecord: TArray<R>;
  const ABeforeExecute: TProc<TMemoryStream>;
  const ACompletionHandler: TProc<TMARSClientCustomResource>;
  const AOnException: TMARSClientExecptionProc; const ASynchronize: Boolean);
begin
  POSTAsync(
    procedure (AContent: TMemoryStream)
    var
      LJSONValue: TJSONValue;
    begin
      LJSONValue := TJSONArray.ArrayOfRecordToJSON<R>(AArrayOfRecord);
      try
        JSONValueToStream(LJSONValue, AContent);
      finally
        LJSONValue.Free;
      end;
      AContent.Position := 0;
      if Assigned(ABeforeExecute) then
        ABeforeExecute(AContent);
    end
  , ACompletionHandler
  , AOnException
  , ASynchronize
  );
end;

procedure TMARSClientResourceJSON.POSTAsync<R>(const ARecord: R;
  const ABeforeExecute: TProc<TMemoryStream>;
  const ACompletionHandler: TProc<TMARSClientCustomResource>;
  const AOnException: TMARSClientExecptionProc; const ASynchronize: Boolean);
begin
  POSTAsync(
    procedure (AContent: TMemoryStream)
    var
      LJSONValue: TJSONValue;
    begin
      LJSONValue := TJSONObject.RecordToJSON<R>(ARecord);
      try
        JSONValueToStream(LJSONValue, AContent);
      finally
        LJSONValue.Free;
      end;
      AContent.Position := 0;
      if Assigned(ABeforeExecute) then
        ABeforeExecute(AContent);
    end
  , ACompletionHandler
  , AOnException
  , ASynchronize
  );
end;

procedure TMARSClientResourceJSON.PUT(const AJSONValue: TJSONValue;
  const ABeforeExecute: TProc<TMemoryStream>;
  const AAfterExecute: TMARSClientResponseProc;
  const AOnException: TMARSClientExecptionProc);
begin
  PUT(
    procedure (AContent: TMemoryStream)
    begin
      JSONValueToStream(AJSONValue, AContent);
      AContent.Position := 0;
      if Assigned(ABeforeExecute) then
        ABeforeExecute(AContent);
    end
  , AAfterExecute
  , AOnException
  );
end;

procedure TMARSClientResourceJSON.PUT<R>(const ARecord: R;
  const ABeforeExecute: TProc<TMemoryStream>;
  const AAfterExecute: TMARSClientResponseProc;
  const AOnException: TMARSClientExecptionProc);
begin
  PUT(
    procedure (AContent: TMemoryStream)
    var
      LJSONValue: TJSONValue;
    begin
      LJSONValue := TJSONObject.RecordToJSON<R>(ARecord);
      try
        JSONValueToStream(LJSONValue, AContent);
      finally
        LJSONValue.Free;
      end;
      AContent.Position := 0;
      if Assigned(ABeforeExecute) then
        ABeforeExecute(AContent);
    end
  , AAfterExecute
  , AOnException
  );
end;

procedure TMARSClientResourceJSON.PUT<R>(const AArrayOfRecord: TArray<R>;
  const ABeforeExecute: TProc<TMemoryStream>;
  const AAfterExecute: TMARSClientResponseProc;
  const AOnException: TMARSClientExecptionProc);
begin
  PUT(
    procedure (AContent: TMemoryStream)
    var
      LJSONValue: TJSONValue;
    begin
      LJSONValue := TJSONArray.ArrayOfRecordToJSON<R>(AArrayOfRecord);
      try
        JSONValueToStream(LJSONValue, AContent);
      finally
        LJSONValue.Free;
      end;
      AContent.Position := 0;
      if Assigned(ABeforeExecute) then
        ABeforeExecute(AContent);
    end
  , AAfterExecute
  , AOnException
  );
end;

procedure TMARSClientResourceJSON.PUTAsync(const AJSONValue: TJSONValue;
  const ABeforeExecute: TProc<TMemoryStream>;
  const ACompletionHandler: TProc<TMARSClientCustomResource>;
  const AOnException: TMARSClientExecptionProc; const ASynchronize: Boolean);
begin
  PUTAsync(
    procedure (AContent: TMemoryStream)
    begin
      JSONValueToStream(AJSONValue, AContent);
      AContent.Position := 0;
      if Assigned(ABeforeExecute) then
        ABeforeExecute(AContent);
    end
  , ACompletionHandler
  , AOnException
  , ASynchronize
  );
end;

procedure TMARSClientResourceJSON.RefreshResponse(const AContent: TStream);
begin
  RefreshResponse(StreamToJSONValue(AContent));
end;

procedure TMARSClientResourceJSON.RefreshResponse(const AContent: TJSONValue);
var
  LSubscriber: TNotifyEvent;
begin
  FreeAndNil(FResponse);
  if Assigned(AContent) then
    FResponse := AContent;

  for LSubscriber in FNotifyList do
    LSubscriber(Self);
end;

procedure TMARSClientResourceJSON.RemoveJSONChangedEvent(
  const ANotify: TNotifyEvent);
var
  LIndex: Integer;
begin
  LIndex := FNotifyList.IndexOf(ANotify);
  if LIndex <> -1 then
    FNotifyList.Delete(LIndex);
end;

function TMARSClientResourceJSON.ResponseAs<T>: T;
begin
  Result := (Response as TJSONObject).ToRecord<T>;
end;

function TMARSClientResourceJSON.ResponseAsArray<T>: TArray<T>;
begin
  Result := [];
  if Response is TJSONArray then
    Result := TJSONArray(Response).ToArrayOfRecord<T>
  else if Response is TJSONObject then
    Result := [TJSONObject(Response).ToRecord<T>];

end;

end.
