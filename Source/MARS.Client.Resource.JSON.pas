(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.Resource.JSON;

{$I MARS.inc}

interface

uses
  SysUtils, Classes
  , MARS.Core.JSON, MARS.Client.Utils
  , MARS.Client.Resource, MARS.Client.CustomResource
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
    procedure AfterGET(const AContent: TStream); override;
    procedure AfterPOST(const AContent: TStream); override;
    function GetResponseAsString: string; virtual;
  public
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
    property ResponseAsString: string read GetResponseAsString;
  end;

implementation

uses
  MARS.Core.Utils, MARS.Core.MediaType
;

{ TMARSClientResourceJSON }

procedure TMARSClientResourceJSON.AfterGET(const AContent: TStream);
begin
  inherited;
  if Assigned(FResponse) then
    FResponse.Free;
  FResponse := StreamToJSONValue(AContent);
end;

procedure TMARSClientResourceJSON.AfterPOST(const AContent: TStream);
begin
  inherited;
  if Assigned(FResponse) then
    FResponse.Free;
  FResponse := StreamToJSONValue(AContent);
end;

constructor TMARSClientResourceJSON.Create(AOwner: TComponent);
begin
  inherited;
  FResponse := TJSONObject.Create;
  SpecificAccept := TMediaType.APPLICATION_JSON;
  SpecificContentType := TMediaType.APPLICATION_JSON;
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

function TMARSClientResourceJSON.ResponseAs<T>: T;
begin
  Result := (Response as TJSONObject).ToRecord<T>;
end;

function TMARSClientResourceJSON.ResponseAsArray<T>: TArray<T>;
begin
  Result := (Response as TJSONArray).ToArrayOfRecord<T>;
end;

end.
