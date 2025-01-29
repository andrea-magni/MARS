unit MARS.Core.Response.Dummy;

interface

uses
  Classes, SysUtils
, MARS.Core.RequestAndResponse.Interfaces
;

type
  TMARSWebResponseDummy = class(TInterfacedObject, IMARSResponse)
  private
    FContent: string;
    FContentType: string;
    FContentEncoding: string;
    FContentLength: Integer;
    FContentStream: TStream;
    FStatusCode: Integer;
    FReasonString: string;
    FHeaders: TStringList;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    // IMARSResponse implementation --------------------------------------------
    function GetContentStream: TStream;
    procedure SetContentStream(const AContentStream: TStream);
    function GetContentType: string;
    procedure SetContentType(const AContentType: string);
    function GetContentLength: Integer;
    procedure SetContentLength(const ALength: Integer);
    function GetContentEncoding: string;
    procedure SetContentEncoding(const AContentEncoding: string);
    function GetStatusCode: Integer;
    procedure SetStatusCode(const AStatusCode: Integer);
    function GetReasonString: string;
    procedure SetReasonString(const AReasonString: string);
    function GetContent: string;
    procedure SetContent(const AContent: string);
    procedure SetHeader(const AName, AValue: string);
    procedure SetCookie(const AName, AValue, ADomain, APath: string; const AExpiration: TDateTime; const ASecure: Boolean);
    procedure RedirectTo(const AURL: string);
    // IMARSResponse implementation --------------------------------------------
  end;

implementation

uses MARS.Core.Utils;

{ TMARSWebResponseDummy }

constructor TMARSWebResponseDummy.Create;
begin
  inherited Create;
  FContent := '';
  FContentType := '';
  FContentEncoding := '';
  FContentLength := 0;
  FContentStream := TMemoryStream.Create;
  FStatusCode := 200;
  FReasonString := '';
  FHeaders := TStringList.Create;
end;

destructor TMARSWebResponseDummy.Destroy;
begin
  FreeAndNil(FHeaders);
  FreeAndNil(FContentStream);
  inherited;
end;

function TMARSWebResponseDummy.GetContent: string;
begin
  Result := FContent;
  if Result.IsEmpty and Assigned(FContentStream) and (FContentStream.Size > 0) then
    Result := StreamToString(FContentStream);

end;

function TMARSWebResponseDummy.GetContentEncoding: string;
begin
  Result := FContentEncoding;
end;

function TMARSWebResponseDummy.GetContentLength: Integer;
begin
  Result := FContentLength;
end;

function TMARSWebResponseDummy.GetContentStream: TStream;
begin
  Result := FContentStream;
end;

function TMARSWebResponseDummy.GetContentType: string;
begin
  Result := FContentType;
end;

function TMARSWebResponseDummy.GetReasonString: string;
begin
  Result := FReasonString;
end;

function TMARSWebResponseDummy.GetStatusCode: Integer;
begin
  Result := FStatusCode;
end;

procedure TMARSWebResponseDummy.RedirectTo(const AURL: string);
begin
  //TODO
end;

procedure TMARSWebResponseDummy.SetContent(const AContent: string);
begin
  FContent := AContent;
end;

procedure TMARSWebResponseDummy.SetContentEncoding(
  const AContentEncoding: string);
begin
  FContentEncoding := AContentEncoding;
end;

procedure TMARSWebResponseDummy.SetContentLength(const ALength: Integer);
begin
  FContentLength := ALength;
end;

procedure TMARSWebResponseDummy.SetContentStream(const AContentStream: TStream);
begin
  if Assigned(FContentStream) then
    FreeAndNil(FContentStream);
  FContentStream := AContentStream;
end;

procedure TMARSWebResponseDummy.SetContentType(const AContentType: string);
begin
  FContentType := AContentType;
end;

procedure TMARSWebResponseDummy.SetCookie(const AName, AValue, ADomain,
  APath: string; const AExpiration: TDateTime; const ASecure: Boolean);
begin
  //TODO
end;

procedure TMARSWebResponseDummy.SetHeader(const AName, AValue: string);
begin
  FHeaders.Values[AName] := AValue;
end;

procedure TMARSWebResponseDummy.SetReasonString(const AReasonString: string);
begin
  FReasonString := AReasonString;
end;

procedure TMARSWebResponseDummy.SetStatusCode(const AStatusCode: Integer);
begin
  FStatusCode := AStatusCode;
end;

end.
