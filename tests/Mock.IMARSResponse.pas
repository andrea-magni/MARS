unit Mock.IMARSResponse;

interface

uses
  Classes, SysUtils
, MARS.Core.RequestAndResponse.Interfaces
;

type
  TMARSResponseMock = class(TInterfacedObject, IMARSResponse)
  private
    FContentStream: TStream;
    FContentStreamSet: Boolean;
    FContentType: string;
    FContentLength: Integer;
    FContentEncoding: string;
    FStatusCode: Integer;
    FReasonString: string;
    FContent: string;
    FContentSet: Boolean;
    FHeaders: TMARSHeaders;
    FCookies: TMARSCookies;
    FRedirectTo: string;
  protected
  public
    // IMARSResponse ------------------------------------------------ BEGIN ----
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
    // IMARSResponse -------------------------------------------------- END ----

    constructor Create();
    destructor Destroy; override;
  end;


implementation

uses
  MARS.Core.Utils
;

{ TMARSResponseMock }

constructor TMARSResponseMock.Create;
begin
  inherited Create;
  FStatusCode := 200;
  FContentStreamSet := False;
  FContentSet := False;
end;

destructor TMARSResponseMock.Destroy;
begin
  if Assigned(FContentStream) then
    FreeAndNil(FContentStream);
  inherited;
end;

function TMARSResponseMock.GetContent: string;
begin
  Result := FContent;
  if (Result = '') and FContentStreamSet then
    Result := StreamToString(FContentStream, TEncoding.UTF8);
end;

function TMARSResponseMock.GetContentEncoding: string;
begin
  Result := FContentEncoding;
end;

function TMARSResponseMock.GetContentLength: Integer;
begin
  Result := FContentLength;
end;

function TMARSResponseMock.GetContentStream: TStream;
begin
  Result := FContentStream;
end;

function TMARSResponseMock.GetContentType: string;
begin
  Result := FContentType;
end;

function TMARSResponseMock.GetReasonString: string;
begin
  Result := FReasonString;
end;

function TMARSResponseMock.GetStatusCode: Integer;
begin
  Result := FStatusCode;
end;

procedure TMARSResponseMock.RedirectTo(const AURL: string);
begin
  FRedirectTo := AURL;
end;

procedure TMARSResponseMock.SetContent(const AContent: string);
begin
  FContent := AContent;
  FContentSet := True;
end;

procedure TMARSResponseMock.SetContentEncoding(const AContentEncoding: string);
begin
  FContentEncoding := AContentEncoding;
end;

procedure TMARSResponseMock.SetContentLength(const ALength: Integer);
begin
  FContentLength := ALength;
end;

procedure TMARSResponseMock.SetContentStream(const AContentStream: TStream);
begin
  FContentStream := AContentStream;
  FContentStreamSet := True;
end;

procedure TMARSResponseMock.SetContentType(const AContentType: string);
begin
  FContentType := AContentType;
end;

procedure TMARSResponseMock.SetCookie(const AName, AValue, ADomain,
  APath: string; const AExpiration: TDateTime; const ASecure: Boolean);
begin
  var LFound := False;
  for var LIndex := Low(FCookies) to High(FCookies) do
  begin
    if SameText(FCookies[LIndex].Name, AName) then
    begin
      FCookies[LIndex].Value := AValue;
      LFound := True;
    end;
  end;

  if not LFound then
    FCookies := FCookies + [TMARSCookie.Create(AName, AValue)];
end;

procedure TMARSResponseMock.SetHeader(const AName, AValue: string);
begin
  var LFound := False;
  for var LIndex := Low(FHeaders) to High(FHeaders) do
  begin
    if SameText(FHeaders[LIndex].Name, AName) then
    begin
      FHeaders[LIndex].Value := AValue;
      LFound := True;
    end;
  end;

  if not LFound then
    FHeaders := FHeaders + [TMARSHeader.Create(AName, AValue)];
end;

procedure TMARSResponseMock.SetReasonString(const AReasonString: string);
begin
  FReasonString := AReasonString;
end;

procedure TMARSResponseMock.SetStatusCode(const AStatusCode: Integer);
begin
  FStatusCode := AStatusCode;
end;

end.
