unit Tests.Client.Resources;

interface

uses
  SysUtils, Classes
, FireDAC.Comp.Client, FireDAC.Comp.DataSet
, MARS.OpenAPI.Elements, MARS.Metadata.Attributes
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.URL
, MARS.Data.FireDAC.Utils
, MARS.Core.JSON, MARS.Core.Response
, MARS.Core.RequestAndResponse.Interfaces
//, MARS.Core.Token
;

type
  TRequestDump = record
    Accept: string;
    ContentType: string;
    // the X-Probe header, (absent) when not sent
    XProbe: string;
  end;

  // a record with a date (Tests.JSONDateIsUTC)
  TDatedRecord = record
    Name: string;
    At: TDateTime;
  end;

  TLoginData = record
    username: string;
    password: string;
  end;

  TLoginResult = record
    name: string;
    Token: string;
  end;

  [Path('test')]
  TTestResource = class
  protected
    [Context] FRequest: IMARSRequest;
  public
    [GET, Path('helloworld')]
    function GetHelloWorld: string;

    [GET, Path('requestDump')]
    function GetRequestDump: TRequestDump;

    // HTTP QUERY: the filter travels in the body
    [QUERY, Path('search')]
    function Search([BodyParam] const AFilter: string): string;

    // client log tests
    [POST, Path('login'), Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
    function Login([BodyParam] const AData: TLoginData): TLoginResult;

    [GET, Path('notfound'), Produces(TMediaType.TEXT_PLAIN)]
    function GetNotFound: string;

    [GET, Path('big'), Produces(TMediaType.TEXT_PLAIN)]
    function GetBig: string;

    // a form (TMARSClientResourceFormUrlEncoded)
    [POST, Path('form'), Consumes(TMediaType.APPLICATION_FORM_URLENCODED_TYPE), Produces(TMediaType.TEXT_PLAIN)]
    function PostForm([FormParam('name')] const AName: string): string;
    [PUT, Path('form'), Consumes(TMediaType.APPLICATION_FORM_URLENCODED_TYPE), Produces(TMediaType.TEXT_PLAIN)]
    function PutForm([FormParam('name')] const AName: string; [FormParam('city')] const ACity: string): string;

    // the same record, its date as UTC or as a local time (#111)
    [GET, Path('dates/utc'), JSONDateIsUTC, Produces(TMediaType.APPLICATION_JSON)]
    function GetDateUTC: TDatedRecord;
    [GET, Path('dates/local'), JSONDateIsUTC(False), Produces(TMediaType.APPLICATION_JSON)]
    function GetDateLocal: TDatedRecord;

    // COM in the thread of the request: 'initialized' when the engine did it (COM.Initialize)
    [GET, Path('com'), Produces(TMediaType.TEXT_PLAIN)]
    function GetCOM: string;
  end;


  // the documentation of the application with Stoplight Elements (Tests.OpenAPIElements)
  [Path('docs'), MetaVisible(False)]
  TDocsResource = class(TOpenAPIElementsResource);

  // the dates of a database table, for the FireDAC client components (Tests.Client.FireDAC): the
  // datasets in the native format, their deltas applied as a FireDAC server does, plain JSON
  [Path('fddates')]
  TFDDatesResource = class
  public
    // a new DATES table with two rows
    [POST, Path('reset')]
    procedure Reset;

    [GET, Produces(TMediaType.APPLICATION_JSON_FireDAC)]
    function GetDates: TFDDataSet;

    [GET, Path('plain'), Produces(TMediaType.APPLICATION_JSON)]
    function GetPlain: TFDDataSet;

    [POST, Consumes(TMediaType.APPLICATION_JSON_FireDAC), Produces(TMediaType.APPLICATION_JSON)]
    function ApplyDeltas([BodyParam] const ADeltas: TArray<TFDMemTable>): TArray<TMARSFDApplyUpdatesRes>;
  end;

implementation

uses
  System.IOUtils, System.DateUtils, Variants
{$IFDEF MSWINDOWS}, Winapi.Windows, Winapi.ActiveX{$ENDIF}
, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Def, FireDAC.Stan.Async, FireDAC.DApt, FireDAC.DApt.Intf
, FireDAC.Phys.SQLite
, MARS.Core.Registry, MARS.Core.Exceptions
, MARS.Data.FireDAC
;

var
  GDatesConnection: TFDConnection = nil;

function DatesQuery(const AName: string): TFDQuery;
begin
  if not Assigned(GDatesConnection) then
  begin
    GDatesConnection := TFDConnection.Create(nil);
    GDatesConnection.DriverName := 'SQLite';
    GDatesConnection.Params.Database := TPath.Combine(TPath.GetTempPath, 'MARSTestsFDDates.sqlite');
    GDatesConnection.LoginPrompt := False;
  end;
  Result := TFDQuery.Create(nil);
  Result.Name := AName;
  Result.Connection := GDatesConnection;
  Result.SQL.Text := 'select * from DATES order by ID';
end;

{ TTestResource }

function TTestResource.GetHelloWorld: string;
begin
  Result := 'Hello World!';
end;

function TTestResource.Login(const AData: TLoginData): TLoginResult;
begin
  Result.name := AData.username;
  Result.Token := 'server-issued-token';
end;

function TTestResource.GetNotFound: string;
begin
  raise EMARSHttpException.Create('nothing here', 404);
end;

function TTestResource.PostForm(const AName: string): string;
begin
  Result := 'form: ' + AName;
end;

function TTestResource.PutForm(const AName, ACity: string): string;
begin
  Result := 'put: ' + AName + ', ' + ACity;
end;

function TTestResource.GetDateUTC: TDatedRecord;
begin
  Result.Name := 'utc';
  Result.At := EncodeDateTime(2026, 7, 15, 9, 30, 0, 0);
end;

function TTestResource.GetDateLocal: TDatedRecord;
begin
  Result.Name := 'local';
  Result.At := EncodeDateTime(2026, 7, 15, 9, 30, 0, 0);
end;

function TTestResource.GetCOM: string;
{$IFDEF MSWINDOWS}
var
  LResult: HRESULT;
begin
  // S_FALSE: COM was already initialized in this thread (multithreaded apartment)
  LResult := CoInitializeEx(nil, COINIT_MULTITHREADED);
  if Succeeded(LResult) then
    CoUninitialize;
  if LResult = S_FALSE then
    Result := 'initialized'
  else
    Result := 'not initialized';
end;
{$ELSE}
begin
  Result := 'not Windows';
end;
{$ENDIF}

function TTestResource.GetBig: string;
begin
  Result := StringOfChar('x', 100000);
end;

function TTestResource.Search(const AFilter: string): string;
begin
  Result := 'found: ' + AFilter;
end;

function TTestResource.GetRequestDump: TRequestDump;
begin
  Result.Accept := FRequest.Accept;
  Result.ContentType := FRequest.GetHeaderParamValue('ContentType');
  if FRequest.GetHeaderParamIndex('X-Probe') < 0 then
    Result.XProbe := '(absent)'
  else
    Result.XProbe := FRequest.GetHeaderParamValue('X-Probe');
end;

{ TFDDatesResource }

procedure TFDDatesResource.Reset;
var
  LQuery: TFDQuery;
begin
  LQuery := DatesQuery('dates');
  try
    GDatesConnection.ExecSQL('drop table if exists DATES');
    GDatesConnection.ExecSQL('create table DATES (ID integer primary key, NAME varchar(20), BORN date, '
      + 'AT datetime, ENDDATE date, QTY integer, PRICE float)');
    LQuery.Open;
    LQuery.AppendRecord([1, 'Ann', EncodeDate(1990, 5, 1), EncodeDateTime(2026, 7, 15, 9, 30, 0, 0), Null,
      Null, Null]);
    // a zero date, zero numbers: not nulls
    LQuery.AppendRecord([2, 'Bob', EncodeDate(1985, 12, 31), EncodeDateTime(2026, 10, 12, 18, 0, 0, 0), TDateTime(0),
      0, 0.0]);
  finally
    LQuery.Free;
  end;
end;

function TFDDatesResource.GetDates: TFDDataSet;
begin
  Result := DatesQuery('dates');
  try
    TFDQuery(Result).Open;
  except
    Result.Free;
    raise;
  end;
end;

function TFDDatesResource.GetPlain: TFDDataSet;
begin
  Result := GetDates;
end;

function TFDDatesResource.ApplyDeltas(const ADeltas: TArray<TFDMemTable>): TArray<TMARSFDApplyUpdatesRes>;
var
  LQuery: TFDQuery;
  LAdapter: TFDTableAdapter;
  LTable: TMARSFDMemTable;
begin
  // as TMARSFireDAC.ApplyUpdates
  Result := [];
  for var LDelta in ADeltas do
  begin
    LQuery := DatesQuery(LDelta.Name);
    LAdapter := TFDTableAdapter.Create(nil);
    LTable := TMARSFDMemTable.Create(nil);
    try
      LAdapter.SelectCommand := LQuery.Command;
      LAdapter.UpdateTableName := 'DATES';
      LTable.Name := LDelta.Name;
      LTable.ResourceOptions.StoreItems := [siMeta, siDelta];
      LTable.CachedUpdates := True;
      LTable.Adapter := LAdapter;
      LTable.Data := LDelta.Data;
      LTable.ApplyUpdates(-1);
      Result := Result + [LTable.ApplyUpdatesRes];
    finally
      LTable.Free;
      LAdapter.Free;
      LQuery.Free;
    end;
  end;
end;

initialization
  MARSRegister(TTestResource);
  MARSRegister(TFDDatesResource);
  MARSRegister(TDocsResource);

finalization
  FreeAndNil(GDatesConnection);

end.
