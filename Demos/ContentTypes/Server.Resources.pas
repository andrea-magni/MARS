(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

{$I MARS.inc}

interface

uses
  SysUtils, Classes, DB, HttpApp

{$ifdef DelphiXE3_UP}
  , FireDAC.Comp.Client // remove this line if you do not have FireDAC installed
{$endif}

  , MARS.Core.Attributes
  , MARS.Core.MediaType

  , MARS.Core.JSON
  ;

type
  [Path('helloworld')]
  THelloWorldResource = class
  private
  protected
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;

    [GET, Path('/html'), Produces(TMediaType.TEXT_HTML)]
    function HtmlDocument: string;

    [GET, Path('/json'), Produces(TMediaType.APPLICATION_JSON)]
    function JSON1: TJSONObject;

    [GET, Path('/json2'), Produces(TMediaType.APPLICATION_JSON)]
    function JSON2: TJSONRawString;

    [GET, Path('/bool'), Produces(TMediaType.WILDCARD)]
    function TestBool: Boolean;

    [GET, Path('/int')]
    function TestInt: Integer;

    [GET, Path('/pi')]
    function TestPi: Double;

    [GET, Path('/string')]
    function TestString: string;

    [GET, Path('/jpeg'), Produces('image/jpg')]
    function JpegImage: TStream;

    [GET, Path('/pdf'), Produces('application/pdf')]
    function PdfDocument: TStream;

    [GET, Path('/stream'), Produces(TMediaType.APPLICATION_OCTET_STREAM)]
    function GetStream([Context] Response: TWebResponse): TStream;

    [
     GET
     , Path('/attachment'), Produces(TMediaType.APPLICATION_OCTET_STREAM)
     , CustomHeader('Content-Disposition', 'attachment; filename="ThisIsMyFile.exe"')
     ]
    function GetStreamAsAttachment(): TStream;

    [GET, Path('/echo/{*}')]
    function Echo([PathParam('*')] AllStrings: string): TJSONObject;

    [GET, Path('/dataset1')
    , Produces(TMediaType.APPLICATION_XML)
    , Produces(TMediaType.APPLICATION_JSON)]
    function DataSet1: TDataSet;

{$ifdef DelphiXE3_UP}
    [GET, Path('/dataset2')]
    function DataSet2: TFDMemTable;

    [GET, Path('/dataset3')
    , Produces(TMediaType.APPLICATION_JSON)]
    function DataSet3: TDataset;
{$endif}

  end;

implementation

uses
    MARS.Core.Registry
    , DBClient
  ;


{ THelloWorldResource }

function THelloWorldResource.DataSet1: TDataSet;
var
  LCDS: TClientDataSet;
begin
  LCDS := TClientDataSet.Create(nil);
  LCDS.FieldDefs.Add('Name', ftString, 100);
  LCDS.FieldDefs.Add('Surname', ftString, 100);
  LCDS.CreateDataSet;
  LCDS.Open;

  Result := LCDS;
  Result.AppendRecord(['Andrea', 'Magni']);
  Result.AppendRecord(['Paolo', 'Verdi']);
  Result.AppendRecord(['Mario', 'Bianchi']);
  Result.AppendRecord(['Luca', 'Viola']);
end;

{$ifdef DelphiXE3_UP}
function THelloWorldResource.DataSet2: TFDMemTable;
begin
  Result := TFDMemTable.Create(nil);
  Result.FieldDefs.Add('Name', ftString, 100);
  Result.FieldDefs.Add('Surname', ftString, 100);
  Result.CreateDataSet;
  Result.AppendRecord(['Andrea', 'Magni']);
  Result.AppendRecord(['Paolo', 'Verdi']);
  Result.AppendRecord(['Mario', 'Bianchi']);
  Result.AppendRecord(['Luca', 'Viola']);
end;

function THelloWorldResource.DataSet3: TDataset;
begin
  Result := DataSet2;
end;
{$endif}

function THelloWorldResource.Echo(AllStrings: string): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.WriteStringValue('echo', AllStrings);
end;

function THelloWorldResource.GetStream(Response: TWebResponse): TStream;
begin
  Result := TFileStream.Create(ParamStr(0), fmOpenRead or fmShareDenyNone);
  Response.CustomHeaders.Values['Content-Disposition'] := 'attachment; filename="ThisIsMyServer.exe"';
end;

function THelloWorldResource.GetStreamAsAttachment: TStream;
begin
  Result := TFileStream.Create(ParamStr(0), fmOpenRead or fmShareDenyNone);
end;

function THelloWorldResource.HtmlDocument: string;
begin
  Result := '<html><body>'
    + '<h2>This is HTML!</h2>'
    + '<p>And this is a test.</p>'
    + '</body></html>';
end;

function THelloWorldResource.JpegImage: TStream;
begin
  Result := TFileStream.Create('..\..\SampleData\Sample.jpg', fmOpenRead or fmShareDenyWrite);
end;

function THelloWorldResource.JSON1: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('Hello', 'World');
end;

function THelloWorldResource.JSON2: TJSONRawString;
begin
  Result := '{ "name": "Andrea", "surname": "Magni" }';
end;

function THelloWorldResource.PdfDocument: TStream;
begin
  Result := TFileStream.Create('..\..\SampleData\Sample.pdf', fmOpenRead or fmShareDenyWrite);
end;

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello World!';
end;

function THelloWorldResource.TestBool: Boolean;
begin
  Result := Random(100) < 50;
end;

function THelloWorldResource.TestInt: Integer;
begin
  Result := Random(100);
end;

function THelloWorldResource.TestPi: Double;
begin
  Result := Pi;
end;

function THelloWorldResource.TestString: string;
begin
  Result := 'MARS REST Server';
end;

initialization
  Randomize;
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;


end.
