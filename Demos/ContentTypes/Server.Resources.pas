(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
  Home: https://github.com/andrea-magni/MARS

*)
unit Server.Resources;

interface

uses
  SysUtils, Classes, DB, HttpApp

  , FireDAC.Comp.Client

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


    [GET, Path('/dataset1')
    , Produces(TMediaType.APPLICATION_XML)
    , Produces(TMediaType.APPLICATION_JSON)]
    function DataSet1: TDataSet;

    [GET, Path('/dataset2')
    , Produces(TMediaType.APPLICATION_XML)
    , Produces(TMediaType.APPLICATION_JSON)]
    function DataSet2: TFDMemTable;

    [GET, Path('/dataset3')
    , Produces(TMediaType.APPLICATION_JSON)]
    function DataSet3: TDataset;


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
  Result := TFileStream.Create('..\SampleData\Sample.jpg', fmOpenRead or fmShareDenyWrite);
end;

function THelloWorldResource.JSON1: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('Hello', 'World');
end;

function THelloWorldResource.PdfDocument: TStream;
begin
  Result := TFileStream.Create('..\SampleData\Sample.pdf', fmOpenRead or fmShareDenyWrite);
end;

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello World!';
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;


end.
