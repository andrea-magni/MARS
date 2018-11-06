(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes, Data.DB

  , MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.JSON, MARS.Core.Response
  , MARS.Core.URL

  , MARS.Core.Token.Resource //, MARS.Core.Token

  , MARS.Data.FireDAC, FireDAC.Phys.FB, FireDAC.Comp.Client, FireDAC.Comp.DataSet

  , Model.Types
;

type
  [Path('helloworld')]
  THelloWorldResource = class
  protected
    [Context] FD: TMARSFireDAC;
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;

    [GET, Path('echo/{what}'), Produces(TMediaType.TEXT_PLAIN)]
    function EchoString([PathParam] what: string): string;

    [GET, Path('reverse/{what}'), Produces(TMediaType.TEXT_PLAIN)]
    function ReverseString([PathParam] what: string): string;


    [GET, Path('json'), Produces(TMediaType.APPLICATION_JSON)]
    function SomeJSON: TJSONObject;

    [GET, Path('record'), Produces(TMediaType.APPLICATION_JSON)]
    function SomeRecord: TMyRecord;

    [POST, Path('recordAge'), Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
    function WhatsMyAge([BodyParam] ARecord: TMyRecord): TAge;

    [GET, Path('recordarray'), Produces(TMediaType.APPLICATION_JSON)]
    function SomeArrayOfRecords: TArray<TMyRecord>;

    [GET, Path('object'), Produces(TMediaType.APPLICATION_JSON)]
    function SomeObject: TMyObject;

    [GET, Path('objectarray'), Produces(TMediaType.APPLICATION_JSON)]
    function SomeArrayOfObject: TArray<TMyObject>;

    [GET, Path('dataset'), Produces(TMediaType.APPLICATION_JSON)]
    function SomeDataset: TDataset;

    [GET, Path('datasetFromDB'), Produces(TMediaType.APPLICATION_JSON)]
    function SomeDatasetFromDB: TDataSet;

    [GET, Path('queryFromDB'), Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON_FireDAC)]
    function SomeQueryFromDB: TFDQuery;

    [GET, Path('singleEmployee/{empno}'), Produces(TMediaType.APPLICATION_JSON)]
    function SingleEmployee: TMyEmployeeRecord;
  end;

  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  end;

implementation

uses
  StrUtils
, MARS.Rtti.Utils
, MARS.Core.Registry
;

{ THelloWorldResource }

function THelloWorldResource.EchoString(what: string): string;
begin
  Result := what;
end;

function THelloWorldResource.ReverseString(what: string): string;
begin
  Result := StrUtils.ReverseString(what);
end;

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello World!';
end;

function THelloWorldResource.SingleEmployee: TMyEmployeeRecord;
begin
  TRecord<TMyEmployeeRecord>.FromDataSet(Result
    , FD.Query('select * from EMPLOYEE where EMP_NO = :PathParam_empno')
  );
end;

function THelloWorldResource.SomeJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.WriteStringValue('message', 'Hello, World!');
  Result.WriteDateTimeValue('timestamp', Now);
end;

function THelloWorldResource.SomeRecord: TMyRecord;
begin
  Result := TMyRecord.Create('Andrea', 'Magni', EncodeDate(1982, 05, 24));
end;

function THelloWorldResource.WhatsMyAge(ARecord: TMyRecord): TAge;
begin
  Result := TAge.Create(
    ARecord.Firstname + ' ' + ARecord.Lastname
  , ARecord.DateOfBirth
  );
end;

function THelloWorldResource.SomeArrayOfRecords: TArray<TMyRecord>;
begin
  Result := [
    TMyRecord.Create('Andrea', 'Magni', EncodeDate(1982, 05, 24))
  , TMyRecord.Create('Michael', 'Schumacher', EncodeDate(1969, 1, 3))
  , TMyRecord.Create('Sebastian', 'Vettel', EncodeDate(1987, 7, 3))
  ]
end;

function THelloWorldResource.SomeDataset: TDataset;
var
  LMemTable: TFDMemTable;
begin
  LMemTable := TFDMemTable.Create(nil);
  // field definition
  LMemTable.FieldDefs.Add('Firstname', ftString, 100);
  LMemTable.FieldDefs.Add('Lastname', ftString, 100);
  LMemTable.FieldDefs.Add('DateOfBirth', ftDate);
  LMemTable.CreateDataSet;
  // data
  LMemTable.AppendRecord(['Andrea', 'Magni', EncodeDate(1982, 05, 24)]);
  LMemTable.AppendRecord(['Michael', 'Schumacher', EncodeDate(1969, 1, 3)]);
  LMemTable.AppendRecord(['Sebastian', 'Vettel', EncodeDate(1987, 7, 3)]);

  Result := LMemTable;
end;

function THelloWorldResource.SomeDatasetFromDB: TDataSet;
begin
  Result := FD.Query('select * from EMPLOYEE order by EMP_NO');
end;

function THelloWorldResource.SomeObject: TMyObject;
begin
  Result := TMyObject.Create('Andrea', 'Magni', EncodeDate(1982, 05, 24));
end;

function THelloWorldResource.SomeArrayOfObject: TArray<TMyObject>;
begin
  Result := [
    TMyObject.Create('Andrea', 'Magni', EncodeDate(1982, 05, 24))
  , TMyObject.Create('Michael', 'Schumacher', EncodeDate(1969, 1, 3))
  , TMyObject.Create('Sebastian', 'Vettel', EncodeDate(1987, 7, 3))
  ]
end;

function THelloWorldResource.SomeQueryFromDB: TFDQuery;
begin
  Result := FD.Query('select * from EMPLOYEE order by EMP_NO');
  Result.Name := 'employees';
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;
end.
