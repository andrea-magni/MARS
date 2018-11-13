(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes

  , MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.JSON, MARS.Core.Response
  , MARS.Core.URL, MARS.Core.Utils

  , MARS.Core.Token.Resource //, MARS.Core.Token
;

type
  TMyResult = record
    JSONData: string;
    PersonName: string;
    FileName: string;
    FileSize: Integer;
  end;

  TPerson = record
    name: string;
    surname: string;
  end;

  [Path('helloworld')]
  THelloWorldResource = class
  protected
  public
    // provides a simple HTML form to test other methods.
    // http://localhost:8080/rest/default/helloworld/1 (or 2, 3 and 4 to address the corresponding POST method)
    [GET, Path('{which}'), Produces(TMediaType.TEXT_HTML)]
    function SayHelloWorld([PathParam] which: string): string;


    // first version uses TArray<TFormParams> as input parameter
    [POST, Path('1'), Consumes(TMediaType.MULTIPART_FORM_DATA), Produces(TMediaType.APPLICATION_JSON)]
    function StoreDataAndFile([FormParams] AParams: TArray<TFormParam>): TMyResult;

    // second version defines specific (name binding) params as method arguments
    [POST, Path('2'), Consumes(TMediaType.MULTIPART_FORM_DATA), Produces(TMediaType.APPLICATION_JSON)]
    function StoreDataAndFile2([FormParam('json')] AJSON: TFormParam;
      [FormParam('image')] AImage: TFormParam): TMyResult;

    // third version, triggers MBR mechanism for JSON (automatic deserialization)
    [POST, Path('3'), Consumes(TMediaType.MULTIPART_FORM_DATA), Produces(TMediaType.APPLICATION_JSON)]
    function StoreDataAndFile3(
      [FormParam('json'), Consumes(TMediaType.Application_JSON)] AJSON: TJSONObject;
      [FormParam('image')] AImage: TFormParam): TMyResult;

    // fourth version, triggers MBR mechanism for record (automatic deserialization)
    [POST, Path('4'), Consumes(TMediaType.MULTIPART_FORM_DATA), Produces(TMediaType.APPLICATION_JSON)]
    function StoreDataAndFile4(
      [FormParam('json'), Consumes(TMediaType.Application_JSON)] ARecord: TPerson;
      [FormParam('image')] AImage: TFormParam): TMyResult;

  end;

  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  end;

implementation

uses
    MARS.Core.Registry
;

{ THelloWorldResource }

function THelloWorldResource.SayHelloWorld(which: string): string;
begin
  Result :=
    '<html>' + sLineBreak
  + '<body>' + sLineBreak
  + '  <h1>Multipart form-data demo</h1>' + sLineBreak
  + '  <form action="./' + which + '" method="POST" enctype="multipart/form-data">' + sLineBreak
  + '    <input type="text" name="json" value=''{"name": "Andrea", "surname": "Magni"}''/> <br/>' + sLineBreak
  + '    <input type="file" name="image" />  <br/>' + sLineBreak
  + '    <input type="submit" value="Submit" />' + sLineBreak
  + '  </form>' + sLineBreak
  + '</body>' + sLineBreak
  + '</html>';
end;

function THelloWorldResource.StoreDataAndFile(
  AParams: TArray<TFormParam>): TMyResult;
var
  LParam: TFormParam;
  LObj: TJSONObject;
begin
  for LParam in AParams do
  begin
    if SameText(LParam.FieldName, 'image') and LParam.IsFile then
    begin
      Result.FileSize := Length(LParam.AsFile.Bytes);
      Result.FileName := LParam.AsFile.FileName;
    end
    else if SameText(LParam.FieldName, 'json') then
    begin
      Result.JSONData := LParam.Value.AsString;

      LObj := TJSONObject.ParseJSONValue(Result.JSONData) as TJSONObject;
      try
        Result.PersonName := LObj.ReadStringValue('name');
      finally
        LObj.Free;
      end;

    end;
  end;
end;

function THelloWorldResource.StoreDataAndFile2(AJSON: TFormParam;
  AImage: TFormParam): TMyResult;
var
  LObj: TJSONObject;
begin
  Result.JSONData := AJSON.Value.ToString;
  LObj := TJSONObject.ParseJSONValue(Result.JSONData) as TJSONObject;
  try
    Result.PersonName := LObj.ReadStringValue('name');
  finally
    LObj.Free;
  end;

  Result.FileName := AImage.AsFile.FileName;
  Result.FileSize := Length(AImage.AsFile.Bytes);
end;


function THelloWorldResource.StoreDataAndFile3(AJSON: TJSONObject;
  AImage: TFormParam): TMyResult;
begin
  Result.JSONData := AJSON.ToString;
  Result.PersonName := AJSON.ReadStringValue('name');
  Result.FileName := AImage.AsFile.FileName;
  Result.FileSize := Length(AImage.AsFile.Bytes);
end;

function THelloWorldResource.StoreDataAndFile4(ARecord: TPerson;
  AImage: TFormParam): TMyResult;
var
  LObj: TJSONObject;
begin
  LObj := TJSONObject.RecordToJSON<TPerson>(ARecord);
  try
    Result.JSONData := LObj.ToString;
  finally
    LObj.Free;
  end;

  Result.PersonName := ARecord.name;
  Result.FileName := AImage.AsFile.FileName;
  Result.FileSize := Length(AImage.AsFile.Bytes);
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;
end.
