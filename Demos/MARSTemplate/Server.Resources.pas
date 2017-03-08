(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes

  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.JSON
  , MARS.Core.Response

  , MARS.Core.Token.Resource
//  , MARS.Core.Invocation


  , Data.DB, FireDAC.Comp.Client, FireDAC.Phys.FB
  , MARS.Data.MessageBodyWriters
  , MARS.Data.FireDAC

  , SimpleRecord
  , Records.ReadersAndWriters
  ;

type
  [Path('helloworld')]
  THelloWorldResource = class
  protected
    [Context] FD: TMARSFireDAC;
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;

    [GET, Path('Record'), Produces(TMediaType.APPLICATION_JSON)]
    function TestRecord: TSimpleRecord;

    [POST, Path('Record'), Consumes(TMediaType.APPLICATION_JSON)]
    function TestInput([BodyParam] ASimpleRecord: TSimpleRecord): string;

    [GET, Path('test'), Produces(TMediaType.APPLICATION_JSON)]
    function Test: TDataSet;

    [GET, Path('array'), Produces(TMediaType.APPLICATION_JSON)]
    function TestArray: TJSONArray;
  end;

  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  end;

implementation

uses
    MARS.Core.Registry
//    , Web.ReqMulti, Web.ReqFiles
  ;

{ THelloWorldResource }

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello World!';
end;

//function THelloWorldResource.Test2: string;
//var
//  LFile: TWebRequestFile;
//  LFS: TFileStream;
//  LIndex: Integer;
//begin
//
//  Result := AR.Request.Files.Count.ToString;
//  for LIndex := 0 to AR.Request.Files.Count-1 do
//  begin
//    LFile := AR.Request.Files.Items[LIndex] as TWebRequestFile;
//
//    LFS := TFileStream.Create('C:\temp\test_upload\' + LFile.FileName, fmCreate or fmOpenWrite or fmShareDenyWrite);
//    try
//      LFS.CopyFrom(LFile.Stream, 0);
//    finally
//      LFS.Free;
//    end;
//  end;
//end;

function THelloWorldResource.Test: TDataSet;
begin
  Result := FD.CreateQuery('select RD.*'
    + ', ''!Token_UserName'' aaa '
//    + ', ''&Token_Token'' TKN '
//    + ', ''Ciao sono &Token_Issuer'' ISS '
//    + ', ''&Token_IsVerified'' ISV '
//    + ', ''&Token_IsExpired'' ISE '
//    + ', ''&Token_Expiration'' EXP '
//    + ', ''&Token_HasRole_admin'' IS_ADM '
//    + ', ''&Token_Claim_iss'' CL_ISS '
//    + ', ''&Token_Claim_roles'' CL_roles '
    + 'from RDB$DATABASE RD'
  );
end;

function THelloWorldResource.TestArray: TJSONArray;
begin
  Result := TJSONArray.Create;
  Result.Add('Andrea');
  Result.Add(123);
  Result.Add(TJSONObject.Create(TJSONPair.Create('Cognome', 'Magni')));
end;

function THelloWorldResource.TestInput(ASimpleRecord: TSimpleRecord): string;
begin
  Result := 'Ciao, mi chiamo ' + ASimpleRecord.NameAndSurname + ' e ho '
    + ASimpleRecord.Age.ToString + ' anni, vuol dire '
    + ASimpleRecord.Eta.ToString + ' anni';
end;

function THelloWorldResource.TestRecord: TSimpleRecord;
begin
  Result := TSimpleRecord.Create('Andrea', 'Magni', EncodeDate(1982, 5, 24));
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;
end.
