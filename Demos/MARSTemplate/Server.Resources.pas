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


//  , Data.DB, FireDAC.Comp.Client, FireDAC.Phys.FB
//  , MARS.Data.MessageBodyWriters
  , MARS.Data.FireDAC

  , SimpleRecord
  , Records.ReadersAndWriters
  ;

type
  [Path('helloworld')]
  THelloWorldResource = class
  protected
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;

    [GET, Path('Record'), Produces(TMediaType.APPLICATION_JSON)]
    function TestRecord: TSimpleRecord;

    [POST, Path('Record'), Consumes(TMediaType.APPLICATION_JSON)]
    function TestInput([BodyParam] ASimpleRecord: TSimpleRecord): string;
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
