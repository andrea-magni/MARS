(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes, HttpApp

  , MARS.Core.Attributes
  , MARS.Core.MediaType
;

type
  [Path('helloworld'), Produces(TMediaType.TEXT_PLAIN)]
  THelloWorldResource = class
  protected
    function GetIniFileName: string;
    procedure SaveToFile(const ALang, AText: string);
    function LoadFromFile(const ALang: string; const ADefault: string): string;
  public
    [GET, Path('/')]
    function SayOnce([QueryParam] lang: string): string;

    [GET, Path('/{times}')]
    function Say([PathParam] times: Integer; [QueryParam] lang: string): string;

    [POST, Path('/{lang}')]
    function Store([PathParam] lang: string; [BodyParam] body: string): string;
  end;

  [Path('simpleform')]
  TSimpleFormResource = class
  protected
  public
    [GET]
    function GetHost([HeaderParam] Host: string): string;
    [POST]
    function GetFullName([FormParam] name: string; [FormParam] surname: string): string;
  end;

  [Path('multifiles')]
  TMultiFilesResource = class
  protected
    [Context] Request: TWebRequest;
  public
    [POST]
    function HashFiles(): string;
  end;


implementation

uses
    StrUtils, IniFiles
  , Web.ReqMulti, Web.ReqFiles
  , MARS.Core.Registry
;

{ THelloWorldResource }

function THelloWorldResource.GetIniFileName: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'HelloWorld.ini';
end;

function THelloWorldResource.LoadFromFile(const ALang,
  ADefault: string): string;
var
  LIniFile: TIniFile;
begin
  LIniFile := TIniFile.Create(GetIniFileName);
  try
    Result := LIniFile.ReadString('HelloWorld', ALang, ADefault);
  finally
    LIniFile.Free;
  end;
end;

procedure THelloWorldResource.SaveToFile(const ALang, AText: string);
var
  LIniFile: TIniFile;
begin
  LIniFile := TIniFile.Create(GetIniFileName);
  try
    LIniFile.WriteString('HelloWorld', ALang, AText);
  finally
    LIniFile.Free;
  end;
end;

function THelloWorldResource.Say(times: Integer; lang: string): string;
var
  LHelloWorld: string;
begin
  LHelloWorld := LoadFromFile(lang, 'Hello, World!') + sLineBreak;

  Result := DupeString(LHelloWorld, times);
end;

function THelloWorldResource.SayOnce(lang: string): string;
begin
  Result := Say(1, lang);
end;

function THelloWorldResource.Store(lang: string; body: string): string;
begin
  SaveToFile(lang, body);
  Result := 'Saved: ' + lang;
end;

{ TSimpleFormResource }

function TSimpleFormResource.GetFullName(name, surname: string): string;
begin
  Result := 'Fullname: ' + Name + ' ' + Surname;
end;

function TSimpleFormResource.GetHost(Host: string): string;
begin
  Result := 'Host header of the request: ' + Host;
end;

{ TMultiFilesResource }

function TMultiFilesResource.HashFiles: string;
var
  LFile: TWebRequestFile;
  LIndex: Integer;
begin

  Result := 'Files: ' + Request.Files.Count.ToString;
  for LIndex := 0 to Request.Files.Count-1 do
  begin
    LFile := Request.Files.Items[LIndex] as TWebRequestFile;
    Result := string.Join(sLineBreak, [Result, LFile.FileName + ' (' + LFile.Stream.Size.ToString + ')']);
  end;
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TSimpleFormResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TMultiFilesResource>;

end.
