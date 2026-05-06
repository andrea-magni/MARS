(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Resources.HelloWorld;

interface

uses
  SysUtils, Classes, Contnrs
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.URL
, MARS.Core.JSON, MARS.Core.Response
//, MARS.Core.Token
, MARS.WebServer.Resources
, MARS.Core.RequestAndResponse.Interfaces
, FireDAC.Comp.Client, FireDAC.Stan.StorageBin
;

type
  [Path('www'), RootFolder('.\www', True)]
  TStaticContentResource = class(TFileSystemResource)
  end;



  [Path('helloworld')]
  THelloWorldResource = class
  private
    function GetName: string;
  protected
    [Context] ARequest: IMARSRequest;
    procedure OnFileHandler(Sender: TObject; const AFilename: string;
      var AText: string; var AHandled: Boolean);
    procedure OnValueHandler(Sender: TObject; const AObjectName, APropName: string;
    var AValue: string; var AHandled: Boolean);
  public
    [GET, Path('/{filename}'), Produces(TMediaType.TEXT_HTML)]
    function SayHelloWorld([PathParam] filename: string): string;

    property Name: string read GetName;
  end;

implementation

uses
  MARS.Core.Registry, MARS.Rtti.Utils, MARS.Core.Exceptions
, Web.Stencils, IOUtils, Web.HTTPApp
, System.Bindings.EvalProtocol, System.Bindings.Methods
, Data.DB
, CodeSiteLogging
;

function ExpandMacros(const AString: string): string;
begin
  Result := AString
    .Replace('{bin}', ExtractFilePath(ParamStr(0)))
    .Replace('\\', PathDelim, [rfReplaceAll])  // doppio backslash → separatore
    .Replace('\', PathDelim, [rfReplaceAll]);   // backslash singolo → separatore
end;

{ THelloWorldResource }

function THelloWorldResource.GetName: string;
begin
  Result := 'Say my name ' + TimeToStr(now);
end;

procedure THelloWorldResource.OnFileHandler(Sender: TObject;
  const AFilename: string; var AText: string; var AHandled: Boolean);
begin
  const LInputFolder = ExpandMacros('{bin}\input');
  const LTemplatesFolder = ExpandMacros('{bin}\templates');

  var LExpandedFileName := TPath.Combine(LInputFolder, AFileName);
  if ExtractFileExt(LExpandedFileName) = '' then
    LExpandedFileName := ChangeFileExt(LExpandedFileName, '.html');
  var LFileExists := TFile.Exists(LExpandedFileName);

  if not LFileExists then
  begin
    LExpandedFileName := TPath.Combine(LTemplatesFolder, AFileName);
    if ExtractFileExt(LExpandedFileName) = '' then
      LExpandedFileName := ChangeFileExt(LExpandedFileName, '.html');
    LFileExists := TFile.Exists(LExpandedFileName);
  end;

  if not LFileExists then
    raise EMARSHttpException.CreateFmt('File not found: %s', [AFileName], 404);

  CodeSite.SendFmtMsg('OnFileHandler: %s --> %s', [AFileName, LExpandedFileName]);

  AText := TFile.ReadAllText(LExpandedFileName);
  AHandled := True;
end;

procedure THelloWorldResource.OnValueHandler(Sender: TObject; const AObjectName,
  APropName: string; var AValue: string; var AHandled: Boolean);
begin
  CodeSite.SendFmtMsg('OnValueHandler [%s]: %s.%s --> %s', [Sender.ClassName, AObjectName, APropName, AValue]);

  if SameText(AObjectName, 'req') then
  begin
    if SameText(APropName, 'RawPath') then
      AValue := ARequest.RawPath
    else if SameText(APropName, 'QueryString') then
      AValue := ARequest.QueryString
    else
      AValue := ReadPropertyValue(ARequest.AsObject, APropName).ToString();

    AHandled := True;
  end;

end;

function THelloWorldResource.SayHelloWorld(filename: string): string;
begin
  var LProcessor := TWebStencilsProcessor.Create(nil);
  try
    LProcessor.OnFile := OnFileHandler;
    LProcessor.OnValue := OnValueHandler;

//    LProcessor.Whitelist.Configure(TDataSet,
//    ['Name', 'Active', 'FieldByName', 'First', 'Last', 'Next', 'Prior',
//     'Bof', 'Eof', 'FieldCount', 'Fields', 'Found', 'RecordCount', 'RecNo'], nil, True);
    LProcessor.Whitelist.Configure(TDataSet, ['Name'], nil, False);

//    LProcessor.WebRequest := TWebRequest(ARequest);
    LProcessor.DataVars.Add('resource', Self, False);

    const LDataFolder = ExpandMacros('{bin}\data');
    var LDatasetList := TObjectList.Create(False);
    for var LDatasetFile in TDirectory.GetFiles(LDataFolder, '*.fds') do
    begin
      const LDatasetName = ChangeFileExt(ExtractFileName(LDatasetFile), '');

      var LMemTable := TFDMemTable.Create(nil);
      try
        LMemTable.LoadFromFile(LDatasetFile);
        LMemTable.Name := LDatasetName;

        LProcessor.DataVars.Add(LDatasetName, LMemTable, True);
        LDatasetList.Add(LMemTable);
      except
        LMemTable.Free;
        raise;
      end;
    end;

    LProcessor.DataVars.Add('datasetList', LDatasetList, True);

    Result := LProcessor.ContentFromFile(filename);
  finally
    LProcessor.Free;
  end;
end;

initialization
  MARSRegister([TStaticContentResource, THelloWorldResource]);

end.
