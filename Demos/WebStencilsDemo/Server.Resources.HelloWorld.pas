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


, MARS.WebStencils
;

type
  [Path('www'), RootFolder('.\www', True)]
  TStaticContentResource = class(TFileSystemResource)
  end;

  [Path('helloworld')]
  THelloWorldResource = class
  protected
    [Context] FWS: TMARSWebStencils;
    function DataFolder: string;
  public
    function GetLocalPropertyValue: string;

    [GET, Produces(TMediaType.TEXT_HTML)]
    function HomePage(): string;

    [GET, Path('/{datasetName}'), Produces(TMediaType.TEXT_HTML)]
    function RenderDataset([PathParam] datasetName: string): string;

    property LocalPropertyValue: string read GetLocalPropertyValue;
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

function THelloWorldResource.HomePage(): string;
begin
  var LDatasetsList := TFDMemTable.Create(nil);
  try
    LDatasetsList.FieldDefs.Add('Name', ftString, 255);
    LDatasetsList.FieldDefs.Add('FileName', ftString, 512);
    LDatasetsList.CreateDataSet;

    for var LDatasetFile in TDirectory.GetFiles(DataFolder, '*.fds') do
    begin
      const LDatasetName = ChangeFileExt(ExtractFileName(LDatasetFile), '');
      LDatasetsList.AppendRecord([LDatasetName, LDatasetFile]);
    end;

    FWS.AddDataVar('availableDatasets', LDatasetsList, True);
  except
    FreeAndNil(LDatasetsList);
    raise;
  end;

  Result := FWS.ContentFromFile('index.html');
end;


function THelloWorldResource.RenderDataset(datasetName: string): string;
begin
  const LDataFolder = ExpandMacros('{bin}\data');
  const LDatasetFile = TPath.Combine(LDataFolder, ChangeFileExt(datasetName, '.fds'));
  const LDatasetName = ChangeFileExt(ExtractFileName(LDatasetFile), '');

  var LMemTable := TFDMemTable.Create(nil);
  try
    LMemTable.LoadFromFile(LDatasetFile);
    LMemTable.Name := LDatasetName;

    FWS.AddVarValue('datasetName', LDatasetName);
    FWS.AddDataVar('dataset', LMemTable, True);
  except
    LMemTable.Free;
    raise;
  end;


  Result := FWS.ContentFromFile('dataset.html');
end;

function THelloWorldResource.DataFolder: string;
begin
  Result := ExpandMacros('{bin}\data');
end;

function THelloWorldResource.GetLocalPropertyValue: string;
begin
  Result := 'CurrentTime is ' + TimeToStr(Now);
end;

initialization
  MARSRegister([TStaticContentResource, THelloWorldResource]);

end.
