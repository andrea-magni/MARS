(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.WebStencils;

{$I MARS.inc}

interface

uses
    System.Classes, System.SysUtils, Generics.Collections, Rtti, Data.DB
// *** BEWARE ***
// if your Delphi edition/license does not include WebStencils,
// remove the MARS_WEBSTENCILS definition in the MARS.inc file!
// This is likely to be the case if you are compiling your first project and
// got a "Web.Stencils not found" error at the following line
, Web.Stencils, System.IOUtils, Web.HTTPApp
, System.Bindings.EvalProtocol, System.Bindings.Methods

, MARS.Core.Activation.Interfaces, MARS.Core.Application.Interfaces
, MARS.Core.Attributes, MARS.Core.Classes, MARS.Core.Declarations, MARS.Core.Exceptions
, MARS.Core.JSON, MARS.Core.MediaType
, MARS.Core.Registry, MARS.Core.Token, MARS.Core.URL, MARS.Utils.Parameters
, MARS.Core.RequestAndResponse.Interfaces
;

type
  EMARSWebStencilsException = class(EMARSApplicationException);

  MARSWebStencilsAttribute = class(TCustomAttribute);

  TMARSWebStencils = class
  private
    FActivation: IMARSActivation;
    FTemplatesFolder: string;
    FInputFolder: string;
    FProcessor: TWebStencilsProcessor;
    FVarValues: TDictionary<string, string>;
  protected
    function ExpandMacros(const AString: string): string;
    function GetRequest: IMARSRequest;

    procedure OnFileHandler(Sender: TObject; const AFilename: string;
      var AText: string; var AHandled: Boolean);
    procedure OnValueHandler(Sender: TObject; const AObjectName, APropName: string;
      var AValue: string; var AHandled: Boolean);

    property Request: IMARSRequest read GetRequest;
  public
    constructor Create(const AActivation: IMARSActivation = nil); virtual;
    destructor Destroy; override;

    procedure AddVarValue(const AName: string; const AValue: string); overload;
    procedure AddDataVar(const AName: string; const AValue: TObject; const AOwned: Boolean = False);
    function ContentFromFile(const AFileName: string): string;
    function ContentFromStream(const AStream: TStream): string;
    function ContentFromString(const AString: string): string;

    property Activation: IMARSActivation read FActivation;
    property InputFolder: string read FInputFolder write FInputFolder;
    property TemplatesFolder: string read FTemplatesFolder write FTemplatesFolder;
    property Processor: TWebStencilsProcessor read FProcessor;
  end;

implementation

uses
  StrUtils, Variants
, MARS.Core.Utils, MARS.Data.Utils, MARS.Rtti.Utils
, MARS.Core.Activation
, MARS.WebStencils.InjectionService
//, MARS.WebStencils.ReadersAndWriters
//, CodeSiteLogging
;

{ TMARSWebStencils }

procedure TMARSWebStencils.AddDataVar(const AName: string;
  const AValue: TObject; const AOwned: Boolean);
begin
  FProcessor.DataVars.Add(AName, AValue, AOwned);
end;

procedure TMARSWebStencils.AddVarValue(const AName, AValue: string);
begin
  FVarValues.AddOrSetValue(AName.ToLower, AValue);
end;

function TMARSWebStencils.ContentFromFile(const AFileName: string): string;
begin
  Result := FProcessor.ContentFromFile(AFileName);
end;

function TMARSWebStencils.ContentFromStream(const AStream: TStream): string;
begin
  Result := FProcessor.ContentFromStream(AStream);
end;

function TMARSWebStencils.ContentFromString(const AString: string): string;
begin
  Result := FProcessor.ContentFromString(AString);
end;

constructor TMARSWebStencils.Create(const AActivation: IMARSActivation);
begin
  inherited Create();
  FActivation := AActivation;
  FInputFolder := ExpandMacros('{bin}\input');
  FTemplatesFolder := ExpandMacros('{bin}\templates');
  FVarValues := TDictionary<string, string>.Create;
  FProcessor := TWebStencilsProcessor.Create(nil);
  try
    FProcessor.OnFile := OnFileHandler;
    FProcessor.OnValue := OnValueHandler;

//    FProcessor.WebRequest := TWebRequest(ARequest);
    FProcessor.Whitelist.Configure(TDataSet, ['Name', 'Tag'], nil, False);

    FProcessor.DataVars.Add('resource', Activation.ResourceInstance, False);
  except
    FreeAndNil(FProcessor);
    raise;
  end;
end;

destructor TMARSWebStencils.Destroy;
begin
  FreeAndNil(FProcessor);
  FreeAndNil(FVarValues);
  inherited;
end;


function TMARSWebStencils.GetRequest: IMARSRequest;
begin
  if not Assigned(FActivation) then
    Exit(nil);
  Result := FActivation.Request;
end;

function TMARSWebStencils.ExpandMacros(const AString: string): string;
begin
  Result := AString
    .Replace('{bin}', ExtractFilePath(ParamStr(0)))
    .Replace('\\', PathDelim, [rfReplaceAll])  // doppio backslash → separatore
    .Replace('\', PathDelim, [rfReplaceAll]);   // backslash singolo → separatore
end;


procedure TMARSWebStencils.OnFileHandler(Sender: TObject;
  const AFilename: string; var AText: string; var AHandled: Boolean);
begin
  var LExpandedFileName := TPath.Combine(InputFolder, AFileName);
  if ExtractFileExt(LExpandedFileName) = '' then
    LExpandedFileName := ChangeFileExt(LExpandedFileName, '.html');
  var LFileExists := TFile.Exists(LExpandedFileName);

  if not LFileExists then
  begin
    LExpandedFileName := TPath.Combine(TemplatesFolder, AFileName);
    if ExtractFileExt(LExpandedFileName) = '' then
      LExpandedFileName := ChangeFileExt(LExpandedFileName, '.html');
    LFileExists := TFile.Exists(LExpandedFileName);
  end;

  if not LFileExists then
    raise EMARSHttpException.CreateFmt('File not found: %s', [AFileName], 404);

//  CodeSite.SendFmtMsg('OnFileHandler: %s --> %s', [AFileName, LExpandedFileName]);

  AText := TFile.ReadAllText(LExpandedFileName);
  AHandled := True;
end;

procedure TMARSWebStencils.OnValueHandler(Sender: TObject; const AObjectName,
  APropName: string; var AValue: string; var AHandled: Boolean);
begin
//  CodeSite.SendFmtMsg('TMARSWebStencils.OnValueHandler [%s]: %s.%s --> %s', [Sender.ClassName, AObjectName, APropName, AValue]);

  if SameText(AObjectName, 'context') then
  begin
    var LValue := TMARSActivation.GetValueByName(APropName, FActivation);
    AHandled := not LValue.IsEmpty;
    if AHandled then
      AValue := LValue.ToString();

    if (not AHandled) then
     AHandled := FVarValues.TryGetValue(APropName.ToLower, AValue);
  end;

end;

end.
