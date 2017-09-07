(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.dmustache;

{$I MARS.inc}

interface

uses
    System.Classes, System.SysUtils, Rtti

  , MARS.Core.Application
  , MARS.Core.Attributes
  , MARS.Core.Classes
  , MARS.Core.Declarations
  , MARS.Core.JSON
  , MARS.Core.MediaType
  , MARS.Core.Registry
  , MARS.Core.Token
  , MARS.Core.URL
  , MARS.Utils.Parameters
  , MARS.Core.Activation.Interfaces

  , SynMustache, SynCommons
;

type
  EdmustacheError = class(EMARSApplicationException);

  dmustacheAttribute = class(MARSAttribute)
  private
    FName: string;
  public
    constructor Create(AName: string);
    property Name: string read FName;
  end;

  TMARSdmustache = class
  private
    FActivation: IMARSActivation;
    FName: string;
    FApplication: TMARSApplication;
    FParameters: TMARSParameters;
    FURL: TMARSURL;
    FToken: TMARSToken;
  protected
    function GetTemplatesFolder: string; virtual;
    function GetTemplateFileName(const AFileName: string): string; virtual;
  public
    constructor Create(const AName: string; const AActivation: IMARSActivation = nil); virtual;
    destructor Destroy; override;

    function RenderTemplateWithJSON(const ATemplateFileName: string; const AJSON: string): string; overload;
    function RenderTemplateWithJSON(const ATemplateFileName: string; const AJSON: TJSONValue;
      const AThenFreeJSON: Boolean = False): string; overload;
    function Render(const ATemplate: string; const AValue: variant): string;

    property Activation: IMARSActivation read FActivation;
    property Application: TMARSApplication read FApplication;
    property URL: TMARSURL read FURL;
    property Token: TMARSToken read FToken;
    property Parameters: TMARSParameters read FParameters;
    property Name: string read FName;
    property TemplatesFolder: string read GetTemplatesFolder;  // MF 170805
  end;

implementation

uses
  IOUTils
  , MARS.Core.Utils
  , MARS.Core.Exceptions
  , MARS.Rtti.Utils
  , MARS.dmustache.InjectionService
;

  { TMARSdmustache }

constructor TMARSdmustache.Create(const AName: string; const AActivation: IMARSActivation);
var
  LdmustacheSlice: TMARSParameters;
begin
  inherited Create;
  FName := AName;
  FActivation := AActivation;

  // shortcuts
  FApplication := FActivation.Application;
  FURL := FActivation.URL;
  FToken := FActivation.Token;

  // dmustache parameters
  FParameters := TMARSParameters.Create(AName);
  try
    if FApplication.Parameters.ContainsSlice('dmustache') then
    begin
      LdmustacheSlice := TMARSParameters.Create('dmustache');
      try
        LdmustacheSlice.CopyFrom(FApplication.Parameters, 'dmustache');
        FParameters.CopyFrom(LdmustacheSlice, FName);
      finally
        LdmustacheSlice.Free;
      end;
    end;
  except
    FParameters.Free;
    raise;
  end;
end;

destructor TMARSdmustache.Destroy;
begin
  FreeAndNil(FParameters);
  inherited;
end;


function TMARSdmustache.GetTemplateFileName(const AFileName: string): string;
begin
  if IsRelativePath(AFileName) then
    Result := TPath.Combine(GetTemplatesFolder, AFileName)
  else
    Result := AFileName;
end;

function TMARSdmustache.GetTemplatesFolder: string;
begin
  Result := Parameters.ByName('TemplatesFolder'
      , IncludeTrailingPathDelimiter(TPath.Combine(ExtractFilePath(ParamStr(0)), 'templates'))
      ).AsString;
end;

function TMARSdmustache.Render(const ATemplate: string;
  const AValue: variant): string;
var
  LMustache: TSynMustache;
begin
  LMustache := TSynMustache.Parse(StringToUTF8(ATemplate));
  Result := UTF8ToString(LMustache.Render(AValue));
end;

function TMARSdmustache.RenderTemplateWithJSON(const ATemplateFileName,
  AJSON: string): string;
var
  LTemplate: TStringList;
  LOutput: RawUTF8;
begin
  LTemplate := TStringList.Create;
  try
    LTemplate.LoadFromFile(GetTemplateFileName(ATemplateFileName));
    if TSynMustache.TryRenderJson(StringToUTF8(LTemplate.Text), StringToUTF8(AJSON), LOutput) then
      Result := UTF8ToString(LOutput)
    else
     raise EdmustacheError.Create('Error rendering JSON');
  finally
    LTemplate.Free;
  end;
end;

function TMARSdmustache.RenderTemplateWithJSON(const ATemplateFileName: string;
  const AJSON: TJSONValue; const AThenFreeJSON: Boolean): string;
begin
  try
    Result := RenderTemplateWithJSON(ATemplateFileName, AJSON.ToJSON);
  finally
    if AThenFreeJSON then
      AJSON.Free;
  end;
end;

{ dmustacheAttribute }

constructor dmustacheAttribute.Create(AName: string);
begin
  inherited Create;
  FName := AName;
end;

end.
