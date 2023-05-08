(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.WebServer.Resources;

interface

uses
  SysUtils, Classes, Generics.Collections

, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.URL, MARS.Core.Response
, MARS.Core.Activation.Interfaces
;

type
  TFileSystemResource = class; // FWD

  WebAttribute = class(TCustomAttribute)
  private
  protected
  public
    procedure ApplyToResource(const AResource: TFileSystemResource); virtual;
  end;

  RootFolderAttribute = class(WebAttribute)
  private
    FPath: string;
    FIncludeSubFolders: Boolean;
  protected
    function ExpandMacros(const AString: string): string; virtual;
  public
    constructor Create(const APath: string; const AIncludeSubFolders: Boolean);
    procedure ApplyToResource(const AResource: TFileSystemResource); override;

    property IncludeSubFolders: Boolean read FIncludeSubFolders;
    property Path: string read FPath;
  end;

  ContentTypeForFileExt = class(WebAttribute)
  private
    FContentType: string;
    FFileExt: string;
  public
    constructor Create(AContentType: string; const AFileExt: string);
    procedure ApplyToResource(const AResource: TFileSystemResource); override;

    property ContentType: string read FContentType;
    property FileExt: string read FFileExt;
  end;

  WebFilterAttribute = class(WebAttribute)
  private
    FPattern: string;
  protected
  public
    constructor Create(const APattern: string = '*.*');
    property Pattern: string read FPattern;
  end;

  IncludeAttribute = class(WebFilterAttribute)
  public
    procedure ApplyToResource(const AResource: TFileSystemResource); override;
  end;

  ExcludeAttribute = class(WebFilterAttribute)
  public
    procedure ApplyToResource(const AResource: TFileSystemResource); override;
  end;


  TFileSystemResource = class
  private
    FRootFolder: string;
    FIncludeSubFolders: Boolean;
    FContentTypesForExt: TDictionary<string, string>;
    FExclusionFilters: TStringList;
    FInclusionFilters: TStringList;
    FIndexFileNames: TStringList;
  protected
    [Context] URL: TMARSURL;
    [Context] Activation: IMARSActivation;
    procedure Init; virtual;
    procedure InitContentTypesForExt; virtual;
    procedure InitIndexFileNames; virtual;
    function CheckFilters(const AString: string): Boolean; virtual;
    procedure ServeFileContent(const AFileName: string; const AResponse: TMARSResponse); virtual;
    procedure ServeDirectoryContent(const ADirectory: string; const AResponse: TMARSResponse); virtual;
    function DirectoryHasIndexFile(const ADirectory: string; out AIndexFullPath: string): Boolean; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // REST METHODS
    [GET]
    function GetContent: TMARSResponse; virtual;

    // PROPERTIES
    property RootFolder: string read FRootFolder write FRootFolder;
    property IncludeSubFolders: Boolean read FIncludeSubFolders write FIncludeSubFolders;
    property ContentTypesForExt: TDictionary<string, string> read FContentTypesForExt;
    property InclusionFilters: TStringList read FInclusionFilters;
    property ExclusionFilters: TStringList read FExclusionFilters;
    property IndexFileNames: TStringList read FIndexFileNames;
  end;

function AtLeastOneMatch(const ASample: string; const AValues: TStringList): Boolean;

implementation

uses
  System.Types, IOUtils, Masks, StrUtils
, MARS.Core.Utils, MARS.Rtti.Utils, MARS.Core.Exceptions
;

function AtLeastOneMatch(const ASample: string; const AValues: TStringList): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := 0 to AValues.Count-1 do
  begin
    if MatchesMask(ASample, AValues[LIndex]) then
    begin
      Result := True;
      Break;
    end;
  end;
end;


{ TFileSystemResource }

function TFileSystemResource.CheckFilters(const AString: string): Boolean;
begin
  if (ExclusionFilters.Count > 0) and AtLeastOneMatch(AString, ExclusionFilters) then
  begin
    Result := False;
  end
  else
  begin
    Result := (InclusionFilters.Count = 0) or AtLeastOneMatch(AString, InclusionFilters);
  end;
end;

constructor TFileSystemResource.Create;
begin
  inherited Create;
  FRootFolder := '';
  FIncludeSubFolders := False;
  FContentTypesForExt := TDictionary<string, string>.Create;
  FInclusionFilters := TStringList.Create;
  FExclusionFilters := TStringList.Create;
  FIndexFileNames := TStringList.Create;

  Init;
end;

destructor TFileSystemResource.Destroy;
begin
  FIndexFileNames.Free;
  FExclusionFilters.Free;
  FInclusionFilters.Free;
  FContentTypesForExt.Free;
  inherited;
end;

function TFileSystemResource.DirectoryHasIndexFile(const ADirectory: string;
  out AIndexFullPath: string): Boolean;
var
  LIndex: Integer;
  LIndexFileName: string;
  LIndexFullFileName: string;
begin
  Result := False;
  for LIndex := 0 to IndexFileNames.Count-1 do
  begin
    LIndexFileName := IndexFileNames[LIndex];
    LIndexFullFileName := TPath.Combine(ADirectory, LIndexFileName);
    if FileExists(LIndexFullFileName) then
    begin
      Result := True;
      AIndexFullPath := LIndexFullFileName;
      Break;
    end;
  end;
end;

function TFileSystemResource.GetContent: TMARSResponse;
begin
  Result := TMARSResponse.Create;
  Result.StatusCode := 404;

  var LRelativePath := SmartConcat(URL.PathTokens
    , '/').Replace('/', PathDelim, [rfReplaceAll]);

  var LBasePath := SmartConcat([
      Activation.Engine.BasePath, Activation.Application.BasePath, Activation.ResourcePath
    ], '/').Replace('/', PathDelim, [rfReplaceAll]);

  // strip eventual initial PathDelim
  if LBasePath.StartsWith(PathDelim) then
    LBasePath := LBasePath.Substring(string(PathDelim).Length);

  // stop at eventual wildcard position
  var LWildcardPosition := LBasePath.IndexOf(TMARSURL.PATH_PARAM_WILDCARD);
  if LWildcardPosition <> -1 then
    LBasePath := LBasePath.Substring(0, LWildcardPosition - 1);

  // make relative path even with base path
  LRelativePath := LRelativePath.Substring(LBasePath.Length);
  if LRelativePath.StartsWith(PathDelim) then
    LRelativePath := LRelativePath.Substring(string(PathDelim).Length);

  var LFullPath := RootFolder;
  LFullPath := SmartConcat([LFullPath, LRelativePath], PathDelim);

  if CheckFilters(LFullPath) then
  begin
    if FileExists(LFullPath) then
      ServeFileContent(LFullPath, Result)
    else if TDirectory.Exists(LFullPath) then
    begin
      var LIndexFileFullPath := '';
      if DirectoryHasIndexFile(LFullPath, LIndexFileFullPath) then
        ServeFileContent(LIndexFileFullPath, Result)
      else
        ServeDirectoryContent(LFullPath, Result);
    end;
  end;
end;

procedure TFileSystemResource.Init;
begin
  InitContentTypesForExt;
  InitIndexFileNames;

  TRttiHelper.ForEachAttribute<WebAttribute>(Self,
    procedure (AAttrib: WebAttribute)
    begin
      AAttrib.ApplyToResource(Self);
    end
  );
end;

procedure TFileSystemResource.InitContentTypesForExt;
begin
  ContentTypesForExt.Add('.jpg', 'image/jpeg');
  ContentTypesForExt.Add('.jpeg', 'image/jpeg');
  ContentTypesForExt.Add('.png', 'image/png');
  ContentTypesForExt.Add('.pdf', 'application/pdf');
  ContentTypesForExt.Add('.htm', 'text/html');
  ContentTypesForExt.Add('.html', 'text/html');
  ContentTypesForExt.Add('.js', 'application/javascript');
  ContentTypesForExt.Add('.css', 'text/css');
  ContentTypesForExt.Add('.txt', 'text/plain');

end;

procedure TFileSystemResource.InitIndexFileNames;
begin
  IndexFileNames.Add('index.html');
  IndexFileNames.Add('index.htm');
  IndexFileNames.Add('default.html');
  IndexFileNames.Add('default.htm');
end;

procedure TFileSystemResource.ServeDirectoryContent(const ADirectory: string;
  const AResponse: TMARSResponse);
var
  LEntries: TStringDynArray;
  LIndex: Integer;
  LEntry: string;
  LEntryRelativePath: string;
  LIsFolder: Boolean;
begin
  AResponse.StatusCode := 200;
  AResponse.ContentType := TMediaType.TEXT_HTML;
  AResponse.Content := '<html><body><ul>';

  LEntries := TDirectory.GetFileSystemEntries(ADirectory);
  for LIndex := Low(LEntries) to High(LEntries) do
  begin
    LEntry := LEntries[LIndex];
    if CheckFilters(LEntry) then
    begin
      LEntryRelativePath := ExtractRelativePath(RootFolder, LEntry);
      LIsFolder := TDirectory.Exists(LEntry);
      AResponse.Content := AResponse.Content
        + '<li>'
        + '<a href="' + LEntryRelativePath + IfThen(LIsFolder, '/') + '">' + LEntryRelativePath + '</a>'
        + IfThen(LIsFolder, ' (folder)')
        + '</li>';
    end;
  end;
  AResponse.Content := AResponse.Content + '</ul></body></html>';
end;

procedure TFileSystemResource.ServeFileContent(const AFileName: string;
  const AResponse: TMARSResponse);
var
  LFileExt: string;
  LContentType: string;
begin
  LFileExt := ExtractFileExt(AFileName);

  AResponse.StatusCode := 200;
  AResponse.ContentStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  if not ContentTypesForExt.TryGetValue(LFileExt, LContentType) then
    LContentType := TMediaType.APPLICATION_OCTET_STREAM;  // default = binary
  AResponse.ContentType := LContentType;
end;

{ RootFolderAttribute }

procedure RootFolderAttribute.ApplyToResource(
  const AResource: TFileSystemResource);
begin
  inherited;
  AResource.RootFolder := ExpandMacros(Path);
  AResource.IncludeSubFolders := IncludeSubFolders;
end;

constructor RootFolderAttribute.Create(const APath: string;
  const AIncludeSubFolders: Boolean);
begin
  inherited Create;
  FPath := IncludeTrailingPathDelimiter(APath);
  FIncludeSubFolders := AIncludeSubFolders;
end;

function RootFolderAttribute.ExpandMacros(const AString: string): string;
begin
  Result := AString;
  Result := Result.Replace('{bin}', ExtractFilePath(ParamStr(0)))
                  .Replace('\\', '\', [rfReplaceAll]);
end;

{ ContentTypeForFileExt }

procedure ContentTypeForFileExt.ApplyToResource(
  const AResource: TFileSystemResource);
begin
  inherited;
  AResource.ContentTypesForExt.Add(FFileExt, FContentType);
end;

constructor ContentTypeForFileExt.Create(AContentType: string;
  const AFileExt: string);
begin
  inherited Create;
  FContentType := AContentType;
  FFileExt := AFileExt;
end;

{ WebAttribute }

procedure WebAttribute.ApplyToResource(const AResource: TFileSystemResource);
begin

end;

{ WebFilterAttribute }

constructor WebFilterAttribute.Create(const APattern: string);
begin
  inherited Create;
  FPattern := APattern;
end;

{ IncludeAttribute }

procedure IncludeAttribute.ApplyToResource(
  const AResource: TFileSystemResource);
begin
  inherited;
  AResource.InclusionFilters.Add(Pattern);
end;

{ ExcludeAttribute }

procedure ExcludeAttribute.ApplyToResource(
  const AResource: TFileSystemResource);
begin
  inherited;
  AResource.ExclusionFilters.Add(Pattern);
end;

end.
