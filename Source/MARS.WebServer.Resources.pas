(*
  Copyright 2025, MARS-Curiosity library

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

  // [DirectoryListing(False)] answers 404 for directories without an index file
  DirectoryListingAttribute = class(WebAttribute)
  private
    FEnabled: Boolean;
  public
    constructor Create(const AEnabled: Boolean = True);
    procedure ApplyToResource(const AResource: TFileSystemResource); override;

    property Enabled: Boolean read FEnabled;
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
    FDirectoryListingEnabled: Boolean;
  protected
    [Context] URL: TMARSURL;
    [Context] Activation: IMARSActivation;
    procedure Init; virtual;
    procedure InitContentTypesForExt; virtual;
    procedure InitIndexFileNames; virtual;
    function CheckFilters(const AString: string): Boolean; virtual;
    // RootFolder in canonical, delimiter-terminated form
    function CanonicalRootFolder: string; virtual;
    // True if the segment may be part of a served path (no dot-segments, separators,
    // reserved characters, trailing dots or spaces)
    function CheckPathSegment(const ASegment: string): Boolean; virtual;
    // Maps the request URL to a path under RootFolder. False when the request does not
    // resolve to something inside RootFolder (or violates IncludeSubFolders): the
    // caller answers 404 and never touches the file system.
    function ResolveFullPath(out AFullPath: string): Boolean; virtual;
    // percent-encodes a single path segment for use in an href
    class function EncodePathSegment(const ASegment: string): string;
    procedure ServeFileContent(const AFileName: string; const AResponse: TMARSResponse); virtual;
    procedure ServeDirectoryContent(const ADirectory: string; const AResponse: TMARSResponse); virtual;
    function DirectoryHasIndexFile(const ADirectory: string; out AIndexFullPath: string): Boolean; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // REST METHODS
    [GET]
    function GetContent: TMARSResponse; virtual;

    // Same status, Content-Type and Content-Length as GET, no body (RFC 9110 9.3.2)
    [HEAD]
    procedure HeadContent; virtual;

    // PROPERTIES
    property RootFolder: string read FRootFolder write FRootFolder;
    property IncludeSubFolders: Boolean read FIncludeSubFolders write FIncludeSubFolders;
    property ContentTypesForExt: TDictionary<string, string> read FContentTypesForExt;
    property InclusionFilters: TStringList read FInclusionFilters;
    property ExclusionFilters: TStringList read FExclusionFilters;
    property IndexFileNames: TStringList read FIndexFileNames;
    // HTML listing for directories without an index file (default True; see [DirectoryListing])
    property DirectoryListingEnabled: Boolean read FDirectoryListingEnabled write FDirectoryListingEnabled;
  end;

function AtLeastOneMatch(const ASample: string; const AValues: TStringList): Boolean;

implementation

uses
  System.Types, IOUtils, Masks, StrUtils, NetEncoding
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
  FDirectoryListingEnabled := True;

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

function TFileSystemResource.CanonicalRootFolder: string;
begin
  Result := IncludeTrailingPathDelimiter(TPath.GetFullPath(RootFolder));
end;

function TFileSystemResource.CheckPathSegment(const ASegment: string): Boolean;
const
  // separators (a '%2f' or '%5c' decoded inside a segment), Windows reserved characters
  // and ':' (drive letters, NTFS alternate data streams such as 'file.txt::$DATA')
  FORBIDDEN_CHARS = [':', '*', '?', '"', '<', '>', '|', '/', '\'];
var
  LChar: Char;
begin
  // '.' and '..' would climb out of RootFolder (or are pointless)
  Result := (ASegment <> '') and (ASegment <> '.') and (ASegment <> '..');
  if not Result then
    Exit;

  for LChar in ASegment do
    if CharInSet(LChar, FORBIDDEN_CHARS) or (LChar < #32) then
      Exit(False);

  // Windows silently strips trailing dots and spaces ('config.ini.' opens 'config.ini'),
  // which would let a request slip past the extension filters
  Result := not (ASegment.EndsWith('.') or ASegment.EndsWith(' '));
end;

function TFileSystemResource.ResolveFullPath(out AFullPath: string): Boolean;
var
  LRelativePath, LBasePath, LRootPath, LCandidate: string;
  LSegments: TArray<string>;
  LIndex, LSegmentCount, LWildcardPosition: Integer;
begin
  Result := False;
  AFullPath := '';

  if RootFolder = '' then
    Exit;

  LRelativePath := SmartConcat(URL.PathTokens, '/').Replace('/', PathDelim, [rfReplaceAll]);

  LBasePath := SmartConcat([
      Activation.Engine.BasePath, Activation.Application.BasePath, Activation.ResourcePath
    ], '/').Replace('/', PathDelim, [rfReplaceAll]);

  // strip eventual initial PathDelim
  if LBasePath.StartsWith(PathDelim) then
    LBasePath := LBasePath.Substring(string(PathDelim).Length);

  // stop at eventual wildcard position
  LWildcardPosition := LBasePath.IndexOf(TMARSURL.PATH_PARAM_WILDCARD);
  if LWildcardPosition <> -1 then
    LBasePath := LBasePath.Substring(0, LWildcardPosition - 1);

  // make relative path even with base path
  LRelativePath := LRelativePath.Substring(LBasePath.Length);
  if LRelativePath.StartsWith(PathDelim) then
    LRelativePath := LRelativePath.Substring(string(PathDelim).Length);

  // 1) every segment is validated before the file system is involved; a trailing empty
  //    segment (request ending with '/') is allowed and means "directory"
  if LRelativePath = '' then
    LSegments := []
  else
    LSegments := LRelativePath.Split([PathDelim]);
  LSegmentCount := 0;
  for LIndex := 0 to High(LSegments) do
  begin
    if (LSegments[LIndex] = '') and (LIndex = High(LSegments)) then
      Continue;
    if not CheckPathSegment(LSegments[LIndex]) then
      Exit;
    Inc(LSegmentCount);
  end;

  // 2) IncludeSubFolders = False confines requests to the root folder itself
  if (not IncludeSubFolders) and (LSegmentCount > 1) then
    Exit;

  // 3) the canonical candidate must still lie under the canonical root: a second,
  //    independent guard should some platform quirk get past the segment checks
  LRootPath := CanonicalRootFolder;
  LCandidate := TPath.GetFullPath(LRootPath + LRelativePath);
  if not IncludeTrailingPathDelimiter(LCandidate).StartsWith(LRootPath, {$IFDEF MSWINDOWS}True{$ELSE}False{$ENDIF}) then
    Exit;

  AFullPath := LCandidate;
  Result := True;
end;

function TFileSystemResource.GetContent: TMARSResponse;
var
  LFullPath, LIndexFileFullPath: string;
begin
  Result := TMARSResponse.Create;
  Result.StatusCode := 404;

  if not ResolveFullPath(LFullPath) then
    Exit;

  // served content is user-provided: browsers must trust the declared Content-Type
  Activation.Response.SetHeader('X-Content-Type-Options', 'nosniff');

  if CheckFilters(LFullPath) then
  begin
    if FileExists(LFullPath) then
      ServeFileContent(LFullPath, Result)
    else if TDirectory.Exists(LFullPath) then
    begin
      LIndexFileFullPath := '';
      if DirectoryHasIndexFile(LFullPath, LIndexFileFullPath) then
        ServeFileContent(LIndexFileFullPath, Result)
      else if DirectoryListingEnabled then
        ServeDirectoryContent(LFullPath, Result);
    end;
  end;
end;

class function TFileSystemResource.EncodePathSegment(const ASegment: string): string;
const
  HEX: array[0..15] of Char = '0123456789ABCDEF';
var
  LByte: Byte;
begin
  // RFC 3986 unreserved characters pass through, everything else is %XX-encoded (UTF-8)
  Result := '';
  for LByte in TEncoding.UTF8.GetBytes(ASegment) do
    if (LByte in [Ord('A')..Ord('Z'), Ord('a')..Ord('z'), Ord('0')..Ord('9')])
      or (LByte in [Ord('-'), Ord('.'), Ord('_'), Ord('~')]) then
      Result := Result + Char(LByte)
    else
      Result := Result + '%' + HEX[LByte shr 4] + HEX[LByte and $F];
end;

procedure TFileSystemResource.HeadContent;
var
  LResponse: TMARSResponse;
begin
  // Reuses GetContent (so subclasses overriding it get HEAD for free) and discards
  // the body: the file gets opened, to report its size, but never read.
  // Headers go straight to Activation.Response instead of through a TMARSResponse
  // result: on the WebBroker/Indy host assigning a (blank) Content resets Content-Length
  // to 0, so the length has to be set with no body assignment following it.
  LResponse := GetContent;
  try
    LResponse.FreeContentStream := True;

    Activation.Response.StatusCode := LResponse.StatusCode;
    if LResponse.ContentType <> '' then
      Activation.Response.ContentType := LResponse.ContentType;

    if Assigned(LResponse.ContentStream) then
      Activation.Response.ContentLength := LResponse.ContentStream.Size
    else if LResponse.Content <> '' then // directory listing, declared UTF-8
      Activation.Response.ContentLength := TEncoding.UTF8.GetByteCount(LResponse.Content);
  finally
    LResponse.Free;
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
// Explicit charset on TEXTUAL types. Without it, the HTTP layer (Indy) applies
// its own default to a text/* that doesn't declare one — ISO-8859-1, the HTTP/1.0 default
// that RFC 7231 removed — and files, which today are UTF-8, reach the client corrupted.
// It has to be declared HERE because the header takes precedence over everything else: over
// the BOM, over a page's `<meta charset>`, and over a stylesheet's `@charset`. The content is
// not touched — these are the file's bytes, which the resource does not decode: we are simply
// no longer declaring them as something they aren't. Anyone serving files in a legacy encoding
// remaps the extension: the method is virtual for that very reason.
const
  UTF8 = '; ' + TMediaType.CHARSET_UTF8_DEF;
begin
  ContentTypesForExt.Add('.jpg', 'image/jpeg');
  ContentTypesForExt.Add('.jpeg', 'image/jpeg');
  ContentTypesForExt.Add('.png', 'image/png');
  ContentTypesForExt.Add('.pdf', 'application/pdf');
  ContentTypesForExt.Add('.htm', 'text/html' + UTF8);
  ContentTypesForExt.Add('.html', 'text/html' + UTF8);
  ContentTypesForExt.Add('.js', 'application/javascript' + UTF8);
  ContentTypesForExt.Add('.css', 'text/css' + UTF8);
  ContentTypesForExt.Add('.txt', 'text/plain' + UTF8);
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
  LEntry, LEntryName, LHrefPrefix, LHref: string;
  LIsFolder: Boolean;
begin
  AResponse.StatusCode := 200;
// Explicit charset, same as for files. Here the content is composed by MARS and
// the NAMES of the directory entries end up in it — outside ASCII, without this they would
// come out as ISO-8859-1 (or mangled, if they don't fit that encoding).
  AResponse.ContentType := TMediaType.TEXT_HTML + '; ' + TMediaType.CHARSET_UTF8_DEF;
  AResponse.Content := '<html><body><ul>';

  // links are relative to the listed directory: when the request URL has no trailing '/'
  // the browser would resolve them against the parent, so the last segment is repeated
  LHrefPrefix := '';
  if (not URL.Path.EndsWith(TMARSURL.URL_PATH_SEPARATOR)) and (Length(URL.PathTokens) > 0) then
    LHrefPrefix := EncodePathSegment(URL.PathTokens[High(URL.PathTokens)]) + TMARSURL.URL_PATH_SEPARATOR;

  LEntries := TDirectory.GetFileSystemEntries(ADirectory);
  for LIndex := Low(LEntries) to High(LEntries) do
  begin
    LEntry := LEntries[LIndex];
    if CheckFilters(LEntry) then
    begin
      // entry names are untrusted (whoever can drop a file in the folder chooses them):
      // percent-encoded in the href, HTML-encoded in the text
      LEntryName := ExtractFileName(LEntry);
      LIsFolder := TDirectory.Exists(LEntry);
      LHref := LHrefPrefix + EncodePathSegment(LEntryName) + IfThen(LIsFolder, TMARSURL.URL_PATH_SEPARATOR);
      AResponse.Content := AResponse.Content
        + '<li>'
        + '<a href="' + LHref + '">' + TNetEncoding.HTML.Encode(LEntryName) + '</a>'
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
  Result := AString
    .Replace('{bin}', ExtractFilePath(ParamStr(0)))
    .Replace('\\', PathDelim, [rfReplaceAll])  // double backslash -> path separator
    .Replace('\', PathDelim, [rfReplaceAll]);   // single backslash -> path separator
  // MF20260319
  // On Windows PathDelim = '\', behavior unchanged.
  // On Linux   PathDelim = '/', backslashes are converted to slashes.
end;

{ ContentTypeForFileExt }

procedure ContentTypeForFileExt.ApplyToResource(
  const AResource: TFileSystemResource);
begin
  inherited;
  // AddOrSetValue (not Add): the defaults set by InitContentTypesForExt are already
  // there, so this attribute can both add a new extension and override a known one.
  AResource.ContentTypesForExt.AddOrSetValue(FFileExt, FContentType);
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

{ DirectoryListingAttribute }

constructor DirectoryListingAttribute.Create(const AEnabled: Boolean);
begin
  inherited Create;
  FEnabled := AEnabled;
end;

procedure DirectoryListingAttribute.ApplyToResource(
  const AResource: TFileSystemResource);
begin
  inherited;
  AResource.DirectoryListingEnabled := Enabled;
end;

end.
