(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.MediaType;

interface

uses
  SysUtils
  , Classes
  , Generics.Defaults
  , Generics.Collections
  , MARS.Core.Declarations;

type
  {$SCOPEDENUMS ON}
  MediaType = (
    Text_Plain,
    Text_XML,
    Text_HTML,
    Application_XML,
    Application_JSON,
    Application_XHTML_XML,
    Application_SVG_XML,
    Application_Atom_XML,
    Application_Octet_Stream,
    Application_Form_Encoded,
    Multipart_Form_Data,
    Wildcard
  );
  {$SCOPEDENUMS OFF}

//  MediaTypeHelper = record helper for MediaType
//    function ToString: string;
//  end;

  TMediaTypeParams = TDictionary<string, string>;

  TMediaType = class
  private
    FPFactor: Integer;
    FMediaType: string;
    FMediaSubType: string;
    FMediaParameters: TStringList;
    FVersion: Integer;
    FQFactor: Double;
    FDialect: string;
    FCharset: string;
    function GetWeigth: Integer;
    function GetIsWildcard: Boolean;
  public
    const DELIM_MEDIA = '/';
    const DELIM_PARAMS = ';';
    const QFACTOR_NAME = 'q';
    const VERSION_NAME = 'version';
    const DIALECT_NAME = 'dialect';
    const CHARSET_NAME = 'charset';

    const CHARSET_ISO_8859_1 = 'iso-8859-1';
    const CHARSET_UTF8 = 'utf-8';
    const CHARSET_UTF16 = 'utf-16';

    const CHARSET_ISO_8859_1_DEF = CHARSET_NAME + '=' +  CHARSET_ISO_8859_1;
    const CHARSET_UTF8_DEF = CHARSET_NAME + '=' +  CHARSET_UTF8;
    const CHARSET_UTF16_DEF = CHARSET_NAME + '=' +  CHARSET_UTF16;

    const TEXT_PLAIN = 'text/plain';
    const TEXT_XML = 'text/xml';
    const TEXT_HTML = 'text/html';
    const APPLICATION_XML = 'application/xml';
    const APPLICATION_JSON = 'application/json';
    const APPLICATION_JSON_FireDAC = 'application/json-firedac';
    const APPLICATION_XHTML_XML = 'application/xhtml+xml';
    const APPLICATION_SVG_XML = 'application/svg+xml';
    const APPLICATION_ATOM_XML = 'application/atom+xml';
    const APPLICATION_OCTET_STREAM = 'application/octet-stream';
    const APPLICATION_FORM_URLENCODED_TYPE = 'application/x-www-form-urlencoded';
    const MULTIPART_FORM_DATA = 'multipart/form-data';
    const WILDCARD = '*/*';
  public
    constructor Create; overload;
    constructor Create(const AMediaType: string); overload;
    constructor Create(const AType, ASubType: string); overload;
    constructor Create(const AType, ASubType, AParams: string); overload;
    constructor Create(const AType, ASubType: string; AParams: TStringList); overload;
    destructor Destroy; override;

    function ToString: string; override;
    function ToStringDebug: string;

    function Matches(const AMediaTypeStr: string): Boolean; overload;
    function Matches(const AMediaType: TMediaType): Boolean; overload;

    property MediaType: string read FMediaType;
    property MediaSubType: string read FMediaSubType;
    property MediaParameters: TStringList read FMediaParameters;

    property QFactor: Double read FQFactor write FQFactor;
    property PFactor: Integer read FPFactor write FPFactor;
    property Version: Integer read FVersion write FVersion;
    property Dialect: string read FDialect write FDialect;
    property Charset: string read FCharset write FCharset;
    property Weight: Integer read GetWeigth;
    property IsWildcard: Boolean read GetIsWildcard;
  end;

  TMediaTypeList = class(TObjectList<TMediaType>)
  private
  protected
  public
    constructor Create; virtual;

    function ToArrayOfString: TArray<string>;


    function Contains(const AMediaType: string): Boolean;
    function GetQualityFactor(const AMediaType: string): Double;

    class function Intersect(const AList1, AList2: TMediaTypeList): TArray<string>; overload;
    class function Intersect(const AList1: TArray<string>;
      const AList2: TMediaTypeList): TArray<string>; overload;
    function ToString: string; override;
  end;

  TAcceptParser = class
  private
    const DELIM_ACCEPT = ',';
  private
    FAccept: string;
    FMediaTypeList: TMediaTypeList;
  public
    constructor Create(AAccept: string);
    destructor Destroy; override;

    procedure Clear;
    procedure ParseAccept;

    property MediaTypeList: TMediaTypeList read FMediaTypeList write FMediaTypeList;
  end;


implementation

uses
  StrUtils;

{ MediaTypeHelper }

//function MediaTypeHelper.ToString: string;
//begin
//  case Self of
//    MediaType.Text_Plain: Result := 'text/plain';
//    MediaType.Text_XML: Result := 'text/xml';
//    MediaType.Text_HTML: Result := 'text/html';
//
//    MediaType.Application_XML: Result := 'application/xml';
//    MediaType.Application_JSON: Result := 'application/json';
//    MediaType.Application_XHTML_XML: Result := 'application/xhtml+xml';
//    MediaType.Application_SVG_XML: Result := 'application/svg+xml';
//    MediaType.Application_Atom_XML: Result := 'application/atom+xml';
//    MediaType.Application_Octet_Stream: Result := 'application/octet-stream';
//    MediaType.Application_Form_Encoded: Result := 'application/x-www-form-urlencoded';
//
//    MediaType.Multipart_Form_Data: Result := 'multipart/form-data';
//
//    MediaType.Wildcard: Result := '*/*';
//  end;
//end;

{ TMediaType }

constructor TMediaType.Create(const AType, ASubType: string);
begin
  Create;
  FMediaType := AType;
  FMediaSubType := ASubType;
end;

constructor TMediaType.Create(const AType, ASubType: string; AParams: TStringList);
begin
  Create;
  FMediaType := AType;
  FMediaSubType := ASubType;
  FMediaParameters.Assign(AParams);
end;

constructor TMediaType.Create;
begin
  FQFactor := 1;
  FMediaParameters := TStringList.Create;
  FMediaParameters.Delimiter := DELIM_PARAMS;
end;

constructor TMediaType.Create(const AMediaType: string);
var
  LSplitted: TArray<string>;

  procedure ParseMediaType(AMediaTypeStr: string);
  var
    LParsed: TArray<string>;
  begin
    LParsed := TArray<string>(SplitString(AMediaTypeStr, DELIM_MEDIA));
    case Length(LParsed) of
      0: ; // Error ;
      1:
      begin
        FMediaType := Trim(LParsed[0]);
        FMediaSubType := '';
      end;
      2:
      begin
        FMediaType := Trim(LParsed[0]);
        FMediaSubType := Trim(LParsed[1]);
      end;
    end;
  end;

  procedure ParseMediaParams(AParams: TArray<string>);
  var
    LUSFormat: TFormatSettings;
    LIndex, LPosition: Integer;
  begin
    LUSFormat := TFormatSettings.Create('en');
    LUSFormat.DecimalSeparator := '.'; // do not localize
    for LIndex := 1 to High(LSplitted) do
    begin
      LPosition := FMediaParameters.Add(Trim(LSplitted[LIndex]));

      if FMediaParameters.Names[LPosition] = QFACTOR_NAME then
        FQFactor := StrToFloatDef(FMediaParameters.ValueFromIndex[LPosition], 0.5, LUSFormat)
      else if FMediaParameters.Names[LPosition] = VERSION_NAME then
        FVersion := StrToIntDef(FMediaParameters.ValueFromIndex[LPosition], 1)
      else if FMediaParameters.Names[LPosition] = DIALECT_NAME then
        FDialect := FMediaParameters.ValueFromIndex[LPosition]
      else if FMediaParameters.Names[LPosition] = CHARSET_NAME then
        FCharset := FMediaParameters.ValueFromIndex[LPosition];
    end;
  end;

begin
  Create;
  // Example: text/html;q=0.5;dialect=extjs
  LSplitted := TArray<string>(SplitString(AMediaType, DELIM_PARAMS));

  case Length(LSplitted) of
    0: ; // Error
    1: ParseMediaType(LSplitted[0]);
    else
    begin
      ParseMediaType(LSplitted[0]);
      ParseMediaParams(LSplitted);
    end;
  end;
end;

constructor TMediaType.Create(const AType, ASubType, AParams: string);
begin
  Create;
  FMediaType := AType;
  FMediaSubType := ASubType;
  FMediaParameters.Text := AParams;
end;

destructor TMediaType.Destroy;
begin
  FMediaParameters.Free;
  inherited;
end;

function TMediaType.GetIsWildcard: Boolean;
begin
  Result := ToString = WILDCARD;
end;

function TMediaType.GetWeigth: Integer;
begin
  Result := Trunc(FQFactor * 10) + (FPFactor * 1);
end;

function TMediaType.Matches(const AMediaTypeStr: string): Boolean;
begin
  Result :=
    SameText(ToString, AMediaTypeStr)
    or IsWildcard;
end;

function TMediaType.Matches(const AMediaType: TMediaType): Boolean;
begin
  Result := Matches(AMediaType.ToString);
end;

function TMediaType.ToString: string;
//var
//  LIndex: Integer;
begin
  Result := FMediaType + DELIM_MEDIA + FMediaSubType;
  if FDialect <> '' then
    Result := Result + DELIM_PARAMS + DIALECT_NAME + '=' + FDialect;
  if FCharset <> '' then
    Result := Result + DELIM_PARAMS + CHARSET_NAME + '=' + FCharset;

//  for LIndex := 0 to FMediaParameters.Count - 1 do
//    Result := Result + DELIM_PARAMS + FMediaParameters[LIndex];
end;

function TMediaType.ToStringDebug: string;
const
  DEBUG_STR = '%s [QFactor:%f] [PFactor:%f] [Weight:%f]';
begin
  Result := Format(DEBUG_STR, [ToString, QFactor, PFactor, Weight]);
end;

{ TAcceptParser }

procedure TAcceptParser.Clear;
begin
  FMediaTypeList.Clear;
end;

constructor TAcceptParser.Create(AAccept: string);
begin
  FAccept := AAccept;
  FMediaTypeList := TMediaTypeList.Create;
  ParseAccept;
end;

destructor TAcceptParser.Destroy;
begin
  FMediaTypeList.Free;
  inherited;
end;

procedure TAcceptParser.ParseAccept;
var
  LMediaArray: TArray<string>;
  LMediaStr: string;
  LMediaType: TMediaType;
  LIndex, LLength: Integer;
begin
  Clear;
  LMediaArray := TArray<string>(SplitString(FAccept, DELIM_ACCEPT));
  LLength := Length(LMediaArray);

  for LIndex := Low(LMediaArray) to High(LMediaArray) do
  begin
    LMediaStr := LMediaArray[LIndex];
    LMediaType := TMediaType.Create(LMediaStr);
    LMediaType.PFactor := LLength - LIndex;
    FMediaTypeList.Add(LMediaType);
  end;

  if FMediaTypeList.Count = 0 then
    FMediaTypeList.Add(TMediaType.Create(TMediaType.WILDCARD));

  FMediaTypeList.Sort;
end;

{ TMediaTypeList }

function TMediaTypeList.Contains(const AMediaType: string): Boolean;
var
  LItem: TMediaType;
begin
  Result := False;
  for LItem in Self do
    if LItem.ToString = AMediaType then
    begin
      Result := True;
      Break;
    end;
end;

constructor TMediaTypeList.Create;
begin
  inherited Create(
    TComparer<TMediaType>.Construct(
      function(const Left, Right: TMediaType): Integer
      begin
        Result := Right.Weight - Left.Weight;
      end
    )
    , True);
end;

function TMediaTypeList.GetQualityFactor(const AMediaType: string): Double;
var
  LItem: TMediaType;
begin
  Result := 0.0;
  for LItem in Self do
    if LItem.ToString = AMediaType then
    begin
      Result := LItem.QFactor;
      Break;
    end;
end;

class function TMediaTypeList.Intersect(const AList1: TArray<string>;
  const AList2: TMediaTypeList): TArray<string>;
var
  LMediaType: string;
begin
  SetLength(Result, 0);
  for LMediaType in AList1 do
  begin
    if AList2.Contains(LMediaType) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) -1 ] := LMediaType;
    end;
  end;
end;

class function TMediaTypeList.Intersect(const AList1,
  AList2: TMediaTypeList): TArray<string>;
begin
  Result := Intersect(AList1.ToArrayOfString, AList2);
end;

function TMediaTypeList.ToArrayOfString: TArray<string>;
var
  LIndex: Integer;
begin
  SetLength(Result, Count);
  for LIndex := 0 to Count - 1 do
    Result[LIndex] := Items[LIndex].ToString;
end;

function TMediaTypeList.ToString: string;
begin
  Result := string.join(',', ToArrayOfString);
end;

end.
