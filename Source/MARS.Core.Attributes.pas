(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Attributes;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, RTTI, Generics.Collections
  , HttpApp
  , MARS.Core.Declarations
  , MARS.Core.Utils
  , MARS.Core.JSON
;

type
  MARSAttribute = class(TCustomAttribute);

  PathAttribute = class(MARSAttribute)
  private
    FValue: string;
  public
    constructor Create(const AValue: string);
    property Value: string read FValue write FValue;
  end;

  HttpMethodAttribute = class(MARSAttribute)
  private
  protected
    function GetHttpMethodName: string; virtual;
  public
    function Matches(const ARequest: TWebRequest): Boolean; virtual;
    property HttpMethodName: string read GetHttpMethodName;
  end;

  GETAttribute     = class(HttpMethodAttribute)
  public
    function Matches(const ARequest: TWebRequest): Boolean; override;
  end;

  POSTAttribute    = class(HttpMethodAttribute)
  public
    function Matches(const ARequest: TWebRequest): Boolean; override;
  end;

  PUTAttribute     = class(HttpMethodAttribute)
  public
    function Matches(const ARequest: TWebRequest): Boolean; override;
  end;

  DELETEAttribute  = class(HttpMethodAttribute)
  public
    function Matches(const ARequest: TWebRequest): Boolean; override;
  end;

  PATCHAttribute   = class(HttpMethodAttribute)
  public
    function Matches(const ARequest: TWebRequest): Boolean; override;
  end;

  HEADAttribute    = class(HttpMethodAttribute)
  public
    function Matches(const ARequest: TWebRequest): Boolean; override;
  end;

  OPTIONSAttribute = class(HttpMethodAttribute);

  ConsumesAttribute = class(MARSAttribute)
  private
    FValue: string;
  public
    constructor Create(const AValue: string);
    property Value: string read FValue write FValue;
  end;

  ProducesAttribute = class(MARSAttribute)
  private
    FValue: string;
  public
    constructor Create(const AValue: string);
    property Value: string read FValue write FValue;
  end;

  RequestParamAttribute = class(MARSAttribute)
  private
  protected
  public
    function GetValue(const ARequest: TWebRequest; const AParam: TRttiParameter): TValue; virtual;
  end;

  NamedRequestParamAttribute = class(RequestParamAttribute)
  private
    FName: string;
  protected
    function GetActualName(const AParam: TRttiParameter): string; virtual;
  public
    constructor Create(const AName: string = ''); virtual;
    property Name: string read FName write FName;
  end;

  PathParamAttribute = class(NamedRequestParamAttribute)
  private
    FParamIndex: Integer;
  protected
    function GetParamIndex(const AParam: TRttiParameter): Integer;
  public
    property ParamIndex: Integer read FParamIndex write FParamIndex;
    function GetValue(const ARequest: TWebRequest; const AParam: TRttiParameter): TValue; override;
  end;

  QueryParamAttribute = class(NamedRequestParamAttribute)
  public
    function GetValue(const ARequest: TWebRequest; const AParam: TRttiParameter): TValue; override;
  end;

  FormParamAttribute = class(NamedRequestParamAttribute)
  public
    function GetValue(const ARequest: TWebRequest; const AParam: TRttiParameter): TValue; override;
  end;

  HeaderParamAttribute = class(NamedRequestParamAttribute)
  public
    function GetValue(const ARequest: TWebRequest; const AParam: TRttiParameter): TValue; override;
  end;

  CookieParamAttribute = class(NamedRequestParamAttribute)
  public
    function GetValue(const ARequest: TWebRequest; const AParam: TRttiParameter): TValue; override;
  end;

  BodyParamAttribute = class(RequestParamAttribute)
  public
    function GetValue(const ARequest: TWebRequest; const AParam: TRttiParameter): TValue; override;
  end;

  ContextAttribute = class(MARSAttribute);

  AuthorizationAttribute = class(MARSAttribute);

  PermitAllAttribute = class(AuthorizationAttribute);
  DenyAllAttribute = class(AuthorizationAttribute);
  RolesAllowedAttribute = class(AuthorizationAttribute)
  private
    FRoles: TStringList;
  protected
  public
    constructor Create(ARoleName: string); overload; virtual;
    constructor Create(ARoleNames: array of string); overload; virtual;
    destructor Destroy; override;

    property Roles: TStringList read FRoles;
  end;

  ResultIsReference = class(MARSAttribute)
  end deprecated 'Use IsReference instead';

{$WARNINGS OFF}
  IsReference = ResultIsReference;
{$WARNINGS ON}

  ContentTypeAttribute = class(MARSAttribute)
  private
    FContentType: string;
  public
    constructor Create(const AContentType: string);
    property ContentType: string read FContentType;
  end;

  CustomHeaderAttribute = class(MARSAttribute)
  private
    FHeaderName: string;
    FValue: string;
  public
    constructor Create(const AHeaderName, AValue: string);
    property HeaderName: string read FHeaderName;
    property Value: string read FValue;
  end;

implementation

uses
    MARS.Rtti.Utils
  , MARS.Core.URL
  , MARS.Core.MessageBodyReader
  , MARS.Core.MediaType
;

function StringToTValue(const AString: string; const ADesiredType: TRttiType): TValue;
begin
  case ADesiredType.TypeKind of
    tkInt64,
    tkInteger: Result := StrToInt(AString);

    tkFloat: Result := StrToFloat(AString);

    tkChar: Result := TValue.From(AnsiChar(AString.Chars[0]));

    else
      Result := AString;
  end;
end;

{ ContentTypeAttribute }

constructor ContentTypeAttribute.Create(const AContentType: string);
begin
  inherited Create;
  FContentType := AContentType;
end;

{ PathAttribute }

constructor PathAttribute.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

{ ConsumesAttribute }

constructor ConsumesAttribute.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

{ ProducesAttribute }

constructor ProducesAttribute.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

{ RolesAllowedAttribute }

constructor RolesAllowedAttribute.Create(ARoleName: string);
begin
  Create([ARoleName]);
end;

constructor RolesAllowedAttribute.Create(ARoleNames: array of string);
var
  LRole: string;
begin
  inherited Create;

  FRoles := TStringList.Create;

  for LRole in ARoleNames do
    FRoles.Add(LRole);
end;

destructor RolesAllowedAttribute.Destroy;
begin
  FRoles.Free;
  inherited;
end;

{ BodyParamAttribute }

function BodyParamAttribute.GetValue(const ARequest: TWebRequest;
  const AParam: TRttiParameter): TValue;
var
  LMediaType: TMediaType;
  LReader: IMessageBodyReader;
  LMethod: TRttiMethod;
begin
  // 1 - MessageBodyReader mechanism (standard)
  TMARSMessageBodyReaderRegistry.Instance.FindReader(AParam, LReader, LMediaType);
  if Assigned(LReader) then
    try
      LMethod := AParam.Parent as TRttiMethod;
      Result := LReader.ReadFrom(ARequest.RawContent, LMethod.GetAttributes, LMediaType, nil);
    finally
      FreeAndNil(LMediaType);
    end
  else // 2 - fallback (raw)
  begin
    case AParam.ParamType.TypeKind of
      tkInt64, tkInteger, tkFloat, tkChar
      , tkLString, tkWString, tkString: StringToTValue(ARequest.Content, AParam.ParamType);

      tkUString: Result := TEncoding.UTF8.GetString(ARequest.RawContent);

      else
        Result := TValue.From<TBytes>(ARequest.RawContent);
    end;
  end;
end;

{ HttpMethodAttribute }

function HttpMethodAttribute.GetHttpMethodName: string;
begin
  Result := ClassName.Replace('Attribute', '');
end;

function HttpMethodAttribute.Matches(const ARequest: TWebRequest): Boolean;
begin
  Result := False;
end;

{ GETAttribute }

function GETAttribute.Matches(const ARequest: TWebRequest): Boolean;
begin
  Result := ARequest.MethodType = TMethodType.mtGet;
end;

{ POSTAttribute }

function POSTAttribute.Matches(const ARequest: TWebRequest): Boolean;
begin
  Result := ARequest.MethodType = TMethodType.mtPost;
end;

{ PUTAttribute }

function PUTAttribute.Matches(const ARequest: TWebRequest): Boolean;
begin
  Result := ARequest.MethodType = TMethodType.mtPut;
end;

{ DELETEAttribute }

function DELETEAttribute.Matches(const ARequest: TWebRequest): Boolean;
begin
{$ifdef DelphiXE7_UP}
  Result := ARequest.MethodType = TMethodType.mtDelete;
{$else}
  Result := SameText(string(ARequest.Method), 'Delete');
{$endif}
end;

{ PATCHAttribute }

function PATCHAttribute.Matches(const ARequest: TWebRequest): Boolean;
begin
{$ifdef DelphiXE7_UP}
  Result := ARequest.MethodType = TMethodType.mtPatch;
{$else}
  Result := SameText(string(ARequest.Method), 'Patch');
{$endif}
end;

{ HEADAttribute }

function HEADAttribute.Matches(const ARequest: TWebRequest): Boolean;
begin
  Result := ARequest.MethodType = TMethodType.mtHead;
end;

{ CustomHeaderAttribute }

constructor CustomHeaderAttribute.Create(const AHeaderName, AValue: string);
begin
  inherited Create;
  FHeaderName := AHeaderName;
  FValue := AValue;
end;

{ NamedRequestParamAttribute }

constructor NamedRequestParamAttribute.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

function NamedRequestParamAttribute.GetActualName(
  const AParam: TRttiParameter): string;
begin
  Result := Name;
  if Name.IsEmpty and Assigned(AParam) then
    Result := AParam.Name;
end;

{ RequestParamAttribute }

function RequestParamAttribute.GetValue(const ARequest: TWebRequest;
  const AParam: TRttiParameter): TValue;
begin
  Result := TValue.Empty;
end;

{ QueryParamAttribute }

function QueryParamAttribute.GetValue(const ARequest: TWebRequest; const AParam: TRttiParameter): TValue;
begin
  Result := StringToTValue(ARequest.QueryFields.Values[GetActualName(AParam)], AParam.ParamType);
end;

{ FormParamAttribute }

function FormParamAttribute.GetValue(const ARequest: TWebRequest;
  const AParam: TRttiParameter): TValue;
begin
  Result := StringToTValue(ARequest.ContentFields.Values[GetActualName(AParam)], AParam.ParamType);
end;

{ HeaderParamAttribute }

function HeaderParamAttribute.GetValue(const ARequest: TWebRequest;
  const AParam: TRttiParameter): TValue;
begin
  Result := StringToTValue(ARequest.GetFieldByName(GetActualName(AParam)), AParam.ParamType);
end;

{ CookieParamAttribute }

function CookieParamAttribute.GetValue(const ARequest: TWebRequest;
  const AParam: TRttiParameter): TValue;
begin
  Result := StringToTValue(ARequest.CookieFields.Values[GetActualName(AParam)], AParam.ParamType);
end;

{ PathParamAttribute }

function PathParamAttribute.GetParamIndex(
  const AParam: TRttiParameter): Integer;
var
  LParamIndex: Integer;
  LSubResourcePath: string;
  LMethod: TRttiMethod;
  LResource: TRttiType;
begin
  Assert(Assigned(AParam));

  LMethod := AParam.Parent as TRttiMethod;
  Assert(Assigned(LMethod));

  LResource := LMethod.Parent;
  Assert(Assigned(LResource));

  LParamIndex := -1;

  LSubResourcePath := '';
  LMethod.HasAttribute<PathAttribute>(
    procedure (ASubResourcePathAttrib: PathAttribute)
    begin
      LSubResourcePath := ASubResourcePathAttrib.Value;
    end
  );

  LResource.HasAttribute<PathAttribute>(
    procedure (AResourcePathAttrib: PathAttribute)
    var
      LResURL: TMARSURL;
      LPair: TPair<Integer, string>;
      LParamName: string;
    begin

      //AM TODO: find a way to use Engine.BasePath, Application.BasePath
      // instead of '/rest', '/default' consts.
      // Reason:
      // The following piece of code calculates a positional index of the
      // parameter in the URL and it builds a dummy URL to get a prototype
      // (with all parameters listed) of the resource. It is unusual (yet
      // not impossible) to have a parameter in the BasePath of the Engine or
      // of the Application. I assume here that there is no parameter defined
      // into the BasePath of current Engine and current Application.
      // i.e. http://host:port/rest/default/myres/{first}/subres/{second}/{third}
      //                     ^^^^^^^^^^^^^^ no params allowed here!

      LResURL := TMARSURL.CreateDummy(['/rest', '/default'
        , AResourcePathAttrib.Value, LSubResourcePath]);
      try
        LParamName := GetActualName(AParam);
        LParamIndex := -1;
        for LPair in LResURL.PathParams do
        begin
          if SameText(LParamName, LPair.Value) then
          begin
            LParamIndex := LPair.Key;
            Break;
          end;
        end;
      finally
        LResURL.Free;
      end;
    end
  );

  Result := LParamIndex;
end;

function PathParamAttribute.GetValue(const ARequest: TWebRequest;
  const AParam: TRttiParameter): TValue;
var
  LURL: TMARSURL;
begin
  LURL := TMARSURL.Create(ARequest);
  try
    Result := StringToTValue(LURL.PathTokens[GetParamIndex(AParam)], AParam.ParamType);
  finally
    LURL.Free;
  end;
end;

end.
