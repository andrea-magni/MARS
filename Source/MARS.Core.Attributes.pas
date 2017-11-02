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
  , MARS.Core.URL  
  , MARS.Core.JSON
  , MARS.Core.Activation.Interfaces
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

  ANYAttribute     = class(HttpMethodAttribute)
  public
    function Matches(const ARequest: TWebRequest): Boolean; override;
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

  OPTIONSAttribute = class(HttpMethodAttribute)
  public
    function Matches(const ARequest: TWebRequest): Boolean; override;
  end;

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

  ContextAttribute = class(MARSAttribute);

  RequestParamAttribute = class(ContextAttribute)
  private
  protected
    function GetKind: string; virtual;
  public
    function GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): TValue; virtual;
    property Kind: string read GetKind;
  end;

  NamedRequestParamAttribute = class(RequestParamAttribute)
  private
    FName: string;
  protected
    function GetActualName(const ADestination: TRttiObject): string; virtual;
  public
    constructor Create(const AName: string = ''); virtual;
    property Name: string read FName write FName;
  end;

  PathParamAttribute = class(NamedRequestParamAttribute)
  private
    FParamIndex: Integer;
  protected
    function GetParamIndex(const ADestination: TRttiObject;
      const APrototypeURL: TMARSURL): Integer;
  public
    property ParamIndex: Integer read FParamIndex write FParamIndex;
    function GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): TValue; override;
  end;

  QueryParamAttribute = class(NamedRequestParamAttribute)
  public
    function GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): TValue; override;
  end;

  FormParamAttribute = class(NamedRequestParamAttribute)
  public
    function GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): TValue; override;
  end;

  HeaderParamAttribute = class(NamedRequestParamAttribute)
  public
    function GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): TValue; override;
  end;

  CookieParamAttribute = class(NamedRequestParamAttribute)
  public
    function GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): TValue; override;
  end;

  BodyParamAttribute = class(RequestParamAttribute)
  public
    function GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): TValue; override;
  end;

  AuthorizationAttribute = class(MARSAttribute)
  public
    function ToString: string; override;
  end;

  PermitAllAttribute = class(AuthorizationAttribute);
  DenyAllAttribute = class(AuthorizationAttribute);
  RolesAllowedAttribute = class(AuthorizationAttribute)
  private
    FRoles: TStringList;
  protected
    constructor Create(const ARoleNames: TArray<string>); overload; virtual;
  public
    constructor Create(const ARoleNames: string); overload; virtual;
    destructor Destroy; override;
    function ToString: string; override;

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

  JSONPAttribute = class(MARSAttribute)
  private
    FEnabled: Boolean;
    FCallbackKey: string;
    FContentType: string;
  protected
  public
    constructor Create(const AEnabled: Boolean; const ACallbackKey: string = 'callback';
      const AContentType: string = 'text/javascript');
    property Enabled: Boolean read FEnabled;
    property CallbackKey: string read FCallbackKey;
    property ContentType: string read FContentType;
  end;

implementation

uses
  StrUtils, DateUtils
, MARS.Rtti.Utils
, MARS.Core.MessageBodyReader
, MARS.Core.MediaType
{$ifndef DelphiXE7_UP}
, TypInfo
{$endif}
;

function StringToTValue(const AString: string; const ADesiredType: TRttiType): TValue;
begin
  if ADesiredType.IsObjectOfType<TJSONValue> then
    Result := TJSONObject.ParseJSONValue(AString)
  else
  begin
    case ADesiredType.TypeKind of
      tkInt64,
      tkInteger: Result := StrToIntDef(AString, 0);
      tkEnumeration: begin
        if SameText(ADesiredType.Name, 'Boolean') then
          Result := StrToBoolDef(AString, False);
      end;
      tkFloat: begin
        if IndexStr(ADesiredType.Name, ['TDate', 'TDateTime', 'TTime']) <> -1  then
        begin
          try
            Result := ISO8601ToDate(AString);
          except
            Result := StrToDateTime(AString)
          end;
        end
        else
          Result := StrToFloatDef(AString, 0.0);
      end;

  {$ifdef DelphiXE7_UP}
      tkChar: begin
                if AString.IsEmpty then
                  Result := ''
                else
                  Result := TValue.From(AString.Chars[0]);
              end;
  {$else}
      tkChar: Result := TValue.From(Copy(AString, 1, 1));
  {$endif}
      else
        Result := AString;
    end;
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

constructor RolesAllowedAttribute.Create(const ARoleNames: string);
{$ifdef DelphiXE7_UP}
begin
  Create(ARoleNames.Split([',', ' ', ';'], TStringSplitOptions.ExcludeEmpty));
{$else}
var
  LTokens: TStringList;
begin
  LTokens := TStringList.Create;
  try
    ExtractStrings([',', ' ', ';'], [], PChar(ARoleNames), LTokens);
    Create(LTokens.ToStringArray);
  finally
    LTokens.Free;
  end;
{$endif}
end;

constructor RolesAllowedAttribute.Create(const ARoleNames: TArray<string>);
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

function RolesAllowedAttribute.ToString: string;
begin
  Result := inherited ToString;
  if Roles.Count > 0 then
    Result := Result + ': ' + Roles.CommaText;
end;

{ BodyParamAttribute }

function BodyParamAttribute.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation): TValue;
var
  LMediaType: TMediaType;
  LReader: IMessageBodyReader;
begin
  // 1 - MessageBodyReader mechanism (standard)
  TMARSMessageBodyReaderRegistry.Instance.FindReader(ADestination, LReader, LMediaType);
  if Assigned(LReader) then
    try
      Result := LReader.ReadFrom(AActivation.Request.RawContent, ADestination, LMediaType, AActivation);
    finally
      FreeAndNil(LMediaType);
    end
  else // 2 - fallback (raw)
  begin
{$ifdef Delphi10Berlin_UP}
    case ADestination.GetRttiType.TypeKind of
      tkInt64, tkInteger, tkFloat, tkChar
      , tkLString, tkWString, tkString: StringToTValue(AActivation.Request.Content, ADestination.GetRttiType);

      tkUString: Result := TEncoding.UTF8.GetString(AActivation.Request.RawContent);

      else
        Result := TValue.From<TBytes>(AActivation.Request.RawContent);
    end;
{$else}
    case ADestination.GetRttiType.TypeKind of
      tkInt64, tkInteger, tkFloat, tkChar
      , tkLString, tkWString, tkString: StringToTValue(AActivation.Request.Content, ADestination.GetRttiType);

      tkUString: Result := AActivation.Request.RawContent;

      else
        Result := AActivation.Request.RawContent;
    end;

{$endif}
  end;
end;

{ HttpMethodAttribute }

function HttpMethodAttribute.GetHttpMethodName: string;
begin
{$ifdef DelphiXE7_UP}
  Result := ClassName.Replace('Attribute', '');
{$else}
  Result := StringReplace(ClassName, 'Attribute', '', [rfIgnoreCase]);
{$endif}
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
  const ADestination: TRttiObject): string;
begin
  Result := Name;
  if (Name = '') and Assigned(ADestination) and (ADestination is TRttiNamedObject) then
    Result := TRttiNamedObject(ADestination).Name;
end;

{ RequestParamAttribute }

function RequestParamAttribute.GetKind: string;
begin
{$ifdef DelphiXE7_UP}
  Result := ClassName.Replace('Attribute', '');
{$else}
  Result := StringReplace(ClassName, 'Attribute', '', [rfIgnoreCase]);
{$endif}
end;

function RequestParamAttribute.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation): TValue;
begin
  Result := TValue.Empty;
end;

{ QueryParamAttribute }

function QueryParamAttribute.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation): TValue;
begin
  Result := StringToTValue(
      AActivation.Request.QueryFields.Values[GetActualName(ADestination)]
    , ADestination.GetRttiType
  );
end;

{ FormParamAttribute }

function FormParamAttribute.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation): TValue;
begin
  Result := StringToTValue(
      AActivation.Request.ContentFields.Values[GetActualName(ADestination)]
    , ADestination.GetRttiType
  );
end;

{ HeaderParamAttribute }

function HeaderParamAttribute.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation): TValue;
begin
  Result := StringToTValue(
      AActivation.Request.GetFieldByName(GetActualName(ADestination))
    , ADestination.GetRttiType
  );
end;

{ CookieParamAttribute }

function CookieParamAttribute.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation): TValue;
begin
  Result := StringToTValue(
      AActivation.Request.CookieFields.Values[GetActualName(ADestination)]
    , ADestination.GetRttiType
  );
end;

{ PathParamAttribute }

function PathParamAttribute.GetParamIndex(
  const ADestination: TRttiObject; const APrototypeURL: TMARSURL): Integer;
begin
  Result := APrototypeURL.GetPathParamIndex(
    GetActualName(ADestination)
  );
end;

function PathParamAttribute.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation): TValue;
var
  LTokenIndex: Integer;
  LTokenPrototype: string;
  LValue: string;
  LIndex: Integer;
begin
  Result := TValue.Empty;
  LTokenIndex := GetParamIndex(ADestination, AActivation.URLPrototype);
  if LTokenIndex > -1 then
  begin
    LTokenPrototype := AActivation.URLPrototype.PathTokens[LTokenIndex];
    if LTokenPrototype <> TMARSURL.PATH_PARAM_WILDCARD then
      LValue := AActivation.URL.PathTokens[LTokenIndex]
    else
    begin
      LValue := '';
      for LIndex := LTokenIndex to High(AActivation.URL.PathTokens) do
      begin
        if LValue <> '' then
          LValue := LValue + TMARSURL.URL_PATH_SEPARATOR;
        LValue := LValue + AActivation.URL.PathTokens[LIndex];
      end;
    end;

    Result := StringToTValue(LValue, ADestination.GetRttiType);
  end;
end;

{ AuthorizationAttribute }

function AuthorizationAttribute.ToString: string;
begin
  Result := ClassName;
  if Result.EndsWith('Attribute', True) then
    Result := Result.Substring(0, Result.Length - 'Attribute'.Length);
end;

{ ANYAttribute }

function ANYAttribute.Matches(const ARequest: TWebRequest): Boolean;
begin
  Result := ARequest.MethodType = TMethodType.mtAny;
end;

{ OPTIONSAttribute }

function OPTIONSAttribute.Matches(const ARequest: TWebRequest): Boolean;
begin
  Result := SameText(ARequest.Method, 'OPTIONS');
end;

{ JSONPAttribute }

constructor JSONPAttribute.Create(const AEnabled: Boolean; const ACallbackKey,
  AContentType: string);
begin
  inherited Create;
  FEnabled := AEnabled;
  FCallbackKey := ACallbackKey;
  FContentType := AContentType;
end;

end.
