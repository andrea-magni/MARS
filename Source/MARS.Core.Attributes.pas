(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Attributes;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, System.Rtti, System.TypInfo, Generics.Collections
, Web.ReqMulti
, MARS.Core.Declarations, MARS.Core.Utils, MARS.Core.URL, MARS.Core.JSON
, MARS.Core.Activation.Interfaces, MARS.Core.RequestAndResponse.Interfaces
, MARS.Core.Exceptions
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
    function Matches(const ARequest: IMARSRequest): Boolean; virtual;
    property HttpMethodName: string read GetHttpMethodName;
  end;

//  ANYAttribute     = class(HttpMethodAttribute)
//  public
//    function Matches(const ARequest: IMARSRequest): Boolean; override;
//  end;

  GETAttribute     = class(HttpMethodAttribute)
  public
    function Matches(const ARequest: IMARSRequest): Boolean; override;
  end;

  POSTAttribute    = class(HttpMethodAttribute)
  public
    function Matches(const ARequest: IMARSRequest): Boolean; override;
  end;

  PUTAttribute     = class(HttpMethodAttribute)
  public
    function Matches(const ARequest: IMARSRequest): Boolean; override;
  end;

  DELETEAttribute  = class(HttpMethodAttribute)
  public
    function Matches(const ARequest: IMARSRequest): Boolean; override;
  end;

  PATCHAttribute   = class(HttpMethodAttribute)
  public
    function Matches(const ARequest: IMARSRequest): Boolean; override;
  end;

  HEADAttribute    = class(HttpMethodAttribute)
  public
    function Matches(const ARequest: IMARSRequest): Boolean; override;
  end;

  OPTIONSAttribute = class(HttpMethodAttribute)
  public
    function Matches(const ARequest: IMARSRequest): Boolean; override;
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

  EncodingAttribute = class(MARSAttribute)
  private
    FEncodingName: string;
    function GetEncoding: TEncoding;
  public
    constructor Create(const AEncodingName: string);
    property Name: string read FEncodingName write FEncodingName;
    property Encoding: TEncoding read GetEncoding;
  end;

  InvocationEventAttribute = class(MARSAttribute);

  BeforeInvokeAttribute = class(InvocationEventAttribute);
  AfterInvokeAttribute = class(InvocationEventAttribute);
  InvokeErrorAttribute = class(InvocationEventAttribute);

  ContextAttribute = class(MARSAttribute);

  ConfigParamAttribute = class(ContextAttribute)
  protected
  public
    function GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): TValue; virtual;
  end;

  ConfigSingleParamAttribute = class(ConfigParamAttribute)
  private
    FName: string;
    FDefaultValue: TValue;
  public
    constructor Create(const AName: string; const ADefaultValue: TValue); overload;
    constructor Create(const AName: string); overload;

    property Name: string read FName write FName;
    property DefaultValue: TValue read FDefaultValue write FDefaultValue;
  end;

  EngineParamAttribute = class(ConfigSingleParamAttribute)
  public
    function GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): TValue; override;
  end;

  ApplicationParamAttribute = class(ConfigSingleParamAttribute)
  public
    function GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): TValue; override;
  end;

  ConfigParamFuncAttribute = class(ConfigParamAttribute);
  TConfigParamFunc = reference to function(const AName: string): TValue;

  EngineParamFuncAttribute = class(ConfigParamFuncAttribute)
  public
    function GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): TValue; override;
  end;

  ApplicationParamFuncAttribute = class(ConfigParamFuncAttribute)
  public
    function GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): TValue; override;
  end;

  RequestParamAttribute = class(ContextAttribute)
  private
  protected
    function GetKind: string; virtual;
    function GetSwaggerKind: string; virtual;
    function GetActualName(const ADestination: TRttiObject): string; virtual;
    procedure CheckRequiredAttribute(const ADestination: TRttiObject); virtual;
  public
    function GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): TValue; virtual;
    property Kind: string read GetKind;
    property SwaggerKind: string read GetSwaggerKind;
  end;

  ERequiredException = class(EMARSHttpException);

  RequiredAttribute = class(MARSAttribute)
  private
  protected
  public
  end;

  NamedRequestParamAttribute = class(RequestParamAttribute)
  private
    FName: string;
  protected
    function GetActualName(const ADestination: TRttiObject): string; override;
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
    function GetSwaggerKind: string; override;
  public
    property ParamIndex: Integer read FParamIndex write FParamIndex;
    function GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): TValue; override;
  end;

  QueryParamAttribute = class(NamedRequestParamAttribute)
  protected
    function GetSwaggerKind: string; override;
  public
    function GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): TValue; override;
  end;

  FormParamAttribute = class(NamedRequestParamAttribute)
  protected
    function GetSwaggerKind: string; override;
  public
    function GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): TValue; override;
  end;

  FormParamsAttribute = class(RequestParamAttribute)
  protected
    function GetSwaggerKind: string; override;
  public
    function GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): TValue; override;
  end;

  HeaderParamAttribute = class(NamedRequestParamAttribute)
  protected
    function GetSwaggerKind: string; override;
  public
    function GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): TValue; override;
  end;

  CookieParamAttribute = class(NamedRequestParamAttribute)
  protected
    function GetSwaggerKind: string; override;
  public
    function GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): TValue; override;
  end;

  BodyParamAttribute = class(RequestParamAttribute)
  protected
    function GetSwaggerKind: string; override;
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
  MARS.Rtti.Utils
, MARS.Core.MessageBodyReader
, MARS.Core.MediaType
{$ifndef DelphiXE7_UP}
, TypInfo
{$endif}
;

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

function BodyParamAttribute.GetSwaggerKind: string;
begin
  Result := 'body';
end;

function BodyParamAttribute.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation): TValue;
var
  LMediaType: TMediaType;
  LReader: IMessageBodyReader;
begin
  if Length(AActivation.Request.RawContent) = 0 then
    CheckRequiredAttribute(ADestination);

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

      tkClass: Result := nil;

      else
        Result := TValue.From<TBytes>(AActivation.Request.RawContent);
    end;
{$else}
    case ADestination.GetRttiType.TypeKind of
      tkInt64, tkInteger, tkFloat, tkChar
      , tkLString, tkWString, tkString: StringToTValue(AActivation.Request.Content, ADestination.GetRttiType);

      tkUString: Result := AActivation.Request.RawContent;

      tkClass: Result := nil;

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

function HttpMethodAttribute.Matches(const ARequest: IMARSRequest): Boolean;
begin
  Result := False;
end;

{ GETAttribute }

function GETAttribute.Matches(const ARequest: IMARSRequest): Boolean;
begin
  Result := SameText(ARequest.Method, 'GET');
end;

{ POSTAttribute }

function POSTAttribute.Matches(const ARequest: IMARSRequest): Boolean;
begin
  Result := SameText(ARequest.Method, 'POST');
end;

{ PUTAttribute }

function PUTAttribute.Matches(const ARequest: IMARSRequest): Boolean;
begin
  Result := SameText(ARequest.Method, 'PUT');
end;

{ DELETEAttribute }

function DELETEAttribute.Matches(const ARequest: IMARSRequest): Boolean;
begin
  Result := SameText(ARequest.Method, 'DELETE');
end;

{ PATCHAttribute }

function PATCHAttribute.Matches(const ARequest: IMARSRequest): Boolean;
begin
  Result := SameText(ARequest.Method, 'PATCH');
end;

{ HEADAttribute }

function HEADAttribute.Matches(const ARequest: IMARSRequest): Boolean;
begin
  Result := SameText(ARequest.Method, 'HEAD');
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
  if Name <> '' then
    Result := Name
  else
    Result := inherited GetActualName(ADestination);
end;

{ RequestParamAttribute }

procedure RequestParamAttribute.CheckRequiredAttribute(const ADestination: TRttiObject);
begin
  if ADestination.HasAttribute<RequiredAttribute> then
    raise ERequiredException.CreateFmt('Required %s parameter missing: %s', [Self.GetSwaggerKind, GetActualName(ADestination)]);
end;

function RequestParamAttribute.GetActualName(
  const ADestination: TRttiObject): string;
begin
  Result := '';
  if Assigned(ADestination) and (ADestination is TRttiNamedObject) then
    Result := TRttiNamedObject(ADestination).Name;
end;

function RequestParamAttribute.GetKind: string;
begin
{$ifdef DelphiXE7_UP}
  Result := ClassName.Replace('Attribute', '');
{$else}
  Result := StringReplace(ClassName, 'Attribute', '', [rfIgnoreCase]);
{$endif}
end;

function RequestParamAttribute.GetSwaggerKind: string;
begin
  Result := '';
end;

function RequestParamAttribute.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation): TValue;
begin
  Result := TValue.Empty;
end;

{ QueryParamAttribute }

function QueryParamAttribute.GetSwaggerKind: string;
begin
  Result := 'query';
end;

function QueryParamAttribute.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation): TValue;
var
  LMediaType: TMediaType;
  LReader: IMessageBodyReader;
  LIndex: Integer;
begin
  LIndex := AActivation.Request.GetQueryParamIndex(GetActualName(ADestination));
  if (LIndex = -1) then
    CheckRequiredAttribute(ADestination)
  else
  begin
    // 1 - MessageBodyReader mechanism (standard)
    TMARSMessageBodyReaderRegistry.Instance.FindReader(ADestination, LReader, LMediaType);
    if Assigned(LReader) then
      try
        Result := LReader.ReadFrom(
          {$ifdef Delphi10Berlin_UP} TEncoding.UTF8.GetBytes( {$endif}
          AActivation.Request.GetQueryParamValue(LIndex)
          {$ifdef Delphi10Berlin_UP} ) {$endif}
          , ADestination, LMediaType, AActivation);
      finally
        FreeAndNil(LMediaType);
      end
    else // 2 - fallback (raw)
    begin
      Result := StringToTValue(
          AActivation.Request.GetQueryParamValue(LIndex)
        , ADestination.GetRttiType
      );
    end;
  end;
end;

{ FormParamAttribute }

function FormParamAttribute.GetSwaggerKind: string;
begin
  Result := 'formData';
end;

function FormParamAttribute.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation): TValue;
var
  LMediaType: TMediaType;
  LReader: IMessageBodyReader;
  LParamIndex, LFileIndex: Integer;
  LActualName: string;
begin
  LActualName := GetActualName(ADestination);
  LParamIndex := AActivation.Request.GetFormParamIndex(LActualName);
  LFileIndex := AActivation.Request.GetFormFileParamIndex(LActualName);
  if (LParamIndex = -1) and (LFileIndex = -1) then
    CheckRequiredAttribute(ADestination)
  else
  begin
    // 1 - MessageBodyReader mechanism (standard)
    TMARSMessageBodyReaderRegistry.Instance.FindReader(ADestination, LReader, LMediaType);
    if Assigned(LReader) then
      try
        if LParamIndex <> -1 then
          Result := LReader.ReadFrom(
            {$ifdef Delphi10Berlin_UP} TEncoding.UTF8.GetBytes( {$endif}
            AActivation.Request.GetFormParamValue(LParamIndex)
            {$ifdef Delphi10Berlin_UP} ) {$endif}
          , ADestination, LMediaType, AActivation)
        else if LFileIndex <> -1 then
          Result := LReader.ReadFrom(
            {$ifdef Delphi10Berlin_UP} TEncoding.UTF8.GetBytes( {$endif}
            ''
            {$ifdef Delphi10Berlin_UP} ) {$endif}
          , ADestination, LMediaType, AActivation);
      finally
        FreeAndNil(LMediaType);
      end
    else // 2 - fallback (raw)
      Result := StringToTValue(AActivation.Request.GetFormParamValue(LParamIndex), ADestination.GetRttiType);
  end;
end;

{ HeaderParamAttribute }

function HeaderParamAttribute.GetSwaggerKind: string;
begin
  Result := 'header';
end;

function HeaderParamAttribute.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation): TValue;

  function GetHeaderParamValue(const TheDestination: TRttiObject; const TheName: string): TValue;
  var
    LMediaType: TMediaType;
    LReader: IMessageBodyReader;
    LValue: string;
  begin
    Result := TValue.Empty;
    LValue := AActivation.Request.GetHeaderParamValue(TheName);
    if (LValue = '') then
      CheckRequiredAttribute(TheDestination);

    // 1 - MessageBodyReader mechanism (standard)
    TMARSMessageBodyReaderRegistry.Instance.FindReader(TheDestination, LReader, LMediaType);
    if Assigned(LReader) then
      try
        Result := LReader.ReadFrom(
          {$ifdef Delphi10Berlin_UP} TEncoding.UTF8.GetBytes( {$endif}
          LValue
          {$ifdef Delphi10Berlin_UP} ) {$endif}
          , TheDestination, LMediaType, AActivation);
      finally
        FreeAndNil(LMediaType);
      end
    else // 2 - fallback (raw)
    begin
      Result := StringToTValue(LValue, TheDestination.GetRttiType);
    end;
  end;

var
  LDestinationType: TRttiType;
  LRecordInstance: Pointer;
  LField: TRttiField;
  LName: string;
begin
  LDestinationType := ADestination.GetRttiType;

  if Name.Equals('*') and LDestinationType.IsRecord then
  begin
    TValue.Make(nil, LDestinationType.Handle, Result);
    LRecordInstance := Result.GetReferenceToRawData;

    for LField in LDestinationType.GetFields do
    begin
      LName := LField.Name;
      LField.HasAttribute<HeaderParamAttribute>(
        procedure (AAttribute: HeaderParamAttribute)
        begin
          LName := AAttribute.Name;
        end
      );

      LField.SetValue(LRecordInstance, GetHeaderParamValue(LField, LName));
    end;
  end
  else
    Result := GetHeaderParamValue(ADestination, GetActualName(ADestination));
end;

{ CookieParamAttribute }

function CookieParamAttribute.GetSwaggerKind: string;
begin
  Result := 'cookie';
end;

function CookieParamAttribute.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation): TValue;
var
  LIndex: Integer;
begin
  LIndex := AActivation.Request.GetCookieParamIndex(GetActualName(ADestination));
  if LIndex = -1 then
    CheckRequiredAttribute(ADestination);
  Result := StringToTValue(
      AActivation.Request.GetCookieParamValue(LIndex)
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

function PathParamAttribute.GetSwaggerKind: string;
begin
  Result := 'path';
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
  end
  else
    CheckRequiredAttribute(ADestination);
end;

{ AuthorizationAttribute }

function AuthorizationAttribute.ToString: string;
begin
  Result := ClassName;
  if Result.EndsWith('Attribute', True) then
    Result := Result.Substring(0, Result.Length - 'Attribute'.Length);
end;

//{ ANYAttribute }
//
//function ANYAttribute.Matches(const ARequest: IMARSRequest): Boolean;
//begin
//  Result := SameText(ARequest.Method, 'ANY');
//end;

{ OPTIONSAttribute }

function OPTIONSAttribute.Matches(const ARequest: IMARSRequest): Boolean;
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

{ FormParamsAttribute }

function FormParamsAttribute.GetSwaggerKind: string;
begin
  Result := 'formData(s)';
end;

function FormParamsAttribute.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation): TValue;
var
  LMediaType: TMediaType;
  LReader: IMessageBodyReader;
begin
  if AActivation.Request.GetFormParamCount = 0 then
    CheckRequiredAttribute(ADestination)
  else
  begin
    // 1 - MessageBodyReader mechanism (standard)
    TMARSMessageBodyReaderRegistry.Instance.FindReader(ADestination, LReader, LMediaType);
    if Assigned(LReader) then
      try

        Result := LReader.ReadFrom({$ifdef Delphi10Berlin_UP}nil{$else}''{$endif}
          , ADestination, LMediaType, AActivation);
      finally
        FreeAndNil(LMediaType);
      end
    else // 2 - fallback (raw)
      Result := StringToTValue(AActivation.Request.GetFormParams, ADestination.GetRttiType);
  end;
end;

{ ConfigSingleParamAttribute }

constructor ConfigSingleParamAttribute.Create(const AName: string;
  const ADefaultValue: TValue);
begin
  inherited Create;
  FName := AName;
  FDefaultValue := ADefaultValue;
end;

constructor ConfigSingleParamAttribute.Create(const AName: string);
begin
  Create(AName, TValue.Empty);
end;


{ ConfigParamAttribute }

function ConfigParamAttribute.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation): TValue;
begin
  Result := TValue.Empty;
end;

{ EngineParamAttribute }

function EngineParamAttribute.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation): TValue;
begin
  if Assigned(AActivation.Engine) then
    Result := AActivation.Engine.Parameters.ByNameText(Name, DefaultValue)
  else
    Result := inherited GetValue(ADestination, AActivation);
end;


{ ApplicationParamAttribute }

function ApplicationParamAttribute.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation): TValue;
begin
  if Assigned(AActivation.Application) then
    Result := AActivation.Application.Parameters.ByNameText(Name, DefaultValue)
  else
    Result := inherited GetValue(ADestination, AActivation);
end;

{ EngineParamFuncAttribute }

function EngineParamFuncAttribute.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation): TValue;
begin
  if ADestination.GetRttiType.Handle = TypeInfo(TConfigParamFunc) then
    Result := TValue.From<TConfigParamFunc>(
      function (const AName: string): TValue
      begin
        Result := AActivation.Engine.Parameters.ByNameText(AName);
      end
    )
  else
    Result := inherited GetValue(ADestination, AActivation);
end;

{ ApplicationParamFuncAttribute }

function ApplicationParamFuncAttribute.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation): TValue;
begin
  if ADestination.GetRttiType.Handle = TypeInfo(TConfigParamFunc) then
    Result := TValue.From<TConfigParamFunc>(
      function (const AName: string): TValue
      begin
        Result := AActivation.Application.Parameters.ByNameText(AName);
      end
    )
  else
    Result := inherited GetValue(ADestination, AActivation);
end;

{ EncodingAttribute }

constructor EncodingAttribute.Create(const AEncodingName: string);
begin
  inherited Create;
  FEncodingName := AEncodingName;
end;

function EncodingAttribute.GetEncoding: TEncoding;
begin
  Result := TEncoding.UTF8;
  if SameText(Name, 'ANSI') then Result := TEncoding.ANSI
  else if SameText(Name, 'ASCII') then Result := TEncoding.ASCII
  else if SameText(Name, 'BigEndianUnicode') then Result := TEncoding.BigEndianUnicode
  else if SameText(Name, 'Default') then Result := TEncoding.Default
  else if SameText(Name, 'Unicode') then Result := TEncoding.Unicode
  else if SameText(Name, 'UTF7') then Result := TEncoding.UTF7
  else if SameText(Name, 'UTF8') then Result := TEncoding.UTF8;
end;

end.
