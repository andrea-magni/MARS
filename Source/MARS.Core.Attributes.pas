(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
  Home: https://github.com/andrea-magni/MARS

*)
unit MARS.Core.Attributes;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, RTTI, Generics.Collections
  , HttpApp
  , MARS.Core.Declarations
  , MARS.Core.Utils;

type
{$REGION 'JAX-Like Attributes'}

  PathAttribute = class(TCustomAttribute)
  private
    FValue: string;
  public
    constructor Create(const AValue: string);
    property Value: string read FValue write FValue;
  end;

  HttpMethodAttribute = class(TCustomAttribute)
  private
  protected
  public
    function Matches(const ARequest: TWebRequest): Boolean; virtual;
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

  /// <summary>
  ///   A list of media types. Each entry may specify a single type or consist of a comma separated list of types. E.g.
  ///   {"text/html, application/pdf"}. <br />Use of the comma-separated form allows definition of a common string
  ///   constant for use on multiple targets
  /// </summary>
  ConsumesAttribute = class(TCustomAttribute)
  private
    FValue: string;
  public
    constructor Create(const AValue: string);
    /// <summary>
    ///   A list of media types
    /// </summary>
    property Value: string read FValue write FValue;
  end;

  /// <summary>
  ///   A list of media types. Each entry may specify a single type or consist of a comma separated list of types. E.g.
  ///   {"text/html, application/pdf"}. <br />Use of the comma-separated form allows definition of a common string
  ///   constant for use on multiple targets
  /// </summary>
  ProducesAttribute = class(TCustomAttribute)
  private
    FValue: string;
  public
    constructor Create(const AValue: string);
    /// <summary>
    ///   A list of media types
    /// </summary>
    property Value: string read FValue write FValue;
  end;

  MethodParamAttribute = class(TCustomAttribute)
  private
    FValue: string;
  public
    constructor Create(const AValue: string = '');

    property Value: string read FValue write FValue;
  end;

  PathParamAttribute = class(MethodParamAttribute)
  private
    FParamIndex: Integer;
  public
    property ParamIndex: Integer read FParamIndex write FParamIndex;
  end;
  QueryParamAttribute = class(MethodParamAttribute);
  FormParamAttribute = class(MethodParamAttribute);
  HeaderParamAttribute = class(MethodParamAttribute);
  CookieParamAttribute = class(MethodParamAttribute);
  BodyParamAttribute = class(MethodParamAttribute)
  public
    constructor Create;
  end;

  ContextAttribute = class(TCustomAttribute);

  AuthorizationAttribute = class(TCustomAttribute);

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

{$ENDREGION}

{$REGION 'MARS-specific Attributes'}
  ResultIsReference = class(TCustomAttribute);
{$ENDREGION}

  LoginRequiredAttribute = class(TCustomAttribute);

  URLParamAttribute = class(TCustomAttribute)
  private
    FPosition: Integer;
    FDefaultValue: TValue;
  public
    constructor Create(APosition: Integer; const ADefaultValue: TValue); overload;

    constructor Create(APosition: Integer); overload;
    constructor Create(APosition: Integer; const ADefaultValue: string); overload;
    constructor Create(APosition: Integer; ADefaultValue: Integer); overload;
    constructor Create(APosition: Integer; ADefaultValue: Double); overload;
    constructor Create(APosition: Integer; ADefaultValue: Boolean); overload;

    property Position: Integer read FPosition write FPosition;
    property DefaultValue: TValue read FDefaultValue write FDefaultValue;
  end;

  ContentTypeAttribute = class(TCustomAttribute)
  private
    FContentType: string;
  public
    constructor Create(const AContentType: string);
    property ContentType: string read FContentType;
  end;

  CustomHeaderAttribute = class(TCustomAttribute)
  private
    FHeaderName: string;
    FValue: string;
  public
    constructor Create(const AHeaderName, AValue: string);
    property HeaderName: string read FHeaderName;
    property Value: string read FValue;
  end;

  RestAttribute = class(TCustomAttribute)
  end;

implementation


{ URLParamAttribute }

constructor URLParamAttribute.Create(APosition: Integer; const ADefaultValue: string);
begin
  Create(APosition, TValue.From<string>(ADefaultValue));
end;

constructor URLParamAttribute.Create(APosition: Integer; ADefaultValue: Integer);
begin
  Create(APosition, TValue.From<Integer>(ADefaultValue));
end;

constructor URLParamAttribute.Create(APosition: Integer; const ADefaultValue: TValue);
begin
  inherited Create;
  FPosition := APosition;
  FDefaultValue := ADefaultValue;
end;

constructor URLParamAttribute.Create(APosition: Integer; ADefaultValue: Double);
begin
  Create(APosition, TValue.From<Double>(ADefaultValue));
end;

constructor URLParamAttribute.Create(APosition: Integer;
  ADefaultValue: Boolean);
begin
  Create(APosition, TValue.From<Boolean>(ADefaultValue));
end;

constructor URLParamAttribute.Create(APosition: Integer);
begin
  Create(APosition, TValue.Empty);
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

{ MethodParamAttribute }

constructor MethodParamAttribute.Create(const AValue: string);
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

constructor BodyParamAttribute.Create;
begin
  inherited Create('body');
end;

{ HttpMethodAttribute }

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

end.
