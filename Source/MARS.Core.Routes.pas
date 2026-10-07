(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Routes;

{$I MARS.inc}

interface

uses
  SysUtils, Classes, Rtti, TypInfo, Generics.Collections, SyncObjs
, MARS.Core.Activation.Interfaces, MARS.Core.Application.Interfaces
, MARS.Core.Engine.Interfaces, MARS.Core.RequestAndResponse.Interfaces
, MARS.Core.URL, MARS.Core.Token, MARS.Core.Attributes, MARS.Core.Exceptions
, MARS.Core.Injection, MARS.Core.Injection.Types, MARS.Core.Registry.Utils, MARS.Rtti.Utils
;

type
  TMARSRouter = class;
  TMARSRoute = class;

  // Handler context: request parameters, injection and response of the current request.
  TMARSRouteContext = record
  private
    FActivation: IMARSActivation;
    function GetApplication: IMARSApplication;
    function GetEngine: IMARSEngine;
    function GetRequest: IMARSRequest;
    function GetResponse: IMARSResponse;
    function GetToken: TMARSToken;
    function GetURL: TMARSURL;
    function ReadParam<T>(const AAttribute: RequestParamAttribute): T;
  public
    constructor Create(const AActivation: IMARSActivation);

    // request parameters ([PathParam], [QueryParam], ... of the resources)
    function Path<T>(const AName: string): T;
    function Query<T>(const AName: string): T; overload;
    function Query<T>(const AName: string; const ADefault: T): T; overload;
    function Header<T>(const AName: string): T; overload;
    function Header<T>(const AName: string; const ADefault: T): T; overload;
    function Cookie<T>(const AName: string): T; overload;
    function Cookie<T>(const AName: string; const ADefault: T): T; overload;
    function Form<T>(const AName: string): T;
    function Body<T>: T;
    // application parameter (configuration)
    function Config<T>(const AName: string; const ADefault: T): T;

    // any value an injection service provides ([Context] of the resources)
    function Inject<T>: T;
    // AObject is freed at the end of the request
    function Own<T: class>(const AObject: T): T;

    // response helpers
    procedure Status(const AStatusCode: Integer);
    procedure Created(const ALocation: string = '');
    procedure NoContent;

    property Activation: IMARSActivation read FActivation;
    property Application: IMARSApplication read GetApplication;
    property Engine: IMARSEngine read GetEngine;
    property Request: IMARSRequest read GetRequest;
    property Response: IMARSResponse read GetResponse;
    property Token: TMARSToken read GetToken;
    property URL: TMARSURL read GetURL;
  end;

  TMARSRouteFunc<TResult> = reference to function (const C: TMARSRouteContext): TResult;
  TMARSRouteBodyFunc<TBody, TResult> = reference to function (const C: TMARSRouteContext; const ABody: TBody): TResult;
  TMARSRouteProc = reference to procedure (const C: TMARSRouteContext);
  TMARSRouteDefineProc = reference to procedure (const R: TMARSRouter);

  TMARSRouteInvoker = reference to function (const AActivation: IMARSActivation): TValue;

  // holder whose field is the injection destination of TMARSRouteContext (type T, [Context])
  TMARSRouteValue<T> = class
  public
    [Context] Value: T;
  end;

  TMARSRouteSegmentKind = (rskLiteral, rskParam, rskWildcard);

  TMARSRouteSegment = record
    Kind: TMARSRouteSegmentKind;
    Text: string;        // literal text or parameter name
    Constraint: string;  // '', 'int', 'guid', 'alpha'
    function Matches(const AToken: string): Boolean;
    function Rank: Integer;
    function ShapeKey(const AWithConstraint: Boolean): string;
  end;

  TMARSRouteItem = class
  private
    FAttributes: TObjectList<TCustomAttribute>;
  protected
    procedure AddAttribute(const AAttribute: TCustomAttribute);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function GetAttributes: TArray<TCustomAttribute>;
  end;

  TMARSRoute = class(TMARSRouteItem)
  private
    FRouter: TMARSRouter;
    FHttpMethod: string;
    FTemplate: string;
    FPrototypePath: string;
    FSegments: TArray<TMARSRouteSegment>;
    FResultType: PTypeInfo;
    FBodyType: PTypeInfo;
    FInvoker: TMARSRouteInvoker;
    FName: string;
    function GetGroupAttributes: TArray<TCustomAttribute>;
  public
    constructor Create(const ARouter: TMARSRouter; const AHttpMethod, ATemplate: string;
      const AResultType, ABodyType: PTypeInfo; const AInvoker: TMARSRouteInvoker); reintroduce;

    function MatchesPath(const ATokens: TArray<string>): Boolean;
    function CompareSpecificity(const AOther: TMARSRoute): Integer;
    function ShapeKey(const AWithConstraints: Boolean): string;
    function Invoke(const AActivation: IMARSActivation): TValue;

    // declarations (same meaning as the attributes of a resource method)
    function RolesAllowed(const ARoles: string): TMARSRoute;
    function PermitAll: TMARSRoute;
    function DenyAll: TMARSRoute;
    function Produces(const AMediaType: string): TMARSRoute;
    function Consumes(const AMediaType: string): TMARSRoute;
    function CustomHeader(const AName, AValue: string): TMARSRoute;
    function NoLog: TMARSRoute;
    function ResultIsReference: TMARSRoute;
    function Attribute(const AAttribute: TCustomAttribute): TMARSRoute;
    function Name(const AName: string): TMARSRoute;

    property HttpMethod: string read FHttpMethod;
    property Template: string read FTemplate;
    property PrototypePath: string read FPrototypePath;
    property Segments: TArray<TMARSRouteSegment> read FSegments;
    property ResultType: PTypeInfo read FResultType;
    property BodyType: PTypeInfo read FBodyType;
    property Router: TMARSRouter read FRouter;
    property RouteName: string read FName;
    property Attributes: TArray<TCustomAttribute> read GetAttributes;
    // attributes of the enclosing groups, innermost first
    property GroupAttributes: TArray<TCustomAttribute> read GetGroupAttributes;
  end;

  TMARSRouteTable = class;

  TMARSRouter = class(TMARSRouteItem)
  private
    FParent: TMARSRouter;
    FTable: TMARSRouteTable;
    FPath: string;
    FFullPath: string;
    FGroups: TObjectList<TMARSRouter>;
    FRoutes: TObjectList<TMARSRoute>;
  protected
    function AddRoute(const AHttpMethod, APath: string; const AResultType, ABodyType: PTypeInfo;
      const AInvoker: TMARSRouteInvoker): TMARSRoute;
  public
    constructor Create(const ATable: TMARSRouteTable; const AParent: TMARSRouter; const APath: string); reintroduce;
    destructor Destroy; override;

    function Group(const APath: string; const ADefine: TMARSRouteDefineProc): TMARSRouter;

    function Map<TResult>(const AHttpMethod, APath: string; const AHandler: TMARSRouteFunc<TResult>): TMARSRoute; overload;
    function Map<TBody, TResult>(const AHttpMethod, APath: string; const AHandler: TMARSRouteBodyFunc<TBody, TResult>): TMARSRoute; overload;
    function Map(const AHttpMethod, APath: string; const AHandler: TMARSRouteProc): TMARSRoute; overload;

    function Get<TResult>(const APath: string; const AHandler: TMARSRouteFunc<TResult>): TMARSRoute; overload;
    function Get(const APath: string; const AHandler: TMARSRouteProc): TMARSRoute; overload;
    function Post<TResult>(const APath: string; const AHandler: TMARSRouteFunc<TResult>): TMARSRoute; overload;
    function Post<TBody, TResult>(const APath: string; const AHandler: TMARSRouteBodyFunc<TBody, TResult>): TMARSRoute; overload;
    function Post(const APath: string; const AHandler: TMARSRouteProc): TMARSRoute; overload;
    function Put<TResult>(const APath: string; const AHandler: TMARSRouteFunc<TResult>): TMARSRoute; overload;
    function Put<TBody, TResult>(const APath: string; const AHandler: TMARSRouteBodyFunc<TBody, TResult>): TMARSRoute; overload;
    function Put(const APath: string; const AHandler: TMARSRouteProc): TMARSRoute; overload;
    function Patch<TResult>(const APath: string; const AHandler: TMARSRouteFunc<TResult>): TMARSRoute; overload;
    function Patch<TBody, TResult>(const APath: string; const AHandler: TMARSRouteBodyFunc<TBody, TResult>): TMARSRoute; overload;
    function Patch(const APath: string; const AHandler: TMARSRouteProc): TMARSRoute; overload;
    function Delete<TResult>(const APath: string; const AHandler: TMARSRouteFunc<TResult>): TMARSRoute; overload;
    function Delete(const APath: string; const AHandler: TMARSRouteProc): TMARSRoute; overload;

    // declarations for every route of the group (same meaning as the attributes of a resource class)
    function RolesAllowed(const ARoles: string): TMARSRouter;
    function PermitAll: TMARSRouter;
    function DenyAll: TMARSRouter;
    function Produces(const AMediaType: string): TMARSRouter;
    function Consumes(const AMediaType: string): TMARSRouter;
    function CustomHeader(const AName, AValue: string): TMARSRouter;
    function NoLog: TMARSRouter;
    function Attribute(const AAttribute: TCustomAttribute): TMARSRouter;

    property Parent: TMARSRouter read FParent;
    property Path: string read FPath;
    property FullPath: string read FFullPath;
    property Table: TMARSRouteTable read FTable;
  end;

  // Routes of an application (IMARSApplication.RouteTable owns it)
  TMARSRouteTable = class
  private
    [Weak] FApplication: IMARSApplication;
    FRoot: TMARSRouter;
    FRoutes: TList<TMARSRoute>;
  protected
    procedure Add(const ARoute: TMARSRoute);
  public
    constructor Create(const AApplication: IMARSApplication);
    destructor Destroy; override;

    function Match(const ATokens: TArray<string>; const AHttpMethod: string;
      out ARoute: TMARSRoute; out AAllowedMethods: string): Boolean;

    property Root: TMARSRouter read FRoot;
    property Routes: TList<TMARSRoute> read FRoutes;
    property Application: IMARSApplication read FApplication;

    class function ForApplication(const AApplication: IMARSApplication): TMARSRouteTable;
  end;

  // Holds the TRttiContext the holder fields belong to
  TMARSRouteRtti = class
  private
    class var FContext: TRttiContext;
    class var FFields: TDictionary<PTypeInfo, TRttiField>;
    class var FLock: TCriticalSection;
  public
    class constructor ClassCreate;
    class destructor ClassDestroy;
    class function ValueField(const AHolderType: PTypeInfo): TRttiField;
    class function RttiType(const ATypeInfo: PTypeInfo): TRttiType;
  end;

  TMARSRouteModule = record
    Name: string;
    Path: string;
    Define: TMARSRouteDefineProc;
  end;

  TMARSRouteModules = class
  private
    class var FModules: TDictionary<string, TMARSRouteModule>;
  public
    class constructor ClassCreate;
    class destructor ClassDestroy;
    class procedure Register(const AName, APath: string; const ADefine: TMARSRouteDefineProc);
    class function AddTo(const AApplication: IMARSApplication; const AModules: string): Boolean;
  end;

  ERouteDefinitionException = class(EMARSException);

  // Registers a module of routes (typically in the initialization section of a unit);
  // IMARSApplication.AddRoutes adds it to an application (wildcards allowed, as AddResource).
  procedure MARSRoutes(const AName, APath: string; const ADefine: TMARSRouteDefineProc);

  // Root router of AApplication: routes defined in code, i.e. in the Server.Ignition
  function MARSRoutesOf(const AApplication: IMARSApplication): TMARSRouter;

implementation

uses
  StrUtils, Character
, MARS.Core.MediaType, MARS.Core.Utils
;

procedure MARSRoutes(const AName, APath: string; const ADefine: TMARSRouteDefineProc);
begin
  TMARSRouteModules.Register(AName, APath, ADefine);
end;

function MARSRoutesOf(const AApplication: IMARSApplication): TMARSRouter;
begin
  Result := TMARSRouteTable.ForApplication(AApplication).Root;
end;

function SplitPath(const APath: string): TArray<string>;
begin
  Result := APath.Split([TMARSURL.URL_PATH_SEPARATOR], TStringSplitOptions.ExcludeEmpty);
end;

function CombineRoutePath(const ALeft, ARight: string): string;
begin
  Result := string.Join(TMARSURL.URL_PATH_SEPARATOR, SplitPath(ALeft) + SplitPath(ARight));
end;

{ TMARSRouteContext }

constructor TMARSRouteContext.Create(const AActivation: IMARSActivation);
begin
  FActivation := AActivation;
end;

function TMARSRouteContext.ReadParam<T>(const AAttribute: RequestParamAttribute): T;
var
  LValue: TValue;
begin
  try
    try
      LValue := AAttribute.GetValue(TMARSRouteRtti.ValueField(TypeInfo(TMARSRouteValue<T>)), FActivation);
    except
      on E: EMARSHttpException do
        raise;
      on E: Exception do
        raise EMARSApplicationException.CreateFmt('Bad parameter value (%s %s): %s'
          , [AAttribute.Kind, NamedRequestParamAttribute(AAttribute).Name, E.Message], 400);
    end;
  finally
    AAttribute.Free;
  end;
  FActivation.AddToContext(LValue); // objects read from the request are freed with the activation

  if LValue.IsEmpty then
    Result := Default(T)
  else
    Result := LValue.AsType<T>;
end;

function TMARSRouteContext.Path<T>(const AName: string): T;
begin
  Result := ReadParam<T>(PathParamAttribute.Create(AName));
end;

function TMARSRouteContext.Query<T>(const AName: string): T;
begin
  Result := ReadParam<T>(QueryParamAttribute.Create(AName));
end;

function TMARSRouteContext.Query<T>(const AName: string; const ADefault: T): T;
begin
  if Request.GetQueryParamIndex(AName) = -1 then
    Result := ADefault
  else
    Result := Query<T>(AName);
end;

function TMARSRouteContext.Header<T>(const AName: string): T;
begin
  Result := ReadParam<T>(HeaderParamAttribute.Create(AName));
end;

function TMARSRouteContext.Header<T>(const AName: string; const ADefault: T): T;
begin
  if Request.GetHeaderParamValue(AName) = '' then
    Result := ADefault
  else
    Result := Header<T>(AName);
end;

function TMARSRouteContext.Cookie<T>(const AName: string): T;
begin
  Result := ReadParam<T>(CookieParamAttribute.Create(AName));
end;

function TMARSRouteContext.Cookie<T>(const AName: string; const ADefault: T): T;
begin
  if Request.GetCookieParamIndex(AName) = -1 then
    Result := ADefault
  else
    Result := Cookie<T>(AName);
end;

function TMARSRouteContext.Form<T>(const AName: string): T;
begin
  Result := ReadParam<T>(FormParamAttribute.Create(AName));
end;

function TMARSRouteContext.Body<T>: T;
var
  LValue: TValue;
  LAttribute: BodyParamAttribute;
begin
  LAttribute := BodyParamAttribute.Create;
  try
    try
      LValue := LAttribute.GetValue(TMARSRouteRtti.ValueField(TypeInfo(TMARSRouteValue<T>)), FActivation);
    except
      on E: EMARSHttpException do
        raise;
      on E: Exception do
        raise EMARSApplicationException.CreateFmt('Bad request body: %s', [E.Message], 400);
    end;
  finally
    LAttribute.Free;
  end;
  FActivation.AddToContext(LValue);

  if LValue.IsEmpty then
    Result := Default(T)
  else
    Result := LValue.AsType<T>;
end;

function TMARSRouteContext.Config<T>(const AName: string; const ADefault: T): T;
var
  LValue: TValue;
begin
  LValue := Application.Parameters.ByName(AName, TValue.From<T>(ADefault));
  if LValue.IsType<T> then
    Result := LValue.AsType<T>
  else
    Result := StringToTValue(LValue.ToString, TMARSRouteRtti.RttiType(TypeInfo(T))).AsType<T>;
end;

function TMARSRouteContext.Inject<T>: T;
var
  LValue: TInjectionValue;
begin
  LValue := TMARSInjectionServiceRegistry.Instance.GetValue(
    TMARSRouteRtti.ValueField(TypeInfo(TMARSRouteValue<T>)), FActivation);
  if not LValue.IsReference then
    FActivation.AddToContext(LValue.Value);

  if LValue.Value.IsEmpty then
    Result := Default(T)
  else
    Result := LValue.Value.AsType<T>;
end;

function TMARSRouteContext.Own<T>(const AObject: T): T;
begin
  FActivation.AddToContext(TValue.From<T>(AObject));
  Result := AObject;
end;

procedure TMARSRouteContext.Status(const AStatusCode: Integer);
begin
  Response.StatusCode := AStatusCode;
end;

procedure TMARSRouteContext.Created(const ALocation: string);
begin
  Response.StatusCode := 201;
  if ALocation <> '' then
    Response.SetHeader('Location', ALocation);
end;

procedure TMARSRouteContext.NoContent;
begin
  Response.StatusCode := 204;
end;

function TMARSRouteContext.GetApplication: IMARSApplication;
begin
  Result := FActivation.Application;
end;

function TMARSRouteContext.GetEngine: IMARSEngine;
begin
  Result := FActivation.Engine;
end;

function TMARSRouteContext.GetRequest: IMARSRequest;
begin
  Result := FActivation.Request;
end;

function TMARSRouteContext.GetResponse: IMARSResponse;
begin
  Result := FActivation.Response;
end;

function TMARSRouteContext.GetToken: TMARSToken;
begin
  Result := FActivation.Token;
end;

function TMARSRouteContext.GetURL: TMARSURL;
begin
  Result := FActivation.URL;
end;

{ TMARSRouteSegment }

function TMARSRouteSegment.Matches(const AToken: string): Boolean;
var
  LInt: Int64;
  LGUID: string;
  LChar: Char;
begin
  case Kind of
    rskLiteral: Result := SameText(Text, AToken);
    rskWildcard: Result := True;
    else // rskParam
    begin
      if Constraint = '' then
        Result := AToken <> ''
      else if Constraint = 'int' then
        Result := TryStrToInt64(AToken, LInt)
      else if Constraint = 'guid' then
      begin
        LGUID := AToken;
        if not LGUID.StartsWith('{') then
          LGUID := '{' + LGUID + '}';
        try
          StringToGUID(LGUID);
          Result := True;
        except
          Result := False;
        end;
      end
      else if Constraint = 'alpha' then
      begin
        Result := AToken <> '';
        for LChar in AToken do
          if not LChar.IsLetter then
            Exit(False);
      end
      else
        Result := False;
    end;
  end;
end;

function TMARSRouteSegment.Rank: Integer;
begin
  case Kind of
    rskLiteral: Result := 3;
    rskParam: if Constraint <> '' then Result := 2 else Result := 1;
    else Result := 0;
  end;
end;

function TMARSRouteSegment.ShapeKey(const AWithConstraint: Boolean): string;
begin
  case Kind of
    rskLiteral: Result := Text.ToLower;
    rskParam:
      if AWithConstraint then
        Result := '{:' + Constraint + '}'
      else
        Result := '{}';
    else Result := TMARSURL.PATH_PARAM_WILDCARD;
  end;
end;

{ TMARSRouteItem }

procedure TMARSRouteItem.AddAttribute(const AAttribute: TCustomAttribute);
begin
  if Assigned(AAttribute) then
    FAttributes.Add(AAttribute);
end;

constructor TMARSRouteItem.Create;
begin
  inherited Create;
  FAttributes := TObjectList<TCustomAttribute>.Create(True);
end;

destructor TMARSRouteItem.Destroy;
begin
  FAttributes.Free;
  inherited;
end;

function TMARSRouteItem.GetAttributes: TArray<TCustomAttribute>;
begin
  Result := FAttributes.ToArray;
end;

{ TMARSRoute }

constructor TMARSRoute.Create(const ARouter: TMARSRouter; const AHttpMethod, ATemplate: string;
  const AResultType, ABodyType: PTypeInfo; const AInvoker: TMARSRouteInvoker);
var
  LTokens: TArray<string>;
  LPrototype: TArray<string>;
  LIndex, LColon: Integer;
  LToken, LInner: string;
  LSegment: TMARSRouteSegment;
begin
  inherited Create;
  FRouter := ARouter;
  FHttpMethod := AHttpMethod.ToUpper;
  FTemplate := ATemplate;
  FResultType := AResultType;
  FBodyType := ABodyType;
  FInvoker := AInvoker;
  FName := FHttpMethod + ' ' + FTemplate;

  LTokens := SplitPath(ATemplate);
  SetLength(FSegments, Length(LTokens));
  SetLength(LPrototype, Length(LTokens));
  for LIndex := 0 to High(LTokens) do
  begin
    LToken := LTokens[LIndex];
    LSegment := Default(TMARSRouteSegment);
    if LToken.StartsWith('{') and LToken.EndsWith('}') then
    begin
      LInner := LToken.Substring(1, LToken.Length - 2).Trim;
      if LInner = '*' then
      begin
        if LIndex < High(LTokens) then
          raise ERouteDefinitionException.CreateFmt('Route %s: {*} must be the last segment', [FName]);
        LSegment.Kind := rskWildcard;
        LSegment.Text := '*';
        LPrototype[LIndex] := TMARSURL.PATH_PARAM_WILDCARD;
      end
      else
      begin
        LSegment.Kind := rskParam;
        LColon := LInner.IndexOf(':');
        if LColon > -1 then
        begin
          LSegment.Text := LInner.Substring(0, LColon).Trim;
          LSegment.Constraint := LInner.Substring(LColon + 1).Trim.ToLower;
          if not MatchStr(LSegment.Constraint, ['int', 'guid', 'alpha']) then
            raise ERouteDefinitionException.CreateFmt('Route %s: unknown constraint "%s" (int, guid, alpha)'
              , [FName, LSegment.Constraint]);
        end
        else
          LSegment.Text := LInner;
        if LSegment.Text = '' then
          raise ERouteDefinitionException.CreateFmt('Route %s: parameter without a name', [FName]);
        LPrototype[LIndex] := '{' + LSegment.Text + '}';
      end;
    end
    else
    begin
      LSegment.Kind := rskLiteral;
      LSegment.Text := LToken;
      LPrototype[LIndex] := LToken;
    end;
    FSegments[LIndex] := LSegment;
  end;
  FPrototypePath := string.Join(TMARSURL.URL_PATH_SEPARATOR, LPrototype);
end;

function TMARSRoute.MatchesPath(const ATokens: TArray<string>): Boolean;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FSegments) do
  begin
    if FSegments[LIndex].Kind = rskWildcard then
      Exit(True);
    if LIndex > High(ATokens) then
      Exit(False);
    if not FSegments[LIndex].Matches(ATokens[LIndex]) then
      Exit(False);
  end;
  Result := Length(ATokens) = Length(FSegments);
end;

function TMARSRoute.CompareSpecificity(const AOther: TMARSRoute): Integer;
var
  LIndex: Integer;
begin
  Result := 0;
  LIndex := 0;
  while (Result = 0) and (LIndex <= High(FSegments)) and (LIndex <= High(AOther.FSegments)) do
  begin
    Result := FSegments[LIndex].Rank - AOther.FSegments[LIndex].Rank;
    Inc(LIndex);
  end;
  if Result = 0 then
    Result := Length(FSegments) - Length(AOther.FSegments);
end;

function TMARSRoute.ShapeKey(const AWithConstraints: Boolean): string;
var
  LSegment: TMARSRouteSegment;
begin
  Result := FHttpMethod + ' ';
  for LSegment in FSegments do
    Result := Result + TMARSURL.URL_PATH_SEPARATOR + LSegment.ShapeKey(AWithConstraints);
end;

function TMARSRoute.GetGroupAttributes: TArray<TCustomAttribute>;
var
  LRouter: TMARSRouter;
begin
  Result := [];
  LRouter := FRouter;
  while Assigned(LRouter) do
  begin
    Result := Result + LRouter.GetAttributes;
    LRouter := LRouter.Parent;
  end;
end;

function TMARSRoute.Invoke(const AActivation: IMARSActivation): TValue;
begin
  Result := FInvoker(AActivation);
end;

function TMARSRoute.Attribute(const AAttribute: TCustomAttribute): TMARSRoute;
begin
  AddAttribute(AAttribute);
  Result := Self;
end;

function TMARSRoute.Consumes(const AMediaType: string): TMARSRoute;
begin
  Result := Attribute(ConsumesAttribute.Create(AMediaType));
end;

function TMARSRoute.CustomHeader(const AName, AValue: string): TMARSRoute;
begin
  Result := Attribute(CustomHeaderAttribute.Create(AName, AValue));
end;

function TMARSRoute.DenyAll: TMARSRoute;
begin
  Result := Attribute(DenyAllAttribute.Create);
end;

function TMARSRoute.Name(const AName: string): TMARSRoute;
begin
  FName := AName;
  Result := Self;
end;

function TMARSRoute.NoLog: TMARSRoute;
begin
  Result := Attribute(NoLogAttribute.Create);
end;

function TMARSRoute.PermitAll: TMARSRoute;
begin
  Result := Attribute(PermitAllAttribute.Create);
end;

function TMARSRoute.Produces(const AMediaType: string): TMARSRoute;
begin
  Result := Attribute(ProducesAttribute.Create(AMediaType));
end;

function TMARSRoute.ResultIsReference: TMARSRoute;
begin
  Result := Attribute(IsReference.Create);
end;

function TMARSRoute.RolesAllowed(const ARoles: string): TMARSRoute;
begin
  Result := Attribute(RolesAllowedAttribute.Create(ARoles));
end;

{ TMARSRouter }

constructor TMARSRouter.Create(const ATable: TMARSRouteTable; const AParent: TMARSRouter; const APath: string);
begin
  inherited Create;
  FTable := ATable;
  FParent := AParent;
  FPath := APath;
  if Assigned(AParent) then
    FFullPath := CombineRoutePath(AParent.FullPath, APath)
  else
    FFullPath := CombineRoutePath('', APath);
  FGroups := TObjectList<TMARSRouter>.Create(True);
  FRoutes := TObjectList<TMARSRoute>.Create(True);
end;

destructor TMARSRouter.Destroy;
begin
  FRoutes.Free;
  FGroups.Free;
  inherited;
end;

function TMARSRouter.Group(const APath: string; const ADefine: TMARSRouteDefineProc): TMARSRouter;
begin
  Result := TMARSRouter.Create(FTable, Self, APath);
  FGroups.Add(Result);
  if Assigned(ADefine) then
    ADefine(Result);
end;

function TMARSRouter.AddRoute(const AHttpMethod, APath: string; const AResultType, ABodyType: PTypeInfo;
  const AInvoker: TMARSRouteInvoker): TMARSRoute;
begin
  Result := TMARSRoute.Create(Self, AHttpMethod, CombineRoutePath(FFullPath, APath)
    , AResultType, ABodyType, AInvoker);
  try
    FTable.Add(Result);
  except
    Result.Free;
    raise;
  end;
  FRoutes.Add(Result);
end;

function TMARSRouter.Map<TResult>(const AHttpMethod, APath: string;
  const AHandler: TMARSRouteFunc<TResult>): TMARSRoute;
begin
  Result := AddRoute(AHttpMethod, APath, TypeInfo(TResult), nil,
    function (const AActivation: IMARSActivation): TValue
    begin
      Result := TValue.From<TResult>(AHandler(TMARSRouteContext.Create(AActivation)));
    end
  );
end;

function TMARSRouter.Map<TBody, TResult>(const AHttpMethod, APath: string;
  const AHandler: TMARSRouteBodyFunc<TBody, TResult>): TMARSRoute;
begin
  Result := AddRoute(AHttpMethod, APath, TypeInfo(TResult), TypeInfo(TBody),
    function (const AActivation: IMARSActivation): TValue
    var
      LContext: TMARSRouteContext;
    begin
      LContext := TMARSRouteContext.Create(AActivation);
      Result := TValue.From<TResult>(AHandler(LContext, LContext.Body<TBody>));
    end
  );
end;

function TMARSRouter.Map(const AHttpMethod, APath: string; const AHandler: TMARSRouteProc): TMARSRoute;
begin
  Result := AddRoute(AHttpMethod, APath, nil, nil,
    function (const AActivation: IMARSActivation): TValue
    begin
      AHandler(TMARSRouteContext.Create(AActivation));
      Result := TValue.Empty;
    end
  );
end;

function TMARSRouter.Get<TResult>(const APath: string; const AHandler: TMARSRouteFunc<TResult>): TMARSRoute;
begin
  Result := Map<TResult>('GET', APath, AHandler);
end;

function TMARSRouter.Get(const APath: string; const AHandler: TMARSRouteProc): TMARSRoute;
begin
  Result := Map('GET', APath, AHandler);
end;

function TMARSRouter.Post<TResult>(const APath: string; const AHandler: TMARSRouteFunc<TResult>): TMARSRoute;
begin
  Result := Map<TResult>('POST', APath, AHandler);
end;

function TMARSRouter.Post<TBody, TResult>(const APath: string;
  const AHandler: TMARSRouteBodyFunc<TBody, TResult>): TMARSRoute;
begin
  Result := Map<TBody, TResult>('POST', APath, AHandler);
end;

function TMARSRouter.Post(const APath: string; const AHandler: TMARSRouteProc): TMARSRoute;
begin
  Result := Map('POST', APath, AHandler);
end;

function TMARSRouter.Put<TResult>(const APath: string; const AHandler: TMARSRouteFunc<TResult>): TMARSRoute;
begin
  Result := Map<TResult>('PUT', APath, AHandler);
end;

function TMARSRouter.Put<TBody, TResult>(const APath: string;
  const AHandler: TMARSRouteBodyFunc<TBody, TResult>): TMARSRoute;
begin
  Result := Map<TBody, TResult>('PUT', APath, AHandler);
end;

function TMARSRouter.Put(const APath: string; const AHandler: TMARSRouteProc): TMARSRoute;
begin
  Result := Map('PUT', APath, AHandler);
end;

function TMARSRouter.Patch<TResult>(const APath: string; const AHandler: TMARSRouteFunc<TResult>): TMARSRoute;
begin
  Result := Map<TResult>('PATCH', APath, AHandler);
end;

function TMARSRouter.Patch<TBody, TResult>(const APath: string;
  const AHandler: TMARSRouteBodyFunc<TBody, TResult>): TMARSRoute;
begin
  Result := Map<TBody, TResult>('PATCH', APath, AHandler);
end;

function TMARSRouter.Patch(const APath: string; const AHandler: TMARSRouteProc): TMARSRoute;
begin
  Result := Map('PATCH', APath, AHandler);
end;

function TMARSRouter.Delete<TResult>(const APath: string; const AHandler: TMARSRouteFunc<TResult>): TMARSRoute;
begin
  Result := Map<TResult>('DELETE', APath, AHandler);
end;

function TMARSRouter.Delete(const APath: string; const AHandler: TMARSRouteProc): TMARSRoute;
begin
  Result := Map('DELETE', APath, AHandler);
end;

function TMARSRouter.Attribute(const AAttribute: TCustomAttribute): TMARSRouter;
begin
  AddAttribute(AAttribute);
  Result := Self;
end;

function TMARSRouter.Consumes(const AMediaType: string): TMARSRouter;
begin
  Result := Attribute(ConsumesAttribute.Create(AMediaType));
end;

function TMARSRouter.CustomHeader(const AName, AValue: string): TMARSRouter;
begin
  Result := Attribute(CustomHeaderAttribute.Create(AName, AValue));
end;

function TMARSRouter.DenyAll: TMARSRouter;
begin
  Result := Attribute(DenyAllAttribute.Create);
end;

function TMARSRouter.NoLog: TMARSRouter;
begin
  Result := Attribute(NoLogAttribute.Create);
end;

function TMARSRouter.PermitAll: TMARSRouter;
begin
  Result := Attribute(PermitAllAttribute.Create);
end;

function TMARSRouter.Produces(const AMediaType: string): TMARSRouter;
begin
  Result := Attribute(ProducesAttribute.Create(AMediaType));
end;

function TMARSRouter.RolesAllowed(const ARoles: string): TMARSRouter;
begin
  Result := Attribute(RolesAllowedAttribute.Create(ARoles));
end;

{ TMARSRouteTable }

constructor TMARSRouteTable.Create(const AApplication: IMARSApplication);
begin
  inherited Create;
  FApplication := AApplication;
  FRoutes := TList<TMARSRoute>.Create;
  FRoot := TMARSRouter.Create(Self, nil, '');
end;

destructor TMARSRouteTable.Destroy;
begin
  FRoutes.Free;
  FRoot.Free;
  inherited;
end;

class function TMARSRouteTable.ForApplication(const AApplication: IMARSApplication): TMARSRouteTable;
begin
  if not Assigned(AApplication.RouteTable) then
    AApplication.RouteTable := TMARSRouteTable.Create(AApplication);
  Result := AApplication.RouteTable as TMARSRouteTable;
end;

procedure TMARSRouteTable.Add(const ARoute: TMARSRoute);
var
  LRoute: TMARSRoute;
  LKey: string;
  LConflict: string;
begin
  // two routes for the same method and shape
  LKey := ARoute.ShapeKey(True);
  for LRoute in FRoutes do
    if SameText(LRoute.ShapeKey(True), LKey) then
      raise ERouteDefinitionException.CreateFmt('Route %s %s conflicts with route %s %s'
        , [ARoute.HttpMethod, ARoute.Template, LRoute.HttpMethod, LRoute.Template]);

  // a route and a resource method for the same method and path
  LConflict := '';
  if Assigned(FApplication) then
  begin
    LKey := ARoute.ShapeKey(False);
    FApplication.EnumerateEndpoints(
      procedure (AName: string; AInfo: TMARSConstructorInfo; APath, AHttpMethod: string)
      var
        LShape: string;
        LToken: string;
      begin
        if LConflict <> '' then
          Exit;
        LShape := AHttpMethod.ToUpper + ' ';
        for LToken in SplitPath(APath) do
        begin
          if LToken = TMARSURL.PATH_PARAM_WILDCARD then
            LShape := LShape + TMARSURL.URL_PATH_SEPARATOR + TMARSURL.PATH_PARAM_WILDCARD
          else if LToken.StartsWith('{') and LToken.EndsWith('}') then
            LShape := LShape + TMARSURL.URL_PATH_SEPARATOR + '{}'
          else
            LShape := LShape + TMARSURL.URL_PATH_SEPARATOR + LToken.ToLower;
        end;
        if SameText(LShape, LKey) then
          LConflict := AHttpMethod + ' ' + APath + ' (' + AName + ')';
      end
    );
  end;
  if LConflict <> '' then
    raise ERouteDefinitionException.CreateFmt('Route %s %s conflicts with resource method %s'
      , [ARoute.HttpMethod, ARoute.Template, LConflict]);

  FRoutes.Add(ARoute);
end;

function TMARSRouteTable.Match(const ATokens: TArray<string>; const AHttpMethod: string;
  out ARoute: TMARSRoute; out AAllowedMethods: string): Boolean;
var
  LRoute: TMARSRoute;
  LAllowed: TStringList;
begin
  ARoute := nil;
  AAllowedMethods := '';
  LAllowed := nil;
  try
    for LRoute in FRoutes do
    begin
      if not LRoute.MatchesPath(ATokens) then
        Continue;

      if SameText(LRoute.HttpMethod, AHttpMethod) then
      begin
        if (not Assigned(ARoute)) or (LRoute.CompareSpecificity(ARoute) > 0) then
          ARoute := LRoute;
      end
      else
      begin
        if not Assigned(LAllowed) then
        begin
          LAllowed := TStringList.Create;
          LAllowed.Sorted := True;
          LAllowed.Duplicates := dupIgnore;
        end;
        LAllowed.Add(LRoute.HttpMethod);
      end;
    end;

    Result := Assigned(ARoute);
    if (not Result) and Assigned(LAllowed) then
      AAllowedMethods := string.Join(', ', LAllowed.ToStringArray);
  finally
    LAllowed.Free;
  end;
end;

{ TMARSRouteRtti }

class constructor TMARSRouteRtti.ClassCreate;
begin
  FContext := TRttiContext.Create;
  FFields := TDictionary<PTypeInfo, TRttiField>.Create;
  FLock := TCriticalSection.Create;
end;

class destructor TMARSRouteRtti.ClassDestroy;
begin
  FLock.Free;
  FFields.Free;
  FContext.Free;
end;

class function TMARSRouteRtti.RttiType(const ATypeInfo: PTypeInfo): TRttiType;
begin
  FLock.Enter;
  try
    Result := FContext.GetType(ATypeInfo);
  finally
    FLock.Leave;
  end;
end;

class function TMARSRouteRtti.ValueField(const AHolderType: PTypeInfo): TRttiField;
begin
  FLock.Enter;
  try
    if not FFields.TryGetValue(AHolderType, Result) then
    begin
      Result := FContext.GetType(AHolderType).GetField('Value');
      FFields.Add(AHolderType, Result);
    end;
  finally
    FLock.Leave;
  end;
end;

{ TMARSRouteModules }

class constructor TMARSRouteModules.ClassCreate;
begin
  FModules := TDictionary<string, TMARSRouteModule>.Create;
end;

class destructor TMARSRouteModules.ClassDestroy;
begin
  FModules.Free;
end;

class procedure TMARSRouteModules.Register(const AName, APath: string; const ADefine: TMARSRouteDefineProc);
var
  LModule: TMARSRouteModule;
begin
  LModule.Name := AName;
  LModule.Path := APath;
  LModule.Define := ADefine;
  FModules.AddOrSetValue(AName.ToLower, LModule);
end;

class function TMARSRouteModules.AddTo(const AApplication: IMARSApplication; const AModules: string): Boolean;
var
  LTable: TMARSRouteTable;
  LModule: TMARSRouteModule;
  LKeys: TArray<string>;
  LKey: string;
  LModulesToLower: string;
begin
  Result := False;
  LTable := TMARSRouteTable.ForApplication(AApplication);
  LModulesToLower := AModules.ToLower;

  LKeys := FModules.Keys.ToArray;
  TArray.Sort<string>(LKeys);
  for LKey in LKeys do
  begin
    if (IsMask(AModules) and MatchesMask(LKey, LModulesToLower)) or (LKey = LModulesToLower) then
    begin
      LModule := FModules[LKey];
      LTable.Root.Group(LModule.Path, LModule.Define);
      Result := True;
    end;
  end;
end;

end.
