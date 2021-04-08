(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Activation;

{$I MARS.inc}

interface

uses
  SysUtils, Classes, Generics.Collections, Rtti, Diagnostics

, MARS.Core.Classes, MARS.Core.URL, MARS.Core.MediaType
, MARS.Core.Application, MARS.Core.Engine, MARS.Core.Token
, MARS.Core.Registry.Utils, MARS.Core.Injection.Types, MARS.Core.Activation.Interfaces
, MARS.Core.MessageBodyWriter, MARS.Core.RequestAndResponse.Interfaces
;

type
  TMARSActivation = class;

  TMARSActivationFactoryFunc = reference to function (const AEngine: TMARSEngine;
    const AApplication: TMARSApplication;
    const ARequest: IMARSRequest; const AResponse: IMARSResponse;
    const AURL: TMARSURL
  ): IMARSActivation;

  TMARSBeforeInvokeProc = reference to procedure(const AActivation: IMARSActivation; out AIsAllowed: Boolean);
  TMARSAfterInvokeProc = reference to procedure(const AActivation: IMARSActivation);
  TMARSInvokeErrorProc = reference to procedure(const AActivation: IMARSActivation;
    const AException: Exception; var AHandled: Boolean);

  TMARSAuthorizationInfo = record
  public
    DenyAll, PermitAll: Boolean;
    AllowedRoles: TArray<string>;
    function NeedsAuthentication: Boolean;
    function NeedsAuthorization: Boolean;
    constructor Create(const ADenyAll, APermitAll: Boolean; const AAllowedRoles: TArray<string>);
  end;

  TMARSActivation = class(TInterfacedObject, IMARSActivation)
  private
    FRequest: IMARSRequest;
    FResponse: IMARSResponse;
    class var FBeforeInvokeProcs: TArray<TMARSBeforeInvokeProc>;
    class var FAfterInvokeProcs: TArray<TMARSAfterInvokeProc>;
    class var FInvokeErrorProcs: TArray<TMARSInvokeErrorProc>;
  protected
    FRttiContext: TRttiContext;
    FConstructorInfo: TMARSConstructorInfo;
    FApplication: TMARSApplication;
    FEngine: TMARSEngine;
    FURL: TMARSURL;
    FURLPrototype: TMARSURL;
    FToken: TMARSToken;
    FContext: TList<TValue>;
    FMethod: TRttiMethod;
    FMethodReturnType: TRttiType;
    FMethodAttributes: TArray<TCustomAttribute>;
    FResource: TRttiType;
    FResourceMethods: TArray<TRttiMethod>;
    FResourceAttributes: TArray<TCustomAttribute>;
    FResourcePath: string;
    FResourceInstance: TObject;
    FMethodArguments: TArray<TValue>;
    FMethodResult: TValue;
    FWriter: IMessageBodyWriter;
    FWriterMediaType: TMediaType;
    FInvocationTime: TStopWatch;
    FSetupTime: TStopWatch;
    FTeardownTime: TStopWatch;
    FSerializationTime: TStopWatch;
    FAuthorizationInfo: TMARSAuthorizationInfo;

    procedure FreeContext; virtual;
    procedure CleanupGarbage(const AValue: TValue; const ADestroyed: TList<TObject>); virtual;
    procedure ContextInjection; virtual;
    function GetContextValue(const ADestination: TRttiObject): TInjectionValue; virtual;
    function GetMethodArgument(const AParam: TRttiParameter): TValue; virtual;
    procedure FillResourceMethodParameters; virtual;
    procedure FindMethodToInvoke; virtual;
    procedure InvokeResourceMethod; virtual;
    procedure SetCustomHeaders; virtual;
    procedure WriteToResponse(const AValue: TValue;
      const AValueContentType: string; const AOriginalContentType: string); virtual;

    function DoBeforeInvoke: Boolean;
    procedure DoAfterInvoke;
    procedure DoInvokeError(const E: Exception); virtual;

    procedure CheckResource; virtual;
    procedure CheckMethod; virtual;
    procedure ReadAuthorizationInfo; virtual;
    procedure CheckAuthentication; virtual;
    procedure CheckAuthorization; virtual;
  public
    constructor Create(const AEngine: TMARSEngine; const AApplication: TMARSApplication;
      const ARequest: IMARSRequest; const AResponse: IMARSResponse; const AURL: TMARSURL); virtual;
    destructor Destroy; override;

    // --- IMARSActivation implementation --------------
    procedure AddToContext(AValue: TValue); virtual;
    function HasToken: Boolean; virtual;
    procedure Invoke; virtual;

    function GetApplication: TMARSApplication; inline;
    function GetEngine: TMARSEngine; inline;
    function GetInvocationTime: TStopwatch; inline;
    function GetSetupTime: TStopwatch; inline;
    function GetTeardownTime: TStopwatch; inline;
    function GetSerializationTime: TStopwatch; inline;
    function GetMethod: TRttiMethod; inline;
    function GetMethodReturnType: TRttiType; inline;
    function GetMethodArguments: TArray<TValue>; inline;
    function GetMethodAttributes: TArray<TCustomAttribute>; inline;
    function GetMethodResult: TValue; inline;
    function GetRequest: IMARSRequest; inline;
    function GetResource: TRttiType; inline;
    function GetResourceAttributes: TArray<TCustomAttribute>; inline;
    function GetResourceInstance: TObject; inline;
    function GetResponse: IMARSResponse; inline;
    function GetURL: TMARSURL; inline;
    function GetURLPrototype: TMARSURL; inline;
    function GetToken: TMARSToken; inline;
    // ---

    property Application: TMARSApplication read FApplication;
    property Engine: TMARSEngine read FEngine;
    property InvocationTime: TStopwatch read FInvocationTime;
    property Method: TRttiMethod read FMethod;
    property MethodArguments: TArray<TValue> read FMethodArguments;
    property Request: IMARSRequest read FRequest;
    property Resource: TRttiType read FResource;
    property ResourceInstance: TObject read FResourceInstance;
    property Response: IMARSResponse read FResponse;
    property URL: TMARSURL read FURL;
    property URLPrototype: TMARSURL read FURLPrototype;
    property Token: TMARSToken read GetToken;

    class procedure RegisterBeforeInvoke(const ABeforeInvoke: TMARSBeforeInvokeProc);
//    class procedure UnregisterBeforeInvoke(const ABeforeInvoke: TMARSBeforeInvokeProc);
    class procedure ClearBeforeInvokes;
    class procedure RegisterAfterInvoke(const AAfterInvoke: TMARSAfterInvokeProc);
//    class procedure UnregisterAfterInvoke(const AAfterInvoke: TMARSAfterInvokeProc);
    class procedure ClearAfterInvokes;
    class procedure RegisterInvokeError(const AInvokeError: TMARSInvokeErrorProc);
    class procedure ClearInvokeErrors;


    class var CreateActivationFunc: TMARSActivationFactoryFunc;
    class function CreateActivation(const AEngine: TMARSEngine;
      const AApplication: TMARSApplication;
      const ARequest: IMARSRequest; const AResponse: IMARSResponse;
      const AURL: TMARSURL): IMARSActivation;
  end;

implementation

uses
  TypInfo
, MARS.Core.Attributes, MARS.Core.Response, MARS.Core.MessageBodyReader
, MARS.Core.Exceptions, MARS.Core.Utils, MARS.Utils.Parameters, MARS.Rtti.Utils
, MARS.Core.Injection, MARS.Core.Activation.InjectionService
;

{ TMARSActivation }

function TMARSActivation.GetMethod: TRttiMethod;
begin
  Result := FMethod;
end;

function TMARSActivation.GetMethodArgument(const AParam: TRttiParameter): TValue;
var
  LParamValue: TValue;
begin
  TValue.Make(nil, AParam.ParamType.Handle, LParamValue);

  AParam.HasAttribute<ContextAttribute>(
    procedure (AContextAttr: ContextAttribute)
    begin
      LParamValue := GetContextValue(AParam).Value;
    end
  );

  Result := LParamValue;
end;

function TMARSActivation.GetMethodArguments: TArray<TValue>;
begin
  Result := FMethodArguments;
end;

function TMARSActivation.GetMethodAttributes: TArray<TCustomAttribute>;
begin
  Result := FMethodAttributes;
end;

function TMARSActivation.GetMethodResult: TValue;
begin
  Result := FMethodResult;
end;

function TMARSActivation.GetMethodReturnType: TRttiType;
begin
  Result := FMethodReturnType;
end;

function TMARSActivation.GetRequest: IMARSRequest;
begin
  Result := FRequest;
end;

function TMARSActivation.GetResource: TRttiType;
begin
  Result := FResource;
end;

function TMARSActivation.GetResourceAttributes: TArray<TCustomAttribute>;
begin
  Result := FResourceAttributes;
end;

function TMARSActivation.GetResourceInstance: TObject;
begin
  Result := FResourceInstance;
end;

function TMARSActivation.GetResponse: IMARSResponse;
begin
  Result := FResponse;
end;

function TMARSActivation.GetSerializationTime: TStopwatch;
begin
  Result := FSerializationTime;
end;

function TMARSActivation.GetSetupTime: TStopwatch;
begin
  Result := FSetupTime;
end;

function TMARSActivation.GetTeardownTime: TStopwatch;
begin
  Result := FTeardownTime;
end;

function TMARSActivation.GetToken: TMARSToken;
begin
  if not Assigned(FToken) then
  begin
    FToken := GetContextValue(FRttiContext.GetType(Self.ClassType).GetField('FToken')).Value.AsType<TMARSToken>;
    if not Assigned(FToken) then
      raise EMARSException.Create('Token injection failed in MARSActivation. '
        + 'Check you have added at least one Token implementation to your uses clause. '
        + 'On Windows, try adding MARS.mORMotJWT.Token unit to your uses clause.');
  end;
  Result := FToken;
end;

function TMARSActivation.GetURL: TMARSURL;
begin
  Result := FURL;
end;

function TMARSActivation.GetURLPrototype: TMARSURL;
begin
  Result := FURLPrototype;
end;

function TMARSActivation.HasToken: Boolean;
begin
  Result := Assigned(FToken);
end;

procedure TMARSActivation.FillResourceMethodParameters;
var
  LParameters: TArray<TRttiParameter>;
  LIndex: Integer;
  LParameter: TRttiParameter;
begin
  Assert(Assigned(FMethod));
  try
    LParameters := FMethod.GetParameters;
    SetLength(FMethodArguments, Length(LParameters));
    for LIndex := Low(LParameters) to High(LParameters) do
    begin
      LParameter := LParameters[LIndex];
      FMethodArguments[LIndex] := GetMethodArgument(LParameter);
    end;
  except
    on E: Exception do
      raise EMARSApplicationException.CreateFmt(
        'Bad parameter value for method %s.%s (%s). %s', [FResource.Name, FMethod.Name, FURLPrototype.Path, E.Message]);

  end;
end;

procedure TMARSActivation.FindMethodToInvoke;
var
  LMethod: TRttiMethod;
  LAttribute: TCustomAttribute;
  LPathMatches: Boolean;
  LHttpMethodMatches: Boolean;
  LMethodPath: string;
begin
  FMethod := nil;
  FMethodReturnType := nil;
  FMethodAttributes := [];
  FreeAndNil(FURLPrototype);

  for LMethod in FResourceMethods do
  begin
    if (LMethod.Visibility < TMemberVisibility.mvPublic)
      or LMethod.IsConstructor or LMethod.IsDestructor
    then
      Continue;

    LMethodPath := '';
    LHttpMethodMatches := False;

    for LAttribute in LMethod.GetAttributes do
    begin
      if LAttribute is PathAttribute then
        LMethodPath := PathAttribute(LAttribute).Value;

      if LAttribute is HttpMethodAttribute then
        LHttpMethodMatches := HttpMethodAttribute(LAttribute).Matches(Request);

      { TODO -oAndrea : Check MediaType (you might have multiple methods matching, so let's discriminate using Request.Accept and Resource+Method's Produces attribute) }
    end;

    if LHttpMethodMatches then
    begin
      FURLPrototype := TMARSURL.CreateDummy([Engine.BasePath, Application.BasePath, FResourcePath, LMethodPath]);
      try
        LPathMatches := FURLPrototype.MatchPath(URL);
        if LPathMatches and LHttpMethodMatches then
        begin
          FMethod := LMethod;
          FMethodAttributes := FMethod.GetAllAttributes(False);
          FMethodReturnType := FMethod.ReturnType;
          Break;
        end;
      finally
        if not Assigned(FMethod) then
          FreeAndNil(FURLPrototype);
      end;
    end;
  end;
end;

procedure TMARSActivation.CleanupGarbage(const AValue: TValue; const ADestroyed: TList<TObject>);

  procedure FreeOnlyOnce(const AValueToFree: TValue);
  var
    LObj: TObject;
  begin
    if AValueToFree.IsArray then Exit; //AM workaround, somehow we can get here with a TValue that is not an object

    LObj := AValueToFree.AsObject;
    if not ADestroyed.Contains(LObj) then
    begin
      ADestroyed.Add(LObj);
      LObj.Free;
    end;
  end;

var
  LIndex: Integer;
  LValue: TValue;
begin
  case AValue.Kind of
    tkClass: FreeOnlyOnce(AValue);
    tkArray,
    tkDynArray:
    begin
      for LIndex := 0 to AValue.GetArrayLength -1 do
      begin
        LValue := AValue.GetArrayElement(LIndex);
        case LValue.Kind of
          tkClass: FreeOnlyOnce(LValue);
          tkArray, tkDynArray: CleanupGarbage(LValue, ADestroyed); //recursion
        end;
      end;
    end;
  end;
end;

class procedure TMARSActivation.ClearAfterInvokes;
begin
  FAfterInvokeProcs := [];
end;

class procedure TMARSActivation.ClearBeforeInvokes;
begin
  FBeforeInvokeProcs := [];
end;

class procedure TMARSActivation.ClearInvokeErrors;
begin
  FInvokeErrorProcs := [];
end;

procedure TMARSActivation.FreeContext;
var
  LDestroyed: TList<TObject>;
  LValue: TValue;
begin
  if FContext.Count = 0 then
    Exit;

  LDestroyed := TList<TObject>.Create;
  try
    while FContext.Count > 0 do
    begin
      LValue := FContext[0];
      CleanupGarbage(LValue, LDestroyed);
      FContext.Delete(0);
    end;
  finally
    LDestroyed.Free;
  end;
end;

procedure TMARSActivation.WriteToResponse(const AValue: TValue;
  const AValueContentType: string; const AOriginalContentType: string);
var
  LStream: TBytesStream;
begin
  // 1 - TMARSResponse (override)
  if (not AValue.IsEmpty) // workaround for IsInstanceOf returning True on empty value (https://quality.embarcadero.com/browse/RSP-15301)
     and AValue.IsInstanceOf(TMARSResponse)
  then
    TMARSResponse(AValue.AsObject).CopyTo(Response)
  // 2 - MessageBodyWriter mechanism (standard)
  else begin
    TMARSMessageBodyRegistry.Instance.FindWriter(Self, FWriter, FWriterMediaType);
    try
      if Assigned(FWriter) then
      begin
        if AValueContentType = AOriginalContentType then
          Response.ContentType := FWriterMediaType.ToString;

        LStream := TBytesStream.Create();
        try
          FWriter.WriteTo(AValue, FWriterMediaType, LStream, Self);
          LStream.Position := 0;
          Response.ContentStream := LStream;
        except
          LStream.Free;
          raise;
        end;
      end
      else
        raise EMARSHttpException.CreateFmt('MessageBodyWriter not found for method %s of resource %s', [Method.Name, Resource.Name]);
    finally
      FWriter := nil;
      FreeAndNil(FWriterMediaType);
    end;
  end;
end;

procedure TMARSActivation.InvokeResourceMethod();
var
  LContentType: string;
begin
  Assert(Assigned(FMethod));

  // cache initial ContentType value to check later if it has been changed
  LContentType := string(Response.ContentType);
  try
    // set attribute-based custom header's values
    SetCustomHeaders;

    // actual method invocation
    FMethodResult := FMethod.Invoke(FResourceInstance, FMethodArguments);

    FSerializationTime := TStopwatch.StartNew;
    if Assigned(FMethodReturnType) then // if the method is actually a function and not a procedure
      WriteToResponse(FMethodResult, string(Response.ContentType), LContentType);
    FSerializationTime.Stop;
  finally
    if not FMethod.HasAttribute<IsReference>(nil) then
      AddToContext(FMethodResult);
  end;
end;

procedure TMARSActivation.ReadAuthorizationInfo;
var
  LProcessAuthorizationAttribute: TProc<AuthorizationAttribute>;
  LAllowedRoles: TStringList;
begin
{$ifdef DelphiXE7_UP}
  FAuthorizationInfo := TMARSAuthorizationInfo.Create(False, False, []);
{$else}
  FAuthorizationInfo := TMARSAuthorizationInfo.Create(False, False, nil);
{$endif}

  LAllowedRoles := TStringList.Create;
  try
    LAllowedRoles.Sorted := True;
    LAllowedRoles.Duplicates := TDuplicates.dupIgnore;

    LProcessAuthorizationAttribute :=
      procedure (AAttribute: AuthorizationAttribute)
      begin
        if AAttribute is DenyAllAttribute then
          FAuthorizationInfo.DenyAll := True
        else if AAttribute is PermitAllAttribute then
          FAuthorizationInfo.PermitAll := True
        else if AAttribute is RolesAllowedAttribute then
          LAllowedRoles.AddStrings(RolesAllowedAttribute(AAttribute).Roles);
      end;

    TRttiHelper.ForEachAttribute<AuthorizationAttribute>(FMethodAttributes, LProcessAuthorizationAttribute);
    TRttiHelper.ForEachAttribute<AuthorizationAttribute>(FResourceAttributes, LProcessAuthorizationAttribute);

    FAuthorizationInfo.AllowedRoles := LAllowedRoles.ToStringArray;
  finally
    LAllowedRoles.Free;
  end;
end;

class procedure TMARSActivation.RegisterAfterInvoke(
  const AAfterInvoke: TMARSAfterInvokeProc);
begin
  FAfterInvokeProcs := FAfterInvokeProcs + [TMARSAfterInvokeProc(AAfterInvoke)];
end;

class procedure TMARSActivation.RegisterBeforeInvoke(
  const ABeforeInvoke: TMARSBeforeInvokeProc);
begin
  FBeforeInvokeProcs := FBeforeInvokeProcs + [TMARSBeforeInvokeProc(ABeforeInvoke)];
end;

class procedure TMARSActivation.RegisterInvokeError(
  const AInvokeError: TMARSInvokeErrorProc);
begin
  FInvokeErrorProcs := FInvokeErrorProcs + [TMARSInvokeErrorProc(AInvokeError)];
end;

procedure TMARSActivation.SetCustomHeaders;
var
  LCustomAtributeProcessor: TProc<CustomHeaderAttribute>;

begin
  LCustomAtributeProcessor :=
    procedure (ACustomHeader: CustomHeaderAttribute)
    begin
      Response.SetHeader(ACustomHeader.HeaderName, ACustomHeader.Value);
    end;
  TRttiHelper.ForEachAttribute<CustomHeaderAttribute>(FResourceAttributes, LCustomAtributeProcessor);
  TRttiHelper.ForEachAttribute<CustomHeaderAttribute>(FMethodAttributes, LCustomAtributeProcessor);
end;

//class procedure TMARSActivation.UnregisterAfterInvoke(
//  const AAfterInvoke: TMARSAfterInvokeProc);
//begin
//  FAfterInvokeProcs := FAfterInvokeProcs - [TMARSAfterInvokeProc(AAfterInvoke)];
//end;
//
//class procedure TMARSActivation.UnregisterBeforeInvoke(
//  const ABeforeInvoke: TMARSBeforeInvokeProc);
//begin
//  FBeforeInvokeProcs := FBeforeInvokeProcs - [TMARSBeforeInvokeProc(ABeforeInvoke)];
//end;

procedure TMARSActivation.Invoke;

  procedure HandleException(const AException: Exception);
  begin
    if AException is EMARSHttpException then
    begin
      Response.StatusCode := EMARSHttpException(AException).Status;
      Response.Content := AException.Message;
      Response.ContentType := TMediaType.TEXT_PLAIN;
    end
    else begin
      Response.StatusCode := 500;
      Response.Content := 'Internal server error';
      {$IFDEF DEBUG}
      Response.Content := 'Internal server error: [' + AException.ClassName + '] ' + AException.Message;
      {$ENDIF}
      Response.ContentType := TMediaType.TEXT_PLAIN;
    end;

    DoInvokeError(AException);
  end;

begin
  try
    try
      Request.CheckWorkaroundForISAPI;

      // setup phase
      FSetupTime := TStopWatch.StartNew;
      CheckResource;
      CheckMethod;

      ReadAuthorizationInfo;
      CheckAuthentication;
      CheckAuthorization;

      FResourceInstance := FConstructorInfo.ConstructorFunc(Self);
      FillResourceMethodParameters;

      ContextInjection;
      FSetupTime.Stop;

      // invocation phase
      FInvocationTime := TStopwatch.StartNew;
      if DoBeforeInvoke then
      begin
        InvokeResourceMethod;
        FInvocationTime.Stop;
        DoAfterInvoke;
      end;

    except on E:Exception do
      HandleException(E);
    end;

  finally
    // teardown phase
    FTeardownTime := TStopwatch.StartNew;
    if Assigned(FResourceInstance) then
      FResourceInstance.Free;
    FreeContext;
    FTeardownTime.Stop;
  end;
end;

procedure TMARSActivation.CheckAuthentication;
begin
  if (Token.Token <> '') and Token.IsExpired then
    Token.Clear;

  if FAuthorizationInfo.NeedsAuthentication then
    if ((Token.Token = '') or not Token.IsVerified) then
    begin
      Token.Clear;
      raise EMARSAuthenticationException.Create('Token missing, not valid or expired', 403);
    end;
end;

procedure TMARSActivation.CheckAuthorization;
begin
  if FAuthorizationInfo.NeedsAuthorization then
    if FAuthorizationInfo.DenyAll // DenyAll (stronger than PermitAll and Roles-based authorization)
       or (
         not FAuthorizationInfo.PermitAll  // PermitAll (stronger than Role-based authorization)
         and ((Length(FAuthorizationInfo.AllowedRoles) > 0) and (not Token.HasRole(FAuthorizationInfo.AllowedRoles)))
       ) then
      raise EMARSAuthorizationException.Create('Forbidden', 403);
end;

procedure TMARSActivation.CheckMethod;
begin
  FindMethodToInvoke;

  if not Assigned(FMethod) then
    raise EMARSMethodNotFoundException.Create(
      Format('[%s] No implementation found for http method %s', [URL.Resource, Request.Method]), 404);
end;

procedure TMARSActivation.CheckResource;
var
  LFound: Boolean;
begin
  LFound := Application.Resources.TryGetValue(URL.Resource.ToLower, FConstructorInfo);

  // second attempt, if DefaultResourcePath is not empty
  if (not LFound) and (Application.DefaultResourcePath <> '') then
    LFound := Application.Resources.TryGetValue(Application.DefaultResourcePath, FConstructorInfo);

  if not LFound then
    raise EMARSResourceNotFoundException.Create(Format('Resource [%s] not found', [URL.Resource]), 404);

  FResource := FRttiContext.GetType(FConstructorInfo.TypeTClass);
  FResourceAttributes := FResource.GetAllAttributes(True);
  FResourceMethods := FResource.GetMethods;
  FResourcePath := '';
  FResource.HasAttribute<PathAttribute>(
    procedure (APathAttribute: PathAttribute)
    begin
      FResourcePath := APathAttribute.Value;
    end
  );
end;

procedure TMARSActivation.AddToContext(AValue: TValue);
begin
  if not AValue.IsEmpty then
    FContext.Add(AValue);
end;

procedure TMARSActivation.ContextInjection();
var
  LType: TRttiType;
begin
  LType := FRttiContext.GetType(FResourceInstance.ClassType);

  // fields
  LType.ForEachFieldWithAttribute<ContextAttribute>(
    function (AField: TRttiField; AAttrib: ContextAttribute): Boolean
    begin
      Result := True; // enumerate all
      AField.SetValue(FResourceInstance, GetContextValue(AField).Value);
    end
  );

  // properties
  LType.ForEachPropertyWithAttribute<ContextAttribute>(
    function (AProperty: TRttiProperty; AAttrib: ContextAttribute): Boolean
    begin
      Result := True; // enumerate all
      AProperty.SetValue(FResourceInstance, GetContextValue(AProperty).Value);
    end
  );
end;

function TMARSActivation.GetApplication: TMARSApplication;
begin
  Result := FApplication;
end;

function TMARSActivation.GetContextValue(const ADestination: TRttiObject): TInjectionValue;
begin
  Result := TMARSInjectionServiceRegistry.Instance.GetValue(ADestination, Self);
  if not Result.IsReference then
    AddToContext(Result.Value);
end;


function TMARSActivation.GetEngine: TMARSEngine;
begin
  Result := FEngine;
end;

function TMARSActivation.GetInvocationTime: TStopwatch;
begin
  Result := FInvocationTime;
end;

constructor TMARSActivation.Create(const AEngine: TMARSEngine;
  const AApplication: TMARSApplication;
  const ARequest: IMARSRequest; const AResponse: IMARSResponse;
  const AURL: TMARSURL);
begin
  inherited Create;
  FEngine := AEngine;
  FApplication := AApplication;
  FRequest := ARequest;
  FResponse := AResponse;
  FURL := AURL;

  FURLPrototype := nil;
  FToken := nil;

  FRttiContext := TRttiContext.Create;
  FContext := TList<TValue>.Create;

  FMethod := nil;
  FMethodReturnType := nil;
  FMethodAttributes := [];
  FMethodArguments := [];
  FMethodResult := TValue.Empty;

  FResource := nil;
  FResourcePath := '';
  FResourceMethods := [];
  FResourceAttributes := [];
  FResourceInstance := nil;

  FInvocationTime.Reset;
  FSetupTime.Reset;
  FTeardownTime.Reset;
  FSerializationTime.Reset;
end;

class function TMARSActivation.CreateActivation(const AEngine: TMARSEngine;
  const AApplication: TMARSApplication; const ARequest: IMARSRequest;
  const AResponse: IMARSResponse; const AURL: TMARSURL): IMARSActivation;
begin
  if Assigned(CreateActivationFunc) then
    Result := CreateActivationFunc(AEngine, AApplication, ARequest, AResponse, AURL)
  else
    Result := TMARSActivation.Create(AEngine, AApplication, ARequest, AResponse, AURL);
end;

destructor TMARSActivation.Destroy;
begin
  FreeContext;
  FContext.Free;
  FreeAndNil(FURLPrototype);
  inherited;
end;

procedure TMARSActivation.DoAfterInvoke;
var
  LSubscriber: TMARSAfterInvokeProc;
begin
  for LSubscriber in FAfterInvokeProcs do
    LSubscriber(Self);

  TRttiHelper.ForEachMethodWithAttribute<AfterInvokeAttribute>(FResourceMethods
  , function (AMethod: TRttiMethod; AAttribute: AfterInvokeAttribute): Boolean
    var
      LReturnValue: TValue;
    begin
      Result := True;
      LReturnValue := AMethod.Invoke(FResourceInstance, []);
      if Assigned(AMethod.ReturnType) and (AMethod.ReturnType.Handle = TypeInfo(Boolean)) then
        Result := LReturnValue.AsBoolean;
    end
  );
end;

function TMARSActivation.DoBeforeInvoke: Boolean;
var
  LSubscriber: TMARSBeforeInvokeProc;
  LResult: Boolean;
begin
  LResult := True;
  for LSubscriber in FBeforeInvokeProcs do
    LSubscriber(Self, LResult);

  if LResult then
    TRttiHelper.ForEachMethodWithAttribute<BeforeInvokeAttribute>(FResourceMethods
    , function (AMethod: TRttiMethod; AAttribute: BeforeInvokeAttribute): Boolean
      var
        LReturnValue: TValue;
      begin
        Result := True;
        LReturnValue := AMethod.Invoke(FResourceInstance, []);
        if Assigned(AMethod.ReturnType) and (AMethod.ReturnType.Handle = TypeInfo(Boolean)) then
        begin
          LResult := LReturnValue.AsBoolean;
          Result := LReturnValue.AsBoolean;
        end;
      end
    );

  Result := LResult;
end;

procedure TMARSActivation.DoInvokeError(const E: Exception);
var
  LSubscriber: TMARSInvokeErrorProc;
  LHandled: Boolean;
begin
  LHandled := False;
  for LSubscriber in FInvokeErrorProcs do
  begin
    LSubscriber(Self, E, LHandled);
    if LHandled then
      Break;
  end;

  if Assigned(FResourceInstance) and (not LHandled) then
    TRttiHelper.ForEachMethodWithAttribute<InvokeErrorAttribute>(FResourceMethods
    , function (AMethod: TRttiMethod; AAttribute: InvokeErrorAttribute): Boolean
      var
        LReturnValue: TValue;
      begin
        Result := True;
        if TRttiHelper.MethodParametersMatch<Exception>(AMethod) then
        begin
          LReturnValue := AMethod.Invoke(FResourceInstance, [E]);
          if Assigned(AMethod.ReturnType) and (AMethod.ReturnType.Handle = TypeInfo(Boolean)) then
            Result := LReturnValue.AsBoolean;
        end;
      end
    );
end;

{ TMARSAuthorizationInfo }

constructor TMARSAuthorizationInfo.Create(const ADenyAll, APermitAll: Boolean; const AAllowedRoles: TArray<string>);
begin
  DenyAll := ADenyAll;
  PermitAll := APermitAll;
  AllowedRoles := AAllowedRoles;
end;

function TMARSAuthorizationInfo.NeedsAuthentication: Boolean;
begin
  Result := Length(AllowedRoles) > 0;
end;

function TMARSAuthorizationInfo.NeedsAuthorization: Boolean;
begin
  Result := (Length(AllowedRoles) > 0) or DenyAll;
end;

end.
