(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Activation;

{$I MARS.inc}

interface

uses
  SysUtils, Classes, Generics.Collections, Rtti, Diagnostics
  , HTTPApp

  , MARS.Core.Classes
  , MARS.Core.URL
  , MARS.Core.Application
  , MARS.Core.Engine
  , MARS.Core.Token
  , MARS.Core.Registry
  , MARS.Core.MessageBodyWriter
  , MARS.Core.MediaType
  , MARS.Core.Injection.Types
  , MARS.Core.Activation.Interfaces
  ;

type
  TMARSActivation = class;

  TMARSActivationFactoryFunc = reference to function (const AEngine: TMARSEngine;
    const AApplication: TMARSApplication;
    const ARequest: TWebRequest; const AResponse: TWebResponse;
    const AURL: TMARSURL
  ): IMARSActivation;

  TMARSBeforeInvokeProc = reference to procedure(const AActivation: IMARSActivation; out AIsAllowed: Boolean);
  TMARSAfterInvokeProc = reference to procedure(const AActivation: IMARSActivation);

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
    FRequest: TWebRequest;
    FResponse: TWebResponse;
    class var FBeforeInvokeProcs: TArray<TMARSBeforeInvokeProc>;
    class var FAfterInvokeProcs: TArray<TMARSAfterInvokeProc>;
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
    FResource: TRttiType;
    FResourceInstance: TObject;
    FMethodArguments: TArray<TValue>;
    FMethodResult: TValue;
    FWriter: IMessageBodyWriter;
    FWriterMediaType: TMediaType;
    FInvocationTime: TStopWatch;
    FAuthorizationInfo: TMARSAuthorizationInfo;

    procedure FreeContext; virtual;
    procedure CleanupGarbage(const AValue: TValue); virtual;
    procedure ContextInjection; virtual;
    function GetContextValue(const ADestination: TRttiObject): TInjectionValue; virtual;
    function GetMethodArgument(const AParam: TRttiParameter): TValue; virtual;
    function GetProducesValue: string; virtual;
    procedure FillResourceMethodParameters; virtual;
    procedure FindMethodToInvoke; virtual;
    procedure InvokeResourceMethod; virtual;
    procedure SetCustomHeaders; virtual;

    function DoBeforeInvoke: Boolean;
    procedure DoAfterInvoke;

    procedure CheckResource; virtual;
    procedure CheckMethod; virtual;
    procedure ReadAuthorizationInfo; virtual;
    procedure CheckAuthentication; virtual;
    procedure CheckAuthorization; virtual;
  public
    constructor Create(const AEngine: TMARSEngine; const AApplication: TMARSApplication;
      const ARequest: TWebRequest; const AResponse: TWebResponse; const AURL: TMARSURL); virtual;
    destructor Destroy; override;

    procedure Prepare; virtual;

    // --- IMARSActivation implementation --------------
    procedure AddToContext(AValue: TValue); virtual;
    function HasToken: Boolean; virtual;
    procedure Invoke; virtual;

    function GetApplication: TMARSApplication;
    function GetEngine: TMARSEngine;
    function GetInvocationTime: TStopwatch;
    function GetMethod: TRttiMethod;
    function GetMethodArguments: TArray<TValue>;
    function GetMethodResult: TValue;
    function GetRequest: TWebRequest;
    function GetResource: TRttiType;
    function GetResourceInstance: TObject;
    function GetResponse: TWebResponse;
    function GetURL: TMARSURL;
    function GetURLPrototype: TMARSURL;
    function GetToken: TMARSToken;
    // ---

    property Application: TMARSApplication read FApplication;
    property Engine: TMARSEngine read FEngine;
    property InvocationTime: TStopwatch read FInvocationTime;
    property Method: TRttiMethod read FMethod;
    property MethodArguments: TArray<TValue> read FMethodArguments;
    property Request: TWebRequest read FRequest;
    property Resource: TRttiType read FResource;
    property ResourceInstance: TObject read FResourceInstance;
    property Response: TWebResponse read FResponse;
    property URL: TMARSURL read FURL;
    property URLPrototype: TMARSURL read FURLPrototype;
    property Token: TMARSToken read GetToken;

    class procedure RegisterBeforeInvoke(const ABeforeInvoke: TMARSBeforeInvokeProc);
//    class procedure UnregisterBeforeInvoke(const ABeforeInvoke: TMARSBeforeInvokeProc);
    class procedure RegisterAfterInvoke(const AAfterInvoke: TMARSAfterInvokeProc);
//    class procedure UnregisterAfterInvoke(const AAfterInvoke: TMARSAfterInvokeProc);

    class var CreateActivationFunc: TMARSActivationFactoryFunc;
    class function CreateActivation(const AEngine: TMARSEngine;
      const AApplication: TMARSApplication;
      const ARequest: TWebRequest; const AResponse: TWebResponse;
      const AURL: TMARSURL): IMARSActivation;
  end;

implementation

uses
    MARS.Core.Attributes
  , MARS.Core.Response
  , MARS.Core.MessageBodyReader
  , MARS.Core.Exceptions
  , MARS.Core.Utils
  , MARS.Utils.Parameters
  , MARS.Rtti.Utils
  , MARS.Core.Injection
  , MARS.Core.Activation.InjectionService
  , TypInfo
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

function TMARSActivation.GetMethodResult: TValue;
begin
  Result := FMethodResult;
end;

function TMARSActivation.GetProducesValue: string;
var
  LProduces: string;
begin
  LProduces := '';

  Method.HasAttribute<ProducesAttribute>(
    procedure(AAttr: ProducesAttribute)
    begin
      LProduces := AAttr.Value;
    end
  );
  if LProduces = '' then
    Resource.HasAttribute<ProducesAttribute>(
      procedure(AAttr: ProducesAttribute)
      begin
        LProduces := AAttr.Value;
      end
    );
  { TODO -oAndrea : Fallback to Application default? }

  Result := LProduces;
end;

function TMARSActivation.GetRequest: TWebRequest;
begin
  Result := FRequest;
end;

function TMARSActivation.GetResource: TRttiType;
begin
  Result := FResource;
end;

function TMARSActivation.GetResourceInstance: TObject;
begin
  Result := FResourceInstance;
end;

function TMARSActivation.GetResponse: TWebResponse;
begin
  Result := FResponse;
end;

function TMARSActivation.GetToken: TMARSToken;
begin
  if not Assigned(FToken) then
    FToken := GetContextValue(FRttiContext.GetType(Self.ClassType).GetField('FToken')).Value.AsType<TMARSToken>;
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
      raise EMARSApplicationException.Create('Bad parameter values for resource method ' + FMethod.Name);
  end;
end;

procedure TMARSActivation.FindMethodToInvoke;
var
  LMethod: TRttiMethod;
  LResourcePath: string;
  LAttribute: TCustomAttribute;
  LPathMatches: Boolean;
  LHttpMethodMatches: Boolean;
  LMethodPath: string;
begin
  FResource := FRttiContext.GetType(FConstructorInfo.TypeTClass);
  FMethod := nil;
  FreeAndNil(FURLPrototype);

  LResourcePath := '';
  FResource.HasAttribute<PathAttribute>(
    procedure (APathAttribute: PathAttribute)
    begin
      LResourcePath := APathAttribute.Value;
    end
  );

  for LMethod in FResource.GetMethods do
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
      FURLPrototype := TMARSURL.CreateDummy([Engine.BasePath, Application.BasePath, LResourcePath, LMethodPath]);
      try
        LPathMatches := FURLPrototype.MatchPath(URL);
        if LPathMatches and LHttpMethodMatches then
        begin
          FMethod := LMethod;
          Break;
        end;
      finally
        if not Assigned(FMethod) then
          FreeAndNil(FURLPrototype);
      end;
    end;
  end;
end;

procedure TMARSActivation.CleanupGarbage(const AValue: TValue);
var
  LIndex: Integer;
  LValue: TValue;
begin
  case AValue.Kind of
    tkClass: AValue.AsObject.Free;
    tkArray,
    tkDynArray:
    begin
      for LIndex := 0 to AValue.GetArrayLength -1 do
      begin
        LValue := AValue.GetArrayElement(LIndex);
        case LValue.Kind of
          tkClass: LValue.AsObject.Free;
          tkArray, tkDynArray: CleanupGarbage(LValue); //recursion
        end;
      end;
    end;
  end;
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
      if LValue.IsObject then
      begin
        if not LDestroyed.Contains(LValue.AsObject) then
        begin
          LDestroyed.Add(LValue.AsObject);
          CleanupGarbage(LValue);
        end;
      end
      else
        CleanupGarbage(LValue);
      FContext.Delete(0);
    end;
  finally
    LDestroyed.Free;
  end;
end;

procedure TMARSActivation.InvokeResourceMethod();
var
  LStream: TBytesStream;
  LContentType: string;
begin
  Assert(Assigned(FMethod));

  // cache initial ContentType value to check later if it has been changed
  LContentType := string(Response.ContentType);

  try
    FMethodResult := FMethod.Invoke(FResourceInstance, FMethodArguments);

    // handle response
    SetCustomHeaders;

    // 1 - TMARSResponse (override)
    if (not FMethodResult.IsEmpty) // workaround for IsInstanceOf returning True on empty value (https://quality.embarcadero.com/browse/RSP-15301)
       and FMethodResult.IsInstanceOf(TMARSResponse)
    then
      TMARSResponse(FMethodResult.AsObject).CopyTo(Response)
    // 2 - MessageBodyWriter mechanism (standard)
    else begin
      TMARSMessageBodyRegistry.Instance.FindWriter(Self, FWriter, FWriterMediaType);
      try
        if Assigned(FWriter) then
        begin
          if string(Response.ContentType) = LContentType then
            Response.ContentType := FWriterMediaType.ToString;

          LStream := TBytesStream.Create();
          try
            FWriter.WriteTo(FMethodResult, FWriterMediaType, LStream, Self);
            LStream.Position := 0;
            Response.ContentStream := LStream;
          except
            LStream.Free;
            raise;
          end;
        end
        // 3 - fallback (raw)
        else
        begin
          Response.ContentType := GetProducesValue;
          if Response.ContentType = '' then
            Response.ContentType := TMediaType.WILDCARD;
          if Assigned(FMethod.ReturnType) then
          begin
            if (FMethodResult.Kind in [tkString, tkUString, tkChar, {$ifdef DelphiXE7_UP}tkWideChar,{$endif} tkLString, tkWString])  then
              Response.Content := FMethodResult.AsString
            else if (FMethodResult.IsType<Boolean>) then
              Response.Content := BoolToStr(FMethodResult.AsType<Boolean>, True)
            else if FMethodResult.TypeInfo = TypeInfo(TDateTime) then
              Response.Content := DateToJSON(FMethodResult.AsType<TDateTime>)
            else if FMethodResult.TypeInfo = TypeInfo(TDate) then
              Response.Content := DateToJSON(FMethodResult.AsType<TDate>)
            else if FMethodResult.TypeInfo = TypeInfo(TTime) then
              Response.Content := DateToJSON(FMethodResult.AsType<TTime>)

            else if (FMethodResult.Kind in [tkInt64]) then
              Response.Content := IntToStr(FMethodResult.AsType<Int64>)
            else if (FMethodResult.Kind in [tkInteger]) then
              Response.Content := IntToStr(FMethodResult.AsType<Integer>)

            else if (FMethodResult.Kind in [tkFloat]) then
              Response.Content := FormatFloat('0.00000000', FMethodResult.AsType<Double>)
            else
              Response.Content := FMethodResult.ToString;
          end;

          Response.StatusCode := 200;
        end;
      finally
        FWriter := nil;
        FreeAndNil(FWriterMediaType);
      end;
    end;
  finally
    if not FMethod.HasAttribute<IsReference>(nil) then
      AddToContext(FMethodResult);
  end;
end;

procedure TMARSActivation.Prepare;
begin
  CheckResource;
  CheckMethod;
  ReadAuthorizationInfo;
  CheckAuthentication;
  CheckAuthorization;
  FillResourceMethodParameters;
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

    FMethod.ForEachAttribute<AuthorizationAttribute>(LProcessAuthorizationAttribute);
    FResource.ForEachAttribute<AuthorizationAttribute>(LProcessAuthorizationAttribute);

    FAuthorizationInfo.AllowedRoles := LAllowedRoles.ToStringArray;
  finally
    LAllowedRoles.Free;
  end;
end;

class procedure TMARSActivation.RegisterAfterInvoke(
  const AAfterInvoke: TMARSAfterInvokeProc);
begin
  SetLength(FAfterInvokeProcs, Length(FAfterInvokeProcs) + 1);
  FAfterInvokeProcs[Length(FAfterInvokeProcs)-1] := TMARSAfterInvokeProc(AAfterInvoke);
end;

class procedure TMARSActivation.RegisterBeforeInvoke(
  const ABeforeInvoke: TMARSBeforeInvokeProc);
begin
  SetLength(FBeforeInvokeProcs, Length(FBeforeInvokeProcs) + 1);
  FBeforeInvokeProcs[Length(FBeforeInvokeProcs)-1] := TMARSBeforeInvokeProc(ABeforeInvoke);
end;

procedure TMARSActivation.SetCustomHeaders;
var
  LCustomAtributeProcessor: TProc<CustomHeaderAttribute>;

begin
  LCustomAtributeProcessor :=
    procedure (ACustomHeader: CustomHeaderAttribute)
    begin
      Response.CustomHeaders.Values[ACustomHeader.HeaderName] := ACustomHeader.Value;
    end;
  FResource.ForEachAttribute<CustomHeaderAttribute>(LCustomAtributeProcessor);
  FMethod.ForEachAttribute<CustomHeaderAttribute>(LCustomAtributeProcessor);
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
begin
  Assert(Assigned(FConstructorInfo));
  Assert(Assigned(FMethod));

  try
    if DoBeforeInvoke then
    begin
      FInvocationTime := TStopwatch.StartNew;
      FResourceInstance := FConstructorInfo.ConstructorFunc();
      try
        ContextInjection;
        InvokeResourceMethod;
      finally
        FResourceInstance.Free;
      end;
      FInvocationTime.Stop;
      DoAfterInvoke;
    end;
  finally
    FreeContext;
  end;
end;

procedure TMARSActivation.CheckAuthentication;
begin
  if FAuthorizationInfo.NeedsAuthentication then
  begin
    if Token.IsVerified and Token.IsExpired then
    begin
      Token.Clear;
      Token.UpdateCookie;
      raise EMARSAuthenticationException.Create('Token expired', 403);
    end;
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
    raise EMARSApplicationException.Create(
      Format('[%s] No implementation found for http method %s'
      , [URL.Resource
{$ifndef Delphi10Seattle_UP}
         , GetEnumName(TypeInfo(TMethodType), Integer(Request.MethodType))
{$else}
         , TRttiEnumerationType.GetName<TMethodType>(Request.MethodType)
{$endif}
      ]), 404);
end;

procedure TMARSActivation.CheckResource;
begin
  if not Application.Resources.TryGetValue(URL.Resource.ToLower, FConstructorInfo) then
    raise EMARSApplicationException.Create(Format('Resource [%s] not found', [URL.Resource]), 404);
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
  const ARequest: TWebRequest; const AResponse: TWebResponse;
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
  FMethodArguments := [];
  FMethodResult := TValue.Empty;
  FResourceInstance := nil;
  FInvocationTime.Reset;
  Prepare;
end;

class function TMARSActivation.CreateActivation(const AEngine: TMARSEngine;
  const AApplication: TMARSApplication; const ARequest: TWebRequest;
  const AResponse: TWebResponse; const AURL: TMARSURL): IMARSActivation;
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
end;

function TMARSActivation.DoBeforeInvoke: Boolean;
var
  LSubscriber: TMARSBeforeInvokeProc;
begin
  Result := True;
  for LSubscriber in FBeforeInvokeProcs do
    LSubscriber(Self, Result);
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
