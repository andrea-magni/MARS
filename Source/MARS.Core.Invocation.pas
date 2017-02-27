(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Invocation;

{$I MARS.inc}

interface

uses
  SysUtils, Classes, Generics.Collections, Rtti, Diagnostics
  , HTTPApp

  , MARS.Core.URL
  , MARS.Core.Application
  , MARS.Core.Engine
  , MARS.Core.Token
  , MARS.Core.Registry
  , MARS.Core.MessageBodyWriter
  , MARS.Core.MediaType
  , MARS.Core.Injection.Types
  ;

type
  TMARSActivationRecord = class;

  TMARSBeforeInvokeProc = reference to procedure(const AActivationRecord: TMARSActivationRecord; out AIsAllowed: Boolean);
  TMARSAfterInvokeProc = reference to procedure(const AActivationRecord: TMARSActivationRecord);

  TMARSActivationRecord = class
  private
    FApplication: TMARSApplication;
    FEngine: TMARSEngine;
    FRequest: TWebRequest;
    FResponse: TWebResponse;
    FURL: TMARSURL;
    FURLPrototype: TMARSURL;
    FToken: TMARSToken;
    FMethodArgumentsToCollect: TList<TValue>;
    class var FBeforeInvokeProcs: TArray<TMARSBeforeInvokeProc>;
    class var FAfterInvokeProcs: TArray<TMARSAfterInvokeProc>;
  protected
    FRttiContext: TRttiContext;
    FConstructorInfo: TMARSConstructorInfo;
    FMethod: TRttiMethod;
    FResource: TRttiType;
    FResourceInstance: TObject;
    FMethodArguments: TArray<TValue>;
    FWriter: IMessageBodyWriter;
    FWriterMediaType: TMediaType;
    FInvocationTime: TStopWatch;

    procedure CleanupContext; virtual;
    procedure CollectContext(AValue: TValue); virtual;
    procedure CleanupGarbage(const AValue: TValue); virtual;
    procedure ContextInjection; virtual;
    function GetContextValue(const ADestination: TRttiObject): TInjectionValue; virtual;
    function GetMethodArgument(const AParam: TRttiParameter): TValue; virtual;
    function GetToken: TMARSToken;
    procedure FillResourceMethodParameters; virtual;
    procedure FindMethodToInvoke; virtual;
    procedure InvokeResourceMethod; virtual;
    procedure SetCustomHeaders;

    function DoBeforeInvoke: Boolean;
    procedure DoAfterInvoke;
  public
    constructor Create(const AEngine: TMARSEngine; const AApplication: TMARSApplication;
      const ARequest: TWebRequest; const AResponse: TWebResponse; const AURL: TMARSURL); virtual;
    destructor Destroy; override;

    procedure CheckResource; virtual;
    procedure CheckMethod; virtual;
    procedure CheckAuthentication; virtual;
    procedure CheckAuthorization; virtual;

    procedure Prepare; virtual;
    procedure Invoke; virtual;

    property Application: TMARSApplication read FApplication;
    property Engine: TMARSEngine read FEngine;
    property InvocationTime: TStopwatch read FInvocationTime;
    property Method: TRttiMethod read FMethod;
    property Request: TWebRequest read FRequest;
    property Resource: TRttiType read FResource;
//    property ResourceInstance: TObject read FResourceInstance;
    property Response: TWebResponse read FResponse;
    property URL: TMARSURL read FURL;
    property URLPrototype: TMARSURL read FURLPrototype;
    property Token: TMARSToken read GetToken;

    function HasToken: Boolean;

    class procedure RegisterBeforeInvoke(const ABeforeInvoke: TMARSBeforeInvokeProc);
//    class procedure UnregisterBeforeInvoke(const ABeforeInvoke: TMARSBeforeInvokeProc);
    class procedure RegisterAfterInvoke(const AAfterInvoke: TMARSAfterInvokeProc);
//    class procedure UnregisterAfterInvoke(const AAfterInvoke: TMARSAfterInvokeProc);
  end;

implementation

uses
    MARS.Core.Attributes
  , MARS.Core.Response
  , MARS.Core.Classes
  , MARS.Core.MessageBodyReader
  , MARS.Core.Exceptions
  , MARS.Utils.Parameters
  , MARS.Rtti.Utils
  , MARS.Core.Injection
  , MARS.Core.Invocation.InjectionService
{$ifndef Delphi10Seattle_UP}
  , TypInfo
{$endif}
;

{ TMARSActivationRecord }

function TMARSActivationRecord.GetMethodArgument(const AParam: TRttiParameter): TValue;
var
  LParamValue: TValue;
begin
  LParamValue := TValue.Empty;

  AParam.HasAttribute<ContextAttribute>(
    procedure (AContextAttr: ContextAttribute)
    begin
      LParamValue := GetContextValue(AParam).Value;
    end
  );

  Result := LParamValue;
end;

function TMARSActivationRecord.GetToken: TMARSToken;
begin
  if not Assigned(FToken) then
    FToken := GetContextValue(FRttiContext.GetType(Self.ClassType).GetField('FToken')).Value.AsType<TMARSToken>;
  Result := FToken;
end;

function TMARSActivationRecord.HasToken: Boolean;
begin
  Result := Assigned(FToken);
end;

procedure TMARSActivationRecord.FillResourceMethodParameters;
var
  LParameters: TArray<TRttiParameter>;
  LIndex: Integer;
begin
  Assert(Assigned(FMethod));
  try
    LParameters := FMethod.GetParameters;
    SetLength(FMethodArguments, Length(LParameters));
    for LIndex := Low(LParameters) to High(LParameters) do
      FMethodArguments[LIndex] := GetMethodArgument(LParameters[LIndex]);
  except
    on E: Exception do
      raise EMARSApplicationException.Create('Bad parameter values for resource method ' + FMethod.Name);
  end;
end;

procedure TMARSActivationRecord.FindMethodToInvoke;
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

procedure TMARSActivationRecord.CleanupGarbage(const AValue: TValue);
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
          tkInterface: TObject(LValue.AsInterface).Free;
          tkArray, tkDynArray: CleanupGarbage(LValue); //recursion
        end;
      end;
    end;
  end;
end;

procedure TMARSActivationRecord.CleanupContext;
begin
  while FMethodArgumentsToCollect.Count > 0 do
  begin
    CleanupGarbage(FMethodArgumentsToCollect[0]);
    FMethodArgumentsToCollect.Delete(0);
  end;
end;

procedure TMARSActivationRecord.InvokeResourceMethod();
var
  LMethodResult: TValue;
  LStream: TBytesStream;
  LContentType: string;
begin
  Assert(Assigned(FMethod));

    // cache initial ContentType value to check later if it has been changed
    LContentType := Response.ContentType;

    FillResourceMethodParameters;
    try
      LMethodResult := FMethod.Invoke(FResourceInstance, FMethodArguments);

      // handle response
      SetCustomHeaders;

      // 1 - TMARSResponse (override)
      if LMethodResult.IsInstanceOf(TMARSResponse) then
        TMARSResponse(LMethodResult.AsObject).CopyTo(Response)
      // 2 - MessageBodyWriter mechanism (standard)
      else begin
        TMARSMessageBodyRegistry.Instance.FindWriter(FMethod, string(Request.Accept)
          , FWriter, FWriterMediaType);
        try
          if Assigned(FWriter) then
          begin
            if Response.ContentType = LContentType then
              Response.ContentType := FWriterMediaType.ToString;

            LStream := TBytesStream.Create();
            try
              FWriter.WriteTo(LMethodResult, FMethod.GetAttributes, FWriterMediaType
                , Response.CustomHeaders, LStream);
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
            case LMethodResult.Kind of

              tkString, tkLString, tkUString, tkWString
    {$ifdef DelphiXE8_UP}
              , tkWideChar, tkAnsiChar
    {$endif}
              , tkInteger, tkInt64, tkFloat, tkVariant:
              begin
                Response.Content := LMethodResult.AsString;
                if (Response.ContentType = LContentType) then
                  Response.ContentType := TMediaType.TEXT_PLAIN; // or check Produces of method!
                Response.StatusCode := 200;
              end;

              // a procedure has Kind = tkUnknown
              tkUnknown : Response.StatusCode := 200;

              //tkRecord: ;
              //tkInterface: ;
              //tkDynArray: ;
              else
                Response.StatusCode := 400;
            end;
          end;
        finally
          FWriter := nil;
          FreeAndNil(FWriterMediaType);
        end;
      end;
    finally
      CleanupContext;

      if not FMethod.HasAttribute<IsReference>(nil) then
        CleanupGarbage(LMethodResult);
    end;
end;

procedure TMARSActivationRecord.Prepare;
begin
  CheckResource;
  CheckMethod;
  CheckAuthentication;
  CheckAuthorization;
end;

class procedure TMARSActivationRecord.RegisterAfterInvoke(
  const AAfterInvoke: TMARSAfterInvokeProc);
begin
  FAfterInvokeProcs := FAfterInvokeProcs + [TMARSAfterInvokeProc(AAfterInvoke)];
end;

class procedure TMARSActivationRecord.RegisterBeforeInvoke(
  const ABeforeInvoke: TMARSBeforeInvokeProc);
begin
  FBeforeInvokeProcs := FBeforeInvokeProcs + [TMARSBeforeInvokeProc(ABeforeInvoke)];
end;

procedure TMARSActivationRecord.SetCustomHeaders;
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

//class procedure TMARSActivationRecord.UnregisterAfterInvoke(
//  const AAfterInvoke: TMARSAfterInvokeProc);
//begin
//  FAfterInvokeProcs := FAfterInvokeProcs - [TMARSAfterInvokeProc(AAfterInvoke)];
//end;
//
//class procedure TMARSActivationRecord.UnregisterBeforeInvoke(
//  const ABeforeInvoke: TMARSBeforeInvokeProc);
//begin
//  FBeforeInvokeProcs := FBeforeInvokeProcs - [TMARSBeforeInvokeProc(ABeforeInvoke)];
//end;

procedure TMARSActivationRecord.Invoke;
begin
  Assert(Assigned(FConstructorInfo));
  Assert(Assigned(FMethod));

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
end;

procedure TMARSActivationRecord.CheckAuthentication;
begin
  if Token.IsVerified and Token.IsExpired then
  begin
    Token.Clear;
    Token.UpdateCookie;
    raise EMARSAuthenticationException.Create('Token expired', 403);
  end;
end;

procedure TMARSActivationRecord.CheckAuthorization;
var
  LDenyAll, LPermitAll: Boolean;
  LAllowedRoles: TStringList;
  LProcessAuthorizationAttribute: TProc<AuthorizationAttribute>;
begin
  LDenyAll := False;
  LPermitAll := False;
  LAllowedRoles := TStringList.Create;
  try
    LAllowedRoles.Sorted := True;
    LAllowedRoles.Duplicates := TDuplicates.dupIgnore;

    LProcessAuthorizationAttribute :=
      procedure (AAttribute: AuthorizationAttribute)
      begin
        if AAttribute is DenyAllAttribute then
          LDenyAll := True
        else if AAttribute is PermitAllAttribute then
          LPermitAll := True
        else if AAttribute is RolesAllowedAttribute then
          LAllowedRoles.AddStrings(RolesAllowedAttribute(AAttribute).Roles);
      end;

    FMethod.ForEachAttribute<AuthorizationAttribute>(LProcessAuthorizationAttribute);
    FResource.ForEachAttribute<AuthorizationAttribute>(LProcessAuthorizationAttribute);

    if LDenyAll // DenyAll (stronger than PermitAll and Roles-based authorization)
       or (
         not LPermitAll  // PermitAll (stronger than Role-based authorization)
         and ((LAllowedRoles.Count > 0) and (not Token.HasRole(LAllowedRoles)))
       ) then
      raise EMARSAuthorizationException.Create('Forbidden', 403);
  finally
    LAllowedRoles.Free;
  end;
end;

procedure TMARSActivationRecord.CheckMethod;
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

procedure TMARSActivationRecord.CheckResource;
begin
  if not Application.Resources.TryGetValue(URL.Resource, FConstructorInfo) then
    raise EMARSApplicationException.Create(Format('Resource [%s] not found', [URL.Resource]), 404);
end;

procedure TMARSActivationRecord.CollectContext(AValue: TValue);
begin
  if not AValue.IsEmpty then
    FMethodArgumentsToCollect.Add(AValue);
end;

procedure TMARSActivationRecord.ContextInjection();
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

function TMARSActivationRecord.GetContextValue(const ADestination: TRttiObject): TInjectionValue;
begin
  Result := TMARSInjectionServiceRegistry.Instance.GetValue(ADestination, Self);
  if not Result.IsReference then
    CollectContext(Result.Value);
end;


constructor TMARSActivationRecord.Create(const AEngine: TMARSEngine;
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
  FMethodArgumentsToCollect := TList<TValue>.Create;
  FInvocationTime.Reset;
  Prepare;
end;

destructor TMARSActivationRecord.Destroy;
begin
  FreeAndNil(FURLPrototype);
  FMethodArgumentsToCollect.Free;
  inherited;
end;

procedure TMARSActivationRecord.DoAfterInvoke;
var
  LSubscriber: TMARSAfterInvokeProc;
begin
  for LSubscriber in FAfterInvokeProcs do
    LSubscriber(Self);
end;

function TMARSActivationRecord.DoBeforeInvoke: Boolean;
var
  LSubscriber: TMARSBeforeInvokeProc;
begin
  Result := True;
  for LSubscriber in FBeforeInvokeProcs do
    LSubscriber(Self, Result);
end;

end.
