(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Invocation;

{$I MARS.inc}

interface

uses
  SysUtils, Classes
  , HTTPApp
  , Generics.Collections
  , Rtti
  , MARS.Core.URL
  , MARS.Core.Application
  , MARS.Core.Engine
  , MARS.Core.Attributes
  , MARS.Core.Token
  , MARS.Core.Registry
  , MARS.Core.MessageBodyWriter
  , MARS.Core.MediaType
  ;

type
  TMARSActivationRecord = class
  private
    FApplication: TMARSApplication;
    FRequest: TWebRequest;
    FResponse: TWebResponse;
    FURL: TMARSURL;
    FToken: TMARSToken;
    FMethodArgumentsToCollect: TList<TValue>;
  protected
    FRttiContext: TRttiContext;
    FConstructorInfo: TMARSConstructorInfo;
    FMethod: TRttiMethod;
    FResource: TRttiType;
    FResourceInstance: TObject;
    FMethodArguments: TArray<TValue>;
    FWriter: IMessageBodyWriter;
    FWriterMediaType: TMediaType;

    procedure CleanupMethodArguments; virtual;
    procedure CollectGarbage(const AValue: TValue); virtual;
    procedure ContextInjection; virtual;
    function ContextInjectionByType(const AType: TRttiType; out AValue: TValue): Boolean; virtual;
    function GetEngine: TMARSEngine; virtual;
    function GetMethodArgument(const AParam: TRttiParameter): TValue; virtual;
    procedure FillResourceMethodParameters; virtual;
    procedure FindMethodToInvoke; virtual;
    procedure InvokeResourceMethod; virtual;
    function ParamNameToParamIndex(const AParamName: string): Integer; virtual;
    procedure SetCustomHeaders;
  public
    constructor Create(const AApplication: TMARSApplication;
      ARequest: TWebRequest; AResponse: TWebResponse; const AURL: TMARSURL); virtual;
    destructor Destroy; override;

    procedure CheckResource; virtual;
    procedure CheckMethod; virtual;
    procedure CheckAuthentication; virtual;
    procedure CheckAuthorization; virtual;

    procedure Invoke; virtual;

    property Application: TMARSApplication read FApplication;
    property Engine: TMARSEngine read GetEngine;
    property Request: TWebRequest read FRequest;
    property Response: TWebResponse read FResponse;
    property URL: TMARSURL read FURL;
    property Token: TMARSToken read FToken;
  end;

implementation

uses
    MARS.Core.Response
  , MARS.Core.Classes
  , MARS.Core.MessageBodyReader
  , MARS.Core.Exceptions
  , MARS.Utils.Parameters
  , MARS.Rtti.Utils
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

  // context injection
  AParam.HasAttribute<ContextAttribute>(
    procedure (AContextAttr: ContextAttribute)
    begin
      ContextInjectionByType(AParam.ParamType, LParamValue);
    end
  );

  // fill parameter with content from request (params, body, ...)
  AParam.HasAttribute<RequestParamAttribute>(
    procedure (ARequestParamAttr: RequestParamAttribute)
    begin
      LParamValue := ARequestParamAttr.GetValue(Request, AParam, Engine.BasePath, Application.BasePath);
      if not (LParamValue.IsEmpty or AParam.HasAttribute<IsReference>) then
        FMethodArgumentsToCollect.Add(LParamValue);
    end
  );

  Result := LParamValue;
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
  LPrototypeURL: TMARSURL;
  LPathMatches: Boolean;
  LHttpMethodMatches: Boolean;
  LMethodPath: string;
begin
  FResource := FRttiContext.GetType(FConstructorInfo.TypeTClass);
  FMethod := nil;

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
      LPrototypeURL := TMARSURL.CreateDummy([Engine.BasePath, Application.BasePath, LResourcePath, LMethodPath]);
      try
        LPathMatches := LPrototypeURL.MatchPath(URL);
      finally
        LPrototypeURL.Free;
      end;

      if LPathMatches and LHttpMethodMatches then
      begin
        FMethod := LMethod;
        Break;
      end;
    end;
  end;
end;


function TMARSActivationRecord.GetEngine: TMARSEngine;
begin
  Assert(Assigned(Application));
  Result := TMARSEngine(Application.Engine);
end;

procedure TMARSActivationRecord.CollectGarbage(const AValue: TValue);
var
  LIndex: Integer;
  LValue: TValue;
begin
  case AValue.Kind of
    tkClass: AValue.AsObject.Free;
//    tkInterface: TObject(AValue.AsInterface).Free;

    tkArray,
    tkDynArray:
    begin
      for LIndex := 0 to AValue.GetArrayLength -1 do
      begin
        LValue := AValue.GetArrayElement(LIndex);
        case LValue.Kind of
          tkClass: LValue.AsObject.Free;
          tkInterface: TObject(LValue.AsInterface).Free;
          tkArray, tkDynArray: CollectGarbage(LValue); //recursion
        end;
      end;
    end;
  end;
end;

procedure TMARSActivationRecord.CleanupMethodArguments;
begin
  while FMethodArgumentsToCollect.Count > 0 do
  begin
    CollectGarbage(FMethodArgumentsToCollect[0]);
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

  TMARSMessageBodyRegistry.Instance.FindWriter(FMethod, string(Request.Accept)
    , FWriter, FWriterMediaType);
  try

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
      else if Assigned(FWriter) then
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
      CleanupMethodArguments;

      if not FMethod.HasAttribute<IsReference>(nil) then
        CollectGarbage(LMethodResult);
    end;
  finally
    FWriter := nil;
    FreeAndNil(FWriterMediaType);
  end;
end;


function TMARSActivationRecord.ParamNameToParamIndex(const AParamName: string): Integer;
var
  LParamIndex: Integer;
  LSubResourcePath: string;
begin
  LParamIndex := -1;

  LSubResourcePath := '';
  FMethod.HasAttribute<PathAttribute>(
    procedure (ASubResourcePathAttrib: PathAttribute)
    begin
      LSubResourcePath := ASubResourcePathAttrib.Value;
    end
  );

  FRttiContext.GetType(FResourceInstance.ClassType).HasAttribute<PathAttribute>(
    procedure (AResourcePathAttrib: PathAttribute)
    var
      LResURL: TMARSURL;
      LPair: TPair<Integer, string>;
    begin
      LResURL := TMARSURL.CreateDummy([Engine.BasePath, Application.BasePath
        , AResourcePathAttrib.Value, LSubResourcePath]);
      try
        LParamIndex := -1;
        for LPair in LResURL.PathParams do
        begin
          if SameText(AParamName, LPair.Value) then
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

procedure TMARSActivationRecord.Invoke;
begin
  Assert(Assigned(FConstructorInfo));
  Assert(Assigned(FMethod));

  FResourceInstance := FConstructorInfo.ConstructorFunc();
  try
    ContextInjection;
    InvokeResourceMethod;
  finally
    FResourceInstance.Free;
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

procedure TMARSActivationRecord.ContextInjection();
var
  LType: TRttiType;
begin
  LType := FRttiContext.GetType(FResourceInstance.ClassType);

  // fields
  LType.ForEachFieldWithAttribute<ContextAttribute>(
    function (AField: TRttiField; AAttrib: ContextAttribute): Boolean
    var
      LValue: TValue;
    begin
      Result := True; // enumerate all
      if ContextInjectionByType(AField.FieldType, LValue) then
        AField.SetValue(FResourceInstance, LValue);
    end
  );

  // properties
  LType.ForEachPropertyWithAttribute<ContextAttribute>(
    function (AProperty: TRttiProperty; AAttrib: ContextAttribute): Boolean
    var
      LValue: TValue;
    begin
      Result := True;
      if ContextInjectionByType(AProperty.PropertyType, LValue) then
        AProperty.SetValue(FResourceInstance, LValue);
    end
  );
end;

function TMARSActivationRecord.ContextInjectionByType(const AType: TRttiType;
  out AValue: TValue): Boolean;
begin
  Result := True;
  if (AType.IsObjectOfType(TMARSToken)) then
    AValue := Token
  else if (AType.IsObjectOfType(TWebRequest)) then
    AValue := Request
  else if (AType.IsObjectOfType(TWebResponse)) then
    AValue := Response
  else if (AType.IsObjectOfType(TMARSURL)) then
    AValue := URL
  else if (AType.IsObjectOfType(TMARSEngine)) then
    AValue := Engine
  else if (AType.IsObjectOfType(TMARSApplication)) then
    AValue := Application
  else
    Result := False;
end;


constructor TMARSActivationRecord.Create(const AApplication: TMARSApplication;
  ARequest: TWebRequest; AResponse: TWebResponse; const AURL: TMARSURL);
begin
  inherited Create;
  FApplication := AApplication;
  FRequest := ARequest;
  FResponse := AResponse;
  FURL := AURL;
  FToken := TMARSToken.Create(FRequest, FResponse, FApplication.Parameters, FURL);
  FRttiContext := TRttiContext.Create;
  FMethodArgumentsToCollect := TList<TValue>.Create;
end;

destructor TMARSActivationRecord.Destroy;
begin
  FMethodArgumentsToCollect.Free;
  FToken.Free;
  inherited;
end;

end.
