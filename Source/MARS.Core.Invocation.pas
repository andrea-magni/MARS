(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
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
    FResourceInstance: TObject;
    FWriter: IMessageBodyWriter;

    procedure CleanupMethodArguments;
    procedure CollectGarbage(const AValue: TValue); virtual;
    procedure ContextInjection; virtual;
    function ContextInjectionByType(const AType: TClass; out AValue: TValue): Boolean; virtual;
    function FillAnnotatedParam(const AParam: TRttiParameter): TValue;
    procedure FillResourceMethodParameters(var AArgumentArray: TArray<TValue>);
    function FindMethodToInvoke: TRttiMethod; virtual;
    procedure InvokeResourceMethod(const AWriter: IMessageBodyWriter; AMediaType: TMediaType); virtual;
    function ParamNameToParamIndex(AResourceInstance: TObject; const AParamName: string; AMethod: TRttiMethod): Integer;
  public
    constructor Create(const AApplication: TMARSApplication;
      ARequest: TWebRequest; AResponse: TWebResponse; const AURL: TMARSURL;
      const AToken: TMARSToken); virtual;
    destructor Destroy; override;

    procedure CheckResource; virtual;
    procedure CheckMethod; virtual;
    procedure CheckAuthorization; virtual;

    procedure Invoke; virtual;

    property Application: TMARSApplication read FApplication;
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
  , MARS.Core.Engine
  ;

{ TMARSActivationRecord }

function StringToTValue(const AString: string; const ATypeKind: TTypeKind): TValue;
begin
  Result := TValue.Empty;
  // type conversions
  case ATypeKind of
    tkInt64,
    tkInteger: Result := TValue.From(StrToInt(AString));

    tkFloat: Result := TValue.From<Double>(StrToFloat(AString));

    tkChar: Result := TValue.From(AnsiChar(AString.Chars[0]));

    tkLString,
    tkUString,
    tkWString,
    tkString: Result := TValue.From(AString);

    tkVariant: Result := TValue.From(AString);

    // not yet supported
//      tkWChar: ;
//      tkEnumeration: ;
//      tkSet: ;
//      tkClass: ;
//      tkMethod: ;
//      tkArray: ;
//      tkRecord: ;
//      tkInterface: ;
//      tkDynArray: ;
//      tkClassRef: ;
//      tkPointer: ;
//      tkProcedure: ;
  end;
end;

function TMARSActivationRecord.FillAnnotatedParam(const AParam: TRttiParameter): TValue;
var
  LAttributes: TArray<TCustomAttribute>;
  LAttribute: TCustomAttribute;
  LParamName, LParamValue: string;
  LParamIndex: Integer;
  LParamClassType: TClass;
  LContextValue: TValue;
  LMediaType: TMediaType;
  LReader: IMessageBodyReader;
  LReaderResult: TValue;
  LReaderFound: Boolean;
  LFreeWhenDone: Boolean;
begin
  Result := TValue.Empty;

  LAttributes := AParam.GetAttributes;
  case Length(LAttributes) of
    0: Exit;
    1: LAttribute := LAttributes[0];
    else
      raise EMARSException.Create('Only 1 attribute permitted');
  end;

  LParamName := '';
  LParamValue := '';
  LReaderFound := False;

  // context injection
  if (LAttribute is ContextAttribute) and (AParam.ParamType.IsInstance) then
  begin
    LParamClassType := TRttiInstanceType(AParam.ParamType).MetaclassType;
    if ContextInjectionByType(LParamClassType, LContextValue) then
      Result := LContextValue;
  end
  // fill with content from request (params, body, ...)
  else if LAttribute is MethodParamAttribute then
  begin
    LParamName := MethodParamAttribute(LAttribute).Value;
    if LParamName = '' then
      LParamName := AParam.Name;

    if LAttribute is PathParamAttribute then // resource/value1
    begin
      LParamIndex := ParamNameToParamIndex(FResourceInstance, LParamName, FMethod);
      LParamValue := URL.PathTokens[LParamIndex];
    end
    else if LAttribute is QueryParamAttribute then  // ?param1=value1
      LParamValue := Request.QueryFields.Values[LParamName]
    else if LAttribute is FormParamAttribute then   // forms
      LParamValue := Request.ContentFields.Values[LParamName]
    else if LAttribute is CookieParamAttribute then // cookies
      LParamValue := Request.CookieFields.Values[LParamName]
    else if LAttribute is HeaderParamAttribute then // http headers
      LParamValue := string(Request.GetFieldByName(LParamName))
    else if LAttribute is BodyParamAttribute then   // body content
    begin
      TMARSMessageBodyReaderRegistry.Instance.FindReader(FMethod, AParam, LReader, LMediaType);
      try
        if Assigned(LReader) then
        begin
          LReaderFound := True;
          LFreeWhenDone := True;
          LReaderResult := LReader.ReadFrom(Request.RawContent, FMethod.GetAttributes, LMediaType, nil, LFreeWhenDone);
          if LFreeWhenDone then
            FMethodArgumentsToCollect.Add(LReaderResult);
        end
        else
          LParamValue := Request.Content;
      finally
        FreeAndNil(LMediaType);
      end;
    end;

    if LReaderFound then
      Result := LReaderResult
    else
      Result := StringToTValue(LParamValue, AParam.ParamType.TypeKind);
  end;
end;

procedure TMARSActivationRecord.FillResourceMethodParameters(
  var AArgumentArray: TArray<TValue>);
var
  LParamArray: TArray<TRttiParameter>;
  LIndex: Integer;
begin
  Assert(Assigned(FMethod));
  try
    LParamArray := FMethod.GetParameters;

    SetLength(AArgumentArray, Length(LParamArray));
    for LIndex := Low(LParamArray) to High(LParamArray) do
      AArgumentArray[LIndex] := FillAnnotatedParam(LParamArray[LIndex]);

  except
    on E: Exception do
      raise EMARSApplicationException.Create('Bad parameter values for resource method ' + FMethod.Name);
  end;
end;

function TMARSActivationRecord.FindMethodToInvoke: TRttiMethod;
var
  LResourceType: TRttiType;
  LMethod: TRttiMethod;
  LResourcePath: string;
  LAttribute: TCustomAttribute;
  LPrototypeURL: TMARSURL;
  LPathMatches: Boolean;
  LHttpMethodMatches: Boolean;
  LMethodPath: string;
begin
  LResourceType := FRttiContext.GetType(FConstructorInfo.TypeTClass);
  Result := nil;
  LResourcePath := '';

  LResourceType.HasAttribute<PathAttribute>(
    procedure (APathAttribute: PathAttribute)
    begin
      LResourcePath := APathAttribute.Value;
    end
  );

  for LMethod in LResourceType.GetMethods do
  begin
    LMethodPath := '';
    LHttpMethodMatches := False;

    for LAttribute in LMethod.GetAttributes do
    begin
      if LAttribute is PathAttribute then
        LMethodPath := PathAttribute(LAttribute).Value;

      if LAttribute is HttpMethodAttribute then
        LHttpMethodMatches := HttpMethodAttribute(LAttribute).Matches(Request);
    end;

    if LHttpMethodMatches then
    begin
      LPrototypeURL := TMARSURL.CreateDummy([TMARSEngine(Application.Engine).BasePath, Application.BasePath, LResourcePath, LMethodPath]);
      try
        LPathMatches := LPrototypeURL.MatchPath(URL);
      finally
        LPrototypeURL.Free;
      end;

      if LPathMatches and LHttpMethodMatches then
      begin
        Result := LMethod;
        Break;
      end;
    end;
  end;
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
var
  LArgument: TValue;
begin
  while FMethodArgumentsToCollect.Count > 0 do
  begin
    LArgument := FMethodArgumentsToCollect[0];
    CollectGarbage(LArgument);
    FMethodArgumentsToCollect.Delete(0);
  end;
end;

procedure TMARSActivationRecord.InvokeResourceMethod(
  const AWriter: IMessageBodyWriter; AMediaType: TMediaType);
var
  LMethodResult: TValue;
  LArgumentArray: TArray<TValue>;
  LMARSResponse: TMARSResponse;
  LStream: TBytesStream;
  LContentType: string;
  LCustomAtributeProcessor: TProc<CustomHeaderAttribute>;
begin
  // The returned object MUST be initially nil (needs to be consistent with the Free method)
  LMethodResult := nil;
  try
    LContentType := Response.ContentType;

    FillResourceMethodParameters(LArgumentArray);
    try
      LMethodResult := FMethod.Invoke(FResourceInstance, LArgumentArray);
    finally
      CleanupMethodArguments;
    end;

    LCustomAtributeProcessor :=
      procedure (ACustomHeader: CustomHeaderAttribute)
      begin
        Response.CustomHeaders.Values[ACustomHeader.HeaderName] := ACustomHeader.Value;
      end;
    FMethod.Parent.ForEachAttribute<CustomHeaderAttribute>(LCustomAtributeProcessor);
    FMethod.ForEachAttribute<CustomHeaderAttribute>(LCustomAtributeProcessor);


    if LMethodResult.IsInstanceOf(TMARSResponse) then // Response Object
    begin
      LMARSResponse := TMARSResponse(LMethodResult.AsObject);
      LMARSResponse.CopyTo(Response);
    end
    else if Assigned(AWriter) then // MessageBodyWriters mechanism
    begin
      {
        If the ContentType has been already changed (i.e., using the Context DI mechanism),
        we should not override the new value.
        This means the application developer has more power than the MBW developer in order
        to determine the ContentType.

        Andrea Magni, 19/11/2015
      }
      if Response.ContentType = LContentType then
        Response.ContentType := AMediaType.ToString;

      LStream := TBytesStream.Create();
      try
        AWriter.WriteTo(LMethodResult, FMethod.GetAttributes, AMediaType
          , Response.CustomHeaders, LStream);
        Response.ContentStream := LStream;
      except
        LStream.Free;
        raise;
      end;
    end
    else // fallback (no MBW, no TMARSResponse)
    begin
      // handle result
      case LMethodResult.Kind of

        tkString, tkLString, tkUString, tkWString // string types
        , tkInteger, tkInt64, tkFloat, tkVariant: // Threated as string, nothing more
        begin
          Response.Content := LMethodResult.AsString;
          if (Response.ContentType = LContentType) then
            Response.ContentType := TMediaType.TEXT_PLAIN; // or check Produces of method!
          Response.StatusCode := 200;
        end;

        tkUnknown : Response.StatusCode := 200; // it's a procedure, not a function!

        //tkRecord: ;
        //tkInterface: ;
        //tkDynArray: ;
        else
          Response.StatusCode := 400;
      end;
    end;
  finally
    if not FMethod.HasAttribute<ResultIsReference>(nil) then
      CollectGarbage(LMethodResult);
  end;
end;


function TMARSActivationRecord.ParamNameToParamIndex(AResourceInstance: TObject;
  const AParamName: string; AMethod: TRttiMethod): Integer;
var
  LParamIndex: Integer;
  LAttrib: TCustomAttribute;
  LSubResourcePath: string;
begin
  LParamIndex := -1;

  LSubResourcePath := '';
  for LAttrib in AMethod.GetAttributes do
  begin
    if LAttrib is PathAttribute then
    begin
      LSubResourcePath := PathAttribute(LAttrib).Value;
      Break;
    end;
  end;

  FRttiContext.GetType(AResourceInstance.ClassType).HasAttribute<PathAttribute>(
  procedure (AResourcePathAttrib: PathAttribute)
  var
    LResURL: TMARSURL;
    LPair: TPair<Integer, string>;
  begin
    LResURL := TMARSURL.CreateDummy([TMARSEngine(Application.Engine).BasePath
      , Application.BasePath, AResourcePathAttrib.Value, LSubResourcePath]);
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
  end);

  Result := LParamIndex;
end;

procedure TMARSActivationRecord.Invoke;
var
  LMediaType: TMediaType;
begin
  Assert(Assigned(FConstructorInfo));
  Assert(Assigned(FMethod));

  FResourceInstance := FConstructorInfo.ConstructorFunc();
  try
    TMARSMessageBodyRegistry.Instance.FindWriter(FMethod, string(Request.Accept)
      , FWriter, LMediaType);

    ContextInjection;
    try
      InvokeResourceMethod(FWriter, LMediaType);
    finally
      FWriter := nil;
      FreeAndNil(LMediaType);
    end;

  finally
    FResourceInstance.Free;
  end;
end;

procedure TMARSActivationRecord.CheckAuthorization;
var
  LDenyAll, LPermitAll, LRolesAllowed: Boolean;
  LAllowedRoles: TStringList;
  LAllowed: Boolean;
  LRole: string;
  LProcessAuthorizationAttribute: TProc<AuthorizationAttribute>;
begin
  LAllowed := True; // Default = True for non annotated-methods
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
        begin
          LRolesAllowed := True;
          LAllowedRoles.AddStrings(RolesAllowedAttribute(AAttribute).Roles);
        end;
      end;

    FMethod.ForEachAttribute<AuthorizationAttribute>(LProcessAuthorizationAttribute);
    // also check the class (resource) of the method
    FMethod.Parent.ForEachAttribute<AuthorizationAttribute>(LProcessAuthorizationAttribute);

    if LDenyAll then
      LAllowed := False
    else
    begin
      if LRolesAllowed then
      begin
        LAllowed := False;
        for LRole in LAllowedRoles do
        begin
          LAllowed := Token.HasRole(LRole);
          if LAllowed then
            Break;
        end;
      end;

      if LPermitAll then
        LAllowed := True;
    end;

    if not LAllowed then
      raise EMARSAuthorizationException.Create('Forbidden', 403);

  finally
    LAllowedRoles.Free;
  end;
end;

procedure TMARSActivationRecord.CheckMethod;
begin
  FMethod := FindMethodToInvoke;

  if not Assigned(FMethod) then
    raise EMARSApplicationException.Create(
      Format('[%s] No implementation found for http method %s'
      , [URL.Resource, TRttiEnumerationType.GetName<TMethodType>(Request.MethodType)]), 404);
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
  // Context injection
  LType.ForEachFieldWithAttribute<ContextAttribute>(
    function (AField: TRttiField; AAttrib: ContextAttribute): Boolean
    var
      LFieldClassType: TClass;
      LValue: TValue;
    begin
      Result := True; // enumerate all
      if (AField.FieldType.IsInstance) then
      begin
        LFieldClassType := TRttiInstanceType(AField.FieldType).MetaclassType;

        if ContextInjectionByType(LFieldClassType, LValue) then
          AField.SetValue(FResourceInstance, LValue);
      end;
    end
  );

  // properties
  LType.ForEachPropertyWithAttribute<ContextAttribute>(
    function (AProperty: TRttiProperty; AAttrib: ContextAttribute): Boolean
    var
      LPropertyClassType: TClass;
      LValue: TValue;
    begin
      Result := True; // enumerate all
      if (AProperty.PropertyType.IsInstance) then
      begin
        LPropertyClassType := TRttiInstanceType(AProperty.PropertyType).MetaclassType;
        if ContextInjectionByType(LPropertyClassType, LValue) then
          AProperty.SetValue(FResourceInstance, LValue);
      end;
    end
  );
end;

function TMARSActivationRecord.ContextInjectionByType(const AType: TClass;
  out AValue: TValue): Boolean;
begin
  Result := True;
  if (AType.InheritsFrom(TMARSToken)) then
    AValue := Token
  else if (AType.InheritsFrom(TWebRequest)) then
    AValue := Request
  else if (AType.InheritsFrom(TWebResponse)) then
    AValue := Response
  else if (AType.InheritsFrom(TMARSURL)) then
    AValue := URL
  else if (AType.InheritsFrom(TMARSEngine)) then
    AValue := Application.Engine
  else if (AType.InheritsFrom(TMARSApplication)) then
    AValue := Application
  else
    Result := False;
end;


constructor TMARSActivationRecord.Create(const AApplication: TMARSApplication;
  ARequest: TWebRequest; AResponse: TWebResponse; const AURL: TMARSURL;
  const AToken: TMARSToken);
begin
  inherited Create;
  FApplication := AApplication;
  FRequest := ARequest;
  FResponse := AResponse;
  FURL := AURL;
  FToken := AToken;
  FRttiContext := TRttiContext.Create;
  FMethodArgumentsToCollect := TList<TValue>.Create;
end;

destructor TMARSActivationRecord.Destroy;
begin
  FMethodArgumentsToCollect.Free;
  inherited;
end;

end.
