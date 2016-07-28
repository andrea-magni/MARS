(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
  Home: https://github.com/andrea-magni/MARS

*)
unit MARS.Core.Application;

{$I MARS.inc}

interface

// JAX-RS Objects Life-Cycle
// https://jersey.java.net/documentation/latest/deployment.html
// https://jersey.java.net/documentation/latest/jaxrs-resources.html
// http://www.mkyong.com/webservices/jax-rs/jax-rs-path-uri-matching-example/

uses
  SysUtils
  , Classes
  , HTTPApp
  , Rtti
  , Generics.Collections
  , MARS.Core.Classes
  , MARS.Core.URL
  , MARS.Core.MessageBodyReader
  , MARS.Core.MessageBodyWriter
  , MARS.Core.Registry
  , MARS.Core.MediaType
  , MARS.Core.Token
  , MARS.Core.Exceptions
  , MARS.Utils.Parameters
  ;

type
  EMARSApplicationException = class(EMARSHttpException);
  EMARSAuthorizationException = class(EMARSApplicationException);

  TAttributeArray = TArray<TCustomAttribute>;
  TArgumentArray = array of TValue;

  TMARSApplication = class
  private
    FRttiContext: TRttiContext;
    FResourceRegistry: TObjectDictionary<string, TMARSConstructorInfo>;
    FBasePath: string;
    FName: string;
    FEngine: TObject;
    FSystem: Boolean;
    FParameters: TMARSParameters;
    function GetResources: TArray<string>;
    function GetRequest: TWebRequest;
    function GetResponse: TWebResponse;
    function GetURL: TMARSURL;
    function GetToken: TMARSToken;
  protected
    function FindMethodToInvoke(const AURL: TMARSURL;
      const AInfo: TMARSConstructorInfo): TRttiMethod; virtual;

    function FillAnnotatedParam(const AParam: TRttiParameter;
      const AResourceInstance: TObject; const AMethod: TRttiMethod): TValue;
    procedure FillResourceMethodParameters(AInstance: TObject; AMethod: TRttiMethod; var AArgumentArray: TArgumentArray);

    /// <summary>
    ///   Invoke the resource's method filling it's parameters (if any)
    /// </summary>
    /// <returns>
    ///   Return the object to be freed after the function goes out of scope
    /// </returns>
    procedure InvokeResourceMethod(AInstance: TObject; AMethod: TRttiMethod;
      const AWriter: IMessageBodyWriter; ARequest: TWebRequest;
      AMediaType: TMediaType); virtual;

    procedure CheckAuthorization(const AMethod: TRttiMethod; const AToken: TMARSToken);
    procedure ContextInjection(AInstance: TObject);
    function ContextInjectionByType(const AType: TClass; out AValue: TValue): Boolean;

    function ParamNameToParamIndex(AResourceInstance: TObject; const AParamName: string; AMethod: TRttiMethod): Integer;

    property Engine: TObject read FEngine;
    property Request: TWebRequest read GetRequest;
    property Response: TWebResponse read GetResponse;
    property URL: TMARSURL read GetURL;
    property Token: TMARSToken read GetToken;
  public
    constructor Create(const AEngine: TObject; const AName: string);
    destructor Destroy; override;

    function AddResource(AResource: string): Boolean;

    function HandleRequest(ARequest: TWebRequest; AResponse: TWebResponse; const AURL: TMARSURL): Boolean;
    procedure CollectGarbage(const AValue: TValue);

    property Name: string read FName;
    property BasePath: string read FBasePath write FBasePath;
    property System: Boolean read FSystem write FSystem;
    property Resources: TArray<string> read GetResources;
    property Parameters: TMARSParameters read FParameters;
  end;

  TMARSApplicationDictionary = class(TObjectDictionary<string, TMARSApplication>)
  end;

implementation

uses
  StrUtils
  , TypInfo
  , MARS.Core.Utils
  , MARS.Rtti.Utils
  , MARS.Core.Response
  , MARS.Core.Attributes
  , MARS.Core.Engine
  ;

{ TMARSApplication }

function TMARSApplication.AddResource(AResource: string): Boolean;

  function AddResourceToApplicationRegistry(const AInfo: TMARSConstructorInfo): Boolean;
  var
    LClass: TClass;
    LResult: Boolean;
  begin
    LResult := False;
    LClass := AInfo.TypeTClass;
    FRttiContext.GetType(LClass).HasAttribute<PathAttribute>(
      procedure (AAttribute: PathAttribute)
      var
        LURL: TMARSURL;
      begin
        LURL := TMARSURL.CreateDummy(AAttribute.Value);
        try
          if not FResourceRegistry.ContainsKey(LURL.PathTokens[0]) then
          begin
            FResourceRegistry.Add(LURL.PathTokens[0], AInfo.Clone);
            LResult := True;
          end;
        finally
          LURL.Free;
        end;
      end
    );
    Result := LResult;
  end;

var
  LRegistry: TMARSResourceRegistry;
  LInfo: TMARSConstructorInfo;
  LKey: string;
begin
  Result := False;
  LRegistry := TMARSResourceRegistry.Instance;

  if IsMask(AResource) then // has wildcards and so on...
  begin
    for LKey in LRegistry.Keys.ToArray do
    begin
      if MatchesMask(LKey, AResource) then
      begin
        if LRegistry.TryGetValue(LKey, LInfo) and AddResourceToApplicationRegistry(LInfo) then
          Result := True;
      end;
    end;
  end
  else // exact match
    if LRegistry.TryGetValue(AResource, LInfo) then
      Result := AddResourceToApplicationRegistry(LInfo);
end;

procedure TMARSApplication.CheckAuthorization(const AMethod: TRttiMethod; const AToken: TMARSToken);
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

    AMethod.ForEachAttribute<AuthorizationAttribute>(LProcessAuthorizationAttribute);
    // also check the class (resource) of the method
    AMethod.Parent.ForEachAttribute<AuthorizationAttribute>(LProcessAuthorizationAttribute);

    if LDenyAll then
      LAllowed := False
    else
    begin
      if LRolesAllowed then
      begin
        LAllowed := False;
        for LRole in LAllowedRoles do
        begin
          LAllowed := AToken.HasRole(LRole);
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

procedure TMARSApplication.CollectGarbage(const AValue: TValue);
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

procedure TMARSApplication.ContextInjection(AInstance: TObject);
var
  LType: TRttiType;
begin
  LType := FRttiContext.GetType(AInstance.ClassType);
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
          AField.SetValue(AInstance, LValue);
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
          AProperty.SetValue(AInstance, LValue);
      end;
    end
  );
end;

function TMARSApplication.ContextInjectionByType(const AType: TClass;
  out AValue: TValue): Boolean;
begin
  Result := True;
  // Token
  if (AType.InheritsFrom(TMARSToken)) then
    AValue := Token
  // HTTP request
  else if (AType.InheritsFrom(TWebRequest)) then
    AValue := Request
  // HTTP response
  else if (AType.InheritsFrom(TWebResponse)) then
    AValue := Response
  // URL info
  else if (AType.InheritsFrom(TMARSURL)) then
    AValue := URL
  // Engine
  else if (AType.InheritsFrom(TMARSEngine)) then
    AValue := Engine
  // Application
  else if (AType.InheritsFrom(TMARSApplication)) then
    AValue := Self
  else
    Result := False;
end;

constructor TMARSApplication.Create(const AEngine: TObject; const AName: string);
begin
  inherited Create;
  FName := AName;
  FEngine := AEngine;
  FRttiContext := TRttiContext.Create;
  FResourceRegistry := TObjectDictionary<string, TMARSConstructorInfo>.Create([doOwnsValues]);
  FParameters := TMARSParameters.Create(AName);
end;

destructor TMARSApplication.Destroy;
begin
  FParameters.Free;
  FResourceRegistry.Free;
  inherited;
end;

function TMARSApplication.FindMethodToInvoke(const AURL: TMARSURL;
  const AInfo: TMARSConstructorInfo): TRttiMethod;
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
  LResourceType := FRttiContext.GetType(AInfo.TypeTClass);
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
      LPrototypeURL := TMARSURL.CreateDummy([TMARSEngine(Engine).BasePath, BasePath, LResourcePath, LMethodPath]);
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

function TMARSApplication.GetRequest: TWebRequest;
begin
  Result := TMARSEngine(Engine).CurrentRequest;
end;

function TMARSApplication.GetResources: TArray<string>;
begin
  Result := FResourceRegistry.Keys.ToArray;
end;

function TMARSApplication.GetResponse: TWebResponse;
begin
  Result := TMARSEngine(Engine).CurrentResponse;
end;

function TMARSApplication.GetToken: TMARSToken;
begin
  Result := TMARSEngine(Engine).CurrentToken;
end;

function TMARSApplication.GetURL: TMARSURL;
begin
  Result := TMARSEngine(Engine).CurrentURL;
end;

function ExtractToken(const AString: string; const ATokenIndex: Integer; const ADelimiter: Char = '/'): string;
var
  LTokens: TArray<string>;
begin
  LTokens := TArray<string>(SplitString(AString, ADelimiter));

  Result := '';
  if ATokenIndex < Length(LTokens) then
    Result := LTokens[ATokenIndex]
  else
    raise EMARSException.CreateFmt('ExtractToken, index: %d from %s', [ATokenIndex, AString]);
end;


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

function TMARSApplication.FillAnnotatedParam(const AParam: TRttiParameter;
  const AResourceInstance: TObject; const AMethod: TRttiMethod): TValue;
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
      LParamIndex := ParamNameToParamIndex(AResourceInstance, LParamName, AMethod);
      LParamValue := URL.PathTokens[LParamIndex];
    end
    else if LAttribute is QueryParamAttribute then  // ?param1=value1
      LParamValue := Request.QueryFields.Values[LParamName]
    else if LAttribute is FormParamAttribute then   // forms
      LParamValue := Request.ContentFields.Values[LParamName]
    else if LAttribute is CookieParamAttribute then // cookies
      LParamValue := Request.CookieFields.Values[LParamName]
    else if LAttribute is HeaderParamAttribute then // http headers
      LParamValue := string(Request.GetFieldByName(AnsiString(LParamName)))
    else if LAttribute is BodyParamAttribute then   // body content
    begin
      TMARSMessageBodyReaderRegistry.Instance.FindReader(AMethod, AParam, LReader, LMediaType);
      try
        if Assigned(LReader) then
        begin
          LReaderFound := True;
          LReaderResult := LReader.ReadFrom(Request.RawContent, AMethod.GetAttributes, LMediaType, nil)
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

procedure TMARSApplication.FillResourceMethodParameters(AInstance: TObject; AMethod: TRttiMethod; var AArgumentArray: TArgumentArray);
var
  LParamArray: TArray<TRttiParameter>;
  LIndex: Integer;
begin
  Assert(Assigned(AMethod));
  try
    LParamArray := AMethod.GetParameters;

    SetLength(AArgumentArray, Length(LParamArray));
    for LIndex := Low(LParamArray) to High(LParamArray) do
      AArgumentArray[LIndex] := FillAnnotatedParam(LParamArray[LIndex], AInstance, AMethod);

  except
    on E: Exception do
      raise EMARSApplicationException.Create('Bad parameter values for resource method ' + AMethod.Name);
  end;
end;

function TMARSApplication.HandleRequest(ARequest: TWebRequest; AResponse: TWebResponse; const AURL: TMARSURL): Boolean;
var
  LInfo: TMARSConstructorInfo;
  LMethod: TRttiMethod;
  LInstance: TObject;
  LWriter: IMessageBodyWriter;
  LMediaType: TMediaType;
begin
  Result := False;
  try
    if not FResourceRegistry.TryGetValue(URL.Resource, LInfo) then
      raise EMARSApplicationException.Create(Format('Resource [%s] not found', [URL.Resource]), 404);

    LMethod := FindMethodToInvoke(URL, LInfo);

    if not Assigned(LMethod) then
      raise EMARSApplicationException.Create(Format('[%s] No implementation found for http method %s'
        , [URL.Resource, GetEnumName(TypeInfo(TMethodType), Ord(ARequest.MethodType))]), 404);

    CheckAuthorization(LMethod, Token);

    LInstance := LInfo.ConstructorFunc();
    try
      TMARSMessageBodyRegistry.Instance.FindWriter(LMethod, string(Request.Accept),
        LWriter, LMediaType);

      ContextInjection(LInstance);
      try
        InvokeResourceMethod(LInstance, LMethod, LWriter, ARequest, LMediaType);
      finally
        LWriter := nil;
        FreeAndNil(LMediaType);
      end;

    finally
      LInstance.Free;
    end;

    Result := True;
  except
    on E: EMARSHttpException do
    begin
      Response.StatusCode := E.Status;
      Response.Content := E.Message;
      Response.ContentType := TMediaType.TEXT_HTML;
    end
    else raise;
  end;
end;

procedure TMARSApplication.InvokeResourceMethod(AInstance: TObject;
  AMethod: TRttiMethod; const AWriter: IMessageBodyWriter;
  ARequest: TWebRequest; AMediaType: TMediaType);
var
  LMethodResult: TValue;
  LArgumentArray: TArgumentArray;
  LMARSResponse: TMARSResponse;
  LStream: TBytesStream;
  LContentType: AnsiString;
  LCustomAtributeProcessor: TProc<CustomHeaderAttribute>;
  LArgument: TValue;
begin
  // The returned object MUST be initially nil (needs to be consistent with the Free method)
  LMethodResult := nil;
  try
    LContentType := Response.ContentType;

    FillResourceMethodParameters(AInstance, AMethod, LArgumentArray);
    try
      LMethodResult := AMethod.Invoke(AInstance, LArgumentArray);
    finally
      for LArgument in LArgumentArray do
        CollectGarbage(LArgument);
    end;

    LCustomAtributeProcessor :=
      procedure (ACustomHeader: CustomHeaderAttribute)
      begin
        Response.CustomHeaders.Values[ACustomHeader.HeaderName] := ACustomHeader.Value;
      end;
    AMethod.Parent.ForEachAttribute<CustomHeaderAttribute>(LCustomAtributeProcessor);
    AMethod.ForEachAttribute<CustomHeaderAttribute>(LCustomAtributeProcessor);


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
        Response.ContentType := AnsiString(AMediaType.ToString);

      LStream := TBytesStream.Create();
      try
        AWriter.WriteTo(LMethodResult, AMethod.GetAttributes, AMediaType
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
    if not AMethod.HasAttribute<ResultIsReference>(nil) then
      CollectGarbage(LMethodResult);
  end;
end;

function TMARSApplication.ParamNameToParamIndex(AResourceInstance: TObject;
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
    LResURL := TMARSURL.CreateDummy([TMARSEngine(Engine).BasePath, BasePath, AResourcePathAttrib.Value, LSubResourcePath]);
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

end.
