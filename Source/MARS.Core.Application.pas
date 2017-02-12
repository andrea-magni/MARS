(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Application;

{$I MARS.inc}

interface

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
  EMARSAuthenticationException = class(EMARSApplicationException);
  EMARSAuthorizationException = class(EMARSApplicationException);

  TMARSApplication = class
  private
    FRttiContext: TRttiContext;
    FResourceRegistry: TObjectDictionary<string, TMARSConstructorInfo>;
    FBasePath: string;
    FName: string;
    FEngine: TObject;
    FSystem: Boolean;
    FParameters: TMARSParameters;
  protected
    function GetRequest: TWebRequest; virtual;
    function GetResponse: TWebResponse; virtual;
    function GetURL: TMARSURL; virtual;

    property Request: TWebRequest read GetRequest;
    property Response: TWebResponse read GetResponse;
    property URL: TMARSURL read GetURL;
  public
    constructor Create(const AEngine: TObject; const AName: string); virtual;
    destructor Destroy; override;

    function AddResource(AResource: string): Boolean;
    procedure EnumerateResources(const ADoSomething: TProc<string, TMARSConstructorInfo>);

    function HandleRequest(ARequest: TWebRequest; AResponse: TWebResponse;
      const AURL: TMARSURL): Boolean; virtual;

    property Engine: TObject read FEngine;
    property Name: string read FName;
    property BasePath: string read FBasePath write FBasePath;
    property System: Boolean read FSystem write FSystem;
    property Resources: TObjectDictionary<string, TMARSConstructorInfo> read FResourceRegistry;
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
  , MARS.Core.Invocation;

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
          if LURL.HasPathTokens and not FResourceRegistry.ContainsKey(LURL.PathTokens[0]) then
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

procedure TMARSApplication.EnumerateResources(
  const ADoSomething: TProc<string, TMARSConstructorInfo>);
var
  LPair: TPair<string, TMARSConstructorInfo>;
begin
  if Assigned(ADoSomething) then
    for LPair in FResourceRegistry do
      ADoSomething(LPair.Key, LPair.Value);
end;

function TMARSApplication.GetRequest: TWebRequest;
begin
  Result := TMARSEngine(Engine).CurrentRequest;
end;

function TMARSApplication.GetResponse: TWebResponse;
begin
  Result := TMARSEngine(Engine).CurrentResponse;
end;

function TMARSApplication.GetURL: TMARSURL;
begin
  Result := TMARSEngine(Engine).CurrentURL;
end;

function TMARSApplication.HandleRequest(ARequest: TWebRequest; AResponse: TWebResponse; const AURL: TMARSURL): Boolean;
var
  LActivationRecord: TMARSActivationRecord;
begin
  Result := False;
  try
    LActivationRecord := TMARSActivationRecord.Create(Self, Request, Response, AURL);
    try
      LActivationRecord.CheckResource;
      LActivationRecord.CheckMethod;
      LActivationRecord.CheckAuthentication;
      LActivationRecord.CheckAuthorization;

      LActivationRecord.Invoke;
      Result := True;
    finally
      LActivationRecord.Free;
    end;
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


end.
