(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.CustomResource;

{$I MARS.inc}

interface

uses
  SysUtils, Classes

  , MARS.Client.Application
  , MARS.Client.Client
  , MARS.Client.Utils
  ;

type
{$REGION 'Fluent scaffolding'}
  TMARSClientCustomResource = class;

  // Abstract base for fluent interfaces
  IMARSClientBaseParams = interface
    ['{19AC2FA4-A913-42AA-9955-53A55193EBB8}']
    function BeforeExecute(const AHandler: TProc<TMemoryStream>): IMARSClientBaseParams;
    function OnException(const AHandler: TMARSClientExecptionProc): IMARSClientBaseParams;
    procedure Go;
  end;

  // Abstract base for fluent interface implementors
  TMARSClientBaseParams = class abstract(TInterfacedObject, IMARSClientBaseParams)
  public
    // You can set these but the preferred way is to use the fluent calls below.
    BeforeExecuteHandler: TProc<TMemoryStream>;
    OnExceptionHandler: TMARSClientExecptionProc;
  public
    // IMARSClientBaseParams
    function BeforeExecute(const AHandler: TProc<TMemoryStream>): IMARSClientBaseParams;
    function OnException(const AHandler: TMARSClientExecptionProc): IMARSClientBaseParams;
    procedure Go; virtual; abstract;
  end;

  // Interface for fluent sync calls
  IMARSClientSyncParams = interface(IMARSClientBaseParams)
    ['{19AC2FA4-A913-42AA-9955-53A55193EBB8}']
    function AfterExecute(const AHandler: TMARSClientResponseProc): IMARSClientSyncParams;
  end;

  // Implements fluent sync calls
  TMARSClientSyncParams = class(TMARSClientBaseParams, IMARSClientSyncParams)
  private
    FGoHandler: TProc<TMARSClientSyncParams>;
  public
    // You can set this but the preferred way is to use the fluent call below.
    AfterExecuteHandler: TMARSClientResponseProc;
  public
    constructor Create(const AGoHandler: TProc<TMARSClientSyncParams>);
    // IMARSClientSyncParams
    function AfterExecute(const AHandler: TMARSClientResponseProc): IMARSClientSyncParams;
    procedure Go; override;
  end;

  // Interface for fluent async calls
  IMARSClientAsyncParams = interface(IMARSClientBaseParams)
    ['{E12D7ACB-1225-48C8-A8B9-5B98F3F60568}']
    function OnCompletion(const AHandler: TProc<TMARSClientCustomResource>): IMARSClientAsyncParams;
    function NoSynchronize: IMARSClientAsyncParams;
  end;

  // Implements fluent async calls
  TMARSClientAsyncParams = class(TMARSClientBaseParams, IMARSClientAsyncParams)
  private
    FGoHandler: TProc<TMARSClientAsyncParams>;
  public
    // You can set these but the preferred way is to use the fluent calls below.
    OnCompletionHandler: TProc<TMARSClientCustomResource>;
    Synchronize: Boolean;
  public
    constructor Create(const AGoHandler: TProc<TMARSClientAsyncParams>);
    procedure AfterConstruction; override;
    // IMARSClientAsyncParams
    function OnCompletion(const AHandler: TProc<TMARSClientCustomResource>): IMARSClientAsyncParams;
    function NoSynchronize: IMARSClientAsyncParams;
    procedure Go; override;
  end;

{$ENDREGION}

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TMARSClientCustomResource = class(TComponent)
  private
    FResource: string;
    FApplication: TMARSClientApplication;
    FSpecificClient: TMARSCustomClient;
    FSpecificToken: string;
    FCustomHeaders: TStrings;
    FPathParamsValues: TStrings;
    FQueryParams: TStrings;
    FSpecificAccept: string;
    FSpecificContentType: string;
    FSpecificURL: string;
    FToken: TMARSClientCustomResource;
    procedure SetPathParamsValues(const Value: TStrings);
    procedure SetQueryParams(const Value: TStrings);
    procedure SetCustomHeaders(const Value: TStrings);
  protected
    procedure ApplyCustomHeaders;
    function GetClient: TMARSCustomClient; virtual;
    function GetPath: string; virtual;
    function GetURL: string; virtual;
    function GetApplication: TMARSClientApplication; virtual;
    function GetAccept: string; virtual;
    function GetContentType: string; virtual;
    function GetAuthToken: string; virtual;
    function GetResponseAsString: string; virtual;

    procedure BeforeGET; virtual;
    procedure AfterGET(const AContent: TStream); virtual;

    procedure BeforePOST(const AContent: TMemoryStream); virtual;
    procedure AfterPOST(const AContent: TStream); virtual;

    procedure BeforePUT(const AContent: TMemoryStream); virtual;
    procedure AfterPUT(const AContent: TStream); virtual;

    procedure BeforeDELETE(const AContent: TMemoryStream); virtual;
    procedure AfterDELETE(const AContent: TStream); virtual;

    procedure DoError(const AException: Exception; const AVerb: TMARSHttpVerb; const AAfterExecute: TMARSClientResponseProc); virtual;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CloneSetup(const ASource: TMARSClientCustomResource); virtual;
    procedure CloneStatus(const ASource: TMARSClientCustomResource); virtual;

    // http verbs
    procedure GET(const ABeforeExecute: TMARSClientProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}); overload;
    {$ifndef DelphiXE2_UP}
    function GETAsString: string; overload;
    {$endif}
    function GETAsString(AEncoding: TEncoding {$ifdef DelphiXE2_UP} = nil{$endif};
      const ABeforeExecute: TMARSClientProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}): string; {$ifndef DelphiXE2_UP}overload;{$endif} virtual;
    procedure POST(const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}); overload; virtual;
    procedure PUT(const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}); overload; virtual;
    procedure DELETE(const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}); overload; virtual;
//    procedure PATCH(const ABeforeExecute: TMARSClientProc{$ifdef DelphiXE2_UP} = nil{$endif};
//      const AAfterExecute: TMARSClientProc{$ifdef DelphiXE2_UP} = nil{$endif};
//      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}); overload;
//    procedure HEAD(const ABeforeExecute: TMARSClientProc{$ifdef DelphiXE2_UP} = nil{$endif};
//      const AAfterExecute: TMARSClientProc{$ifdef DelphiXE2_UP} = nil{$endif};
//      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}); overload;
//    procedure OPTIONS(const ABeforeExecute: TMARSClientProc{$ifdef DelphiXE2_UP} = nil{$endif};
//      const AAfterExecute: TMARSClientProc{$ifdef DelphiXE2_UP} = nil{$endif};
//      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}); overload;

{$ifdef DelphiXE7_UP}
    procedure DELETEAsync(
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const ACompletionHandler: TProc<TMARSClientCustomResource>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const ASynchronize: Boolean = True); overload; virtual;
    procedure GETAsync(
      const ACompletionHandler: TProc<TMARSClientCustomResource>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const ASynchronize: Boolean = True); overload; virtual;
    procedure POSTAsync(
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const ACompletionHandler: TProc<TMARSClientCustomResource>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const ASynchronize: Boolean = True); overload; virtual;
    procedure PUTAsync(
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const ACompletionHandler: TProc<TMARSClientCustomResource>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const ASynchronize: Boolean = True); overload; virtual;
{$endif}

    property Accept: string read GetAccept;
    property ContentType: string read GetContentType;
    property Application: TMARSClientApplication read GetApplication write FApplication;
    property AuthToken: string read GetAuthToken;
    property Client: TMARSCustomClient read GetClient;
    property CustomHeaders: TStrings read FCustomHeaders write SetCustomHeaders;
    property SpecificAccept: string read FSpecificAccept write FSpecificAccept;
    property SpecificClient: TMARSCustomClient read FSpecificClient write FSpecificClient;
    property SpecificContentType: string read FSpecificContentType write FSpecificContentType;
    property SpecificToken: string read FSpecificToken write FSpecificToken;
    property SpecificURL: string read FSpecificURL write FSpecificURL;
    property Resource: string read FResource write FResource;
    property Path: string read GetPath;
    property PathParamsValues: TStrings read FPathParamsValues write SetPathParamsValues;
    property QueryParams: TStrings read FQueryParams write SetQueryParams;
    property ResponseAsString: string read GetResponseAsString;
    property Token: TMARSClientCustomResource read FToken write FToken;
    property URL: string read GetURL;
  published
  end;

  TMARSClientCustomResourceClass = class of TMARSClientCustomResource;


implementation

uses
  {$ifdef DelphiXE7_UP}System.Threading,{$endif}
  MARS.Core.URL, MARS.Core.Utils, MARS.Client.Token
, MARS.Client.Resource, MARS.Core.MediaType
;

{ TMARSClientCustomResource }

procedure TMARSClientCustomResource.AfterDELETE;
begin

end;

procedure TMARSClientCustomResource.AfterGET(const AContent: TStream);
begin

end;

procedure TMARSClientCustomResource.AfterPOST(const AContent: TStream);
begin

end;

procedure TMARSClientCustomResource.AfterPUT(const AContent: TStream);
begin

end;

procedure TMARSClientCustomResource.ApplyCustomHeaders;
begin
  Client.ApplyCustomHeaders(CustomHeaders);
end;

procedure TMARSClientCustomResource.AssignTo(Dest: TPersistent);
var
  LDestResource: TMARSClientCustomResource;
begin
//  inherited;
  LDestResource := Dest as TMARSClientCustomResource;
  LDestResource.Application := Application;

  LDestResource.SpecificAccept := SpecificAccept;
  LDestResource.SpecificContentType := SpecificContentType;
  LDestResource.SpecificClient := SpecificClient;
  LDestResource.SpecificToken := SpecificToken;
  LDestResource.SpecificURL := SpecificURL;
  LDestResource.Resource := Resource;
  LDestResource.CustomHeaders.Assign(CustomHeaders);
  LDestResource.PathParamsValues.Assign(PathParamsValues);
  LDestResource.QueryParams.Assign(QueryParams);
  LDestResource.Token := Token;
end;

procedure TMARSClientCustomResource.BeforeDELETE(const AContent: TMemoryStream);
begin
  ApplyCustomHeaders;

end;

procedure TMARSClientCustomResource.BeforeGET;
begin
  ApplyCustomHeaders;

end;

procedure TMARSClientCustomResource.BeforePOST(const AContent: TMemoryStream);
begin
  ApplyCustomHeaders;

end;

procedure TMARSClientCustomResource.BeforePUT(const AContent: TMemoryStream);
begin
  ApplyCustomHeaders;

end;

procedure TMARSClientCustomResource.CloneSetup(
  const ASource: TMARSClientCustomResource);
begin
  if not Assigned(ASource) then
    Exit;

  Assign(ASource);
end;

procedure TMARSClientCustomResource.CloneStatus(
  const ASource: TMARSClientCustomResource);
begin
end;

constructor TMARSClientCustomResource.Create(AOwner: TComponent);
begin
  inherited;
  FResource := 'main';
  if TMARSComponentHelper.IsDesigning(Self) then
  begin
    FApplication := TMARSComponentHelper.FindDefault<TMARSClientApplication>(Self);
    FToken := TMARSComponentHelper.FindDefault<TMARSClientToken>(Self);
  end;
  FSpecificAccept := TMediaType.WILDCARD;
  FSpecificContentType := '';
  FCustomHeaders := TStringList.Create;
  FPathParamsValues := TStringList.Create;
  FQueryParams := TStringList.Create;
end;

function TMARSClientCustomResource.GetAuthToken: string;
begin
  Result := SpecificToken;
  if (Result = '') and Assigned(Token) then
    Result := (Token as TMARSClientToken).Token;
end;

function TMARSClientCustomResource.GetClient: TMARSCustomClient;
begin
  Result := nil;
  if Assigned(FSpecificClient) then
    Result := FSpecificClient
  else
  begin
    if Assigned(FApplication) then
      Result := FApplication.Client;
  end;
end;

function TMARSClientCustomResource.GetContentType: string;
begin
  Result := FSpecificContentType;
  if (Result = '') and Assigned(Application) then
    Result := Application.DefaultContentType;
end;

function TMARSClientCustomResource.GetPath: string;
var
  LEngine: string;
  LApplication: string;
begin
  LEngine := '';
  if Assigned(Client) then
    LEngine := Client.MARSEngineURL;

  LApplication := '';
  if Assigned(Application) then
    LApplication := Application.AppName;

  Result := TMARSURL.CombinePath([LEngine, LApplication, Resource]);
end;


function TMARSClientCustomResource.GetResponseAsString: string;
begin
  Result := '';
end;

function TMARSClientCustomResource.GetURL: string;
var
  LQueryString: string;
begin
  if FSpecificURL <> '' then
    Result := FSpecificURL
  else
    Result := TMARSURL.CombinePath([
      Path
      , TMARSURL.CombinePath(TMARSURL.URLEncode(FPathParamsValues.ToStringArray))
    ]);

  if FQueryParams.Count > 0 then
  begin
    LQueryString := SmartConcat(TMARSURL.URLEncode(FQueryParams.ToStringArray), TMARSURL.URL_QUERY_SEPARATOR);
    if Result.Contains(TMARSURL.URL_QUERY_PREFIX) then
      Result := Result + TMARSURL.URL_QUERY_SEPARATOR + LQueryString
    else
      Result := Result + TMARSURL.URL_QUERY_PREFIX + LQueryString;
  end;
end;

procedure TMARSClientCustomResource.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if SpecificClient = AComponent then
      SpecificClient := nil;
    if Token = AComponent then
      Token := nil;
    if Application = AComponent then
      Application := nil;
  end;

end;

procedure TMARSClientCustomResource.DELETE(const ABeforeExecute: TProc<TMemoryStream>;
  const AAfterExecute: TMARSClientResponseProc; const AOnException: TMARSClientExecptionProc);
var
  LResponseStream: TMemoryStream;
  LContent: TMemoryStream;
begin
  try
    LContent := TMemoryStream.Create;
    try
      BeforeDELETE(LContent);

      if Assigned(ABeforeExecute) then
        ABeforeExecute(LContent);

      LResponseStream := TMemoryStream.Create;
      try
        Client.Delete(URL, LContent, LResponseStream, AuthToken, Accept, ContentType);

        AfterDELETE(LResponseStream);

        if Assigned(AAfterExecute) then
          AAfterExecute(LResponseStream);
      finally
        LResponseStream.Free;
      end;
    finally
      LContent.Free;
    end;
  except
    on E:Exception do
    begin
      if Assigned(AOnException) then
        AOnException(E)
      else
        DoError(E, TMARSHttpVerb.Delete
          , procedure (AStream: TStream)
            begin
              if Assigned(AAfterExecute) then
                AAfterExecute(AStream);
            end);
    end;
  end;
end;

{$ifdef DelphiXE7_UP}
procedure TMARSClientCustomResource.DELETEAsync(
  const ABeforeExecute: TProc<TMemoryStream>;
  const ACompletionHandler: TProc<TMARSClientCustomResource>;
  const AOnException: TMARSClientExecptionProc;
  const ASynchronize: Boolean);
var
  LClient: TMARSCustomClient;
  LApplication: TMARSClientApplication;
  LResource: TMARSClientCustomResource;
begin
  LClient := TMARSCustomClientClass(Client.ClassType).Create(nil);
  try
    LClient.CloneSetup(Client);

    LApplication := TMARSClientApplication.Create(nil);
    try
      LApplication.CloneSetup(Application);
      LApplication.Client := LClient;

      LResource := TMARSClientCustomResourceClass(ClassType).Create(nil);
      try
        LResource.CloneSetup(Self);
        LResource.SpecificClient := nil;
        LResource.Application := LApplication;

        TTask.Run(
          procedure
          var
            LOnException: TProc<Exception>;
          begin
            try
              LOnException := nil;
              if Assigned(AOnException) then
                LOnException :=
                  procedure (AException: Exception)
                  begin
                    if ASynchronize then
                      TThread.Synchronize(nil
                        , procedure
                          begin
                            AOnException(AException);
                          end
                      )
                    else
                      AOnException(AException);
                  end;

              LResource.DELETE(
                  ABeforeExecute
                , procedure (AStream: TStream)
                  begin
                    CloneStatus(LResource);

                    if Assigned(ACompletionHandler) then
                    begin
                      if ASynchronize then
                        TThread.Synchronize(nil
                          , procedure
                            begin
                              ACompletionHandler(LResource);
                            end
                        )
                      else
                        ACompletionHandler(LResource);
                    end;
                  end
                , LOnException
                );
              finally
                LResource.Free;
                LApplication.Free;
                LClient.Free;
              end;
          end
        );
      except
        LResource.Free;
        raise;
      end;
    except
      LApplication.Free;
      raise;
    end;
  except
    LClient.Free;
    raise;
  end;
end;
{$endif}


destructor TMARSClientCustomResource.Destroy;
begin
  FCustomHeaders.Free;
  FQueryParams.Free;
  FPathParamsValues.Free;
  inherited;
end;

procedure TMARSClientCustomResource.DoError(const AException: Exception;
  const AVerb: TMARSHttpVerb; const AAfterExecute: TMARSClientResponseProc);
begin
  if Assigned(Application) then
    Application.DoError(Self, AException, AVerb, AAfterExecute)
  else if Assigned(Client) then
    Client.DoError(Self, AException, AVerb, AAfterExecute)
  else
    raise EMARSClientException.Create(AException.Message);
end;

procedure TMARSClientCustomResource.GET(const ABeforeExecute: TMARSCLientProc;
  const AAfterExecute: TMARSClientResponseProc;
  const AOnException: TMARSClientExecptionProc);
var
  LResponseStream: TMemoryStream;
begin
  try
    BeforeGET();

    if Assigned(ABeforeExecute) then
      ABeforeExecute();

    LResponseStream := TMemoryStream.Create;
    try
      Client.Get(URL, LResponseStream, AuthToken, Accept, ContentType);

      AfterGET(LResponseStream);

      if Assigned(AAfterExecute) then
        AAfterExecute(LResponseStream);
    finally
      LResponseStream.Free;
    end;
  except
    on E:Exception do
    begin
      if Assigned(AOnException) then
        AOnException(E)
      else
        DoError(E, TMARSHttpVerb.Get, AAfterExecute);
    end;
  end;
end;

{$ifndef DelphiXE2_UP}
function TMARSClientCustomResource.GETAsString: string;
begin
  Result := GetAsString(nil, nil, nil);
end;
{$endif}

function TMARSClientCustomResource.GETAsString(AEncoding: TEncoding;
  const ABeforeExecute: TMARSClientProc;
  const AOnException: TMARSClientExecptionProc): string;
var
  LResult: string;
  LEncoding: TEncoding;
begin
  LResult := '';
  LEncoding := AEncoding;
  if not Assigned(LEncoding) then
    LEncoding := TEncoding.UTF8;

  GET(ABeforeExecute
    , procedure (AResponse: TStream)
      begin
        LResult := StreamToString(AResponse, LEncoding)
      end
    , AOnException
  );
  Result := LResult;
end;

function TMARSClientCustomResource.GetAccept: string;
begin
  Result := FSpecificAccept;
  if (Result = '') and Assigned(Application) then
    Result := Application.DefaultMediaType;
end;

function TMARSClientCustomResource.GetApplication: TMARSClientApplication;
begin
  Result := FApplication;
end;

{$ifdef DelphiXE7_UP}
procedure TMARSClientCustomResource.GETAsync(
  const ACompletionHandler: TProc<TMARSClientCustomResource>;
  const AOnException: TMARSClientExecptionProc;
  const ASynchronize: Boolean);
var
  LClient: TMARSCustomClient;
  LApplication: TMARSClientApplication;
  LResource: TMARSClientCustomResource;
begin
  LClient := TMARSCustomClientClass(Client.ClassType).Create(nil);
  try
    LClient.CloneSetup(Client);

    LApplication := TMARSClientApplication.Create(nil);
    try
      LApplication.CloneSetup(Application);
      LApplication.Client := LClient;

      LResource := TMARSClientCustomResourceClass(ClassType).Create(nil);
      try
        LResource.CloneSetup(Self);
        LResource.SpecificClient := nil;
        LResource.Application := LApplication;

        TTask.Run(
          procedure
          var
            LOnException: TProc<Exception>;
          begin
            try
              LOnException := nil;
              if Assigned(AOnException) then
                LOnException :=
                  procedure (AException: Exception)
                  begin
                    if ASynchronize then
                      TThread.Synchronize(nil
                        , procedure
                          begin
                            AOnException(AException);
                          end
                      )
                    else
                      AOnException(AException);
                  end;

              LResource.GET(
                  nil
                , procedure (AStream: TStream)
                  begin
                    CloneStatus(LResource);

                    if Assigned(ACompletionHandler) then
                    begin
                      if ASynchronize then
                        TThread.Synchronize(nil
                          , procedure
                            begin
                              ACompletionHandler(LResource);
                            end
                        )
                      else
                        ACompletionHandler(LResource);
                    end;
                  end
                , LOnException
                );
              finally
                FreeAndNil(LResource);
                FreeAndNil(LApplication);
                FreeAndNil(LClient);
              end;
          end
        );
      except
        LResource.Free;
        raise;
      end;
    except
      LApplication.Free;
      raise;
    end;
  except
    LClient.Free;
    raise;
  end;
end;
{$endif}

procedure TMARSClientCustomResource.POST(
  const ABeforeExecute: TProc<TMemoryStream>;
  const AAfterExecute: TMARSClientResponseProc;
  const AOnException: TMARSClientExecptionProc);
var
  LResponseStream: TMemoryStream;
  LContent: TMemoryStream;
begin
  try
    LContent := TMemoryStream.Create;
    try
      BeforePOST(LContent);

      if Assigned(ABeforeExecute) then
        ABeforeExecute(LContent);

      LResponseStream := TMemoryStream.Create;
      try
        Client.Post(URL, LContent, LResponseStream, AuthToken, Accept, ContentType);

        AfterPOST(LResponseStream);

        if Assigned(AAfterExecute) then
          AAfterExecute(LResponseStream);
      finally
        LResponseStream.Free;
      end;
    finally
      LContent.Free;
    end;
  except
    on E:Exception do
    begin
      if Assigned(AOnException) then
        AOnException(E)
      else
        DoError(E, TMARSHttpVerb.Post, AAfterExecute);
    end;
  end;
end;

{$ifdef DelphiXE7_UP}
procedure TMARSClientCustomResource.POSTAsync(
  const ABeforeExecute: TProc<TMemoryStream>;
  const ACompletionHandler: TProc<TMARSClientCustomResource>;
  const AOnException: TMARSClientExecptionProc;
  const ASynchronize: Boolean);
var
  LClient: TMARSCustomClient;
  LApplication: TMARSClientApplication;
  LResource: TMARSClientCustomResource;
begin
  LClient := TMARSCustomClientClass(Client.ClassType).Create(nil);
  try
    LClient.CloneSetup(Client);

    LApplication := TMARSClientApplication.Create(nil);
    try
      LApplication.CloneSetup(Application);
      LApplication.Client := LClient;

      LResource := TMARSClientCustomResourceClass(ClassType).Create(nil);
      try
        LResource.CloneSetup(Self);
        LResource.SpecificClient := nil;
        LResource.Application := LApplication;

        TTask.Run(
          procedure
          var
            LOnException: TProc<Exception>;
          begin
            try
              LOnException := nil;
              if Assigned(AOnException) then
                LOnException :=
                  procedure (AException: Exception)
                  begin
                    if ASynchronize then
                      TThread.Synchronize(nil
                        , procedure
                          begin
                            AOnException(AException);
                          end
                      )
                    else
                      AOnException(AException);
                  end;

              LResource.POST(
                ABeforeExecute
              , procedure (AStream: TStream)
                begin
                  CloneStatus(LResource);

                  if Assigned(ACompletionHandler) then
                  begin
                    if ASynchronize then
                      TThread.Synchronize(nil
                        , procedure
                          begin
                            ACompletionHandler(LResource);
                          end
                      )
                    else
                      ACompletionHandler(LResource);
                  end;
                end
              , LOnException
              );
            finally
              FreeAndNil(LResource);
              FreeAndNil(LApplication);
              FreeAndNil(LClient);
            end;
          end
        );
      except
        LResource.Free;
        raise;
      end;
    except
      LApplication.Free;
      raise;
    end;
  except
    LClient.Free;
    raise;
  end;
end;
{$endif}

procedure TMARSClientCustomResource.PUT(const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
  const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
  const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}
);
var
  LResponseStream: TMemoryStream;
  LContent: TMemoryStream;
begin
  try
    LContent := TMemoryStream.Create;
    try
      BeforePUT(LContent);

      if Assigned(ABeforeExecute) then
        ABeforeExecute(LContent);

      LResponseStream := TMemoryStream.Create;
      try
        Client.Put(URL, LContent, LResponseStream, AuthToken, Accept, ContentType);

        AfterPUT(LResponseStream);

        if Assigned(AAfterExecute) then
          AAfterExecute(LResponseStream);
      finally
        LResponseStream.Free;
      end;
    finally
      LContent.Free;
    end;
  except
    on E:Exception do
    begin
      if Assigned(AOnException) then
        AOnException(E)
      else
        DoError(E, TMARSHttpVerb.Put, AAfterExecute);
    end;
  end;
end;

{$ifdef DelphiXE7_UP}
procedure TMARSClientCustomResource.PUTAsync(
  const ABeforeExecute: TProc<TMemoryStream>;
  const ACompletionHandler: TProc<TMARSClientCustomResource>;
  const AOnException: TMARSClientExecptionProc;
  const ASynchronize: Boolean);
var
  LClient: TMARSCustomClient;
  LApplication: TMARSClientApplication;
  LResource: TMARSClientCustomResource;
begin
  LClient := TMARSCustomClientClass(Client.ClassType).Create(nil);
  try
    LClient.CloneSetup(Client);

    LApplication := TMARSClientApplication.Create(nil);
    try
      LApplication.CloneSetup(Application);
      LApplication.Client := LClient;

      LResource := TMARSClientCustomResourceClass(ClassType).Create(nil);
      try
        LResource.CloneSetup(Self);
        LResource.SpecificClient := nil;
        LResource.Application := LApplication;

        TTask.Run(
          procedure
          var
            LOnException: TProc<Exception>;
          begin
            try
              LOnException := nil;
              if Assigned(AOnException) then
                LOnException :=
                  procedure (AException: Exception)
                  begin
                    if ASynchronize then
                      TThread.Synchronize(nil
                        , procedure
                          begin
                            AOnException(AException);
                          end
                      )
                    else
                      AOnException(AException);
                  end;

              LResource.PUT(
                ABeforeExecute
              , procedure (AStream: TStream)
                begin
                  CloneStatus(LResource);

                  if Assigned(ACompletionHandler) then
                  begin
                    if ASynchronize then
                      TThread.Synchronize(nil
                        , procedure
                          begin
                            ACompletionHandler(LResource);
                          end
                      )
                    else
                      ACompletionHandler(LResource);
                  end;
                end
              , LOnException
              );
            finally
              FreeAndNil(LResource);
              FreeAndNil(LApplication);
              FreeAndNil(LClient);
            end;
          end
        );
      except
        LResource.Free;
        raise;
      end;
    except
      LApplication.Free;
      raise;
    end;
  except
    LClient.Free;
    raise;
  end;
end;
{$endif}

procedure TMARSClientCustomResource.SetCustomHeaders(const Value: TStrings);
begin
  FCustomHeaders.Assign(Value);
end;

procedure TMARSClientCustomResource.SetPathParamsValues(const Value: TStrings);
begin
  FPathParamsValues.Assign(Value);
end;

procedure TMARSClientCustomResource.SetQueryParams(const Value: TStrings);
begin
  FQueryParams.Assign(Value);
end;

{$REGION 'Fluent scaffolding implementation'}
{ TMARSClientBaseParams }

function TMARSClientBaseParams.BeforeExecute(
  const AHandler: TProc<TMemoryStream>): IMARSClientBaseParams;
begin
  BeforeExecuteHandler := AHandler;
  Result := Self;
end;

function TMARSClientBaseParams.OnException(
  const AHandler: TMARSClientExecptionProc): IMARSClientBaseParams;
begin
  OnExceptionHandler := AHandler;
  Result := Self;
end;

{ TMARSClientParams }

function TMARSClientSyncParams.AfterExecute(
  const AHandler: TMARSClientResponseProc): IMARSClientSyncParams;
begin
  AfterExecuteHandler := AHandler;
  Result := Self;
end;

{ TMARSClientAsyncParams }

procedure TMARSClientAsyncParams.AfterConstruction;
begin
  inherited;
  Synchronize := True;
end;

function TMARSClientAsyncParams.OnCompletion(
  const AHandler: TProc<TMARSClientCustomResource>): IMARSClientAsyncParams;
begin
  OnCompletionHandler := AHandler;
  Result := Self;
end;

constructor TMARSClientAsyncParams.Create(
  const AGoHandler: TProc<TMARSClientAsyncParams>);
begin
  inherited Create;
  FGoHandler := AGoHandler;
end;

procedure TMARSClientAsyncParams.Go;
begin
  Assert(Assigned(FGoHandler));

  FGoHandler(Self);
end;

function TMARSClientAsyncParams.NoSynchronize: IMARSClientAsyncParams;
begin
  Synchronize := False;
  Result := Self;
end;

constructor TMARSClientSyncParams.Create(
  const AGoHandler: TProc<TMARSClientSyncParams>);
begin
  inherited Create;
  FGoHandler := AGoHandler;
end;

procedure TMARSClientSyncParams.Go;
begin
  Assert(Assigned(FGoHandler));

  FGoHandler(Self);
end;

{$ENDREGION}

end.

