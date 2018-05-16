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
  {$ifdef DelphiXE2_UP}
    [ComponentPlatformsAttribute(
        pidWin32 or pidWin64
     or pidOSX32
     or pidiOSSimulator
     or pidiOSDevice
    {$ifdef DelphiXE8_UP}
     or pidiOSDevice32 or pidiOSDevice64
    {$endif}
     or pidAndroid)]
  {$endif}
  TMARSClientCustomResource = class(TComponent)
  private
    FResource: string;
    FApplication: TMARSClientApplication;
    FSpecificClient: TMARSCustomClient;
    FSpecificToken: string;
    FPathParamsValues: TStrings;
    FQueryParams: TStrings;
    FSpecificAccept: string;
    FSpecificContentType: string;
    FToken: TMARSClientCustomResource;
    procedure SetPathParamsValues(const Value: TStrings);
    procedure SetQueryParams(const Value: TStrings);
  protected
    function GetClient: TMARSCustomClient; virtual;
    function GetPath: string; virtual;
    function GetURL: string; virtual;
    function GetApplication: TMARSClientApplication; virtual;
    function GetAccept: string; virtual;
    function GetContentType: string; virtual;
    function GetAuthToken: string; virtual;

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
      const ASynchronize: Boolean = True); overload;
    procedure GETAsync(
      const ACompletionHandler: TProc<TMARSClientCustomResource>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const ASynchronize: Boolean = True); overload;
    procedure POSTAsync(
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const ACompletionHandler: TProc<TMARSClientCustomResource>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const ASynchronize: Boolean = True); overload;
    procedure PUTAsync(
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const ACompletionHandler: TProc<TMARSClientCustomResource>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const ASynchronize: Boolean = True); overload;
{$endif}

    property Accept: string read GetAccept;
    property ContentType: string read GetContentType;
    property Application: TMARSClientApplication read GetApplication write FApplication;
    property AuthToken: string read GetAuthToken;
    property Client: TMARSCustomClient read GetClient;
    property SpecificAccept: string read FSpecificAccept write FSpecificAccept;
    property SpecificClient: TMARSCustomClient read FSpecificClient write FSpecificClient;
    property SpecificContentType: string read FSpecificContentType write FSpecificContentType;
    property SpecificToken: string read FSpecificToken write FSpecificToken;
    property Resource: string read FResource write FResource;
    property Path: string read GetPath;
    property PathParamsValues: TStrings read FPathParamsValues write SetPathParamsValues;
    property QueryParams: TStrings read FQueryParams write SetQueryParams;
    property Token: TMARSClientCustomResource read FToken write FToken;
    property URL: string read GetURL;
  published
  end;

  TMARSClientCustomResourceClass = class of TMARSClientCustomResource;


implementation

uses
  {$ifdef DelphiXE7_UP}System.Threading,{$endif}
   MARS.Core.URL, MARS.Core.Utils, MARS.Client.Token
 , MARS.Client.Resource, MARS.Client.SubResource
 , MARS.Core.MediaType
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
  LDestResource.Resource := Resource;
  LDestResource.PathParamsValues.Assign(PathParamsValues);
  LDestResource.QueryParams.Assign(QueryParams);
  LDestResource.Token := Token;
end;

procedure TMARSClientCustomResource.BeforeDELETE(const AContent: TMemoryStream);
begin

end;

procedure TMARSClientCustomResource.BeforeGET;
begin

end;

procedure TMARSClientCustomResource.BeforePOST(const AContent: TMemoryStream);
begin

end;

procedure TMARSClientCustomResource.BeforePUT(const AContent: TMemoryStream);
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
  FPathParamsValues := TStringList.Create;
  FQueryParams := TStringList.Create;
  FSpecificAccept := TMediaType.WILDCARD;
  FSpecificContentType := '';
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


function TMARSClientCustomResource.GetURL: string;
begin
  Result := TMARSURL.CombinePath([
    Path
    , TMARSURL.CombinePath(TMARSURL.URLEncode(FPathParamsValues.ToStringArray))
  ]);

  if FQueryParams.Count > 0 then
    Result := Result + '?' + SmartConcat(TMARSURL.URLEncode(FQueryParams.ToStringArray), '&');
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
  LResource, LParentResource: TMARSClientCustomResource;
begin
  LClient := TMARSCustomClientClass(Client.ClassType).Create(nil);
  try
    LClient.Assign(Client);
    LApplication := TMARSClientApplication.Create(nil);
    try
      LApplication.Assign(Application);
      LApplication.Client := LClient;
      LResource := TMARSClientCustomResourceClass(ClassType).Create(nil);
      try
        LResource.Assign(Self);
        LResource.SpecificClient := nil;
        LResource.Application := LApplication;

        LParentResource := nil;
        if Self is TMARSClientSubResource then
        begin
          LParentResource := TMARSClientCustomResourceClass(TMARSClientSubResource(Self).ParentResource.ClassType).Create(nil);
          try
            LParentResource.Assign(TMARSClientSubResource(Self).ParentResource);
            LParentResource.SpecificClient := nil;
            LParentResource.Application := LApplication;
            (LResource as TMARSClientSubResource).ParentResource := LParentResource as TMARSClientResource;
          except
            LParentResource.Free;
            raise;
          end;
        end;

        TTask.Run(
          procedure
          var
            LOnException: TProc<Exception>;
          begin
            try
              if Assigned(AOnException) then
              begin
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
              end
              else
                LOnException := nil;

              LResource.DELETE(
                  ABeforeExecute
                , procedure (AStream: TStream)
                  begin
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
                if Assigned(LParentResource) then
                  LParentResource.Free;
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
    LEncoding := TEncoding.Default;

  GET(ABeforeExecute
    , procedure (AResponse: TStream)
      var
        LStreamReader: TStreamReader;
      begin
        AResponse.Position := 0;
        LStreamReader := TStreamReader.Create(AResponse, LEncoding);
        try
          LResult := LStreamReader.ReadToEnd;
        finally
          LStreamReader.Free;
        end;
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
  LResource, LParentResource: TMARSClientCustomResource;
begin
  LClient := TMARSCustomClientClass(Client.ClassType).Create(nil);
  try
    LClient.Assign(Client);
    LApplication := TMARSClientApplication.Create(nil);
    try
      LApplication.Assign(Application);
      LApplication.Client := LClient;
      LResource := TMARSClientCustomResourceClass(ClassType).Create(nil);
      try
        LResource.Assign(Self);
        LResource.SpecificClient := nil;
        LResource.Application := LApplication;

        LParentResource := nil;
        if Self is TMARSClientSubResource then
        begin
          LParentResource := TMARSClientCustomResourceClass(TMARSClientSubResource(Self).ParentResource.ClassType).Create(nil);
          try
            LParentResource.Assign(TMARSClientSubResource(Self).ParentResource);
            LParentResource.SpecificClient := nil;
            LParentResource.Application := LApplication;
            (LResource as TMARSClientSubResource).ParentResource := LParentResource as TMARSClientResource;
          except
            LParentResource.Free;
            raise;
          end;
        end;

        TTask.Run(
          procedure
          var
            LOnException: TProc<Exception>;
          begin
            try
              if Assigned(AOnException) then
              begin
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
              end
              else
                LOnException := nil;

              LResource.GET(nil
                , procedure (AStream: TStream)
                  begin
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
                if Assigned(LParentResource) then
                  LParentResource.Free;
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
  LResource, LParentResource: TMARSClientCustomResource;
begin
  LClient := TMARSCustomClientClass(Client.ClassType).Create(nil);
  try
    LClient.Assign(Client);
    LApplication := TMARSClientApplication.Create(nil);
    try
      LApplication.Assign(Application);
      LApplication.Client := LClient;
      LResource := TMARSClientCustomResourceClass(ClassType).Create(nil);
      try
        LResource.Assign(Self);
        LResource.SpecificClient := nil;
        LResource.Application := LApplication;

        LParentResource := nil;
        if Self is TMARSClientSubResource then
        begin
          LParentResource := TMARSClientCustomResourceClass(TMARSClientSubResource(Self).ParentResource.ClassType).Create(nil);
          try
            LParentResource.Assign(TMARSClientSubResource(Self).ParentResource);
            LParentResource.SpecificClient := nil;
            LParentResource.Application := LApplication;
            (LResource as TMARSClientSubResource).ParentResource := LParentResource as TMARSClientResource;
          except
            LParentResource.Free;
            raise;
          end;
        end;

        TTask.Run(
          procedure
          var
            LOnException: TProc<Exception>;
          begin
            try
              if Assigned(AOnException) then
              begin
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
              end
              else
                LOnException := nil;

              LResource.POST(
                ABeforeExecute
              , procedure (AStream: TStream)
                begin
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
              if Assigned(LParentResource) then
                LParentResource.Free;
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
  LResource, LParentResource: TMARSClientCustomResource;
begin
  LClient := TMARSCustomClientClass(Client.ClassType).Create(nil);
  try
    LClient.Assign(Client);
    LApplication := TMARSClientApplication.Create(nil);
    try
      LApplication.Assign(Application);
      LApplication.Client := LClient;
      LResource := TMARSClientCustomResourceClass(ClassType).Create(nil);
      try
        LResource.Assign(Self);
        LResource.SpecificClient := nil;
        LResource.Application := LApplication;

        LParentResource := nil;
        if Self is TMARSClientSubResource then
        begin
          LParentResource := TMARSClientCustomResourceClass(TMARSClientSubResource(Self).ParentResource.ClassType).Create(nil);
          try
            LParentResource.Assign(TMARSClientSubResource(Self).ParentResource);
            LParentResource.SpecificClient := nil;
            LParentResource.Application := LApplication;
            (LResource as TMARSClientSubResource).ParentResource := LParentResource as TMARSClientResource;
          except
            LParentResource.Free;
            raise;
          end;
        end;

        TTask.Run(
          procedure
          var
            LOnException: TProc<Exception>;
          begin
            try
              if Assigned(AOnException) then
              begin
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
              end
              else
                LOnException := nil;

              LResource.PUT(
                ABeforeExecute
              , procedure (AStream: TStream)
                begin
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
              if Assigned(LParentResource) then
                LParentResource.Free;
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

procedure TMARSClientCustomResource.SetPathParamsValues(const Value: TStrings);
begin
  FPathParamsValues.Assign(Value);
end;

procedure TMARSClientCustomResource.SetQueryParams(const Value: TStrings);
begin
  FQueryParams.Assign(Value);
end;

end.
