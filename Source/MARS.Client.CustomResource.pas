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
    {$ifdef DelphiXE7_UP}
     or pidiOSDevice32 or pidiOSDevice64
    {$endif}
     or pidAndroid)]
  {$endif}
  TMARSClientCustomResource = class(TComponent)
  private
    FResource: string;
    FApplication: TMARSClientApplication;
    FSpecificClient: TMARSClient;
    FPathParamsValues: TStrings;
    FQueryParams: TStrings;
    FSpecificAccept: string;
    FToken: TMARSClientCustomResource;
    procedure SetPathParamsValues(const Value: TStrings);
    procedure SetQueryParams(const Value: TStrings);
  protected
    function GetClient: TMARSClient; virtual;
    function GetPath: string; virtual;
    function GetURL: string; virtual;
    function GetApplication: TMARSClientApplication; virtual;
    function GetAccept: string;
    function GetAuthToken: string; virtual;

    procedure BeforeGET; virtual;
    procedure AfterGET; virtual;

    procedure BeforePOST(AContent: TMemoryStream); virtual;
    procedure AfterPOST; virtual;

    procedure BeforePUT(AContent: TMemoryStream); virtual;
    procedure AfterPUT; virtual;

    procedure BeforeDELETE; virtual;
    procedure AfterDELETE; virtual;

    procedure DoError(const AException: Exception; const AVerb: TMARSHttpVerb; const AAfterExecute: TMARSClientResponseProc); virtual;
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
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif}): string; {$ifndef DelphiXE2_UP}overload;{$endif}
    procedure POST(const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif});
    procedure PUT(const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const AAfterExecute: TMARSClientResponseProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif});
    procedure DELETE(const ABeforeExecute: TMARSClientProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AAfterExecute: TMARSClientProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif});
//    procedure PATCH(const ABeforeExecute: TMARSClientProc{$ifdef DelphiXE2_UP} = nil{$endif};
//      const AAfterExecute: TMARSClientProc{$ifdef DelphiXE2_UP} = nil{$endif};
//      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif});
//    procedure HEAD(const ABeforeExecute: TMARSClientProc{$ifdef DelphiXE2_UP} = nil{$endif};
//      const AAfterExecute: TMARSClientProc{$ifdef DelphiXE2_UP} = nil{$endif};
//      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif});
//    procedure OPTIONS(const ABeforeExecute: TMARSClientProc{$ifdef DelphiXE2_UP} = nil{$endif};
//      const AAfterExecute: TMARSClientProc{$ifdef DelphiXE2_UP} = nil{$endif};
//      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif});

    procedure GETAsync(const ACompletionHandler: TMARSClientProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif};
      ASynchronize: Boolean = True);
    procedure POSTAsync(
      const ABeforeExecute: TProc<TMemoryStream>{$ifdef DelphiXE2_UP} = nil{$endif};
      const ACompletionHandler: TMARSClientProc{$ifdef DelphiXE2_UP} = nil{$endif};
      const AOnException: TMARSClientExecptionProc{$ifdef DelphiXE2_UP} = nil{$endif};
      ASynchronize: Boolean = True);

    property Accept: string read GetAccept;
    property Application: TMARSClientApplication read GetApplication write FApplication;
    property AuthToken: string read GetAuthToken;
    property Client: TMARSClient read GetClient;
    property SpecificAccept: string read FSpecificAccept write FSpecificAccept;
    property SpecificClient: TMARSClient read FSpecificClient write FSpecificClient;
    property Resource: string read FResource write FResource;
    property Path: string read GetPath;
    property PathParamsValues: TStrings read FPathParamsValues write SetPathParamsValues;
    property QueryParams: TStrings read FQueryParams write SetQueryParams;
    property Token: TMARSClientCustomResource read FToken write FToken;
    property URL: string read GetURL;
  published
  end;


implementation

uses
    MARS.Core.URL
  , MARS.Core.Utils
  , MARS.Client.Token
  ;

{ TMARSClientCustomResource }

procedure TMARSClientCustomResource.AfterDELETE;
begin

end;

procedure TMARSClientCustomResource.AfterGET;
begin

end;

procedure TMARSClientCustomResource.AfterPOST;
begin

end;

procedure TMARSClientCustomResource.AfterPUT;
begin

end;

procedure TMARSClientCustomResource.BeforeDELETE;
begin

end;

procedure TMARSClientCustomResource.BeforeGET;
begin

end;

procedure TMARSClientCustomResource.BeforePOST(AContent: TMemoryStream);
begin

end;

procedure TMARSClientCustomResource.BeforePUT(AContent: TMemoryStream);
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
end;

function TMARSClientCustomResource.GetAuthToken: string;
begin
  Result := ''; // SpecificToken
  if (Result = '') and Assigned(Token) then
    Result := (Token as TMARSClientToken).Token;
end;

function TMARSClientCustomResource.GetClient: TMARSClient;
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

procedure TMARSClientCustomResource.DELETE(const ABeforeExecute,
  AAfterExecute: TMARSClientProc; const AOnException: TMARSClientExecptionProc);
var
  LResponseStream: TMemoryStream;
begin
  try
    BeforeDELETE();

    if Assigned(ABeforeExecute) then
      ABeforeExecute();

    LResponseStream := TMemoryStream.Create;
    try
      Client.Delete(URL, LResponseStream, AuthToken);

      AfterDELETE();

      if Assigned(AAfterExecute) then
        AAfterExecute();
    finally
      LResponseStream.Free;
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
                AAfterExecute();
            end);
    end;
  end;
end;

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
      Client.Get(URL, LResponseStream, Accept, AuthToken);

      AfterGET();

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

procedure TMARSClientCustomResource.GETAsync(
  const ACompletionHandler: TMARSClientProc;
  const AOnException: TMARSClientExecptionProc;
  ASynchronize: Boolean);
begin
  Client.ExecuteAsync(
    procedure
    begin
      GET(nil
        , procedure (AStream: TStream)
          begin
            if Assigned(ACompletionHandler) then
            begin
              if ASynchronize then
                TThread.Queue(nil, TThreadProcedure(ACompletionHandler))
              else
                ACompletionHandler();
            end;
          end
        , AOnException);
    end
  );
end;

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
        Client.Post(URL, LContent, LResponseStream, AuthToken);

        AfterPOST();

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

procedure TMARSClientCustomResource.POSTAsync(
  const ABeforeExecute: TProc<TMemoryStream>;
  const ACompletionHandler: TMARSClientProc;
  const AOnException: TMARSClientExecptionProc;
  ASynchronize: Boolean);
begin
  Client.ExecuteAsync(
    procedure
    begin
      POST(
        ABeforeExecute
      , procedure (AStream: TStream)
        begin
          if Assigned(ACompletionHandler) then
          begin
            if ASynchronize then
              TThread.Queue(nil, TThreadProcedure(ACompletionHandler))
            else
              ACompletionHandler();
          end;
        end
      , AOnException
      );
    end
  );
end;

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
        Client.Put(URL, LContent, LResponseStream, AuthToken);

        AfterPUT();

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

procedure TMARSClientCustomResource.SetPathParamsValues(const Value: TStrings);
begin
  FPathParamsValues.Assign(Value);
end;

procedure TMARSClientCustomResource.SetQueryParams(const Value: TStrings);
begin
  FQueryParams.Assign(Value);
end;

end.
