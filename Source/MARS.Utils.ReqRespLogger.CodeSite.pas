(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

{$M+}
unit MARS.Utils.ReqRespLogger.CodeSite;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Diagnostics,
  System.Rtti,
  MARS.Core.Classes,
  MARS.Core.Activation,
  MARS.Core.Activation.Interfaces,
  MARS.Utils.ReqRespLogger.Interfaces,
  MARS.Core.RequestAndResponse.Interfaces,
  CodeSiteLogging;

type

  TMARSRequestLogCodeSite = class
  private
    FAccept: String;
    FAcceptEncoding: String;
    FPathInfo: String;
    FAuthorization: String;
    FContentEncoding: String;
    FContentType: String;
    FContentLength: String;
    FContent: String;
    FUserAgent: String;
    FPort: Integer;
    FRemoteAddr: String;
    FHostName: String;
    FMethod: String;
    FQueryString: String;
    FCookies: String;
  public
    constructor Create(ARequest: IMARSRequest);
  published
    property Accept: String read FAccept;
    property AcceptEncoding: String read FAcceptEncoding;
    property Authorization: String read FAuthorization;
    property HostName: String read FHostName;
    property Port: Integer read FPort;
    property PathInfo: String read FPathInfo;
    property Method: String read FMethod;
    property QueryString: String read FQueryString;
    property Cookies: String read FCookies;
    property ContentEncoding: String read FContentEncoding;
    property ContentType: String read FContentType;
    property ContentLength: String read FContentLength;
    property Content: String read FContent;
    property UserAgent: String read FUserAgent;
    property RemoteAddr: String read FRemoteAddr;
  end;

  TMARSResponseLogCodeSite = class
  private
    FStatusCode: Integer;
    FContentType: String;
    FContentEncoding: String;
  public
    constructor Create(AResponse: IMARSResponse);
  published
    property StatusCode: Integer read FStatusCode;
    property ContentType: String read FContentType;
    property ContentEncoding: String read FContentEncoding;
  end;

  TMARSReqRespLoggerCodeSite = class(TInterfacedObject, IMARSReqRespLogger)
  private
    class var _Instance: TMARSReqRespLoggerCodeSite;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // IMARSReqRespLogger
    procedure Clear;
    function GetLogBuffer: TValue;
    procedure Log(const AMessage: String; const ALogEntry: TObject);

    class function Instance: TMARSReqRespLoggerCodeSite;
    class constructor ClassCreate;
    class destructor ClassDestroy;
  end;


implementation

const
  LOGFIELD_SEPARATOR = ' | ';

{ TMARSReqRespLoggerCodeSite }

class constructor TMARSReqRespLoggerCodeSite.ClassCreate;
begin
  TMARSActivation.RegisterBeforeInvoke(
    procedure (const AR: IMARSActivation; out AIsAllowed: Boolean)
    var
      LRequest: TMARSRequestLogCodeSite;
    begin
      if not AR.Engine.Parameters.ByName('CodeSiteLogging.Enabled').AsBoolean then
        Exit;

      LRequest := TMARSRequestLogCodeSite.Create(AR.Request);
      try
        TMARSReqRespLoggerCodeSite.Instance.Log(
          String.Join(LOGFIELD_SEPARATOR,
            [
             'Incoming',
             'Engine: ' + AR.Engine.Name,
             'Application: ' + AR.Application.Name
            ]
          ), LRequest
        );
      finally
        LRequest.Free;
      end;
    end
  );

  TMARSActivation.RegisterAfterInvoke(
    procedure (const AR: IMARSActivation)
    var
      LResponse: TMARSResponseLogCodeSite;
    begin
      if not AR.Engine.Parameters.ByName('CodeSiteLogging.Enabled').AsBoolean then
        Exit;

      LResponse := TMARSResponseLogCodeSite.Create(AR.Response);
      try
        TMARSReqRespLoggerCodeSite.Instance.Log(
          string.Join(LOGFIELD_SEPARATOR
          , [
              'Outgoing'
            , 'Time: ' + AR.InvocationTime.ElapsedMilliseconds.ToString + ' ms'
            , 'Engine: ' + AR.Engine.Name
            , 'Application: ' + AR.Application.Name
            ]
          ), LResponse
        );
      finally
        LResponse.Free;
      end;
    end
  );
end;

class destructor TMARSReqRespLoggerCodeSite.ClassDestroy;
begin
  if Assigned(_Instance) then
    FreeAndNil(_Instance);
end;

procedure TMARSReqRespLoggerCodeSite.Clear;
begin
  CodeSite.Clear;
end;

constructor TMARSReqRespLoggerCodeSite.Create;
begin
  inherited Create;
  CodeSite.SendMsg('Logger started');
end;

destructor TMARSReqRespLoggerCodeSite.Destroy;
begin
  CodeSite.SendMsg('Logger stopped');
  inherited;
end;

function TMARSReqRespLoggerCodeSite.GetLogBuffer: TValue;
begin
  Result := TValue.Empty;
end;

class function TMARSReqRespLoggerCodeSite.Instance: TMARSReqRespLoggerCodeSite;
begin
  if not Assigned(_Instance) then
    _Instance := TMARSReqRespLoggerCodeSite.Create;
  Result := _Instance;
end;

procedure TMARSReqRespLoggerCodeSite.Log(const AMessage: String; const ALogEntry: TObject);
begin
  CodeSite.Send(AMessage, ALogEntry);
end;

{ TMARSRequestLoggerCodeSite }

constructor TMARSRequestLogCodeSite.Create(ARequest: IMARSRequest);
begin
  FAccept := ARequest.Accept;
  FAuthorization := ARequest.Authorization;
  FHostName := ARequest.HostName;
  FPort := ARequest.Port;
  FMethod := ARequest.Method;
  FPathInfo := ARequest.RawPath;
  FQueryString := ARequest.QueryString;
  FContent := ARequest.Content;
  FCookies := ARequest.GetHeaderParamValue('Cookie');
  FAcceptEncoding := ARequest.GetHeaderParamValue('Accept-Encoding');
  FUserAgent := ARequest.GetHeaderParamValue('User-Agent');
  FContentType := ARequest.GetHeaderParamValue('Content-Type');
  FContentEncoding := ARequest.GetHeaderParamValue('Content-Encoding');
  FContentLength := ARequest.GetHeaderParamValue('Content-Length');
end;

{ TMARSResponseLoggerCodeSite }

constructor TMARSResponseLogCodeSite.Create(AResponse: IMARSResponse);
begin
  FStatusCode := AResponse.StatusCode;
  FContentEncoding := AResponse.ContentEncoding;
  FContentType := AResponse.ContentType;
end;

initialization
  TMARSReqRespLoggerCodeSite.Instance;

end.
