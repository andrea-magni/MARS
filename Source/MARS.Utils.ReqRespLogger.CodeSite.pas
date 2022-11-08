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
  System.StrUtils,
  System.Rtti,
  MARS.Core.Classes,
  MARS.Core.MediaType,
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
    FUserAgent: String;
    FPort: Integer;
    FRemoteIP: String;
    FHostName: String;
    FMethod: String;
    FQueryString: String;
    FCookies: String;
    FQueryFields: String;
    FContentFields: String;
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
    property UserAgent: String read FUserAgent;
    property RemoteIP: String read FRemoteIP;
    property QueryFields: String read FQueryFields;
    property ContentFields: String read FContentFields;
  end;

  TMARSResponseLogCodeSite = class
  private
    FStatusCode: Integer;
    FContentType: String;
    FContentEncoding: String;
    FContentLength: Integer;
  public
    constructor Create(AResponse: IMARSResponse);
  published
    property StatusCode: Integer read FStatusCode;
    property ContentType: String read FContentType;
    property ContentEncoding: String read FContentEncoding;
    property ContentLength: Integer read FContentLength;
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
    procedure Log(const AMessage: String; const ALogEntry: TObject); overload;
    procedure Log(const AMsgType: Integer; const ACategory, AMessage: String; const ALogEntry: TObject); overload;

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
    procedure (const AActivation: IMARSActivation; out AIsAllowed: Boolean)
    var
      LRequest: TMARSRequestLogCodeSite;
    begin
      if not AActivation.Engine.Parameters.ByName('CodeSiteLogging.Enabled').AsBoolean then
        Exit;

      LRequest := TMARSRequestLogCodeSite.Create(AActivation.Request);
      try
        TMARSReqRespLoggerCodeSite.Instance.Log(csmOrange, 'Request',
          String.Join(LOGFIELD_SEPARATOR,
          [
            'Incoming',
            'Engine: ' + AActivation.Engine.Name,
            'Application: ' + AActivation.Application.Name,
            'Verb: ' + AActivation.Request.Method,
            'Path: ' + AActivation.URL.Path
          ]), LRequest
        );
      finally
        LRequest.Free;
      end;
    end
  );

  TMARSActivation.RegisterAfterInvoke(
    procedure (const AActivation: IMARSActivation)
    var
      LResponse: TMARSResponseLogCodeSite;
    begin
      if not AActivation.Engine.Parameters.ByName('CodeSiteLogging.Enabled').AsBoolean then
        Exit;

      LResponse := TMARSResponseLogCodeSite.Create(AActivation.Response);
      try
        TMARSReqRespLoggerCodeSite.Instance.Log(csmGreen, 'Response',
          String.Join(LOGFIELD_SEPARATOR,
          [
            'Outgoing',
            'Time: ' + AActivation.InvocationTime.ElapsedMilliseconds.ToString + ' ms',
            'Engine: ' + AActivation.Engine.Name,
            'Application: ' + AActivation.Application.Name,
            'Resource: ' + IfThen(Assigned(AActivation.ResourceInstance), AActivation.ResourceInstance.ClassName, 'Unknown'),
            'Method: ' + IfThen(Assigned(AActivation.Method), AActivation.Method.Name, 'Unknown')
            ]
          ), LResponse
        );
      finally
        LResponse.Free;
      end;
    end
  );

  TMARSActivation.RegisterInvokeError(
    procedure (const AActivation: IMARSActivation; const AException: Exception; var AHandled: Boolean)
    begin
      CodeSite.Category := '';
      CodeSite.SendException(
          String.Join(LOGFIELD_SEPARATOR,
          [
            'Exception: ' + AException.ClassName,
            'Engine: ' + AActivation.Engine.Name,
            'Application: ' + AActivation.Application.Name,
            'Verb: ' + AActivation.Request.Method,
            'Path: ' + AActivation.URL.Path,
            'Method: ' + IfThen(Assigned(AActivation.Method), AActivation.Method.Name, 'Unknown')
          ]) + ' : ', AException);
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
  CodeSite.Category := '';
  CodeSite.Send(csmCheckPoint, 'Logger started');
end;

destructor TMARSReqRespLoggerCodeSite.Destroy;
begin
  CodeSite.Category := '';
  CodeSite.Send(csmCheckPoint, 'Logger stopped');
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

procedure TMARSReqRespLoggerCodeSite.Log(const AMsgType: Integer; const ACategory, AMessage: String;
  const ALogEntry: TObject);
begin
  CodeSite.Category := ACategory;
  CodeSite.Send(AMsgType, AMessage, ALogEntry);
end;

procedure TMARSReqRespLoggerCodeSite.Log(const AMessage: String; const ALogEntry: TObject);
begin
  Log(csmInfo, '', AMessage, ALogEntry);
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
  FCookies := ARequest.GetHeaderParamValue('Cookie');
  FAcceptEncoding := ARequest.GetHeaderParamValue('Accept-Encoding');
  FUserAgent := ARequest.GetHeaderParamValue('User-Agent');
  FContentType := ARequest.GetHeaderParamValue('Content-Type');
  FContentEncoding := ARequest.GetHeaderParamValue('Content-Encoding');
  FContentLength := ARequest.GetHeaderParamValue('Content-Length');
  FQueryFields := String.Join(',', ARequest.QueryFields);
  FRemoteIP := ARequest.RemoteIP;
  if FContentType.StartsWith(TMediaType.MULTIPART_FORM_DATA, True) or
     FContentType.StartsWith(TMediaType.APPLICATION_FORM_URLENCODED_TYPE, True) then
    FContentFields := String.Join(',', ARequest.ContentFields);
end;

{ TMARSResponseLoggerCodeSite }

constructor TMARSResponseLogCodeSite.Create(AResponse: IMARSResponse);
begin
  FStatusCode := AResponse.StatusCode;
  FContentEncoding := AResponse.ContentEncoding;
  FContentType := AResponse.ContentType;
  FContentLength := AResponse.ContentLength;
end;

initialization
  TMARSReqRespLoggerCodeSite.Instance;

end.
