(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Utils.ReqRespLogger.CodeSite;

interface

uses
  Classes, SysUtils, Diagnostics, Rtti
  , MARS.Core.Engine
  , MARS.Core.Application, MARS.Utils.ReqRespLogger.Interfaces
  , Web.HttpApp
;

type
  TMARSReqRespLoggerCodeSite=class(TInterfacedObject
    , IMARSHandleRequestEventListener, IMARSReqRespLogger)
  private
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // IMARSHandleRequestEventListener
    procedure AfterHandleRequest(const ASender: TMARSEngine;
      const AApplication: TMARSApplication; const AStopWatch: TStopwatch);
    procedure BeforeHandleRequest(const ASender: TMARSEngine;
      const AApplication: TMARSApplication; var AIsAllowed: Boolean);

    // IMARSReqRespLogger
    procedure Clear;
    function GetLogBuffer: TValue;
  end;

  TWebRequestHelper = class helper for TWebRequest
  public
    function ToLogString: string;
  end;

  TWebResponseHelper = class helper for TWebResponse
  public
    function ToLogString: string;
  end;


implementation

uses
  CodeSiteLogging
;

const
  LOGFIELD_SEPARATOR = ' | ';

{ TMARSReqRespLoggerCodeSite }

procedure TMARSReqRespLoggerCodeSite.AfterHandleRequest(const ASender: TMARSEngine;
  const AApplication: TMARSApplication; const AStopWatch: TStopwatch);
begin
  CodeSite.SendMsg(
    string.Join(LOGFIELD_SEPARATOR
    , [
        'Outgoing'
      , ASender.CurrentResponse.ToLogString
      , 'Time: ' + AStopWatch.ElapsedMilliseconds.ToString + ' ms'
      , 'Engine: ' + ASender.Name
      , 'Application: ' + AApplication.Name
      ]
    )
  );
end;

procedure TMARSReqRespLoggerCodeSite.BeforeHandleRequest(const ASender: TMARSEngine;
  const AApplication: TMARSApplication; var AIsAllowed: Boolean);
begin
  CodeSite.SendMsg(
    string.Join(LOGFIELD_SEPARATOR
    , [
       'Incoming'
      , ASender.CurrentRequest.ToLogString
      , 'Engine: ' + ASender.Name
      , 'Application: ' + AApplication.Name
      ]
    )
  );
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

{ TWebRequestHelper }

function TWebRequestHelper.ToLogString: string;
begin
  Result := string.Join(LOGFIELD_SEPARATOR
  , [
        Method
      , PathInfo
      , 'Content: [' + Content + ']'
      , 'Cookie: ['  + CookieFields.Text  + ']'
      , 'Query: ['   + QueryFields.Text   + ']'
      , 'Length: '  + Length(Content).ToString
      , 'RemoteIP: ' + RemoteIP
      , 'RemoteAddress: ' + RemoteAddr
      , 'RemoteHost: ' + RemoteHost
    ]
  );
end;

{ TWebResponseHelper }

function TWebResponseHelper.ToLogString: string;
var
  LContentSize: Int64;
begin
  if Assigned(ContentStream) then
    LContentSize := ContentStream.Size
  else
    LContentSize := 0;


  Result := string.Join(LOGFIELD_SEPARATOR
    , [
        HTTPRequest.Method
      , HTTPRequest.PathInfo
      , 'StatusCode: ' + StatusCode.ToString
      , 'ReasonString: ' + ReasonString
      , 'ContentType: ' + ContentType
      , 'Content.Size: ' + LContentSize.ToString
      , 'Content: [' + Content + ']'
      , 'Cookies.Count: '  + Cookies.Count.ToString
    ]
  );
end;

end.
