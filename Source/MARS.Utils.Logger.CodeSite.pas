unit MARS.Utils.Logger.CodeSite;

interface

uses
  Classes, SysUtils, Diagnostics
  , MARS.Core.Engine
  , MARS.Core.Application
  , Web.HttpApp
;

type
  TMARSLoggerCodeSite=class(TInterfacedObject, IMARSHandleRequestEventListener)
  private
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // IMARSHandleRequestEventListener
    procedure AfterHandleRequest(const ASender: TMARSEngine;
      const AApplication: TMARSApplication; const AStopWatch: TStopwatch);
    procedure BeforeHandleRequest(const ASender: TMARSEngine;
      const AApplication: TMARSApplication; var AIsAllowed: Boolean);
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

{ TMARSLoggerCodeSite }

procedure TMARSLoggerCodeSite.AfterHandleRequest(const ASender: TMARSEngine;
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

procedure TMARSLoggerCodeSite.BeforeHandleRequest(const ASender: TMARSEngine;
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

constructor TMARSLoggerCodeSite.Create;
begin
  inherited Create;
  CodeSite.SendMsg('Logger started');
end;

destructor TMARSLoggerCodeSite.Destroy;
begin
  CodeSite.SendMsg('Logger stopped');
  inherited;
end;

{ TWebRequestHelper }

function TWebRequestHelper.ToLogString: string;
begin
  Result := string.Join(LOGFIELD_SEPARATOR
  , [
        Method
      , PathInfo
      , 'Content: [' + ContentFields.Text + ']'
      , 'Cookie: ['  + CookieFields.Text  + ']'
      , 'Query: ['   + QueryFields.Text   + ']'
      , 'Length: '  + Length(RawContent).ToString
      , 'RemoteIP: ' + RemoteIP
      , 'RemoteAddress: ' + RemoteAddr
      , 'RemoteHost: ' + RemoteHost
    ]
  );
end;

{ TWebResponseHelper }

function TWebResponseHelper.ToLogString: string;
begin
  Result := string.Join(LOGFIELD_SEPARATOR
    , [
        HTTPRequest.Method
      , HTTPRequest.PathInfo
      , 'StatusCode: ' + StatusCode.ToString
      , 'ReasonString: ' + ReasonString
      , 'ContentType: ' + ContentType
      , 'Content.Size: ' + ContentStream.Size.ToString
      , 'Cookies.Count: '  + Self.Cookies.Count.ToString
    ]
  );
end;

end.
