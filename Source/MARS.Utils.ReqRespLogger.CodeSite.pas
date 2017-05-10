(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Utils.ReqRespLogger.CodeSite;

interface

uses
  Classes, SysUtils, Diagnostics, Rtti
  , MARS.Core.Classes
  , MARS.Core.Activation
  , MARS.Core.Activation.Interfaces, MARS.Utils.ReqRespLogger.Interfaces
  , Web.HttpApp
;

type
  TMARSReqRespLoggerCodeSite=class(TInterfacedObject, IMARSReqRespLogger)
  private
    class var _Instance: TMARSReqRespLoggerCodeSite;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // IMARSReqRespLogger
    procedure Clear;
    function GetLogBuffer: TValue;
    procedure Log(const AMessage: string);

    class function Instance: TMARSReqRespLoggerCodeSite;
    class constructor ClassCreate;
    class destructor ClassDestroy;
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

class constructor TMARSReqRespLoggerCodeSite.ClassCreate;
begin
  TMARSActivation.RegisterBeforeInvoke(
    procedure (const AR: IMARSActivation; out AIsAllowed: Boolean)
    begin
      TMARSReqRespLoggerCodeSite.Instance.Log(
        string.Join(LOGFIELD_SEPARATOR
        , [
           'Incoming'
          , AR.Request.ToLogString
          , 'Engine: ' + AR.Engine.Name
          , 'Application: ' + AR.Application.Name
          ]
        )
      );
    end
  );

  TMARSActivation.RegisterAfterInvoke(
    procedure (const AR: IMARSActivation)
    begin
      TMARSReqRespLoggerCodeSite.Instance.Log(
        string.Join(LOGFIELD_SEPARATOR
        , [
            'Outgoing'
          , AR.Response.ToLogString
          , 'Time: ' + AR.InvocationTime.ElapsedMilliseconds.ToString + ' ms'
          , 'Engine: ' + AR.Engine.Name
          , 'Application: ' + AR.Application.Name
          ]
        )
      );
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

procedure TMARSReqRespLoggerCodeSite.Log(const AMessage: string);
begin
  CodeSite.SendMsg(AMessage);
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
