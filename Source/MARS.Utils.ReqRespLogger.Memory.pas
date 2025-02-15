(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Utils.ReqRespLogger.Memory;

interface

uses
  Classes, SysUtils, Diagnostics
  , Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.Comp.DataSet
//  , MARS.Core.Engine, MARS.Core.Application
  , MARS.Utils.ReqRespLogger.Interfaces
  , Web.HttpApp, SyncObjs, Rtti
  , MARS.Core.Activation
  , MARS.Core.Activation.Interfaces
  , MARS.Core.RequestAndResponse.Interfaces, MARS.http.Server.Indy
;

type
  TMARSReqRespLoggerMemory=class(TInterfacedObject, IMARSReqRespLogger)
  private
    FMemory: TFDMemTable;
    FCriticalSection: TCriticalSection;
    class var _Instance: TMARSReqRespLoggerMemory;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // IMARSReqRespLogger
    procedure Clear;
    function GetLogBuffer: TValue;
    procedure LogIncoming(const AR: IMARSActivation);
    procedure LogOutgoing(const AR: IMARSActivation);

    // protected access to FMemory from outside
    procedure ReadMemory(const AProc: TProc<TDataset>);
    function GetMemorySnapshot: TDataset;

    class function Instance: TMARSReqRespLoggerMemory;
    class constructor ClassCreate;
    class destructor ClassDestroy;
  end;

  TMARSWebRequestHelper = class helper for TMARSWebRequest
  public
    procedure ToDataSet(const ADataSet: TDataSet);
  end;

  TMARSWebResponseHelper = class helper for TMARSWebResponse
  public
    procedure ToDataSet(const ADataSet: TDataSet);
  end;


implementation
uses
  MARS.Core.Utils
, MARS.Core.Attributes
, MARS.Rtti.Utils
;

{ TMARSReqRespLoggerMemory }

class constructor TMARSReqRespLoggerMemory.ClassCreate;
begin
  TMARSActivation.RegisterBeforeInvoke(
    procedure (const AR: IMARSActivation; out AIsAllowed: Boolean)
    begin
      TMARSReqRespLoggerMemory.Instance.LogIncoming(AR);
    end
  );

  TMARSActivation.RegisterAfterInvoke(
    procedure (const AR: IMARSActivation)
    begin
      TMARSReqRespLoggerMemory.Instance.LogOutgoing(AR);
    end
  );
end;

class destructor TMARSReqRespLoggerMemory.ClassDestroy;
begin
  if Assigned( _Instance ) then
    _Instance.Free;

  _Instance := nil;
end;

procedure TMARSReqRespLoggerMemory.Clear;
begin
  FCriticalSection.Enter;
  try
    FMemory.EmptyDataSet;
  finally
    FCriticalSection.Leave;
  end;
end;

constructor TMARSReqRespLoggerMemory.Create;
begin
  inherited Create;
  FCriticalSection := TCriticalSection.Create;

  FCriticalSection.Enter;
  try
    FMemory := TFDMemTable.Create(nil);
    try
      FMemory.FieldDefs.Add('TimeStamp', ftTimeStamp);
      FMemory.FieldDefs.Add('Engine', ftString, 255);
      FMemory.FieldDefs.Add('Application', ftString, 255);
      FMemory.FieldDefs.Add('ActivationID', ftString, 100);
      FMemory.FieldDefs.Add('TokenClaims', ftString, 1024); // 1K
      FMemory.FieldDefs.Add('Kind', ftString, 100);
      FMemory.FieldDefs.Add('Verb', ftString, 100);
      FMemory.FieldDefs.Add('Path', ftString, 2048); // 2K
      FMemory.FieldDefs.Add('StatusCode', ftInteger);
      FMemory.FieldDefs.Add('StatusText', ftString, 200);
      FMemory.FieldDefs.Add('ContentType', ftString, 200);
      FMemory.FieldDefs.Add('ContentSize', ftInteger);
      FMemory.FieldDefs.Add('CookieCount', ftInteger);
      FMemory.FieldDefs.Add('Cookie', ftMemo);
      FMemory.FieldDefs.Add('Content', ftMemo);
      FMemory.FieldDefs.Add('Query', ftMemo);
      FMemory.FieldDefs.Add('RemoteIP', ftString, 100);
      FMemory.FieldDefs.Add('RemoteAddr', ftString, 255);
      FMemory.FieldDefs.Add('RemoteHost', ftString, 255);
      FMemory.FieldDefs.Add('ExecutionTime', ftInteger);
      FMemory.CreateDataSet;
    except
      FMemory.Free;
      raise;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

destructor TMARSReqRespLoggerMemory.Destroy;
begin
  FCriticalSection.Enter;
  try
    FMemory.Free;
  finally
    FCriticalSection.Leave;
  end;
  FCriticalSection.Free;
  inherited;
end;

function TMARSReqRespLoggerMemory.GetLogBuffer: TValue;
begin
  Result := TValue.From<TDataSet>(GetMemorySnapshot);
end;

function TMARSReqRespLoggerMemory.GetMemorySnapshot: TDataset;
var
  LSnapshot: TFDMemTable;
begin
  LSnapshot := TFDMemTable.Create(nil);
  try
    ReadMemory(
      procedure (AMemory: TDataSet)
      begin
        LSnapShot.Data := (AMemory as TFDMemTable).Data;
      end
    );
    Result := LSnapshot;
  except
    LSnapshot.Free;
    raise;
  end;
end;

class function TMARSReqRespLoggerMemory.Instance: TMARSReqRespLoggerMemory;
begin
  if not Assigned(_Instance) then
    _Instance := TMARSReqRespLoggerMemory.Create;
  Result := _Instance;
end;

procedure TMARSReqRespLoggerMemory.LogIncoming(const AR: IMARSActivation);
begin
  if AR.Resource.HasAttribute<NoLogAttribute> or
     AR.Method.HasAttribute<NoLogAttribute> then
    Exit;

  FCriticalSection.Enter;
  try
    FMemory.Append;
    try
      FMemory.FieldByName('TimeStamp').AsDateTime := Now;
      FMemory.FieldByName('Engine').AsString := AR.Engine.Name;
      FMemory.FieldByName('Application').AsString := AR.Application.Name;
      FMemory.FieldByName('ActivationID').AsString := AR.Id;
      FMemory.FieldByName('Kind').AsString := 'Incoming';

      if Assigned( AR.Token ) then
        FMemory.FieldByName('TokenClaims').AsString := AR.Token.Claims.ToString;

      (AR.Request as TMARSWebRequest).ToDataSet(FMemory);
      FMemory.Post;
    except
      FMemory.Cancel;
      raise;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TMARSReqRespLoggerMemory.LogOutgoing(const AR: IMARSActivation);
begin
  if AR.Resource.HasAttribute<NoLogAttribute> or
     AR.Method.HasAttribute<NoLogAttribute> then
    Exit;

  FCriticalSection.Enter;
  try
    FMemory.Append;
    try
      FMemory.FieldByName('TimeStamp').AsDateTime := Now;
      FMemory.FieldByName('Engine').AsString := AR.Engine.Name;
      FMemory.FieldByName('Application').AsString := AR.Application.Name;
      FMemory.FieldByName('ActivationID').AsString := AR.Id;
      FMemory.FieldByName('Kind').AsString := 'Outgoing';
      FMemory.FieldByName('ExecutionTime').AsInteger := AR.InvocationTime.ElapsedMilliseconds;

      if Assigned( AR.Token ) then
        FMemory.FieldByName('TokenClaims').AsString := AR.Token.Claims.ToString;

      (AR.Response as TMARSWebResponse).ToDataSet(FMemory);
      FMemory.Post;
    except
      FMemory.Cancel;
      raise;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TMARSReqRespLoggerMemory.ReadMemory(const AProc: TProc<TDataset>);
begin
  FCriticalSection.Enter;
  try
    if Assigned(AProc) then
      AProc(FMemory);
  finally
    FCriticalSection.Leave;
  end;
end;

{ TWebRequestHelper }

procedure TMARSWebRequestHelper.ToDataSet(const ADataSet: TDataSet);
begin
  ADataSet.FieldByName('Verb').AsString := GetMethod;
  ADataSet.FieldByName('Path').AsString := GetRawPath;
  ADataSet.FieldByName('Query').AsString := StringArrayToString( GetQueryFields, sLineBreak );
  ADataSet.FieldByName('RemoteIP').AsString := GetRemoteIP;
  ADataSet.FieldByName('RemoteAddr').AsString := WebRequest.RemoteAddr;
  ADataSet.FieldByName('RemoteHost').AsString := WebRequest.RemoteHost;

  ADataSet.FieldByName('StatusCode').Clear;
  ADataSet.FieldByName('StatusText').Clear;

  try
    ADataSet.FieldByName('Content').AsString := GetContent;
  except on E: EEncodingError do
    ADataSet.FieldByName('Content').AsString := E.ToString;
  end;

  ADataSet.FieldByName('ContentType').AsString := WebRequest.ContentType;
  ADataSet.FieldByName('ContentSize').AsInteger := WebRequest.ContentLength;
  ADataSet.FieldByName('CookieCount').AsInteger := GetCookieParamCount;
  ADataSet.FieldByName('Cookie').AsString := WebRequest.CookieFields.Text;
end;

{ TWebResponseHelper }

procedure TMARSWebResponseHelper.ToDataSet(const ADataSet: TDataSet);
var
  LCookie: TCookie;
  LCookies: string;
  LCookieIndex: Integer;
  LContentString: string;
  LContentStream: TStream;
begin
  //from request
  ADataSet.FieldByName('Verb').AsString := WebResponse.HTTPRequest.Method;
  ADataSet.FieldByName('Path').AsString := WebResponse.HTTPRequest.RawPathInfo;
  ADataSet.FieldByName('Query').AsString := WebResponse.HTTPRequest.QueryFields.Text;
  ADataSet.FieldByName('RemoteIP').AsString := WebResponse.HTTPRequest.RemoteIP;
  ADataSet.FieldByName('RemoteAddr').AsString := WebResponse.HTTPRequest.RemoteAddr;
  ADataSet.FieldByName('RemoteHost').AsString := WebResponse.HTTPRequest.RemoteHost;

  ADataSet.FieldByName('StatusCode').Clear;
  ADataSet.FieldByName('StatusText').Clear;

  //response
  LContentString := '';
  LContentStream := GetContentStream;
  if Assigned(LContentStream) then
  begin
    ADataSet.FieldByName('ContentSize').AsInteger := GetContentLength;

    try
      LContentString := StreamToString(LContentStream);
    except on E:EEncodingError do
      LContentString := E.ToString;
    end;
    LContentStream.Position := 0;

    ADataSet.FieldByName('Content').AsString := LContentString;
  end;

  ADataSet.FieldByName('StatusCode').AsInteger := GetStatusCode;
  ADataSet.FieldByName('StatusText').AsString := WebResponse.ReasonString;
  ADataSet.FieldByName('ContentType').AsString := GetContentType;

  ADataSet.FieldByName('CookieCount').AsInteger := WebResponse.Cookies.Count;

  LCookies := '';
  for LCookieIndex := 0 to WebResponse.Cookies.Count-1 do
  begin
    LCookie := WebResponse.Cookies.Items[LCookieIndex];
    LCookies := LCookie.Name + '=' + LCookie.Value + sLineBreak;
  end;
  ADataSet.FieldByName('Cookie').AsString := LCookies;
end;

end.
