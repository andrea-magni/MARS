(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Resources.HelloWorld;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.URL
, MARS.Core.JSON, MARS.Core.Response
, MARS.Core.RequestAndResponse.Interfaces
, MARS.Core.ServerSideEvents, Web.HTTPApp
//, MARS.Core.Token
;

type

  [Path('helloworld')]
  THelloWorldResource = class
    type
      TMyEventPayload = record
        sequence: UInt64;
        timeStamp: TDateTime;
        procedure Update;
      end;
  protected
  public
    [GET, Produces(TMediaType.TEXT_EVENT_STREAM)]
    function SayHelloWorld: TMARSServerSideEvent;
  end;

implementation

uses
  System.Rtti
, MARS.Core.Registry
, MARS.Core.Utils
, MARS.Core.Exceptions
, CodeSiteLogging
;

{ THelloWorldResource }

function THelloWorldResource.SayHelloWorld: TMARSServerSideEvent;
begin
  Result := TMARSServerSideEvent.Create(
    procedure (AStream: TWebResponseStream)
    var
      LPayload: TMyEventPayload;
    begin
      LPayload := Default(TMyEventPayload);

      try
        while AStream.Connected do
        begin
          LPayload.Update;

          AStream.WriteEvent('heartbeat');
          AStream.Write<TMyEventPayload>(LPayload.sequence.ToString, LPayload);
          AStream.EndEvent;

          Sleep(250);
        end;
      except on E: Exception do
//        CodeSite.SendException(E);
      end;
    end
  );
end;

{ THelloWorldResource.TMyEventPayload }

procedure THelloWorldResource.TMyEventPayload.Update;
begin
  Inc(sequence);
  timeStamp := Now;
end;

initialization
  MARSRegister(THelloWorldResource);

end.
