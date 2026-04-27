(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.ServerSideEvents.MessageBodyWriters;

interface

uses
  Classes, SysUtils, Rtti, TypInfo

  , Web.HttpApp

  , MARS.Core.Attributes, MARS.Core.Declarations, MARS.Core.MediaType
  , MARS.Core.MessageBodyWriter
  , MARS.Core.Activation.Interfaces
  , MARS.Core.ServerSideEvents
;

type
  [Produces(TMediaType.TEXT_EVENT_STREAM), Produces(TMediaType.WILDCARD)]
  TServerSideEventWriter = class(TInterfacedObject, IMessageBodyWriter, IMessageServerSideEventsProvider)
  public
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType; AOutputStream: TStream;
      const AActivation: IMARSActivation);
    procedure GenerateEvents(const AValue: TValue;
      const AMediaType: TMediaType; const AActivation: IMARSActivation);
  end;

implementation

uses
  MARS.Core.Registry
, MARS.Core.Exceptions
, MARS.http.Server.Indy
;

{ TServerSideEventWriter }

procedure TServerSideEventWriter.GenerateEvents(const AValue: TValue;
  const AMediaType: TMediaType; const AActivation: IMARSActivation);
var
  LResponse: TWebResponse;
  LResponseStream: TWebResponseStream;
  LServerSideEvent: TMARSServerSideEvent;
  LKeepAliveTimeout: Integer;
begin
  LResponse := TMARSWebResponse(AActivation.Response).WebResponse;  // hard-cast
  LServerSideEvent := AValue.AsType<TMARSServerSideEvent>;
  LKeepAliveTimeout := 15000; // default
  if LServerSideEvent.KeepAliveTimeout <> 0 then
    LKeepAliveTimeout := LServerSideEvent.KeepAliveTimeout;

  LResponseStream := TWebResponseStream.BeginEventsStream(LResponse, LKeepAliveTimeout);
  if Assigned(LServerSideEvent.Worker) then
    LServerSideEvent.Worker(LResponseStream);
end;

procedure TServerSideEventWriter.WriteTo(const AValue: TValue;
  const AMediaType: TMediaType; AOutputStream: TStream;
  const AActivation: IMARSActivation);
begin
  raise EMARSException.Create('Cannot write events to the stream, please use IMessageServerSideEventsProvider');
end;

initialization
  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TServerSideEventWriter
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := (AType.Handle = TypeInfo(TMARSServerSideEvent));
    end
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TMARSMessageBodyRegistry.AFFINITY_MEDIUM;
    end
  );


end.
