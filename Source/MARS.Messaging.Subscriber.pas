(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Messaging.Subscriber;

interface

uses
  Classes, SysUtils

  , MARS.Messaging.Message
  , MARS.Core.Classes

  ;

type
  IMARSMessageSubscriber = interface
    procedure OnMessage(AMessage: TMARSMessage);
  end;

  TMARSAnonymousSubscriber<T: TMARSCustomMessage> = class(TNonInterfacedObject, IMARSMessageSubscriber)
  private
    FProc: TProc<T>;
  protected
  public
    constructor Create(const AProc: TProc<T>); virtual;

    procedure OnMessage(AMessage: TMARSMessage);
  end;


implementation

{ TMARSAnonymousSubscriber }

constructor TMARSAnonymousSubscriber<T>.Create(const AProc: TProc<T>);
begin
  inherited Create;
  FProc := AProc;
end;

procedure TMARSAnonymousSubscriber<T>.OnMessage(AMessage: TMARSMessage);
begin
  if Assigned(FProc) and (AMessage is T) then
  begin
    FProc(AMessage as T);
  end;
end;

end.
