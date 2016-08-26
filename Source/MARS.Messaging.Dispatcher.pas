(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Messaging.Dispatcher;

{$I MARS.inc}

interface

uses
  Classes, SysUtils
  , Rtti
  , Generics.Collections
{$ifdef DelphiXE7_UP}
  , Threading
{$endif}
  , SyncObjs

  , MARS.Core.Utils
  , MARS.Messaging.Message
  , MARS.Messaging.Subscriber
  ;

type
  TMARSMessageDispatcher = class
  private
    FSubscribers: TList<IMARSMessageSubscriber>;
    FQueue: TThreadedQueue<TMARSMessage>;
    FCriticalSection: TCriticalSection;
{$ifdef DelphiXE7_UP}
    FWorkerTask: ITask;
{$endif}
  protected
    class var _Instance: TMARSMessageDispatcher;
    class function GetInstance: TMARSMessageDispatcher; static;

    procedure DoRegisterSubscriber(const ASubscriber: IMARSMessageSubscriber); virtual;
    procedure DoUnRegisterSubscriber(const ASubscriber: IMARSMessageSubscriber); virtual;

    property Subscribers: TList<IMARSMessageSubscriber> read FSubscribers;
  public
    const MESSAGE_QUEUE_DEPTH = 100;
    constructor Create; virtual;
    destructor Destroy; override;

    function Enqueue(AMessage: TMARSMessage): Integer;
    procedure RegisterSubscriber(const ASubscriber: IMARSMessageSubscriber);
    procedure UnRegisterSubscriber(const ASubscriber: IMARSMessageSubscriber);

    class property Instance: TMARSMessageDispatcher read GetInstance;
    class destructor ClassDestructor;
  end;


implementation

{ TMARSMessageDispatcher }

class destructor TMARSMessageDispatcher.ClassDestructor;
begin
  if Assigned(_Instance) then
    FreeAndNil(_Instance);
end;

constructor TMARSMessageDispatcher.Create;
begin
  inherited Create();

  FSubscribers := TList<IMARSMessageSubscriber>.Create;
  FQueue := TThreadedQueue<TMARSMessage>.Create(MESSAGE_QUEUE_DEPTH);
  FCriticalSection := TCriticalSection.Create;

{$ifdef DelphiXE7_UP}
  FWorkerTask := TTask.Create(
    procedure
    var
      LMessage: TMARSMessage;
      LSubscriber: IMARSMessageSubscriber;
    begin
      while TTask.CurrentTask.Status = TTaskStatus.Running do
      begin
        // pop
        LMessage := FQueue.PopItem;

        if Assigned(LMessage) then // this can be async
        try
          // dispatch
          for LSubscriber in Subscribers do
          begin
            try
              LSubscriber.OnMessage(LMessage);
            except
              // handle errors? ignore errors?
            end;
          end;
        finally
          LMessage.Free;
        end;

      end;
    end
  );

  FWorkerTask.Start;
{$endif}
end;

destructor TMARSMessageDispatcher.Destroy;
begin
{$ifdef DelphiXE7_UP}
  if Assigned(FWorkerTask) and (FWorkerTask.Status < TTaskStatus.Canceled) then
    FWorkerTask.Cancel;
{$endif}

  FCriticalSection.Free;
  FSubscribers.Free;
  FQueue.Free;

  inherited;
end;

procedure TMARSMessageDispatcher.DoRegisterSubscriber(
  const ASubscriber: IMARSMessageSubscriber);
begin
  FSubscribers.Add(ASubscriber);
end;

procedure TMARSMessageDispatcher.DoUnRegisterSubscriber(
  const ASubscriber: IMARSMessageSubscriber);
begin
  FSubscribers.Remove(ASubscriber);
end;

function TMARSMessageDispatcher.Enqueue(AMessage: TMARSMessage): Integer;
begin
  FQueue.PushItem(AMessage, Result);
end;

class function TMARSMessageDispatcher.GetInstance: TMARSMessageDispatcher;
begin
  if not Assigned(_Instance) then
    _Instance := TMARSMessageDispatcher.Create;
  Result := _Instance;
end;

procedure TMARSMessageDispatcher.RegisterSubscriber(
  const ASubscriber: IMARSMessageSubscriber);
begin
  DoRegisterSubscriber(ASubscriber);
end;

procedure TMARSMessageDispatcher.UnRegisterSubscriber(
  const ASubscriber: IMARSMessageSubscriber);
begin
  DoUnRegisterSubscriber(ASubscriber);
end;

end.
