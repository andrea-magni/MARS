(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Messaging.Queue;

interface

uses
    Classes, SysUtils

  , Generics.Collections

  , MARS.Messaging.Dispatcher
  , MARS.Messaging.Subscriber
  , MARS.Messaging.Message

  , MARS.Stateful.Dictionary

  , MARS.Core.Token
  ;

type
  TMARSMessagingQueueForToken = class
  private
  protected
    class function GetQueueName<T: TMARSCustomMessage>(const AQueueName: string = ''): string;
  public
    class procedure Create<T: TMARSCustomMessage, constructor>(AToken: TMARSToken; const AQueueName: string = '');
    class procedure Use<T: TMARSCustomMessage>(AToken: TMARSToken; const ADoSomething: TProc<TQueue<T>>; const AQueueName: string = '');
  end;

implementation

{ TMARSMessagingQueue }

class procedure TMARSMessagingQueueForToken.Create<T>(AToken: TMARSToken;
  const AQueueName: string);
var
  LSubscriber: IMARSMessageSubscriber;
  LDictionary: TMARSStatefulDictionary;
  LQueueName: string;
begin
  LQueueName := GetQueueName<T>(AQueueName);

  LDictionary := TMARSStatefulDictionaryRegistry.Instance.GetDictionaryForToken(AToken);
  LDictionary.Add(LQueueName, TQueue<T>.Create());

  LSubscriber := TMARSAnonymousSubscriber<T>.Create(
    procedure(AMessage: T)
    begin
      LDictionary.Use<TQueue<T>>(LQueueName,
        procedure (AQueue: TQueue<T>)
        begin
          AQueue.Enqueue(T.Clone<T>(AMessage));
        end
      );
    end
  );

  TMARSMessageDispatcher.Instance.RegisterSubscriber(LSubscriber);
end;

class function TMARSMessagingQueueForToken.GetQueueName<T>(
  const AQueueName: string): string;
begin
  Result := AQueueName;
  if Result = '' then
    Result := 'MessageQueue.' + T.ClassName;
end;

class procedure TMARSMessagingQueueForToken.Use<T>(AToken: TMARSToken;
  const ADoSomething: TProc<TQueue<T>>; const AQueueName: string);
var
  LDictionary: TMARSStatefulDictionary;
begin
  LDictionary := TMARSStatefulDictionaryRegistry.Instance.GetDictionaryForToken(AToken);

  LDictionary.Use<TQueue<T>>(GetQueueName<T>(AQueueName), ADoSomething);
end;

end.
