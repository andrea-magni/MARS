(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Messaging.Resource;

{$I MARS.inc}

interface

uses
  SysUtils, Classes

  , MARS.Core.JSON
  , MARS.Core.Attributes
  , MARS.Core.Token
  , MARS.Core.MediaType

  , MARS.Messaging.Message
  , MARS.Messaging.Queue

  ;

type
  TMARSMessagingResourceForToken<T: TMARSCustomMessage, constructor> = class
  private
  protected
  [Context] Token: TMARSToken;
  public
    [Path('listen'), GET]
    [Produces(TMediaType.APPLICATION_JSON)]
    procedure Subscribe;

    [Path('myqueue'), GET]
    [Produces(TMediaType.APPLICATION_JSON)]
    function Consume: TJSONObject;
  end;


implementation

uses
  Generics.Collections;

{ TMARSMessagingResourceForToken<T> }

function TMARSMessagingResourceForToken<T>.Consume: TJSONObject;
var
  LCount: Integer;
  LArrayMessaggi: TJSONArray;
begin
  LCount := -1;

  LArrayMessaggi := TJSONArray.Create;
  try

    TMARSMessagingQueueForToken.Use<T>(Token,
      procedure (AQueue: TQueue<T>)
      var
        LMessage: T;
      begin
        LCount := AQueue.Count;
        while AQueue.Count > 0 do
        begin
          LMessage := AQueue.Dequeue;
          try
            LArrayMessaggi.Add(LMessage.ToJSON);
          finally
            LMessage.Free;
          end;
        end;
      end
    );

    Result := TJSONObject.Create;
    try
      Result.AddPair('Count', TJSONNumber.Create(LCount));
      Result.AddPair('Messages', LArrayMessaggi);
    except
      Result.Free;
      raise;
    end;
  except
    LArrayMessaggi.Free;
    raise;
  end;

end;

procedure TMARSMessagingResourceForToken<T>.Subscribe;
begin
  TMARSMessagingQueueForToken.Create<T>(Token);
end;

end.
