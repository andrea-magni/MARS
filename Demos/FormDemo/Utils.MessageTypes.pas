unit Utils.MessageTypes;

interface

uses
  Classes, SysUtils, System.Messaging;

type
  TLogInfo = record
    Text: string;
    Context: string;
    constructor Create(const AText: string; const AContext: string = '');
  end;

  TLogMessage = class(TMessage<TLogInfo>)
  public
    constructor Create(const AText: string; const AContext: string = ''); virtual;
  end;

implementation

{ TLogInfo }

constructor TLogInfo.Create(const AText, AContext: string);
begin
  Text := AText;
  Context := AContext;
end;

{ TLogMessage }

constructor TLogMessage.Create(const AText, AContext: string);
begin
  inherited Create(TLogInfo.Create(AText, AContext));
end;

end.
