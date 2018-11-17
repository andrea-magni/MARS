unit Server.Resources;

interface

uses
  Classes, SysUtils
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Utils.Parameters
;

type
  TSex = (Male, Female, Other);
  TSexs = set of TSex;

  TMyMessage = record
    text: string;
    timestamp: TDateTime;
    data: TArray<TMyMessage>;
    sex: TSexs;
    constructor Create(const AText: string);
  end;

  [Path('helloworld')]
  TSampleResource = class
  private
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;

    [GET, Path('ARR'), Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorldArr: TArray<string>;

    [GET, Path('ARRI'), Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorldArrI: TArray<Integer>;

    [GET, Path('ARRD'), Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorldArrD: TArray<Double>;

    [GET, Path('ARRB'), Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorldArrB: TArray<Boolean>;

    [GET, Path('REC'), Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorldRec: TMyMessage;


    [GET, Path('JSON'), Produces(TMediaType.APPLICATION_JSON)]
    function SayMyMessage: TMyMessage;
  end;

implementation

uses Math;

{ TSampleResource }

function TSampleResource.SayHelloWorld: string;
begin
  Result := 'Hello, World! ';
end;

function TSampleResource.SayHelloWorldArr: TArray<string>;
begin
  Result := ['Hello', 'World', '!'];
end;

function TSampleResource.SayHelloWorldArrB: TArray<Boolean>;
begin
  Result := [True, False, True, False, True, True];
end;

function TSampleResource.SayHelloWorldArrD: TArray<Double>;
begin
  Result := [10, 9, 7.5, 21/2, 3.123456789012345, Pi()];
end;

function TSampleResource.SayHelloWorldArrI: TArray<Integer>;
begin
  Result := [123, 456, 768, 0, 1, 2, 3];
end;

function TSampleResource.SayHelloWorldRec: TMyMessage;
begin
  Result := TMyMessage.Create('Hello, World!');
  Result.data := [
    TMyMessage.Create('Second one')
  , TMyMessage.Create('Third one')
  ];
end;

function TSampleResource.SayMyMessage: TMyMessage;
begin
  Result.text := 'Hello, World!';
  Result.timestamp := Now;
end;

{ TMyMessage }

constructor TMyMessage.Create(const AText: string);
begin
  text := AText;
  timestamp := Now;
  sex := [Male, Female];
  data := [];
end;

end.
