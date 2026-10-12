(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.MCP.Stdio;

// The stdio transport of MCP: a local server started by the client (Claude Code, Claude Desktop,
// the MCP Inspector...), which reads the JSON-RPC messages from its standard input, one per line,
// and writes the answers to its standard output, one per line, in UTF-8. Nothing else may be
// written to the standard output: logs and messages go to the standard error.
//
//   TMCPStdioServer.Serve(TMyTools.Create, 'my-tools', '1.0.0');
//
// The tools, resources and prompts are the ones of TMCPDispatcher ([MCPTool], [MCPResource],
// [MCPPrompt] on the methods of an object); no HTTP server, no token: the client starts the
// process with the rights of its user.

interface

uses
  Classes, SysUtils, System.JSON
, MARS.MCP
;

type
  TMCPStdioServer = class
  private
    FDispatcher: TMCPDispatcher;
    FOwnsDispatcher: Boolean;
  public
    constructor Create(const ADispatcher: TMCPDispatcher; const AOwnsDispatcher: Boolean = False);
    destructor Destroy; override;

    // a message (a line of JSON) and its answer (a line of JSON; empty for a notification)
    function HandleLine(const ALine: string): string;
    // the messages of AInput, until its end, the answers on AOutput (UTF-8, a line each)
    procedure Run(const AInput, AOutput: TStream); overload;
    // the messages of the standard input, until its end, the answers on the standard output
    procedure Run; overload;

    // a dispatcher of AInstance (owned) served on the standard input and output
    class procedure Serve(const AInstance: TObject; const AServerName: string;
      const AServerVersion: string = '1.0.0'; const AInstructions: string = ''); static;

    property Dispatcher: TMCPDispatcher read FDispatcher;
  end;

implementation

{ TMCPStdioServer }

constructor TMCPStdioServer.Create(const ADispatcher: TMCPDispatcher; const AOwnsDispatcher: Boolean);
begin
  inherited Create;
  FDispatcher := ADispatcher;
  FOwnsDispatcher := AOwnsDispatcher;
end;

destructor TMCPStdioServer.Destroy;
begin
  if FOwnsDispatcher then
    FDispatcher.Free;
  inherited;
end;

function TMCPStdioServer.HandleLine(const ALine: string): string;
var
  LMessage: TJSONValue;
  LAnswer: TJSONObject;
begin
  Result := '';
  if ALine.Trim = '' then
    Exit;
  // a line that is not JSON: a parse error (HandleMessage answers nil with one)
  LMessage := TJSONValue.ParseJSONValue(ALine);
  try
    LAnswer := FDispatcher.HandleMessage(LMessage);
    try
      if Assigned(LAnswer) then
        Result := LAnswer.ToJSON;
    finally
      LAnswer.Free;
    end;
  finally
    LMessage.Free;
  end;
end;

procedure TMCPStdioServer.Run(const AInput, AOutput: TStream);
var
  LReader: TStreamReader;
  LAnswer: string;
  LBytes: TBytes;
begin
  LReader := TStreamReader.Create(AInput, TEncoding.UTF8, False);
  try
    while not LReader.EndOfStream do
    begin
      LAnswer := HandleLine(LReader.ReadLine);
      if LAnswer <> '' then
      begin
        // a line each: JSON has no line breaks outside its strings, where they are escaped
        LBytes := TEncoding.UTF8.GetBytes(LAnswer + #10);
        AOutput.WriteBuffer(LBytes, Length(LBytes));
      end;
    end;
  finally
    LReader.Free;
  end;
end;

procedure TMCPStdioServer.Run;
var
  LLine, LAnswer: string;
begin
  // UTF-8 both ways, whatever the code page of the console
  SetTextCodePage(Input, CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  while not Eof(Input) do
  begin
    Readln(Input, LLine);
    LAnswer := HandleLine(LLine);
    if LAnswer <> '' then
    begin
      Write(Output, LAnswer, #10);
      Flush(Output);
    end;
  end;
end;

class procedure TMCPStdioServer.Serve(const AInstance: TObject; const AServerName, AServerVersion,
  AInstructions: string);
var
  LServer: TMCPStdioServer;
begin
  try
    LServer := TMCPStdioServer.Create(TMCPDispatcher.Create(AInstance, AServerName, AServerVersion, AInstructions), True);
    try
      LServer.Run;
    finally
      LServer.Free;
    end;
  finally
    // the dispatcher does not own its instance
    AInstance.Free;
  end;
end;

end.
