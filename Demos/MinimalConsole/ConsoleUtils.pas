unit ConsoleUtils;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Classes, SysUtils, Windows;

procedure ClearScreen;
procedure GotoXY(x, y: Integer);
procedure ExecuteUntilEnterKeyPressed(const ATask: TProc; const AIntervalSeconds: Integer = 1);

implementation


(*
 From https://stackoverflow.com/questions/29794559/delphi-console-xe7-clearscreen
*)
procedure ClearScreen;
var
  stdout: THandle;
  csbi: TConsoleScreenBufferInfo;
  ConsoleSize: DWORD;
  NumWritten: DWORD;
  Origin: TCoord;
begin
  stdout := GetStdHandle(STD_OUTPUT_HANDLE);
  Win32Check(stdout<>INVALID_HANDLE_VALUE);
  Win32Check(GetConsoleScreenBufferInfo(stdout, csbi));
  ConsoleSize := csbi.dwSize.X * csbi.dwSize.Y;
  Origin.X := 0;
  Origin.Y := 0;
  Win32Check(FillConsoleOutputCharacter(stdout, ' ', ConsoleSize, Origin,
    NumWritten));
  Win32Check(FillConsoleOutputAttribute(stdout, csbi.wAttributes, ConsoleSize, Origin,
    NumWritten));
  Win32Check(SetConsoleCursorPosition(stdout, Origin));
end;

(*
 From https://stackoverflow.com/questions/9946239/gotoxy-implementation
*)
procedure GotoXY(x, y: Integer);
var
  CursorCoord: _COORD;
begin
  CursorCoord.x := x;
  CursorCoord.y := y;

  SetConsoleCursorPosition(GetStdHandle(STD_OUTPUT_HANDLE), CursorCoord);
end;

(*
  Derived from
  https://stackoverflow.com/questions/16022126/how-to-make-a-console-application-wait-for-the-enter-key-but-automatically-co
*)
procedure ExecuteUntilEnterKeyPressed(const ATask: TProc; const AIntervalSeconds: Integer = 1);
  function KeyPressed(ExpectedKey: Word):Boolean;
  var lpNumberOfEvents: DWORD;
      lpBuffer: TInputRecord;
      lpNumberOfEventsRead : DWORD;
      nStdHandle: THandle;
  begin
    result := false;
    nStdHandle := GetStdHandle(STD_INPUT_HANDLE);
    lpNumberOfEvents := 0;
    GetNumberOfConsoleInputEvents(nStdHandle,lpNumberOfEvents);
    if lpNumberOfEvents<>0 then begin
      PeekConsoleInput(nStdHandle,lpBuffer,1,lpNumberOfEventsRead);
      if lpNumberOfEventsRead<>0 then
        if lpBuffer.EventType=KEY_EVENT then
          if lpBuffer.Event.KeyEvent.bKeyDown and
             ((ExpectedKey=0) or (lpBuffer.Event.KeyEvent.wVirtualKeyCode=ExpectedKey)) then
            result := true else
            FlushConsoleInputBuffer(nStdHandle) else
          FlushConsoleInputBuffer(nStdHandle);
    end;
  end;
var
  LCount: Integer;
const
  SLEEP_MS = 100;
begin
  ATask();
  LCount := 0;
  while (not KeyPressed(VK_RETURN)) do
  begin
    Sleep(SLEEP_MS);
    Inc(LCount);
    if LCount >= ((AIntervalSeconds * 1000)/SLEEP_MS) then
    begin
      LCount := 0;
      ATask();
    end;
  end;
end;


end.
