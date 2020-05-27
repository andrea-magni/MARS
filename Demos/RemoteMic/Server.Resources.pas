(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes

  , MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.JSON, MARS.Core.Response
  , MARS.Core.URL

  , MARS.Core.Token.Resource //, MARS.Core.Token
;

type
  [Path('command')]
  TCommandResource = class
  private
    type TApplication = (Skype, Teams, Zoom, Other, Unknown);
    function GetCurrentApplication: TApplication;
  protected
  public
    [GET, Path('/{param1}/{param2}'), Produces(TMediaType.TEXT_PLAIN)]
    function Execute([PathParam] param1: Integer; [PathParam] param2: Integer): string;
  end;

  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  end;

implementation

uses
  Windows
,  MARS.Core.Registry
, CodeSiteLogging
;

{ TCommandResource }

function TCommandResource.GetCurrentApplication: TApplication;
var
  LText: array[0..255] of Char;
  LTitle: string;
  LHWND: HWND;
begin
  Result := Unknown;

  LHWND := GetForegroundWindow;
  GetWindowText(LHWND, @LText[0], SizeOf(LText));
  LTitle := LText;

  if LTitle.Contains('Microsoft Teams') then
    Result := Teams
  else if LTitle.Contains('Skype') then
    Result := Skype
  else if LTitle.Contains('Zoom') then
    Result := Zoom
  else
    Result := Other;
end;


function TCommandResource.Execute(param1: Integer; param2: Integer): string;
begin
  if (param1 + param2 < 2) then
  begin
    case GetCurrentApplication of
      Teams: begin
        Result := 'Teams: toggle mic';
        SetForegroundWindow(FindWindow(nil, PChar('Microsoft Teams')));

        keybd_event(VK_LCONTROL, $9D,0 , 0); // Press Control
        keybd_event(VK_LSHIFT, $AA,0 , 0); // Press Shift
        keybd_event(Ord('M'), Ord('M'), 0 , 0); // Press M

        keybd_event(Ord('M'), Ord('M'), KEYEVENTF_KEYUP , 0); // Release M
        keybd_event(VK_LSHIFT, $AA, KEYEVENTF_KEYUP, 0); // Release Shift
        keybd_event(VK_LCONTROL, $9D, KEYEVENTF_KEYUP, 0); // Release Control
      end;
      Zoom: begin
        Result := 'Zoom: toggle mic';
        SetForegroundWindow(FindWindow(nil, PChar('Zoom')));

        keybd_event(VK_LCONTROL, $9D,0 , 0); // Press Control
        keybd_event(VK_LSHIFT, $AA,0 , 0); // Press Shift
        keybd_event(Ord('A'), Ord('A'), 0 , 0); // Press A

        keybd_event(Ord('A'), Ord('A'), KEYEVENTF_KEYUP , 0); // Release A
        keybd_event(VK_LSHIFT, $AA, KEYEVENTF_KEYUP, 0); // Release Shift
        keybd_event(VK_LCONTROL, $9D, KEYEVENTF_KEYUP, 0); // Release Control
      end;
      Skype: begin
        Result := 'Skype: toggle mic';
        SetForegroundWindow(FindWindow(nil, PChar('Skype')));

        keybd_event(VK_LCONTROL, $9D,0 , 0); // Press Control
        keybd_event(Ord('M'), Ord('M'), 0 , 0); // Press M

        keybd_event(Ord('M'), Ord('M'), KEYEVENTF_KEYUP , 0); // Release M
        keybd_event(VK_LSHIFT, $AA, KEYEVENTF_KEYUP, 0); // Release Shift
        keybd_event(VK_LCONTROL, $9D, KEYEVENTF_KEYUP, 0); // Release Control
      end;
      else
        Result := 'Other app';
    end;

    CodeSite.SendMsg(Result);
  end;
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TCommandResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;
end.
