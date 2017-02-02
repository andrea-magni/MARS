(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.CustomResource.Editor;

{$I MARS.inc}

interface

uses
  Classes, SysUtils
  , DesignEditors
  , MARS.Client.CustomResource;

type
  TMARSClientCustomResourceEditor = class(TComponentEditor)
  private
    function CurrentObj: TMARSClientCustomResource;
  protected
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

uses
{$ifdef DelphiXE7_UP}
  VCL.Dialogs
{$else}
  Dialogs
{$endif}
  , DesignIntf
  , Windows;

procedure Register;
begin
  RegisterComponentEditor(TMARSClientCustomResource, TMARSClientCustomResourceEditor);
end;

{ TMARSClientCustomResourceEditor }

function TMARSClientCustomResourceEditor.CurrentObj: TMARSClientCustomResource;
begin
  Result := Component as TMARSClientCustomResource;
end;

procedure TMARSClientCustomResourceEditor.ExecuteVerb(Index: Integer);
begin
  inherited;

  case Index of
    0: CurrentObj.GET(nil, nil, nil);
    1: CurrentObj.POST(nil, nil, nil);
    2: CurrentObj.DELETE(nil, nil, nil);
//    3: CurrentObj.PUT;
//    4: CurrentObj.PATCH;
//    5: CurrentObj.HEAD;
//    6: CurrentObj.OPTIONS;
  end;

  if (GetKeyState(VK_LSHIFT) < 0) then
    ShowMessage(CurrentObj.Client.ResponseText);

  Designer.Modified;
end;

function TMARSClientCustomResourceEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'GET';
    1: Result := 'POST';
    2: Result := 'DELETE';
//    3: Result := 'PUT';
//    4: Result := 'PATCH';
//    5: Result := 'HEAD';
//    6: Result := 'OPTIONS';
  end;
end;

function TMARSClientCustomResourceEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

end.


