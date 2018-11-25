(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.Utils;

interface

uses
  Classes, SysUtils
  ;

{$IFNDEF Delphi10Rio_UP}
const pidAllPlatforms = $FFFF;
{$ENDIF}

type
  EMARSClientException = class(Exception);
  EMARSClientHttpException = class(EMARSClientException)
  private
    FStatusText: string;
    FStatusCode: Integer;
    FContent: TMemoryStream;
    FContentType: string;
  public
    constructor Create(const AStatusText: string; const AStatusCode: Integer = 500;
      const AContent: TStream = nil; const AContentType: string = ''); virtual;
    destructor Destroy; override;

    property StatusText: string read FStatusText;
    property StatusCode: Integer read FStatusCode;
    property Content: TMemoryStream read FContent;
    property ContentType: string read FContentType;
  end;

  TMARSClientProc = TProc;
  TMARSClientResponseProc = TProc<TStream>;
  TMARSClientExecptionProc = TProc<Exception>;

  TMARSComponentHelper = class
  public
    class function IsDesigning(AComponent: TComponent): Boolean;
    class function FindDefault<T: class>(AComponent: TComponent): T;
  end;


implementation

class function TMARSComponentHelper.IsDesigning(AComponent: TComponent): Boolean;
begin
  Result :=
    ([csDesigning, csLoading] * AComponent.ComponentState = [csDesigning]) and
    ((AComponent.Owner = nil) or
     ([csDesigning, csLoading] * AComponent.Owner.ComponentState = [csDesigning]));
end;

class function TMARSComponentHelper.FindDefault<T>(AComponent: TComponent): T;
var
  LRoot: TComponent;
  LIndex: Integer;
begin
  Result := nil;
  LRoot := AComponent;
  while (LRoot.Owner <> nil) and (Result = nil) do begin
    LRoot := LRoot.Owner;
    for LIndex := 0 to LRoot.ComponentCount - 1 do
      if LRoot.Components[LIndex] is T then begin
        Result := T(LRoot.Components[LIndex]);
        Break;
      end;
  end;
end;


{ EMARSClientHttpException }

constructor EMARSClientHttpException.Create(const AStatusText: string;
  const AStatusCode: Integer; const AContent: TStream; const AContentType: string);
begin
  inherited Create(AStatusCode.ToString + ': ' + AStatusText);
  FStatusCode := AStatusCode;
  FStatusText := AStatusText;
  FContent := nil;
  if Assigned(AContent) then
  begin
    FContent := TMemoryStream.Create;
    FContent.CopyFrom(AContent, 0);
  end;
  FContentType := AContentType;
end;

destructor EMARSClientHttpException.Destroy;
begin
  FreeAndNil(FContent);
  inherited;
end;

end.
