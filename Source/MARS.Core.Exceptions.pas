(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Exceptions;

interface

uses
  SysUtils;

type
  EMARSException = class(Exception);

  EMARSHttpException = class(EMARSException)
  private
    FStatus: Integer;
  public
    constructor Create(const AMessage: string; AStatus: Integer = 500); reintroduce; virtual;
    constructor CreateFmt(const AMessage: string; const Args: array of const; AStatus: Integer = 500); reintroduce; virtual;

    property Status: Integer read FStatus write FStatus;
  end;

implementation

{ EMARSHttpException }

constructor EMARSHttpException.Create(const AMessage: string; AStatus: Integer);
begin
  inherited Create(AMessage);
  FStatus := AStatus;
end;

constructor EMARSHttpException.CreateFmt(const AMessage: string;
  const Args: array of const; AStatus: Integer);
begin
  inherited CreateFmt(AMessage, Args);
  FStatus := AStatus;
end;

end.
