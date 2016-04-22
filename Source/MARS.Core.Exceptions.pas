(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
  Home: https://github.com/andrea-magni/MARS

*)
unit MARS.Core.Exceptions;

interface

uses
  SysUtils
  , MARS.Core.Response;

type
  EMARSException = class(Exception);

  EMARSHttpException = class(EMARSException)
  private
    FStatus: Integer;
  public
    constructor Create; overload;
    constructor Create(const AMessage: string); overload;
    constructor Create(const AMessage: string; AStatus: Integer); overload;

    property Status: Integer read FStatus write FStatus;
  end;

implementation

{ EMARSHttpException }

constructor EMARSHttpException.Create;
begin
  inherited Create('');
  FStatus := 500;
end;

constructor EMARSHttpException.Create(const AMessage: string);
begin
  inherited Create(AMessage);
  FStatus := 500;
end;

constructor EMARSHttpException.Create(const AMessage: string; AStatus: Integer);
begin
  inherited Create(AMessage);
  FStatus := AStatus;
end;

end.
