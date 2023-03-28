(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Exceptions;

interface

uses
  SysUtils
, MARS.Core.MediaType
;

type
  EMARSException = class(Exception);

  EMARSHttpException = class(EMARSException)
  private
    FStatus: Integer;
    FContentType: string;
  public
    constructor Create(const AMessage: string;
      AStatus: Integer = 500; AContentType: string = TMediaType.TEXT_PLAIN); reintroduce; virtual;
    constructor CreateFmt(const AMessage: string; const Args: array of const;
      AStatus: Integer = 500; AContentType: string = TMediaType.TEXT_PLAIN); reintroduce; virtual;

    property Status: Integer read FStatus write FStatus;
    property ContentType: string read FContentType write FContentType;
  end;

implementation

{ EMARSHttpException }

constructor EMARSHttpException.Create(const AMessage: string; AStatus: Integer; AContentType: string);
begin
  inherited Create(AMessage);
  FStatus := AStatus;
  FContentType := AContentType;
end;

constructor EMARSHttpException.CreateFmt(const AMessage: string;
  const Args: array of const; AStatus: Integer; AContentType: string);
begin
  inherited CreateFmt(AMessage, Args);
  FStatus := AStatus;
  FContentType := AContentType;
end;

end.
