(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)

unit MARS.Core.Exceptions;

interface

uses
  SysUtils, System.Rtti
, MARS.Core.MediaType
;

type
  EMARSException = class(Exception);

  EMARSHttpException = class(EMARSException)
  private
    FStatus: Integer;
    FContentType: string;
    FReasonString: string;
  public
    constructor Create(const AMessage: string;
      const AStatus: Integer = 500;
      const AContentType: string = TMediaType.TEXT_PLAIN;
      const AReasonString: string = ''
    ); reintroduce; virtual;
    constructor CreateFmt(const AMessage: string; const Args: array of const;
      const AStatus: Integer = 500;
      const AContentType: string = TMediaType.TEXT_PLAIN;
      const AReasonString: string = ''
    ); reintroduce; virtual;

    property Status: Integer read FStatus write FStatus;
    property ContentType: string read FContentType write FContentType;
    property ReasonString: string read FReasonString write FReasonString;
  end;

  EMARSEngineException = class(EMARSHttpException);

  EMARSApplicationException = class(EMARSHttpException);
  EMARSResourceNotFoundException = class(EMARSApplicationException);
  EMARSMethodNotFoundException = class(EMARSApplicationException);
  EMARSAuthenticationException = class(EMARSApplicationException);
  EMARSAuthorizationException = class(EMARSApplicationException);

  EMARSWithResponseException = class(EMARSApplicationException)
  protected
    FResponseContent: TValue;
    FUseMBW: Boolean;
    FIsReference: Boolean;
  public
    constructor Create(const AMessage: string;
      const AContent: TValue;
      const AStatus: Integer = 500;
      const AReasonString: string = '';
      const AContentType: string = TMediaType.APPLICATION_JSON;
      const AUseMBW: Boolean = True; const AIsReference: Boolean = False); reintroduce; virtual;

    property ResponseContent: TValue read FResponseContent;
    property UseMBW: Boolean read FUseMBW;
    property IsReference: Boolean read FIsReference;
  end;

implementation

{ EMARSHttpException }

constructor EMARSHttpException.Create(const AMessage: string;
  const AStatus: Integer; const AContentType: string;
  const AReasonString: string);
begin
  inherited Create(AMessage);
  FStatus := AStatus;
  FContentType := AContentType;
  FReasonString := AReasonString;
end;

constructor EMARSHttpException.CreateFmt(const AMessage: string;
  const Args: array of const; const AStatus: Integer;
  const AContentType: string; const AReasonString: string);
begin
  inherited CreateFmt(AMessage, Args);
  FStatus := AStatus;
  FContentType := AContentType;
  FReasonString := AReasonString;
end;

{ EMARSWithResponseException }

constructor EMARSWithResponseException.Create(const AMessage: string;
  const AContent: TValue; const AStatus: Integer;
  const AReasonString: string; const AContentType: string;
  const AUseMBW: Boolean; const AIsReference: Boolean
);
begin
  FResponseContent := AContent;
  FUseMBW := AUseMBW;
  FIsReference := AIsReference;

  inherited Create(AMessage, AStatus, AContentType, AReasonString);
end;

end.
