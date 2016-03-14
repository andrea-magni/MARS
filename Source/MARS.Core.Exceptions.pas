(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit MARS.Core.Exceptions;

interface

uses
  SysUtils
  , MARS.Core.Response;

type
  EMARSException = class(Exception);

  /// <summary>
  ///   This exception may be thrown by a resource method if a specific HTTP error response needs to be produced.
  /// </summary>
  EMARSWebApplicationException = class(EMARSException)
  private
    FStatus: Integer;
  public
    /// <summary>
    ///   Construct a new instance with a blank message and default HTTP status code of 500.
    /// </summary>
    constructor Create; overload;
    /// <summary>
    ///   Construct a new instance with specified message and default HTTP status code of 500.
    /// </summary>
    constructor Create(const AMessage: string); overload;

    /// <summary>
    ///   Construct a new instance with specified message and specified HTTP status code.
    /// </summary>
    constructor Create(const AMessage: string; AStatus: Integer); overload;

    property Status: Integer read FStatus write FStatus;
  end;

implementation

{ EMARSWebApplicationException }

constructor EMARSWebApplicationException.Create;
begin
  inherited Create('');
  FStatus := 500;
end;

constructor EMARSWebApplicationException.Create(const AMessage: string);
begin
  inherited Create(AMessage);
  FStatus := 500;
end;

constructor EMARSWebApplicationException.Create(const AMessage: string; AStatus: Integer);
begin
  inherited Create(AMessage);
  FStatus := AStatus;
end;

end.
