(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.MessageBodyReaders;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, Rtti

  , MARS.Core.Attributes
  , MARS.Core.Declarations
  , MARS.Core.MediaType
  , MARS.Core.MessageBodyReader
  ;

type
  [Consumes(TMediaType.APPLICATION_JSON)]
  TJSONValueReader = class(TInterfacedObject, IMessageBodyReader)
  public
    function AsObject: TObject;
{$ifdef Delphi10Berlin_UP}
    function ReadFrom(const AInputData: TBytes;
      const AAttributes: TAttributeArray;
      AMediaType: TMediaType; ARequestHeaders: TStrings): TValue; virtual;
{$else}
    function ReadFrom(const AInputData: AnsiString;
      const AAttributes: TAttributeArray;
      AMediaType: TMediaType; ARequestHeaders: TStrings): TValue; virtual;
{$endif}
  end;

implementation

uses
    MARS.Core.JSON
  , MARS.Core.Utils
  , MARS.Rtti.Utils
  ;

{ TJSONValueReader }

function TJSONValueReader.AsObject: TObject;
begin
  Result := Self;
end;

{$ifdef Delphi10Berlin_UP}
function TJSONValueReader.ReadFrom(const AInputData: TBytes;
  const AAttributes: TAttributeArray;
  AMediaType: TMediaType; ARequestHeaders: TStrings): TValue;
var
  LJSONValue: TJSONValue;
begin
  Result := TValue.Empty;

  LJSONValue := TJSONObject.ParseJSONValue(AInputData, 0);
  if Assigned(LJSONValue) then
    Result := LJSONValue;
end;
{$else}
function TJSONValueReader.ReadFrom(const AInputData: AnsiString;
  const AAttributes: TAttributeArray;
  AMediaType: TMediaType; ARequestHeaders: TStrings): TValue;
var
  LJSONValue: TJSONValue;
begin
  Result := TValue.Empty;

  LJSONValue := TJSONObject.ParseJSONValue(string(AInputData));
  if Assigned(LJSONValue) then
    Result := LJSONValue;
end;
{$endif}

procedure RegisterReaders;
begin
  TMARSMessageBodyReaderRegistry.Instance.RegisterReader<TJSONValue>(TJSONValueReader);
end;

initialization
  RegisterReaders;

end.
