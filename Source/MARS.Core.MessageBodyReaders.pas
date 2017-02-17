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

  [Consumes(TMediaType.APPLICATION_OCTET_STREAM), Consumes(TMediaType.WILDCARD)]
  TStreamReader = class(TInterfacedObject, IMessageBodyReader)
  public
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
{$ifdef DelphiXE6_UP}
  , System.JSON
{$endif}
  ;

{ TJSONValueReader }


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

{ TStreamReader }

{$ifdef Delphi10Berlin_UP}
function TStreamReader.ReadFrom(const AInputData: TBytes;
  const AAttributes: TAttributeArray;
  AMediaType: TMediaType; ARequestHeaders: TStrings): TValue;
var
  LStream: TBytesStream;
begin
  LStream := TBytesStream.Create(AInputData);
  try
    LStream.Position := 0;
    Result := LStream;
  except
    LStream.Free;
    raise;
  end;
end;
{$else}
function TStreamReader.ReadFrom(const AInputData: AnsiString;
  const AAttributes: TAttributeArray;
  AMediaType: TMediaType; ARequestHeaders: TStrings): TValue;
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create(AInputData);
  try
    LStream.Position := 0;
    Result := LStream;
  except
    LStream.Free;
    raise;
  end;
end;
{$endif}

procedure RegisterReaders;
begin
  TMARSMessageBodyReaderRegistry.Instance.RegisterReader<TJSONValue>(TJSONValueReader);
  TMARSMessageBodyReaderRegistry.Instance.RegisterReader<TStream>(TStreamReader);
end;

initialization
  RegisterReaders;

end.
