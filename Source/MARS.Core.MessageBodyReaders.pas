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
  , MARS.Core.Activation.Interfaces
  , MARS.Core.Declarations
  , MARS.Core.MediaType
  , MARS.Core.MessageBodyReader
  ;

type
  [Consumes(TMediaType.APPLICATION_JSON)]
  TJSONValueReader = class(TInterfacedObject, IMessageBodyReader)
  public
    function ReadFrom(
    {$ifdef Delphi10Berlin_UP}const AInputData: TBytes;{$else}const AInputData: AnsiString;{$endif}
      const ADestination: TRttiObject; const AMediaType: TMediaType;
      const AActivation: IMARSActivation
    ): TValue; virtual;
  end;

  [Consumes(TMediaType.APPLICATION_JSON)]
  TRecordReader = class(TInterfacedObject, IMessageBodyReader)
  public
    function ReadFrom(
    {$ifdef Delphi10Berlin_UP}const AInputData: TBytes;{$else}const AInputData: AnsiString;{$endif}
      const ADestination: TRttiObject; const AMediaType: TMediaType;
      const AActivation: IMARSActivation
    ): TValue; virtual;
  end;

  [Consumes(TMediaType.APPLICATION_JSON)]
  TArrayOfRecordReader = class(TInterfacedObject, IMessageBodyReader)
  public
    function ReadFrom(
    {$ifdef Delphi10Berlin_UP}const AInputData: TBytes;{$else}const AInputData: AnsiString;{$endif}
      const ADestination: TRttiObject; const AMediaType: TMediaType;
      const AActivation: IMARSActivation
    ): TValue; virtual;
  end;

  [Consumes(TMediaType.APPLICATION_OCTET_STREAM), Consumes(TMediaType.WILDCARD)]
  TStreamReader = class(TInterfacedObject, IMessageBodyReader)
  public
    function ReadFrom(
    {$ifdef Delphi10Berlin_UP}const AInputData: TBytes;{$else}const AInputData: AnsiString;{$endif}
      const ADestination: TRttiObject; const AMediaType: TMediaType;
      const AActivation: IMARSActivation
    ): TValue; virtual;
  end;


implementation

uses
    MARS.Core.JSON
  , MARS.Core.Utils
  , MARS.Rtti.Utils
  {$ifdef DelphiXE7_UP}, System.JSON {$endif}
  ;

{ TJSONValueReader }


function TJSONValueReader.ReadFrom(
{$ifdef Delphi10Berlin_UP}const AInputData: TBytes;{$else}const AInputData: AnsiString;{$endif}
  const ADestination: TRttiObject; const AMediaType: TMediaType;
  const AActivation: IMARSActivation
): TValue;
var
  LJSONValue: TJSONValue;
begin
  Result := TValue.Empty;

{$ifdef Delphi10Berlin_UP}
  LJSONValue := TJSONObject.ParseJSONValue(AInputData, 0);
{$else}
  LJSONValue := TJSONObject.ParseJSONValue(string(AInputData));
{$endif}
  if Assigned(LJSONValue) then
    Result := LJSONValue;
end;

{ TStreamReader }

function TStreamReader.ReadFrom(
{$ifdef Delphi10Berlin_UP}const AInputData: TBytes;{$else}const AInputData: AnsiString;{$endif}
  const ADestination: TRttiObject; const AMediaType: TMediaType;
  const AActivation: IMARSActivation
): TValue;
var
  LStream: TStream;
begin
{$ifdef Delphi10Berlin_UP}
  LStream := TBytesStream.Create(AInputData);
{$else}
  LStream := TStringStream.Create(AInputData);
{$endif}
  try
    LStream.Position := 0;
    Result := LStream;
  except
    LStream.Free;
    raise;
  end;
end;

{ TRecordReader }

function TRecordReader.ReadFrom(
{$ifdef Delphi10Berlin_UP}const AInputData: TBytes;{$else}const AInputData: AnsiString;{$endif}
  const ADestination: TRttiObject; const AMediaType: TMediaType;
  const AActivation: IMARSActivation
): TValue;
var
  LJSON: TJSONObject;
begin
  Result := TValue.Empty;

{$ifdef Delphi10Berlin_UP}
  LJSON := TJSONObject.ParseJSONValue(AInputData, 0) as TJSONObject;
{$else}
  LJSON := TJSONObject.ParseJSONValue(string(AInputData)) as TJSONObject;
{$endif}
  if Assigned(LJSON) then
    try
      Result := LJSON.ToRecord(ADestination.GetRttiType);
    finally
      LJSON.Free;
    end;
end;

{ TArrayOfRecordReader }

function TArrayOfRecordReader.ReadFrom(
{$ifdef Delphi10Berlin_UP}const AInputData: TBytes;{$else}const AInputData: AnsiString;{$endif}
  const ADestination: TRttiObject; const AMediaType: TMediaType;
  const AActivation: IMARSActivation
): TValue;
var
  LJSONArray: TJSONArray;
  LJSONObject: TJSONObject;
  LRecordType: TRttiType;
  LArray: TValue;
  LArrayType: TRttiType;
  LIndex: Integer;
begin
  Result := TValue.Empty;
  LArrayType := ADestination.GetRttiType;

{$ifdef Delphi10Berlin_UP}
  LJSONArray := TJSONObject.ParseJSONValue(AInputData, 0) as TJSONArray;
{$else}
  LJSONArray := TJSONObject.ParseJSONValue(string(AInputData)) as TJSONArray;
{$endif}

  LRecordType := LArrayType.GetArrayElementType;
  if Assigned(LJSONArray) and Assigned(LRecordType) then
    try
      TValue.Make(nil, LArrayType.Handle, LArray);
      SetArrayLength(LArray, LArrayType, LJSONArray.Count);
      for LIndex := 0 to LJSONArray.Count-1 do
      begin
        LJSONObject := LJSONArray.Items[LIndex] as TJSONObject;
        if Assigned(LJSONObject) then
          LArray.SetArrayElement(LIndex, LJSONObject.ToRecord(LRecordType));
      end;
      Result := LArray;
    finally
      LJSONArray.Free;
    end;
end;

procedure RegisterReaders;
begin
  TMARSMessageBodyReaderRegistry.Instance.RegisterReader<TJSONValue>(TJSONValueReader);
  TMARSMessageBodyReaderRegistry.Instance.RegisterReader<TStream>(TStreamReader);

  TMARSMessageBodyReaderRegistry.Instance.RegisterReader(
    TRecordReader
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := AType.IsRecord;
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyReaderRegistry.AFFINITY_MEDIUM;
      end
  );

  TMARSMessageBodyReaderRegistry.Instance.RegisterReader(
    TArrayOfRecordReader
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := AType.IsDynamicArrayOfRecord;
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyReaderRegistry.AFFINITY_MEDIUM;
      end
  );

end;

initialization
  RegisterReaders;

end.
