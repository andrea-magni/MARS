unit ReadersAndWriters;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, Rtti

  , MARS.Core.Attributes, MARS.Core.Invocation, MARS.Core.Declarations
  , MARS.Core.MediaType
  , MARS.Core.MessageBodyWriter, MARS.Core.MessageBodyReader
  , MARS.Core.Utils, MARS.Data.Utils

  , Model
;

type
  // --- READERS ---
  [Consumes(TMediaType.APPLICATION_JSON)]
  TToDoItemReader = class(TInterfacedObject, IMessageBodyReader)
  public
    function ReadFrom(
    {$ifdef Delphi10Berlin_UP}const AInputData: TBytes;{$else}const AInputData: AnsiString;{$endif}
      const ADestination: TRttiObject; const AMediaType: TMediaType;
      const AContext: TMARSActivationRecord
    ): TValue; virtual;
  end;

  [Consumes(TMediaType.APPLICATION_JSON)]
  TAccountReader = class(TInterfacedObject, IMessageBodyReader)
  public
    function ReadFrom(
    {$ifdef Delphi10Berlin_UP}const AInputData: TBytes;{$else}const AInputData: AnsiString;{$endif}
      const ADestination: TRttiObject; const AMediaType: TMediaType;
      const AContext: TMARSActivationRecord
    ): TValue; virtual;
  end;


  // --- WRITERS ---
  [Produces(TMediaType.APPLICATION_JSON)]
  TToDoItemWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponseHeaders: TStrings; AOutputStream: TStream);
  end;

  [Produces(TMediaType.APPLICATION_JSON)]
  TAccountWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponseHeaders: TStrings; AOutputStream: TStream);
  end;


  [Produces(TMediaType.APPLICATION_JSON)]
  TToDoItemsWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponseHeaders: TStrings; AOutputStream: TStream);
  end;


implementation

uses
    MARS.Core.JSON
  {$ifdef DelphiXE7_UP}, System.JSON {$endif}
  , MARS.Core.Exceptions
  , MARS.Rtti.Utils
;

{ TToDoItemReader }

function TToDoItemReader.ReadFrom(const AInputData: TBytes;
  const ADestination: TRttiObject; const AMediaType: TMediaType;
  const AContext: TMARSActivationRecord): TValue;
var
  LJSON: TJSONObject;
begin
  Result := TValue.Empty;

  LJSON := TJSONObject.ParseJSONValue(AInputData, 0) as TJSONObject;
  if Assigned(LJSON) then
    try
      Result := TValue.From<TToDoItem>(LJSON.ToRecord<TToDoItem>);
    finally
      LJSON.Free;
    end;
end;

{ TToDoItemWriter }

procedure TToDoItemWriter.WriteTo(const AValue: TValue;
  const AAttributes: TAttributeArray; AMediaType: TMediaType;
  AResponseHeaders: TStrings; AOutputStream: TStream);
var
  LStreamWriter: TStreamWriter;
  LToDoItem: TToDoItem;
  LJSONObj: TJSONObject;
begin
  LStreamWriter := TStreamWriter.Create(AOutputStream);
  try
    if not AValue.IsEmpty then
    begin
      LToDoItem := AValue.AsType<TToDoItem>;

      LJSONObj := TJSONObject.RecordToJSON<TToDoItem>(LToDoItem);
      try
        LStreamWriter.Write(LJSONObj.ToJSON);
      finally
        LJSONObj.Free;
      end;
    end;
  finally
    LStreamWriter.Free;
  end;
end;

{ TToDoItemsWriter }

procedure TToDoItemsWriter.WriteTo(const AValue: TValue;
  const AAttributes: TAttributeArray; AMediaType: TMediaType;
  AResponseHeaders: TStrings; AOutputStream: TStream);
var
  LStreamWriter: TStreamWriter;
  LToDoItems: TToDoItems;
  LToDoItem: TToDoItem;
  LJSONArray: TJSONArray;
begin
  LStreamWriter := TStreamWriter.Create(AOutputStream);
  try
    LToDoItems := AValue.AsType<TToDoItems>;

    LJSONArray := TJSONArray.Create;
    try
      for LToDoItem in LToDoItems do
        LJSONArray.Add(
          TJSONObject.RecordToJSON<TToDoItem>(LToDoItem)
        );
      LStreamWriter.Write(LJSONArray.ToJSON);
    finally
      LJSONArray.Free;
    end;
  finally
    LStreamWriter.Free;
  end;
end;

{ TAccountReader }

function TAccountReader.ReadFrom(
    {$ifdef Delphi10Berlin_UP}const AInputData: TBytes;{$else}const AInputData: AnsiString;{$endif}
  const ADestination: TRttiObject; const AMediaType: TMediaType;
  const AContext: TMARSActivationRecord): TValue;
var
  LJSON: TJSONObject;
begin
  Result := TValue.Empty;

  LJSON := TJSONObject.ParseJSONValue(AInputData, 0) as TJSONObject;
  if Assigned(LJSON) then
    try
      Result := TValue.From<TAccount>(LJSON.ToRecord<TAccount>);
    finally
      LJSON.Free;
    end;
end;

{ TAccountWriter }

procedure TAccountWriter.WriteTo(const AValue: TValue;
  const AAttributes: TAttributeArray; AMediaType: TMediaType;
  AResponseHeaders: TStrings; AOutputStream: TStream);
var
  LStreamWriter: TStreamWriter;
  LAccount: TAccount;
  LJSONObj: TJSONObject;
begin
  LStreamWriter := TStreamWriter.Create(AOutputStream);
  try
    if not AValue.IsEmpty then
    begin
      LAccount := AValue.AsType<TAccount>;

      LJSONObj := TJSONObject.RecordToJSON<TAccount>(LAccount);
      try
        LStreamWriter.Write(LJSONObj.ToJSON);
      finally
        LJSONObj.Free;
      end;
    end;
  finally
    LStreamWriter.Free;
  end;
end;

procedure RegisterReadersAndWriters;
begin
  TMARSMessageBodyReaderRegistry.Instance.RegisterReader<TToDoItem>(TToDoItemReader);
  TMARSMessageBodyReaderRegistry.Instance.RegisterReader<TAccount>(TAccountReader);

  TMARSMessageBodyRegistry.Instance.RegisterWriter<TToDoItem>(TToDoItemWriter);
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TAccount>(TAccountWriter);
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TToDoItems>(TToDoItemsWriter);
end;


initialization
  RegisterReadersAndWriters;

end.
