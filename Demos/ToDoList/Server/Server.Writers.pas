(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit Server.Writers;

interface

uses
    Classes, SysUtils, Rtti
  , MARS.Core.Classes
  , MARS.Core.Attributes
  , MARS.Core.MessageBodyWriter
  , MARS.Core.MediaType
  ;

type
  [Produces(TMediaType.APPLICATION_JSON)]
  TToDoItemWriter=class(TNonInterfacedObject, IMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue;
      const AAttributes: System.TArray<TCustomAttribute>;
      AMediaType: TMediaType; AResponseHeaders: TStrings;
      AOutputStream: TStream);
  end;

  [Produces(TMediaType.APPLICATION_XML)]
  TToDoItemWriterXML=class(TNonInterfacedObject, IMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue;
      const AAttributes: System.TArray<TCustomAttribute>;
      AMediaType: TMediaType; AResponseHeaders: TStrings;
      AOutputStream: TStream);
  end;


  [Produces(TMediaType.APPLICATION_JSON)]
  TArrayToDoItemWriter=class(TNonInterfacedObject, IMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue;
      const AAttributes: System.TArray<TCustomAttribute>;
      AMediaType: TMediaType; AResponseHeaders: TStrings;
      AOutputStream: TStream);
  end;

  [Produces(TMediaType.APPLICATION_XML)]
  TArrayToDoItemWriterXML=class(TNonInterfacedObject, IMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue;
      const AAttributes: System.TArray<TCustomAttribute>;
      AMediaType: TMediaType; AResponseHeaders: TStrings;
      AOutputStream: TStream);
  end;


implementation

uses
    MARS.Rtti.Utils
  , MARS.Core.JSON
  , MARS.Core.Declarations
  , Model
  ;

{ TToDoItemWriter }

procedure TToDoItemWriter.WriteTo(const AValue: TValue;
  const AAttributes: System.TArray<TCustomAttribute>; AMediaType: TMediaType;
  AResponseHeaders: TStrings; AOutputStream: TStream);
var
  LWriter: TStreamWriter;
  LItem: TToDoItem;
begin
  LWriter := TStreamWriter.Create(AOutputStream);
  try
    LItem := AValue.AsType<TToDoItem>;
    LWriter.Write(LItem.ToJson.ToJSON);
  finally
    LWriter.Free;
  end;
end;

{ TArrayToDoItemWriter }

procedure TArrayToDoItemWriter.WriteTo(const AValue: TValue;
  const AAttributes: System.TArray<TCustomAttribute>; AMediaType: TMediaType;
  AResponseHeaders: TStrings; AOutputStream: TStream);
var
  LArray: TArray<TToDoItem>;
  LItem: TToDoItem;
  LWriter: TStreamWriter;
  LJSONArray: TJSONArray;
begin
  LWriter := TStreamWriter.Create(AOutputStream);
  try
    LJSONArray := TJSONArray.Create;
    try
      LArray := AValue.AsType<TArray<TToDoItem>>;
      for LItem in LArray do
        LJSONArray.Add(LItem.ToJson);

      LWriter.Write(LJSONArray.ToJSON);
    finally
      LJSONArray.Free;
    end;
  finally
    LWriter.Free;
  end;
end;

{ TToDoItemWriterXML }

procedure TToDoItemWriterXML.WriteTo(const AValue: TValue;
  const AAttributes: System.TArray<TCustomAttribute>; AMediaType: TMediaType;
  AResponseHeaders: TStrings; AOutputStream: TStream);
var
  LWriter: TStreamWriter;
  LItem: TToDoItem;
begin
  LWriter := TStreamWriter.Create(AOutputStream);
  try
    LItem := AValue.AsType<TToDoItem>;
    LWriter.Write(LItem.ToXML);
  finally
    LWriter.Free;
  end;
end;


{ TArrayToDoItemWriterXML }

procedure TArrayToDoItemWriterXML.WriteTo(const AValue: TValue;
  const AAttributes: System.TArray<TCustomAttribute>; AMediaType: TMediaType;
  AResponseHeaders: TStrings; AOutputStream: TStream);
var
  LArray: TArray<TToDoItem>;
  LItem: TToDoItem;
  LWriter: TStreamWriter;
begin
  LWriter := TStreamWriter.Create(AOutputStream);
  try
    LWriter.Write('<items>');
    LArray := AValue.AsType<TArray<TToDoItem>>;
    for LItem in LArray do
      LWriter.Write(LItem.ToXML);

    LWriter.Write('</items>');
  finally
    LWriter.Free;
  end;
end;

initialization
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TToDoItem>(TToDoItemWriter);
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TToDoItem>(TToDoItemWriterXML);

  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TArrayToDoItemWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := Assigned(AType) and AType.IsDynamicArrayOf<TToDoItem>;
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyRegistry.AFFINITY_HIGH
      end
  );

  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TArrayToDoItemWriterXML
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := Assigned(AType) and AType.IsDynamicArrayOf<TToDoItem>;
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyRegistry.AFFINITY_HIGH
      end
  );


end.
