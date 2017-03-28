(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.JsonDataObjects.ReadersAndWriters;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, Rtti

  , MARS.Core.Attributes
  , MARS.Core.Declarations
  , MARS.Core.MediaType
  , MARS.Core.MessageBodyWriter
  , MARS.Core.MessageBodyReader
  , MARS.Core.Activation.Interfaces
  ;

type
  [Produces(TMediaType.APPLICATION_JSON)]
  TJsonDataObjectsWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Consumes(TMediaType.APPLICATION_JSON)]
  TJsonDataObjectsReader = class(TInterfacedObject, IMessageBodyReader)
  public
    function ReadFrom(
    {$ifdef Delphi10Berlin_UP}const AInputData: TBytes;{$else}const AInputData: AnsiString;{$endif}
      const ADestination: TRttiObject; const AMediaType: TMediaType;
      const AActivation: IMARSActivation
    ): TValue;
  end;


implementation

uses
    JsonDataObjects
  , MARS.Core.Utils
  , MARS.Rtti.Utils
  ;

{ TJsonDataObjectsWriter }

procedure TJsonDataObjectsWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LStreamWriter: TStreamWriter;
  LJsonBO: TJsonBaseObject;
begin
  LStreamWriter := TStreamWriter.Create(AOutputStream);
  try
    LJsonBO := AValue.AsObject as TJsonBaseObject;
    if Assigned(LJsonBO) then
      LStreamWriter.Write(LJsonBO.ToJSON);
  finally
    LStreamWriter.Free;
  end;
end;

{ TJsonDataObjectsReader }

function TJsonDataObjectsReader.ReadFrom(
  {$ifdef Delphi10Berlin_UP}const AInputData: TBytes;{$else}const AInputData: AnsiString;{$endif}
    const ADestination: TRttiObject; const AMediaType: TMediaType;
    const AActivation: IMARSActivation
  ): TValue;
var
  LJson: TJsonBaseObject;
begin
  Result := TValue.Empty;

  LJson := TJsonBaseObject.Parse(AInputData);
  if Assigned(LJson) then
    Result := LJson;
end;

procedure RegisterReadersAndWriters;
begin
  TMARSMessageBodyReaderRegistry.Instance.RegisterReader<TJsonBaseObject>(TJsonDataObjectsReader);

  TMARSMessageBodyRegistry.Instance.RegisterWriter<TJsonBaseObject>(TJsonDataObjectsWriter);
end;


initialization
  RegisterReadersAndWriters;

end.
