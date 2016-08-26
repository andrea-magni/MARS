(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Metadata.Engine.MessageBodyWriter;

interface

uses
  Classes, SysUtils, Rtti

  , MARS.Core.Attributes
  , MARS.Core.Declarations
  , MARS.Core.MediaType
  , MARS.Core.MessageBodyWriter
  , MARS.Core.Engine
  , MARS.Core.JSON
;

type
  [Produces(TMediaType.APPLICATION_JSON)]
  TMARSEngineJSONWriter = class(TInterfacedObject, IMessageBodyWriter)
  private
    FEngine: TMARSEngine;
  protected
    function GetEngineJSON: TJSONObject; virtual;
    property Engine: TMARSEngine read FEngine;
  public
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponseHeaders: TStrings; AOutputStream: TStream);
  end;

implementation

uses
    MARS.Core.Utils
  , MARS.Metadata
  , MARS.Metadata.JSON
  , MARS.Metadata.Reader
  ;

{ TMARSEngineJSONWriter }

function TMARSEngineJSONWriter.GetEngineJSON: TJSONObject;
var
  LReader: TMARSMetadataReader;
begin
  LReader := TMARSMetadataReader.Create(Engine);
  try
    Result := LReader.Metadata.ToJSON;
  finally
    LReader.Free;
  end;
end;

procedure TMARSEngineJSONWriter.WriteTo(const AValue: TValue;
  const AAttributes: TAttributeArray; AMediaType: TMediaType;
  AResponseHeaders: TStrings; AOutputStream: TStream);
var
  LStreamWriter: TStreamWriter;
  LEngineJSON: TJSONObject;
begin
  FEngine := AValue.AsType<TMARSEngine>;
  Assert(Assigned(FEngine));

  LEngineJSON := GetEngineJSON;
  try
    LStreamWriter := TStreamWriter.Create(AOutputStream);
    try
      LStreamWriter.Write(LEngineJSON.ToJSON);
    finally
      LStreamWriter.Free;
    end;
  finally
    LEngineJSON.Free;
  end;
end;

initialization
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TMARSEngine>(TMARSEngineJSONWriter);

end.
