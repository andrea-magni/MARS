(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Metadata.ReadersAndWriters;

interface

uses
  Classes, SysUtils, Rtti

  , MARS.Core.Attributes
  , MARS.Core.Declarations
  , MARS.Core.MediaType
  , MARS.Core.MessageBodyWriter
  , MARS.Core.Engine
  , MARS.Core.JSON
  , MARS.Core.Activation.Interfaces
;

type
  [Produces(TMediaType.APPLICATION_JSON)]
  TMARSMetadataJSONWriter = class(TInterfacedObject, IMessageBodyWriter)
  private
  protected
  public
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

implementation

uses
    MARS.Core.Utils
  , MARS.Metadata
  , MARS.Metadata.JSON
  , MARS.Metadata.Reader
  ;

{ TMARSMetadataJSONWriter }

procedure TMARSMetadataJSONWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LStreamWriter: TStreamWriter;
  LJSON: TJSONObject;
  LMetadata: TMARSMetadata;
begin
  LMetadata := AValue.AsType<TMARSMetadata>;
  if not Assigned(LMetadata) then
    Exit;

  LJSON := LMetadata.ToJSON;
  try
    LStreamWriter := TStreamWriter.Create(AOutputStream);
    try
      LStreamWriter.Write(LJSON.ToJSON);
    finally
      LStreamWriter.Free;
    end;
  finally
    LJSON.Free;
  end;
end;

initialization
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TMARSMetadata>(TMARSMetadataJSONWriter);

end.
