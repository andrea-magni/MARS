(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit MARS.Persistence.Base;

interface

uses
  System.SysUtils, System.Classes, System.JSON, REST.Json,
  MARS.Core.Classes;

type
  TMARSConfigHelper = class helper for TMARSBaseConf
  public
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);

    procedure LoadFromFile(AFileName: string);
    procedure SaveToFile(AFileName: string);
  end;

implementation

procedure TMARSConfigHelper.LoadFromFile(AFileName: string);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TMARSConfigHelper.LoadFromStream(AStream: TStream);
var
  LBuffer: TBytes;
  LDeserializer: TJSONObject;
  LJson: TJSONValue;
begin
  LDeserializer := nil;
  SetLength(LBuffer, AStream.Size);
  AStream.ReadData(LBuffer, AStream.Size);


  LDeserializer := TJSONObject.ParseJSONValue(LBuffer, 0) as TJSONObject;

  try
    if Assigned(LDeserializer) then
      TJson.JsonToObject(Self, LDeserializer);
  finally
    LDeserializer.Free;
  end;

end;

procedure TMARSConfigHelper.SaveToFile(AFileName: string);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(AFileName, fmCreate or fmOpenWrite);
  try
    SaveToStream(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TMARSConfigHelper.SaveToStream(AStream: TStream);
var
  LSerializer: TJSONObject;
  LBuffer: TBytes;
begin
  LSerializer := TJson.ObjectToJsonObject(Self);
  try
    LBuffer := TEncoding.UTF8.GetBytes(LSerializer.ToString);
    AStream.WriteData(LBuffer, Length(LBuffer));
  finally
    LSerializer.Free;
  end;
end;

end.
