(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit MARS.Persistence.OXml;

interface

uses
  System.SysUtils
  , System.Classes
  , System.TypInfo
  , MARS.Core.Classes
  , OXmlUtils
  , OXmlRTTISerialize;

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
  LDeserializer: TXMLRTTIDeserializer;
  LRootName: string;
begin
  LRootName := '';
  LDeserializer := TXMLRTTIDeserializer.Create;
  try
    LDeserializer.Visibility := [mvPublished];
    LDeserializer.UseRoot := False;
    LDeserializer.InitFile(AFileName);
    LDeserializer.ReadObjectInfo(LRootName);
    LDeserializer.ReadObject(Self);
  finally
    LDeserializer.Free;
  end;
end;

procedure TMARSConfigHelper.LoadFromStream(AStream: TStream);
begin

end;

procedure TMARSConfigHelper.SaveToFile(AFileName: string);
var
  LSerializer: TXMLRTTISerializer;
begin
  LSerializer := TXMLRTTISerializer.Create;
  try
    LSerializer.Visibility := [mvPublished];
    LSerializer.UseRoot := False;
    LSerializer.XMLDeclaration.Enabled := True;
    LSerializer.InitFile(AFileName);
    LSerializer.WriterSettings.IndentType := itIndent;
    LSerializer.WriteObject(Self);
  finally
    LSerializer.Free;
  end;
end;

procedure TMARSConfigHelper.SaveToStream(AStream: TStream);
begin

end;

end.
