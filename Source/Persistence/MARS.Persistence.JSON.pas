(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit MARS.Persistence.JSON;

interface

uses
  System.SysUtils, System.Classes, System.JSON, REST.Json,
  MARS.Core.Classes;

type
  TMARSConfigHelper = class helper for TMARSBaseConf
  private
    class function Format(const AJSON: string): string;
  public

    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);

    procedure LoadFromFile(AFileName: string);
    procedure SaveToFile(AFileName: string);
  end;

implementation

class function TMARSConfigHelper.Format(const AJSON: string): string;
const
  EOL = #13#10;
  INDENT = '  ';
var
  LChar: char;
  LIndent: string;
  isEOL: boolean;
  isInString: boolean;
  isEscape: boolean;
begin
  isEOL := true;
  isInString := false;
  isEscape := false;

  for LChar in AJSON do
  begin
    if not isInString and ((LChar = '{') or (LChar = '[')) then
    begin
      if not isEOL then
        Result := Result + EOL;
      Result := Result + LIndent + LChar + EOL;
      LIndent := LIndent + INDENT;
      Result := Result + LIndent;
      isEOL := true;
    end
    else if not isInString and (LChar = ',') then
    begin
      isEOL := false;
      Result := Result + LChar + EOL + LIndent;
    end
    else if not isInString and ((LChar = '}') or (LChar = ']')) then
    begin
      Delete(LIndent, 1, Length(INDENT));
      if not isEOL then
        Result := Result + EOL;
      Result := Result + LIndent + LChar + EOL;
      isEOL := true;
    end
    else
    begin
      isEOL := false;
      Result := Result + LChar;
    end;
    isEscape := (LChar = '\') and not isEscape;
    if not isEscape and (LChar = '"') then
      isInString := not isInString;
  end;
end;

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
begin
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
  //LSerializer: TJSONObject;
  LBuffer: TBytes;
begin
  try
  //LStr := TJson.ObjectToJsonString(LEngineConfig);
  //Memo1.Lines.Add(LStr);

    LBuffer := TEncoding.UTF8.GetBytes(Format(TJson.JsonEncode(TJson.ObjectToJsonObject(Self))));
    AStream.WriteData(LBuffer, Length(LBuffer));
  finally
    //LSerializer.Free;
  end;
end;

end.
