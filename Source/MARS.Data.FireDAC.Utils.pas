(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
  Home: https://github.com/andrea-magni/MARS

*)
unit MARS.Data.FireDAC.Utils;

interface

uses
  Classes, SysUtils
  , MARS.Core.JSON
  , FireDACJSONReflect
  , FireDAC.Comp.Client
  , FireDAC.Stan.StorageBin;

procedure FireDACJSONToMemTable(const AJSONContent: string; ADataSetName: string;
  AMemTable: TFDMemTable);

implementation

procedure FireDACJSONToMemTable(const AJSONContent: string; ADataSetName: string;
  AMemTable: TFDMemTable);
var
  LJSONObj: TJSONObject;
  LDataSets: TFDJSONDataSets;
begin
  LJSONObj := TJSONObject.ParseJSONValue(AJSONContent) as TJSONObject;

  LDataSets := TFDJSONDataSets.Create;
  try
    if not TFDJSONInterceptor.JSONObjectToDataSets(LJSONObj, LDataSets) then
      raise Exception.Create('Error deserializing data');

    AMemTable.Close;
    AMemTable.Data := TFDJSONDataSetsReader.GetListValueByName(LDataSets, ADataSetName);
    AMemTable.ApplyUpdates;
  finally
    LDataSets.Free;
  end;
end;


end.
