(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Data.FireDAC.Utils;

interface

uses
  Classes, SysUtils
  , MARS.Core.JSON
  , MARS.Core.Exceptions
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
      raise EMARSException.Create('Error deserializing data');

    AMemTable.Close;
    AMemTable.Data := TFDJSONDataSetsReader.GetListValueByName(LDataSets, ADataSetName);
    AMemTable.ApplyUpdates;
  finally
    LDataSets.Free;
  end;
end;


end.
