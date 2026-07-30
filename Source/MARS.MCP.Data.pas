(*
  Copyright 2026, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS

  MCP support for dataset-returning tools: a tool method of a TMCPDataResource
  descendant can return any TDataSet (i.e. a TFDQuery obtained through
  [Context] FD: TMARSFireDAC) and the rows are serialized into the tool result
  (text content + structuredContent: { rowCount, rows }).

  Note: dataset results are NOT freed by the dispatcher. Return context-owned
  datasets (the default for TMARSFireDAC.Query/CreateQuery) or manage their
  lifetime in the resource.
*)
unit MARS.MCP.Data;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, System.JSON, System.Rtti
, Data.DB
, MARS.Core.JSON
, MARS.MCP, MARS.MCP.Resource
;

type
  TMCPDataDispatcher = class(TMCPDispatcher)
  protected
    function SerializationOptionsFor(const AMethod: TRttiMethod): TMARSJSONSerializationOptions; virtual;
    function BuildToolResult(const AMethod: TRttiMethod; const AValue: TValue): TJSONObject; override;
    function BuildResourceContents(const AInfo: TMCPResourceInfo; const AURI: string;
      const AValue: TValue): TJSONObject; override;
    procedure DisposeToolResult(const AMethod: TRttiMethod; const AValue: TValue); override;
  end;

  TMCPDataResource = class(TMCPResource)
  protected
    function GetDispatcherClass: TMCPDispatcherClass; override;
  end;

implementation

uses
  MARS.Data.Utils
;

{ TMCPDataDispatcher }

function TMCPDataDispatcher.SerializationOptionsFor(
  const AMethod: TRttiMethod): TMARSJSONSerializationOptions;
begin
  // same adjustment chain as TDataSetWriterJSON (resource then method attributes),
  // so serialization attributes affect MCP results like standard HTTP responses
  Result := DefaultMARSJSONSerializationOptions
    .AdjustWith(FRttiContext.GetType(Instance.ClassType).GetAttributes);
  if Assigned(AMethod) then
    Result := Result.AdjustWith(AMethod.GetAttributes);
end;

function TMCPDataDispatcher.BuildToolResult(const AMethod: TRttiMethod;
  const AValue: TValue): TJSONObject;
var
  LDataSet: TDataSet;
  LStructured: TJSONObject;
  LContent: TJSONArray;
  LBlock: TJSONObject;
begin
  if AValue.IsObject and (AValue.AsObject is TDataSet) then
  begin
    LDataSet := TDataSet(AValue.AsObject);

    LStructured := TJSONObject.Create;
    try
      LStructured.AddPair('rows', DataSetToJSONArray(LDataSet, SerializationOptionsFor(AMethod)));
      LStructured.AddPair('rowCount', TJSONNumber.Create(LDataSet.RecordCount));

      Result := TJSONObject.Create;
      LContent := TJSONArray.Create;
      Result.AddPair('content', LContent);

      LBlock := TJSONObject.Create;
      LBlock.AddPair('type', 'text');
      LBlock.AddPair('text', LStructured.ToJSON);
      LContent.AddElement(LBlock);

      Result.AddPair('structuredContent', LStructured);
      LStructured := nil;
    finally
      LStructured.Free;
    end;
  end
  else
    Result := inherited BuildToolResult(AMethod, AValue);
end;

function TMCPDataDispatcher.BuildResourceContents(const AInfo: TMCPResourceInfo;
  const AURI: string; const AValue: TValue): TJSONObject;
var
  LRows: TJSONArray;
begin
  if AValue.IsObject and (AValue.AsObject is TDataSet) then
  begin
    Result := TJSONObject.Create;
    Result.AddPair('uri', AURI);
    Result.AddPair('mimeType', 'application/json');
    LRows := DataSetToJSONArray(TDataSet(AValue.AsObject), SerializationOptionsFor(AInfo.RttiMethod));
    try
      Result.AddPair('text', LRows.ToJSON);
    finally
      LRows.Free;
    end;
  end
  else
    Result := inherited BuildResourceContents(AInfo, AURI, AValue);
end;

procedure TMCPDataDispatcher.DisposeToolResult(const AMethod: TRttiMethod;
  const AValue: TValue);
begin
  // datasets are typically context-owned (TMARSFireDAC.Query): never free them here
  if AValue.IsObject and (AValue.AsObject is TDataSet) then
    Exit;
  inherited;
end;

{ TMCPDataResource }

function TMCPDataResource.GetDispatcherClass: TMCPDispatcherClass;
begin
  Result := TMCPDataDispatcher;
end;

end.
