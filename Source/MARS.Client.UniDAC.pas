(*
  Copyright 2025, MARS-Curiosity library
  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.UniDAC;

{$I MARS.inc}

{$IFDEF MARS_UNIDAC}

interface

uses
  Classes, SysUtils, Rtti, System.JSON
// Devart UniDAC
, MemDS, VirtualTable
// MARS
, MARS.Client.Resource, MARS.Client.Client, MARS.Client.Utils
, MARS.Data.UniDAC.Utils
;

type
  TMARSUniDACResourceDatasets = class;

  TMARSUniDACResourceDatasetsItem = class(TCollectionItem)
  private
    FDataSet: TVirtualTable;
    FDataSetName: string;
    FSendData: Boolean;
    FSynchronize: Boolean;
    procedure SetDataSet(const Value: TVirtualTable);
    function Collection: TMARSUniDACResourceDatasets;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
  published
    property DataSetName: string read FDataSetName write FDataSetName;
    property DataSet: TVirtualTable read FDataSet write SetDataSet;
    // POST sends the whole dataset (UniDAC has no delta)
    property SendData: Boolean read FSendData write FSendData default False;
    property Synchronize: Boolean read FSynchronize write FSynchronize default True;
  end;

  TMARSUniDACResourceDatasets = class(TCollection)
  private
    FOwnerComponent: TComponent;
    function GetItem(Index: Integer): TMARSUniDACResourceDatasetsItem;
  public
    function Add: TMARSUniDACResourceDatasetsItem;
    function FindItemByDataSetName(AName: string): TMARSUniDACResourceDatasetsItem;
    procedure ForEach(const ADoSomething: TProc<TMARSUniDACResourceDatasetsItem>);
    property Item[Index: Integer]: TMARSUniDACResourceDatasetsItem read GetItem;
  end;

  /// <summary> Client of a resource that returns one or more UniDAC datasets
  /// (application/json-mydac): GET fills the TVirtualTable of each item, POST sends the
  /// datasets of the items with SendData </summary>
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TMARSUniDACResource = class(TMARSClientResource)
  private
    FResourceDataSets: TMARSUniDACResourceDatasets;
    FPOSTResponse: TJSONValue;
  protected
    procedure AfterGET(const AContent: TStream); override;
    procedure BeforePOST(const AContent: TStream); override;
    procedure AfterPOST(const AContent: TStream); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AssignTo(Dest: TPersistent); override;
    function GetResponseAsString: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property POSTResponse: TJSONValue read FPOSTResponse;
    property ResourceDataSets: TMARSUniDACResourceDatasets read FResourceDataSets write FResourceDataSets;
  end;

  /// <summary> Client of a resource that returns a single UniDAC dataset </summary>
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TMARSUniDACDataSetResource = class(TMARSClientResource)
  private
    FDataSet: TVirtualTable;
    FSynchronize: Boolean;
    FSendData: Boolean;
  protected
    procedure SetDataSet(const Value: TVirtualTable);
    procedure AssignTo(Dest: TPersistent); override;
    procedure AfterGET(const AContent: TStream); override;
    procedure BeforePOST(const AContent: TStream); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DataSet: TVirtualTable read FDataSet write SetDataSet;
    property Synchronize: Boolean read FSynchronize write FSynchronize default True;
    property SendData: Boolean read FSendData write FSendData default False;
  end;

implementation

uses
  StrUtils, Generics.Collections
, MARS.Core.JSON, MARS.Core.Utils, MARS.Core.Exceptions
;

// loads the dataset encoded in AValue (a member of the JSON object of the response) in ADataSet
procedure LoadDataSet(const AValue: TJSONValue; const ADataSet: TVirtualTable;
  const ASynchronize: Boolean);
var
  LEncoded: string;
  LLoadProc: TThreadProcedure;
begin
  if not (AValue is TJSONString) then
    raise EMARSException.Create('Invalid JSON format [UniDAC dataset]');
  LEncoded := TJSONString(AValue).Value;

  LLoadProc :=
    procedure
    begin
      ADataSet.DisableControls;
      try
        ADataSet.Close;
        TUniDataSets.EncodedBinaryStringToDataSet(LEncoded, ADataSet);
        if not ADataSet.Active then
          ADataSet.Open;
      finally
        ADataSet.EnableControls;
      end;
    end;

  if ASynchronize and (TThread.CurrentThread.ThreadID <> MainThreadID) then
    TThread.Synchronize(nil, LLoadProc)
  else
    LLoadProc();
end;

{ TMARSUniDACResource }

procedure TMARSUniDACResource.AfterGET(const AContent: TStream);
var
  LJSON: TJSONObject;
  LPair: TJSONPair;
  LItem: TMARSUniDACResourceDatasetsItem;
  LIndex: Integer;
begin
  inherited;

  LJSON := StreamToJSONValue(AContent) as TJSONObject;
  try
    if not Assigned(LJSON) then
      Exit;

    // purge the items of datasets no more present on the server
    LIndex := 0;
    while LIndex < FResourceDataSets.Count do
    begin
      if LJSON.GetValue(FResourceDataSets.Item[LIndex].DataSetName) = nil then
        FResourceDataSets.Delete(LIndex)
      else
        Inc(LIndex);
    end;

    for LPair in LJSON do
    begin
      LItem := FResourceDataSets.FindItemByDataSetName(LPair.JsonString.Value);
      if not Assigned(LItem) then
      begin
        LItem := FResourceDataSets.Add;
        LItem.DataSetName := LPair.JsonString.Value;
      end;
      if Assigned(LItem.DataSet) then
        LoadDataSet(LPair.JsonValue, LItem.DataSet, LItem.Synchronize);
    end;
  finally
    LJSON.Free;
  end;
end;

procedure TMARSUniDACResource.AfterPOST(const AContent: TStream);
begin
  inherited;

  FreeAndNil(FPOSTResponse);
  if Client.LastCmdSuccess then
    FPOSTResponse := StreamToJSONValue(AContent);
end;

procedure TMARSUniDACResource.AssignTo(Dest: TPersistent);
var
  LDest: TMARSUniDACResource;
begin
  inherited AssignTo(Dest);

  LDest := Dest as TMARSUniDACResource;
  LDest.ResourceDataSets.Assign(ResourceDataSets);
end;

procedure TMARSUniDACResource.BeforePOST(const AContent: TStream);
var
  LJSON: TJSONObject;
begin
  inherited;

  LJSON := TJSONObject.Create;
  try
    FResourceDataSets.ForEach(
      procedure (AItem: TMARSUniDACResourceDatasetsItem)
      begin
        if AItem.SendData and Assigned(AItem.DataSet) and AItem.DataSet.Active then
          LJSON.AddPair(AItem.DataSetName, TUniDataSets.DataSetToEncodedBinaryString(AItem.DataSet));
      end
    );
    JSONValueToStream(LJSON, AContent);
  finally
    LJSON.Free;
  end;
end;

constructor TMARSUniDACResource.Create(AOwner: TComponent);
begin
  inherited;

  FResourceDataSets := TMARSUniDACResourceDatasets.Create(TMARSUniDACResourceDatasetsItem);
  FResourceDataSets.FOwnerComponent := Self;
  SpecificAccept := APPLICATION_JSON_UniDAC;
  SpecificContentType := APPLICATION_JSON_UniDAC;
end;

destructor TMARSUniDACResource.Destroy;
begin
  FPOSTResponse.Free;
  FResourceDataSets.Free;

  inherited;
end;

function TMARSUniDACResource.GetResponseAsString: string;
var
  LIndex: Integer;
  LItem: TMARSUniDACResourceDatasetsItem;
  LDataSetInfo: string;
begin
  Result := inherited GetResponseAsString;

  for LIndex := 0 to FResourceDataSets.Count-1 do
  begin
    LItem := FResourceDataSets.Item[LIndex];
    if Result <> '' then
      Result := Result + sLineBreak;
    LDataSetInfo := 'N/A';
    if Assigned(LItem.DataSet) then
      LDataSetInfo := LItem.DataSet.RecordCount.ToString + ' records';
    Result := Result + LItem.DataSetName + ': ' + LDataSetInfo;
  end;
end;

procedure TMARSUniDACResource.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if Operation = TOperation.opRemove then
    FResourceDataSets.ForEach(
      procedure (AItem: TMARSUniDACResourceDatasetsItem)
      begin
        if AItem.DataSet = AComponent then
          AItem.DataSet := nil;
      end
    );
end;

{ TMARSUniDACResourceDatasets }

function TMARSUniDACResourceDatasets.Add: TMARSUniDACResourceDatasetsItem;
begin
  Result := inherited Add as TMARSUniDACResourceDatasetsItem;
end;

function TMARSUniDACResourceDatasets.FindItemByDataSetName(
  AName: string): TMARSUniDACResourceDatasetsItem;
var
  LIndex: Integer;
begin
  Result := nil;
  for LIndex := 0 to Count-1 do
    if SameText(GetItem(LIndex).DataSetName, AName) then
    begin
      Result := GetItem(LIndex);
      Break;
    end;
end;

procedure TMARSUniDACResourceDatasets.ForEach(
  const ADoSomething: TProc<TMARSUniDACResourceDatasetsItem>);
var
  LIndex: Integer;
begin
  if Assigned(ADoSomething) then
    for LIndex := 0 to Count-1 do
      ADoSomething(GetItem(LIndex));
end;

function TMARSUniDACResourceDatasets.GetItem(
  Index: Integer): TMARSUniDACResourceDatasetsItem;
begin
  Result := inherited GetItem(Index) as TMARSUniDACResourceDatasetsItem;
end;

{ TMARSUniDACResourceDatasetsItem }

procedure TMARSUniDACResourceDatasetsItem.AssignTo(Dest: TPersistent);
var
  LDest: TMARSUniDACResourceDatasetsItem;
begin
  LDest := Dest as TMARSUniDACResourceDatasetsItem;
  LDest.DataSetName := DataSetName;
  LDest.DataSet := DataSet;
  LDest.SendData := SendData;
  LDest.Synchronize := Synchronize;
end;

function TMARSUniDACResourceDatasetsItem.Collection: TMARSUniDACResourceDatasets;
begin
  Result := inherited Collection as TMARSUniDACResourceDatasets;
end;

constructor TMARSUniDACResourceDatasetsItem.Create(Collection: TCollection);
begin
  inherited;

  FSendData := False;
  FSynchronize := True;
end;

function TMARSUniDACResourceDatasetsItem.GetDisplayName: string;
begin
  Result := DataSetName;
  if Assigned(DataSet) then
    Result := Result + ' -> ' + DataSet.Name;
end;

procedure TMARSUniDACResourceDatasetsItem.SetDataSet(const Value: TVirtualTable);
begin
  if FDataSet <> Value then
  begin
    if Assigned(FDataSet) and Assigned(Collection.FOwnerComponent) then
      FDataSet.RemoveFreeNotification(Collection.FOwnerComponent);
    FDataSet := Value;
    if Assigned(FDataSet) and Assigned(Collection.FOwnerComponent) then
      FDataSet.FreeNotification(Collection.FOwnerComponent);
  end;
end;

{ TMARSUniDACDataSetResource }

procedure TMARSUniDACDataSetResource.AfterGET(const AContent: TStream);
var
  LJSON: TJSONObject;
begin
  inherited;

  if not Assigned(FDataSet) then
    Exit;

  LJSON := StreamToJSONValue(AContent) as TJSONObject;
  try
    if Assigned(LJSON) and (LJSON.Count > 0) then
      LoadDataSet(LJSON.Pairs[0].JsonValue, FDataSet, FSynchronize);
  finally
    LJSON.Free;
  end;
end;

procedure TMARSUniDACDataSetResource.AssignTo(Dest: TPersistent);
var
  LDest: TMARSUniDACDataSetResource;
begin
  inherited AssignTo(Dest);

  LDest := Dest as TMARSUniDACDataSetResource;
  LDest.DataSet := DataSet;
  LDest.Synchronize := Synchronize;
  LDest.SendData := SendData;
end;

procedure TMARSUniDACDataSetResource.BeforePOST(const AContent: TStream);
var
  LJSON: TJSONObject;
begin
  inherited;

  if SendData and Assigned(FDataSet) and FDataSet.Active then
  begin
    LJSON := TJSONObject.Create;
    try
      LJSON.AddPair(IfThen(FDataSet.Name <> '', FDataSet.Name, 'DataSet')
        , TUniDataSets.DataSetToEncodedBinaryString(FDataSet));
      JSONValueToStream(LJSON, AContent);
    finally
      LJSON.Free;
    end;
  end;
end;

constructor TMARSUniDACDataSetResource.Create(AOwner: TComponent);
begin
  inherited;

  FSynchronize := True;
  FSendData := False;
  SpecificAccept := APPLICATION_JSON_UniDAC;
  SpecificContentType := APPLICATION_JSON_UniDAC;
end;

procedure TMARSUniDACDataSetResource.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (AComponent = FDataSet) and (Operation = TOperation.opRemove) then
    FDataSet := nil;
end;

procedure TMARSUniDACDataSetResource.SetDataSet(const Value: TVirtualTable);
begin
  if FDataSet <> Value then
  begin
    if Assigned(FDataSet) then
      FDataSet.RemoveFreeNotification(Self);
    FDataSet := Value;
    if Assigned(FDataSet) then
      FDataSet.FreeNotification(Self);
  end;
end;

{$ELSE}
interface
implementation
{$ENDIF}

end.
