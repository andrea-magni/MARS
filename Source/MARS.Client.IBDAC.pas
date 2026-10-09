(*
  Copyright 2025, MARS-Curiosity library
  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.IBDAC;

{$I MARS.inc}

{$IFDEF MARS_IBDAC}

interface

uses
  Classes, SysUtils, Rtti, System.JSON
// Devart IBDAC
, MemDS, VirtualTable
// MARS
, MARS.Client.Resource, MARS.Client.Client, MARS.Client.Utils
, MARS.Data.IBDAC.Utils
;

type
  TMARSIBDACResourceDatasets = class;

  TMARSIBDACResourceDatasetsItem = class(TCollectionItem)
  private
    FDataSet: TVirtualTable;
    FDataSetName: string;
    FSendData: Boolean;
    FSynchronize: Boolean;
    procedure SetDataSet(const Value: TVirtualTable);
    function Collection: TMARSIBDACResourceDatasets;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
  published
    property DataSetName: string read FDataSetName write FDataSetName;
    property DataSet: TVirtualTable read FDataSet write SetDataSet;
    // POST sends the whole dataset (IBDAC has no delta)
    property SendData: Boolean read FSendData write FSendData default False;
    property Synchronize: Boolean read FSynchronize write FSynchronize default True;
  end;

  TMARSIBDACResourceDatasets = class(TCollection)
  private
    FOwnerComponent: TComponent;
    function GetItem(Index: Integer): TMARSIBDACResourceDatasetsItem;
  public
    function Add: TMARSIBDACResourceDatasetsItem;
    function FindItemByDataSetName(AName: string): TMARSIBDACResourceDatasetsItem;
    procedure ForEach(const ADoSomething: TProc<TMARSIBDACResourceDatasetsItem>);
    property Item[Index: Integer]: TMARSIBDACResourceDatasetsItem read GetItem;
  end;

  /// <summary> Client of a resource that returns one or more IBDAC datasets
  /// (application/json-mydac): GET fills the TVirtualTable of each item, POST sends the
  /// datasets of the items with SendData </summary>
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TMARSIBDACResource = class(TMARSClientResource)
  private
    FResourceDataSets: TMARSIBDACResourceDatasets;
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
    property ResourceDataSets: TMARSIBDACResourceDatasets read FResourceDataSets write FResourceDataSets;
  end;

  /// <summary> Client of a resource that returns a single IBDAC dataset </summary>
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TMARSIBDACDataSetResource = class(TMARSClientResource)
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
    raise EMARSException.Create('Invalid JSON format [IBDAC dataset]');
  LEncoded := TJSONString(AValue).Value;

  LLoadProc :=
    procedure
    begin
      ADataSet.DisableControls;
      try
        ADataSet.Close;
        TIBCDataSets.EncodedXMLStringToDataSet(LEncoded, ADataSet);
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

{ TMARSIBDACResource }

procedure TMARSIBDACResource.AfterGET(const AContent: TStream);
var
  LJSON: TJSONObject;
  LPair: TJSONPair;
  LItem: TMARSIBDACResourceDatasetsItem;
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

procedure TMARSIBDACResource.AfterPOST(const AContent: TStream);
begin
  inherited;

  FreeAndNil(FPOSTResponse);
  if Client.LastCmdSuccess then
    FPOSTResponse := StreamToJSONValue(AContent);
end;

procedure TMARSIBDACResource.AssignTo(Dest: TPersistent);
var
  LDest: TMARSIBDACResource;
begin
  inherited AssignTo(Dest);

  LDest := Dest as TMARSIBDACResource;
  LDest.ResourceDataSets.Assign(ResourceDataSets);
end;

procedure TMARSIBDACResource.BeforePOST(const AContent: TStream);
var
  LJSON: TJSONObject;
begin
  inherited;

  LJSON := TJSONObject.Create;
  try
    FResourceDataSets.ForEach(
      procedure (AItem: TMARSIBDACResourceDatasetsItem)
      begin
        if AItem.SendData and Assigned(AItem.DataSet) and AItem.DataSet.Active then
          LJSON.AddPair(AItem.DataSetName, TIBCDataSets.DataSetToEncodedXMLString(AItem.DataSet));
      end
    );
    JSONValueToStream(LJSON, AContent);
  finally
    LJSON.Free;
  end;
end;

constructor TMARSIBDACResource.Create(AOwner: TComponent);
begin
  inherited;

  FResourceDataSets := TMARSIBDACResourceDatasets.Create(TMARSIBDACResourceDatasetsItem);
  FResourceDataSets.FOwnerComponent := Self;
  SpecificAccept := APPLICATION_JSON_IBDAC;
  SpecificContentType := APPLICATION_JSON_IBDAC;
end;

destructor TMARSIBDACResource.Destroy;
begin
  FPOSTResponse.Free;
  FResourceDataSets.Free;

  inherited;
end;

function TMARSIBDACResource.GetResponseAsString: string;
var
  LIndex: Integer;
  LItem: TMARSIBDACResourceDatasetsItem;
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

procedure TMARSIBDACResource.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if Operation = TOperation.opRemove then
    FResourceDataSets.ForEach(
      procedure (AItem: TMARSIBDACResourceDatasetsItem)
      begin
        if AItem.DataSet = AComponent then
          AItem.DataSet := nil;
      end
    );
end;

{ TMARSIBDACResourceDatasets }

function TMARSIBDACResourceDatasets.Add: TMARSIBDACResourceDatasetsItem;
begin
  Result := inherited Add as TMARSIBDACResourceDatasetsItem;
end;

function TMARSIBDACResourceDatasets.FindItemByDataSetName(
  AName: string): TMARSIBDACResourceDatasetsItem;
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

procedure TMARSIBDACResourceDatasets.ForEach(
  const ADoSomething: TProc<TMARSIBDACResourceDatasetsItem>);
var
  LIndex: Integer;
begin
  if Assigned(ADoSomething) then
    for LIndex := 0 to Count-1 do
      ADoSomething(GetItem(LIndex));
end;

function TMARSIBDACResourceDatasets.GetItem(
  Index: Integer): TMARSIBDACResourceDatasetsItem;
begin
  Result := inherited GetItem(Index) as TMARSIBDACResourceDatasetsItem;
end;

{ TMARSIBDACResourceDatasetsItem }

procedure TMARSIBDACResourceDatasetsItem.AssignTo(Dest: TPersistent);
var
  LDest: TMARSIBDACResourceDatasetsItem;
begin
  LDest := Dest as TMARSIBDACResourceDatasetsItem;
  LDest.DataSetName := DataSetName;
  LDest.DataSet := DataSet;
  LDest.SendData := SendData;
  LDest.Synchronize := Synchronize;
end;

function TMARSIBDACResourceDatasetsItem.Collection: TMARSIBDACResourceDatasets;
begin
  Result := inherited Collection as TMARSIBDACResourceDatasets;
end;

constructor TMARSIBDACResourceDatasetsItem.Create(Collection: TCollection);
begin
  inherited;

  FSendData := False;
  FSynchronize := True;
end;

function TMARSIBDACResourceDatasetsItem.GetDisplayName: string;
begin
  Result := DataSetName;
  if Assigned(DataSet) then
    Result := Result + ' -> ' + DataSet.Name;
end;

procedure TMARSIBDACResourceDatasetsItem.SetDataSet(const Value: TVirtualTable);
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

{ TMARSIBDACDataSetResource }

procedure TMARSIBDACDataSetResource.AfterGET(const AContent: TStream);
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

procedure TMARSIBDACDataSetResource.AssignTo(Dest: TPersistent);
var
  LDest: TMARSIBDACDataSetResource;
begin
  inherited AssignTo(Dest);

  LDest := Dest as TMARSIBDACDataSetResource;
  LDest.DataSet := DataSet;
  LDest.Synchronize := Synchronize;
  LDest.SendData := SendData;
end;

procedure TMARSIBDACDataSetResource.BeforePOST(const AContent: TStream);
var
  LJSON: TJSONObject;
begin
  inherited;

  if SendData and Assigned(FDataSet) and FDataSet.Active then
  begin
    LJSON := TJSONObject.Create;
    try
      LJSON.AddPair(IfThen(FDataSet.Name <> '', FDataSet.Name, 'DataSet')
        , TIBCDataSets.DataSetToEncodedXMLString(FDataSet));
      JSONValueToStream(LJSON, AContent);
    finally
      LJSON.Free;
    end;
  end;
end;

constructor TMARSIBDACDataSetResource.Create(AOwner: TComponent);
begin
  inherited;

  FSynchronize := True;
  FSendData := False;
  SpecificAccept := APPLICATION_JSON_IBDAC;
  SpecificContentType := APPLICATION_JSON_IBDAC;
end;

procedure TMARSIBDACDataSetResource.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (AComponent = FDataSet) and (Operation = TOperation.opRemove) then
    FDataSet := nil;
end;

procedure TMARSIBDACDataSetResource.SetDataSet(const Value: TVirtualTable);
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
