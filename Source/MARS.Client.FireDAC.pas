(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.FireDAC;

interface

uses
  Classes, SysUtils
  , MARS.Core.JSON

  , FireDAC.Comp.Client

  , MARS.Client.Resource
  , MARS.Client.Client

  ;

type
  TMARSFDResourceDatasetsItem = class(TCollectionItem)
  private
    FDataSet: TFDMemTable;
    FDataSetName: string;
    FSendDelta: Boolean;
    FSynchronize: Boolean;
    procedure SetDataSet(const Value: TFDMemTable);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection); override;
  published
    property DataSetName: string read FDataSetName write FDataSetName;
    property DataSet: TFDMemTable read FDataSet write SetDataSet;
    property SendDelta: Boolean read FSendDelta write FSendDelta;
    property Synchronize: Boolean read FSynchronize write FSynchronize;
  end;

  TMARSFDResourceDatasets = class(TCollection)
  private
    function GetItem(Index: Integer): TMARSFDResourceDatasetsItem;
  public
    function Add: TMARSFDResourceDatasetsItem;
    function FindItemByDataSetName(AName: string): TMARSFDResourceDatasetsItem;
    procedure ForEach(const ADoSomething: TProc<TMARSFDResourceDatasetsItem>);
    property Item[Index: Integer]: TMARSFDResourceDatasetsItem read GetItem;
  end;


  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  TMARSFDResource = class(TMARSClientResource)
  private
    FResourceDataSets: TMARSFDResourceDatasets;
    FPOSTResponse: TJSONValue;
  protected
    procedure AfterGET(); override;
    procedure BeforePOST(AContent: TMemoryStream); override;
    procedure AfterPOST(); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property POSTResponse: TJSONValue read FPOSTResponse write FPOSTResponse;
    property ResourceDataSets: TMARSFDResourceDatasets read FResourceDataSets write FResourceDataSets;
  end;

procedure Register;

implementation

uses
    Data.FireDACJSONReflect
  , FireDAC.Comp.DataSet
  , MARS.Core.Utils
  , MARS.Client.Utils
  , MARS.Data.FireDAC.Utils
  ;

procedure Register;
begin
  RegisterComponents('MARS-Curiosity Client', [TMARSFDResource]);
end;

{ TMARSFDResource }

procedure TMARSFDResource.AfterGET();
var
  LJSONObj: TJSONObject;
  LDataSets: TFDJSONDataSets;
  LName: string;
  LData: TFDAdaptedDataSet;
  LCount: Integer;
  LItem: TMARSFDResourceDatasetsItem;
  LIndex: Integer;
begin
  inherited;

  LJSONObj := TJSONObject.ParseJSONValue(StreamToString(Client.Response.ContentStream)) as TJSONObject;

  LDataSets := TFDJSONDataSets.Create;
  try
    if not TFDJSONInterceptor.JSONObjectToDataSets(LJSONObj, LDataSets) then
      raise EMARSClientException.Create('Error deserializing data');

    LCount := TFDJSONDataSetsReader.GetListCount(LDataSets);
    for LIndex := 0 to LCount-1 do
    begin
      LName := TFDJSONDataSetsReader.GetListKey(LDataSets, LIndex);
      LData := TFDJSONDataSetsReader.GetListValue(LDataSets, LIndex);

      LItem := FResourceDataSets.FindItemByDataSetName(LName);
      if Assigned(LItem) then
      begin
        if Assigned(LItem.DataSet) then
        begin
          if LItem.Synchronize then
            TThread.Synchronize(nil,
              procedure
              begin
                LItem.DataSet.DisableControls;
                try
                  LItem.DataSet.Close;
//                  LItem.DataSet.CopyDataSet(LData, [coStructure, coRestart, coAppend]);
                  LItem.DataSet.Data := LData;
                  LItem.DataSet.ApplyUpdates;
                finally
                  LItem.DataSet.EnableControls;
                end;
              end
            )
          else
          begin
            LItem.DataSet.DisableControls;
            try
              LItem.DataSet.Close;
//              LItem.DataSet.CopyDataSet(LData, [coStructure, coRestart, coAppend]);
              LItem.DataSet.Data := LData;
              LItem.DataSet.ApplyUpdates;
            finally
              LItem.DataSet.EnableControls;
            end;
          end;
        end;
      end
      else
      begin
        LItem := FResourceDataSets.Add;
        LItem.DataSetName := LName;
      end;
    end;
  finally
    LDataSets.Free;
  end;
end;

procedure TMARSFDResource.AfterPOST();
begin
  inherited;
  if Client.LastCmdSuccess then
    FResourceDataSets.ForEach(
      procedure (AItem: TMARSFDResourceDatasetsItem)
      begin
        if AItem.SendDelta and Assigned(AItem.DataSet) and (AItem.DataSet.Active) then
          AItem.DataSet.ApplyUpdates;
      end
    );

  if Assigned(FPOSTResponse) then
    FPOSTResponse.Free;
  FPOSTResponse := StreamToJSONValue(Client.Response.ContentStream);
end;

procedure TMARSFDResource.AssignTo(Dest: TPersistent);
var
  LDest: TMARSFDResource;
begin
  inherited AssignTo(Dest);
  LDest := Dest as TMARSFDResource;

  LDest.ResourceDataSets.Assign(ResourceDataSets);
end;

procedure TMARSFDResource.BeforePOST(AContent: TMemoryStream);
var
  LDeltas: TFDJSONDeltas;
  LJSONObj: TJSONObject;
  LWriter: TStreamWriter;
begin
  inherited;

  LDeltas := TFDJSONDeltas.Create;
  try
    FResourceDataSets.ForEach(
      procedure(AItem: TMARSFDResourceDatasetsItem)
      begin
        if AItem.SendDelta and Assigned(AItem.DataSet) and (AItem.DataSet.Active) then
          TFDJSONDeltasWriter.ListAdd(LDeltas, AItem.DataSetName, AItem.DataSet);
      end
    );

    // serialize deltas to JSON (TJSONObject)
    LJSONObj := TJSONObject.Create;
    try
      TFDJSONInterceptor.DataSetsToJSONObject(LDeltas, LJSONObj);

      LWriter := TStreamWriter.Create(AContent);
      try
        LWriter.Write(LJSONObj.ToJSON);
      finally
        LWriter.Free;
      end;
    finally
      LJSONObj.Free;
    end;
  finally
    LDeltas.Free;
  end;
end;

constructor TMARSFDResource.Create(AOwner: TComponent);
begin
  inherited;
  FResourceDataSets := TMARSFDResourceDatasets.Create(TMARSFDResourceDatasetsItem);
end;

destructor TMARSFDResource.Destroy;
begin
  FPOSTResponse.Free;
  FResourceDataSets.Free;
  inherited;
end;

procedure TMARSFDResource.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = TOperation.opRemove then
  begin
    FResourceDataSets.ForEach(
      procedure (AItem: TMARSFDResourceDatasetsItem)
      begin
        if AItem.DataSet = AComponent then
          AItem.DataSet := nil;
      end
    );
  end;

end;

{ TMARSFDResourceDatasets }

function TMARSFDResourceDatasets.Add: TMARSFDResourceDatasetsItem;
begin
  Result := inherited Add as TMARSFDResourceDatasetsItem;
end;

function TMARSFDResourceDatasets.FindItemByDataSetName(
  AName: string): TMARSFDResourceDatasetsItem;
var
  LIndex: Integer;
  LItem: TMARSFDResourceDatasetsItem;
begin
  Result := nil;
  for LIndex := 0 to Count-1 do
  begin
    LItem := GetItem(LIndex);
    if SameText(LItem.DataSetName, AName) then
    begin
      Result := LItem;
      Break;
    end;
  end;
end;

procedure TMARSFDResourceDatasets.ForEach(
  const ADoSomething: TProc<TMARSFDResourceDatasetsItem>);
var
  LIndex: Integer;
begin
  if Assigned(ADoSomething) then
  begin
    for LIndex := 0 to Count-1 do
      ADoSomething(GetItem(LIndex));
  end;
end;

function TMARSFDResourceDatasets.GetItem(
  Index: Integer): TMARSFDResourceDatasetsItem;
begin
  Result := inherited GetItem(Index) as TMARSFDResourceDatasetsItem;
end;

{ TMARSFDResourceDatasetsItem }

procedure TMARSFDResourceDatasetsItem.AssignTo(Dest: TPersistent);
var
  LDest: TMARSFDResourceDatasetsItem;
begin
//   inherited;
  LDest := Dest as TMARSFDResourceDatasetsItem;

  LDest.DataSetName := DataSetName;
  LDest.DataSet := DataSet;
  LDest.SendDelta := SendDelta;
  LDest.Synchronize := Synchronize;
end;

constructor TMARSFDResourceDatasetsItem.Create(Collection: TCollection);
begin
  inherited;
  FSendDelta := True;
  FSynchronize := True;
end;

procedure TMARSFDResourceDatasetsItem.SetDataSet(const Value: TFDMemTable);
begin
  if FDataSet <> Value then
  begin
    FDataSet := Value;
    if Assigned(FDataSet) then
    begin
      if SendDelta then
        FDataSet.CachedUpdates := True;
      FDataSet.ActiveStoredUsage := [];
    end;
  end;
end;

end.
