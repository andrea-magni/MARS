(*
  Copyright 2016, MARS-Curiosity library
  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.FireDAC;
{$I MARS.inc}

interface

uses
  Classes, SysUtils, Rtti
, FireDAC.Comp.Client
, MARS.Core.JSON
  {$ifdef DelphiXE7_UP}, System.JSON {$endif}
, MARS.Client.Resource, MARS.Client.Client, MARS.Client.Utils
, MARS.Data.FireDAC.Utils
;

type
  TMARSFDResourceDatasets = class;

  TMARSFDResourceDatasetsItem = class(TCollectionItem)
  private
    FDataSet: TFDMemTable;
    FDataSetName: string;
    FSendDelta: Boolean;
    FSynchronize: Boolean;
    procedure SetDataSet(const Value: TFDMemTable);
    function Collection: TMARSFDResourceDatasets;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
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
    FOwnerComponent: TComponent;
    function GetItem(Index: Integer): TMARSFDResourceDatasetsItem;
  public
    function Add: TMARSFDResourceDatasetsItem;
    function FindItemByDataSetName(AName: string): TMARSFDResourceDatasetsItem;
    procedure ForEach(const ADoSomething: TProc<TMARSFDResourceDatasetsItem>);
    property Item[Index: Integer]: TMARSFDResourceDatasetsItem read GetItem;
  end;

  TOnApplyUpdatesErrorEvent = procedure (const ASender: TObject;
      const AItem: TMARSFDResourceDatasetsItem; const AErrorCount: Integer;
      const AErrors: TArray<string>; var AHandled: Boolean) of object;

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TMARSFDResource = class(TMARSClientResource)
  private
    FResourceDataSets: TMARSFDResourceDatasets;
    FPOSTResponse: TJSONValue;
    FOnApplyUpdatesError: TOnApplyUpdatesErrorEvent;
    FApplyUpdatesResults: TArray<TMARSFDApplyUpdatesRes>;
  protected
    procedure AfterGET(const AContent: TStream); override;
    procedure BeforePOST(const AContent: TMemoryStream); override;
    procedure AfterPOST(const AContent: TStream); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure AssignTo(Dest: TPersistent); override;
    function ApplyUpdatesHadErrors(const ADataSetName: string; var AErrorCount: Integer;
      var AErrorText: TArray<string>): Boolean; virtual;
    function GetResponseAsString: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property POSTResponse: TJSONValue read FPOSTResponse write FPOSTResponse;
    property ResourceDataSets: TMARSFDResourceDatasets read FResourceDataSets write FResourceDataSets;
    property OnApplyUpdatesError: TOnApplyUpdatesErrorEvent read FOnApplyUpdatesError write FOnApplyUpdatesError;
    property ApplyUpdatesResults: TArray<TMARSFDApplyUpdatesRes> read FApplyUpdatesResults;
  end;

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TMARSFDDataSetResource = class(TMARSClientResource)
  private
    FApplyUpdatesResult: TMARSFDApplyUpdatesRes;
    FDataSet: TFDMemTable;
    FSynchronize: Boolean;
    FSendDelta: Boolean;
    FSort: string;
    FFilter: string;
  protected
    procedure SetDataSet(const Value: TFDMemTable);
    procedure AssignTo(Dest: TPersistent); override;
    procedure BeforeGET; override;
    procedure AfterGET(const AContent: TStream); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Filter: string read FFilter write FFilter;
    property Sort: string read FSort write FSort;
    property DataSet: TFDMemTable read FDataSet write SetDataSet;
    property Synchronize: Boolean read FSynchronize write FSynchronize;
    property SendDelta: Boolean read FSendDelta write FSendDelta;
    property ApplyUpdatesResult: TMARSFDApplyUpdatesRes read FApplyUpdatesResult;
  end;

implementation

uses
  FireDAC.Comp.DataSet
, FireDAC.Stan.StorageBin, FireDAC.Stan.StorageJSON, FireDAC.Stan.StorageXML
, MARS.Core.Utils, MARS.Rtti.Utils
, MARS.Core.Exceptions, MARS.Core.MediaType
;

{ TMARSFDResource }

procedure TMARSFDResource.AfterGET(const AContent: TStream);
var
  LDataSet: TFDMemTable;
  LDataSets: TArray<TFDMemTable>;
  LName: string;
  LItem: TMARSFDResourceDatasetsItem;
  LCopyDataSetProc: TThreadProcedure;
  LIndex: Integer;
  LFound: Boolean;
begin
  inherited;

  LDataSets := TFDDataSets.FromJSON(AContent);
  try
    LCopyDataSetProc :=
      procedure
      begin
        LItem.DataSet.DisableControls;
        try
          LItem.DataSet.Close;
          LItem.DataSet.Data := LDataset;
          LItem.DataSet.ApplyUpdates;
        finally
          LItem.DataSet.EnableControls;
        end;
      end;
    //purge dataset on client no more present on the server
    LIndex := 0;
    while LIndex < FResourceDataSets.Count do
    begin
      LName := FResourceDataSets.Item[LIndex].DataSetName;
      LFound := False;
      for LDataSet in LDataSets do
      begin
        if SameText(LDataSet.Name, LName) then
        begin
          LFound := True;
          Break;
        end;
      end;
      if not LFound then
        FResourceDataSets.Delete(LIndex)
      else
        Inc(LIndex);
    end;

    for LDataSet in LDataSets do
    begin
      LName := LDataSet.Name;
      LItem := FResourceDataSets.FindItemByDataSetName(LName);
      if Assigned(LItem) then
      begin
        if Assigned(LItem.DataSet) then
        begin
          if LItem.Synchronize then
            TThread.Synchronize(nil, LCopyDataSetProc)
          else
            LCopyDataSetProc();
        end;
      end
      else
      begin
        LItem := FResourceDataSets.Add;
        LItem.DataSetName := LName;
      end;
    end;
  finally
    TFDDataSets.FreeAll(LDataSets);
  end;
end;

procedure TMARSFDResource.AfterPOST(const AContent: TStream);
begin
  inherited;

  if Client.LastCmdSuccess then
  begin
    if Assigned(FPOSTResponse) then
      FPOSTResponse.Free;
    FPOSTResponse := StreamToJSONValue(AContent);
    FApplyUpdatesResults := [];
    if FPOSTResponse is TJSONArray then
      FApplyUpdatesResults := TJSONArray(FPOSTResponse).ToArrayOfRecord<TMARSFDApplyUpdatesRes>;
    FResourceDataSets.ForEach(
      procedure (AItem: TMARSFDResourceDatasetsItem)
      var
        LErrorCount: Integer;
        LErrorText: TArray<string>;
        LHandled: Boolean;
      begin
        if AItem.SendDelta and Assigned(AItem.DataSet) and (AItem.DataSet.Active) then
        begin
          if ApplyUpdatesHadErrors(AItem.DataSetName, LErrorCount, LErrorText) then
          begin
            LHandled := False;
            if Assigned(OnApplyUpdatesError) then
              OnApplyUpdatesError(Self, AItem, LErrorCount, LErrorText, LHandled);
            if not LHandled then
              raise EMARSException.CreateFmt('Error applying updates to dataset %s. Error: %s. Error count: %d'
                , [AItem.DataSetName, StringArrayToString(LErrorText, sLineBreak), LErrorCount]);
          end;
          AItem.DataSet.ApplyUpdates;
        end;
      end
    );
  end;
end;

function TMARSFDResource.ApplyUpdatesHadErrors(const ADataSetName: string;
  var AErrorCount: Integer; var AErrorText: TArray<string>): Boolean;
var
  LRes: TMARSFDApplyUpdatesRes;
begin
  Result := False;

  for LRes in ApplyUpdatesResults do
  begin
    if SameText(LRes.dataset, ADataSetName) then
    begin
      AErrorCount := LRes.errorCount;
      AErrorText := LRes.errors;
      Result := AErrorCount > 0;
      Break;
    end;
  end;
end;

procedure TMARSFDResource.AssignTo(Dest: TPersistent);
var
  LDest: TMARSFDResource;
begin
  inherited AssignTo(Dest);

  LDest := Dest as TMARSFDResource;
  LDest.ResourceDataSets.Assign(ResourceDataSets);
end;

procedure TMARSFDResource.BeforePOST(const AContent: TMemoryStream);
var
  LDeltas: TArray<TFDDataSet>;
  LDelta: TFDDataSet;
begin
  inherited;

  FApplyUpdatesResults := [];
  LDeltas := [];
  try
    FResourceDataSets.ForEach(
      procedure(AItem: TMARSFDResourceDatasetsItem)
      begin
        if AItem.SendDelta and Assigned(AItem.DataSet) and (AItem.DataSet.Active) then
        begin
          LDelta := TFDMemTable.Create(nil);
          try
            LDelta.Name := AItem.DataSetName;
            LDelta.Data := AItem.DataSet.Delta;
            LDeltas := LDeltas + [LDelta];
          except
            FreeAndNil(LDelta);
            raise;
          end;
        end;
      end
    );
    TFDDataSets.ToJSON(LDeltas, AContent);
  finally
    TFDDataSets.FreeAll(LDeltas);
  end;
end;

constructor TMARSFDResource.Create(AOwner: TComponent);
begin
  inherited;

  FResourceDataSets := TMARSFDResourceDatasets.Create(TMARSFDResourceDatasetsItem);
  FResourceDataSets.FOwnerComponent := Self;
  SpecificAccept := TMediaType.APPLICATION_JSON_FireDAC + ',' + TMediaType.APPLICATION_JSON;
  SpecificContentType := TMediaType.APPLICATION_JSON_FireDAC;
end;

destructor TMARSFDResource.Destroy;
begin
  FPOSTResponse.Free;
  FResourceDataSets.Free;

  inherited;
end;

function TMARSFDResource.GetResponseAsString: string;
var
  LIndex: Integer;
  LDataSet: TMARSFDResourceDatasetsItem;
  LDataSetInfo: string;
begin
  Result := inherited GetResponseAsString;

  for LIndex := 0 to FResourceDataSets.Count-1 do
  begin
    LDataSet := FResourceDataSets.Item[LIndex];
    if Result <> '' then
      Result := Result + sLineBreak;
    LDataSetInfo := 'N/A';
    if Assigned(LDataSet.DataSet) then
      LDataSetInfo := LDataSet.DataSet.RecordCount.ToString + ' records';
    Result := Result + LDataSet.DataSetName + ': ' + LDataSetInfo;
  end;
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

function TMARSFDResourceDatasetsItem.Collection: TMARSFDResourceDatasets;
begin
  Result := inherited Collection as TMARSFDResourceDatasets;
end;

constructor TMARSFDResourceDatasetsItem.Create(Collection: TCollection);
begin
  inherited;

  FSendDelta := True;
  FSynchronize := True;
end;

function TMARSFDResourceDatasetsItem.GetDisplayName: string;
begin
  Result := DataSetName;
  if Assigned(DataSet) then
    Result := Result + ' -> ' + DataSet.Name;
end;

procedure TMARSFDResourceDatasetsItem.SetDataSet(const Value: TFDMemTable);
begin
  if FDataSet <> Value then
  begin
    if Assigned(FDataSet) then
      FDataSet.RemoveFreeNotification(Collection.FOwnerComponent);
    FDataSet := Value;
    if Assigned(FDataSet) then
    begin
      FDataSet.FreeNotification(Collection.FOwnerComponent);
      if SendDelta then
        FDataSet.CachedUpdates := True;
      FDataSet.ActiveStoredUsage := [];
    end;
  end;
end;

{ TMARSFDDataSetResource }

procedure TMARSFDDataSetResource.AfterGET(const AContent: TStream);
var
  LDataSet: TFDMemTable;
  LDataSets: TArray<TFDMemTable>;
  LCopyDataSetProc: TThreadProcedure;
begin
  inherited;

  if not Assigned(FDataSet) then
    Exit;
  LDataSets := TFDDataSets.FromJSON(AContent);
  try
    for LDataSet in LDataSets do
    begin
      LCopyDataSetProc :=
        procedure
        begin
          FDataSet.DisableControls;
          try
            FDataSet.Close;
            FDataSet.Data := LDataset;
            FDataSet.ApplyUpdates;
          finally
            FDataSet.EnableControls;
          end;
        end;
        if Synchronize then
          TThread.Synchronize(nil, LCopyDataSetProc)
        else
          LCopyDataSetProc();
    end;
  finally
    TFDDataSets.FreeAll(LDataSets);
  end;
end;

procedure TMARSFDDataSetResource.AssignTo(Dest: TPersistent);
var
  LDest: TMARSFDDataSetResource;
begin
  inherited AssignTo(Dest);

  LDest := Dest as TMARSFDDataSetResource;
  LDest.Filter := Filter;
  LDest.Sort := Sort;
  LDest.DataSet := DataSet;
  LDest.Synchronize := Synchronize;
  LDest.SendDelta := SendDelta;
// ApplyUpdatesResult
end;

procedure TMARSFDDataSetResource.BeforeGET;
begin
  inherited;

  QueryParams.Values['filter'] := Filter;
  QueryParams.Values['sort'] := Sort;
end;

constructor TMARSFDDataSetResource.Create(AOwner: TComponent);
begin
  inherited;

  FSynchronize := True;
  FSendDelta := True;
  SpecificAccept := TMediaType.APPLICATION_JSON_FireDAC + ',' + TMediaType.APPLICATION_JSON;
  SpecificContentType := TMediaType.APPLICATION_JSON_FireDAC;
end;

destructor TMARSFDDataSetResource.Destroy;
begin
  inherited;
end;

procedure TMARSFDDataSetResource.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (AComponent = FDataSet) and (Operation = TOperation.opRemove) then
    FDataSet := nil;
end;

procedure TMARSFDDataSetResource.SetDataSet(const Value: TFDMemTable);
begin
  if FDataSet <> Value then
  begin
    FDataSet := Value;
    if Assigned(FDataSet) then
    begin
      FDataSet.CachedUpdates := True;
      FDataSet.ActiveStoredUsage := [];
    end;
  end;
end;

end.
