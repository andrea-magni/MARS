(*
  Copyright 2016-2023, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.JSON, MARS.Core.Response
, MARS.Core.URL
//, MARS.Core.Token
, MARS.Core.Token.Resource, MARS.WebServer.Resources
, MARS.OpenAPI.v3, MARS.Metadata.Attributes
, MARS.Report.FastReport, frxClass

, Generics.Collections
;

type
  [Path('report')]
  TReportResource = class
  private
  protected
    [Context] FR: TMARSFastReport;
  public
    [GET, Path('simple'), Produces(TMediaType.APPLICATION_PDF)]
    function SimpleReport: TStream;

    [GET, Path('dataset'), Produces(TMediaType.APPLICATION_PDF)]
    function DataSetReport: TStream;

    [GET, Path('masterdetail'), Produces(TMediaType.APPLICATION_PDF)]
    function MasterDetailReport: TStream;

    [GET, Path('dbdataset'), Produces(TMediaType.APPLICATION_PDF)]
    function DBDataSetReport: TStream;

  end;

  [Path('openapi'), MetaVisible(False)]
  TOpenAPIResource = class
  protected
  public
    [GET, Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_YAML)]
    function GetOpenAPI([Context] AOpenAPI: TOpenAPI): TOpenAPI;
  end;

  [Path('www/{*}'), RootFolder('{bin}\..\..\..\www\swagger-ui-3.52.5-dist', True), MetaVisible(False)]
  TStaticContentResource = class(TFileSystemResource)
  end;

  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  end;

implementation

uses
  MARS.Core.Registry
, Data.Report
, Data.DB, FireDac.Comp.Client
;

{ TReportResource }

function TReportResource.DataSetReport: TStream;
begin
  var LMemTable := TFDMemTable.Create(nil);
  LMemTable.Name := 'DataSet1';

  LMemTable.FieldDefs.Add('IntegerField', ftInteger);
  LMemTable.FieldDefs.Add('StringField', ftString, 200);
  LMemTable.FieldDefs.Add('Description', ftString, 200);
  LMemTable.CreateDataSet;

  LMemTable.Active := True;

  for var LIndex := 0 to 1000 do
    LMemTable.AppendRecord([LIndex, LIndex.ToString, 'This is value ' + LIndex.ToString]);

  LMemTable.IndexFieldNames := 'IntegerField';

  FR.ReportDefName := 'DATASET_REPORT';

  FR.SetMasterDataSet(LMemTable, 'MyData');

  Result := TBytesStream.Create(FR.ExportToPDF);
end;

function TReportResource.DBDataSetReport: TStream;
begin
  Result := nil; // TODO
end;

function TReportResource.MasterDetailReport: TStream;
begin

  // MASTER DATASET SETUP
  var LMasterDataSet := TFDMemTable.Create(nil);
  LMasterDataSet.Name := 'MyOrders';

  LMasterDataSet.FieldDefs.Add('OrderNo', ftInteger);
  LMasterDataSet.FieldDefs.Add('Customer', ftString, 200);
  LMasterDataSet.FieldDefs.Add('OrderDate', ftDate);
  LMasterDataSet.CreateDataSet;

  LMasterDataSet.Active := True;

  for var LOrderNo := 1 to 10 do
    LMasterDataSet.AppendRecord([LOrderNo, 'Customer #' + LOrderNo.ToString, Date() + LOrderNo]);

  LMasterDataSet.IndexFieldNames := 'OrderNo';

  // DETAIL DATASET SETUP
  var LDetailsDataSet := TFDMemTable.Create(nil);
  LDetailsDataSet.Name := 'MyItems';

  LDetailsDataSet.FieldDefs.Add('OrderNo', ftInteger);
  LDetailsDataSet.FieldDefs.Add('ItemNo', ftInteger);
  LDetailsDataSet.FieldDefs.Add('Description', ftString, 200);
  LDetailsDataSet.CreateDataSet;

  LDetailsDataSet.Active := True;

  for var LOrderNo := 1 to 10 do
  begin
    for var LItemNo := 1 to Random(100) do
    begin
      LDetailsDataSet.AppendRecord([LOrderNo, LItemNo,  'Item #' + LItemNo.ToString + ' for order #' + LOrderNo.ToString]);
    end;
  end;

  LDetailsDataSet.IndexFieldNames := 'OrderNo;ItemNo';

  // MASTER DETAIL SETUP
  var LDataSource := TDataSource.Create(LDetailsDataSet);
  LDataSource.DataSet := LMasterDataSet;
  LDetailsDataSet.MasterSource := LDataSource;
  LDetailsDataSet.MasterFields := 'ORDERNO';
  LDetailsDataSet.DetailFields := 'ORDERNO';


  // REPORT SETUP
  FR.ReportDefName := 'MASTERDETAIL_REPORT';

  FR.SetMasterDataSet(LMasterDataSet, 'MyOrders');
  FR.SetDataSetForBand(
    FR.AddDataSet(LDetailsDataSet, 'MyItems')
  , 'DetailData1'
  );

//  (FR.Report.FindObject('DetailData1') as TfrxDetailData).Filter := '<MyOrders."OrderNo">=<MyItems."OrderNo">';


  Result := TBytesStream.Create(FR.ExportToPDF);
end;

function TReportResource.SimpleReport: TStream;
begin
  FR.ReportOnGetValue :=
    procedure (const AReport: TfrxReport; const VarName: string; var Value: Variant)
    begin
      if VarName = 'Test' then
        Value := 'This is my title';
    end;

  Result := TBytesStream.Create(FR.ExportToPDF);
end;

{ TOpenAPIResource }

function TOpenAPIResource.GetOpenAPI(AOpenAPI: TOpenAPI): TOpenAPI;
begin
  Result := AOpenAPI;
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResources([TReportResource, TTokenResource, TOpenAPIResource, TStaticContentResource]);

end.
