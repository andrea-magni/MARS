unit Data.Report;

interface

uses
  System.SysUtils, System.Classes, frxClass, frxExportBaseDialog, frxExportPDF,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  frxDBSet, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.Stan.StorageBin;

type
  TReportData = class(TDataModule)
    frxReport1: TfrxReport;
    frxPDFExport1: TfrxPDFExport;
    frxReport2: TfrxReport;
    DataSet1: TFDMemTable;
    DataSet1IntegerField: TIntegerField;
    DataSet1StringField: TStringField;
    DataSet1Description: TStringField;
    frxDBDataset1: TfrxDBDataset;
    frxReport3: TfrxReport;
    FDMemTable1: TFDMemTable;
    FDMemTable2: TFDMemTable;
    frxDBDataset2: TfrxDBDataset;
    frxDBDataset3: TfrxDBDataset;
    DataSource1: TDataSource;
  private
  public
    function ExportReport1(): TBytes;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses IOUtils;

{ TReportData }

function TReportData.ExportReport1(): TBytes;
begin
  frxReport1.PrepareReport();
  var LFileName := TPath.GetTempFileName;
  try
    frxPDFExport1.FileName := LFileName;
    frxReport1.Export(frxPDFExport1);

    Result := TFile.ReadAllBytes(LFileName);
  finally
    TFile.Delete(LFileName);
  end;
end;

end.
