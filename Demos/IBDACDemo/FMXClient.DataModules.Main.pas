(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit FMXClient.DataModules.Main;

interface

uses
  System.SysUtils, System.Classes, Data.DB
, MemDS, VirtualTable
, MARS.Client.Application
, MARS.Client.Client, MARS.Client.Client.Net, MARS.Client.Log
, MARS.Client.IBDAC
;

type
  TMainDataModule = class(TDataModule)
    MARSApplication: TMARSClientApplication;
    MARSClient: TMARSNetClient;
    CustomersTable: TVirtualTable;
    CitiesTable: TVirtualTable;
    procedure MARSClientLog(Sender: TObject; const AEntry: TMARSClientLogEntry);
    procedure DataModuleCreate(Sender: TObject);
  private
    FSummaryResource: TMARSIBDACResource;
    FImportResource: TMARSIBDACResource;
    function GetCustomers: TDataSet;
    function GetCities: TDataSet;
  public
    /// <summary> GET customers/summary: fills CustomersTable and CitiesTable </summary>
    procedure Load;
    /// <summary> POST customers/import: sends CustomersTable, returns the number of records
    /// the server inserted or updated </summary>
    function Save: Integer;
    procedure NewCustomer;

    property Customers: TDataSet read GetCustomers;
    property Cities: TDataSet read GetCities;
  end;

var
  MainDataModule: TMainDataModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  System.JSON
, MARS.Core.JSON
;

procedure TMainDataModule.DataModuleCreate(Sender: TObject);
begin
  // The IBDAC client resources are created here, not in the .dfm: so the data module opens in
  // the IDE also when the MARSClient.IBDACDesign package (built with MARS_IBDAC defined in
  // MARS.inc) is not installed. With the package installed, they can be dropped on the data
  // module as any other component (MARS-Curiosity Client palette page).

  // GET: one TVirtualTable for each dataset of the response, by name
  FSummaryResource := TMARSIBDACResource.Create(Self);
  FSummaryResource.Application := MARSApplication;
  FSummaryResource.Resource := 'customers/summary';
  with FSummaryResource.ResourceDataSets.Add do
  begin
    DataSetName := 'Customers';
    DataSet := CustomersTable;
  end;
  with FSummaryResource.ResourceDataSets.Add do
  begin
    DataSetName := 'Cities';
    DataSet := CitiesTable;
  end;

  // POST: the whole CustomersTable (SendData), IBDAC has no delta of the changes
  FImportResource := TMARSIBDACResource.Create(Self);
  FImportResource.Application := MARSApplication;
  FImportResource.Resource := 'customers/import';
  FImportResource.SpecificAccept := 'application/json';
  with FImportResource.ResourceDataSets.Add do
  begin
    DataSetName := 'Customers';
    DataSet := CustomersTable;
    SendData := True;
  end;
end;

function TMainDataModule.GetCities: TDataSet;
begin
  Result := CitiesTable;
end;

function TMainDataModule.GetCustomers: TDataSet;
begin
  Result := CustomersTable;
end;

procedure TMainDataModule.Load;
begin
  FSummaryResource.GET;
end;

procedure TMainDataModule.NewCustomer;
begin
  // no id: the server assigns a new one (identity column)
  CustomersTable.Append;
  CustomersTable.FieldByName('name').AsString := 'New customer';
  CustomersTable.FieldByName('credit').AsCurrency := 0;
  CustomersTable.Post;
end;

function TMainDataModule.Save: Integer;
begin
  if CustomersTable.State in dsEditModes then
    CustomersTable.Post;
  FImportResource.POST;
  Result := (FImportResource.POSTResponse as TJSONObject).ReadIntegerValue('imported');
end;

procedure TMainDataModule.MARSClientLog(Sender: TObject; const AEntry: TMARSClientLogEntry);
begin
  // Called after each request of MARSClient (also when it fails), in the thread of the call.
  // MARSClient.LogOptions decides what is logged (bodies up to 64 KB by default) and what is
  // masked (credentials by default). See https://andrea-magni.github.io/MARS/client/logging
{$IFDEF DEBUG}
  TMARSClientLog.ToDebugOutput(AEntry); // Delphi IDE: View > Debug Windows > Event Log
{$ENDIF}
end;

end.
