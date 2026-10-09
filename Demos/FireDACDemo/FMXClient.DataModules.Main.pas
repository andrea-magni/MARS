(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit FMXClient.DataModules.Main;

interface

uses
  System.SysUtils, System.Classes, Data.DB
, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error
, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client
, MARS.Client.Application
, MARS.Client.Client, MARS.Client.Client.Net, MARS.Client.Log
, MARS.Client.CustomResource, MARS.Client.Resource, MARS.Client.FireDAC
;

type
  TMainDataModule = class(TDataModule)
    MARSApplication: TMARSClientApplication;
    MARSClient: TMARSNetClient;
    CustomersDataResource: TMARSFDResource;
    CustomersTable: TFDMemTable;
    CitiesTable: TFDMemTable;
    procedure MARSClientLog(Sender: TObject; const AEntry: TMARSClientLogEntry);
  private
    function GetCustomers: TDataSet;
    function GetCities: TDataSet;
  public
    /// <summary> GET customersdata: fills CustomersTable and CitiesTable </summary>
    procedure Load;
    /// <summary> POST customersdata: sends the changes of CustomersTable (its delta) and the
    /// server applies them; returns the number of changes </summary>
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
  // one TFDMemTable for each dataset of the response, by name (ResourceDataSets)
  CustomersDataResource.GET;
end;

procedure TMainDataModule.NewCustomer;
begin
  // the id comes from the identity column of the database, when the server applies the change
  CustomersTable.FieldByName('id').Required := False;
  CustomersTable.Append;
  CustomersTable.FieldByName('name').AsString := 'New customer';
  CustomersTable.FieldByName('credit').AsCurrency := 0;
  CustomersTable.Post;
end;

function TMainDataModule.Save: Integer;
begin
  if CustomersTable.State in dsEditModes then
    CustomersTable.Post;
  Result := CustomersTable.ChangeCount;
  // the delta of the items with SendDelta: an error on the server raises an exception, unless
  // CustomersDataResource.OnApplyUpdatesError handles it
  CustomersDataResource.POST;
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
