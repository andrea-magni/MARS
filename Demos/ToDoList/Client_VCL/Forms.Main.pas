(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit Forms.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, MARS.Client.CustomResource,
  MARS.Client.Resource, MARS.Client.FireDAC, MARS.Client.Application,
  MARS.Client.Client, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, Vcl.Grids, Vcl.DBGrids, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.DBCtrls;

type
  TMainForm = class(TForm)
    MARSClient: TMARSClient;
    MARSTodoApplication: TMARSClientApplication;
    MARSAccountsResource: TMARSFDResource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    SendAccountsToServerButton: TButton;
    DBGrid2: TDBGrid;
    QueryAccounts1: TFDMemTable;
    QueryItems1: TFDMemTable;
    ItemsDataSource: TDataSource;
    AccountsDataSource: TDataSource;
    MARSItemsResource: TMARSFDResource;
    GetItemsButton: TButton;
    DBNavigator2: TDBNavigator;
    SendItemsToServerButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure SendAccountsToServerButtonClick(Sender: TObject);
    procedure SendItemsToServerButtonClick(Sender: TObject);
    procedure GetItemsButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.GetItemsButtonClick(Sender: TObject);
begin
  MARSItemsResource.QueryParams.Values['username'] := QueryAccounts1.FieldByName('USERNAME').AsString;
  MARSItemsResource.GET();
end;

procedure TMainForm.SendAccountsToServerButtonClick(Sender: TObject);
begin
  MARSAccountsResource.POST();
end;

procedure TMainForm.SendItemsToServerButtonClick(Sender: TObject);
begin
  MARSItemsResource.POST();
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MARSAccountsResource.GET();
end;

end.
