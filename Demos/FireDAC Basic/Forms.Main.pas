(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
  Home: https://github.com/andrea-magni/MARS

*)
unit Forms.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  MARS.Client.CustomResource, MARS.Client.Resource, MARS.Client.FireDAC,
  MARS.Client.Application, MARS.Client.Client, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, System.Rtti, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Fmx.Bind.Grid, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope, FMX.Layouts,
  FMX.Grid, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FMX.Controls.Presentation, FMX.StdCtrls, Data.Bind.Controls,
  Fmx.Bind.Navigator, FMX.ListView.Types, FMX.ListView;

type
  TForm1 = class(TForm)
    MARSClient1: TMARSClient;
    MARSClientApplication1: TMARSClientApplication;
    MARSFDResource1: TMARSFDResource;
    StringGrid1: TStringGrid;
    BindingsList1: TBindingsList;
    ButtonPOST: TButton;
    BindNavigator1: TBindNavigator;
    Layout1: TLayout;
    employee1: TFDMemTable;
    BindSourceDB2: TBindSourceDB;
    LinkGridToDataSourceBindSourceDB2: TLinkGridToDataSource;
    procedure FormCreate(Sender: TObject);
    procedure ButtonPOSTClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.ButtonPOSTClick(Sender: TObject);
begin
  MARSFDResource1.POST();
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MARSFDResource1.GET();
end;

end.
