(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit Forms.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, System.Rtti, FMX.Grid.Style,
  Data.Bind.Controls, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  MARS.Client.CustomResource, MARS.Client.Resource, MARS.Client.FireDAC,
  MARS.Client.Application, MARS.Client.Client, FMX.StdCtrls, Fmx.Bind.Navigator,
  FMX.Layouts, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Grid,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, Fmx.Bind.Grid, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope;

type
  TForm1 = class(TForm)
    MARSClient1: TMARSClient;
    MARSClientApplication1: TMARSClientApplication;
    StringGrid1: TStringGrid;
    ButtonPOST: TButton;
    BindNavigator1: TBindNavigator;
    Layout1: TLayout;
    ButtonGET: TButton;
    MARSFDResource1: TMARSFDResource;
    employee1: TFDMemTable;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    procedure FormCreate(Sender: TObject);
    procedure ButtonPOSTClick(Sender: TObject);
    procedure ButtonGETClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.ButtonGETClick(Sender: TObject);
begin
  MARSFDResource1.GET();
end;

procedure TForm1.ButtonPOSTClick(Sender: TObject);
begin
  MARSFDResource1.POST();
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ButtonGETClick(ButtonGET);
end;

end.
