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
  Fmx.Bind.Editors, Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope,
  Fmx.Dialogs, MARS.Client.Client.Indy
  ;

type
  TMainForm = class(TForm)
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
    procedure MARSFDResource1ApplyUpdatesError(const ASender: TObject;
      const AItem: TMARSFDResourceDatasetsItem; const AErrorCount: Integer;
      const AErrors: TArray<System.string>; var AHandled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.ButtonGETClick(Sender: TObject);
begin
  MARSFDResource1.GET();
end;

procedure TMainForm.ButtonPOSTClick(Sender: TObject);
begin
  MARSFDResource1.POST();
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ButtonGETClick(ButtonGET);
end;

procedure TMainForm.MARSFDResource1ApplyUpdatesError(const ASender: TObject;
  const AItem: TMARSFDResourceDatasetsItem; const AErrorCount: Integer;
  const AErrors: TArray<System.string>; var AHandled: Boolean);
begin
  if AErrorCount > 0 then
    ShowMessage( string.Join(sLineBreak, AErrors) );
end;

end.
