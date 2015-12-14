unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, Vcl.Grids, Vcl.DBGrids, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, MARS.Client.CustomResource, MARS.Client.Resource,
  MARS.Client.FireDAC, MARS.Client.Application, MARS.Client.Client, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    MARSClient1: TMARSClient;
    MARSClientApplication1: TMARSClientApplication;
    MARSDatamoduleResource: TMARSFDResource;
    employee1: TFDMemTable;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    SendToServerButton: TButton;
    FilterEdit: TEdit;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure SendToServerButtonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  MARSDatamoduleResource.QueryParams.Values['filter'] := FilterEdit.Text;
  MARSDatamoduleResource.GET();
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MARSDatamoduleResource.GET();
end;

procedure TForm1.SendToServerButtonClick(Sender: TObject);
begin
  MARSDatamoduleResource.POST();
end;

end.
