unit Server.Resources.EmployeeDetails;

interface

uses
  System.SysUtils, System.Classes, MARS.Data.FireDAC.DataModule,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client
  , MARS.Core.Attributes, MARS.Core.URL, MARS.Core.Token
;

type
  [Path('employeeDetails')]
  TEmployeeDetailsResource = class(TMARSFDDataModuleResource)
    EmployeeQuery: TFDQuery;
    ProjectsQuery: TFDQuery;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  MARS.Core.Registry
;


initialization
  TMARSResourceRegistry.Instance.RegisterResource<TEmployeeDetailsResource>;
end.
