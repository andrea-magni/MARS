(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit FMXClient.DataModules.Main;

interface

uses
  System.SysUtils, System.Classes, MARS.Client.Application,
  MARS.Client.Client, MARS.Client.Client.Net, MARS.Client.CustomResource,
  MARS.Client.Resource, System.JSON, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.StorageBin, MARS.Client.FireDAC,
  MARS.Client.Resource.JSON, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  Model.Types
;

type
  TMainDataModule = class(TDataModule)
    MARSApplication: TMARSClientApplication;
    MARSClient: TMARSNetClient;
    HelloWorldRes: TMARSClientResource;
    RecordSubRes: TMARSClientResourceJSON;
    RecordArraySubRes: TMARSClientResourceJSON;
    ObjectSubRes: TMARSClientResourceJSON;
    ObjectArraySubRes: TMARSClientResourceJSON;
    DatasetSubRes: TMARSClientResourceJSON;
    QueryFromDBRes: TMARSFDDataSetResource;
    QueryFromDBDataset: TFDMemTable;
    EchoSubRes: TMARSClientResource;
    ReverseSubRes: TMARSClientResource;
    RecordAgeRes: TMARSClientResourceJSON;
    employeeDetails: TMARSFDResource;
    EmployeeQuery1: TFDMemTable;
    ProjectsQuery1: TFDMemTable;
  private
  public
    function HelloWorld: string;
    function Echo(const AWhat: string): string;
    function Reverse(const AWhat: string): string;
    function SomeRecord: TMyRecord;
    function RecordAge(const ARecord: TMyRecord): TAge;
    function SomeArrayOfRecords: TArray<TMyRecord>;
    function SomeObject: TMyObject;
    function SomeArrayOfObjects: TArray<TMyObject>;
    function QueryFromDB: Integer;
  end;

var
  MainDataModule: TMainDataModule;

implementation

uses
  MARS.Core.JSON
;

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TMainDataModule }

function TMainDataModule.HelloWorld: string;
begin
  Result := HelloWorldRes.GETAsString();
end;

function TMainDataModule.Echo(const AWhat: string): string;
begin
  EchoSubRes.PathParamsValues.Clear;
  EchoSubRes.PathParamsValues.Add(AWhat);
  Result := EchoSubRes.GETAsString();
end;

function TMainDataModule.RecordAge(const ARecord: TMyRecord): TAge;
begin
  RecordAgeRes.POST<TMyRecord>(ARecord); // sync
  Result := RecordAgeRes.ResponseAs<TAge>;
end;

function TMainDataModule.Reverse(const AWhat: string): string;
begin
  ReverseSubRes.PathParamsValues.Clear;
  ReverseSubRes.PathParamsValues.Add(AWhat);
  Result := ReverseSubRes.GETAsString();
end;

function TMainDataModule.SomeRecord: TMyRecord;
begin
  RecordSubRes.GET(); // sync call
  Result := RecordSubRes.ResponseAs<TMyRecord>;
end;

function TMainDataModule.SomeArrayOfRecords: TArray<TMyRecord>;
begin
  RecordArraySubRes.GET(); // sync call
  Result := RecordArraySubRes.ResponseAsArray<TMyRecord>;
end;

function TMainDataModule.SomeObject: TMyObject;
var
  LJSON: TJSONObject;
begin
  Result := nil;

  ObjectSubRes.GET(); // sync call
  LJSON := ObjectSubRes.Response as TJSONObject;
  if Assigned(LJSON) then
    Result := TJSONObject.JSONToObject<TMyObject>(LJSON);
end;

function TMainDataModule.QueryFromDB: Integer;
begin
  QueryFromDBRes.GET(); // sync call
  Result := QueryFromDBDataset.RecordCount;
end;

function TMainDataModule.SomeArrayOfObjects: TArray<TMyObject>;
var
  LJSON: TJSONArray;
  LArray: TArray<TMyObject>;
begin
  LArray := [];

  ObjectArraySubRes.GET(); // sync call
  LJSON := ObjectArraySubRes.Response as TJSONArray;
  if Assigned(LJSON) then
    LJSON.ForEach<TJSONObject>(
      function (AObj: TJSONObject): Boolean
      begin
        Result := True; // keep enumerating
        LArray := LArray + [TJSONObject.JSONToObject<TMyObject>(AObj)];
      end
    );

  Result := LArray;
end;


end.
