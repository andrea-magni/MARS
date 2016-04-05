(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
  Home: https://github.com/andrea-magni/MARS

*)
unit FMXClient.DataModules.Main;

interface

uses
  System.SysUtils, System.Classes, MARS.Client.CustomResource,
  MARS.Client.Resource, MARS.Client.Resource.JSON, MARS.Client.Application,
  MARS.Client.Client, MARS.Client.SubResource, MARS.Client.SubResource.JSON,
  MARS.Client.Messaging.Resource, System.JSON;

type
  TMainDataModule = class(TDataModule)
    MARSClient1: TMARSClient;
    MARSClientApplication1: TMARSClientApplication;
    HelloWorldResource: TMARSClientResource;
    EchoStringResource: TMARSClientSubResource;
    ReverseStringResource: TMARSClientSubResource;
    SumSubResource: TMARSClientSubResource;
  private
    { Private declarations }
  public
    function ExecuteHelloWorld: string;
    function EchoString(AString: string): string;
    function ReverseString(AString: string): string;
    function Sum(AFirst, ASecond: Integer) : Integer;
  end;

var
  MainDataModule: TMainDataModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TMainDataModule }

function TMainDataModule.EchoString(AString: string): string;
begin
  EchoStringResource.PathParamsValues.Text := AString;
  Result := EchoStringResource.GETAsString();
end;

function TMainDataModule.ExecuteHelloWorld: string;
begin
  Result := HelloWorldResource.GETAsString();
end;

function TMainDataModule.ReverseString(AString: string): string;
begin
  ReverseStringResource.PathParamsValues.Text := AString;
  Result := ReverseStringResource.GETAsString();
end;

function TMainDataModule.Sum(AFirst, ASecond: Integer): Integer;
begin
  SumSubResource.PathParamsValues.Clear;
  SumSubResource.PathParamsValues.Add(AFirst.ToString);
  SumSubResource.PathParamsValues.Add(ASecond.ToString);
  Result := SumSubResource.GETAsString().ToInteger;
end;

end.
