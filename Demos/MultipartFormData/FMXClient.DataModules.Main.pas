(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit FMXClient.DataModules.Main;

interface

uses
  System.SysUtils, System.Classes, MARS.Client.Application,
  MARS.Client.Client, MARS.Client.Client.Net, MARS.Client.CustomResource,
  MARS.Client.Resource, MARS.Client.Resource.FormData, MARS.Core.JSON
;

type
  TMainDataModule = class(TDataModule)
    MARSApplication: TMARSClientApplication;
    MARSClient: TMARSNetClient;
    HelloWorldRes1: TMARSClientResourceFormData;
  private
  public
    procedure SendDataToServer(const AJSON: TJSONObject; const AImageFileName: string;
      const AOnCompletion: TProc<TJSONObject>);
  end;

var
  MainDataModule: TMainDataModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses MARS.Core.Utils;

{ TMainDataModule }

procedure TMainDataModule.SendDataToServer(const AJSON: TJSONObject;
  const AImageFileName: string; const AOnCompletion: TProc<TJSONObject>);
var
  LBytesStream: TBytesStream;
  LBytes: TBytes;
begin
  LBytesStream := TBytesStream.Create();
  try
    LBytesStream.LoadFromFile(AImageFileName);
    LBytes := LBytesStream.Bytes;
  finally
    LBytesStream.Free;
  end;

  HelloWorldRes1.FormData := [TFormParam.Create('json', AJSON.ToJSON)
    , TFormParam.CreateFile('image', AImageFileName, LBytes, 'image/jpg')];

  HelloWorldRes1.POSTAsync(nil
  , procedure (ARes: TMARSClientCustomResource)
    begin
      if Assigned(AOnCompletion) then
        AOnCompletion((ARes as TMARSClientResourceFormData).ResponseAsJSON as TJSONObject);
    end
  , nil, True
  );

end;

end.
