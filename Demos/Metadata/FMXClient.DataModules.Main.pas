(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit FMXClient.DataModules.Main;

interface

uses
  System.SysUtils, System.Classes, MARS.Client.CustomResource,
  MARS.Client.Resource, MARS.Client.Resource.JSON, MARS.Client.Application,
  MARS.Client.Client, System.JSON, MARS.Metadata, MARS.Metadata.JSON,
  MARS.Client.Client.Indy
;

type
  TMainDataModule = class(TDataModule)
    Client: TMARSClient;
    DefaultApplication: TMARSClientApplication;
    MetadataResource: TMARSClientResourceJSON;
  private
    { Private declarations }
    FMetadata: TMARSEngineMetadata;
    function GetMetadata: TMARSEngineMetadata;
  public
    procedure Refresh(const AThen: TProc = nil);
    property Metadata: TMARSEngineMetadata read GetMetadata;
  end;

var
  MainDataModule: TMainDataModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TMainDataModule }

function TMainDataModule.GetMetadata: TMARSEngineMetadata;
begin
  if not Assigned(FMetadata) then
  begin
    FMetadata := TMARSEngineMetadata.Create(nil);
    try
      MetadataResource.GET(nil,
        procedure (AStream: TStream)
        begin
          FMetadata.FromJSON(MetadataResource.Response as TJSONObject);
        end
      );
    except
      FreeAndNil(FMetadata);
      raise;
    end;
  end;
  Result := FMetadata;
end;

procedure TMainDataModule.Refresh(const AThen: TProc);
begin
  FreeAndNil(FMetadata);
  GetMetadata;
  if Assigned(AThen) then
    AThen();
end;

end.
