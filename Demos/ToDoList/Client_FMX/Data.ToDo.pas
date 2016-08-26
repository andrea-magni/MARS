(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit Data.ToDo;

interface

uses
  System.SysUtils, System.Classes, MARS.Client.CustomResource,
  MARS.Client.Resource, MARS.Client.Token, MARS.Client.Application,
  MARS.Client.Client, MARS.Client.SubResource, MARS.Client.SubResource.JSON,
  MARS.Client.Resource.JSON, MARS.Core.JSON, System.JSON, MARS.Utils.Parameters;

type
  TTodoDM = class(TDataModule)
    ToDoClient: TMARSClient;
    ToDoApplication: TMARSClientApplication;
    Token: TMARSClientToken;
    ItemResource: TMARSClientResourceJSON;
    AllItemsSubResource: TMARSClientSubResourceJSON;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Login(AUserName, APassword: string; const AOnSuccess: TProc);
    procedure GetList(const AOnSuccess: TProc);
    procedure Delete(AID: Integer);
    procedure Add(AText: string; const AOnSuccess: TProc);
    procedure Update(AID: Integer; AText: string; const AOnSuccess: TProc);
  end;

var
  TodoDM: TTodoDM;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TDataModule1 }

procedure TTodoDM.Add(AText: string; const AOnSuccess: TProc);
begin
  ItemResource.PathParamsValues.Clear;
  ItemResource.POST(
    procedure (AStream: TMemoryStream)
    var
      LWriter: TStreamWriter;
    begin
      LWriter := TStreamWriter.Create(AStream, TEncoding.Default);
      try
        LWriter.Write('Text=' + AText);
      finally
        LWriter.Free;
      end;
    end
  );
  if Assigned(AOnSuccess) then
    AOnSuccess();
end;

procedure TTodoDM.Delete(AID: Integer);
begin
  ItemResource.PathParamsValues.Clear;
  ItemResource.PathParamsValues.Add(AID.ToString);
  ItemResource.DELETE();
end;

procedure TTodoDM.GetList(const AOnSuccess: TProc);
begin
  AllItemsSubResource.GETAsync(AOnSuccess);
end;

procedure TTodoDM.Login(AUserName, APassword: string; const AOnSuccess: TProc);
begin
  Token.UserName := AUserName;
  Token.Password := APassword;
  Token.POSTAsync(nil, AOnSuccess);
end;

procedure TTodoDM.Update(AID: Integer; AText: string; const AOnSuccess: TProc);
begin
  ItemResource.PathParamsValues.Clear;
  ItemResource.PathParamsValues.Add(AID.ToString);
  ItemResource.PUT(
    procedure (AStream: TMemoryStream)
    var
      LWriter: TStreamWriter;
    begin
      LWriter := TStreamWriter.Create(AStream, TEncoding.Default);
      try
        LWriter.Write('Text=' + AText);
      finally
        LWriter.Free;
      end;
    end
  );
  if Assigned(AOnSuccess) then
    AOnSuccess();
end;

end.
