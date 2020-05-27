unit Server.DCS.Forms.Main;

{$I MARS.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, System.Actions, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls
, MARS.http.Server.DCS
;

type
  TMainForm = class(TForm)
    TopPanel: TPanel;
    Label1: TLabel;
    StartButton: TButton;
    StopButton: TButton;
    PortNumberEdit: TEdit;
    MainActionList: TActionList;
    StartServerAction: TAction;
    StopServerAction: TAction;
    MainTreeView: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure PortNumberEditChange(Sender: TObject);
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
  private
    FServer: TMARShttpServerDCS;
  protected
    procedure RenderEngines(const ATreeView: TTreeView);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  StrUtils
, MARS.Core.URL, MARS.Core.Engine, MARS.Core.Application, MARS.Core.Registry
, MARS.Core.Registry.Utils
, Server.Ignition
;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PortNumberEdit.Text := IntToStr(TServerEngine.Default.Port);
  RenderEngines(MainTreeView);
  StartServerAction.Execute;
end;

procedure TMainForm.PortNumberEditChange(Sender: TObject);
begin
  TServerEngine.Default.Port := StrToInt(PortNumberEdit.Text);
end;

procedure TMainForm.RenderEngines(const ATreeView: TTreeView);
begin

  ATreeview.Items.BeginUpdate;
  try
    ATreeview.Items.Clear;
    TMARSEngineRegistry.Instance.EnumerateEngines(
      procedure (AName: string; AEngine: TMARSEngine)
      var
        LEngineItem: TTreeNode;
      begin
        LEngineItem := ATreeview.Items.AddChild(nil
          , AName +  ' [ :' + AEngine.Port.ToString + AEngine.BasePath + ']'
        );

        AEngine.EnumerateApplications(
          procedure (AName: string; AApplication: TMARSApplication)
          var
            LApplicationItem: TTreeNode;
          begin
            LApplicationItem := ATreeview.Items.AddChild(LEngineItem
              , AApplication.Name +  ' [' + AApplication.BasePath + ']'
            );

            AApplication.EnumerateResources(
              procedure (AName: string; AInfo: TMARSConstructorInfo)
              begin
                ATreeview.Items.AddChild(
                  LApplicationItem
                , AInfo.TypeTClass.ClassName +  ' [' + AName + ']'
                );

              end
            );
          end
        );
      end
    );

    if ATreeView.Items.Count > 0 then
      ATreeView.Items[0].Expand(True);
  finally
    ATreeView.Items.EndUpdate;
  end;

end;

procedure TMainForm.StartServerActionExecute(Sender: TObject);
begin
  // http server implementation
  FServer := TMARShttpServerDCS.Create(TServerEngine.Default);
  try
    FServer.DefaultPort := TServerEngine.Default.Port;
    FServer.Active := True;
  except
    FServer.Free;
    raise;
  end;
end;

procedure TMainForm.StartServerActionUpdate(Sender: TObject);
begin
  StartServerAction.Enabled := (FServer = nil) or (FServer.Active = False);
end;

procedure TMainForm.StopServerActionExecute(Sender: TObject);
begin
  FServer.Active := False;
  FreeAndNil(FServer);
end;

procedure TMainForm.StopServerActionUpdate(Sender: TObject);
begin
  StopServerAction.Enabled := Assigned(FServer) and (FServer.Active = True);
end;

end.
