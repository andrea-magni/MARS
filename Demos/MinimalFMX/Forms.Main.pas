unit Forms.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Layouts,
  System.Actions, FMX.ActnList;

type
  TMainForm = class(TForm)
    Button1: TButton;
    TopToolBar: TToolBar;
    TitleText: TText;
    MainContentLayout: TLayout;
    ServerLayout: TLayout;
    StartStopLayout: TLayout;
    ServerText: TText;
    ResourcesText: TText;
    MainActionList: TActionList;
    StartStopAction: TAction;
    Timer1: TTimer;
    StatusBar1: TStatusBar;
    StatusText: TText;
    procedure FormCreate(Sender: TObject);
    procedure StartStopActionExecute(Sender: TObject);
    procedure StartStopActionUpdate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FRequestCount: Integer;
    FRequestTime: Integer;
  protected
    procedure UpdateServerText;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  SyncObjs
, MARS.Core.Registry, MARS.Utils.Parameters, MARS.Core.Activation, MARS.Core.Activation.Interfaces
, Server, Server.Resources
;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;

  FRequestCount := 0;
  FRequestTime := 0;

  TMARSActivation.RegisterAfterInvoke(
    procedure(const AActivation: IMARSActivation)
    begin
      TInterlocked.Increment(FRequestCount);
      TInterlocked.Add(FRequestTime
        , AActivation.InvocationTime.ElapsedMilliseconds + AActivation.SetupTime.ElapsedMilliseconds
      );
    end
  );

  TServer.Instance.OnStatusChange :=
    procedure (AServer: TServer)
    var
      LResources: string;
    begin
      UpdateServerText;
      if AServer.IsRunning then
      begin
        LResources := '';
        AServer.ForEachApplicationResource(
          procedure (APath: string; AClassName: string)
          begin
            if LResources <> '' then
              LResources := LResources + ', ';
            LResources := LResources + AClassName + ': ' + APath;
          end
        );
        ResourcesText.Text := LResources;
      end;
    end;

  TServer.Instance.Start;
end;

procedure TMainForm.StartStopActionExecute(Sender: TObject);
begin
  if TServer.Instance.IsRunning then
    TServer.Instance.Stop
  else
  begin
    FRequestCount := 0;
    FRequestTime := 0;

    TServer.Instance.Start;
  end;
end;

procedure TMainForm.StartStopActionUpdate(Sender: TObject);
begin
  if TServer.Instance.IsRunning then
    StartStopAction.Text := 'Stop'
  else
    StartStopAction.Text := 'Start';
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
var
  LTimePerReq: Double;
begin
  UpdateServerText;
  if FRequestCount > 0 then
  begin
    LTimePerReq := FRequestTime / FRequestCount;
    StatusText.Text := 'Req #: ' + FRequestcount.ToString + ' | '
      + 'Invoc. (ms/req): ' + FormatFloat('#,#0.0000', LTimePerReq) + ' | '
      + 'Speed (req/s): ' + FormatFloat('#,#0.00', 1000 / LTimePerReq);
  end
  else
    StatusText.Text := 'No request served so far.';
end;

procedure TMainForm.UpdateServerText;
var
  LServer: TServer;
begin
  LServer := TServer.Instance;
  if LServer.IsRunning then
    ServerText.Text := Format('Listening on http://localhost:%d%s', [LServer.EnginePort, LServer.EngineBasePath])
     + sLineBreak
     + 'Up Time: ' + LServer.HttpServer.UpTime.ToString
  else
  ServerText.Text := 'Stopped';
end;

end.
