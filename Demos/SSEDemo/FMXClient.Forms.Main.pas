(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit FMXClient.Forms.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Controls.Presentation, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, System.Actions,
  FMX.ActnList, FMX.ListView, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Data.Bind.Components, Data.Bind.DBScope, FMX.Objects;

type
  TMainForm = class(TForm)
    TopToolBar: TToolBar;
    TitleLabel: TLabel;
    EventsDataset: TFDMemTable;
    EventsListView: TListView;
    Layout1: TLayout;
    ConnectButton: TButton;
    DisconnectButton: TButton;
    ActionList1: TActionList;
    ConnectAction: TAction;
    DisconnectAction: TAction;
    EventsDatasetTimeStamp: TDateTimeField;
    EventsDatasetKind: TStringField;
    EventsDatasetContent: TStringField;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkListControlToField1: TLinkListControlToField;
    StatusRectangle: TRectangle;
    procedure FormCreate(Sender: TObject);
    procedure ConnectActionUpdate(Sender: TObject);
    procedure DisconnectActionUpdate(Sender: TObject);
    procedure ConnectActionExecute(Sender: TObject);
    procedure DisconnectActionExecute(Sender: TObject);
    procedure EventsListViewUpdateObjects(const Sender: TObject;
      const AItem: TListViewItem);
  private
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  FMXClient.DataModules.Main
, System.Net.HttpSse
, System.JSON, MARS.Core.JSON
;

procedure TMainForm.ConnectActionExecute(Sender: TObject);
begin
  MainDataModule.Connected := True;
end;

procedure TMainForm.ConnectActionUpdate(Sender: TObject);
begin
  ConnectAction.Enabled := not MainDataModule.Connected;
end;

procedure TMainForm.DisconnectActionExecute(Sender: TObject);
begin
  MainDataModule.Connected := False;
end;

procedure TMainForm.DisconnectActionUpdate(Sender: TObject);
begin
  DisconnectAction.Enabled := MainDataModule.Connected;
end;

procedure TMainForm.EventsListViewUpdateObjects(const Sender: TObject;
  const AItem: TListViewItem);
begin
  var LDetailDrawable := AItem.Objects.FindDrawable('D') as TListItemText;
  if Assigned(LDetailDrawable) then
  begin

    LDetailDrawable.TextColor := TAlphaColorRec.Black;

    if EventsDatasetKind.Value = 'heartbeat' then
      LDetailDrawable.TextColor := TAlphaColorRec.Blue
    else if EventsDatasetKind.Value = 'error' then
      LDetailDrawable.TextColor := TAlphaColorRec.Red
    else if EventsDatasetKind.Value = 'status' then
    begin
      var LStatus := EventsDatasetContent.Value;
      if LStatus = 'Open' then
        LDetailDrawable.TextColor := TAlphaColorRec.Green
      else if LStatus = 'Connecting' then
        LDetailDrawable.TextColor := TAlphaColorRec.Orange
      else
        LDetailDrawable.TextColor := TAlphaColorRec.Gray
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  EventsDataset.Active := True;

  EventsDataset.AppendRecord([Now, 'Client', 'Application started']);

  MainDataModule.OnHeartbeat :=
    procedure (AEventID: string; APayload: TJSONObject)
    begin
      EventsDataset.AppendRecord([Now, 'heartbeat', APayload.ToJSON()]);
    end;

  MainDataModule.OnStatusChanged :=
    procedure (AStatus: THTTPEventSourceStatus)
    begin
      const LStatusAsString = TRttiEnumerationType.GetName<THTTPEventSourceStatus>(AStatus);
      EventsDataset.AppendRecord([Now, 'status', LStatusAsString]);
      StatusRectangle.Fill.Color :=
        if MainDataModule.Connected then
          (if (AStatus = Open) then TAlphaColors.Green else TAlphaColors.Orange)
        else TAlphaColors.Gray;
    end;

  MainDataModule.OnError :=
    procedure (AMessage: string)
    begin
      EventsDataset.AppendRecord([Now, 'error', AMessage]);
    end;

end;

end.
