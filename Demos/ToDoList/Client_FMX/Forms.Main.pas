(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
  Home: https://github.com/andrea-magni/MARS

*)
unit Forms.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.TabControl, FMX.Controls.Presentation, System.Actions,
  FMX.ActnList, FMX.ListView.Types, FMX.ListView, FMX.ScrollBox, FMX.Memo;

type
  TMainForm = class(TForm)
    TopToolBar: TToolBar;
    Label1: TLabel;
    MainTabControl: TTabControl;
    LoginTabItem: TTabItem;
    ListTabItem: TTabItem;
    EditUser: TEdit;
    EditPassword: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    LoginButton: TButton;
    ActionList1: TActionList;
    LoginAction: TAction;
    ItemsListView: TListView;
    ItemTabItem: TTabItem;
    Memo1: TMemo;
    Button2: TButton;
    Button3: TButton;
    UpdateAction: TAction;
    AddAction: TAction;
    Button1: TButton;
    NewAction: TAction;
    procedure LoginActionExecute(Sender: TObject);
    procedure ItemsListViewDeletingItem(Sender: TObject; AIndex: Integer;
      var ACanDelete: Boolean);
    procedure AddActionExecute(Sender: TObject);
    procedure ItemsListViewItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure UpdateActionExecute(Sender: TObject);
    procedure NewActionExecute(Sender: TObject);
    procedure NewActionUpdate(Sender: TObject);
  private
    function GetCurrentId: Integer;
    { Private declarations }
  protected
    procedure RefreshList;
    procedure RenderList;

    property CurrentID: Integer read GetCurrentId;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
    Data.ToDo
  , MARS.Core.JSON
  ;

procedure TMainForm.AddActionExecute(Sender: TObject);
begin
  ToDoDM.Add(Memo1.Text,
    procedure
    begin
      RefreshList;
    end
  );
end;

function TMainForm.GetCurrentId: Integer;
begin
  Result := StrToInt(ItemsListView.Selected.Detail);
end;

procedure TMainForm.ItemsListViewDeletingItem(Sender: TObject; AIndex: Integer;
  var ACanDelete: Boolean);
begin
  TodoDM.Delete(CurrentID);
end;

procedure TMainForm.ItemsListViewItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
  Memo1.Text := AItem.Text;
  MainTabControl.SetActiveTabWithTransition(ItemTabItem, TTabTransition.Slide);
end;

procedure TMainForm.LoginActionExecute(Sender: TObject);
begin
  ToDoDM.Login(EditUser.Text, EditPassword.Text,
    procedure
    begin
      ToDoDM.GetList(
        procedure
        begin
          RefreshList;
        end
      );
    end
  );
end;

procedure TMainForm.NewActionExecute(Sender: TObject);
begin
  Memo1.Lines.Clear;
  MainTabControl.SetActiveTabWithTransition(ItemTabItem, TTabTransition.Slide);
  Memo1.SetFocus;
end;

procedure TMainForm.NewActionUpdate(Sender: TObject);
begin
  NewAction.Enabled := TodoDM.Token.Authenticated;
end;

procedure TMainForm.RefreshList;
begin
  ToDoDM.GetList(
    procedure
    begin
      RenderList;
      MainTabControl.SetActiveTabWithTransition(ListTabItem, TTabTransition.Slide);
    end
  );
end;

procedure TMainForm.RenderList;
var
  LArray: TJSONArray;
  LElement: TJSONValue;
  LObj: TJSONObject;
  LItem: TListViewItem;
  LText: string;
  LID: string;
begin
  LArray := TodoDM.AllItemsSubResource.Response as TJSONArray;
  ItemsListView.Items.Clear;
  for LElement in LArray do
  begin
    LObj := LElement as TJSONObject;
    LItem := ItemsListView.Items.Add;

    if LObj.TryGetValue<string>('Text', LText) then
      LItem.Text := LText;
    if LObj.TryGetValue<string>('ID', LID) then
      LItem.Detail := LID;
  end;
end;

procedure TMainForm.UpdateActionExecute(Sender: TObject);
begin
  TodoDM.Update(CurrentID
    , Memo1.Text
    , procedure
      begin
        RefreshList;
      end
  );
end;

end.
