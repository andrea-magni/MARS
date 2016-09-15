(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit FMXClient.Forms.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FMX.MultiView, FMX.Memo,
  FMX.Controls.Presentation, FMX.Edit, FMX.ScrollBox, FMX.TreeView
  , MARS.Metadata, System.ImageList, FMX.ImgList
  ;

type
  TMetadataTreeViewItem = class(TTreeViewItem)
  private
    FMetadata: TMARSMetadata;
  public
    property Metadata: TMARSMetadata read FMetadata write FMetadata;
  end;

  TMainForm = class(TForm)
    ToolBar1: TToolBar;
    Label1: TLabel;
    MetadataTreeView: TTreeView;
    DetailLayout: TLayout;
    RefreshButton: TButton;
    MetadataImageList: TImageList;
    FullPathEdit: TEdit;
    Label2: TLabel;
    ConsumesEdit: TEdit;
    Label3: TLabel;
    ProducesEdit: TEdit;
    Label4: TLabel;
    NameEdit: TEdit;
    Label5: TLabel;
    KindEdit: TEdit;
    Label6: TLabel;
    DataTypeEdit: TEdit;
    Label7: TLabel;
    procedure RefreshButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MetadataTreeViewClick(Sender: TObject);
  private
    function AddMetadataTreeviewItem(const AMetadata: TMARSMetadata; const AText: string;
      const AParent: TTreeviewItem = nil): TMetadataTreeViewItem;
    procedure UpdateDetails(const AItem: TMetadataTreeViewItem);
  protected
    procedure UpdateGUI(const AMetadata: TMARSEngineMetadata); virtual;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
   FMXClient.DataModules.Main
  , MARS.Rtti.Utils
  , MARS.Client.Utils
  , MARS.Core.Utils
  , MARS.Core.JSON
  ;

function TMainForm.AddMetadataTreeviewItem(const AMetadata: TMARSMetadata; const AText: string;
  const AParent: TTreeviewItem = nil): TMetadataTreeViewItem;
begin
  Result := TMetadataTreeViewItem.Create(MetadataTreeview);
  try
    Result.Text := AText;
    Result.Metadata := AMetadata;

    if AMetadata is TMARSEngineMetadata then
      Result.ImageIndex := 0
    else if AMetadata is TMARSApplicationMetadata then
      Result.ImageIndex := 1
    else if AMetadata is TMARSResourceMetadata then
      Result.ImageIndex := 2
    else
      Result.ImageIndex := -1;

    if AParent = nil then
      MetadataTreeView.AddObject(Result)
    else
      AParent.AddObject(Result);

    if (AMetadata is TMARSEngineMetadata) or (AMetadata is TMARSApplicationMetadata) then
      Result.Expand;
  except
    Result.Free;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  RefreshButtonClick(RefreshButton);
end;

procedure TMainForm.MetadataTreeViewClick(Sender: TObject);
begin
  UpdateDetails(MetadataTreeView.Selected as TMetadataTreeViewItem);
end;

procedure TMainForm.RefreshButtonClick(Sender: TObject);
begin
  MainDataModule.Refresh(
    procedure
    begin
      UpdateGUI(MainDataModule.Metadata);
    end
  );
end;

procedure TMainForm.UpdateDetails(const AItem: TMetadataTreeViewItem);
var
  LMetadata: TMARSMetadata;
begin
  if Assigned(AItem) and Assigned(AItem.Metadata) then
  begin
    DetailLayout.Visible := True;
    LMetadata := AItem.Metadata;

    FullPathEdit.Visible := False;
    NameEdit.Visible := False;
    ProducesEdit.Visible := False;
    ConsumesEdit.Visible := False;
    KindEdit.Visible := False;
    DataTypeEdit.Visible := False;

    if LMetadata is TMARSPathItemMetadata then
    begin
      FullPathEdit.Text := TMARSPathItemMetadata(LMetadata).FullPath;
      NameEdit.Text := TMARSPathItemMetadata(LMetadata).Name;

      ProducesEdit.Text := TMARSPathItemMetadata(LMetadata).Produces;
      ConsumesEdit.Text := TMARSPathItemMetadata(LMetadata).Consumes;

      FullPathEdit.Visible := True;
      NameEdit.Visible := True;
      ProducesEdit.Visible := True;
      ConsumesEdit.Visible := True;
      if LMetadata is TMARSMethodMetadata then
      begin
        NameEdit.Text := TMARSMethodMetadata(LMetadata).QualifiedName;
        KindEdit.Text := TMARSMethodMetadata(LMetadata).HttpMethod;
        KindEdit.Visible := True;
      end
      else
        KindEdit.Visible := False;
    end
    else if LMetadata is TMARSRequestParamMetadata then
    begin
      NameEdit.Text := TMARSRequestParamMetadata(LMetadata).Name;
      NameEdit.Visible := True;
      KindEdit.Text := TMARSRequestParamMetadata(LMetadata).Kind;
      KindEdit.Visible := True;
      DataTypeEdit.Text := TMARSRequestParamMetadata(LMetadata).DataType;
      DataTypeEdit.Visible := True;
    end;
  end
  else
  begin
    DetailLayout.Visible := False;
  end;
end;

procedure TMainForm.UpdateGUI(const AMetadata: TMARSEngineMetadata);
var
  LEngineItem: TMetadataTreeViewItem;
begin
  MetadataTreeview.BeginUpdate;
  try
    MetadataTreeView.Clear;
    UpdateDetails(nil);
    if not Assigned(AMetadata) then
      Exit;

    LEngineItem := AddMetadataTreeviewItem(AMetadata, AMetadata.Path);
    LEngineItem.Select;
    UpdateDetails(LEngineItem);

    AMetadata.ForEachApplication(
      procedure (AApplication: TMARSApplicationMetadata)
      var
        LApplicationItem: TTreeViewItem;
      begin
        LApplicationItem := AddMetadataTreeviewItem(AApplication
          , AApplication.Path, LEngineItem);

        AApplication.ForEachResource(
          procedure (AResource: TMARSResourceMetadata)
          var
            LResourceItem: TTreeViewItem;
          begin
            LResourceItem := AddMetadataTreeviewItem(AResource
              , AResource.Path, LApplicationItem);

            AResource.ForEachMethod(
              procedure (AMethod: TMARSMethodMetadata)
              var
                LMethodItem: TTreeViewItem;
              begin
                LMethodItem := AddMetadataTreeviewItem(AMethod
                  , '[' + AMethod.HttpMethod + '] ' + AMethod.Path + ' (' + AMethod.Name + ')'
                  , LResourceItem);

                AMethod.ForEachParameter(
                  procedure (ARequestParameter: TMARSRequestParamMetadata)
                  var
                    LParameterItem: TTreeViewItem;
                  begin
                    LParameterItem := AddMetadataTreeviewItem(ARequestParameter
                      , '[' + ARequestParameter.Kind + '] ' + ARequestParameter.Name
                      , LMethodItem);
                  end
                );
              end
            );
          end
        );
      end
    );
  finally
    MetadataTreeview.EndUpdate;
  end;
end;

end.
