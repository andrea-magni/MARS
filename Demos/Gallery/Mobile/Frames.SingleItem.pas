unit Frames.SingleItem;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation
, Gallery.Model, FMX.Ani, FMX.Layouts
;

type
  TSingleItemFrame = class(TFrame)
    ItemImage: TImage;
    ItemLabel: TLabel;
    LoadingImage: TImage;
    SpinAnimation: TFloatAnimation;
    DetailsLayout: TLayout;
    CategoryLabel: TLabel;
  private
    FItem: TItem;
    FCategory: TCategory;
    procedure UpdateLabels;
    procedure Loading(const ALoading: Boolean);
  public
    property Category: TCategory read FCategory;
    property Item: TItem read FItem;
    procedure Load(ACategory: TCategory; AItem: TItem);
  end;

implementation

{$R *.fmx}

uses Data.Main, Data.Remote;


{ TSingleItemFrame }

procedure TSingleItemFrame.Load(ACategory: TCategory; AItem: TItem);
begin
  FCategory := ACategory;
  FItem := AItem;
  UpdateLabels;
  Loading(True);
  RemoteData.GetItem(FCategory.Name, FItem.Name
  , procedure (AStream: TStream)
    begin
      ItemImage.Bitmap.LoadFromStream(AStream);
      Loading(False);
    end
  );
end;

procedure TSingleItemFrame.Loading(const ALoading: Boolean);
begin
  ItemImage.Visible := not ALoading;
  LoadingImage.Visible := ALoading;
  SpinAnimation.Enabled := ALoading;
end;

procedure TSingleItemFrame.UpdateLabels;
begin
  ItemLabel.Text := Format('%s [%s]', [FItem.Name, FItem.SizeHumanReadable]);
  CategoryLabel.Text := FCategory.Name;
end;

end.
