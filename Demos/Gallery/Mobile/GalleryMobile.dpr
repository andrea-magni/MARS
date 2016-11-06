program GalleryMobile;

uses
  System.StartUpCopy,
  FMX.Forms,
  Forms.Main in 'Forms.Main.pas' {MainForm},
  Data.Remote in 'Data.Remote.pas' {RemoteData: TDataModule},
  Frames.Categories in 'Frames.Categories.pas' {CategoriesFrame: TFrame},
  Data.Main in 'Data.Main.pas' {MainData: TDataModule},
  Gallery.Model in '..\Model\Gallery.Model.pas',
  Gallery.Model.JSON in '..\Model\Gallery.Model.JSON.pas',
  Frames.Items in 'Frames.Items.pas' {ItemsFrame: TFrame},
  Frames.Error in 'Frames.Error.pas' {ErrorFrame: TFrame},
  Frames.SingleItem in 'Frames.SingleItem.pas' {SingleItemFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRemoteData, RemoteData);
  Application.CreateForm(TMainData, MainData);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
