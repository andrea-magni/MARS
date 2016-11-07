program GalleryProject;

uses
  Forms,
  Forms.Main in 'Forms.Main.pas' {MainForm},
  Resources.Gallery in 'Resources.Gallery.pas',
  Gallery.Model in '..\Model\Gallery.Model.pas',
  ReadersAndWriters.HTML in 'ReadersAndWriters.HTML.pas',
  Gallery.Model.JSON in '..\Model\Gallery.Model.JSON.pas',
  ReadersAndWriters.JSON in 'ReadersAndWriters.JSON.pas',
  Resources.Razor in 'Resources.Razor.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
