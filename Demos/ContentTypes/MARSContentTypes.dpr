(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

*)
program MARSContentTypes;

uses
  Forms,
  Server.Forms.Main in 'Server.Forms.Main.pas' {MainForm},
  Server.Resources in 'Server.Resources.pas'
  , Xml.xmldom, XML.OmniXMLDom;


{$R *.res}

begin
  DefaultDOMVendor := sOmniXmlVendor;

  ReportMemoryLeaksOnShutdown := False;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
