(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)

program OTPDemoServerApplication;

uses
  Forms,
  Server.Forms.Main in 'Server.Forms.Main.pas' {MainForm},
  Server.Resources.OTP in 'Server.Resources.OTP.pas',
  Server.Ignition in 'Server.Ignition.pas',
  Server.Resources.OpenAPI in 'Server.Resources.OpenAPI.pas',
  Server.Resources.Token in 'Server.Resources.Token.pas',
  Utils.OTP in 'Utils.OTP.pas',
  UserRepository in 'UserRepository.pas',
  DelphiZXIngQRCode in 'ThirdParty\DelphiZXIngQRCode.pas',
  Utils.QRCode in 'Utils.QRCode.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
